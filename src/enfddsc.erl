-module(enfddsc).

-behaviour(gen_server).

%% export API
-export([
	 start_device/0,
	 start_subscriber/0,

	 start_device/3,
	 start_subscriber/4,

	 stop/1,
	 send_message/3,
	 send_message/2,

	 get_message_count/1
	]).

%% export gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-export([
	 start_test_device/0,
	 start_test_subscriber/0
]).

-export([
	 get_application/0,
	 get_env/2,
	 priv_dir/0
]).

-include("definitions.hrl").

-record(state, {ip, type, session_token, ddsc, queue, certfile, keyfile, control_ipv6, sent = 0, received = 0}).

%%====================================
%% API
%%====================================
start_device() ->
    {ok, App} = enfddsc:get_application(),
    {ok, DeviceIp} = enfddsc:get_env(App, ipv6),
    {ok, CertFile} = enfddsc:get_env(App, cert_file),
    {ok, KeyFile} = enfddsc:get_env(App, key_file),
    start_device(DeviceIp, CertFile, KeyFile).

start_subscriber() ->
    {ok, App} = enfddsc:get_application(),
    {ok, SubIp} = enfddsc:get_env(App, ipv6),
    {ok, Queue} = enfddsc:get_env(App, dds_queue),
    {ok, CertFile} = enfddsc:get_env(App, cert_file),
    {ok, KeyFile} = enfddsc:get_env(App, key_file),
    start_subscriber(SubIp, Queue, CertFile, KeyFile).

start_device(DeviceIp, Certfile, Keyfile) when is_list(DeviceIp) ->
    DIP = ipv6_to_binary(DeviceIp),
    gen_server:start_link({local, ?DEVICE}, ?MODULE, [{?DEVICE, DIP, Certfile, Keyfile}], []).

start_subscriber(SubIp, Queue, Certfile, Keyfile) when is_list(SubIp), is_list(Queue) ->
    Q = list_to_binary(Queue),
    SIP = ipv6_to_binary(SubIp),
    gen_server:start_link({local, ?SUBSCRIBER}, ?MODULE, [{?SUBSCRIBER, SIP, Q, Certfile, Keyfile}], []).

send_message(Server, Msg) when is_list(Msg) ->
    send_message(Server, list_to_binary(Msg));
send_message(Server, Msg) when is_binary(Msg) ->
    gen_server:cast(Server, {send, Msg}).

send_message(Server, Dest, Msg) when is_list(Dest), is_list(Msg) ->
    send_message(Server, Dest, list_to_binary(Msg));
send_message(Server, Dest, Msg) when is_list(Dest), is_binary(Msg) ->
    D = ipv6_to_binary(Dest),
    gen_server:cast(Server, {send, D, Msg}).

stop(Server) ->
    gen_server:cast(Server, stop).

get_message_count(Server) ->
    gen_server:call(Server, get_message_count).


start_test_device() ->
    DeviceIp = "1111:1111:1111:1119::2",
    Priv = enfddsc_app:priv_dir(),
    CF = filename:join(Priv, "cert.ip2.pem"),
    KF = filename:join(Priv, "priv.ip2.pem"),
    start_device(DeviceIp, CF, KF).

start_test_subscriber() ->
    SubscriberIp = "1111:1111:1111:1119::1",
    Queue = "$rr:1111:1111:1111:1119::2",
    Priv = enfddsc_app:priv_dir(),
    CF = filename:join(Priv, "cert.ip1.pem"),
    KF = filename:join(Priv, "priv.ip1.pem"),
    start_subscriber(SubscriberIp, Queue, CF, KF).
    
    
%%====================================
%% Helper API's
%%====================================
get_application() ->
    enfddsc_app:get_application().

get_env(App, EnvVar) ->
    enfddsc_app:get_env(App, EnvVar).

priv_dir() ->
    enfddsc_app:priv_dir().
%%====================================
%% callbacks
%%====================================
init([{?DEVICE, DIP, CF, KF}]) ->
    self() ! create_ddsc,
    {ok, #state{ip = DIP, type = ?DEVICE, certfile = CF, keyfile = KF}};

init([{?SUBSCRIBER, SIP, Q, CF, KF}]) ->
    self() ! create_ddsc,
    {ok, App} = enfddsc:get_application(),
    {ok, ControlIp} = enfddsc:get_env(App, control_ipv6),
    {ok, #state{ip = SIP, type = ?SUBSCRIBER, queue = Q, certfile = CF, keyfile = KF, control_ipv6 = ControlIp}}.

terminate(_Reason, #state{ddsc = undefined} = _State) ->
    ok;
terminate(_Reason, #state{ddsc = C} = _State) ->
    catch ddslib:close(C),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(create_ddsc, #state{ip = DIP, type = ?DEVICE, keyfile = KF, certfile = CF} = State) ->
    {ok, C, ST} = create_dds_device(DIP, CF, KF),
    NewState = State#state{ddsc = C, session_token = ST},
    send_loop(),
    {noreply, NewState};

handle_info(create_ddsc, #state{ip = SIP, type = ?SUBSCRIBER, queue = Q, keyfile = KF, certfile = CF} = State) ->
    {ok, C, ST} = create_dds_subscriber(SIP, Q, CF, KF),
    NewState = State#state{ddsc = C, session_token = ST},
    {noreply, NewState};

handle_info(send_loop, #state{type = ?DEVICE} = State) ->
    Msg = <<"buzzwigs">>,
    ?MODULE:send_message(self(), Msg),
    send_loop(),
    {noreply, State};
    
handle_info({ssl_closed, _Socket}, State) ->
    %% Relaunch 
    handle_info(create_ddsc, State);

handle_info({ssl, Socket, _RawData}, #state{type = T, control_ipv6 = ControlIp, received = R, sent = S} = State) ->
    %% Log dds message packets
    lager:info("Sent ~p Packets, Received ~p Packets", [S, R+1]),
    case T of
	?SUBSCRIBER ->
	    %% Send control message
	    Control = <<"zigzaggy">>,
	    ?MODULE:send_message(self(), ControlIp, Control);

	?DEVICE ->
	    ignore
    end,
    erltls:setopts(Socket, [{active, once}]),
    {noreply, State#state{received = R+1}};

handle_info(_Msg, State) ->
    {noreply, State}.


handle_cast({send, Msg}, #state{ddsc = C, session_token = ST, sent = S} = State) ->
    %% send a regular message
    ddslib:send_reg_message(C, ST, Msg),
    {noreply, State#state{sent = S+1}};

handle_cast({send, Dest, Msg}, #state{ddsc = C, session_token = ST, sent = S} = State) ->
    %% send a control message
    ControlMessage = <<Dest/binary,Msg/binary>>,
    ddslib:send_control_message(C, ST, ControlMessage),
    {noreply, State#state{sent = S+1}};

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(get_message_count, _From, #state{type = T, sent = S, received = R} = State) ->
    Reply = {T, S, R},
    {reply, Reply, State};
handle_call(_Msg, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%%====================================
%% Private functions
%%====================================
ipv6_to_binary(IpText) ->
    {ok, Ip} = inet:parse_ipv6_address(IpText),
    List = [ <<I:16>> || I <- tuple_to_list(Ip) ],
    iolist_to_binary(List).

create_dds_device(Ip, CF, KF) ->
    %% connect to broker
    {ok, C} = connect_to_broker(CF, KF),
    
    %% Send authentication
    ddslib:send_pub_req(C, Ip),
    lager:info("Sent device authentication Request"),

    %% Get a valid response
    {ok, SessionToken} = recv_auth_resp(C),

    %% change socket mode to active
    erltls:setopts(C, [{active, once}]),

    {ok, C, SessionToken}.

create_dds_subscriber(Ip, Q, CF, KF) ->
    %% connect to broker
    {ok, C} = connect_to_broker(CF, KF),

    %% Send Authentication Request
    ddslib:send_sub_req(C, Ip, Q),
    lager:info("Sent subscriber authentication Request"),

    %% Get an Validate Response
    {ok, SessionToken} = recv_auth_resp(C),

    %% Change socket mode to active
    erltls:setopts(C, [{active, once}]),

    {ok, C, SessionToken}.


connect_to_broker(CF, KF) ->
    {ok, App} = enfddsc:get_application(),
    {ok, Host} = enfddsc:get_env(App, xaptum_host),
    {ok, Port} = enfddsc:get_env(App, xaptum_port),

    %% Connect to XMB
    {ok, C} = ddslib:connect(Host, Port, CF, KF),
    lager:info("Connected to Broker"),
    {ok, C}.

recv_auth_resp(C) ->
    Resp = ddslib:recv(C),
    <<_FixedHeader:4/bytes, SessionToken/binary>> = Resp,
    lager:info("Received Authentication Response"),
    {ok, SessionToken}.

send_loop() ->
    erlang:send_after(1000, self(), send_loop).
