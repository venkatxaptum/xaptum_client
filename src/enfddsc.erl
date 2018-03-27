-module(enfddsc).

-behaviour(gen_server).

%% export API
-export([start_device/3,
	 start_subscriber/4,

	 stop/1,
	 send_message/3,
	 send_message/2
	]).

%% export gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-record(state, {ip, type, session_token, ddsc, user, token, queue}).

-define(DEVICE, device).
-define(SUBSCRIBER, subscriber).

%%====================================
%% API
%%====================================
start_device(DeviceIp, User, Token) when is_list(DeviceIp), is_list(User), is_list(Token) ->
    U = decode(User),
    T = decode(Token),
    DIP = ipv6_to_binary(DeviceIp),
    gen_server:start_link({local, ?DEVICE}, ?MODULE, [{?DEVICE, DIP, U, T}], []).

start_subscriber(SubIp, User, Token, Queue) when is_list(SubIp), is_list(User), is_list(Token), is_list(Queue) ->
    U = decode(User),
    T = decode(Token),
    Q = list_to_binary(Queue),
    SIP = ipv6_to_binary(SubIp),
    gen_server:start_link({local, ?SUBSCRIBER}, ?MODULE, [{?SUBSCRIBER, SIP, U, T, Q}], []).

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

%%====================================
%% callbacks
%%====================================
init([{?DEVICE, DIP, U, T}]) ->
    self() ! create_ddsc,
    {ok, #state{ip = DIP, type = ?DEVICE, user = U, token = T}};

init([{?SUBSCRIBER, SIP, U, T, Q}]) ->
    self() ! create_ddsc,
    {ok, #state{ip = SIP, type = ?SUBSCRIBER, user = U, token = T, queue = Q}}.

terminate(_Reason, #state{ddsc = undefined} = _State) ->
    ok;
terminate(_Reason, #state{ddsc = C} = _State) ->
    catch ddslib:close(C),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(create_ddsc, #state{ip = DIP, type = ?DEVICE, user = U, token = T} = State) ->
    {ok, C, ST} = create_dds_device(DIP, U, T),
    NewState = State#state{ddsc = C, session_token = ST},
    {noreply, NewState};

handle_info(create_ddsc, #state{ip = SIP, type = ?SUBSCRIBER, user = U, token = T, queue = Q} = State) ->
    {ok, C, ST} = create_dds_subscriber(SIP, U, T, Q),
    NewState = State#state{ddsc = C, session_token = ST},
    {noreply, NewState};
    
handle_info({ssl_closed, _Socket}, State) ->
    %% Relaunch 
    handle_info(create_ddsc, State);

handle_info({ssl, Socket, _RawData}, #state{type = T} = State) ->
    %% Ignore an dds message packets

    lager:info("~p got ~p", [T, _RawData]),
    ssl:setopts(Socket, [{active, once}]),
    {noreply, State};

handle_info(_Msg, State) ->
    {noreply, State}.


handle_cast({send, Msg}, #state{ddsc = C, session_token = ST} = State) ->
    %% send a regular message
    ddslib:send_reg_message(C, ST, Msg),
    {noreply, State};
handle_cast({send, Dest, Msg}, #state{ddsc = C, session_token = ST} = State) ->
    %% send a control message
    ControlMessage = <<Dest/binary,Msg/binary>>,
    ddslib:send_control_message(C, ST, ControlMessage),
    {noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(_Msg, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%%====================================
%% Private functions
%%====================================
decode(L) ->
    base64:decode(list_to_binary(L)).

ipv6_to_binary(IpText) ->
    {ok, Ip} = inet:parse_ipv6_address(IpText),
    List = [ <<I:16>> || I <- tuple_to_list(Ip) ],
    iolist_to_binary(List).

create_dds_device(Ip, U, T) ->
    %% connect to broker
    {ok, C} = connect_to_broker(),
    
    %% Send authentication
    ddslib:send_auth_req(C, Ip, U, T),
    lager:info("Sent device authentication Request"),

    %% Get a valid response
    {ok, SessionToken} = recv_auth_resp(C),

    %% change socket mode to active
    ssl:setopts(C, [{active, once}]),

    {ok, C, SessionToken}.

create_dds_subscriber(Ip, U, T, Q) ->
    %% connect to broker
    {ok, C} = connect_to_broker(),

    %% Send Authentication Request
    ddslib:send_auth_sub_req(C, Ip, U, T, Q),
    lager:info("Sent subscriber authentication Request"),

    %% Get an Validate Response
    {ok, SessionToken} = recv_auth_resp(C),

    %% Change socket mode to active
    ssl:setopts(C, [{active, once}]),

    {ok, C, SessionToken}.


connect_to_broker() ->
    Host = "mb.xaptum.net",
    Port = 443,
    GroupKeysFileName = "/opt/xaptum/secrets/group_keys.csv",
    {ok, GID, MyDSAPrivKey, ServerDSAPubKey} = xdaa:get_group_keys(GroupKeysFileName),

    %% Connect to XMB
    {ok, C} = ddslib:connect(Host, Port, GID, MyDSAPrivKey, ServerDSAPubKey),
    lager:info("Connected to Broker"),
    {ok, C}.

recv_auth_resp(C) ->
    Resp = ddslib:recv(C),
    <<_FixedHeader:4/bytes, SessionToken/binary>> = Resp,
    lager:info("Received Authentication Response"),
    {ok, SessionToken}.
