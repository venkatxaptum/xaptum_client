-module(bacnet_proxy).

-behaviour(gen_server).

%% export API
-export([
	 start_proxy/0,

	 start_device/1,

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

-include("bacnet.hrl").

-record(state, {ip, type, session_token, queue, sent = 0, received = 0, fsm = init, udp}).

%%====================================
%% API
%%====================================
start_proxy() ->
    {ok, App} = enfddsc:get_application(),
    {ok, IpFile} = enfddsc:get_env(App, ipv6_file),

    DeviceIp = enfddsc:read_ipv6_file(IpFile),
    start_device(DeviceIp).

start_device(DeviceIp) when is_list(DeviceIp) ->
    DIP = enfddsc:ipv6_to_binary(DeviceIp),
    gen_enfc:start_link({local, ?BACNET_PROXY}, ?MODULE, [{?BACNET_PROXY, DIP}], []).

send_message(Server, Msg) when is_list(Msg) ->
    send_message(Server, list_to_binary(Msg));
send_message(Server, Msg) when is_binary(Msg) ->
    gen_server:cast(Server, {send, Msg}).

send_message(Server, Dest, Msg) when is_list(Dest), is_list(Msg) ->
    send_message(Server, Dest, list_to_binary(Msg));
send_message(Server, Dest, Msg) when is_list(Dest), is_binary(Msg) ->
    D = enfddsc:ipv6_to_binary(Dest),
    gen_server:cast(Server, {send, D, Msg}).

stop(Server) ->
    gen_server:cast(Server, stop).

get_message_count(Server) ->
    gen_server:call(Server, get_message_count).
    
%%====================================
%% Helper API's
%%====================================

%%====================================
%% callbacks
%%====================================
init([{?BACNET_PROXY, DIP}]) ->
    self() ! init_session,
    {ok, #state{ip = DIP, type = ?BACNET_PROXY}}.

terminate(_Reason, #state{udp = Udp} = _State) ->
    catch gen_udp:close(Udp),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(init_session, #state{ip = DIP, type = ?BACNET_PROXY} = State) ->
    ok = create_dds_device(DIP),
    NewState = State#state{fsm = init},
    send_init_timeout(),
    {noreply, NewState};

handle_info(init_timeout, #state{fsm = init} = State) ->
    {stop, init_timeout, State};

handle_info(init_timeout, #state{fsm = op} = State) ->
    {noreply, State};

handle_info({recv, RawData}, #state{fsm = init, type = ?BACNET_PROXY} = State) ->
    <<120, _PacketType:8, _Size:16, SessionToken/binary>> = RawData,
    lager:info("Received Authentication Response"),
    self() ! heartbeat_loop,
    {ok, Socket} = gen_udp:open(8780, [binary, {active, false}]),
    {noreply, State#state{session_token = SessionToken, fsm = op, udp = Socket}};
    
handle_info({recv, RawData}, #state{fsm = op, type = ?BACNET_PROXY, received = R, sent = S, udp = Socket} = State) ->
    %% Log dds message packets
    lager:info("Sent ~p Packets, Received ~p Packets", [S+1, R+1]),
    <<120, _PacketType:8, _Size:16, _SessionToken:36/binary, BacnetRequest/binary>> = RawData,
    
    %% Send socket to 47808
    ok = gen_udp:send(Socket, {127,0,0,1}, 47808, BacnetRequest),

    %% Read the response from bacserv
    {ok, {_Address, _Port, BacnetAck}} = gen_udp:recv(Socket, 0, 5000),
    
    %% Now Send the ACK to control	    
    ?MODULE:send_message(self(), BacnetAck),
    
    {noreply, State#state{received = R+1}};

handle_info(heartbeat_loop, #state{ip = DIP, type = ?BACNET_PROXY} = State) ->
    Msg = <<?IAM, DIP/binary>>,
    ?MODULE:send_message(self(), Msg),
    heartbeat_loop(),
    {noreply, State};

handle_info(_Msg, State) ->
    {noreply, State}.


handle_cast({send, Msg}, #state{session_token = ST, sent = S} = State) ->
    %% send a regular message
    Packet = ddslib:build_reg_message(ST, Msg),
    ok = gen_enfc:send(Packet),
    {noreply, State#state{sent = S+1}};

handle_cast({send, Dest, Msg}, #state{session_token = ST, sent = S} = State) ->
    %% send a control message
    Control = <<Dest/binary,Msg/binary>>,
    Packet = ddslib:build_control_message(ST, Control),
    ok = gen_enfc:send(Packet),
    {noreply, State#state{sent = S+1}};

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(get_message_count, _From, #state{type = ?BACNET_PROXY, sent = S, received = R} = State) ->
    Reply = {?BACNET_PROXY, S, R},
    {reply, Reply, State};
handle_call(_Msg, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%%====================================
%% Private functions
%%====================================
create_dds_device(Ip) ->
    %% Build authentication
    PubReq = ddslib:build_init_pub_req(Ip),
    ok = gen_enfc:send(PubReq),
    lager:info("Sent device authentication Request"),
    ok.

heartbeat_loop() ->
    erlang:send_after(10000, self(), heartbeat_loop).

send_init_timeout() ->
    erlang:send_after(2000, self(), init_timeout).
