-module(gen_enfc).

-behaviour(gen_server).

%% export API
-export([start_link/4,
	 stop/1,

	 send/1
	]).

%% export gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-record(state, {name, target_module, target_state, ddsc, port}).

%%====================================
%% API
%%====================================
start_link(Name, TargetModule, TargetArgs, Options) ->
    Args = {init, Name, TargetModule, TargetArgs},
    gen_server:start_link(Name, ?MODULE, [Args], Options).

stop(Server) ->
    gen_server:cast(Server, stop).

send(Data) ->
    gen_server:cast(self(), {send_to_broker, Data}),
    ok.

%%====================================
%% callbacks
%%====================================
init([{init, Name, TargetModule, TargetArgs}]) ->
    %% Start netlink monitor
    self() ! start_netlink_monitor,

    %% Send a message to connect
    self() ! connect_to_broker,

    %% initialize callback server:
    TargetResult =  TargetModule:init(TargetArgs),
    case TargetResult of
    {ok, TargetState} ->
        State = #state{name = Name, target_module = TargetModule, target_state = TargetState},               
        {ok, State};
    {ok, TargetState, _Timeout} ->
        State = #state{name = Name, target_module = TargetModule, target_state = TargetState},               
        {ok, State};
    {stop, Reason} ->
        {stop, Reason};
    ignore ->
        ignore
    end.

terminate(Reason, #state{target_module = TargetModule, ddsc = C} = State) ->
    TargetModule:terminate(Reason, State#state.target_state),

    %% close the connection
    catch erltls:close(C),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(start_netlink_monitor, State) ->
    lager:info("Starting netlink port"),
    PrivDir = enfddsc:priv_dir(),
    PortFile = filename:join([PrivDir, "xaptum_client"]),
    Port = open_port({spawn, PortFile}, [binary, {packet,4}, exit_status]),
    {noreply, State#state{port = Port}};

handle_info({Port, {data, Data}}, #state{port = Port} = State) ->
    case handle_port_data(Data) of
	ok ->
	    {noreply, State};
	stop ->
	    {stop, normal, State}
    end;

handle_info({Port, {exit_status, Status}}, #state{port = Port} = State) ->
    lager:info("Netlink port exited with status ~p", [Status]),
    self() ! start_netlink_monitor,
    {noreply, State};

handle_info(connect_to_broker, State) ->
    {ok, C} = connect_to_broker(),
    {noreply, State#state{ddsc = C}};

handle_info({ssl_closed, _Socket}, State) ->
    %% Relaunch 
    handle_info(connect_to_broker, State);

handle_info({ssl, Socket, RawData}, State) ->
    Result = delegate_to_target(State, handle_info, [{recv, RawData}, State#state.target_state]),
    erltls:setopts(Socket, [{active, once}]),
    Result;
    
handle_info(Msg, State) ->
    delegate_to_target(State, handle_info, [Msg, State#state.target_state]).

handle_cast({send_to_broker, Data}, #state{ddsc = C} = State) ->
    ok = erltls:send(C, Data),
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Msg, State) ->
    delegate_to_target(State, handle_cast, [Msg, State#state.target_state]).


handle_call(Msg, From, State) ->
    delegate_to_target(State, handle_call, [Msg, From, State#state.target_state]).

%%====================================
%% Private functions
%%====================================
handle_port_data(<<131, _/binary>> = Data) ->
    Term = binary_to_term(Data),
    handle_port_data(Term);

handle_port_data({deladdr, Interface}) ->
    lager:info("Interface ~p: address was removed", [Interface]),
    lager:info("Detected ip address change. Reconnecting...."),
    stop;

handle_port_data({newaddr, Interface, IpAddress}) ->
    lager:info("Interface ~p: new address was assigned: ~p", [Interface, IpAddress]),
    ok;

handle_port_data({newlink, Interface, down, not_running}) ->
    lager:info("New network interface ~p, state: DOWN NOT RUNNING", [Interface]),
    ok;

handle_port_data({newlink,Interface,up,not_running}) ->
    lager:info("New network interface ~p, state: UP NOT RUNNING", [Interface]),
    ok;

handle_port_data({newlink,Interface,up,running}) ->
    lager:info("New network interface ~p, state: UP RUNNING", [Interface]),
    ok;

handle_port_data(Data) ->
    lager:info("Got ~p from port", [Data]),
    ok.

get_app() ->
    application:get_application(?MODULE).

get_env(App, Key) ->
     case application:get_env(App, Key) of
	undefined -> no_default_env_value;
	V -> V
    end.

delegate_to_target(State, TargetCall, Args) ->
    TargetModule = State#state.target_module,
    TargetResult = apply(TargetModule, TargetCall, Args),
    %% index of state in tuple:
    IndexState = case TargetResult of
             {reply, _Reply, TargetStateUpdate} ->
             3;
             {reply, _Reply, TargetStateUpdate, _Timeout}  ->
             3;
             {noreply, TargetStateUpdate} ->
             2;
             {noreply, TargetStateUpdate, _Timeout} ->
             2;
             {stop, _Reason, _Reply, TargetStateUpdate} ->
             4;
             {stop, _Reason, TargetStateUpdate} ->
             3;
             {ok, TargetStateUpdate} ->  %% for code change
             2
         end,
    NewState = State#state{target_state = TargetStateUpdate},
    Result = setelement(IndexState, TargetResult, NewState),
    Result.

connect_to_broker() ->
    {ok, App} = get_app(),
    {ok, Host} = get_env(App, xaptum_host),
    {ok, Port} = get_env(App, xaptum_port),
    {ok, Certfile} = get_env(App, cert_file),
    {ok, Keyfile} = get_env(App, key_file),

    %% Connect to XMB
    Connect = erltls:connect(Host, Port, 
			     [binary, {active, once}, 
			      {reuseaddr, true}, 
			      {packet, 0}, 
			      {keepalive, true}, 
			      {nodelay, true},
			      {verify, verify_none},
			      {fail_if_no_peer_cert, false},
			      {certfile, Certfile},
			      {keyfile, Keyfile}
			     ],2000),
    case Connect of
	{ok, C} ->
	    lager:info("Connected to Broker"),
	    {ok, C};
	_ ->
	    lager:info("Unable to connect to broker. Retrying after 1 second"),
	    timer:sleep(1000),
	    connect_to_broker()
    end.
