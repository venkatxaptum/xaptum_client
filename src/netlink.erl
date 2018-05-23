%%-------------------------------------------------------------------------------------------
%% 
%% XAPTUM CONFIDENTIAL
%% __________________
%% 
%%  2017(C) Xaptum, Inc.
%%  All Rights Reserved.Patents Pending.
%% 
%% NOTICE:  All information contained herein is, and remains
%% the property of Xaptum, Inc.  The intellectual and technical concepts contained
%% herein are proprietary to Xaptum, Inc and may be covered by U.S. and Foreign Patents,
%% patents in process, and are protected by trade secret or copyright law.
%% Dissemination of this information or reproduction of this material
%% is strictly forbidden unless prior written permission is obtained
%% from Xaptum, Inc.
%%
%% @author Venkatakumar Srinivasan
%%
%%-------------------------------------------------------------------------------------------
-module(netlink).

-behaviour(gen_server).

%% export API
-export([start_link/0,
	 stop/0
	]).

%% export gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-record(state, {port}).

-define(SERVER_NAME, ?MODULE).
-define(LOCAL_SERVER, {local, ?SERVER_NAME}).


%%====================================
%% API
%%====================================
start_link() ->
    gen_server:start_link(?LOCAL_SERVER, ?MODULE, [], []).

stop() ->
    gen_server:cast(?SERVER_NAME, stop).

%%====================================
%% callbacks
%%====================================
init([]) ->
    self() ! start_netlink_monitor,
    {ok, #state{}}.

terminate(_Reason, _State) ->
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
    handle_port_data(Data),
    {noreply, State};

handle_info({Port, {exit_status, Status}}, #state{port = Port} = State) ->
    lager:info("Netlink port exited with status ~p", [Status]),
    self() ! start_netlink_monitor,
    {noreply, State};

handle_info(_Msg, State) ->
    lager:info("Got ~p", [_Msg]),
    {noreply, State}.


handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(_Msg, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_port_data(<<131, _/binary>> = Data) ->
    Term = binary_to_term(Data),
    handle_port_data(Term);


handle_port_data({deladdr, Interface}) ->
    lager:info("Interface ~p: address was removed", [Interface]);

handle_port_data({newaddr, Interface, IpAddress}) ->
    lager:info("Interface ~p: new address was assigned: ~p", [Interface, IpAddress]),
    {ok, App} = enfddsc:get_application(),
    {ok, Type} = enfddsc:get_env(App, type),
    case whereis(Type) of
	undefined ->
	    lager:info("Not able to get running ~p", [Type]);
	Pid ->
	    lager:info("Reconnecting ~p!", [Type]),
	    Pid ! connect_to_broker
    end;

handle_port_data({newlink, Interface, down, not_running}) ->
    lager:info("New network interface ~p, state: DOWN NOT RUNNING", [Interface]);

handle_port_data({newlink,Interface,up,not_running}) ->
    lager:info("New network interface ~p, state: UP NOT RUNNING", [Interface]);

handle_port_data({newlink,Interface,up,running}) ->
    lager:info("New network interface ~p, state: UP RUNNING", [Interface]);

handle_port_data(Data) ->
    lager:info("Got ~p from port", [Data]).
    
