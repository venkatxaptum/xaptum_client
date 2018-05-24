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
-module(poll_counter).

-behaviour(gen_server).

%% export API
-export([start_link/0,
	 stop/0,

	 inc_preq/0,
	 inc_presp/0
	]).

%% export gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-record(state, {poll_req = 0, poll_resp = 0}).

-define(SERVER_NAME, ?MODULE).
-define(LOCAL_SERVER, {local, ?SERVER_NAME}).



%%====================================
%% API
%%====================================
start_link() ->
    gen_server:start_link(?LOCAL_SERVER, ?MODULE, [], []).

stop() ->
    gen_server:cast(?SERVER_NAME, stop).

inc_preq() ->
    gen_server:cast(?SERVER_NAME, inc_preq).

inc_presp() ->
    gen_server:cast(?SERVER_NAME, inc_presp).
%%====================================
%% callbacks
%%====================================
init([]) ->
    self() ! write_log,
    {ok, #state{poll_req = 0, poll_resp = 0}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_info(write_log, #state{poll_req = PREQ, poll_resp = PRESP} = State) ->
    log:info("Sent ~p Poll Responses, Received ~p Poll Requests", [PRESP, PREQ]),
    erlang:send_after(1000, self(), write_log),
    {noreply, State};

handle_info(_Msg, State) ->
    {noreply, State}.

handle_cast(inc_preq, #state{poll_req = P} = State) ->
    {noreply, State#state{poll_req = P+1}};

handle_cast(inc_presp, #state{poll_resp = P} = State) ->
    {noreply, State#state{poll_resp = P+1}};

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(_Msg, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.
