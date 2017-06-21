%%%-------------------------------------------------------------------
%% @doc xaptum_client top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('xaptum_client_sup').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([
  init/1,
  create/3]).

-define(SERVER, ?MODULE).

-include("../include/definitions.hrl").

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

create(Type, Mode, #creds{} = Creds)->
  supervisor:start_child(?MODULE, [Type, Mode, Creds]). %% calls gen_xaptum:start_link(Type, Mode, Creds)

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  RestartStrategy = {simple_one_for_one, 60, 3600},

  Children = [#{id => gen_xaptum, start => {gen_xaptum, start_link, []}, shutdown => 1000}],

  {ok, {RestartStrategy, Children}}.


%%====================================================================
%% Internal functions
%%====================================================================
