%%%-------------------------------------------------------------------
%% @doc xaptum_client public API
%% @end
%%%-------------------------------------------------------------------

-module(enfddsc_app).


%% Application behaviour and callbacks
-behaviour(application).

-export([start/2,
         stop/1
]).

-export([
	 get_application/0,
	 get_env/2,
	 priv_dir/0
]).

%% supervisor behaviour and callbacks
-behaviour(supervisor).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% application behaviour callbacks
%%====================================================================
start(_StartType, _StartArgs) ->
    lager:info("Starting enfddsc application"),

    %% Start root supervisor
    ?MODULE:start_link().

stop(_State) ->
    lager:info("Application receive stop. State is ~p", [_State]),
    ok.

%%====================================================================
%% application helper API
%%====================================================================
get_application() ->
    application:get_application(?MODULE).

get_env(App, EnvVar) ->
    case application:get_env(App, EnvVar) of
	undefined -> no_default_env_value;
	V -> V
    end.

priv_dir() ->
    case code:priv_dir(?MODULE) of
	{error, bad_name} ->
	    lager:info("Couldn't find priv dir for the application, using ./priv~n"), "./priv";
	PrivDir -> filename:absname(PrivDir)
    end.

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    %% Restart Strategy
    RestartStrategy = {one_for_one, 4, 3600},

    {ok, {RestartStrategy, []}}.
