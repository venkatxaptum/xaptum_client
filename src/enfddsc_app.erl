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
-export([
	 start_link/0,
	 init/1
	]).

-define(SERVER, ?MODULE).

%%====================================================================
%% application behaviour implementation
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
    case code:priv_dir(enfddsc) of
	{error, bad_name} ->
	    lager:info("Couldn't find priv dir for the application, using ./priv~n"), "./priv";
	PrivDir -> filename:absname(PrivDir)
    end.

%%====================================================================
%% supervisor behaviour implementation
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    %% Restart Strategy
    RestartStrategy = {one_for_one, 40, 3600},

    %% Create device/subscriber child spec
    {ok, App} = get_application(),
    {ok, Type} = get_env(App, type),
    Child = bacnet_child_spec(Type),

    %% Create elli child spec
    {ok, ElliPort} = get_env(App, stat_port), 
    ElliOpts = [{callback, enfddsc_http}, {port, ElliPort}],
    Elli = {
        enfddsc_http,
        {elli, start_link, [ElliOpts]},
        permanent,
        5000,
        worker,
        [elli]},

    {ok, {RestartStrategy, [Child, Elli]}}.


%% dds child spec
child_spec(xaptum_device) ->
    {xaptum_device, {enfddsc, start_device, []}, permanent, 2000, worker, [enfddsc]};
child_spec(xaptum_subscriber) ->
    {xaptum_subscriber, {enfddsc, start_subscriber, []}, permanent, 2000, worker, [enfddsc]}.

%% Bacnet child spec
bacnet_child_spec(xaptum_device) ->
    {xaptum_device, {bacnet_proxy, start_proxy, []}, permanent, 2000, worker, [enfddsc]};
bacnet_child_spec(xaptum_subscriber) ->
    {xaptum_subscriber, {bacnet_control, start_control, []}, permanent, 2000, worker, [enfddsc]}.
