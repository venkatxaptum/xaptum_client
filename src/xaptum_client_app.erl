%%%-------------------------------------------------------------------
%% @doc xaptum_client public API
%% @end
%%%-------------------------------------------------------------------

-module('xaptum_client_app').

-behaviour(application).

%% Application callbacks
-export([
  start/2,
  stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  Ret = 'xaptum_client_sup':start_link(),
  maybe_create_device_or_subscriber(),
  Ret.

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================

maybe_create_device_or_subscriber()->
  case get_type() of
    {ok, Type} -> create_single_entity(Type); %% proceed to creating device or subscriber
    undefined -> ok %% not creating device or subscriber at this time
  end.

create_single_entity(Type)->
  Guid = application:get_env(xaptum_client, guid, undefined),
  User = application:get_env(xaptum_client, user, undefined),
  Token = application:get_env(xaptum_client, token, undefined),

  case Type:populate_credentials(Guid, User, Token) of
    %% Env is setup to run a single device or subscriber
    {ok, Creds} -> xaptum_client_sup:create(Type, single, Creds);
    %% Env is not set, device(s) or subscriber(s) will be created on the fly
    {warning, _Warning} ->
      Type:not_created_warning_log(),
      ok;
    {error, Guid, Error} ->
      lager:error("Error starting ~p for ~p: ~p", [Type, Guid, Error]),
      {error, invalid_env}
  end.


get_type()->
  case application:get_env(type) of
    {ok, xaptum_subscriber} -> {ok, xaptum_subscriber};
    {ok, xaptum_device} -> {ok, xaptum_device};
    %% if it's a gateway, multiple devices/subscribers can be created from the console,
    %% no need to set env for a single device or subscriber
    {ok, xaptum_gateway } -> undefined;
    {ok, Type} -> {error, invalid_type, Type};
    undefined -> undefined
  end.