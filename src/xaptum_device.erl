%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2017, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 08. May 2017 11:12 AM
%%%-------------------------------------------------------------------
-module(xaptum_device).
-author("iguberman").

-include("../include/definitions.hrl").

-define(XAPTUM_DEV_GUID, "XAPTUM_DEV_GUID").
-define(XAPTUM_DEV_USER, "XAPTUM_DEV_USER").
-define(XAPTUM_DEV_TOKEN, "XAPTUM_DEV_TOKEN").

%% API
-export([
  start/3,
  send_message/1,
  send_message/2,
  populate_credentials/3,
  generate_auth_request/1,
  generate_message_request/2,
  not_created_warning_log/0]).

%% This method is for creating device(s) on the fly
start(Guid, User, Token)->
  xaptum_client_sup:create(?MODULE, multi, #creds{guid = Guid, user = User, token = Token}).

%% When single device is created from env, the message will be sent to the one device identified by env
send_message(Message) ->
  gen_server:cast(?MODULE, {send_message, Message}).

%% When multiple devices are created on the fly, need to identify device by unique id when sending a message
%% Guid (= Ipv6 address) of the device is the initial reg name.
%% However, device can be registered as any unique atom, which then can be used here instead of Guid
%% TODO: consider using gproc to register with multiple names
send_message(RegName, Message) when is_atom(RegName)->
  gen_server:cast(RegName, {send_message, Message}).
send_message(Guid, Message) when is_list(Guid)->
  gen_server:cast(list_to_atom(Guid), {send_message, Message}).

populate_credentials(undefined, _User, _Token)->
  case os:getenv(?XAPTUM_DEV_GUID) of
    false -> {warning, "No XAPTUM_DEV_GUID in env!"};
    Guid ->
      case os:getenv(?XAPTUM_DEV_USER) of
        false -> {error, Guid, "No XAPTUM_DEV_USER in env!"};
        User ->
          case os:getenv(?XAPTUM_DEV_TOKEN) of
            false -> {error, Guid, "No XAPTUM_DEV_TOKEN in env!"};
            Token ->
              lager:info("Device credentials from system env: guid = ~p, user = ~p, token = ~p ", [Guid, User, Token]),
              {ok, #creds{guid = Guid, user = User, token = Token}}
          end
      end
  end;
populate_credentials(Guid, User, Token) when Guid =/= undefined, User =/= undefined, Token =/= undefined ->
  lager:info("Device credentials from app env: guid = ~p, user = ~p, token = ~p ", [Guid, User, Token]),
  {ok, #creds{guid = Guid, user = User, token = Token}};
populate_credentials(Guid, User, Token)->
  lager:error("Missing credentials in application env: Guid = ~p, User = ~p, Token = ~p", [Guid, User, Token]),
  {error, invalid_application_env}.

generate_auth_request(#creds{guid = Guid, user = User, token = Token})->
  PayloadSize = ?GUID_SIZE + ?USER_SIZE + ?TOKEN_SIZE,
  <<?DDS_MARKER, ?AUTH_EMP_REQ, PayloadSize:16, Guid:?GUID_SIZE/bytes, User:?USER_SIZE/bytes, Token:?TOKEN_SIZE/bytes>>.

generate_message_request(#creds{} = DeviceCreds, Message) when is_list(Message)->
  generate_message_request(DeviceCreds, list_to_binary(Message));
generate_message_request(#creds{guid = Guid, session_token = SessionToken}, Message) when is_binary(Message)->
  lager:info("Sending message from ~p with SessionToken ~p: ~p", [Guid, SessionToken, Message]),
  Size = ?SESSION_TOKEN_SIZE + size(Message),
  <<?DDS_MARKER, ?REG_MSG, Size:16, SessionToken:?SESSION_TOKEN_SIZE/bytes, Message/binary>>.

not_created_warning_log()->
  lager:warning("Device not created.  Call device:start(Guid, User, Token) to create device(s). ~nTo create device on app startup:~nEither set guid, user, and token app env or XAPTUM_DEV_GUID, XAPTUM_DEV_USER, and XAPTUM_DEV_TOKEN sys env and restart xaptum_client app").
