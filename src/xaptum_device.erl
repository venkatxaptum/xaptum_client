%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2017, Xaptum, Inc.
%%% @doc
%%%
%%% device specific functionality to be used by gen_xaptum when type = device
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
  send_message/1,
  populate_credentials/3,
  generate_auth_request/1,
  generate_message_request/2]).

send_message(Message) ->
  gen_server:cast(gen_xaptum, {send_message, Message}).

populate_credentials(undefined, _User, _Token)->
  case os:getenv(?XAPTUM_DEV_GUID) of
    false -> {error, "No XAPTUM_DEV_GUID in env!"};
    Guid ->
      case os:getenv(?XAPTUM_DEV_USER) of
        false -> {error, "No XAPTUM_DEV_USER in env!"};
        User ->
          case os:getenv(?XAPTUM_DEV_TOKEN) of
            false -> {error, "No XAPTUM_DEV_TOKEN in env!"};
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