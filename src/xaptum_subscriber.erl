%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2017, Xaptum, Inc.
%%% @doc
%%%
%%% subscriber specific functionality to be used by gen_xaptum when type = xaptum_subscriber
%%%
%%% @end
%%% Created : 08. May 2017 5:37 PM
%%%-------------------------------------------------------------------
-module(xaptum_subscriber).
-author("iguberman").

%% API
-export([]).

-include("../include/definitions.hrl").

-define(XAPTUM_SUB_GUID, "XAPTUM_SUB_GUID").
-define(XAPTUM_SUB_USER, "XAPTUM_SUB_USER").
-define(XAPTUM_SUB_TOKEN, "XAPTUM_SUB_TOKEN").
-define(XAPTUM_SUB_QUEUE, "XAPTUM_SUB_QUEUE").

%% API
-export([
  start/4,
  start/5,
  send_message/2,
  send_message/3,
  populate_credentials/3,
  generate_auth_request/1,
  generate_message_request/3,
  not_created_warning_log/0]).

%% TODO registering with the queue is temporary hack
start(Guid, User, Token, Queue) when is_binary(Queue) ->
  start(Guid, User, Token, Queue, binary_to_atom(Queue, utf8));
start(Guid, User, Token, Queue) when is_list(Queue) ->
  start(Guid, User, Token, Queue, list_to_atom(Queue)).

start(Guid, User, Token, Queue, RegName) when is_list(Queue)->
  start(Guid, User, Token, list_to_binary(Queue), RegName);
start(Guid, User, Token, Queue, RegName) when is_binary(Queue), is_atom(RegName) ->
  xaptum_client_sup:create(?MODULE, multi, #creds{guid = Guid, user = User, token = Token, queue = Queue, reg_name = RegName}).


send_message(Message, DestinationGuid) ->
  gen_server:cast(?MODULE, {send_message, Message, DestinationGuid}).

send_message(RegName, Message, DestinationGuid) when is_atom(RegName)->
  gen_server:cast(RegName, {send_message, Message, DestinationGuid});
send_message(SrcGuid, Message, DestinationGuid) when is_list(SrcGuid)->
  gen_server:cast(list_to_atom(SrcGuid), {send_message, Message, DestinationGuid}).

populate_credentials(undefined, _User, _Token)->
  case os:getenv(?XAPTUM_SUB_GUID) of
    false -> {warning, "No XAPTUM_SUB_GUID in env!"};
    Guid ->
      case os:getenv(?XAPTUM_SUB_USER) of
        false -> {error, Guid, "No XAPTUM_SUB_USER in env!"};
        User ->
          case os:getenv(?XAPTUM_SUB_TOKEN) of
            false -> {error, Guid, "No XAPTUM_SUB_TOKEN in env!"};
            Token ->
              {ok, Queue} = get_queue(),
              {ok, #creds{guid = Guid, user = User, token = Token, queue = list_to_binary(Queue)}}
          end
      end
  end;
populate_credentials(Guid, User, Token) when Guid =/= undefined, User =/= undefined, Token =/= undefined ->
  {ok, #creds{guid = Guid, user = User, token = Token}};
populate_credentials(Guid, User, Token)->
  lager:error("Missing credentials in application env: Guid = ~p, User = ~p, Token = ~p", [Guid, User, Token]),
  {error, invalid_application_env}.


get_queue()->
  case application:get_env(xaptum_client, queue, undefined) of
    undefined ->
      case os:getenv(?XAPTUM_SUB_QUEUE) of
        false -> {error, "No XAPTUM_SUB_QUEUE in application or os env!"};
        Queue -> {ok, Queue}
      end;
    Queue -> {ok, Queue}
  end.

generate_auth_request(#creds{guid = Guid, user = User, token = Token, queue = Queue})->
  PayloadSize = ?GUID_SIZE + ?USER_SIZE + ?TOKEN_SIZE + size(Queue),
  <<?DDS_MARKER, ?AUTH_SUB_REQ, PayloadSize:16, Guid:?GUID_SIZE/bytes, User:?USER_SIZE/bytes, Token:?TOKEN_SIZE/bytes, Queue/binary>>.

generate_message_request(#creds{} = SubCreds, Message, DestinationGuid) when is_list(DestinationGuid)->
  generate_message_request(SubCreds, Message, list_to_binary(DestinationGuid));
generate_message_request(#creds{} = SubCreds, Message, DestinationGuid) when is_list(Message)->
  generate_message_request(SubCreds, list_to_binary(Message), DestinationGuid);
generate_message_request(#creds{guid = Guid, session_token = SessionToken}, Message, DestinationGuid) when is_binary(Message), is_binary(DestinationGuid) ->
  lager:info("Sending control message from ~p to ~p: '~p'", [Guid, DestinationGuid, Message]),
  Payload = <<DestinationGuid/binary, Message/binary>>,
  Size = size(SessionToken) + size(Payload),
  <<?DDS_MARKER, ?CONTROL_MSG, Size:16, SessionToken:?SESSION_TOKEN_SIZE/bytes, Payload/binary>>.

not_created_warning_log()->
  lager:warning("Subscriber not created.  Call subscriber:start(Guid, User, Token, Queue) to create subscriber(s). ~nTo create a single device on app startup:~nEither set guid, user, token, and queue app env or ~nXAPTUM_SUB_GUID, XAPTUM_SUB_USER, XAPTUM_SUB_TOKEN, and XAPTUM_SUB_QUEUE sys env and restart xaptum_client app").
