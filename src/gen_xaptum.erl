%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2017, Xaptum, Inc.
%%%
%%% gen_xaptum is a gen_server implementing xaptum communication protocol
%%%
%%% @end
%%% Created : 27. Mar 2017 12:01 AM
%%%-------------------------------------------------------------------
-module(gen_xaptum).
-behaviour(gen_server).
-author("iguberman").

%% API
-export([
  start_link/3]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-export([convert_to_Ipv6Text/1, convert_from_Ipv6Text/1]).

-export([receive_message/2, receive_request_raw/1, receive_request_raw/2]).

-include("../include/definitions.hrl").

-record(state, {xaptum_host, xaptum_port, client_ip, socket, type, creds, handler, meta_data}).

-callback on_message(From :: pid(), Msg :: binary()) -> Void :: any().
-callback on_disconnect(From :: pid()) -> Void :: any().
-callback on_connect(From :: pid()) -> Void :: any().
-callback on_connect_retry(From :: pid()) -> Void :: any().

%%%===================================================================
%%% API
%%%===================================================================

start_link(Type, single, #creds{} = Creds) ->
  gen_server:start_link({local, Type}, ?MODULE, [Type, Creds], []);
start_link(Type, multi, #creds{reg_name = RegName} = Creds) when is_atom(RegName) ->
  lager:info("Starting ~p registered as ~p", [Type, RegName]),
  gen_server:start_link({local, RegName}, ?MODULE, [Type, Creds], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Type, Creds]) ->
  State = init_state(#state{creds = Creds, type = Type}),
  start(State).

start(#state{} = State)->
  gen_server:cast(self(), authenticate),
  {ok, State}.

init_state(#state{creds = #creds{guid = Guid, user = User, token = Token} = Creds, type = _Type} = State)->
  {ok, XaptumHost} = application:get_env(xaptum_host),
  {ok, XaptumPort} = application:get_env(xaptum_port),
  {ok, LocalIp} = application:get_env(local_ip),
  {ok, Handler} = application:get_env(message_handler),
  State#state{
    creds = Creds#creds{guid = convert_from_Ipv6Text(Guid), user = base64:decode(User), token = base64:decode(Token)},
    xaptum_host = XaptumHost,
    xaptum_port = XaptumPort,
    client_ip = LocalIp,
    handler = Handler}.

handle_call({set_meta, MetaData}, _From, State) ->
  {reply, ok, State#state{meta_data = MetaData}};
handle_call(get_meta, _From, #state{meta_data = MetaData} = State) ->
  {reply, MetaData, State}.

handle_cast(authenticate, #state{ handler = Handler} = State) ->
  {ok, ConnectedState} = authenticate(State),
  Handler:on_connect(self()),
  gen_server:cast(self(), receive_message),
  {noreply, ConnectedState};
handle_cast(receive_message, State) ->
  start_message_receiver(State),
  {noreply, State};
handle_cast({send_message, Payload, DestinationIp}, #state{socket = Socket, creds = Creds, type = xaptum_subscriber} = State) ->
  Guid = convert_from_Ipv6Text(DestinationIp),
  DDSMessage = xaptum_subscriber:generate_message_request(Creds, Payload, Guid),
  gen_tcp:send(Socket, DDSMessage),
  {noreply, State};
handle_cast({send_message, Payload}, #state{socket = Socket, creds = Creds, type = xaptum_device} = State) ->
  DDSMessage = xaptum_device:generate_message_request(Creds, Payload),
  gen_tcp:send(Socket, DDSMessage),
  {noreply, State};
handle_cast(_Other, State) ->
  lager:warning("Don't know how to handle_cast(~p, ~p)", [_Other, State]),
  {noreply, State}.

handle_info(_Info, State) ->
  lager:warning("Don't know how to handle_info(~p, ~p)", [_Info, State]),
  {noreply, State}.

terminate(_Reason, #state{socket = undefined} = State) ->
  ok;
terminate(_Reason, #state{socket = Socket} = State) when is_port(Socket)->
  gen_tcp:close(Socket),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

authenticate(#state{xaptum_host = Host, xaptum_port = Port, client_ip = ClientIp, creds = Info,  type = Type, handler = Handler} = State) ->
  lager:info("Authenticating ~p with credentials ~p", [Type, Info]),

  AuthRequest = Type:generate_auth_request(Info),

  case connect(Host, Port, ClientIp) of
    {ok, Socket} ->
      lager:info("Connected to ~p:~b from ~p", [Host, Port, Socket]),
      gen_tcp:send(Socket, AuthRequest),
      case receive_request_raw(Socket, 100000) of
        {ok, ?AUTH_RES, ?SESSION_TOKEN_SIZE, <<SessionToken:?SESSION_TOKEN_SIZE/bytes>>} ->
          lager:info("Received AUTH_RES with SessionToken ~p", [SessionToken]),
          {ok, State#state{creds = Info#creds{session_token = SessionToken}, socket = Socket}};
        {error, Error} ->
          lager:error("Error getting auth response: ~p, retrying auth indefinitely...", [Error]),
          timer:sleep(5000),
          authenticate(State)
      end;
    {error, Error} ->
      lager:warning("Can't connect to server due to connection error ~p, we'll be retrying indefinitely...", [Error]),
      timer:sleep(2000),
      Handler:on_connect_retry(self()),
      authenticate(State)
  end.


start_message_receiver(#state{socket = Socket, creds = #creds{session_token = SessionToken}} = State) ->
  lager:info("Entering receive_control_message loop with Socket ~p and SessionToken ~p...", [Socket, SessionToken]),
  Pid = spawn(?MODULE, receive_message, [self(), State]),
  gen_tcp:controlling_process(Socket, Pid).

receive_message(ParentPid, #state{socket = Socket, creds = #creds{session_token = SessionToken}, type = Type, handler = Handler} = State) ->
  case receive_request_raw(Socket, 30000) of
    {ok, ?CONTROL_MSG, _PayloadSize, <<ASessionToken:?SESSION_TOKEN_SIZE/bytes, Payload/binary>>} ->
      case Type of
        xaptum_device -> ASessionToken = SessionToken;
        xaptum_subscriber -> ok
      end,
      Handler:on_message(ParentPid, Payload),
      receive_message(ParentPid, State);
    {error, timeout} -> % no requests within the timeout, keep trying
      lager:warning("Haven't received a message in 30000 ms"),
      receive_message(ParentPid, State);
    {error, Error} ->
      lager:error("Didn't receive message due to error ~p... Resetting the connection...", [Error]),
        catch gen_tcp:close(Socket),
      timer:sleep(5000),
      Handler:on_disconnect(ParentPid),
      gen_server:cast(ParentPid, authenticate);
    Other -> lager:error("Received unexpected response: ~p", [Other]),
        catch gen_tcp:close(Socket),
      timer:sleep(5000),
      Handler:on_disconnect(ParentPid),
      gen_server:cast(ParentPid, authenticate)
  end.

connect(Host, Port, _ClientIp) ->
  gen_tcp:connect(Host, Port,
    [binary, {active, false}, {reuseaddr, true}, {packet, 0}, {keepalive, true}, {nodelay, true}]).

receive_request_raw(Socket) ->
  receive_request_raw(Socket, 50000).

receive_request_raw(Socket, Timeout) ->
  case gen_tcp:recv(Socket, 4, Timeout) of
    {ok, <<?DDS_MARKER:8, ReqType:8, PayloadSize:16>>} ->
      case gen_tcp:recv(Socket, PayloadSize, 20000) of
        {ok, Payload} ->
          {ok, ReqType, PayloadSize, Payload};
        {error, Error} ->
          lager:error("Error receiving DDS payload of size ~b ~p~n", [PayloadSize, Error]),
          {error, Error}
      end;
    {ok, Invalid} ->
      lager:warning("Received invalid message ~p", [Invalid]),
      {error, invalid_message};
    {error, timeout} -> {error, timeout};
    {error, Error} ->
      lager:error("Error receiving DDS header ~p~n", [Error]),
      {error, Error}
  end.

convert_to_Ipv6Text(Ipv6Bytes) ->
  <<IP1:16, IP2:16, IP3:16, IP4:16, IP5:16, IP6:16, IP7:16, IP8:16>> = Ipv6Bytes,
  list_to_binary(inet:ntoa({IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8})).

convert_from_Ipv6Text(Ipv6Text) ->
  {ok, Addr} = inet:parse_ipv6_address(Ipv6Text),
  <<<<X:16>> || X <- tuple_to_list(Addr)>>.
