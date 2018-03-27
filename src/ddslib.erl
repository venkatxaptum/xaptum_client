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
-module(ddslib).

-compile(export_all).

-define(TOKEN, <<"abcdefghijklmnopqrstuvwzyz1234567890ABCD">>).
-define(TYPE, <<0>>).
-define(TOTAL, 10).
-define(DELAY, 200).

-define(DDS_MARKER, 120).

-define(NOOP, 0).
-define(ACT_REQ, 18).
-define(ACT_RES, 146).

-define(AUTH_EMP_REQ, 64).
-define(AUTH_REG_REQ, 80).
-define(AUTH_OBB_REQ, 112).
-define(AUTH_SUB_REQ, 88).
-define(AUTH_RES, 208).

-define(OBB_MSG, 48).
-define(REG_MSG, 16).

-define(SIGNAL_MSG, 148).
%%-define(MB_HOST, "broker.xaptum.net").
-define(MB_HOST, {192,168,1,10}).

-ifndef(CONNECT_DELAY).
-define(CONNECT_DELAY, 5).
-endif.

-ifndef(TOTAL_MSG).
-define(TOTAL_MSG, 10000).
-endif.

-ifndef(MSG_BYTE_SIZE).
-define(MSG_BYTE_SIZE, 8).
-endif.

-ifndef(MSG_DELAY).
-define(MSG_DELAY, 4).
-endif.

-define(LOG_PROC, test_log_proc).
-define(LOG_SQL, <<"insert into test_log(dev_id, msg_id, message, type, ts) values (?,?,?,?,?)">>).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% DDS Protocol Implementation
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
connect(Host, Port, GID, PrivKey, ServerPubKey) ->
    {ok, Socket} = gen_tcp:connect(Host, Port, 
				   [binary, {active, false}, {reuseaddr, true}, {packet, 0}, {keepalive, true}, {nodelay, true}],2000),
    xdaa:start(Socket, GID, PrivKey, ServerPubKey).

send_activation_req(Client, ActCode) ->
    %% Create a activation request
    Size = 36,
    FixedHeader = <<?DDS_MARKER:8, ?ACT_REQ:8, Size:16>>,
    VariableHeader = ActCode,
    Packet = <<FixedHeader/binary, VariableHeader/binary>>,
    send(Client, Packet).

send_auth_req(Client, Guid, User, Tok) ->
    send_auth_req(Client, Guid, User, Tok, <<>>).

send_auth_req(Client, Guid, User, Tok, Msg) ->
    send_auth_with_payload(Client, ?AUTH_REG_REQ, Guid, User, Tok, Msg).

send_auth_sub_req(Client, Guid, User, Tok, Queue) ->
    send_auth_with_payload(Client, ?AUTH_SUB_REQ, Guid, User, Tok, Queue).

send_message(Client, Session, Msg) ->
    Size = 36 + byte_size(Msg),
    FixedHeader = <<?DDS_MARKER:8, ?REG_MSG:8, Size:16>>,
    VariableHeader = Session,
    Payload = Msg,
    Packet = <<FixedHeader/binary, VariableHeader/binary, Payload/binary>>,
    send(Client, Packet).

close(Client) ->
    ssl:close(Client).

send_auth_with_payload(Client, AuthType, Guid, User, Tok, MsgPayload) ->
    Size = 32 + byte_size(MsgPayload),
    FixedHeader = <<?DDS_MARKER:8, AuthType:8, Size:16>>,
    VariableHeader = <<Guid/binary, User/binary, Tok/binary>>,
    Payload = MsgPayload,
    Packet = <<FixedHeader/binary, VariableHeader/binary, Payload/binary>>,
    send(Client, Packet).

send_reg_message(Client, SessionToken, Message) ->
    Size = 36 + byte_size(Message),
    FixedHeader = <<?DDS_MARKER:8, ?REG_MSG, Size:16>>,
    VariableHeader = SessionToken,
    Payload = Message,
    Packet = <<FixedHeader/binary, VariableHeader/binary, Payload/binary>>,
    send(Client, Packet).

send_control_message(Client, SessionToken, Message) ->
    Size = 36 + byte_size(Message),
    FixedHeader = <<?DDS_MARKER:8, ?SIGNAL_MSG, Size:16>>,
    VariableHeader = SessionToken,
    Payload = Message,
    Packet = <<FixedHeader/binary, VariableHeader/binary, Payload/binary>>,
    send(Client, Packet).

send(Client, Packet) ->
    ok = ssl:send(Client, Packet).

recv(Client) ->
    {ok, FixedHeader} = ssl:recv(Client, 4),
    <<?DDS_MARKER:8, _Type:8, Size:16>> = FixedHeader,
    {ok, Rest} = ssl:recv(Client, Size),
    <<FixedHeader/binary, Rest/binary>>.
    

	    
	    
    

