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

-export([
	 connect/4,
	 close/1,

	 send_reg_message/3,
	 send_control_message/3,
	 send_pub_req/2,
	 send_sub_req/3,
	 recv/1
]).

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
connect(Host, Port, Certfile, Keyfile) ->
    erltls:connect(Host, Port, 
		   [binary, {active, false}, 
		    {reuseaddr, true}, 
		    {packet, 0}, 
		    {keepalive, true}, 
		    {nodelay, true},
		    {verify, verify_none},
		    {fail_if_no_peer_cert, false},
		    {certfile, Certfile},
		    {keyfile, Keyfile}
		   ],2000).

send_pub_req(Client, Guid) ->
    send_req(Client, Guid, ?AUTH_EMP_REQ, <<>>). 

send_sub_req(Client, Guid, Queue) ->
    send_req(Client, Guid, ?AUTH_SUB_REQ, Queue).

send_reg_message(Client, SessionToken, Message) ->
    send_message(Client, SessionToken, ?REG_MSG, Message).

send_control_message(Client, SessionToken, Message) ->
    send_message(Client, SessionToken, ?SIGNAL_MSG, Message).

close(Client) ->
    erltls:close(Client).

recv(Client) ->
    {ok, FixedHeader} = erltls:recv(Client, 4, 2000),
    <<?DDS_MARKER:8, _Type:8, Size:16>> = FixedHeader,
    {ok, Rest} = erltls:recv(Client, Size, 2000),
    <<FixedHeader/binary, Rest/binary>>.
    
%%=============================================================
%% Private functions
%%=============================================================
send(Client, Packet) ->
    ok = erltls:send(Client, Packet).

send_req(Client, Guid, Type, ReqPayload) ->
    Size = 16 + byte_size(ReqPayload),
    FixedHeader = <<?DDS_MARKER:8, Type:8, Size:16>>,
    VariableHeader = Guid,
    Payload = ReqPayload,
    Packet = <<FixedHeader/binary, VariableHeader/binary, Payload/binary>>,
    send(Client, Packet).

send_message(Client, SessionToken, MsgType, Message) ->
    Size = 36 + byte_size(Message),
    FixedHeader = <<?DDS_MARKER:8, MsgType:8, Size:16>>,
    VariableHeader = SessionToken,
    Payload = Message,
    Packet = <<FixedHeader/binary, VariableHeader/binary, Payload/binary>>,
    send(Client, Packet).
	    
	    
    

