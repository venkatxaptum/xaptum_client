%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2017, Xaptum, Inc.
%%% @doc
%%%
%%% default gen_xaptum implementation
%%%
%%% @end
%%% Created : 27. Mar 2017 12:15 PM
%%%-------------------------------------------------------------------
-module(xaptum_dummy_message_handler).
-author("iguberman").
-behavior(gen_xaptum).

%% API
-export([
  on_message/2,
  on_connect/1,
  on_disconnect/1,
  on_connect_retry/1]).

%% NOTE this message handling business can get complicated, so in real life it is
%% best handled by a call to
%% gen_server:cast(?MY_HANDLER_GEN_SERVER, Message) or a gen_fsm or something like that
on_message(ParentPid, Message)->
  io:format("Got message: ~p from ~p~n", [Message, ParentPid]),
  ok.

on_connect(_ParentPid)-> ok.
on_disconnect(_ParentPid)->ok.
on_connect_retry(_ParentPid)->ok.
