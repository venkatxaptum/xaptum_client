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
-module(dummy_message_handler).
-author("iguberman").
-behavior(gen_xaptum).

%% API
-export([async_handle_message/1]).

%% NOTE this message handling business can get complicated, so in real life it is
%% best handled by a call to
%% gen_server:cast(?MY_HANDLER_GEN_SERVER, Message) or a gen_fsm or something like that
async_handle_message(Message)->
  io:format("Got message: ~p~n", [Message]),
  ok.
