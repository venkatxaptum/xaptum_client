%%%-------------------------------------------------------------------
%%% @author zanebeckwith
%%% @copyright (C) 2017, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% group_keys_parsing gets GID, Other's Public DSA Key, and My Private DSA Key,
%%% from the group_keys_file.
%%%-------------------------------------------------------------------

-module(group_keys_parsing).
-export([get_group_keys/1]).

get_group_keys(GroupKeysFileName) ->
        open_file(GroupKeysFileName).

open_file(GroupKeysFileName) ->
        case file:open(GroupKeysFileName, [read]) of
                {ok, GroupKeysFile} ->
                        ignore_first_line(GroupKeysFile);
                {error, Reason} ->
                        lager:error("XDAA: Error opening group keys file \"~p\": ~p", [GroupKeysFileName, Reason]),
                        exit("XDAA: Error opening group keys file")
        end.

ignore_first_line(GroupKeysFile) ->
        case file:read_line(GroupKeysFile) of
                {ok, _} ->
                        parse_keys_line(GroupKeysFile);
                eof ->
                        exit("XDAA: Empty group keys file!");
                {error, Reason} ->
                        lager:error("XDAA: Error reading first line in group keys file: ~p", [Reason]),
                        exit("XDAA: Error reading first line in group keys file")
        end.

parse_keys_line(GroupKeysFile) ->
        case file:read_line(GroupKeysFile) of
                {ok, Line} ->
                        [GIDRaw,PublicKeyRaw,PrivateKeyRaw] = string:tokens(Line, ",\n\r"),
                        GID = list_to_binary(GIDRaw),
                        ServerDSAPubKey = list_to_integer(PublicKeyRaw, 16),
                        MyDSAPrivKey = list_to_integer(PrivateKeyRaw, 16),

                        file:close(GroupKeysFile),

                        {ok, GID, MyDSAPrivKey, ServerDSAPubKey};
                eof ->
                        lager:error("XDAA: Group keys file has no keys!"),
                        exit("XDAA: Group keys file has no keys!");
                {error, Reason} ->
                        lager:error("XDAA: Error reading line in group keys file: ~p", [Reason]),
                        exit("XDAA: Error reading line in group keys file")
        end.

