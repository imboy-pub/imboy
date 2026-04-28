#!/usr/bin/env escript
%%% -*- erlang -*-
%%%
%%% mint_token.escript <uid>
%%%
%%% 通过 RPC 调 imboy@127.0.0.1 节点的 token_ds:encrypt_token/1 产出 JWT。
%%% 前置：imboy@127.0.0.1 节点已启动，cookie=imboy。
%%% 输出：仅一行，token 字符串本身（方便 shell 用 $() 捕获）。

-define(NODE, 'imboy@127.0.0.1').

main([UidStr]) ->
    {ok, _} = net_kernel:start(['probe_mint@127.0.0.1', longnames]),
    erlang:set_cookie(node(), imboy),
    pong = case net_adm:ping(?NODE) of
               pong -> pong;
               pang ->
                   io:format(standard_error,
                             "ERROR: cannot reach ~p (is imboy node up?)~n",
                             [?NODE]),
                   halt(1)
           end,
    Uid = list_to_integer(UidStr),
    case rpc:call(?NODE, token_ds, encrypt_token, [Uid]) of
        {badrpc, Reason} ->
            io:format(standard_error,
                      "ERROR: rpc failed ~p~n", [Reason]),
            halt(1);
        Token when is_binary(Token); is_list(Token) ->
            io:format("~s~n", [Token]),
            halt(0);
        Other ->
            io:format(standard_error,
                      "ERROR: unexpected return ~p~n", [Other]),
            halt(1)
    end;
main(_) ->
    io:format(standard_error,
              "usage: mint_token.escript <uid>~n", []),
    halt(2).
