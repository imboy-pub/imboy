#!/usr/bin/env escript
%%% -*- erlang -*-
%%%-------------------------------------------------------------------
%%% 商业冒烟的 RPC 助手 / RPC helper for the commercial smoke
%%%
%%% 只做「HTTP 打不到但属于本地环境准备」的动作，全部走运行中节点的真实
%%% 业务函数，不绕过任何校验：
%%%   token <uid> [did]  经 token_ds:encrypt_token/2 签发登录 token
%%%   license            输出 imboy_license:public_info/0（脱敏 7 字段）
%%%   user_count         user_ds:count/0
%%%   seed_plan <code>   经 billing_logic:create_plan/1 建套餐（已存在则复用）
%%%
%%% 节点与 cookie 经环境变量注入（与 scripts/imboy_ctl 同口径）：
%%%   IMBOY_CTL_NODE（默认 imboy_dev@127.0.0.1）
%%%   IMBOY_CTL_COOKIE（默认 imboycookie）
%%%-------------------------------------------------------------------

main(Args) ->
    Node = list_to_atom(os:getenv("IMBOY_CTL_NODE", "imboy_dev@127.0.0.1")),
    Cookie = list_to_atom(os:getenv("IMBOY_CTL_COOKIE", "imboycookie")),
    %% 目标节点用 IP 主机名，必须 longnames（与 scripts/imboy_ctl 同口径）
    Self = list_to_atom(
        "smoke_rpc_" ++ integer_to_list(erlang:unique_integer([positive])) ++ "@127.0.0.1"
    ),
    case net_kernel:start([Self, longnames]) of
        {ok, _} -> ok;
        {error, Why} -> die("ERROR: 无法启动分布式: ~p", [Why])
    end,
    erlang:set_cookie(node(), Cookie),
    case net_adm:ping(Node) of
        pong -> dispatch(Node, Args);
        pang -> die("ERROR: 节点不可达 ~p", [Node])
    end.

dispatch(Node, ["token", Uid]) ->
    dispatch(Node, ["token", Uid, ""]);
dispatch(Node, ["token", Uid, Did]) ->
    Token = rpc(Node, token_ds, encrypt_token, [
        list_to_integer(Uid), list_to_binary(Did)
    ]),
    io:format("~s~n", [Token]);
dispatch(Node, ["license"]) ->
    Info = rpc(Node, imboy_license, public_info, []),
    maps:foreach(fun(K, V) -> io:format("~s=~s~n", [K, fmt(V)]) end, Info);
dispatch(Node, ["user_count"]) ->
    io:format("~p~n", [rpc(Node, user_ds, count, [])]);
dispatch(Node, ["seed_plan", Code]) ->
    CodeBin = list_to_binary(Code),
    case rpc(Node, billing_logic, create_plan, [
        #{
            <<"code">> => CodeBin,
            <<"name">> => <<"冒烟套餐"/utf8>>,
            <<"price">> => 100,
            <<"billing_period">> => <<"month">>,
            <<"quota_config">> => #{<<"api_call">> => 1000},
            <<"description">> => <<"commercial smoke fixture"/utf8>>
        }
    ]) of
        {ok, Id} ->
            io:format("~p~n", [Id]);
        {error, Msg} ->
            %% 已存在时复用（冒烟可重复执行）
            case find_plan(Node, CodeBin) of
                0 -> die("ERROR: 建套餐失败且未找到同 code 套餐: ~s", [fmt(Msg)]);
                Id -> io:format("~p~n", [Id])
            end
    end;
dispatch(_Node, Args) ->
    die("用法: commercial_rpc.escript token <uid> [did] | license | user_count | seed_plan <code>~n实参: ~p", [
        Args
    ]).

find_plan(Node, CodeBin) ->
    Plans = rpc(Node, billing_logic, list_plans, []),
    case [P || P <- Plans, maps:get(<<"code">>, P, <<>>) =:= CodeBin] of
        [P | _] -> to_int(maps:get(<<"id">>, P, 0));
        [] -> 0
    end.

to_int(I) when is_integer(I) -> I;
to_int(B) when is_binary(B) -> binary_to_integer(B);
to_int(_) -> 0.

rpc(Node, M, F, A) ->
    case rpc:call(Node, M, F, A, 20000) of
        {badrpc, Reason} -> die("ERROR: rpc ~p:~p 失败: ~p", [M, F, Reason]);
        R -> R
    end.

fmt(V) when is_binary(V) -> V;
fmt(V) when is_atom(V) -> atom_to_list(V);
fmt(V) -> io_lib:format("~p", [V]).

die(Fmt, Args) ->
    io:format(standard_error, Fmt ++ "~n", Args),
    halt(1).
