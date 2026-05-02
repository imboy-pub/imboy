#!/usr/bin/env escript
%%% -*- erlang -*-
%%%
%%% ensure_test_user.escript <uid> <account> <nickname> <password>
%%%
%%% 确保测试用户存在（不存在则创建），并 mint JWT token。
%%% 前置：imboy@127.0.0.1 节点已启动，cookie=imboy。
%%%
%%% 输出（stdout，每行一个 key=value）：
%%%   UID=<uid>
%%%   TOKEN=<jwt>
%%%   CREATED=true|false
%%%
%%% 退出码：0 成功，1 失败

-define(NODE, 'imboy@127.0.0.1').

main([UidStr, Account, Nickname, Password]) ->
    {ok, _} = net_kernel:start(['ensure_user@127.0.0.1', longnames]),
    erlang:set_cookie(node(), imboy),
    pong = case net_adm:ping(?NODE) of
               pong -> pong;
               pang ->
                   io:format(standard_error,
                             "ERROR: cannot reach ~p~n", [?NODE]),
                   halt(1)
           end,
    Uid = list_to_integer(UidStr),

    %% 检查用户是否已存在
    Exists = case rpc:call(?NODE, user_repo, detail, [Uid]) of
                 {ok, _} -> true;
                 _ -> false
             end,

    Created =
        case Exists of
            true ->
                io:format(standard_error, "用户 ~s (uid=~p) 已存在，跳过创建~n", [Account, Uid]),
                false;
            false ->
                %% 生成密码哈希
                PwdHash = rpc:call(?NODE, elib_password, generate, [list_to_binary(Password)]),
                %% 构造用户数据并插入
                Now = rpc:call(?NODE, elib_dt, now, []),
                UserData = #{
                    <<"id">> => Uid,
                    <<"account">> => list_to_binary(Account),
                    <<"nickname">> => list_to_binary(Nickname),
                    <<"password">> => PwdHash,
                    <<"mobile">> => list_to_binary(Account),
                    <<"email">> => <<>>,
                    <<"region">> => <<>>,
                    <<"avatar">> => <<>>,
                    <<"sign">> => <<>>,
                    <<"gender">> => 0,
                    <<"status">> => 1,
                    <<"created_at">> => Now,
                    <<"reg_ip">> => <<"127.0.0.1">>,
                    <<"reg_cosv">> => <<"integration-test">>,
                    <<"level_id">> => 1,
                    <<"experience">> => 0,
                    <<"ref_user_id">> => 0,
                    <<"ref_parent_user_id">> => 0,
                    <<"source">> => <<"test">>
                },
                case rpc:call(?NODE, user_repo, save, [UserData]) of
                    {ok, Id} ->
                        io:format(standard_error, "创建用户 ~s (uid=~p) 成功~n", [Account, Id]),
                        true;
                    {error, CreateErr} ->
                        io:format(standard_error, "ERROR: 创建用户失败 ~p~n", [CreateErr]),
                        halt(1)
                end
        end,

    %% Mint JWT token
    case rpc:call(?NODE, token_ds, encrypt_token, [Uid]) of
        {badrpc, Reason} ->
            io:format(standard_error, "ERROR: mint token rpc failed ~p~n", [Reason]),
            halt(1);
        Token when is_binary(Token); is_list(Token) ->
            io:format("UID=~p~n", [Uid]),
            io:format("TOKEN=~s~n", [Token]),
            io:format("CREATED=~p~n", [Created]),
            halt(0);
        Other ->
            io:format(standard_error, "ERROR: unexpected return ~p~n", [Other]),
            halt(1)
    end;

main(_) ->
    io:format(standard_error,
              "usage: ensure_test_user.escript <uid> <account> <nickname> <password>~n", []),
    halt(2).
