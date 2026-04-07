#!/usr/bin/env escript
%% -*- erlang -*-
%%
%% TSID 运行时验证脚本
%% 用法: 在 imboy 远程 console 中执行:
%%   tsid_runtime_verify:run().
%%
%% 或复制各测试函数独立执行
%%

-module(tsid_runtime_verify).
-export([run/0]).

run() ->
    io:format("~n=== TSID Runtime Verification ===~n~n"),

    %% 1. 验证 TSID 生成器已初始化
    io:format("[1] TSID Generator Status~n"),
    Registered = elib_tsid:registered(),
    io:format("    Registered generators: ~p~n", [length(Registered)]),
    case length(Registered) >= 80 of
        true -> io:format("    ✓ All generators registered~n");
        false -> io:format("    ✗ Expected >= 80 generators, got ~p~n", [length(Registered)])
    end,

    %% 2. 验证关键表的 TSID 生成
    io:format("~n[2] Key Table TSID Generation~n"),
    Tables = [user, user_setting, user_friend, group, group_member,
              msg_c2c, msg_c2g, msg_s2c, channel, feedback],
    lists:foreach(fun(T) ->
        Id = elib_tsid:generate(T),
        Digits = length(integer_to_list(Id)),
        Status = case Id > 0 andalso Digits =< 19 of
            true -> "✓";
            false -> "✗"
        end,
        io:format("    ~s ~-20s => ~p (~p digits)~n", [Status, T, Id, Digits])
    end, Tables),

    %% 3. 验证 id_to_binary 转换
    io:format("~n[3] id_to_binary Conversion~n"),
    TestId = elib_tsid:generate(user),
    Bin = elib_cnv:id_to_binary(TestId),
    case is_binary(Bin) andalso Bin =:= integer_to_binary(TestId) of
        true -> io:format("    ✓ id_to_binary(~p) => ~p~n", [TestId, Bin]);
        false -> io:format("    ✗ id_to_binary failed for ~p~n", [TestId])
    end,
    %% 边界值
    case elib_cnv:id_to_binary(0) of
        <<"0">> -> io:format("    ✓ id_to_binary(0) => <<\"0\">>~n");
        Other -> io:format("    ✗ id_to_binary(0) => ~p~n", [Other])
    end,
    case elib_cnv:id_to_binary(<<"already_binary">>) of
        <<"already_binary">> -> io:format("    ✓ id_to_binary(binary) passthrough~n");
        Other2 -> io:format("    ✗ id_to_binary(binary) => ~p~n", [Other2])
    end,

    %% 4. 验证旧 BIGSERIAL ID 兼容性 (小整数)
    io:format("~n[4] Old BIGSERIAL Compatibility~n"),
    OldIds = [1, 42, 1000, 99999],
    lists:foreach(fun(OldId) ->
        Bin2 = elib_cnv:id_to_binary(OldId),
        Int = ec_cnv:to_integer(Bin2),
        case Int =:= OldId of
            true -> io:format("    ✓ Old ID ~p => binary ~p => integer ~p~n", [OldId, Bin2, Int]);
            false -> io:format("    ✗ Roundtrip failed for ~p~n", [OldId])
        end
    end, OldIds),

    %% 5. 验证并发安全性
    io:format("~n[5] Concurrent Safety~n"),
    Parent = self(),
    _ = [spawn(fun() ->
        Ids = [elib_tsid:generate(user) || _ <- lists:seq(1, 100)],
        Parent ! {ids, Ids}
    end) || _ <- lists:seq(1, 10)],
    AllIds = lists:flatten([receive {ids, R} -> R end || _ <- lists:seq(1, 10)]),
    UniqueCount = length(lists:usort(AllIds)),
    TotalCount = length(AllIds),
    case UniqueCount =:= TotalCount of
        true -> io:format("    ✓ ~p/~p unique (concurrent 10 procs × 100 ids)~n", [UniqueCount, TotalCount]);
        false -> io:format("    ✗ ~p/~p unique — COLLISION DETECTED~n", [UniqueCount, TotalCount])
    end,

    io:format("~n=== Verification Complete ===~n~n"),
    ok.
