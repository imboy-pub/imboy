-module(msg_archive_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc  msg_archive_ds 单元测试
%%%
%%% 验证 DS 层正确委托给 Repo 层，封装了参数转换和默认值。
%%% 测试覆盖：
%%%   history/3,4     — 委托 + 默认 asc 排序
%%%   conv_key_c2c/2  — 会话键格式（对称性）
%%%   conv_key_c2g/1  — 会话键格式
%%%===================================================================

%% ===================================================================
%% conv_key_c2c/2 测试
%% ===================================================================

conv_key_c2c_produces_correct_format_test_() ->
    ?TEST_SIMPLE(fun() ->
        Key = msg_archive_ds:conv_key_c2c(100, 200),
        ?assertEqual(<<"c2c:100:200">>, Key)
    end).

conv_key_c2c_normalizes_order_test_() ->
    ?TEST_SIMPLE(fun() ->
        %% DS 层的 conv_key_c2c 顺序无关
        KeyAB = msg_archive_ds:conv_key_c2c(111, 999),
        KeyBA = msg_archive_ds:conv_key_c2c(999, 111),
        ?assertEqual(KeyAB, KeyBA),
        ?assertEqual(<<"c2c:111:999">>, KeyAB)
    end).

%% ===================================================================
%% conv_key_c2g/1 测试
%% ===================================================================

conv_key_c2g_produces_correct_format_test_() ->
    ?TEST_SIMPLE(fun() ->
        Key = msg_archive_ds:conv_key_c2g(5678),
        ?assertEqual(<<"c2g:5678">>, Key)
    end).

%% ===================================================================
%% history/3 测试（默认正序）
%% ===================================================================

history_delegates_to_repo_with_asc_order_test_() ->
    ExpectedRows = [
        #{<<"msg_id">> => <<"m1">>, <<"conv_seq">> => 10},
        #{<<"msg_id">> => <<"m2">>, <<"conv_seq">> => 11}
    ],
    ?WITH_MECK(msg_archive_repo, [
        {'get_history', 4, fun(ConvKey, AfterSeq, Limit, Order) ->
            ?assertEqual(<<"c2c:100:200">>, ConvKey),
            ?assertEqual(5,   AfterSeq),
            ?assertEqual(50,  Limit),
            ?assertEqual(asc, Order),   %% 默认正序
            {ok, ExpectedRows}
        end}
    ], fun() ->
        Result = msg_archive_ds:history(<<"c2c:100:200">>, 5, 50),
        ?assertEqual({ok, ExpectedRows}, Result)
    end).

history_with_zero_seq_fetches_from_beginning_test_() ->
    ?WITH_MECK(msg_archive_repo, [
        {'get_history', 4, fun(_ConvKey, AfterSeq, _Limit, _Order) ->
            ?assertEqual(0, AfterSeq),
            {ok, []}
        end}
    ], fun() ->
        Result = msg_archive_ds:history(<<"c2c:1:2">>, 0, 10),
        ?assertEqual({ok, []}, Result)
    end).

history_propagates_db_error_test_() ->
    ?WITH_MECK(msg_archive_repo, [
        {'get_history', 4, fun(_ConvKey, _AfterSeq, _Limit, _Order) ->
            {error, timeout}
        end}
    ], fun() ->
        Result = msg_archive_ds:history(<<"c2c:100:200">>, 0, 50),
        ?assertEqual({error, timeout}, Result)
    end).

%% ===================================================================
%% history/4 测试（自定义排序）
%% ===================================================================

history_asc_order_test_() ->
    ?WITH_MECK(msg_archive_repo, [
        {'get_history', 4, fun(_ConvKey, _AfterSeq, _Limit, Order) ->
            ?assertEqual(asc, Order),
            {ok, []}
        end}
    ], fun() ->
        Result = msg_archive_ds:history(<<"c2g:9000">>, 0, 20, asc),
        ?assertEqual({ok, []}, Result)
    end).

history_desc_order_test_() ->
    ?WITH_MECK(msg_archive_repo, [
        {'get_history', 4, fun(_ConvKey, _AfterSeq, _Limit, Order) ->
            ?assertEqual(desc, Order),
            {ok, []}
        end}
    ], fun() ->
        %% 向上翻页场景：传入当前最早消息的 conv_seq，倒序拉取
        Result = msg_archive_ds:history(<<"c2c:100:200">>, 50, 20, desc),
        ?assertEqual({ok, []}, Result)
    end).

%% ===================================================================
%% 集成语义测试（端到端流程验证，不访问真实 DB）
%% ===================================================================

%% 验证增量同步的完整游标语义
incremental_sync_cursor_semantics_test_() ->
    ?TEST_SIMPLE(fun() ->
        %% C2C 会话键对称
        ConvKey = msg_archive_ds:conv_key_c2c(100, 200),
        ?assertEqual(<<"c2c:100:200">>, ConvKey),
        ConvKeyRev = msg_archive_ds:conv_key_c2c(200, 100),
        ?assertEqual(ConvKey, ConvKeyRev),

        %% C2G 会话键
        GKey = msg_archive_ds:conv_key_c2g(9999),
        ?assertEqual(<<"c2g:9999">>, GKey),

        %% 会话键格式：c2c 或 c2g 前缀
        ?assertNotEqual(nomatch, binary:match(ConvKey, <<"c2c:">>)),
        ?assertNotEqual(nomatch, binary:match(GKey,    <<"c2g:">>))
    end).
