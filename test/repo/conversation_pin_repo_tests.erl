-module(conversation_pin_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% conversation_pin_repo 模块的 EUnit 测试
%%%
%%% 目标：验证会话置顶数据访问层功能
%%% 覆盖：置顶、取消置顶、查询置顶列表、检查是否置顶
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_returns_correct_table_test_() ->
    ?WITH_MECK(elib_pg_sql, [
        {'public_tablename', 1, fun(_Table) -> <<"public.conversation_pin">> end}
    ], fun() ->
        Result = conversation_pin_repo:tablename(),
        ?assertEqual(<<"public.conversation_pin">>, Result)
    end).

%% ===================================================================
%% pin/3 测试
%% ===================================================================

pin_conversation_success_test_() ->
    ?WITH_MECK(elib_pg, [
        {'execute', 2, fun(_Sql, _Params) -> {ok, 1} end}
    ], fun() ->
        Uid = 12345,
        ConversationId = <<"conv123">>,
        Type = <<"c2c">>,

        Result = conversation_pin_repo:pin(Uid, ConversationId, Type),
        ?assertMatch({ok, 1}, Result)
    end).

pin_conversation_duplicate_test_() ->
    ?WITH_MECK(elib_pg, [
        {'execute', 2, fun(_Sql, _Params) ->
            % 模拟唯一性约束违反
            {error, #{
                <<"code">> => <<"23505">>,
                <<"message">> => <<"duplicate key value violates unique constraint">>
            }}
        end}
    ], fun() ->
        Uid = 12345,
        ConversationId = <<"conv123">>,
        Type = <<"c2c">>,

        Result = conversation_pin_repo:pin(Uid, ConversationId, Type),
        ?assertMatch({error, _}, Result)
    end).

%% ===================================================================
%% unpin/3 测试
%% ===================================================================

unpin_conversation_success_test_() ->
    ?WITH_MECK(elib_pg, [
        {'execute', 2, fun(_Sql, _Params) -> {ok, 1} end}
    ], fun() ->
        Uid = 12345,
        ConversationId = <<"conv123">>,
        Type = <<"c2c">>,

        Result = conversation_pin_repo:unpin(Uid, ConversationId, Type),
        ?assertMatch({ok, 1}, Result)
    end).

unpin_conversation_not_found_test_() ->
    ?WITH_MECK(elib_pg, [
        {'execute', 2, fun(_Sql, _Params) -> {ok, 0} end}
    ], fun() ->
        Uid = 12345,
        ConversationId = <<"conv456">>,
        Type = <<"c2c">>,

        Result = conversation_pin_repo:unpin(Uid, ConversationId, Type),
        ?assertMatch({ok, 0}, Result)
    end).

%% ===================================================================
%% is_pinned/3 测试
%% ===================================================================

is_pinned_true_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, [#{<<"count">> => 1}]}
        end}
    ], fun() ->
        Uid = 12345,
        ConversationId = <<"conv123">>,
        Type = <<"c2c">>,

        Result = conversation_pin_repo:is_pinned(Uid, ConversationId, Type),
        ?assertEqual(true, Result)
    end).

is_pinned_false_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, [#{<<"count">> => 0}]}
        end}
    ], fun() ->
        Uid = 12345,
        ConversationId = <<"conv456">>,
        Type = <<"c2c">>,

        Result = conversation_pin_repo:is_pinned(Uid, ConversationId, Type),
        ?assertEqual(false, Result)
    end).

is_pinned_database_error_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {error, database_error}
        end}
    ], fun() ->
        Uid = 12345,
        ConversationId = <<"conv789">>,
        Type = <<"c2c">>,

        Result = conversation_pin_repo:is_pinned(Uid, ConversationId, Type),
        ?assertEqual(false, Result)
    end).

%% ===================================================================
%% list/1 测试
%% ===================================================================

list_pinned_conversations_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, [
                #{
                    <<"conversation_id">> => <<"conv1">>,
                    <<"conversation_type">> => <<"c2c">>,
                    <<"pinned_at">> => <<"2026-02-16T12:00:00Z">>
                },
                #{
                    <<"conversation_id">> => <<"conv2">>,
                    <<"conversation_type">> => <<"c2g">>,
                    <<"pinned_at">> => <<"2026-02-16T11:30:00Z">>
                }
            ]}
        end}
    ], fun() ->
        Uid = 12345,

        Result = conversation_pin_repo:list(Uid),
        ?assertMatch({ok, _}, Result),
        {ok, List} = Result,
        ?assertEqual(2, length(List))
    end).

list_pinned_conversations_empty_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, []}
        end}
    ], fun() ->
        Uid = 12345,

        Result = conversation_pin_repo:list(Uid),
        ?assertMatch({ok, []}, Result)
    end).

list_pinned_conversations_error_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {error, database_error}
        end}
    ], fun() ->
        Uid = 12345,

        Result = conversation_pin_repo:list(Uid),
        ?assertMatch({error, _}, Result)
    end).

%% ===================================================================
%% delete_by_user/1 测试
%% ===================================================================

delete_by_user_success_test_() ->
    ?WITH_MECK(elib_pg, [
        {'execute', 2, fun(_Sql, _Params) -> {ok, 3} end}
    ], fun() ->
        Uid = 12345,

        Result = conversation_pin_repo:delete_by_user(Uid),
        ?assertMatch({ok, 3}, Result)
    end).

delete_by_user_no_records_test_() ->
    ?WITH_MECK(elib_pg, [
        {'execute', 2, fun(_Sql, _Params) -> {ok, 0} end}
    ], fun() ->
        Uid = 99999,

        Result = conversation_pin_repo:delete_by_user(Uid),
        ?assertMatch({ok, 0}, Result)
    end).
