-module(conversation_pin_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% conversation_pin_ds 模块的 EUnit 测试
%%%
%%% 目标：验证会话置顶数据服务层功能
%%% 覆盖：置顶、取消置顶、查询置顶列表、检查是否置顶
%%%===================================================================

%% ===================================================================
%% pin_conversation/3 测试
%% ===================================================================

pin_conversation_success_test_() ->
    ?WITH_MECK(conversation_pin_repo, [
        {'pin', 3, fun(_Uid, _ConversationId, _Type) -> {ok, 1} end}
    ], fun() ->
        Uid = 12345,
        ConversationId = <<"conv123">>,
        Type = <<"c2c">>,

        Result = conversation_pin_ds:pin_conversation(Uid, ConversationId, Type),
        ?assertEqual(ok, Result)
    end).

pin_conversation_error_test_() ->
    ?WITH_MECK(conversation_pin_repo, [
        {'pin', 3, fun(_Uid, _ConversationId, _Type) ->
            {error, <<"duplicate key">>}
        end}
    ], fun() ->
        Uid = 12345,
        ConversationId = <<"conv123">>,
        Type = <<"c2c">>,

        Result = conversation_pin_ds:pin_conversation(Uid, ConversationId, Type),
        ?assertMatch({error, _}, Result)
    end).

%% ===================================================================
%% unpin_conversation/3 测试
%% ===================================================================

unpin_conversation_success_test_() ->
    ?WITH_MECK(conversation_pin_repo, [
        {'unpin', 3, fun(_Uid, _ConversationId, _Type) -> {ok, 1} end}
    ], fun() ->
        Uid = 12345,
        ConversationId = <<"conv123">>,
        Type = <<"c2c">>,

        Result = conversation_pin_ds:unpin_conversation(Uid, ConversationId, Type),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% get_pinned_conversations/1 测试
%% ===================================================================

get_pinned_conversations_success_test_() ->
    ?WITH_MECK(conversation_pin_repo, [
        {'list', 1, fun(_Uid) ->
            {ok, [
                #{
                    <<"conversation_id">> => <<"conv1">>,
                    <<"conversation_type">> => <<"c2c">>,
                    <<"pinned_at">> => <<"2026-02-16T12:00:00Z">>
                }
            ]}
        end}
    ], fun() ->
        Uid = 12345,

        Result = conversation_pin_ds:get_pinned_conversations(Uid),
        ?assertMatch({ok, [_]}, Result)
    end).

get_pinned_conversations_empty_test_() ->
    ?WITH_MECK(conversation_pin_repo, [
        {'list', 1, fun(_Uid) -> {ok, []} end}
    ], fun() ->
        Uid = 12345,

        Result = conversation_pin_ds:get_pinned_conversations(Uid),
        ?assertMatch({ok, []}, Result)
    end).

%% ===================================================================
%% is_conversation_pinned/3 测试
%% ===================================================================

is_conversation_pinned_true_test_() ->
    ?WITH_MECK(conversation_pin_repo, [
        {'is_pinned', 3, fun(_Uid, _ConversationId, _Type) -> true end}
    ], fun() ->
        Uid = 12345,
        ConversationId = <<"conv123">>,
        Type = <<"c2c">>,

        Result = conversation_pin_ds:is_conversation_pinned(Uid, ConversationId, Type),
        ?assertEqual(true, Result)
    end).

is_conversation_pinned_false_test_() ->
    ?WITH_MECK(conversation_pin_repo, [
        {'is_pinned', 3, fun(_Uid, _ConversationId, _Type) -> false end}
    ], fun() ->
        Uid = 12345,
        ConversationId = <<"conv456">>,
        Type = <<"c2c">>,

        Result = conversation_pin_ds:is_conversation_pinned(Uid, ConversationId, Type),
        ?assertEqual(false, Result)
    end).
