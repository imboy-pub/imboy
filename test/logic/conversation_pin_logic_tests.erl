-module(conversation_pin_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% conversation_pin_logic 模块的 EUnit 测试
%%%
%%% 目标：验证会话置顶业务逻辑层功能
%%% 覆盖：置顶会话、取消置顶、获取置顶列表、权限验证
%%%===================================================================

%% ===================================================================
%% pin/3 测试 - 单聊会话
%% ===================================================================

pin_c2c_conversation_success_test_() ->
    ?WITH_MECKS([{conversation_pin_ds, [
        {'is_conversation_pinned', 3, fun(_Uid, _ConversationId, _Type) -> false end},
        {'pin_conversation', 3, fun(_Uid, _ConversationId, _Type) -> ok end}
    ]}, {msg_s2c_ds, [
        {'send', 7, fun(_Uid, _To, _Action, _MsgId, _Ref, _Payload, _Mode) -> ok end}
    ]}], fun() ->
        Uid = 12345,
        ConversationId = 67890,
        Type = <<"c2c">>,

        Result = conversation_pin_logic:pin(Uid, ConversationId, Type),
        ?assertEqual(ok, Result)
    end).

pin_c2c_conversation_already_pinned_test_() ->
    ?WITH_MECK(conversation_pin_ds, [
        {'is_conversation_pinned', 3, fun(_Uid, _ConversationId, _Type) -> true end}
    ], fun() ->
        Uid = 12345,
        ConversationId = 67890,
        Type = <<"c2c">>,

        Result = conversation_pin_logic:pin(Uid, ConversationId, Type),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% pin/3 测试 - 群聊会话
%% ===================================================================

pin_c2g_conversation_success_test_() ->
    ?WITH_MECKS([{conversation_pin_ds, [
        {'is_conversation_pinned', 3, fun(_Uid, _ConversationId, _Type) -> false end},
        {'pin_conversation', 3, fun(_Uid, _ConversationId, _Type) -> ok end}
    ]}, {msg_s2c_ds, [
        {'send', 7, fun(_Uid, _To, _Action, _MsgId, _Ref, _Payload, _Mode) -> ok end}
    ]}], fun() ->
        Uid = 12345,
        ConversationId = 67890,
        Type = <<"c2g">>,

        Result = conversation_pin_logic:pin(Uid, ConversationId, Type),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% unpin/3 测试
%% ===================================================================

unpin_conversation_success_test_() ->
    ?WITH_MECKS([{conversation_pin_ds, [
        {'unpin_conversation', 3, fun(_Uid, _ConversationId, _Type) -> ok end}
    ]}, {msg_s2c_ds, [
        {'send', 7, fun(_Uid, _To, _Action, _MsgId, _Ref, _Payload, _Mode) -> ok end}
    ]}], fun() ->
        Uid = 12345,
        ConversationId = 67890,
        Type = <<"c2c">>,

        Result = conversation_pin_logic:unpin(Uid, ConversationId, Type),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% list/1 测试
%% ===================================================================

list_pinned_conversations_success_test_() ->
    ?WITH_MECK(conversation_pin_ds, [
        {'get_pinned_conversations', 1, fun(_Uid) ->
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

        Result = conversation_pin_logic:list(Uid),
        ?assertMatch({ok, _}, Result),
        {ok, List} = Result,
        ?assertEqual(2, length(List))
    end).

list_pinned_conversations_empty_test_() ->
    ?WITH_MECK(conversation_pin_ds, [
        {'get_pinned_conversations', 1, fun(_Uid) -> {ok, []} end}
    ], fun() ->
        Uid = 12345,

        Result = conversation_pin_logic:list(Uid),
        ?assertMatch({ok, []}, Result)
    end).

%% ===================================================================
%% is_pinned/3 测试
%% ===================================================================

is_pinned_true_test_() ->
    ?WITH_MECK(conversation_pin_ds, [
        {'is_conversation_pinned', 3, fun(_Uid, _ConversationId, _Type) -> true end}
    ], fun() ->
        Uid = 12345,
        ConversationId = <<"conv123">>,
        Type = <<"c2c">>,

        Result = conversation_pin_logic:is_pinned(Uid, ConversationId, Type),
        ?assertEqual(true, Result)
    end).

is_pinned_false_test_() ->
    ?WITH_MECK(conversation_pin_ds, [
        {'is_conversation_pinned', 3, fun(_Uid, _ConversationId, _Type) -> false end}
    ], fun() ->
        Uid = 12345,
        ConversationId = <<"conv456">>,
        Type = <<"c2c">>,

        Result = conversation_pin_logic:is_pinned(Uid, ConversationId, Type),
        ?assertEqual(false, Result)
    end).

%% ===================================================================
%% 参数验证测试
%% ===================================================================

pin_with_invalid_type_test_() ->
    fun() ->
        Uid = 12345,
        ConversationId = <<"conv123">>,
        Type = <<"invalid">>,

        Result = conversation_pin_logic:pin(Uid, ConversationId, Type),
        ?assertMatch({error, _}, Result)
    end.

pin_with_empty_conversation_id_test_() ->
    fun() ->
        Uid = 12345,
        ConversationId = 0,
        Type = <<"c2c">>,

        Result = conversation_pin_logic:pin(Uid, ConversationId, Type),
        ?assertMatch({error, _}, Result)
    end.
