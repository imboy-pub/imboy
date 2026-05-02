-module(conversation_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% ===================================================================
%% Helper: track deleted conversations in process dictionary
%% ===================================================================

setup_mock_state() ->
    put(conversation_deleted_set, sets:new()),
    ok.

cleanup_mock_state() ->
    erase(conversation_deleted_set),
    ok.

is_mock_deleted(Uid, ConversationId, Type) ->
    Set = get(conversation_deleted_set),
    sets:is_element({Uid, ConversationId, Type}, Set).

mock_mark_deleted(Uid, ConversationId, Type) ->
    Set = get(conversation_deleted_set),
    put(conversation_deleted_set, sets:add_element({Uid, ConversationId, Type}, Set)),
    ok.

mock_restore(Uid, ConversationId, Type) ->
    Set = get(conversation_deleted_set),
    put(conversation_deleted_set, sets:del_element({Uid, ConversationId, Type}, Set)),
    ok.

mock_deleted_list(Uid) ->
    Set = get(conversation_deleted_set),
    Items = sets:to_list(Set),
    Filtered = [{CId, CType} || {U, CId, CType} <- Items, U =:= Uid],
    {ok, [#{<<"conversation_id">> => CId, <<"conversation_type">> => CType}
          || {CId, CType} <- Filtered]}.

%% ===================================================================
%% Test: delete a c2c conversation
%% ===================================================================

delete_c2c_conversation_test_() ->
    ?WITH_MECKS([
        {conversation_delete_ds, [
            {'is_conversation_deleted', 3, fun(_Uid, _CId, _Type) -> false end},
            {'delete_conversation', 3, fun(Uid, CId, Type) ->
                mock_mark_deleted(Uid, CId, Type),
                ok
            end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_From, _To, _Action, _MsgType, _E2EE, _Payload, _Save) -> ok end}
        ]}
    ], fun() ->
        setup_mock_state(),
        Uid = 2001,
        ConversationId = 90001,
        Type = <<"c2c">>,

        ?assertEqual(ok, conversation_logic:delete(Uid, ConversationId, Type)),
        ?assertEqual(true, is_mock_deleted(Uid, ConversationId, Type)),
        cleanup_mock_state()
    end).

%% ===================================================================
%% Test: delete a c2g conversation
%% ===================================================================

delete_c2g_conversation_test_() ->
    ?WITH_MECKS([
        {conversation_delete_ds, [
            {'is_conversation_deleted', 3, fun(_Uid, _CId, _Type) -> false end},
            {'delete_conversation', 3, fun(Uid, CId, Type) ->
                mock_mark_deleted(Uid, CId, Type),
                ok
            end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_From, _To, _Action, _MsgType, _E2EE, _Payload, _Save) -> ok end}
        ]}
    ], fun() ->
        setup_mock_state(),
        Uid = 2002,
        ConversationId = 90002,
        Type = <<"c2g">>,

        ?assertEqual(ok, conversation_logic:delete(Uid, ConversationId, Type)),
        ?assertEqual(true, is_mock_deleted(Uid, ConversationId, Type)),
        cleanup_mock_state()
    end).

%% ===================================================================
%% Test: delete nonexistent conversation is idempotent
%% ===================================================================

delete_nonexistent_conversation_is_idempotent_test_() ->
    ?WITH_MECKS([
        {conversation_delete_ds, [
            {'is_conversation_deleted', 3, fun(_Uid, _CId, _Type) -> false end},
            {'delete_conversation', 3, fun(Uid, CId, Type) ->
                mock_mark_deleted(Uid, CId, Type),
                ok
            end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_From, _To, _Action, _MsgType, _E2EE, _Payload, _Save) -> ok end}
        ]}
    ], fun() ->
        setup_mock_state(),
        Uid = 2003,
        ConversationId = 90003,
        Type = <<"c2c">>,

        ?assertEqual(ok, conversation_logic:delete(Uid, ConversationId, Type)),
        ?assertEqual(true, is_mock_deleted(Uid, ConversationId, Type)),
        cleanup_mock_state()
    end).

%% ===================================================================
%% Test: restore a deleted conversation
%% ===================================================================

restore_deleted_conversation_test_() ->
    ?WITH_MECKS([
        {conversation_delete_ds, [
            {'is_conversation_deleted', 3, fun(Uid, CId, Type) ->
                is_mock_deleted(Uid, CId, Type)
            end},
            {'delete_conversation', 3, fun(Uid, CId, Type) ->
                mock_mark_deleted(Uid, CId, Type),
                ok
            end},
            {'restore_conversation', 3, fun(Uid, CId, Type) ->
                mock_restore(Uid, CId, Type),
                ok
            end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_From, _To, _Action, _MsgType, _E2EE, _Payload, _Save) -> ok end}
        ]}
    ], fun() ->
        setup_mock_state(),
        Uid = 2004,
        ConversationId = 90004,
        Type = <<"c2c">>,

        ?assertEqual(ok, conversation_logic:delete(Uid, ConversationId, Type)),
        ?assertEqual(true, is_mock_deleted(Uid, ConversationId, Type)),
        ?assertEqual(ok, conversation_logic:restore(Uid, ConversationId, Type)),
        ?assertEqual(false, is_mock_deleted(Uid, ConversationId, Type)),
        cleanup_mock_state()
    end).

%% ===================================================================
%% Test: filter deleted conversations
%% ===================================================================

filter_deleted_conversations_test_() ->
    ?WITH_MECKS([
        {conversation_delete_ds, [
            {'is_conversation_deleted', 3, fun(_Uid, _CId, _Type) -> false end},
            {'delete_conversation', 3, fun(Uid, CId, Type) ->
                mock_mark_deleted(Uid, CId, Type),
                ok
            end},
            {'get_deleted_conversations', 1, fun(Uid) ->
                mock_deleted_list(Uid)
            end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_From, _To, _Action, _MsgType, _E2EE, _Payload, _Save) -> ok end}
        ]}
    ], fun() ->
        setup_mock_state(),
        Uid = 2005,

        MsgList = [
            #{<<"from_id">> => 1001, <<"payload">> => <<"message1">>},
            #{<<"from_id">> => 1002, <<"payload">> => <<"message2">>},
            #{<<"from_id">> => 1003, <<"payload">> => <<"message3">>}
        ],

        %% conversation_id must be integer (TSID bigint)
        ConversationId2 = 1002,
        ?assertEqual(ok, conversation_logic:delete(Uid, ConversationId2, <<"c2c">>)),

        FilteredList = conversation_logic:filter_deleted_conversations(Uid, MsgList),
        ?assertEqual(2, length(FilteredList)),
        FromIds = [maps:get(<<"from_id">>, Msg) || Msg <- FilteredList],
        ?assertEqual(false, lists:member(1002, FromIds)),

        cleanup_mock_state()
    end).

%% ===================================================================
%% Test: repeated delete is idempotent
%% ===================================================================

repeated_delete_is_idempotent_test_() ->
    ?WITH_MECKS([
        {conversation_delete_ds, [
            {'is_conversation_deleted', 3, fun(Uid, CId, Type) ->
                is_mock_deleted(Uid, CId, Type)
            end},
            {'delete_conversation', 3, fun(Uid, CId, Type) ->
                mock_mark_deleted(Uid, CId, Type),
                ok
            end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_From, _To, _Action, _MsgType, _E2EE, _Payload, _Save) -> ok end}
        ]}
    ], fun() ->
        setup_mock_state(),
        Uid = 2006,
        ConversationId = 90006,
        Type = <<"c2c">>,

        ?assertEqual(ok, conversation_logic:delete(Uid, ConversationId, Type)),
        ?assertEqual(true, is_mock_deleted(Uid, ConversationId, Type)),
        %% Second delete -- logic sees already deleted and returns ok (idempotent)
        ?assertEqual(ok, conversation_logic:delete(Uid, ConversationId, Type)),
        ?assertEqual(true, is_mock_deleted(Uid, ConversationId, Type)),
        cleanup_mock_state()
    end).
