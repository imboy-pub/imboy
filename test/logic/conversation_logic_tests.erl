-module(conversation_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

cleanup_user(Uid) ->
    conversation_delete_repo:delete_by_user(Uid).

delete_c2c_conversation_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 2001,
        ConversationId = <<"gdwqa5">>,
        Type = <<"c2c">>,

        cleanup_user(Uid),
        ?assertEqual(ok, conversation_logic:delete(Uid, ConversationId, Type)),
        ?assertEqual(true, conversation_logic:is_deleted(Uid, ConversationId, Type)),
        cleanup_user(Uid)
    end).

delete_c2g_conversation_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 2002,
        ConversationId = <<"group123">>,
        Type = <<"c2g">>,

        cleanup_user(Uid),
        ?assertEqual(ok, conversation_logic:delete(Uid, ConversationId, Type)),
        ?assertEqual(true, conversation_logic:is_deleted(Uid, ConversationId, Type)),
        cleanup_user(Uid)
    end).

delete_nonexistent_conversation_is_idempotent_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 2003,
        ConversationId = <<"nonexistent">>,
        Type = <<"c2c">>,

        cleanup_user(Uid),
        ?assertEqual(ok, conversation_logic:delete(Uid, ConversationId, Type)),
        ?assertEqual(true, conversation_logic:is_deleted(Uid, ConversationId, Type)),
        cleanup_user(Uid)
    end).

restore_deleted_conversation_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 2004,
        ConversationId = <<"conv1">>,
        Type = <<"c2c">>,

        cleanup_user(Uid),
        ?assertEqual(ok, conversation_logic:delete(Uid, ConversationId, Type)),
        ?assertEqual(true, conversation_logic:is_deleted(Uid, ConversationId, Type)),
        ?assertEqual(ok, conversation_logic:restore(Uid, ConversationId, Type)),
        ?assertEqual(false, conversation_logic:is_deleted(Uid, ConversationId, Type)),
        cleanup_user(Uid)
    end).

filter_deleted_conversations_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 2005,

        cleanup_user(Uid),
        MsgList = [
            #{<<"from_id">> => 1001, <<"payload">> => <<"message1">>},
            #{<<"from_id">> => 1002, <<"payload">> => <<"message2">>},
            #{<<"from_id">> => 1003, <<"payload">> => <<"message3">>}
        ],

        ConversationId2 = integer_to_binary(1002),
        ?assertEqual(ok, conversation_logic:delete(Uid, ConversationId2, <<"c2c">>)),

        FilteredList = conversation_logic:filter_deleted_conversations(Uid, MsgList),
        ?assertEqual(2, length(FilteredList)),
        FromIds = [maps:get(<<"from_id">>, Msg) || Msg <- FilteredList],
        ?assertEqual(false, lists:member(1002, FromIds)),

        cleanup_user(Uid)
    end).

repeated_delete_is_idempotent_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 2006,
        ConversationId = <<"conv1">>,
        Type = <<"c2c">>,

        cleanup_user(Uid),
        ?assertEqual(ok, conversation_logic:delete(Uid, ConversationId, Type)),
        ?assertEqual(true, conversation_logic:is_deleted(Uid, ConversationId, Type)),
        ?assertEqual(ok, conversation_logic:delete(Uid, ConversationId, Type)),
        ?assertEqual(true, conversation_logic:is_deleted(Uid, ConversationId, Type)),
        cleanup_user(Uid)
    end).
