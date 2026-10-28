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
    {ok, [
        #{<<"conversation_id">> => CId, <<"conversation_type">> => CType}
     || {CId, CType} <- Filtered
    ]}.

%% W09.c（J17.e）后 logic 走 elib_pg:with_tx + *_tx 事务内契约：mock
%% with_tx 直接执行闭包（abort_tx 折叠为 {error, Reason}，与真实现回滚
%% 语义对齐），mock 事务内读写的 _tx 函数与版本推进。
tx_mocks(IsDeletedFun) ->
    [
        {elib_pg, [
            {'with_tx', 1, fun(Transaction) ->
                try
                    Transaction(self())
                catch
                    throw:{abort_tx, Reason} -> {error, Reason}
                end
            end}
        ]},
        {conversation_delete_ds, [
            {'is_conversation_deleted_tx', 4, fun(_Conn, Uid, CId, Type) ->
                IsDeletedFun(Uid, CId, Type)
            end},
            {'mark_deleted_tx', 4, fun(_Conn, Uid, CId, Type) ->
                mock_mark_deleted(Uid, CId, Type),
                ok
            end}
        ]},
        {user_setting_ds, [
            {'lock_preferences_tx', 2, fun(_Conn, _Uid) -> ok end},
            {'bump_conversation_version_tx', 3, fun(_Conn, _Uid, _Uk3) ->
                {ok, #{<<"version">> => 1}}
            end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_From, _To, _Action, _MsgType, _E2EE, _Payload, _Save) ->
                ok
            end}
        ]}
    ].

%% ===================================================================
%% Test: delete a c2c conversation
%% ===================================================================

delete_c2c_conversation_test_() ->
    ?WITH_MECKS(tx_mocks(fun(_Uid, _CId, _Type) -> false end), fun() ->
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
    ?WITH_MECKS(tx_mocks(fun(_Uid, _CId, _Type) -> false end), fun() ->
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
    ?WITH_MECKS(tx_mocks(fun(_Uid, _CId, _Type) -> false end), fun() ->
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
    ?WITH_MECKS(
        tx_mocks(fun(Uid, CId, Type) -> is_mock_deleted(Uid, CId, Type) end),
        fun() ->
            meck:expect(
                conversation_delete_ds,
                restore_conversation_tx,
                fun(_Conn, Uid, CId, Type) ->
                    mock_restore(Uid, CId, Type),
                    {ok, 1}
                end
            ),
            setup_mock_state(),
            Uid = 2004,
            ConversationId = 90004,
            Type = <<"c2c">>,

            ?assertEqual(ok, conversation_logic:delete(Uid, ConversationId, Type)),
            ?assertEqual(true, is_mock_deleted(Uid, ConversationId, Type)),
            ?assertEqual(ok, conversation_logic:restore(Uid, ConversationId, Type)),
            ?assertEqual(false, is_mock_deleted(Uid, ConversationId, Type)),
            cleanup_mock_state()
        end
    ).

%% ===================================================================
%% Test: filter deleted conversations
%% ===================================================================

filter_deleted_conversations_test_() ->
    ?WITH_MECKS(tx_mocks(fun(_Uid, _CId, _Type) -> false end), fun() ->
        setup_mock_state(),
        meck:expect(
            conversation_delete_ds,
            get_deleted_conversations,
            1,
            fun(Uid) -> mock_deleted_list(Uid) end
        ),
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
    ?WITH_MECKS(
        tx_mocks(fun(Uid, CId, Type) -> is_mock_deleted(Uid, CId, Type) end),
        fun() ->
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
        end
    ).
