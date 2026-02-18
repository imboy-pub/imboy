-module(conversation_logic_tests).
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Setup and Cleanup
%% ===================================================================

setup() ->
    {ok, _Pid} = eunit_runner:eunit_setup_with_db(),
    ok.

cleanup(_Pid) ->
    eunit_runner:eunit_cleanup({app_started, imboy}).

%% ===================================================================
%% Test Generators
%% ===================================================================

conversation_logic_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun test_delete_c2c_conversation/1,
      fun test_delete_c2g_conversation/1,
      fun test_delete_nonexistent_conversation/1,
      fun test_restore_deleted_conversation/1,
      fun test_filter_deleted_conversations/1,
      fun test_idempotent_delete/1
     ]}.

%% ===================================================================
%% Test Cases
%% ===================================================================

%% @doc 测试删除单聊会话
test_delete_c2c_conversation(_Pid) ->
    fun() ->
        Uid = 2001,
        ConversationId = <<"gdwqa5">>,  % hashids 编码的对方 UID
        Type = <<"c2c">>,

        % 清理测试数据
        conversation_delete_repo:delete_by_user(Uid),

        % 删除会话
        ?assertEqual(ok, conversation_logic:delete(Uid, ConversationId, Type)),

        % 验证会话已删除
        ?assertEqual(true, conversation_logic:is_deleted(Uid, ConversationId, Type)),

        % 清理
        conversation_delete_repo:delete_by_user(Uid)
    end.

%% @doc 测试删除群聊会话
test_delete_c2g_conversation(_Pid) ->
    fun() ->
        Uid = 2002,
        ConversationId = <<"group123">>,
        Type = <<"c2g">>,

        % 清理测试数据
        conversation_delete_repo:delete_by_user(Uid),

        % 删除会话
        ?assertEqual(ok, conversation_logic:delete(Uid, ConversationId, Type)),

        % 验证会话已删除
        ?assertEqual(true, conversation_logic:is_deleted(Uid, ConversationId, Type)),

        % 清理
        conversation_delete_repo:delete_by_user(Uid)
    end.

%% @doc 测试删除不存在的会话（幂等性）
test_delete_nonexistent_conversation(_Pid) ->
    fun() ->
        Uid = 2003,
        ConversationId = <<"nonexistent">>,
        Type = <<"c2c">>,

        % 清理测试数据
        conversation_delete_repo:delete_by_user(Uid),

        % 删除不存在的会话应该成功（幂等性）
        ?assertEqual(ok, conversation_logic:delete(Uid, ConversationId, Type)),

        % 验证会话已删除
        ?assertEqual(true, conversation_logic:is_deleted(Uid, ConversationId, Type)),

        % 清理
        conversation_delete_repo:delete_by_user(Uid)
    end.

%% @doc 测试恢复已删除的会话
test_restore_deleted_conversation(_Pid) ->
    fun() ->
        Uid = 2004,
        ConversationId = <<"conv1">>,
        Type = <<"c2c">>,

        % 清理测试数据
        conversation_delete_repo:delete_by_user(Uid),

        % 删除会话
        ?assertEqual(ok, conversation_logic:delete(Uid, ConversationId, Type)),
        ?assertEqual(true, conversation_logic:is_deleted(Uid, ConversationId, Type)),

        % 恢复会话
        ?assertEqual(ok, conversation_logic:restore(Uid, ConversationId, Type)),

        % 验证会话已恢复
        ?assertEqual(false, conversation_logic:is_deleted(Uid, ConversationId, Type)),

        % 清理
        conversation_delete_repo:delete_by_user(Uid)
    end.

%% @doc 测试过滤已删除的会话
test_filter_deleted_conversations(_Pid) ->
    fun() ->
        Uid = 2005,

        % 清理测试数据
        conversation_delete_repo:delete_by_user(Uid),

        % 构造消息列表
        MsgList = [
            #{<<"from_id">> => 1001, <<"payload">> => <<"message1">>},
            #{<<"from_id">> => 1002, <<"payload">> => <<"message2">>},
            #{<<"from_id">> => 1003, <<"payload">> => <<"message3">>}
        ],

        % 删除 from_id = 1002 的会话
        ConversationId2 = elib_hashids:encode(1002),
        ?assertEqual(ok, conversation_logic:delete(Uid, ConversationId2, <<"c2c">>)),

        % 过滤已删除的会话
        FilteredList = conversation_logic:filter_deleted_conversations(Uid, MsgList),

        % 验证结果：应该只有 2 条消息（1002 的消息被过滤）
        ?assertEqual(2, length(FilteredList)),

        % 验证过滤后的 from_id
        FromIds = [maps:get(<<"from_id">>, Msg) || Msg <- FilteredList],
        ?assertEqual(false, lists:member(1002, FromIds)),

        % 清理
        conversation_delete_repo:delete_by_user(Uid)
    end.

%% @doc 测试幂等性：重复删除同一会话应该成功
test_idempotent_delete(_Pid) ->
    fun() ->
        Uid = 2006,
        ConversationId = <<"conv1">>,
        Type = <<"c2c">>,

        % 清理测试数据
        conversation_delete_repo:delete_by_user(Uid),

        % 第一次删除
        ?assertEqual(ok, conversation_logic:delete(Uid, ConversationId, Type)),
        ?assertEqual(true, conversation_logic:is_deleted(Uid, ConversationId, Type)),

        % 第二次删除（幂等性）
        ?assertEqual(ok, conversation_logic:delete(Uid, ConversationId, Type)),

        % 仍然标记为已删除
        ?assertEqual(true, conversation_logic:is_deleted(Uid, ConversationId, Type)),

        % 清理
        conversation_delete_repo:delete_by_user(Uid)
    end.
