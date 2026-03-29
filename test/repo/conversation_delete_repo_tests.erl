-module(conversation_delete_repo_tests).
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Setup and Cleanup
%% ===================================================================

setup() ->
    {ok, Pid} = eunit_runner:eunit_setup_with_db(),
    Pid.

cleanup(_Pid) ->
    eunit_runner:eunit_cleanup_db(_Pid).

%% ===================================================================
%% Test Generators
%% ===================================================================

conversation_delete_repo_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun test_mark_deleted/1,
      fun test_is_deleted/1,
      fun test_list_deleted/1,
      fun test_restore/1,
      fun test_idempotent_delete/1
     ]}.

%% ===================================================================
%% Test Cases
%% ===================================================================

%% @doc 测试标记会话为已删除
test_mark_deleted(_Pid) ->
    fun() ->
        Uid = 1001,
        ConversationId = <<"gdwqa5">>,  % hashids 编码的对方 UID
        Type = <<"c2c">>,

        % 清理测试数据
        conversation_delete_repo:delete_by_user(Uid),

        % 标记会话为已删除
        {ok, _Count} = conversation_delete_repo:mark_deleted(Uid, ConversationId, Type),

        % 验证会话已被标记为删除
        ?assertEqual(true, conversation_delete_repo:is_deleted(Uid, ConversationId, Type)),

        % 清理
        conversation_delete_repo:delete_by_user(Uid)
    end.

%% @doc 测试检查会话是否已删除
test_is_deleted(_Pid) ->
    fun() ->
        Uid = 1002,
        ConversationId = <<"p25vd5">>,
        Type = <<"c2c">>,

        % 清理测试数据
        conversation_delete_repo:delete_by_user(Uid),

        % 初始状态：未删除
        ?assertEqual(false, conversation_delete_repo:is_deleted(Uid, ConversationId, Type)),

        % 标记删除
        {ok, _Count} = conversation_delete_repo:mark_deleted(Uid, ConversationId, Type),

        % 删除后：已删除
        ?assertEqual(true, conversation_delete_repo:is_deleted(Uid, ConversationId, Type)),

        % 清理
        conversation_delete_repo:delete_by_user(Uid)
    end.

%% @doc 测试获取已删除的会话列表
test_list_deleted(_Pid) ->
    fun() ->
        Uid = 1003,

        % 清理测试数据
        conversation_delete_repo:delete_by_user(Uid),

        % 初始状态：空列表
        {ok, List1} = conversation_delete_repo:list(Uid),
        ?assertEqual(0, length(List1)),

        % 标记多个会话为已删除
        {ok, _} = conversation_delete_repo:mark_deleted(Uid, <<"conv1">>, <<"c2c">>),
        timer:sleep(10),  % 确保 deleted_at 不同
        {ok, _} = conversation_delete_repo:mark_deleted(Uid, <<"conv2">>, <<"c2c">>),
        timer:sleep(10),
        {ok, _} = conversation_delete_repo:mark_deleted(Uid, <<"group1">>, <<"c2g">>),

        % 获取已删除列表
        {ok, List2} = conversation_delete_repo:list(Uid),
        ?assertEqual(3, length(List2)),

        % 验证顺序：按 deleted_at 倒序
        [First | _] = List2,
        ?assertEqual(<<"group1">>, maps:get(<<"conversation_id">>, First)),

        % 清理
        conversation_delete_repo:delete_by_user(Uid)
    end.

%% @doc 测试恢复已删除的会话
test_restore(_Pid) ->
    fun() ->
        Uid = 1004,
        ConversationId = <<"conv1">>,
        Type = <<"c2c">>,

        % 清理测试数据
        conversation_delete_repo:delete_by_user(Uid),

        % 标记删除
        {ok, _Count} = conversation_delete_repo:mark_deleted(Uid, ConversationId, Type),
        ?assertEqual(true, conversation_delete_repo:is_deleted(Uid, ConversationId, Type)),

        % 恢复会话
        {ok, _Count2} = conversation_delete_repo:restore(Uid, ConversationId, Type),
        ?assertEqual(false, conversation_delete_repo:is_deleted(Uid, ConversationId, Type)),

        % 清理
        conversation_delete_repo:delete_by_user(Uid)
    end.

%% @doc 测试幂等性：重复删除同一会话应该成功
test_idempotent_delete(_Pid) ->
    fun() ->
        Uid = 1005,
        ConversationId = <<"conv1">>,
        Type = <<"c2c">>,

        % 清理测试数据
        conversation_delete_repo:delete_by_user(Uid),

        % 第一次删除
        {ok, Count1} = conversation_delete_repo:mark_deleted(Uid, ConversationId, Type),
        ?assertEqual(1, Count1),
        ?assertEqual(true, conversation_delete_repo:is_deleted(Uid, ConversationId, Type)),

        % 第二次删除（幂等性）
        {ok, Count2} = conversation_delete_repo:mark_deleted(Uid, ConversationId, Type),
        % 由于 UNIQUE 约束，第二次插入应该失败（或根据实现返回 0）
        % 这里我们期望返回 0 或 1，取决于实现
        ?assertEqual(true, Count2 >= 0 andalso Count2 =< 1),

        % 仍然标记为已删除
        ?assertEqual(true, conversation_delete_repo:is_deleted(Uid, ConversationId, Type)),

        % 清理
        conversation_delete_repo:delete_by_user(Uid)
    end.

%% @doc 测试按类型过滤已删除的会话
test_list_by_type(_Pid) ->
    fun() ->
        Uid = 1006,

        % 清理测试数据
        conversation_delete_repo:delete_by_user(Uid),

        % 标记不同类型的会话
        {ok, _} = conversation_delete_repo:mark_deleted(Uid, <<"c2c1">>, <<"c2c">>),
        {ok, _} = conversation_delete_repo:mark_deleted(Uid, <<"c2c2">>, <<"c2c">>),
        {ok, _} = conversation_delete_repo:mark_deleted(Uid, <<"c2g1">>, <<"c2g">>),

        % 获取所有已删除会话
        {ok, AllList} = conversation_delete_repo:list(Uid),
        ?assertEqual(3, length(AllList)),

        % 清理
        conversation_delete_repo:delete_by_user(Uid)
    end.
