-module(msg_reaction_repo_tests).
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% 测试固具
%% ===================================================================

%% 在测试开始前设置
setup() ->
    {ok, _} = application:ensure_all_started(imboy),
    elib_pg:query(<<"DELETE FROM msg_reaction WHERE user_id = $1">>, [999999]),
    ok.

%% 在测试结束后清理
cleanup(_) ->
    elib_pg:query(<<"DELETE FROM msg_reaction WHERE user_id = $1">>, [999999]),
    ok.

%% ===================================================================
%% 测试用例
%% ===================================================================

msg_reaction_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun test_add_reaction/0,
      fun test_add_duplicate_reaction/0,
      fun test_remove_reaction/0,
      fun test_find_by_msg/0,
      fun test_find_by_msg_emoji/0,
      fun test_count_by_msg/0,
      fun test_count_by_emoji/0,
      fun test_find_user_reactions/0,
      fun test_remove_all_by_msg/0,
      fun test_add_multiple_reactions/0,
      fun test_edge_cases/0
     ]}.

%% @doc 测试添加表情回应
test_add_reaction() ->
    MsgId = <<"test_msg_001">>,
    MsgType = <<"c2c">>,
    UserId = 999999,
    Emoji = <<"👍"/utf8>>,

    % 添加表情
    ok = msg_reaction_repo:add(MsgId, MsgType, UserId, Emoji),

    % 验证表情已添加
    {ok, Reactions} = msg_reaction_repo:find_by_msg(MsgId, MsgType),
    ?assertEqual(1, length(Reactions)),
    [Reaction] = Reactions,
    ?assertEqual(MsgId, maps:get(<<"msg_id">>, Reaction)),
    ?assertEqual(MsgType, maps:get(<<"msg_type">>, Reaction)),
    ?assertEqual(UserId, maps:get(<<"user_id">>, Reaction)),
    ?assertEqual(Emoji, maps:get(<<"emoji">>, Reaction)).

%% @doc 测试重复添加相同表情（应该幂等）
test_add_duplicate_reaction() ->
    MsgId = <<"test_msg_002">>,
    MsgType = <<"c2c">>,
    UserId = 999999,
    Emoji = <<"❤️"/utf8>>,

    % 添加两次相同的表情
    ok = msg_reaction_repo:add(MsgId, MsgType, UserId, Emoji),
    ok = msg_reaction_repo:add(MsgId, MsgType, UserId, Emoji),

    % 验证只有一个记录
    {ok, Reactions} = msg_reaction_repo:find_by_msg(MsgId, MsgType),
    ?assertEqual(1, length(Reactions)).

%% @doc 测试移除表情
test_remove_reaction() ->
    MsgId = <<"test_msg_003">>,
    MsgType = <<"c2c">>,
    UserId = 999999,
    Emoji = <<"😄"/utf8>>,

    % 添加表情
    ok = msg_reaction_repo:add(MsgId, MsgType, UserId, Emoji),

    % 移除表情
    ok = msg_reaction_repo:remove(MsgId, MsgType, UserId, Emoji),

    % 验证表情已移除
    {ok, Reactions} = msg_reaction_repo:find_by_msg(MsgId, MsgType),
    ?assertEqual(0, length(Reactions)).

%% @doc 测试查询消息的所有表情
test_find_by_msg() ->
    MsgId = <<"test_msg_004">>,
    MsgType = <<"c2g">>,

    % 添加多个表情
    ok = msg_reaction_repo:add(MsgId, MsgType, 999999, <<"👍"/utf8>>),
    ok = msg_reaction_repo:add(MsgId, MsgType, 999998, <<"❤️"/utf8>>),
    ok = msg_reaction_repo:add(MsgId, MsgType, 999999, <<"😄"/utf8>>),

    % 查询所有表情
    {ok, Reactions} = msg_reaction_repo:find_by_msg(MsgId, MsgType),
    ?assertEqual(3, length(Reactions)).

%% @doc 测试查询消息的特定emoji回应
test_find_by_msg_emoji() ->
    MsgId = <<"test_msg_005">>,
    MsgType = <<"c2c">>,

    % 添加多个相同emoji
    ok = msg_reaction_repo:add(MsgId, MsgType, 999999, <<"👍"/utf8>>),
    ok = msg_reaction_repo:add(MsgId, MsgType, 999998, <<"👍"/utf8>>),
    ok = msg_reaction_repo:add(MsgId, MsgType, 999997, <<"❤️"/utf8>>),

    % 查询特定emoji
    {ok, Reactions} = msg_reaction_repo:find_by_msg_emoji(MsgId, MsgType, <<"👍"/utf8>>),
    ?assertEqual(2, length(Reactions)).

%% @doc 测试统计消息表情总数
test_count_by_msg() ->
    MsgId = <<"test_msg_006">>,
    MsgType = <<"c2g">>,

    % 添加多个表情
    ok = msg_reaction_repo:add(MsgId, MsgType, 999999, <<"👍"/utf8>>),
    ok = msg_reaction_repo:add(MsgId, MsgType, 999998, <<"👍"/utf8>>),
    ok = msg_reaction_repo:add(MsgId, MsgType, 999997, <<"❤️"/utf8>>),

    % 统计总数
    Count = msg_reaction_repo:count_by_msg(MsgId, MsgType),
    ?assertEqual(3, Count).

%% @doc 测试统计特定emoji数量
test_count_by_emoji() ->
    MsgId = <<"test_msg_007">>,
    MsgType = <<"c2c">>,

    % 添加多个表情
    ok = msg_reaction_repo:add(MsgId, MsgType, 999999, <<"👍"/utf8>>),
    ok = msg_reaction_repo:add(MsgId, MsgType, 999998, <<"👍"/utf8>>),
    ok = msg_reaction_repo:add(MsgId, MsgType, 999997, <<"❤️"/utf8>>),

    % 统计特定emoji
    Count = msg_reaction_repo:count_by_emoji(MsgId, MsgType, <<"👍"/utf8>>),
    ?assertEqual(2, Count).

%% @doc 测试查询用户的表情历史
test_find_user_reactions() ->
    MsgId1 = <<"test_msg_008_1">>,
    MsgId2 = <<"test_msg_008_2">>,
    MsgType = <<"c2c">>,
    UserId = 999999,

    % 添加多个表情
    ok = msg_reaction_repo:add(MsgId1, MsgType, UserId, <<"👍"/utf8>>),
    ok = msg_reaction_repo:add(MsgId2, MsgType, UserId, <<"❤️"/utf8>>),

    % 查询用户表情历史
    {ok, Reactions} = msg_reaction_repo:find_user_reactions(UserId, 1, 10),
    ?assertEqual(2, length(Reactions)).

%% @doc 测试删除消息的所有表情
test_remove_all_by_msg() ->
    MsgId = <<"test_msg_009">>,
    MsgType = <<"c2g">>,

    % 添加多个表情
    ok = msg_reaction_repo:add(MsgId, MsgType, 999999, <<"👍"/utf8>>),
    ok = msg_reaction_repo:add(MsgId, MsgType, 999998, <<"❤️"/utf8>>),
    ok = msg_reaction_repo:add(MsgId, MsgType, 999997, <<"😄"/utf8>>),

    % 删除所有表情
    ok = msg_reaction_repo:remove_all_by_msg(MsgId, MsgType),

    % 验证已删除
    {ok, Reactions} = msg_reaction_repo:find_by_msg(MsgId, MsgType),
    ?assertEqual(0, length(Reactions)).

%% @doc 测试同一用户对同一消息添加多个不同emoji
test_add_multiple_reactions() ->
    MsgId = <<"test_msg_010">>,
    MsgType = <<"c2c">>,
    UserId = 999999,

    % 添加多个不同的emoji
    ok = msg_reaction_repo:add(MsgId, MsgType, UserId, <<"👍"/utf8>>),
    ok = msg_reaction_repo:add(MsgId, MsgType, UserId, <<"❤️"/utf8>>),
    ok = msg_reaction_repo:add(MsgId, MsgType, UserId, <<"😄"/utf8>>),

    % 验证有三个记录
    {ok, Reactions} = msg_reaction_repo:find_by_msg(MsgId, MsgType),
    ?assertEqual(3, length(Reactions)).

%% @doc 测试边界情况
test_edge_cases() ->
    MsgId = <<"test_msg_011">>,
    MsgType = <<"c2c">>,
    UserId = 999999,

    % 测试空字符串emoji（应该失败）
    {error, _} = msg_reaction_repo:add(MsgId, MsgType, UserId, <<>>),

    % 测试移除不存在的表情（应该成功）
    ok = msg_reaction_repo:remove(MsgId, MsgType, UserId, <<"👍"/utf8>>),

    % 测试查询不存在的消息
    {ok, Reactions} = msg_reaction_repo:find_by_msg(<<"non_existent_msg">>, MsgType),
    ?assertEqual(0, length(Reactions)).

%% ===================================================================
%% 辅助函数
%% ===================================================================
