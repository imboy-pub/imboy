-module(msg_reaction_ds_tests).
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% 测试固具
%% ===================================================================

setup() ->
    {ok, _} = application:ensure_all_started(imboy),
    elib_pg:query(<<"DELETE FROM msg_reaction WHERE user_id = $1">>, [999999]),
    ok.

cleanup(_) ->
    elib_pg:query(<<"DELETE FROM msg_reaction WHERE user_id = $1">>, [999999]),
    ok.

%% ===================================================================
%% 测试用例
%% ===================================================================

msg_reaction_ds_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun test_add_reaction/0,
      fun test_remove_reaction/0,
      fun test_get_reactions/0,
      fun test_get_reaction_stats/0,
      fun test_is_reacted/0,
      fun test_multiple_users_same_emoji/0,
      fun test_cache_invalidation/0
     ]}.

%% @doc 测试添加表情
test_add_reaction() ->
    MsgId = <<"test_msg_ds_001">>,
    MsgType = <<"c2c">>,
    UserId = 999999,
    Emoji = <<"👍"/utf8>>,

    % 添加表情
    {ok, Result} = msg_reaction_ds:add_reaction(MsgId, MsgType, UserId, Emoji),
    ?assertEqual(MsgId, maps:get(<<"msg_id">>, Result)),
    ?assertEqual(Emoji, maps:get(<<"emoji">>, Result)).

%% @doc 测试移除表情
test_remove_reaction() ->
    MsgId = <<"test_msg_ds_002">>,
    MsgType = <<"c2c">>,
    UserId = 999999,
    Emoji = <<"❤️"/utf8>>,

    % 先添加
    {ok, _} = msg_reaction_ds:add_reaction(MsgId, MsgType, UserId, Emoji),

    % 再移除
    ok = msg_reaction_ds:remove_reaction(MsgId, MsgType, UserId, Emoji),

    % 验证已移除
    {ok, Stats} = msg_reaction_ds:get_reaction_stats(MsgId, MsgType),
    ?assertEqual(0, length(Stats)).

%% @doc 测试获取表情列表
test_get_reactions() ->
    MsgId = <<"test_msg_ds_003">>,
    MsgType = <<"c2g">>,

    % 添加多个表情
    {ok, _} = msg_reaction_ds:add_reaction(MsgId, MsgType, 999999, <<"👍"/utf8>>),
    {ok, _} = msg_reaction_ds:add_reaction(MsgId, MsgType, 999998, <<"👍"/utf8>>),
    {ok, _} = msg_reaction_ds:add_reaction(MsgId, MsgType, 999997, <<"❤️"/utf8>>),

    % 获取表情列表
    {ok, Reactions} = msg_reaction_ds:get_reactions(MsgId, MsgType),
    ?assertEqual(2, length(Reactions)),

    % 验证第一个表情的统计
    [{<<"👍"/utf8>>, ThumbsUpData}] = lists:filter(fun({E, _}) -> E =:= <<"👍"/utf8>> end, Reactions),
    ?assertEqual(2, maps:get(<<"count">>, ThumbsUpData)),
    ?assertEqual(2, length(maps:get(<<"users">>, ThumbsUpData))).

%% @doc 测试获取表情统计
test_get_reaction_stats() ->
    MsgId = <<"test_msg_ds_004">>,
    MsgType = <<"c2c">>,

    % 添加表情
    {ok, _} = msg_reaction_ds:add_reaction(MsgId, MsgType, 999999, <<"👍"/utf8>>),
    {ok, _} = msg_reaction_ds:add_reaction(MsgId, MsgType, 999998, <<"👍"/utf8>>),
    {ok, _} = msg_reaction_ds:add_reaction(MsgId, MsgType, 999999, <<"❤️"/utf8>>),

    % 获取统计
    {ok, Stats} = msg_reaction_ds:get_reaction_stats(MsgId, MsgType),
    ?assertEqual(2, length(Stats)),

    % 验证总数
    TotalCount = lists:foldl(fun(_, Acc) -> Acc + 1 end, 0, Stats),
    ?assertEqual(2, TotalCount).

%% @doc 测试检查用户是否已添加表情
test_is_reacted() ->
    MsgId = <<"test_msg_ds_005">>,
    MsgType = <<"c2c">>,
    UserId = 999999,
    Emoji = <<"😄"/utf8>>,

    % 初始状态
    false = msg_reaction_ds:is_reacted(MsgId, MsgType, UserId, Emoji),

    % 添加表情
    {ok, _} = msg_reaction_ds:add_reaction(MsgId, MsgType, UserId, Emoji),

    % 验证已添加
    true = msg_reaction_ds:is_reacted(MsgId, MsgType, UserId, Emoji).

%% @doc 测试多个用户添加相同emoji
test_multiple_users_same_emoji() ->
    MsgId = <<"test_msg_ds_006">>,
    MsgType = <<"c2g">>,

    % 多个用户添加相同emoji
    {ok, _} = msg_reaction_ds:add_reaction(MsgId, MsgType, 999999, <<"👍"/utf8>>),
    {ok, _} = msg_reaction_ds:add_reaction(MsgId, MsgType, 999998, <<"👍"/utf8>>),
    {ok, _} = msg_reaction_ds:add_reaction(MsgId, MsgType, 999997, <<"👍"/utf8>>),

    % 获取统计
    {ok, Stats} = msg_reaction_ds:get_reaction_stats(MsgId, MsgType),
    [{<<"👍"/utf8>>, Data}] = Stats,
    ?assertEqual(3, maps:get(<<"count">>, Data)),
    ?assertEqual(3, length(maps:get(<<"users">>, Data))).

%% @doc 测试缓存失效
test_cache_invalidation() ->
    MsgId = <<"test_msg_ds_007">>,
    MsgType = <<"c2c">>,
    UserId = 999999,
    Emoji = <<"🎉"/utf8>>,

    % 添加表情
    {ok, _} = msg_reaction_ds:add_reaction(MsgId, MsgType, UserId, Emoji),

    % 获取统计（缓存）
    {ok, Stats1} = msg_reaction_ds:get_reaction_stats(MsgId, MsgType),

    % 移除表情
    ok = msg_reaction_ds:remove_reaction(MsgId, MsgType, UserId, Emoji),

    % 再次获取统计（应该从数据库重新加载）
    {ok, Stats2} = msg_reaction_ds:get_reaction_stats(MsgId, MsgType),
    ?assertEqual(length(Stats1) - 1, length(Stats2)).
