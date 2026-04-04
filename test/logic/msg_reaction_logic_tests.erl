-module(msg_reaction_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("error_code.hrl").

%% ===================================================================
%% 测试固具
%% ===================================================================

setup() ->
    {ok, _} = application:ensure_all_started(imboy),
    elib_pg:query(<<"DELETE FROM msg_reaction WHERE user_id IN ($1, $2, $3)">>, [999999, 999998, 999997]),
    % 创建测试消息
    setup_test_messages(),
    % mock group_member_ds:is_member/2
    % uid 999999/999998 是成员，999997 不是
    meck:new(group_member_ds, [passthrough, no_link]),
    meck:expect(group_member_ds, is_member, fun
        (_, 999997) -> false;
        (_, _) -> true
    end),
    ok.

cleanup(_) ->
    elib_pg:query(<<"DELETE FROM msg_reaction WHERE user_id IN ($1, $2, $3)">>, [999999, 999998, 999997]),
    cleanup_test_messages(),
    catch meck:unload(group_member_ds),
    ok.

setup_test_messages() ->
    NowTs = elib_dt:to_rfc3339(elib_dt:now(millisecond)),
    Payload = <<"{}">>,
    MsgType = <<"text">>,
    % 插入 c2c 测试消息（from_id=999999, to_id=999998）
    C2CMsgIds = [
        <<"test_msg_logic_c2c_001">>,
        <<"test_msg_logic_002">>,
        <<"test_msg_logic_003">>,
        <<"test_msg_logic_004">>,
        <<"test_msg_logic_006">>,
        <<"test_msg_logic_007">>
    ],
    lists:foreach(fun(MsgId) ->
        elib_pg:query(
            <<"INSERT INTO public.msg_c2c (from_id, to_id, msg_id, msg_type, payload, server_ts, created_at) "
              "VALUES ($1, $2, $3, $4, $5, $6, $7) ON CONFLICT DO NOTHING">>,
            [999999, 999998, MsgId, MsgType, Payload, NowTs, NowTs]
        )
    end, C2CMsgIds),
    % 插入 c2g 测试消息（from_id=999999, to_id=100 即 group_id）
    C2GMsgIds = [
        <<"test_msg_logic_c2g_001">>,
        <<"test_msg_logic_005">>,
        <<"test_msg_logic_008">>
    ],
    lists:foreach(fun(MsgId) ->
        elib_pg:query(
            <<"INSERT INTO public.msg_c2g (from_id, to_id, msg_id, msg_type, payload, server_ts, created_at) "
              "VALUES ($1, $2, $3, $4, $5, $6, $7) ON CONFLICT DO NOTHING">>,
            [999999, 100, MsgId, MsgType, Payload, NowTs, NowTs]
        )
    end, C2GMsgIds),
    ok.

cleanup_test_messages() ->
    AllMsgIds = [
        <<"test_msg_logic_c2c_001">>,
        <<"test_msg_logic_c2g_001">>,
        <<"test_msg_logic_002">>,
        <<"test_msg_logic_003">>,
        <<"test_msg_logic_004">>,
        <<"test_msg_logic_005">>,
        <<"test_msg_logic_006">>,
        <<"test_msg_logic_007">>,
        <<"test_msg_logic_008">>
    ],
    lists:foreach(fun(MsgId) ->
        elib_pg:query(<<"DELETE FROM public.msg_c2c WHERE msg_id = $1">>, [MsgId]),
        elib_pg:query(<<"DELETE FROM public.msg_c2g WHERE msg_id = $1">>, [MsgId])
    end, AllMsgIds),
    ok.

%% ===================================================================
%% 测试用例
%% ===================================================================

msg_reaction_logic_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun test_add_reaction_c2c/0,
      fun test_add_reaction_c2g/0,
      fun test_add_duplicate_reaction/0,
      fun test_remove_reaction/0,
      fun test_list_reactions/0,
      fun test_stats_reactions/0,
      fun test_check_is_reacted/0,
      fun test_permission_denied_c2c/0,
      fun test_permission_denied_c2g/0,
      fun test_msg_not_found/0
     ]}.

%% @doc 测试单聊消息添加表情
test_add_reaction_c2c() ->
    MsgId = <<"test_msg_logic_c2c_001">>,
    CurrentUid = 999999,
    Emoji = <<"👍"/utf8>>,

    % 添加表情
    {ok, Result} = msg_reaction_logic:add(MsgId, <<"c2c">>, CurrentUid, Emoji),
    ?assertEqual(MsgId, maps:get(<<"msg_id">>, Result)),
    ?assertEqual(Emoji, maps:get(<<"emoji">>, Result)).

%% @doc 测试群聊消息添加表情
test_add_reaction_c2g() ->
    MsgId = <<"test_msg_logic_c2g_001">>,
    CurrentUid = 999999,
    Emoji = <<"❤️"/utf8>>,

    % 添加表情
    {ok, Result} = msg_reaction_logic:add(MsgId, <<"c2g">>, CurrentUid, Emoji),
    ?assertEqual(MsgId, maps:get(<<"msg_id">>, Result)).

%% @doc 测试重复添加相同表情（幂等性）
test_add_duplicate_reaction() ->
    MsgId = <<"test_msg_logic_002">>,
    CurrentUid = 999999,
    Emoji = <<"😄"/utf8>>,

    % 添加两次相同的表情
    {ok, _} = msg_reaction_logic:add(MsgId, <<"c2c">>, CurrentUid, Emoji),
    {ok, _} = msg_reaction_logic:add(MsgId, <<"c2c">>, CurrentUid, Emoji),

    % 验证只有一个记录
    {ok, Stats} = msg_reaction_logic:stats(MsgId, <<"c2c">>),
    TotalCount = maps:get(<<"total_count">>, Stats),
    ?assertEqual(1, TotalCount).

%% @doc 测试移除表情
test_remove_reaction() ->
    MsgId = <<"test_msg_logic_003">>,
    CurrentUid = 999999,
    Emoji = <<"🎉"/utf8>>,

    % 先添加
    {ok, _} = msg_reaction_logic:add(MsgId, <<"c2c">>, CurrentUid, Emoji),

    % 再移除
    ok = msg_reaction_logic:remove(MsgId, <<"c2c">>, CurrentUid, Emoji),

    % 验证已移除
    {ok, Stats} = msg_reaction_logic:stats(MsgId, <<"c2c">>),
    TotalCount = maps:get(<<"total_count">>, Stats),
    ?assertEqual(0, TotalCount).

%% @doc 测试查询表情列表
test_list_reactions() ->
    MsgId = <<"test_msg_logic_004">>,

    % 添加多个表情
    {ok, _} = msg_reaction_logic:add(MsgId, <<"c2c">>, 999999, <<"👍"/utf8>>),
    {ok, _} = msg_reaction_logic:add(MsgId, <<"c2c">>, 999998, <<"👍"/utf8>>),
    {ok, _} = msg_reaction_logic:add(MsgId, <<"c2c">>, 999999, <<"❤️"/utf8>>),

    % 查询表情列表
    {ok, List} = msg_reaction_logic:list(MsgId, <<"c2c">>),
    Reactions = maps:get(<<"reactions">>, List),
    ?assertEqual(2, length(Reactions)),

    % 验证第一个表情的统计
    [{<<"👍"/utf8>>, ThumbsUpData}] = lists:filter(fun({E, _}) -> E =:= <<"👍"/utf8>> end, Reactions),
    ?assertEqual(2, maps:get(<<"count">>, ThumbsUpData)).

%% @doc 测试统计表情信息
test_stats_reactions() ->
    MsgId = <<"test_msg_logic_005">>,

    % 添加多个表情
    {ok, _} = msg_reaction_logic:add(MsgId, <<"c2g">>, 999999, <<"👍"/utf8>>),
    {ok, _} = msg_reaction_logic:add(MsgId, <<"c2g">>, 999998, <<"👍"/utf8>>),
    {ok, _} = msg_reaction_logic:add(MsgId, <<"c2g">>, 999999, <<"❤️"/utf8>>),
    {ok, _} = msg_reaction_logic:add(MsgId, <<"c2g">>, 999997, <<"😄"/utf8>>),

    % 获取统计
    {ok, Stats} = msg_reaction_logic:stats(MsgId, <<"c2g">>),
    Reactions = maps:get(<<"reactions">>, Stats),
    ?assertEqual(3, length(Reactions)),

    % 验证总数
    TotalCount = maps:get(<<"total_count">>, Stats),
    ?assertEqual(4, TotalCount).

%% @doc 测试检查用户是否已添加表情
test_check_is_reacted() ->
    MsgId = <<"test_msg_logic_006">>,
    CurrentUid = 999999,
    Emoji = <<"🔥"/utf8>>,

    % 初始状态
    false = msg_reaction_logic:is_reacted(MsgId, <<"c2c">>, CurrentUid, Emoji),

    % 添加表情
    {ok, _} = msg_reaction_logic:add(MsgId, <<"c2c">>, CurrentUid, Emoji),

    % 验证已添加
    true = msg_reaction_logic:is_reacted(MsgId, <<"c2c">>, CurrentUid, Emoji).

%% @doc 测试单聊权限验证（非消息参与者）
test_permission_denied_c2c() ->
    MsgId = <<"test_msg_logic_007">>,
    CurrentUid = 999997,  % 非消息参与者（消息 from=999999, to=999998）
    Emoji = <<"👍"/utf8>>,

    % 尝试添加表情（应该被拒绝）
    {error, permission_denied} = msg_reaction_logic:add(MsgId, <<"c2c">>, CurrentUid, Emoji).

%% @doc 测试群聊权限验证（非群成员）
test_permission_denied_c2g() ->
    MsgId = <<"test_msg_logic_008">>,
    CurrentUid = 999997,  % mock 返回 false，视为非群成员
    Emoji = <<"👍"/utf8>>,

    % 尝试添加表情（应该被拒绝）
    {error, not_group_member} = msg_reaction_logic:add(MsgId, <<"c2g">>, CurrentUid, Emoji).

%% @doc 测试消息不存在
test_msg_not_found() ->
    MsgId = <<"non_existent_msg">>,
    CurrentUid = 999999,
    Emoji = <<"👍"/utf8>>,

    % 尝试添加表情（消息不存在）
    {error, msg_not_found} = msg_reaction_logic:add(MsgId, <<"c2c">>, CurrentUid, Emoji).
