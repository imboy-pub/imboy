%% @doc 消息表情回应功能集成测试
%% 测试范围：
%% - 添加表情回应
%% - 移除表情回应
%% - 查询表情列表
%% - 表情统计
%% - 通知机制
-module(msg_reaction_integration_tests).

-include_lib("eunit/include/eunit.hrl").

%% 支持的测试 emoji
-define(EMOJI_LIKE, <<"👍"/utf8>>).
-define(EMOJI_HEART, <<"❤️"/utf8>>).
-define(EMOJI_SMILE, <<"😄"/utf8>>).

%% 测试夹具
msg_reaction_test_() ->
    _ = eunit_runner:eunit_setup(),
    application:set_env(imboy, env, test),
    case eunit_runner:eunit_try_db() of
        {ok, _Driver, _Conn} ->
            {foreach,
             fun setup/0,
             fun cleanup/1,
             [
              {"单聊添加表情回应", fun test_c2c_add_reaction/0},
              {"群聊添加表情回应", fun test_c2g_add_reaction/0},
              {"移除表情回应", fun test_remove_reaction/0},
              {"重复添加表情（幂等性）", fun test_duplicate_reaction/0},
              {"查询表情列表", fun test_list_reactions/0},
              {"表情统计", fun test_reaction_stats/0},
              {"多种表情组合", fun test_multiple_emojis/0},
              {"多人回应同一消息", fun test_multiple_users_reaction/0},
              {"检查用户是否已回应", fun test_is_reacted/0}
             ]
            };
        {error, _Reason} ->
            {"Database not available",
             fun() -> {skip, "Database not available"} end}
    end.

setup() ->
    % 创建测试用户
    {ok, User1} = create_test_user(<<"user1_reaction">>),
    {ok, User2} = create_test_user(<<"user2_reaction">>),
    {ok, User3} = create_test_user(<<"user3_reaction">>),
    % 创建好友关系
    ok = ensure_friends(User1, User2),
    ok = ensure_friends(User1, User3),
    ok = ensure_friends(User2, User3),
    % 创建测试群组
    {ok, Group} = create_test_group(User1, <<"reaction_test_group">>),
    ok = group_member_ds:add_member(Group, User2),
    ok = group_member_ds:add_member(Group, User3),
    Context = #{user1 => User1, user2 => User2, user3 => User3, group => Group},
    persistent_term:put({?MODULE, test_context}, Context),
    Context.

cleanup(_Context) ->
    persistent_term:erase({?MODULE, test_context}),
    ok.

%% ===================================================================
%% 测试用例
%% ===================================================================

test_c2c_add_reaction() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    User2 = maps:get(user2, Context),

    % 1. 发送一条消息
    MsgId = send_c2c_message(User1, User2, <<"测试表情回应"/utf8>>),

    % 2. 添加表情回应
    {ok, Result} = msg_reaction_logic:add(MsgId, <<"c2c">>, User2, ?EMOJI_LIKE),

    % 3. 验证返回结果
    ?assertEqual(MsgId, maps:get(<<"msg_id">>, Result)),
    ?assertEqual(?EMOJI_LIKE, maps:get(<<"emoji">>, Result)),
    {ok, Stats} = msg_reaction_logic:stats(MsgId, <<"c2c">>),
    ?assertEqual(1, maps:get(<<"total_count">>, Stats)),
    [LikeStat | _] = lists:filter(fun(S) ->
        maps:get(<<"emoji">>, S) =:= ?EMOJI_LIKE
    end, maps:get(<<"reactions">>, Stats)),
    ?assertEqual(1, maps:get(<<"count">>, LikeStat)),

    ok.

test_c2g_add_reaction() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    Group = maps:get(group, Context),

    % 1. 发送一条群聊消息
    MsgId = send_c2g_message(User1, Group, <<"群聊表情测试"/utf8>>),

    % 2. 添加表情回应
    {ok, Result} = msg_reaction_logic:add(MsgId, <<"c2g">>, User1, ?EMOJI_HEART),

    % 3. 验证返回结果
    ?assertEqual(?EMOJI_HEART, maps:get(<<"emoji">>, Result)),

    ok.

test_remove_reaction() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    User2 = maps:get(user2, Context),

    % 1. 发送消息并添加表情
    MsgId = send_c2c_message(User1, User2, <<"移除表情测试"/utf8>>),
    {ok, _} = msg_reaction_logic:add(MsgId, <<"c2c">>, User2, ?EMOJI_LIKE),

    % 2. 移除表情
    ok = msg_reaction_logic:remove(MsgId, <<"c2c">>, User2, ?EMOJI_LIKE),

    % 3. 验证移除成功
    false = msg_reaction_logic:is_reacted(MsgId, <<"c2c">>, User2, ?EMOJI_LIKE),

    ok.

test_duplicate_reaction() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    User2 = maps:get(user2, Context),

    % 1. 发送消息
    MsgId = send_c2c_message(User1, User2, <<"重复表情测试"/utf8>>),

    % 2. 第一次添加表情
    {ok, _} = msg_reaction_logic:add(MsgId, <<"c2c">>, User2, ?EMOJI_LIKE),

    % 3. 重复添加相同表情（应该幂等或报错）
    Result = msg_reaction_logic:add(MsgId, <<"c2c">>, User2, ?EMOJI_LIKE),

    % 4. 验证行为（这里假设幂等成功）
    ?assertMatch({ok, _}, Result),

    % 5. 验证计数仍然是1
    {ok, Stats} = msg_reaction_logic:stats(MsgId, <<"c2c">>),
    EmojiStats = lists:filter(fun(S) ->
        maps:get(<<"emoji">>, S) =:= ?EMOJI_LIKE
    end, maps:get(<<"reactions">>, Stats)),
    ?assertEqual(1, length(EmojiStats)),
    [Stat | _] = EmojiStats,
    ?assertEqual(1, maps:get(<<"count">>, Stat)),

    ok.

test_list_reactions() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    User2 = maps:get(user2, Context),
    User3 = maps:get(user3, Context),
    Group = maps:get(group, Context),

    % 1. 发送群聊消息，允许多成员响应
    MsgId = send_c2g_message(User1, Group, <<"列表测试"/utf8>>),

    % 2. 多人添加不同表情
    {ok, _} = msg_reaction_logic:add(MsgId, <<"c2g">>, User2, ?EMOJI_LIKE),
    {ok, _} = msg_reaction_logic:add(MsgId, <<"c2g">>, User3, ?EMOJI_HEART),
    {ok, _} = msg_reaction_logic:add(MsgId, <<"c2g">>, User1, ?EMOJI_SMILE),

    % 3. 查询表情列表
    {ok, Reactions} = msg_reaction_logic:list(MsgId, <<"c2g">>),

    % 4. 验证结果
    ?assertEqual(3, maps:get(<<"total_count">>, Reactions)),
    ReactionList = maps:get(<<"reactions">>, Reactions),
    ?assertEqual(3, length(ReactionList)),

    ok.

test_reaction_stats() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    User2 = maps:get(user2, Context),
    User3 = maps:get(user3, Context),
    Group = maps:get(group, Context),

    % 1. 发送群聊消息，允许多成员响应
    MsgId = send_c2g_message(User1, Group, <<"统计测试"/utf8>>),

    % 2. 添加表情
    {ok, _} = msg_reaction_logic:add(MsgId, <<"c2g">>, User2, ?EMOJI_LIKE),
    {ok, _} = msg_reaction_logic:add(MsgId, <<"c2g">>, User3, ?EMOJI_LIKE),
    {ok, _} = msg_reaction_logic:add(MsgId, <<"c2g">>, User1, ?EMOJI_HEART),

    % 3. 获取统计
    {ok, Stats} = msg_reaction_logic:stats(MsgId, <<"c2g">>),

    % 4. 验证统计结果
    ?assertEqual(3, maps:get(<<"total_count">>, Stats)),

    % 5. 验证每个 emoji 的统计
    Reactions = maps:get(<<"reactions">>, Stats),
    LikeStats = lists:filter(fun(S) ->
        maps:get(<<"emoji">>, S) =:= ?EMOJI_LIKE
    end, Reactions),
    [LikeStat | _] = LikeStats,
    ?assertEqual(2, maps:get(<<"count">>, LikeStat)),

    ok.

test_multiple_emojis() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    User2 = maps:get(user2, Context),

    % 1. 发送消息
    MsgId = send_c2c_message(User1, User2, <<"多表情测试"/utf8>>),

    % 2. 同一用户添加多种表情
    {ok, _} = msg_reaction_logic:add(MsgId, <<"c2c">>, User2, ?EMOJI_LIKE),
    {ok, _} = msg_reaction_logic:add(MsgId, <<"c2c">>, User2, ?EMOJI_HEART),
    {ok, _} = msg_reaction_logic:add(MsgId, <<"c2c">>, User2, ?EMOJI_SMILE),

    % 3. 验证可以添加多种表情
    true = msg_reaction_logic:is_reacted(MsgId, <<"c2c">>, User2, ?EMOJI_LIKE),
    true = msg_reaction_logic:is_reacted(MsgId, <<"c2c">>, User2, ?EMOJI_HEART),
    true = msg_reaction_logic:is_reacted(MsgId, <<"c2c">>, User2, ?EMOJI_SMILE),

    ok.

test_multiple_users_reaction() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    User2 = maps:get(user2, Context),
    User3 = maps:get(user3, Context),
    Group = maps:get(group, Context),

    % 1. 发送群聊消息
    MsgId = send_c2g_message(User1, Group, <<"多人回应测试"/utf8>>),

    % 2. 多人添加表情
    {ok, _} = msg_reaction_logic:add(MsgId, <<"c2g">>, User1, ?EMOJI_LIKE),
    {ok, _} = msg_reaction_logic:add(MsgId, <<"c2g">>, User2, ?EMOJI_LIKE),
    {ok, _} = msg_reaction_logic:add(MsgId, <<"c2g">>, User3, ?EMOJI_LIKE),

    % 3. 验证统计
    {ok, Stats} = msg_reaction_logic:stats(MsgId, <<"c2g">>),
    ?assertEqual(3, maps:get(<<"total_count">>, Stats)),

    ok.

test_is_reacted() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    User2 = maps:get(user2, Context),

    % 1. 发送消息
    MsgId = send_c2c_message(User1, User2, <<"检查回应测试"/utf8>>),

    % 2. 未添加表情时检查
    false = msg_reaction_logic:is_reacted(MsgId, <<"c2c">>, User2, ?EMOJI_LIKE),

    % 3. 添加表情
    {ok, _} = msg_reaction_logic:add(MsgId, <<"c2c">>, User2, ?EMOJI_LIKE),

    % 4. 已添加表情后检查
    true = msg_reaction_logic:is_reacted(MsgId, <<"c2c">>, User2, ?EMOJI_LIKE),

    ok.

%% ===================================================================
%% 辅助函数
%% ===================================================================

get_context() ->
    persistent_term:get({?MODULE, test_context}).

wait_for_c2c_message(MsgId) ->
    wait_for_c2c_message(MsgId, 40).

wait_for_c2c_message(_MsgId, 0) ->
    error(c2c_message_not_ready);
wait_for_c2c_message(MsgId, AttemptsLeft) ->
    case msg_c2c_repo:find_msg_by_id(MsgId) of
        {ok, _Msg} ->
            ok;
        _ ->
            timer:sleep(50),
            wait_for_c2c_message(MsgId, AttemptsLeft - 1)
    end.

wait_for_c2g_message(MsgId) ->
    wait_for_c2g_message(MsgId, 40).

wait_for_c2g_message(_MsgId, 0) ->
    error(c2g_message_not_ready);
wait_for_c2g_message(MsgId, AttemptsLeft) ->
    case msg_c2g_repo:find_msg_by_id(MsgId) of
        {ok, _Msg} ->
            ok;
        _ ->
            timer:sleep(50),
            wait_for_c2g_message(MsgId, AttemptsLeft - 1)
    end.

ensure_friends(User1, User2) ->
    NowTs = elib_dt:now(),
    ok = friend_ds:confirm_friend(friend_ds:is_friend(User1, User2),
                                  User1,
                                  User2,
                                  <<>>,
                                  #{<<"is_from">> => 1, <<"source">> => <<"test">>},
                                  <<>>,
                                  NowTs),
    ok = friend_ds:confirm_friend(friend_ds:is_friend(User2, User1),
                                  User2,
                                  User1,
                                  <<>>,
                                  #{<<"source">> => <<"test">>},
                                  <<>>,
                                  NowTs),
    ok = friend_ds:invalidate_cache(User1, User2),
    imboy_cache:flush({check_relationship3, User1, User2}),
    imboy_cache:flush({check_relationship3, User2, User1}),
    ok.

create_test_user(Nickname) ->
    Uid = elib_tsid:generate(),
    Suffix = integer_to_binary(erlang:phash2(Uid, 1000000000)),
    User = #{
        <<"uid">> => Uid,
        <<"nickname">> => Nickname,
        <<"account">> => <<Nickname/binary, "_", Suffix/binary>>,
        <<"mobile">> => list_to_binary(io_lib:format("13~9..0B", [erlang:phash2(Uid, 1000000000)])),
        <<"email">> => <<"test_", Suffix/binary, "@example.com">>,
        <<"password">> => <<"password123">>,
        <<"created_at">> => elib_dt:millisecond()
    },
    ok = user_repo:create(User),
    {ok, Uid}.

create_test_group(OwnerId, Name) ->
    Gid = elib_tsid:generate(),
    Group = #{
        <<"gid">> => Gid,
        <<"owner_uid">> => OwnerId,
        <<"name">> => Name,
        <<"created_at">> => elib_dt:millisecond()
    },
    ok = group_repo:create(Group),
    ok = group_member_ds:add_member(Gid, OwnerId),
    {ok, Gid}.

send_c2c_message(From, To, Content) ->
    MsgId = integer_to_binary(elib_tsid:generate()),
    MsgData = #{
        <<"payload">> => Content,
        <<"msg_type">> => <<"text">>,
        <<"action">> => <<"send">>,
        <<"created_at">> => elib_dt:millisecond()
    },
    ok = msg_c2c_logic:c2c(MsgId, From, MsgData#{<<"to">> => integer_to_binary(To)}),
    ok = wait_for_c2c_message(MsgId),
    MsgId.

send_c2g_message(From, Group, Content) ->
    MsgId = integer_to_binary(elib_tsid:generate()),
    MsgData = #{
        <<"payload">> => Content,
        <<"msg_type">> => <<"text">>,
        <<"action">> => <<"send">>,
        <<"created_at">> => elib_dt:millisecond()
    },
    ok = msg_c2g_logic:c2g(MsgId, From, MsgData#{<<"to">> => integer_to_binary(Group)}),
    ok = wait_for_c2g_message(MsgId),
    MsgId.
