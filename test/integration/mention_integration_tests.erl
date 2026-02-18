%% @doc @提及功能集成测试
%% 测试范围：
%% - @单个用户
%% - @多个用户
%% - @所有人
%% - 提及通知
%% - 提及列表查询
%% - 已读状态
-module(mention_integration_tests).

-include_lib("eunit/include/eunit.hrl").

%% 测试夹具
mention_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      {"@单个用户", fun test_mention_single_user/0},
      {"@多个用户", fun test_mention_multiple_users/0},
      {"@所有人", fun test_mention_all/0},
      {"提及通知发送", fun test_mention_notification/0},
      {"查询@我的消息", fun test_query_my_mentions/0},
      {"标记提及已读", fun test_mark_mention_read/0},
      {"未读提及计数", fun test_unread_mention_count/0},
      {"组合@和普通消息", fun test_mixed_mention_message/0}
     ]
    }.

setup() ->
    application:set_env(imboy, env, test),
    % 创建测试用户
    {ok, User1} = create_test_user(<<"user1_mention">>),
    {ok, User2} = create_test_user(<<"user2_mention">>),
    {ok, User3} = create_test_user(<<"user3_mention">>),
    % 创建测试群组
    {ok, Group} = create_test_group(User1, <<"mention_test_group">>),
    ok = group_member_ds:add_member(Group, User2),
    ok = group_member_ds:add_member(Group, User3),
    #{
        user1 => User1,
        user2 => User2,
        user3 => User3,
        group => Group
    }.

cleanup(_Context) ->
    ok.

%% ===================================================================
%% 测试用例
%% ===================================================================

test_mention_single_user() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    User2 = maps:get(user2, Context),
    Group = maps:get(group, Context),

    % 1. 发送带@的消息
    MsgId = imboy_hashid:uid(),
    MsgData = #{
        <<"payload">> => <<"你好 @user2 这是一个测试"/utf8>>,
        <<"msg_type">> => <<"text">>,
        <<"action">> => <<"send">>,
        <<"mentions">> => [User2],  % 被@的用户ID列表
        <<"created_at">> => elib_dt:millisecond()
    },
    ok = msg_c2g_logic:c2g(MsgId, User1, MsgData#{<<"to_gid">> => elib_hashids:encode(Group)}),

    % 2. 验证@记录
    {ok, Mentions} = mention_repo:list_by_user(User2, #{limit => 10}),
    ?assertEqual(1, length(Mentions)),

    ok.

test_mention_multiple_users() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    User2 = maps:get(user2, Context),
    User3 = maps:get(user3, Context),
    Group = maps:get(group, Context),

    % 1. 发送带多个@的消息
    MsgId = imboy_hashid:uid(),
    MsgData = #{
        <<"payload">> => <<"@user2 @user3 请查看"/utf8>>,
        <<"msg_type">> => <<"text">>,
        <<"action">> => <<"send">>,
        <<"mentions">> => [User2, User3],
        <<"created_at">> => elib_dt:millisecond()
    },
    ok = msg_c2g_logic:c2g(MsgId, User1, MsgData#{<<"to_gid">> => elib_hashids:encode(Group)}),

    % 2. 验证 User2 的@记录
    {ok, Mentions2} = mention_repo:list_by_user(User2, #{limit => 10}),
    ?assertEqual(1, length(Mentions2)),

    % 3. 验证 User3 的@记录
    {ok, Mentions3} = mention_repo:list_by_user(User3, #{limit => 10}),
    ?assertEqual(1, length(Mentions3)),

    ok.

test_mention_all() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    Group = maps:get(group, Context),

    % 1. 发送@所有人的消息
    MsgId = imboy_hashid:uid(),
    MsgData = #{
        <<"payload">> => <<"@所有人 重要通知"/utf8>>,
        <<"msg_type">> => <<"text">>,
        <<"action">> => <<"send">>,
        <<"mentions">> => [<<"all">>],  % 特殊标记 @所有人
        <<"created_at">> => elib_dt:millisecond()
    },
    ok = msg_c2g_logic:c2g(MsgId, User1, MsgData#{<<"to_gid">> => elib_hashids:encode(Group)}),

    % 2. 验证消息中的 mentions 字段
    {ok, Msg} = msg_c2g_repo:find_msg_by_id(MsgId),
    Mentions = maps:get(<<"mentions">>, Msg),
    ?assert(lists:member(<<"all">>, Mentions)),

    ok.

test_mention_notification() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    User2 = maps:get(user2, Context),
    Group = maps:get(group, Context),

    % 1. 发送@消息
    MsgId = imboy_hashid:uid(),
    MsgData = #{
        <<"payload">> => <<"@user2 通知测试"/utf8>>,
        <<"msg_type">> => <<"text">>,
        <<"action">> => <<"send">>,
        <<"mentions">> => [User2],
        <<"created_at">> => elib_dt:millisecond()
    },
    ok = msg_c2g_logic:c2g(MsgId, User1, MsgData#{<<"to_gid">> => elib_hashids:encode(Group)}),

    % 2. 验证通知已发送（通过 WebSocket 或其他方式）
    % 这里假设有通知记录
    {ok, Notifications} = msg_s2c_repo:list_by_user(User2, #{limit => 10}),
    HasMentionNotification = lists:any(fun(N) ->
        maps:get(<<"action">>, N) =:= <<"mention">>
    end, Notifications),
    ?assert(HasMentionNotification),

    ok.

test_query_my_mentions() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    User2 = maps:get(user2, Context),
    Group = maps:get(group, Context),

    % 1. 发送多条@消息
    lists:foreach(fun(N) ->
        MsgId = imboy_hashid:uid(),
        MsgData = #{
            <<"payload">> => <<N/integer, "@user2 消息"/utf8>>,
            <<"msg_type">> => <<"text">>,
            <<"action">> => <<"send">>,
            <<"mentions">> => [User2],
            <<"created_at">> => elib_dt:millisecond()
        },
        ok = msg_c2g_logic:c2g(MsgId, User1, MsgData#{<<"to_gid">> => elib_hashids:encode(Group)})
    end, lists:seq(1, 3)),

    % 2. 查询@User2 的消息
    {ok, Mentions} = mention_logic:list_my_mentions(User2, #{limit => 10}),

    % 3. 验证数量
    ?assertEqual(3, length(Mentions)),

    ok.

test_mark_mention_read() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    User2 = maps:get(user2, Context),
    Group = maps:get(group, Context),

    % 1. 发送@消息
    MsgId = imboy_hashid:uid(),
    MsgData = #{
        <<"payload">> => <<"@user2 已读测试"/utf8>>,
        <<"msg_type">> => <<"text">>,
        <<"action">> => <<"send">>,
        <<"mentions">> => [User2],
        <<"created_at">> => elib_dt:millisecond()
    },
    ok = msg_c2g_logic:c2g(MsgId, User1, MsgData#{<<"to_gid">> => elib_hashids:encode(Group)}),

    % 2. 标记已读
    ok = mention_logic:mark_read(User2, MsgId),

    % 3. 验证已读状态
    {ok, Mention} = mention_repo:find_by_msg_and_user(MsgId, User2),
    ?assertEqual(true, maps:get(<<"is_read">>, Mention)),

    ok.

test_unread_mention_count() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    User2 = maps:get(user2, Context),
    Group = maps:get(group, Context),

    % 1. 发送多条@消息
    lists:foreach(fun(N) ->
        MsgId = imboy_hashid:uid(),
        MsgData = #{
            <<"payload">> => <<N/integer, "@user2 计数测试"/utf8>>,
            <<"msg_type">> => <<"text">>,
            <<"action">> => <<"send">>,
            <<"mentions">> => [User2],
            <<"created_at">> => elib_dt:millisecond()
        },
        ok = msg_c2g_logic:c2g(MsgId, User1, MsgData#{<<"to_gid">> => elib_hashids:encode(Group)})
    end, lists:seq(1, 5)),

    % 2. 获取未读计数
    {ok, Count} = mention_logic:unread_count(User2),

    % 3. 验证计数
    ?assertEqual(5, Count),

    % 4. 标记部分已读
    {ok, Mentions} = mention_repo:list_by_user(User2, #{limit => 2}),
    lists:foreach(fun(M) ->
        ok = mention_logic:mark_read(User2, maps:get(<<"msg_id">>, M))
    end, Mentions),

    % 5. 再次获取未读计数
    {ok, NewCount} = mention_logic:unread_count(User2),
    ?assertEqual(3, NewCount),

    ok.

test_mixed_mention_message() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    User2 = maps:get(user2, Context),
    User3 = maps:get(user3, Context),
    Group = maps:get(group, Context),

    % 1. 发送普通消息（无@）
    MsgId1 = imboy_hashid:uid(),
    MsgData1 = #{
        <<"payload">> => <<"普通消息"/utf8>>,
        <<"msg_type">> => <<"text">>,
        <<"action">> => <<"send">>,
        <<"created_at">> => elib_dt:millisecond()
    },
    ok = msg_c2g_logic:c2g(MsgId1, User1, MsgData1#{<<"to_gid">> => elib_hashids:encode(Group)}),

    % 2. 发送@消息
    MsgId2 = imboy_hashid:uid(),
    MsgData2 = #{
        <<"payload">> => <<"@user2 @消息"/utf8>>,
        <<"msg_type">> => <<"text">>,
        <<"action">> => <<"send">>,
        <<"mentions">> => [User2],
        <<"created_at">> => elib_dt:millisecond()
    },
    ok = msg_c2g_logic:c2g(MsgId2, User1, MsgData2#{<<"to_gid">> => elib_hashids:encode(Group)}),

    % 3. 验证只有 User2 有@记录
    {ok, Mentions2} = mention_repo:list_by_user(User2, #{limit => 10}),
    ?assertEqual(1, length(Mentions2)),

    % 4. 验证 User3 没有被@
    {ok, Mentions3} = mention_repo:list_by_user(User3, #{limit => 10}),
    ?assertEqual(0, length(Mentions3)),

    ok.

%% ===================================================================
%% 辅助函数
%% ===================================================================

get_context() ->
    get(test_context).

create_test_user(Nickname) ->
    Uid = imboy_hashid:uid(),
    User = #{
        <<"uid">> => Uid,
        <<"nickname">> => Nickname,
        <<"account">> => Nickname,
        <<"password">> => <<"password123">>,
        <<"created_at">> => elib_dt:millisecond()
    },
    ok = user_repo:create(User),
    {ok, Uid}.

create_test_group(OwnerId, Name) ->
    Gid = imboy_hashid:uid(),
    Group = #{
        <<"gid">> => Gid,
        <<"owner_uid">> => OwnerId,
        <<"name">> => Name,
        <<"created_at">> => elib_dt:millisecond()
    },
    ok = group_repo:create(Group),
    ok = group_member_ds:add_member(Gid, OwnerId),
    {ok, Gid}.
