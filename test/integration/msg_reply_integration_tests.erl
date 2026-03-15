%% @doc 消息引用回复功能集成测试
%% 测试范围：
%% - 单聊引用回复
%% - 群聊引用回复
%% - 嵌套引用
%% - 引用消息摘要
%% - 引用不存在的消息
-module(msg_reply_integration_tests).

-include_lib("eunit/include/eunit.hrl").

%% 测试夹具
msg_reply_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      {"单聊引用回复", fun test_c2c_reply/0},
      {"群聊引用回复", fun test_c2g_reply/0},
      {"引用消息摘要生成", fun test_reply_snippet/0},
      {"引用不存在的消息", fun test_reply_nonexistent_msg/0},
      {"引用回复消息列表查询", fun test_get_reply_chain/0},
      {"批量引用回复", fun test_batch_reply/0}
     ]
    }.

setup() ->
    _ = eunit_runner:eunit_setup(),
    application:set_env(imboy, env, test),
    % 创建测试用户
    {ok, User1} = create_test_user(<<"user1_reply">>),
    {ok, User2} = create_test_user(<<"user2_reply">>),
    % 创建好友关系
    ok = friend_ds:add_friend(User1, User2),
    % 创建测试群组
    {ok, Group} = create_test_group(User1, <<"reply_test_group">>),
    ok = group_member_ds:add_member(Group, User2),
    Context = #{user1 => User1, user2 => User2, group => Group},
    put(test_context, Context),
    Context.

cleanup(_Context) ->
    erase(test_context),
    ok.

%% ===================================================================
%% 测试用例
%% ===================================================================

test_c2c_reply() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    User2 = maps:get(user2, Context),

    % 1. 发送原始消息
    OriginalMsgId = imboy_hashid:uid(),
    OriginalContent = <<"这是一条原始消息，用于测试引用回复功能"/utf8>>,
    MsgData = #{
        <<"payload">> => OriginalContent,
        <<"msg_type">> => <<"text">>,
        <<"action">> => <<"send">>,
        <<"created_at">> => elib_dt:millisecond()
    },
    ok = msg_c2c_logic:c2c(OriginalMsgId, User1, MsgData#{<<"to">> => elib_hashids:encode(User2)}),

    % 2. 发送引用回复
    ReplyMsgId = imboy_hashid:uid(),
    ReplyData = #{
        <<"payload">> => <<"这是回复内容"/utf8>>,
        <<"msg_type">> => <<"text">>,
        <<"action">> => <<"reply">>,
        <<"reply_to_msg_id">> => OriginalMsgId,
        <<"created_at">> => elib_dt:millisecond()
    },
    ok = msg_c2c_logic:c2c(ReplyMsgId, User2, ReplyData#{<<"to">> => elib_hashids:encode(User1)}),

    % 3. 验证引用回复字段
    {ok, ReplyMsg} = msg_c2c_repo:find_msg_by_id(ReplyMsgId),
    ?assertEqual(OriginalMsgId, maps:get(<<"reply_to_msg_id">>, ReplyMsg)),
    ?assertEqual(User1, maps:get(<<"reply_to_from_id">>, ReplyMsg)),
    ?assertMatch({ok, _}, maps:find(<<"reply_snippet">>, ReplyMsg)).

test_c2g_reply() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    Group = maps:get(group, Context),

    % 1. 发送原始群聊消息
    OriginalMsgId = imboy_hashid:uid(),
    MsgData = #{
        <<"payload">> => <<"群聊原始消息"/utf8>>,
        <<"msg_type">> => <<"text">>,
        <<"action">> => <<"send">>,
        <<"created_at">> => elib_dt:millisecond()
    },
    ok = msg_c2g_logic:c2g(OriginalMsgId, User1, MsgData#{<<"to_gid">> => elib_hashids:encode(Group)}),

    % 2. 发送引用回复
    ReplyMsgId = imboy_hashid:uid(),
    ReplyData = #{
        <<"payload">> => <<"群聊回复内容"/utf8>>,
        <<"msg_type">> => <<"text">>,
        <<"action">> => <<"reply">>,
        <<"reply_to_msg_id">> => OriginalMsgId,
        <<"created_at">> => elib_dt:millisecond()
    },
    ok = msg_c2g_logic:c2g(ReplyMsgId, User1, ReplyData#{<<"to_gid">> => elib_hashids:encode(Group)}),

    % 3. 验证引用回复字段
    {ok, ReplyMsg} = msg_c2g_repo:find_msg_by_id(ReplyMsgId),
    ?assertEqual(OriginalMsgId, maps:get(<<"reply_to_msg_id">>, ReplyMsg)).

test_reply_snippet() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    User2 = maps:get(user2, Context),

    % 1. 发送一条长消息
    OriginalMsgId = imboy_hashid:uid(),
    LongContent = <<"这是一条很长的消息，用于测试消息摘要功能。消息摘要应该只截取前50个字符，以便在引用回复时显示简洁的预览。"/utf8>>,
    MsgData = #{
        <<"payload">> => LongContent,
        <<"msg_type">> => <<"text">>,
        <<"action">> => <<"send">>,
        <<"created_at">> => elib_dt:millisecond()
    },
    ok = msg_c2c_logic:c2c(OriginalMsgId, User1, MsgData#{<<"to">> => elib_hashids:encode(User2)}),

    % 2. 发送引用回复
    ReplyMsgId = imboy_hashid:uid(),
    ReplyData = #{
        <<"payload">> => <<"回复"/utf8>>,
        <<"msg_type">> => <<"text">>,
        <<"action">> => <<"reply">>,
        <<"reply_to_msg_id">> => OriginalMsgId,
        <<"created_at">> => elib_dt:millisecond()
    },
    ok = msg_c2c_logic:c2c(ReplyMsgId, User2, ReplyData#{<<"to">> => elib_hashids:encode(User1)}),

    % 3. 验证摘要长度（假设限制50字符）
    {ok, ReplyMsg} = msg_c2c_repo:find_msg_by_id(ReplyMsgId),
    Snippet = maps:get(<<"reply_snippet">>, ReplyMsg),
    ?assertMatch(true, byte_size(Snippet) =< 150). % 50中文字符约150字节

test_reply_nonexistent_msg() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    User2 = maps:get(user2, Context),

    % 尝试引用不存在的消息
    ReplyMsgId = imboy_hashid:uid(),
    ReplyData = #{
        <<"payload">> => <<"回复不存在消息"/utf8>>,
        <<"msg_type">> => <<"text">>,
        <<"action">> => <<"reply">>,
        <<"reply_to_msg_id">> => <<"nonexistent_msg_id">>,
        <<"created_at">> => elib_dt:millisecond()
    },

    % 验证行为：可以选择允许发送但标记为无效引用，或者拒绝发送
    % 这里假设允许发送
    Result = msg_c2c_logic:c2c(ReplyMsgId, User1, ReplyData#{<<"to">> => elib_hashids:encode(User2)}),
    ?assertMatch(ok, Result).

test_get_reply_chain() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    User2 = maps:get(user2, Context),

    % 1. 发送消息链：Msg1 -> Reply1 -> Reply2
    MsgId1 = imboy_hashid:uid(),
    MsgData1 = #{
        <<"payload">> => <<"消息1"/utf8>>,
        <<"msg_type">> => <<"text">>,
        <<"action">> => <<"send">>,
        <<"created_at">> => elib_dt:millisecond()
    },
    ok = msg_c2c_logic:c2c(MsgId1, User1, MsgData1#{<<"to">> => elib_hashids:encode(User2)}),

    ReplyId1 = imboy_hashid:uid(),
    ReplyData1 = #{
        <<"payload">> => <<"回复1"/utf8>>,
        <<"msg_type">> => <<"text">>,
        <<"action">> => <<"reply">>,
        <<"reply_to_msg_id">> => MsgId1,
        <<"created_at">> => elib_dt:millisecond()
    },
    ok = msg_c2c_logic:c2c(ReplyId1, User2, ReplyData1#{<<"to">> => elib_hashids:encode(User1)}),

    ReplyId2 = imboy_hashid:uid(),
    ReplyData2 = #{
        <<"payload">> => <<"回复2"/utf8>>,
        <<"msg_type">> => <<"text">>,
        <<"action">> => <<"reply">>,
        <<"reply_to_msg_id">> => ReplyId1,
        <<"created_at">> => elib_dt:millisecond()
    },
    ok = msg_c2c_logic:c2c(ReplyId2, User1, ReplyData2#{<<"to">> => elib_hashids:encode(User2)}),

    % 2. 查询引用链
    % 假设有查询引用链的接口
    {ok, Chain} = msg_c2c_repo:get_reply_chain(ReplyId2),

    % 3. 验证引用链
    ?assertEqual(3, length(Chain)).

test_batch_reply() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    User2 = maps:get(user2, Context),

    % 1. 发送多条消息
    MsgIds = lists:map(fun(N) ->
        MsgId = imboy_hashid:uid(),
        MsgData = #{
            <<"payload">> => <<N/integer, "批量回复测试消息"/utf8>>,
            <<"msg_type">> => <<"text">>,
            <<"action">> => <<"send">>,
            <<"created_at">> => elib_dt:millisecond()
        },
        ok = msg_c2c_logic:c2c(MsgId, User1, MsgData#{<<"to">> => elib_hashids:encode(User2)}),
        MsgId
    end, lists:seq(1, 3)),

    % 2. 对多条消息进行引用回复
    lists:foreach(fun(OriginalMsgId) ->
        ReplyMsgId = imboy_hashid:uid(),
        ReplyData = #{
            <<"payload">> => <<"批量回复"/utf8>>,
            <<"msg_type">> => <<"text">>,
            <<"action">> => <<"reply">>,
            <<"reply_to_msg_id">> => OriginalMsgId,
            <<"created_at">> => elib_dt:millisecond()
        },
        ok = msg_c2c_logic:c2c(ReplyMsgId, User2, ReplyData#{<<"to">> => elib_hashids:encode(User1)})
    end, MsgIds),

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
