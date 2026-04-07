%% @doc 会话置顶与删除功能集成测试
%% 测试范围：
%% - 会话置顶
%% - 会话取消置顶
%% - 获取置顶列表
%% - 会话软删除
%% - 会话恢复
%% - 置顶与删除组合场景
-module(conversation_pin_delete_integration_tests).

-include_lib("eunit/include/eunit.hrl").

%% 测试夹具
conversation_pin_delete_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      {"置顶单聊会话", fun test_pin_c2c_conversation/0},
      {"置顶群聊会话", fun test_pin_c2g_conversation/0},
      {"取消置顶会话", fun test_unpin_conversation/0},
      {"获取置顶列表", fun test_get_pinned_list/0},
      {"删除单聊会话", fun test_delete_c2c_conversation/0},
      {"删除群聊会话", fun test_delete_c2g_conversation/0},
      {"恢复已删除会话", fun test_restore_deleted_conversation/0},
      {"置顶后删除会话", fun test_pin_then_delete/0},
      {"批量置顶操作", fun test_batch_pin/0},
      {"会话列表排序（置顶优先）", fun test_conversation_list_with_pin/0}
     ]
    }.

setup() ->
    _ = eunit_runner:eunit_setup(),
    application:set_env(imboy, env, test),
    % 创建测试用户
    {ok, User1} = create_test_user(<<"user1_conv">>),
    {ok, User2} = create_test_user(<<"user2_conv">>),
    {ok, User3} = create_test_user(<<"user3_conv">>),
    % 创建好友关系
    ok = ensure_friends(User1, User2),
    ok = ensure_friends(User1, User3),
    % 创建测试群组
    {ok, Group1} = create_test_group(User1, <<"conv_test_group1">>),
    {ok, Group2} = create_test_group(User1, <<"conv_test_group2">>),
    Context = #{
        user1 => User1,
        user2 => User2,
        user3 => User3,
        group1 => Group1,
        group2 => Group2
    },
    persistent_term:put({?MODULE, test_context}, Context),
    Context.

cleanup(_Context) ->
    persistent_term:erase({?MODULE, test_context}),
    ok.

%% ===================================================================
%% 测试用例
%% ===================================================================

test_pin_c2c_conversation() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    User2 = maps:get(user2, Context),

    % 1. 置顶会话
    ok = conversation_pin_logic:pin(User1, integer_to_binary(User2), <<"c2c">>),

    % 2. 验证置顶状态
    true = conversation_pin_logic:is_pinned(User1, integer_to_binary(User2), <<"c2c">>),

    % 3. 再次置顶（幂等性测试）
    ok = conversation_pin_logic:pin(User1, integer_to_binary(User2), <<"c2c">>),

    ok.

test_pin_c2g_conversation() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    Group1 = maps:get(group1, Context),

    % 1. 置顶群聊会话
    ok = conversation_pin_logic:pin(User1, integer_to_binary(Group1), <<"c2g">>),

    % 2. 验证置顶状态
    true = conversation_pin_logic:is_pinned(User1, integer_to_binary(Group1), <<"c2g">>),

    ok.

test_unpin_conversation() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    User2 = maps:get(user2, Context),

    % 1. 先置顶
    ok = conversation_pin_logic:pin(User1, integer_to_binary(User2), <<"c2c">>),

    % 2. 取消置顶
    ok = conversation_pin_logic:unpin(User1, integer_to_binary(User2), <<"c2c">>),

    % 3. 验证取消成功
    false = conversation_pin_logic:is_pinned(User1, integer_to_binary(User2), <<"c2c">>),

    ok.

test_get_pinned_list() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    User2 = maps:get(user2, Context),
    User3 = maps:get(user3, Context),
    Group1 = maps:get(group1, Context),

    % 1. 置顶多个会话
    ok = conversation_pin_logic:pin(User1, integer_to_binary(User2), <<"c2c">>),
    ok = conversation_pin_logic:pin(User1, integer_to_binary(User3), <<"c2c">>),
    ok = conversation_pin_logic:pin(User1, integer_to_binary(Group1), <<"c2g">>),

    % 2. 获取置顶列表
    {ok, PinnedList} = conversation_pin_logic:list(User1),

    % 3. 验证数量
    ?assertEqual(3, length(PinnedList)),

    ok.

test_delete_c2c_conversation() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    User2 = maps:get(user2, Context),

    % 1. 发送一些消息
    lists:foreach(fun(N) ->
        _ = send_c2c_message(User1, User2, <<N/integer, "测试消息"/utf8>>)
    end, lists:seq(1, 5)),

    % 2. 删除会话（软删除）
    ok = conversation_logic:delete(User1, integer_to_binary(User2), <<"c2c">>),

    % 3. 验证删除状态
    true = conversation_logic:is_deleted(User1, integer_to_binary(User2), <<"c2c">>),

    ok.

test_delete_c2g_conversation() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    Group1 = maps:get(group1, Context),

    % 1. 发送一些群聊消息
    lists:foreach(fun(N) ->
        _ = send_c2g_message(User1, Group1, <<N/integer, "群聊消息"/utf8>>)
    end, lists:seq(1, 3)),

    % 2. 删除会话
    ok = conversation_logic:delete(User1, integer_to_binary(Group1), <<"c2g">>),

    % 3. 验证删除状态
    true = conversation_logic:is_deleted(User1, integer_to_binary(Group1), <<"c2g">>),

    ok.

test_restore_deleted_conversation() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    User2 = maps:get(user2, Context),

    % 1. 删除会话
    ok = conversation_logic:delete(User1, integer_to_binary(User2), <<"c2c">>),

    % 2. 恢复会话
    ok = conversation_logic:restore(User1, integer_to_binary(User2), <<"c2c">>),

    % 3. 验证恢复成功
    false = conversation_logic:is_deleted(User1, integer_to_binary(User2), <<"c2c">>),

    ok.

test_pin_then_delete() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    User2 = maps:get(user2, Context),

    % 1. 置顶会话
    ok = conversation_pin_logic:pin(User1, integer_to_binary(User2), <<"c2c">>),

    % 2. 删除会话（置顶状态应该保持或被清除，取决于业务逻辑）
    ok = conversation_logic:delete(User1, integer_to_binary(User2), <<"c2c">>),

    % 3. 恢复会话
    ok = conversation_logic:restore(User1, integer_to_binary(User2), <<"c2c">>),

    % 4. 验证置顶状态（假设保持）
    true = conversation_pin_logic:is_pinned(User1, integer_to_binary(User2), <<"c2c">>),

    ok.

test_batch_pin() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    User2 = maps:get(user2, Context),
    User3 = maps:get(user3, Context),
    Group1 = maps:get(group1, Context),
    Group2 = maps:get(group2, Context),

    % 1. 批量置顶
    Conversations = [
        {integer_to_binary(User2), <<"c2c">>},
        {integer_to_binary(User3), <<"c2c">>},
        {integer_to_binary(Group1), <<"c2g">>},
        {integer_to_binary(Group2), <<"c2g">>}
    ],

    lists:foreach(fun({ConvId, ConvType}) ->
        ok = conversation_pin_logic:pin(User1, ConvId, ConvType)
    end, Conversations),

    % 2. 验证所有会话都已置顶
    {ok, PinnedList} = conversation_pin_logic:list(User1),
    ?assertEqual(4, length(PinnedList)),

    ok.

test_conversation_list_with_pin() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    User2 = maps:get(user2, Context),
    User3 = maps:get(user3, Context),
    Group1 = maps:get(group1, Context),

    % 1. 发送消息创建会话
    send_c2c_message(User2, User1, <<"消息1"/utf8>>),
    send_c2c_message(User3, User1, <<"消息2"/utf8>>),
    send_c2g_message(User1, Group1, <<"群消息1"/utf8>>),

    % 2. 置顶 User3 的会话
    ok = conversation_pin_logic:pin(User1, integer_to_binary(User3), <<"c2c">>),

    % 3. 获取会话列表
    {ok, ConversationList} = conversation_logic:list(User1, #{limit => 20}),

    % 4. 验证置顶会话在最前面
    [FirstConv | _] = ConversationList,
    ?assertEqual(integer_to_binary(User3), maps:get(<<"conversation_id">>, FirstConv)),
    ?assertEqual(true, maps:get(<<"is_pinned">>, FirstConv)),

    ok.

%% ===================================================================
%% 辅助函数
%% ===================================================================

get_context() ->
    persistent_term:get({?MODULE, test_context}).

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
    Uid = binary_to_integer(imboy_hashid:uid()),
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
    Gid = binary_to_integer(imboy_hashid:uid()),
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
    MsgId = imboy_hashid:uid(),
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
    MsgId = imboy_hashid:uid(),
    MsgData = #{
        <<"payload">> => Content,
        <<"msg_type">> => <<"text">>,
        <<"action">> => <<"send">>,
        <<"created_at">> => elib_dt:millisecond()
    },
    ok = msg_c2g_logic:c2g(MsgId, From, MsgData#{<<"to">> => integer_to_binary(Group)}),
    ok = wait_for_c2g_message(MsgId),
    MsgId.

wait_for_c2c_message(MsgId) ->
    wait_for_c2c_message(MsgId, 100).

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
    wait_for_c2g_message(MsgId, 100).

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
