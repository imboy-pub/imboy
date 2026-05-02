%% @doc 消息转发功能集成测试
%% 测试范围：
%% - 单聊消息转发到单聊
%% - 单聊消息转发到群聊
%% - 群聊消息转发到单聊
%% - 群聊消息转发到群聊
%% - 批量转发
%% - 转发记录溯源
-module(msg_forward_integration_tests).

-include("error_code.hrl").
-include_lib("eunit/include/eunit.hrl").

%% 测试夹具
msg_forward_test_() ->
    _ = eunit_runner:eunit_setup(),
    application:set_env(imboy, env, test),
    case eunit_runner:eunit_try_db() of
        {ok, _Driver, _Conn} ->
            {foreach,
             fun setup/0,
             fun cleanup/1,
             [
              {"单聊消息转发到单聊", fun test_c2c_to_c2c_forward/0},
              {"单聊消息转发到群聊", fun test_c2c_to_c2g_forward/0},
              {"群聊消息转发到单聊", fun test_c2g_to_c2c_forward/0},
              {"群聊消息转发到群聊", fun test_c2g_to_c2g_forward/0},
              {"批量转发消息", fun test_batch_forward/0},
              {"转发记录溯源", fun test_forward_trace/0},
              {"非好友转发失败", fun test_forward_to_non_friend/0},
              {"非群成员转发失败", fun test_forward_to_non_group_member/0},
              {"转发不存在消息失败", fun test_forward_nonexistent_message/0},
              {"批量转发数量限制", fun test_batch_forward_limit/0}
             ]
            };
        {error, _Reason} ->
            {"Database not available",
             fun() -> {skip, "Database not available"} end}
    end.

setup() ->
    % 创建测试用户
    {ok, User1} = create_test_user(<<"user1_forward">>),
    {ok, User2} = create_test_user(<<"user2_forward">>),
    {ok, User3} = create_test_user(<<"user3_forward">>),
    % 创建好友关系
    ok = ensure_friends(User1, User2),
    ok = ensure_friends(User1, User3),
    % 创建测试群组
    {ok, Group} = create_test_group(User1, <<"forward_test_group">>),
    ok = group_member_ds:add_member(Group, User2),
    Context = #{user1 => User1, user2 => User2, user3 => User3, group => Group},
    persistent_term:put({?MODULE, test_context}, Context),
    Context.

cleanup(_Context) ->
    persistent_term:erase({?MODULE, test_context}),
    ok.

%% ===================================================================
%% 测试用例
%% ===================================================================

test_c2c_to_c2c_forward() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    User2 = maps:get(user2, Context),
    User3 = maps:get(user3, Context),

    % 1. 发送一条单聊消息
    MsgId = integer_to_binary(elib_tsid:generate()),
    MsgData = #{
        <<"payload">> => <<"测试转发消息"/utf8>>,
        <<"msg_type">> => <<"text">>,
        <<"action">> => <<"send">>,
        <<"created_at">> => elib_dt:millisecond()
    },
    ok = msg_c2c_logic:c2c(MsgId, User1, MsgData#{<<"to">> => integer_to_binary(User2)}),
    ok = wait_for_source_message(MsgId),

    % 2. 转发到另一个单聊
    {ok, ForwardMsgIds} = msg_forward_logic:forward([MsgId], User1, User3, <<"c2c">>),

    % 3. 验证转发成功
    ?assertEqual(1, length(ForwardMsgIds)),

    % 4. 验证转发记录
    [ForwardMsgId | _] = ForwardMsgIds,
    {ok, Record} = find_forward_record(MsgId, ForwardMsgId),
    ?assertEqual(ForwardMsgId, maps:get(<<"forward_msg_id">>, Record)).

test_c2c_to_c2g_forward() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    User2 = maps:get(user2, Context),
    Group = maps:get(group, Context),

    % 1. 发送一条单聊消息
    MsgId = integer_to_binary(elib_tsid:generate()),
    MsgData = #{
        <<"payload">> => <<"转发到群聊"/utf8>>,
        <<"msg_type">> => <<"text">>,
        <<"action">> => <<"send">>,
        <<"created_at">> => elib_dt:millisecond()
    },
    ok = msg_c2c_logic:c2c(MsgId, User1, MsgData#{<<"to">> => integer_to_binary(User2)}),
    ok = wait_for_source_message(MsgId),

    % 2. 转发到群聊
    {ok, ForwardMsgIds} = msg_forward_logic:forward([MsgId], User1, Group, <<"c2g">>),

    % 3. 验证转发成功
    ?assertEqual(1, length(ForwardMsgIds)).

test_c2g_to_c2c_forward() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    User2 = maps:get(user2, Context),
    Group = maps:get(group, Context),

    % 1. 发送一条群聊消息
    MsgId = integer_to_binary(elib_tsid:generate()),
    MsgData = #{
        <<"payload">> => <<"群聊消息转发到单聊"/utf8>>,
        <<"msg_type">> => <<"text">>,
        <<"action">> => <<"send">>,
        <<"created_at">> => elib_dt:millisecond()
    },
    ok = msg_c2g_logic:c2g(MsgId, User1, MsgData#{<<"to">> => integer_to_binary(Group)}),
    ok = wait_for_source_message(MsgId),

    % 2. 转发到单聊
    {ok, ForwardMsgIds} = msg_forward_logic:forward([MsgId], User1, User2, <<"c2c">>),

    % 3. 验证转发成功
    ?assertEqual(1, length(ForwardMsgIds)).

test_c2g_to_c2g_forward() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    Group = maps:get(group, Context),

    % 1. 创建另一个群组
    {ok, Group2} = create_test_group(User1, <<"forward_test_group2">>),

    % 2. 发送一条群聊消息
    MsgId = integer_to_binary(elib_tsid:generate()),
    MsgData = #{
        <<"payload">> => <<"群聊到群聊转发"/utf8>>,
        <<"msg_type">> => <<"text">>,
        <<"action">> => <<"send">>,
        <<"created_at">> => elib_dt:millisecond()
    },
    ok = msg_c2g_logic:c2g(MsgId, User1, MsgData#{<<"to">> => integer_to_binary(Group)}),
    ok = wait_for_source_message(MsgId),

    % 3. 转发到另一个群聊
    {ok, ForwardMsgIds} = msg_forward_logic:forward([MsgId], User1, Group2, <<"c2g">>),

    % 4. 验证转发成功
    ?assertEqual(1, length(ForwardMsgIds)).

test_batch_forward() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    User2 = maps:get(user2, Context),
    User3 = maps:get(user3, Context),

    % 1. 发送多条消息
    MsgIds = lists:map(fun(N) ->
        MsgId = integer_to_binary(elib_tsid:generate()),
        MsgData = #{
            <<"payload">> => <<N/integer, "批量转发消息"/utf8>>,
            <<"msg_type">> => <<"text">>,
            <<"action">> => <<"send">>,
            <<"created_at">> => elib_dt:millisecond()
        },
        ok = msg_c2c_logic:c2c(MsgId, User1, MsgData#{<<"to">> => integer_to_binary(User2)}),
        MsgId
    end, lists:seq(1, 5)),
    ok = wait_for_source_messages(MsgIds),

    % 2. 批量转发
    {ok, ForwardMsgIds} = msg_forward_logic:forward(MsgIds, User1, User3, <<"c2c">>),

    % 3. 验证转发数量
    ?assertEqual(5, length(ForwardMsgIds)).

test_forward_trace() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    User2 = maps:get(user2, Context),
    User3 = maps:get(user3, Context),

    % 1. 发送原始消息
    MsgId = integer_to_binary(elib_tsid:generate()),
    MsgData = #{
        <<"payload">> => <<"溯源测试"/utf8>>,
        <<"msg_type">> => <<"text">>,
        <<"action">> => <<"send">>,
        <<"created_at">> => elib_dt:millisecond()
    },
    ok = msg_c2c_logic:c2c(MsgId, User1, MsgData#{<<"to">> => integer_to_binary(User2)}),
    ok = wait_for_source_message(MsgId),

    % 2. 转发消息
    {ok, [ForwardMsgId | _]} = msg_forward_logic:forward([MsgId], User1, User3, <<"c2c">>),

    % 3. 查询转发记录
    {ok, Record} = find_forward_record(MsgId, ForwardMsgId),

    % 4. 验证溯源信息
    ?assertEqual(MsgId, maps:get(<<"original_msg_id">>, Record)),
    ?assertEqual(User1, maps:get(<<"original_from_id">>, Record)),
    ?assertEqual(User2, maps:get(<<"original_to_id">>, Record)),
    ?assertEqual(<<"c2c">>, maps:get(<<"original_type">>, Record)),
    ?assertEqual(ForwardMsgId, maps:get(<<"forward_msg_id">>, Record)),
    ?assertEqual(User1, maps:get(<<"forward_from_id">>, Record)),
    ?assertEqual(User3, maps:get(<<"forward_to_id">>, Record)),
    ?assertEqual(<<"c2c">>, maps:get(<<"forward_type">>, Record)).

test_forward_to_non_friend() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    User2 = maps:get(user2, Context),

    % 1. 先向现有好友发送一条原始消息
    MsgId = integer_to_binary(elib_tsid:generate()),
    MsgData = #{
        <<"payload">> => <<"非好友转发测试"/utf8>>,
        <<"msg_type">> => <<"text">>,
        <<"action">> => <<"send">>,
        <<"created_at">> => elib_dt:millisecond()
    },
    ok = msg_c2c_logic:c2c(MsgId, User1, MsgData#{<<"to">> => integer_to_binary(User2)}),
    ok = wait_for_source_message(MsgId),

    % 2. 转发到一个未建立好友关系的新用户
    {ok, User4} = create_test_user(<<"user4_forward">>),
    Result = msg_forward_logic:forward([MsgId], User1, User4, <<"c2c">>),

    % 3. 验证转发失败
    ?assertMatch({error, {not_friends, _}}, Result).

test_forward_to_non_group_member() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    User2 = maps:get(user2, Context),

    % 1. 创建一个新群组，User2 不是成员
    {ok, Group2} = create_test_group(User1, <<"non_member_group">>),

    % 2. 发送消息
    MsgId = integer_to_binary(elib_tsid:generate()),
    MsgData = #{
        <<"payload">> => <<"非群成员转发测试"/utf8>>,
        <<"msg_type">> => <<"text">>,
        <<"action">> => <<"send">>,
        <<"created_at">> => elib_dt:millisecond()
    },
    ok = msg_c2c_logic:c2c(MsgId, User2, MsgData#{<<"to">> => integer_to_binary(User1)}),
    ok = wait_for_source_message(MsgId),

    % 3. User2 尝试转发到 Group2（不是群成员）
    Result = msg_forward_logic:forward([MsgId], User2, Group2, <<"c2g">>),

    % 4. 验证转发失败
    ?assertMatch({error, {not_group_member, _}}, Result).

test_forward_nonexistent_message() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    User3 = maps:get(user3, Context),

    % 尝试转发不存在的消息
    FakeMsgId = <<"nonexistent_msg_id">>,
    Result = msg_forward_logic:forward([FakeMsgId], User1, User3, <<"c2c">>),

    % 验证转发失败
    ?assertMatch({error, {msg_not_found, _}}, Result).

test_batch_forward_limit() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    User2 = maps:get(user2, Context),
    User3 = maps:get(user3, Context),

    % 1. 创建超过限制的消息数量（假设限制是10条）
    MsgIds = lists:map(fun(N) ->
        MsgId = integer_to_binary(elib_tsid:generate()),
        MsgData = #{
            <<"payload">> => <<N/integer, "超限转发消息"/utf8>>,
            <<"msg_type">> => <<"text">>,
            <<"action">> => <<"send">>,
            <<"created_at">> => elib_dt:millisecond()
        },
        ok = msg_c2c_logic:c2c(MsgId, User1, MsgData#{<<"to">> => integer_to_binary(User2)}),
        MsgId
    end, lists:seq(1, 15)),

    % 2. 尝试批量转发超过限制的消息
    Result = msg_forward_logic:forward(MsgIds, User1, User3, <<"c2c">>),

    % 3. 验证转发失败
    ?assertMatch({error, {invalid_param, _, _, _}}, Result).

%% ===================================================================
%% 辅助函数
%% ===================================================================

get_context() ->
    % 从进程字典获取测试上下文
    persistent_term:get({?MODULE, test_context}).

wait_for_source_messages(MsgIds) ->
    lists:foreach(fun wait_for_source_message/1, MsgIds),
    ok.

wait_for_source_message(MsgId) ->
    wait_for_source_message(MsgId, 100).

wait_for_source_message(_MsgId, 0) ->
    error(source_message_not_ready);
wait_for_source_message(MsgId, AttemptsLeft) ->
    case source_message_ready(MsgId) of
        true ->
            ok;
        false ->
            timer:sleep(50),
            wait_for_source_message(MsgId, AttemptsLeft - 1)
    end.

source_message_ready(MsgId) ->
    case msg_c2c_ds:find_msg_by_id(MsgId) of
        {ok, _Msg} ->
            true;
        {error, not_found} ->
            case msg_c2g_timeline_repo:find_by_msg_id(MsgId) of
                {ok, [_ | _]} -> true;
                _ -> false
            end;
        _ ->
            false
    end.

find_forward_record(OriginalMsgId, ForwardMsgId) ->
    case msg_forward_repo:find_by_original_msg_id(OriginalMsgId) of
        {ok, Records} ->
            case lists:filter(
                fun(Record) ->
                    maps:get(<<"forward_msg_id">>, Record, undefined) =:= ForwardMsgId
                end,
                Records
            ) of
                [Record | _] -> {ok, Record};
                [] -> {error, not_found}
            end;
        Error ->
            Error
    end.

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
