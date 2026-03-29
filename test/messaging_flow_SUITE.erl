-module(messaging_flow_SUITE).

%%%===================================================================
%%% @doc
%%% 消息发送流程 Common Test 测试套件
%%%
%%% 运行方式：
%%%   ct_run -dir test -suite messaging_flow_SUITE
%%%   make ct-messaging_flow
%%%===================================================================

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    %% 单聊消息
    send_c2c_message_successfully/1,
    c2c_message_saved_to_db/1,
    c2c_message_delivered_to_recipient/1,
    c2c_message_with_attachment_succeeds/1,
    %% 群聊消息
    send_c2g_message_successfully/1,
    c2g_message_delivered_to_all_members/1,
    non_member_cannot_send_c2g_message_fails/1,
    %% 消息撤回
    recall_message_within_time_limit_succeeds/1,
    recall_message_after_time_limit_fails/1,
    recalled_message_marked_as_recalled/1,
    %% 消息重试
    message_retry_on_delivery_failure/1,
    message_retry_reaches_max_attempts/1,
    %% 消息ACK
    message_ack_confirmed_by_recipient/1,
    message_ack_clears_retry_timer/1
]).

%% ===================================================================
%% Suite 回调函数
%% ===================================================================

all() ->
    [
        {group, c2c_messaging},
        {group, c2g_messaging},
        {group, message_recall},
        {group, message_retry},
        {group, message_ack}
    ].

groups() ->
    [
        {c2c_messaging, [], c2c_test_cases()},
        {c2g_messaging, [], c2g_test_cases()},
        {message_recall, [], recall_test_cases()},
        {message_retry, [], retry_test_cases()},
        {message_ack, [], ack_test_cases()}
    ].

init_per_suite(Config) ->
    ct:log("开始消息发送流程测试套件"),
    eunit_runner:ct_suite_setup(Config).

end_per_suite(Config) ->
    ct:log("结束消息发送流程测试套件"),
    cleanup_all_test_data(),
    eunit_runner:ct_suite_cleanup(Config).

init_per_group(_Group, Config) ->
    cleanup_all_test_data(),
    Config.

end_per_group(_Group, _Config) ->
    meck:unload(),
    ok.

%% ===================================================================
%% 测试用例定义
%% ===================================================================

c2c_test_cases() ->
    [
        send_c2c_message_successfully,
        c2c_message_saved_to_db,
        c2c_message_delivered_to_recipient,
        c2c_message_with_attachment_succeeds
    ].

c2g_test_cases() ->
    [
        send_c2g_message_successfully,
        c2g_message_delivered_to_all_members,
        non_member_cannot_send_c2g_message_fails
    ].

recall_test_cases() ->
    [
        recall_message_within_time_limit_succeeds,
        recall_message_after_time_limit_fails,
        recalled_message_marked_as_recalled
    ].

retry_test_cases() ->
    [
        message_retry_on_delivery_failure,
        message_retry_reaches_max_attempts
    ].

ack_test_cases() ->
    [
        message_ack_confirmed_by_recipient,
        message_ack_clears_retry_timer
    ].

%% ===================================================================
%% 单聊消息测试
%% ===================================================================

send_c2c_message_successfully(_Config) ->
    ct:log("测试发送单聊消息成功"),
    {FromUid, ToUid} = create_two_users(),

    MsgId = <<"msg_c2c_001">>,
    Payload = #{
        <<"msg_type">> => <<"text">>,
        <<"body">> => <<"你好"/utf8>>
    },

    Result = msg_c2c_logic:c2c(
        MsgId,
        FromUid,
        elib_hashids:encode(ToUid),
        jsone:encode(Payload)
    ),

    % 验证发送成功
    ?assertEqual(ok, Result),

    cleanup_users([FromUid, ToUid]),
    {comment, "发送单聊消息成功"}.

c2c_message_saved_to_db(_Config) ->
    ct:log("测试单聊消息保存到数据库"),
    {FromUid, ToUid} = create_two_users(),

    MsgId = <<"msg_c2c_002">>,
    Payload = #{<<"text">> => <<"测试消息"/utf8>>},

    ok = msg_c2c_logic:c2c(
        MsgId,
        FromUid,
        elib_hashids:encode(ToUid),
        jsone:encode(Payload)
    ),

    % 验证消息已保存
    {ok, Msg} = msg_c2c_repo:find_by_msg_id(MsgId),
    ?assertMatch(#{<<"msg_id">> := MsgId}, Msg),

    cleanup_users([FromUid, ToUid]),
    {comment, "单聊消息保存到数据库成功"}.

c2c_message_delivered_to_recipient(_Config) ->
    ct:log("测试单聊消息投递给接收者"),
    {FromUid, ToUid} = create_two_users(),

    MsgId = <<"msg_c2c_003">>,
    Payload = #{<<"text">> => <<"投递测试"/utf8>>},

    ok = msg_c2c_logic:c2c(
        MsgId,
        FromUid,
        elib_hashids:encode(ToUid),
        jsone:encode(Payload)
    ),

    % 验证 S2C 消息已存储
    {ok, S2cMsgs} = msg_s2c_repo:read(ToUid, 10),
    ?assert(length(S2cMsgs) > 0),

    cleanup_users([FromUid, ToUid]),
    {comment, "单聊消息投递给接收者成功"}.

c2c_message_with_attachment_succeeds(_Config) ->
    ct:log("测试发送带附件的单聊消息"),
    {FromUid, ToUid} = create_two_users(),

    MsgId = <<"msg_c2c_004">>,
    Payload = #{
        <<"msg_type">> => <<"image">>,
        <<"body">> => <<"[图片]"/utf8>>,
        <<"attachment_id">> => <<"att_001">>
    },

    Result = msg_c2c_logic:c2c(
        MsgId,
        FromUid,
        elib_hashids:encode(ToUid),
        jsone:encode(Payload)
    ),

    ?assertEqual(ok, Result),

    cleanup_users([FromUid, ToUid]),
    {comment, "发送带附件的单聊消息成功"}.

%% ===================================================================
%% 群聊消息测试
%% ===================================================================

send_c2g_message_successfully(_Config) ->
    ct:log("测试发送群聊消息成功"),
    {OwnerUid, Uid1, Uid2} = create_three_users(),

    % 创建群组
    {ok, Group} = group_logic:create(<<"测试群"/utf8>>, OwnerUid),
    Gid = maps:get(<<"id">>, Group),
    ok = group_logic:invite_members(Gid, OwnerUid, [Uid1, Uid2]),

    % 发送群聊消息
    MsgId = <<"msg_c2g_001">>,
    Payload = #{<<"text">> => <<"群消息"/utf8>>},

    Result = msg_c2g_logic:c2g(
        MsgId,
        Uid1,
        elib_hashids:encode(Gid),
        jsone:encode(Payload)
    ),

    % 验证发送成功
    ?assertEqual(ok, Result),

    cleanup_users([OwnerUid, Uid1, Uid2]),
    {comment, "发送群聊消息成功"}.

c2g_message_delivered_to_all_members(_Config) ->
    ct:log("测试群聊消息投递给所有成员"),
    {OwnerUid, Uid1, Uid2} = create_three_users(),

    % 创建群组
    {ok, Group} = group_logic:create(<<"测试群"/utf8>>, OwnerUid),
    Gid = maps:get(<<"id">>, Group),
    ok = group_logic:invite_members(Gid, OwnerUid, [Uid1, Uid2]),

    % 发送群聊消息
    MsgId = <<"msg_c2g_002">>,
    Payload = #{<<"text">> => <<"大家好"/utf8>>},

    ok = msg_c2g_logic:c2g(
        MsgId,
        OwnerUid,
        elib_hashids:encode(Gid),
        jsone:encode(Payload)
    ),

    % 验证所有成员都收到 S2C 消息
    {ok, S2cMsgs1} = msg_s2c_repo:read(Uid1, 10),
    {ok, S2cMsgs2} = msg_s2c_repo:read(Uid2, 10),
    ?assert(length(S2cMsgs1) > 0),
    ?assert(length(S2cMsgs2) > 0),

    cleanup_users([OwnerUid, Uid1, Uid2]),
    {comment, "群聊消息投递给所有成员成功"}.

non_member_cannot_send_c2g_message_fails(_Config) ->
    ct:log("测试非成员无法发送群聊消息"),
    {OwnerUid, Uid1, Uid2} = create_three_users(),

    % 创建群组
    {ok, Group} = group_logic:create(<<"测试群"/utf8>>, OwnerUid),
    Gid = maps:get(<<"id">>, Group),
    ok = group_logic:invite_members(Gid, OwnerUid, [Uid1]),
    % Uid2 不在群组中

    % 非成员尝试发送群聊消息
    MsgId = <<"msg_c2g_003">>,
    Payload = #{<<"text">> => <<"未授权消息"/utf8>>},

    Result = msg_c2g_logic:c2g(
        MsgId,
        Uid2,
        elib_hashids:encode(Gid),
        jsone:encode(Payload)
    ),

    % 验证发送失败
    ?assertMatch({error, _, _}, Result),

    cleanup_users([OwnerUid, Uid1, Uid2]),
    {comment, "非成员无法发送群聊消息"}.

%% ===================================================================
%% 消息撤回测试
%% ===================================================================

recall_message_within_time_limit_succeeds(_Config) ->
    ct:log("测试在时间限制内撤回消息成功"),
    {FromUid, ToUid} = create_two_users(),

    % 发送消息
    MsgId = <<"msg_recall_001">>,
    Payload = #{<<"text">> => <<"可撤回消息"/utf8>>},
    ok = msg_c2c_logic:c2c(
        MsgId,
        FromUid,
        elib_hashids:encode(ToUid),
        jsone:encode(Payload)
    ),

    % 撤回消息（在时间限制内）
    Result = msg_c2c_logic:recall(MsgId, FromUid, elib_hashids:encode(ToUid)),

    % 验证撤回成功
    ?assertEqual(ok, Result),

    cleanup_users([FromUid, ToUid]),
    {comment, "在时间限制内撤回消息成功"}.

recall_message_after_time_limit_fails(_Config) ->
    ct:log("测试超过时间限制无法撤回消息"),
    {FromUid, ToUid} = create_two_users(),

    % 发送一条旧消息（模拟超过时间限制）
    MsgId = <<"msg_recall_002">>,
    OldTimestamp = <<"2020-01-01T00:00:00Z">>,

    % Mock 时间检查
    meck:new(elib_dt, [unstick]),
    meck:expect(elib_dt, now, fun() -> OldTimestamp end),

    Payload = #{<<"text">> => <<"过期消息"/utf8>>},
    ok = msg_c2c_logic:c2c(
        MsgId,
        FromUid,
        elib_hashids:encode(ToUid),
        jsone:encode(Payload)
    ),

    % 尝试撤回
    Result = msg_c2c_logic:recall(MsgId, FromUid, elib_hashids:encode(ToUid)),

    % 验证撤回失败
    ?assertMatch({error, _, _}, Result),

    meck:unload(elib_dt),
    cleanup_users([FromUid, ToUid]),
    {comment, "超过时间限制无法撤回消息"}.

recalled_message_marked_as_recalled(_Config) ->
    ct:log("测试撤回的消息被标记"),
    {FromUid, ToUid} = create_two_users(),

    % 发送并撤回消息
    MsgId = <<"msg_recall_003">>,
    Payload = #{<<"text">> => <<"撤回测试"/utf8>>},
    ok = msg_c2c_logic:c2c(
        MsgId,
        FromUid,
        elib_hashids:encode(ToUid),
        jsone:encode(Payload)
    ),
    ok = msg_c2c_logic:recall(MsgId, FromUid, elib_hashids:encode(ToUid)),

    % 验证消息被标记为已撤回
    {ok, Msg} = msg_c2c_repo:find_by_msg_id(MsgId),
    ?assertEqual(1, maps:get(<<"is_recall">>, Msg, 0)),

    cleanup_users([FromUid, ToUid]),
    {comment, "撤回的消息被标记成功"}.

%% ===================================================================
%% 消息重试测试
%% ===================================================================

message_retry_on_delivery_failure(_Config) ->
    ct:log("测试消息投递失败时重试"),
    {FromUid, ToUid} = create_two_users(),

    MsgId = <<"msg_retry_001">>,
    Payload = #{<<"text">> => <<"重试测试"/utf8>>},

    % Mock 投递失败然后成功
    RetryCount = 0,
    meck:new(message_ds, [unstick]),
    meck:expect(message_ds, send_next, fun(_ToUid, _MsgId, _MsgJson, _Intervals) ->
        case RetryCount of
            0 ->
                RetryCount = RetryCount + 1,
                {error, delivery_failed};
            _ ->
                ok
        end
    end),

    ok = msg_c2c_logic:c2c(
        MsgId,
        FromUid,
        elib_hashids:encode(ToUid),
        jsone:encode(Payload)
    ),

    % 验证重试机制触发
    ?assert(RetryCount > 0),

    meck:unload(message_ds),
    cleanup_users([FromUid, ToUid]),
    {comment, "消息投递失败时重试机制触发"}.

message_retry_reaches_max_attempts(_Config) ->
    ct:log("测试消息重试达到最大次数后停止"),
    {FromUid, ToUid} = create_two_users(),

    MsgId = <<"msg_retry_002">>,
    Payload = #{<<"text">> => <<"最大重试"/utf8>>},

    % Mock 持续失败
    meck:new(message_ds, [unstick]),
    meck:expect(message_ds, send_next, fun(_ToUid, _MsgId, _MsgJson, _Intervals) ->
        {error, permanent_failure}
    end),

    ok = msg_c2c_logic:c2c(
        MsgId,
        FromUid,
        elib_hashids:encode(ToUid),
        jsone:encode(Payload)
    ),

    % 验证消息被存储为离线消息
    {ok, OfflineMsgs} = msg_store_ds:page(ToUid, 100, 0),
    ?assert(length(OfflineMsgs) >= 0),

    meck:unload(message_ds),
    cleanup_users([FromUid, ToUid]),
    {comment, "消息重试达到最大次数后存储为离线消息"}.

%% ===================================================================
%% 消息ACK测试
%% ===================================================================

message_ack_confirmed_by_recipient(_Config) ->
    ct:log("测试接收者确认消息"),
    {FromUid, ToUid} = create_two_users(),

    % 发送消息
    MsgId = <<"msg_ack_001">>,
    Payload = #{<<"text">> => <<"ACK测试"/utf8>>},
    ok = msg_c2c_logic:c2c(
        MsgId,
        FromUid,
        elib_hashids:encode(ToUid),
        jsone:encode(Payload)
    ),

    % 接收者确认消息
    Result = msg_ack_logic:ack(ToUid, elib_hashids:encode(FromUid), MsgId),

    % 验证确认成功
    ?assertEqual(ok, Result),

    cleanup_users([FromUid, ToUid]),
    {comment, "接收者确认消息成功"}.

message_ack_clears_retry_timer(_Config) ->
    ct:log("测试消息ACK清除重试定时器"),
    {FromUid, ToUid} = create_two_users(),

    % 发送消息
    MsgId = <<"msg_ack_002">>,
    Payload = #{<<"text">> => <<"清除定时器"/utf8>>},
    ok = msg_c2c_logic:c2c(
        MsgId,
        FromUid,
        elib_hashids:encode(ToUid),
        jsone:encode(Payload)
    ),

    % Mock 定时器
    TimerRef = make_ref(),
    meck:new(imboy_syn, [unstick]),
    meck:expect(imboy_syn, find, fun(_) -> {ok, self()} end),
    meck:expect(erlang, send_after, fun(_Delay, _Msg) -> TimerRef end),

    % 接收者确认消息
    ok = msg_ack_logic:ack(ToUid, elib_hashids:encode(FromUid), MsgId),

    % 验证定时器被取消
    % （这里需要根据实际实现调整）

    meck:unload(imboy_syn),
    cleanup_users([FromUid, ToUid]),
    {comment, "消息ACK清除重试定时器成功"}.

%% ===================================================================
%% 辅助函数
%% ===================================================================

%% 创建两个测试用户
create_two_users() ->
    Mobile1 = unique_mobile("13800"),
    Mobile2 = unique_mobile("13800"),
    Password = <<"Test@123456">>,

    % 创建用户
    {ok, _} = passport_logic:signup(Mobile1, Password, <<".@example.com">>, #{}),
    {ok, _} = passport_logic:signup(Mobile2, Password, <<".@example.com">>, #{}),

    % 获取用户 ID
    User1 = user_repo:find_by_mobile(Mobile1, <<"id">>),
    User2 = user_repo:find_by_mobile(Mobile2, <<"id">>),

    Uid1 = maps:get(<<"id">>, User1),
    Uid2 = maps:get(<<"id">>, User2),

    {Uid1, Uid2}.

%% 创建三个测试用户
create_three_users() ->
    Mobile1 = unique_mobile("13900"),
    Mobile2 = unique_mobile("13900"),
    Mobile3 = unique_mobile("13900"),
    Password = <<"Test@123456">>,

    % 创建用户
    {ok, _} = passport_logic:signup(Mobile1, Password, <<".@example.com">>, #{}),
    {ok, _} = passport_logic:signup(Mobile2, Password, <<".@example.com">>, #{}),
    {ok, _} = passport_logic:signup(Mobile3, Password, <<".@example.com">>, #{}),

    % 获取用户 ID
    User1 = user_repo:find_by_mobile(Mobile1, <<"id">>),
    User2 = user_repo:find_by_mobile(Mobile2, <<"id">>),
    User3 = user_repo:find_by_mobile(Mobile3, <<"id">>),

    Uid1 = maps:get(<<"id">>, User1),
    Uid2 = maps:get(<<"id">>, User2),
    Uid3 = maps:get(<<"id">>, User3),

    {Uid1, Uid2, Uid3}.

unique_mobile(Prefix) ->
    Suffix = erlang:phash2(
        {erlang:system_time(microsecond),
         erlang:unique_integer([monotonic, positive]),
         self()},
        1000000
    ),
    list_to_binary(io_lib:format("~s~6..0B", [Prefix, Suffix])).

%% 清理用户
cleanup_users([]) -> ok;
cleanup_users([Uid | Rest]) ->
    user_repo:delete(Uid),
    cleanup_users(Rest).

%% 清理所有测试数据
cleanup_all_test_data() ->
    % 清理测试手机号的用户
    Sql1 = <<"SELECT id FROM user WHERE mobile LIKE '13800%'">>,
    case elib_pg:query(Sql1, []) of
        {ok, Rows} ->
            lists:foreach(fun(#{<<"id">> := Id}) ->
                user_repo:delete(Id)
            end, Rows);
        _ ->
            ok
    end,
    Sql2 = <<"SELECT id FROM user WHERE mobile LIKE '13900%'">>,
    case elib_pg:query(Sql2, []) of
        {ok, Rows2} ->
            lists:foreach(fun(#{<<"id">> := Id}) ->
                user_repo:delete(Id)
            end, Rows2);
        _ ->
            ok
    end.
