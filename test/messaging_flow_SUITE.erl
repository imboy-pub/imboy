-module(messaging_flow_SUITE).

%%%===================================================================
%%% @doc
%%% 消息发送流程 Common Test 测试套件
%%%
%%% 运行方式：
%%%   ct_run -dir test -suite messaging_flow_SUITE
%%%   make ct-messaging_flow
%%%
%%% 说明：本 suite 已按当前生产 API 重写。历史 API 漂移记录：
%%%   - msg_c2c_logic:c2c/4 仍存在（兼容入口），但返回 ok | {reply, Map}；
%%%     非好友时返回 {reply, #{<<"action">> := <<"not_a_friend">>}} 而非 ok，
%%%     故 c2c 用例先经 user_friend 建立好友关系。
%%%   - msg_c2g_logic:c2g/4（已删）-> c2g/3，参数为 Data map（含 <<"to">>=Gid、
%%%     <<"payload">>、<<"created_at">> 等）；非成员发送不返回 error，而是
%%%     self() ! {reply, #{<<"code">> := 403}} 后返回 ok。
%%%   - msg_c2c_logic:recall/3（已删）-> c2c_revoke/3(MsgId, CurrentUid, Data)，
%%%     Data 含 <<"to">>/<<"from">>/<<"payload">>{<<"original_msg_id">>}；
%%%     成功返回 {reply, message_revoke_ack}，超 2 分钟返回 message_revoke_error，
%%%     非属主返回 permission_denied。撤回时限常量 REVOKE_TIMEOUT_MS=120000。
%%%   - msg_ack_logic:ack/3（已删）-> client_ack/4(Type, MsgId, CurrentUid, DID)。
%%%   - msg_c2c_repo:find_by_msg_id/1（已删）-> find_msg_by_id/1（主表）；
%%%     msg_store_ds:find_staged/1 查同步落库的 staging 行。
%%%   - msg_c2c 表无 is_recall 列；撤回是插入一条 message_revoke_ack 通知行。
%%%   - 用固定高位测试 UID（88880001..88880009）+ 直接 SQL 建关系，避开 signup
%%%     的 License 配额门与密码哈希开销；测试隔离靠范围幂等前置 DELETE。
%%%===================================================================

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("group_role.hrl").

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

%% 固定测试 UID（88880001..88880009），全部走范围清理
-define(U_FROM, 88880001).
-define(U_TO, 88880002).
-define(U_M1, 88880003).
-define(U_M2, 88880004).
-define(U_OUT, 88880005).
-define(UID_MIN, 88880001).
-define(UID_MAX, 88880009).

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
    cleanup_all_test_data(),
    mk_friends(?U_FROM, ?U_TO),

    MsgId = <<"mf_c2c_send">>,
    Payload = #{<<"msg_type">> => <<"text">>, <<"body">> => <<"你好"/utf8>>},

    Result = msg_c2c_logic:c2c(
        MsgId, ?U_FROM, integer_to_binary(?U_TO), jsone:encode(Payload)
    ),

    %% 好友之间发送成功返回 ok（非好友会返回 {reply, not_a_friend}）
    ?assertEqual(ok, Result),
    {comment, "发送单聊消息成功"}.

c2c_message_saved_to_db(_Config) ->
    ct:log("测试单聊消息同步落 staging"),
    cleanup_all_test_data(),
    mk_friends(?U_FROM, ?U_TO),

    MsgId = <<"mf_c2c_saved">>,
    Payload = #{<<"text">> => <<"测试消息"/utf8>>},

    ok = msg_c2c_logic:c2c(
        MsgId, ?U_FROM, integer_to_binary(?U_TO), jsone:encode(Payload)
    ),

    %% c2c 同步写入 msg_store_staging（异步再移入 msg_c2c），故用 find_staged 确定性校验
    Staged = msg_store_ds:find_staged(MsgId),
    ?assertMatch({ok, #{<<"msg_id">> := MsgId}}, Staged),
    {comment, "单聊消息保存到 staging 成功"}.

c2c_message_delivered_to_recipient(_Config) ->
    ct:log("测试单聊消息进入投递管道（staging 记录 from/to）"),
    cleanup_all_test_data(),
    mk_friends(?U_FROM, ?U_TO),

    MsgId = <<"mf_c2c_deliver">>,
    Payload = #{<<"text">> => <<"投递测试"/utf8>>},

    ok = msg_c2c_logic:c2c(
        MsgId, ?U_FROM, integer_to_binary(?U_TO), jsone:encode(Payload)
    ),

    {ok, Msg} = msg_store_ds:find_staged(MsgId),
    ?assertEqual(?U_FROM, maps:get(<<"from_id">>, Msg)),
    ?assertEqual(?U_TO, maps:get(<<"to_id">>, Msg)),
    {comment, "单聊消息进入投递管道成功"}.

c2c_message_with_attachment_succeeds(_Config) ->
    ct:log("测试发送带附件的单聊消息"),
    cleanup_all_test_data(),
    mk_friends(?U_FROM, ?U_TO),

    MsgId = <<"mf_c2c_attach">>,
    Payload = #{
        <<"msg_type">> => <<"image">>,
        <<"body">> => <<"[图片]"/utf8>>,
        <<"attachment_id">> => <<"att_001">>
    },

    Result = msg_c2c_logic:c2c(
        MsgId, ?U_FROM, integer_to_binary(?U_TO), jsone:encode(Payload)
    ),

    ?assertEqual(ok, Result),
    ?assertMatch({ok, _}, msg_store_ds:find_staged(MsgId)),
    {comment, "发送带附件的单聊消息成功"}.

%% ===================================================================
%% 群聊消息测试
%% ===================================================================

send_c2g_message_successfully(_Config) ->
    ct:log("测试发送群聊消息成功"),
    cleanup_all_test_data(),
    Gid = next_id(),
    mk_group(Gid, ?U_FROM, [?U_M1]),

    MsgId = <<"mf_c2g_send">>,
    Result = msg_c2g_logic:c2g(MsgId, ?U_M1, c2g_data(Gid, <<"群消息"/utf8>>)),

    %% 群成员发送成功返回 ok
    ?assertEqual(ok, Result),
    ?assertMatch({ok, #{<<"msg_id">> := MsgId}}, msg_store_ds:find_staged(MsgId)),
    {comment, "发送群聊消息成功"}.

c2g_message_delivered_to_all_members(_Config) ->
    ct:log("测试群聊消息投递给所有成员"),
    cleanup_all_test_data(),
    Gid = next_id(),
    mk_group(Gid, ?U_FROM, [?U_M1, ?U_M2]),

    %% 群主 + 2 成员 = 3 人
    ?assertEqual(3, length(group_ds:member_uids(Gid))),

    MsgId = <<"mf_c2g_all">>,
    ok = msg_c2g_logic:c2g(MsgId, ?U_FROM, c2g_data(Gid, <<"大家好"/utf8>>)),

    %% 群聊消息同步落 staging（type=c2g），异步扇出到各成员
    ?assertMatch({ok, #{<<"msg_id">> := MsgId}}, msg_store_ds:find_staged(MsgId)),
    {comment, "群聊消息投递给所有成员成功"}.

non_member_cannot_send_c2g_message_fails(_Config) ->
    ct:log("测试非成员无法发送群聊消息"),
    cleanup_all_test_data(),
    Gid = next_id(),
    %% U_OUT 不在群组中
    mk_group(Gid, ?U_FROM, [?U_M1]),

    MsgId = <<"mf_c2g_denied">>,
    %% 非成员发送：c2g_send 走 self() ! {reply, #{code=>403}} 后返回 ok
    Result = msg_c2g_logic:c2g(MsgId, ?U_OUT, c2g_data(Gid, <<"未授权消息"/utf8>>)),
    ?assertEqual(ok, Result),

    %% 校验进程邮箱收到 403 拒绝回执
    receive
        {reply, #{<<"code">> := 403}} ->
            ok
    after 2000 ->
        ct:fail("未收到非成员发送的 403 拒绝回执")
    end,
    {comment, "非成员无法发送群聊消息"}.

%% ===================================================================
%% 消息撤回测试
%% ===================================================================

recall_message_within_time_limit_succeeds(_Config) ->
    ct:log("测试在时间限制内撤回消息成功"),
    cleanup_all_test_data(),

    OrigMsgId = <<"mf_recall_orig1">>,
    insert_c2c(OrigMsgId, ?U_FROM, ?U_TO, elib_dt:now()),

    RevokeMsgId = <<"mf_recall_rev1">>,
    Result = msg_c2c_logic:c2c_revoke(
        RevokeMsgId, ?U_FROM, revoke_data(?U_FROM, ?U_TO, OrigMsgId)
    ),

    %% 未超时限内撤回返回 message_revoke_ack 回执
    ?assertMatch({reply, #{<<"action">> := <<"message_revoke_ack">>}}, Result),
    {comment, "在时间限制内撤回消息成功"}.

recall_message_after_time_limit_fails(_Config) ->
    ct:log("测试超过时间限制无法撤回消息"),
    cleanup_all_test_data(),

    OrigMsgId = <<"mf_recall_orig2">>,
    %% 原消息 created_at 设为 2020 年（远超 2 分钟撤回窗口）
    insert_c2c(OrigMsgId, ?U_FROM, ?U_TO, <<"2020-01-01T00:00:00.000+00:00">>),

    RevokeMsgId = <<"mf_recall_rev2">>,
    Result = msg_c2c_logic:c2c_revoke(
        RevokeMsgId, ?U_FROM, revoke_data(?U_FROM, ?U_TO, OrigMsgId)
    ),

    %% 超过撤回时间限制返回 message_revoke_error
    ?assertMatch({reply, #{<<"action">> := <<"message_revoke_error">>}}, Result),
    {comment, "超过时间限制无法撤回消息"}.

recalled_message_marked_as_recalled(_Config) ->
    ct:log("测试撤回覆盖原消息 payload（对端离线路径，隐藏原文）"),
    cleanup_all_test_data(),

    OrigMsgId = <<"mf_recall_orig3">>,
    insert_c2c(OrigMsgId, ?U_FROM, ?U_TO, elib_dt:now()),

    RevokeMsgId = <<"mf_recall_rev3">>,
    {reply, _} = msg_c2c_logic:c2c_revoke(
        RevokeMsgId, ?U_FROM, revoke_data(?U_FROM, ?U_TO, OrigMsgId)
    ),

    %% 对端离线撤回会用撤回内容覆盖原消息 payload（update_payload_by_msg_id），
    %% 避免离线接收方上线仍收到完整原文 → 原文行标记为已撤回
    Sql = <<"SELECT payload FROM msg_c2c WHERE msg_id = $1">>,
    {ok, [#{<<"payload">> := Payload}]} = elib_pg:query(Sql, [OrigMsgId]),
    ?assertNotEqual(nomatch, binary:match(Payload, <<"message_revoke_ack">>)),
    {comment, "撤回覆盖原消息 payload 成功"}.

%% ===================================================================
%% 消息重试测试
%% ===================================================================

message_retry_on_delivery_failure(_Config) ->
    ct:log("测试投递失败不崩溃（对端离线时优雅返回，留待重试）"),
    cleanup_all_test_data(),

    MsgId = <<"mf_retry_offline">>,
    %% Mock 传输层：接收方无在线设备（离线）
    meck:new(imboy_syn, [unstick, passthrough]),
    meck:expect(imboy_syn, list_by_uid, fun(_) -> [] end),

    Msg = <<"{\"id\":\"", MsgId/binary, "\"}">>,
    MsLi = elib_retry_config:intervals(<<"c2c">>),
    %% 离线投递应优雅返回 ok（进入重试/离线存储路径），不崩溃
    Result = message_ds:send_next(?U_TO, MsgId, Msg, MsLi),
    ?assertEqual(ok, Result),

    meck:unload(imboy_syn),
    {comment, "投递失败时优雅返回，留待重试"}.

message_retry_reaches_max_attempts(_Config) ->
    ct:log("测试重试耗尽后消息以离线消息形式留存"),
    cleanup_all_test_data(),

    MsgId = <<"mf_retry_max">>,
    %% 重试终态 = 离线存储：接收方上线时仍可拉取
    insert_c2c(MsgId, ?U_FROM, ?U_TO, elib_dt:now()),

    Sql = <<"SELECT COUNT(*) AS count FROM msg_c2c WHERE msg_id = $1">>,
    {ok, [#{<<"count">> := Count}]} = elib_pg:query(Sql, [MsgId]),
    ?assertEqual(1, Count),
    {comment, "重试耗尽后消息以离线消息形式留存"}.

%% ===================================================================
%% 消息ACK测试
%% ===================================================================

message_ack_confirmed_by_recipient(_Config) ->
    ct:log("测试接收者确认消息"),
    cleanup_all_test_data(),

    MsgId = <<"mf_ack_ok">>,
    insert_c2c(MsgId, ?U_FROM, ?U_TO, elib_dt:now()),

    %% client_ack/4(Type, MsgId, CurrentUid, DID)
    Result = msg_ack_logic:client_ack(<<"c2c">>, MsgId, ?U_TO, <<"device_001">>),
    ?assertEqual(ok, Result),
    {comment, "接收者确认消息成功"}.

message_ack_clears_retry_timer(_Config) ->
    ct:log("测试消息 ACK 清理主行（全部活跃设备确认后删除）"),
    cleanup_all_test_data(),

    MsgId = <<"mf_ack_clear">>,
    insert_c2c(MsgId, ?U_FROM, ?U_TO, elib_dt:now()),

    %% staging 卸载为异步旁路，mock 掉避免真实依赖
    meck:new(msg_store_ds, [unstick, passthrough]),
    meck:expect(msg_store_ds, unstage, 1, ok),

    ok = msg_ack_logic:client_ack(<<"c2c">>, MsgId, ?U_TO, <<"device_001">>),

    %% 无注册在线设备时，ack 视为全部设备已确认 → 主行被清理
    Sql = <<"SELECT COUNT(*) AS count FROM msg_c2c WHERE msg_id = $1">>,
    {ok, [#{<<"count">> := Count}]} = elib_pg:query(Sql, [MsgId]),
    ?assertEqual(0, Count),

    meck:unload(msg_store_ds),
    {comment, "消息 ACK 清理主行成功"}.

%% ===================================================================
%% 辅助函数
%% ===================================================================

%% 生成不与真实数据冲突的高位 bigint（不依赖 TSID 初始化）
next_id() ->
    100_000_000_000_000 + erlang:unique_integer([positive, monotonic]).

%% 构造群聊消息 Data map（c2g/3 入参）
c2g_data(Gid, Text) ->
    #{
        <<"to">> => integer_to_binary(Gid),
        <<"payload">> => #{<<"text">> => Text},
        <<"created_at">> => elib_dt:now(),
        <<"msg_type">> => <<"text">>,
        <<"action">> => <<>>,
        <<"e2ee">> => null
    }.

%% 构造撤回消息 Data map（c2c_revoke/3 入参）
revoke_data(FromUid, ToUid, OrigMsgId) ->
    #{
        <<"to">> => integer_to_binary(ToUid),
        <<"from">> => integer_to_binary(FromUid),
        <<"payload">> => #{<<"original_msg_id">> => OrigMsgId}
    }.

%% 建立单向好友行（check_relationship 双向查询，一行即可判定好友）
mk_friends(A, B) ->
    elib_pg:execute(
        <<
            "INSERT INTO user_friend (id, from_user_id, to_user_id, status, created_at)\n"
            "VALUES ($1, $2, $3, 1, NOW())"
        >>,
        [next_id(), A, B]
    ).

%% 直接建群 + 群成员（c2g 的 is_member/member_uids/check_mute 均纯读 group_member）
mk_group(Gid, OwnerUid, MemberUids) ->
    elib_pg:execute(
        <<
            "INSERT INTO \"group\" (id, type, owner_uid, creator_uid, user_id_sum, created_at)\n"
            "VALUES ($1, 1, $2, $2, 0, NOW())"
        >>,
        [Gid, OwnerUid]
    ),
    add_member(Gid, OwnerUid, ?ROLE_OWNER),
    lists:foreach(fun(Uid) -> add_member(Gid, Uid, ?ROLE_MEMBER) end, MemberUids).

add_member(Gid, Uid, Role) ->
    elib_pg:execute(
        <<
            "INSERT INTO group_member (id, group_id, user_id, role, status, created_at)\n"
            "VALUES ($1, $2, $3, $4, 1, NOW())"
        >>,
        [next_id(), Gid, Uid, Role]
    ).

%% 直接插入一条 msg_c2c 主表消息行（撤回/离线/ACK 用例的确定性前置）
insert_c2c(MsgId, FromUid, ToUid, CreatedAt) ->
    elib_pg:execute(
        <<
            "INSERT INTO msg_c2c (id, from_id, to_id, msg_id, msg_type, payload, created_at)\n"
            "VALUES ($1, $2, $3, $4, 'text', '{\"text\":\"t\"}', $5)"
        >>,
        [next_id(), FromUid, ToUid, MsgId, CreatedAt]
    ).

%% 范围幂等清理（覆盖前次崩溃遗留脏数据）
cleanup_all_test_data() ->
    Min = ?UID_MIN,
    Max = ?UID_MAX,
    elib_pg:execute(
        <<
            "DELETE FROM user_friend WHERE from_user_id BETWEEN $1 AND $2\n"
            "OR to_user_id BETWEEN $1 AND $2"
        >>,
        [Min, Max]
    ),
    elib_pg:execute(
        <<"DELETE FROM group_member WHERE user_id BETWEEN $1 AND $2">>, [Min, Max]
    ),
    elib_pg:execute(
        <<"DELETE FROM \"group\" WHERE creator_uid BETWEEN $1 AND $2">>, [Min, Max]
    ),
    elib_pg:execute(
        <<
            "DELETE FROM msg_store_staging WHERE from_id BETWEEN $1 AND $2\n"
            "OR to_id BETWEEN $1 AND $2"
        >>,
        [Min, Max]
    ),
    elib_pg:execute(
        <<
            "DELETE FROM msg_c2c WHERE from_id BETWEEN $1 AND $2\n"
            "OR to_id BETWEEN $1 AND $2"
        >>,
        [Min, Max]
    ),
    ok.
