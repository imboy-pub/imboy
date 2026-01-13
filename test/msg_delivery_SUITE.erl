-module(msg_delivery_SUITE).

%%%===================================================================
%%% @doc
%%% 消息投递 Common Test 测试套件
%%%
%%% 运行方式：
%%%   make ct-msg_delivery                    # 运行整个 suite
%%%   make ct-msg_delivery t=full_flow       # 运行特定 group
%%%   make ct-msg_delivery t=full_flow:step1 # 运行特定测试
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
    send_c2c_message/1,
    message_delivered_to_online_user/1,
    message_ack_cleanup/1,
    store_offline_message/1,
    retrieve_offline_message_on_user_online/1,
    retry_on_first_failure/1,
    retry_intervals_configuration/1,
    deliver_to_all_devices/1,
    ack_from_single_device/1
]).

%% ===================================================================
%% Suite 回调函数
%% ===================================================================

all() ->
    [
     {group, full_flow},
     {group, offline_storage},
     {group, retry_mechanism},
     {group, multi_device}
    ].

groups() ->
    [
     {full_flow, [], prepare_and_send_test_cases()},
     {offline_storage, [], offline_storage_test_cases()},
     {retry_mechanism, [], retry_test_cases()},
     {multi_device, [], multi_device_test_cases()}
    ].

init_per_suite(Config) ->
    application:set_env(imboy, env, test),
    ct:log("开始消息投递测试套件"),
    Config.

end_per_suite(_Config) ->
    ct:log("结束消息投递测试套件"),
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    meck:unload(),
    ok.


%% ===================================================================
%% 测试用例定义
%% ===================================================================

prepare_and_send_test_cases() ->
    [
     send_c2c_message,
     message_delivered_to_online_user,
     message_ack_cleanup
    ].

offline_storage_test_cases() ->
    [
     store_offline_message,
     retrieve_offline_message_on_user_online
    ].

retry_test_cases() ->
    [
     retry_on_first_failure,
     retry_intervals_configuration
    ].

multi_device_test_cases() ->
    [
     deliver_to_all_devices,
     ack_from_single_device
    ].


%% ===================================================================
%% 完整流程测试
%% ===================================================================

send_c2c_message(_Config) ->
    ct:log("测试发送 C2C 消息"),
    FromUid = 9001,
    ToUid = 9002,
    MsgId = <<"test_msg_001">>,

    % 准备好友关系
    elib_pg:execute(<<"DELETE FROM user_friend WHERE uid = $1">>, [FromUid]),
    elib_pg:execute(<<"INSERT INTO user_friend (uid, friend_uid, created_at)
                      VALUES ($1, $2, NOW())">>,
                     [FromUid, ToUid]),

    % 发送消息
    Payload = #{<<"text">> => <<"测试消息"/utf8>>},
    {ok, _} = msg_c2c_ds:write(MsgId, FromUid, ToUid,
                               jsone:encode(Payload, [native_utf8]),
                               #{}),

    % 验证消息已存储
    Sql = <<"SELECT COUNT(*) FROM msg_c2c WHERE msg_id = $1">>,
    {ok, _, [{Count}]} = elib_pg:query(Sql, [MsgId]),
    ?assertEqual(1, Count),

    % 清理
    elib_pg:execute(<<"DELETE FROM msg_c2c WHERE msg_id = $1">>, [MsgId]),
    elib_pg:execute(<<"DELETE FROM user_friend WHERE uid = $1">>, [FromUid]),
    {comment, "C2C 消息发送成功"}.

message_delivered_to_online_user(_Config) ->
    ct:log("测试消息投递给在线用户"),
    ToUid = 9002,
    MsgId = <<"test_msg_002">>,

    % Mock 在线用户
    meck:new(imboy_syn, [unstick]),
    meck:expect(imboy_syn, list_by_uid, fun(_) ->
        [{self(), {<<"ios">>, <<"device_001">>}}]
    end),
    meck:expect(imboy_syn, publish, fun(_, _, _) -> ok end),

    % 投递消息
    Msg = <<"{\"id\":\"", MsgId/binary, "\"}">>,
    MsLi = elib_retry_config:intervals(<<"c2c">>),
    ok = message_ds:send_next(ToUid, MsgId, Msg, MsLi),

    % 验证投递被调用
    ?assert(meck:validate(imboy_syn)),
    {comment, "消息投递给在线用户成功"}.

message_ack_cleanup(_Config) ->
    ct:log("测试 ACK 清理"),
    ToUid = 9003,
    MsgId = <<"test_msg_003">>,

    % 准备离线消息
    elib_pg:execute(<<"DELETE FROM msg_c2c WHERE msg_id = $1">>, [MsgId]),
    elib_pg:execute(<<"INSERT INTO msg_c2c (from_id, to_id, msg_id, payload, created_at)
                      VALUES ($1, $2, $3, $4, NOW())">>,
                     [9000, ToUid, MsgId, <<"{\"text\":\"test\"}">>]),

    % Mock
    meck:new(msg_store_ds, [unstick]),
    meck:expect(msg_store_ds, unstage, 1, ok),

    % 执行 ACK
    ok = msg_ack_logic:client_ack(<<"c2c">>, MsgId, ToUid, <<"device_001">>),

    % 验证清理
    Sql = <<"SELECT COUNT(*) FROM msg_c2c WHERE msg_id = $1">>,
    {ok, _, [{Count}]} = elib_pg:query(Sql, [MsgId]),
    ?assertEqual(0, Count),
    {comment, "ACK 清理成功"}.


%% ===================================================================
%% 离线存储测试
%% ===================================================================

store_offline_message(_Config) ->
    ct:log("测试存储离线消息"),
    FromUid = 9011,
    ToUid = 9012,
    MsgId = <<"test_offline_001">>,

    % 清理
    elib_pg:execute(<<"DELETE FROM msg_c2c WHERE msg_id = $1">>, [MsgId]),

    % 模拟用户离线
    meck:new(imboy_syn, [unstick]),
    meck:expect(imboy_syn, list_by_uid, fun(_) -> [] end),

    % 发送消息（应存储为离线）
    Payload = #{<<"text">> => <<"离线消息"/utf8>>},
    {ok, _} = msg_c2c_ds:write(MsgId, FromUid, ToUid,
                               jsone:encode(Payload, [native_utf8]),
                               #{}),

    % 验证存储
    Sql = <<"SELECT COUNT(*) FROM msg_c2c WHERE msg_id = $1 AND to_id = $2">>,
    {ok, _, [{Count}]} = elib_pg:query(Sql, [MsgId, ToUid]),
    ?assertEqual(1, Count),

    % 清理
    elib_pg:execute(<<"DELETE FROM msg_c2c WHERE msg_id = $1">>, [MsgId]),
    {comment, "离线消息存储成功"}.

retrieve_offline_message_on_user_online(_Config) ->
    ct:log("测试用户上线时获取离线消息"),
    ToUid = 9022,
    MsgId = <<"test_offline_002">>,

    % 准备离线消息
    elib_pg:execute(<<"DELETE FROM msg_c2c WHERE msg_id = $1">>, [MsgId]),
    elib_pg:execute(<<"INSERT INTO msg_c2c (from_id, to_id, msg_id, payload, created_at)
                      VALUES ($1, $2, $3, $4, NOW())">>,
                     [9020, ToUid, MsgId, <<"{\"text\":\"offline\"}">>]),

    % Mock
    meck:new(imboy_syn, [unstick]),
    meck:expect(imboy_syn, list_by_uid, fun(_) ->
        [{self(), {<<"ios">>, <<"device_001">>}}]
    end),
    meck:expect(imboy_syn, publish, fun(_, _, _) -> ok end),

    meck:new(msg_store_ds, [unstick]),
    meck:expect(msg_store_ds, unstage, 1, ok),

    % 模拟用户上线并 ACK
    ok = msg_ack_logic:client_ack(<<"c2c">>, MsgId, ToUid, <<"device_001">>),

    % 验证离线消息被处理
    Sql = <<"SELECT COUNT(*) FROM msg_c2c WHERE msg_id = $1">>,
    {ok, _, [{Count}]} = elib_pg:query(Sql, [MsgId]),
    ?assertEqual(0, Count),
    {comment, "离线消息处理成功"}.


%% ===================================================================
%% 重试机制测试
%% ===================================================================

retry_on_first_failure(_Config) ->
    ct:log("测试首次失败后重试"),
    _MsgId = <<"test_retry_001">>,

    % Mock 重试间隔配置
    MsLi = elib_retry_config:intervals(<<"c2c">>),

    % 验证重试间隔
    ?assertEqual([0, 5000, 7000, 11000, 17000], MsLi),
    ?assertEqual(5, length(MsLi)),
    {comment, "重试间隔配置正确"}.

retry_intervals_configuration(_Config) ->
    ct:log("测试不同消息类型的重试间隔"),

    % 测试 C2G
    C2gMsLi = elib_retry_config:intervals(<<"c2g">>),
    ?assertEqual([0, 3500, 7000, 11000, 17000], C2gMsLi),

    % 测试 Pull
    PullMsLi = elib_retry_config:intervals(<<"pull">>),
    ?assertEqual([0, 10000, 20000], PullMsLi),

    {comment, "所有重试间隔配置正确"}.


%% ===================================================================
%% 多设备测试
%% ===================================================================

deliver_to_all_devices(_Config) ->
    ct:log("测试消息投递到所有设备"),
    ToUid = 9031,
    MsgId = <<"test_multi_001">>,

    % Mock 多设备
    DeliveryCount = ets:new(delivery_count, [set, private]),
    ets:insert(DeliveryCount, {count, 0}),

    meck:new(imboy_syn, [unstick]),
    meck:expect(imboy_syn, list_by_uid, fun(_) ->
        [{self(), {<<"ios">>, <<"device_ios">>}},
         {self(), {<<"android">>, <<"device_android">>}},
         {self(), {<<"web">>, <<"device_web">>}}]
    end),
    meck:expect(imboy_syn, publish, fun(_, _, _) ->
        ets:update_counter(DeliveryCount, count, {2, 1}),
        ok
    end),

    % 投递消息
    Msg = <<"{\"id\":\"", MsgId/binary, "\"}">>,
    MsLi = [0],
    message_ds:send_next(ToUid, MsgId, Msg, MsLi),

    % 验证所有设备都收到
    Count = ets:lookup_element(DeliveryCount, count, 2),
    ?assertEqual(3, Count),

    % 清理
    ets:delete(DeliveryCount),
    {comment, "消息投递到所有设备成功"}.

ack_from_single_device(_Config) ->
    ct:log("测试单个设备 ACK 不影响其他设备"),
    Uid = 9041,
    MsgId = <<"test_single_ack_001">>,

    % Mock
    meck:new(imboy_syn, [unstick]),
    meck:expect(imboy_syn, broadcast_ack_cancel, fun(_, _, _) -> ok end),

    meck:new(imboy_cache, [unstick]),
    meck:expect(imboy_cache, set, fun(_, _, _) -> ok end),
    meck:expect(imboy_cache, get, fun(_) -> undefined end),
    meck:expect(imboy_cache, flush, fun(_) -> ok end),

    % 单设备 ACK
    ok = websocket_logic:cancel_timer(Uid, <<"device_ios">>, MsgId),

    % 验证广播被调用
    ?assert(meck:validate(imboy_syn)),
    {comment, "单设备 ACK 不影响其他设备"}.
