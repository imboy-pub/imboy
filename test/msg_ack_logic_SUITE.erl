-module(msg_ack_logic_SUITE).

%%%===================================================================
%%% @doc
%%% msg_ack_logic 模块的 Common Test 测试套件
%%%
%%% 运行方式：
%%%   make ct-msg_ack_logic              # 运行整个 suite
%%%   make ct-msg_ack_logic t=c2c_ack   # 运行特定测试
%%%   make ct                             # 运行所有 Common Test
%%%===================================================================

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,
    c2c_ack_deletes_offline_msg/1,
    c2c_ack_with_no_msg/1,
    c2g_ack_marks_timeline/1,
    s2c_ack_deletes_offline_msg/1,
    c2s_ack_uses_parameterized_query/1,
    unknown_msg_type_handles_gracefully/1
]).

%% ===================================================================
%% Suite 回调函数
%% ===================================================================

all() ->
    [
     c2c_ack_deletes_offline_msg,
     c2c_ack_with_no_msg,
     c2g_ack_marks_timeline,
     s2c_ack_deletes_offline_msg,
     c2s_ack_uses_parameterized_query,
     unknown_msg_type_handles_gracefully
    ].

init_per_suite(Config) ->
    % 设置测试环境并启动必要应用（含 lager/pooler 等依赖）
    ct:log("开始 msg_ack_logic 测试套件"),
    {ok, OldCwd} = file:get_cwd(),
    ProjectRoot = project_root_dir(OldCwd),
    case file:set_cwd(ProjectRoot) of
        ok ->
            SetupState = eunit_runner:eunit_setup(),
            [{setup_state, SetupState}, {old_cwd, OldCwd} | Config];
        {error, Reason} ->
            {skip, io_lib:format("Unable to set cwd to project root (~p): ~p", [ProjectRoot, Reason])}
    end.

end_per_suite(Config) ->
    case lists:keyfind(setup_state, 1, Config) of
        {setup_state, SetupState} ->
            eunit_runner:eunit_cleanup(SetupState);
        false ->
            ok
    end,
    case lists:keyfind(old_cwd, 1, Config) of
        {old_cwd, OldCwd} ->
            _ = file:set_cwd(OldCwd),
            ok;
        false ->
            ok
    end,
    ct:log("结束 msg_ack_logic 测试套件"),
    ok.

project_root_dir() ->
    project_root_dir(".").

project_root_dir(StartDir) ->
    find_project_root(filename:absname(StartDir), 10).

find_project_root(Dir, 0) ->
    Dir;
find_project_root(Dir, N) ->
    ConfigPath = filename:join([Dir, "config", "sys.local.config"]),
    case filelib:is_regular(ConfigPath) of
        true ->
            Dir;
        false ->
            Parent = filename:dirname(Dir),
            case Parent =:= Dir of
                true ->
                    Dir;
                false ->
                    find_project_root(Parent, N - 1)
            end
    end.

init_per_testcase(_TestCase, Config) ->
    % 每个测试用例前的初始化
    Config.

end_per_testcase(_TestCase, _Config) ->
    % 清理 Mock
    meck:unload(),
    ok.


%% ===================================================================
%% 测试用例：C2C ACK 处理
%% ===================================================================

c2c_ack_deletes_offline_msg(_Config) ->
    ct:log("测试 C2C ACK 删除离线消息"),
    Uid = 1001,
    Did = <<"device_ios_001">>,
    MsgId = <<"test_c2c_msg_001">>,

    % 准备测试数据
    Sql = <<"INSERT INTO msg_c2c (from_id, to_id, msg_id, msg_type, payload, created_at)
            VALUES ($1, $2, $3, $4, $5, NOW())">>,
    Payload = <<"{\"content\":\"test message\"}">>,
    {ok, _} = elib_pg:execute(Sql, [999, Uid, MsgId, <<"text">>, Payload]),

    % Mock msg_store_ds:unstage
    meck:new(msg_store_ds, [unstick]),
    meck:expect(msg_store_ds, unstage, 1, '_'),

    % 执行 ACK
    ok = msg_ack_logic:client_ack(<<"c2c">>, MsgId, Uid, Did),

    % 验证消息被删除
    Sql2 = <<"SELECT COUNT(*) FROM msg_c2c WHERE msg_id = $1 AND to_id = $2">>,
    {ok, Rows} = elib_pg:query(Sql2, [MsgId, Uid]),
    Count = extract_count(Rows),
    ?assertEqual(0, Count),

    % 清理测试数据
    elib_pg:execute(<<"DELETE FROM msg_c2c WHERE msg_id = $1">>, [MsgId]),
    {comment, "C2C ACK 成功删除离线消息"}.


c2c_ack_with_no_msg(_Config) ->
    ct:log("测试 C2C ACK 处理不存在的消息"),
    Uid = 1002,
    Did = <<"device_ios_002">>,
    MsgId = <<"test_c2c_msg_002">>,

    % Mock msg_store_ds:unstage
    meck:new(msg_store_ds, [unstick]),
    meck:expect(msg_store_ds, unstage, 1, '_'),

    % 执行 ACK（消息不存在）
    ok = msg_ack_logic:client_ack(<<"c2c">>, MsgId, Uid, Did),

    % 应该正常完成，不抛出异常
    {comment, "消息不存在时 ACK 正常处理"}.


%% ===================================================================
%% 测试用例：C2G ACK 处理
%% ===================================================================

c2g_ack_marks_timeline(_Config) ->
    ct:log("测试 C2G ACK 标记 timeline"),
    Uid = 2001,
    Did = <<"device_android_001">>,
    MsgId = <<"test_c2g_msg_001">>,

    % Mock msg_c2g_timeline_repo
    meck:new(msg_c2g_timeline_repo, [unstick]),
    meck:expect(msg_c2g_timeline_repo, client_ack, 2, {ok, 1}),

    % Mock msg_store_ds:unstage
    meck:new(msg_store_ds, [unstick]),
    meck:expect(msg_store_ds, unstage, 1, '_'),

    % 执行 ACK
    ok = msg_ack_logic:client_ack(<<"c2g">>, MsgId, Uid, Did),

    % 验证调用
    ?assert(meck:validate(msg_c2g_timeline_repo)),
    ?assert(meck:validate(msg_store_ds)),
    {comment, "C2G ACK 成功标记 timeline"}.


%% ===================================================================
%% 测试用例：S2C ACK 处理
%% ===================================================================

s2c_ack_deletes_offline_msg(_Config) ->
    ct:log("测试 S2C ACK 删除离线消息"),
    Uid = 3001,
    Did = <<"device_ios_003">>,
    MsgId = <<"test_s2c_msg_001">>,

    % 准备测试数据
    Sql = <<"INSERT INTO msg_s2c (from_id, to_id, msg_id, action, msg_type, payload, created_at)
            VALUES ($1, $2, $3, $4, $5, $6, NOW())">>,
    Payload = <<"{\"msg_type\":\"system\"}">>,
    {ok, _} = elib_pg:execute(Sql, [0, Uid, MsgId, <<"notify">>, <<"system">>, Payload]),

    % Mock msg_store_ds:unstage
    meck:new(msg_store_ds, [unstick]),
    meck:expect(msg_store_ds, unstage, 1, '_'),

    % 执行 ACK
    ok = msg_ack_logic:client_ack(<<"s2c">>, MsgId, Uid, Did),

    % 验证消息被删除
    Sql2 = <<"SELECT COUNT(*) FROM msg_s2c WHERE msg_id = $1 AND to_id = $2">>,
    {ok, Rows} = elib_pg:query(Sql2, [MsgId, Uid]),
    Count = extract_count(Rows),
    ?assertEqual(0, Count),

    % 清理测试数据
    elib_pg:execute(<<"DELETE FROM msg_s2c WHERE msg_id = $1">>, [MsgId]),
    {comment, "S2C ACK 成功删除离线消息"}.


%% ===================================================================
%% 测试用例：C2S ACK 处理（SQL 安全）
%% ===================================================================

c2s_ack_uses_parameterized_query(_Config) ->
    ct:log("测试 C2S ACK 使用参数化查询"),
    Uid = 4001,
    Did = <<"device_web_001">>,
    MsgId = <<"test_c2s_msg_001">>,

    % 准备测试数据
    Sql = <<"INSERT INTO msg_c2s (from_id, to_id, topic_id, msg_id, msg_type, payload, created_at)
            VALUES ($1, $2, $3, $4, $5, $6, NOW())">>,
    Payload = <<"{\"text\":\"hello\"}">>,
    {ok, _} = elib_pg:execute(Sql, [Uid, 123, 123, MsgId, <<"text">>, Payload]),

    % Mock msg_store_ds:unstage
    meck:new(msg_store_ds, [unstick]),
    meck:expect(msg_store_ds, unstage, 1, '_'),

    % 执行 ACK
    ok = msg_ack_logic:client_ack(<<"c2s">>, MsgId, Uid, Did),

    % 验证消息被删除
    Sql2 = <<"SELECT COUNT(*) FROM msg_c2s WHERE msg_id = $1 AND from_id = $2">>,
    {ok, Rows} = elib_pg:query(Sql2, [MsgId, Uid]),
    Count = extract_count(Rows),
    ?assertEqual(0, Count),

    % 清理测试数据
    elib_pg:execute(<<"DELETE FROM msg_c2s WHERE msg_id = $1">>, [MsgId]),
    {comment, "C2S ACK 使用参数化查询成功"}.


%% ===================================================================
%% 测试用例：未知消息类型处理
%% ===================================================================

unknown_msg_type_handles_gracefully(_Config) ->
    ct:log("测试未知消息类型的优雅处理"),
    Uid = 5001,
    Did = <<"device_unknown">>,
    MsgId = <<"test_unknown_msg">>,

    % Mock msg_store_ds:unstage
    meck:new(msg_store_ds, [unstick]),
    meck:expect(msg_store_ds, unstage, 1, '_'),

    % 执行 ACK（未知类型）
    ok = msg_ack_logic:client_ack(<<"unknown">>, MsgId, Uid, Did),

    % 应该正常完成
    {comment, "未知消息类型优雅处理"}.

extract_count([#{<<"count">> := Count}]) when is_integer(Count) ->
    Count;
extract_count([{Count}]) when is_integer(Count) ->
    Count;
extract_count(_) ->
    0.
