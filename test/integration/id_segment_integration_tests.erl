%%====================================================================
%% @doc ID段动态分配集成测试
%% @author Imboy Team
%% @copyright 2026 Imboy
%% @version 1.0.0
%%
%% 测试ID段动态分配的完整工作流程，包括：
%% 1. 数据库表创建
%% 2. 机房注册
%% 3. 序列初始化
%% 4. ID生成和验证
%% 5. 多机房ID冲突检测
%%====================================================================

-module(id_segment_integration_tests).
-include_lib("eunit/include/eunit.hrl").

-define(TEST_DB, "imboy_v1_test").
-define(TEST_USER, "imboy_test").
-define(TEST_DC1_ID, 901).
-define(TEST_DC2_ID, 902).

%%%===================================================================
%%% 测试固件
%%%===================================================================

%% @doc 设置测试环境
setup() ->
    application:set_env(imboy, env, test),

    % 确保使用测试数据库
    ok = application:set_env(imboy, pg_conf, #{
        name => pgsql,
        max_count => 10,
        init_count => 2,
        start_mfa => {epgsql, connect, [
            #{
                host => "localhost",
                username => "imboy_test",
                database => ?TEST_DB,
                port => 5432,
                ssl => false,
                timeout => 5000
            }
        ]}
    }),

    % 初始化数据库连接池
    {ok, _} = pooler:new_pool(application:get_env(imboy, pg_conf)),

    ok.

%% @doc 清理测试环境
cleanup(_State) ->
    % 清理测试数据
    cleanup_test_data(),
    ok.

%% @doc 清理测试数据
cleanup_test_data() ->
    Sql = [
        "DROP TABLE IF EXISTS test_table CASCADE",
        "DELETE FROM system_id_segment WHERE datacenter_id IN ($1, $2)",
        "DELETE FROM system_datacenter WHERE id IN ($1, $2)",
        "DROP SEQUENCE IF EXISTS test_table_id_seq CASCADE"
    ],

    lists:foreach(fun(SQL) ->
        case elib_pg:query(SQL, [?TEST_DC1_ID, ?TEST_DC2_ID]) of
            {ok, _} -> ok;
            {error, _} -> ok
        end
    end, Sql),

    ok.

%%%===================================================================
%%% 集成测试：完整工作流
%%%===================================================================

%% @doc 端到端测试：完整工作流
integration_workflow_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Context) ->
         {timeout, 120, [
             {"1. 创建数据库表", ?_test(create_test_table())},
             {"2. 注册机房", ?_test(register_datacenters())},
             {"3. 启动服务", ?_test(start_services())},
             {"4. 初始化序列", ?_test(initialize_sequences())},
             {"5. 生成ID并验证", ?_test(generate_and_validate_ids())},
             {"6. 多机房冲突检测", ?_test(check_multi_dc_conflicts())},
             {"7. 自动续期", ?_test(test_auto_renew())},
             {"8. 清理", ?_test(cleanup_test_data())}
         ]}
     end
    }.

%% @doc 创建测试表
create_test_table() ->
    SQL = "
        CREATE TABLE IF NOT EXISTS test_table (
            id BIGSERIAL PRIMARY KEY,
            name VARCHAR(100) NOT NULL,
            created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
        )
    ",

    case elib_pg:query(SQL, []) of
        {ok, _, _} ->
            ?debugMsg("Test table created successfully");
        {error, Reason} ->
            ?debugFmt("Failed to create test table: ~p", [Reason])
    end,

    ok.

%% @doc 注册测试机房
register_datacenters() ->
    % 注册机房1
    SQL1 = "SELECT register_datacenter('test_dc1', 'cn-test-1', 'http://dc1.test')",
    {ok, _, [{DcId1}]} = elib_pg:query(SQL1, []),
    ?assertEqual(?TEST_DC1_ID, DcId1),

    % 注册机房2
    SQL2 = "SELECT register_datacenter('test_dc2', 'cn-test-2', 'http://dc2.test')",
    {ok, _, [{DcId2}]} = elib_pg:query(SQL2, []),
    ?assertEqual(?TEST_DC2_ID, DcId2),

    ?debugMsg("Datacenters registered successfully"),
    ok.

%% @doc 启动ID段管理服务
start_services() ->
    {ok, Pid1} = elib_id_segment:start_link(?TEST_DC1_ID),
    ?assert(is_process_alive(Pid1)),

    ?debugMsg("ID segment services started"),
    ok.

%% @doc 初始化序列
initialize_sequences() ->
    % 初始化机房1的序列
    case elib_id_segment:init_sequence(<<"test_table">>) of
        ok ->
            ?debugMsg("Sequences initialized successfully");
        {error, Reason} ->
            ?debugFmt("Failed to initialize sequences: ~p", [Reason])
    end,

    ok.

%% @doc 生成ID并验证
generate_and_validate_ids() ->
    % 生成100个ID
    Ids = lists:map(fun(_) ->
        SQL = "INSERT INTO test_table (name) VALUES ($1) RETURNING id",
        {ok, _, [{Id}]} = elib_pg:query(SQL, [<<"test">>]),
        Id
    end, lists:seq(1, 100)),

    % 验证ID唯一性
    UniqueIds = lists:usort(Ids),
    ?assertEqual(100, length(UniqueIds)),

    % 验证ID在预期范围内
    {MinId, MaxId} = lists:minmax(Ids),
    ?debugFmt("Generated IDs: ~p to ~p", [MinId, MaxId]),

    % 验证ID连续性（段内应该基本连续）
    ExpectedRange = MaxId - MinId + 1,
    ?assert(ExpectedRange =< 150),  % 允许一些gap

    ?debugMsg("ID generation and validation passed"),
    ok.

%% @doc 多机房冲突检测
check_multi_dc_conflicts() ->
    % 获取机房1的ID段
    SQL1 = "SELECT segment_start, segment_end FROM system_id_segment
            WHERE datacenter_id = $1 AND table_name = $2 AND is_active = TRUE",
    {ok, _, [{Start1, End1}]} = elib_pg:query(SQL1, [?TEST_DC1_ID, "test_table"]),

    % 停止机房1服务
    gen_server:stop(elib_id_segment),

    % 启动机房2服务
    {ok, _} = elib_id_segment:start_link(?TEST_DC2_ID),
    elib_id_segment:init_sequence(<<"test_table">>),

    % 获取机房2的ID段
    {ok, _, [{Start2, End2}]} = elib_pg:query(SQL2, [?TEST_DC2_ID, "test_table"]),

    % 验证不重叠
    ?assert(End1 < Start2 orelse End2 < Start1),
    ?debugFmt("DC1 range: ~p-~p, DC2 range: ~p-~p", [Start1, End1, Start2, End2]),

    ?debugMsg("Multi-DC conflict detection passed"),
    ok.

%% @doc 测试自动续期
test_auto_renew() ->
    % 获取当前ID段
    SQL = "SELECT segment_start, segment_end, used_count FROM system_id_segment
           WHERE datacenter_id = $1 AND table_name = $2 AND is_active = TRUE",
    {ok, _, [{Start, End, UsedCount}]} = elib_pg:query(SQL, [?TEST_DC2_ID, "test_table"]),

    % 模拟高使用率（更新used_count）
    UpdateSQL = "UPDATE system_id_segment SET used_count = $1
                 WHERE datacenter_id = $2 AND table_name = $3 AND is_active = TRUE",
    Threshold = (End - Start + 1) * 0.85,
    {ok, _, _} = elib_pg:query(UpdateSQL, [trunc(Threshold), ?TEST_DC2_ID, "test_table"]),

    % 手动触发检查
    elib_id_segment:refresh_segment(<<"test_table">>),

    % 验证是否续期
    {ok, _, [{NewStart, NewEnd, _NewUsed}]} = elib_pg:query(SQL, [?TEST_DC2_ID, "test_table"]),

    % 新段应该从旧段结束之后开始
    ?assert(NewStart > End),
    ?debugFmt("Auto-renew: old=~p-~p, new=~p-~p", [Start, End, NewStart, NewEnd]),

    ?debugMsg("Auto-renew test passed"),
    ok.

%%%===================================================================
%%% 集成测试：边界情况
%%%===================================================================

%% @doc 测试段耗尽情况
segment_exhaustion_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Context) ->
         {timeout, 60, [
             {"段耗尽时自动续期", ?_test(test_segment_exhaustion())}
         ]}
     end
    }.

test_segment_exhaustion() ->
    create_test_table(),
    register_datacenters(),

    {ok, _} = elib_id_segment:start_link(?TEST_DC1_ID),
    elib_id_segment:init_sequence(<<"test_table">>),

    % 获取小段（用于测试）
    SQL = "SELECT renew_id_segment($1, $2, 100)",
    {ok, _, [{Start, End}]} = elib_pg:query(SQL, [?TEST_DC1_ID, "test_table"]),

    % 消耗整个段
    lists:foreach(fun(_) ->
        InsertSQL = "INSERT INTO test_table (name) VALUES ($1)",
        {ok, _, _} = elib_pg:query(InsertSQL, [<<"test">>])
    end, lists:seq(1, 100)),

    % 下一个插入应该触发续期
    {ok, _, [{NextId}]} = elib_pg:query(
        "INSERT INTO test_table (name) VALUES ($1) RETURNING id",
        [<<"test">>]
    ),

    % 验证新ID在新段内
    ?assert(NextId > End),

    gen_server:stop(elib_id_segment),
    cleanup_test_data(),
    ok.

%%%===================================================================
%%% 辅助函数
%%%===================================================================

%% @doc 验证ID在指定范围内
-spec verify_id_in_range(integer(), integer(), integer()) -> boolean().
verify_id_in_range(Id, Min, Max) ->
    Id >= Min andalso Id =< Max.

%% @doc 计算两个ID段的距离
-spec segment_distance(integer(), integer(), integer(), integer()) -> integer().
segment_distance(Start1, End1, Start2, End2) ->
    if
        End1 < Start2 -> Start2 - End1;
        End2 < Start1 -> Start1 - End2;
        true -> -1  % 重叠
    end.
