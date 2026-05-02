%%%-------------------------------------------------------------------
%%% @doc E2EE 分片传输日志仓库测试
%%% @end
%%%-------------------------------------------------------------------
-module(e2ee_shard_transmission_log_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% 测试数据
-define(TEST_SHARD_ID, <<"test_shard_123">>).
-define(TEST_KEY_VERSION, <<"v1">>).
-define(TEST_UID, 12345).
-define(TEST_PROXY_UID, 67890).
-define(TEST_LOG_ID, 99887766).

%%%===================================================================
%%% 测试用例
%%%===================================================================

%% @doc 测试插入单条传输日志
insert_test_() ->
    ?WITH_MECKS([
        {elib_tsid, [
            {'generate', 1, fun(_Table) -> ?TEST_LOG_ID end}
        ]},
        {elib_pg, [
            {'execute', 2, fun(_Sql, _Params) -> {ok, 1} end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> 1700000000000 end}
        ]}
    ], fun() ->
        LogMap = #{
            shard_id => ?TEST_SHARD_ID,
            key_version => ?TEST_KEY_VERSION,
            uid => ?TEST_UID,
            proxy_uid => ?TEST_PROXY_UID,
            action => <<"shard_created">>,
            direction => <<"server_to_proxy">>,
            metadata => #{},
            ip_address => <<"127.0.0.1">>,
            user_agent => <<"test_agent">>
        },
        Result = e2ee_shard_transmission_log_repo:insert(LogMap),
        ?assertEqual({ok, ?TEST_LOG_ID}, Result)
    end).

%% @doc 测试查询分片日志
get_by_shard_id_test_() ->
    ?WITH_MECKS([
        {elib_tsid, [
            {'generate', 1, fun(_Table) -> ?TEST_LOG_ID end}
        ]},
        {elib_pg, [
            {'execute', 2, fun(_Sql, _Params) -> {ok, 1} end},
            {'query', 2, fun(_Sql, _Params) ->
                {ok, [#{<<"id">> => ?TEST_LOG_ID, <<"shard_id">> => ?TEST_SHARD_ID}]}
            end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> 1700000000000 end}
        ]}
    ], fun() ->
        LogMap = #{
            shard_id => ?TEST_SHARD_ID,
            key_version => ?TEST_KEY_VERSION,
            uid => ?TEST_UID,
            proxy_uid => ?TEST_PROXY_UID,
            action => <<"shard_sent">>,
            direction => <<"server_to_proxy">>,
            metadata => #{},
            ip_address => null,
            user_agent => null
        },
        {ok, _} = e2ee_shard_transmission_log_repo:insert(LogMap),

        Result = e2ee_shard_transmission_log_repo:list_by_shard_id(?TEST_SHARD_ID),
        ?assertMatch({ok, [_|_]}, Result)
    end).

%% @doc 测试按操作类型查询
list_by_action_test_() ->
    ?WITH_MECKS([
        {elib_pg, [
            {'query', 2, fun(_Sql, _Params) ->
                {ok, [#{<<"id">> => ?TEST_LOG_ID, <<"action">> => <<"create">> }]}
            end}
        ]}
    ], fun() ->
        Result = e2ee_shard_transmission_log_repo:list_by_action(<<"create">>, 10),
        ?assertMatch({ok, [_|_]}, Result)
    end).

%% @doc 测试获取统计数据
get_transmission_stats_test_() ->
    ?WITH_MECKS([
        {elib_pg, [
            {'query', 2, fun(_Sql, _Params) ->
                {ok, [#{<<"shard_id">> => ?TEST_SHARD_ID, <<"created_count">> => 1}]}
            end}
        ]}
    ], fun() ->
        Result = e2ee_shard_transmission_log_repo:get_transmission_stats(?TEST_SHARD_ID),
        ?assertMatch({ok, _}, Result)
    end).

%% @doc 测试异常检查
check_anomaly_test_() ->
    ?WITH_MECKS([
        {elib_pg, [
            {'query', 2, fun(_Sql, _Params) -> {ok, []} end}
        ]}
    ], fun() ->
        Result = e2ee_shard_transmission_log_repo:check_anomaly(?TEST_KEY_VERSION, ?TEST_SHARD_ID),
        ?assertMatch({ok, _}, Result)
    end).
