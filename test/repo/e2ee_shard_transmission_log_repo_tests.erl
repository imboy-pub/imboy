%%%-------------------------------------------------------------------
%%% @doc E2EE 分片传输日志仓库测试
%%% @end
%%%-------------------------------------------------------------------
-module(e2ee_shard_transmission_log_repo_tests).
-include_lib("eunit/include/eunit.hrl").

%% 测试数据
-define(TEST_SHARD_ID, <<"test_shard_123">>).
-define(TEST_KEY_VERSION, <<"v1">>).
-define(TEST_UID, 12345).
-define(TEST_PROXY_UID, 67890).

%%%===================================================================
%%% 测试用例
%%%===================================================================

%% @doc 测试插入单条传输日志
insert_test_() ->
    LogMap = #{
        <<"shard_id">> => ?TEST_SHARD_ID,
        <<"key_version">> => ?TEST_KEY_VERSION,
        <<"uid">> => ?TEST_UID,
        <<"proxy_uid">> => ?TEST_PROXY_UID,
        <<"action">> => <<"shard_created">>,
        <<"direction">> => <<"server_to_proxy">>,
        <<"metadata">> => #{},
        <<"ip_address">> => <<"127.0.0.1">>,
        <<"user_agent">> => <<"test_agent">>
    },

    % 测试插入
    case e2ee_shard_transmission_log_repo:insert(LogMap) of
        {ok, _LogId} ->
            ?assert(true);
        {error, Reason} ->
            ?debugFmt("插入失败: ~p~n", [Reason]),
            ?assert(false)
    end.

%% @doc 测试查询分片日志
get_by_shard_id_test_() ->
    % 先插入测试数据
    LogMap = #{
        <<"shard_id">> => ?TEST_SHARD_ID,
        <<"key_version">> => ?TEST_KEY_VERSION,
        <<"uid">> => ?TEST_UID,
        <<"proxy_uid">> => ?TEST_PROXY_UID,
        <<"action">> => <<"shard_sent">>,
        <<"direction">> => <<"server_to_proxy">>,
        <<"metadata">> => #{},
        <<"ip_address">> => null,
        <<"user_agent">> => null
    },
    e2ee_shard_transmission_log_repo:insert(LogMap),

    % 测试查询
    case e2ee_shard_transmission_log_repo:get_by_shard_id(?TEST_SHARD_ID) of
        {ok, Logs} when is_list(Logs) ->
            ?assert(length(Logs) > 0);
        {error, Reason} ->
            ?debugFmt("查询失败: ~p~n", [Reason]),
            ?assert(false)
    end.

%% @doc 测试按密钥版本查询
get_by_key_version_test_() ->
    % 测试查询
    case e2ee_shard_transmission_log_repo:get_by_key_version(?TEST_KEY_VERSION) of
        {ok, Logs} when is_list(Logs) ->
            ?assert(length(Logs) > 0);
        {error, Reason} ->
            ?debugFmt("查询失败: ~p~n", [Reason]),
            ?assert(false)
    end.

%% @doc 测试获取统计数据
get_stats_test_() ->
    % 测试统计
    case e2ee_shard_transmission_log_repo:get_stats(?TEST_KEY_VERSION, ?TEST_SHARD_ID) of
        {ok, Stats} when is_map(Stats) ->
            ?assert(maps:get(key_version, Stats) =:= ?TEST_KEY_VERSION);
        {error, Reason} ->
            ?debugFmt("统计失败: ~p~n", [Reason]),
            ?assert(false)
    end.

%% @doc 测试异常检查
check_anomaly_test_() ->
    % 测试异常检查
    case e2ee_shard_transmission_log_repo:check_anomaly(?TEST_KEY_VERSION, ?TEST_SHARD_ID) of
        {ok, Anomalies} when is_list(Anomalies) ->
            ?assert(true);
        {error, Reason} ->
            ?debugFmt("异常检查失败: ~p~n", [Reason]),
            ?assert(false)
    end.
