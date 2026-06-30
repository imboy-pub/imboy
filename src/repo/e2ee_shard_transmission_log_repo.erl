-module(e2ee_shard_transmission_log_repo).
%%%===================================================================
%%% @doc E2EE 分片传输日志 Repo 层
%%%
%%% 管理 E2EE 分片传输审计日志的数据库操作
%%% 用于零信任架构下的分片传输审计和安全监控
%%% @end
%%%===================================================================

-include("log.hrl").

%%===================================================================
%%% API Functions
%%===================================================================

%% 导出函数
-export([insert/1]).
-export([list_by_shard_id/1]).
-export([list_by_action/2]).
-export([get_transmission_stats/1]).
-export([check_anomaly/2]).

%%===================================================================
%%% Constants
%%===================================================================

%% 操作类型
-define(ACTION_SHARD_CREATED, <<"shard_created">>).
-define(ACTION_SHARD_SENT, <<"shard_sent">>).
-define(ACTION_SHARD_STORED, <<"shard_stored">>).
-define(ACTION_SHARD_DECRYPTED, <<"shard_decrypted">>).
-define(ACTION_SHARD_RECOVERED, <<"shard_recovered">>).

%% 传输方向
-define(DIRECTION_SERVER_TO_PROXY, <<"server_to_proxy">>).
-define(DIRECTION_PROXY_TO_USER, <<"proxy_to_user">>).

%%===================================================================
%%% API Implementation
%%===================================================================

%% @doc 插入分片传输日志
%% @param Data 日志数据 map，包含：
%%   - shard_id: 分片唯一标识符
%%   - key_version: 密钥版本号
%%   - uid: 密钥所有者用户 ID
%%   - proxy_uid: 代理用户 ID
%%   - action: 操作类型
%%   - direction: 传输方向
%%   - metadata: 额外元数据（可选）
%%   - ip_address: 客户端 IP（可选）
%%   - user_agent: 客户端 User-Agent（可选）
%% @returns {ok, Id} | {error, Reason}
-spec insert(map()) -> {ok, integer()} | {error, term()}.
insert(Data) ->
    ShardId = maps:get(shard_id, Data),
    KeyVersion = maps:get(key_version, Data, <<"latest">>),
    Uid = maps:get(uid, Data),
    ProxyUid = maps:get(proxy_uid, Data),
    Action = maps:get(action, Data),
    Direction = maps:get(direction, Data),
    Metadata = maps:get(metadata, Data, null),
    IpAddress = maps:get(ip_address, Data, null),
    UserAgent = maps:get(user_agent, Data, null),
    % ponytail: to_timestamp($11::bigint/1000) expects ms integer, not RFC3339 binary
    CreatedAt = maps:get(created_at, Data, elib_dt:millisecond()),

    Id = elib_tsid:generate(e2ee_shard_transmission_log),
    Sql =
        <<"INSERT INTO e2ee_shard_transmission_log ",
            "(id, shard_id, key_version, uid, proxy_uid, action, direction, ",
            "metadata, ip_address, user_agent, created_at) ",
            "VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, to_timestamp($11::bigint/1000))">>,
    case
        elib_pg:execute(Sql, [
            Id,
            ShardId,
            KeyVersion,
            Uid,
            ProxyUid,
            Action,
            Direction,
            Metadata,
            IpAddress,
            UserAgent,
            CreatedAt
        ])
    of
        {ok, _Count} ->
            {ok, Id};
        {error, Reason} ->
            ?ERROR_LOG([e2ee_shard_transmission_log_insert, failed, Reason]),
            {error, Reason}
    end.

%% @doc 查询分片的完整传输链路
%% @param ShardId 分片 ID
%% @returns {ok, [Log]} | {error, Reason}
-spec list_by_shard_id(binary()) -> {ok, [map()]} | {error, term()}.
list_by_shard_id(ShardId) ->
    Sql =
        <<"SELECT id, shard_id, key_version, uid, proxy_uid, action, direction, ",
            "metadata, ip_address, user_agent, created_at ", "FROM e2ee_shard_transmission_log ",
            "WHERE shard_id = $1 ", "ORDER BY created_at ASC">>,
    case elib_pg:query(Sql, [ShardId]) of
        {ok, Rows} -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 查询特定操作类型的日志
%% @param Action 操作类型
%% @param Limit 返回数量限制
%% @returns {ok, [Log]} | {error, Reason}
-spec list_by_action(binary(), integer()) -> {ok, [map()]} | {error, term()}.
list_by_action(Action, Limit) ->
    Sql =
        <<"SELECT id, shard_id, key_version, uid, proxy_uid, action, direction, ",
            "metadata, ip_address, user_agent, created_at ", "FROM e2ee_shard_transmission_log ",
            "WHERE action = $1 ", "ORDER BY created_at DESC ", "LIMIT $2">>,
    case elib_pg:query(Sql, [Action, Limit]) of
        {ok, Rows} -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 获取分片传输统计信息
%% 使用视图 v_e2ee_shard_transmission_stats
%% @param ShardId 分片 ID
%% @returns {ok, Stats} | {error, Reason}
-spec get_transmission_stats(binary()) -> {ok, map()} | {error, term()}.
get_transmission_stats(ShardId) ->
    Sql =
        <<"SELECT key_version, shard_id, uid, ",
            "created_count, sent_count, stored_count, decrypted_count, recovered_count, ",
            "first_transmission_at, last_transmission_at ", "FROM v_e2ee_shard_transmission_stats ",
            "WHERE shard_id = $1">>,
    case elib_pg:query(Sql, [ShardId]) of
        {ok, []} -> {error, not_found};
        {ok, [Stats | _]} -> {ok, Stats};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 检查分片传输异常
%% 使用数据库函数 check_e2ee_shard_transmission_anomaly
%% @param KeyVersion 密钥版本
%% @param ShardId 分片 ID
%% @returns {ok, [Anomaly]} | {error, Reason}
-spec check_anomaly(binary(), binary()) -> {ok, [map()]} | {error, term()}.
check_anomaly(KeyVersion, ShardId) ->
    Sql =
        <<"SELECT anomaly_type, description, details ",
            "FROM check_e2ee_shard_transmission_anomaly($1, $2)">>,
    case elib_pg:query(Sql, [KeyVersion, ShardId]) of
        {ok, Rows} -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.
