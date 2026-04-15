%%%-------------------------------------------------------------------
%%% @doc E2EE 分片传输日志记录器
%%% 记录分片传输日志，用于审计和安全监控
%%% @end
%%%-------------------------------------------------------------------
-module(e2ee_shard_validator).

%% API
-export([log_shard_transmission/3]).
-export([log_shard_transmission/5]).
-export([log_shard_created/3]).
-export([log_shard_sent/3]).
-export([log_shard_stored/3]).
-export([log_shard_decrypted/3]).
-export([log_shard_recovered/3]).

-include("log.hrl").

%%%===================================================================
%%% Constants
%%%===================================================================

-define(ACTION_SHARD_CREATED, <<"shard_created">>).
-define(ACTION_SHARD_SENT, <<"shard_sent">>).
-define(ACTION_SHARD_STORED, <<"shard_stored">>).
-define(ACTION_SHARD_DECRYPTED, <<"shard_decrypted">>).
-define(ACTION_SHARD_RECOVERED, <<"shard_recovered">>).

-define(DIRECTION_SERVER_TO_PROXY, <<"server_to_proxy">>).
-define(DIRECTION_PROXY_TO_USER, <<"proxy_to_user">>).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc 记录分片传输日志（简化版，向后兼容）
%% @param FromUid 发送方用户ID
%% @param ToUid 接收方用户ID
%% @param ShardId 分片ID
%% @returns ok
-spec log_shard_transmission(integer(), integer(), binary()) -> ok.
log_shard_transmission(FromUid, ToUid, ShardId) ->
    log_shard_transmission(FromUid, ToUid, ShardId, ?ACTION_SHARD_SENT, #{}).

%% @doc 记录分片传输日志（完整版）
%% @param FromUid 发送方用户ID
%% @param ToUid 接收方用户ID
%% @param ShardId 分片ID
%% @param Action 操作类型
%% @param Metadata 额外元数据
%% @returns ok
-spec log_shard_transmission(integer(), integer(), binary(), binary(), map()) -> ok.
log_shard_transmission(FromUid, ToUid, ShardId, Action, Metadata) ->
    Data = #{
        shard_id => ShardId,
        key_version => <<"latest">>,
        uid => FromUid,
        proxy_uid => ToUid,
        action => Action,
        direction => ?DIRECTION_SERVER_TO_PROXY,
        metadata => Metadata,
        created_at => elib_dt:now()
    },
    case e2ee_shard_transmission_log_ds:insert(Data) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            % 日志记录失败不影响主流程，仅记录错误
            ?ERROR_LOG([log_shard_transmission, failed, Reason,
                       #{from_uid => FromUid, to_uid => ToUid, shard_id => ShardId}]),
            ok
    end.

%% @doc 记录分片创建日志
%% @param Uid 密钥所有者用户ID
%% @param ProxyUid 代理用户ID
%% @param ShardId 分片ID
%% @returns ok
-spec log_shard_created(integer(), integer(), binary()) -> ok.
log_shard_created(Uid, ProxyUid, ShardId) ->
    log_shard_transmission(Uid, ProxyUid, ShardId, ?ACTION_SHARD_CREATED, #{}).

%% @doc 记录分片发送日志
%% @param Uid 密钥所有者用户ID
%% @param ProxyUid 代理用户ID
%% @param ShardId 分片ID
%% @returns ok
-spec log_shard_sent(integer(), integer(), binary()) -> ok.
log_shard_sent(Uid, ProxyUid, ShardId) ->
    log_shard_transmission(Uid, ProxyUid, ShardId, ?ACTION_SHARD_SENT, #{}).

%% @doc 记录分片存储确认日志
%% @param Uid 密钥所有者用户ID
%% @param ProxyUid 代理用户ID
%% @param ShardId 分片ID
%% @returns ok
-spec log_shard_stored(integer(), integer(), binary()) -> ok.
log_shard_stored(Uid, ProxyUid, ShardId) ->
    log_shard_transmission(Uid, ProxyUid, ShardId, ?ACTION_SHARD_STORED, #{}).

%% @doc 记录分片解密日志（恢复过程）
%% @param Uid 密钥所有者用户ID
%% @param ProxyUid 代理用户ID
%% @param ShardId 分片ID
%% @returns ok
-spec log_shard_decrypted(integer(), integer(), binary()) -> ok.
log_shard_decrypted(Uid, ProxyUid, ShardId) ->
    log_shard_transmission(Uid, ProxyUid, ShardId, ?ACTION_SHARD_DECRYPTED,
                          #{direction => ?DIRECTION_PROXY_TO_USER}).

%% @doc 记录分片用于密钥恢复日志
%% @param Uid 密钥所有者用户ID
%% @param ProxyUid 代理用户ID
%% @param ShardId 分片ID
%% @returns ok
-spec log_shard_recovered(integer(), integer(), binary()) -> ok.
log_shard_recovered(Uid, ProxyUid, ShardId) ->
    log_shard_transmission(Uid, ProxyUid, ShardId, ?ACTION_SHARD_RECOVERED,
                          #{direction => ?DIRECTION_PROXY_TO_USER}).
