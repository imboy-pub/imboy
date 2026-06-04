-module(e2ee_social_logic).
-dialyzer(no_return).
%%%===================================================================
%%% @doc E2EE 社交恢复 Logic 层
%%%
%%% 服务端持久化加密分片：服务端仅保存加密态分片，明文分片始终不落库
%%%===================================================================

%%===================================================================
%%% API Functions Export
%%===================================================================
-include("log.hrl").
-include("error_code.hrl").

%% 安全限制常量

% 最大分片数（防止 DOS）
-define(MAX_SHARDS, 5).
% 最小阈值
-define(MIN_THRESHOLD, 2).
% 最大阈值（实际需要的分片数）
-define(MAX_THRESHOLD, 3).

%% API 导出
-export([create_shards/3]).
-export([get_user_shards/2]).
-export([get_proxy_shards/1]).
-export([recover_key/2]).
-export([validate_shards/2]).
-export([can_recover/2]).

%% @doc 创建恢复分片（C1 FIX：客户端已完成 Shamir 分割和代理公钥加密，服务端只存储）
%% @param Uid 用户 ID
%% @param KeyVersion 密钥版本号
%% @param Shards 客户端预加密分片列表 [{proxy_uid, encrypted_shard}]
%% @returns {ok, StoredShards} | {error, Reason}
-spec create_shards(
    integer(),
    binary(),
    list(map())
) -> {ok, list(map())} | {error, term()}.
create_shards(Uid, KeyVersion, Shards) ->
    try
        % 1. 验证分片列表
        TotalShards = length(Shards),
        case TotalShards =:= 0 of
            true -> throw({error, {<<"分片列表不能为空"/utf8>>, ?ERR_BAD_REQUEST}});
            false -> ok
        end,

        % 验证总分片数上限（防止 DOS 攻击）
        case TotalShards > ?MAX_SHARDS of
            true -> throw({error, {<<"分片数不能超过 5"/utf8>>, ?ERR_BAD_REQUEST}});
            false -> ok
        end,

        % 验证每个分片有必填字段
        lists:foreach(
            fun(Shard) ->
                case
                    {
                        maps:get(<<"proxy_uid">>, Shard, undefined),
                        maps:get(<<"encrypted_shard">>, Shard, undefined)
                    }
                of
                    {undefined, _} ->
                        throw({error, {<<"分片缺少 proxy_uid 字段"/utf8>>, ?ERR_BAD_REQUEST}});
                    {_, undefined} ->
                        throw({error, {<<"分片缺少 encrypted_shard 字段"/utf8>>, ?ERR_BAD_REQUEST}});
                    _ ->
                        ok
                end
            end,
            Shards
        ),

        % 2. 构建分片存储记录
        ShardRecords = lists:zipwith(
            fun(Shard, Index) ->
                ShardId = elib_tsid:generate(),
                #{
                    <<"shard_id">> => ShardId,
                    <<"uid">> => Uid,
                    <<"proxy_uid">> => maps:get(<<"proxy_uid">>, Shard),
                    <<"key_version">> => KeyVersion,
                    <<"shard_index">> => Index,
                    <<"total_shards">> => TotalShards,
                    <<"threshold">> => 0,
                    <<"encrypted_shard">> => maps:get(<<"encrypted_shard">>, Shard)
                }
            end,
            Shards,
            lists:seq(1, TotalShards)
        ),

        % 3. 持久化分片（仅加密态，私钥从未经过服务端）
        PersistedShardRecords =
            case persist_shards(ShardRecords) of
                {ok, Records} -> Records;
                {error, PersistReason} -> throw({error, PersistReason})
            end,

        % 4. 记录审计日志
        lists:foreach(
            fun(ShardRecord) ->
                ShardId = maps:get(<<"shard_id">>, ShardRecord),
                ProxyUid = maps:get(<<"proxy_uid">>, ShardRecord),
                e2ee_shard_validator:log_shard_transmission(
                    shard_created,
                    ShardId,
                    #{
                        <<"uid">> => Uid,
                        <<"proxy_uid">> => ProxyUid,
                        <<"key_version">> => KeyVersion,
                        <<"shard_index">> => maps:get(<<"shard_index">>, ShardRecord),
                        <<"total_shards">> => TotalShards
                    }
                )
            end,
            PersistedShardRecords
        ),

        {ok, PersistedShardRecords}
    catch
        {error, {Msg, Code}} when is_binary(Msg), is_integer(Code) ->
            {error, {Msg, Code}};
        {error, Reason} when is_binary(Reason) ->
            {error, {Reason, ?ERR_INTERNAL_SERVER_ERROR}};
        _:Reason:_Stack ->
            ?ERROR_LOG([e2ee_social_logic, create_shards_failed, Reason]),
            {error, {<<"创建分片失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR}}
    end.

%% @doc 获取用户的所有恢复分片（服务端持久化）
-spec get_user_shards(integer(), binary()) -> {ok, list(map())} | {error, term()}.
get_user_shards(Uid, KeyVersion) ->
    case e2ee_social_ds:get_user_shards(Uid, KeyVersion) of
        {ok, Shards} ->
            ClientShards = [
                S#{
                    <<"uid">> => maps:get(<<"uid">>, S, 0),
                    <<"proxy_uid">> => maps:get(<<"proxy_uid">>, S, 0)
                }
             || S <- Shards
            ],
            {ok, ClientShards};
        Error ->
            Error
    end.

%% @doc 获取用户作为代理的所有分片（服务端持久化）
-spec get_proxy_shards(integer()) -> {ok, list(map())} | {error, term()}.
get_proxy_shards(ProxyUid) ->
    e2ee_social_ds:get_proxy_shards(ProxyUid).

%% @doc 恢复密钥
%% @param Uid 用户 ID
%% @param DecryptedShards 已解密的分片列表（从代理获取）
%% @returns {ok, PrivateKeyPem} | {error, Reason}
%% @doc 零信任架构：恢复时客户端从代理获取解密后的分片，服务端只负责重组
-spec recover_key(integer(), list(binary())) -> {ok, binary()} | {error, term()}.
recover_key(_Uid, DecryptedShards) ->
    try
        % 1. 验证分片数量
        case length(DecryptedShards) < ?MIN_THRESHOLD of
            true -> throw({error, {<<"分片数量不足，至少需要 2 个分片"/utf8>>, ?ERR_BAD_REQUEST}});
            false -> ok
        end,

        % 2. 使用 Shamir Secret Sharing 重组密钥
        PrivateKeyPem = shamir_secret_sharing:combine_shares(DecryptedShards),

        {ok, PrivateKeyPem}
    catch
        {error, {Msg, Code}} when is_binary(Msg), is_integer(Code) ->
            {error, {Msg, Code}};
        {error, Reason} when is_binary(Reason) ->
            {error, {Reason, ?ERR_INTERNAL_SERVER_ERROR}};
        _:Reason:_Stack ->
            ?ERROR_LOG([e2ee_social_logic, recover_key_failed, Reason]),
            {error, {<<"密钥恢复失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR}}
    end.

%% @doc C1 FIX: 验证分片列表并原样返回，不在服务端重组私钥
%% 客户端在本地完成 Shamir 重组，服务端只校验数量合法性
-spec validate_shards(integer(), list(binary())) -> {ok, list(binary())} | {error, term()}.
validate_shards(_Uid, DecryptedShards) ->
    case length(DecryptedShards) < ?MIN_THRESHOLD of
        true ->
            {error, {<<"分片数量不足，至少需要 2 个分片"/utf8>>, ?ERR_BAD_REQUEST}};
        false ->
            {ok, DecryptedShards}
    end.

%% @doc 检查是否可以恢复（基于服务端持久化分片）
-spec can_recover(integer(), binary()) -> {ok, boolean()} | {error, term()}.
can_recover(Uid, KeyVersion) ->
    e2ee_social_ds:can_recover(Uid, KeyVersion).

%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc 持久化分片记录
-spec persist_shards(list(map())) -> {ok, list(map())} | {error, term()}.
persist_shards(ShardRecords) ->
    try
        PersistedReversed = lists:foldl(
            fun(ShardRecord, Acc) ->
                case e2ee_social_ds:create_shard(ShardRecord) of
                    {ok, Id} ->
                        [ShardRecord#{<<"id">> => Id, <<"status">> => <<"active">>} | Acc];
                    {error, Reason} ->
                        throw(
                            {error, {elib_cnv:safe_to_binary(Reason), ?ERR_INTERNAL_SERVER_ERROR}}
                        )
                end
            end,
            [],
            ShardRecords
        ),
        {ok, lists:reverse(PersistedReversed)}
    catch
        {error, Reason} ->
            {error, Reason}
    end.
