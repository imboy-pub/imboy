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
-export([create_shards/6]).
-export([get_user_shards/2]).
-export([get_proxy_shards/1]).
-export([recover_key/2]).
-export([can_recover/2]).

%% @doc 创建恢复分片
%% @param Uid 用户 ID
%% @param KeyVersion 密钥版本号
%% @param TotalShards 总分片数
%% @param Threshold 恢复阈值
%% @param PrivateKeyPem 私钥 PEM 格式
%% @param Proxies 代理列表（兼容 map/tuple）
%% @returns {ok, Shards} | {error, Reason}
%% @doc 服务端生成并持久化加密分片，返回分片元数据给客户端
-spec create_shards(
    integer(),
    binary(),
    integer(),
    integer(),
    binary(),
    list(map() | {term(), term()})
) -> {ok, list(map())} | {error, term()}.
create_shards(Uid, KeyVersion, TotalShards, Threshold, PrivateKeyPem, Proxies) ->
    try
        % 1. 验证参数（安全加固）
        % 验证阈值下限
        case Threshold < ?MIN_THRESHOLD of
            true -> throw({error, {<<"阈值至少需要 2"/utf8>>, ?ERR_BAD_REQUEST}});
            false -> ok
        end,

        % 验证阈值上限
        case Threshold > ?MAX_THRESHOLD of
            true -> throw({error, {<<"阈值不能超过 3"/utf8>>, ?ERR_BAD_REQUEST}});
            false -> ok
        end,

        % 验证总分片数上限（防止 DOS 攻击）
        case TotalShards > ?MAX_SHARDS of
            true -> throw({error, {<<"分片数不能超过 5"/utf8>>, ?ERR_BAD_REQUEST}});
            false -> ok
        end,

        % 验证分片数必须大于等于阈值
        case TotalShards =< Threshold of
            true -> throw({error, {<<"分片数必须大于阈值"/utf8>>, ?ERR_BAD_REQUEST}});
            false -> ok
        end,

        % 验证代理数量
        case length(Proxies) < TotalShards of
            true -> throw({error, {<<"代理数量不足"/utf8>>, ?ERR_BAD_REQUEST}});
            false -> ok
        end,

        % 验证私钥非空
        case PrivateKeyPem of
            <<>> -> throw({error, {<<"私钥不能为空"/utf8>>, ?ERR_BAD_REQUEST}});
            _ -> ok
        end,

        % 2. 使用 Shamir Secret Sharing 分割私钥
        Shards = shamir_secret_sharing:split_secret(PrivateKeyPem, TotalShards, Threshold),

        % 3. 为每个分片加密（使用代理的公钥）
        {ok, ShardRecords} = encrypt_shards_for_proxies(
            Uid, KeyVersion, Shards, Proxies, TotalShards, Threshold
        ),

        % 4. 服务端持久化分片记录（仅加密态）
        PersistedShardRecords =
            case persist_shards(ShardRecords) of
                {ok, Records} ->
                    Records;
                {error, PersistReason} ->
                    throw({error, PersistReason})
            end,

        % 5. 记录分片创建日志
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
                        <<"total_shards">> => TotalShards,
                        <<"threshold">> => Threshold
                    }
                )
            end,
            PersistedShardRecords
        ),

        % 转换 ID 为 binary（客户端安全传输）
        ClientShards = [
            S#{
                <<"uid">> => maps:get(<<"uid">>, S),
                <<"proxy_uid">> => maps:get(<<"proxy_uid">>, S)
            }
         || S <- PersistedShardRecords
        ],
        {ok, ClientShards}
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

%% @doc 检查是否可以恢复（基于服务端持久化分片）
-spec can_recover(integer(), binary()) -> {ok, boolean()} | {error, term()}.
can_recover(Uid, KeyVersion) ->
    e2ee_social_ds:can_recover(Uid, KeyVersion).

%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc 加密分片用于代理存储
%% @param Uid 用户 ID
%% @param KeyVersion 密钥版本号
%% @param Shards 分片列表
%% @param Proxies 代理列表（兼容 map/tuple）
%% @param TotalShards 总分片数
%% @param Threshold 恢复阈值
%% @returns {ok, list(map())} 加密后的分片列表
-spec encrypt_shards_for_proxies(
    integer(),
    binary(),
    list(binary() | map()),
    list(map() | {term(), term()}),
    integer(),
    integer()
) -> {ok, list(map())}.
encrypt_shards_for_proxies(Uid, KeyVersion, Shards, Proxies, TotalShards, Threshold) ->
    % 为每个分片分配代理并加密
    EncryptedShardList = lists:map(
        fun({ShardJson, Index}) ->
            ShardIndex = Index - 1,
            Proxy = lists:nth(Index, Proxies),
            {ProxyUid, EncryptedPublicKey} = normalize_proxy(Proxy),
            ShardPayload = encode_shard_payload(ShardJson),

            % 使用代理的公钥加密分片
            EncryptedShard = encrypt_shard_for_proxy(ShardPayload, EncryptedPublicKey),

            ShardId = elib_uuid:gen_v7(),

            #{
                <<"uid">> => Uid,
                <<"key_version">> => KeyVersion,
                <<"shard_index">> => ShardIndex,
                <<"total_shards">> => TotalShards,
                <<"threshold">> => Threshold,
                <<"encrypted_shard">> => EncryptedShard,
                <<"proxy_uid">> => ProxyUid,
                <<"shard_id">> => ShardId,
                <<"status">> => <<"active">>
            }
        end,
        lists:zip(Shards, lists:seq(1, length(Shards)))
    ),
    {ok, EncryptedShardList}.

%% @doc 兼容代理参数协议（map 和 tuple）
-spec normalize_proxy(map() | {term(), term()}) -> {term(), binary()} | no_return().
normalize_proxy({ProxyUid, EncryptedPublicKey}) when is_binary(EncryptedPublicKey) ->
    {ProxyUid, EncryptedPublicKey};
normalize_proxy(#{<<"proxy_uid">> := ProxyUid, <<"encrypted_public_key">> := EncryptedPublicKey}) when
    is_binary(EncryptedPublicKey)
->
    {ProxyUid, EncryptedPublicKey};
normalize_proxy(#{proxy_uid := ProxyUid, encrypted_public_key := EncryptedPublicKey}) when
    is_binary(EncryptedPublicKey)
->
    {ProxyUid, EncryptedPublicKey};
normalize_proxy(_) ->
    throw({error, {<<"代理参数格式错误"/utf8>>, ?ERR_BAD_REQUEST}}).

%% @doc 将分片统一编码为二进制，满足 RSA-OAEP 入参要求
-spec encode_shard_payload(binary() | map()) -> binary() | no_return().
encode_shard_payload(Shard) when is_binary(Shard) ->
    Shard;
encode_shard_payload(Shard) when is_map(Shard) ->
    jsx:encode(Shard);
encode_shard_payload(_) ->
    throw({error, {<<"分片数据格式错误"/utf8>>, ?ERR_BAD_REQUEST}}).

%% @doc 加密分片用于代理存储
-spec encrypt_shard_for_proxy(binary(), binary()) -> binary() | no_return().
encrypt_shard_for_proxy(Shard, ProxyPublicKeyPem) ->
    % 使用 elib_cipher 中的 RSA-OAEP 加密
    case elib_cipher:encrypt_rsa_oaep(Shard, ProxyPublicKeyPem) of
        {ok, Encrypted} ->
            Encrypted;
        {error, Reason} ->
            throw({error, {elib_cnv:safe_to_binary(Reason), ?ERR_INTERNAL_SERVER_ERROR}})
    end.

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
