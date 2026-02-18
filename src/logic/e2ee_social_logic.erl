-module(e2ee_social_logic).
-dialyzer(no_return).
%%%===================================================================
%%% @doc E2EE 社交恢复 Logic 层
%%%
%%% 零信任架构：服务端不存储分片，分片通过 WebSocket 直接发送给代理
%%% 代理将分片存储在本地设备，服务端仅作为传输通道
%%%===================================================================

%%===================================================================
%%% API Functions Export
%%===================================================================
-include("log.hrl").
-include("error_code.hrl").

%% 安全限制常量
-define(MAX_SHARDS, 5).        % 最大分片数（防止 DOS）
-define(MIN_THRESHOLD, 2).     % 最小阈值
-define(MAX_THRESHOLD, 3).     % 最大阈值（实际需要的分片数）

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
%% @param Proxies 代理用户 ID 列表 [{proxy_uid, encrypted_public_key}]
%% @returns {ok, Shards} | {error, Reason}
%% @doc 零信任架构：服务端不存储分片，只返回加密后的分片给客户端
%%       客户端需要通过 WebSocket 将分片发送给对应的代理
-spec create_shards(
    integer(),
    binary(),
    integer(),
    integer(),
    binary(),
    list(map())
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
        case TotalShards < Threshold of
            true -> throw({error, {<<"分片数必须大于等于阈值"/utf8>>, ?ERR_BAD_REQUEST}});
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

        % 4. 记录分片创建日志（零信任架构审计）
        lists:foreach(fun(ShardRecord) ->
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
        end, ShardRecords),

        % 注意：服务端不存储分片，只返回给客户端
        % 客户端需要通过 WebSocket 将分片发送给对应的代理

        {ok, ShardRecords}
    catch
        {error, {Msg, Code}} when is_binary(Msg), is_integer(Code) ->
            {error, {Msg, Code}};
        {error, Reason} when is_binary(Reason) ->
            {error, {Reason, ?ERR_INTERNAL_SERVER_ERROR}};
        _:Reason:_Stack ->
            ?ERROR_LOG([e2ee_social_logic, create_shards_failed, Reason]),
            {error, {<<"创建分片失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR}}
    end.

%% @doc 获取用户的所有恢复分片
%% @doc 零信任架构：从代理设备获取分片信息，服务端不存储
-spec get_user_shards(integer(), binary()) -> {ok, list(map())} | {error, term()}.
get_user_shards(_Uid, _KeyVersion) ->
    % 零信任架构：分片存储在代理设备，服务端无法直接获取
    % 这里返回空列表，实际分片需要通过 WebSocket 从代理获取
    {ok, []}.

%% @doc 获取用户作为代理的所有分片
%% @doc 零信任架构：代理的分片存储在本地设备，不在服务端
%% @deprecated 代理应从本地 Secure Storage 读取分片
-spec get_proxy_shards(integer()) -> {ok, list(map())} | {error, term()}.
get_proxy_shards(_ProxyUid) ->
    % 零信任架构：代理的分片存储在本地设备
    % 返回空列表，实际分片应从本地 Secure Storage 读取
    {ok, []}.

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

%% @doc 检查是否可以恢复
%% @doc 零信任架构：客户端自行检查是否有足够的代理愿意协助
%% @deprecated 客户端应自行联系代理确认是否可恢复
-spec can_recover(_Uid, _KeyVersion) -> {ok, boolean()}.
can_recover(_, _) ->
    % 零信任架构：服务端无法知道代理是否有分片
    % 返回 false，让客户端自行联系代理确认
    {ok, false}.

%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc 加密分片用于代理存储
%% @doc 零信任架构：不存储到数据库，返回给客户端由其通过 WebSocket 发送
%% @param Uid 用户 ID
%% @param KeyVersion 密钥版本号
%% @param Shards 分片列表
%% @param Proxies 代理列表 [{proxy_uid, encrypted_public_key}]
%% @param TotalShards 总分片数
%% @param Threshold 恢复阈值
%% @returns {ok, list(map())} 加密后的分片列表
-spec encrypt_shards_for_proxies(
    integer(),
    binary(),
    list(binary()),
    list(map()),
    integer(),
    integer()
) -> {ok, list(map())}.
encrypt_shards_for_proxies(Uid, KeyVersion, Shards, Proxies, TotalShards, Threshold) ->
    % 为每个分片分配代理并加密
    EncryptedShardList = lists:map(fun({ShardJson, Index}) ->
        ShardIndex = Index - 1,
        {ProxyUid, EncryptedPublicKey} = lists:nth(Index, Proxies),

        % 使用代理的公钥加密分片
        EncryptedShard = encrypt_shard_for_proxy(ShardJson, EncryptedPublicKey),

        % 生成分片 ID（用于后续恢复时识别）
        ShardId = generate_shard_id(),

        % 返回分片信息，不存储到数据库
        #{
            <<"uid">> => Uid,
            <<"key_version">> => KeyVersion,
            <<"shard_index">> => ShardIndex,
            <<"total_shards">> => TotalShards,
            <<"threshold">> => Threshold,
            <<"encrypted_shard">> => EncryptedShard,
            <<"proxy_uid">> => ProxyUid,
            <<"shard_id">> => ShardId,
            <<"status">> => <<"pending">>  % 等待发送给代理
        }
    end, lists:zip(Shards, lists:seq(1, length(Shards)))),
    {ok, EncryptedShardList}.

%% @doc 加密分片用于代理存储
-spec encrypt_shard_for_proxy(binary(), binary()) -> binary() | {error, term()}.
encrypt_shard_for_proxy(Shard, ProxyPublicKeyPem) ->
    % 使用 elib_cipher 中的 RSA-OAEP 加密
    case elib_cipher:encrypt_rsa_oaep(Shard, ProxyPublicKeyPem) of
        {ok, Encrypted} -> Encrypted;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 生成分片 ID
-spec generate_shard_id() -> binary().
generate_shard_id() ->
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
    C4 = (C band 16#0FFF) bor 16#4000,
    D4 = (D band 16#3FFF) bor 16#8000,
    Str = lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b",
        [A, B, C4, D4, E])),
    list_to_binary(Str).
