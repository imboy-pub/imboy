-module(e2ee_social_ds).
-dialyzer(no_return).
%%%===================================================================
%%% @doc E2EE 社交恢复 DS 层
%%%
%%% 数据服务层，封装社交恢复功能的数据库操作和缓存逻辑
%%% 调用 e2ee_social_repo 进行数据库操作
%%%===================================================================

%%===================================================================
%%% API Functions Export
%%===================================================================
-export([add_trusted_contact/3]).
-export([remove_trusted_contact/2]).
-export([list_trusted_contacts/1]).
-export([is_trusted_contact/2]).
-export([create_key_shares/5]).
-export([get_user_shards/2]).
-export([get_proxy_shards/1]).
-export([get_proxy_shard/2]).
-export([get_shard_by_id/2]).
-export([decrypt_shard/2]).
-export([recover_key/3]).
-export([can_recover/2]).
-export([delete_restored_shards/2]).
-export([clear_trusted_contacts_cache/1]).
-export([clear_user_shards_cache/1]).

%%===================================================================
%%% API Functions - 可信联系人管理
%%===================================================================

%% @doc 添加可信联系人
-spec add_trusted_contact(integer(), integer(), binary()) -> ok | {error, term()}.
add_trusted_contact(Uid, ContactUid, Nickname) ->
    % 检查是否添加自己
    case Uid =:= ContactUid of
        true ->
            {error, cannot_add_self};
        false ->
            % 检查是否为好友
            case friend_ds:is_friend(Uid, ContactUid) of
                true ->
                    ContactMap = #{
                        <<"uid">> => Uid,
                        <<"contact_uid">> => ContactUid,
                        <<"contact_nickname">> => Nickname
                    },
                    case e2ee_social_repo:add_contact(ContactMap) of
                        {ok, _} ->
                            % 清除缓存
                            clear_trusted_contacts_cache(Uid),
                            ok;
                        {error, Reason} ->
                            {error, Reason}
                    end;
                false ->
                    {error, not_friend}
            end
    end.

%% @doc 移除可信联系人
-spec remove_trusted_contact(integer(), integer()) -> ok | {error, term()}.
remove_trusted_contact(Uid, ContactUid) ->
    case e2ee_social_repo:remove_contact(Uid, ContactUid) of
        ok ->
            % 清除缓存
            clear_trusted_contacts_cache(Uid),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 列出用户的所有可信联系人
-spec list_trusted_contacts(integer()) -> {ok, [map()]} | {error, term()}.
list_trusted_contacts(Uid) ->
    CacheKey = {e2ee_trusted_contacts, Uid},
    case imboy_cache:get(CacheKey) of
        {ok, Contacts} ->
            {ok, Contacts};
        undefined ->
            case e2ee_social_repo:list_contacts(Uid) of
                {ok, Contacts} ->
                    imboy_cache:set(CacheKey, Contacts, 300),
                    {ok, Contacts};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @doc 检查是否为可信联系人
-spec is_trusted_contact(integer(), integer()) -> boolean().
is_trusted_contact(Uid, ContactUid) ->
    case list_trusted_contacts(Uid) of
        {ok, Contacts} ->
            lists:any(fun(C) ->
                maps:get(<<"contact_uid">>, C) =:= ContactUid
            end, Contacts);
        _ ->
            false
    end.

%%===================================================================
%%% API Functions - 密钥分片管理
%%===================================================================

%% @doc 创建密钥分片
%% @param Uid 用户 ID
%% @param Proxies 代理用户 ID 列表 [{proxy_uid, nickname}]
%% @param PrivateKeyPem 私钥 PEM 格式
%% @param TotalShards 总分片数（默认 3）
%% @param Threshold 恢复阈值（默认 2）
-spec create_key_shares(integer(), list({integer(), binary()}), binary(), pos_integer(), pos_integer()) ->
    {ok, list(map())} | {error, term()}.
create_key_shares(Uid, Proxies, PrivateKeyPem, TotalShards, Threshold) ->
    try
        % 1. 验证参数
        case length(Proxies) < TotalShards of
            true -> throw({error, insufficient_proxies});
            false -> ok
        end,

        case Threshold > TotalShards of
            true -> throw({error, invalid_threshold});
            false -> ok
        end,

        % 2. 使用 Shamir Secret Sharing 分割私钥
        Shares = shamir_secret_sharing:create_shares(PrivateKeyPem, Threshold, TotalShards),

        % 3. 为每个分片创建加密记录
        ShardRecords = lists:map(fun({ShareMap, Index}) ->
            {ProxyUid, _Nickname} = lists:nth(Index, Proxies),

            % 获取代理的公钥
            {ok, ProxyDevice} = user_device_ds:get_default_device(ProxyUid),
            ProxyPublicKey = maps:get(<<"public_key">>, ProxyDevice),

            % 使用代理公钥加密分片
            ShareJson = jsx:encode(ShareMap),
            {ok, EncryptedShard} = elib_cipher:encrypt_rsa_oaep(ShareJson, ProxyPublicKey),

            ShardId = e2ee_social_repo:generate_shard_id(),

            ShardRecord = #{
                <<"uid">> => Uid,
                <<"key_version">> => <<"latest">>,
                <<"shard_index">> => Index - 1,
                <<"total_shards">> => TotalShards,
                <<"threshold">> => Threshold,
                <<"encrypted_shard">> => EncryptedShard,
                <<"proxy_uid">> => ProxyUid,
                <<"shard_id">> => ShardId
            },

            case e2ee_social_repo:create(ShardRecord) of
                {ok, Id} ->
                    ShardRecord#{<<"id">> => Id};
                {error, Reason} ->
                    throw({error, Reason})
            end
        end, lists:zip(Shares, lists:seq(1, length(Shares)))),

        % 4. 清除用户分片缓存
        clear_user_shards_cache(Uid),

        {ok, ShardRecords}
    catch
        {error, Reason} ->
            {error, Reason};
        _:Error:Stack ->
            {error, {Error, Stack}}
    end.

%% @doc 获取用户的所有密钥分片
-spec get_user_shards(integer(), binary()) -> {ok, [map()]} | {error, term()}.
get_user_shards(Uid, KeyVersion) ->
    CacheKey = {e2ee_user_shards, Uid, KeyVersion},
    case imboy_cache:get(CacheKey) of
        {ok, Shards} ->
            {ok, Shards};
        undefined ->
            case e2ee_social_repo:get_user_shards(Uid, KeyVersion) of
                {ok, Shards} ->
                    imboy_cache:set(CacheKey, Shards, 300),
                    {ok, Shards};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @doc 获取用户作为代理的所有分片
-spec get_proxy_shards(integer()) -> {ok, [map()]} | {error, term()}.
get_proxy_shards(ProxyUid) ->
    CacheKey = {e2ee_proxy_shards, ProxyUid},
    case imboy_cache:get(CacheKey) of
        {ok, Shards} ->
            {ok, Shards};
        undefined ->
            case e2ee_social_repo:get_proxy_shards(ProxyUid) of
                {ok, Shards} ->
                    imboy_cache:set(CacheKey, Shards, 300),
                    {ok, Shards};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @doc 获取单个分片（用于恢复）
-spec get_shard_by_id(integer(), binary()) -> {ok, map()} | {error, term()}.
get_shard_by_id(Uid, ShardId) ->
    case e2ee_social_repo:find_shard_by_id(ShardId) of
        {ok, Shard} ->
            % 验证分片属于该用户
            ShardUid = maps:get(<<"uid">>, Shard),
            case ShardUid =:= Uid of
                true ->
                    {ok, Shard};
                false ->
                    {error, unauthorized}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 获取代理的分片（用于解密）
%% 验证指定用户是否是某个分片的代理，并返回该分片
-spec get_proxy_shard(binary(), integer()) -> {ok, map()} | {error, term()}.
get_proxy_shard(ShardId, ProxyUid) ->
    case e2ee_social_repo:find_shard_by_id(ShardId) of
        {ok, Shard} ->
            % 验证当前用户是该分片的代理
            ShardProxyUid = maps:get(<<"proxy_uid">>, Shard),
            ShardStatus = maps:get(<<"status">>, Shard, <<>>),
            case ShardProxyUid =:= ProxyUid andalso ShardStatus =:= <<"active">> of
                true ->
                    {ok, Shard};
                false when ShardProxyUid =/= ProxyUid ->
                    {error, not_proxy};
                false ->
                    {error, shard_not_active}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 解密分片（需要代理私钥）
%% 注意：这个函数需要代理提供私钥，实际实现中应该通过 API 调用
-spec decrypt_shard(binary(), binary()) -> {ok, binary()} | {error, term()}.
decrypt_shard(EncryptedShard, PrivateKeyPem) ->
    try
        {ok, Decrypted} = elib_cipher:decrypt_rsa_oaep(EncryptedShard, PrivateKeyPem),
        ShareMap = jsx:decode(Decrypted, [return_maps]),
        {ok, ShareMap}
    catch
        _:_ ->
            {error, decryption_failed}
    end.

%% @doc 恢复密钥
-spec recover_key(integer(), binary(), list(binary())) -> {ok, binary()} | {error, term()}.
recover_key(Uid, KeyVersion, ShardIds) ->
    try
        % 1. 获取用户的所有分片
        {ok, AllShards} = get_user_shards(Uid, KeyVersion),

        % 2. 过滤出要使用的分片
        SelectedShards = lists:filter(fun(Shard) ->
            ShardId = maps:get(<<"shard_id">>, Shard),
            lists:member(ShardId, ShardIds)
        end, AllShards),

        % 3. 验证分片数量
        Threshold = case AllShards of
            [S | _] -> maps:get(<<"threshold">>, S);
            [] -> throw({error, no_shares})
        end,

        case length(SelectedShards) < Threshold of
            true -> throw({error, insufficient_shares});
            false -> ok
        end,

        % 4. 抛出错误，因为需要代理解密
        throw({error, proxy_decrypt_required})
    catch
        throw:{error, ErrorReason} ->
            {error, ErrorReason};
        _:Error:Stack ->
            {error, {Error, Stack}}
    end.

%% @doc 检查用户是否可以恢复密钥
-spec can_recover(integer(), binary()) -> {ok, boolean()} | {error, term()}.
can_recover(Uid, KeyVersion) ->
    e2ee_social_repo:can_recover(Uid, KeyVersion).

%% @doc 删除已恢复的分片
-spec delete_restored_shards(integer(), binary()) -> ok | {error, term()}.
delete_restored_shards(Uid, KeyVersion) ->
    case e2ee_social_repo:delete_restored_shards(Uid, KeyVersion) of
        ok ->
            clear_user_shards_cache(Uid),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%%===================================================================
%%% Internal Functions
%%===================================================================

%% @doc 清除可信联系人缓存
-spec clear_trusted_contacts_cache(integer()) -> ok.
clear_trusted_contacts_cache(Uid) ->
    CacheKey = {e2ee_trusted_contacts, Uid},
    imboy_cache:delete(CacheKey),
    ok.

%% @doc 清除用户分片缓存
-spec clear_user_shards_cache(integer()) -> ok.
clear_user_shards_cache(Uid) ->
    CacheKey = {e2ee_user_shards, Uid, <<"latest">>},
    imboy_cache:delete(CacheKey),
    ok.
