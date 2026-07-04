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
-export([get_user_shards/2]).
-export([get_proxy_shards/1]).
-export([get_proxy_shard/2]).
-export([consume_proxy_shard/2]).
-export([get_shard_by_id/2]).
-export([can_recover/2]).
-export([delete_restored_shards/2]).
-export([create_shard/1]).

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
%% 级联失效该联系人持有的活跃分片，防止撤销信任后其仍可参与恢复合谋。
%% 先撤分片再删联系人：分片撤销失败时整体失败，联系人保留可重试，
%% 避免出现"联系人已删但分片仍活跃"的不一致。
-spec remove_trusted_contact(integer(), integer()) -> ok | {error, term()}.
remove_trusted_contact(Uid, ContactUid) ->
    case e2ee_social_repo:revoke_shards_by_proxy(Uid, ContactUid) of
        {ok, _} ->
            case e2ee_social_repo:remove_contact(Uid, ContactUid) of
                ok ->
                    % 清除缓存
                    clear_trusted_contacts_cache(Uid),
                    clear_user_shards_cache(Uid),
                    clear_proxy_shards_cache(ContactUid),
                    ok;
                {error, Reason} ->
                    {error, Reason}
            end;
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
            lists:any(
                fun(C) ->
                    maps:get(<<"contact_uid">>, C) =:= ContactUid
                end,
                Contacts
            );
        _ ->
            false
    end.

%%===================================================================
%%% API Functions - 密钥分片管理
%%===================================================================

%% @doc 获取用户的所有密钥分片
-spec get_user_shards(integer(), binary()) -> {ok, [map()]} | {error, term()}.
get_user_shards(Uid, KeyVersion) ->
    %% 不做读缓存：分片曾按 {e2ee_user_shards, Uid, KeyVersion} 分键缓存，
    %% 而 clear_user_shards_cache/1 只清 <<"latest">> 版本键；撤销可信联系人
    %% （revoke_shards_by_proxy）或删除已恢复分片后，带具体版本号的缓存不会失效，
    %% 会把已 revoked 分片当作 active 读出（最长 300s），削弱撤销即时性。
    %% 恢复分片读取属低频操作，直接查库消除该陈旧窗口。
    e2ee_social_repo:get_user_shards(Uid, KeyVersion).

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

%% @doc 取用代理分片（一次性语义）
%% 读取并校验归属后，CAS 将分片置为 used；已取用/已撤销的分片不可再取。
%% CAS 影响 0 行 = 并发取用竞争失败，按 shard_not_active 处理。
-spec consume_proxy_shard(binary(), integer()) -> {ok, map()} | {error, term()}.
consume_proxy_shard(ShardId, ProxyUid) ->
    case get_proxy_shard(ShardId, ProxyUid) of
        {ok, Shard} ->
            case e2ee_social_repo:mark_shard_used(ShardId, ProxyUid) of
                {ok, N} when N > 0 ->
                    clear_proxy_shards_cache(ProxyUid),
                    {ok, Shard};
                {ok, _} ->
                    {error, shard_not_active};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
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

%% @doc 清除代理分片缓存（分片取用/撤销后失效）
-spec clear_proxy_shards_cache(integer()) -> ok.
clear_proxy_shards_cache(ProxyUid) ->
    CacheKey = {e2ee_proxy_shards, ProxyUid},
    imboy_cache:delete(CacheKey),
    ok.

%% G3: e2ee_social_logic 不应直调 e2ee_social_repo
-spec create_shard(map()) -> {ok, integer()} | {error, term()}.
create_shard(ShardRecord) ->
    Result = e2ee_social_repo:create(ShardRecord),
    %% 新分片落库后失效代理的 proxy_shards 缓存（TTL 300s）：
    %% 否则代理调 get_proxy_shards 最多 300s 看不到新分片，拖慢恢复协作。
    case Result of
        {ok, _} ->
            clear_proxy_shards_cache(ec_cnv:to_integer(maps:get(<<"proxy_uid">>, ShardRecord))),
            Result;
        _ ->
            Result
    end.
