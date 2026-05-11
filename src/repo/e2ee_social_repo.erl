-module(e2ee_social_repo).
%%%===================================================================
%%% @doc E2EE 社交恢复 Repo 层
%%%
%%% 管理 E2EE 社交恢复的数据库操作
%%%===================================================================

%%===================================================================
%%% API Functions Export
%%===================================================================
-export([create/1]).
-export([get_user_shards/2]).
-export([get_proxy_shards/1]).
-export([find_shard_by_id/1]).
-export([delete_restored_shards/2]).
-export([can_recover/2]).
-export([generate_shard_id/0]).
-export([add_contact/1]).
-export([remove_contact/2]).
-export([list_contacts/1]).

%%===================================================================
%%% Query Functions
%%===================================================================

%% @doc 创建恢复分片
-spec create(map()) -> {ok, integer()} | {error, term()}.
create(Params) ->
    Uid = maps:get(<<"uid">>, Params),
    KeyVersion = maps:get(<<"key_version">>, Params),
    ShardIndex = maps:get(<<"shard_index">>, Params),
    TotalShards = maps:get(<<"total_shards">>, Params),
    Threshold = maps:get(<<"threshold">>, Params),
    EncryptedShard = maps:get(<<"encrypted_shard">>, Params),
    ProxyUid = maps:get(<<"proxy_uid">>, Params),
    ShardId = maps:get(<<"shard_id">>, Params),

    Id = elib_tsid:generate(e2ee_social),
    Sql1 = <<"INSERT INTO e2ee_social_shards ",
             "(id, uid, key_version, shard_index, total_shards, threshold, ",
             "encrypted_shard, proxy_uid, shard_id, status) "
             "VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, 'active')">>,
    case elib_pg:execute(Sql1, [Id, Uid, KeyVersion, ShardIndex, TotalShards, Threshold,
                             EncryptedShard, ProxyUid, ShardId]) of
        {ok, _Count} -> {ok, Id};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 获取用户的所有恢复分片
-spec get_user_shards(integer(), binary()) -> {ok, list(map())} | {error, term()}.
get_user_shards(Uid, KeyVersion) ->
    Sql1 = <<"SELECT id, uid, key_version, shard_index, total_shards, threshold, ",
             "encrypted_shard, proxy_uid, shard_id, status, created_at, used_at ",
             "FROM e2ee_social_shards ",
             "WHERE uid = $1 AND key_version = $2 ",
             "ORDER BY shard_index">>,
    case elib_pg:query(Sql1, [Uid, KeyVersion]) of
        {ok, Rows} -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 获取用户作为代理的所有分片
-spec get_proxy_shards(integer()) -> {ok, list(map())} | {error, term()}.
get_proxy_shards(ProxyUid) ->
    Sql1 = <<"SELECT id, uid, key_version, shard_index, total_shards, threshold, ",
             "encrypted_shard, shard_id, status, created_at, used_at ",
             "FROM e2ee_social_shards ",
             "WHERE proxy_uid = $1 AND status = 'active' ",
             "ORDER BY created_at DESC">>,
    case elib_pg:query(Sql1, [ProxyUid]) of
        {ok, Rows} -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 标记分片已使用
-spec mark_shard_used(integer()) -> ok | {error, term()}.
mark_shard_used(ShardId) ->
    Sql1 = <<"UPDATE e2ee_social_shards ",
             "SET status = 'used', used_at = NOW() ",
             "WHERE id = $1">>,
    case elib_pg:execute(Sql1, [ShardId]) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 删除已恢复的分片
-spec delete_restored_shards(integer(), binary()) -> ok | {error, term()}.
delete_restored_shards(Uid, KeyVersion) ->
    Sql1 = <<"DELETE FROM e2ee_social_shards ",
             "WHERE uid = $1 AND key_version = $2 ",
             "AND status = 'used'">>,
    case elib_pg:execute(Sql1, [Uid, KeyVersion]) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 检查用户是否有足够的分片恢复
-spec can_recover(integer(), binary()) -> {ok, boolean()} | {error, term()}.
can_recover(Uid, KeyVersion) ->
    Sql1 = <<"SELECT COUNT(*) as count, threshold, total_shards ",
             "FROM e2ee_social_shards ",
             "WHERE uid = $1 AND key_version = $2 AND status = 'active' ",
             "GROUP BY threshold, total_shards">>,
    case elib_pg:query(Sql1, [Uid, KeyVersion]) of
        {ok, [Row | _]} ->
            Count = maps:get(<<"count">>, Row),
            Threshold = maps:get(<<"threshold">>, Row),
            TotalShards = maps:get(<<"total_shards">>, Row),
            {ok, Count >= Threshold andalso TotalShards >= Threshold};
        {ok, []} ->
            {ok, false};
        {error, Reason} ->
            {error, Reason}
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

%% @doc 根据 ID 查找分片
-spec find_shard_by_id(binary()) -> {ok, map()} | {error, term()}.
find_shard_by_id(ShardId) ->
    Sql1 = <<"SELECT id, uid, key_version, shard_index, total_shards, threshold, ",
             "encrypted_shard, proxy_uid, shard_id, status, created_at, used_at ",
             "FROM e2ee_social_shards ",
             "WHERE shard_id = $1">>,
    case elib_pg:query(Sql1, [ShardId]) of
        {ok, []} -> {error, not_found};
        {ok, [Row | _]} -> {ok, Row};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 添加可信联系人
-spec add_contact(map()) -> {ok, integer()} | {error, term()}.
add_contact(Params) ->
    Uid = maps:get(<<"uid">>, Params),
    ContactUid = maps:get(<<"contact_uid">>, Params),
    Nickname = maps:get(<<"contact_nickname">>, Params, <<>>),

    Id = elib_tsid:generate(e2ee_social),
    Sql1 = <<"INSERT INTO e2ee_trusted_contacts ",
             "(id, uid, contact_uid, nickname) ",
             "VALUES ($1, $2, $3, $4) ",
             "ON CONFLICT (uid, contact_uid) ",
             "DO UPDATE SET nickname = $4">>,
    case elib_pg:execute(Sql1, [Id, Uid, ContactUid, Nickname]) of
        {ok, _Count} -> {ok, Id};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 移除可信联系人
-spec remove_contact(integer(), integer()) -> ok | {error, term()}.
remove_contact(Uid, ContactUid) ->
    Sql1 = <<"DELETE FROM e2ee_trusted_contacts ",
             "WHERE uid = $1 AND contact_uid = $2">>,
    case elib_pg:execute(Sql1, [Uid, ContactUid]) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 列出可信联系人
-spec list_contacts(integer()) -> {ok, list(map())} | {error, term()}.
list_contacts(Uid) ->
    Sql1 = <<"SELECT id, uid, contact_uid, contact_nickname, created_at ",
             "FROM e2ee_trusted_contacts ",
             "WHERE uid = $1 ",
             "ORDER BY created_at DESC">>,
    case elib_pg:query(Sql1, [Uid]) of
        {ok, Rows} -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.
