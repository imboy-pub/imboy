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
-export([create_shards/4]).
-export([get_user_shards/2]).
-export([get_proxy_shards/1]).
-export([validate_shards/2]).
-export([can_recover/2]).
-export([get_proxy_shard/2]).
-export([consume_proxy_shard/2]).
-export([list_trusted_contacts/1]).
-export([add_trusted_contact/3]).
-export([remove_trusted_contact/2]).

%% @doc 创建恢复分片（C1 FIX：客户端已完成 Shamir 分割和代理公钥加密，服务端只存储）
%% @param Uid 用户 ID
%% @param KeyVersion 密钥版本号
%% @param Threshold 恢复门限（客户端 Shamir 分割时的 K 值，落库供 can_recover 判定）
%% @param Shards 客户端预加密分片列表 [{proxy_uid, encrypted_shard}]
%% @returns {ok, StoredShards} | {error, Reason}
-spec create_shards(
    integer(),
    binary(),
    integer(),
    list(map())
) -> {ok, list(map())} | {error, term()}.
create_shards(Uid, KeyVersion, Threshold, Shards) ->
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

        % 验证恢复门限：曾硬编码 0 落库导致 can_recover 门限形同虚设
        case
            is_integer(Threshold) andalso Threshold >= ?MIN_THRESHOLD andalso
                Threshold =< TotalShards
        of
            true -> ok;
            false -> throw({error, {<<"恢复门限必须在 [2, 分片总数] 内"/utf8>>, ?ERR_BAD_REQUEST}})
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
                    {ProxyUid, _} ->
                        %% 校验 proxy_uid 归属：必须是当前用户的可信联系人，
                        %% 防止向任意 uid 注入恢复分片（越权写入）。
                        case e2ee_social_ds:is_trusted_contact(Uid, ProxyUid) of
                            true ->
                                ok;
                            false ->
                                throw(
                                    {error, {<<"proxy_uid 非当前用户的可信联系人"/utf8>>, ?ERR_FORBIDDEN}}
                                )
                        end
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
                    <<"threshold">> => Threshold,
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

%% @doc 根据分片 ID 和代理 UID 获取单条代理分片记录，用于后续解密
-spec get_proxy_shard(binary(), integer()) -> {ok, map()} | {error, term()}.
get_proxy_shard(ShardId, ProxyUid) ->
    e2ee_social_ds:get_proxy_shard(ShardId, ProxyUid).

%% @doc 取用代理分片（一次性语义）：成功即置 used 并写审计日志
%% 已取用/已撤销的分片不可重复取用，封堵"代理任意时间合谋重放重建私钥"
-spec consume_proxy_shard(binary(), integer()) -> {ok, map()} | {error, term()}.
consume_proxy_shard(ShardId, ProxyUid) ->
    case e2ee_social_ds:consume_proxy_shard(ShardId, ProxyUid) of
        {ok, Shard} ->
            e2ee_shard_validator:log_shard_transmission(
                shard_decrypted,
                ShardId,
                #{
                    <<"uid">> => maps:get(<<"uid">>, Shard),
                    <<"proxy_uid">> => ProxyUid,
                    <<"key_version">> => maps:get(<<"key_version">>, Shard, <<"latest">>)
                }
            ),
            {ok, Shard};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 列出当前用户的所有可信联系人
-spec list_trusted_contacts(integer()) -> {ok, [map()]} | {error, term()}.
list_trusted_contacts(Uid) ->
    e2ee_social_ds:list_trusted_contacts(Uid).

%% @doc 添加可信联系人（含好友校验和自我添加校验）
-spec add_trusted_contact(integer(), integer(), binary()) -> ok | {error, term()}.
add_trusted_contact(Uid, ContactUid, Nickname) ->
    e2ee_social_ds:add_trusted_contact(Uid, ContactUid, Nickname).

%% @doc 移除当前用户的一个可信联系人
-spec remove_trusted_contact(integer(), integer()) -> ok | {error, term()}.
remove_trusted_contact(Uid, ContactUid) ->
    e2ee_social_ds:remove_trusted_contact(Uid, ContactUid).

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
