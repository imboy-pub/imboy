-module(enterprise_asset_repo).

%%%
% EPGZ-04 INT-07/08 企业附件仓储（Application 域 Garage 直传链）。
%
% 零新表复用既有存储（本卡禁建 migration）：
%   * attach_pending（迁移 54）——presign 登记待确认对象；scope 固定
%     'enterprise'，creator_user_id=0（Application 非 user，归属经 object_key
%     前缀 eoa/<org>/<app>/ 表达，与个人 u<Uid>/ 前缀物理隔离）。
%   * attachment（迁移 1/13/26）——confirm 转正行：scope='enterprise'
%     （varchar(16) 无 CHECK，10 字符），scope_ref='org:<org>/app:<app>'
%     编码 ownership，info jsonb 只存 origin 元数据（无 URL、无密钥）。
%
% 与 attach_pending_repo/attachment_repo 的关系：不改动它们（个人域共享
% 模块）；本模块提供 tx 版企业行操作，SQL 语义对齐 attach_logic 确认链
% （pending 登记失败不阻断签发；转正后销账）。
%%%

-export([
    object_key_prefix/2,
    build_object_key/3,
    owned_key/3,
    scope_ref/2,
    pending_add_tx/4,
    pending_find_tx/2,
    pending_remove_tx/2,
    confirm_save_tx/5,
    find_confirmed_tx/4,
    find_by_object_key_tx/2
]).

-include_lib("epgsql/include/epgsql.hrl").

-define(FILE_ID_PREFIX, "file").

%%%===================================================================
%%% Object key（企业域路径前缀，与个人 u<Uid>/ 隔离）
%%%===================================================================

-spec object_key_prefix(integer(), integer()) -> binary().
object_key_prefix(OrgId, AppId) ->
    <<"eoa/", (integer_to_binary(OrgId))/binary, "/", (integer_to_binary(AppId))/binary, "/">>.

%% @doc 生成企业域 object key：
%%   eoa/<org>/<app>/<Ymd>/file_<ms>_<rand16hex>/<basename>
%% 随机段（毫秒时间戳 + 8 字节 CSPRNG hex）保证 uk_attachment_path 唯一。
-spec build_object_key(integer(), integer(), binary()) -> binary().
build_object_key(OrgId, AppId, FileName) ->
    Ts = integer_to_binary(erlang:system_time(millisecond)),
    Rand = binary:encode_hex(crypto:strong_rand_bytes(8)),
    FileId = <<?FILE_ID_PREFIX, "_", Ts/binary, "_", Rand/binary>>,
    Ymd = ymd(),
    SafeName = safe_basename(FileName),
    <<
        (object_key_prefix(OrgId, AppId))/binary,
        Ymd/binary,
        "/",
        FileId/binary,
        "/",
        SafeName/binary
    >>.

%% @doc object_key 是否属于本 (org, app) 企业域前缀（confirm/引用时的
%% ownership 校验：跨 org/app 的 key 一律拒绝）。
-spec owned_key(binary(), integer(), integer()) -> boolean().
owned_key(ObjectKey, OrgId, AppId) when is_binary(ObjectKey) ->
    Prefix = object_key_prefix(OrgId, AppId),
    PrefixLen = byte_size(Prefix),
    byte_size(ObjectKey) >= PrefixLen andalso
        binary:part(ObjectKey, 0, PrefixLen) =:= Prefix;
owned_key(_, _, _) ->
    false.

%%%===================================================================
%%% attach_pending（presign 登记 / confirm 销账）
%%%===================================================================

%% @doc presign 登记待确认对象（迁移 54 生命周期：超龄未 confirm 由
%% attach_cleanup_logic 连同 S3 对象一并回收）。
-spec pending_add_tx(any(), binary(), binary(), binary()) ->
    {ok, inserted} | {error, term()}.
pending_add_tx(Conn, ObjectKey, Bucket, Scope) ->
    Sql =
        <<
            "INSERT INTO attach_pending (object_key, bucket, scope, creator_user_id)"
            " VALUES ($1, $2, $3, 0) ON CONFLICT (object_key) DO NOTHING"
        >>,
    case elib_pg:query(Conn, Sql, [ObjectKey, Bucket, Scope]) of
        {ok, [_]} -> {ok, inserted};
        {ok, N} when is_integer(N), N > 0 -> {ok, inserted};
        {ok, _} -> {ok, inserted};
        {error, Reason} -> {error, Reason}
    end.

-spec pending_find_tx(any(), binary()) -> {ok, map()} | {error, not_found | term()}.
pending_find_tx(Conn, ObjectKey) ->
    Sql =
        <<
            "SELECT object_key, bucket, scope, creator_user_id, created_at"
            " FROM attach_pending WHERE object_key = $1"
        >>,
    case elib_pg:query(Conn, Sql, [ObjectKey]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc confirm 转正后销账（迁移 54 语义：放在 attachment 落库同事务之后；
%% 本模块由调用方在同一 tx 内先 confirm_save_tx 再销账）。
-spec pending_remove_tx(any(), binary()) -> ok.
pending_remove_tx(Conn, ObjectKey) ->
    _ = elib_pg:execute(
        Conn,
        <<"DELETE FROM attach_pending WHERE object_key = $1">>,
        [ObjectKey]
    ),
    ok.

%%%===================================================================
%%% attachment 企业行（confirm 转正）
%%%===================================================================

%% @doc 确认落库（服务端 HEAD 核实值覆盖客户端自报值）。
%% InfoMap 只允许 origin 元数据（origin_kind/origin_application_id 等）；
%% cipher 恒 null——企业托管对象为服务端明文，不做客户端加密标记。
-spec confirm_save_tx(
    any(),
    binary(),
    binary(),
    non_neg_integer(),
    map()
) ->
    {ok, pos_integer()} | {error, term()}.
confirm_save_tx(Conn, ObjectKey, RealType, RealSize, InfoMap) ->
    AttId = next_attachment_id(),
    Ext = filename:extension(ObjectKey),
    Sql =
        <<
            "INSERT INTO attachment (id, file_hash256, mime_type, ext, name, path, url,"
            " size, info, referer_time, last_referer_user_id, last_referer_at,"
            " creator_user_id, scope, scope_ref, cipher, status, created_at, updated_at)"
            " VALUES ($1,$2,$3,$4,$5,$6,$6,$7,$8::jsonb,1,0,NOW(),0,'enterprise',$9,"
            " null,1,NOW(),NOW())"
            " ON CONFLICT (path) DO UPDATE SET referer_time = attachment.referer_time + 1,"
            " updated_at = NOW() RETURNING id"
        >>,
    Params = [
        AttId,
        maps:get(file_hash256, InfoMap, <<>>),
        RealType,
        Ext,
        filename:basename(ObjectKey),
        ObjectKey,
        RealSize,
        jsone:encode(InfoMap),
        maps:get(scope_ref, InfoMap, <<>>)
    ],
    case elib_pg:query(Conn, Sql, Params) of
        {ok, [#{<<"id">> := Id} | _]} -> {ok, Id};
        {ok, _} -> {ok, AttId};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 校验 object_key 已被本 (org, app) confirm（status=1 且 scope='enterprise'
%% 且 scope_ref 匹配）——消息引用附件的前置（confirm 后的 file 才能引用）。
-spec find_confirmed_tx(any(), integer(), integer(), binary()) ->
    {ok, map()} | {error, not_found}.
find_confirmed_tx(Conn, OrgId, AppId, ObjectKey) ->
    ScopeRef = scope_ref(OrgId, AppId),
    Sql =
        <<
            "SELECT id, path, name, mime_type, size, scope, scope_ref, info"
            " FROM attachment WHERE path = $1 AND status = 1 AND scope = 'enterprise'"
            " AND scope_ref = $2"
        >>,
    case elib_pg:query(Conn, Sql, [ObjectKey, ScopeRef]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, _} -> {error, not_found}
    end.

-spec find_by_object_key_tx(any(), binary()) -> {ok, map()} | {error, not_found}.
find_by_object_key_tx(Conn, ObjectKey) ->
    Sql =
        <<
            "SELECT id, path, name, mime_type, size, scope, scope_ref, info, status"
            " FROM attachment WHERE path = $1"
        >>,
    case elib_pg:query(Conn, Sql, [ObjectKey]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, _} -> {error, not_found}
    end.

-spec scope_ref(integer(), integer()) -> binary().
scope_ref(OrgId, AppId) ->
    <<"org:", (integer_to_binary(OrgId))/binary, "/app:", (integer_to_binary(AppId))/binary>>.

%%%===================================================================
%%% Internal
%%%===================================================================

next_attachment_id() ->
    ensure_tsid(attachment),
    elib_tsid:generate(attachment).

ensure_tsid(Name) ->
    case lists:member(Name, elib_tsid:registered()) of
        true -> ok;
        false -> elib_tsid:register(Name)
    end,
    ok.

safe_basename(FileName) ->
    Base = filename:basename(FileName),
    case byte_size(Base) of
        0 -> <<"file">>;
        _ -> Base
    end.

ymd() ->
    {{Y, M, D} = Date, _} = calendar:universal_time(),
    _ = Date,
    iolist_to_binary(io_lib:format("~4..0B~2..0B~2..0B", [Y, M, D])).
