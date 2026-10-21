-module(enterprise_asset_repo).

-export([
    object_key_prefix/2,
    build_object_key/3,
    owned_key/3,
    scope_ref/2,
    pending_add_tx/4,
    pending_find_tx/2,
    recovery_session_tx/2,
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
owned_key(ObjectKey, OrgId, AppId) ->
    Prefix = object_key_prefix(OrgId, AppId),
    PrefixLen = byte_size(Prefix),
    byte_size(ObjectKey) >= PrefixLen andalso
        binary:part(ObjectKey, 0, PrefixLen) =:= Prefix.

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
    %% The application confirm path must enter I/path before taking pending.
    %% 应用确认入口先取统一对象锁；禁止 pending 行锁后再等待身份锁。
    case elib_pg:query(Conn, <<"SELECT fn_v5_lock_ordinary_upload($1)">>, [ObjectKey]) of
        {ok, _} -> pending_find_locked_tx(Conn, ObjectKey);
        {error, Reason} -> {error, Reason}
    end.

pending_find_locked_tx(Conn, ObjectKey) ->
    Sql =
        <<
            "SELECT object_key, bucket, scope, creator_user_id, created_at,"
            " client_upload_id, upload_content, upload_registration, session_state, storage_driver,"
            " storage_namespace, storage_container, storage_locator_sha256"
            " FROM attach_pending WHERE object_key = $1 FOR UPDATE"
        >>,
    case elib_pg:query(Conn, Sql, [ObjectKey]) of
        {ok, [Row | _]} -> {ok, decode_pending(Row)};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% Recovery uses the same I/path/session/metadata lock order as confirm/intent.
%% 恢复与确认/删除共用锁序，保留真实 SQL 错误，不把未知结果伪装为不存在。
-spec recovery_session_tx(any(), binary()) -> {ok, map()} | {error, term()}.
recovery_session_tx(Conn, Key) ->
    case elib_pg:query(Conn, <<"SELECT fn_v5_lock_ordinary_upload($1)">>, [Key]) of
        {ok, _} ->
            Sql = <<
                "SELECT p.object_key,p.bucket,p.scope,p.client_upload_id,p.upload_registration,"
                " p.session_state,p.confirmed_attachment_id,p.storage_driver,p.storage_namespace,"
                " p.storage_container,p.storage_locator_sha256,p.upload_content,"
                " extract(epoch from p.session_deadline)::bigint AS deadline,"
                " extract(epoch from p.upload_expires_at)::bigint AS expires_at,"
                " a.id AS asset_id, a.scope AS asset_scope, a.scope_ref AS asset_scope_ref,"
                " a.size AS asset_size, a.mime_type AS asset_mime_type,"
                " a.lifecycle_state AS asset_lifecycle_state,"
                " (EXISTS (SELECT 1 FROM attachment_object_deletion d WHERE d.path=p.object_key)"
                " OR EXISTS (SELECT 1 FROM enterprise_asset_delete_queue q"
                " WHERE q.client_upload_id=p.client_upload_id OR (q.storage_driver=p.storage_driver"
                " AND q.storage_namespace=p.storage_namespace AND q.storage_container=p.storage_container"
                " AND q.object_key=p.object_key))) AS deletion_pending"
                " FROM attach_pending p LEFT JOIN attachment a ON a.path=p.object_key"
                " WHERE p.object_key=$1"
            >>,
            case elib_pg:query(Conn, Sql, [Key]) of
                {ok, [Row]} -> {ok, decode_pending(Row)};
                {ok, []} -> {error, not_found};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
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
    {ok, pos_integer(), First :: boolean()} | {error, term()}.
confirm_save_tx(Conn, ObjectKey, RealType, RealSize, InfoMap) ->
    AttId = next_attachment_id(),
    Ext = filename:extension(ObjectKey),
    Sql =
        <<
            "INSERT INTO attachment (id, file_hash256, mime_type, ext, name, path, url,"
            " size, info, referer_time, last_referer_user_id, last_referer_at,"
            " creator_user_id, scope, scope_ref, cipher, status, created_at, updated_at,"
            " storage_driver, storage_namespace, storage_container, storage_locator_sha256)"
            " VALUES ($1,$2,$3,$4,$5,$6,$6,$7,$8::jsonb,1,0,NOW(),0,'enterprise',$9,"
            " null,1,NOW(),NOW(),$10,$11,$12,$13)"
            " ON CONFLICT (path) DO NOTHING RETURNING id"
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
        maps:get(scope_ref, InfoMap, <<>>),
        maps:get(storage_driver, InfoMap, null),
        maps:get(storage_namespace, InfoMap, null),
        maps:get(storage_container, InfoMap, null),
        maps:get(storage_locator_sha256, InfoMap, null)
    ],
    case elib_pg:query(Conn, Sql, Params) of
        {ok, [#{<<"id">> := Id} | _]} ->
            {ok, Id, true};
        {ok, []} ->
            %% W2-02 confirm 幂等：重放复用既有行（不递增 referer），返回原 id
            case find_by_object_key_tx(Conn, ObjectKey) of
                {ok, #{<<"id">> := Id2}} -> {ok, Id2, false};
                _ -> {error, confirm_row_vanished}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 校验 object_key 已被本 (org, app) confirm（status=1 且 scope='enterprise'
%% 且 scope_ref 匹配）——消息引用附件的前置（confirm 后的 file 才能引用）。
-spec find_confirmed_tx(any(), integer(), integer(), binary()) ->
    {ok, map()} | {error, not_found}.
find_confirmed_tx(Conn, OrgId, AppId, ObjectKey) ->
    ScopeRef = scope_ref(OrgId, AppId),
    Sql =
        <<
            "SELECT id, path, name, mime_type, size, scope, scope_ref, info,"
            " lifecycle_state"
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
    elib_tsid:generate(attachment).

safe_basename(FileName) ->
    case unicode:characters_to_binary(filename:basename(FileName)) of
        B when is_binary(B), byte_size(B) > 0 -> B;
        _ -> <<"file">>
    end.

ymd() ->
    {{Y, M, D}, _} = calendar:universal_time(),
    case unicode:characters_to_binary(io_lib:format("~4..0B~2..0B~2..0B", [Y, M, D])) of
        B when is_binary(B) -> B;
        {incomplete, B, _} -> B;
        _ -> <<>>
    end.

%% PostgreSQL JSON codecs may return either decoded maps or JSON bytes.
decode_pending(Row) ->
    lists:foldl(
        fun(K, Acc) ->
            case maps:get(K, Acc, null) of
                B when is_binary(B) -> Acc#{K => jsone:decode(B)};
                _ -> Acc
            end
        end,
        Row,
        [<<"upload_content">>, <<"upload_registration">>]
    ).
