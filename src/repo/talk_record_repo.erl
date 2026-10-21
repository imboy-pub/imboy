-module(talk_record_repo).
-moduledoc "面聊记录主表仓储（T05：创建/查询/编辑核心 CRUD；迁移 00000211）。".
%%%
% talk_record 主表数据仓库层。
%
% 访问谓词（设计 §1/§8，CONTRACT-FREEZE-T02 §2）：
%   * 个人记录（scope=personal）默认仅 owner 可见（显式 grant 除外）；
%   * 企业记录（scope=organization）读写都要求**当前有效** organization
%     成员资格；绑定 workspace 的记录再叠加 workspace 资源授权——复用
%     organization_resource_access_repo:workspace_predicate_sql 的统一
%     谓词（workspace_member active 或 org owner/admin），不重写边界；
%   * 单凭传入 org_id/workspace_id 不授权，列表/详情一律走同一条
%     visible 谓词，不扫描全企业记录。
%
% 参数占位约定：所有含访问谓词的 SQL 中 **$1 恒为 actor uid**
% （workspace_predicate_sql 内嵌 $1 占位所致），其余参数从 $2 起递增。
%%%

-export([tablename/0]).
-export([detail_columns/0, visible_access_sql/1]).
-export([insert_tx/2]).
-export([find_by_id/2, find_by_id_tx/3]).
-export([find_by_creator_client_tx/3, find_recovery_tx/3]).
-export([find_visible/2, find_visible_tx/3]).
-export([page_visible/4]).
-export([update_meta_tx/4]).
-export([find_active_grant/2]).
%% T15 内容域（原文/发言人/摘要编辑）
-export([insert_audit_tx/6, insert_audit/5, bump_content_revision_tx/2]).
%% T17 授权域（grants PUT/DELETE 事务：行锁 + acl_revision CAS 推进）
-export([lock_for_acl_tx/3, bump_acl_revision_tx/2]).
%% T19 责任移交域（§9：行锁 + owner 事务更新 + 原 owner grant 决定 +
%% custody 投影判定）
-export([
    lock_for_transfer_tx/3,
    transfer_owner_tx/3,
    ensure_transfer_grant_tx/4,
    revoke_grantee_grants_tx/3,
    owner_org_access_ok/1,
    project_custody/1
]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

-define(LIST_COLUMNS,
    <<"tr.id,tr.title,tr.scope,tr.organization_id,tr.workspace_id,",
        "tr.capture_status,tr.media_status,tr.transcript_status,tr.summary_status,",
        "tr.processing_mode,tr.duration_ms,tr.revision,tr.owner_user_id,",
        "tr.created_at,tr.updated_at">>
).
-define(DETAIL_COLUMNS,
    <<"tr.id,tr.title,tr.scope,tr.organization_id,tr.workspace_id,tr.language,",
        "tr.notes,tr.capture_status,tr.media_status,tr.transcript_status,",
        "tr.summary_status,tr.processing_mode,tr.duration_ms,tr.content_revision,",
        "tr.revision,tr.acl_revision,tr.custody_status,tr.owner_user_id,tr.created_by,tr.client_recording_id,",
        "tr.created_at,tr.updated_at">>
).

%% ===================================================================
%% API
%% ===================================================================

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"talk_record">>).

%% @doc 详情列（DS 快照/DTO 与 repo 查询共用同一份列清单，防漂移）。
-spec detail_columns() -> binary().
detail_columns() ->
    ?DETAIL_COLUMNS.

%% @doc 事务内插入记录（id 由 DS 层生成）。唯一键冲突（created_by+
%% client_recording_id 或 PK）收敛为 {error, unique_violation}，由上层
%% 走业务键重放路径，不向客户端泄漏驱动错误形态。
-spec insert_tx(any(), map()) -> {ok, integer()} | {error, unique_violation | term()}.
insert_tx(Conn, Data) ->
    Id = maps:get(<<"id">>, Data),
    {Sql0, Params} = elib_pg_sql:insert(tablename(), Data),
    %% ON CONFLICT DO NOTHING：业务键 (created_by, client_recording_id) 撞库
    %% 不 abort 事务（§11.1 离线补传重放），后续查询仍可用；0 行即撞库。
    Sql = <<Sql0/binary, " ON CONFLICT (created_by, client_recording_id) DO NOTHING">>,
    case elib_pg:execute(Conn, Sql, Params) of
        {ok, 1} ->
            {ok, Id};
        {ok, 0} ->
            {error, unique_violation};
        {error, Reason} ->
            case sqlstate(Reason) of
                <<"23505">> -> {error, unique_violation};
                _ -> {error, Reason}
            end
    end.

%% @doc 按主键取行（不过 ACL；仅限已通过可见性判定的内部复用）。
%% elib_pg:one 空结果回 #{}——此处统一收敛为 not_found。
-spec find_by_id(integer(), binary()) -> {ok, map()} | {error, not_found | term()}.
find_by_id(Id, Columns) ->
    Sql = <<"SELECT ", Columns/binary, " FROM talk_record tr WHERE tr.id = $1">>,
    case elib_pg:one(Sql, [Id]) of
        {ok, Row} when map_size(Row) > 0 -> {ok, Row};
        {ok, _} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 事务内按主键取行（create 回读未提交行必须走事务自身连接，
%% 不能另取池连接——READ COMMITTED 下未提交行不可见）。
-spec find_by_id_tx(any(), integer(), binary()) -> {ok, map()} | {error, not_found | term()}.
find_by_id_tx(Conn, Id, Columns) ->
    Sql = <<"SELECT ", Columns/binary, " FROM talk_record tr WHERE tr.id = $1">>,
    case elib_pg:query(Conn, Sql, [Id]) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 离线重放业务键查找（§11：created_by + client_recording_id 去重）。
%% _tx 版本在事务内走事务连接（未提交行可见）。
-spec find_by_creator_client_tx(any(), integer(), binary()) ->
    {ok, map()} | {error, not_found | term()}.
find_by_creator_client_tx(Conn, CreatedBy, ClientRecordingId) ->
    Sql =
        <<"SELECT ", ?DETAIL_COLUMNS/binary, " FROM talk_record tr",
            " WHERE tr.created_by = $1 AND tr.client_recording_id = $2">>,
    case elib_pg:query(Conn, Sql, [CreatedBy, ClientRecordingId]) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 详情可见行（ACL 单条）：无权/不存在统一 not_found（§4/§11.3）。
-spec find_visible(integer(), integer()) -> {ok, map()} | {error, not_found | term()}.
find_visible(Uid, Id) ->
    Sql =
        <<"SELECT ", ?DETAIL_COLUMNS/binary, " FROM talk_record tr WHERE tr.id = $2 AND (",
            (visible_access_sql(all))/binary, ")">>,
    case elib_pg:one(Sql, [Uid, Id]) of
        {ok, Row} when map_size(Row) > 0 -> {ok, Row};
        {ok, _} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% 同事务当前ACL检查；模型结果发布不能绕过已撤销的处理主体权限。
find_visible_tx(Conn, Uid, Id) ->
    Sql =
        <<"SELECT ", ?DETAIL_COLUMNS/binary, " FROM talk_record tr WHERE tr.id=$2 AND (",
            (visible_access_sql(all))/binary, ")">>,
    case elib_pg:query(Conn, Sql, [Uid, Id]) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        E -> E
    end.

%% @doc 列表页（keyset 游标，created_at DESC, id DESC 稳定倒序）。
%% Filter：#{scope, organization_id, workspace_id, filter, q}（已由 logic
%% 归一化：filter ∈ all|owned|shared；q 已限长）；Cursor 为 undefined 或
%% {LastCreatedAt, LastId}。
-spec page_visible(integer(), map(), {binary(), integer()} | undefined, pos_integer()) ->
    {ok, [map()]} | {error, term()}.
page_visible(Uid, Filter, Cursor, Limit) ->
    Next = fun(N) -> <<"$", (integer_to_binary(N))/binary>> end,
    Params1 = [Uid],
    {ScopeFrag, Params2} = scope_frag(Filter, 2, Next, Params1),
    {QFrag, Params3} = q_frag(maps:get(q, Filter, <<>>), length(Params2) + 1, Next, Params2),
    {CursorFrag, Params4} = cursor_frag(Cursor, length(Params3) + 1, Next, Params3),
    LimitPh = Next(length(Params4) + 1),
    Sql =
        <<"SELECT ", ?LIST_COLUMNS/binary, " FROM talk_record tr WHERE (",
            (visible_access_sql(maps:get(filter, Filter, all)))/binary, ")", ScopeFrag/binary,
            QFrag/binary, CursorFrag/binary, " ORDER BY tr.created_at DESC, tr.id DESC LIMIT ",
            LimitPh/binary>>,
    elib_pg:query(Sql, Params4 ++ [Limit]).

%% @doc 元信息 CAS 更新（title/language/notes；revision+1；§11.2 revision
%% 域）。命中 0 行时由上层区分 not_found / precondition_failed。
-spec update_meta_tx(any(), integer(), pos_integer(), map()) ->
    {ok, map()} | {error, not_found | term()}.
update_meta_tx(Conn, Id, ExpectedRevision, Sets) ->
    Ordered = [K || K <- [<<"title">>, <<"language">>, <<"notes">>], maps:is_key(K, Sets)],
    Sets1 = [{K, maps:get(K, Sets)} || K <- Ordered] ++ [{<<"updated_at">>, elib_dt:now()}],
    SetSql = join_set_clause(Sets1, 1),
    Base = length(Sets1) + 1,
    Sql =
        <<"UPDATE talk_record tr SET ", SetSql/binary, ", revision = revision + 1",
            " WHERE tr.id = $", (integer_to_binary(Base))/binary,
            " AND tr.deleted_at IS NULL AND tr.revision = $", (integer_to_binary(Base + 1))/binary,
            " RETURNING ", ?DETAIL_COLUMNS/binary>>,
    Params = [V || {_, V} <- Sets1] ++ [Id, ExpectedRevision],
    case elib_pg:query(Conn, Sql, Params) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 活跃授权行（viewer/editor；撤销/过期不算，§2 grant 表）。
-spec find_active_grant(integer(), integer()) -> {ok, map()} | {error, not_found | term()}.
find_active_grant(RecordId, Uid) ->
    Sql =
        <<"SELECT id, role, allow_export FROM talk_record_grant",
            " WHERE record_id = $1 AND grantee_user_id = $2 AND revoked_at IS NULL",
            " AND (expires_at IS NULL OR expires_at > now()) LIMIT 1">>,
    case elib_pg:one(Sql, [RecordId, Uid]) of
        {ok, Row} when map_size(Row) > 0 -> {ok, Row};
        {ok, _} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% T15 内容域：审计 / content_revision（原文·发言人编辑路径共用）
%% ===================================================================

%% @doc 审计行写入（事务内；§2：记录授权/编辑/导出/删除/识别纠正，
%% 不写音频、原文、向量或外部授权 URL）。Metadata 为白名单键的 map
%% （revision/content_revision/fields/outcome/matched_user_id 等），正文
%% 一律不得进入；id 由调用方生成（elib_tsid）。
%% request_id 本卡无传递渠道，恒 NULL（接入网关 request_id 留 T22）。
-spec insert_audit_tx(
    any(),
    integer() | null,
    integer() | null,
    binary(),
    integer() | null,
    map() | null
) -> {ok, integer()} | {error, term()}.
insert_audit_tx(Conn, RecordId, ActorId, Action, TargetId, Metadata) ->
    Sql =
        <<"INSERT INTO talk_record_audit",
            "(id, record_id, actor_id, action, target_id, request_id, metadata_allowlist)",
            " VALUES ($1, $2, $3, $4, $5, NULL, $6::jsonb)">>,
    Id = elib_tsid:generate(talk_record),
    Json =
        case Metadata of
            Map when is_map(Map) -> jsone:encode(Metadata);
            _ -> <<"null">>
        end,
    case elib_pg:execute(Conn, Sql, [Id, RecordId, ActorId, Action, TargetId, Json]) of
        {ok, 1} -> {ok, Id};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 独立事务审计写入（无事务上下文的路径——如 claim 冲突 409 也要
%% 留痕：冲突判定回滚业务事务，审计必须单独提交才不丢失）。
-spec insert_audit(
    integer() | null,
    integer() | null,
    binary(),
    integer() | null,
    map() | null
) -> {ok, integer()} | {error, term()}.
insert_audit(RecordId, ActorId, Action, TargetId, Metadata) ->
    Fun = fun(Conn) -> insert_audit_tx(Conn, RecordId, ActorId, Action, TargetId, Metadata) end,
    case elib_pg:with_tx(Fun, [{reraise, false}]) of
        {ok, AuditId} -> {ok, AuditId};
        {rollback, Reason} -> {error, Reason};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 内容版本推进（§11.2：原文/发言人署名等内容变化时 +1；不碰
%% record.revision / acl_revision）。软删记录命中 0 行 → not_found。
-spec bump_content_revision_tx(any(), integer()) ->
    {ok, pos_integer()} | {error, not_found | term()}.
bump_content_revision_tx(Conn, RecordId) ->
    Sql =
        <<"UPDATE talk_record SET content_revision = content_revision + 1,",
            " updated_at = $1 WHERE id = $2 AND deleted_at IS NULL",
            " RETURNING content_revision">>,
    case elib_pg:query(Conn, Sql, [elib_dt:now(), RecordId]) of
        {ok, [#{<<"content_revision">> := N}]} -> {ok, N};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% T17 授权域：ACL 行锁 + acl_revision 推进（§11.2 版本域纪律）
%% ===================================================================

%% @doc 授权事务行锁（§4 PUT/DELETE grants：与并发删除互斥）。
%% SELECT ... FOR UPDATE 序列化并发 PUT/DELETE/软删：拿到锁后复核
%% deleted_at（并发删除已置位 → not_found）与 acl_revision（并发授权
%% 已推进 → precondition_failed）。ExpectedAcl 为 If-Match 解析值。
%% 复核通过时行仍被持有，随后 bump/写入在同一事务内安全。
-spec lock_for_acl_tx(any(), integer(), pos_integer()) ->
    {ok, map()} | {error, not_found | precondition_failed | term()}.
lock_for_acl_tx(Conn, Id, ExpectedAcl) ->
    Sql =
        <<"SELECT id, owner_user_id, scope, organization_id, workspace_id,",
            " acl_revision, deleted_at FROM talk_record tr WHERE tr.id = $1 FOR UPDATE">>,
    case elib_pg:query(Conn, Sql, [Id]) of
        {ok, [#{<<"deleted_at">> := null, <<"acl_revision">> := ExpectedAcl} = Row]} ->
            {ok, Row};
        {ok, [#{<<"deleted_at">> := null}]} ->
            {error, precondition_failed};
        {ok, [_]} ->
            {error, not_found};
        {ok, []} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc ACL 版本推进（§11.2：授权/撤销/失效/责任移交时 +1；不碰
%% record.revision / content_revision——上传进度等不得使分享表单必然
%% 412）。调用方须先 lock_for_acl_tx 持锁。
-spec bump_acl_revision_tx(any(), integer()) ->
    {ok, pos_integer()} | {error, not_found | term()}.
bump_acl_revision_tx(Conn, RecordId) ->
    Sql =
        <<"UPDATE talk_record SET acl_revision = acl_revision + 1,",
            " updated_at = $1 WHERE id = $2 AND deleted_at IS NULL", " RETURNING acl_revision">>,
    case elib_pg:query(Conn, Sql, [elib_dt:now(), RecordId]) of
        {ok, [#{<<"acl_revision">> := N}]} -> {ok, N};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% T19 责任移交域（§9：owner 事务更新 / 原 owner grant 决定 / custody 投影）
%% ===================================================================

%% @doc 移交事务行锁（§4 移交必须 If-Match，record.revision 域——T21
%% `record:<revision>` 惯例）。SELECT ... FOR UPDATE 序列化并发移交/删除/
%% 编辑：拿锁后复核 deleted_at（并发删除 → not_found）与 revision
%% （并发编辑 → precondition_failed）。返回行含移交所需归属字段。
-spec lock_for_transfer_tx(any(), integer(), pos_integer()) ->
    {ok, map()} | {error, not_found | precondition_failed | term()}.
lock_for_transfer_tx(Conn, Id, ExpectedRevision) ->
    Sql =
        <<"SELECT id, owner_user_id, created_by, scope, organization_id,",
            " workspace_id, revision, acl_revision, custody_status, deleted_at",
            " FROM talk_record tr WHERE tr.id = $1 FOR UPDATE">>,
    case elib_pg:query(Conn, Sql, [Id]) of
        {ok, [#{<<"deleted_at">> := null, <<"revision">> := ExpectedRevision} = Row]} ->
            {ok, Row};
        {ok, [#{<<"deleted_at">> := null}]} ->
            {error, precondition_failed};
        {ok, [_]} ->
            {error, not_found};
        {ok, []} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc owner 事务更新（§9：owner_user_id 为当前管理责任人；created_by/
%% scope 不动）。新 owner 到位即管理责任恢复——custody_status 一并回
%% active（handover_required 仅在无有效 owner 时成立）。调用方须先
%% lock_for_transfer_tx 持锁；acl_revision 推进由 bump_acl_revision_tx
%% 在同事务完成（§11.2 责任移交时推进）。
-spec transfer_owner_tx(any(), integer(), integer()) ->
    {ok, map()} | {error, not_found | term()}.
transfer_owner_tx(Conn, Id, TargetUid) ->
    Sql =
        <<"UPDATE talk_record SET owner_user_id = $2, custody_status = 'active',",
            " updated_at = $3 WHERE id = $1 AND deleted_at IS NULL",
            " RETURNING id, owner_user_id, created_by, scope, organization_id,",
            " workspace_id, revision, content_revision, acl_revision, custody_status">>,
    case elib_pg:query(Conn, Sql, [Id, TargetUid, elib_dt:now()]) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc keep_access=true：原 owner 补 viewer grant（无**有效**活跃行时
%% 插入；已有有效活跃行保持原样——不降级既有 editor，也不重复建行）。
%% P4（Q-review）：有效 = revoked_at IS NULL **且未过期**——仅 revoked
%% 判据会把"活跃但已过期"的行误当有效（原 owner 实际已无访问）；过期
%% 行仍占部分唯一索引，插入走 ON CONFLICT DO UPDATE 重置 expires_at
%% （对齐 T17 grant_repo upsert_one_tx 惯例；revoked 历史行不受影响，
%% 部分唯一索引只看 revoked_at IS NULL）。
%% grant 写入本应属 talk_record_grant_repo（T17），该文件不在 T19 写
%% 权限清单，按最小续写放本文件，归并时可平移。
-spec ensure_transfer_grant_tx(any(), integer(), integer(), integer()) ->
    ok | {error, term()}.
ensure_transfer_grant_tx(Conn, RecordId, GranteeUid, GrantorUid) ->
    Exists =
        <<"SELECT 1 FROM talk_record_grant g WHERE g.record_id = $1",
            " AND g.grantee_user_id = $2 AND g.revoked_at IS NULL",
            " AND (g.expires_at IS NULL OR g.expires_at > now())">>,
    case elib_pg:query(Conn, Exists, [RecordId, GranteeUid]) of
        {ok, [_ | _]} ->
            ok;
        {ok, []} ->
            Insert =
                <<"INSERT INTO talk_record_grant",
                    "(id, record_id, grantee_user_id, role, allow_export, granted_by, expires_at,",
                    " created_at, updated_at) VALUES ($1, $2, $3, 'viewer', false, $4, NULL, $5, $5)",
                    " ON CONFLICT (record_id, grantee_user_id) WHERE revoked_at IS NULL",
                    " DO UPDATE SET role = 'viewer', allow_export = false, granted_by = $4,",
                    " expires_at = NULL, updated_at = $5">>,
            case
                elib_pg:execute(
                    Conn,
                    Insert,
                    [
                        elib_tsid:generate(talk_record),
                        RecordId,
                        GranteeUid,
                        GrantorUid,
                        elib_dt:now()
                    ]
                )
            of
                {ok, _} -> ok;
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc keep_access=false：确认无遗留 grant——撤销原 owner 的全部活跃
%% 授权行（§9 原 owner 不再有权访问；owner 在主表的访问随 owner_user_id
%% 更新即失，残留 grant 行会使其仍可读，必须清掉）。返回撤销行数。
-spec revoke_grantee_grants_tx(any(), integer(), integer()) ->
    {ok, non_neg_integer()} | {error, term()}.
revoke_grantee_grants_tx(Conn, RecordId, GranteeUid) ->
    Sql =
        <<"UPDATE talk_record_grant SET revoked_at = now(), updated_at = now()",
            " WHERE record_id = $1 AND grantee_user_id = $2 AND revoked_at IS NULL">>,
    %% execute（非 query）：计数形态（query 对无 RETURNING UPDATE 收敛 {ok, []}）
    case elib_pg:execute(Conn, Sql, [RecordId, GranteeUid]) of
        {ok, N} when is_integer(N) -> {ok, N};
        {ok, _} -> {ok, 0};
        {error, Reason} -> {error, Reason}
    end.

%% @doc owner 归属访问判定（§9 custody 投影）：企业记录的当前 owner 是否
%% 仍满足归属访问（org active + owner 成员 active + 绑定 ws 时 ws 资源
%% 授权）。personal 恒 true（无企业归属语义）。独立连接查询（读路径投影，
%% 无事务上下文）。
-spec owner_org_access_ok(map()) -> boolean().
owner_org_access_ok(
    #{
        <<"scope">> := <<"organization">>,
        <<"organization_id">> := OrgId,
        <<"owner_user_id">> := OwnerUid
    } = Row
) when
    is_integer(OrgId), is_integer(OwnerUid)
->
    WsPred = organization_resource_access_repo:workspace_predicate_sql(
        <<"tr.workspace_id">>, <<"$1">>
    ),
    Sql =
        <<"SELECT 1 WHERE EXISTS (SELECT 1 FROM organization o",
            " WHERE o.id = $2 AND o.status = 'active')",
            " AND EXISTS (SELECT 1 FROM organization_member om",
            " WHERE om.organization_id = $2 AND om.user_id = $1", " AND om.status = 'active')",
            " AND NOT EXISTS (SELECT 1 FROM talk_record tr WHERE tr.id = $3",
            " AND tr.workspace_id IS NOT NULL AND NOT (", WsPred/binary, "))">>,
    case elib_pg:one(Sql, [OwnerUid, OrgId, maps:get(<<"id">>, Row)]) of
        {ok, Row1} when map_size(Row1) > 0 -> true;
        {ok, _} ->
            false;
        {error, Reason} ->
            ?WARN_LOG("talk_record owner_org_access_ok failed ~p", [Reason]),
            %% 判定查询失败不放大为 500：保守按仍有权（false 会把活记录
            %% 误投影为待移交——展示字段，宁可漏报不可误报）
            true
    end;
owner_org_access_ok(_) ->
    true.

%% @doc custody_status 读路径投影（§9：owner 失去归属访问时记录进入
%% handover_required——本卡仅判定与展示字段就绪，metadata-only recovery
%% 属后续企业治理，不实现）。存储值为 handover_required 时原样展示
%% （治理侧写入的事实）；存储 active 且企业记录 owner 已失归属访问 →
%% 投影为 handover_required（不写库，每次读取实时判定）。
-spec project_custody(map()) -> map().
project_custody(#{<<"scope">> := <<"organization">>, <<"custody_status">> := <<"active">>} = Row) ->
    case owner_org_access_ok(Row) of
        true -> Row;
        false -> Row#{<<"custody_status">> => <<"handover_required">>}
    end;
project_custody(Row) ->
    Row.

%% ===================================================================
%% Internal：访问谓词（$1 恒为 uid）
%% ===================================================================

%% @private owner / 显式 grant 两支 + 企业记录归属有效性。
%% ViewFilter 接受 atom（all/owned/shared）与等值 binary（经 parse_filters
%% 归一化后的 <<"all">>/<<"owned">>/<<"shared">>）。
-spec visible_access_sql(all | owned | shared | binary()) -> binary().
visible_access_sql(ViewFilter) ->
    Grant = grant_exists_sql(),
    Ownership =
        case norm_filter(ViewFilter) of
            owned -> <<"tr.owner_user_id = $1">>;
            shared -> <<"(tr.owner_user_id <> $1 AND ", Grant/binary, ")">>;
            all -> <<"(tr.owner_user_id = $1 OR ", Grant/binary, ")">>
        end,
    <<Ownership/binary, " AND tr.deleted_at IS NULL", " AND (tr.scope = 'personal' OR (",
        (org_lifecycle_sql())/binary, "))">>.

%% @private 企业记录：organization active + 当前有效成员 + （绑定 ws 时）
%% workspace 资源授权（organization_resource_access_repo 统一谓词）。
-spec org_lifecycle_sql() -> binary().
org_lifecycle_sql() ->
    WsPred = organization_resource_access_repo:workspace_predicate_sql(
        <<"tr.workspace_id">>, <<"$1">>
    ),
    <<"EXISTS (SELECT 1 FROM organization o WHERE o.id = tr.organization_id",
        " AND o.status = 'active')", " AND EXISTS (SELECT 1 FROM organization_member om",
        " WHERE om.organization_id = tr.organization_id AND om.user_id = $1",
        " AND om.status = 'active')", " AND (tr.workspace_id IS NULL OR ", WsPred/binary, ")">>.

-spec grant_exists_sql() -> binary().
grant_exists_sql() ->
    <<"EXISTS (SELECT 1 FROM talk_record_grant g WHERE g.record_id = tr.id",
        " AND g.grantee_user_id = $1 AND g.revoked_at IS NULL",
        " AND (g.expires_at IS NULL OR g.expires_at > now()))">>.

-spec norm_filter(all | owned | shared | binary()) -> all | owned | shared.
norm_filter(F) when F =:= owned; F =:= <<"owned">> -> owned;
norm_filter(F) when F =:= shared; F =:= <<"shared">> -> shared;
norm_filter(F) when F =:= all; F =:= <<"all">>; F =:= <<>> -> all.

%% ===================================================================
%% Internal：列表过滤片段（参数占位递增拼装）
%% ===================================================================

-spec scope_frag(map(), pos_integer(), fun(), list()) -> {binary(), list()}.
scope_frag(Filter, N, Next, Params) ->
    OrgId = maps:get(organization_id, Filter, 0),
    WsId = maps:get(workspace_id, Filter, 0),
    case maps:get(scope, Filter, <<>>) of
        <<>> ->
            org_ws_frag(OrgId, WsId, N, Next, Params, <<>>);
        Scope when Scope =:= <<"personal">>; Scope =:= <<"organization">> ->
            Base = <<" AND tr.scope = '", Scope/binary, "'">>,
            org_ws_frag(OrgId, WsId, N, Next, Params, Base);
        _ ->
            {<<>>, Params}
    end.

-spec org_ws_frag(integer(), integer(), pos_integer(), fun(), list(), binary()) ->
    {binary(), list()}.
org_ws_frag(0, 0, _N, _Next, Params, Base) ->
    {Base, Params};
org_ws_frag(0, WsId, N, Next, Params, Base) when WsId =/= 0 ->
    {<<Base/binary, " AND tr.workspace_id = ", (Next(N))/binary>>, Params ++ [WsId]};
org_ws_frag(OrgId, 0, N, Next, Params, Base) when OrgId =/= 0 ->
    {<<Base/binary, " AND tr.organization_id = ", (Next(N))/binary>>, Params ++ [OrgId]};
org_ws_frag(OrgId, WsId, N, Next, Params, Base) ->
    OrgPh = Next(N),
    WsPh = Next(N + 1),
    {
        <<Base/binary, " AND tr.organization_id = ", OrgPh/binary, " AND tr.workspace_id = ",
            WsPh/binary>>,
        Params ++ [OrgId, WsId]
    }.

-spec q_frag(binary(), pos_integer(), fun(), list()) -> {binary(), list()}.
q_frag(<<>>, _N, _Next, Params) ->
    {<<>>, Params};
q_frag(Q, N, Next, Params) ->
    Pattern = <<"%", (elib_pg:escape_like(Q))/binary, "%">>,
    {<<" AND (tr.title ILIKE ", (Next(N))/binary, ")">>, Params ++ [Pattern]}.

-spec cursor_frag({binary(), integer()} | undefined, pos_integer(), fun(), list()) ->
    {binary(), list()}.
cursor_frag(undefined, _N, _Next, Params) ->
    {<<>>, Params};
cursor_frag({LastCreatedAt, LastId}, N, Next, Params) ->
    C1 = Next(N),
    C2 = Next(N + 1),
    {
        <<" AND (tr.created_at, tr.id) < (", C1/binary, ", ", C2/binary, ")">>,
        Params ++ [LastCreatedAt, LastId]
    }.

-spec join_set_clause([{binary(), term()}], pos_integer()) -> binary().
join_set_clause(Sets, Start) ->
    {Parts, _N} =
        lists:mapfoldl(
            fun({Col, _V}, N) ->
                {<<Col/binary, " = $", (integer_to_binary(N))/binary>>, N + 1}
            end,
            Start,
            Sets
        ),
    list_to_binary(join_binary(Parts, <<", ">>)).

join_binary([], _Sep) ->
    [];
join_binary([P], _Sep) ->
    [P];
join_binary([P | Rest], Sep) ->
    [P, Sep | join_binary(Rest, Sep)].

%% @private epgsql 错误形态 → SQLSTATE（#error{} record 元组的第 3 位；
%% tuple_size 随 epgsql 版本可有增减，按位取且校验 5 位十六进制形态）。
-spec sqlstate(term()) -> binary() | undefined.
sqlstate({error, Inner}) when
    is_tuple(Inner), tuple_size(Inner) >= 3, element(1, Inner) =:= error
->
    case element(3, Inner) of
        Code when is_binary(Code), byte_size(Code) =:= 5 -> Code;
        _ -> undefined
    end;
sqlstate(_) ->
    undefined.

%% 主体只来自JWT；归属必须与本机不可变绑定完全一致。
find_recovery_tx(C, Uid, V) ->
    Sql =
        <<"SELECT ", ?DETAIL_COLUMNS/binary,
            " FROM talk_record tr"
            " WHERE tr.created_by=$1 AND tr.owner_user_id=$1"
            " AND tr.client_recording_id=$2 AND tr.scope=$3"
            " AND tr.organization_id IS NOT DISTINCT FROM $4"
            " AND tr.workspace_id IS NOT DISTINCT FROM $5"
            " AND tr.deleted_at IS NULL AND (", (visible_access_sql(all))/binary, ")">>,
    Org = recovery_id(maps:get(organization_id, V, 0)),
    Ws = recovery_id(maps:get(workspace_id, V, 0)),
    case
        elib_pg:query(C, Sql, [
            Uid,
            maps:get(client_recording_id, V),
            maps:get(scope, V),
            Org,
            Ws
        ])
    of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

recovery_id(0) -> null;
recovery_id(Id) -> Id.
