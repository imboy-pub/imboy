-module(organization_admin_logic).

%% Platform Admin Organization 治理通道（TASK_ID=ORG-ADM-ORG-API）。
%%
%% 定位（用户 2026-09-18 拍板 ORG-14 选 A + 平台 logic 层方案）：
%%   organization application 层全部 command（含读列表）的 actor 强校验
%%   「该 Org 的 active 租户 owner/admin」；Platform Admin 的身份是
%%   adm_user.id，不在租户 organization_member 表内——直接调 app 层恒 403。
%%   本模块即平台侧专用通道，镜像 workspace_logic:admin_* 既有平台治理惯例：
%%   * 不做租户角色校验（平台鉴权由 adm_acl 在 handler 层完成）；
%%   * 平台操作者是 adm_user.id，绝不写任何指向 user 表的 actor 列
%%     （updated_by_user_id/invited_by 固定 NULL，操作者审计由 handler 层
%%     adm_operation_log_ds 承担——镜像 workspace_logic:admin_archive_tx
%%     写 archived_by=NULL 的 FK 先例）；
%%   * 不映射 Platform Admin 为租户身份、不签发/代理/冒充租户 session；
%%   * 业务规则最大化复用 src/lib/organization 的 domain 校验器与
%%     infrastructure 原语（lifecycle_pg / owner_store / owner_invariant /
%%     member_repo / invitation_pg / department_pg / department domain），
%%     状态迁移语义（幂等、锁序「组织行先、成员行后」、archived 门禁、
%%     owner 保护）与 app 层同构；仅成员精确单列迁移语句为镜像实现
%%     （organization_member_logic 的同名 *_tx 为私有，见各函数注释）。
%% 错误码稳定口径（与 app 层一致）：400 形状 / 404 不存在 / 409 archived、
%% stale、非成员、owner 保护 / 500 兜底；不泄露内部 SQL/PII。

-export([
    %% 读（只读关系事实；不做 archived 门禁——平台治理需要看到归档组织全貌）
    admin_page/4,
    admin_detail/1,
    admin_member_page/3,
    admin_invitation_list/3,
    admin_department_list/2,
    admin_workspace_page/3,
    %% 写（全部走组织行锁 + 与 app 层同构的状态机裁决；治理写均带 AuditCtx，
    %% 平台审计在业务事务内经 adm_operation_log_ds:insert_tx 写入——CP-ASSET-05）
    admin_create/5,
    admin_create_pending_owner/5,
    admin_create_pending_owner/6,
    admin_archive/3,
    admin_restore/3,
    admin_transfer_owner/4,
    admin_member_suspend/4,
    admin_member_restore/4,
    admin_member_remove/4,
    admin_invitation_create/5,
    admin_invitation_cancel/4,
    admin_invite_code_get/1,
    admin_invite_code_create/5,
    admin_invite_code_revoke/3,
    admin_review/4,
    admin_department_create/4,
    admin_department_rename/5,
    admin_department_move/5,
    admin_department_archive/4
]).

-include("log.hrl").

%% 平台建码换码重试上限（镜像 invite_code_app 的 ?CODE_RETRY_LIMIT）。
-define(INVITE_CODE_RETRY_LIMIT, 3).

%% ===================================================================
%% 读：组织分页 + 搜索（status=all|active|archived；keyword 按组织名/ID）
%% ===================================================================

-spec admin_page(integer(), integer(), binary() | all, binary()) ->
    {ok, map()} | {error, {500, binary()}}.
admin_page(Page0, Size0, Status, Keyword) ->
    Page = max(Page0, 1),
    Size = max(1, min(Size0, 100)),
    {WhereSql, Params} = page_where(normalize_status(Status), Keyword),
    CountSql = <<"SELECT COUNT(*) AS count FROM organization o", WhereSql/binary>>,
    Total =
        case elib_pg:one(CountSql, Params) of
            {ok, #{<<"count">> := C}} -> C;
            _ -> 0
        end,
    Offset = (Page - 1) * Size,
    DataSql =
        [
            <<"SELECT o.id, o.name, o.owner_id, o.status, o.created_at, o.updated_at,",
                " u.nickname AS owner_nickname, u.account AS owner_account,",
                " (SELECT count(*) FROM organization_member om",
                "   WHERE om.organization_id = o.id AND om.status = 'active') AS member_count,",
                " (SELECT count(*) FROM workspace w",
                "   WHERE w.organization_id = o.id) AS workspace_count",
                " FROM organization o LEFT JOIN \"user\" u ON u.id = o.owner_id">>,
            WhereSql,
            <<" ORDER BY o.id DESC LIMIT $">>,
            integer_to_binary(length(Params) + 1),
            <<" OFFSET $">>,
            integer_to_binary(length(Params) + 2)
        ],
    case elib_pg:query(DataSql, Params ++ [Size, Offset]) of
        {ok, Items} ->
            TotalPage =
                case Total > 0 of
                    true -> ((Total - 1) div Size) + 1;
                    false -> 0
                end,
            {ok, #{
                list => Items,
                page => Page,
                size => Size,
                total => Total,
                total_page => TotalPage
            }};
        {error, Reason} ->
            _ = ?ERROR_LOG([organization_admin_page_failed, Reason]),
            {error, {500, <<"查询失败，请稍后重试"/utf8>>}}
    end.

page_where(Status, Keyword) ->
    %% ⚠ status 条件自带 $1 占位符 ⇒ 它的**值必须同步进 params**。只写占位符不绑值，
    %% epgsql 会收到「占位符多于参数」的语句并在结果解码阶段以 function_clause 崩成 500
    %% （实测：GET /api/adm/organizations?status=active ⇒ 配置向导拉不到组织列表）。
    %% 故这里 Conds 与 Params 成对构造；`::text` 让枚举/varchar 两种列型都能绑。
    %% 枚举覆盖 00000155 全状态：active|archived|pending|rejected（待审核列表
    %% 即 status=pending 筛选）。
    {Conds0, StatusParams} =
        case Status of
            <<"active">> -> {[<<" o.status::text = $1">>], [<<"active">>]};
            <<"archived">> -> {[<<" o.status::text = $1">>], [<<"archived">>]};
            <<"pending">> -> {[<<" o.status::text = $1">>], [<<"pending">>]};
            <<"rejected">> -> {[<<" o.status::text = $1">>], [<<"rejected">>]};
            _ -> {[], []}
        end,
    KwParams =
        case is_binary(Keyword) andalso byte_size(Keyword) > 0 of
            true ->
                %% keyword 命中组织名或 TSID 前缀（safe_to_integer 失败时按名称）
                case elib_cnv:safe_to_integer(Keyword) of
                    Id when is_integer(Id), Id > 0 ->
                        {<<" (o.name ILIKE $K OR o.id = $ID)">>, Keyword, Id};
                    _ ->
                        {<<" o.name ILIKE $K">>, Keyword, undefined}
                end;
            false ->
                undefined
        end,
    {Conds, Params0} =
        case KwParams of
            undefined ->
                {Conds0, StatusParams};
            {CondTpl, Kw, MaybeId} ->
                N = length(Conds0) + 1,
                Cond1 = binary:replace(CondTpl, <<"$K">>, <<"$", (integer_to_binary(N))/binary>>),
                case MaybeId of
                    undefined ->
                        {Conds0 ++ [Cond1], StatusParams ++ [<<"%", Kw/binary, "%">>]};
                    Id2 ->
                        Cond2 =
                            binary:replace(
                                Cond1,
                                <<"$ID">>,
                                <<"$", (integer_to_binary(N + 1))/binary>>
                            ),
                        {Conds0 ++ [Cond2], StatusParams ++ [<<"%", Kw/binary, "%">>, Id2]}
                end
        end,
    WhereSql =
        case Conds of
            [] -> <<>>;
            _ -> [" WHERE", lists:join(" AND", Conds)]
        end,
    {iolist_to_binary(WhereSql), Params0}.

normalize_status(all) -> all;
normalize_status(Bin) when is_binary(Bin) -> Bin;
normalize_status(_) -> all.

%% ===================================================================
%% 读：组织详情（基本信息 + owner 概要 + 关系计数）
%% ===================================================================

-spec admin_detail(integer()) -> {ok, map()} | {error, {404, binary()}} | {error, {500, binary()}}.
admin_detail(OrgId) ->
    Sql =
        <<"SELECT o.id, o.name, o.owner_id, o.status, o.branding, o.settings,",
            " o.created_at, o.updated_at,",
            " u.nickname AS owner_nickname, u.account AS owner_account, u.avatar AS owner_avatar,",
            " (SELECT count(*) FROM organization_member om",
            "   WHERE om.organization_id = o.id AND om.status = 'active') AS member_count,",
            " (SELECT count(*) FROM workspace w",
            "   WHERE w.organization_id = o.id) AS workspace_count",
            " FROM organization o LEFT JOIN \"user\" u ON u.id = o.owner_id", " WHERE o.id = $1">>,
    %% elib_pg:one/3 空结果集回落 Default——必须显式传 undefined，
    %% 否则空 map 也匹配 is_map 分支、404 永不触发。
    case elib_pg:one(Sql, [OrgId], undefined) of
        {ok, Row} when is_map(Row), map_size(Row) > 0 ->
            {ok, Row};
        {ok, _EmptyOrUndefined} ->
            {error, {404, <<"Organization 不存在"/utf8>>}};
        {error, Reason} ->
            _ = ?ERROR_LOG([organization_admin_detail_failed, OrgId, Reason]),
            {error, {500, <<"查询失败，请稍后重试"/utf8>>}}
    end.

%% ===================================================================
%% 读：成员分页（复用 organization_member_repo 原语，无租户 actor 校验）
%% ===================================================================

-spec admin_member_page(integer(), integer(), integer()) ->
    {ok, map()} | {error, {integer(), binary()}}.
admin_member_page(OrgId, Page, Size) ->
    case org_exists(OrgId) of
        false ->
            {error, {404, <<"Organization 不存在"/utf8>>}};
        true ->
            case
                organization_member_repo:page_by_organization(
                    OrgId,
                    max(Page, 1),
                    max(1, min(Size, 100)),
                    <<"om.organization_id,om.user_id,om.role,om.invited_by,",
                        "om.joined_at,om.status,u.nickname,u.avatar,u.account">>
                )
            of
                {ok, Result} ->
                    {ok, Result};
                {error, Reason} ->
                    _ = ?ERROR_LOG([organization_admin_member_page_failed, OrgId, Reason]),
                    {error, {500, <<"查询失败，请稍后重试"/utf8>>}}
            end
    end.

%% ===================================================================
%% 读：邀请列表（status 可选 pending|accepted|rejected|revoked|expired|all）
%% ===================================================================

-spec admin_invitation_list(integer(), binary() | all, integer()) ->
    {ok, [map()]} | {error, {integer(), binary()}}.
admin_invitation_list(OrgId, Status, Limit) ->
    case org_exists(OrgId) of
        false ->
            {error, {404, <<"Organization 不存在"/utf8>>}};
        true ->
            StatusArg =
                case Status of
                    all -> undefined;
                    S when is_binary(S), byte_size(S) > 0 -> S;
                    _ -> undefined
                end,
            Tx = fun(Conn) ->
                organization_invitation_pg:list_for_org_tx(
                    Conn, OrgId, StatusArg, max(1, min(Limit, 100))
                )
            end,
            case elib_pg:with_tx(Tx) of
                {ok, Rows} ->
                    %% 与租户面 organization_invitation_app:list_for_org 同口径：
                    %% 逐行套响应投影白名单（invitation_id 键名归一 + token_digest
                    %% 永不出站——_pg 裸行含 digest 列，禁止直出）
                    {ok, [invitation_view(Row) || Row <- Rows]};
                {error, Reason} ->
                    _ = ?ERROR_LOG([organization_admin_invitation_list_failed, OrgId, Reason]),
                    {error, {500, <<"查询失败，请稍后重试"/utf8>>}}
            end
    end.

%% ===================================================================
%% 读：部门列表（复用 organization_department_pg 原语；status=all|active|archived）
%% ===================================================================

-spec admin_department_list(integer(), binary() | all) ->
    {ok, [map()]} | {error, {integer(), binary()}}.
admin_department_list(OrgId, Status) ->
    case org_exists(OrgId) of
        false ->
            {error, {404, <<"Organization 不存在"/utf8>>}};
        true ->
            StatusArg =
                case Status of
                    <<"active">> -> active;
                    <<"archived">> -> archived;
                    _ -> all
                end,
            case organization_department_pg:list_departments(OrgId, StatusArg, none) of
                {ok, Rows} ->
                    {ok, Rows};
                {error, Reason} ->
                    _ = ?ERROR_LOG([organization_admin_department_list_failed, OrgId, Reason]),
                    {error, {500, <<"查询失败，请稍后重试"/utf8>>}}
            end
    end.

%% ===================================================================
%% 读：组织下 Workspace 只读关系事实（分页）
%% ===================================================================

%% is_default 投影（CP-CON-03）：每行的「是否本 Org 默认 Workspace」由服务端
%% 真源 organization_default_workspace（迁移 00000130；PK=organization_id，
%% 每 Org 至多一条显式默认关系）经 LEFT JOIN 计算，不回落 min-ID 推导
%% （organization_default_workspace_pg 读取同口径）。无默认关系的 Org（0 条
%% 合法）全部投影 false；默认指向 archived ws 的历史态照实投影 true（真源
%% 如实，归档交接由 workspace 域守卫另行保证）。行数不变：JOIN 键含
%% workspace_id，每行至多匹配一条（PK 唯一）。
-define(ADMIN_WORKSPACE_PAGE_SQL,
    <<"SELECT w.id, w.name, w.owner_id, w.organization_id, w.status,",
        " w.created_at, w.updated_at,", " (odw.workspace_id IS NOT NULL) AS is_default",
        " FROM workspace w", " LEFT JOIN organization_default_workspace odw",
        "  ON odw.organization_id = w.organization_id AND odw.workspace_id = w.id",
        " WHERE w.organization_id = $1", " ORDER BY w.id DESC LIMIT $2 OFFSET $3">>
).

-spec admin_workspace_page(integer(), integer(), integer()) ->
    {ok, map()} | {error, {integer(), binary()}}.
admin_workspace_page(OrgId, Page, Size) ->
    case org_exists(OrgId) of
        false ->
            {error, {404, <<"Organization 不存在"/utf8>>}};
        true ->
            Page1 = max(Page, 1),
            Size1 = max(1, min(Size, 100)),
            CountSql = <<"SELECT COUNT(*) AS count FROM workspace WHERE organization_id = $1">>,
            Total =
                case elib_pg:one(CountSql, [OrgId]) of
                    {ok, #{<<"count">> := C}} -> C;
                    _ -> 0
                end,
            case elib_pg:query(?ADMIN_WORKSPACE_PAGE_SQL, [OrgId, Size1, (Page1 - 1) * Size1]) of
                {ok, Items} ->
                    TotalPage =
                        case Total > 0 of
                            true -> ((Total - 1) div Size1) + 1;
                            false -> 0
                        end,
                    {ok, #{
                        list => Items,
                        page => Page1,
                        size => Size1,
                        total => Total,
                        total_page => TotalPage
                    }};
                {error, Reason} ->
                    _ = ?ERROR_LOG([organization_admin_workspace_page_failed, OrgId, Reason]),
                    {error, {500, <<"查询失败，请稍后重试"/utf8>>}}
            end
    end.

%% ===================================================================
%% 写：Organization 原子创建（合同 EADM-01/C2；POST /api/adm/organizations）
%% 单事务：
%%   1) pg_advisory_xact_lock((owner, lower(trim(name)))) 串行（TSID 超 int4，
%%      两侧各取 hashtext；碰撞只损失并行度，正确性由锁内复查保证）；
%%   2) 锁内幂等：同 owner + 归一化名的 active 组织已存在 → created=false
%%      返回既有 org/default workspace（默认关系缺失（历史数据）如实返回 null，
%%      不回落 min-ID 推导——organization_default_workspace_app 同口径）；
%%   3) owner 门禁 fail-closed：存在 / status=1 活跃 / account_type=0 human；
%%   4) INSERT organization（trg_organization_owner_member_sync 在 INSERT 时
%%      同步 owner membership 行——迁移 00000113 既有事实）；
%%   5) 显式 owner membership upsert（与触发器幂等收敛为 role=owner/active；
%%      Platform Admin（adm_user.id）绝不写入任何租户身份列）；
%%   6) INSERT active default workspace；
%%   7) INSERT organization_default_workspace（复用
%%      organization_default_workspace_pg:upsert_tx 原语）。
%% 任一步失败整回滚。
%%
%% ⚠ 合同 EADM-01/C2（实施计划:109）：平台审计是**事务内第 8 步**，不是事后补写。
%% 此前实现把审计放在事务外由 handler 调用且吞掉写入错误 ⇒ 审计可静默丢失，
%% 违反「失败必须整事务回滚」。现已改为事务内 adm_operation_log_ds:insert_tx/7，
%% 写入失败即 throw({abort_tx,{audit_failed,_}}) ⇒ 业务数据一并回滚。
%% ===================================================================

-spec admin_create(integer(), binary(), integer(), binary(), map()) ->
    {ok, map()} | {error, {integer(), binary()}}.
admin_create(AdmUserId, Name, OwnerUid, WsName, AuditCtx) when
    is_integer(OwnerUid), OwnerUid > 0
->
    case valid_name(Name) of
        {error, _} ->
            {error, {400, <<"name 必填（1-200 字节，不能为空白）"/utf8>>}};
        ok ->
            case valid_name(WsName) of
                {error, _} ->
                    {error, {400, <<"default_workspace_name 必填（1-200 字节，不能为空白）"/utf8>>}};
                ok ->
                    Name1 = string:trim(Name),
                    WsName1 = string:trim(WsName),
                    Tx =
                        fun(Conn) ->
                            create_org_tx(Conn, AdmUserId, OwnerUid, Name1, WsName1, AuditCtx)
                        end,
                    case elib_pg:with_tx(Tx) of
                        {ok, Result} when is_map(Result) ->
                            ok = ?INFO_LOG([
                                organization_admin_created,
                                OwnerUid,
                                Name1,
                                maps:get(<<"created">>, Result)
                            ]),
                            {ok, Result};
                        {error, {Code, Msg}} when is_integer(Code), is_binary(Msg) ->
                            {error, {Code, Msg}};
                        {rollback, Reason} ->
                            _ = ?ERROR_LOG([
                                organization_admin_create_failed, OwnerUid, Name1, Reason
                            ]),
                            {error, {500, <<"创建 Organization 失败，请稍后重试"/utf8>>}};
                        {error, Reason} ->
                            _ = ?ERROR_LOG([
                                organization_admin_create_failed, OwnerUid, Name1, Reason
                            ]),
                            {error, {500, <<"创建 Organization 失败，请稍后重试"/utf8>>}}
                    end
            end
    end;
admin_create(_, _, _, _, _) ->
    {error, {400, <<"name、default_workspace_name 必填，owner_user_id 必须是正整数"/utf8>>}}.

create_org_tx(Conn, AdmUserId, OwnerUid, Name, WsName, AuditCtx) ->
    %% 1) 事务级 advisory lock：同 (owner, 归一化名) 的并发创建串行
    LockSql = <<"SELECT pg_advisory_xact_lock(hashtext($1::text), hashtext(lower(trim($2))))">>,
    case elib_pg:query(Conn, LockSql, [OwnerUid, Name]) of
        {ok, _} ->
            ok;
        {error, Reason0} ->
            throw({abort_tx, {internal, Reason0}})
    end,
    %% 2) 锁内幂等：同 owner + 归一化名的 active 组织已存在 → 返回既有资源
    case find_active_org_tx(Conn, OwnerUid, Name) of
        {ok, OrgRow} ->
            Result = #{
                <<"created">> => false,
                <<"organization">> => org_view(OrgRow),
                <<"default_workspace">> =>
                    existing_default_ws_view(Conn, maps:get(<<"id">>, OrgRow))
            },
            %% 8) 平台审计（事务内；幂等命中同样要留痕——「谁在何时请求创建」是治理事实）
            ok = audit_create_tx(Conn, AdmUserId, OwnerUid, Result, AuditCtx),
            {ok, Result};
        {error, not_found} ->
            create_new_org_tx(Conn, AdmUserId, OwnerUid, Name, WsName, AuditCtx);
        {error, Reason1} ->
            throw({abort_tx, {internal, Reason1}})
    end.

create_new_org_tx(Conn, AdmUserId, OwnerUid, Name, WsName, AuditCtx) ->
    %% 3) owner 门禁（存在 / active / human），fail-closed
    ok = owner_gate_tx(Conn, OwnerUid),
    {ok, OrgId, WsId} = insert_org_core_tx(Conn, OwnerUid, Name, WsName),
    Result = #{
        <<"created">> => true,
        <<"organization">> =>
            #{
                <<"id">> => OrgId,
                <<"name">> => Name,
                <<"owner_id">> => OwnerUid,
                <<"status">> => <<"active">>
            },
        <<"default_workspace">> =>
            #{<<"id">> => WsId, <<"name">> => WsName, <<"status">> => <<"active">>}
    },
    ok = audit_create_tx(Conn, AdmUserId, OwnerUid, Result, AuditCtx),
    {ok, Result}.

%% 创建审计必须与 Organization、Owner 和默认 Workspace 在同一事务提交。
-spec audit_create_tx(term(), integer(), integer(), map(), map()) -> ok.
audit_create_tx(Conn, AdmUserId, OwnerUid, Result, AuditCtx) ->
    Org = maps:get(<<"organization">>, Result),
    Ws = maps:get(<<"default_workspace">>, Result, null),
    OrgId = maps:get(<<"id">>, Org),
    Detail = #{
        <<"action">> => <<"create">>,
        <<"organization_id">> => OrgId,
        <<"owner_user_id">> => OwnerUid,
        <<"default_workspace_id">> => ws_field(Ws, <<"id">>),
        <<"default_workspace_name">> => ws_field(Ws, <<"name">>),
        <<"created">> => maps:get(<<"created">>, Result),
        <<"request">> => maps:get(request, AuditCtx, #{})
    },
    case
        adm_operation_log_ds:insert_tx(
            Conn,
            AdmUserId,
            <<"organization_create">>,
            OrgId,
            <<"organization">>,
            Detail,
            maps:get(ip, AuditCtx, undefined)
        )
    of
        ok -> ok;
        {error, Reason} -> throw({abort_tx, {audit_failed, Reason}})
    end.

ws_field(Ws, Key) when is_map(Ws) ->
    maps:get(Key, Ws, null);
ws_field(_, _) ->
    null.

%% 步骤 4-7 共享核心（registered 与 pending_phone 两模式同构）：
%%   4) INSERT organization（trg_organization_owner_member_sync 同步 owner
%%      membership 行——迁移 00000113；对预创建 Owner 同样成立：触发器与
%%      00000126/00000127 invariant 只校验 account_type=0 Human，不校验
%%      user.status——「org.owner_id 暂锚预创建 user」与既有不变量兼容，
%%      激活时仅翻转 user.status 0→1（GZAPP-06 锚点决策）；
%%   5) 显式 owner membership upsert；
%%   6) 默认 Workspace 模板（workspace_ds:create_default_template_tx/4）：
%%      workspace 行 + Org 默认关系 + owner workspace_member + 全员群 +
%%      公告频道（含订阅）——与建工作区路径同一实现（R3-3/F3）。
insert_org_core_tx(Conn, OwnerUid, Name, WsName) ->
    OrgId = elib_tsid:generate(organization),
    case
        elib_pg:execute(
            Conn,
            <<"INSERT INTO organization (id, name, owner_id, status)",
                " VALUES ($1, $2, $3, 'active')">>,
            [OrgId, Name, OwnerUid]
        )
    of
        {ok, 1} ->
            ok;
        {ok, _} ->
            throw({abort_tx, {internal, organization_insert_affected}});
        {error, Reason1} ->
            throw({abort_tx, {internal, Reason1}})
    end,
    %% 5) 显式 owner membership upsert（与触发器幂等收敛为 role=owner/active；
    %%    PK 冲突时 DO UPDATE 收敛，不产生第二行）
    case
        elib_pg:execute(
            Conn,
            <<"INSERT INTO organization_member (organization_id, user_id, role, status, joined_at)",
                " VALUES ($1, $2, 'owner', 'active', now())",
                " ON CONFLICT (organization_id, user_id)",
                " DO UPDATE SET role = 'owner', status = 'active', updated_at = now()">>,
            [OrgId, OwnerUid]
        )
    of
        {ok, _} ->
            ok;
        {error, Reason2} ->
            throw({abort_tx, {internal, Reason2}})
    end,
    %% 6-7) 默认 Workspace 模板（R3-3/F3）：改走 workspace_ds 的模板原语，
    %% 与「建工作区」路径同一实现——workspace 行 + Org 默认关系 +
    %% owner workspace_member + 全员群（General）+ 公告频道（Announcements，含订阅）。
    %% 此前这里是一句裸 INSERT 加一条默认关系：建出的组织没有全员群/公告频道，
    %% owner 也不在 workspace_member（连自己工作区的企业群都进不去），
    %% 加入编排查不到这两行只能返回 none 并静默跳过——撞
    %% plan.snapshot.md:54/106/222/224 与 GZ-J01「全员群、公告频道原子成功」。
    {ok, Template} = workspace_ds:create_default_template_tx(Conn, OwnerUid, OrgId, WsName),
    {ok, OrgId, maps:get(workspace_id, Template)}.

%% ===================================================================
%% 写：Organization 原子创建 · pending_phone 模式（GZAPP-06 / D11-D12）
%% 单事务（与 registered 模式同构，owner 门禁换成「手机号 → 预创建 Human」）：
%%   1) pg_advisory_xact_lock(("mob:" + mobile, lower(trim(name)))) 串行；
%%   2) 手机号裁决：活跃注册用户 → 409 引导已注册模式；预创建 Human → 复用；
%%      不存在 → 预创建不可登录 Human（status=0, account_type=0）；
%%   3) 锁内幂等（owner, 归一化名）→ created=false 返回既有资源 + 当前
%%      live invite 视图（不重发短信、不吐新 token）；
%%   4-7) 复用 insert_org_core_tx（org+membership+default ws+默认关系）；
%%   8) INSERT owner_activation_invite（pending，token 只存 digest）。
%% 提交后：尝试发一次激活短信（fake）——失败不回滚企业，仅置
%% invite.status=sms_failed，可重发（D12）。
%% ===================================================================

-spec admin_create_pending_owner(integer(), binary(), binary(), binary(), binary()) ->
    {ok, map()} | {error, {integer(), binary()}}.
admin_create_pending_owner(AdmUserId, Name, Mobile, WsName, PeerIP) when
    is_binary(Mobile), is_binary(PeerIP)
->
    admin_create_pending_owner(
        AdmUserId, Name, Mobile, WsName, PeerIP, #{ip => PeerIP, request => #{}}
    );
admin_create_pending_owner(_, _, _, _, _) ->
    {error, {400, <<"name、default_workspace_name、owner_mobile 必填"/utf8>>}}.

-spec admin_create_pending_owner(integer(), binary(), binary(), binary(), binary(), map()) ->
    {ok, map()} | {error, {integer(), binary()}}.
admin_create_pending_owner(AdmUserId, Name, Mobile, WsName, PeerIP, AuditCtx) when
    is_binary(Mobile), is_binary(PeerIP), is_map(AuditCtx)
->
    case valid_name(Name) of
        {error, _} ->
            {error, {400, <<"name 必填（1-200 字节，不能为空白）"/utf8>>}};
        ok ->
            case valid_name(WsName) of
                {error, _} ->
                    {error, {400, <<"default_workspace_name 必填（1-200 字节，不能为空白）"/utf8>>}};
                ok ->
                    create_pending_owner_gated(
                        AdmUserId, Name, Mobile, WsName, PeerIP, AuditCtx
                    )
            end
    end;
admin_create_pending_owner(_, _, _, _, _, _) ->
    {error, {400, <<"name、default_workspace_name、owner_mobile 必填"/utf8>>}}.

create_pending_owner_gated(AdmUserId, Name, Mobile, WsName, PeerIP, AuditCtx) ->
    case imboy_mobile:normalize(Mobile) of
        {error, invalid} ->
            {error, {400, <<"owner_mobile 格式非法（5-20 位数字）"/utf8>>}};
        {ok, Mobile1} ->
            Name1 = string:trim(Name),
            WsName1 = string:trim(WsName),
            Tx = fun(Conn) ->
                create_pending_org_tx(
                    Conn, AdmUserId, Mobile1, Name1, WsName1, PeerIP, AuditCtx
                )
            end,
            case elib_pg:with_tx(Tx) of
                {ok, #{created := Created} = Result} when is_map(Result) ->
                    ok = ?INFO_LOG([
                        organization_admin_pending_created,
                        maps:get(owner_uid, Result),
                        Name1,
                        Created,
                        imboy_mobile:mask(Mobile1)
                    ]),
                    finalize_pending_create(Result);
                {error, {Code, Msg}} when is_integer(Code), is_binary(Msg) ->
                    {error, {Code, Msg}};
                {rollback, Reason} ->
                    _ = ?ERROR_LOG([
                        organization_admin_pending_create_failed, Reason, imboy_mobile:mask(Mobile1)
                    ]),
                    {error, {500, <<"创建 Organization 失败，请稍后重试"/utf8>>}};
                {error, Reason} ->
                    _ = ?ERROR_LOG([
                        organization_admin_pending_create_failed, Reason, imboy_mobile:mask(Mobile1)
                    ]),
                    {error, {500, <<"创建 Organization 失败，请稍后重试"/utf8>>}}
            end
    end.

create_pending_org_tx(Conn, AdmUserId, Mobile, Name, WsName, PeerIP, AuditCtx) ->
    %% 1) 事务级 advisory lock：同 (mobile, 归一化名) 的并发创建串行
    LockSql = <<"SELECT pg_advisory_xact_lock(hashtext($1::text), hashtext(lower(trim($2))))">>,
    case elib_pg:query(Conn, LockSql, [<<"mob:", Mobile/binary>>, Name]) of
        {ok, _} ->
            ok;
        {error, Reason0} ->
            throw({abort_tx, {internal, Reason0}})
    end,
    %% 2) 手机号裁决（预创建/复用；活跃注册用户 → 409 引导已注册模式）
    case organization_owner_activation_logic:resolve_target_tx(Conn, Mobile, PeerIP) of
        {ok, OwnerUid, pending} ->
            pending_owner_locked(
                Conn, AdmUserId, OwnerUid, Mobile, Name, WsName, AuditCtx
            );
        {ok, _Uid, _Kind} ->
            %% 活跃注册用户（registered）：引导改用已注册 Owner 模式
            abort(409, <<"该手机号已是注册活跃用户，请改用已注册 Owner 模式选择该用户"/utf8>>);
        {error, {Code, Msg}} when is_integer(Code), is_binary(Msg) ->
            abort(Code, Msg);
        {error, {500, Reason1}} ->
            throw({abort_tx, {internal, Reason1}})
    end.

pending_owner_locked(Conn, AdmUserId, OwnerUid, Mobile, Name, WsName, AuditCtx) ->
    %% 3) 锁内幂等：同 owner + 归一化名的 active 组织已存在 → 既有资源 + 当前 invite
    case find_active_org_tx(Conn, OwnerUid, Name) of
        {ok, OrgRow} ->
            OrgId = maps:get(<<"id">>, OrgRow),
            Org = org_view(OrgRow),
            Ws = existing_default_ws_view(Conn, OrgId),
            ok = audit_create_tx(
                Conn,
                AdmUserId,
                OwnerUid,
                #{
                    <<"created">> => false,
                    <<"organization">> => Org,
                    <<"default_workspace">> => Ws
                },
                AuditCtx
            ),
            {ok, #{
                created => false,
                owner_uid => OwnerUid,
                org => Org,
                ws => Ws,
                invite =>
                    case organization_owner_activation_logic:live_invite_tx(Conn, OrgId) of
                        {ok, InviteRow} ->
                            organization_owner_activation_logic:invite_view(InviteRow);
                        _ ->
                            null
                    end,
                invite_ctx => undefined
            }};
        {error, not_found} ->
            pending_owner_create_new(
                Conn, AdmUserId, OwnerUid, Mobile, Name, WsName, AuditCtx
            );
        {error, Reason1} ->
            throw({abort_tx, {internal, Reason1}})
    end.

pending_owner_create_new(Conn, AdmUserId, OwnerUid, Mobile, Name, WsName, AuditCtx) ->
    %% 4-7) 共享创建核心（含 00000113 同步触发器 + invariant 提交校验）
    {ok, OrgId, WsId} = insert_org_core_tx(Conn, OwnerUid, Name, WsName),
    %% 8) owner_activation_invite 行（pending；token 只存 digest）
    Token = organization_invitation:new_token(),
    InviteCtx = organization_owner_activation_logic:new_invite_ctx(
        OrgId, OwnerUid, Mobile, AdmUserId, Token, Name
    ),
    case organization_owner_activation_logic:insert_invite_tx(Conn, InviteCtx) of
        ok ->
            Org = #{
                <<"id">> => OrgId,
                <<"name">> => Name,
                <<"owner_id">> => OwnerUid,
                <<"status">> => <<"active">>
            },
            Ws = #{<<"id">> => WsId, <<"name">> => WsName, <<"status">> => <<"active">>},
            ok = audit_create_tx(
                Conn,
                AdmUserId,
                OwnerUid,
                #{
                    <<"created">> => true,
                    <<"organization">> => Org,
                    <<"default_workspace">> => Ws
                },
                AuditCtx
            ),
            {ok, #{
                created => true,
                owner_uid => OwnerUid,
                org => Org,
                ws => Ws,
                invite => undefined,
                invite_ctx => InviteCtx,
                org_name => Name
            }};
        {error, Reason2} ->
            throw({abort_tx, {internal, Reason2}})
    end.

%% 提交后收口：仅 created=true 路径尝试发一次激活短信（D12：失败不回滚）；
%% 幂等命中（created=false）不重发、不吐新 token。
finalize_pending_create(#{created := true, invite_ctx := InviteCtx} = Result) ->
    SendStatus =
        organization_owner_activation_logic:record_send_result(
            organization_owner_activation_logic:attempt_send(InviteCtx), InviteCtx
        ),
    Mobile = maps:get(mobile, InviteCtx),
    InviteStatus =
        case SendStatus of
            <<"sent">> -> <<"pending">>;
            _ -> <<"sms_failed">>
        end,
    {ok, #{
        <<"created">> => true,
        <<"organization">> => maps:get(org, Result),
        <<"default_workspace">> => maps:get(ws, Result),
        <<"owner_activation">> => #{
            <<"invite_id">> => maps:get(invite_id, InviteCtx),
            <<"organization_id">> => maps:get(organization_id, InviteCtx),
            <<"owner_user_id">> => maps:get(owner_user_id, InviteCtx),
            <<"status">> => InviteStatus,
            <<"mobile_masked">> => imboy_mobile:mask(Mobile),
            <<"expires_at">> => maps:get(expires_at, InviteCtx),
            <<"resend_count">> => 0,
            <<"activation_token">> => maps:get(token, InviteCtx)
        },
        <<"sms_sent">> => SendStatus =:= <<"sent">>
    }};
finalize_pending_create(#{created := false} = Result) ->
    {ok, #{
        <<"created">> => false,
        <<"organization">> => maps:get(org, Result),
        <<"default_workspace">> => maps:get(ws, Result),
        <<"owner_activation">> => maps:get(invite, Result),
        <<"sms_sent">> => false
    }}.

%% owner 门禁：存在（404）/ status=1 活跃（400）/ account_type=0 human（400）。
%% status 口径与 passport_logic 签发收口一致：1 启用；0 禁用 / 2 注销中 /
%% 负值已删除一律 fail-closed 拒绝。account_type：0=human（迁移 00000027 注释）。
owner_gate_tx(Conn, OwnerUid) ->
    case
        elib_pg:query(
            Conn, <<"SELECT status, account_type FROM \"user\" WHERE id = $1">>, [OwnerUid]
        )
    of
        {ok, []} ->
            abort(404, <<"Owner 用户不存在"/utf8>>);
        {ok, [#{<<"status">> := Status, <<"account_type">> := AccountType}]} ->
            case Status of
                1 ->
                    ok;
                _ ->
                    abort(400, <<"Owner 用户不是活跃账号"/utf8>>)
            end,
            case AccountType of
                0 ->
                    ok;
                _ ->
                    abort(400, <<"Owner 必须是人类账号"/utf8>>)
            end;
        {ok, _OtherShape} ->
            throw({abort_tx, {internal, owner_row_shape}});
        {error, Reason} ->
            throw({abort_tx, {internal, Reason}})
    end.

find_active_org_tx(Conn, OwnerUid, Name) ->
    Sql =
        <<"SELECT id, name, owner_id, status FROM organization",
            " WHERE owner_id = $1 AND lower(trim(name)) = lower(trim($2)) AND status = 'active'">>,
    case elib_pg:query(Conn, Sql, [OwnerUid, Name]) of
        {ok, [Row | _]} ->
            {ok, Row};
        {ok, []} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

%% 幂等命中路径回读既有 default workspace；显式关系缺失（历史 Org）→ null，
%% 不回落 min-ID 推导（organization_default_workspace_app 读取同口径）。
existing_default_ws_view(Conn, OrgId) ->
    case organization_default_workspace_pg:find_tx(Conn, OrgId) of
        {ok, WsId} ->
            case
                elib_pg:query(
                    Conn, <<"SELECT id, name, status FROM workspace WHERE id = $1">>, [WsId]
                )
            of
                {ok, [Row | _]} ->
                    ws_view(Row);
                _ ->
                    null
            end;
        _ ->
            null
    end.

org_view(Row) ->
    #{
        <<"id">> => maps:get(<<"id">>, Row),
        <<"name">> => maps:get(<<"name">>, Row),
        <<"owner_id">> => maps:get(<<"owner_id">>, Row),
        <<"status">> => maps:get(<<"status">>, Row)
    }.

ws_view(Row) ->
    #{
        <<"id">> => maps:get(<<"id">>, Row),
        <<"name">> => maps:get(<<"name">>, Row),
        <<"status">> => maps:get(<<"status">>, Row)
    }.

valid_name(Name) when is_binary(Name) ->
    case byte_size(string:trim(Name)) of
        N when N >= 1, N =< 200 ->
            ok;
        _ ->
            {error, invalid}
    end;
valid_name(_) ->
    {error, invalid}.

%% ===================================================================
%% 写：archive / restore（幂等；镜像 organization_lifecycle 状态机，平台侧
%% 无租户 actor 校验；复用 lifecycle_pg 锁与 set_status 原语）
%% 平台审计（CP-ASSET-05）：状态迁移与 adm_operation_log_ds:insert_tx 同一
%% 事务提交，审计写入失败 throw({abort_tx,{audit_failed,_}}) ⇒ 状态迁移一并回滚。
%% ===================================================================

-spec admin_archive(integer(), integer(), map()) -> {ok, map()} | {error, {integer(), binary()}}.
admin_archive(AdmUserId, OrgId, AuditCtx) ->
    transition(AdmUserId, OrgId, <<"archived">>, AuditCtx).

-spec admin_restore(integer(), integer(), map()) -> {ok, map()} | {error, {integer(), binary()}}.
admin_restore(AdmUserId, OrgId, AuditCtx) ->
    transition(AdmUserId, OrgId, <<"active">>, AuditCtx).

transition(AdmUserId, OrgId, TargetStatus, AuditCtx) when is_integer(OrgId), OrgId > 0 ->
    Action =
        case TargetStatus of
            <<"archived">> -> <<"archive">>;
            <<"active">> -> <<"restore">>
        end,
    Tx =
        fun(Conn) ->
            Res =
                case organization_lifecycle_pg:lock_organization_tx(Conn, OrgId) of
                    {ok, Org} ->
                        case maps:get(<<"status">>, Org) of
                            TargetStatus ->
                                %% 幂等重放：状态未变，零写入（与 app 层同口径）
                                {ok, {Org, false}};
                            <<"pending">> ->
                                %% 审核状态机唯一放行出口是 admin_review，
                                %% 平台 archive/restore 不得把 pending 推成 active
                                abort(409, <<"Organization 待审核，审核通过前不能执行此操作"/utf8>>);
                            <<"rejected">> ->
                                abort(409, <<"Organization 未通过审核，不能执行此操作"/utf8>>);
                            _Current ->
                                case
                                    organization_lifecycle_pg:set_status_tx(
                                        Conn, OrgId, TargetStatus
                                    )
                                of
                                    {ok, Updated} ->
                                        {ok, {Updated, true}};
                                    {error, Reason} ->
                                        throw({abort_tx, {internal, Reason}})
                                end
                        end;
                    {error, not_found} ->
                        abort(404, <<"Organization 不存在"/utf8>>);
                    {error, Reason} ->
                        throw({abort_tx, {internal, Reason}})
                end,
            %% 平台审计（事务内；幂等重放同样留痕）：失败整事务回滚
            ok = audit_governance_tx(Conn, AdmUserId, Action, OrgId, #{}, AuditCtx),
            Res
        end,
    case elib_pg:with_tx(Tx) of
        {ok, {_Org, Changed}} ->
            AuditTag =
                case TargetStatus of
                    <<"archived">> -> organization_admin_archived;
                    <<"active">> -> organization_admin_restored
                end,
            case Changed of
                true -> ok = ?INFO_LOG([AuditTag, OrgId]);
                false -> ok
            end,
            {ok, #{organization_id => OrgId, status => TargetStatus, changed => Changed}};
        {error, {Code, Msg}} when is_integer(Code), is_binary(Msg) ->
            {error, {Code, Msg}};
        {error, Reason} ->
            _ = ?ERROR_LOG([organization_admin_transition_failed, OrgId, TargetStatus, Reason]),
            {error, {500, <<"操作失败，请稍后重试"/utf8>>}};
        {rollback, Reason} ->
            _ = ?ERROR_LOG([organization_admin_transition_failed, OrgId, TargetStatus, Reason]),
            {error, {500, <<"操作失败，请稍后重试"/utf8>>}}
    end;
transition(_, _, _, _) ->
    {error, {400, <<"organization_id 必须是正整数"/utf8>>}}.

%% ===================================================================
%% 写：Owner 转移（镜像 organization_owner_transfer 单事务；差异仅两点：
%% ① 无租户 actor ACL——发起方是平台，操作者审计在 handler 层；
%% ② 降级的是锁内读出的当前 owner_id，而非操作者）。
%% 复用 owner_store 原语 + owner_invariant 校验器；提交时双侧 DEFERRABLE
%% invariant 触发器做最终防线（与生产一致）。
%% 并发口径：组织行 FOR UPDATE 串行化；organization 行无 version 列
%% （expected-version 语义 app 层即不存在，此处如实不提供）。
%% ===================================================================

-spec admin_transfer_owner(integer(), integer(), integer(), map()) ->
    {ok, map()} | {error, {integer(), binary()}}.
admin_transfer_owner(AdmUserId, OrgId, TargetUid, AuditCtx) when
    is_integer(OrgId), OrgId > 0, is_integer(TargetUid), TargetUid > 0
->
    Tx =
        fun(Conn) ->
            Res =
                case organization_owner_store:lock_organization_tx(Conn, OrgId) of
                    {ok, #{<<"status">> := <<"active">>} = OrgRow} ->
                        CurrentOwner = maps:get(<<"owner_id">>, OrgRow),
                        transfer_locked(Conn, OrgId, CurrentOwner, TargetUid);
                    {ok, _Archived} ->
                        abort(409, <<"Organization 已归档，不能转移 Owner"/utf8>>);
                    {error, not_found} ->
                        abort(404, <<"Organization 不存在"/utf8>>);
                    {error, Reason1} ->
                        throw({abort_tx, {internal, Reason1}})
                end,
            %% 平台审计（事务内；CP-ASSET-05）：失败整事务回滚
            {ok, TransferInfo} = Res,
            ok = audit_governance_tx(
                Conn,
                AdmUserId,
                <<"owner_transfer">>,
                OrgId,
                #{
                    <<"target_user_id">> => TargetUid,
                    <<"previous_owner_id">> => maps:get(previous_owner_id, TransferInfo, 0)
                },
                AuditCtx
            ),
            Res
        end,
    case elib_pg:with_tx(Tx) of
        {ok, Result} when is_map(Result) ->
            ok = ?INFO_LOG([
                organization_admin_owner_transferred,
                OrgId,
                maps:get(previous_owner_id, Result),
                TargetUid
            ]),
            {ok, Result};
        {error, {Code, Msg}} when is_integer(Code), is_binary(Msg) ->
            {error, {Code, Msg}};
        {rollback, Reason} ->
            _ = ?ERROR_LOG([organization_admin_owner_transfer_failed, OrgId, TargetUid, Reason]),
            {error, {500, <<"Owner 转移失败，请稍后重试"/utf8>>}};
        {error, Reason} ->
            _ = ?ERROR_LOG([organization_admin_owner_transfer_failed, OrgId, TargetUid, Reason]),
            {error, {500, <<"Owner 转移失败，请稍后重试"/utf8>>}}
    end;
admin_transfer_owner(_, _, _, _) ->
    {error, {400, <<"organization_id 和 user_id 必须是正整数"/utf8>>}}.

transfer_locked(Conn, OrgId, CurrentOwner, TargetUid) ->
    case organization_owner_invariant:validate_self_transfer(CurrentOwner, TargetUid) of
        {error, {_Code, Msg}} ->
            abort(400, Msg);
        ok ->
            case organization_owner_store:lock_member_with_account_tx(Conn, OrgId, TargetUid) of
                {ok, TargetMember} ->
                    case organization_owner_invariant:validate_target_membership(TargetMember) of
                        ok ->
                            do_transfer_locked(Conn, OrgId, CurrentOwner, TargetUid);
                        {error, {Code5, Msg5}} ->
                            abort(Code5, Msg5)
                    end;
                {error, not_found} ->
                    abort(409, <<"该用户不是组织成员或已被移除"/utf8>>);
                {error, Reason4} ->
                    throw({abort_tx, {internal, Reason4}})
            end
    end.

do_transfer_locked(Conn, OrgId, CurrentOwner, TargetUid) ->
    %% 先降旧 owner（提交时 deferred guard 以 owner_id 最终值放行）
    case organization_owner_store:demote_previous_owner_tx(Conn, OrgId, CurrentOwner) of
        ok ->
            case organization_owner_store:promote_target_tx(Conn, OrgId, TargetUid) of
                ok ->
                    case
                        organization_owner_store:update_owner_projection_tx(Conn, OrgId, TargetUid)
                    of
                        {ok, _} ->
                            {ok, #{
                                organization_id => OrgId,
                                owner_id => TargetUid,
                                previous_owner_id => CurrentOwner,
                                previous_owner_role => <<"admin">>
                            }};
                        {error, Reason8} ->
                            throw({abort_tx, {internal, Reason8}})
                    end;
                {error, Reason7} ->
                    throw({abort_tx, {internal, Reason7}})
            end;
        {error, Reason6} ->
            throw({abort_tx, {internal, Reason6}})
    end.

%% ===================================================================
%% 写：成员 suspend / restore / remove（镜像 organization_member_logic
%% 的事务裁决与锁序「组织行先、成员行后」；差异仅①无租户 actor ACL
%% ②组织行用 lifecycle_pg 的 FOR UPDATE 锁。owner 保护的 409 先判，
%% 避免把 DB 的 23514 当内部错误——与 app 层同口径）。
%% ===================================================================

-spec admin_member_suspend(integer(), integer(), integer(), map()) ->
    {ok, map()} | {error, {integer(), binary()}}.
admin_member_suspend(AdmUserId, OrgId, TargetUid, AuditCtx) ->
    member_transition(AdmUserId, OrgId, TargetUid, suspend, AuditCtx).

-spec admin_member_restore(integer(), integer(), integer(), map()) ->
    {ok, map()} | {error, {integer(), binary()}}.
admin_member_restore(AdmUserId, OrgId, TargetUid, AuditCtx) ->
    member_transition(AdmUserId, OrgId, TargetUid, restore, AuditCtx).

-spec admin_member_remove(integer(), integer(), integer(), map()) ->
    {ok, map()} | {error, {integer(), binary()}}.
admin_member_remove(AdmUserId, OrgId, TargetUid, AuditCtx) ->
    member_transition(AdmUserId, OrgId, TargetUid, remove, AuditCtx).

member_transition(AdmUserId, OrgId, TargetUid, Action, AuditCtx) when
    is_integer(OrgId), OrgId > 0, is_integer(TargetUid), TargetUid > 0
->
    Tx =
        fun(Conn) ->
            Res =
                %% 组织行先锁 + archived 门禁（C16：归档禁新写）
                case organization_lifecycle_pg:lock_organization_tx(Conn, OrgId) of
                    {ok, #{<<"status">> := <<"active">>}} ->
                        member_transition_locked(Conn, OrgId, TargetUid, Action);
                    {ok, _Archived} ->
                        abort(409, <<"Organization 已归档，成员管理被拒绝"/utf8>>);
                    {error, not_found} ->
                        abort(404, <<"Organization 不存在"/utf8>>);
                    {error, Reason1} ->
                        throw({abort_tx, {internal, Reason1}})
                end,
            %% 平台审计（事务内；CP-ASSET-05）：失败整事务回滚
            {ok, _} = Res,
            ok = audit_governance_tx(
                Conn,
                AdmUserId,
                member_action_name(Action),
                OrgId,
                #{<<"target_user_id">> => TargetUid},
                AuditCtx
            ),
            Res
        end,
    case elib_pg:with_tx(Tx) of
        {ok, Result} when is_map(Result) ->
            lists:foreach(
                fun workspace_ds:member_removed/1, maps:get(affected_workspaces, Result, [])
            ),
            ok = ?INFO_LOG([member_audit_tag(Action), OrgId, TargetUid]),
            {ok, Result};
        {error, {Code, Msg}} when is_integer(Code), is_binary(Msg) ->
            {error, {Code, Msg}};
        {error, {membership_conflict, _}} ->
            {error, {409, <<"该成员仍有需交接的群、频道、项目或任务，请先完成交接"/utf8>>}};
        {rollback, Reason} ->
            _ = ?ERROR_LOG([organization_admin_member_failed, Action, OrgId, TargetUid, Reason]),
            {error, {500, <<"操作失败，请稍后重试"/utf8>>}};
        {error, Reason} ->
            _ = ?ERROR_LOG([organization_admin_member_failed, Action, OrgId, TargetUid, Reason]),
            {error, {500, <<"操作失败，请稍后重试"/utf8>>}}
    end;
member_transition(_, _, _, _, _) ->
    {error, {400, <<"organization_id 和 user_id 必须是正整数"/utf8>>}}.

member_action_name(suspend) -> <<"member_suspend">>;
member_action_name(restore) -> <<"member_restore">>;
member_action_name(remove) -> <<"member_remove">>.

member_transition_locked(Conn, OrgId, TargetUid, Action) ->
    case organization_member_repo:find_for_update_tx(Conn, OrgId, TargetUid, <<"role,status">>) of
        {ok, Member} ->
            decide_member(Conn, OrgId, TargetUid, Action, Member);
        {error, not_found} ->
            abort(409, member_not_active_msg());
        {error, Reason} ->
            throw({abort_tx, {internal, Reason}})
    end.

decide_member(Conn, OrgId, TargetUid, suspend, #{<<"status">> := <<"active">>, <<"role">> := Role}) ->
    case Role of
        <<"owner">> ->
            abort(409, <<"主 Owner 不能被暂停，请先转移 Owner"/utf8>>);
        _ ->
            case set_member_status_tx(Conn, OrgId, TargetUid, <<"active">>, <<"suspended">>) of
                ok ->
                    {ok, member_result(OrgId, TargetUid, Role, <<"suspended">>)};
                {error, Reason} ->
                    throw({abort_tx, {internal, Reason}})
            end
    end;
decide_member(_Conn, _OrgId, _TargetUid, suspend, _NotActive) ->
    abort(409, member_not_active_msg());
decide_member(Conn, OrgId, TargetUid, restore, #{
    <<"status">> := <<"suspended">>, <<"role">> := Role
}) ->
    case set_member_status_tx(Conn, OrgId, TargetUid, <<"suspended">>, <<"active">>) of
        ok ->
            {ok, member_result(OrgId, TargetUid, Role, <<"active">>)};
        {error, Reason} ->
            throw({abort_tx, {internal, Reason}})
    end;
decide_member(_Conn, _OrgId, _TargetUid, restore, _NotSuspended) ->
    %% active（无需恢复）与 removed（终态，恢复走重新邀请）都 409——与 app 层同口径
    abort(409, <<"该成员不在暂停状态，无法恢复"/utf8>>);
decide_member(_Conn, _OrgId, _TargetUid, remove, #{<<"role">> := <<"owner">>}) ->
    abort(409, <<"主 Owner 不能被移除，请先转移 Owner"/utf8>>);
decide_member(Conn, OrgId, TargetUid, remove, #{<<"status">> := Status}) when
    Status =:= <<"active">>; Status =:= <<"suspended">>
->
    Workspaces = workspace_ds:remove_organization_memberships_tx(Conn, OrgId, TargetUid),
    case remove_member_row_tx(Conn, OrgId, TargetUid) of
        ok ->
            Result = member_result(OrgId, TargetUid, undefined, <<"removed">>),
            {ok, Result#{affected_workspaces => Workspaces}};
        {error, Reason} ->
            case organization_member_logic:dependent_resources_conflict(Reason) of
                {conflict, Message} ->
                    abort(409, Message);
                none ->
                    throw({abort_tx, {internal, Reason}})
            end
    end;
decide_member(_Conn, _OrgId, _TargetUid, remove, _Other) ->
    abort(409, member_not_active_msg()).

%% 镜像 organization_member_logic:set_member_status_tx（私有）：精确单列状态迁移，
%% 只接受仍处于来源态的行；影响行数不为 1 即显式失败，不静默成功。
set_member_status_tx(Conn, OrgId, Uid, FromStatus, Status) ->
    Sql =
        <<
            "UPDATE organization_member SET status = $4, updated_at = CURRENT_TIMESTAMP"
            " WHERE organization_id = $1 AND user_id = $2 AND status = $3"
        >>,
    case elib_pg:execute(Conn, Sql, [OrgId, Uid, FromStatus, Status]) of
        {ok, 1} ->
            ok;
        {ok, _} ->
            {error, member_not_active};
        {error, Reason} ->
            {error, Reason}
    end.

%% 镜像 organization_member_logic:remove_member_row_tx（私有）：从 active|suspended
%% 出发的精确移除；依赖资源守卫（active 经办关系 23514）由调用方经
%% organization_member_logic:dependent_resources_conflict/1（已导出）翻译成 409。
remove_member_row_tx(Conn, OrgId, Uid) ->
    Sql =
        <<
            "UPDATE organization_member SET status = 'removed', updated_at = CURRENT_TIMESTAMP"
            " WHERE organization_id = $1 AND user_id = $2 AND status IN ('active','suspended')"
        >>,
    case elib_pg:execute(Conn, Sql, [OrgId, Uid]) of
        {ok, 1} ->
            ok;
        {ok, _} ->
            {error, member_not_active};
        {error, Reason} ->
            {error, Reason}
    end.

member_result(OrgId, Uid, Role, Status) ->
    #{
        organization_id => OrgId,
        user_id => Uid,
        role => Role,
        status => Status
    }.

member_audit_tag(suspend) -> organization_admin_member_suspended;
member_audit_tag(restore) -> organization_admin_member_restored;
member_audit_tag(remove) -> organization_admin_member_removed.

member_not_active_msg() ->
    <<"该成员不存在或已移除"/utf8>>.

%% ===================================================================
%% 写：邀请 create / cancel（镜像 organization_invitation_app 的 create_tx /
%% revoke_tx；差异：平台无租户 inviter——invited_by 固定 NULL，操作者审计在
%% handler 层。token 明文只返回一次；digest 不出投影）。
%% ===================================================================

-spec admin_invitation_create(integer(), integer(), integer(), integer() | undefined, map()) ->
    {ok, map()} | {error, {integer(), binary()}}.
admin_invitation_create(AdmUserId, OrgId, TargetUid, ExpiresAt, AuditCtx) when
    is_integer(OrgId), OrgId > 0, is_integer(TargetUid), TargetUid > 0
->
    Tx =
        fun(Conn) ->
            %% 1) 锁组织行 + 生命周期裁决（C16：archived 禁新写）
            case organization_invitation_pg:lock_organization_tx(Conn, OrgId) of
                {ok, #{<<"status">> := <<"active">>}} ->
                    ok;
                {ok, _Archived} ->
                    abort(409, <<"Organization 已归档，不能创建邀请"/utf8>>);
                {error, not_found} ->
                    abort(404, <<"Organization 不存在"/utf8>>);
                {error, Reason1} ->
                    throw({abort_tx, {internal, Reason1}})
            end,
            %% 2) target 必须是已注册用户（与租户面同口径同话术）
            case organization_invitation_pg:target_user_tx(Conn, TargetUid) of
                {ok, _} ->
                    ok;
                {error, not_found} ->
                    abort(404, <<"用户不存在（仅支持邀请已注册用户）"/utf8>>);
                {error, Reason35} ->
                    throw({abort_tx, {internal, Reason35}})
            end,
            %% 3) lazy expire sweep：先释放已到期占位，再插入（唯一索引是最终裁决）
            case organization_invitation_pg:expire_due_tx(Conn, OrgId) of
                {ok, _} ->
                    ok;
                {error, Reason4} ->
                    throw({abort_tx, {internal, Reason4}})
            end,
            %% 4) 插入；pending 唯一冲突 → 409；23503（校验后并发删号）→ 404
            Token = organization_invitation:new_token(),
            FinalRow = #{
                id => elib_tsid:generate(),
                organization_id => OrgId,
                target_user_id => TargetUid,
                invited_by => null,
                token_digest => organization_invitation:token_digest(Token),
                expires_at =>
                    case is_integer(ExpiresAt) andalso ExpiresAt > 0 of
                        true -> ExpiresAt;
                        false -> organization_invitation:expires_at_from_now(os:system_time(second))
                    end
            },
            case organization_invitation_pg:insert_tx(Conn, FinalRow) of
                ok ->
                    %% 平台审计（事务内；CP-ASSET-05）：失败整事务回滚（邀请行一并回滚）
                    ok = audit_governance_tx(
                        Conn,
                        AdmUserId,
                        <<"invitation_create">>,
                        OrgId,
                        #{<<"target_user_id">> => TargetUid},
                        AuditCtx
                    ),
                    PendingRow = FinalRow#{status => <<"pending">>},
                    {ok, (invitation_view(PendingRow))#{token => Token}};
                {error, pending_conflict} ->
                    abort(409, <<"该用户已有待处理邀请"/utf8>>);
                {error, target_user_missing} ->
                    abort(404, <<"用户不存在（仅支持邀请已注册用户）"/utf8>>);
                {error, Reason5} ->
                    throw({abort_tx, {internal, Reason5}})
            end
        end,
    case elib_pg:with_tx(Tx) of
        {ok, View} ->
            ok = ?INFO_LOG([organization_admin_invitation_created, OrgId, TargetUid]),
            %% 触达（P1）：与管理面/用户面共用同一触达模块（fire-and-forget）
            _ = organization_invitation_notify:notify_created(TargetUid),
            {ok, View};
        {error, {Code, Msg}} when is_integer(Code), is_binary(Msg) ->
            {error, {Code, Msg}};
        {rollback, Reason} ->
            _ = ?ERROR_LOG([organization_admin_invitation_failed, create, OrgId, TargetUid, Reason]),
            {error, {500, <<"创建邀请失败，请稍后重试"/utf8>>}};
        {error, Reason} ->
            _ = ?ERROR_LOG([organization_admin_invitation_failed, create, OrgId, TargetUid, Reason]),
            {error, {500, <<"创建邀请失败，请稍后重试"/utf8>>}}
    end;
admin_invitation_create(_, _, _, _, _) ->
    {error, {400, <<"organization_id 和 target_user_id 必须是正整数"/utf8>>}}.

-spec admin_invitation_cancel(integer(), integer(), integer(), map()) ->
    {ok, map()} | {error, {integer(), binary()}}.
admin_invitation_cancel(AdmUserId, OrgId, InvitationId, AuditCtx) when
    is_integer(OrgId), OrgId > 0, is_integer(InvitationId), InvitationId > 0
->
    Tx =
        fun(Conn) ->
            case organization_invitation_pg:lock_organization_tx(Conn, OrgId) of
                {ok, #{<<"status">> := <<"active">>}} ->
                    ok;
                {ok, _Archived} ->
                    abort(409, <<"Organization 已归档"/utf8>>);
                {error, not_found} ->
                    abort(404, <<"Organization 不存在"/utf8>>);
                {error, Reason1} ->
                    throw({abort_tx, {internal, Reason1}})
            end,
            case organization_invitation_pg:expire_due_tx(Conn, OrgId) of
                {ok, _} ->
                    ok;
                {error, Reason0} ->
                    throw({abort_tx, {internal, Reason0}})
            end,
            case organization_invitation_pg:find_tx(Conn, OrgId, InvitationId, 0) of
                {error, not_found} ->
                    abort(404, <<"邀请不存在"/utf8>>);
                {error, Reason2} ->
                    throw({abort_tx, {internal, Reason2}});
                {ok, Row} ->
                    case maps:get(<<"status">>, Row) of
                        <<"pending">> ->
                            case
                                organization_invitation_pg:consume_pending_tx(
                                    Conn, InvitationId, revoke
                                )
                            of
                                {ok, Consumed} ->
                                    %% 平台审计（事务内；CP-ASSET-05）：失败整事务回滚
                                    ok = audit_governance_tx(
                                        Conn,
                                        AdmUserId,
                                        <<"invitation_cancel">>,
                                        OrgId,
                                        #{<<"invitation_id">> => InvitationId},
                                        AuditCtx
                                    ),
                                    {ok, invitation_view(Consumed)};
                                {error, Reason3} ->
                                    throw({abort_tx, {internal, Reason3}})
                            end;
                        <<"revoked">> ->
                            %% 幂等重放同样留痕（事务内；零写入仅审计）
                            ok = audit_governance_tx(
                                Conn,
                                AdmUserId,
                                <<"invitation_cancel">>,
                                OrgId,
                                #{<<"invitation_id">> => InvitationId},
                                AuditCtx
                            ),
                            {ok, (invitation_view(Row))#{already_terminal => true}};
                        <<"accepted">> ->
                            abort(409, <<"邀请已被接受"/utf8>>);
                        <<"expired">> ->
                            abort(409, <<"邀请已过期"/utf8>>);
                        <<"rejected">> ->
                            abort(409, <<"邀请已被拒绝"/utf8>>)
                    end
            end
        end,
    case elib_pg:with_tx(Tx) of
        {ok, View} ->
            ok = ?INFO_LOG([organization_admin_invitation_cancelled, OrgId, InvitationId]),
            {ok, View};
        {error, {Code, Msg}} when is_integer(Code), is_binary(Msg) ->
            {error, {Code, Msg}};
        {rollback, Reason} ->
            _ = ?ERROR_LOG([
                organization_admin_invitation_failed, cancel, OrgId, InvitationId, Reason
            ]),
            {error, {500, <<"操作失败，请稍后重试"/utf8>>}};
        {error, Reason} ->
            _ = ?ERROR_LOG([
                organization_admin_invitation_failed, cancel, OrgId, InvitationId, Reason
            ]),
            {error, {500, <<"操作失败，请稍后重试"/utf8>>}}
    end;
admin_invitation_cancel(_, _, _, _) ->
    {error, {400, <<"organization_id 和 invitation_id 必须是正整数"/utf8>>}}.

%% 响应投影白名单：token_digest 永不出现（与 organization_invitation_app 同口径）；
%% create 视图的明文 token 由调用点临时加入。
invitation_view(Row) ->
    #{
        invitation_id => maps:get(<<"id">>, Row, maps:get(id, Row, undefined)),
        organization_id => maps:get(
            <<"organization_id">>, Row, maps:get(organization_id, Row, undefined)
        ),
        target_user_id => maps:get(
            <<"target_user_id">>, Row, maps:get(target_user_id, Row, undefined)
        ),
        invited_by => maps:get(<<"invited_by">>, Row, maps:get(invited_by, Row, undefined)),
        status => maps:get(<<"status">>, Row, maps:get(status, Row, undefined)),
        expires_at => maps:get(<<"expires_at">>, Row, maps:get(expires_at, Row, undefined)),
        responded_at => maps:get(
            <<"responded_at">>, Row, maps:get(responded_at, Row, undefined)
        ),
        created_at => maps:get(<<"created_at">>, Row, maps:get(created_at, Row, undefined))
    }.

%% ===================================================================
%% 写：部门 create / rename / move / archive
%% （复用 organization_department domain 校验与 department_pg 原语；
%% 原语的环防/跨 Org/同名冲突/CAS 裁决原样生效。updated_by 审计快照列
%% 固定 undefined——平台操作者不是租户 user。平台审计（CP-ASSET-05）：
%% 部门写与 adm_operation_log_ds:insert_tx 在同一事务提交，审计失败
%% 整事务回滚；门禁/校验读保持在事务外（与迁移前行为一致）。
%% ===================================================================

-spec admin_department_create(integer(), integer(), map(), map()) ->
    {ok, map()} | {error, {integer(), binary()}}.
admin_department_create(AdmUserId, OrgId, #{<<"name">> := Name} = Params, AuditCtx) when
    is_integer(OrgId), OrgId > 0
->
    ParentId = normalize_parent(maps:get(<<"parent_id">>, Params, undefined)),
    case org_exists(OrgId) of
        false ->
            {error, {404, <<"Organization 不存在"/utf8>>}};
        true ->
            case organization_department:valid_name(Name) of
                {error, _} ->
                    {error, {400, <<"部门名称非法（1-200 字节，不能为空白）"/utf8>>}};
                ok ->
                    case parent_gate(OrgId, ParentId) of
                        ok ->
                            department_create_tx(
                                AdmUserId, OrgId, ParentId, string:trim(Name), AuditCtx
                            );
                        {error, _} = GateErr ->
                            GateErr
                    end
            end
    end;
admin_department_create(_AdmUserId, _OrgId, _Params, _AuditCtx) ->
    {error, {400, <<"name 必填，organization_id 必须是正整数"/utf8>>}}.

department_create_tx(AdmUserId, OrgId, ParentId, Name, AuditCtx) ->
    Tx =
        fun(Conn) ->
            case
                organization_department_pg:insert_department_tx(
                    Conn, OrgId, ParentId, Name, undefined, fun elib_tsid:generate/0
                )
            of
                {ok, Row} ->
                    %% 平台审计（事务内；CP-ASSET-05）：失败整事务回滚
                    ok = audit_governance_tx(
                        Conn,
                        AdmUserId,
                        <<"department_create">>,
                        OrgId,
                        #{<<"name">> => Name, <<"parent_id">> => ParentId},
                        AuditCtx
                    ),
                    {ok, department_view(Row)};
                {error, name_conflict} ->
                    abort(409, <<"同级同名部门已存在"/utf8>>);
                {error, cycle} ->
                    abort(400, <<"非法的部门父子关系"/utf8>>);
                {error, parent_not_found} ->
                    abort(404, <<"父部门不存在"/utf8>>);
                {error, Reason} ->
                    _ = ?ERROR_LOG([
                        organization_admin_department_failed, create, OrgId, Reason
                    ]),
                    throw({abort_tx, {internal, Reason}})
            end
        end,
    case elib_pg:with_tx(Tx) of
        {ok, View} ->
            ok = ?INFO_LOG([organization_admin_department_created, OrgId, maps:get(id, View)]),
            {ok, View};
        {error, {Code, Msg}} when is_integer(Code), is_binary(Msg) ->
            {error, {Code, Msg}};
        {error, Reason} ->
            _ = ?ERROR_LOG([organization_admin_department_failed, create, OrgId, Reason]),
            {error, {500, <<"操作失败，请稍后重试"/utf8>>}};
        {rollback, Reason} ->
            _ = ?ERROR_LOG([organization_admin_department_failed, create, OrgId, Reason]),
            {error, {500, <<"操作失败，请稍后重试"/utf8>>}}
    end.

parent_gate(_OrgId, null) ->
    ok;
parent_gate(OrgId, ParentId) when is_integer(ParentId) ->
    case organization_department_pg:fetch_department(OrgId, ParentId, none) of
        {error, not_found} ->
            {error, {404, <<"父部门不存在"/utf8>>}};
        {ok, #{status := archived}} ->
            {error, {409, <<"父部门已归档"/utf8>>}};
        {ok, _} ->
            ok;
        {error, Reason} ->
            _ = ?ERROR_LOG([organization_admin_department_parent_gate_failed, OrgId, Reason]),
            {error, {500, <<"操作失败，请稍后重试"/utf8>>}}
    end;
parent_gate(_OrgId, _Invalid) ->
    {error, {400, <<"parent_id 必须是正整数或 null"/utf8>>}}.

normalize_parent(undefined) ->
    null;
normalize_parent(null) ->
    null;
normalize_parent(Bin) when is_binary(Bin) ->
    elib_cnv:safe_to_integer(Bin);
normalize_parent(Other) ->
    Other.

-spec admin_department_rename(integer(), integer(), integer(), map(), map()) ->
    {ok, map()} | {error, {integer(), binary()}}.
admin_department_rename(AdmUserId, OrgId, DeptId, #{<<"name">> := Name} = Params, AuditCtx) when
    is_integer(OrgId), OrgId > 0, is_integer(DeptId), DeptId > 0
->
    Expected = require_expected_version(maps:get(<<"expected_version">>, Params, undefined)),
    case organization_department:valid_name(Name) of
        {error, _} ->
            {error, {400, <<"部门名称非法（1-200 字节，不能为空白）"/utf8>>}};
        ok ->
            case Expected of
                {error, _} = Err ->
                    Err;
                {ok, ExpectedVersion} ->
                    department_rename_tx(
                        AdmUserId,
                        OrgId,
                        DeptId,
                        string:trim(Name),
                        ExpectedVersion,
                        Params,
                        AuditCtx
                    )
            end
    end;
admin_department_rename(_, _, _, _, _) ->
    {error, {400, <<"name 与 expected_version 必填，ID 必须是正整数"/utf8>>}}.

department_rename_tx(AdmUserId, OrgId, DeptId, Name, ExpectedVersion, Params, AuditCtx) ->
    Tx =
        fun(Conn) ->
            %% 门禁（事务内读；与迁移前同口径：404 不存在 / 409 已归档）
            case organization_department_pg:fetch_department_tx(Conn, OrgId, DeptId) of
                {error, not_found} ->
                    abort(404, <<"部门不存在"/utf8>>);
                {ok, #{status := archived}} ->
                    abort(409, <<"部门已归档，禁止修改"/utf8>>);
                {ok, _Dept} ->
                    rename_cas_tx(
                        Conn, AdmUserId, OrgId, DeptId, Name, ExpectedVersion, Params, AuditCtx
                    );
                {error, Reason0} ->
                    _ = ?ERROR_LOG([
                        organization_admin_department_failed, rename, OrgId, DeptId, Reason0
                    ]),
                    throw({abort_tx, {internal, Reason0}})
            end
        end,
    case elib_pg:with_tx(Tx) of
        ok ->
            %% 成功后回读新行出站（与迁移前口径一致：事务外池读已提交数据）
            fresh_department_view(OrgId, DeptId);
        {error, {Code, Msg}} when is_integer(Code), is_binary(Msg) ->
            {error, {Code, Msg}};
        {error, Reason} ->
            _ = ?ERROR_LOG([organization_admin_department_failed, rename, OrgId, DeptId, Reason]),
            {error, {500, <<"操作失败，请稍后重试"/utf8>>}};
        {rollback, Reason} ->
            _ = ?ERROR_LOG([organization_admin_department_failed, rename, OrgId, DeptId, Reason]),
            {error, {500, <<"操作失败，请稍后重试"/utf8>>}}
    end.

rename_cas_tx(Conn, AdmUserId, OrgId, DeptId, Name, ExpectedVersion, Params, AuditCtx) ->
    case
        organization_department_pg:update_name_tx(
            Conn, OrgId, DeptId, Name, undefined, ExpectedVersion
        )
    of
        ok ->
            %% 平台审计（事务内；CP-ASSET-05）：失败整事务回滚（CAS 写一并回滚）
            ok = audit_governance_tx(
                Conn,
                AdmUserId,
                <<"department_rename">>,
                OrgId,
                #{<<"department_id">> => DeptId, <<"request">> => Params},
                AuditCtx
            ),
            ok;
        {error, conflict} ->
            abort(409, <<"部门版本已过期（stale version），请刷新后重试"/utf8>>);
        {error, name_conflict} ->
            abort(409, <<"同级同名部门已存在"/utf8>>);
        {error, Reason} ->
            _ = ?ERROR_LOG([
                organization_admin_department_failed, rename, OrgId, DeptId, Reason
            ]),
            throw({abort_tx, {internal, Reason}})
    end.

-spec admin_department_move(integer(), integer(), integer(), map(), map()) ->
    {ok, map()} | {error, {integer(), binary()}}.
admin_department_move(AdmUserId, OrgId, DeptId, Params, AuditCtx) when
    is_integer(OrgId), OrgId > 0, is_integer(DeptId), DeptId > 0
->
    NewParentId = normalize_parent(maps:get(<<"parent_id">>, Params, undefined)),
    Expected = require_expected_version(maps:get(<<"expected_version">>, Params, undefined)),
    case Expected of
        {error, _} = Err ->
            Err;
        {ok, ExpectedVersion} ->
            case org_exists(OrgId) of
                false ->
                    {error, {404, <<"Organization 不存在"/utf8>>}};
                true ->
                    department_move_tx(
                        AdmUserId, OrgId, DeptId, NewParentId, ExpectedVersion, Params, AuditCtx
                    )
            end
    end;
admin_department_move(_, _, _, _, _) ->
    {error, {400, <<"expected_version 必填，ID 必须是正整数"/utf8>>}}.

department_move_tx(AdmUserId, OrgId, DeptId, NewParentId, ExpectedVersion, Params, AuditCtx) ->
    Tx =
        fun(Conn) ->
            case
                organization_department_pg:move_in_tx(
                    Conn, OrgId, DeptId, NewParentId, undefined, ExpectedVersion
                )
            of
                {ok, Row} ->
                    %% 平台审计（事务内；CP-ASSET-05）：失败整事务回滚
                    ok = audit_governance_tx(
                        Conn,
                        AdmUserId,
                        <<"department_move">>,
                        OrgId,
                        #{<<"department_id">> => DeptId, <<"request">> => Params},
                        AuditCtx
                    ),
                    {ok, department_view(maps:merge(#{status => active}, Row))};
                {error, not_found} ->
                    abort(404, <<"部门不存在"/utf8>>);
                {error, department_archived} ->
                    abort(409, <<"部门已归档，禁止修改"/utf8>>);
                {error, conflict} ->
                    abort(409, <<"部门版本已过期（stale version），请刷新后重试"/utf8>>);
                {error, {new_parent_not_found, _}} ->
                    abort(404, <<"父部门不存在"/utf8>>);
                {error, {new_parent_archived, _}} ->
                    abort(409, <<"父部门已归档"/utf8>>);
                {error, {self_parent, _}} ->
                    abort(400, <<"不能移动到自身之下"/utf8>>);
                {error, {cycle, _, _}} ->
                    abort(400, <<"不能移动到自身子树之下"/utf8>>);
                {error, {invalid_parent_id, _}} ->
                    abort(400, <<"parent_id 必须是正整数或 null"/utf8>>);
                {error, Reason} ->
                    _ = ?ERROR_LOG([
                        organization_admin_department_failed, move, OrgId, DeptId, Reason
                    ]),
                    throw({abort_tx, {internal, Reason}})
            end
        end,
    case elib_pg:with_tx(Tx) of
        {ok, View} ->
            ok = ?INFO_LOG([organization_admin_department_moved, OrgId, DeptId]),
            {ok, View};
        {error, {Code, Msg}} when is_integer(Code), is_binary(Msg) ->
            {error, {Code, Msg}};
        {error, Reason} ->
            _ = ?ERROR_LOG([organization_admin_department_failed, move, OrgId, DeptId, Reason]),
            {error, {500, <<"操作失败，请稍后重试"/utf8>>}};
        {rollback, Reason} ->
            _ = ?ERROR_LOG([organization_admin_department_failed, move, OrgId, DeptId, Reason]),
            {error, {500, <<"操作失败，请稍后重试"/utf8>>}}
    end.

-spec admin_department_archive(integer(), integer(), integer(), map()) ->
    {ok, map()} | {error, {integer(), binary()}}.
admin_department_archive(AdmUserId, OrgId, DeptId, AuditCtx) when
    is_integer(OrgId), OrgId > 0, is_integer(DeptId), DeptId > 0
->
    case org_exists(OrgId) of
        false ->
            {error, {404, <<"Organization 不存在"/utf8>>}};
        true ->
            Tx =
                fun(Conn) ->
                    case
                        organization_department_pg:archive_subtree_in_tx(
                            Conn, OrgId, DeptId, undefined
                        )
                    of
                        {ok, Dept} ->
                            %% 平台审计（事务内；CP-ASSET-05）：失败整事务回滚；
                            %% 幂等命中（archive_idempotent）同样留痕（与迁移前口径一致）
                            ok = audit_governance_tx(
                                Conn,
                                AdmUserId,
                                <<"department_archive">>,
                                OrgId,
                                #{<<"department_id">> => DeptId},
                                AuditCtx
                            ),
                            {ok, Dept};
                        {error, not_found} ->
                            abort(404, <<"部门不存在"/utf8>>);
                        {error, Reason} ->
                            _ = ?ERROR_LOG([
                                organization_admin_department_failed, archive, OrgId, DeptId, Reason
                            ]),
                            throw({abort_tx, {internal, Reason}})
                    end
                end,
            case elib_pg:with_tx(Tx) of
                {ok, Dept} ->
                    Idempotent = maps:get(archive_idempotent, Dept, false),
                    case Idempotent of
                        true ->
                            ok;
                        false ->
                            ok = ?INFO_LOG([
                                organization_admin_department_archived, OrgId, DeptId
                            ])
                    end,
                    %% archive_subtree 返回归档前旧行（status 仍 active）——
                    %% 与租户面 organization_department_app:archive_result 同款：
                    %% 成功后回读新行出站，幂等标记只用于裁决日志
                    fresh_department_view(OrgId, DeptId);
                {error, {Code, Msg}} when is_integer(Code), is_binary(Msg) ->
                    {error, {Code, Msg}};
                {error, Reason} ->
                    _ = ?ERROR_LOG([
                        organization_admin_department_failed, archive, OrgId, DeptId, Reason
                    ]),
                    {error, {500, <<"操作失败，请稍后重试"/utf8>>}};
                {rollback, Reason} ->
                    _ = ?ERROR_LOG([
                        organization_admin_department_failed, archive, OrgId, DeptId, Reason
                    ]),
                    {error, {500, <<"操作失败，请稍后重试"/utf8>>}}
            end
    end;
admin_department_archive(_, _, _, _) ->
    {error, {400, <<"ID 必须是正整数"/utf8>>}}.

require_expected_version(Value) ->
    case elib_cnv:safe_to_integer(Value) of
        V when is_integer(V), V > 0 ->
            {ok, V};
        _ ->
            {error, {400, <<"expected_version 必须是正整数（当前部门 version）"/utf8>>}}
    end.

fresh_department_view(OrgId, DeptId) ->
    case organization_department_pg:fetch_department(OrgId, DeptId, none) of
        {ok, Fresh} ->
            {ok, department_view(Fresh)};
        {error, _} ->
            {ok, #{id => DeptId, organization_id => OrgId}}
    end.

%% department_pg 行键为原子（normalize_row）；统一二进制键出站由 handler 归一化
department_view(Row) when is_map(Row) ->
    Row#{organization_id => maps:get(organization_id, Row, undefined), id => maps:get(id, Row)}.

%% ===================================================================
%% 写：组织邀请码（平台通道，QR 入企码；created_by=NULL 平台口径——
%% adm_user.id 不指向 user 表，镜像 admin_invitation invited_by=NULL 先例）。
%% 租户治理门不做（平台鉴权由 handler 层 adm_acl 承担）；org 状态门禁与
%% app 层同构：仅 active 可建码（pending/rejected/archived 409），撤销
%% 放行任何状态（收紧操作）。一组织至多一个 active 码；重新生成=旧码失效。
%% ===================================================================

-spec admin_invite_code_get(integer()) -> {ok, map()} | {error, {404, binary()} | {500, binary()}}.
admin_invite_code_get(OrgId) when is_integer(OrgId), OrgId > 0 ->
    Tx = fun(Conn) ->
        organization_invite_code_pg:find_active_by_org_tx(Conn, OrgId)
    end,
    case elib_pg:with_tx(Tx) of
        {ok, Row} ->
            {ok, invite_code_view(Row)};
        {error, not_found} ->
            {error, {404, <<"该组织当前没有有效邀请码"/utf8>>}};
        {error, Reason} ->
            _ = ?ERROR_LOG([organization_admin_invite_code_failed, get, OrgId, Reason]),
            {error, {500, <<"读取邀请码失败，请稍后重试"/utf8>>}}
    end;
admin_invite_code_get(_) ->
    {error, {400, <<"organization_id 必须是正整数"/utf8>>}}.

-spec admin_invite_code_create(integer(), integer(), binary(), integer() | undefined, map()) ->
    {ok, map()} | {error, {integer(), binary()}}.
admin_invite_code_create(AdmUserId, OrgId, Role, ExpiresIn, AuditCtx) when
    is_integer(OrgId), OrgId > 0
->
    case organization_invite_code_app:normalize_role(Role) of
        invalid ->
            {error, {400, <<"role 仅支持 admin 或 member"/utf8>>}};
        NormRole ->
            ExpiresAt =
                case is_integer(ExpiresIn) andalso ExpiresIn > 0 of
                    true -> os:system_time(second) + ExpiresIn;
                    false -> os:system_time(second) + organization_invite_code_app:ttl_seconds()
                end,
            insert_code_with_retry(
                AdmUserId, OrgId, NormRole, ExpiresAt, ?INVITE_CODE_RETRY_LIMIT, AuditCtx
            )
    end;
admin_invite_code_create(_, _, _, _, _) ->
    {error, {400, <<"organization_id 必须是正整数"/utf8>>}}.

%% 换码重试（镜像 invite_code_app:insert_code_tx 的收敛策略；差异仅
%% 治理门：平台通道只裁决 org 状态，不做成员角色校验）。
insert_code_with_retry(_AdmUserId, _OrgId, _Role, _ExpiresAt, 0, _AuditCtx) ->
    {error, {500, <<"生成邀请码失败，请稍后重试"/utf8>>}};
insert_code_with_retry(AdmUserId, OrgId, Role, ExpiresAt, Left, AuditCtx) ->
    %% R3-7（与 invite_code_app:insert_code_tx 同口径）：撤旧只在**首次尝试**
    %% 做，重试 insert_only——否则重试窗口内并发建码者刚拿到的码会被静默撤销。
    Mode =
        case Left =:= ?INVITE_CODE_RETRY_LIMIT of
            true -> revoke_first;
            false -> insert_only
        end,
    Code = organization_invite_code_pg:generate_code(),
    Tx = fun(Conn) ->
        case organization_lifecycle_pg:lock_organization_tx(Conn, OrgId) of
            {ok, #{<<"status">> := <<"active">>}} ->
                ok;
            {ok, #{<<"status">> := <<"pending">>}} ->
                abort(409, <<"Organization 待审核，审核通过后才能生成邀请码"/utf8>>);
            {ok, #{<<"status">> := <<"rejected">>}} ->
                abort(409, <<"Organization 未通过审核，不能生成邀请码"/utf8>>);
            {ok, _Archived} ->
                abort(409, <<"Organization 已归档，不能生成邀请码"/utf8>>);
            {error, not_found} ->
                abort(404, <<"Organization 不存在"/utf8>>);
            {error, Reason1} ->
                throw({abort_tx, {internal, Reason1}})
        end,
        case Mode of
            revoke_first ->
                case organization_invite_code_pg:revoke_active_by_org_tx(Conn, OrgId) of
                    {ok, _} -> ok;
                    {error, Reason2} -> throw({abort_tx, {internal, {revoke_old_code, Reason2}}})
                end;
            insert_only ->
                ok
        end,
        case organization_invite_code_pg:add_tx(Conn, OrgId, Code, null, ExpiresAt, Role) of
            {ok, Row} ->
                ok = audit_governance_tx(
                    Conn,
                    AdmUserId,
                    <<"invite_code_create">>,
                    OrgId,
                    #{<<"role">> => Role},
                    AuditCtx
                ),
                {ok, Row};
            {error, code_conflict} ->
                {error, code_conflict};
            {error, Reason3} ->
                throw({abort_tx, {internal, Reason3}})
        end
    end,
    case elib_pg:with_tx(Tx) of
        {ok, Row} ->
            _ = ?INFO_LOG([organization_admin_invite_code_created, OrgId, Role]),
            {ok, invite_code_view(Row)};
        {error, code_conflict} ->
            insert_code_with_retry(AdmUserId, OrgId, Role, ExpiresAt, Left - 1, AuditCtx);
        {error, {Code, Msg}} when is_integer(Code), is_binary(Msg) ->
            {error, {Code, Msg}};
        {error, Reason} ->
            _ = ?ERROR_LOG([organization_admin_invite_code_failed, create, OrgId, Reason]),
            {error, {500, <<"生成邀请码失败，请稍后重试"/utf8>>}}
    end.

-spec admin_invite_code_revoke(integer(), integer(), map()) ->
    {ok, map()} | {error, {integer(), binary()}}.
admin_invite_code_revoke(AdmUserId, OrgId, AuditCtx) when is_integer(OrgId), OrgId > 0 ->
    Tx = fun(Conn) ->
        case organization_lifecycle_pg:lock_organization_tx(Conn, OrgId) of
            {ok, _AnyStatus} ->
                ok;
            {error, not_found} ->
                abort(404, <<"Organization 不存在"/utf8>>);
            {error, Reason1} ->
                throw({abort_tx, {internal, Reason1}})
        end,
        Res = organization_invite_code_pg:revoke_active_by_org_tx(Conn, OrgId),
        ok = audit_governance_tx(Conn, AdmUserId, <<"invite_code_revoke">>, OrgId, #{}, AuditCtx),
        Res
    end,
    case elib_pg:with_tx(Tx) of
        {ok, Count} ->
            _ = ?INFO_LOG([organization_admin_invite_code_revoked, OrgId, Count]),
            {ok, #{organization_id => OrgId, revoked => Count}};
        {error, {Code, Msg}} when is_integer(Code), is_binary(Msg) ->
            {error, {Code, Msg}};
        {error, Reason} ->
            _ = ?ERROR_LOG([organization_admin_invite_code_failed, revoke, OrgId, Reason]),
            {error, {500, <<"撤销邀请码失败，请稍后重试"/utf8>>}}
    end;
admin_invite_code_revoke(_, _, _) ->
    {error, {400, <<"organization_id 必须是正整数"/utf8>>}}.

invite_code_view(Row) ->
    #{
        organization_id => maps:get(<<"organization_id">>, Row, undefined),
        code => maps:get(<<"code">>, Row, undefined),
        role => safe_role(maps:get(<<"role">>, Row, <<"member">>)),
        status => maps:get(<<"status">>, Row, undefined),
        expires_at => maps:get(<<"expires_at">>, Row, undefined),
        created_at => maps:get(<<"created_at">>, Row, undefined)
    }.

safe_role(<<"admin">>) -> <<"admin">>;
safe_role(_) -> <<"member">>.

%% ===================================================================
%% 写：注册审核 approve / reject（00000155；CAS 仅 pending 行生效——
%% 并发审核/重复点击幂等收敛，0 行归一 409「非待审核状态」）。
%% reject 的 reason 仅入审计 detail，不写组织行（状态机单真源）。
%% ===================================================================

-spec admin_review(integer(), integer(), approve | reject, map()) ->
    {ok, map()} | {error, {integer(), binary()}}.
admin_review(AdmUserId, OrgId, Action, AuditCtx) when
    is_integer(OrgId), OrgId > 0, Action =:= approve; Action =:= reject
->
    TargetStatus =
        case Action of
            approve -> <<"active">>;
            reject -> <<"rejected">>
        end,
    Tx = fun(Conn) ->
        case organization_lifecycle_pg:lock_organization_tx(Conn, OrgId) of
            {ok, #{<<"status">> := <<"pending">>}} ->
                ok;
            {ok, #{<<"status">> := <<"active">>}} ->
                abort(409, <<"Organization 已是审核通过状态"/utf8>>);
            {ok, #{<<"status">> := <<"rejected">>}} ->
                abort(409, <<"Organization 已被驳回（终态）"/utf8>>);
            {ok, #{<<"status">> := <<"archived">>}} ->
                abort(409, <<"Organization 已归档，无待审核事项"/utf8>>);
            {error, not_found} ->
                abort(404, <<"Organization 不存在"/utf8>>);
            {error, Reason1} ->
                throw({abort_tx, {internal, Reason1}})
        end,
        case organization_lifecycle_pg:set_review_status_tx(Conn, OrgId, TargetStatus) of
            {ok, 1} ->
                ok = audit_governance_tx(
                    Conn,
                    AdmUserId,
                    case Action of
                        approve -> <<"approve">>;
                        reject -> <<"reject">>
                    end,
                    OrgId,
                    review_detail(Action, AuditCtx),
                    AuditCtx
                ),
                {ok, changed};
            {ok, 0} ->
                %% 锁行后仍 0 行：理论不可达（防御），按并发已处理归一
                abort(409, <<"Organization 不在待审核状态"/utf8>>);
            {error, Reason2} ->
                throw({abort_tx, {internal, Reason2}})
        end
    end,
    case elib_pg:with_tx(Tx) of
        {ok, changed} ->
            _ = ?INFO_LOG([organization_admin_reviewed, Action, OrgId]),
            {ok, #{organization_id => OrgId, status => TargetStatus, changed => true}};
        {error, {Code, Msg}} when is_integer(Code), is_binary(Msg) ->
            {error, {Code, Msg}};
        {error, Reason} ->
            _ = ?ERROR_LOG([organization_admin_review_failed, Action, OrgId, Reason]),
            {error, {500, <<"审核操作失败，请稍后重试"/utf8>>}};
        {rollback, Reason} ->
            _ = ?ERROR_LOG([organization_admin_review_failed, Action, OrgId, Reason]),
            {error, {500, <<"审核操作失败，请稍后重试"/utf8>>}}
    end;
admin_review(_, _, _, _) ->
    {error, {400, <<"审核参数无效"/utf8>>}}.

%% reject 可带 reason（handler 从 body 提取放 AuditCtx.reason）；approve 无 extra。
review_detail(reject, AuditCtx) ->
    case maps:get(reason, AuditCtx, undefined) of
        Reason when is_binary(Reason), byte_size(Reason) > 0 ->
            #{<<"reason">> => Reason};
        _ ->
            #{}
    end;
review_detail(approve, _AuditCtx) ->
    #{}.

%% ===================================================================
%% 内部
%% ===================================================================

%% 平台治理审计（事务内；CP-ASSET-05）：与业务写共用同一 Conn 提交。
%% 审计能静默丢失等于「治理写做了却没人知道是谁做的」，属治理链路完整性要求，
%% 故这里不做任何错误吞没（镜像 EADM-01 audit_create_tx 范式）：
%% 写入失败 throw({abort_tx, {audit_failed, Reason}}) ⇒ elib_pg 回滚整个事务，
%% 业务写一并回滚（验收 CP-ASSET-A10）。
%%
%% action 传短名（如 <<"archive">>），DB action 列写 <<"organization_archive">>、
%% detail 基础键与迁移前 handler 弱审计完全一致（organization_id + action + Extra）。
-spec audit_governance_tx(term(), integer(), binary(), integer(), map(), map()) -> ok.
audit_governance_tx(Conn, AdmUserId, Action, OrgId, Extra, AuditCtx) ->
    Detail = maps:merge(
        #{
            <<"organization_id">> => OrgId,
            <<"action">> => Action
        },
        Extra
    ),
    case
        adm_operation_log_ds:insert_tx(
            Conn,
            AdmUserId,
            <<"organization_", Action/binary>>,
            OrgId,
            <<"organization">>,
            Detail,
            maps:get(ip, AuditCtx, undefined)
        )
    of
        ok -> ok;
        {error, Reason} -> throw({abort_tx, {audit_failed, Reason}})
    end.

-spec org_exists(integer()) -> boolean().
org_exists(OrgId) ->
    case elib_pg:one(<<"SELECT id FROM organization WHERE id = $1">>, [OrgId]) of
        {ok, #{<<"id">> := _}} ->
            true;
        _ ->
            false
    end.

-spec abort(integer(), binary()) -> no_return().
abort(Code, Msg) ->
    throw({abort_tx, {Code, Msg}}).
