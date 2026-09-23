-module(enterprise_group_repo).

%%%
% enterprise_group_repo 是 EPGZ-03（INT-04/05/06）Workspace 企业群的 tx 仓储层。
%
% 设计事实（EPGZ-03 选型，详见 checkpoint）：
%   * 企业群复用现有 "group".scope='workspace' + workspace_id（migration 77），
%     不建新表、不加标记列；OA 可管理的是本 Org Workspace 内
%     scope='workspace' 且 status=1 的群（manifest grant：
%     "workspace scoped + group in workspace"）。
%   * 全部函数是事务内（Conn 直连）形态，配合 A2 幂等 begin/complete 模式
%     与真库 marker 测试；现网 group_repo/group_ds 的池化函数无法进入
%     本层事务，故此处镜像最小 SQL（均在注释中标明镜像来源）。
%   * group_member ⊆ workspace_member（trg_group_member_ws_subset，DEFERRABLE）
%     是 DB 第二道兜底；本层 activate 前由 logic 做应用层同事务校验
%     （镜像 group_member_ds:ensure_workspace_membership 的读法）。
%%%

-export([
    next_group_id/0,
    create_workspace_group_tx/7,
    activate_member_tx/5,
    deactivate_member_tx/4,
    refresh_member_count_tx/2,
    find_group_in_org_tx/3,
    find_group_in_org_any_tx/3,
    find_workspace_in_org_tx/3,
    non_ws_member_uids_tx/3,
    active_member_uids_tx/2,
    count_active_owners_tx/2,
    group_member_status_tx/3,
    update_group_meta_tx/4,
    set_group_status_tx/3,
    set_member_role_tx/4,
    active_member_rows_tx/2,
    active_member_rows_in_ws_tx/3,
    internal_origin_page_tx/5,
    internal_member_page_tx/4
]).

-include_lib("epgsql/include/epgsql.hrl").

-define(GROUP_COLUMNS,
    <<"id, type, join_limit, owner_uid, creator_uid, member_max, member_count, ",
        "introduction, avatar, title, status, scope, workspace_id, created_at, updated_at">>
).

%% 详情面成员行硬上限（FULL-02）：超过即拒，不静默截断（无全量导出形态）。
-define(DETAIL_MEMBER_LIMIT, 200).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc group TSID（group_info 命名空间；惰性注册，镜像 A1 identity repo 口径）。
-spec next_group_id() -> pos_integer().
next_group_id() ->
    ensure_tsid(group_info),
    elib_tsid:generate(group_info).

%% @doc 事务内创建 Workspace 企业群行（仅 "group" 行，不含成员）。
%% 镜像 group_ds:create_scoped_group/7 的 GMap 列语义：
%%   type=2（私有）、join_limit=3（只允许邀请加入）、status=1、
%%   scope='workspace' + workspace_id（XOR CHECK 由 DB 强制）；
%% member_count 初始 0，随后 activate_member_tx(owner) 刷新为 1。
%% creator/owner 均为指定 mapped Human（企业群由 Human 拥有，OA 不占位）。
-spec create_workspace_group_tx(
    any(), pos_integer(), pos_integer(), binary(), binary(), pos_integer(), binary()
) ->
    {ok, map()} | {error, term()}.
create_workspace_group_tx(Conn, Gid, OwnerUid, Title, Introduction, WorkspaceId, Now) when
    is_integer(Gid),
    Gid > 0,
    is_integer(OwnerUid),
    OwnerUid > 0,
    is_binary(Title),
    is_integer(WorkspaceId),
    WorkspaceId > 0
->
    Sql =
        <<"INSERT INTO \"group\" (id, type, join_limit, owner_uid, creator_uid, ",
            "introduction, title, status, member_count, scope, workspace_id, created_at, updated_at) ",
            "VALUES ($1, 2, 3, $2, $2, $3, $4, 1, 0, 'workspace', $5, $6, $6) ", "RETURNING ",
            ?GROUP_COLUMNS/binary>>,
    case elib_pg:query(Conn, Sql, [Gid, OwnerUid, Introduction, Title, WorkspaceId, Now]) of
        {ok, [Row | _]} ->
            {ok, Row};
        {ok, []} ->
            {error, insert_empty_result};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 事务内幂等激活成员（新建或复活 status<>1 行，role 固定写入）。
%% 复用 group_member_repo:upsert_active/5（导出口），新激活时补齐两件
%% group_member_ds:join_group 的私有后续（均未导出，此处镜像并注明）：
%%   1. open_history_generation（E2EE-2026-012 世代开启，ON CONFLICT 幂等）；
%%   2. refresh_member_count_tx（member_count 刷新）。
%% 返回 {ok, NewActivation :: boolean()}（false = 已是 active，幂等无变化）。
%% workspace membership 前置校验由 logic 层完成（应用层先拒 + DB 触发器兜底）。
-spec activate_member_tx(any(), pos_integer(), pos_integer(), integer(), binary()) ->
    {ok, boolean()} | {error, term()}.
activate_member_tx(Conn, Gid, Uid, Role, JoinMode) when
    is_integer(Gid), Gid > 0, is_integer(Uid), Uid > 0, is_integer(Role)
->
    case group_member_repo:upsert_active(Conn, Gid, Uid, Role, JoinMode) of
        {ok, false} ->
            {ok, false};
        {ok, true} ->
            ok = open_history_generation_tx(Conn, Gid, Uid),
            {ok, _} = refresh_member_count_tx(Conn, Gid),
            {ok, true};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 事务内幂等停用成员（status 1 -> 0，行保留供审计/复活）。
%% 镜像 workspace_logic:remove_member_tx 的群成员下级处理：
%%   status=0 + close_history_generation（导出复用）+ member_count 刷新。
%% 已非 active（重复移除）返回 {ok, false}，不重复关世代。
-spec deactivate_member_tx(any(), pos_integer(), pos_integer(), binary()) ->
    {ok, boolean()} | {error, term()}.
deactivate_member_tx(Conn, Gid, Uid, Reason) when
    is_integer(Gid), Gid > 0, is_integer(Uid), Uid > 0
->
    Sql =
        <<"UPDATE group_member SET status = 0, updated_at = $1 ",
            "WHERE group_id = $2 AND user_id = $3 AND status = 1 RETURNING id">>,
    case elib_pg:query(Conn, Sql, [elib_dt:now(), Gid, Uid]) of
        {ok, [_ | _]} ->
            ok = group_member_ds:close_history_generation(Conn, Gid, Uid, Reason),
            {ok, _} = refresh_member_count_tx(Conn, Gid),
            {ok, true};
        {ok, []} ->
            {ok, false};
        {error, Reason2} ->
            {error, Reason2}
    end.

%% @doc 事务内刷新 member_count（本仓 enterprise 口径：active(status=1) 成员数；
%% 与 group_member_ds:update_statistics 的 status > -1 口径的差异见 checkpoint
%% ——企业群停用行不应计数，personal 群既有路径不受影响）。
-spec refresh_member_count_tx(any(), pos_integer()) -> {ok, non_neg_integer()} | {error, term()}.
refresh_member_count_tx(Conn, Gid) when is_integer(Gid), Gid > 0 ->
    SqlCount =
        <<"SELECT COUNT(*) AS member_count FROM group_member ",
            "WHERE group_id = $1 AND status = 1">>,
    case elib_pg:query(Conn, SqlCount, [Gid]) of
        {ok, [#{<<"member_count">> := Count}]} ->
            Update =
                <<"UPDATE \"group\" SET member_count = $2, updated_at = $3 WHERE id = $1">>,
            case elib_pg:execute(Conn, Update, [Gid, Count, elib_dt:now()]) of
                {ok, _} ->
                    {ok, Count};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason2} ->
            {error, Reason2}
    end.

%% @doc 事务内 Org 边界群定位：群存在、scope='workspace'、status=1、
%% 且其 workspace 属于指定 Org。任一不满足 → {error, not_found}
%% （跨 Org / 个人群 / 已删群统一不泄露存在性，logic 转 resource_not_found）。
-spec find_group_in_org_tx(any(), integer(), integer()) ->
    {ok, map()} | {error, not_found | term()}.
find_group_in_org_tx(Conn, OrgId, GroupId) when
    is_integer(OrgId), is_integer(GroupId), GroupId > 0
->
    Sql =
        <<"SELECT g.id, g.title, g.owner_uid, g.member_count, g.scope, ",
            "g.workspace_id, g.status, w.id AS ws_id, w.status AS ws_status ",
            "FROM \"group\" g JOIN workspace w ON w.id = g.workspace_id ",
            "WHERE g.id = $1 AND g.scope = 'workspace' AND g.status = 1 ",
            "AND w.organization_id = $2 LIMIT 1">>,
    case elib_pg:query(Conn, Sql, [GroupId, OrgId]) of
        {ok, [Row | _]} ->
            {ok, Row};
        {ok, []} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 事务内 Org 边界 Workspace 定位（organization_id 匹配）。
%% 不存在 / 未挂 Org（历史 Workspace）/ 跨 Org → {error, not_found}。
-spec find_workspace_in_org_tx(any(), integer(), integer()) ->
    {ok, map()} | {error, not_found | term()}.
find_workspace_in_org_tx(Conn, OrgId, WsId) when
    is_integer(OrgId), is_integer(WsId), WsId > 0
->
    Sql =
        <<"SELECT id, name, status, organization_id FROM workspace ",
            "WHERE id = $1 AND organization_id = $2 LIMIT 1">>,
    case elib_pg:query(Conn, Sql, [WsId, OrgId]) of
        {ok, [Row | _]} ->
            {ok, Row};
        {ok, []} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 事务内批量筛出「非该 Workspace active 成员」的 uid（应用层
%% Group Member ⊆ Workspace Member 预检；镜像
%% group_member_ds:ensure_workspace_membership 的读法，批量化）。
-spec non_ws_member_uids_tx(any(), pos_integer(), [integer()]) ->
    {ok, [integer()]} | {error, term()}.
non_ws_member_uids_tx(_Conn, _WsId, []) ->
    {ok, []};
non_ws_member_uids_tx(Conn, WsId, Uids) when is_list(Uids) ->
    Placeholders = placeholders(Uids, 2, <<>>),
    Params = [WsId | Uids],
    Sql =
        <<"SELECT user_id FROM workspace_member ", "WHERE workspace_id = $1 AND status = 'active' ",
            "AND user_id IN (", Placeholders/binary, ")">>,
    case elib_pg:query(Conn, Sql, Params) of
        {ok, Rows} ->
            Members = [Uid || #{<<"user_id">> := Uid} <- Rows],
            {ok, [U || U <- Uids, not lists:member(U, Members)]};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 事务内取群 active(status=1) 成员 uid 列表。
-spec active_member_uids_tx(any(), pos_integer()) -> {ok, [integer()]} | {error, term()}.
active_member_uids_tx(Conn, Gid) when is_integer(Gid), Gid > 0 ->
    Sql =
        <<"SELECT user_id FROM group_member WHERE group_id = $1 AND status = 1">>,
    case elib_pg:query(Conn, Sql, [Gid]) of
        {ok, Rows} ->
            {ok, [Uid || #{<<"user_id">> := Uid} <- Rows]};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 事务内统计群 active 群主（role=4 且 status=1）数量（owner invariant）。
-spec count_active_owners_tx(any(), pos_integer()) -> {ok, non_neg_integer()} | {error, term()}.
count_active_owners_tx(Conn, Gid) when is_integer(Gid), Gid > 0 ->
    Sql =
        <<"SELECT COUNT(*) AS owners FROM group_member ",
            "WHERE group_id = $1 AND role = 4 AND status = 1">>,
    case elib_pg:query(Conn, Sql, [Gid]) of
        {ok, [#{<<"owners">> := Count}]} ->
            {ok, Count};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 事务内按 group_id 读单个成员行状态（owner invariant 判定用）。
%% 返回 not_found / status smallint。
-spec group_member_status_tx(any(), pos_integer(), pos_integer()) ->
    {ok, map()} | {error, not_found | term()}.
group_member_status_tx(Conn, Gid, Uid) when
    is_integer(Gid), Gid > 0, is_integer(Uid), Uid > 0
->
    Sql =
        <<"SELECT id, group_id, user_id, role, status FROM group_member ",
            "WHERE group_id = $1 AND user_id = $2 LIMIT 1">>,
    case elib_pg:query(Conn, Sql, [Gid, Uid]) of
        {ok, [Row | _]} ->
            {ok, Row};
        {ok, []} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

%%%===================================================================
%%% FULL-02 生命周期（群详情 / 更新 / 归档 / 成员角色）
%%%===================================================================

%% @doc 事务内 Org 边界群定位（**不过滤 group.status**，FULL-02 归档幂等判定用）：
%% 群存在、scope='workspace'，且其 workspace 属于指定 Org。
%% 跨 Org / 个人群 / 不存在的群 → {error, not_found}（同 find_group_in_org_tx/3）。
-spec find_group_in_org_any_tx(any(), integer(), integer()) ->
    {ok, map()} | {error, not_found | term()}.
find_group_in_org_any_tx(Conn, OrgId, GroupId) when
    is_integer(OrgId), is_integer(GroupId), GroupId > 0
->
    Sql =
        <<"SELECT g.id, g.title, g.introduction, g.owner_uid, g.member_count, g.scope, ",
            "g.workspace_id, g.status, w.id AS ws_id, w.status AS ws_status ",
            "FROM \"group\" g JOIN workspace w ON w.id = g.workspace_id ",
            "WHERE g.id = $1 AND g.scope = 'workspace' ", "AND w.organization_id = $2 LIMIT 1">>,
    case elib_pg:query(Conn, Sql, [GroupId, OrgId]) of
        {ok, [Row | _]} ->
            {ok, Row};
        {ok, []} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 事务内更新群标题/简介（成员数等统计列不在此路径）。
%% 只影响 status=1 的群行；0 行更新归一 {error, not_found}（调用方已先行边界判定，
%% 此处是「群在判定与更新之间被归档」的终局兜底）。
-spec update_group_meta_tx(any(), pos_integer(), binary(), binary()) ->
    ok | {error, not_found | term()}.
update_group_meta_tx(Conn, Gid, Title, Introduction) when
    is_integer(Gid), Gid > 0, is_binary(Title), is_binary(Introduction)
->
    Sql =
        <<"UPDATE \"group\" SET title = $2, introduction = $3, updated_at = $4 ",
            "WHERE id = $1 AND status = 1">>,
    case elib_pg:execute(Conn, Sql, [Gid, Title, Introduction, elib_dt:now()]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 事务内设置群状态（1 启用 / 0 禁用=归档；-1 删除语义不开放给 OA）。
-spec set_group_status_tx(any(), pos_integer(), integer()) ->
    ok | {error, not_found | term()}.
set_group_status_tx(Conn, Gid, Status) when
    is_integer(Gid), Gid > 0, (Status =:= 0 orelse Status =:= 1)
->
    Sql =
        <<"UPDATE \"group\" SET status = $2, updated_at = $3 ", "WHERE id = $1 AND status <> -1">>,
    case elib_pg:execute(Conn, Sql, [Gid, Status, elib_dt:now()]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 事务内改成员角色（仅 status=1 的成员行）。返回 {ok, OldRole}——
%% **更新前**的角色（幂等判定用：NewRole =/= OldRole 才算变更）。
%% 先读旧值（group_member_status_tx）再更新（PG 的 RETURNING 反映的是更新后的
%% 行，不能当旧值用；W4 实测把 NEW 当 OLD 会恒判「无变化」）。
-spec set_member_role_tx(any(), pos_integer(), pos_integer(), integer()) ->
    {ok, integer()} | {error, not_found | term()}.
set_member_role_tx(Conn, Gid, Uid, Role) when
    is_integer(Gid), Gid > 0, is_integer(Uid), Uid > 0, is_integer(Role)
->
    case group_member_status_tx(Conn, Gid, Uid) of
        {ok, #{<<"status">> := 1, <<"role">> := OldRole}} ->
            Sql =
                <<"UPDATE group_member SET role = $3, updated_at = $4 ",
                    "WHERE group_id = $1 AND user_id = $2 AND status = 1">>,
            case elib_pg:execute(Conn, Sql, [Gid, Uid, Role, elib_dt:now()]) of
                {ok, 1} -> {ok, OldRole};
                {ok, 0} -> {error, not_found};
                {error, Reason} -> {error, Reason}
            end;
        {ok, _NotActive} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 事务内列群 active 成员行（详情面）：最小字段（user_id / role / status）
%% + 上限（详情面永不全量返回成员表：超过上限即 {error, too_many_members}，
%% 调用方转 invalid_request，绝不静默截断）。
-spec active_member_rows_tx(any(), pos_integer()) ->
    {ok, [map()]} | {error, too_many_members | term()}.
active_member_rows_tx(Conn, Gid) when is_integer(Gid), Gid > 0 ->
    select_member_rows(
        Conn,
        ?DETAIL_MEMBER_LIMIT,
        <<"SELECT user_id, role, status FROM group_member ",
            "WHERE group_id = $1 AND status = 1 ORDER BY user_id LIMIT $2">>,
        [Gid]
    ).

%% @doc 同 active_member_rows_tx/2，但限定为该 Workspace 的 active 成员
%% （详情面的 workspace 过滤读面）。
-spec active_member_rows_in_ws_tx(any(), pos_integer(), pos_integer()) ->
    {ok, [map()]} | {error, too_many_members | term()}.
active_member_rows_in_ws_tx(Conn, Gid, WsId) when
    is_integer(Gid), Gid > 0, is_integer(WsId), WsId > 0
->
    select_member_rows(
        Conn,
        ?DETAIL_MEMBER_LIMIT,
        <<"SELECT gm.user_id, gm.role, gm.status FROM group_member gm ",
            "JOIN workspace_member wm ON wm.workspace_id = $2 ",
            "AND wm.user_id = gm.user_id AND wm.status = 'active' ",
            "WHERE gm.group_id = $1 AND gm.status = 1 ORDER BY gm.user_id LIMIT $3">>,
        [Gid, WsId]
    ).

%% ===================================================================
%% Internal Functions
%% ===================================================================

%% @doc 成员行读取骨架：按 MaxRows+1 取行，真取到 MaxRows+1 行即
%% {error, too_many_members}（绝不静默截断——详情面返回被截断的成员列表会让
%% 调用方误判群规模）。SQL 的最后占位符恒为 LIMIT。
-spec select_member_rows(any(), pos_integer(), binary(), list()) ->
    {ok, [map()]} | {error, too_many_members | term()}.
select_member_rows(Conn, MaxRows, Sql, Params) ->
    case elib_pg:query(Conn, Sql, Params ++ [MaxRows + 1]) of
        {ok, Rows} when length(Rows) > MaxRows ->
            {error, too_many_members};
        {ok, Rows} ->
            {ok, Rows};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 镜像 group_member_ds:open_history_generation/3（私有未导出）：
%% 新激活成员开启 E2EE 历史世代（ON CONFLICT 幂等，锁内计数器下一值）。
-spec open_history_generation_tx(any(), pos_integer(), pos_integer()) -> ok.
open_history_generation_tx(Conn, Gid, Uid) ->
    ConvKey = <<"c2g:", (integer_to_binary(Gid))/binary>>,
    Sql =
        <<"WITH lock_row AS (",
            "  INSERT INTO public.msg_store_seq (conv_key, seq) VALUES ($1, 0) ",
            "  ON CONFLICT (conv_key) DO UPDATE SET seq = public.msg_store_seq.seq ",
            "  RETURNING seq", "), gen AS (",
            "  SELECT COALESCE(MAX(generation_no), 0) + 1 AS next_no ",
            "  FROM public.group_member_generation WHERE group_id = $2 AND user_id = $3", ") ",
            "INSERT INTO public.group_member_generation ",
            "  (group_id, user_id, generation_no, start_seq) ",
            "SELECT $2, $3, gen.next_no, lock_row.seq + 1 FROM lock_row, gen ",
            "ON CONFLICT DO NOTHING">>,
    case elib_pg:execute(Conn, Sql, [ConvKey, Gid, Uid]) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            erlang:error({generation_open_failed, Reason})
    end.

%% @doc TSID 惰性注册（镜像 enterprise_external_identity_repo:next_id 口径）。
-spec ensure_tsid(atom()) -> ok.
ensure_tsid(Name) ->
    case lists:member(Name, elib_tsid:registered()) of
        true ->
            ok;
        false ->
            elib_tsid:register(Name)
    end.

%% @doc 生成 $2,$3,... 逐项占位符（$1 是 workspace_id）。
-spec placeholders([integer()], pos_integer(), binary()) -> binary().
placeholders([], _N, Acc) ->
    Acc;
placeholders([_ | Rest], N, <<>>) ->
    placeholders(Rest, N + 1, <<"$", (integer_to_binary(N))/binary>>);
placeholders([_ | Rest], N, Acc) when is_binary(Acc) ->
    Next = <<Acc/binary, ", $", (integer_to_binary(N))/binary>>,
    placeholders(Rest, N + 1, Next).

%% ===================================================================
%% V2.1 Internal 只读面（INT-26/27 adapter）
%% ===================================================================

%% @doc INT-26 keyset 列表：本 Application origin 建立（enterprise_group_origin
%% active）的企业群，active workspace 内 scope=workspace 群，行集收窄为
%% Grant 覆盖 W（kind='none' 覆盖 Org 全域或显式命中该 W；覆盖谓词在 SQL 内，
%% kind=list 的行级收窄义务，禁止整表回读后内存过滤）。
%% 排序 created_at DESC, id DESC；Pivot 为续页 keyset 元组。
-spec internal_origin_page_tx(
    any(), integer(), integer(), undefined | {binary(), integer()}, pos_integer()
) -> {ok, [map()]} | {error, term()}.
internal_origin_page_tx(Conn, OrgId, AppId, Pivot, Limit) when
    is_integer(OrgId), is_integer(AppId), is_integer(Limit), Limit > 0
->
    Scope = <<"groups:read">>,
    GrantView = enterprise_application_grant_repo:effective_view(),
    ScopeTb = enterprise_application_grant_repo:scope_tablename(),
    WsTb = enterprise_application_grant_repo:workspace_tablename(),
    OriginTb = enterprise_group_origin_repo:tablename(),
    CoveredExists =
        <<
            " AND EXISTS (SELECT 1 FROM ",
            GrantView/binary,
            " g",
            " JOIN ",
            ScopeTb/binary,
            " s ON s.grant_id = g.grant_id",
            " LEFT JOIN ",
            WsTb/binary,
            " gw ON gw.grant_id = g.grant_id",
            "   AND gw.workspace_id = g2.workspace_id",
            " WHERE g.organization_id = $1 AND g.application_id = $2",
            "   AND s.scope = $3",
            "   AND (g.workspace_scope_kind = 'none' OR gw.grant_id IS NOT NULL))"
        >>,
    {KeysetClause, Params0} =
        case Pivot of
            undefined ->
                {<<>>, []};
            {CreatedAt, Id} ->
                {<<" AND (g2.created_at, g2.id) < ($4, $5)">>, [CreatedAt, Id]}
        end,
    Sql =
        <<"SELECT g2.id, g2.workspace_id, g2.title, g2.member_count, g2.created_at",
            " FROM \"group\" g2", " JOIN ", OriginTb/binary, " o ON o.group_id = g2.id",
            "   AND o.organization_id = $1 AND o.application_id = $2", "   AND o.status = 'active'",
            " JOIN workspace w ON w.id = g2.workspace_id",
            "   AND w.organization_id = $1 AND w.status = 'active'",
            " WHERE g2.scope = 'workspace' AND g2.status = 1", CoveredExists/binary,
            KeysetClause/binary, " ORDER BY g2.created_at DESC, g2.id DESC", " LIMIT $",
            (integer_to_binary(4 + length(Params0)))/binary>>,
    case elib_pg:query(Conn, Sql, [OrgId, AppId, Scope] ++ Params0 ++ [Limit]) of
        {ok, Rows} -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.

%% @doc INT-27 群成员 keyset 列表：active（status=1）成员。
%% 排序 created_at ASC, id ASC（§10.2 唯一升序 family；tie-breaker 是
%% group_member.id，不在投影内，仅供 keyset 谓词）。
%% 群定位与 Grant 覆盖判定由 handler 经 locate + boundary enforce（INT-27，
%% workspace kind，W=群所属 W）承担。
-spec internal_member_page_tx(
    any(), pos_integer(), undefined | {binary(), integer()}, pos_integer()
) -> {ok, [map()]} | {error, term()}.
internal_member_page_tx(Conn, Gid, Pivot, Limit) when
    is_integer(Gid), Gid > 0, is_integer(Limit), Limit > 0
->
    {KeysetClause, Params0} =
        case Pivot of
            undefined ->
                {<<>>, []};
            {CreatedAt, Id} ->
                {<<" AND (m.created_at, m.id) > ($2, $3)">>, [CreatedAt, Id]}
        end,
    Sql =
        <<"SELECT m.id, m.user_id, m.role, m.created_at FROM group_member m",
            " WHERE m.group_id = $1 AND m.status = 1", KeysetClause/binary,
            " ORDER BY m.created_at ASC, m.id ASC", " LIMIT $",
            (integer_to_binary(2 + length(Params0)))/binary>>,
    case elib_pg:query(Conn, Sql, [Gid] ++ Params0 ++ [Limit]) of
        {ok, Rows} -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.
