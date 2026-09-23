%%% @doc Human Organization Directory 数据访问（infrastructure，仅 SQL）。
%%%
%%% 计划 §14.2（P1 CORE 类企业微信浏览）的 4 个只读端点背后的全部查询：
%%%   * gate/2            —— 一次 LEFT JOIN 同时取 org 生命周期与 caller 成员态
%%%   * list_children_departments/4 —— 直接 active 子部门 + member_count（单 SQL）
%%%   * list_department_humans/4    —— 某部门本级 active Human（user_id ASC keyset）
%%%   * list_root_humans/4          —— 未挂任何 active 部门的根成员（同 keyset）
%%%   * batch_user_departments/2    —— 一批 user 的全部 active 部门 id（禁 N+1）
%%%   * list_my_departments/2       —— /me：当前用户 active 部门（含 member_count）
%%%   * search_directory/5          —— 部门名/昵称/账号三列 UNION 检索（keyset）
%%%
%%% 铁律：
%%%   * 全部查询以 organization_id 收敛租户边界（跨 Org 行不泄漏）。
%%%   * search 只触碰 organization_department.name、user.nickname、user.account
%%%     （U-02：不搜手机号/邮箱/职位；语句级白名单，无其他用户列出现）。
%%%   * LIKE 模式串先做 ESCAPE 转义，%/_ 仅作字面量。
%%%   * member_count 与 members 列表同口径：仅计 organization_member.status='active'。
%%%   * 排序列（id / user_id）均为 PK 组成列 NOT NULL；若异常行返回 NULL 由
%%%     application 层记 data-integrity 失败日志并丢弃该行（§10.1 null handling）。
-module(organization_directory_pg).

-export([
    gate/2,
    list_children_departments/4,
    list_department_humans/4,
    list_root_humans/3,
    batch_user_departments/2,
    list_my_departments/2,
    search_directory/5
]).

%% 与 members 列表/计数同口径的 active 成员 JOIN 片段（department_member →
%% organization_member active）。member_count 相关子查询与 humans 列表共用。
-define(ACTIVE_MEMBER_JOIN,
    "JOIN organization_member om ON om.organization_id = dm.organization_id"
    " AND om.user_id = dm.user_id AND om.status = 'active'"
).

%% member_count 相关子查询：只计本 Org 的 active 组织成员（口径=可见列表）。
-define(DIRECT_MEMBER_COUNT(DeptAlias),
    "(SELECT count(*) FROM organization_department_member dm "
    ?ACTIVE_MEMBER_JOIN
    " WHERE dm.department_id = "
    ??DeptAlias
    ".id) AS member_count"
).

%% ===================================================================
%% 授权门（org 生命周期 + caller 成员态，一次查询）
%% ===================================================================

%% @doc 目录读授权事实（单一真源查询）。
%% 返回：
%%   {error, not_found}                      —— org 不存在（→404）
%%   {ok, #{org_status, member_status}}      —— member_status 为 null 时表示非成员
%% 授权裁决（404/403/放行）在 application 层；本函数不吞不改事实。
-spec gate(integer(), integer()) ->
    {ok, #{binary() => binary() | null}} | {error, not_found} | {error, term()}.
gate(OrgId, Uid) ->
    Sql =
        <<
            "SELECT o.status::text AS org_status, m.status::text AS member_status"
            " FROM organization o"
            " LEFT JOIN organization_member m ON m.organization_id = o.id AND m.user_id = $2"
            " WHERE o.id = $1"
        >>,
    case elib_pg:one(Sql, [OrgId, Uid], undefined) of
        {ok, undefined} -> {error, not_found};
        {ok, Row} -> {ok, Row};
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% departments：直接 active 子部门（parent_id 缺省=根级）
%% ===================================================================

%% @doc Org 内某父节点的直接 active 子部门（ParentId = null 即根级），
%% id ASC keyset（AfterId 之后取 Limit 行），member_count 内联相关子查询
%% —— 单条 SQL 出全页投影，无每行二次查询。
-spec list_children_departments(integer(), null | integer(), integer(), pos_integer()) ->
    {ok, [map()]} | {error, term()}.
list_children_departments(OrgId, ParentId, AfterId, Limit) ->
    %% 两分支占位符独立编号：根级分支不引用 $2（parent_id 无参数），
    %% epgsql 按位置绑定，缺号会 pg_analyze_and_rewrite_varparams 失败。
    {Sql, Params} =
        case ParentId of
            null ->
                {
                    [
                        <<"SELECT d.id, d.parent_id, d.name, ">>,
                        ?DIRECT_MEMBER_COUNT(d),
                        <<
                            " FROM organization_department d"
                            " WHERE d.organization_id = $1 AND d.status = 'active'"
                            " AND d.parent_id IS NULL"
                            " AND d.id > $2 ORDER BY d.id ASC LIMIT $3"
                        >>
                    ],
                    [OrgId, AfterId, Limit]
                };
            _ ->
                {
                    [
                        <<"SELECT d.id, d.parent_id, d.name, ">>,
                        ?DIRECT_MEMBER_COUNT(d),
                        <<
                            " FROM organization_department d"
                            " WHERE d.organization_id = $1 AND d.status = 'active'"
                            " AND d.parent_id = $2"
                            " AND d.id > $3 ORDER BY d.id ASC LIMIT $4"
                        >>
                    ],
                    [OrgId, ParentId, AfterId, Limit]
                }
        end,
    elib_pg:query(Sql, Params).

%% ===================================================================
%% members：本级 active Human（user_id ASC keyset）
%% ===================================================================

%% @doc 某部门的直接成员，仅 organization_member.status='active' 且部门本身
%% active；user_id ASC keyset。
-spec list_department_humans(integer(), integer(), integer(), pos_integer()) ->
    {ok, [map()]} | {error, term()}.
list_department_humans(OrgId, DeptId, AfterUid, Limit) ->
    Sql =
        <<
            "SELECT om.user_id, u.nickname, u.account, u.avatar"
            " FROM organization_department_member dm"
            " JOIN organization_department d ON d.id = dm.department_id AND d.status = 'active'"
            " JOIN organization_member om ON om.organization_id = dm.organization_id"
            "  AND om.user_id = dm.user_id AND om.status = 'active'"
            " JOIN \"user\" u ON u.id = om.user_id"
            " WHERE dm.organization_id = $1 AND dm.department_id = $2 AND om.user_id > $3"
            " ORDER BY om.user_id ASC LIMIT $4"
        >>,
    elib_pg:query(Sql, [OrgId, DeptId, AfterUid, Limit]).

%% @doc 根成员：本 Org active 成员中**未挂任何 active 部门**者
%% （department_id 缺省语义；挂在已归档部门的人回到根成员视图）。
-spec list_root_humans(integer(), integer(), pos_integer()) ->
    {ok, [map()]} | {error, term()}.
list_root_humans(OrgId, AfterUid, Limit) ->
    Sql =
        <<
            "SELECT om.user_id, u.nickname, u.account, u.avatar"
            " FROM organization_member om"
            " JOIN \"user\" u ON u.id = om.user_id"
            " WHERE om.organization_id = $1 AND om.status = 'active' AND om.user_id > $2"
            " AND NOT EXISTS ("
            "   SELECT 1 FROM organization_department_member dm"
            "   JOIN organization_department d ON d.id = dm.department_id"
            "    AND d.status = 'active' AND d.organization_id = om.organization_id"
            "   WHERE dm.organization_id = om.organization_id AND dm.user_id = om.user_id)"
            " ORDER BY om.user_id ASC LIMIT $3"
        >>,
    elib_pg:query(Sql, [OrgId, AfterUid, Limit]).

%% @doc 一批用户在本 Org 的全部 active 部门 id（批量，禁 N+1）。
%% 返回按 user_id 分组、组内 department_id 升序。
-spec batch_user_departments(integer(), [integer()]) ->
    {ok, #{integer() => [integer()]}} | {error, term()}.
batch_user_departments(_OrgId, []) ->
    {ok, #{}};
batch_user_departments(OrgId, UserIds) ->
    Sql =
        <<
            "SELECT dm.user_id, dm.department_id"
            " FROM organization_department_member dm"
            " JOIN organization_department d ON d.id = dm.department_id AND d.status = 'active'"
            " WHERE dm.organization_id = $1 AND dm.user_id = ANY($2::bigint[])"
            " ORDER BY dm.user_id ASC, dm.department_id ASC"
        >>,
    case elib_pg:query(Sql, [OrgId, UserIds]) of
        {ok, Rows} ->
            {ok, group_rows(Rows)};
        {error, Reason} ->
            {error, Reason}
    end.

group_rows(Rows) ->
    lists:foldl(
        fun(Row, Acc) ->
            Uid = maps:get(<<"user_id">>, Row),
            DeptId = maps:get(<<"department_id">>, Row),
            Acc#{Uid => [DeptId | maps:get(Uid, Acc, [])]}
        end,
        #{},
        Rows
    ).

%% ===================================================================
%% /me：当前用户的 active 部门
%% ===================================================================

%% @doc 用户在本 Org 的全部 active 部门（含 member_count，单 SQL）。
-spec list_my_departments(integer(), integer()) -> {ok, [map()]} | {error, term()}.
list_my_departments(OrgId, Uid) ->
    Sql = iolist_to_binary([
        <<
            "SELECT d.id, d.parent_id, d.name, "
        >>,
        ?DIRECT_MEMBER_COUNT(d),
        <<
            " FROM organization_department_member dm"
            " JOIN organization_department d ON d.id = dm.department_id AND d.status = 'active'"
            " WHERE dm.organization_id = $1 AND dm.user_id = $2"
            " ORDER BY d.id ASC"
        >>
    ]),
    elib_pg:query(Sql, [OrgId, Uid]).

%% ===================================================================
%% search：部门名 / 昵称 / 账号三列（U-02：不搜手机号/邮箱/职位）
%% ===================================================================

%% @doc 联合检索：kind 0=部门（name 命中），kind 1=成员（nickname/account 命中）。
%% 统一 (kind ASC, id ASC) keyset：AfterKind=0 取全部两支，AfterKind=1 只取成员支
%% 的 id 进位。Pattern 必须是已转义的 LIKE 模式串（含 % 包裹），本函数不二次转义。
-spec search_directory(
    integer(),
    binary(),
    0 | 1,
    integer(),
    pos_integer()
) -> {ok, [map()]} | {error, term()}.
search_directory(OrgId, Pattern, AfterKind, AfterId, Limit) ->
    DeptBranch =
        <<
            "SELECT 0 AS kind, d.id AS id, d.name AS name, d.parent_id AS parent_id,"
        >>,
    Sql = iolist_to_binary([
        <<
            "SELECT t.kind, t.id, t.name, t.parent_id, t.member_count,"
            " t.user_id, t.nickname, t.account, t.avatar FROM ("
        >>,
        DeptBranch,
        <<" ">>,
        ?DIRECT_MEMBER_COUNT(d),
        <<
            ", NULL::bigint AS user_id, NULL::varchar AS nickname,"
            " NULL::varchar AS account, NULL::text AS avatar"
            " FROM organization_department d"
            " WHERE d.organization_id = $1 AND d.status = 'active'"
            "   AND d.name ILIKE $2 ESCAPE '\\'"
            "   AND ($3 = 0 AND d.id > $4)"
            " UNION ALL "
            "SELECT 1 AS kind, om.user_id AS id, NULL::varchar AS name,"
            " NULL::bigint AS parent_id, NULL::bigint AS member_count,"
            " om.user_id, u.nickname, u.account, u.avatar"
            " FROM organization_member om"
            " JOIN \"user\" u ON u.id = om.user_id"
            " WHERE om.organization_id = $1 AND om.status = 'active'"
            "   AND (u.nickname ILIKE $2 ESCAPE '\\' OR u.account ILIKE $2 ESCAPE '\\')"
            "   AND (($3 = 0) OR ($3 = 1 AND om.user_id > $4))"
            ") t ORDER BY t.kind ASC, t.id ASC LIMIT $5"
        >>
    ]),
    elib_pg:query(Sql, [OrgId, Pattern, AfterKind, AfterId, Limit]).
