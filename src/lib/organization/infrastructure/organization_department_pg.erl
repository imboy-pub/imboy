%%% @doc Organization Department 数据访问（infrastructure，仅 SQL）。
%%%
%%% 职责：department / department_member 两张表的读写与**锁内树操作**。
%%% 本模块不理解授权；同 Org / active membership / 环防的**权威**裁决在 DB
%%% （组合 FK + 触发器），本模块把它们如实暴露为错误返回，不吞不改。
%%%
%%% 并发口径（ORG-04 卡「move 锁相关树节点」）：
%%%   所有对某 Org 树的**结构性写**（move / archive 子树）在单事务内先
%%%   `SELECT ... ORDER BY id FOR UPDATE` 锁定该 Org 的全部部门节点（全序加锁
%%%   ⇒ 同 Org 的结构性写天然串行、跨事务零死锁），再在锁内做环/状态裁决。
-module(organization_department_pg).

-export([
    insert_department/5,
    fetch_department/3,
    update_name/5,
    move_tx/5,
    archive_subtree_tx/4,
    list_departments/3,
    list_active_subtree_ids/3,
    fetch_member/3,
    insert_member/5,
    delete_member/3,
    set_admin/5,
    list_members/2,
    count_members/2,
    is_department_admin/3,
    org_role_of/2
]).

-include_lib("epgsql/include/epgsql.hrl").

%% 出站列白名单（department 行）
-define(DEPT_COLS, <<
    "id, organization_id, parent_id, name, status, version, "
    "created_by_user_id, updated_by_user_id, created_at, updated_at"
>>).

%% 出站列白名单（department_member 行）
-define(MEMBER_COLS, <<
    "organization_id, department_id, user_id, is_admin, "
    "added_by_user_id, created_at, updated_at"
>>).

%% ===================================================================
%% department
%% ===================================================================

%% @doc 建部门（parent_id = null 即根）。唯一冲突（同 Org active 同名）返回
%% {error, name_conflict}；环/self-parent 由 CHECK+触发器兜底返回 {error, cycle}。
-spec insert_department(integer(), integer() | null, binary(), integer() | undefined, term()) ->
    {ok, map()} | {error, term()}.
insert_department(OrgId, ParentId, Name, ActorId, IdGen) ->
    Id = IdGen(),
    case
        normalize(
            elib_pg:one(
                <<
                    "INSERT INTO organization_department"
                    " (id, organization_id, parent_id, name, status, version, created_by_user_id)"
                    " VALUES ($1, $2, $3, $4, 'active', 1, $5)"
                    " RETURNING ",
                    ?DEPT_COLS/binary
                >>,
                [Id, OrgId, ParentId, Name, ActorId],
                undefined
            )
        )
    of
        {ok, undefined} -> {error, insert_failed};
        {ok, Row} -> {ok, Row};
        {error, #error{code = <<"23505">>}} -> {error, name_conflict};
        {error, #error{code = <<"23514">>}} -> {error, cycle};
        {error, #error{code = <<"23503">>}} -> {error, parent_not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 取部门（org 作用域内；跨 Org 一律 not_found，不做租户枚举）。
-spec fetch_department(integer(), integer(), term()) ->
    {ok, map()} | {error, not_found} | {error, term()}.
fetch_department(OrgId, DeptId, _Ctx) ->
    case
        normalize(
            elib_pg:one(
                <<"SELECT ", ?DEPT_COLS/binary,
                    " FROM organization_department WHERE id = $1 AND organization_id = $2">>,
                [DeptId, OrgId],
                undefined
            )
        )
    of
        {ok, undefined} -> {error, not_found};
        {ok, Row} -> {ok, Row};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 改名（乐观锁 CAS；version 不符返回 {error, conflict}）。
-spec update_name(integer(), integer(), binary(), integer() | undefined, integer()) ->
    ok | {error, conflict} | {error, name_conflict} | {error, term()}.
update_name(OrgId, DeptId, Name, ActorId, ExpectedVersion) ->
    case
        elib_pg:query(
            <<
                "UPDATE organization_department"
                " SET name = $3, updated_by_user_id = $4,"
                "     version = version + 1, updated_at = CURRENT_TIMESTAMP"
                " WHERE id = $1 AND organization_id = $2 AND version = $5 AND status = 'active'"
            >>,
            [DeptId, OrgId, Name, ActorId, ExpectedVersion]
        )
    of
        {ok, 1} ->
            ok;
        {ok, 0} ->
            %% 区分：不存在 / 版本冲突 / 非 active / 同名冲突
            case fetch_department(OrgId, DeptId, none) of
                {ok, #{version := ExpectedVersion, status := active}} ->
                    case
                        elib_pg:one(
                            <<
                                "SELECT 1 FROM organization_department"
                                " WHERE organization_id = $1 AND name = $2 AND status = 'active'"
                                " AND id <> $3"
                            >>,
                            [OrgId, Name, DeptId],
                            undefined
                        )
                    of
                        {ok, undefined} -> {error, conflict};
                        {ok, _} -> {error, name_conflict};
                        {error, Reason} -> {error, Reason}
                    end;
                _Other ->
                    {error, conflict}
            end;
        {error, #error{code = <<"23505">>}} ->
            {error, name_conflict};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc move（单事务）：全 Org 节点按 id 全序加锁 → 锁内校验（存在/active/新父同
%% Org 且 active）→ 锁内取新父祖先链 → 环裁决 → CAS 更新 parent_id。
%% ExpectedVersion 并发裁决：锁串行化后仍是旧版本的请求按 stale 拒绝。
-spec move_tx(integer(), integer(), integer() | null, integer() | undefined, integer()) ->
    {ok, map()} | {error, term()}.
move_tx(OrgId, DeptId, NewParentId, ActorId, ExpectedVersion) ->
    elib_pg:with_tx(fun(Conn) ->
        %% 1. 全序锁：该 Org 的全部部门行（相关树节点全部锁定）
        case
            epgsql:equery(
                Conn,
                <<
                    "SELECT id FROM organization_department"
                    " WHERE organization_id = $1 ORDER BY id FOR UPDATE"
                >>,
                [OrgId]
            )
        of
            {ok, _Cols, _Locked} -> ok;
            {error, PgErr} -> erlang:error({pg, PgErr})
        end,
        case locked_fetch(Conn, OrgId, DeptId) of
            {error, not_found} ->
                {error, not_found};
            {ok, #{status := archived}} ->
                {error, department_archived};
            {ok, #{version := ExpectedVersion} = Dept} ->
                move_locked(Conn, OrgId, Dept, NewParentId, ActorId);
            {ok, _StaleVersion} ->
                {error, conflict}
        end
    end).

%% 锁内：新父校验 + 祖先链环裁决 + CAS 落库。
move_locked(Conn, OrgId, Dept, NewParentId, ActorId) ->
    DeptId = maps:get(id, Dept),
    case organization_department:ensure_not_self_parent(DeptId, NewParentId) of
        {error, _} = Err ->
            Err;
        ok ->
            case NewParentId of
                null ->
                    move_update(Conn, OrgId, Dept, null, [], ActorId);
                _ when is_integer(NewParentId) ->
                    case locked_fetch(Conn, OrgId, NewParentId) of
                        {error, not_found} ->
                            {error, {new_parent_not_found, NewParentId}};
                        {ok, #{status := archived}} ->
                            {error, {new_parent_archived, NewParentId}};
                        {ok, Parent} ->
                            Ancestors = locked_ancestor_ids(Conn, OrgId, NewParentId),
                            case
                                organization_department:ensure_not_in_ancestors(
                                    DeptId, Ancestors
                                )
                            of
                                {error, _} = Err ->
                                    Err;
                                ok ->
                                    move_update(
                                        Conn, OrgId, Dept, maps:get(id, Parent), Ancestors, ActorId
                                    )
                            end
                    end;
                _Other ->
                    {error, {invalid_parent_id, NewParentId}}
            end
    end.

move_update(Conn, _OrgId, Dept, NewParentId, _Ancestors, ActorId) ->
    DeptId = maps:get(id, Dept),
    ExpectedVersion = maps:get(version, Dept),
    case
        epgsql:equery(
            Conn,
            <<
                "UPDATE organization_department"
                " SET parent_id = $3, updated_by_user_id = $4,"
                "     version = version + 1, updated_at = CURRENT_TIMESTAMP"
                " WHERE id = $1 AND version = $2"
            >>,
            [DeptId, ExpectedVersion, NewParentId, ActorId]
        )
    of
        {ok, 1} ->
            %% CAS 持锁成功：结果由语句语义完全确定，无需回读
            {ok, #{
                id => DeptId,
                parent_id => NewParentId,
                version => ExpectedVersion + 1,
                organization_id => maps:get(organization_id, Dept)
            }};
        {ok, 0} ->
            {error, conflict};
        {error, PgErr} ->
            erlang:error({pg, PgErr})
    end.

%% @doc archive（单事务）：全序锁 → 域裁决（幂等）→ 子树（自身+全部后代）
%% 原子置 archived。只改目录状态，不触任何 membership/权限表（C10）。
-spec archive_subtree_tx(integer(), integer(), integer() | undefined, term()) ->
    {ok, map()} | {error, term()}.
archive_subtree_tx(OrgId, DeptId, ActorId, _Ctx) ->
    elib_pg:with_tx(fun(Conn) ->
        case
            epgsql:equery(
                Conn,
                <<
                    "SELECT id FROM organization_department"
                    " WHERE organization_id = $1 ORDER BY id FOR UPDATE"
                >>,
                [OrgId]
            )
        of
            {ok, _Cols, _Locked} -> ok;
            {error, PgErr} -> erlang:error({pg, PgErr})
        end,
        case locked_fetch(Conn, OrgId, DeptId) of
            {error, not_found} ->
                {error, not_found};
            {ok, #{status := archived} = Dept} ->
                %% 幂等：已归档返回当前状态，零写入
                {ok, Dept#{archive_idempotent => true}};
            {ok, Dept} ->
                SubtreeIds = locked_subtree_ids(Conn, OrgId, DeptId),
                case
                    epgsql:equery(
                        Conn,
                        <<
                            "UPDATE organization_department"
                            " SET status = 'archived', updated_by_user_id = $3,"
                            "     updated_at = CURRENT_TIMESTAMP"
                            " WHERE organization_id = $1 AND id = ANY($2) AND status = 'active'"
                        >>,
                        [OrgId, SubtreeIds, ActorId]
                    )
                of
                    {ok, UpdatedCount} when is_integer(UpdatedCount) ->
                        {ok, Dept#{archive_subtree_count => UpdatedCount}};
                    {error, UpdErr} ->
                        erlang:error({pg, UpdErr})
                end
        end
    end).

%% @doc 列出部门（org 内；可选 status 白名单过滤；id 升序=创建序）。
-spec list_departments(integer(), active | archived | all, term()) ->
    {ok, [map()]} | {error, term()}.
list_departments(OrgId, Status, _Ctx) ->
    {Sql, Params} =
        case Status of
            all ->
                {
                    <<"SELECT ", ?DEPT_COLS/binary,
                        " FROM organization_department WHERE organization_id = $1 ORDER BY id">>,
                    [OrgId]
                };
            S when S =:= active; S =:= archived ->
                {
                    <<"SELECT ", ?DEPT_COLS/binary,
                        " FROM organization_department"
                        " WHERE organization_id = $1 AND status = $2 ORDER BY id">>,
                    [OrgId, atom_to_binary(S, utf8)]
                }
        end,
    normalize(elib_pg:query(Sql, Params)).

%% @doc 某部门的全部 active 后代 id（不含自身；读路径用，不加锁）。
-spec list_active_subtree_ids(integer(), integer(), term()) -> {ok, [integer()]} | {error, term()}.
list_active_subtree_ids(OrgId, DeptId, _Ctx) ->
    elib_pg:with_tx(fun(Conn) ->
        {ok, locked_subtree_ids(Conn, OrgId, DeptId)}
    end).

%% ===================================================================
%% 锁内辅助（调用方必须已持有事务连接）
%% ===================================================================

locked_fetch(Conn, OrgId, DeptId) ->
    case
        epgsql:equery(
            Conn,
            <<"SELECT ", ?DEPT_COLS/binary,
                " FROM organization_department WHERE id = $1 AND organization_id = $2">>,
            [DeptId, OrgId]
        )
    of
        {ok, _Cols, []} -> {error, not_found};
        {ok, Cols, [Row]} -> {ok, normalize_row(row_to_map(Cols, Row))};
        {error, PgErr} -> erlang:error({pg, PgErr})
    end.

%% 从 FromId 沿 parent 链上行到根的 id 列表（首元素是 FromId 自身；
%% 自身与 self-parent 的冲突由 CHECK 独立兜住，整表成员判定对环检测无害）。
locked_ancestor_ids(Conn, OrgId, FromId) ->
    case
        epgsql:equery(
            Conn,
            <<
                "WITH RECURSIVE anc AS ("
                "  SELECT d.id, d.parent_id"
                "    FROM organization_department d"
                "   WHERE d.id = $2 AND d.organization_id = $1"
                "  UNION ALL"
                "  SELECT d.id, d.parent_id"
                "    FROM organization_department d"
                "    JOIN anc ON d.id = anc.parent_id"
                "   WHERE d.organization_id = $1"
                ") SELECT id FROM anc"
            >>,
            [OrgId, FromId]
        )
    of
        {ok, _Cols, Rows} ->
            [element(1, Row) || Row <- Rows];
        {error, PgErr} ->
            erlang:error({pg, PgErr})
    end.

%% 子树（自身+全部后代）id 列表（锁内）。
locked_subtree_ids(Conn, OrgId, DeptId) ->
    case
        epgsql:equery(
            Conn,
            <<
                "WITH RECURSIVE sub AS ("
                "  SELECT d.id, d.parent_id"
                "    FROM organization_department d"
                "   WHERE d.id = $2 AND d.organization_id = $1"
                "  UNION ALL"
                "  SELECT d.id, d.parent_id"
                "    FROM organization_department d"
                "    JOIN sub ON d.parent_id = sub.id"
                "   WHERE d.organization_id = $1"
                ") SELECT id FROM sub"
            >>,
            [OrgId, DeptId]
        )
    of
        {ok, _Cols, Rows} ->
            [element(1, Row) || Row <- Rows];
        {error, PgErr} ->
            erlang:error({pg, PgErr})
    end.

row_to_map(Cols, Row) when is_list(Cols) ->
    %% epgsql equery 返回的 Cols 是 #column{} 记录的 LIST（非 tuple）
    Names = [element(2, C) || C <- Cols],
    Values = tuple_to_list(Row),
    maps:from_list(lists:zip(Names, Values)).

%% ===================================================================
%% department_member
%% ===================================================================

-spec fetch_member(integer(), integer(), term()) ->
    {ok, map()} | {error, not_found} | {error, term()}.
fetch_member(DeptId, UserId, _Ctx) ->
    case
        normalize(
            elib_pg:one(
                <<"SELECT ", ?MEMBER_COLS/binary,
                    " FROM organization_department_member WHERE department_id = $1 AND user_id = $2">>,
                [DeptId, UserId],
                undefined
            )
        )
    of
        {ok, undefined} -> {error, not_found};
        {ok, Row} -> {ok, Row};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 加成员。已存在 ⇒ {ok, Row, already_member}（幂等语义由调用方表述）。
%% 同 Org active membership 由组合 FK + 触发器权威裁决：
%%   23503 = membership 行不存在；23514 = membership 非 active（或 department 不存在）。
-spec insert_member(integer(), integer(), integer(), integer() | undefined, term()) ->
    {ok, map(), added | already_member} | {error, member_not_org_member} | {error, term()}.
insert_member(OrgId, DeptId, UserId, ActorId, _Ctx) ->
    case
        normalize(
            elib_pg:query(
                <<
                    "INSERT INTO organization_department_member"
                    " (organization_id, department_id, user_id, is_admin, added_by_user_id)"
                    " VALUES ($1, $2, $3, false, $4)"
                    " ON CONFLICT (department_id, user_id) DO NOTHING"
                    " RETURNING ",
                    ?MEMBER_COLS/binary
                >>,
                [OrgId, DeptId, UserId, ActorId]
            )
        )
    of
        {ok, [Row]} ->
            {ok, Row, added};
        {ok, []} ->
            case fetch_member(DeptId, UserId, none) of
                {ok, Row} -> {ok, Row, already_member};
                {error, Reason} -> {error, Reason}
            end;
        {error, #error{code = <<"23503">>}} ->
            {error, member_not_org_member};
        {error, #error{code = <<"23514">>}} ->
            {error, member_not_org_member};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 移除成员；返回是否真的删除（false=本就不在，幂等）。
-spec delete_member(integer(), integer(), term()) -> {ok, removed | not_present} | {error, term()}.
delete_member(DeptId, UserId, _Ctx) ->
    case
        elib_pg:query(
            <<
                "DELETE FROM organization_department_member"
                " WHERE department_id = $1 AND user_id = $2"
            >>,
            [DeptId, UserId]
        )
    of
        {ok, 1} -> {ok, removed};
        {ok, 0} -> {ok, not_present};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 设置/取消局部目录管理员标记（只改本行 is_admin，不触任何权限表）。
-spec set_admin(integer(), integer(), boolean(), integer() | undefined, term()) ->
    ok | {error, not_member} | {error, term()}.
set_admin(DeptId, UserId, IsAdmin, _ActorId, _Ctx) when is_boolean(IsAdmin) ->
    %% 注意参数序：调用方 app 层负责 actor 审计；本函数只做标记位翻转
    case
        elib_pg:query(
            <<
                "UPDATE organization_department_member SET is_admin = $3, updated_at = CURRENT_TIMESTAMP"
                " WHERE department_id = $1 AND user_id = $2"
            >>,
            [DeptId, UserId, IsAdmin]
        )
    of
        {ok, 1} -> ok;
        {ok, 0} -> {error, not_member};
        {error, Reason} -> {error, Reason}
    end.

-spec list_members(integer(), term()) -> {ok, [map()]} | {error, term()}.
list_members(DeptId, _Ctx) ->
    normalize(
        elib_pg:query(
            <<"SELECT ", ?MEMBER_COLS/binary,
                " FROM organization_department_member WHERE department_id = $1 ORDER BY user_id">>,
            [DeptId]
        )
    ).

-spec count_members(integer(), term()) -> {ok, integer()} | {error, term()}.
count_members(DeptId, _Ctx) ->
    case
        elib_pg:one(
            <<"SELECT count(*) AS n FROM organization_department_member WHERE department_id = $1">>,
            [DeptId],
            #{<<"n">> => 0}
        )
    of
        {ok, #{<<"n">> := N}} -> {ok, N};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 该用户是否是部门管理员（局部目录角色事实；不是授权结论）。
-spec is_department_admin(integer(), integer(), term()) -> {ok, boolean()} | {error, term()}.
is_department_admin(DeptId, UserId, _Ctx) ->
    case
        normalize(
            elib_pg:one(
                <<
                    "SELECT is_admin FROM organization_department_member"
                    " WHERE department_id = $1 AND user_id = $2"
                >>,
                [DeptId, UserId],
                undefined
            )
        )
    of
        {ok, undefined} -> {ok, false};
        {ok, #{is_admin := Flag}} -> {ok, Flag};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 用户的 organization_member 行（org_role / status 事实查询，零写）。
-spec org_role_of(integer(), integer()) -> {ok, map()} | {error, not_found} | {error, term()}.
org_role_of(OrgId, UserId) ->
    case
        normalize(
            elib_pg:one(
                <<
                    "SELECT organization_id, user_id, role, status FROM organization_member"
                    " WHERE organization_id = $1 AND user_id = $2"
                >>,
                [OrgId, UserId],
                undefined
            )
        )
    of
        {ok, undefined} -> {error, not_found};
        {ok, Row} -> {ok, Row};
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% 行归一化：elib_pg 返回二进制列名/二进制枚举值；本模块边界统一转为
%% 原子键与原子枚举（status/role/is_admin），上层按原子模式匹配。
%% ===================================================================

normalize({ok, undefined}) ->
    {ok, undefined};
normalize({ok, Row}) when is_map(Row) ->
    {ok, normalize_row(Row)};
normalize({ok, Rows}) when is_list(Rows) ->
    {ok, [normalize_row(R) || R <- Rows]};
normalize(Other) ->
    Other.

normalize_row(Row) when is_map(Row) ->
    maps:fold(
        fun(K, V, Acc) -> maps:put(to_atom(K), norm_value(K, V), Acc) end,
        #{},
        Row
    ).

to_atom(K) when is_binary(K) ->
    binary_to_atom(K, utf8);
to_atom(K) ->
    K.

norm_value(<<"status">>, V) when is_binary(V) -> binary_to_atom(V, utf8);
norm_value(<<"role">>, V) when is_binary(V) -> binary_to_atom(V, utf8);
norm_value(_K, V) -> V.
