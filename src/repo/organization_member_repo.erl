-module(organization_member_repo).

%% Organization 治理成员关系。它与 workspace_member 相互独立：
%% Workspace 可以邀请不属于其 Organization 的外部用户。

-export([
    tablename/0,
    find_active/3,
    find_active_tx/4,
    find_active_for_share_tx/4,
    find_for_update_tx/4,
    find_organization_for_share_tx/3,
    page_by_organization/4,
    member_workspaces/2,
    upsert_active_tx/5,
    update_role_tx/4,
    remove_tx/3
]).

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"organization_member">>).

-spec find_active(integer(), integer(), binary()) -> {ok, map()} | {error, not_found | term()}.
find_active(OrgId, Uid, Columns) when
    is_integer(OrgId), is_integer(Uid), is_binary(Columns)
->
    find_active_with(fun elib_pg:query/2, OrgId, Uid, Columns).

-spec find_active_tx(any(), integer(), integer(), binary()) ->
    {ok, map()} | {error, not_found | term()}.
find_active_tx(Conn, OrgId, Uid, Columns) when
    is_integer(OrgId), is_integer(Uid), is_binary(Columns)
->
    find_active_with(fun(Sql, Params) -> elib_pg:query(Conn, Sql, Params) end, OrgId, Uid, Columns).

%% @doc 写组织级资源前锁定成员行，使撤权更新等待当前事务结束。
-spec find_active_for_share_tx(any(), integer(), integer(), binary()) ->
    {ok, map()} | {error, not_found | term()}.
find_active_for_share_tx(Conn, OrgId, Uid, Columns) when
    is_integer(OrgId), is_integer(Uid), is_binary(Columns)
->
    find_active_with(
        fun(Sql, Params) -> elib_pg:query(Conn, <<Sql/binary, " FOR SHARE">>, Params) end,
        OrgId,
        Uid,
        Columns
    ).

-spec find_for_update_tx(any(), integer(), integer(), binary()) ->
    {ok, map()} | {error, not_found | term()}.
find_for_update_tx(Conn, OrgId, Uid, Columns) when
    is_integer(OrgId), is_integer(Uid), is_binary(Columns)
->
    Sql =
        <<"SELECT ", Columns/binary, " FROM ", (tablename())/binary,
            " WHERE organization_id = $1 AND user_id = $2 FOR UPDATE">>,
    one_tx(Conn, Sql, [OrgId, Uid]).

%% @doc 锁住 Organization 行，稳定校验生命周期与主 Owner。
-spec find_organization_for_share_tx(any(), integer(), binary()) ->
    {ok, map()} | {error, not_found | term()}.
find_organization_for_share_tx(Conn, OrgId, Columns) when
    is_integer(OrgId), is_binary(Columns)
->
    Sql =
        <<"SELECT ", Columns/binary, " FROM organization WHERE id = $1 FOR SHARE">>,
    one_tx(Conn, Sql, [OrgId]).

-spec page_by_organization(integer(), integer(), integer(), binary()) ->
    {ok, map()} | {error, term()}.
page_by_organization(OrgId, Page, Size, Columns) ->
    Tb = tablename(),
    CountSql =
        <<"SELECT COUNT(*) AS count FROM ", Tb/binary,
            " WHERE organization_id = $1 AND status = 'active'">>,
    case elib_pg:one(CountSql, [OrgId]) of
        {ok, #{<<"count">> := Total}} ->
            Offset = (Page - 1) * Size,
            DataSql =
                <<"SELECT ", Columns/binary, " FROM ", Tb/binary, " om", " LEFT JOIN ",
                    (user_repo:tablename())/binary, " u ON u.id = om.user_id",
                    " WHERE om.organization_id = $1 AND om.status = 'active'",
                    " ORDER BY CASE om.role WHEN 'owner' THEN 1 WHEN 'admin' THEN 2 ELSE 3 END,",
                    " om.joined_at ASC, om.user_id ASC LIMIT $2 OFFSET $3">>,
            case elib_pg:query(DataSql, [OrgId, Size, Offset]) of
                {ok, Items} ->
                    TotalPage =
                        case Total of
                            0 -> 0;
                            _ -> ((Total - 1) div Size) + 1
                        end,
                    {ok, #{
                        list => Items,
                        page => Page,
                        size => Size,
                        total => Total,
                        total_page => TotalPage
                    }};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason};
        Other ->
            {error, {unexpected_count_result, Other}}
    end.

%% @doc 一批成员在该 Organization 内的**有权 Workspace**（计划 §5.2：成员详情
%% 必须含有权 Workspace 信息）。
%%
%% 判定口径：workspace_member.status='active' ∧ workspace.status='active'
%% ∧ workspace.organization_id = 目标 Org（跨 Org 的 Workspace 授权不算数——
%% 本接口回答的是"在本企业内有权的工作区"）。
%% 一次查完整页（user_id = ANY($1)），不在上层做 N+1；返回按 user_id 分组的
%% 列表，缺省空列表。
-spec member_workspaces(integer(), [integer()]) ->
    {ok, #{integer() => [map()]}} | {error, term()}.
member_workspaces(_OrgId, []) ->
    {ok, #{}};
member_workspaces(OrgId, UserIds) when is_integer(OrgId), OrgId > 0 ->
    Sql =
        <<"SELECT wm.user_id, w.id, w.name FROM ", (workspace_member_tablename())/binary,
            " wm JOIN ", (workspace_tablename())/binary, " w ON w.id = wm.workspace_id",
            " WHERE wm.user_id = ANY($1::bigint[]) AND wm.status = 'active'",
            " AND w.status = 'active' AND w.organization_id = $2", " ORDER BY w.id ASC">>,
    case elib_pg:query(Sql, [UserIds, OrgId]) of
        {ok, Rows} ->
            {ok, grouping_workspaces(Rows)};
        {error, Reason} ->
            {error, Reason}
    end.

-spec grouping_workspaces([map()]) -> #{integer() => [map()]}.
grouping_workspaces(Rows) ->
    Reversed =
        lists:foldl(
            fun(Row, Acc) ->
                Uid = maps:get(<<"user_id">>, Row),
                Item = #{
                    <<"id">> => maps:get(<<"id">>, Row), <<"name">> => maps:get(<<"name">>, Row)
                },
                Acc#{Uid => [Item | maps:get(Uid, Acc, [])]}
            end,
            #{},
            Rows
        ),
    %% SQL 已按 w.id ASC 返回，foldl 前插后每组成员是倒序——这里恢复升序，
    %% 让出站顺序与 SQL 一致（前端按 workspace id 升序展示，稳定可断言）。
    maps:map(fun(_Uid, Items) -> lists:reverse(Items) end, Reversed).

-spec workspace_member_tablename() -> binary().
workspace_member_tablename() ->
    elib_pg_sql:public_tablename(<<"workspace_member">>).

-spec workspace_tablename() -> binary().
workspace_tablename() ->
    elib_pg_sql:public_tablename(<<"workspace">>).

%% @doc 幂等邀请或恢复。active 同角色不写库；active 异角色交给角色接口处理。
-spec upsert_active_tx(any(), integer(), integer(), binary(), integer()) ->
    {ok, changed | unchanged | role_conflict, map()} | {error, term()}.
upsert_active_tx(Conn, OrgId, Uid, Role, InvitedBy) ->
    case find_for_update_tx(Conn, OrgId, Uid, <<"role,status">>) of
        {ok, #{<<"status">> := <<"active">>, <<"role">> := Role}} ->
            {ok, unchanged, #{}};
        {ok, #{<<"status">> := <<"active">>}} ->
            {ok, role_conflict, #{}};
        {ok, _Removed} ->
            activate_tx(Conn, OrgId, Uid, Role, InvitedBy);
        {error, not_found} ->
            activate_tx(Conn, OrgId, Uid, Role, InvitedBy);
        {error, Reason} ->
            {error, Reason}
    end.

-spec update_role_tx(any(), integer(), integer(), binary()) -> ok | {error, term()}.
update_role_tx(Conn, OrgId, Uid, Role) ->
    Sql =
        <<"UPDATE ", (tablename())/binary, " SET role = $1, updated_at = CURRENT_TIMESTAMP",
            " WHERE organization_id = $2 AND user_id = $3 AND status = 'active'">>,
    execute_one(Conn, Sql, [Role, OrgId, Uid]).

-spec remove_tx(any(), integer(), integer()) -> ok | {error, term()}.
remove_tx(Conn, OrgId, Uid) ->
    Sql =
        <<"UPDATE ", (tablename())/binary,
            " SET status = 'removed', updated_at = CURRENT_TIMESTAMP",
            " WHERE organization_id = $1 AND user_id = $2 AND status = 'active'">>,
    execute_one(Conn, Sql, [OrgId, Uid]).

-spec find_active_with(fun((binary(), list()) -> term()), integer(), integer(), binary()) ->
    {ok, map()} | {error, not_found | term()}.
find_active_with(Query, OrgId, Uid, Columns) ->
    Sql =
        <<"SELECT ", Columns/binary, " FROM ", (tablename())/binary,
            " WHERE organization_id = $1 AND user_id = $2 AND status = 'active' LIMIT 1">>,
    case Query(Sql, [OrgId, Uid]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

-spec activate_tx(any(), integer(), integer(), binary(), integer()) ->
    {ok, changed, map()} | {error, term()}.
activate_tx(Conn, OrgId, Uid, Role, InvitedBy) ->
    Sql =
        <<"INSERT INTO ", (tablename())/binary,
            " (organization_id,user_id,role,invited_by,joined_at,status,created_at,updated_at)",
            " VALUES ($1,$2,$3,$4,CURRENT_TIMESTAMP,'active',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP)",
            " ON CONFLICT (organization_id,user_id) DO UPDATE SET",
            " role = EXCLUDED.role, invited_by = EXCLUDED.invited_by,",
            " joined_at = EXCLUDED.joined_at, status = 'active', updated_at = EXCLUDED.updated_at">>,
    case elib_pg:execute(Conn, Sql, [OrgId, Uid, Role, InvitedBy]) of
        {ok, 1} -> {ok, changed, #{}};
        {error, Reason} -> {error, Reason};
        Other -> {error, {unexpected_write_result, Other}}
    end.

-spec one_tx(any(), binary(), list()) -> {ok, map()} | {error, not_found | term()}.
one_tx(Conn, Sql, Params) ->
    case elib_pg:query(Conn, Sql, Params) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

-spec execute_one(any(), binary(), list()) -> ok | {error, term()}.
execute_one(Conn, Sql, Params) ->
    case elib_pg:execute(Conn, Sql, Params) of
        {ok, 1} -> ok;
        {ok, _} -> {error, member_not_active};
        {error, Reason} -> {error, Reason}
    end.
