-module(organization_repo).

%% Organization 本体仓储。成员资格由 organization_member_repo 独立维护。

-export([
    tablename/0,
    create_tx/4,
    find_by_id/1,
    find_for_update_tx/2,
    update_tx/5,
    update_owner_tx/3,
    page_by_member/3
]).

-define(COLUMNS, <<"id,name,owner_id,status,branding,settings,created_at,updated_at">>).

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"organization">>).

%% Status 由调用方决定：APP 建企传 'pending'（注册审核），平台/Admin 路径
%% 自行 INSERT（status='active'，视为已审核）。DB CHECK 兜底枚举合法性。
-spec create_tx(any(), integer(), binary(), binary()) -> {ok, map()} | {error, term()}.
create_tx(Conn, OwnerUid, Name, Status) ->
    Id = elib_tsid:generate(organization),
    Sql =
        <<"INSERT INTO ", (tablename())/binary,
            " (id,name,owner_id,status,branding,settings,created_at,updated_at)",
            " VALUES ($1,$2,$3,$4,'{}'::jsonb,'{}'::jsonb,",
            "CURRENT_TIMESTAMP,CURRENT_TIMESTAMP) RETURNING ", ?COLUMNS/binary>>,
    one_tx(Conn, Sql, [Id, Name, OwnerUid, Status]).

-spec find_by_id(integer()) -> {ok, map()} | {error, not_found | term()}.
find_by_id(OrgId) when is_integer(OrgId), OrgId > 0 ->
    Sql = <<"SELECT ", ?COLUMNS/binary, " FROM ", (tablename())/binary, " WHERE id = $1">>,
    case elib_pg:query(Sql, [OrgId]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

-spec find_for_update_tx(any(), integer()) -> {ok, map()} | {error, not_found | term()}.
find_for_update_tx(Conn, OrgId) ->
    Sql =
        <<"SELECT ", ?COLUMNS/binary, " FROM ", (tablename())/binary, " WHERE id = $1 FOR UPDATE">>,
    one_tx(Conn, Sql, [OrgId]).

%% branding/settings 使用浅合并，保留其他客户端写入的命名空间。
-spec update_tx(any(), integer(), binary() | null, binary() | null, binary() | null) ->
    {ok, map()} | {error, not_found | term()}.
update_tx(Conn, OrgId, Name, BrandingJson, SettingsJson) ->
    Sql =
        <<"UPDATE ", (tablename())/binary, " SET name = COALESCE($1,name),",
            " branding = CASE WHEN $2::jsonb IS NULL THEN branding ELSE branding || $2::jsonb END,",
            " settings = CASE WHEN $3::jsonb IS NULL THEN settings ELSE settings || $3::jsonb END,",
            " updated_at = CURRENT_TIMESTAMP WHERE id = $4 RETURNING ", ?COLUMNS/binary>>,
    one_tx(Conn, Sql, [Name, BrandingJson, SettingsJson, OrgId]).

-spec update_owner_tx(any(), integer(), integer()) -> {ok, map()} | {error, not_found | term()}.
update_owner_tx(Conn, OrgId, NewOwnerUid) ->
    Sql =
        <<"UPDATE ", (tablename())/binary, " SET owner_id = $1, updated_at = CURRENT_TIMESTAMP",
            " WHERE id = $2 RETURNING ", ?COLUMNS/binary>>,
    one_tx(Conn, Sql, [NewOwnerUid, OrgId]).

%% active 成员可看到自己所属的 active/archived Organization。
-spec page_by_member(integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
page_by_member(Uid, Page, Size) ->
    OrgTb = tablename(),
    MemberTb = organization_member_repo:tablename(),
    From =
        <<" FROM ", OrgTb/binary, " o JOIN ", MemberTb/binary, " om",
            " ON om.organization_id = o.id", " WHERE om.user_id = $1 AND om.status = 'active'">>,
    case elib_pg:one(<<"SELECT COUNT(*) AS count", From/binary>>, [Uid]) of
        {ok, #{<<"count">> := Total}} ->
            Offset = (Page - 1) * Size,
            Sql =
                <<"SELECT o.id,o.name,o.owner_id,o.status,o.branding,o.settings,",
                    "o.created_at,o.updated_at,om.role AS member_role", From/binary,
                    " ORDER BY o.created_at DESC,o.id DESC LIMIT $2 OFFSET $3">>,
            case elib_pg:query(Sql, [Uid, Size, Offset]) of
                {ok, Items} ->
                    {ok, #{
                        list => Items,
                        page => Page,
                        size => Size,
                        total => Total,
                        total_page => total_page(Total, Size)
                    }};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason};
        Other ->
            {error, {unexpected_count_result, Other}}
    end.

-spec one_tx(any(), binary(), list()) -> {ok, map()} | {error, not_found | term()}.
one_tx(Conn, Sql, Params) ->
    case elib_pg:query(Conn, Sql, Params) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

-spec total_page(non_neg_integer(), pos_integer()) -> non_neg_integer().
total_page(0, _Size) -> 0;
total_page(Total, Size) -> ((Total - 1) div Size) + 1.
