-module(ai_agent_role_repo).

%% AI Agent 角色模板及版本仓储。
%% 角色编码是稳定业务标识；版本内容只在发布后供运行时继承。

-export([
    tablename/0,
    version_tablename/0,
    page/3,
    find/1,
    find_published/1,
    create/1,
    update_metadata/2,
    save_draft/2,
    publish/3,
    set_status/2,
    count_bound_agents/1,
    count_published_version/1
]).

-include("log.hrl").

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"ai_agent_role">>).

-spec version_tablename() -> binary().
version_tablename() ->
    elib_pg_sql:public_tablename(<<"ai_agent_role_version">>).

-spec page(pos_integer(), pos_integer(), map()) -> {ok, map()} | {error, term()}.
page(Page, Size, Filters) when Page > 0, Size > 0, is_map(Filters) ->
    Tb = tablename(),
    AgentTb = elib_pg_sql:public_tablename(<<"ai_agent">>),
    {Where, Params} = build_where(Filters),
    CountSql = <<"SELECT count(*) AS total FROM ", Tb/binary, " r", Where/binary>>,
    case elib_pg:query(CountSql, Params) of
        {ok, [#{<<"total">> := 0} | _]} ->
            {ok, empty_page(Page, Size)};
        {ok, [#{<<"total">> := Total} | _]} ->
            Offset = (Page - 1) * Size,
            ListSql =
                <<
                    "SELECT r.code, r.name, r.description, r.status, r.active_version,"
                    " r.created_by, r.created_at, r.updated_at,"
                    " COUNT(a.user_id) AS bound_agent_count"
                    " FROM ",
                    Tb/binary,
                    " r LEFT JOIN ",
                    AgentTb/binary,
                    " a ON a.role_id = r.code",
                    Where/binary,
                    " GROUP BY r.code, r.name, r.description, r.status, r.active_version,"
                    " r.created_by, r.created_at, r.updated_at"
                    " ORDER BY r.updated_at DESC LIMIT ",
                    (integer_to_binary(Size))/binary,
                    " OFFSET ",
                    (integer_to_binary(Offset))/binary
                >>,
            case elib_pg:query(ListSql, Params) of
                {ok, Rows} ->
                    {ok, #{total => Total, page => Page, size => Size, list => Rows}};
                {error, Reason} ->
                    ?ERROR_LOG("ai_agent_role_repo:page list error ~p~n", [Reason]),
                    {error, Reason}
            end;
        {ok, []} ->
            {ok, empty_page(Page, Size)};
        {error, Reason} ->
            ?ERROR_LOG("ai_agent_role_repo:page count error ~p~n", [Reason]),
            {error, Reason}
    end.

-spec find(binary()) -> {ok, map()} | {error, notfound | term()}.
find(Code) ->
    Tb = tablename(),
    VersionTb = version_tablename(),
    Sql =
        <<
            "SELECT r.code, r.name, r.description, r.status, r.active_version,"
            " r.created_by, r.created_at, r.updated_at,"
            " COALESCE(d.id, v.id) AS version_id,"
            " COALESCE(d.version, v.version) AS version,"
            " COALESCE(d.state, v.state) AS state,"
            " COALESCE(d.system_prompt, v.system_prompt) AS system_prompt,"
            " COALESCE(d.capabilities, v.capabilities) AS capabilities,"
            " COALESCE(d.knowledge_policy, v.knowledge_policy) AS knowledge_policy,"
            " COALESCE(d.created_by, v.created_by) AS version_created_by,"
            " COALESCE(d.published_by, v.published_by) AS published_by,"
            " COALESCE(d.created_at, v.created_at) AS version_created_at,"
            " COALESCE(d.published_at, v.published_at) AS published_at"
            " FROM ",
            Tb/binary,
            " r LEFT JOIN ",
            VersionTb/binary,
            " v ON v.role_code = r.code AND v.version = r.active_version"
            " LEFT JOIN LATERAL ("
            " SELECT d.id, d.version, d.state, d.system_prompt, d.capabilities,"
            " d.knowledge_policy, d.created_by, d.published_by, d.created_at, d.published_at"
            " FROM ",
            VersionTb/binary,
            " d WHERE d.role_code = r.code AND d.state = 'draft'"
            " ORDER BY d.version DESC LIMIT 1"
            " ) d ON TRUE"
            " WHERE r.code = $1"
        >>,
    case elib_pg:query(Sql, [Code]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, notfound};
        {error, Reason} -> {error, Reason}
    end.

-spec find_published(binary()) -> {ok, map()} | {error, notfound | term()}.
find_published(Code) ->
    Tb = tablename(),
    VersionTb = version_tablename(),
    Sql =
        <<
            "SELECT r.code, r.name, r.description, r.status, r.active_version,"
            " r.created_by, r.created_at, r.updated_at,"
            " v.id AS version_id, v.version, v.state, v.system_prompt,"
            " v.capabilities, v.knowledge_policy, v.created_by AS version_created_by,"
            " v.published_by, v.created_at AS version_created_at, v.published_at"
            " FROM ",
            Tb/binary,
            " r LEFT JOIN ",
            VersionTb/binary,
            " v ON v.role_code = r.code"
            " AND v.version = r.active_version"
            " AND v.state = 'published'"
            " WHERE r.code = $1"
        >>,
    case elib_pg:query(Sql, [Code]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, notfound};
        {error, Reason} -> {error, Reason}
    end.

-spec create(map()) -> {ok, map()} | {error, term()}.
create(Data) ->
    Tb = tablename(),
    Sql =
        <<"INSERT INTO ", Tb/binary,
            " (code, name, description, status, created_by, created_at, updated_at)"
            " VALUES ($1,$2,$3,$4,$5,NOW(),NOW())"
            " RETURNING code, name, description, status, active_version">>,
    Params = [
        maps:get(code, Data),
        maps:get(name, Data),
        maps:get(description, Data, <<>>),
        maps:get(status, Data, 1),
        maps:get(created_by, Data, 0)
    ],
    case elib_pg:query(Sql, Params) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, create_empty_result};
        {error, Reason} -> {error, Reason}
    end.

-spec update_metadata(binary(), map()) -> {ok, term()} | {error, term()}.
update_metadata(Code, Patch) when is_binary(Code), is_map(Patch) ->
    Tb = tablename(),
    Allowed = maps:with([name, description], Patch),
    case map_size(Allowed) of
        0 ->
            {ok, 0};
        _ ->
            elib_pg:update(Tb, Allowed#{updated_at => elib_dt:now()}, <<"code = $1">>, [Code])
    end.

-spec save_draft(binary(), map()) -> {ok, [map()]} | {error, term()}.
save_draft(Code, Data) ->
    VersionTb = version_tablename(),
    Id = maps:get(id, Data, elib_tsid:generate(ai_agent_role_version)),
    Version = maps:get(version, Data),
    Sql =
        <<"INSERT INTO ", VersionTb/binary,
            " (id, role_code, version, state, system_prompt, capabilities,"
            " knowledge_policy, created_by, created_at)"
            " VALUES ($1,$2,$3,'draft',$4,$5::jsonb,$6::jsonb,$7,NOW())"
            " ON CONFLICT (role_code, version) DO UPDATE SET"
            " state = 'draft', system_prompt = EXCLUDED.system_prompt,"
            " capabilities = EXCLUDED.capabilities,"
            " knowledge_policy = EXCLUDED.knowledge_policy,"
            " created_by = EXCLUDED.created_by"
            " RETURNING id, role_code, version, state">>,
    Params = [
        Id,
        Code,
        Version,
        maps:get(system_prompt, Data),
        maps:get(capabilities, Data, <<"{}">>),
        maps:get(knowledge_policy, Data, <<"{}">>),
        maps:get(created_by, Data, 0)
    ],
    Result = elib_pg:query(Sql, Params),
    case Result of
        {ok, _} -> audit_event(role_draft_saved, #{role_code => Code, version => Version});
        _ -> ok
    end,
    Result.

-spec publish(binary(), pos_integer(), integer()) -> {ok, term()} | {error, term()}.
publish(Code, Version, PublishedBy) ->
    Result = elib_pg:with_tx(fun(Conn) -> publish_tx(Conn, Code, Version, PublishedBy) end),
    case Result of
        {rollback, Reason} ->
            audit_event(role_publish_failed, #{
                role_code => Code,
                version => Version,
                reason => Reason
            }),
            {error, Reason};
        {ok, Value} ->
            audit_event(role_published, #{
                role_code => Code,
                version => Version,
                published_by => PublishedBy
            }),
            {ok, Value};
        {error, Reason} ->
            {error, Reason}
    end.

publish_tx(Conn, Code, Version, PublishedBy) ->
    VersionTb = version_tablename(),
    RoleTb = tablename(),
    ArchiveSql =
        <<"UPDATE ", VersionTb/binary,
            " SET state = 'archived'"
            " WHERE role_code = $1 AND state = 'published' AND version <> $2">>,
    PublishSql =
        <<"UPDATE ", VersionTb/binary,
            " SET state = 'published', published_by = $3, published_at = NOW()"
            " WHERE role_code = $1 AND version = $2 AND state = 'draft'">>,
    RoleSql =
        <<"UPDATE ", RoleTb/binary,
            " SET active_version = $2, updated_at = NOW() WHERE code = $1">>,
    case elib_pg:query(Conn, ArchiveSql, [Code, Version]) of
        {ok, _} ->
            case elib_pg:query(Conn, PublishSql, [Code, Version, PublishedBy]) of
                {ok, []} ->
                    throw({rollback, draft_not_found});
                {ok, _} ->
                    elib_pg:query(Conn, RoleSql, [Code, Version]);
                {error, Reason} ->
                    throw({rollback, Reason})
            end;
        {error, Reason} ->
            throw({rollback, Reason})
    end.

-spec set_status(binary(), 0 | 1) -> {ok, term()} | {error, term()}.
set_status(Code, Status) ->
    Tb = tablename(),
    elib_pg:update(Tb, #{status => Status, updated_at => elib_dt:now()}, <<"code = $1">>, [Code]).

-spec count_bound_agents(binary()) -> {ok, non_neg_integer()} | {error, term()}.
count_bound_agents(Code) ->
    AgentTb = elib_pg_sql:public_tablename(<<"ai_agent">>),
    Sql = <<"SELECT count(*) AS total FROM ", AgentTb/binary, " WHERE role_id = $1">>,
    case elib_pg:query(Sql, [Code]) of
        {ok, [#{<<"total">> := Total} | _]} -> {ok, Total};
        {ok, []} -> {ok, 0};
        {error, Reason} -> {error, Reason}
    end.

-spec count_published_version(binary()) -> {ok, non_neg_integer()} | {error, term()}.
count_published_version(Code) ->
    VersionTb = version_tablename(),
    Sql =
        <<"SELECT version FROM ", VersionTb/binary,
            " WHERE role_code = $1 AND state = 'published' LIMIT 1">>,
    case elib_pg:query(Sql, [Code]) of
        {ok, [#{<<"version">> := Version} | _]} -> {ok, Version};
        {ok, []} -> {ok, 0};
        {error, Reason} -> {error, Reason}
    end.

build_where(Filters) ->
    Keyword = maps:get(keyword, Filters, <<>>),
    Status = maps:get(status, Filters, undefined),
    case {Keyword, Status} of
        {<<>>, undefined} ->
            {<<>>, []};
        {<<>>, _} ->
            {<<" WHERE r.status = $1">>, [Status]};
        {_, undefined} ->
            Like = <<"%", (elib_pg:escape_like(Keyword))/binary, "%">>,
            {<<" WHERE (r.code ILIKE $1 OR r.name ILIKE $1)">>, [Like]};
        {_, _} ->
            Like = <<"%", (elib_pg:escape_like(Keyword))/binary, "%">>,
            {<<" WHERE (r.code ILIKE $1 OR r.name ILIKE $1) AND r.status = $2">>, [
                Like, Status
            ]}
    end.

empty_page(Page, Size) ->
    #{total => 0, page => Page, size => Size, list => []}.

audit_event(Name, Details) ->
    try
        ?INFO_LOG([Name, Details])
    catch
        _:_ -> ok
    end,
    ok.
