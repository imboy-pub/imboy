-module(moderation_appeal_repo).

%% R-04：申诉行仓储。申诉自身事实（pending/accepted/rejected）全在本表；
%% 翻案的执行语义在 moderation_action_logic:reverse（本表只落终审记录）。

-export([tablename/0]).
-export([insert/1]).
-export([find_by_id/1]).
-export([find_by_action_appellant/2]).
-export([list_by_appellant/1]).
-export([list_page/3]).
-export([mark_reviewed/5]).

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"moderation_appeal">>).

%% @doc 插入申诉行（pending）。零行 RETURNING 报错而非假装成功（fail-closed）。
-spec insert(map()) -> {ok, map()} | {error, binary()}.
insert(#{action_id := ActionId, appellant_uid := AppellantUid} = A) ->
    Tb = tablename(),
    Id = elib_tsid:generate(moderation_appeal),
    CaseId = maps:get(case_id, A, 0),
    Reason = maps:get(reason, A, <<>>),
    Sql =
        <<"INSERT INTO ", Tb/binary,
            " (id, action_id, case_id, appellant_uid, reason, status)"
            " VALUES ($1, $2, $3, $4, $5, 'pending')"
            " RETURNING id, action_id, case_id, appellant_uid, reason, status,"
            " reviewer_id, review_reason, reviewed_at, created_at">>,
    case elib_pg:query(Sql, [Id, ActionId, CaseId, AppellantUid, Reason]) of
        {ok, [Row]} ->
            {ok, Row};
        {ok, []} ->
            {error, <<"申诉写入失败"/utf8>>};
        {error, Reason0} ->
            {error, Reason0}
    end.

-spec find_by_id(integer()) -> {ok, map()} | {error, not_found}.
find_by_id(Id) ->
    Tb = tablename(),
    Sql =
        <<
            "SELECT id, action_id, case_id, appellant_uid, reason, status,"
            " reviewer_id, review_reason, reviewed_at, created_at"
            " FROM ",
            Tb/binary,
            " WHERE id = $1"
        >>,
    case elib_pg:query(Sql, [Id]) of
        {ok, [Row]} -> {ok, Row};
        _ -> {error, not_found}
    end.

-spec find_by_action_appellant(integer(), integer()) -> {ok, map()} | {error, not_found}.
find_by_action_appellant(ActionId, AppellantUid) ->
    Tb = tablename(),
    Sql =
        <<
            "SELECT id, action_id, case_id, appellant_uid, reason, status,"
            " reviewer_id, review_reason, reviewed_at, created_at"
            " FROM ",
            Tb/binary,
            " WHERE action_id = $1 AND appellant_uid = $2"
        >>,
    case elib_pg:query(Sql, [ActionId, AppellantUid]) of
        {ok, [Row]} -> {ok, Row};
        _ -> {error, not_found}
    end.

-spec list_by_appellant(integer()) -> {ok, [map()]} | {error, term()}.
list_by_appellant(AppellantUid) ->
    Tb = tablename(),
    Sql =
        <<
            "SELECT id, action_id, reason, status, review_reason, reviewed_at, created_at"
            " FROM ",
            Tb/binary,
            " WHERE appellant_uid = $1 ORDER BY created_at DESC LIMIT 100"
        >>,
    elib_pg:query(Sql, [AppellantUid]).

%% @doc Admin 分页（可选 status 筛选）。含 appellant_uid/case_id（admin 面可见）。
-spec list_page(integer(), integer(), binary() | undefined) ->
    {ok, [map()]} | {error, term()}.
list_page(Page, Size, Status) ->
    Tb = tablename(),
    Offset = (Page - 1) * Size,
    {Where, Params} =
        case is_binary(Status) andalso Status =/= <<>> of
            true -> {<<" WHERE status = $1">>, [Status]};
            false -> {<<>>, []}
        end,
    Sql =
        <<
            "SELECT id, action_id, case_id, appellant_uid, reason, status,"
            " reviewer_id, review_reason, reviewed_at, created_at"
            " FROM ",
            Tb/binary,
            Where,
            " ORDER BY created_at DESC"
            " LIMIT $",
            (integer_to_binary(length(Params) + 1))/binary,
            " OFFSET $",
            (integer_to_binary(length(Params) + 2))/binary
        >>,
    elib_pg:query(Sql, Params ++ [Size, Offset]).

%% @doc 终审落库：pending → accepted | rejected。
-spec mark_reviewed(integer(), binary(), integer(), binary(), integer()) ->
    {ok, integer()} | {error, term()}.
mark_reviewed(Id, Verdict, ReviewerId, ReviewReason, NowTs) when
    Verdict =:= <<"accepted">> orelse Verdict =:= <<"rejected">>
->
    Tb = tablename(),
    Sql =
        <<"UPDATE ", Tb/binary,
            " SET status = $2, reviewer_id = $3, review_reason = $4,"
            " reviewed_at = to_timestamp($5 / 1000.0), updated_at = NOW()"
            " WHERE id = $1 AND status = 'pending'">>,
    case elib_pg:query(Sql, [Id, Verdict, ReviewerId, ReviewReason, NowTs]) of
        {ok, _} -> {ok, 1};
        {error, Reason} -> {error, Reason}
    end.
