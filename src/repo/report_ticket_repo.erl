-module(report_ticket_repo).

-export([tablename/0]).
-export([create/5]).
-export([find_by_id/1]).
-export([page_admin/3]).
-export([resolve/4]).
-export([delete_by_id/1]).

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"report_ticket">>).

-spec create(binary(), integer(), integer(), binary(), binary()) -> {ok, integer()} | {error, any()}.
create(TargetType, TargetId, ReporterUid, Reason, Desc) ->
    Tb = tablename(),
    Id = elib_tsid:generate(report_ticket),
    Sql = <<"INSERT INTO ", Tb/binary,
            " (id, target_type, target_id, reporter_uid, reason, description, status, created_at, updated_at)"
            " VALUES ($1, $2, $3, $4, $5, $6, 0, NOW(), NOW())">>,
    case elib_pg:execute(Sql, [Id, TargetType, TargetId, ReporterUid, Reason, Desc]) of
        {ok, _Count} ->
            {ok, Id};
        {error, ReasonErr} ->
            {error, ReasonErr}
    end.

-spec find_by_id(integer()) -> map() | {error, any()}.
find_by_id(ReportId) ->
    Tb = tablename(),
    Sql = <<"SELECT * FROM ", Tb/binary, " WHERE id = $1 LIMIT 1">>,
    case elib_pg:one(Sql, [ReportId]) of
        {ok, Row} ->
            Row;
        {error, Reason} ->
            {error, Reason}
    end.

-spec page_admin(map(), integer(), integer()) -> {ok, map()} | {error, any()}.
page_admin(Filter, Page, Size) ->
    Tb = tablename(),
    Offset = erlang:max(0, (Page - 1) * Size),
    {WhereSql, Params} = build_where(Filter),
    BaseSql = <<" FROM ", Tb/binary, WhereSql/binary>>,

    LimitArgN = length(Params) + 1,
    OffsetArgN = length(Params) + 2,
    Column = <<"id, target_type, target_id, reporter_uid, reason, description,"
               " status, handled_by, handled_at, created_at, updated_at">>,
    ListSql = <<"SELECT ", Column/binary, BaseSql/binary,
                " ORDER BY id DESC"
                " LIMIT $", (integer_to_binary(LimitArgN))/binary,
                " OFFSET $", (integer_to_binary(OffsetArgN))/binary>>,
    CountSql = <<"SELECT COUNT(*) AS count", BaseSql/binary>>,

    ListParams = Params ++ [Size, Offset],
    case {elib_pg:query(ListSql, ListParams), elib_pg:one(CountSql, Params)} of
        {{ok, Rows}, {ok, #{<<"count">> := Total0}}} ->
            Total = ec_cnv:to_integer(Total0),
            {ok, #{
                list => Rows,
                total => Total,
                page => Page,
                size => Size,
                total_pages => calc_total_pages(Total, Size)
            }};
        {{error, Reason}, _} ->
            {error, Reason};
        {{ok, _}, {error, Reason}} ->
            {error, Reason}
    end.

-spec resolve(integer(), integer(), binary(), integer()) -> {ok, non_neg_integer()} | {error, any()}.
resolve(ReportId, Result, _Note, AdmUid) ->
    Tb = tablename(),
    Sql = <<"UPDATE ", Tb/binary,
            " SET status = $1, handled_by = $2, handled_at = NOW(), updated_at = NOW()"
            " WHERE id = $3">>,
    elib_pg:execute(Sql, [Result, AdmUid, ReportId]).

-spec delete_by_id(integer()) -> {ok, non_neg_integer()} | {error, any()}.
delete_by_id(ReportId) ->
    Tb = tablename(),
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE id = $1">>,
    elib_pg:execute(Sql, [ReportId]).

-spec build_where(map()) -> {binary(), list()}.
build_where(Filter) ->
    Status = maps:get(status, Filter, -1),
    TargetType = maps:get(target_type, Filter, <<>>),
    TargetId = maps:get(target_id, Filter, 0),
    ReporterUid = maps:get(reporter_uid, Filter, 0),
    Keyword = maps:get(keyword, Filter, <<>>),

    {Conds1, Params1, N1} = maybe_add_int_cond(Status >= 0, <<"status">>, Status, [], [], 1),
    {Conds2, Params2, N2} = maybe_add_bin_cond(is_binary(TargetType) andalso TargetType =/= <<>>, <<"target_type">>, TargetType, Conds1, Params1, N1),
    {Conds3, Params3, N3} = maybe_add_int_cond(TargetId > 0, <<"target_id">>, TargetId, Conds2, Params2, N2),
    {Conds4, Params4, N4} = maybe_add_int_cond(ReporterUid > 0, <<"reporter_uid">>, ReporterUid, Conds3, Params3, N3),
    {Conds5, Params5, _N5} = maybe_add_keyword_cond(Keyword, Conds4, Params4, N4),

    case Conds5 of
        [] ->
            {<<>>, Params5};
        _ ->
            {<<" WHERE ", (join_binary(lists:reverse(Conds5), <<" AND ">>))/binary>>, Params5}
    end.

-spec maybe_add_int_cond(boolean(), binary(), integer(), [binary()], list(), integer()) -> {[binary()], list(), integer()}.
maybe_add_int_cond(false, _Field, _Value, Conds, Params, N) ->
    {Conds, Params, N};
maybe_add_int_cond(true, Field, Value, Conds, Params, N) ->
    Cond = <<Field/binary, " = $", (integer_to_binary(N))/binary>>,
    {[Cond | Conds], Params ++ [Value], N + 1}.

-spec maybe_add_bin_cond(boolean(), binary(), binary(), [binary()], list(), integer()) -> {[binary()], list(), integer()}.
maybe_add_bin_cond(false, _Field, _Value, Conds, Params, N) ->
    {Conds, Params, N};
maybe_add_bin_cond(true, Field, Value, Conds, Params, N) ->
    Cond = <<Field/binary, " = $", (integer_to_binary(N))/binary>>,
    {[Cond | Conds], Params ++ [Value], N + 1}.

-spec maybe_add_keyword_cond(term(), [binary()], list(), integer()) -> {[binary()], list(), integer()}.
maybe_add_keyword_cond(Keyword, Conds, Params, N) ->
    Kw = normalize_keyword(Keyword),
    case Kw of
        <<>> ->
            {Conds, Params, N};
        _ ->
            Like = <<"%", Kw/binary, "%">>,
            Cond = <<"(reason ILIKE $", (integer_to_binary(N))/binary,
                     " OR description ILIKE $", (integer_to_binary(N + 1))/binary, ")">>,
            {[Cond | Conds], Params ++ [Like, Like], N + 2}
    end.

-spec normalize_keyword(term()) -> binary().
normalize_keyword(Value) when is_binary(Value) ->
    trim_binary(Value);
normalize_keyword(Value) when is_list(Value) ->
    trim_binary(unicode:characters_to_binary(Value));
normalize_keyword(_) ->
    <<>>.

-spec trim_binary(binary()) -> binary().
trim_binary(Bin) ->
    list_to_binary(string:trim(binary_to_list(Bin))).

-spec join_binary([binary()], binary()) -> binary().
join_binary([], _Sep) ->
    <<>>;
join_binary([One], _Sep) ->
    One;
join_binary([H | T], Sep) ->
    lists:foldl(fun(Item, Acc) -> <<Acc/binary, Sep/binary, Item/binary>> end, H, T).

-spec calc_total_pages(integer(), integer()) -> integer().
calc_total_pages(_Total, Size) when Size =< 0 ->
    0;
calc_total_pages(Total, _Size) when Total =< 0 ->
    0;
calc_total_pages(Total, Size) ->
    (Total + Size - 1) div Size.
