-module(review_queue_repo).

%%%
% 消息人工复审队列数据仓库模块
% Manual message review queue repository module
%%%

-export([tablename/0]).
-export([page/3]).
-export([find_by_id/1]).
-export([moderate/4]).

-include("common.hrl").
-include("log.hrl").

-define(COLUMNS, <<
    "id, msg_id, msg_type, content, from_id, from_account, to_id, to_type,"
    " hit_words, review_status, reviewer_id, reason, reviewed_at, created_at"
>>).

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"review_queue">>).

%% @doc 分页查询复审队列
%% Filters: status(binary) / keyword(binary, 命中 from_account|content) / start / end(YYYY-MM-DD)
-spec page(map(), integer(), integer()) -> {ok, map()} | {error, term()}.
page(Filter, Page, Size) ->
    Tb = tablename(),
    Offset = erlang:max(0, (Page - 1) * Size),
    {WhereSql, Params} = build_where(Filter),
    BaseSql = <<" FROM ", Tb/binary, WhereSql/binary>>,
    LimitArgN = length(Params) + 1,
    OffsetArgN = length(Params) + 2,
    ListSql =
        <<"SELECT ", ?COLUMNS/binary, BaseSql/binary,
            " ORDER BY id DESC"
            " LIMIT $", (integer_to_binary(LimitArgN))/binary, " OFFSET $",
            (integer_to_binary(OffsetArgN))/binary>>,
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

-spec find_by_id(integer()) -> {ok, map()} | {error, term()}.
find_by_id(Id) ->
    Tb = tablename(),
    Sql = <<"SELECT ", ?COLUMNS/binary, " FROM ", Tb/binary, " WHERE id = $1 LIMIT 1">>,
    elib_pg:one(Sql, [Id]).

%% @doc 执行复审（仅 pending 可流转）。返回受影响行数。
-spec moderate(integer(), binary(), binary() | undefined, integer()) ->
    {ok, non_neg_integer()} | {error, term()}.
moderate(Id, Status, Reason, ReviewerId) ->
    Tb = tablename(),
    Sql =
        <<"UPDATE ", Tb/binary,
            " SET review_status = $1, reviewer_id = $2, reason = $3, reviewed_at = NOW()"
            " WHERE id = $4 AND review_status = 'pending'">>,
    elib_pg:execute(Sql, [Status, ReviewerId, Reason, Id]).

%% ===================================================================
%% Internal helpers
%% ===================================================================

-spec build_where(map()) -> {binary(), list()}.
build_where(Filter) ->
    Status = maps:get(status, Filter, undefined),
    Keyword = maps:get(keyword, Filter, undefined),
    Start = maps:get(start, Filter, undefined),
    End = maps:get('end', Filter, undefined),

    {C1, P1, N1} = add_bin_eq(<<"review_status">>, Status, [], [], 1),
    {C2, P2, N2} = add_keyword(Keyword, C1, P1, N1),
    {C3, P3, N3} = add_date_ge(<<"created_at">>, Start, C2, P2, N2),
    {C4, P4, _N4} = add_date_le(<<"created_at">>, End, C3, P3, N3),

    case C4 of
        [] -> {<<>>, P4};
        _ -> {<<" WHERE ", (join_and(lists:reverse(C4)))/binary>>, P4}
    end.

add_bin_eq(_Field, V, Conds, Params, N) when V =:= undefined; V =:= <<>> ->
    {Conds, Params, N};
add_bin_eq(Field, V, Conds, Params, N) when is_binary(V) ->
    Cond = <<Field/binary, " = $", (integer_to_binary(N))/binary>>,
    {[Cond | Conds], Params ++ [V], N + 1};
add_bin_eq(_Field, _V, Conds, Params, N) ->
    {Conds, Params, N}.

add_keyword(V, Conds, Params, N) when V =:= undefined; V =:= <<>> ->
    {Conds, Params, N};
add_keyword(V, Conds, Params, N) when is_binary(V) ->
    Like = <<"%", (elib_pg:escape_like(V))/binary, "%">>,
    Cond =
        <<"(from_account ILIKE $", (integer_to_binary(N))/binary, " OR content ILIKE $",
            (integer_to_binary(N + 1))/binary, ")">>,
    {[Cond | Conds], Params ++ [Like, Like], N + 2};
add_keyword(_V, Conds, Params, N) ->
    {Conds, Params, N}.

add_date_ge(_Field, V, Conds, Params, N) when V =:= undefined; V =:= <<>> ->
    {Conds, Params, N};
add_date_ge(Field, V, Conds, Params, N) when is_binary(V) ->
    Cond = <<Field/binary, " >= $", (integer_to_binary(N))/binary, "::date">>,
    {[Cond | Conds], Params ++ [V], N + 1};
add_date_ge(_Field, _V, Conds, Params, N) ->
    {Conds, Params, N}.

add_date_le(_Field, V, Conds, Params, N) when V =:= undefined; V =:= <<>> ->
    {Conds, Params, N};
add_date_le(Field, V, Conds, Params, N) when is_binary(V) ->
    %% 含当日：< (end + 1 day)
    Cond = <<Field/binary, " < ($", (integer_to_binary(N))/binary, "::date + INTERVAL '1 day')">>,
    {[Cond | Conds], Params ++ [V], N + 1};
add_date_le(_Field, _V, Conds, Params, N) ->
    {Conds, Params, N}.

-spec join_and([binary()]) -> binary().
join_and([One]) ->
    One;
join_and([H | T]) ->
    lists:foldl(fun(Item, Acc) -> <<Acc/binary, " AND ", Item/binary>> end, H, T).

-spec calc_total_pages(integer(), integer()) -> integer().
calc_total_pages(_Total, Size) when Size =< 0 -> 0;
calc_total_pages(Total, _Size) when Total =< 0 -> 0;
calc_total_pages(Total, Size) -> (Total + Size - 1) div Size.
