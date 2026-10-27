-module(review_queue_repo).

%%%
% 消息人工复审队列数据仓库模块
% Manual message review queue repository module
%%%

-export([tablename/0]).
-export([page/3]).
-export([find_by_id/1]).
-export([moderate/4]).
%% 导出供测试断言 SQL 构造（R12-BE）
-export([build_where/1]).

-include("common.hrl").
-include("log.hrl").

-define(COLUMNS, <<
    "id, msg_id, msg_type, content, from_id, from_account, to_id, to_type,"
    " hit_words, review_status, reviewer_id, reason, reviewed_at, created_at, metadata"
>>).

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"review_queue">>).

%% @doc 分页查询复审队列
%% Filters:
%%   status(binary) / keyword(binary, 命中 from_account|content)
%%   start / end(binary, YYYY-MM-DD 纯日期，旧参数，保留)
%%   start_at / end_at(binary, rfc3339 瞬间，新参数)
%%   zone(binary, IANA 时区名，仅解释纯日期 start/end；缺省 UTC)
%%
%% R12-BE 时间边界合同：
%%   - 业务日定义 = 按请求 zone 的当地自然日；zone 缺省 'UTC'（与旧实现
%%     在 session TimeZone=UTC 时的语义一致，不静默改变现有报表含义）。
%%   - [start, endExclusive) 半开区间：结束日上界为该 zone 次日 00:00。
%%   - 边界经 `AT TIME ZONE $zone` 构造明确瞬间（timestamptz 域比较），
%%     消除对 PG session TimeZone 的隐含依赖；不用 DATE(created_at) 包装，
%%     保持 created_at 索引可用。
%%   - 优先级/冲突：start_at > start，end_at > end（同时传时忽略纯日期）；
%%     zone 在纯日期模式下生效，对 start_at/end_at 无影响。
-spec page(map(), integer(), integer()) -> {ok, map()} | {error, term()}.
page(Filter, Page, Size) ->
    %% P3（审查发现修复轮）：zone 传入即校验 IANA 合法性（无论纯日期/时刻
    %% 模式，入口早失败）；非法 → {error, invalid_zone}，由 handler 层映射 400。
    %% 此前非法 zone 仅在纯日期边界实际参与 SQL 时由 PG 运行时报错兜住
    %% （浪费 DB 往返 + 报错噪音），时刻模式下被静默忽略。
    case validate_zone_param(maps:get(zone, Filter, undefined)) of
        ok ->
            page_do(Filter, Page, Size);
        {error, _} = Err ->
            Err
    end.

page_do(Filter, Page, Size) ->
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

%% zone 校验（default_zone/1 的上游入口）：合法 IANA 名单以 PG
%% pg_timezone_names 为权威——与 SQL 中 `AT TIME ZONE $zone` 使用同一份
%% tzdata，保证"校验通过则查询必不因 zone 报错"。缺省/空值走 default_zone
%% 的 UTC 兜底语义，不校验。管理端低频列表页，一次 EXISTS 查询开销可忽略。
%% P3 修复：仅 {ok, ok=false/空行} 判 invalid_zone；{error, Reason} 原样
%% 透传——瞬态连接故障不得误报为 zone 非法（handler 泛化分支兜底文案）。
-spec validate_zone_param(term()) -> ok | {error, invalid_zone} | {error, term()}.
validate_zone_param(Zone) when is_binary(Zone), Zone =/= <<>> ->
    Sql = <<"SELECT EXISTS (SELECT 1 FROM pg_timezone_names WHERE name = $1) AS ok">>,
    case elib_pg:one(Sql, [Zone]) of
        {ok, #{<<"ok">> := true}} ->
            ok;
        {ok, #{<<"ok">> := false}} ->
            {error, invalid_zone};
        {ok, #{}} ->
            {error, invalid_zone};
        {error, Reason} ->
            {error, Reason}
    end;
validate_zone_param(_) ->
    ok.

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
    elib_pg:execute_count(Sql, [Status, ReviewerId, Reason, Id]).

%% ===================================================================
%% Internal helpers
%% ===================================================================

-spec build_where(map()) -> {binary(), list()}.
build_where(Filter) ->
    Status = maps:get(status, Filter, undefined),
    Keyword = maps:get(keyword, Filter, undefined),
    StartAt = maps:get(start_at, Filter, undefined),
    EndAt = maps:get(end_at, Filter, undefined),
    Start = maps:get(start, Filter, undefined),
    End = maps:get('end', Filter, undefined),
    Zone = maps:get(zone, Filter, undefined),
    %% R12-BE 优先级：时刻参数 > 纯日期参数；zone 仅在纯日期模式下生效
    {C0, P0, N0} =
        case truthy(StartAt) of
            true -> add_instant_ge(<<"created_at">>, StartAt, [], [], 1);
            false -> add_date_ge(<<"created_at">>, Start, Zone, [], [], 1)
        end,
    {C1, P1, N1} =
        case truthy(EndAt) of
            true -> add_instant_le(<<"created_at">>, EndAt, C0, P0, N0);
            false -> add_date_le(<<"created_at">>, End, Zone, C0, P0, N0)
        end,
    {C2, P2, N2} = add_bin_eq(<<"review_status">>, Status, C1, P1, N1),
    {C3, P3, _N3} = add_keyword(Keyword, C2, P2, N2),

    case C3 of
        [] -> {<<>>, P3};
        _ -> {<<" WHERE ", (join_and(lists:reverse(C3)))/binary>>, P3}
    end.

-spec truthy(term()) -> boolean().
truthy(V) when V =:= undefined; V =:= <<>> -> false;
truthy(_) -> true.

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

%% rfc3339 瞬间参数：直接与 timestamptz 列比较（明确瞬间边界，最高优先级）
add_instant_ge(Field, V, Conds, Params, N) ->
    Cond = <<Field/binary, " >= $", (integer_to_binary(N))/binary, "::text::timestamptz">>,
    {[Cond | Conds], Params ++ [V], N + 1}.

add_instant_le(Field, V, Conds, Params, N) ->
    Cond = <<Field/binary, " < $", (integer_to_binary(N))/binary, "::text::timestamptz">>,
    {[Cond | Conds], Params ++ [V], N + 1}.

%% 纯日期参数：经 AT TIME ZONE $zone 构造该时区当地日 00:00 的明确瞬间，
%% 消除对 PG session TimeZone 的隐含依赖（旧形式 `>= $N::date` 按 session zone 解释）。
add_date_ge(_Field, V, _Zone, Conds, Params, N) when V =:= undefined; V =:= <<>> ->
    {Conds, Params, N};
add_date_ge(Field, V, Zone, Conds, Params, N) when is_binary(V) ->
    Z = default_zone(Zone),
    Cond =
        <<Field/binary, " >= (($", (integer_to_binary(N))/binary,
            "::text)::date)::timestamp AT TIME ZONE $", (integer_to_binary(N + 1))/binary>>,
    {[Cond | Conds], Params ++ [V, Z], N + 2};
add_date_ge(_Field, _V, _Zone, Conds, Params, N) ->
    {Conds, Params, N}.

%% [start, endExclusive)：结束日上界 = 该时区当地日次日 00:00（半开区间）
add_date_le(_Field, V, _Zone, Conds, Params, N) when V =:= undefined; V =:= <<>> ->
    {Conds, Params, N};
add_date_le(Field, V, Zone, Conds, Params, N) when is_binary(V) ->
    Z = default_zone(Zone),
    Cond =
        <<Field/binary, " < ((($", (integer_to_binary(N))/binary,
            "::text)::date + 1)::timestamp AT TIME ZONE $", (integer_to_binary(N + 1))/binary,
            ")">>,
    {[Cond | Conds], Params ++ [V, Z], N + 2};
add_date_le(_Field, _V, _Zone, Conds, Params, N) ->
    {Conds, Params, N}.

%% zone 缺省 UTC：与旧实现（session TimeZone=UTC 时）语义一致，不静默改变报表含义。
%% P3（审查发现修复轮）：IANA 合法性校验上移至 page/3 入口
%% （validate_zone_param/1，pg_timezone_names 权威名单）；本函数保持纯
%% fallback 语义，仅对缺省/空值兜底 UTC，非法值到不了这里。
-spec default_zone(term()) -> binary().
default_zone(Z) when is_binary(Z), Z =/= <<>> ->
    Z;
default_zone(_) ->
    <<"UTC">>.

-spec join_and([binary()]) -> binary().
join_and([One]) ->
    One;
join_and([H | T]) ->
    lists:foldl(fun(Item, Acc) -> <<Acc/binary, " AND ", Item/binary>> end, H, T).

-spec calc_total_pages(integer(), integer()) -> integer().
calc_total_pages(_Total, Size) when Size =< 0 -> 0;
calc_total_pages(Total, _Size) when Total =< 0 -> 0;
calc_total_pages(Total, Size) -> (Total + Size - 1) div Size.
