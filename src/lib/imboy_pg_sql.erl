%%%===================================================================
%%% @doc
%%% SQL 构造辅助模块（纯函数）
%%%
%%% 设计原则：
%%% - 只负责 SQL 结构拼装
%%% - 永远返回 {Sql, Params}
%%% - 不执行 SQL
%%% - 不接触 epgsql / pooler
%%%===================================================================
-module(imboy_pg_sql).
-include("log.hrl").

-export([
    public_tablename/1,
    insert/3,
    insert_with_params/4,
    update/4,
    insert_batch/3,
    select/2,
    build_select/4,
    build_where_clause/1,
    page/6,
    value_or_empty/1,
    placeholders/1,
    parse_result/1
]).

public_tablename(<<"public.", Tb/binary>>) ->
    public_tablename(Tb);
public_tablename(Tb) ->
    case config_ds:env(sql_driver) of
        pgsql ->
            <<"public.", (ec_cnv:to_binary(Tb))/binary>>;
        _ ->
            Tb
    end.

%%--------------------------------------------------------------------
%% @doc
%% 构造 INSERT SQL
%% Map 的 key 可以是字段名 atom 或 binary，value 为 参数值
%% 支持 {raw, Value} 语法，直接插入 SQL 表达式而不参数化
%%--------------------------------------------------------------------
-spec insert(binary(), map(), binary()) ->
    {iodata(), [term()]}.
insert(Table, Map, RETURNING) when is_map(Map) ->
    {Cols, Vals, RawClauses} = unzip_map(Map),
    % ?DEBUG_LOG(["Vals ", length(Vals), Vals]),
    % 构建 VALUES 子句，处理 raw 值和参数值
    % 将 ? 替换为 $1, $2, ... 等参数占位符
    ParamNum = 1,
    {ValuesClauses, _} = lists:mapfoldl(fun(Clause, Num) ->
        case Clause of
            <<"?">> ->
                {<<"$", (integer_to_binary(Num))/binary>>, Num + 1};
            RawSql ->
                {RawSql, Num}
        end
    end, ParamNum, RawClauses),
    ValuesSql = lists:join(<<",">>, ValuesClauses),
    Sql = [
        <<"INSERT INTO ">>, Table,
        <<" (">>, join_cols(Cols), <<") VALUES (">>,
        ValuesSql, <<") ", RETURNING/binary>>
    ],
    {Sql, Vals}.

%%--------------------------------------------------------------------
%% @doc
%% 构造 INSERT SQL，支持额外的前置参数
%% 用于 raw SQL 中包含占位符的情况
%% ExtraParams: raw SQL 中使用的参数，会放在参数列表的最前面
%%--------------------------------------------------------------------
-spec insert_with_params(binary(), map(), binary(), [term()]) ->
    {iodata(), [term()]}.
insert_with_params(Table, Map, RETURNING, ExtraParams) when is_map(Map) ->
    {Cols, Vals, RawClauses} = unzip_map(Map),
    % 构建 VALUES 子句，处理 raw 值和参数值
    % 参数编号从 ExtraParams 的数量 + 1 开始
    ParamNum = length(ExtraParams) + 1,
    {ValuesClauses, _} = lists:mapfoldl(fun(Clause, Num) ->
        case Clause of
            <<"?">> ->
                {<<"$", (integer_to_binary(Num))/binary>>, Num + 1};
            RawSql ->
                {RawSql, Num}
        end
    end, ParamNum, RawClauses),
    ValuesSql = lists:join(<<",">>, ValuesClauses),
    Sql = [
        <<"INSERT INTO ">>, Table,
        <<" (">>, join_cols(Cols), <<") VALUES (">>,
        ValuesSql, <<") ", RETURNING/binary>>
    ],
    {Sql, ExtraParams ++ Vals}.


%%--------------------------------------------------------------------
%% @doc
%% 构造 UPDATE SQL（支持 WHERE 参数化）
%% WhereSql 可以包含占位符（如 $1, $2），WhereParams 是对应的参数值
%% SET 子句的参数会接在 WHERE 参数之后
%% 支持 {raw, Value} 语法，直接插入 SQL 表达式而不参数化
%%--------------------------------------------------------------------
-spec update(binary(), map(), iodata(), [term()]) ->
    {iodata(), [term()]}.
update(Table, Map, WhereSql, WhereParams) when is_map(Map) ->
    {Cols, SetVals, RawClauses} = unzip_map(Map),
    % SET 子句的占位符编号从 (WHERE 参数数量 + 1) 开始
    SetOffset = length(WhereParams),
    {SetSql, _} = join_set_with_offset(Cols, RawClauses, SetOffset),
    Sql = [
        <<"UPDATE ">>, Table,
        <<" SET ">>, SetSql,
        <<" WHERE ">>, WhereSql
    ],
    {Sql, WhereParams ++ SetVals}.

%%--------------------------------------------------------------------
%% @doc
%% 构造批量 INSERT
%% Cols 可以是 atom 或 binary 类型的字段名列表
%% Rows 为 [[V1,V2,...],[V1,V2,...]]
%%--------------------------------------------------------------------
-spec insert_batch(binary(), [atom() | binary()], [[term()]]) ->
    {iodata(), [term()]}.
insert_batch(Table, Cols, Rows) ->
    NumCols = length(Cols),
    % 为每行生成独立的参数占位符
    % 第1行: $1,$2,$3,$4
    % 第2行: $5,$6,$7,$8
    % 第3行: $9,$10,$11,$12
    ValuesSql = lists:foldl(
        fun(_Row, {SqlAcc, Offset}) ->
            Group = placeholders_with_offset(NumCols, Offset),
            NewSql = case SqlAcc of
                <<>> -> <<"(", Group/binary, ")">>;
                _ -> <<SqlAcc/binary, ",(", Group/binary, ")">>
            end,
            {NewSql, Offset + NumCols}
        end,
        {<<>>, 1},
        Rows
    ),
    Sql = [
        <<"INSERT INTO ">>, Table,
        <<" (">>, join_cols(Cols), <<") VALUES ">>,
        element(1, ValuesSql)
    ],
    {Sql, lists:flatten(Rows)}.

%%--------------------------------------------------------------------
%% @doc
%% 构造简单 SELECT *
%%--------------------------------------------------------------------
-spec select(binary(), iodata()) ->
    iodata().
select(Table, WhereSql) ->
    [
        <<"SELECT * FROM ">>, Table,
        <<" WHERE ">>, WhereSql
    ].

%%--------------------------------------------------------------------
%% @doc
%% 构造动态 SELECT 查询，支持复杂的 WHERE 条件和选项
%%--------------------------------------------------------------------
-spec build_select(binary(), [binary()], map(), map()) ->
    {iodata(), [term()]}.
build_select(Table, Fields, Where, Opts) ->
    % 构建字段列表
    FieldsSql = case Fields of
        Bin when is_binary(Bin) -> Bin;
        List when is_list(List) -> binary:join(List, <<",">>)
    end,

    % 构建 WHERE 条件
    {WhereSql, Params} = build_where_clause(Where),

    % 构建 ORDER BY
    OrderBySql = case maps:get(order_by, Opts, undefined) of
        undefined -> <<>>;
        OrderBy ->
            OrderByClauses = lists:map(fun({Field, Direction}) ->
                FieldBin = case is_atom(Field) of
                    true -> atom_to_binary(Field, utf8);
                    false -> Field
                end,
                DirBin = case Direction of
                    asc -> <<"ASC">>;
                    desc -> <<"DESC">>;
                    _ -> Direction
                end,
                <<FieldBin/binary, " ", DirBin/binary>>
            end, OrderBy),
            <<" ORDER BY ", (lists:join(<<",">>, OrderByClauses))/binary>>
    end,

    % 构建 LIMIT 和 OFFSET，以及对应的参数
    {LimitSql, OffsetSql, FinalParams} = case {maps:get(limit, Opts, undefined), maps:get(offset, Opts, undefined)} of
        {undefined, undefined} ->
            {<<>>, <<>>, Params};
        {Limit, undefined} when Limit =/= undefined ->
            ParamNum = length(Params) + 1,
            {<<" LIMIT $", (integer_to_binary(ParamNum))/binary>>, <<>>, Params ++ [Limit]};
        {undefined, Offset} when Offset =/= undefined ->
            ParamNum = length(Params) + 1,
            {<<>>, <<" OFFSET $", (integer_to_binary(ParamNum))/binary>>, Params ++ [Offset]};
        {Limit, Offset} when Limit =/= undefined, Offset =/= undefined ->
            LimitParamNum = length(Params) + 1,
            OffsetParamNum = length(Params) + 2,
            {<<" LIMIT $", (integer_to_binary(LimitParamNum))/binary>>,
             <<" OFFSET $", (integer_to_binary(OffsetParamNum))/binary>>,
             Params ++ [Limit, Offset]}
    end,

    Sql = [
        <<"SELECT ">>, FieldsSql, <<" FROM ">>, Table,
        WhereSql, OrderBySql, LimitSql, OffsetSql
    ],

    {Sql, FinalParams}.

%% ================== internal helpers ==================

%% @doc 构建单个条件表达式
%% 返回 {SqlFragment, Params}
build_condition_clause(Field, Value, ParamOffset) ->
    FieldBin = field_to_binary(Field),
    case Value of
        {raw, RawSql} ->
            {<<FieldBin/binary, " ", RawSql/binary>>, []};
        {in, List} when is_list(List) ->
            ParamEnd = ParamOffset + length(List) - 1,
            ParamPlaceholders = lists:join(<<",">>,
                [<<"$", (integer_to_binary(I))/binary>> || I <- lists:seq(ParamOffset, ParamEnd)]),
            {<<FieldBin/binary, " IN (", ParamPlaceholders/binary, ")">>, List};
        {not_in, List} when is_list(List) ->
            ParamEnd = ParamOffset + length(List) - 1,
            ParamPlaceholders = lists:join(<<",">>,
                [<<"$", (integer_to_binary(I))/binary>> || I <- lists:seq(ParamOffset, ParamEnd)]),
            {<<FieldBin/binary, " NOT IN (", ParamPlaceholders/binary, ")">>, List};
        {op, Op, Val} ->
            {<<FieldBin/binary, " ", Op/binary, " $", (integer_to_binary(ParamOffset))/binary>>, [Val]};
        _ ->
            {<<FieldBin/binary, " = $", (integer_to_binary(ParamOffset))/binary>>, [Value]}
    end.

%% @doc 构建 map 条件（AND 连接），返回 {SqlFragment, Params}
build_map_conditions(WhereMap, ParamOffset) ->
    % 过滤特殊键
    FilteredMap = maps:without([<<"__or">>, <<"__and">>], WhereMap),
    {Clauses, Params} = maps:fold(fun(Field, Value, {ClausesAcc, ParamsAcc}) ->
        {Clause, ClauseParams} = build_condition_clause(Field, Value, ParamOffset + length(ParamsAcc)),
        {[Clause | ClausesAcc], ParamsAcc ++ ClauseParams}
    end, {[], []}, FilteredMap),
    Sql = iolist_to_binary(lists:join(<<" AND ">>, lists:reverse(Clauses))),
    {Sql, Params}.

%% @doc 构建条件组（支持 OR/AND 嵌套），返回 {SqlFragment, Params}
build_condition_group(Conditions, Operator, ParamOffset) ->
    {Clauses, Params} = lists:foldl(fun(Condition, {ClausesAcc, ParamsAcc}) ->
        case Condition of
            Map when is_map(Map) ->
                {Sql, NewParams} = build_map_conditions(Map, ParamOffset + length(ParamsAcc)),
                {[<<"(", Sql/binary, ")">> | ClausesAcc], ParamsAcc ++ NewParams};
            _ ->
                {ClausesAcc, ParamsAcc}
        end
    end, {[], []}, Conditions),
    Sql = iolist_to_binary(lists:join(<<" ", Operator/binary, " ">>, lists:reverse(Clauses))),
    {Sql, Params}.

%%--------------------------------------------------------------------
%% @doc
%% 构造 WHERE 条件子句，支持多种查询条件格式
%%
%% WhereMap 支持的选项：
%%
%% 1. 空 map #{}
%%    - 返回空的 WHERE 子句
%%    - 示例: #{}
%%    - 生成: <<>>, []
%%
%% 2. <<"__raw">>> 键 - 原始 SQL 条件
%%    - 直接使用提供的 SQL 字符串作为 WHERE 子句
%%    - 适用于复杂条件、JOIN 查询等无法用 map 表示的场景
%%    - 示例: #{<<"__raw">> => <<"status = 1 AND created_at > 100">>}
%%    - 生成: <<" WHERE status = 1 AND created_at > 100">>, []
%%
%% 3. 简单等值查询
%%    - 字段 = 值
%%    - 示例: #{status => 1, user_id => 123}
%%    - 生成: <<" WHERE 1=1 AND status = $1 AND user_id = $2">>, [1, 123]
%%
%% 4. {raw, RawSql} - 原始 SQL 表达式
%%    - 字段后面直接拼接原始 SQL
%%    - 示例: #{created_at => {raw, "> NOW() - INTERVAL '1 day'"}}
%%    - 生成: <<" WHERE 1=1 AND created_at > NOW() - INTERVAL '1 day'">>, []
%%
%% 5. {in, List} - IN 查询
%%    - 字段 IN (值1, 值2, ...)
%%    - 示例: #{status => {in, [1, 2, 3]}}
%%    - 生成: <<" WHERE 1=1 AND status IN ($1, $2, $3)">>, [1, 2, 3]
%%
%% 6. {not_in, List} - NOT IN 查询
%%    - 字段 NOT IN (值1, 值2, ...)
%%    - 示例: #{status => {not_in, [0, -1]}}
%%    - 生成: <<" WHERE 1=1 AND status NOT IN ($1, $2)">>, [0, -1]
%%
%% 7. {op, Op, Val} - 自定义操作符查询
%%    - 字段 操作符 值
%%    - 示例: #{age => {op, <<">">>, 18}}
%%    - 示例: #{name => {op, <<"LIKE">>, <<"John%">>}}
%%    - 生成: <<" WHERE 1=1 AND age > $1">>, [18]
%%
%% 8. {or, Conditions} - OR 连接多个条件组
%%    - 多个条件组之间用 OR 连接，每个条件组内用 AND 连接
%%    - 示例: (a=1 AND b=3) OR (a=4 AND b=5 AND c LIKE '%c%')
%%    - 写法: #{<<"__or">> => [#{a => 1, b => 3}, #{a => 4, b => 5, c => {op, <<"LIKE">>, <<"%c%">>}}]}
%%    - 生成: <<" WHERE (a = $1 AND b = $2) OR (a = $3 AND b = $4 AND c LIKE $5)">>, [1, 3, 4, 5, <<"%c%">>]
%%
%% 9. {and, Conditions} - AND 连接多个条件组（用于嵌套）
%%    - 示例: (a=1 AND (b=2 OR b=3)) OR (a=4)
%%    - 写法: #{<<"__or">> => [#{a => 1, <<"__and">> => [#{b => 2}, #{b => 3}]}, #{a => 4}]}
%%    - 生成: <<" WHERE (a = $1 AND (b = $2 OR b = $3)) OR (a = $4)">>, [1, 2, 3, 4]
%%
%% 注意：
%% - 字段名可以是 atom 或 binary
%% - 所有值都会被参数化，防止 SQL 注入（除了 {raw} 和 <<"__raw">>>）
%% - map 内部多个条件默认用 AND 连接
%% - 使用 <<"__or">>> 和 <<"__and">>> 键可以构建复杂的嵌套逻辑
%% - 参数编号自动管理，无需手动处理
%%--------------------------------------------------------------------
-spec build_where_clause(map()) -> {binary(), [term()]}.
build_where_clause(Where) when map_size(Where) == 0 ->
    {<<>>, []};
build_where_clause(Where) ->
    % 检查是否有 <<"__raw">>> 键，用于处理原始 SQL 条件
    case maps:get(<<"__raw">>, Where, undefined) of
        undefined ->
            % 检查是否有 <<"__or">>> 或 <<"__and">>> 键
            case {maps:get(<<"__or">>, Where, undefined), maps:get(<<"__and">>, Where, undefined)} of
                {undefined, undefined} ->
                    % 简单条件，用 AND 连接
                    {Clauses, Params} = maps:fold(fun(Field, Value, {ClausesAcc, ParamsAcc}) ->
                        FieldBin = case is_binary(Field) of
                            true -> Field;
                            false -> atom_to_binary(Field, utf8)
                        end,
                        case Value of
                            {raw, RawSql} ->
                                {[<<FieldBin/binary, " ", RawSql/binary>> | ClausesAcc], ParamsAcc};
                            {in, List} when is_list(List) ->
                                ParamStart = length(ParamsAcc) + 1,
                                ParamEnd = ParamStart + length(List) - 1,
                                ParamPlaceholders = lists:join(<<",">>, [<<"$", (integer_to_binary(I))/binary>> || I <- lists:seq(ParamStart, ParamEnd)]),
                                {[<<FieldBin/binary, " IN (", ParamPlaceholders/binary, ")">> | ClausesAcc], ParamsAcc ++ List};
                            {not_in, List} when is_list(List) ->
                                ParamStart = length(ParamsAcc) + 1,
                                ParamEnd = ParamStart + length(List) - 1,
                                ParamPlaceholders = lists:join(<<",">>, [<<"$", (integer_to_binary(I))/binary>> || I <- lists:seq(ParamStart, ParamEnd)]),
                                {[<<FieldBin/binary, " NOT IN (", ParamPlaceholders/binary, ")">> | ClausesAcc], ParamsAcc ++ List};
                            {op, Op, Val} ->
                                ParamNum = length(ParamsAcc) + 1,
                                {[<<FieldBin/binary, " ", Op/binary, " $", (integer_to_binary(ParamNum))/binary>> | ClausesAcc], ParamsAcc ++ [Val]};
                            _ ->
                                ParamNum = length(ParamsAcc) + 1,
                                {[<<FieldBin/binary, " = $", (integer_to_binary(ParamNum))/binary>> | ClausesAcc], ParamsAcc ++ [Value]}
                        end
                    end, {[], []}, Where),

                    case Clauses of
                        [] -> {<<>>, []};
                        _ ->
                            ClauseBinary = iolist_to_binary(lists:join(<<" AND ">>, lists:reverse(Clauses))),
                            {<<" WHERE ", ClauseBinary/binary>>, Params}
                    end;
                {OrConditions, undefined} when is_list(OrConditions) ->
                    % OR 连接多个条件组
                    {Sql, Params} = build_condition_group(OrConditions, <<"OR">>, 1),
                    case Sql of
                        <<>> -> {<<>>, []};
                        _ -> {<<" WHERE ", Sql/binary>>, Params}
                    end;
                {undefined, AndConditions} when is_list(AndConditions) ->
                    % AND 连接多个条件组
                    {Sql, Params} = build_condition_group(AndConditions, <<"AND">>, 1),
                    case Sql of
                        <<>> -> {<<>>, []};
                        _ -> {<<" WHERE ", Sql/binary>>, Params}
                    end;
                _ ->
                    % 同时有 OR 和 AND，优先处理 OR
                    OrConditions = maps:get(<<"__or">>, Where, []),
                    {Sql, Params} = build_condition_group(OrConditions, <<"OR">>, 1),
                    case Sql of
                        <<>> -> {<<>>, []};
                        _ -> {<<" WHERE ", Sql/binary>>, Params}
                    end
            end;
        RawWhereSql ->
            % 有 <<"__raw">>> 键，直接使用原始 SQL 条件
            {<<" WHERE ", RawWhereSql/binary>>, []}
    end.

%%--------------------------------------------------------------------
%% @doc
%% 构造分页查询（LIMIT / OFFSET 走参数）
%%--------------------------------------------------------------------
-spec page(binary(), binary(), map(), iodata(), pos_integer(), non_neg_integer()) ->
    {iodata(), [term()]}.
page(Table, Column, WhereMap, OrderBy, Limit, Offset) ->
    {WhereSql, WhereParams} = build_where_clause(WhereMap),
    ParamNum1 = length(WhereParams) + 1,
    ParamNum2 = length(WhereParams) + 2,
    Sql = [
        <<"SELECT ">>, Column, <<" FROM ">>, Table,
        WhereSql,
        <<" ORDER BY ">>, OrderBy,
        <<" LIMIT $", (integer_to_binary(ParamNum1))/binary>>,
        <<" OFFSET $", (integer_to_binary(ParamNum2))/binary>>
    ],
    {Sql, WhereParams ++ [Limit, Offset]}.

%% ================== internal helpers ==================

%% @doc 解析 map，返回 {Columns, Values, RawClauses}
%% RawClauses 是一个列表，每个元素是 raw SQL 或 <<"?">>（表示需要参数化）
unzip_map(Map) ->
    List = maps:to_list(Map),
    Cols = [field_to_binary(K) || {K, _V} <- List],
    {Vals, RawClauses} = lists:unzip(lists:map(fun({_K, V}) ->
        case V of
            {raw, RawSql} when is_binary(RawSql) ->
                {undefined, RawSql};
            _ ->
                {V, <<"?">>}
        end
    end, List)),
    % 只保留非 raw 的参数值
    FilteredVals = [V || {V, Clause} <- lists:zip(Vals, RawClauses), Clause =:= <<"?">>],
    {Cols, FilteredVals, RawClauses}.

%% @doc 将字段名（atom 或 binary）转换为 binary
field_to_binary(Field) when is_atom(Field) ->
    atom_to_binary(Field, utf8);
field_to_binary(Field) when is_binary(Field) ->
    Field.

join_cols(Cols) ->
    lists:join(<<",">>, [field_to_binary(C) || C <- Cols]).

%% @doc 生成 SET 子句，支持 raw 值
%% Cols: 字段列表
%% RawClauses: raw 子句列表（每个元素是 raw SQL 或 <<"?">>）
%% Offset: 参数编号偏移量
%% 返回: {SetSql, ParamCount}
join_set_with_offset(Cols, RawClauses, Offset) ->
    {SetClauses, ParamCount} = lists:mapfoldl(fun({Col, Clause}, Count) ->
        case Clause of
            <<"?">> ->
                {[field_to_binary(Col), <<" = $">>, integer_to_binary(Count)], Count + 1};
            RawSql ->
                {[field_to_binary(Col), <<" = ">>, RawSql], Count}
        end
    end, Offset + 1, lists:zip(Cols, RawClauses)),
    {lists:join(<<",">>, SetClauses), ParamCount - Offset - 1}.

placeholders(N) ->
    iolist_to_binary(lists:join(
        <<",">>,
        [ <<"$", (integer_to_binary(I))/binary>> || I <- lists:seq(1, N) ]
    )).

%% @doc 生成从指定偏移量开始的 N 个占位符
%% 例如: placeholders_with_offset(3, 5) -> <<"$5,$6,$7">>
-spec placeholders_with_offset(pos_integer(), pos_integer()) -> binary().
placeholders_with_offset(N, Offset) ->
    iolist_to_binary(lists:join(
        <<",">>,
        [ <<"$", (integer_to_binary(I))/binary>> || I <- lists:seq(Offset, Offset + N - 1) ]
    )).

value_or_empty({ok, Row}) when is_map(Row) -> Row;
value_or_empty({ok, undefined}) -> #{};
value_or_empty(_) -> #{}.

parse_result({ok, _, [#{<<"id">> := Id}]}) -> {ok, Id, #{}};
parse_result({ok, _, [{Id}]}) when is_integer(Id) -> {ok, Id, #{}}; % For epgsql result [{123}]
parse_result({ok, _, [{ {Id} }]}) when is_integer(Id) -> {ok, Id, #{}}; % For epgsql result [{123}]
parse_result({ok, _, [Id]}) when is_integer(Id) -> {ok, Id, #{}};
parse_result({error, Reason}) -> {error, Reason}.
