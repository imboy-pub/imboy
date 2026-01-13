%%%===================================================================
%%% @doc
%%% PostgreSQL 核心访问模块
%%%
%%% 职责：
%%% - pooler 连接管理
%%% - epgsql 执行
%%% - 事务封装
%%% - 返回值模型统一
%%%
%%% 禁止：
%%% - 拼 SQL 结构
%%% - 理解业务字段
%%%===================================================================
-module(elib_pg).

-include_lib("epgsql/include/epgsql.hrl").

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").


-export([
    with_conn/1,
    with_conn/2,
    with_tx/1,
    with_tx/2,

    execute/2,
    execute/3,

    query/2,
    query/3,

    one/2,
    one/3,

    pluck/4,
    pluck/5,
    pluck_value/4,
    pluck_value/5,

    insert/2,
    insert/3,
    insert/4,

    update/4,
    update/5,

    insert_batch/3,

    select/2,
    select/3,

    page/4,
    page/6,
    page_with_total/4,
    page_with_total/6,
    page_safe/7,
    page_with_total_safe/7
]).

-define(DEFAULT_TIMEOUT, 1000).

%%--------------------------------------------------------------------
%% @doc
%% 从 pooler 获取连接并执行 Fun
%%--------------------------------------------------------------------
%% ===================================================================
%% 统一的安全封装：带重试 + 异常捕获 + 连接回收
%% ===================================================================
%% @doc 简化版本的with_conn，使用默认重试参数
-spec with_conn(fun((epgsql:connection()) -> term())) ->
          term() | {error, term()}.
with_conn(Fun) ->
    with_conn(Fun, ?DEFAULT_TIMEOUT).
-spec with_conn(fun((epgsql:connection()) -> term()), pos_integer()) ->
          term() | {error, term()}.
with_conn(Fun, Timeout) ->
    % Driver = config_ds:env(sql_driver),
    with_conn(pgsql, Fun, 3, Timeout).

%% Fun    : (Conn -> Result)
%% Retries: 最大重试次数
%% Delay  : 初始重试延迟（毫秒）
with_conn(Driver, Fun, Retries, Delay) ->
    case pooler:take_member(Driver) of
        error_no_members when Retries > 0 ->
            timer:sleep(Delay),
            with_conn(Driver, Fun, Retries - 1, Delay + 1000);
        error_no_members ->
            {error, no_connection};
        Conn when is_pid(Conn) ->
            Result = try Fun(Conn) catch
                    %% 接受 throw({abort_tx, Reason})
                    %% 业务主动失败：不重试
                    throw:{abort_tx, Reason} ->
                        %% 回滚事务，使得调用方返回二元组 {error, Reason};
                        _ = epgsql:squery(Conn, <<"ROLLBACK">>),
                        {error, Reason};
                    %% 其他 DB / 运行时异常
                    Class:Reason:Stacktrace ->
                        ok = ?ERROR_LOG(
                            "DB operation failed: ~p:~p stack=~p~n",
                            [Class, Reason, Stacktrace]
                        ),
                        {error, Class, Reason}
                after
                    pooler:return_member(Driver, Conn)
                end,
            case Result of
                {error, _C, _R} when Retries > 0 ->
                    timer:sleep(Delay),
                    with_conn(Driver, Fun, Retries - 1, Delay + 1000);
                _ ->
                    Result
            end
    end.

%%--------------------------------------------------------------------
%% @doc
%% 在事务中执行 Fun
%% 禁止业务层自行 BEGIN / COMMIT
%%--------------------------------------------------------------------


%% @doc 事务封装
%% @deprecated 请使用 elib_pg:with_tx/1,2 替代
-spec with_tx(fun((epgsql:connection() | pid()) -> term())) -> term() | {rollback, term()}.
with_tx(F) ->
    with_tx(F, [{reraise, true}]).


-spec with_tx(fun((epgsql:connection() | pid()) -> term()), epgsql:transaction_opts()) ->
          term() | {rollback, term()} | no_return().
with_tx(F, Opts0) ->
    with_tx(F, Opts0, 3, 200). % 最大重试3次，初始延迟100毫秒


%% ===================================================================
%% 事务封装
%% ===================================================================
-spec with_tx(fun((epgsql:connection() | pid()) -> term()), list(), non_neg_integer(), non_neg_integer()) -> term().
with_tx(F, Opts0, RetriesLeft, Delay) ->
    Driver = config_ds:env(sql_driver),
    with_conn(Driver,
        fun(Conn) ->
            epgsql:with_transaction(Conn, F, Opts0)
        end,
        RetriesLeft,
        Delay).

%%--------------------------------------------------------------------
%% @doc
%% 执行无返回行的 SQL（INSERT / UPDATE / DELETE）
%%--------------------------------------------------------------------
-spec execute(iodata(), [term()]) ->
    {ok, term()} | {ok, term(), term()} | {error, term()}.
execute(Sql, Params) ->
    with_conn(fun(C) ->
        execute(C, Sql, Params)
    end, ?DEFAULT_TIMEOUT).

-spec execute(epgsql:connection(), iodata(), [term()]) ->
    {ok, term()} | {ok, term(), term()} | {error, term()}.
execute(Conn, Sql, Params) ->
    _ = ?DEBUG_LOG(io:format("sql: ~s\n", [Sql])),
    % ?DEBUG_LOG(io:format("Params: ~p\n", [Params])),
    % Res = epgsql:parse(Conn, Sql),
    % ?DEBUG_LOG(io:format("epgsql:parse Res: ~p\n", [Res])),
    % {ok, Stmt} = Res,
    %% 关键改动：消除 badmatch 异常传播。任何驱动异常/异常形态都收敛为 {error, Reason}。
    try
        case epgsql:parse(Conn, Sql) of
            {ok, Stmt} ->
                case epgsql:execute_batch(Conn, [{Stmt, Params}]) of
                    [Res2] ->
                        Res2;
                    [] ->
                        {error, {epgsql, empty_batch_result}};
                    Other2 ->
                        {error, {epgsql, unexpected_batch_result, Other2}}
                end;
            {error, Reason} ->
                {error, Reason}
        end
    catch
        Class:CatchReason:Stacktrace ->
            _ = ?ERROR_LOG("DB execute failed: ~p:~p stack=~p~n", [Class, CatchReason, Stacktrace]),
            {error, {Class, CatchReason, Stacktrace}}
    end.
    % {ok, 1} | {ok, 1, {ReturningField}} | {ok,1,[{5}]}
    % 没有 RETURNING 子句：返回 {ok, 1} （二元组）
    % 有 RETURNING 子句：返回 {ok, 1, Result} （三元组），其中 Result 可能是 {Id} 或 [{Id}]



%%--------------------------------------------------------------------
%% @doc
%% 查询多行，返回 map 列表
%%--------------------------------------------------------------------
-spec query(iodata(), [term()]) ->
    {ok, [map()]} | {error, term()}.
query(Sql, Params) ->
    with_conn(fun(C) ->
        query(C, Sql, Params)
    end, ?DEFAULT_TIMEOUT).

-spec query(epgsql:connection(), iodata(), [term()]) ->
    {ok, [map()]} | {error, term()}.
query(C, Sql, Params) ->
    % ?INFO_LOG(["query ", iolist_to_binary(Sql), Params, epgsql:equery(C, iolist_to_binary(Sql), Params)]),
    case epgsql:equery(C, iolist_to_binary(Sql), Params) of
        {ok, Cols, Rows} ->
            {ok, rows_to_maps(Cols, Rows)};
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% 查询单行：
%% - 1 行 -> {ok, Row}
%% - 多行 -> {ok, FirstRow}
%%--------------------------------------------------------------------
-spec one(iodata(), [term()]) ->
    {ok, map()} | {error, term()}.
one(Sql, Params) ->
    one(Sql, Params, #{}).

-spec one(iodata(), [term()], term()) ->
    {ok, map() | undefined} | {error, term()}.
one(Sql, Params, Default) ->
    case query(Sql, Params) of
        {ok, [Row | _Rest]} -> {ok, Row};
        {ok, []}           -> {ok, Default};
        Error              -> Error
    end.

-spec pluck(
    Table  :: binary(),
    Field  :: binary(),
    Where  :: map(),
    Default :: term()
) -> {ok, term()} | {ok, undefined} | {error, term()}.
pluck(Table, Field, Where, Default) ->
    pluck(Table, Field, Where,  #{limit => 1}, Default).

%% @doc
%% 从表中取一个字段值，返回 undefined 或 Value
-spec pluck(
    Table  :: binary(),
    Field  :: binary(),
    Where  :: map(),
    Opts   :: map(), %% #{order_by => [{Field, asc|desc}], limit => pos_integer()},
    Default :: term()
) -> {ok, term()} | {error, term()}.
pluck(Table, Field, Where, Opts, Default) ->
    Fields = [Field],
    {Sql, Params} =
        elib_pg_sql:build_select(Table, Fields, Where, Opts#{limit => 1}),
    case one(Sql, Params, undefined) of
        {ok, Row} when is_map(Row) ->
            % ?INFO_LOG(["pluck ", Row]),
            % 提取字段别名或函数名
            FieldKey = case binary:split(Field, <<" ">>, [global]) of
                Parts when length(Parts) > 1 ->
                    % 有空格，取最后一个部分作为别名
                    % 支持 "count(*) as count", "count(*) AS count", "sum(price) total" 等
                    lists:last(Parts);
                _ ->
                    % 没有空格，检查是否是聚合函数
                    case Field of
                        <<"count(", _/binary>> -> <<"count">>;
                        <<"sum(", _/binary>> -> <<"sum">>;
                        <<"avg(", _/binary>> -> <<"avg">>;
                        <<"max(", _/binary>> -> <<"max">>;
                        <<"min(", _/binary>> -> <<"min">>;
                        % 普通字段名直接使用
                        _ -> Field
                    end
            end,
            {ok, maps:get(FieldKey, Row, Default)};
        {ok, undefined} ->
            {ok, Default};
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc
%% 便捷方法：直接返回值或默认值，忽略错误
%% 适用于：配置获取、计数查询等容错性高的场景
%%--------------------------------------------------------------------
-spec pluck_value(binary(), binary(), map(), term()) -> term().
pluck_value(Table, Field, Where, Default) ->
    pluck_value(Table, Field, Where, #{limit => 1}, Default).

%% @doc
%% 便捷方法：直接返回值或默认值，忽略错误
%% 适用于：配置获取、计数查询等容错性高的场景
-spec pluck_value(binary(), binary(), map(), map(), term()) -> term().
pluck_value(Table, Field, Where, Opts, Default) ->
    case pluck(Table, Field, Where, Opts, Default) of
        {ok, Value} -> Value;
        {error, _} -> Default
    end.


%% ================== CRUD helpers ==================


-spec insert(binary(), map()) ->
    {ok, term()} | {error, term()}.
insert(Table, Map) ->
    insert(Table, Map, <<>>).

-spec insert(binary(), map(), binary()) ->
    {ok, term()} | {ok, term(), term()} | {error, term()}.
insert(Table, Map, RETURNING) ->
    {Sql, Params} = elib_pg_sql:insert(Table, Map, RETURNING),
    execute(Sql, Params).

-spec insert(epgsql:connection(), binary(), map(), binary()) ->
    {ok, term()} | {ok, term(), term()} | {error, term()}.
insert(Conn, Table, Map, RETURNING) ->
    {Sql, Params} = elib_pg_sql:insert(Table, Map, RETURNING),
    execute(Conn, Sql, Params).


%%--------------------------------------------------------------------
%% @doc
%% 执行 UPDATE 操作（支持 WHERE 参数化，防止 SQL 注入）
%%
%% 使用场景：
%% - WHERE 条件包含用户输入时，必须使用此函数
%% - WhereSql 中使用占位符（如 $1, $2），WhereParams 提供对应的参数值
%%
%% 示例：
%%   elib_pg:update(<<"user">>, #{name => "Alice", age => 30},
%%                   <<"id = $1 AND status = $2">>, [UserId, Status])
%%
%% 安全性：
%%   ✅ SET 子句的值：参数化，安全
%%   ✅ WHERE 子句的值：参数化，安全
%%   ⚠️  WhereSql 本身：直接拼接，调用方需确保 WhereSql 不包含用户输入
%%--------------------------------------------------------------------
-spec update(binary(), map(), iodata(), [term()]) ->
    {ok, term()} | {error, term()}.
update(Table, Map, WhereSql, WhereParams) ->
    {Sql, Params} = elib_pg_sql:update(Table, Map, WhereSql, WhereParams),
    execute(Sql, Params).

%%--------------------------------------------------------------------
%% @doc
%% 在事务中执行 UPDATE 操作（使用现有连接，支持 WHERE 参数化）
%%
%% 使用场景：
%% - 在事务（with_tx）中执行 UPDATE 操作
%% - 需要复用现有连接对象，避免重复获取连接
%% - WHERE 条件包含用户输入时，必须使用此函数
%%
%% 示例：
%%   elib_pg:with_tx(fun(Conn) ->
%%       elib_pg:update(Conn, <<"user">>, #{name => "Alice", age => 30},
%%                       <<"id = $1 AND status = $2">>, [UserId, Status])
%%   end)
%%
%% 安全性：
%%   ✅ SET 子句的值：参数化，安全
%%   ✅ WHERE 子句的值：参数化，安全
%%   ⚠️  WhereSql 本身：直接拼接，调用方需确保 WhereSql 不包含用户输入
%%
%% @param Conn PostgreSQL 连接对象（通常由 with_tx 提供）
%% @param Table 表名
%% @param Map 要更新的字段和值的映射
%% @param WhereSql WHERE 条件（可以包含占位符如 $1, $2）
%% @param WhereParams WHERE 条件的参数值
%%--------------------------------------------------------------------
-spec update(epgsql:connection(), binary(), map(), iodata(), [term()]) ->
    {ok, term()} | {error, term()}.
update(Conn, Table, Map, WhereSql, WhereParams) ->
    {Sql, Params} = elib_pg_sql:update(Table, Map, WhereSql, WhereParams),
    execute(Conn, Sql, Params).

-spec insert_batch(binary(), [atom()], [[term()]]) ->
    {ok, term()} | {error, term()}.
insert_batch(Table, Cols, Rows) ->
    {Sql, Params} = elib_pg_sql:insert_batch(Table, Cols, Rows),
    execute(Sql, Params).

-spec select(binary(), iodata()) ->
    {ok, [map()]} | {error, term()}.
select(Table, WhereSql) ->
    select(Table, WhereSql, []).

-spec select(binary(), iodata(), [term()]) ->
    {ok, [map()]} | {error, term()}.
select(Table, WhereSql, Params) ->
    Sql = elib_pg_sql:select(Table, WhereSql),
    query(Sql, Params).


-spec page(binary(), map(), pos_integer(), pos_integer()) ->
    {ok, [map()]} | {error, term()}.
page(Table, WhereMap, Page, Size) ->
    page(Table, <<"*">>, WhereMap, <<"id desc">>, Page, Size).

-spec page(binary(), binary(), map(), iodata(), pos_integer(), pos_integer()) ->
    {ok, [map()]} | {error, term()}.
page(Table, Column, WhereMap, OrderBy, Page, Size) when Page > 0, Size > 0 ->
    Limit = Size,
    Offset = (Page - 1) * Size,
    {Sql, Params} = elib_pg_sql:page(Table, Column, WhereMap, OrderBy, Limit, Offset),
    query(Sql, Params).

-spec page_safe(binary(), binary(), map(), [{atom() | binary(), asc | desc}], [binary()], pos_integer(), pos_integer()) ->
    {ok, [map()]} | {error, term()}.
page_safe(Table, Column, WhereMap, OrderSpec, ValidFields, Page, Size) when Page > 0, Size > 0 ->
    Limit = Size,
    Offset = (Page - 1) * Size,
    {Sql, Params} =
        elib_pg_sql:build_select_safe(
            Table, Column, WhereMap, OrderSpec, ValidFields,
            #{limit => Limit, offset => Offset}
        ),
    query(Sql, Params).

%%--------------------------------------------------------------------
%% @doc
%% 分页查询（自动查询总数）
%%
%% 返回完整的分页信息，包括总数、当前页、每页大小和数据列表
%%
%% 示例：
%%   {ok, #{total := Total, page := Page, size := Size, list := List}} =
%%       elib_pg:page_with_total(Tb, Column, Where, OrderBy, Page, Size)
%%--------------------------------------------------------------------
-spec page_with_total(binary(), map(), pos_integer(), pos_integer()) ->
    {ok, map()} | {error, term()}.
page_with_total(Table, WhereMap, Page, Size) ->
    page_with_total(Table, <<"*">>, WhereMap, <<"id desc">>, Page, Size).

-spec page_with_total(binary(), binary(), map(), iodata(), pos_integer(), pos_integer()) ->
    {ok, map()} | {error, term()}.
page_with_total(Table, Column, WhereMap, OrderBy, Page, Size) when Page > 0, Size > 0 ->
    case pluck(Table, <<"count(*)">>, WhereMap, 0) of
        {ok, 0} ->
            {ok, #{
                total => 0,
                page => Page,
                size => Size,
                list => []
            }};
        {ok, Total} ->
            case page(Table, Column, WhereMap, OrderBy, Page, Size) of
                {ok, List} ->
                    {ok, #{
                        total => Total,
                        page => Page,
                        size => Size,
                        list => List
                    }};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec page_with_total_safe(binary(), binary(), map(), [{atom() | binary(), asc | desc}], [binary()], pos_integer(), pos_integer()) ->
    {ok, map()} | {error, term()}.
page_with_total_safe(Table, Column, WhereMap, OrderSpec, ValidFields, Page, Size) when Page > 0, Size > 0 ->
    case pluck(Table, <<"count(*)">>, WhereMap, 0) of
        {ok, 0} ->
            {ok, #{total => 0, page => Page, size => Size, list => []}};
        {ok, Total} ->
            case page_safe(Table, Column, WhereMap, OrderSpec, ValidFields, Page, Size) of
                {ok, List} ->
                    {ok, #{total => Total, page => Page, size => Size, list => List}};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% ================== internal ==================

-spec rows_to_maps([#column{}], [tuple()]) -> [map()].
rows_to_maps(Cols, Rows) ->
    Keys = [C#column.name || C <- Cols],
    [ maps:from_list(lists:zip(Keys, tuple_to_list(Row)))
      || Row <- Rows ].
