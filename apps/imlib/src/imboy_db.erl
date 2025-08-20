-module(imboy_db).

%% @doc
%% 数据库访问工具模块，封装了通用的查询、执行、分页、插入与更新等操作。
%% 设计目标：接口简洁、实现高效、日志清晰、错误可观测。
%% 注意：本模块保留向后兼容的行为，不删除任何既有注释与对外 API。

-export([pluck/2]).
-export([pluck/3]).
-export([pluck/4]).
-export([find/1, find/2, find/4]).
-export([list/1, list/2]).
-export([proplists/1]).
-export([page/6]).

-export([count_for_where/2, page_for_where/6]).

-export([query/1]).
-export([query/2]).
-export([execute/1, execute/2, execute/3]).

-export([assemble_sql/4]).
-export([assemble_where/1]).
-export([assemble_value/1]).

-export([get_set/1]).

-export([add/3, add/4]).
-export([insert_into/2, insert_into/3, insert_into/4]).
-export([update/3]).
-export([update/4]).

-export([with_transaction/1]).
-export([with_transaction/2]).

-export([public_tablename/1]).


-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("kernel/include/logger.hrl").
-include_lib("imlib/include/log.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 使用事务执行函数 F，当连接暂不可用时进行指数退避重试。
-spec with_transaction(fun((epgsql:connection()) -> Reply)) -> Reply | {rollback, any()} when Reply :: any().
with_transaction(F) ->
    with_transaction(F, [{reraise, true}]).


%% @doc 使用事务执行函数 F，支持自定义事务选项，基于 with_pooled_conn 统一连接管理。
-spec with_transaction(fun((epgsql:connection()) -> Reply), epgsql:transaction_opts()) ->
          Reply | {rollback, any()} | no_return() when Reply :: any().
with_transaction(F, Opts0) ->
    Driver = config_ds:env(sql_driver),
    with_pooled_conn(
        Driver,
        fun() ->
            imboy_log:error(io_lib:format("imboy_db:with_transaction/2 no members available for driver:~p~n", [Driver]))
        end,
        fun(Conn) ->
            epgsql:with_transaction(Conn, F, Opts0)
        end,
        fun(Res) ->
            %% 兼容旧行为：将重试耗尽的错误映射为 {error, retries_exhausted}
            case Res of
                {error, {no_available_members, _}} -> {error, retries_exhausted};
                _ -> Res
            end
        end,
        fun(Class, Reason, Stacktrace) ->
            imboy_log:error(io_lib:format("imboy_db:with_transaction/2 exception: ~p:~p ~p for driver:~p~n", [Class, Reason, Stacktrace, Driver])),
            %% 与历史行为兼容：当选项包含 {reraise, true} 时，继续向上抛出异常
            case proplists:get_value(reraise, Opts0, false) of
                true -> erlang:raise(Class, Reason, Stacktrace);
                false -> ok
            end
        end,
        fun(UnknownConn) ->
            imboy_log:error(io_lib:format("imboy_db:with_transaction/2 unknown connection: ~p for driver:~p~n", [UnknownConn, Driver]))
        end
    ).

% imboy_db:pluck(<<"SELECT to_tsquery('jiebacfg', '软件中国')"/utf8>>, <<"">>).
% imboy_db:pluck(<<"adm_user">>, <<>>, <<"count(*) as count">>, 0).
% imboy_db:pluck(<<"user">>, <<"1=1">>, <<"count(*) as count">>, 0).

% pluck(<<"public.", Tb/binary>>, Field, Default) ->
%     pluck(Tb, Field, Default);
%% @doc 执行简单的单列查询，返回第一行第一列的值或默认值。
pluck(Tb, Field, Default) ->
    Tb2 = public_tablename(Tb),
    Sql = <<"SELECT ", Field/binary, " FROM ", Tb2/binary>>,
    % ?DEBUG_LOG([pluck, Sql]),
    pluck(Sql, Default).


%% @doc 以可选 Where 子句从表中摘取单列，返回第一行值或默认值。
pluck(Tb, <<>>, Field, Default) ->
    Tb2 = public_tablename(Tb),
    Sql = <<"SELECT ", Field/binary, " FROM ", Tb2/binary>>,
    % ?DEBUG_LOG([pluck, Sql]),
    pluck(Sql, Default);
pluck(Tb, Where, Field, Default) ->
    Tb2 = public_tablename(Tb),
    Sql = <<"SELECT ", Field/binary, " FROM ", Tb2/binary, " WHERE ", Where/binary>>,
    % ?DEBUG_LOG([pluck, Sql]),
    pluck(Sql, Default).


pluck(<<"SELECT ", Query/binary>>, Default) ->
    pluck(Query, Default);
pluck(Query, Default) ->
    Res = imboy_db:query(<<"SELECT ", Query/binary>>),
    % imboy_log:info(io_lib:format("imboy_db:pluck/2 Query:SELECT ~s ~n", [Query])),
    % imboy_log:info(io_lib:format("imboy_db:pluck/2 Res:~p ~n", [Res])),
    case Res of
        {ok, _, [{Val}]} ->
            % imboy_log:info(io_lib:format("imboy_db:pluck/2 1 Val:~p ~n", [Val])),
            Val;
        {ok, _, [{Val}|_]} ->
            % imboy_log:info(io_lib:format("imboy_db:pluck/2 2 Val:~p ~n", [Val])),
            Val;
        _ ->
            Default
      end.

%% @doc 根据条件与排序从表中拿到一行，返回列名到值的映射。
find(Tb, Where, OrderBy, Column) ->
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE ", Where/binary, " ORDER BY ", OrderBy/binary, " LIMIT 1">>,
    % ?DEBUG_LOG([find, Sql]),
    find(Sql).

%% @doc 执行原始 SQL，返回第一行的列名到值的映射。
find(Sql) ->
    query_resp_map(imboy_db:query(Sql)).

%% @doc 执行带参数的原始 SQL，返回第一行的列名到值的映射。
find(Sql, Params) ->
    query_resp_map(imboy_db:query(Sql, Params)).


%% @doc 通用分页封装，返回 total/page/size/items 结构。
-spec page(integer(), integer(), binary(), binary(), binary(), binary()) -> list().
page(Page, Size, Tb, Where, OrderBy, Column) when Page > 0 ->
    Offset = (Page - 1) * Size,
    Total = count_for_where(Tb, Where),
    Items = page_for_where(Tb,
        Size,
        Offset,
        Where,
        OrderBy,
        Column),
    imboy_response:page_payload(Total, Page, Size, Items).

-spec count_for_where(binary(), binary()) -> binary().
count_for_where(Tb, Where) ->
    % Tb = tablename(),
    imboy_db:pluck(<<Tb/binary>>, Where, <<"count(*) as count">>, 0).
-spec page_for_where(integer(), integer(), binary(), binary(), binary(), binary())
    -> list().
page_for_where(Tb, Limit, Offset, Where, OrderBy, Column) ->
    Where2 = <<" WHERE ", Where/binary, " ORDER BY ", OrderBy/binary, " LIMIT $1 OFFSET $2">>,
    % Tb = tablename(),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, Where2/binary>>,
    % Res = imboy_db:query(Sql, [Limit, Offset]),
    % ?DEBUG_LOG(['Sql', Sql]),
    % ?DEBUG_LOG(['Res', Res]),
    % case Res of
    case imboy_db:query(Sql, [Limit, Offset]) of
        {ok, _, []} ->
            [];
        {ok, [{column, C1, _, _, _, _, _, _,_}], Items0} ->
            to_proplists([C1], Items0);
        {ok, ColumnLi, Items0} ->
            to_proplists(ColumnLi, Items0);
        _ ->
            []
    end.

%% @doc 将查询结果转换为 [{Key,Val}] 形式的列表。
proplists(Sql) ->
    case imboy_db:query(Sql) of
        {ok, Col, Val} ->
            to_proplists(Col, Val);
        _ ->
            []
    end.

%% @doc 执行查询并返回原始行数据（列表）。
list(Sql) ->
    case imboy_db:query(Sql) of
        {ok, _, Val} ->
            Val;
        _ ->
            []
    end.

%% @doc 在外部已持有连接的情况下执行查询，返回原始行数据。
list(Conn, Sql) ->
    case epgsql:equery(Conn, Sql) of
        {ok, _, Val} ->
            Val;
        _ ->
            []
    end.

% imboy_db:query("select * from user where id = 2")
% imboy_db:query("select created_at from adm_user limit 1").
%% @doc 执行原始 SQL 查询（无参数），返回 {ok, Columns, Rows} 或 {error, Reason}。
-spec query(binary() | list()) -> {ok, list(), list()} | {error, any()}.
query(Sql) ->
    % ?DEBUG_LOG([imboy_dt:now(), Sql]),
    Driver = config_ds:env(sql_driver),
    with_pooled_conn(
        Driver,
        fun() ->
            imboy_log:error(io_lib:format("imboy_db:query/1 sql:~s~n", [Sql]))
        end,
        fun(Conn) ->
            case Driver of
                pgsql ->
                    epgsql:equery(Conn, Sql);
                _ ->
                    {error, not_supported}
            end
        end,
        fun(Res) -> query_resp(Res) end,
        fun(Class, Reason, Stacktrace) ->
            %% 确保即使出现异常也归还连接的日志
            imboy_log:error(io_lib:format("imboy_db:query/1 exception: ~p:~p ~p for sql:~s~n", [Class, Reason, Stacktrace, Sql]))
        end,
        fun(UnknownConn) ->
            imboy_log:error(io_lib:format("imboy_db:query/1 unknown connection: ~p for sql:~s~n", [UnknownConn, Sql]))
        end
    ).


%% @doc 执行带参数的 SQL 查询，返回 {ok, Columns, Rows} 或 {error, Reason}。
-spec query(binary() | list(), list()) -> {ok, list(), list()} | {error, any()}.
query(Sql, Params) ->
    Driver = config_ds:env(sql_driver),
    with_pooled_conn(
        Driver,
        fun() ->
            imboy_log:error(io_lib:format("imboy_db:query/2 sql:~s, ~p;\n", [Sql,Params]))
        end,
        fun(Conn) ->
            case Driver of
                pgsql ->
                    epgsql:equery(Conn, Sql, Params);
                _ ->
                    {error, not_supported}
            end
        end,
        fun(Res) -> query_resp(Res) end,
        fun(Class, Reason, Stacktrace) ->
            %% 确保即使出现异常也归还连接的日志
            imboy_log:error(io_lib:format("imboy_db:query/2 exception: ~p:~p ~p for sql:~s with params:~p~n", [Class, Reason, Stacktrace, Sql, Params]))
        end,
        fun(UnknownConn) ->
            imboy_log:error(io_lib:format("imboy_db:query/2 unknown connection: ~p for sql:~s with params:~p~n", [UnknownConn, Sql, Params]))
        end
    ).

execute(Sql) ->
    % ?DEBUG_LOG(io:format("~s\n", [Sql])),
    execute(Sql, []).

%% @doc 执行带参数的 DML/DDL，将返回底层驱动返回值或错误。
-spec execute(any(), list()) -> {ok, LastInsertId :: integer()} | {error, any()}.
execute(Sql, Params) ->
    % ?DEBUG_LOG(io:format("~s\n", [Sql])),
    Driver = config_ds:env(sql_driver),
    with_pooled_conn(
        Driver,
        fun() ->
            imboy_log:error(io_lib:format("imboy_db:execute/2 sql:~s, ~p;\n", [Sql,Params]))
        end,
        fun(Conn) ->
            case Driver of
                pgsql ->
                    execute(Conn, Sql, Params);
                _ ->
                    {error, not_supported}
            end
        end,
        fun(Res) ->
            ?DEBUG_LOG(['execute ', Res]),
            Res
        end,
        fun(Class, Reason, Stacktrace) ->
            %% 确保即使出现异常也归还连接的日志
            imboy_log:error(io_lib:format("imboy_db:execute/2 exception: ~p:~p ~p for sql:~s with params:~p~n", [Class, Reason, Stacktrace, Sql, Params]))
        end,
        fun(UnknownConn) ->
            imboy_log:error(io_lib:format("imboy_db:execute/2 unknown connection: ~p for sql:~s with params:~p~n", [UnknownConn, Sql, Params]))
        end
    ).


%% @doc 使用已有连接执行预编译并批量执行，返回底层返回值。
execute(Conn, Sql, Params) ->
    ?DEBUG_LOG(io:format("sql: ~s\n", [Sql])),
    % ?DEBUG_LOG(io:format("Params: ~p\n", [Params])),
    % Res = epgsql:parse(Conn, Sql),
    % ?DEBUG_LOG(io:format("epgsql:parse Res: ~p\n", [Res])),
    % {ok, Stmt} = Res,
    {ok, Stmt} = epgsql:parse(Conn, Sql),
    [Res2] = epgsql:execute_batch(Conn, [{Stmt, Params}]),
    % ?DEBUG_LOG(io:format("execute/3 Res2: ~p\n", [Res2])),
    % {ok, 1} | {ok, 1, {ReturningField}} | {ok,1,[{5}]}
    Res2.

% imboy_db:insert_into/3
%% @doc 以 Map 数据进行插入，自动生成列和值。
insert_into(Tb, Data) when is_map(Data) ->
    {Column, Value} = process_insert_data(Data),
    imboy_db:insert_into(Tb, Column, Value).

%% @doc 以 Map 数据进行插入，支持 Returning/On Conflict 子句。
insert_into(Tb, Data, ReturningOnConflict) when is_map(Data) ->
    {Column, Value} = process_insert_data(Data),
    insert_into(Tb, Column, Value, ReturningOnConflict);
insert_into(Tb, Column, Value) ->
    insert_into(Tb, Column, Value, <<"RETURNING id;">>).


%% @doc 通用插入实现，Column/Value 可为列表或拼装完成的二进制。
insert_into(Tb, Column, Value, ReturningOnConflict) ->
    % Sql like this "INSERT INTO foo (k,v) VALUES (1,0), (2,0)"
    % return {ok,1,[{10}]}
    Sql = assemble_sql(<<"INSERT INTO">>, Tb, Column, Value),
    ?DEBUG_LOG([insert_into, Sql]),
    execute(<<Sql/binary, " ", ReturningOnConflict/binary>>, []).

%% @doc 在指定连接上插入，适用于事务中批量插入。
add(Conn, Tb, Data) ->
    add(Conn, Tb, Data, <<"RETURNING id;">>).
% imboy_db:add/4
add(Conn, Tb, Data, ReturningOnConflict) ->
    {Column, Value} = process_insert_data(Data),
    Sql = assemble_sql(<<"INSERT INTO">>, Tb, Column, Value),
    % ?DEBUG_LOG(io:format("~s\n", [Sql])),
    execute(Conn, <<Sql/binary, " ", ReturningOnConflict/binary>>, []).


% 组装 SQL 语句
assemble_sql(Prefix, Tb, Column, Value) when is_list(Column) ->
    ColumnBin = imboy_cnv:implode(",", Column),
    assemble_sql(Prefix, Tb, <<"(", ColumnBin/binary, ")">>, Value);
assemble_sql(Prefix, Tb, Column, Value) when is_list(Value) ->
    ValueBin = imboy_cnv:implode(",", Value),
    assemble_sql(Prefix, Tb, Column, <<"(", ValueBin/binary, ")">>);
assemble_sql(Prefix, Tb, Column, Value) ->
    Tb2 = public_tablename(Tb),
    Sql = <<Prefix/binary, " ", Tb2/binary, " ", Column/binary, " VALUES ", Value/binary>>,
    % ?DEBUG_LOG(io:format("~s\n", [Sql])),
    Sql.


% UPDATE public.config SET remark = '', system = 1, tab = 'sys', title = '', updated_at = '2025-03-24 08:47:22.625575+08:00', value = '"ws:\/\/192.168.1.195:9800\/ws\/"' WHERE key = 'ws_url'
% imboy_db:update(<<"config">>, <<"key='ws_url'">>, #{updated_at => 1742777443331}).
% imboy_db:update(<<"config">>, <<"key='ws_url'">>, #{updated_at => <<"1742777443331">>}).
% imboy_db:update(<<"config">>, <<"key='ws_url'">>, #{updated_at => <<"2025-03-24 08:51:19.562949+08:00">>}).
%% @doc 更新表记录，KV 可为 list/map，内部转换为 SET 子句。
-spec update(binary(), binary(), [list() | binary()]) -> ok | {error, {integer(), binary(), Msg :: binary()}}.
update(Tb, Where, KV) ->
    Driver = config_ds:env(sql_driver),
    with_pooled_conn(
        Driver,
        fun() ->
            imboy_log:error(io_lib:format("imboy_db:update/3 Tb:~p Where:~p KV:~p~n", [Tb, Where, KV]))
        end,
        fun(Conn) ->
            update(Conn, Tb, Where, KV)
        end,
        fun(Res) -> Res end,
        fun(Class, Reason, Stacktrace) ->
            imboy_log:error(io_lib:format("imboy_db:update/3 exception: ~p:~p ~p Tb:~p Where:~p KV:~p~n", [Class, Reason, Stacktrace, Tb, Where, KV]))
        end,
        fun(UnknownConn) ->
            imboy_log:error(io_lib:format("imboy_db:update/3 unknown connection: ~p Tb:~p Where:~p KV:~p~n", [UnknownConn, Tb, Where, KV]))
        end
    ).

update(Conn, Tb, Where, KV) when is_list(KV) ->
    Set = get_set(KV),
    update(Conn, Tb, Where, Set);
update(Conn, Tb, Where, KV) when is_map(KV) ->
    Set = get_set(maps:to_list(KV)),
    update(Conn, Tb, Where, Set);
update(Conn, Tb, Where, SetBin) ->
    Tb2 = public_tablename(Tb),
    Sql = <<"UPDATE ", Tb2/binary, " SET ", SetBin/binary, " WHERE ", Where/binary>>,
    ?DEBUG_LOG(io:format("update/4 sql ~s\n", [Sql])),
    imboy_db:execute(Conn, Sql, []).


%% @doc 将 KV 列表转换为 SET 语句片段。
-spec get_set(list()) -> binary().
get_set(KV) ->
    Set1 = [ <<(ec_cnv:to_binary(K))/binary, " = ", (assemble_value_filter(K, V))/binary>> || {K, V} <- KV ],
    Set2 = [ binary_to_list(S) || S <- Set1 ],
    Set3 = lists:concat(lists:join(", ", Set2)),
    list_to_binary(Set3).

%% @doc 按 [K,Op,V] 三元组列表拼装 where 子句，自动 AND 连接。
assemble_where(Where) ->
    Separator = <<" AND ">>,
    Li2 = [<<
        Separator/binary
        , (ec_cnv:to_binary(K))/binary
        , " "
        , (ec_cnv:to_binary(Op))/binary
        , " "
        , (assemble_value_filter(K, V))/binary
    >> || [K, Op, V] <- Where],
    iolist_to_binary(string:replace(iolist_to_binary(Li2), Separator, "")).


% imboy_db:assemble_value(#{mobile => "13692177080", password => "admin888", account => "13692177080", "status" => 1}).
% imboy_db:assemble_value(#{mobile => <<"13692177080">>, password => "admin888", account => "13692177080A", "status" => 1, "role_id" => {1,3}, "nickname" => <<"大大大"/utf8>>}).
%% @doc 将值集合转换为 SQL 字面量片段，支持 map/list 输入。
assemble_value(Values) when is_map(Values) ->
    assemble_value(maps:values(Values));
assemble_value(Values) when is_list(Values) ->
    [assemble_value_filter(V) || V <- Values].

assemble_value_filter({raw, V}) ->
    V;
assemble_value_filter(V) ->
    original_value_processing(V).

assemble_value_filter(_K, {raw, V}) ->
    V;
assemble_value_filter(K, V) ->
    case K =/= undefined andalso imboy_str:endswith(<<"_at">>, ec_cnv:to_binary(K)) of
        true -> handle_at_field_value(V);
        false -> original_value_processing(V)
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 统一封装连接获取/归还、指数退避等待与异常日志，减少重复样板。
-spec with_pooled_conn(atom(), fun(() -> any()), fun((pid()) -> any()), fun((any()) -> any()), fun((any(), any(), any()) -> any()), fun((any()) -> any())) -> any().
with_pooled_conn(Driver, NoMembersLogFun, ExecFun, ProcessFun, ErrorLogFun, UnknownConnLogFun) ->
    %% 从配置读取退避与重试策略，默认与当前行为一致：1秒固定间隔、无限重试
    RetryConf = get_retry_conf(),
    InitialDelay = proplists:get_value(initial_delay_ms, RetryConf, 1000),
    MaxRetries = proplists:get_value(max_retries, RetryConf, infinity),
    with_pooled_conn_loop(
        Driver, NoMembersLogFun, ExecFun, ProcessFun, ErrorLogFun, UnknownConnLogFun,
        InitialDelay, MaxRetries, RetryConf
    ).

%% @doc 递归循环：统一封装连接获取/归还、指数退避等待与异常日志，带可配置重试策略。
-spec with_pooled_conn_loop(atom(), fun(() -> any()), fun((pid()) -> any()), fun((any()) -> any()), fun((any(), any(), any()) -> any()), fun((any()) -> any()), non_neg_integer(), non_neg_integer() | infinity, list()) -> any().
with_pooled_conn_loop(Driver, NoMembersLogFun, ExecFun, ProcessFun, ErrorLogFun, UnknownConnLogFun, DelayMs, RetriesLeft, RetryConf) ->
    case pooler:take_member(Driver) of
        error_no_members ->
            NoMembersLogFun(),
            %% 若重试次数已耗尽，立即返回明确错误，避免不必要的等待
            case RetriesLeft of
                0 -> {error, {no_available_members, Driver}};
                _ ->
                    % 休眠 DelayMs 毫秒，避免忙等
                    timer:sleep(DelayMs),
                    NextDelay = compute_next_delay(DelayMs, RetryConf),
                    NextRetries = case RetriesLeft of
                        infinity -> infinity;
                        N when N > 0 -> N - 1;
                        0 -> 0
                    end,
                    with_pooled_conn_loop(Driver, NoMembersLogFun, ExecFun, ProcessFun, ErrorLogFun, UnknownConnLogFun, NextDelay, NextRetries, RetryConf)
            end;
        Conn when is_pid(Conn) ->
            try
                Res = ExecFun(Conn),
                % 确保优先归还连接，再处理结果
                pooler:return_member(Driver, Conn),
                ProcessFun(Res)
            catch
                Class:Reason:Stacktrace ->
                    % 确保即使出现异常也归还连接
                    pooler:return_member(Driver, Conn),
                    ErrorLogFun(Class, Reason, Stacktrace),
                    {error, {Class, Reason}}
            end;
        UnknownConn ->
            UnknownConnLogFun(UnknownConn),
            {error, {invalid_connection, UnknownConn}}
    end.

%% @doc 读取连接重试与退避策略的配置。
%% 支持嵌套键：config/sys.config 中可设置 {imboy, [{db, [{connection_retry, [{initial_delay_ms, 1000}, ...]}]}]}。
-spec get_retry_conf() -> list().
get_retry_conf() ->
    Default = [
        {initial_delay_ms, 1000},
        {max_delay_ms, 1000},
        {multiplier, 1.0},
        {jitter_ms, 0},
        {max_retries, 100}
    ],
    case catch config_ds:env([db, connection_retry], Default) of
        Conf when is_list(Conf) -> Conf;
        _ -> Default
    end.

%% @doc 计算下一次延迟：delay = min(delay * multiplier, max_delay_ms)，并加入抖动。
-spec compute_next_delay(non_neg_integer(), list()) -> non_neg_integer().
compute_next_delay(DelayMs, RetryConf) ->
    Multiplier = proplists:get_value(multiplier, RetryConf, 1.0),
    MaxDelay = proplists:get_value(max_delay_ms, RetryConf, DelayMs),
    Jitter = proplists:get_value(jitter_ms, RetryConf, 0),
    Base = min(trunc(DelayMs * Multiplier), MaxDelay),
    apply_jitter(Base, Jitter).

%% @doc 对基础延迟应用抖动，范围 [-Jitter, +Jitter]，并保证非负。
-spec apply_jitter(non_neg_integer(), non_neg_integer()) -> non_neg_integer().
apply_jitter(Base, 0) -> Base;
apply_jitter(Base, Jitter) when Jitter > 0 ->
    Rand = rand:uniform(2 * Jitter + 1) - (Jitter + 1),
    New = Base + Rand,
    case New < 0 of true -> 0; false -> New end.

%% @doc 处理插入数据 Map，生成列与值的二进制片段。
process_insert_data(DataMap) when is_map(DataMap) ->
    Keys = maps:keys(DataMap),
    Column = <<"(", (imboy_cnv:implode(",", Keys))/binary, ")">>,
    Values = [assemble_value_filter(K, maps:get(K, DataMap)) || K <- Keys],
    ValueBin = imboy_cnv:implode(",", Values),
    {Column, <<"(", ValueBin/binary, ")">>}.

%% @doc 处理 *_at 字段的值，数值视为时间戳，其他直接包裹引号。
handle_at_field_value(V) ->
    case imboy_type:is_numeric(V) of
        true ->
            Rfc3339 = imboy_dt:to_rfc3339(ec_cnv:to_integer(V), millisecond),
            imboy_cnv:implode("", ["'", Rfc3339, "'"]);
        false ->
            imboy_cnv:implode("", ["'", escape_sql_literal(V), "'"]) % 假设已经是合法格式
    end.

%% @doc 将原始值转换为 SQL 字面量，内置安全转义，避免注入与语法错误。
original_value_processing(V) ->
    if
        is_list(V); is_binary(V) ->
            Esc = escape_sql_literal(V),
            imboy_cnv:implode("", ["'", Esc, "'"]);
        is_tuple(V) ->
            % 注意：数组/复合类型按原有行为拼接；如需更严格转义可逐项处理
            imboy_cnv:implode("", ["'{", imboy_cnv:implode(",", tuple_to_list(V)), "}'"]);
        true ->
            ec_cnv:to_binary(V)
    end.

%% @doc 将查询结果转换为 map，仅取第一行。
query_resp_map(Res) ->
    % ?DEBUG_LOG([Res]),
    case Res of
        {ok, _, []} ->
            #{};
        {ok, [{column, Col1, _, _, _, _, _, _, _}], [{Val}]} ->
            #{
                Col1 => Val
            };
        {ok, Col, [Val]} ->
            maps:from_list(lists:zipwith(fun(X, Y) -> {X, Y} end, Col, tuple_to_list(Val)));
        _ ->
            #{}
    end.

query_resp({error, Msg}) ->
    {error, Msg};
query_resp({ok, Num}) ->
    {ok, Num};
query_resp({ok, [K], Rows}) ->
    % {ok,[<<"count">>],[{1}]}
    {ok, [K], Rows};
query_resp({ok, ColumnList, Rows}) ->
    % {ok,[{column,<<"max">>,int4,23,4,-1,1,0,0}],[{551223}]}
    % {ok,
    %     [{column,<<"count">>>,int8,20,8,-1,1,0,0}]
    %     , [1]
    % }
    % imboy_log:info(io_lib:format("imboy_db/query_resp: ColumnList ~p, Rows ~p ~n", [ColumnList, Rows])),
    ColumnList2 = [ element(2, C) || C <- ColumnList ],
    {ok, ColumnList2, Rows}.

% private
to_proplists(ColumnLi, Items0) ->
    Items1 = [tuple_to_list(Item) || Item <- Items0],
    [lists:zipwith(fun(X, Y) -> {X, imboy_cnv:json_maybe(Y)} end, ColumnLi, Row) || Row <- Items1].


public_tablename(<<"public.", Tb/binary>>) ->
    public_tablename(Tb);
public_tablename(Tb) ->
    case config_ds:env(sql_driver) of
        pgsql ->
            <<"public.", Tb/binary>>;
        _ ->
            Tb
    end.

%% @doc 转义 SQL 字符串字面量中的单引号，防止语法错误与注入。
-spec escape_sql_literal(binary() | list()) -> binary().
escape_sql_literal(Bin) when is_binary(Bin) ->
    binary:replace(Bin, <<"'">>, <<"''">>, [global]);
escape_sql_literal(List) when is_list(List) ->
    list_to_binary(string:replace(List, "'", "''", all)).


%% ===================================================================
%% EUnit tests.
%% ===================================================================

-ifdef(EUNIT).


updateuser_test_() ->
    KV1 = [{<<"gender">>, <<"1">>}, {<<"nickname">>, <<"中国你好！😆"/utf8>>}],
    KV2 = [{<<"gender">>, <<"1">>}, {<<"nickname">>, "中国你好！😆😆"}],
    Tb = user_repo:tablename(),
    imboy_db:update(Tb, Where, #{
        <<"gender">> => <<"1">>
    });
    [?_assert(imboy_db:update(Tb, <<"id=", (ec_cnv:to_binary(1))/binary>>, KV1)), ?_assert(imboy_db:update(Tb, <<"id=", (ec_cnv:to_binary(2))/binary>>, KV2))].


-endif.
