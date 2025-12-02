-module(imboy_db).


%% 数据库操作模块 - 提供高级数据库访问接口
%% 支持 PostgreSQL 数据库，包含查询、更新、事务、分页等功能

-export([pluck/2]).
-export([pluck/3]).
-export([pluck/4]).
-export([find/1, find/2, find/4]).
-export([list/1, list/2]).
-export([proplists/1, proplists/2]).
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

-spec with_transaction(fun((epgsql:connection()) -> Reply)) -> Reply | {rollback, any()} when Reply :: any().
with_transaction(F) ->
    with_transaction(F, [{reraise, true}]).


-spec with_transaction(fun((epgsql:connection()) -> Reply), epgsql:transaction_opts()) ->
          Reply | {rollback, any()} | no_return() when Reply :: any().
with_transaction(F, Opts0) ->
    with_transaction(F, Opts0, 3, 200). % 最大重试3次，初始延迟100毫秒


%% ===================================================================
%% 事务封装
%% ===================================================================
-spec with_transaction(fun((pid()) -> any()), list(), non_neg_integer(), non_neg_integer()) -> any().
with_transaction(F, Opts0, RetriesLeft, Delay) ->
    Driver = config_ds:env(sql_driver),
    do_with_conn(Driver,
        fun(Conn) ->
            epgsql:with_transaction(Conn, F, Opts0)
        end,
        RetriesLeft,
        Delay).

%% @doc 执行 SQL 查询并返回第一行的第一个字段值
%% @param Query 完整的 SQL 查询语句（不包含 SELECT）
%% @param Default 默认值
%% @returns 查询结果或默认值
%% 示例: imboy_db:pluck(<<"to_tsquery('jiebacfg', '软件中国')">>, <<>>)
-spec pluck(binary(), any()) -> any().
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


%% @doc 从指定表查询字段值，无 WHERE 条件
%% @param Tb 表名
%% @param Field 要查询的字段
%% @param Default 默认值
%% @returns 查询结果或默认值
%% 示例: imboy_db:pluck(<<"adm_user">>, <<>>, <<"count(*) as count">>, 0)
-spec pluck(binary(), binary(), any()) -> any().
pluck(Tb, Field, Default) ->
    Tb2 = public_tablename(Tb),
    Sql = <<"SELECT ", Field/binary, " FROM ", Tb2/binary>>,
    % ?DEBUG_LOG([pluck, Sql]),
    pluck(Sql, Default).


%% @doc 从指定表查询字段值，支持 WHERE 条件
%% @param Tb 表名
%% @param Where WHERE 条件，空二进制表示无条件
%% @param Field 要查询的字段
%% @param Default 默认值
%% @returns 查询结果或默认值
%% 示例: imboy_db:pluck(<<"user">>, <<"1=1">>, <<"count(*) as count">>, 0)
-spec pluck(binary(), binary(), binary(), any()) -> any().
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

%% @doc 查找表中单条记录，支持排序条件
%% @param Tb 表名
%% @param Where WHERE 条件
%% @param OrderBy 排序条件
%% @param Column 要查询的列
%% @returns 包含查询结果的 map 或空 map
-spec find(binary(), binary(), binary(), binary()) -> map().
find(Tb, Where, OrderBy, Column) ->
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE ", Where/binary, " ORDER BY ", OrderBy/binary, " LIMIT 1">>,
    % ?DEBUG_LOG([find, Sql]),
    find(Sql).

%% @doc 执行查询并返回第一条记录的 map 格式
%% @param Sql SQL 查询语句
%% @returns 包含查询结果的 map 或空 map
-spec find(binary()) -> map().
find(Sql) ->
    find(Sql, []).

%% @doc 执行带参数的查询并返回第一条记录的 map 格式
%% @param Sql SQL 查询语句
%% @param Params 查询参数列表
%% @returns 包含查询结果的 map 或空 map
-spec find(binary(), list()) -> map().
find(Sql, Params) ->
    find_resp_map(imboy_db:query(Sql, Params)).


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

%% @doc 统计满足条件的记录数
%% @param Tb 表名
%% @param Where WHERE 条件
%% @returns 记录总数
-spec count_for_where(binary(), binary()) -> non_neg_integer().
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
    ?DEBUG_LOG(['Sql', Sql, ' ', Limit, Offset]),
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

proplists(Sql) ->
    case imboy_db:query(Sql) of
        {ok, Col, Val} ->
            to_proplists(Col, Val);
        _ ->
            []
    end.

proplists(Sql, Params) ->
    case imboy_db:query(Sql, Params) of
        {ok, Col, Val} ->
            to_proplists(Col, Val);
        _ ->
            []
    end.

list(Sql) ->
    case imboy_db:query(Sql) of
        {ok, _, Val} ->
            Val;
        _ ->
            []
    end.

list(Conn, Sql) ->
    case epgsql:equery(Conn, Sql) of
        {ok, _, Val} ->
            Val;
        _ ->
            []
    end.

% imboy_db:query("select * from public.user where id = 2").
% imboy_db:query("select created_at from adm_user limit 1").
-spec query(binary() | list()) -> {ok, list(), list()} | {error, any()}.
query(Sql) ->
    % ?DEBUG_LOG([imboy_dt:now(), Sql]),
    query(Sql, []).


-spec query(binary() | list(), list()) -> {ok, list(), list()} | {error, any()}.
query(Sql, Params) ->
    Driver = config_ds:env(sql_driver),
    do_with_conn(Driver,
        fun(Conn) ->
            case Driver of
                pgsql ->
                    Res = epgsql:equery(Conn, Sql, Params),
                    query_resp(Res);
                _ ->
                    {error, not_supported}
            end
        end,
        3, 100
    ).

execute(Sql) ->
    % ?DEBUG_LOG(io:format("~s\n", [Sql])),
    execute(Sql, []).

%% @doc 执行 SQL 语句并支持参数化查询
%% @param Sql SQL 语句
%% @param Params 查询参数列表
%% @returns {ok, LastInsertId} | {error, any()}
-spec execute(binary(), list()) -> {ok, non_neg_integer()} | {error, any()}.
execute(Sql, Params) ->
    Driver = config_ds:env(sql_driver),
    do_with_conn(Driver,
        fun(Conn) ->
            case Driver of
                pgsql -> execute(Conn, Sql, Params);
                _ -> {error, not_supported}
            end
        end,
        3, 100).

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
    % 没有 RETURNING 子句：返回 {ok, 1} （二元组）
    % 有 RETURNING 子句：返回 {ok, 1, Result} （三元组），其中 Result 可能是 {Id} 或 [{Id}]
    Res2.

% imboy_db:insert_into/3
insert_into(Tb, Data) when is_map(Data) ->
    {Column, Value} = process_insert_data(Data),
    imboy_db:insert_into(Tb, Column, Value).

insert_into(Tb, Data, ReturningOnConflict) when is_map(Data) ->
    {Column, Value} = process_insert_data(Data),
    insert_into(Tb, Column, Value, ReturningOnConflict);
insert_into(Tb, Column, Value) ->
    insert_into(Tb, Column, Value, <<"RETURNING id;">>).


insert_into(Tb, Column, Value, ReturningOnConflict) ->
    % Sql like this "INSERT INTO foo (k,v) VALUES (1,0), (2,0)"
    % return {ok,1,[{10}]}
    Sql = assemble_sql(<<"INSERT INTO">>, Tb, Column, Value),
    % ?DEBUG_LOG([insert_into, Sql]),
    % {ok,1,[{5}]}
    execute(<<Sql/binary, " ", ReturningOnConflict/binary>>, []).

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
-spec update(binary(), binary(), [list() | binary()])
    -> ok | {error, {integer(), binary(), Msg :: binary()}}.
update(Tb, Where, KV) ->
    Driver = config_ds:env(sql_driver),
    Conn = pooler:take_member(Driver),
    Res = update(Conn, Tb, Where, KV),
    pooler:return_member(Driver, Conn),
    Res.

update(Conn, Tb, Where, KV) when is_list(KV) ->
    Set = get_set(KV),
    update(Conn, Tb, Where, Set);
update(Conn, Tb, Where, KV) when is_map(KV) ->
    Set = get_set(maps:to_list(KV)),
    update(Conn, Tb, Where, Set);
update(Conn, Tb, Where, SetBin) ->
    Tb2 = public_tablename(Tb),
    Sql = <<"UPDATE ", Tb2/binary, " SET ", SetBin/binary, " WHERE ", Where/binary>>,
    % ?DEBUG_LOG(io:format("update/4 sql ~s\n", [Sql])),
    imboy_db:execute(Conn, Sql, []).


-spec get_set(list()) -> binary().
get_set(KV) ->
    Set1 = [ <<(ec_cnv:to_binary(K))/binary, " = ", (safe_assemble_value_filter(K, V))/binary>> || {K, V} <- KV ],
    case Set1 of
        [] ->
            <<>>;
        _ ->
            Set2 = [ binary_to_list(S) || S <- Set1 ],
            Set3 = lists:concat(lists:join(", ", Set2)),
            list_to_binary(Set3)
    end.

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

%% 确保返回值总是二进制
safe_assemble_value_filter(K, V) ->
    Result = assemble_value_filter(K, V),
    case is_binary(Result) of
        true -> Result;
        false -> ec_cnv:to_binary(Result)
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% ===================================================================
%% 统一的安全封装：带重试 + 异常捕获 + 连接回收
%% ===================================================================
%% Fun    : (Conn -> Result)
%% Retries: 最大重试次数
%% Delay  : 初始重试延迟（毫秒）
do_with_conn(Driver, Fun, Retries, Delay) ->
    case pooler:take_member(Driver) of
        error_no_members when Retries > 0 ->
            timer:sleep(Delay),
            do_with_conn(Driver, Fun, Retries - 1, Delay + 1000);
        error_no_members ->
            {error, no_connection};
        Conn when is_pid(Conn) ->
            try
                Res = Fun(Conn),
                pooler:return_member(Driver, Conn),
                Res
            catch
                Class:Reason:Stacktrace ->
                    pooler:return_member(Driver, Conn),
                    ?LOG_ERROR("DB operation failed: ~p:~p stack=~p~n", [Class, Reason, Stacktrace]),
                    if Retries > 0 ->
                        timer:sleep(Delay),
                        do_with_conn(Driver, Fun, Retries - 1, Delay + 1000);
                       true ->
                        {error, {Class, Reason}}
                    end
            end;
        Other ->
            {error, {unexpected_conn, Other}}
    end.

process_insert_data(DataMap) when is_map(DataMap) ->
    Keys = maps:keys(DataMap),
    Column = <<"(", (imboy_cnv:implode(",", Keys))/binary, ")">>,
    Values = [assemble_value_filter(K, maps:get(K, DataMap)) || K <- Keys],
    ValueBin = imboy_cnv:implode(",", Values),
    {Column, <<"(", ValueBin/binary, ")">>}.

handle_at_field_value(V) ->
    case imboy_type:is_numeric(V) of
        true ->
            Rfc3339 = imboy_dt:to_rfc3339(ec_cnv:to_integer(V), millisecond),
            imboy_cnv:implode("", ["'", Rfc3339, "'"]);
        false ->
            imboy_cnv:implode("", ["'", V, "'"]) % 假设已经是合法格式
    end.

original_value_processing(V) ->
    if
        is_list(V) ->
            case V of
                [] -> "'{}'";  % 空列表转换为 PostgreSQL 空数组格式
                _ -> imboy_cnv:implode("", ["'", V, "'"])
            end;
        is_binary(V) ->
            imboy_cnv:implode("", ["'", V, "'"]);
        is_tuple(V) ->
            imboy_cnv:implode("", ["'{", imboy_cnv:implode(",", tuple_to_list(V)), "}'"]);
        true ->
            ec_cnv:to_binary(V)
    end.

find_resp_map(Res) ->
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
    %     [{column,<<"count">>,int8,20,8,-1,1,0,0}]
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
            <<"public.", (ec_cnv:to_binary(Tb))/binary>>;
        _ ->
            Tb
    end.



%% ===================================================================
%% EUnit tests.
%% ===================================================================

-ifdef(EUNIT).


updateuser_test_() ->
    KV1 = [{<<"gender">>, <<"1">>}, {<<"nickname">>, <<"中国你好！😆"/utf8>>}],
    KV2 = [{<<"gender">>, <<"1">>}, {<<"nickname">>, "中国你好！😆😆"}],
    Tb = user_repo:tablename(),
    Where = <<"id = 1">>,
    
    [?_assert(imboy_db:update(Tb, <<"id=", (ec_cnv:to_binary(1))/binary>>, KV1)), ?_assert(imboy_db:update(Tb, <<"id=", (ec_cnv:to_binary(2))/binary>>, KV2))].


-endif.
