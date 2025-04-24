-module(imboy_db).


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
-export([execute/2, execute/3]).

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
    with_transaction(F, Opts0, 3, 1). % 最大重试3次，初始延迟1秒

with_transaction(_F, _Opts0, 0, _Delay) ->
    {error, retries_exhausted};
with_transaction(F, Opts0, RetriesLeft, Delay) ->
    Driver = config_ds:env(sql_driver),
    case pooler:take_member(Driver) of
        error_no_members ->
            timer:sleep(Delay * 1000),
            with_transaction(F, Opts0, RetriesLeft - 1, Delay * 2);
        Conn ->
            Res = epgsql:with_transaction(Conn, F, Opts0),
            pooler:return_member(Driver, Conn),
            Res
    end.

% imboy_db:pluck(<<"SELECT to_tsquery('jiebacfg', '软件中国')"/utf8>>, <<"">>).
% imboy_db:pluck(<<"adm_user">>, <<>>, <<"count(*) as count">>, 0).
% imboy_db:pluck(<<"user">>, <<"1=1">>, <<"count(*) as count">>, 0).

% pluck(<<"public.", Tb/binary>>, Field, Default) ->
%     pluck(Tb, Field, Default);
pluck(Tb, Field, Default) ->
    Tb2 = public_tablename(Tb),
    Sql = <<"SELECT ", Field/binary, " FROM ", Tb2/binary>>,
    % ?LOG([pluck, Sql]),
    pluck(Sql, Default).


pluck(Tb, <<>>, Field, Default) ->
    Tb2 = public_tablename(Tb),
    Sql = <<"SELECT ", Field/binary, " FROM ", Tb2/binary>>,
    % ?LOG([pluck, Sql]),
    pluck(Sql, Default);
pluck(Tb, Where, Field, Default) ->
    Tb2 = public_tablename(Tb),
    Sql = <<"SELECT ", Field/binary, " FROM ", Tb2/binary, " WHERE ", Where/binary>>,
    % ?LOG([pluck, Sql]),
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

find(Tb, Where, OrderBy, Column) ->
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE ", Where/binary, " ORDER BY ", OrderBy/binary, " LIMIT 1">>,
    % ?LOG([find, Sql]),
    find(Sql).

find(Sql) ->
    query_resp_map(imboy_db:query(Sql)).

find(Sql, Params) ->
    query_resp_map(imboy_db:query(Sql, Params)).


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
    % ?LOG(['Sql', Sql]),
    % ?LOG(['Res', Res]),
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

% imboy_db:query("select * from user where id = 2")
% imboy_db:query("select created_at from adm_user limit 1").
-spec query(binary() | list()) -> {ok, list(), list()} | {error, any()}.
query(Sql) ->
    % ?LOG([imboy_dt:now(), Sql]),
    Driver = config_ds:env(sql_driver),
    Conn = pooler:take_member(Driver),
    Res =
        case Driver of
            pgsql when is_pid(Conn) ->
                epgsql:equery(Conn, Sql);
            pgsql when Conn == error_no_members ->
                imboy_log:error(io_lib:format("imboy_db:query/1 sql:~s;~n", [Sql])),
                % ?LOG([imboy_dt:now(), Sql]),
                % 休眠 1秒
                timer:sleep(1),
                query(Sql);
            _ ->
                {error, not_supported}
        end,
    pooler:return_member(Driver, Conn),
    query_resp(Res).


-spec query(binary() | list(), list()) -> {ok, list(), list()} | {error, any()}.
query(Sql, Params) ->
    Driver = config_ds:env(sql_driver),
    Conn = pooler:take_member(Driver),
    Res =
        case Driver of
            pgsql when is_pid(Conn) ->
                epgsql:equery(Conn, Sql, Params);
            pgsql when Conn == error_no_members ->
                imboy_log:error(io_lib:format("imboy_db:query/1 sql:~s, ~p;~n", [Sql,Params])),
                % 休眠 1秒
                timer:sleep(1),
                query(Sql, Params);
            _ ->
                {error, not_supported}
        end,
    pooler:return_member(Driver, Conn),
    query_resp(Res).


-spec execute(any(), list()) -> {ok, LastInsertId :: integer()} | {error, any()}.
execute(Sql, Params) ->
    % ?LOG(io:format("~s\n", [Sql])),
    Driver = config_ds:env(sql_driver),
    Conn = pooler:take_member(Driver),
    Res =
        case Driver of
            pgsql when is_pid(Conn) ->
                % {ok, 1} | {ok, 1, {ReturningField}}
                execute(Conn, Sql, Params);
            pgsql when Conn == error_no_members ->
                imboy_log:error(io_lib:format("imboy_db:execute/1 sql:~s, ~p;~n", [Sql,Params])),
                % 休眠 1秒
                timer:sleep(1),
                execute(Sql, Params);
            _ ->
                {error, not_supported}
        end,
    pooler:return_member(Driver, Conn),
    ?LOG(['execute ', Res]),
    Res.


execute(Conn, Sql, Params) ->
    ?LOG(io:format("sql: ~s\n", [Sql])),
    % ?LOG(io:format("Params: ~p\n", [Params])),
    % Res = epgsql:parse(Conn, Sql),
    % ?LOG(io:format("epgsql:parse Res: ~p\n", [Res])),
    % {ok, Stmt} = Res,
    {ok, Stmt} = epgsql:parse(Conn, Sql),
    [Res2] = epgsql:execute_batch(Conn, [{Stmt, Params}]),
    % ?LOG(io:format("execute/3 Res2: ~p\n", [Res2])),
    % {ok, 1} | {ok, 1, {ReturningField}} | {ok,1,[{5}]}
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
    ?LOG([insert_into, Sql]),
    execute(<<Sql/binary, " ", ReturningOnConflict/binary>>, []).

add(Conn, Tb, Data) ->
    add(Conn, Tb, Data, <<"RETURNING id;">>).
% imboy_db:add/4
add(Conn, Tb, Data, ReturningOnConflict) ->
    {Column, Value} = process_insert_data(Data),
    Sql = assemble_sql(<<"INSERT INTO">>, Tb, Column, Value),
    % ?LOG(io:format("~s\n", [Sql])),
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
    % ?LOG(io:format("~s\n", [Sql])),
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
    ?LOG(io:format("update/4 sql ~s\n", [Sql])),
    imboy_db:execute(Conn, Sql, []).


-spec get_set(list()) -> binary().
get_set(KV) ->
    Set1 = [ <<(ec_cnv:to_binary(K))/binary, " = ", (assemble_value_filter(K, V))/binary>> || {K, V} <- KV ],
    Set2 = [ binary_to_list(S) || S <- Set1 ],
    Set3 = lists:concat(lists:join(", ", Set2)),
    list_to_binary(Set3).

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

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

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
        is_list(V); is_binary(V) ->
            imboy_cnv:implode("", ["'", V, "'"]);
        is_tuple(V) ->
            imboy_cnv:implode("", ["'{", imboy_cnv:implode(",", tuple_to_list(V)), "}'"]);
        true ->
            ec_cnv:to_binary(V)
    end.

query_resp_map(Res) ->
    % ?LOG([Res]),
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
            <<"public.", Tb/binary>>;
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
    imboy_db:update(Tb, Where, #{
        <<"gender">> => <<"1">>
    });
    [?_assert(imboy_db:update(Tb, <<"id=", (ec_cnv:to_binary(1))/binary>>, KV1)), ?_assert(imboy_db:update(Tb, <<"id=", (ec_cnv:to_binary(2))/binary>>, KV2))].


-endif.
