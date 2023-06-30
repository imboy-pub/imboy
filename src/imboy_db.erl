-module(imboy_db).

-export([pluck/2]).
-export([pluck/3]).
-export([pluck/4]).
-export([query/1]).
-export([query/2]).
-export([execute/2, execute/3]).
-export([insert_into/3]).
-export([assemble_sql/4]).

-export([get_set/1]).
-export([update/3]).
-export([update/4]).
-export([public_tablename/1]).

-export([with_transaction/1]).
-export([with_transaction/2]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("kernel/include/logger.hrl").
-include_lib("imboy/include/log.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec with_transaction(fun((epgsql:connection()) -> Reply)) ->
                              Reply | {rollback, any()}
                                  when
      Reply :: any().
with_transaction(F) ->
    with_transaction(F, [{reraise, false}]).

-spec with_transaction(fun((epgsql:connection()) -> Reply), epgsql:transaction_opts()) -> Reply | {rollback, any()} | no_return() when
      Reply :: any().
with_transaction(F, Opts0) ->
    Driver = imboy_func:env(sql_driver),
    case pooler:take_member(Driver) of
        error_no_members ->
            % 休眠 1秒
            timer:sleep(1),
            with_transaction(F, Opts0);
        Conn ->
            Res = epgsql:with_transaction(Conn, F, Opts0),
            pooler:return_member(Driver, Conn),
        Res
    end.

% imboy_db:pluck(<<"to_tsquery('jiebacfg', '软件中国')"/utf8>>, <<"">>).
pluck(Field, Default) ->
    Sql = <<"SELECT ", Field/binary>>,
    % ?LOG([pluck, Sql]),
    case imboy_db:query(Sql) of
        % {ok,[{column,<<"max">>,int4,23,4,-1,1,0,0}],[{551223}]}
        {ok, _, [{Val}]} ->
            Val;
        _ ->
            Default
    end.
% pluck(<<"public.", Table/binary>>, Field, Default) ->
%     pluck(Table, Field, Default);
pluck(Table, Field, Default) ->
    Table2 = public_tablename(Table),
    Sql = <<"SELECT ", Field/binary, " FROM ", Table2/binary>>,
    % ?LOG([pluck, Sql]),
    case imboy_db:query(Sql) of
        % {ok,[{column,<<"max">>,int4,23,4,-1,1,0,0}],[{551223}]}
        {ok, _, [{Val}]} ->
            Val;
        _ ->
            Default
    end.

pluck(Table, Where, Field, Default) ->
    Table2 = public_tablename(Table),
    Sql = <<"SELECT ", Field/binary, " FROM ", Table2/binary, " WHERE ", Where/binary>>,
    ?LOG([pluck, Sql]),
    case imboy_db:query(Sql) of
        % {ok,[{column,<<"max">>,int4,23,4,-1,1,0,0}],[{551223}]}
        {ok, _, [Val]} when size(Val) == 1 ->
            element(1, Val);
        {ok, _, [Val]} ->
            Val;
        _ ->
            Default
    end.

% imboy_db:query("select * from user where id = 2")
-spec query(binary() | list()) -> {ok, list(), list()} | {error, any()}.
query(Sql) ->
    Driver = imboy_func:env(sql_driver),
    Conn = pooler:take_member(Driver),
    Res = case Driver of
        pgsql when is_pid(Conn) ->
            epgsql:equery(Conn, Sql);
        pgsql when Conn == error_no_members->
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
    Driver = imboy_func:env(sql_driver),
    Conn = pooler:take_member(Driver),
    Res = case Driver of
        pgsql when is_pid(Conn) ->
            epgsql:equery(Conn, Sql, Params);
        pgsql when Conn == error_no_members->
            % 休眠 1秒
            timer:sleep(1),
            query(Sql, Params);
        _ ->
            {error, not_supported}
    end,
    pooler:return_member(Driver, Conn),
    query_resp(Res).

-spec execute(any(), list()) ->
          {ok, LastInsertId :: integer()} | {error, any()}.
execute(Sql, Params) ->
    Driver = imboy_func:env(sql_driver),
    Conn = pooler:take_member(Driver),
    Res = case Driver of
        pgsql when is_pid(Conn) ->
            % {ok, 1} | {ok, 1, {ReturningField}}
            execute(Conn, Sql, Params);
        pgsql when Conn == error_no_members->
            % 休眠 1秒
            timer:sleep(1),
            execute(Sql, Params);
        _ ->
            {error, not_supported}
    end,
    pooler:return_member(Driver, Conn),
    Res.

execute(Conn, Sql, Params) ->
    {ok, Stmt} = epgsql:parse(Conn, Sql),
    [Res0] = epgsql:execute_batch(Conn, [{Stmt, Params}]),
    % {ok, 1} | {ok, 1, {ReturningField}}
    Res0.

% replace_into(Table, Column, Value) ->
%     % Sql like this "REPLACE INTO foo (k,v) VALUES (1,0), (2,0)"
%     Sql = assemble_sql(<<"REPLACE INTO">>, Table, Column, Value),
%     imboy_db:execute(Sql, []).


insert_into(Table, Column, Value) ->
    % Sql like this "INSERT INTO foo (k,v) VALUES (1,0), (2,0)"
    Sql = assemble_sql(<<"INSERT INTO">>, Table, Column, Value),
    imboy_db:execute(Sql, []).


% 组装 SQL 语句
assemble_sql(Prefix, Table, Column, Value) when is_list(Column) ->
    ColumnBin = imboy_func:implode(",", Column),
    assemble_sql(Prefix, Table, <<"(", ColumnBin/binary, ")">>, Value);
assemble_sql(Prefix, Table, Column, Value) when is_list(Value) ->
    ValueBin = imboy_func:implode(",", Value),
    assemble_sql(Prefix, Table, Column, <<"(", ValueBin/binary, ")">>);
assemble_sql(Prefix, Table, Column, Value) ->
    Table2 = public_tablename(Table),
    Sql = <<Prefix/binary, " ", Table2/binary, " ", Column/binary,
            " VALUES ", Value/binary>>,
    ?LOG(io:format("~s\n", [Sql])),
    Sql.

% imboy_db:update(<<"user">>, 1, <<"sign">>, <<"中国你好！😆"/utf8>>).
-spec update(binary(), integer(), binary(), list() | binary()) ->
    ok | {error,  {integer(), binary(), Msg::binary()}}.
update(Table, ID, Field, Value) when is_list(Value) ->
    update(Table, ID, Field, unicode:characters_to_binary(Value));
update(Table, ID, Field, Value) ->
    Table2 = public_tablename(Table),
    Sql = <<"UPDATE ", Table2/binary," SET ",
        Field/binary, " = $1 WHERE id = $2">>,
    imboy_db:execute(Sql, [Value, ID]).

-spec update(binary(), integer(), list()) ->
    ok | {error,  {integer(), binary(), Msg::binary()}}.
update(Table, ID, KV) ->
    Set = get_set(KV),
    Table2 = public_tablename(Table),
    Sql = <<"UPDATE ", Table2/binary," SET ", Set/binary," WHERE id = $1">>,
    % ?LOG(io:format("~s\n", [Sql])),
    imboy_db:execute(Sql, [ID]).

-spec get_set(list()) -> binary().
get_set(KV) ->
    KV2 = [{K, update_filter_value(V)} || {K, V} <- KV],
    Set1 = [<<K/binary, " = '", V/binary, "'">> || {K, V} <- KV2],
    Set2 = [binary_to_list(S) || S <- Set1],
    Set3 = lists:concat(lists:join(", ", Set2)),
    list_to_binary(Set3).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================


query_resp({error, Msg}) ->
    {error, Msg};
query_resp({ok,[K], Rows}) ->
    % {ok,[<<"count">>],[{1}]}
    {ok, [K], Rows};
query_resp({ok, ColumnList, Rows}) ->
    % {ok,[{column,<<"max">>,int4,23,4,-1,1,0,0}],[{551223}]}
    % {ok,[{column,<<"count">>,int8,20,8,-1,1,0,0}],[1]}
    % lager:info(io_lib:format("imboy_db/query_resp: ColumnList ~p, Rows ~p ~n", [ColumnList, Rows])),
    ColumnList2 = [element(2, C) || C <- ColumnList],
    {ok, ColumnList2, Rows}.

public_tablename(<<"public.", Table/binary>>) ->
    public_tablename(Table);
public_tablename(Table) ->
    case imboy_func:env(sql_driver) of
        pgsql ->
            <<"public.", Table/binary>>;
        _ ->
            Table
    end.

update_filter_value(Val) when is_binary(Val) ->
    Val;
update_filter_value(Val) ->
    unicode:characters_to_binary(Val).

%% ===================================================================
%% EUnit tests.
%% ===================================================================

-ifdef(EUNIT).

updateuser_test_() ->
    KV1 = [{<<"gender">>, <<"1">>}, {<<"nickname">>, <<"中国你好！😆"/utf8>>}],
    KV2 = [{<<"gender">>, <<"1">>}, {<<"nickname">>, "中国你好！😆😆"}],

    [
        ?_assert(imboy_db:update(<<"user">>, 1, KV1)),
        ?_assert(imboy_db:update(<<"user">>, 2, KV2))
    ].

-endif.
