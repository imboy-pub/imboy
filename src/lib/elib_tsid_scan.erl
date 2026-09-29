%%%-------------------------------------------------------------------
%%% @doc TSID catalog ↔ 数据库扫描器（只读；自举 floor 的唯一权威实现）
%%%
%%% 用户批准方案·步骤 5/7 的数据库侧。escript（scripts/tsid/）必须复用
%%% 本模块，禁止在脚本里复制 SQL（步骤 6：退役玩具脚本）。
%%%
%%% 语义（全部 FAIL 级，出错返回 {error, Reason}，不得降 warning）：
%%%  1. 只读快照：BEGIN ISOLATION LEVEL REPEATABLE READ READ ONLY
%%%     （经 elib_pg:with_tx(F, Opts) 的 begin_opts；不占用业务写路径）；
%%%  2. schema 校验（check_schema/2）：catalog 每个 {Table, Column}
%%%     存在且数据类型为 bigint；缺表/坏列 → {error, {schema_drift, D}}；
%%%  3. 反向发现：information_schema 找出「单列 bigint 主键」全集，
%%%     与 catalog 求差——
%%%       发现了 catalog 没有的   → {error, {unclassified_primary_keys, L}}
%%%       catalog 声明了但主键非单列 bigint → {error, {catalog_mismatch, D}}
%%%  4. 自举 floor（bootstrap_floor/1）：每个 catalog 表
%%%     SELECT <col> FROM <table> ORDER BY <col> DESC LIMIT 1
%%%     （禁止 count(*) / 全表聚合）；
%%%     floor_safe_before 为相对毫秒域（与 elib_tsid_store 的 safe_before
%%%     同域，guard 直接与 wall-clock rel-ms 比较）：
%%%       floor = (max_slot bsr 11) + 1，
%%%       其中 max_slot = max(各表 max_id → elib_tsid:id_to_slot/1)；
%%%     所有表为空 → 0（守卫从当前时钟起步，空库开箱即用）。
%%%
%%% == Implementation notes ==
%%%
%%% <ul>
%%% <li>begin_opts: epgsql:with_transaction itself concatenates
%%%     &lt;&lt;"BEGIN "&gt;&gt; ++ begin_opts (see
%%%     deps/epgsql/src/epgsql.erl), so the option value carries only
%%%     "ISOLATION LEVEL REPEATABLE READ READ ONLY" and must NOT repeat
%%%     the BEGIN keyword. elib_pg:with_tx/2 passes transaction_opts
%%%     through verbatim; no hand-written BEGIN/COMMIT anywhere, so the
%%%     "business layer must not issue BEGIN/COMMIT" discipline holds.</li>
%%% <li>Query seam: besides conn_fun (snapshot boundary) and schema_fun
%%%     (information_schema reader), every per-table max query goes
%%%     through query_fun, defaulting to elib_pg:query/3. Unit tests
%%%     inject all three and never touch a real database.</li>
%%% <li>Identifier injection safety: PostgreSQL cannot bind identifiers
%%%     (table/column names) as query parameters, so the per-table max
%%%     statement must interpolate them. Safety argument: every name is
%%%     validated against ^[a-z][a-z0-9_]*$ (max 63 bytes, PostgreSQL's
%%%     identifier limit) in normalize_catalog/1 BEFORE any SQL is built;
%%%     names originate solely from the compile-time catalog or test
%%%     injections, never from user input; the statement's parameter
%%%     list is always empty. Validation failure yields
%%%     {error, {invalid_identifier, _}} and is rejected fail-fast,
%%%     before a connection is even opened.</li>
%%% <li>Representation: table/column names in all return values and
%%%     error payloads are binaries. information_schema natively returns
%%%     binaries; catalog atoms are converted once in normalize_catalog/1
%%%     (no atoms are ever created from database-sourced data). Empty
%%%     tables appear in per_table as max_id => undefined and
%%%     slot => undefined, so per_table always covers the whole catalog.</li>
%%% </ul>
%%%-------------------------------------------------------------------
-module(elib_tsid_scan).

-export([scan/1, check_schema/2, bootstrap_floor/1]).

-export_type([catalog_entry/0, conn_fun/0, schema_fun/0, query_fun/0, schema_map/0]).

-type catalog_entry() ::
    {Table :: atom() | binary(), Column :: atom() | binary()}.

%% Connection seam: wraps the entire read-only snapshot. Default opens a
%% pooled connection via elib_pg:with_tx with a REPEATABLE READ READ ONLY
%% begin_opts; tests inject a fun that just runs the inner fun on a dummy.
-type conn_fun() :: fun((fun((term()) -> term())) -> term()).

%% information_schema reader seam (columns + primary keys of one schema).
-type schema_fun() :: fun((Conn :: term()) -> {ok, schema_map()} | {error, term()}).

%% Data-path seam for per-table max queries. Rows are maps keyed by the
%% column-name atom (the shape elib_pg:query/3 returns).
-type query_fun() ::
    fun((Conn :: term(), Sql :: iodata(), Params :: [term()]) -> {ok, [map()]} | {error, term()}).

-type schema_map() :: #{
    columns := [{Table :: binary(), Column :: binary(), DataType :: binary()}],
    primary_keys := [{Table :: binary(), [Column :: binary()]}]
}.

%%--------------------------------------------------------------------
%% @doc Full scan: schema check + reverse discovery + bootstrap floor,
%% all inside ONE read-only snapshot provided by conn_fun.
%%--------------------------------------------------------------------
-spec scan(map()) ->
    {ok, #{floor_safe_before := non_neg_integer(), per_table := [map()]}}
    | {error, term()}.
scan(Opts) when is_map(Opts) ->
    case normalized_catalog(Opts) of
        {ok, Catalog} ->
            ConnFun = maps:get(conn_fun, Opts, fun default_conn_fun/1),
            SchemaFun = maps:get(schema_fun, Opts, fun default_schema_fun/1),
            QueryFun = maps:get(query_fun, Opts, fun default_query_fun/3),
            Result = ConnFun(fun(Conn) ->
                run_snapshot(Catalog, Conn, SchemaFun, QueryFun)
            end),
            normalize_snapshot(Result);
        {error, _} = Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc Standalone schema validation against one read-only snapshot.
%%--------------------------------------------------------------------
-spec check_schema([catalog_entry()], map()) -> ok | {error, term()}.
check_schema(Catalog, Opts) when is_map(Opts) ->
    case normalize_catalog(Catalog) of
        {ok, Norm} ->
            ConnFun = maps:get(conn_fun, Opts, fun default_conn_fun/1),
            SchemaFun = maps:get(schema_fun, Opts, fun default_schema_fun/1),
            normalize_snapshot(
                ConnFun(fun(Conn) ->
                    check_schema_in_conn(Norm, Conn, SchemaFun)
                end)
            );
        {error, _} = Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc Standalone bootstrap floor (escript entry point:
%% elib_tsid_scan:bootstrap_floor(#{})).
%%--------------------------------------------------------------------
-spec bootstrap_floor(map()) -> {ok, non_neg_integer()} | {error, term()}.
bootstrap_floor(Opts) when is_map(Opts) ->
    case normalized_catalog(Opts) of
        {ok, Catalog} ->
            ConnFun = maps:get(conn_fun, Opts, fun default_conn_fun/1),
            QueryFun = maps:get(query_fun, Opts, fun default_query_fun/3),
            normalize_snapshot(
                ConnFun(fun(Conn) ->
                    case bootstrap_floor_in_conn(Catalog, Conn, QueryFun) of
                        {ok, #{floor_safe_before := Floor}} -> {ok, Floor};
                        {error, _} = Error -> Error
                    end
                end)
            );
        {error, _} = Error ->
            Error
    end.

%%%===================================================================
%%% Internal — snapshot orchestration
%%%===================================================================

%% All three stages observe the same database state because they run
%% inside a single conn_fun invocation.
run_snapshot(Catalog, Conn, SchemaFun, QueryFun) ->
    case check_schema_in_conn(Catalog, Conn, SchemaFun) of
        ok ->
            case discover_and_compare(Catalog, Conn, SchemaFun) of
                ok ->
                    bootstrap_floor_in_conn(Catalog, Conn, QueryFun);
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

check_schema_in_conn(Catalog, Conn, SchemaFun) ->
    case read_schema(SchemaFun, Conn) of
        {ok, SchemaMap} ->
            Columns = maps:get(columns, SchemaMap, []),
            {Missing, WrongType} = classify_catalog_columns(Catalog, Columns),
            case Missing =:= [] andalso WrongType =:= [] of
                true ->
                    ok;
                false ->
                    {error,
                        {schema_drift, #{
                            missing => lists:sort(Missing),
                            wrong_type => lists:sort(WrongType)
                        }}}
            end;
        {error, _} = Error ->
            Error
    end.

classify_catalog_columns(Catalog, Columns) ->
    lists:foldl(
        fun({Table, Column}, {Missing, WrongType}) ->
            case find_column_type(Columns, Table, Column) of
                not_found ->
                    {Missing ++ [{Table, Column}], WrongType};
                <<"bigint">> ->
                    {Missing, WrongType};
                ActualType ->
                    {Missing, WrongType ++ [{Table, Column, ActualType}]}
            end
        end,
        {[], []},
        Catalog
    ).

%%%===================================================================
%%% Internal — reverse discovery
%%%===================================================================

discover_and_compare(Catalog, Conn, SchemaFun) ->
    case read_schema(SchemaFun, Conn) of
        {ok, SchemaMap} ->
            Columns = maps:get(columns, SchemaMap, []),
            PrimaryKeys = maps:get(primary_keys, SchemaMap, []),
            Classified = maps:from_list([{Table, true} || {Table, _Col} <- Catalog]),
            %% "single-column bigint primary key" tables not named by catalog
            Discovered =
                [
                    Table
                 || {Table, KeyCols} <- PrimaryKeys,
                    length(KeyCols) =:= 1,
                    find_column_type(Columns, Table, hd(KeyCols)) =:= <<"bigint">>,
                    not maps:is_key(Table, Classified)
                ],
            case lists:usort(Discovered) of
                [] ->
                    check_catalog_pks(Catalog, PrimaryKeys, Columns);
                Extra ->
                    {error, {unclassified_primary_keys, Extra}}
            end;
        {error, _} = Error ->
            Error
    end.

%% Every catalog entry's actual primary key must be exactly the declared
%% single column; all mismatch kinds are collected into one error map
%% (only non-empty kinds are included).
check_catalog_pks(Catalog, PrimaryKeys, Columns) ->
    Details =
        lists:foldl(
            fun({Table, Column}, Acc) ->
                case proplists:get_value(Table, PrimaryKeys) of
                    undefined ->
                        add_detail(missing_pk, Table, Acc);
                    [KeyCol] when KeyCol =:= Column ->
                        case find_column_type(Columns, Table, KeyCol) of
                            <<"bigint">> ->
                                Acc;
                            Type ->
                                add_detail(non_bigint_pk, {Table, Column, Type}, Acc)
                        end;
                    [KeyCol] ->
                        add_detail(pk_column_mismatch, {Table, Column, KeyCol}, Acc);
                    KeyCols ->
                        case partition_pk_ok(Table, Column, KeyCols, Columns) of
                            true -> Acc;
                            false -> add_detail(composite_pk, {Table, KeyCols}, Acc)
                        end
                end
            end,
            #{},
            Catalog
        ),
    case maps:size(Details) =:= 0 of
        true -> ok;
        false -> {error, {catalog_mismatch, Details}}
    end.

add_detail(Key, Item, Acc) ->
    maps:update_with(Key, fun(List) -> List ++ [Item] end, [Item], Acc).

%% TimescaleDB hypertables require a composite primary key whose leading
%% column is the entity id (e.g. msg_c2c PRIMARY KEY (id, created_at)).
%% Such a PK is acceptable exactly when the declared column leads it, the
%% declared column is bigint, and every trailing column is a timestamp
%% (the partition dimension). Anything else stays composite_pk FAIL.
partition_pk_ok(Table, Column, [Column | Rest], Columns) ->
    Rest =/= [] andalso
        find_column_type(Columns, Table, Column) =:= <<"bigint">> andalso
        lists:all(fun(C) -> is_ts_partition_col(Table, C, Columns) end, Rest);
partition_pk_ok(_, _, _, _) ->
    false.

is_ts_partition_col(Table, Col, Columns) ->
    case find_column_type(Columns, Table, Col) of
        <<"timestamp without time zone">> -> true;
        <<"timestamp with time zone">> -> true;
        _ -> false
    end.

%%%===================================================================
%%% Internal — bootstrap floor
%%%===================================================================

bootstrap_floor_in_conn(Catalog, Conn, QueryFun) ->
    case scan_tables(Catalog, Conn, QueryFun) of
        {error, _} = Error ->
            Error;
        PerTable ->
            Slots = [Slot || #{slot := Slot} <- PerTable, is_integer(Slot)],
            %% floor_safe_before is relative-millisecond (same domain as
            %% elib_tsid_store's safe_before, which the guard compares
            %% against wall-clock rel-ms directly): slot_to_ts(Slot) is
            %% Slot bsr 11, and the fenced floor starts at the ms AFTER
            %% the max observed slot's ms.
            Floor =
                case Slots of
                    [] -> 0;
                    _ -> (lists:max(Slots) bsr 11) + 1
                end,
            {ok, #{floor_safe_before => Floor, per_table => PerTable}}
    end.

scan_tables(Catalog, Conn, QueryFun) ->
    scan_tables(Catalog, Conn, QueryFun, []).

scan_tables([], _Conn, _QueryFun, Acc) ->
    lists:reverse(Acc);
scan_tables([{Table, Column} | Rest], Conn, QueryFun, Acc) ->
    case table_max_id(Conn, Table, Column, QueryFun) of
        {error, _} = Error ->
            Error;
        {ok, undefined} ->
            Entry = #{
                table => Table,
                column => Column,
                max_id => undefined,
                slot => undefined
            },
            scan_tables(Rest, Conn, QueryFun, [Entry | Acc]);
        {ok, MaxId} ->
            case max_id_to_slot(Table, Column, MaxId) of
                {ok, Slot} ->
                    Entry = #{
                        table => Table,
                        column => Column,
                        max_id => MaxId,
                        slot => Slot
                    },
                    scan_tables(Rest, Conn, QueryFun, [Entry | Acc]);
                {error, _} = Error ->
                    Error
            end
    end.

%% Identifier interpolation safety: see the module doc. Table/Column passed
%% the ^[a-z][a-z0-9_]*$ whitelist in normalize_catalog/1 before this point;
%% PostgreSQL identifiers cannot be bound parameters and this statement has
%% no value inputs at all. ORDER BY ... DESC LIMIT 1 avoids full-table
%% aggregation (count(*)/max() are forbidden here by contract).
table_max_id(Conn, Table, Column, QueryFun) ->
    Sql =
        <<"SELECT ", Column/binary, " FROM ", Table/binary, " ORDER BY ", Column/binary,
            " DESC LIMIT 1">>,
    case QueryFun(Conn, Sql, []) of
        {ok, []} ->
            {ok, undefined};
        {ok, [Row]} ->
            Key = binary_to_atom(Column, utf8),
            case maps:find(Key, Row) of
                {ok, Id} when is_integer(Id) ->
                    {ok, Id};
                {ok, Other} ->
                    {error,
                        {invalid_tsid_value, #{table => Table, column => Column, value => Other}}};
                error ->
                    {error, {unexpected_row_shape, #{table => Table, column => Column, row => Row}}}
            end;
        {error, Reason} ->
            {error, {max_query_failed, {Table, Column}, Reason}}
    end.

%% elib_tsid:id_to_slot/1 raises for ids outside 1..MAX_ID; a historical
%% max id in that range is impossible for TSID data and is a hard failure.
max_id_to_slot(Table, Column, Id) ->
    try
        {ok, elib_tsid:id_to_slot(Id)}
    catch
        error:{elib_tsid_invalid_input, _} ->
            {error, {invalid_tsid_value, #{table => Table, column => Column, id => Id}}}
    end.

%%%===================================================================
%%% Internal — schema map plumbing
%%%===================================================================

read_schema(SchemaFun, Conn) ->
    case SchemaFun(Conn) of
        {ok, SchemaMap} when is_map(SchemaMap) ->
            {ok, SchemaMap};
        {ok, Other} ->
            {error, {bad_schema_map, Other}};
        {error, Reason} ->
            {error, {schema_read_failed, Reason}}
    end.

find_column_type(Columns, Table, Column) ->
    case
        lists:search(
            fun({T, C, _Type}) -> T =:= Table andalso C =:= Column end,
            Columns
        )
    of
        {value, {_T, _C, Type}} -> Type;
        false -> not_found
    end.

%%%===================================================================
%%% Internal — catalog normalization / identifier whitelist
%%%===================================================================

normalized_catalog(Opts) ->
    normalize_catalog(maps:get(catalog, Opts, elib_tsid_catalog:primary_keys())).

%% Strict identifier whitelist: ^[a-z][a-z0-9_]*$, 1..63 bytes (PostgreSQL
%% identifier limit). Accepts atoms or binaries; everything is normalized
%% to binaries so no atoms are ever created from database-sourced names.
normalize_catalog(Catalog) when is_list(Catalog) ->
    case [Entry || Entry <- Catalog, not valid_entry(Entry)] of
        [] ->
            {ok, [{to_name(T), to_name(C)} || {T, C} <- Catalog]};
        Bad ->
            {error, {invalid_identifier, Bad}}
    end;
normalize_catalog(Other) ->
    {error, {invalid_catalog, Other}}.

valid_entry({T, C}) ->
    valid_name(T) andalso valid_name(C);
valid_entry(_) ->
    false.

valid_name(Name) when is_atom(Name) ->
    valid_name(atom_to_binary(Name, utf8));
valid_name(Name) when is_binary(Name) ->
    byte_size(Name) > 0 andalso
        byte_size(Name) =< 63 andalso
        re:run(Name, <<"^[a-z][a-z0-9_]*$">>, [{capture, none}]) =/= nomatch;
valid_name(_) ->
    false.

to_name(Name) when is_atom(Name) ->
    atom_to_binary(Name, utf8);
to_name(Name) when is_binary(Name) ->
    Name.

%% {rollback, Reason} from the transaction layer is still a hard failure here.
normalize_snapshot({rollback, Reason}) ->
    {error, {rollback, Reason}};
normalize_snapshot(Other) ->
    Other.

%%%===================================================================
%%% Default seams (real database paths; tests never exercise these)
%%%===================================================================

%% Default snapshot: one pooled connection inside a REPEATABLE READ READ
%% ONLY transaction. epgsql:with_transaction builds <<"BEGIN ">> ++
%% begin_opts itself, so the option value must NOT repeat the BEGIN keyword.
default_conn_fun(Fun) ->
    elib_pg:with_tx(Fun, [
        {reraise, true},
        {begin_opts, <<"ISOLATION LEVEL REPEATABLE READ READ ONLY">>}
    ]).

default_query_fun(Conn, Sql, Params) ->
    elib_pg:query(Conn, Sql, Params).

%% Default information_schema reader, scoped to the connection's current
%% schema so unqualified catalog names resolve consistently.
default_schema_fun(Conn) ->
    ColumnsSql =
        <<
            "SELECT table_name, column_name, data_type "
            "FROM information_schema.columns "
            "WHERE table_schema = current_schema()"
        >>,
    PkSql =
        <<
            "SELECT tc.table_name AS table_name, kcu.column_name AS column_name "
            "FROM information_schema.table_constraints tc "
            "JOIN information_schema.key_column_usage kcu "
            "  ON kcu.constraint_name = tc.constraint_name "
            " AND kcu.table_schema = tc.table_schema "
            "WHERE tc.constraint_type = 'PRIMARY KEY' "
            "  AND tc.table_schema = current_schema() "
            "ORDER BY tc.table_name, kcu.ordinal_position"
        >>,
    case elib_pg:query(Conn, ColumnsSql, []) of
        {ok, ColumnRows} ->
            case elib_pg:query(Conn, PkSql, []) of
                {ok, PkRows} ->
                    {ok, build_schema_map(ColumnRows, PkRows)};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

build_schema_map(ColumnRows, PkRows) ->
    Columns =
        [
            {v(table_name, Row), v(column_name, Row), v(data_type, Row)}
         || Row <- ColumnRows
        ],
    %% PkRows arrive ORDER BY table_name, ordinal_position; grouping keeps
    %% the key columns in ordinal order per table.
    PrimaryKeys =
        lists:reverse(
            lists:foldl(
                fun(Row, Acc) ->
                    Table = v(table_name, Row),
                    Column = v(column_name, Row),
                    case lists:keytake(Table, 1, Acc) of
                        {value, {Table, Cols}, Rest} ->
                            [{Table, Cols ++ [Column]} | Rest];
                        false ->
                            [{Table, [Column]} | Acc]
                    end
                end,
                [],
                PkRows
            )
        ),
    #{columns => Columns, primary_keys => PrimaryKeys}.

v(Key, Row) when is_map(Row) ->
    maps:get(Key, Row, undefined).
