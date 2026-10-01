%%% elib_tsid_scan_tests — TSID 自举扫描器测试（scan / check_schema / bootstrap_floor）
%%%
%%% 全部通过注入的 conn_fun / schema_fun / query_fun 运行，绝不连接真实数据库。
%%% 覆盖：schema_drift（缺表/坏列）、unclassified_primary_keys、
%%% catalog_mismatch（复合主键 / 主键列不一致 / 缺主键）、多表 floor 计算
%%% （含空表混合）、全空库 → 0、conn_fun / schema_fun / query_fun 错误传播、
%%% 标识符白名单拒绝注入形表名（且在开连接前 fail-fast）、单快照只开一次连接。
-module(elib_tsid_scan_tests).

-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% fixtures
%% ===================================================================

%% dummy connection: inner fun runs against an atom no driver understands —
%% proof that the scanner never touches epgsql itself (all I/O is injected).
fake_conn() ->
    fun(F) -> F(fake_conn) end.

%% conn_fun that counts how many times the snapshot boundary is opened.
snapshot_count_conn() ->
    put(scan_conn_openings, 0),
    fun(F) ->
        put(scan_conn_openings, get(scan_conn_openings) + 1),
        F(fake_conn)
    end.

schema(Columns, Pks) ->
    #{columns => Columns, primary_keys => Pks}.

schema_fun(SchemaMap) ->
    fun(_Conn) -> {ok, SchemaMap} end.

%% Mirrors the per-table max SQL the scanner builds. A shape change breaks
%% the fixture lookup on purpose — that pins "WHERE .. <= $1 +
%% ORDER BY .. DESC LIMIT 1, no count(*)/max()" and double-quoted
%% identifiers (reserved-word table names like "group"/"user" are
%% bare-identifier syntax errors) as part of the contract.
max_sql(Table, Column) ->
    <<"SELECT \"", Column/binary, "\" FROM \"", Table/binary, "\" WHERE \"", Column/binary,
        "\" <= $1 ORDER BY \"", Column/binary, "\" DESC LIMIT 1">>.

query_fun(MaxByIdent) ->
    fun(_Conn, Sql, [_Cap]) ->
        Matches =
            [
                Res
             || {{T, C}, Res} <- maps:to_list(MaxByIdent),
                max_sql(T, C) =:= Sql
            ],
        case Matches of
            [Res] -> {ok, Res};
            _ -> {error, {unexpected_sql, Sql}}
        end
    end.

%% Catalog {Table, Column} fully classified in the fake schema:
%% column present with type bigint + single-column PK on it.
ok_schema(Catalog) ->
    Columns =
        [{T, C, <<"bigint">>} || {T, C} <- Catalog] ++
            [{T, X, <<"bigint">>} || {T, C} <- Catalog, X <- extra_cols(C)],
    Pks = [{T, [C]} || {T, C} <- Catalog],
    schema(Columns, Pks).

extra_cols(<<"id">>) -> [];
extra_cols(_) -> [].

%% ===================================================================
%% bootstrap floor
%% ===================================================================

all_empty_database_floor_zero_test() ->
    Catalog = [{<<"user">>, <<"id">>}, {<<"group">>, <<"id">>}],
    Opts = opts(Catalog, ok_schema(Catalog), #{
        {<<"user">>, <<"id">>} => [],
        {<<"group">>, <<"id">>} => []
    }),
    {ok, #{floor_safe_before := 0, per_table := PerTable}} =
        elib_tsid_scan:scan(Opts),
    [
        #{
            table := <<"user">>,
            column := <<"id">>,
            max_id := undefined,
            slot := undefined
        },
        #{
            table := <<"group">>,
            column := <<"id">>,
            max_id := undefined,
            slot := undefined
        }
    ] = PerTable.

multi_table_floor_with_empty_mix_test() ->
    Catalog = [{<<"a">>, <<"id">>}, {<<"b">>, <<"id">>}, {<<"c">>, <<"id">>}],
    Opts = opts(Catalog, ok_schema(Catalog), #{
        {<<"a">>, <<"id">>} => [#{<<"id">> => 12345}],
        {<<"b">>, <<"id">>} => [],
        {<<"c">>, <<"id">>} => [#{<<"id">> => 999999}]
    }),
    SlotA = elib_tsid:id_to_slot(12345),
    SlotC = elib_tsid:id_to_slot(999999),
    %% floor_safe_before is relative-millisecond: slot_to_ts = Slot bsr 11.
    ExpectFloor = (max(SlotA, SlotC) bsr 11) + 1,
    {ok, #{floor_safe_before := ExpectFloor, per_table := PerTable}} =
        elib_tsid_scan:scan(Opts),
    [
        #{table := <<"a">>, max_id := 12345, slot := SlotA},
        #{table := <<"b">>, max_id := undefined, slot := undefined},
        #{table := <<"c">>, max_id := 999999, slot := SlotC}
    ] = PerTable.

bootstrap_floor_standalone_entry_test() ->
    Catalog = [{<<"a">>, <<"id">>}, {<<"b">>, <<"id">>}],
    Opts = opts(Catalog, ok_schema(Catalog), #{
        {<<"a">>, <<"id">>} => [#{<<"id">> => 42}],
        {<<"b">>, <<"id">>} => []
    }),
    ?assertEqual(
        {ok, (elib_tsid:id_to_slot(42) bsr 11) + 1},
        elib_tsid_scan:bootstrap_floor(Opts)
    ).

default_empty_catalog_test() ->
    %% Explicit empty catalog injection: no tables to scan -> floor 0,
    %% no per_table rows. (The compiled v1 catalog is non-empty; the
    %% default-catalog path is exercised by the injected-catalog tests.)
    Opts = #{
        catalog => [],
        conn_fun => fake_conn(),
        schema_fun => schema_fun(schema([], [])),
        query_fun => query_fun(#{})
    },
    ?assertEqual(
        {ok, #{floor_safe_before => 0, per_table => []}},
        elib_tsid_scan:scan(Opts)
    ).

%% ===================================================================
%% schema drift
%% ===================================================================

schema_drift_missing_table_test() ->
    Catalog = [{<<"user">>, <<"id">>}],
    Other = schema(
        [{<<"other">>, <<"id">>, <<"bigint">>}],
        [{<<"other">>, [<<"id">>]}]
    ),
    Opts = opts(Catalog, Other, #{}),
    ?assertMatch(
        {error,
            {schema_drift, #{
                missing := [{<<"user">>, <<"id">>}],
                wrong_type := []
            }}},
        elib_tsid_scan:scan(Opts)
    ).

schema_drift_missing_column_test() ->
    Catalog = [{<<"user">>, <<"id">>}],
    %% table exists with a bigint pk column, but not the catalog's "id"
    S = schema(
        [{<<"user">>, <<"uid">>, <<"bigint">>}],
        [{<<"user">>, [<<"uid">>]}]
    ),
    Opts = opts(Catalog, S, #{}),
    ?assertMatch(
        {error,
            {schema_drift, #{
                missing := [{<<"user">>, <<"id">>}],
                wrong_type := []
            }}},
        elib_tsid_scan:scan(Opts)
    ).

schema_drift_wrong_column_type_test() ->
    Catalog = [{<<"user">>, <<"id">>}],
    S = schema(
        [{<<"user">>, <<"id">>, <<"character varying">>}],
        [{<<"user">>, [<<"id">>]}]
    ),
    Opts = opts(Catalog, S, #{}),
    ?assertMatch(
        {error,
            {schema_drift, #{
                missing := [],
                wrong_type := [{<<"user">>, <<"id">>, <<"character varying">>}]
            }}},
        elib_tsid_scan:scan(Opts)
    ).

check_schema_ok_on_valid_schema_test() ->
    Catalog = [{<<"user">>, <<"id">>}],
    Opts = opts(Catalog, ok_schema(Catalog), #{}),
    ?assertEqual(ok, elib_tsid_scan:check_schema(Catalog, Opts)).

%% ===================================================================
%% reverse discovery
%% ===================================================================

unclassified_primary_keys_test() ->
    Catalog = [{<<"user">>, <<"id">>}],
    S = ok_schema(
        Catalog ++
            [
                {<<"legacy_thing">>, <<"id">>},
                {<<"legacy_thing2">>, <<"id">>}
            ]
    ),
    Opts = opts(Catalog, S, #{}),
    ?assertMatch(
        {error, {unclassified_primary_keys, [<<"legacy_thing">>, <<"legacy_thing2">>]}},
        elib_tsid_scan:scan(Opts)
    ).

catalog_mismatch_composite_pk_test() ->
    Catalog = [{<<"user">>, <<"id">>}],
    S = schema(
        [
            {<<"user">>, <<"id">>, <<"bigint">>},
            {<<"user">>, <<"org">>, <<"bigint">>}
        ],
        [{<<"user">>, [<<"id">>, <<"org">>]}]
    ),
    Opts = opts(Catalog, S, #{}),
    ?assertMatch(
        {error, {catalog_mismatch, #{composite_pk := [{<<"user">>, [<<"id">>, <<"org">>]}]}}},
        elib_tsid_scan:scan(Opts)
    ).

catalog_partition_pk_timescaledb_ok_test() ->
    %% TimescaleDB hypertable shape: PRIMARY KEY (id, created_at) with a
    %% bigint leading column and a timestamp trailing column passes the
    %% catalog check and the scan proceeds to the floor stage.
    Catalog = [{<<"msg_c2c">>, <<"id">>}],
    S = schema(
        [
            {<<"msg_c2c">>, <<"id">>, <<"bigint">>},
            {<<"msg_c2c">>, <<"created_at">>, <<"timestamp without time zone">>}
        ],
        [{<<"msg_c2c">>, [<<"id">>, <<"created_at">>]}]
    ),
    Opts = opts(Catalog, S, #{{<<"msg_c2c">>, <<"id">>} => []}),
    ?assertMatch(
        {ok, #{floor_safe_before := 0}},
        elib_tsid_scan:scan(Opts)
    ).

catalog_mismatch_pk_column_mismatch_test() ->
    Catalog = [{<<"user">>, <<"id">>}],
    %% catalog column id exists (check_schema passes) but the actual PK is uid
    S = schema(
        [
            {<<"user">>, <<"id">>, <<"bigint">>},
            {<<"user">>, <<"uid">>, <<"bigint">>}
        ],
        [{<<"user">>, [<<"uid">>]}]
    ),
    Opts = opts(Catalog, S, #{}),
    ?assertMatch(
        {error, {catalog_mismatch, #{pk_column_mismatch := [{<<"user">>, <<"id">>, <<"uid">>}]}}},
        elib_tsid_scan:scan(Opts)
    ).

catalog_mismatch_missing_pk_test() ->
    Catalog = [{<<"user">>, <<"id">>}],
    S = schema([{<<"user">>, <<"id">>, <<"bigint">>}], []),
    Opts = opts(Catalog, S, #{}),
    ?assertMatch(
        {error, {catalog_mismatch, #{missing_pk := [<<"user">>]}}},
        elib_tsid_scan:scan(Opts)
    ).

%% ===================================================================
%% error propagation
%% ===================================================================

conn_fun_error_propagation_test() ->
    Catalog = [{<<"user">>, <<"id">>}],
    S = ok_schema(Catalog),
    Base = opts(Catalog, S, #{}),
    E1 = elib_tsid_scan:scan(Base#{conn_fun => fun(_F) -> {error, pool_exhausted} end}),
    ?assertMatch({error, pool_exhausted}, E1),
    %% transaction-layer rollback surfaces as FAIL too
    E2 = elib_tsid_scan:scan(Base#{conn_fun => fun(_F) -> {rollback, tx_lost} end}),
    ?assertMatch({error, {rollback, tx_lost}}, E2).

schema_fun_error_propagation_test() ->
    Catalog = [{<<"user">>, <<"id">>}],
    Base = opts(Catalog, ok_schema(Catalog), #{}),
    Opts = Base#{schema_fun => fun(_Conn) -> {error, schema_boom} end},
    ?assertMatch(
        {error, {schema_read_failed, schema_boom}},
        elib_tsid_scan:scan(Opts)
    ).

max_query_error_propagation_test() ->
    Catalog = [{<<"user">>, <<"id">>}],
    Base = opts(Catalog, ok_schema(Catalog), #{}),
    Opts = Base#{query_fun => fun(_Conn, _Sql, [_Cap]) -> {error, timeout} end},
    ?assertMatch(
        {error, {max_query_failed, {<<"user">>, <<"id">>}, timeout}},
        elib_tsid_scan:scan(Opts)
    ).

invalid_tsid_value_test() ->
    Catalog = [{<<"user">>, <<"id">>}],
    %% id 0 is outside elib_tsid:id_to_slot/1's 1..MAX_ID contract → FAIL
    Opts = opts(Catalog, ok_schema(Catalog), #{
        {<<"user">>, <<"id">>} => [#{<<"id">> => 0}]
    }),
    ?assertMatch(
        {error, {invalid_tsid_value, #{table := <<"user">>, column := <<"id">>, id := 0}}},
        elib_tsid_scan:scan(Opts)
    ).

%% ===================================================================
%% identifier whitelist
%% ===================================================================

invalid_identifier_rejected_fail_fast_test() ->
    Bad = [{<<"user; drop table users">>, <<"id">>}],
    %% fail-fast: no connection may be opened for invalid identifiers
    NoConn = fun(_F) -> error(conn_must_not_open) end,
    Opts = #{
        catalog => Bad,
        conn_fun => NoConn,
        schema_fun => schema_fun(schema([], [])),
        query_fun => query_fun(#{})
    },
    ?assertMatch({error, {invalid_identifier, _}}, elib_tsid_scan:scan(Opts)),
    ?assertMatch(
        {error, {invalid_identifier, _}},
        elib_tsid_scan:check_schema(Bad, Opts)
    ),
    ?assertMatch(
        {error, {invalid_identifier, _}},
        elib_tsid_scan:bootstrap_floor(Opts)
    ).

invalid_identifier_uppercase_and_bad_shape_test() ->
    Bad = [{<<"User">>, <<"id">>}, {<<"ok">>, <<"id-x">>}],
    NoConn = fun(_F) -> error(conn_must_not_open) end,
    Opts = #{
        catalog => Bad,
        conn_fun => NoConn,
        schema_fun => schema_fun(schema([], [])),
        query_fun => query_fun(#{})
    },
    ?assertMatch({error, {invalid_identifier, _}}, elib_tsid_scan:scan(Opts)),
    %% injection-shaped identifiers hidden as atoms are rejected the same way
    BadAtom = [{'user;--', id}],
    ?assertMatch(
        {error, {invalid_identifier, _}},
        elib_tsid_scan:scan(Opts#{catalog => BadAtom})
    ).

%% ===================================================================
%% snapshot boundary
%% ===================================================================

single_snapshot_per_scan_test() ->
    Catalog = [{<<"a">>, <<"id">>}, {<<"b">>, <<"id">>}],
    Conn = snapshot_count_conn(),
    Base = opts(Catalog, ok_schema(Catalog), #{
        {<<"a">>, <<"id">>} => [#{<<"id">> => 7}],
        {<<"b">>, <<"id">>} => []
    }),
    Opts = Base#{conn_fun => Conn},
    {ok, _} = elib_tsid_scan:scan(Opts),
    %% schema check + discovery + floor all share ONE snapshot
    ?assertEqual(1, get(scan_conn_openings)),
    put(scan_conn_openings, 0),
    {ok, _} = elib_tsid_scan:bootstrap_floor(Opts),
    ?assertEqual(1, get(scan_conn_openings)).

%% ===================================================================
%% default seams 回归（真实 epgsql 返回形状）
%% ===================================================================

%% 2026-09-30 生产事故回归：epgsql 的 #column.name 是 binary，
%% elib_pg:rows_to_maps 产出 binary-key map；build_schema_map 的 v/2
%% 曾用 atom key 取值 → 列名全 undefined → catalog 183 项全部
%% schema_drift missing（guard fail-closed 拒启，heart 复活循环）。
%% table_max_id 的 binary_to_atom 查 key 同源：非空表全报
%% unexpected_row_shape。default seams 此前零测试覆盖，本用例用
%% binary-key 行形状钉死 schema 与 floor 两阶段全链路。
default_seams_binary_key_regression_test() ->
    meck:new(elib_pg, [passthrough, no_link]),
    meck:expect(
        elib_pg,
        with_tx,
        fun(F, _Opts) -> F(fake_conn) end
    ),
    meck:expect(
        elib_pg,
        query,
        fun
            (_Conn, <<"SELECT table_name, column_name", _/binary>>, []) ->
                {ok, [
                    #{
                        <<"table_name">> => <<"adm_user">>,
                        <<"column_name">> => <<"id">>,
                        <<"data_type">> => <<"bigint">>
                    }
                ]};
            (_Conn, <<"SELECT tc.table_name AS table_name", _/binary>>, []) ->
                {ok, [
                    #{
                        <<"table_name">> => <<"adm_user">>,
                        <<"column_name">> => <<"id">>
                    }
                ]};
            (_Conn, <<"SELECT \"id\" FROM \"adm_user\"", _/binary>>, [_Cap]) ->
                {ok, [#{<<"id">> => 12345}]}
        end
    ),
    try
        %% atom catalog（elib_tsid_catalog:primary_keys 的真实形状）
        %% 走 default conn/schema/query seams 全链路
        ExpectFloor = (elib_tsid:id_to_slot(12345) bsr 11) + 1,
        {ok, #{floor_safe_before := ExpectFloor}} =
            elib_tsid_scan:scan(#{catalog => [{adm_user, id}]})
    after
        meck:unload(elib_pg)
    end.

reserved_keyword_table_quoted_test() ->
    %% 2026-09-30 生产事故回归：catalog 含 {group, id}/{user, id} 等保留字
    %% 表名，裸拼 FROM group 是 42601 语法错误（max_query_failed → guard
    %% fail-closed 拒启 + heart 复活循环）。query_fun 经 max_sql/2 只匹配
    %% 双引号形态的 SQL——裸标识符形态在此失配报 unexpected_sql。
    Catalog = [{<<"group">>, <<"id">>}, {<<"user">>, <<"id">>}],
    Opts = opts(Catalog, ok_schema(Catalog), #{
        {<<"group">>, <<"id">>} => [],
        {<<"user">>, <<"id">>} => []
    }),
    ?assertMatch({ok, #{floor_safe_before := 0}}, elib_tsid_scan:scan(Opts)).

%% ===================================================================
%% 远未来魔法大数剔除（2026-09-30 生产事故根治）
%% ===================================================================

future_cap_is_now_plus_tolerance_test() ->
    %% cap = (now + 24h 钳到 42 位域顶) 的完整 id 上界；注入固定时钟钉死
    %% 传递给 query_fun 的绑定值——生产 30 行 137 年领先的魔法大数靠该
    %% WHERE 过滤剔除，floor 才能落回合法历史区间。
    NowMs = 1759180800000,
    NowRel = NowMs - 1735689600000,
    Self = self(),
    QF = fun(_Conn, _Sql, [Cap]) ->
        Self ! {cap, Cap},
        {ok, []}
    end,
    Catalog = [{<<"user">>, <<"id">>}],
    Opts = #{
        catalog => Catalog,
        conn_fun => fake_conn(),
        schema_fun => schema_fun(ok_schema(Catalog)),
        query_fun => QF,
        wall_clock_ms => fun(millisecond) -> NowMs end
    },
    {ok, #{floor_safe_before := 0}} = elib_tsid_scan:scan(Opts),
    ExpectCap =
        (min(max(NowRel + 86400000, 0), 4398046511103) bsl 21) bor ((1 bsl 21) - 1),
    receive
        {cap, ExpectCap} -> ok
    after 1000 ->
        erlang:error(cap_missing_or_mismatch)
    end.

far_future_id_excluded_from_floor_test() ->
    %% fixture 模拟 DB 端 WHERE 过滤：库内虽有 137 年领先的魔法大数
    %% （数值 > cap 被 SQL 剔除），查询只返回 ≤cap 的最大合法 id；
    %% floor 由合法 id 决定，魔法数不参与。
    Magic = 9189620641444157625,
    Legal = 12345,
    Catalog = [{<<"enterprise_contact">>, <<"id">>}],
    Opts = opts(Catalog, ok_schema(Catalog), #{
        {<<"enterprise_contact">>, <<"id">>} => [#{<<"id">> => Legal}]
    }),
    true = Magic > Legal,
    Slot = elib_tsid:id_to_slot(Legal),
    {ok, #{floor_safe_before := Floor, per_table := PerTable}} =
        elib_tsid_scan:scan(Opts),
    ?assertEqual((Slot bsr 11) + 1, Floor),
    [#{max_id := Legal, slot := Slot}] = PerTable.

future_cap_clamped_to_max_rel_ts_test() ->
    %% now + 24h 超出 42 位时间域时钳到域顶：cap 永不溢出 63 位 id 域。
    NowMs = 1735689600000 + 4398046511103 * 1000 - 1000,
    Self = self(),
    QF = fun(_Conn, _Sql, [Cap]) ->
        Self ! {cap, Cap},
        {ok, []}
    end,
    Catalog = [{<<"user">>, <<"id">>}],
    Opts = #{
        catalog => Catalog,
        conn_fun => fake_conn(),
        schema_fun => schema_fun(ok_schema(Catalog)),
        query_fun => QF,
        wall_clock_ms => fun(millisecond) -> NowMs end
    },
    {ok, _} = elib_tsid_scan:scan(Opts),
    ExpectCap = (4398046511103 bsl 21) bor ((1 bsl 21) - 1),
    receive
        {cap, ExpectCap} -> ok
    after 1000 ->
        erlang:error(clamped_cap_missing_or_mismatch)
    end.

%% ===================================================================
%% helpers
%% ===================================================================

opts(Catalog, SchemaMap, MaxByIdent) ->
    #{
        catalog => Catalog,
        conn_fun => fake_conn(),
        schema_fun => schema_fun(SchemaMap),
        query_fun => query_fun(MaxByIdent)
    }.
