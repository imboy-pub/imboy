#!/usr/bin/env escript
%%! -pa ebin
%% TSID-08 只读高水位 scanner（计划：离线 bootstrap 工具；不连接生产——
%% 由调用方传入 scratch/clone DB 的连接参数）。
%%
%% 步骤 6 去重说明（唯一权威实现 = src/lib/elib_tsid_scan.erl）：
%%   - schema 校验（catalog 存在性 / bigint 类型 / 反向发现单列 bigint
%%     主键全集）已委托 elib_tsid_scan:check_schema/2，本文件不再自带该 SQL；
%%   - 自举 floor 已委托 elib_tsid_scan:bootstrap_floor/1（slot 口径：
%%     max(id_to_slot(max id)) + 1，空库 0，只读 REPEATABLE READ）。
%%     旧的 ts 口径公式（max ts + 1）退役；割接决策以 lib 值为唯一权威。
%%   - 保留的本脚本独有能力：inventory TSV 装载、allow_exclude 白名单
%%     运维闸门（拒绝静默豁免）、逐表报告（count/min/max + 缺陷分类）、
%%     scratch-high-water.json / scanner-manifest.json 输出格式。逐表聚合
%%     SQL 与 lib 的 max-id 语义同源（见 scan_table 注释交叉引用）。
%%
%% 功能（对应 AC-08A/AC-08B）：
%%   1. 从 inventory TSV 构造 catalog（[{Table, Column}]，合同形态）
%%   2. schema 闸门：elib_tsid_scan:check_schema/2
%%      （UNKNOWN 列合同：db_only 的 bigint 主键 → BLOCKED_CUTOVER；
%%      allow_exclude 仍须精确匹配，多豁免同样 BLOCKED）
%%   3. 逐表扫描：min/max id、行数、合法范围分类
%%      （negative / future_beyond_tolerance / beyond_max_id / ...）
%%   4. 权威 floor：elib_tsid_scan:bootstrap_floor/1（slot 口径）
%%   5. 输出 scratch-high-water.json + scanner-manifest.json
%%
%% 用法：escript tsid_scanner.escript <inventory.tsv> <outdir> <allow_exclude_csv> -- host port user password database
%%   allow_exclude_csv：显式豁免的 db_only 表名（逗号分隔；合同要求逐表人工判定后
%%   记录于 inventory-supplement.tsv，scanner 拒绝静默豁免）
%%（password 可为 - 表示 trust/无密码）
-module(tsid_scanner_main).
-export([main/1]).

-define(EPOCH_MS, 1735689600000).
-define(MAX_REL_TS, 4398046511103).
-define(MAX_ID, 9223372036854775807).
%% 未来容忍度：扫描到的 ID 时间戳领先当前墙钟超过该值 → BLOCKED_CUTOVER
-define(FUTURE_TOLERANCE_MS, 60000).

main(Args) ->
    case parse_args(Args) of
        {ok, InvPath, OutDir, Conn} ->
            ensure_paths(),
            run(InvPath, OutDir, Conn);
        usage ->
            io:format(
                "usage: tsid_scanner.escript <inventory.tsv> <outdir> <allow_exclude_csv> -- host port user password database~n"
            ),
            halt(2)
    end.

parse_args(Args) ->
    case lists:splitwith(fun(X) -> X =/= "--" end, Args) of
        {[InvPath, OutDir, AllowCsv], ["--" | ConnStr]} when length(ConnStr) =:= 5 ->
            [H, P, U, PW, D] = ConnStr,
            {ok, InvPath, OutDir, #{
                host => H,
                port => list_to_integer(P),
                user => U,
                password =>
                    case PW of
                        "-" -> "";
                        _ -> PW
                    end,
                database => D,
                allow_exclude => [
                    plain(T)
                 || T <- binary:split(list_to_binary(AllowCsv), <<",">>, [global, trim_all]),
                    T =/= <<>>
                ]
            }};
        _ ->
            usage
    end.

run(InvPath, OutDir, Conn) ->
    StartedAt = os:system_time(millisecond),
    Inventory = load_inventory(InvPath),
    Opts = #{
        catalog => inventory_catalog(Inventory),
        conn_fun => conn_fun(Conn)
    },
    try
        %% 1) schema 闸门（lib 权威实现）
        ok = gate_schema(Opts, Conn, OutDir),
        %% 2) 逐表报告（单只读快照；脚本独有能力）
        Rows = per_table_pass(Opts),
        Problems = lists:append([P || #{problems := P} <- Rows]),
        ok = gate_scan_integrity(Opts, Rows, Problems, OutDir),
        %% 3) 权威 floor（lib 权威实现，slot 口径）
        Floor = gate_floor(Opts, OutDir),
        finish_ok(OutDir, InvPath, StartedAt, Rows, Problems, Floor)
    catch
        C:R:ST ->
            io:format(
                "BLOCKED_CUTOVER: scanner crashed (~p:~p); "
                "high-water mark would be unreliable~n~p~n",
                [C, R, ST]
            ),
            halt(3)
    end.

%% inventory TSV → scan 合同 catalog（[{Table :: atom(), Column :: atom()}]）。
%% inventory 是运行清单超集口径（含 supplement 逐表判定）；运行时权威仍是
%% elib_tsid_catalog（割接 manifest digest 绑定），两者一致性由 check_schema
%% 的反向发现保障。
inventory_catalog(Inventory) ->
    lists:sort([
        {binary_to_atom(list_to_binary(T), utf8), binary_to_atom(Col, utf8)}
     || {T, Col} <- maps:to_list(Inventory)
    ]).

%% 1) schema 闸门 — 委托权威实现 elib_tsid_scan:check_schema/2
%% （存在性 + bigint 类型 + 反向发现单列 bigint 主键 vs catalog 求差）。
%% lib 合同为 FAIL 级（schema_drift / catalog_mismatch / unclassified_primary_keys），
%% 本脚本不得降 warning：任何非 ok 结果都收敛为 BLOCKED_CUTOVER。
gate_schema(Opts, Conn, OutDir) ->
    case elib_tsid_scan:check_schema(maps:get(catalog, Opts), Opts) of
        ok ->
            ok;
        {error, {unclassified_primary_keys, Found}} ->
            gate_unclassified(Found, maps:get(allow_exclude, Conn, []), OutDir);
        {error, Reason} ->
            blocked(<<"schema_check_failed">>, Reason, OutDir)
    end.

%% lib 反向发现结果 → {TableList, ColumnList}（兼容 atom/binary/{T,C} 形态，
%% 仅取表名参与 allow_exclude 精确匹配；形态以 lib 实现为准）。
normalize_pk({T, C}) when is_atom(T), is_atom(C) ->
    {atom_to_list(T), atom_to_list(C)};
normalize_pk({T, C}) when is_binary(T), is_binary(C) ->
    {plain(T), plain(C)};
normalize_pk({T, C}) when is_list(T), is_list(C) ->
    {T, C};
normalize_pk(T) when is_atom(T) ->
    {atom_to_list(T), "id"};
normalize_pk(T) when is_binary(T) ->
    {plain(T), "id"};
normalize_pk(T) when is_list(T) ->
    {T, "id"}.

%% UNKNOWN 列合同（沿用原语义）：db_only 主键未在 inventory 登记即 BLOCKED；
%% allow_exclude 精确豁免，名单里多出的表同样 BLOCKED——防止白名单膨胀失管。
gate_unclassified(Found, Allow, OutDir) ->
    DbOnly0 = [normalize_pk(E) || E <- Found],
    Tables0 = [T || {T, _} <- DbOnly0],
    {DbOnly, _AllowedFound} =
        lists:partition(fun({T, _}) -> not lists:member(T, Allow) end, DbOnly0),
    AllowUnknown = [T || T <- Allow, not lists:member(T, Tables0)],
    case AllowUnknown of
        [] ->
            ok;
        _ ->
            io:format(
                "BLOCKED_CUTOVER: allow-exclude names not found in db: ~p~n",
                [AllowUnknown]
            ),
            write_json(
                OutDir ++ "/scanner-manifest.json",
                #{
                    status => <<"BLOCKED_CUTOVER">>,
                    reason => <<"allow_exclude_unknown_tables">>,
                    unknown_allow => [a2b(L) || L <- AllowUnknown]
                }
            ),
            halt(3)
    end,
    case DbOnly of
        [] ->
            ok;
        _ ->
            io:format(
                "BLOCKED_CUTOVER: db-only bigint PK tables not in inventory: ~p~n",
                [DbOnly]
            ),
            write_json(
                OutDir ++ "/scanner-manifest.json",
                #{
                    status => <<"BLOCKED_CUTOVER">>,
                    reason => <<"db_only_unknown_columns">>,
                    db_only => [a2b(T) || {T, _} <- DbOnly]
                }
            ),
            halt(3)
    end.

%% 2) 逐表报告 — 本脚本独有能力（保留）。聚合语义与 elib_tsid_scan 同源：
%%    max id 即 scan/bootstrap_floor 的高水位输入；floor 权威值不取自本段，
%%    本段仅承担报告与缺陷分类（negative/future/beyond_max/ts_before_epoch）。
%%    整段在 conn_fun 提供的单个只读 REPEATABLE READ 快照内完成。
per_table_pass(Opts) ->
    ConnFun = maps:get(conn_fun, Opts),
    NowMs = os:system_time(millisecond),
    ConnFun(fun(PConn) ->
        [scan_table_guarded(PConn, atom_to_list(T), atom_to_list(C), NowMs)
         || {T, C} <- maps:get(catalog, Opts)]
    end).

%% 失败表恒产出一个 scan_error 行（review F3：失败不得被高水位 max 静默
%% 跳过；闸门在 gate_scan_errors）。本检查兜底未来重构改为可跳表时不静默漏表。
scan_table_guarded(PConn, Table, Col, NowMs) ->
    try
        scan_table(PConn, Table, Col, NowMs)
    catch
        C:Re:_ -> error_row(Table, Col, {C, Re})
    end.

error_row(Table, Col, Reason) ->
    #{
        table => a2b(Table),
        column => a2b(Col),
        row_count => 0,
        min_id => 0,
        max_id => 0,
        max_ts => 0,
        problems => [{scan_error, Table, Col, Reason}]
    }.

gate_scan_integrity(Opts, Rows, Problems, OutDir) ->
    Scanned = [plain(maps:get(table, R)) || R <- Rows],
    Missing =
        [atom_to_list(T)
         || {T, _} <- maps:get(catalog, Opts),
            not lists:member(atom_to_list(T), Scanned)],
    ok = gate_missing(Missing, OutDir),
    ok = gate_scan_errors(Problems, OutDir),
    ok = gate_future(Problems, OutDir).

gate_missing([], _OutDir) ->
    ok;
gate_missing(Missing, OutDir) ->
    io:format("BLOCKED_CUTOVER: inventory tables missing in db: ~p~n", [Missing]),
    write_json(
        OutDir ++ "/scanner-manifest.json",
        #{
            status => <<"BLOCKED_CUTOVER">>,
            reason => <<"inventory_tables_missing">>,
            missing => [a2b(M) || M <- Missing]
        }
    ),
    halt(3).

%% review F3：单表扫描失败必须 BLOCKED——失败表 row_count=0 会被高水位
%% max 静默跳过，floor 候选可能低于真实高水位（cutover 撞号风险）
gate_scan_errors([], _OutDir) ->
    ok;
gate_scan_errors(Problems, OutDir) ->
    ScanErrors = [F || F <- Problems, element(1, F) =:= scan_error],
    case ScanErrors of
        [] ->
            ok;
        _ ->
            io:format(
                "BLOCKED_CUTOVER: ~p table scan(s) failed; "
                "high-water mark would be unreliable~n",
                [length(ScanErrors)]
            ),
            write_json(
                OutDir ++ "/scanner-manifest.json",
                #{
                    status => <<"BLOCKED_CUTOVER">>,
                    reason => <<"table_scan_errors">>,
                    scan_errors =>
                        [
                            {a2b(element(2, F)), a2b(element(3, F))}
                         || F <- ScanErrors
                        ]
                }
            ),
            halt(3)
    end.

gate_future(Problems, OutDir) ->
    Future = [F || F <- Problems, element(1, F) =:= future_beyond_tolerance],
    case Future of
        [] ->
            ok;
        _ ->
            io:format("BLOCKED_CUTOVER: future ids beyond tolerance: ~p~n", [Future]),
            write_json(
                OutDir ++ "/scanner-manifest.json",
                #{
                    status => <<"BLOCKED_CUTOVER">>,
                    reason => <<"future_beyond_tolerance">>,
                    detail => [problem_json(F) || F <- Future]
                }
            ),
            halt(3)
    end.

%% 3) 权威 floor — 委托 elib_tsid_scan:bootstrap_floor/1（slot 口径，唯一权威）。
%%    与逐表段分属两个快照：scanner 仅在 runbook 停写合同下使用（停写后
%%    数据静态），双扫 digest（runbook §2）兜底快照间漂移。
gate_floor(Opts, OutDir) ->
    FloorOpts = #{
        catalog => maps:get(catalog, Opts),
        conn_fun => maps:get(conn_fun, Opts)
    },
    case elib_tsid_scan:bootstrap_floor(FloorOpts) of
        {ok, Floor} when is_integer(Floor), Floor >= 0 ->
            Floor;
        {ok, Other} ->
            %% 防御：合同返回形态漂移（如 map）时不 crash，收敛为 BLOCKED
            blocked(<<"floor_invalid_value">>, Other, OutDir);
        {error, Reason} ->
            blocked(<<"floor_computation_failed">>, Reason, OutDir)
    end.

finish_ok(OutDir, InvPath, StartedAt, Rows, Problems, Floor) ->
    HighWaterTs = lists:max([
        0
        | [
            maps:get(max_ts, R, 0)
         || R <- Rows,
            maps:get(row_count, R, 0) > 0
        ]
    ]),
    MaxId = lists:max([
        0
        | [
            maps:get(max_id, R, 0)
         || R <- Rows,
            maps:get(row_count, R, 0) > 0
        ]
    ]),
    %% floor：slot 口径权威值（elib_tsid_scan:bootstrap_floor/1）。保留
    %% floor_candidate 键名以兼容 runbook §2 取值口径——含义已由 ts+1
    %% 变更为 slot（见文件头）；floor_safe_before 为合同键名。
    Result = #{
        status => <<"OK">>,
        scanned_at_ms => StartedAt,
        tables => Rows,
        table_count => length(Rows),
        %% OK 前提下恒为空：inventory 有表但 DB 无单列 bigint 主键时
        %% check_schema 已 FAIL（schema_drift / catalog_mismatch）。
        inventory_only_tables => [],
        problems => [problem_json(P) || P <- Problems],
        high_water => #{
            floor_safe_before => Floor,
            floor_candidate => Floor,
            floor_source => <<"elib_tsid_scan:bootstrap_floor/1">>,
            max_ts_rel => HighWaterTs,
            max_ts_abs_ms => HighWaterTs + ?EPOCH_MS,
            max_id_seen => MaxId
        }
    },
    write_json(OutDir ++ "/scratch-high-water.json", Result),
    write_json(
        OutDir ++ "/scanner-manifest.json",
        #{
            status => <<"OK">>,
            inventory_file => a2b(InvPath),
            table_count => length(Rows),
            scanned_at_ms => StartedAt
        }
    ),
    io:format(
        "SCAN_OK tables=~p high_water_rel=~p floor_candidate=~p problems=~p~n",
        [length(Rows), HighWaterTs, Floor, length(Problems)]
    ).

%% CLI 连接传输层（scratch/clone DB 显式连接参数；禁止直连生产）。
%% 与合同缺省路径语义同源：elib_pg:with_tx 的只读快照
%% （BEGIN ISOLATION LEVEL REPEATABLE READ READ ONLY + reraise）。
%% 不含任何扫描 SQL——扫描语义全部在 elib_tsid_scan；连接/事务失败原样
%% 抛出，由 run/3 的 catch 收敛为 BLOCKED_CUTOVER。
conn_fun(Conn) ->
    fun(F) ->
        {ok, Pid} =
            epgsql:connect(#{
                host => maps:get(host, Conn),
                port => maps:get(port, Conn),
                username => maps:get(user, Conn),
                password => maps:get(password, Conn),
                database => maps:get(database, Conn),
                timeout => 10000
            }),
        try
            epgsql:with_transaction(Pid, F, [
                {begin_opts, "ISOLATION LEVEL REPEATABLE READ READ ONLY"},
                {reraise, true}
            ])
        after
            epgsql:close(Pid)
        end
    end.

%% -------------------------------------------------------------------
%% inventory TSV：kind/key/tables/note/unknown；kind in generator|default-site；
%% tables 列为实表名（排除 (unused)/(multi)/( 开头）
load_inventory(Path) ->
    {ok, Bin} = file:read_file(Path),
    Lines = binary:split(Bin, <<"\n">>, [global, trim_all]),
    lists:foldl(
        fun(Line, Acc) ->
            case binary:split(Line, <<"\t">>, [global]) of
                [Kind, _Key, Table | Rest] when
                    Kind =:= <<"generator">>;
                    Kind =:= <<"default-site">>;
                    Kind =:= <<"supplement-tsid08">>
                ->
                    case Table of
                        <<"(", _/binary>> ->
                            Acc;
                        <<>> ->
                            Acc;
                        _ ->
                            Col = col_from_note(Rest),
                            maps:put(plain(Table), Col, Acc)
                    end;
                _ ->
                    Acc
            end
        end,
        #{},
        Lines
    ).

%% note 字段里的 column=<name>（缺省 id）
col_from_note(Rest) ->
    Parsed = [binary:split(B, <<"column=">>) || B <- Rest],
    Hits = [Col || [_, Col] <- Parsed],
    case Hits of
        [Col | _] -> Col;
        _ -> <<"id">>
    end.

%% 逐表聚合：count/min/max（脚本报告独有能力）。max id 与 elib_tsid_scan
%% 的高水位输入语义同源（lib 自举 floor 用 ORDER BY <col> DESC LIMIT 1 取
%% max id，本聚合额外提供 count/min 供缺陷分类与报告）；修改聚合口径须与
%% lib 同步，不得分叉。
scan_table(PConn, Table, Col, NowMs) ->
    Tq = quote_ident(Table),
    Cq = quote_ident(Col),
    Sql = iolist_to_binary(
        ["SELECT count(*), coalesce(min(", Cq, "),0), coalesce(max(", Cq, "),0) FROM public.", Tq]
    ),
    case q(PConn, Sql) of
        {ok, _, [{Count0, MinId0, MaxId0}]} ->
            Count = to_int(Count0),
            MinId = to_int(MinId0),
            MaxId = to_int(MaxId0),
            TsMin = ts_of(MinId),
            TsMax = ts_of(MaxId),
            Problems =
                tables_problems(Table, Col, Count, MinId, MaxId, TsMin, TsMax, NowMs),
            #{
                table => a2b(Table),
                column => a2b(Col),
                row_count => to_int(Count),
                min_id => to_int(MinId),
                max_id => to_int(MaxId),
                min_ts_rel => TsMin,
                max_ts_rel => TsMax,
                max_ts => TsMax,
                problems => Problems
            };
        {error, R} ->
            error_row(Table, Col, R)
    end.

to_int(V) when is_integer(V) -> V;
to_int(V) when is_binary(V) -> binary_to_integer(V);
to_int(V) when is_list(V) -> list_to_integer(V).

tables_problems(Table, Col, Count, MinId, MaxId, TsMin, TsMax, NowMs) ->
    P0 =
        case MinId < 0 of
            true -> [{negative_id, Table, Col, MinId}];
            false -> []
        end,
    P1 =
        case MaxId > ?MAX_ID of
            true -> [{beyond_max_id, Table, Col, MaxId}];
            false -> P0
        end,
    P2 =
        case Count > 0 andalso TsMax > (NowMs - ?EPOCH_MS + ?FUTURE_TOLERANCE_MS) of
            true -> [{future_beyond_tolerance, Table, Col, TsMax, NowMs - ?EPOCH_MS}];
            false -> P1
        end,
    P3 =
        case Count > 0 andalso TsMax > ?MAX_REL_TS of
            true -> [{beyond_max_rel_ts, Table, Col, TsMax} | P2];
            false -> P2
        end,
    P4 =
        case Count > 0 andalso TsMin < 0 of
            true -> [{ts_before_epoch, Table, Col, TsMin} | P3];
            false -> P3
        end,
    %% 双写重复无法在单表内从 id 发现（同 id 两行会被 PK 拒绝）；跨表重复
    %% 不属于分类缺陷（各表独立命名空间），TSID-00 结论维持。
    P4.

ts_of(Id) when is_integer(Id), Id > 0 -> Id bsr 21;
ts_of(_) -> 0.

q(Pid, Sql) when is_binary(Sql) ->
    epgsql:squery(Pid, binary_to_list(Sql));
q(Pid, Sql) when is_list(Sql) ->
    epgsql:squery(Pid, Sql).

plain(B) -> binary_to_list(B).
a2b(A) when is_atom(A) -> atom_to_binary(A, utf8);
a2b(L) when is_list(L) -> unicode:characters_to_binary(L);
a2b(B) when is_binary(B) -> B.

%% 标识符白名单校验后加引号（防注入纵深；来源本身是 inventory 常量）
quote_ident(Name) ->
    case re:run(Name, "^[a-z_][a-z0-9_]*$", []) of
        {match, _} -> [$", Name, $"];
        nomatch -> error({invalid_identifier, Name})
    end.

%% lib 合同错误 → BLOCKED_CUTOVER（合同为 FAIL 级，不得降 warning）
blocked(Reason0, Detail, OutDir) ->
    Reason = a2b(Reason0),
    io:format("BLOCKED_CUTOVER: ~ts~n~tp~n", [Reason, Detail]),
    write_json(
        OutDir ++ "/scanner-manifest.json",
        #{
            status => <<"BLOCKED_CUTOVER">>,
            reason => Reason,
            detail => iolist_to_binary(io_lib:format("~tp", [Detail]))
        }
    ),
    halt(3).

problem_json({scan_error, Table, Col, R}) ->
    #{
        type => <<"scan_error">>,
        table => a2b(Table),
        column => a2b(Col),
        error => a2b(io_lib:format("~p", [R]))
    };
problem_json({Tag, Table, Col}) ->
    #{type => a2b(Tag), table => a2b(Table), column => a2b(Col)};
problem_json({Tag, Table, Col, V}) ->
    M = problem_json({Tag, Table, Col}),
    M#{value => V};
problem_json({Tag, Table, Col, V, W}) ->
    M = problem_json({Tag, Table, Col, V}),
    M#{now_rel_ts => W}.

write_json(Path, Term) ->
    ok = filelib:ensure_dir(Path),
    ok = file:write_file(Path, json(Term)).

json(M) when is_map(M) ->
    [${, join([[$", a2b(K), $", $:, json(V)] || {K, V} <- lists:sort(maps:to_list(M))]), $}];
json(L) when is_list(L) ->
    [$[, join([json(V) || V <- L]), $]];
json(B) when is_binary(B) -> [$", esc(B), $"];
json(A) when is_atom(A) -> [$", atom_to_binary(A, utf8), $"];
json(I) when is_integer(I) -> integer_to_list(I);
json(F) when is_float(F) -> float_to_list(F, [{decimals, 6}]);
json(T) when is_tuple(T) -> json(tuple_to_list(T)).

join([]) -> [];
join([X]) -> [X];
join([X | Rest]) -> [X, $, | join(Rest)].

esc(B) when is_binary(B) ->
    <<<<(esc_c(C))/binary>> || <<C>> <= B>>.
esc_c($") -> <<"\\\"">>;
esc_c($\\) -> <<"\\\\">>;
esc_c($\n) -> <<"\\n">>;
esc_c($\r) -> <<"\\r">>;
esc_c($\t) -> <<"\\t">>;
esc_c(C) when C < 16#20 -> io_lib:format("\\u~4.16.0b", [C]);
esc_c(C) -> <<C/utf8>>.

%% Beams live in repo-root ebin (make compile). Resolve relative to the
%% caller's working directory and to this script's own location so both
%% invocation styles (runbook cwd=scripts/tsid, repo-root cwd) keep working.
ensure_paths() ->
    add_path("ebin"),
    add_path("deps/epgsql/ebin"),
    ScriptDir = filename:dirname(escript:script_name()),
    add_path(filename:join(ScriptDir, "../../ebin")),
    add_path(filename:join(ScriptDir, "../../deps/epgsql/ebin")),
    ok.

add_path(Path) ->
    case filelib:is_dir(Path) of
        true -> code:add_pathz(Path);
        false -> ok
    end.
