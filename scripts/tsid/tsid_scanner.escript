#!/usr/bin/env escript
%%! -escript main tsid_scanner_main
%% TSID-08 只读高水位 scanner（计划：离线 bootstrap 工具；不连接生产——
%% 由调用方传入 scratch/clone DB 的连接参数）。
%%
%% 功能（对应 AC-08A/AC-08B）：
%%   1. 从 inventory TSV 构造所有 TSID 列查询（id 或显式主键列）
%%   2. 动态发现 DB 中的 bigint 主键列并对照 inventory：
%%      in_both / inventory_only / db_only —— UNKNOWN 列（db_only 的 bigint id）
%%      立即 BLOCKED_CUTOVER 退出（Retry/STOP 合同）
%%   3. 逐表扫描：min/max id、行数、合法范围分类
%%      （negative / future_beyond_max_rel / at_max_id / legal）
%%   4. 计算全局高水位 = max(所有表 max id 的 timestamp 部分)
%%   5. 输出 scratch-high-water.json + scanner-manifest.json
%%
%% 用法：escript tsid_scanner.escript <inventory.tsv> <outdir> <allow_exclude_csv> -- host port user password database
%%   allow_exclude_csv：显式豁免的 db_only 表名（逗号分隔；合同要求逐表人工判定后
%%   记录于 inventory-supplement.tsv，scanner 拒绝静默豁免）
%%（password 可为 - 表示 trust/无密码）
-module(tsid_scanner_main).
-main([tsid_scanner_main]).

-define(EPOCH_MS, 1735689600000).
-define(MAX_REL_TS, 4398046511103).
-define(MAX_ID, 9223372036854775807).
%% 未来容忍度：扫描到的 ID 时间戳领先当前墙钟超过该值 → BLOCKED_CUTOVER
-define(FUTURE_TOLERANCE_MS, 60000).

main(Args) ->
    case parse_args(Args) of
        {ok, InvPath, OutDir, Conn} ->
            run(InvPath, OutDir, Conn);
        usage ->
            io:format("usage: tsid_scanner.escript <inventory.tsv> <outdir> <allow_exclude_csv> -- host port user password database~n"),
            halt(2)
    end.

parse_args(Args) ->
    case lists:splitwith(fun(X) -> X =/= "--" end, Args) of
        {[InvPath, OutDir, AllowCsv], ["--" | ConnStr]} when length(ConnStr) =:= 5 ->
            [H, P, U, PW, D] = ConnStr,
            {ok, InvPath, OutDir,
             #{host => H, port => list_to_integer(P), user => U,
               password => case PW of "-" -> ""; _ -> PW end,
               database => D,
               allow_exclude => [plain(T) || T <- binary:split(list_to_binary(AllowCsv), <<",">>, [global, trim_all]), T =/= <<>>]}};
        _ ->
            usage
    end.

run(InvPath, OutDir, Conn) ->
    StartedAt = os:system_time(millisecond),
    Inventory = load_inventory(InvPath),
    {ok, Pid} = epgsql:connect(
        #{host => maps:get(host, Conn), port => maps:get(port, Conn),
          username => maps:get(user, Conn), password => maps:get(password, Conn),
          database => maps:get(database, Conn), timeout => 10000}),
    try
        %% 1) 动态发现：所有含单列 bigint 主键的表（id 或其他名字）
        DbCols = discover_bigint_pk_tables(Pid),
        %% 2) inventory 对照（UNKNOWN 列合同）：豁免名单必须精确匹配，
        %%    多豁免（名单里没有的表）同样 BLOCKED——防止白名单膨胀失管
        Allow = maps:get(allow_exclude, Conn, []),
        {InBoth, InvOnly, DbOnly0} = classify(Inventory, DbCols),
        {DbOnly, _AllowedFound} =
            lists:partition(fun(T) -> not lists:member(T, Allow) end, DbOnly0),
        AllowUnknown = [T || T <- Allow, not lists:member(T, DbOnly0)],
        case AllowUnknown of
            [] -> ok;
            _ ->
                io:format("BLOCKED_CUTOVER: allow-exclude names not found in db: ~p~n",
                          [AllowUnknown]),
                write_json(OutDir ++ "/scanner-manifest.json",
                           #{status => <<"BLOCKED_CUTOVER">>,
                             reason => <<"allow_exclude_unknown_tables">>,
                             unknown_allow => [a2b(L) || L <- AllowUnknown]}),
                halt(3)
        end,
        case DbOnly of
            [] -> ok;
            _ ->
                io:format("BLOCKED_CUTOVER: db-only bigint PK tables not in inventory: ~p~n", [DbOnly]),
                write_json(OutDir ++ "/scanner-manifest.json",
                           #{status => <<"BLOCKED_CUTOVER">>, reason => <<"db_only_unknown_columns">>,
                             db_only => [a2b(L) || L <- DbOnly]}),
                halt(3)
        end,
        %% 3) 逐表扫描（每表一条聚合查询；表名来自 inventory 固定清单 + 白名单化
        %%    校验 ^[a-z0-9_]+$，非用户输入拼接面）
        NowMs = os:system_time(millisecond),
        {Rows, _FailedScan} =
            lists:mapfoldl(
                fun({T, Col}, Acc) ->
                    Res =
                        try scan_table(Pid, T, Col, NowMs) catch
                            C:Re:_ -> {'EXIT', {C, Re}}
                        end,
                    case Res of
                        {'EXIT', Reason} ->
                            {#{table => plain(T), column => plain(Col),
                               row_count => 0, min_id => 0, max_id => 0,
                               max_ts => 0,
                               problems => [{scan_error, T, Col, Reason}]}, Acc};
                        _ ->
                            {Res, Acc}
                    end
                end,
                [],
                lists:sort(InBoth)),
        Scanned = [plain(maps:get(table, R)) || R <- Rows],
        Missing = [T || {T, _} <- lists:sort(InBoth), not lists:member(T, Scanned)],
        case Missing of
            [] -> ok;
            _ ->
                io:format("BLOCKED_CUTOVER: inventory tables missing in db: ~p~n", [Missing]),
                write_json(OutDir ++ "/scanner-manifest.json",
                           #{status => <<"BLOCKED_CUTOVER">>, reason => <<"inventory_tables_missing">>,
                             missing => [a2b(M) || M <- Missing]}),
                halt(3)
        end,
        %% 4) 分类与高水位
        Problems = lists:append([P || #{problems := P} <- Rows]),
        Future = [F || F <- Problems, element(1, F) =:= future_beyond_tolerance],
        case Future of
            [] -> ok;
            _ ->
                io:format("BLOCKED_CUTOVER: future ids beyond tolerance: ~p~n", [Future]),
                write_json(OutDir ++ "/scanner-manifest.json",
                           #{status => <<"BLOCKED_CUTOVER">>, reason => <<"future_beyond_tolerance">>,
                             detail => [problem_json(F) || F <- Future]}),
                halt(3)
        end,
        HighWaterTs = lists:max([0 | [maps:get(max_ts, R, 0) || R <- Rows,
                                                         maps:get(row_count, R, 0) > 0]]),
        MaxId = lists:max([0 | [maps:get(max_id, R, 0) || R <- Rows,
                                                         maps:get(row_count, R, 0) > 0]]),
        Result = #{
            status => <<"OK">>,
            scanned_at_ms => StartedAt,
            tables => Rows,
            table_count => length(Rows),
            inventory_only_tables => [a2b(T) || T <- InvOnly],
            problems => [problem_json(P) || P <- Problems],
            high_water => #{
                max_ts_rel => HighWaterTs,
                max_ts_abs_ms => HighWaterTs + ?EPOCH_MS,
                floor_candidate => HighWaterTs + 1,
                max_id_seen => MaxId
            }
        },
        write_json(OutDir ++ "/scratch-high-water.json", Result),
        write_json(OutDir ++ "/scanner-manifest.json",
                   #{status => <<"OK">>, inventory_file => a2b(InvPath),
                     table_count => length(Rows),
                     scanned_at_ms => StartedAt}),
        io:format("SCAN_OK tables=~p high_water_rel=~p floor_candidate=~p problems=~p~n",
                  [length(Rows), HighWaterTs, HighWaterTs + 1, length(Problems)])
    after
        epgsql:close(Pid)
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
                [Kind, _Key, Table | Rest] when Kind =:= <<"generator">>;
                                             Kind =:= <<"default-site">>;
                                             Kind =:= <<"supplement-tsid08">> ->
                    case Table of
                        <<"(", _/binary>> -> Acc;
                        <<>> -> Acc;
                        _ ->
                            Col = col_from_note(Rest),
                            maps:put(plain(Table), Col, Acc)
                    end;
                _ ->
                    Acc
            end
        end,
        #{}, Lines).

%% note 字段里的 column=<name>（缺省 id）
col_from_note(Rest) ->
    Parsed = [binary:split(B, <<"column=">>) || B <- Rest],
    Hits = [Col || [_, Col] <- Parsed],
    case Hits of
        [Col | _] -> Col;
        _ -> <<"id">>
    end.

%% 动态发现：public schema 下有单列 bigint 主键的表（含列名）
discover_bigint_pk_tables(Pid) ->
    {ok, _Cols, Rows} = q(Pid,
        "SELECT tc.table_name, kcu.column_name "
        "FROM information_schema.table_constraints tc "
        "JOIN information_schema.key_column_usage kcu "
        "  ON tc.constraint_name = kcu.constraint_name AND tc.table_schema = kcu.table_schema "
        "JOIN information_schema.columns c "
        "  ON c.table_schema = tc.table_schema AND c.table_name = tc.table_name "
        " AND c.column_name = kcu.column_name "
        "WHERE tc.table_schema = 'public' AND tc.constraint_type = 'PRIMARY KEY' "
        "  AND c.data_type = 'bigint' "
        "GROUP BY tc.table_name, kcu.column_name "
        "HAVING count(*) = 1"),
    [{plain(T), plain(C)} || {T, C} <- Rows].

classify(Inventory, DbCols) ->
    InvKeys = lists:sort(maps:keys(Inventory)),
    DbKeys = lists:sort(lists:map(fun({T, _}) -> T end, DbCols)),
    DbMap = maps:from_list(DbCols),
    InBoth = [{T, maps:get(T, DbMap)} || T <- InvKeys, maps:is_key(T, DbMap)],
    InvOnly = [T || T <- InvKeys, not maps:is_key(T, DbMap)],
    DbOnly = [T || T <- DbKeys, not maps:is_key(T, Inventory)],
    {InBoth, InvOnly, DbOnly}.

scan_table(Pid, Table, Col, NowMs) ->
    Tq = quote_ident(Table),
    Cq = quote_ident(Col),
    Sql = iolist_to_binary(
        ["SELECT count(*), coalesce(min(", Cq, "),0), coalesce(max(", Cq, "),0) FROM public.", Tq]),
    case q(Pid, Sql) of
        {ok, _, [{Count0, MinId0, MaxId0}]} ->
            Count = to_int(Count0), MinId = to_int(MinId0), MaxId = to_int(MaxId0),
            TsMin = ts_of(MinId), TsMax = ts_of(MaxId),
            Problems =
                tables_problems(Table, Col, Count, MinId, MaxId, TsMin, TsMax, NowMs),
            #{table => a2b(Table), column => a2b(Col),
              row_count => to_int(Count), min_id => to_int(MinId), max_id => to_int(MaxId),
              min_ts_rel => TsMin, max_ts_rel => TsMax,
              max_ts => TsMax, problems => Problems};
        {error, R} ->
            #{table => a2b(Table), column => a2b(Col), row_count => 0,
              min_id => 0, max_id => 0, max_ts => 0,
              problems => [{scan_error, Table, Col, R}]}
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

problem_json({scan_error, Table, Col, R}) ->
    #{type => <<"scan_error">>, table => a2b(Table), column => a2b(Col),
      error => a2b(io_lib:format("~p", [R]))};
problem_json({Tag, Table, Col}) ->
    #{type => a2b(Tag), table => a2b(Table), column => a2b(Col)};
problem_json({Tag, Table, Col, V}) ->
    M = problem_json({Tag, Table, Col}), M#{value => V};
problem_json({Tag, Table, Col, V, W}) ->
    M = problem_json({Tag, Table, Col, V}), M#{now_rel_ts => W}.

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
    << <<(esc_c(C))/binary>> || <<C>> <= B >>.
esc_c($") -> <<"\\\"">>;
esc_c($\\) -> <<"\\\\">>;
esc_c($\n) -> <<"\\n">>;
esc_c($\r) -> <<"\\r">>;
esc_c($\t) -> <<"\\t">>;
esc_c(C) when C < 16#20 -> io_lib:format("\\u~4.16.0b", [C]);
esc_c(C) -> <<C/utf8>>.
