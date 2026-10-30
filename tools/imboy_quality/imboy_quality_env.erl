%% IMBOY_ERLANG_QUALITY：一轮执行的资源编排（纯 OTP + epgsql，直连项目配置的真实 PG）。
%% 由 scripts/quality.escript 调用；所有准备在整条命令计时之内（EUNIT_120S §2）。
%%
%% 准备项（缺资源一律 fail-closed 或按根归属硬失败，不以空环境假绿）：
%%   - TALK_OWNED 维护库 + 系统标识 + RUN/SCRATCH_ROOT（talk_record_owned_eunit_fixture 合同）
%%   - 候选模板库（候选 priv/migrations 构建，digest 键缓存）→ 每 worker 私有克隆
%%   - 独占车道按根资源：group_schedule 独占库、AG31×4 隔离库、HIRD（编译器 + PoC 库）、
%%     attv5/W403 清单库 + 清单/配置文件 + 任务私有 Garage、顺序依赖根的 fresh 克隆
%%   - talk_record 网关根：私有 Garage 配置 + 合成 AAC 夹具（普通车道公共 env）
-module(imboy_quality_env).
-export([
    prepare/3,
    release/1,
    release_once/1,
    worker_config/3,
    cleanup_configs/1,
    attv5_db_name/2,
    planned_db_names/2
]).

-define(HIRD_ROOTS, [
    agent_hird_mini_spike_tests,
    imboy_hird_acceptance_tests,
    imboy_hird_gates_tests,
    imboy_hird_p1_auth_tool_tests,
    imboy_hird_p2_replay_tests,
    imboy_hird_p3_compare_tests,
    imboy_hird_replay_offline_tests
]).
%% W403_RESOURCE_MANIFEST/W403_CONFIG_PATH + W202_GARAGE_*（test/lib/attachment_lifecycle_v5_*）。
%% 第二元为库名短标签：PG 标识符上限 63 字符（NAMEDATALEN-1），全名会被 CREATE DATABASE
%% 静默截断，而清单仍写全名 → 套件 assert_database 失败（2026-10-29 group 实证）。
-define(W403_ROOTS, [
    {attachment_lifecycle_v5_w403_gateway_protocol_tests, "w403gw"},
    {attachment_lifecycle_v5_w403_driver_matrix_tests, "w403dm"},
    {attachment_ordinary_http_protocol_tests, "w403ord"},
    {eb_asset_frozen_storage_protocol_tests, "w403eb"},
    {enterprise_asset_driver_pg_tests, "w403ead"},
    %% 2026-10-30 接入（att-v5 集成后落入普通车道缺 W202_GARAGE_*/W403 资源的根）：
    %% W6-01 W2/W3/W4 电池组（HTTP 面走 attachment_w403_http_fixture）与
    %% w403_storage/w401_rollback（显式 no_go garage_env_missing）。
    {attachment_lifecycle_v5_w601_w2_confirm_tests, "w601w2c"},
    {attachment_lifecycle_v5_w601_w2_pending_tests, "w601w2p"},
    {attachment_lifecycle_v5_w601_w2_upload_tests, "w601w2u"},
    {attachment_lifecycle_v5_w601_w3_bind_tests, "w601w3b"},
    {attachment_lifecycle_v5_w601_w3_hold_tests, "w601w3h"},
    {attachment_lifecycle_v5_w601_w3_race_tests, "w601w3r"},
    {attachment_lifecycle_v5_w601_w4_activation_tests, "w601w4a"},
    {attachment_lifecycle_v5_w601_w4_hold_tests, "w601w4h"},
    {attachment_lifecycle_v5_w601_w4_intent_tests, "w601w4i"},
    {attachment_lifecycle_v5_w601_w4_purged_tests, "w601w4p"},
    {attachment_lifecycle_v5_w601_w4_queue_tests, "w601w4q"},
    {attachment_lifecycle_v5_w601_w4_race_tests, "w601w4r"},
    {attachment_lifecycle_v5_w601_w4_worker_tests, "w601w4w"},
    {attachment_lifecycle_v5_w403_storage_tests, "w403st"},
    {attachment_lifecycle_v5_w401_rollback_tests, "w401rb"},
    %% V5 marker provision 面（inttest_marker_db:provision(V5_INTTEST_*)）：
    %% V5_RUN_DIR/V5_SLOT/<PREFIX>_DB 由 attv5/7 统一注入（见下）。
    {attachment_lifecycle_v5_w401_entrypoints_tests, "w401ep"},
    {attachment_lifecycle_v5_w402_purged_recovery_tests, "w402pr"},
    {attachment_lifecycle_v5_w402_worker_tests, "w402wk"},
    {attachment_lifecycle_v5_w403_predecessor_tests, "w403pd"},
    {attachment_lifecycle_v5_hold_protocol_tests, "holdpt"},
    {attachment_lifecycle_v5_pg_tests, "v5pg"},
    {attachment_lifecycle_v5_w404_activation_tests, "w404ac"}
]).
%% ATTACHMENT_REVIEW_MANIFEST/ATTACHMENT_REVIEW_CONFIG（test/repo/message_result_pg_tests 等）
-define(REVIEW_ROOTS, [
    {message_result_pg_tests, "msgres"},
    {attachment_pending_location_pg_tests, "pendloc"}
]).
%% 顺序依赖根：独占车道 fresh VM + 本根专属克隆（imboy_quality_full ?EXCL 同列）。
%% msg_safe：共享 UID 对 920001/920002 的姊妹根直插残留抬高 observed_head（2026-10-30
%% probe2 seq 审计实证），独占克隆库消除跨根残留，详见 ?EXCL 注释。
-define(ISOLATED_ROOTS, [
    throttle_middleware,
    oa_direct_cloud_retention_pg_tests,
    msg_safe_boundary_pg_tests
]).

%% ---- 准备 ----

prepare(Repo, Ctrl, Workers) ->
    true = code:add_patha(filename:join(Repo, "deps/epgsql/ebin")),
    true = code:add_patha(filename:join(Repo, "deps/erlang_migrate/ebin")),
    {ok, _} = application:ensure_all_started(epgsql),
    Base = sys_config_base(),
    Args = imboy_quality_fixture:pg_args_from_config(filename:join(Repo, Base ++ ".config")),
    Pid = os:getpid(),
    {ok, C} = imboy_quality_fixture:connect(Args#{database => "postgres"}),
    try
        Reaped = reap_orphans(C, Args, Repo, Ctrl),
        Talk = talk_owned(C, Ctrl, Args),
        Tpl = imboy_quality_resources:ensure_candidate_template(C, Args, Repo),
        Template = maps:get(name, Tpl),
        SrcLiteral = maps:get(database, Args),
        Clone = fun(Name) ->
            ok = imboy_quality_fixture:clone_live(C, Args, Template, Name),
            Name
        end,
        Clones = maps:from_list([
            {K, Clone(lists:flatten(io_lib:format("imboy_quality_w~p_~s", [K, Pid])))}
         || K <- lists:seq(0, Workers - 1)
        ]),
        GsDb = Clone("imboy_quality_gsremind_" ++ Pid),
        ok = imboy_quality_fixture:comment_database(
            C, GsDb, "imboy-delivery-v3.2 merge schedule IMBOY_ERLANG_QUALITY exclusive " ++ Pid
        ),
        Ag31 = [
            {R, Clone("imboy_quality_ag31_" ++ Tag ++ "_" ++ Pid)}
         || {R, Tag} <- [
                {agent_grant_pg, "grant"},
                {agent_run_pg, "run"},
                {agent_recovery_pg_tests, "recovery"},
                {agent_tool_authorizer_pg_tests, "toolauth"}
            ]
        ],
        PgEnv = pg_env(Args),
        Hird = hird(Repo, Ctrl, Args, Clone, Pid),
        Garage = garage(Ctrl, Pid),
        Aac = imboy_quality_resources:ensure_aac_fixture(Ctrl),
        Attv5 = attv5(Repo, Base, Ctrl, Garage, Clone, SrcLiteral, Pid),
        Iso = isolated(Repo, Base, Clone, SrcLiteral, Pid),
        ExclusiveEnv = maps:merge(
            maps:merge(
                #{
                    group_schedule_remind_pg_tests => [
                        {"PGHOST", maps:get(host, PgEnv)},
                        {"PGPORT", maps:get(port, PgEnv)},
                        {"PGUSER", maps:get(user, PgEnv)},
                        {"PGPASSWORD", maps:get(pass, PgEnv)},
                        {"PGDATABASE", GsDb},
                        {"IMBOY_TEST_EXCLUSIVE_DATABASE", GsDb},
                        {"IMBOY_TEST_PG_SYSTEM_ID", maps:get(sys_id, Talk)}
                    ]
                },
                maps:from_list([
                    {R, [
                        {"AG31_PG_HOST", maps:get(host, PgEnv)},
                        {"AG31_PG_PORT", maps:get(port, PgEnv)},
                        {"AG31_PG_USER", maps:get(user, PgEnv)},
                        {"AG31_PG_PASSWORD", maps:get(pass, PgEnv)},
                        {"AG31_PG_DB", N}
                    ]}
                 || {R, N} <- Ag31
                ])
            ),
            maps:merge(maps:get(env, Hird), maps:get(env, Attv5))
        ),
        #{
            env => maps:get(env, Talk) ++ talk_garage_env(Ctrl, Garage, Aac, Pid),
            report => #{
                <<"reaped_orphans">> => Reaped,
                <<"talk_owned">> => maps:get(report, Talk),
                <<"template">> => #{
                    <<"name">> => list_to_binary(Template),
                    <<"digest">> => list_to_binary(maps:get(digest, Tpl)),
                    <<"migration_head">> => maps:get(head, Tpl),
                    <<"status">> => atom_to_binary(maps:get(status, Tpl))
                },
                <<"worker_db_clones">> => #{
                    <<"source">> => list_to_binary(Template),
                    <<"clones">> => [
                        list_to_binary(N)
                     || {_, N} <- lists:sort(maps:to_list(Clones))
                    ]
                },
                <<"exclusive_resources">> => #{
                    <<"group_schedule_remind_pg_tests">> => list_to_binary(GsDb),
                    <<"ag31">> => [list_to_binary(N) || {_, N} <- Ag31],
                    <<"hird">> => maps:get(report, Hird),
                    <<"garage">> => garage_report(Garage),
                    <<"aac_fixture">> => aac_report(Aac),
                    <<"attv5">> => maps:get(report, Attv5),
                    <<"isolated">> => maps:get(report, Iso)
                }
            },
            pg_args => Args,
            source_db => SrcLiteral,
            template => Template,
            clones => Clones,
            exclusive_env => ExclusiveEnv,
            exclusive_config => maps:merge(maps:get(config, Attv5), maps:get(config, Iso)),
            drop_dbs =>
                [GsDb | [N || {_, N} <- Ag31]] ++ maps:get(drop_dbs, Hird) ++
                maps:get(drop_dbs, Attv5) ++ maps:get(drop_dbs, Iso),
            garage => Garage,
            ctrl => Ctrl,
            runtime_dirs => [
                filename:join([Ctrl, "runtime", D, Pid])
             || D <- ["attv5", "hird"]
            ]
        }
    after
        close_quiet(C)
    end.

%% TALK_OWNED 合同（test/common/talk_record_owned_eunit_fixture.erl）：维护库的 database
%% COMMENT 为 marker、PG system_identifier 与 env 一致、RUN_ROOT 位于 SCRATCH_ROOT 之下。
%% legacy 入口从不提供这些变量；本入口在计时内自动准备，使这些义务真实执行。
talk_owned(C, Ctrl, Args) ->
    Maint = "imboy_quality_talk_owned_maint",
    Marker = "IMBOY_ERLANG_QUALITY:talk_owned:maint",
    ok = imboy_quality_fixture:ensure_marked_database(C, Maint, Marker),
    SysId = imboy_quality_fixture:system_identifier(C),
    Scratch = filename:join(Ctrl, "runtime/talk_owned"),
    Run = filename:join(Scratch, "run"),
    ok = filelib:ensure_dir(filename:join(Run, "x")),
    PgEnv = pg_env(Args),
    #{
        sys_id => SysId,
        env => [
            {"TALK_OWNED_PG_HOST", maps:get(host, PgEnv)},
            {"TALK_OWNED_PG_PORT", maps:get(port, PgEnv)},
            {"TALK_OWNED_PG_USER", maps:get(user, PgEnv)},
            {"TALK_OWNED_PG_PASSWORD", maps:get(pass, PgEnv)},
            {"TALK_OWNED_MAINT_DB", Maint},
            {"TALK_OWNED_PG_MARKER", Marker},
            {"TALK_OWNED_PG_SYSTEM_ID", SysId},
            {"TALK_OWNED_RUN_ROOT", Run},
            {"TALK_OWNED_SCRATCH_ROOT", Scratch}
        ],
        report => #{
            <<"maint_db">> => list_to_binary(Maint),
            <<"marker">> => list_to_binary(Marker),
            <<"pg">> => list_to_binary(maps:get(host, PgEnv) ++ ":" ++ maps:get(port, PgEnv)),
            <<"system_id">> => list_to_binary(SysId),
            <<"run_root">> => list_to_binary(Run)
        }
    }.

%% HIRD 七根：编译器经 IMBOY_HIRD_BIN，PoC 三根另需 HIRDPOC_PG_*（inttest_marker_db 以
%% MAINT_DB 自建一次性 marker 库）。产物根目录放任务 runtime（默认会写 worktree _build）。
hird(Repo, Ctrl, Args, Clone, Pid) ->
    case imboy_quality_resources:hird_bin(Repo) of
        {ok, Bin} ->
            Db = Clone("imboy_quality_hirdpoc_" ++ Pid),
            Out = filename:join([Ctrl, "runtime", "hird", Pid]),
            ok = filelib:ensure_dir(filename:join(Out, "x")),
            PgEnv = pg_env(Args),
            Env = [
                {"IMBOY_HIRD_BIN", Bin},
                {"IMBOY_HIRD_OUT_ROOT", Out},
                {"IMBOY_HIRD_FORCE_REBUILD", "1"},
                {"HIRDPOC_PG_HOST", maps:get(host, PgEnv)},
                {"HIRDPOC_PG_PORT", maps:get(port, PgEnv)},
                {"HIRDPOC_PG_USER", maps:get(user, PgEnv)},
                {"HIRDPOC_PG_PASSWORD", maps:get(pass, PgEnv)},
                {"HIRDPOC_PG_DB", Db},
                {"HIRDPOC_PG_MAINT_DB", "postgres"}
            ],
            #{
                env => maps:from_list([{R, Env} || R <- ?HIRD_ROOTS]),
                drop_dbs => [Db],
                report => #{
                    <<"bin">> => list_to_binary(Bin),
                    <<"poc_db">> => list_to_binary(Db),
                    <<"out_root">> => list_to_binary(Out)
                }
            };
        {error, Reason} ->
            #{env => #{}, drop_dbs => [], report => #{<<"error">> => fmt(Reason)}}
    end.

garage(Ctrl, Pid) ->
    try
        {ok,
            imboy_quality_resources:start_garage(Ctrl, Pid, [
                "imboy-w403-" ++ Pid, "talk-record-relay"
            ])}
    catch
        Class:Reason -> {error, {Class, Reason}}
    end.

garage_report({ok, G}) ->
    #{
        <<"endpoint">> => list_to_binary(maps:get(endpoint, G)),
        <<"buckets">> => [list_to_binary(B) || B <- maps:get(buckets, G)],
        <<"dir">> => list_to_binary(maps:get(dir, G))
    };
garage_report({error, Reason}) ->
    #{<<"error">> => fmt(Reason)}.

aac_report({ok, Path}) -> #{<<"path">> => list_to_binary(Path)};
aac_report({error, Reason}) -> #{<<"error">> => fmt(Reason)}.

%% attv5/W403（5 根）与 ATTACHMENT_REVIEW（2 根）：每根专属 attv5_chain_* 克隆 + 清单 JSON
%% + 私有 -config（app 启动即以清单库为 pgsql 池，套件 assert_database 才成立）+ W202 Garage。
attv5(Repo, Base, Ctrl, Garage, Clone, SrcLiteral, Pid) ->
    Dir = filename:join([Ctrl, "runtime", "attv5", Pid]),
    GarageEnv =
        case Garage of
            {ok, G} ->
                [
                    {"W202_GARAGE_ENDPOINT", maps:get(endpoint, G)},
                    {"W202_GARAGE_BUCKET", "imboy-w403-" ++ Pid},
                    {"W202_GARAGE_ACCESS_KEY", maps:get(access_key, G)},
                    {"W202_GARAGE_SECRET_KEY", maps:get(secret_key, G)}
                ];
            {error, _} ->
                []
        end,
    All = ?W403_ROOTS ++ ?REVIEW_ROOTS,
    Prepared = [
        begin
            Tag = atom_to_list(R),
            Db = Clone(attv5_db_name(Short, Pid)),
            Manifest = filename:join(Dir, Tag ++ ".json"),
            ok = imboy_quality_resources:write_json(Manifest, #{database => list_to_binary(Db)}),
            Config = imboy_quality_resources:write_config(Repo, Base, Tag, K, {SrcLiteral, Db}),
            ConfigFile = Config ++ ".config",
            Env =
                [
                    {"W403_RESOURCE_MANIFEST", Manifest},
                    {"W403_CONFIG_PATH", ConfigFile},
                    {"ATTACHMENT_REVIEW_MANIFEST", Manifest},
                    {"ATTACHMENT_REVIEW_CONFIG", ConfigFile}
                ] ++ GarageEnv,
            {R, Db, Config, Env}
        end
     || {K, {R, Short}} <- lists:zip(lists:seq(20, 20 + length(All) - 1), All)
    ],
    #{
        env => maps:from_list([{R, Env} || {R, _, _, Env} <- Prepared]),
        config => maps:from_list([{R, Config} || {R, _, Config, _} <- Prepared]),
        drop_dbs => [Db || {_, Db, _, _} <- Prepared],
        report => #{
            <<"databases">> => [list_to_binary(Db) || {_, Db, _, _} <- Prepared],
            <<"garage_env">> => GarageEnv =/= []
        }
    }.

isolated(Repo, Base, Clone, SrcLiteral, Pid) ->
    Prepared = [
        begin
            Tag = atom_to_list(R),
            Db = Clone(iso_db_name(R, Pid)),
            {R, Db, imboy_quality_resources:write_config(Repo, Base, Tag, K, {SrcLiteral, Db})}
        end
     || {K, R} <- lists:zip(lists:seq(40, 40 + length(?ISOLATED_ROOTS) - 1), ?ISOLATED_ROOTS)
    ],
    #{
        config => maps:from_list([{R, Config} || {R, _, Config} <- Prepared]),
        drop_dbs => [Db || {_, Db, _} <- Prepared],
        report => #{<<"databases">> => [list_to_binary(Db) || {_, Db, _} <- Prepared]}
    }.

attv5_db_name(Short, Pid) -> "attv5_chain_imboy_quality_" ++ Short ++ "_" ++ Pid.

iso_db_name(Root, Pid) -> "imboy_quality_iso_" ++ atom_to_list(Root) ++ "_" ++ Pid.

%% 本轮将创建的全部库名（自测用：每个都必须 ≤ 63 字符，PG 会静默截断更长的标识符）。
planned_db_names(Workers, Pid) ->
    [
        lists:flatten(io_lib:format("imboy_quality_w~p_~s", [K, Pid]))
     || K <- lists:seq(0, Workers - 1)
    ] ++
        ["imboy_quality_gsremind_" ++ Pid, "imboy_quality_hirdpoc_" ++ Pid] ++
        ["imboy_quality_ag31_" ++ T ++ "_" ++ Pid || T <- ["grant", "run", "recovery", "toolauth"]] ++
        [attv5_db_name(S, Pid) || {_, S} <- ?W403_ROOTS ++ ?REVIEW_ROOTS] ++
        [iso_db_name(R, Pid) || R <- ?ISOLATED_ROOTS].

%% talk_record 网关根（普通车道）：TALK_RECORD_PRIVATE_GARAGE_CONFIG 要求桶名恒为
%% talk-record-relay、endpoint 为 http://127.0.0.1:*；TALK_HTTP_AAC_FIXTURE 为合成 AAC。
%% JSON 键集 = 全部消费者读取的并集：网关根读连接 5 键，
%% private_garage 根另读 spool_dir（attachment_gateway_tmp_dir），缺键即 setup badkey。
talk_garage_env(Ctrl, {ok, G}, Aac, Pid) ->
    Scratch = filename:join([Ctrl, "runtime", "talk_owned"]),
    Path = filename:join(Scratch, "garage_" ++ Pid ++ ".json"),
    Spool = filename:join(Scratch, "spool_" ++ Pid),
    ok = filelib:ensure_dir(filename:join(Spool, "x")),
    ok = imboy_quality_resources:write_json(Path, #{
        endpoint => list_to_binary(maps:get(endpoint, G)),
        bucket => <<"talk-record-relay">>,
        region => list_to_binary(maps:get(region, G)),
        access_key => list_to_binary(maps:get(access_key, G)),
        secret_key => list_to_binary(maps:get(secret_key, G)),
        spool_dir => list_to_binary(Spool)
    }),
    [{"TALK_RECORD_PRIVATE_GARAGE_CONFIG", Path} | aac_env(Aac)];
talk_garage_env(_Ctrl, {error, _}, Aac, _Pid) ->
    aac_env(Aac).

aac_env({ok, Path}) -> [{"TALK_HTTP_AAC_FIXTURE", Path}];
aac_env({error, _}) -> [].

%% ---- 孤儿回收 ----

%% 控制器被 KILL（非 TERM）时 try/after 与信号处理器都不会执行；下一轮 prepare 回收
%% 已死控制器 pid 留下的克隆库、配置副本、Garage 守护进程与 runtime 目录。判据：资源名
%% 尾部 _<pid> 对应的 OS 进程不存在；活跃 pid（并行中的另一轮）一概不碰。维护库与模板库
%% 不带运行 pid，天然不在范围内。
reap_orphans(C, Args, Repo, Ctrl) ->
    {ok, _, Rows} = q(C, [
        "SELECT datname FROM pg_database WHERE datname LIKE 'imboy_quality_%' ",
        "OR datname LIKE 'attv5_chain_imboy_quality_%'"
    ]),
    DeadDbs = [
        N
     || {B} <- Rows,
        N <- [binary_to_list(B)],
        not lists:prefix("imboy_quality_tpl_", N),
        trailing_pid(N) =/= undefined,
        not pid_alive(trailing_pid(N))
    ],
    DbResults = [{N, catch imboy_quality_fixture:drop_owned(C, Args, N)} || N <- DeadDbs],
    GarageDirs = dead_pid_dirs(filename:join([Ctrl, "runtime", "garage"])),
    [reap_garage(Ctrl, P) || P <- GarageDirs],
    ScratchDirs = [
        filename:join([Ctrl, "runtime", D, P])
     || D <- ["attv5", "hird"], P <- dead_pid_dirs(filename:join([Ctrl, "runtime", D]))
    ],
    [_ = file:del_dir_r(Dir) || Dir <- ScratchDirs],
    Configs = [
        F
     || F <- filelib:wildcard(filename:join(Repo, "config/sys.quality.*")),
        Pid <- [config_pid(F)],
        Pid =/= undefined,
        not pid_alive(Pid)
    ],
    [_ = file:delete(F) || F <- Configs],
    #{
        <<"databases">> => [list_to_binary(N) || {N, ok} <- DbResults],
        <<"database_drop_failed">> => [
            fmt({N, R})
         || {N, R} <- DbResults, R =/= ok
        ],
        <<"garage_daemons">> => [list_to_binary(P) || P <- GarageDirs],
        <<"scratch_dirs">> => length(ScratchDirs),
        <<"config_copies">> => length(Configs)
    }.

reap_garage(Ctrl, PidDir) ->
    Dir = filename:join([Ctrl, "runtime", "garage", PidDir]),
    case file:read_file(filename:join(Dir, "server.pid")) of
        {ok, B} ->
            OsPid = string:trim(binary_to_list(B)),
            case pid_alive(list_to_integer(OsPid)) of
                true -> _ = os:cmd("kill -TERM " ++ OsPid);
                false -> ok
            end;
        _ ->
            ok
    end,
    _ = file:copy(
        filename:join(Dir, "server.log"),
        filename:join([Ctrl, "evidence", "garage-" ++ PidDir ++ "-orphan.log"])
    ),
    _ = file:del_dir_r(Dir),
    ok.

dead_pid_dirs(Parent) ->
    case file:list_dir(Parent) of
        {ok, Names} ->
            [
                N
             || N <- Names,
                re:run(N, "^\\d+$") =/= nomatch,
                filelib:is_dir(filename:join(Parent, N)),
                not pid_alive(list_to_integer(N))
            ];
        _ ->
            []
    end.

trailing_pid(Name) ->
    case re:run(Name, "_(\\d+)$", [{capture, all_but_first, list}]) of
        {match, [P]} -> list_to_integer(P);
        nomatch -> undefined
    end.

%% config/sys.quality.<pid>.<tag>.config
config_pid(Path) ->
    case
        re:run(filename:basename(Path), "^sys\\.quality\\.(\\d+)\\.", [
            {capture, all_but_first, list}
        ])
    of
        {match, [P]} -> list_to_integer(P);
        nomatch -> undefined
    end.

pid_alive(Pid) when is_integer(Pid) ->
    Pid =:= list_to_integer(os:getpid()) orelse
        string:trim(os:cmd("ps -p " ++ integer_to_list(Pid) ++ " -o pid=")) =/= "".

q(C, Sql) ->
    SqlStr = iolist_to_binary(Sql),
    case epgsql:squery(C, SqlStr) of
        {ok, _} = Ok -> Ok;
        {ok, _, _} = Ok -> Ok;
        {ok, _, _, _} = Ok -> Ok;
        {error, E} -> error({sql_failed, SqlStr, E})
    end.

%% ---- 回收 ----

%% 回收所有本轮克隆库（worker VM 已退出，后端断开有界等待）、停止私有 Garage、删除
%% runtime 目录（Garage 日志先存入 evidence）。回收失败不掩盖：记入结果并由退出码体现。
release(#{pg_args := Args, clones := Clones} = Prepared) ->
    put(resources_released, true),
    {ok, C} = imboy_quality_fixture:connect(Args#{database => "postgres"}),
    DbResults =
        try
            [
                {Name,
                    try imboy_quality_fixture:drop_owned(C, Args, Name) of
                        ok -> ok
                    catch
                        Class:Reason -> {Class, Reason}
                    end}
             || Name <-
                    [N || {_, N} <- lists:sort(maps:to_list(Clones))] ++
                        maps:get(drop_dbs, Prepared, [])
            ]
        after
            close_quiet(C)
        end,
    GarageExit =
        case maps:get(garage, Prepared, undefined) of
            {ok, G} ->
                Evidence = filename:join([
                    maps:get(ctrl, Prepared), "evidence", "garage-" ++ os:getpid() ++ ".log"
                ]),
                _ = file:copy(maps:get(log, G), Evidence),
                maps:get(exit, imboy_quality_resources:stop_garage(G));
            _ ->
                not_started
        end,
    [_ = file:del_dir_r(D) || D <- maps:get(runtime_dirs, Prepared, [])],
    Failed = [
        iolist_to_binary(io_lib:format("~s: ~0p", [N, R]))
     || {N, R} <- DbResults, R =/= ok
    ],
    #{
        <<"clones_dropped">> => [list_to_binary(N) || {N, ok} <- DbResults],
        <<"clone_drop_failed">> => Failed,
        <<"garage">> => fmt(GarageExit),
        <<"ok">> => Failed =:= []
    };
release(_) ->
    #{<<"clones_dropped">> => [], <<"clone_drop_failed">> => [], <<"ok">> => true}.

%% after 子句的兜底回收：正常路径已回收则跳过；异常路径尽力回收不再抛错。
release_once(Prepared) ->
    case get(resources_released) of
        true ->
            ok;
        _ ->
            try
                _ = release(Prepared),
                ok
            catch
                _:_ -> ok
            end
    end.

%% ---- 配置副本 ----

%% 与 Makefile eunit-local 相同口径：每 worker K 一份私有 http 端口副本（19980+K /
%% 19970+K），database 指向本 worker 克隆，直连 PG，不写 git。
worker_config(Repo, K, Prepared) ->
    Src = maps:get(source_db, Prepared),
    Db =
        case Prepared of
            #{clones := Clones} when is_map_key(K, Clones) -> maps:get(K, Clones);
            _ -> Src
        end,
    imboy_quality_resources:write_config(Repo, sys_config_base(), integer_to_list(K), K, {Src, Db}).

cleanup_configs(Repo) ->
    [
        file:delete(filename:join([Repo, "config", F]))
     || F <- filelib:wildcard(filename:join(Repo, "config/sys.quality.*"))
    ],
    ok.

%% ---- 辅助 ----

sys_config_base() ->
    case os:getenv("EUNIT_CONFIG") of
        false -> "config/sys.local";
        B -> B
    end.

pg_env(Args) ->
    #{
        host => maps:get(host, Args, "127.0.0.1"),
        port => integer_to_list(maps:get(port, Args, 4323)),
        user => maps:get(username, Args, "imboy_user"),
        pass => maps:get(password, Args, "")
    }.

close_quiet(C) ->
    try
        epgsql:close(C)
    catch
        _:_ -> ok
    end.

fmt(Term) -> iolist_to_binary(io_lib:format("~0p", [Term])).
