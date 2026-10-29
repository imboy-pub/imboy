%% IMBOY_ERLANG_QUALITY：独占/外部资源准备（纯 OTP，无 Shell/Python 控制器）。
%%
%% 候选模板库（EUNIT_120S §3.5）：共享 eunit 库 imboy_test_v1 是可变的、被同机
%% 其他任务的迁移写过（opt-run8 现场：DEP-B 的 00000237 于 13:39Z 写入共享库，
%% 候选最高 236，断言 236 的根因此失败）。模板必须由候选自己的 priv/migrations
%% 与扩展清单构建，键为迁移内容 digest，可跨轮复用；worker/独占克隆一律从模板出。
%%
%% 任务私有原生 Garage（§2.2/§3.7）：attv5/W403 与 talk_record 网关根需要真实 S3；
%% 本机 Homebrew `garage` 原生拉起（独立数据目录/随机端口/随机密钥），跑完即停并清理。
%%
%% HIRD 编译器定位、attv5 清单文件、ffmpeg 合成 AAC 夹具：只准备资源，不伪造结果；
%% 二进制缺失时返回 {error, _}，调用方让对应根按各自 no-skip 铁律硬失败（可归属）。
-module(imboy_quality_resources).
-export([
    ensure_candidate_template/3,
    template_digest/2,
    migration_head/1,
    start_garage/3,
    stop_garage/1,
    garage_toml/1,
    parse_key_output/1,
    parse_node_id/1,
    free_port/0,
    hird_bin/1,
    ensure_aac_fixture/1,
    write_json/2,
    write_config/5,
    port_base/0
]).

%% 与 test/common/inttest_marker_db.erl 的 clean-deploy 前置扩展清单一致。
-define(EXTENSIONS, [
    "pgcrypto",
    "pg_jieba",
    "timescaledb",
    "vector",
    "postgis",
    "pg_trgm",
    "btree_gin",
    "btree_gist",
    "citext",
    "unaccent",
    "intarray",
    "pg_stat_statements"
]).
-define(TEMPLATE_PREFIX, "imboy_quality_tpl_").
-define(TEMPLATE_TABLE, "imboy_quality_meta.imboy_quality_template").

%% ---- 候选模板库 ----

%% 返回 #{name, digest, head, status => built | reused}。Conn 连在 postgres 维护库。
ensure_candidate_template(Conn, Args, Repo) ->
    Head = migration_head(Repo),
    Digest = template_digest(Repo, pg_version(Conn)),
    Name = ?TEMPLATE_PREFIX ++ Digest,
    prune_stale_templates(Conn, Args, Name),
    case db_exists(Conn, Name) andalso template_valid(Args, Name, Digest, Head) of
        true ->
            #{name => Name, digest => Digest, head => Head, status => reused};
        false ->
            case db_exists(Conn, Name) of
                true -> imboy_quality_fixture:drop_owned(Conn, Args, Name);
                false -> ok
            end,
            build_template(Conn, Args, Repo, Name, Digest, Head),
            #{name => Name, digest => Digest, head => Head, status => built}
    end.

build_template(Conn, Args, Repo, Name, Digest, Head) ->
    q(Conn, ["CREATE DATABASE \"", Name, "\""]),
    try
        imboy_quality_fixture:connect_db(Args, Name, fun(C) ->
            [q(C, ["CREATE EXTENSION IF NOT EXISTS ", E]) || E <- ?EXTENSIONS],
            Dir = filename:join(Repo, "priv/migrations"),
            Config = #{conn => C, dir => Dir, strict => true},
            case erlang_migrate:up(Config) of
                ok -> ok;
                {ok, _Applied} -> ok;
                {error, Reason} -> error({template_migrate_failed, Reason})
            end,
            {ok, Head, false} = erlang_migrate:version(#{conn => C, dir => Dir}),
            ok = imboy_quality_fixture:ensure_marker(C),
            q(C, [
                "CREATE TABLE IF NOT EXISTS ",
                ?TEMPLATE_TABLE,
                " (digest text NOT NULL, head integer NOT NULL, built_at timestamptz DEFAULT now())"
            ]),
            q(C, [
                "INSERT INTO ",
                ?TEMPLATE_TABLE,
                " (digest, head) VALUES ('",
                Digest,
                "', ",
                integer_to_list(Head),
                ")"
            ]),
            ok
        end),
        q(Conn, ["ALTER DATABASE \"", Name, "\" IS_TEMPLATE true"]),
        q(Conn, [
            "COMMENT ON DATABASE \"", Name, "\" IS 'IMBOY_ERLANG_QUALITY:template:", Digest, "'"
        ]),
        ok
    catch
        Class:Reason:Stack ->
            catch q(Conn, ["DROP DATABASE \"", Name, "\""]),
            erlang:raise(Class, {template_build_failed, Name, Reason}, Stack)
    end.

template_valid(Args, Name, Digest, Head) ->
    try
        imboy_quality_fixture:connect_db(Args, Name, fun(C) ->
            {ok, _, Rows} = q(C, [
                "SELECT 1 FROM ", ?TEMPLATE_TABLE, " WHERE digest = '", Digest, "' AND head = ",
                integer_to_list(Head)
            ]),
            {ok, _, [{V}]} = q(C, "SELECT version::text FROM schema_migrations LIMIT 1"),
            Rows =/= [] andalso binary_to_integer(V) =:= Head
        end)
    catch
        _:_ -> false
    end.

%% 同前缀但 digest 不同的旧模板（候选迁移已变）：归属核验后回收，避免库累积。
prune_stale_templates(Conn, Args, Keep) ->
    {ok, _, Rows} = q(Conn, [
        "SELECT datname FROM pg_database WHERE datname LIKE '", ?TEMPLATE_PREFIX, "%'"
    ]),
    [
        catch imboy_quality_fixture:drop_owned(Conn, Args, binary_to_list(N))
     || {N} <- Rows, binary_to_list(N) =/= Keep
    ],
    ok.

%% digest = sha256(迁移文件名+内容 ‖ 扩展清单 ‖ PG 版本) 前 12 位 hex。
template_digest(Repo, PgVersion) ->
    Files = lists:sort(filelib:wildcard(filename:join(Repo, "priv/migrations/*.sql"))),
    Parts = [
        [filename:basename(F), 0, element(2, file:read_file(F)), 0]
     || F <- Files
    ],
    Bin = iolist_to_binary([Parts, "ext:", lists:join(",", ?EXTENSIONS), 0, "pg:", PgVersion]),
    Hex = string:lowercase(binary_to_list(binary:encode_hex(crypto:hash(sha256, Bin)))),
    lists:sublist(Hex, 12).

migration_head(Repo) ->
    Files = filelib:wildcard(filename:join(Repo, "priv/migrations/*.up.sql")),
    Versions = [
        list_to_integer(V)
     || F <- Files,
        {match, [V]} <- [
            re:run(filename:basename(F), "^(\\d{8})_.*\\.up\\.sql$", [{capture, all_but_first, list}])
        ]
    ],
    case Versions of
        [] -> error({no_migration_files, Repo});
        _ -> lists:max(Versions)
    end.

pg_version(Conn) ->
    {ok, _, [{V}]} = q(Conn, "SHOW server_version_num"),
    binary_to_list(V).

db_exists(Conn, Name) ->
    {ok, _, Rows} = q(Conn, ["SELECT 1 FROM pg_database WHERE datname = '", Name, "'"]),
    Rows =/= [].

%% ---- 任务私有原生 Garage ----

%% Buckets 在本实例内创建并授权给同一密钥。失败即 error（调用方决定是否降级为归属失败）。
start_garage(Ctrl, Tag, Buckets) ->
    Exe =
        case os:find_executable("garage") of
            false -> error({garage_binary_missing, "garage"});
            P -> P
        end,
    Dir = filename:join([Ctrl, "runtime", "garage", Tag]),
    _ = file:del_dir_r(Dir),
    ok = filelib:ensure_dir(filename:join(Dir, "x")),
    [S3, Rpc, Admin] = [free_port() || _ <- [1, 2, 3]],
    Cfg = filename:join(Dir, "garage.toml"),
    ok = file:write_file(
        Cfg,
        garage_toml(#{
            dir => Dir,
            s3_port => S3,
            rpc_port => Rpc,
            admin_port => Admin,
            rpc_secret => hex(crypto:strong_rand_bytes(32)),
            admin_token => hex(crypto:strong_rand_bytes(16))
        })
    ),
    Log = filename:join(Dir, "server.log"),
    Parent = self(),
    Ref = make_ref(),
    Owner = spawn(fun() -> garage_owner(Exe, Cfg, Log, Parent, Ref) end),
    OsPid =
        receive
            {Ref, os_pid, P0} -> P0
        after 10000 -> error(garage_spawn_timeout)
        end,
    %% 守护进程 pidfile：控制器被 KILL 时无法自清理，下一轮 prepare 据此回收孤儿实例。
    ok = file:write_file(filename:join(Dir, "server.pid"), integer_to_list(OsPid)),
    try
        ok = wait_tcp(S3, 300),
        NodeId = parse_node_id(cli_ok(Exe, Cfg, ["node", "id"])),
        _ = cli_ok(Exe, Cfg, ["layout", "assign", "-z", "dc1", "-c", "1G", NodeId]),
        _ = cli_ok(Exe, Cfg, ["layout", "apply", "--version", "1"]),
        [_ = cli_ok(Exe, Cfg, ["bucket", "create", B]) || B <- Buckets],
        #{key_id := KeyId, secret := Secret} =
            parse_key_output(cli_ok(Exe, Cfg, ["key", "create", "imboy-quality-" ++ Tag])),
        [
            _ = cli_ok(Exe, Cfg, [
                "bucket", "allow", "--read", "--write", "--owner", B, "--key", KeyId
            ])
         || B <- Buckets
        ],
        #{
            owner => Owner,
            os_pid => OsPid,
            dir => Dir,
            config => Cfg,
            log => Log,
            s3_port => S3,
            endpoint => "http://127.0.0.1:" ++ integer_to_list(S3),
            region => "garage",
            access_key => KeyId,
            secret_key => Secret,
            buckets => Buckets
        }
    catch
        Class:Reason:Stack ->
            _ = stop_garage(#{owner => Owner, dir => Dir, log => Log, keep_dir => true}),
            erlang:raise(Class, {garage_setup_failed, Reason}, Stack)
    end.

garage_owner(Exe, Cfg, Log, Parent, Ref) ->
    {ok, Fd} = file:open(Log, [write, raw, binary]),
    Port = open_port(
        {spawn_executable, Exe},
        [{args, ["-c", Cfg, "server"]}, exit_status, use_stdio, stderr_to_stdout, binary]
    ),
    {os_pid, OsPid} = erlang:port_info(Port, os_pid),
    Parent ! {Ref, os_pid, OsPid},
    garage_loop(Port, OsPid, Fd, undefined).

garage_loop(Port, OsPid, Fd, Exit) ->
    receive
        {Port, {data, D}} ->
            _ = file:write(Fd, D),
            garage_loop(Port, OsPid, Fd, Exit);
        {Port, {exit_status, Code}} ->
            garage_loop(Port, OsPid, Fd, Code);
        {stop, From, Ref} ->
            Code =
                case Exit of
                    undefined ->
                        _ = os:cmd("kill -TERM " ++ integer_to_list(OsPid)),
                        wait_exit(Port, OsPid, 100);
                    C ->
                        C
                end,
            _ = file:close(Fd),
            From ! {Ref, stopped, Code}
    end.

wait_exit(Port, OsPid, 0) ->
    _ = os:cmd("kill -KILL " ++ integer_to_list(OsPid)),
    receive
        {Port, {exit_status, Code}} -> Code
    after 5000 -> killed
    end;
wait_exit(Port, OsPid, Left) ->
    receive
        {Port, {exit_status, Code}} -> Code
    after 100 -> wait_exit(Port, OsPid, Left - 1)
    end.

%% 停止守护进程并删除数据目录（keep_dir => true 仅用于失败取证路径）。
stop_garage(#{owner := Owner, dir := Dir} = G) ->
    Ref = make_ref(),
    Owner ! {stop, self(), Ref},
    Code =
        receive
            {Ref, stopped, C} -> C
        after 20000 -> stop_timeout
        end,
    case maps:get(keep_dir, G, false) of
        true -> ok;
        false -> _ = file:del_dir_r(Dir)
    end,
    #{exit => Code}.

garage_toml(#{
    dir := Dir,
    s3_port := S3,
    rpc_port := Rpc,
    admin_port := Admin,
    rpc_secret := Secret,
    admin_token := Token
}) ->
    io_lib:format(
        "metadata_dir = \"~s\"~n"
        "data_dir = \"~s\"~n"
        "db_engine = \"lmdb\"~n"
        "replication_factor = 1~n"
        "rpc_bind_addr = \"127.0.0.1:~p\"~n"
        "rpc_public_addr = \"127.0.0.1:~p\"~n"
        "rpc_secret = \"~s\"~n"
        "~n"
        "[s3_api]~n"
        "s3_region = \"garage\"~n"
        "api_bind_addr = \"127.0.0.1:~p\"~n"
        "root_domain = \".garage.localhost\"~n"
        "~n"
        "[admin]~n"
        "api_bind_addr = \"127.0.0.1:~p\"~n"
        "admin_token = \"~s\"~n",
        [filename:join(Dir, "meta"), filename:join(Dir, "data"), Rpc, Rpc, Secret, S3, Admin, Token]
    ).

%% `garage key create` 输出形如 "Key ID: GK..." / "Secret key: ..."（大小写与空白不敏感）。
parse_key_output(Out) ->
    Bin = iolist_to_binary(Out),
    Grab = fun(Re) ->
        case re:run(Bin, Re, [caseless, {capture, all_but_first, binary}]) of
            {match, [V]} -> binary_to_list(V);
            nomatch -> error({garage_key_output_unparsed, Bin})
        end
    end,
    #{key_id => Grab(<<"Key ID:\\s*(\\S+)">>), secret => Grab(<<"Secret key:\\s*(\\S+)">>)}.

%% `garage node id` 输出 "<hex id>@<ip>:<port>"；布局分配接受完整 id。
parse_node_id(Out) ->
    Bin = iolist_to_binary(Out),
    case re:run(Bin, <<"([0-9a-f]{16,})@">>, [caseless, {capture, all_but_first, binary}]) of
        {match, [Id]} -> binary_to_list(Id);
        nomatch -> error({garage_node_id_unparsed, Bin})
    end.

cli_ok(Exe, Cfg, Args) ->
    case cli(Exe, Cfg, Args) of
        {0, Out} -> Out;
        {Code, Out} -> error({garage_cli_failed, Args, Code, iolist_to_binary(Out)})
    end.

cli(Exe, Cfg, Args) ->
    Port = open_port(
        {spawn_executable, Exe},
        [{args, ["-c", Cfg | Args]}, exit_status, use_stdio, stderr_to_stdout, binary]
    ),
    cli_collect(Port, []).

cli_collect(Port, Acc) ->
    receive
        {Port, {data, D}} -> cli_collect(Port, [D | Acc]);
        {Port, {exit_status, Code}} -> {Code, lists:reverse(Acc)}
    after 60000 ->
        catch port_close(Port),
        error(garage_cli_timeout)
    end.

free_port() ->
    {ok, L} = gen_tcp:listen(0, [{ip, {127, 0, 0, 1}}, {reuseaddr, true}]),
    {ok, P} = inet:port(L),
    ok = gen_tcp:close(L),
    P.

wait_tcp(_Port, 0) ->
    error(garage_not_listening);
wait_tcp(Port, Left) ->
    case gen_tcp:connect({127, 0, 0, 1}, Port, [], 500) of
        {ok, S} ->
            gen_tcp:close(S),
            ok;
        {error, _} ->
            timer:sleep(100),
            wait_tcp(Port, Left - 1)
    end.

hex(B) -> string:lowercase(binary_to_list(binary:encode_hex(B))).

%% ---- HIRD 编译器 ----

%% 候选优先级：IMBOY_QUALITY_HIRD_BIN > IMBOY_HIRD_BIN > 工作区祖先目录的
%% hird/target/release/hird > PATH 上的 hird。只返回存在的常规文件。
hird_bin(Repo) ->
    Env = [os:getenv(K) || K <- ["IMBOY_QUALITY_HIRD_BIN", "IMBOY_HIRD_BIN"]],
    Ancestors = [
        filename:join([A, "hird", "target", "release", "hird"])
     || A <- ancestors(Repo, 8)
    ],
    Path = [os:find_executable("hird")],
    Cands = [C || C <- Env ++ Ancestors ++ Path, is_list(C), filelib:is_regular(C)],
    case Cands of
        [C | _] -> {ok, C};
        [] -> {error, hird_binary_missing}
    end.

ancestors(_Dir, 0) ->
    [];
ancestors(Dir, N) ->
    Parent = filename:dirname(Dir),
    case Parent =:= Dir of
        true -> [Dir];
        false -> [Dir | ancestors(Parent, N - 1)]
    end.

%% ---- 合成 AAC 夹具（TALK_HTTP_AAC_FIXTURE） ----
%% 唯一消费者是 talk_record_gateway_http_tests 链（client_audio 经 available_source/2 复用），
%% 该链注册的分段声明 end_ms=1000、manifest duration_ms=1000；产品渲染器
%% talk_record_audio_render:fit_pcm 按声明时长 ±160ms 校验解码 PCM，30 秒夹具会被如实判
%% audio_duration_mismatch → media_status=retryable_failed。夹具时长必须等于声明的 1 秒。
%% t12_support 自生成 30 秒夹具并声明 30 秒，不读本 env，不受影响。

ensure_aac_fixture(Ctrl) ->
    Path = filename:join([Ctrl, "runtime", "fixtures", "synthetic_440hz_1s.aac"]),
    case filelib:file_size(Path) > 0 of
        true ->
            {ok, Path};
        false ->
            case os:find_executable("ffmpeg") of
                false ->
                    {error, ffmpeg_missing};
                Exe ->
                    ok = filelib:ensure_dir(Path),
                    Port = open_port(
                        {spawn_executable, Exe},
                        [
                            {args, [
                                "-nostdin", "-y", "-v", "error",
                                "-f", "lavfi", "-i", "sine=frequency=440:duration=1",
                                "-ac", "1", "-ar", "16000", "-c:a", "aac", "-f", "adts", Path
                            ]},
                            exit_status, use_stdio, stderr_to_stdout, binary
                        ]
                    ),
                    case cli_collect(Port, []) of
                        {0, _} ->
                            case filelib:file_size(Path) > 0 of
                                true -> {ok, Path};
                                false -> {error, aac_fixture_empty}
                            end;
                        {Code, Out} ->
                            {error, {ffmpeg_failed, Code, iolist_to_binary(Out)}}
                    end
            end
    end.

%% ---- 清单与配置文件 ----

write_json(Path, Map) ->
    ok = filelib:ensure_dir(Path),
    ok = file:write_file(Path, iolist_to_binary(json:encode(Map))),
    ok.

%% 从 Base.config 派生一份私有配置：http/adm 端口按 PortIndex 唯一化，
%% database => "<SrcLiteral>" 全部指向 Db。写入 config/sys.quality.<pid>.<Tag>.config，
%% 返回不含扩展名的路径（-config 用）；caller 的 cleanup glob sys.quality.* 一并回收。
write_config(Repo, Base, Tag, PortIndex, {SrcLiteral, Db}) ->
    {ok, Bin} = file:read_file(filename:join(Repo, Base ++ ".config")),
    Sub = fun(B, From, To) -> binary:replace(B, From, To, [global]) end,
    PortBase = port_base(),
    Out0 = Sub(
        Bin,
        <<"{http_port, 9800}">>,
        list_to_binary(io_lib:format("{http_port, ~p}", [PortBase + PortIndex]))
    ),
    Out1 = Sub(
        Out0,
        <<"{http_port_adm, 9706}">>,
        list_to_binary(io_lib:format("{http_port_adm, ~p}", [PortBase - 10 + PortIndex]))
    ),
    Out = Sub(
        Out1,
        list_to_binary("database => \"" ++ SrcLiteral ++ "\""),
        list_to_binary("database => \"" ++ Db ++ "\"")
    ),
    Path = filename:join(Repo, "config/sys.quality." ++ os:getpid() ++ "." ++ Tag),
    ok = file:write_file(Path ++ ".config", Out),
    Path.

%% 多个独立控制器并行诊断时各自 worker 0 不能同绑 19980/19970；
%% IMBOY_QUALITY_PORT_BASE 调 http 基址，adm 基址恒为其 -10（默认 19980/19970 不变）。
port_base() ->
    case os:getenv("IMBOY_QUALITY_PORT_BASE") of
        false -> 19980;
        "" -> 19980;
        S ->
            case string:to_integer(string:trim(S)) of
                {N, ""} when N > 1024, N < 65000 -> N;
                _ -> error({bad_port_base, S})
            end
    end.

q(C, Sql) ->
    SqlStr = iolist_to_binary(Sql),
    case epgsql:squery(C, SqlStr) of
        {ok, _} = Ok -> Ok;
        {ok, _, _} = Ok -> Ok;
        {ok, _, _, _} = Ok -> Ok;
        {error, E} -> error({sql_failed, SqlStr, E})
    end.
