%% IMBOY_ERLANG_QUALITY：独占/外部资源准备（纯 OTP，无 Shell/Python 控制器）。
%%
%% 候选模板库（EUNIT_120S §3.5）：共享 eunit 库 imboy_test_v1 是可变的、被同机
%% 其他任务的迁移写过（opt-run8 现场：DEP-B 的 00000237 于 13:39Z 写入共享库，
%% 候选最高 236，断言 236 的根因此失败）。模板必须由候选自己的 priv/migrations
%% 与扩展清单构建，键绑定迁移、扩展版本和 locale，可跨轮复用；worker/独占克隆一律从模板出。
%%
%% 任务私有原生 Garage（§2.2/§3.7）：attv5/W403 与 talk_record 网关根需要真实 S3；
%% 本机 Homebrew `garage` 原生拉起（独立数据目录/随机端口/随机密钥），跑完即停并清理。
%%
%% HIRD 编译器定位、attv5 清单文件、内置固定 ADTS 字节的 AAC 夹具：只准备资源，不伪造结果；
%% 二进制缺失时返回 {error, _}，调用方让对应根按各自 no-skip 铁律硬失败（可归属）。
-module(imboy_quality_resources).
-include_lib("kernel/include/file.hrl").
-export([
    ensure_candidate_template/3,
    template_action/2,
    template_digest/2,
    migration_head/1,
    start_garage/3,
    garage_executable/0,
    create_garage_dir/2,
    owned_port_identity/3,
    close_owned_port/1,
    await_garage_start/5,
    stop_garage/1,
    cleanup_stopped_garage/2,
    directory_identity/1,
    garage_toml/1,
    parse_key_output/1,
    parse_node_id/1,
    free_port/0,
    hird_bin/1,
    textguard_bin/1,
    ensure_aac_fixture/1,
    write_json/2,
    write_config/5,
    begin_config_receipts/2,
    cleanup_config_receipts/0,
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

%% 返回 #{name, digest, head, environment, status => built | reused}。Conn 连在 postgres 维护库。
ensure_candidate_template(Conn, Args, Repo) ->
    Head = migration_head(Repo),
    Environment = template_environment(Conn, Args),
    Digest = template_digest(Repo, Environment),
    Name = ?TEMPLATE_PREFIX ++ Digest,
    Exists = db_exists(Conn, Name),
    %% Only reopen a template carrying our exact ownership comment.
    case Exists of
        true ->
            {ok, _, [{Comment}]} = epgsql:equery(
                Conn,
                "SELECT shobj_description(oid, 'pg_database') FROM pg_database WHERE datname=$1",
                [Name]
            ),
            Expected = list_to_binary("IMBOY_ERLANG_QUALITY:template:" ++ Digest),
            Comment =:= Expected orelse error({template_owner_mismatch, Name}),
            q(Conn, ["ALTER DATABASE \"", Name, "\" ALLOW_CONNECTIONS true"]);
        false ->
            ok
    end,
    Valid = Exists andalso template_valid(Args, Name, Digest, Head, Environment),
    Result =
        case template_action(Exists, Valid) of
            reuse ->
                #{
                    name => Name,
                    digest => Digest,
                    head => Head,
                    status => reused,
                    environment => Environment
                };
            conflict ->
                q(Conn, ["ALTER DATABASE \"", Name, "\" ALLOW_CONNECTIONS false"]),
                error({template_conflict_retained, Name});
            create ->
                build_template(Conn, Args, Repo, Name, Digest, Head, Environment),
                #{
                    name => Name,
                    digest => Digest,
                    head => Head,
                    status => built,
                    environment => Environment
                }
        end,
    %% A TimescaleDB scheduler can reconnect after migration even after our
    %% client disconnects. Stop only this immutable template's scheduler;
    %% cloned test databases retain normal background workers and durability.
    imboy_quality_fixture:connect_db(Args, Name, fun(C) ->
        %% Prevent a scheduler reconnect between the last check and CREATE
        %% DATABASE TEMPLATE. The new database defaults to allowing connections.
        q(Conn, ["ALTER DATABASE \"", Name, "\" ALLOW_CONNECTIONS false"]),
        {ok, _, [{<<"t">>}]} = q(C, "SELECT _timescaledb_functions.stop_background_workers()")
    end),
    ok = wait_template_disconnected(Conn, Name, 20),
    Result.

wait_template_disconnected(Conn, Name, Left) ->
    {ok, _, Rows} = epgsql:equery(
        Conn,
        "SELECT pid FROM pg_stat_activity WHERE datname=$1",
        [Name]
    ),
    case {Rows, Left} of
        {[], _} ->
            ok;
        {_, 0} ->
            error({template_still_in_use, Name, length(Rows)});
        _ ->
            timer:sleep(100),
            wait_template_disconnected(Conn, Name, Left - 1)
    end.

template_action(true, true) -> reuse;
template_action(true, false) -> conflict;
template_action(false, false) -> create.

build_template(Conn, Args, Repo, Name, Digest, Head, Environment) ->
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
            Environment = actual_template_environment(C),
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
            erlang:raise(Class, {template_build_failed_retained, Name, Reason}, Stack)
    end.

template_valid(Args, Name, Digest, Head, Environment) ->
    try
        imboy_quality_fixture:connect_db(Args, Name, fun(C) ->
            {ok, _, Rows} = q(C, [
                "SELECT 1 FROM ",
                ?TEMPLATE_TABLE,
                " WHERE digest = '",
                Digest,
                "' AND head = ",
                integer_to_list(Head)
            ]),
            {ok, _, [{V}]} = q(C, "SELECT version::text FROM schema_migrations LIMIT 1"),
            Rows =/= [] andalso binary_to_integer(V) =:= Head andalso
                actual_template_environment(C) =:= Environment
        end)
    catch
        _:_ -> false
    end.

%% The key binds migrations to the environment CREATE DATABASE inherits from
%% template1, including installed extension versions (not just extension names).
%% A map is mandatory: accepting a bare PG version would silently omit inputs.
template_digest(Repo, #{pg_version := _, extension_versions := _, locale := _} = Environment) ->
    Files = lists:sort(filelib:wildcard(filename:join(Repo, "priv/migrations/*.sql"))),
    Parts = [
        [filename:basename(F), 0, element(2, file:read_file(F)), 0]
     || F <- Files
    ],
    Bin = iolist_to_binary([
        Parts, "template-environment-v2:", term_to_binary(Environment, [deterministic])
    ]),
    Hex = string:lowercase(binary_to_list(binary:encode_hex(crypto:hash(sha256, Bin)))),
    lists:sublist(Hex, 12).

template_environment(Conn, Args) ->
    %% Inspect template1 rather than the maintenance connection: CREATE
    %% EXTENSION IF NOT EXISTS retains any version inherited from template1.
    Version = pg_version(Conn),
    binary_to_integer(Version) >= 180000 orelse error({postgres_18_required, Version}),
    imboy_quality_fixture:connect_db(Args, "template1", fun(C) ->
        Installed = installed_extension_versions(C),
        {ok, _, AvailableRows} = q(C, "SELECT name, default_version FROM pg_available_extensions"),
        Available = maps:from_list(AvailableRows),
        Expected = lists:foldl(
            fun(E, Acc) ->
                Key = list_to_binary(E),
                case maps:is_key(Key, Acc) of
                    true ->
                        Acc;
                    false ->
                        case maps:find(Key, Available) of
                            {ok, V} when is_binary(V) -> maps:put(Key, V, Acc);
                            _ -> error({template_extension_unavailable, E})
                        end
                end
            end,
            Installed,
            ?EXTENSIONS
        ),
        #{pg_version => Version, extension_versions => Expected, locale => database_locale(C)}
    end).

actual_template_environment(C) ->
    #{
        pg_version => pg_version(C),
        extension_versions => installed_extension_versions(C),
        locale => database_locale(C)
    }.

installed_extension_versions(C) ->
    {ok, _, Rows} = q(C, "SELECT extname, extversion FROM pg_extension"),
    maps:from_list(Rows).

database_locale(C) ->
    %% PG18: datlocale holds the builtin/ICU locale; libc uses collate/ctype.
    %% Include recorded and current provider versions to invalidate on upgrades.
    {ok, _, [Values]} = q(
        C,
        "SELECT pg_encoding_to_char(encoding), datlocprovider::text, datcollate, datctype, "
        "datlocale, daticurules, datcollversion, pg_database_collation_actual_version(oid) "
        "FROM pg_database WHERE datname=current_database()"
    ),
    maps:from_list(
        lists:zip(
            [
                encoding,
                provider,
                collate,
                ctype,
                locale,
                icu_rules,
                collation_version,
                actual_collation_version
            ],
            tuple_to_list(Values)
        )
    ).

migration_head(Repo) ->
    Files = filelib:wildcard(filename:join(Repo, "priv/migrations/*.up.sql")),
    Versions = [
        list_to_integer(V)
     || F <- Files,
        {match, [V]} <- [
            re:run(filename:basename(F), "^(\\d{8})_.*\\.up\\.sql$", [
                {capture, all_but_first, list}
            ])
        ]
    ],
    case Versions of
        [] -> error({no_migration_files, Repo});
        _ -> lists:max(Versions)
    end.

pg_version(Conn) ->
    {ok, _, [{V}]} = q(Conn, "SHOW server_version_num"),
    V.

db_exists(Conn, Name) ->
    {ok, _, Rows} = q(Conn, ["SELECT 1 FROM pg_database WHERE datname = '", Name, "'"]),
    Rows =/= [].

%% ---- 任务私有原生 Garage ----

%% Buckets 在本实例内创建并授权给同一密钥。失败即 error（调用方决定是否降级为归属失败）。
%% Never silently substitute a Docker/S3 mock for the real Garage process.
garage_executable() ->
    case os:getenv("IMBOY_QUALITY_GARAGE_BIN") of
        false ->
            case os:find_executable("garage") of
                false -> error({garage_binary_missing, "garage"});
                P -> P
            end;
        Path when is_list(Path), Path =/= [] ->
            case filename:pathtype(Path) of
                absolute ->
                    case os:find_executable(Path) of
                        false -> error({garage_binary_missing, Path});
                        P -> P
                    end;
                _ -> error({garage_binary_must_be_absolute, Path})
            end;
        _ -> error(invalid_garage_binary_override)
    end.

start_garage(Ctrl, Tag, Buckets) ->
    Exe = garage_executable(),
    {Dir, Identity, Token} = create_garage_dir(Ctrl, Tag),
    [S3, Rpc, Admin] = [free_port() || _ <- [1, 2, 3]],
    case length(lists:usort([S3, Rpc, Admin])) of
        3 -> ok;
        _ -> error({garage_port_collision, [S3, Rpc, Admin]})
    end,
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
        }),
        [exclusive]
    ),
    Log = filename:join(Dir, "server.log"),
    Parent = self(),
    Ref = make_ref(),
    {Owner, Monitor} = spawn_monitor(fun() -> garage_owner(Exe, Cfg, Log, Parent, Ref) end),
    Startup = #{
        dir => Dir, dir_identity => Identity, dir_token => Token, exe => Exe, config => Cfg
    },
    try
        {ok, OsPid, ProcessIdentity} = await_garage_start(Owner, Monitor, Ref, Startup, 10000),
        {ok, VerifiedIdentity} = ProcessIdentity,
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
            process_identity => VerifiedIdentity,
            dir => Dir,
            dir_identity => Identity,
            dir_token => Token,
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
            Stop =
                case is_process_alive(Owner) of
                    true ->
                        stop_garage(#{owner => Owner, dir => Dir, log => Log, keep_dir => true});
                    false ->
                        #{exit => owner_not_alive, cleanup => retained}
                end,
            erlang:raise(Class, {garage_setup_failed, Reason, Stop}, Stack)
    after
        erlang:demonitor(Monitor, [flush])
    end.

garage_owner(Exe, Cfg, Log, Parent, Ref) ->
    {ok, Fd} = file:open(Log, [write, raw, binary, exclusive]),
    Port = open_port(
        {spawn_executable, Exe},
        [{args, ["-c", Cfg, "server"]}, exit_status, use_stdio, stderr_to_stdout, binary]
    ),
    {os_pid, OsPid} = erlang:port_info(Port, os_pid),
    PidFile = filename:join(filename:dirname(Cfg), "server.pid"),
    case file:write_file(PidFile, integer_to_list(OsPid), [exclusive]) of
        ok -> Parent ! {Ref, spawned, OsPid};
        {error, Why} -> Parent ! {Ref, pidfile_error, OsPid, Why}
    end,
    Identity = owned_port_identity(Port, Exe, Cfg),
    Parent ! {Ref, os_pid, OsPid, Identity},
    garage_loop(Port, Fd, undefined).

%% Suspend the owner before cancellation so it cannot launch after timeout.
await_garage_start(Owner, Monitor, Ref, Startup, Timeout) ->
    Deadline = erlang:monotonic_time(millisecond) + Timeout,
    await_garage_start(Owner, Monitor, Ref, Startup, Deadline, undefined).

await_garage_start(Owner, Monitor, Ref, Startup, Deadline, SeenPid) ->
    Remaining = max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {Ref, spawned, Pid} -> await_garage_start(Owner, Monitor, Ref, Startup, Deadline, Pid);
        {Ref, pidfile_error, Pid, Why} -> {error, {pidfile_write_failed, Pid, Why}};
        {Ref, os_pid, Pid, Identity} -> {ok, Pid, Identity};
        {'DOWN', Monitor, process, Owner, Why} -> {error, {garage_owner_down, Why, SeenPid}}
    after Remaining ->
        cancel_garage_start(Owner, Monitor, Startup, SeenPid)
    end.

cancel_garage_start(Owner, Monitor, Startup, SeenPid) ->
    Suspended =
        try
            erlang:suspend_process(Owner)
        catch
            error:badarg -> false
        end,
    Links =
        case Suspended of
            true ->
                case erlang:process_info(Owner, links) of
                    {links, L} -> L;
                    _ -> []
                end;
            false ->
                []
        end,
    Pids = lists:usort(
        [
            Pid
         || Port <- Links,
            is_port(Port),
            {os_pid, Pid} <- [safe_port_pid(Port)]
        ] ++
            case SeenPid of
                P when is_integer(P) -> [P];
                _ -> []
            end
    ),
    Snapshots = [{Pid, #{scope => linked_port_or_spawn_notice, exit => unverified}} || Pid <- Pids],
    Record = record_startup_timeout(Startup, Snapshots),
    case Suspended of
        true -> exit(Owner, kill);
        false -> ok
    end,
    Down =
        receive
            {'DOWN', Monitor, process, Owner, _} -> confirmed
        after 2000 -> unconfirmed
        end,
    {error, {garage_start_timeout, Snapshots, Record, Down}}.

safe_port_pid(Port) ->
    try
        erlang:port_info(Port, os_pid)
    catch
        error:badarg -> undefined
    end.

record_startup_timeout(#{dir := Dir} = Startup, Snapshots) ->
    try
        ok = imboy_quality_garage_cleanup:verify(Startup),
        file:write_file(
            filename:join(Dir, "startup-timeout.term"),
            term_to_binary(#{status => exit_unverified, snapshots => Snapshots}),
            [exclusive]
        )
    catch
        _:_ -> {error, ownership_not_verified}
    end.

garage_loop(Port, Fd, Exit) ->
    receive
        {Port, {data, D}} ->
            _ = file:write(Fd, D),
            garage_loop(Port, Fd, Exit);
        {Port, {exit_status, Code}} ->
            garage_loop(Port, Fd, Code);
        {stop, From, Ref} ->
            Code =
                case Exit of
                    undefined ->
                        close_owned_port(Port);
                    C ->
                        C
                end,
            _ = file:close(Fd),
            From ! {Ref, stopped, Code}
    end.

%% Closing the owned port does not prove the external process exited.
close_owned_port(Port) ->
    try
        Self = self(),
        {connected, Self} = erlang:port_info(Port, connected),
        receive
            {Port, {exit_status, Code}} -> Code
        after 0 ->
            true = erlang:port_close(Port),
            {error, process_exit_unverified}
        end
    catch
        _:_ -> {error, process_exit_unverified}
    end.

owned_port_identity(Port, Exe, Cfg) when is_port(Port) ->
    try
        Self = self(),
        {connected, Self} = erlang:port_info(Port, connected),
        {name, PortExe} = erlang:port_info(Port, name),
        {os_pid, Pid} = erlang:port_info(Port, os_pid),
        true = is_integer(Pid) andalso Pid > 0,
        ExeIdentity = file_identity(Exe),
        ExeIdentity = file_identity(PortExe),
        {ok, #{
            pid => Pid,
            executable => ExeIdentity,
            executable_path => Exe,
            config => Cfg,
            observed_at_unix_ms => erlang:system_time(millisecond),
            scope => <<"LOCAL_OTP_PORT_ONLY_NOT_CROSS_SESSION_IDENTITY">>
        }}
    catch
        _:_ -> {error, port_identity_unverified}
    end;
owned_port_identity(_, _, _) ->
    {error, port_identity_unverified}.

file_identity(Path) ->
    {ok, #file_info{type = regular, inode = Inode, major_device = Major, minor_device = Minor}} =
        file:read_file_info(Path),
    true = is_integer(Inode) andalso Inode > 0,
    {Major, Minor, Inode}.

%% 停止守护进程并删除数据目录（keep_dir => true 仅用于失败取证路径）。
stop_garage(#{owner := Owner} = G) ->
    Ref = make_ref(),
    Owner ! {stop, self(), Ref},
    Code =
        receive
            {Ref, stopped, StopCode} -> StopCode
        after 25000 -> stop_timeout
        end,
    case {Code, maps:get(keep_dir, G, false)} of
        {ExitCode, true} when is_integer(ExitCode) ->
            retain_garage_stop(G, ExitCode);
        {ExitCode, false} when is_integer(ExitCode) ->
            Stop = retain_garage_stop(G, ExitCode),
            case cleanup_stopped_garage(G, Stop) of
                ok -> #{exit => ExitCode, cleanup => ok};
                {error, Reason} -> #{exit => {cleanup_failed, Reason}, cleanup => {error, Reason}}
            end;
        _ ->
            #{exit => Code, cleanup => {error, process_exit_unverified}}
    end.

cleanup_stopped_garage(G, Stop) -> imboy_quality_garage_cleanup:cleanup(G, Stop).

retain_garage_stop(#{dir := Dir, dir_identity := Identity, dir_token := Token}, ExitCode) ->
    Receipt = make_ref(),
    put({imboy_quality_garage_cleanup, Receipt}, {Dir, Identity, Token, ExitCode}),
    #{exit => ExitCode, cleanup => retained, receipt => Receipt};
retain_garage_stop(_, ExitCode) ->
    #{exit => ExitCode, cleanup => retained}.

create_garage_dir(Ctrl, Tag) ->
    true = valid_garage_tag(Tag),
    Runtime = filename:join(Ctrl, "runtime"),
    Base = filename:join(Runtime, "garage"),
    {ok, #file_info{type = directory}} = file:read_link_info(Ctrl),
    ok = ensure_directory(Runtime),
    ok = ensure_directory(Base),
    Dir = filename:join(Base, Tag),
    case file:make_dir(Dir) of
        ok -> ok;
        {error, Reason} -> error({garage_directory_create_failed, Dir, Reason})
    end,
    ok = file:change_mode(Dir, 8#700),
    Identity = directory_identity(Dir),
    Token = crypto:strong_rand_bytes(32),
    ok = file:write_file(filename:join(Dir, ".imboy-quality-owner"), Token, [exclusive]),
    {Dir, Identity, Token}.

valid_garage_tag(Tag) when is_list(Tag), Tag =/= [] ->
    lists:all(
        fun(C) ->
            (C >= $0 andalso C =< $9) orelse
                (C >= $a andalso C =< $z) orelse (C >= $A andalso C =< $Z) orelse
                C =:= $- orelse C =:= $_
        end,
        Tag
    );
valid_garage_tag(_) ->
    false.

ensure_directory(Path) ->
    case file:make_dir(Path) of
        ok ->
            ok;
        {error, eexist} ->
            {ok, #file_info{type = directory}} = file:read_link_info(Path),
            ok;
        {error, Reason} ->
            error({garage_parent_directory_failed, Path, Reason})
    end.

directory_identity(Path) ->
    {ok, #file_info{type = directory, inode = Inode, major_device = Major, minor_device = Minor}} =
        file:read_link_info(Path),
    true = is_integer(Inode) andalso Inode > 0,
    {Major, Minor, Inode}.

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

%% ---- TextGuard 服务二进制（pinned Rust 源构建，scripts/ci/native-eunit-bootstrap.sh:92） ----

%% 与 hird_bin/1 同构：IMBOY_QUALITY_TEXTGUARD_BIN > TEXTGUARD_TEST_BINARY >
%% 工作区祖先目录的 imboy-textguard/target/release/imboy-textguard > PATH 上的
%% imboy-textguard。只返回存在的常规文件；缺失 fail-closed（textguard 三根 setup
%% 的 service owner 必须真实启动本机服务，不以占位二进制假绿）。
textguard_bin(Repo) ->
    Env = [os:getenv(K) || K <- ["IMBOY_QUALITY_TEXTGUARD_BIN", "TEXTGUARD_TEST_BINARY"]],
    Ancestors = [
        filename:join([A, "imboy-textguard", "target", "release", "imboy-textguard"])
     || A <- ancestors(Repo, 8)
    ],
    Path = [os:find_executable("imboy-textguard")],
    Cands = [C || C <- Env ++ Ancestors ++ Path, is_list(C), filelib:is_regular(C)],
    case Cands of
        [C | _] -> {ok, C};
        [] -> {error, textguard_binary_missing}
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

%% no-backend-ffmpeg V1：固定合法 ADTS 字节（16kHz 单声道 39 字节帧 ×64
%% = 1024ms，与 talk_record_audio_validate 同构），替换旧 ffmpeg 动态
%% 生成——无 FFmpeg 环境仍产出真实可解码夹具。
ensure_aac_fixture(Ctrl) ->
    Path = filename:join([Ctrl, "runtime", "fixtures", "synthetic_440hz_1s.aac"]),
    case filelib:file_size(Path) > 0 of
        true ->
            {ok, Path};
        false ->
            ok = filelib:ensure_dir(Path),
            ok = file:write_file(Path, aac_fixture_bytes(), [write, binary, exclusive]),
            {ok, Path}
    end.

aac_fixture_bytes() ->
    Length = 39,
    B3 = 16#40 bor (Length bsr 11),
    B4 = (Length bsr 3) band 16#FF,
    B5 = (Length band 7) bsl 5,
    Frame = <<16#FF, 16#F1, 16#60, B3, B4, B5, 16#00, 0:256>>,
    iolist_to_binary(lists:duplicate(64, Frame)).

%% ---- 清单与配置文件 ----

write_json(Path, Map) ->
    ok = filelib:ensure_dir(Path),
    {ok, F} = file:open(Path, [write, binary, raw, exclusive]),
    try
        ok = file:change_mode(Path, 8#600),
        ok = file:write(F, iolist_to_binary(json:encode(Map))),
        file:sync(F)
    after
        file:close(F)
    end.

begin_config_receipts(Ctrl, RunId) -> imboy_quality_config_receipts:begin_run(Ctrl, RunId).
cleanup_config_receipts() -> imboy_quality_config_receipts:cleanup().

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
    ok = imboy_quality_config_receipts:write(Path ++ ".config", Out),
    Path.

%% 多个独立控制器并行诊断时各自 worker 0 不能同绑 19980/19970；
%% IMBOY_QUALITY_PORT_BASE 调 http 基址，adm 基址恒为其 -10（默认 19980/19970 不变）。
port_base() ->
    case os:getenv("IMBOY_QUALITY_PORT_BASE") of
        false ->
            19980;
        "" ->
            19980;
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
