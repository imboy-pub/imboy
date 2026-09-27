-module(inttest_marker_db).

%% 真库集成测试的一次性 marker 库供给助手（test/common）。
%%
%% 由 ORG-BACKEND-TESTFIX-B（organization_member_repo_integration_tests，
%% 提交 3e9a011c）验证过的配方泛化而来：
%%   * 目标服务器解析优先级：环境变量 <PREFIX>_PG_HOST/_PG_PORT/_PG_USER/
%%     _PG_PASSWORD(/_MAINT_DB) 逐项覆盖 eunit VM 配置（-config 装载的
%%     application:get_env(imboy, pg_conf) 的 epgsql connect 参数）。
%%     源码不内置任何主机/端口/库名/口令。
%%   * 每次运行建一次性 marker 库 inttest_<prefix>_<微秒>_<rand>：
%%     建库 → 预置 clean-deploy 前置的 12 个 PG 扩展 → erlang_migrate:up
%%     全链（strict，与生产/演练同口径）→ 调用方自持事务（BEGIN/ROLLBACK
%%     或自带 SAVEPOINT 方案）→ release/1 时 DROP DATABASE。
%%   * 无 skip 语义：环境/配置/建库/扩展/迁移任一失败都显式 error（带原因
%%     与 env 变量名），调用方不得 catch 成 skip。
%%   * 两个实操注意（TESTFIX-D 六套件实证）：① eunit 单测默认 5 秒超时会
%%     掐死全链迁移（130 迁移约 10-15 秒）——用例必须外包 {timeout, N≥900}；
%%     ② provision 中途进程被杀（超时杀/VM 崩溃）时 catch 清理不会执行，
%%     marker 库会残留并仅打 WARNING——调用方保证 {timeout,...} 防杀，
%%     残留库可按名称（inttest_ 前缀）人工 DROP。
%%
%% 用法（测试套件内）：
%%   setup_conn() ->
%%       inttest_marker_db:provision(
%%           #{env_prefix => <<"MOYA_INTTEST">>,
%%             connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}}).
%%   close_conn(State) -> inttest_marker_db:release(State).
%%   %% State 内 conn 字段为已连接 epgsql pid；测试体自行 BEGIN/ROLLBACK。

-export([provision/1, release/1, safe_connect/1]).

-define(EXTENSIONS, [
    <<"pgcrypto">>,
    <<"pg_jieba">>,
    <<"timescaledb">>,
    <<"vector">>,
    <<"postgis">>,
    <<"pg_trgm">>,
    <<"btree_gin">>,
    <<"btree_gist">>,
    <<"citext">>,
    <<"unaccent">>,
    <<"intarray">>,
    <<"pg_stat_statements">>
]).

-spec provision(map()) ->
    #{conn := pid(), maint_conn := pid(), db := binary(), server := map()}.
provision(Opts) when is_map(Opts) ->
    case maps:get(reuse, Opts, false) of
        true -> provision_shared(Opts);
        false -> provision_fresh(Opts)
    end.

provision_fresh(Opts) when is_map(Opts) ->
    Prefix = maps:get(env_prefix, Opts, <<"INTTEST">>),
    ConnectExtra = maps:get(connect_extra, Opts, #{}),
    {ok, _} = application:ensure_all_started(epgsql),
    _ = code:add_patha("deps/erlang_migrate/ebin"),
    Server = resolve_server(Prefix, ConnectExtra),
    MaintConn = connect_server(Server, maint_db(Prefix)),
    DbName = marker_db_name(Prefix),
    try
        ok = create_db(MaintConn, DbName),
        ok = ensure_extensions(Server, DbName),
        ok = migrate_result(migrate_fresh_db(Server, DbName)),
        %% A1c：marker 库自身连接同样走重试（原先裸 safe_connect 一次失败即
        %% 整个 provision 报废），与 connect_server 同预算。
        Conn = connect_retry(Server, DbName),
        #{conn => Conn, maint_conn => MaintConn, db => DbName, server => Server}
    catch
        Class:Reason:Stack ->
            close_quiet(MaintConn),
            try_drop_db(Server, DbName),
            erlang:raise(Class, {marker_db_provision_failed, DbName, Reason}, Stack)
    end.

-spec release(map()) -> ok.
release(#{reuse_key := Key}) ->
    release_shared(Key);
release(#{conn := Conn, maint_conn := MaintConn, db := DbName, server := Server}) ->
    release_fresh(#{conn => Conn, maint_conn => MaintConn, db => DbName, server => Server}).

release_fresh(#{conn := Conn, maint_conn := MaintConn, db := DbName, server := Server}) ->
    close_quiet(Conn),
    close_quiet(MaintConn),
    try_drop_db(Server, DbName),
    ok.

%% ===================================================================
%% VM 级 marker 库复用（A1c，provision/1 传 reuse => true 时启用）
%%
%% 背景（evidence/CP-TD-A02 full_run1/run2 实证）：全量 eunit 长跑中，
%% 本 VM 对 127.0.0.1:<port> 的新建 TCP 连接会被 macOS Docker 端口转发层
%% 持续 econnrefused（同刻 pg_isready / 全新 erl VM 直连均成功、容器与
%% postmaster 全程健康——按进程拒绝，非全局窗口）。moya_ai_worker_tests
%% 每用例独立 provision（18 次/全量），一次被拒即整组 cancel。
%%
%% 复用语义：同 env_prefix 的 provision 共享同一 marker 库与同一条连接
%% （引用计数），最后一个 release 负责关连接 + DROP DATABASE。仅适用于
%% 「调用方自持事务（BEGIN/ROLLBACK）、用例串行」的套件（marker-db 配方
%% 的标准约定）。setup 自身失败时 eunit 不会跑 cleanup——引用计数可能
%% 泄漏导致库残留，与模块头「残留库按 inttest_ 前缀人工 DROP」同口径。
%% 首次 provision 失败缓存 60s（TTL 内后续 acquire 秒败不重复烧重试预算）。
%% ===================================================================

-define(REUSE_SRV, inttest_marker_db_reuse).
-define(REUSE_FAIL_TTL_MS, 60000).

provision_shared(Opts) ->
    Key = reuse_key(Opts),
    Srv = ensure_reuse_server(),
    Srv ! {self(), reuse_acquire, Key, Opts},
    receive
        {reuse_acquired, Srv, {ok, State}} -> State#{reuse_key => Key};
        {reuse_acquired, Srv, {error, Class, Reason, Stack}} ->
            erlang:raise(Class, Reason, Stack)
    after 2400000 ->
        erlang:error({marker_db_reuse_acquire_timeout, Key})
    end.

release_shared(Key) ->
    case whereis(?REUSE_SRV) of
        undefined -> ok;
        Srv ->
            Srv ! {self(), reuse_release, Key},
            receive {reuse_released, Srv, ok} -> ok after 120000 -> ok end
    end.

reuse_key(Opts) ->
    {reuse, maps:get(env_prefix, Opts, <<"INTTEST">>)}.

ensure_reuse_server() ->
    case whereis(?REUSE_SRV) of
        undefined ->
            Pid = spawn(fun reuse_loop/0),
            try erlang:register(?REUSE_SRV, Pid) of
                true -> Pid
            catch _:_ ->
                exit(Pid, kill),
                whereis(?REUSE_SRV)
            end;
        Pid ->
            Pid
    end.

reuse_loop() ->
    receive
        {From, reuse_acquire, Key, Opts} ->
            case get({reuse_state, Key}) of
                undefined ->
                    Now = os:system_time(millisecond),
                    case get({reuse_fail, Key}) of
                        {FailAt, Class, Reason, Stack} when Now - FailAt < ?REUSE_FAIL_TTL_MS ->
                            From ! {reuse_acquired, self(), {error, Class, Reason, Stack}};
                        _ ->
                            try provision_fresh(Opts) of
                                State ->
                                    put({reuse_state, Key}, State),
                                    put({reuse_count, Key}, 1),
                                    erase({reuse_fail, Key}),
                                    From ! {reuse_acquired, self(), {ok, State}}
                            catch C:R:S ->
                                put({reuse_fail, Key},
                                    {os:system_time(millisecond), C, R, S}),
                                From ! {reuse_acquired, self(), {error, C, R, S}}
                            end
                    end;
                State ->
                    put({reuse_count, Key}, get({reuse_count, Key}) + 1),
                    From ! {reuse_acquired, self(), {ok, State}}
            end;
        {From, reuse_release, Key} ->
            case get({reuse_count, Key}) of
                1 ->
                    try
                        release_fresh(get({reuse_state, Key}))
                    catch _C:_R:_S ->
                        %% 释放失败不阻断计数清理；库残留走 WARNING 口径
                        ok
                    end,
                    erase({reuse_state, Key}),
                    erase({reuse_count, Key}),
                    From ! {reuse_released, self(), ok};
                N when is_integer(N), N > 1 ->
                    put({reuse_count, Key}, N - 1),
                    From ! {reuse_released, self(), ok};
                _ ->
                    From ! {reuse_released, self(), ok}
            end
    end,
    reuse_loop().

%% ------------------------------------------------------------------
%% Internal
%% ------------------------------------------------------------------

resolve_server(Prefix, ConnectExtra) ->
    Defaults = config_pg_conn(),
    User = env_or(<<Prefix/binary, "_PG_USER">>, maps:get(username, Defaults, undefined)),
    Pass = env_or(<<Prefix/binary, "_PG_PASSWORD">>, maps:get(password, Defaults, undefined)),
    case {User, Pass} of
        {undefined, _} ->
            erlang:error({marker_db_missing_pg_user, env_name(Prefix, "PG_USER")});
        {_, undefined} ->
            erlang:error({marker_db_missing_pg_password, env_name(Prefix, "PG_PASSWORD")});
        _ ->
            ok
    end,
    #{
        host => env_or(<<Prefix/binary, "_PG_HOST">>, maps:get(host, Defaults, "127.0.0.1")),
        port => normalize_port(
            env_or(<<Prefix/binary, "_PG_PORT">>, maps:get(port, Defaults, 5432))
        ),
        username => User,
        password => Pass,
        maint_db => maint_db(Prefix),
        connect_extra => ConnectExtra
    }.

env_name(Prefix, Suffix) -> <<Prefix/binary, "_", (list_to_binary(Suffix))/binary>>.

config_pg_conn() ->
    %% -config 注入的应用环境在应用 load 时才合并；测试进程先 load（零进程副作用）。
    case application:load(imboy) of
        ok -> ok;
        {error, {already_loaded, imboy}} -> ok;
        {error, LoadReason} -> erlang:error({marker_db_app_load_failed, LoadReason})
    end,
    case application:get_env(imboy, pg_conf) of
        {ok, #{start_mfa := {epgsql, connect, [Opts]}}} when is_map(Opts) -> Opts;
        {ok, Other} -> erlang:error({marker_db_pg_conf_shape, Other});
        undefined -> erlang:error({marker_db_pg_conf_missing, 'imboy.pg_conf'})
    end.

env_or(Key, Default) when is_binary(Key) ->
    case os:getenv(binary_to_list(Key)) of
        false -> Default;
        "" -> Default;
        Value -> Value
    end.

normalize_port(Port) when is_integer(Port) -> Port;
normalize_port(Str) when is_list(Str) -> list_to_integer(Str).

maint_db(Prefix) ->
    case env_or(<<Prefix/binary, "_MAINT_DB">>, undefined) of
        undefined -> <<"postgres">>;
        Name -> list_to_binary(Name)
    end.

marker_db_name(Prefix) ->
    Stem = re:replace(
        string:lowercase(binary_to_list(Prefix)),
        "[^a-z0-9]+",
        "_",
        [global, {return, binary}]
    ),
    Ts = os:system_time(microsecond),
    Rand = rand:uniform(16#FFFFFFFF),
    <<"inttest_", Stem/binary, "_", (integer_to_binary(Ts))/binary, "_",
        (integer_to_binary(Rand))/binary>>.

conn_opts(#{host := Host, port := Port, username := User, password := Pass} = Server, Db) ->
    %% 调用方附加连接参数（如 timestamptz rfc3339 binary codec）最后合并，
    %% 仅用于 codec/超时类微调，不得改变连接目标语义。
    maps:merge(
        #{
            host => Host,
            port => Port,
            username => User,
            password => Pass,
            database => Db,
            timeout => 10000
        },
        maps:get(connect_extra, Server, #{})
    ).

connect_server(Server, Db) ->
    %% 整树全量下 Docker 端口转发在高 I/O（每次供给一轮全链迁移触发重
    %% checkpoint）时会瞬态 econnrefused（容器内 postmaster 存活）——
    %% 对连接做退避重试；持续失败仍显式 error，不弱化 no-skip 语义。
    %% A1c：重试预算可经 env 调大（gate 场景需穿过 macOS Docker Desktop
    %% com.docker.backend 端口转发 1-2 分钟瞬态窗口）。默认 4 次 × 3s 间隔，
    %% 总等待 ~9s，与原 1.5+3+4.5s 梯度预算等价——默认行为不变。
    connect_retry(Server, Db).

connect_retry(Server, Db) ->
    Attempts = env_int("INTTEST_CONNECT_RETRIES", 4),
    SleepMs = env_int("INTTEST_CONNECT_SLEEP_MS", 3000),
    CapMs = env_int("INTTEST_CONNECT_BACKOFF_CAP_MS", 15000),
    case {Attempts, SleepMs, CapMs} of
        {4, 3000, 15000} -> ok;
        _ -> io:format("~n[inttest_marker_db] retry budget override: ~p attempts x ~pms (cap ~pms)~n",
                       [Attempts, SleepMs, CapMs])
    end,
    connect_retry(Server, Db, Attempts, SleepMs, CapMs, none, 0).

%% A1c：LastReason 取证——预算耗尽时把最后一次连接失败原因带进 error，
%% 供 eunit 日志区分「端口转发瞬态拒绝(econnrefused)」与其它原因。
%% 退避：base × 2^k，封顶可调（evidence/CP-TD-A02 full_run1/2/3 实证：
%% 拒绝窗口可达 30min+ 且重连越勤越久——gate 用长静默间隔（45-90s/次）
%% 桥接；默认 4 次 3s 起步 15s 封顶（36s 总预算），默认行为只增不减。
connect_retry(_Server, _Db, 0, _SleepMs, _CapMs, LastReason, _Idx) ->
    erlang:error({marker_db_connect_failed, retry_exhausted, last_reason, LastReason});
connect_retry(Server, Db, Attempts, SleepMs, CapMs, _LastReason, Idx) ->
    Opts = conn_opts(Server, Db),
    case safe_connect(Opts) of
        {ok, Conn} ->
            Conn;
        {error, Reason} when
            Reason =:= econnrefused; Reason =:= etimedout; Reason =:= ehosunreach
        ->
            timer:sleep(min(SleepMs * (1 bsl min(Idx, 4)), CapMs)),
            connect_retry(Server, Db, Attempts - 1, SleepMs, CapMs, Reason, Idx + 1);
        {error, Reason} ->
            erlang:error({marker_db_connect_failed, Db, Reason})
    end.

env_int(Name, Default) ->
    case os:getenv(Name) of
        false -> Default;
        "" -> Default;
        Str ->
            case string:to_integer(string:trim(Str)) of
                {N, []} when N >= 0 -> N;
                _ -> Default
            end
    end.

%% ===================================================================
%% epgsql 连接的 EXIT 信号隔离（CP-TD-A02）
%%
%% epgsql:connect/1,4 内部 epgsql_sock:start_link/0（gen_server:start_link）
%% 把连接进程 link 到调用者；连接失败（如全量高并发下瞬态 econnrefused，
%% 见 connect_server/2 注释）时 sock 进程以裸原因（econnrefused 等）stop：
%% 调用者即使正常拿到 {error, Reason} 返回值，仍会被紧随其后的
%% {'EXIT', Sock, econnrefused} 信号杀死（非 trap_exit 进程收到非 normal
%% exit 信号即死）。在 eunit 全量里所有用例内联运行在 eunit Runner 进程，
%% 该信号杀掉 Runner → 顶层组 cancel（"*unexpected termination of test
%% process* ::econnrefused"），剩余用例全部取消、make exit 2——且失败点
%% 跨套件随机漂移（取决于哪个套件恰好在做 DB 连接）。
%%
%% safe_connect/1 把 epgsql:connect 放进一次性 worker 进程执行：sock 永远
%% 只 link 到 worker（worker 自身 trap_exit 并吸收 sock 的 EXIT 后才向
%% 调用者回传结果），调用者从头到尾与 sock 零 link——连接失败确定性还原
%% 为普通 {error, Reason}，不存在"信号迟到补刀"的竞态窗口。成功路径下
%% 连接进程在 worker 退出时收到的是 normal exit 信号（gen_server 不 trap
%% 时对 normal 信号免疫），连接存活、由调用方 close，语义与直连一致。
%% ===================================================================
-spec safe_connect(map()) -> {ok, epgsql:connection()} | {error, term()}.
safe_connect(Opts) ->
    Parent = self(),
    Worker =
        spawn(fun() ->
            process_flag(trap_exit, true),
            Result =
                try epgsql:connect(Opts) of
                    {ok, _Conn} = Ok ->
                        Ok;
                    {error, _Reason} = Err ->
                        %% sock 已 stop；trap 下其 EXIT 已成消息，吸收之
                        flush_sock_exit(),
                        Err
                catch
                    Class:R:S ->
                        flush_sock_exit(),
                        {safe_connect_raise, Class, R, S}
                end,
            Parent ! {safe_connect_result, self(), Result}
        end),
    receive
        {safe_connect_result, Worker, {safe_connect_raise, Class, R, S}} ->
            erlang:raise(Class, R, S);
        {safe_connect_result, Worker, Result} ->
            Result
    after 120000 ->
        %% epgsql connect 自带 timeout；此处仅兜底防 worker 意外挂死
        erlang:error({safe_connect_worker_timeout, maps:get(host, Opts, undefined)})
    end.

flush_sock_exit() ->
    receive
        {'EXIT', _Pid, _Why} ->
            ok
    after 5000 ->
        %% 信号序保证必达；超时仅防极端调度下的挂死
        ok
    end.

create_db(Conn, DbName) ->
    case epgsql:squery(Conn, <<"CREATE DATABASE ", DbName/binary>>) of
        {ok, _, _} -> ok;
        {error, Reason} -> erlang:error({marker_db_create_failed, DbName, Reason})
    end.

ensure_extensions(Server, DbName) ->
    Conn = connect_server(Server, DbName),
    try
        lists:foreach(
            fun(Ext) ->
                Sql = <<"CREATE EXTENSION IF NOT EXISTS ", Ext/binary>>,
                case epgsql:squery(Conn, Sql) of
                    {ok, _, _} -> ok;
                    {error, Reason} -> erlang:error({marker_db_extension_failed, Ext, Reason})
                end
            end,
            ?EXTENSIONS
        )
    after
        close_quiet(Conn)
    end.

migrate_fresh_db(Server, DbName) ->
    MigConn = connect_server(Server, DbName),
    try
        erlang_migrate:up(#{conn => MigConn, dir => "priv/migrations", strict => true})
    after
        close_quiet(MigConn)
    end.

migrate_result(ok) -> ok;
migrate_result({ok, _Applied}) -> ok;
migrate_result({error, Reason}) -> erlang:error({marker_db_migrate_failed, Reason}).

try_drop_db(Server, DbName) ->
    case connect_maint_quiet(Server) of
        {ok, C} ->
            Drop =
                try epgsql:squery(C, <<"DROP DATABASE ", DbName/binary>>) of
                    {ok, _, _} -> ok;
                    Other -> {left_behind, Other}
                catch
                    _:_ -> {left_behind, drop_crashed}
                end,
            case Drop of
                ok ->
                    ok;
                {left_behind, Why} ->
                    io:format("~nWARNING: marker db not dropped (~0p): ~ts~n", [Why, DbName])
            end,
            close_quiet(C);
        error ->
            io:format("~nWARNING: maint reconnect failed, marker db ~ts left behind~n", [DbName])
    end.

connect_maint_quiet(Server) ->
    try
        {ok, _} = safe_connect(conn_opts(Server, maps:get(maint_db, Server, <<"postgres">>)))
    catch
        _:_ -> error
    end.

close_quiet(Conn) ->
    try
        epgsql:close(Conn)
    catch
        _:_ -> ok
    end.
