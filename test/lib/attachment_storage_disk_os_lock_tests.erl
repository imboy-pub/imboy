-module(attachment_storage_disk_os_lock_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

kernel_lock_test_() -> {timeout, 30, fun kernel_lock/0}.
kernel_lock() ->
    %% NIF 自足：attachment_disk_lock_nif 的 -on_load 只在模块首次加载时执行一次，
    %% 彼时若 imboy app 尚未 load 则 code:priv_dir 失败、NIF 加载失败且无人重试——
    %% 测试绿否取决于同 VM 是否先跑过 boot app 的根（隐式分片顺序依赖）。显式
    %% 确保加载前提：load app（幂等）后确保 NIF 模块已加载；仍失败则硬错（不 skip）。
    %% 必须用 ensure_loaded 而非 load_file：C 库未定义 upgrade 回调，模块已被同
    %% VM 先前根（inventory 的同名 src 冒烟根经 EUnit 隐式自动加载）加载时，
    %% load_file 强制重载走 upgrade 路径必失败 {error,{upgrade,_}} → on_load_failure
    %% （full-20261030T225326Z group_2 全量红根因，单跑 fresh VM 无此依赖）；
    %% ensure_loaded 对已加载模块零重载（on_load 不重跑），首次加载路径零变化。
    case application:load(imboy) of
        ok -> ok;
        {error, {already_loaded, imboy}} -> ok
    end,
    {module, attachment_disk_lock_nif} = code:ensure_loaded(attachment_disk_lock_nif),
    Root = filename:join(
        temp_dir(),
        "attv5-oslock-" ++
            binary_to_list(binary:encode_hex(crypto:strong_rand_bytes(12), lowercase))
    ),
    ok = file:make_dir(Root),
    Prior = application:get_env(imboy, attachment_storage_registry),
    Registry = #{
        version => 1,
        roots => #{
            <<"root">> => #{
                driver => <<"disk">>,
                namespace => <<"n">>,
                aliases => [<<"alias">>],
                endpoint => list_to_binary("file://" ++ Root),
                container => <<"c">>
            }
        }
    },
    try
        ok = file:change_mode(Root, 8#700),
        application:set_env(imboy, attachment_storage_registry, Registry),
        ok = attachment_storage_disk:put(<<"n">>, <<"c">>, <<"owned">>, <<"before">>),
        {ok, Identity0} = attachment_storage:identity(<<"n">>, <<"c">>, <<"owned">>),
        Identity = Identity0#{endpoint => list_to_binary("file://" ++ Root)},
        {ok, Before} = attachment_storage_disk:head_snapshot(Identity),
        ok = attachment_storage_disk:put(<<"n">>, <<"c">>, <<"owned">>, <<"BEFORE">>),
        %% Preserve old stat evidence: replacement must still be detected.
        %% 固定旧大小和 mtime，仍须识别真实原子替换。
        ok = file:write_file_info(
            filename:join([Root, "c", "owned"]),
            #file_info{mtime = maps:get(mtime, Before)},
            [{time, posix}]
        ),
        {ok, After} = attachment_storage_disk:head_snapshot(Identity),
        ?assertEqual(
            maps:with([size, mtime, etag], Before),
            maps:with([size, mtime, etag], After)
        ),
        ?assertNotEqual(Before, After),
        ok = attachment_storage_disk:put(<<"n">>, <<"c">>, <<"owned">>, <<"before">>),
        Holder = start_holder(Root),
        try
            Start = erlang:monotonic_time(millisecond),
            ?assertMatch(
                {error, _},
                attachment_storage_disk:put(
                    <<"alias">>, <<"c">>, <<"owned">>, <<"timeout-must-not-write">>
                )
            ),
            Elapsed = erlang:monotonic_time(millisecond) - Start,
            ?assert(Elapsed >= 4000 andalso Elapsed < 8000),
            ?assertEqual(
                {ok, <<"before">>}, attachment_storage_disk:read(<<"n">>, <<"c">>, <<"owned">>)
            )
        after
            kill_holder(Holder)
        end,
        %% 被 SIGKILL 的 BEAM 关闭子进程 stdin，内核锁无需过期或删锁文件即释放。
        timer:sleep(100),
        ok = attachment_storage_disk:put(<<"alias">>, <<"c">>, <<"owned">>, <<"after-crash">>),
        ?assertEqual(
            {ok, <<"after-crash">>}, attachment_storage_disk:read(<<"n">>, <<"c">>, <<"owned">>)
        ),
        locked_symlink_race(Root),
        ok = attachment_storage_disk:delete(<<"n">>, <<"c">>, <<"owned">>),
        ?assertEqual(
            {error, not_found}, attachment_storage_disk:read(<<"n">>, <<"c">>, <<"owned">>)
        ),
        ok = file:make_symlink("/tmp", filename:join([Root, "c", "escape"])),
        ?assertEqual(
            {error, path_escape},
            attachment_storage_disk:put(<<"n">>, <<"c">>, <<"escape/not-owned">>, <<"never">>)
        )
    after
        case Prior of
            {ok, V} -> application:set_env(imboy, attachment_storage_registry, V);
            undefined -> application:unset_env(imboy, attachment_storage_registry)
        end,
        %% del_dir_r removes symlinks themselves, never their outside targets.
        ok = file:del_dir_r(Root)
    end.
locked_symlink_race(Root) ->
    Holder = start_holder(Root),
    Parent = self(),
    Ref = make_ref(),
    {Worker, Monitor} = spawn_monitor(fun() ->
        receive
            start ->
                Parent !
                    {Ref, write_result,
                        attachment_storage_disk:put(
                            <<"n">>, <<"c">>, <<"owned">>, <<"must-not-escape">>
                        )}
        end
    end),
    Dir = filename:join(Root, "c"),
    Old = Dir ++ "-original",
    Outside = filename:join(Root, "outside-owned"),
    try
        %% d4bef2e9e 重构删除了 hold_lock/3 且未同步本测试；Holder 先持锁时
        %% worker 的 put 阻塞在等锁循环——屏障语义 = "writer 已进入等锁窗口"，
        %% 锚 wait_lock/4（with_os_lock → acquire eagain → wait_lock）。
        erlang:trace_pattern({attachment_storage_disk, wait_lock, 4}, true, [local]),
        erlang:trace(Worker, true, [call, {tracer, Parent}]),
        Worker ! start,
        receive
            {trace, Worker, call, {attachment_storage_disk, wait_lock, _}} -> ok
        after 2000 -> erlang:error(lock_barrier_missing)
        end,
        ok = file:make_dir(Outside),
        ok = file:change_mode(Outside, 8#700),
        ok = file:rename(Dir, Old),
        ok = file:make_symlink(Outside, Dir),
        kill_holder(Holder),
        receive
            {Ref, write_result, Result} -> ?assertEqual({error, path_escape}, Result)
        after 7000 -> erlang:error(race_worker_timeout)
        end,
        ?assertNot(filelib:is_file(filename:join(Outside, "owned"))),
        ok = file:delete(Dir),
        ok = file:rename(Old, Dir)
    after
        %% A failed barrier/assertion must not leave a writer or lock owner alive.
        try
            stop_worker(Worker, Monitor)
        after
            try
                kill_holder(Holder)
            after
                erlang:trace_pattern({attachment_storage_disk, wait_lock, 4}, false, [local])
            end
        end
    end.

temp_dir() ->
    %% Linux has /tmp; macOS aliases it to /private/tmp. Never seed a symlink root.
    case file:read_link_all("/tmp") of
        {ok, Target} -> filename:absname(Target, "/");
        {error, einval} -> "/tmp"
    end.

stop_worker(Worker, Monitor) ->
    exit(Worker, kill),
    receive
        {'DOWN', Monitor, process, Worker, _} -> ok
    after 2000 -> erlang:error(race_worker_not_dead)
    end.

start_holder(Root) ->
    %% 测试子 VM 单独加载 export_all binary，以便在真实内核锁内确定性暂停；不修改生产 beam。
    %% 子 VM 无 app boot，code:priv_dir(imboy) 恒 bad_name → NIF 的 -on_load 需
    %% init/0 的 beam 位置推导 fallback（attachment_disk_lock_nif 侧修复）才能挂上；
    %% erlang:load_nif 不能从模块外调用（bad_lib），故此处不做额外动作。
    Source = filename:absname("src/lib/attachment_storage/attachment_storage_disk.erl"),
    Eval = lists:flatten(
        io_lib:format(
            "application:ensure_all_started(crypto), {ok,M,B}=compile:file(~p,[binary,export_all,nowarn_export_all]), {module,M}=code:load_binary(M,~p,B), attachment_storage_disk:with_os_lock(~p,<<\"c\">>,<<\"owned\">>,fun(_Lock)->io:format(\"LOCK_HELD~n\"),receive after infinity->ok end end),halt().",
            [Source, Source, Root]
        )
    ),
    Port = open_port({spawn_executable, os:find_executable("erl")}, [
        binary,
        exit_status,
        use_stdio,
        stderr_to_stdout,
        {line, 1024},
        {args, ["+S", "1:1", "-noshell", "-pa", filename:absname("ebin"), "-eval", Eval]}
    ]),
    try
        receive
            {Port, {data, {eol, <<"LOCK_HELD">>}}} -> Port;
            {Port, {data, Other}} -> erlang:error({holder_failed, Other});
            {Port, {exit_status, Status}} -> erlang:error({holder_exited, Status})
        after 10000 -> erlang:error(holder_timeout)
        end
    catch
        Class:Reason:Stack ->
            kill_holder(Port),
            erlang:raise(Class, Reason, Stack)
    end.
kill_holder(Port) ->
    case erlang:port_info(Port, os_pid) of
        undefined -> ok;
        {os_pid, Pid} -> kill_holder(Port, Pid)
    end.

kill_holder(Port, Pid) ->
    Kill = open_port({spawn_executable, "/bin/kill"}, [
        exit_status, {args, ["-KILL", integer_to_list(Pid)]}
    ]),
    receive
        {Kill, {exit_status, 0}} -> ok
    after 2000 -> erlang:error(kill_timeout)
    end,
    receive
        {Port, {exit_status, _}} -> ok
    after 2000 -> erlang:error(holder_not_dead)
    end.
