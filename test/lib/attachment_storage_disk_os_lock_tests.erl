-module(attachment_storage_disk_os_lock_tests).
-include_lib("eunit/include/eunit.hrl").

kernel_lock_test_() -> {timeout,30,fun kernel_lock/0}.
kernel_lock() ->
    Root="/private/tmp/attv5-oslock-"++binary_to_list(binary:encode_hex(crypto:strong_rand_bytes(12),lowercase)),
    Prior=application:get_env(imboy,attachment_storage_registry),
    Registry=#{version=>1,roots=>#{<<"root">>=>#{driver=><<"disk">>,namespace=><<"n">>,aliases=>[<<"alias">>],endpoint=>list_to_binary("file://"++Root),container=><<"c">>}}},
    application:set_env(imboy,attachment_storage_registry,Registry),
    try
        ok=attachment_storage_disk:put(<<"n">>,<<"c">>,<<"owned">>,<<"before">>),
        Holder=start_holder(Root),
        try
            Start=erlang:monotonic_time(millisecond),
            ?assertMatch({error,_},attachment_storage_disk:put(<<"alias">>,<<"c">>,<<"owned">>,<<"timeout-must-not-write">>)),
            Elapsed=erlang:monotonic_time(millisecond)-Start,
            ?assert(Elapsed>=4000 andalso Elapsed<8000),
            ?assertEqual({ok,<<"before">>},attachment_storage_disk:read(<<"n">>,<<"c">>,<<"owned">>))
        after kill_holder(Holder) end,
        %% 被 SIGKILL 的 BEAM 关闭子进程 stdin，内核锁无需过期或删锁文件即释放。
        timer:sleep(100),
        ok=attachment_storage_disk:put(<<"alias">>,<<"c">>,<<"owned">>,<<"after-crash">>),
        ?assertEqual({ok,<<"after-crash">>},attachment_storage_disk:read(<<"n">>,<<"c">>,<<"owned">>)),
        locked_symlink_race(Root),
        ok=attachment_storage_disk:delete(<<"n">>,<<"c">>,<<"owned">>),
        ?assertEqual({error,not_found},attachment_storage_disk:read(<<"n">>,<<"c">>,<<"owned">>)),
        ok=file:make_symlink("/private/tmp",filename:join([Root,"c","escape"])),
        ?assertEqual({error,path_escape},attachment_storage_disk:put(<<"n">>,<<"c">>,<<"escape/not-owned">>,<<"never">>))
    after
        case Prior of {ok,V}->application:set_env(imboy,attachment_storage_registry,V);undefined->application:unset_env(imboy,attachment_storage_registry) end
    end.
locked_symlink_race(Root) ->
    Holder=start_holder(Root),Parent=self(),
    Worker=spawn(fun()->receive start->Parent!{write_result,attachment_storage_disk:put(<<"n">>,<<"c">>,<<"owned">>,<<"must-not-escape">>)} end end),
    erlang:trace_pattern({attachment_storage_disk,hold_lock,3},true,[local]),
    erlang:trace(Worker,true,[call,{tracer,Parent}]),Worker!start,
    Dir=filename:join(Root,"c"),Old=Dir++"-original",Outside=Root++"-outside-owned",
    try
        receive {trace,Worker,call,{attachment_storage_disk,hold_lock,_}}->ok after 2000->erlang:error(lock_barrier_missing) end,
        ok=file:make_dir(Outside),ok=file:rename(Dir,Old),ok=file:make_symlink(Outside,Dir),
        kill_holder(Holder),
        receive {write_result,Result}->?assertEqual({error,path_escape},Result) after 7000->erlang:error(race_worker_timeout) end,
        ?assertNot(filelib:is_file(filename:join(Outside,"owned"))),
        ok=file:delete(Dir),ok=file:rename(Old,Dir)
    after erlang:trace_pattern({attachment_storage_disk,hold_lock,3},false,[local]) end.

start_holder(Root) ->
    %% 测试子 VM 单独加载 export_all binary，以便在真实内核锁内确定性暂停；不修改生产 beam。
    Source=filename:absname("src/lib/attachment_storage/attachment_storage_disk.erl"),
    Eval=lists:flatten(io_lib:format("application:ensure_all_started(crypto), {ok,M,B}=compile:file(~p,[binary,export_all,nowarn_export_all]), {module,M}=code:load_binary(M,~p,B), attachment_storage_disk:with_os_lock(~p,<<\"c\">>,<<\"owned\">>,fun()->io:format(\"LOCK_HELD~n\"),receive after infinity->ok end end),halt().",[Source,Source,Root])),
    Port=open_port({spawn_executable,os:find_executable("erl")},[binary,exit_status,use_stdio,stderr_to_stdout,{line,1024},{args,["+S","1:1","-noshell","-pa",filename:absname("ebin"),"-eval",Eval]}]),
    receive {Port,{data,{eol,<<"LOCK_HELD">>}}}->Port;
        {Port,{data,Other}} -> erlang:error({holder_failed,Other});
        {Port,{exit_status,Status}} -> erlang:error({holder_exited,Status})
    after 10000 -> kill_holder(Port),erlang:error(holder_timeout) end.
kill_holder(Port) ->
    {os_pid,Pid}=erlang:port_info(Port,os_pid),
    Kill=open_port({spawn_executable,"/bin/kill"},[exit_status,{args,["-KILL",integer_to_list(Pid)]}]),
    receive {Kill,{exit_status,0}}->ok after 2000->erlang:error(kill_timeout) end,
    receive {Port,{exit_status,_}}->ok after 2000->erlang:error(holder_not_dead) end.
