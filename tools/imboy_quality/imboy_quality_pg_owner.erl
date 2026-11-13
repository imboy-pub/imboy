%% PostgreSQL is trusted only while this VM holds the port that started it.
-module(imboy_quality_pg_owner).
-include_lib("kernel/include/file.hrl").
-export([
    start/3,
    with_owner/5,
    verify/3,
    stop/2,
    validate_port/4,
    validate_pidfile/5,
    validate_empty_pidfile/1,
    execution_context/1,
    claim_receipt/5,
    commit_receipt/5,
    cancel_start/3
]).

-ifdef(TEST).
-export([owner_loop/7, nonce_hash/1, await_start/5]).
-endif.

start(Ctrl, Root, Register) when is_function(Register, 3) ->
    undefined = get(imboy_pg_owner),
    Root = imboy_quality_env_cleanup:garage_resource_root(Ctrl),
    State = read_state(Ctrl),
    ok = require_execution_gate(State),
    PG = postgres_entry(State),
    <<"PREPARED">> = maps:get(<<"status">>, PG),
    Meta = validate_prepared(PG, Root),
    ok = validate_empty_pidfile(maps:get(data_dir, Meta)),
    Id = string:lowercase(binary:encode_hex(crypto:strong_rand_bytes(16))),
    Log = filename:join(Root, "postgres-owner-" ++ binary_to_list(nonce_hash(Id)) ++ ".log"),
    Args = [
        "-D",
        maps:get(data_dir, Meta),
        "-h",
        "127.0.0.1",
        "-p",
        integer_to_list(maps:get(port, Meta)),
        "-c",
        "logging_collector=off",
        "-c",
        %% 门十七/十八实证：默认 max_connections=100 在 4 worker × (worker 池 +
        %% 隔离夹具 fresh peer 池 + marker provision + boot 迁移) 并发下被打满
        %% （too_many_connections 53300 共 40+72 处）。run 独占 PG 提高连接槽；
        %% 启动参数不在 provision_digest 输入内，不改 provision 身份。
        "max_connections=400",
        "-c",
        "log_statement=none",
        "-c",
        "log_min_duration_statement=-1"
    ],
    Parent = self(),
    Ref = make_ref(),
    ok = persist_launch_intent(Ctrl, Id, Meta),
    {Owner, Mon} = spawn_monitor(fun() ->
        receive {pg_launch, Ref} -> owner_start(Parent, Ref, Meta, Log, Args, Id, Ctrl, Root) end
    end),
    imboy_quality_backend:track_launch(Owner,Ref,Id),
    Owner ! {pg_launch,Ref},
    StartResult = await_start(Owner, Mon, Ref, Id, 10000),
    erlang:demonitor(Mon, [flush]),
    {Port, Pid, StartReceipt} =
        case StartResult of
            {ok, P, OsPid, Receipt} -> {P, OsPid, Receipt};
            {error, Why} -> error(Why)
        end,
    Handle = Meta#{
        port_handle => Port,
        owner => Owner,
        pid => Pid,
        ctrl => Ctrl,
        root => Root,
        log_path => Log,
        nonce => Id
    },
    put(imboy_pg_owner, Handle),
    Registration = safe_call(fun() ->
        Register(Pid, registration_meta(Handle), {Owner, StartReceipt})
    end),
    case Registration of
        ok ->
            case safe_call(fun() -> verify_registered(Handle, Ctrl, Root) end) of
                ok -> imboy_quality_backend:track_pg(Handle), {ok, Handle};
                Error -> fail_start(Handle, {registration_mismatch, Error})
            end;
        Error ->
            fail_start(Handle, {registration_failed, Error})
    end.

%% Keep this task in the foreground while its owner attempts cancellation.
%% A deadline or owner failure is explicitly EXIT_UNVERIFIED, never success.
await_start(Owner, Mon, Ref, Id, Timeout) ->
    receive
        {Ref, started, Owner, Port, Pid, Receipt} -> {ok, Port, Pid, Receipt};
        {Ref, error, Why} -> await_cancel(Owner, Mon, Ref, Id, {start_failed, Why});
        {'DOWN', Mon, process, Owner, Why} -> {error, {postgres_owner_down, Why}}
    after Timeout ->
        await_cancel(Owner, Mon, Ref, Id, start_timeout)
    end.

await_cancel(Owner, Mon, Ref, Id, Cause) ->
    Owner ! {pg_cancel_start, Ref},
    Deadline = erlang:monotonic_time(millisecond) + 20000,
    await_cancel_result(Owner, Mon, Ref, Id, Cause, Deadline).

await_cancel_result(Owner, Mon, Ref, Id, Cause, Deadline) ->
    Remaining = max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {Ref, cancel_result, Owner, Result} ->
            {error, {postgres_owner_start_cancelled, Cause, Result, nonce_hash(Id)}};
        {Ref, started, Owner, _, _, _} ->
            await_cancel_result(Owner, Mon, Ref, Id, Cause, Deadline);
        {Ref, error, _} ->
            await_cancel_result(Owner, Mon, Ref, Id, Cause, Deadline);
        {'DOWN', Mon, process, Owner, Why} ->
            {error, {postgres_owner_exit_unverified, Cause, Why, nonce_hash(Id)}}
    after Remaining ->
        {error, {postgres_owner_cancel_timeout_exit_unverified, Cause, Owner, nonce_hash(Id)}}
    end.

with_owner(Ctrl, Root, Register, RecordExit, Fun) when is_function(Fun, 1) ->
    case start(Ctrl, Root, Register) of
        {ok, Handle} ->
            try
                Fun(Handle)
            catch
                Class:Reason:Stack ->
                    Stop = stop(Handle, RecordExit),
                    erlang:raise(Class, {prepare_failed_postgres_stop, Reason, Stop}, Stack)
            end;
        {error, Reason, _RetainedHandle} ->
            error({postgres_start_failed, Reason})
    end.

%% This gate only accepts the real Port in the current BEAM, never a stored PID.
verify(Ctrl, Args, Root) ->
    case get(imboy_pg_owner) of
        undefined -> {error, live_postgres_handle_unavailable};
        H -> verify_handle(H, Ctrl, Args, Root)
    end.

verify_handle(H, Ctrl, Args, Root) ->
    try
        true = is_map(H),
        Ctrl = maps:get(ctrl, H),
        Root = maps:get(root, H),
        ok = verify_registered(H, Ctrl, Root),
        <<"127.0.0.1">> = iolist_to_binary(maps:get(host, Args)),
        PortNo = maps:get(port, H),
        PortNo = maps:get(port, Args),
        ok = wait_ready(H, 200),
        {ok, postgres_entry(read_state(Ctrl))}
    catch
        Class:Reason:Stack ->
            %% 2026-11-07 排障：此 catch-all 曾把真实抛点吞成单一原子，
            %% IMBOY_QUALITY_KEY_DIAG=1 时输出完整抛点（默认零开销）。
            case os:getenv("IMBOY_QUALITY_KEY_DIAG") of
                "1" ->
                    io:format("PG-VERIFY-DIAG ~tp:~tp~n~tp~n", [
                        Class,
                        Reason,
                        lists:sublist(Stack, 6)
                    ]);
                _ ->
                    ok
            end,
            {error, postgres_port_or_readiness_unverified}
    end.

require_execution_gate(#{<<"schema">> := <<"IMBOY_BACKEND_ACCEPTANCE_V1">>} = State) ->
    imboy_quality_backend:authorize(State, start_pg);
require_execution_gate(State) ->
    {ok, Repo} = file:get_cwd(),
    true = unicode:characters_to_binary(Repo) =:= maps:get(<<"worktree">>, State),
    #{<<"token">> := Token} = maps:get(<<"owner">>, State),
    true = is_binary(Token) andalso byte_size(Token) > 0,
    Supplied = os:getenv("IMBOY_QUALITY_OWNER_TOKEN"),
    true = is_list(Supplied) andalso Token =:= unicode:characters_to_binary(Supplied),
    Context = execution_context(State),
    imboy_quality_cli:permit(Context, eunit_full).

execution_context(State) ->
    (imboy_quality_state:trusted_context(State))#{
        owner_verified => imboy_quality_full_budget:owner_verified(State)
    }.

persist_launch_intent(Ctrl, Id, Meta) ->
    Path = filename:join([
        Ctrl, "evidence", "postgres-intent-" ++ binary_to_list(nonce_hash(Id)) ++ ".json"
    ]),
    {ok, F} = file:open(Path, [write, binary, raw, exclusive]),
    try
        ok = file:change_mode(Path, 8#600),
        ok = file:write(
            F,
            json:encode(#{
                <<"status">> => <<"START_INTENT_EXIT_UNVERIFIED">>,
                <<"data_dir">> => list_to_binary(maps:get(data_dir, Meta)),
                <<"port">> => maps:get(port, Meta)
            })
        ),
        ok = file:sync(F)
    after
        file:close(F)
    end.

persist_launch_receipt(H) ->
    Evidence = filename:join(maps:get(ctrl, H), "evidence"),
    {ok, #file_info{type = directory}} = file:read_link_info(Evidence),
    Path = filename:join(
        Evidence, "postgres-launch-" ++ binary_to_list(nonce_hash(maps:get(nonce, H))) ++ ".json"
    ),
    {ok, F} = file:open(Path, [write, binary, raw, exclusive]),
    try
        ok = file:change_mode(Path, 8#600),
        Data = #{
            <<"status">> => <<"LAUNCHED_EXIT_UNVERIFIED">>,
            <<"pid">> => maps:get(pid, H),
            <<"metadata">> => registration_meta(H)
        },
        ok = file:write(F, json:encode(Data)),
        ok = file:sync(F)
    after
        file:close(F)
    end.

stop(Handle, RecordExit) when is_map(Handle), is_function(RecordExit, 3) ->
    case
        safe_call(fun() ->
            ok = verify_registered(Handle, maps:get(ctrl, Handle), maps:get(root, Handle)),
            request_stop(Handle)
        end)
    of
        {ok, Exit, Receipt} ->
            Recorded = safe_call(fun() ->
                RecordExit(maps:get(pid, Handle), Exit, {maps:get(owner, Handle), Receipt})
            end),
            case Recorded of
                ok ->
                    erase(imboy_pg_owner),
                    case {maps:get(postgres_exit, Exit), maps:get(log_ok, Exit)} of
                        {0, true} -> {ok, Exit};
                        {_, false} -> {error, {postgres_log_incomplete, Exit}};
                        {_, true} -> {error, {postgres_nonzero_exit, Exit}}
                    end;
                _ ->
                    {error, {postgres_exit_record_failed, Exit, Recorded}}
            end;
        Error ->
            {error, {postgres_stop_unconfirmed, Error}}
    end.

fail_start(Handle, Reason) ->
    %% No deletion or PID signal is attempted unless our Port and pidfile agree.
    Stop = safe_call(fun() -> request_stop(Handle) end),
    {error, {Reason, Stop}, Handle}.

validate_port(Port, Owner, Exec, Pid) ->
    try
        true = is_port(Port),
        true = is_pid(Owner) andalso node(Owner) =:= node(),
        true = is_process_alive(Owner),
        {connected, Owner} = erlang:port_info(Port, connected),
        {name, Exec} = erlang:port_info(Port, name),
        {os_pid, Pid} = erlang:port_info(Port, os_pid),
        true = is_integer(Pid) andalso Pid > 0,
        ok
    catch
        _:_ -> {error, port_identity_unverified}
    end.

validate_pidfile(Bytes, Pid, Dir, Port, ExpectedState) ->
    try
        [P, D, _, N, _, Addr, _, Status | _] = binary:split(Bytes, <<"\n">>, [global]),
        Pid = binary_to_integer(P),
        Dir = binary_to_list(D),
        Port = binary_to_integer(N),
        <<"127.0.0.1">> = string:trim(Addr),
        true =
            case ExpectedState of
                ready -> string:trim(Status) =:= <<"ready">>;
                stopping -> lists:member(string:trim(Status), [<<"ready">>, <<"starting">>])
            end,
        ok
    catch
        _:_ -> {error, postmaster_identity_unverified}
    end.

verify_registered(H, Ctrl, Root) ->
    Root = imboy_quality_env_cleanup:garage_resource_root(Ctrl),
    State = read_state(Ctrl),
    PG = postgres_entry(State),
    <<"RUNNING">> = maps:get(<<"status">>, PG),
    Pid = maps:get(pid, H),
    Pid = maps:get(<<"pid">>, PG),
    Reg = registration_meta(H),
    lists:foreach(fun({K, V}) -> V = maps:get(K, PG) end, maps:to_list(Reg)),
    Meta = validate_prepared(PG, Root),
    true = maps:without([port_handle, owner, pid, ctrl, root, log_path, nonce], H) =:= Meta,
    ok = validate_port(maps:get(port_handle, H), maps:get(owner, H), maps:get(executable, H), Pid),
    ok.

validate_prepared(PG, Root) ->
    <<"127.0.0.1">> = maps:get(<<"host">>, PG),
    Port = maps:get(<<"port">>, PG),
    true = is_integer(Port) andalso Port > 0 andalso Port < 65536,
    Data = filename:join(Root, "pgdata"),
    Data = binary_to_list(maps:get(<<"data_dir">>, PG)),
    Exec = filename:join([Root, "postgres-toolchain", "bin", "postgres"]),
    Exec = binary_to_list(maps:get(<<"executable">>, PG)),
    Ctl = filename:join([Root, "postgres-toolchain", "bin", "pg_ctl"]),
    _ = directory_identity(filename:join(Root, "postgres-toolchain")),
    _ = directory_identity(filename:join([Root, "postgres-toolchain", "bin"])),
    DirIdentity = directory_identity(Data),
    ExecIdentity = regular_identity(Exec),
    CtlIdentity = regular_identity(Ctl),
    {ok, ExeBytes} = file:read_file(Exec),
    ExeHash = string:lowercase(binary:encode_hex(crypto:hash(sha256, ExeBytes))),
    ExeHash = maps:get(<<"executable_sha256">>, PG),
    {ok, CtlBytes} = file:read_file(Ctl),
    SystemId = maps:get(<<"system_identifier">>, PG),
    true = is_binary(SystemId) andalso byte_size(SystemId) > 0,
    SystemInt = binary_to_integer(SystemId),
    true = SystemInt > 0,
    SystemId = integer_to_binary(SystemInt),
    #{
        data_dir => Data,
        data_identity => DirIdentity,
        executable => Exec,
        executable_identity => ExecIdentity,
        executable_sha256 => ExeHash,
        pg_ctl => Ctl,
        pg_ctl_identity => CtlIdentity,
        pg_ctl_sha256 => crypto:hash(sha256, CtlBytes),
        port => Port,
        system_identifier => SystemId
    }.

registration_meta(H) ->
    #{
        <<"host">> => <<"127.0.0.1">>,
        <<"port">> => maps:get(port, H),
        <<"data_dir">> => list_to_binary(maps:get(data_dir, H)),
        <<"executable">> => list_to_binary(maps:get(executable, H)),
        <<"executable_sha256">> => maps:get(executable_sha256, H),
        <<"system_identifier">> => maps:get(system_identifier, H),
        <<"executable_identity">> => identity_json(maps:get(executable_identity, H)),
        <<"data_dir_identity">> => identity_json(maps:get(data_identity, H)),
        <<"log_path">> => list_to_binary(maps:get(log_path, H)),
        <<"owner_nonce_sha256">> => string:lowercase(
            binary:encode_hex(crypto:hash(sha256, maps:get(nonce, H)))
        )
    }.

nonce_hash(Nonce) -> string:lowercase(binary:encode_hex(crypto:hash(sha256, Nonce))).

identity_json({Major, Minor, Inode}) ->
    #{<<"major_device">> => Major, <<"minor_device">> => Minor, <<"inode">> => Inode}.

directory_identity(Path) -> file_identity(Path, directory).
regular_identity(Path) -> file_identity(Path, regular).
file_identity(Path, Type) ->
    {ok, #file_info{type = Type, inode = I, major_device = A, minor_device = B}} = file:read_link_info(
        Path
    ),
    true = is_integer(I) andalso I > 0,
    {A, B, I}.

read_state(Ctrl) ->
    {ok, Bytes} = file:read_file(filename:join(Ctrl, "STATE.json")),
    {ok, Hash} = file:read_file(filename:join(Ctrl, "STATE.json.sha256")),
    true = string:lowercase(binary:encode_hex(crypto:hash(sha256, Bytes))) =:= string:trim(Hash),
    json:decode(Bytes).

postgres_entry(State) ->
    [PG] = [
        R
     || #{<<"kind">> := <<"ISOLATED_POSTGRESQL">>} = R <- maps:get(<<"owned_resources">>, State)
    ],
    PG.

stop_verified(H) ->
    Port = maps:get(port_handle, H),
    Pid = maps:get(pid, H),
    Dir = maps:get(data_dir, H),
    ok = validate_port(Port, maps:get(owner, H), maps:get(executable, H), Pid),
    DirIdentity = maps:get(data_identity, H),
    DirIdentity = directory_identity(Dir),
    Ctl = maps:get(pg_ctl, H),
    CtlIdentity = maps:get(pg_ctl_identity, H),
    CtlIdentity = regular_identity(Ctl),
    {ok, CtlBytes} = file:read_file(Ctl),
    CtlHash = maps:get(pg_ctl_sha256, H),
    CtlHash = crypto:hash(sha256, CtlBytes),
    ok = wait_pidfile(H, 50),
    ok = validate_port(Port, maps:get(owner, H), maps:get(executable, H), Pid),
    CtlPort = open_port({spawn_executable, Ctl}, [
        binary,
        exit_status,
        stderr_to_stdout,
        {args, ["-D", Dir, "-m", "fast", "-w", "-t", "10", "stop"]}
    ]),
    wait_stop(H, CtlPort, undefined, undefined, erlang:monotonic_time(millisecond) + 15000).

owner_start(Parent, Ref, Meta, Log, Args, Nonce, Ctrl, Root) ->
    try
        {ok, F} = file:open(Log, [write, binary, raw, exclusive]),
        try
            ok = file:change_mode(Log, 8#600),
            receive
                {pg_cancel_start_from, From, ObserverRef, Ref, Nonce} ->
                    From ! {ObserverRef,{not_started,start_cancelled}},
                    error(start_cancelled);
                {pg_cancel_start, Ref} -> error(start_cancelled)
            after 0 -> ok
            end,
            Port = open_port(
                {spawn_executable, maps:get(executable, Meta)},
                [binary, exit_status, stderr_to_stdout, {args, Args}]
            ),
            {os_pid, Pid} = erlang:port_info(Port, os_pid),
            put(pg_owner_ctrl, Ctrl),
            Mon = erlang:monitor(process, Parent),
            put(pg_owner_parent_monitor, Mon),
            put(pg_owner_parent, Parent),
            put(pg_owner_start_ref, Ref),
            Handle = Meta#{
                port_handle => Port,
                owner => self(),
                pid => Pid,
                ctrl => Ctrl,
                root => Root,
                log_path => Log,
                nonce => Nonce
            },
            put(pg_owner_handle, Handle),
            case safe_call(fun() -> persist_launch_receipt(Handle) end) of
                ok ->
                    Receipt = make_ref(),
                    Payload = registration_meta(Handle),
                    Parent ! {Ref, started, self(), Port, Pid, Receipt},
                    owner_loop(
                        Port,
                        F,
                        Meta,
                        Pid,
                        Nonce,
                        ok,
                        {Receipt, register, Payload}
                    );
                Error ->
                    Parent ! {Ref, error, {launch_receipt_failed_exit_unverified, Pid, Error}},
                    owner_loop(Port, F, Meta, Pid, Nonce, ok, none)
            end
        after
            file:close(F)
        end
    catch
        Class:Reason -> Parent ! {Ref, error, {Class, Reason}}
    end.

require_exit_receipt(Pid, Nonce, Exit) ->
    case persist_exit_receipt(Pid, Nonce, Exit) of
        ok -> Exit;
        _ -> Exit#{log_ok := false}
    end.

persist_exit_receipt(Pid, Nonce, Exit) ->
    safe_call(fun() ->
        Path = filename:join([
            get(pg_owner_ctrl),
            "evidence",
            "postgres-exit-" ++ binary_to_list(nonce_hash(Nonce)) ++ ".json"
        ]),
        {ok, F} = file:open(Path, [write, binary, raw, exclusive]),
        try
            ok = file:change_mode(Path, 8#600),
            ok = file:write(
                F,
                json:encode(#{
                    <<"pid">> => Pid,
                    <<"postgres_exit">> => maps:get(postgres_exit, Exit),
                    <<"pg_ctl_exit">> => maps:get(pg_ctl_exit, Exit),
                    <<"log_ok">> => maps:get(log_ok, Exit)
                })
            ),
            ok = file:sync(F)
        after
            file:close(F)
        end
    end).

claim_receipt(Owner, Receipt, Action, Pid, Payload) ->
    receipt_request(claim_receipt, Owner, Receipt, Action, Pid, Payload).

commit_receipt(Owner, Receipt, Action, Pid, Payload) ->
    receipt_request(commit_receipt, Owner, Receipt, Action, Pid, Payload).

receipt_request(Operation, Owner, Receipt, Action, Pid, Payload) ->
    Ref = make_ref(),
    Mon = erlang:monitor(process, Owner),
    Owner ! {Operation, self(), Ref, Receipt, Action, Pid, Payload},
    try
        receive
            {Ref, Result} -> Result;
            {'DOWN', Mon, process, Owner, _} -> {error, owner_unavailable}
        after 5000 -> {error, receipt_timeout}
        end
    after
        erlang:demonitor(Mon, [flush])
    end.

owner_loop(Port, F, Meta, Pid, Nonce, LogStatus, Pending) ->
    receive
        {Port, {data, Data}} ->
            Next =
                case LogStatus of
                    ok -> file:write(F, Data);
                    Error -> Error
                end,
            owner_loop(Port, F, Meta, Pid, Nonce, Next, Pending);
        {Port, {exit_status, Code}} ->
            Sync = file:sync(F),
            Close = file:close(F),
            Exit = #{
                postgres_exit => Code,
                pg_ctl_exit => <<"NOT_INVOKED">>,
                log_ok => LogStatus =:= ok andalso Sync =:= ok andalso Close =:= ok
            },
            RecordedExit = require_exit_receipt(Pid, Nonce, Exit),
            Receipt = make_ref(),
            Owner = self(),
            spawn(fun() ->
                imboy_quality_pg_register:record_exit(Pid, RecordedExit, {Owner, Receipt})
            end),
            owner_loop(Port, F, Meta, Pid, Nonce, LogStatus, {Receipt, record_exit, RecordedExit});
        {claim_receipt, From, Ref, Receipt, Action, Pid, Payload} ->
            Result =
                case Pending of
                    {Receipt, Action, Payload} -> {ok, Port, Nonce};
                    _ -> {error, invalid_or_consumed_receipt}
                end,
            From ! {Ref, Result},
            owner_loop(Port, F, Meta, Pid, Nonce, LogStatus, Pending);
        {commit_receipt, From, Ref, Receipt, Action, Pid, Payload} ->
            Entry = {Receipt, Action, Payload},
            Tombstones =
                case get(pg_receipt_tombstones) of
                    undefined -> [];
                    T -> T
                end,
            case {Pending =:= Entry, lists:member(Entry, Tombstones)} of
                {true, _} ->
                    put(pg_receipt_tombstones, [Entry | Tombstones]),
                    From ! {Ref, {ok, Port, Nonce}},
                    owner_loop(Port, F, Meta, Pid, Nonce, LogStatus, none);
                {false, true} ->
                    From ! {Ref, {ok, Port, Nonce}},
                    owner_loop(Port, F, Meta, Pid, Nonce, LogStatus, Pending);
                _ ->
                    From ! {Ref, {error, invalid_receipt}},
                    owner_loop(Port, F, Meta, Pid, Nonce, LogStatus, Pending)
            end;
        {pg_cancel_start_from, From, ObserverRef, Ref, Nonce} ->
            case Ref =:= get(pg_owner_start_ref) of
                true ->
                    put(pg_cancel_observer,{From,ObserverRef}),
                    cancel_owner(Port,F,Meta,Pid,Nonce,LogStatus,Pending);
                false -> owner_loop(Port,F,Meta,Pid,Nonce,LogStatus,Pending)
            end;
        {pg_cancel_start, Ref} ->
            case Ref =:= get(pg_owner_start_ref) of
                true -> cancel_owner(Port, F, Meta, Pid, Nonce, LogStatus, Pending);
                false -> owner_loop(Port, F, Meta, Pid, Nonce, LogStatus, Pending)
            end;
        {'DOWN', Mon, process, _Parent, _Reason} ->
            case Mon =:= get(pg_owner_parent_monitor) of
                true -> cancel_owner(Port, F, Meta, Pid, Nonce, LogStatus, Pending);
                false -> owner_loop(Port, F, Meta, Pid, Nonce, LogStatus, Pending)
            end;
        {pg_check, From, Ref, Nonce} ->
            From ! {Ref, LogStatus},
            owner_loop(Port, F, Meta, Pid, Nonce, LogStatus, Pending);
        {pg_stop, From, Ref, #{nonce := Nonce, owner := Owner, pid := Pid} = H} when
            Owner =:= self()
        ->
            Response = safe_call(fun() ->
                Meta = maps:without([port_handle, owner, pid, ctrl, root, log_path, nonce], H),
                Port = maps:get(port_handle, H),
                stop_verified(H#{log_fd => F, log_status => LogStatus})
            end),
            case Response of
                {ok, Exit} ->
                    RecordedExit = require_exit_receipt(Pid, Nonce, Exit),
                    Receipt = make_ref(),
                    From ! {Ref, {ok, RecordedExit, Receipt}},
                    owner_loop(
                        Port,
                        F,
                        Meta,
                        Pid,
                        Nonce,
                        LogStatus,
                        {Receipt, record_exit, RecordedExit}
                    );
                _ ->
                    From ! {Ref, Response},
                    owner_loop(Port, F, Meta, Pid, Nonce, LogStatus, Pending)
            end;
        _ ->
            owner_loop(Port, F, Meta, Pid, Nonce, LogStatus, Pending)
    end.

cancel_owner(Port, F, Meta, Pid, Nonce, LogStatus, Pending) ->
    H = get(pg_owner_handle),
    Result = safe_call(fun() -> stop_verified(H#{log_fd => F, log_status => LogStatus}) end),
    case Result of
        {ok, Exit} ->
            RecordedExit = require_exit_receipt(Pid, Nonce, Exit),
            notify_start_cancel({stop_confirmed, RecordedExit}),
            Receipt = make_ref(),
            Owner = self(),
            spawn(fun() ->
                imboy_quality_pg_register:record_exit(Pid, RecordedExit, {Owner, Receipt})
            end),
            owner_loop(Port, F, Meta, Pid, Nonce, LogStatus, {Receipt, record_exit, RecordedExit});
        _ ->
            %% The launch receipt remains EXIT_UNVERIFIED; retain the Port and log.
            notify_start_cancel({exit_unverified, Result}),
            owner_loop(Port, F, Meta, Pid, Nonce, LogStatus, Pending)
    end.

notify_start_cancel(Result) ->
    case erase(pg_cancel_observer) of
        {Observer,ObserverRef} -> Observer ! {ObserverRef,Result};
        _ -> ok
    end,
    case {get(pg_owner_parent), get(pg_owner_start_ref)} of
        {Parent, Ref} when is_pid(Parent), is_reference(Ref) ->
            Parent ! {Ref, cancel_result, self(), Result},
            ok;
        _ ->
            ok
    end.

%% Cancellation uses the original launch reference and the owner's verified stop
%% path. A timeout is explicitly unconfirmed and leaves launch evidence intact.
cancel_start(Owner,StartRef,Nonce) when is_pid(Owner),is_reference(StartRef),is_binary(Nonce) ->
    Ref=make_ref(),
    Owner ! {pg_cancel_start_from,self(),Ref,StartRef,Nonce},
    receive {Ref,Result}->Result after 25000->{error,postgres_cancel_exit_unverified} end.

request_stop(H) ->
    Owner = maps:get(owner, H),
    Ref = make_ref(),
    Mon = erlang:monitor(process, Owner),
    Owner ! {pg_stop, self(), Ref, H},
    try
        receive
            {Ref, Result} -> Result;
            {'DOWN', Mon, process, Owner, Reason} -> {error, {postgres_owner_down, Reason}}
        after 21000 -> {error, postgres_stop_timeout}
        end
    after
        erlang:demonitor(Mon, [flush])
    end.

owner_healthy(H) ->
    Owner = maps:get(owner, H),
    Ref = make_ref(),
    Mon = erlang:monitor(process, Owner),
    Owner ! {pg_check, self(), Ref, maps:get(nonce, H)},
    try
        receive
            {Ref, Result} -> Result;
            {'DOWN', Mon, process, Owner, Reason} -> {error, {postgres_owner_down, Reason}}
        after 1000 -> {error, postgres_owner_unresponsive}
        end
    after
        erlang:demonitor(Mon, [flush])
    end.

wait_ready(_H, 0) ->
    {error, postgres_not_ready};
wait_ready(H, N) ->
    ok = owner_healthy(H),
    ok = validate_port(
        maps:get(port_handle, H), maps:get(owner, H), maps:get(executable, H), maps:get(pid, H)
    ),
    case read_pidfile(maps:get(data_dir, H)) of
        {ok, B} ->
            case
                validate_pidfile(
                    B, maps:get(pid, H), maps:get(data_dir, H), maps:get(port, H), ready
                )
            of
                ok ->
                    ok;
                _ ->
                    case
                        validate_pidfile(
                            B, maps:get(pid, H), maps:get(data_dir, H), maps:get(port, H), stopping
                        )
                    of
                        ok -> receive
                            after 100 -> wait_ready(H, N - 1)
                            end;
                        Error -> Error
                    end
            end;
        {error, enoent} ->
            receive
            after 100 -> wait_ready(H, N - 1)
            end;
        Error ->
            Error
    end.

wait_pidfile(_H, 0) ->
    {error, pidfile_unavailable};
wait_pidfile(H, N) ->
    case read_pidfile(maps:get(data_dir, H)) of
        {ok, B} ->
            validate_pidfile(
                B, maps:get(pid, H), maps:get(data_dir, H), maps:get(port, H), stopping
            );
        {error, enoent} ->
            receive
            after 100 -> wait_pidfile(H, N - 1)
            end;
        {error, E} ->
            {error, E}
    end.

read_pidfile(Dir) ->
    Path = filename:join(Dir, "postmaster.pid"),
    case file:read_link_info(Path) of
        {ok, #file_info{type = regular, links = 1, size = Size}} when Size > 0, Size =< 4096 ->
            file:read_file(Path);
        {error, enoent} ->
            {error, enoent};
        _ ->
            {error, unsafe_pidfile}
    end.

validate_empty_pidfile(Dir) ->
    case file:read_link_info(Dir) of
        {ok, #file_info{type = directory}} ->
            case file:read_link_info(filename:join(Dir, "postmaster.pid")) of
                {error, enoent} -> ok;
                {ok, _} -> {error, existing_postmaster_pidfile};
                {error, Reason} -> {error, {pidfile_inspection_failed, Reason}}
            end;
        _ ->
            {error, pgdata_directory_unverified}
    end.

wait_stop(H, CtlPort, PgExit, CtlExit, Deadline) ->
    case {PgExit, CtlExit} of
        {P, 0} when is_integer(P) ->
            Sync = file:sync(maps:get(log_fd, H)),
            Close = file:close(maps:get(log_fd, H)),
            LogOk = maps:get(log_status, H) =:= ok andalso Sync =:= ok andalso Close =:= ok,
            {ok, #{postgres_exit => P, pg_ctl_exit => 0, log_ok => LogOk}};
        {_, C} when is_integer(C), C =/= 0 -> {error, {pg_ctl_failed, C}};
        _ ->
            Remaining = max(0, Deadline - erlang:monotonic_time(millisecond)),
            PgPort = maps:get(port_handle, H),
            receive
                {PgPort, {data, Data}} ->
                    Status =
                        case maps:get(log_status, H) of
                            ok -> file:write(maps:get(log_fd, H), Data);
                            Error -> Error
                        end,
                    wait_stop(H#{log_status => Status}, CtlPort, PgExit, CtlExit, Deadline);
                {PgPort, {exit_status, Code}} ->
                    wait_stop(H, CtlPort, Code, CtlExit, Deadline);
                {CtlPort, {data, _}} ->
                    wait_stop(H, CtlPort, PgExit, CtlExit, Deadline);
                {CtlPort, {exit_status, Code}} ->
                    wait_stop(H, CtlPort, PgExit, Code, Deadline)
            after Remaining -> {error, postgres_exit_unverified}
            end
    end.

safe_call(Fun) ->
    try
        Fun()
    catch
        Class:Reason -> {error, {Class, Reason}}
    end.
