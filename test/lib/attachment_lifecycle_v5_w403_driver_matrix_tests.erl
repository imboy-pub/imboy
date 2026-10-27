%%% 每叶通过真实入口执行；缺入口 harness 是 NO_GO，不能用 raw driver 顶替。
-module(attachment_lifecycle_v5_w403_driver_matrix_tests).
-include_lib("eunit/include/eunit.hrl").

%% Focused first-stage proof. The immutable matrix below remains independent;
%% a successful round trip never seals an unexecuted race/fault scenario.
roundtrip_test_() ->
    {setup, fun attachment_w403_http_fixture:setup/0, fun attachment_w403_http_fixture:cleanup/1,
        fun(S) ->
            [
                {
                    atom_to_list(Surface) ++ "/" ++ atom_to_list(Driver),
                    {timeout, 180, fun() ->
                        Leaf =
                            <<"roundtrip/", (atom_to_binary(Surface))/binary, "/",
                                (atom_to_binary(Driver))/binary>>,
                        try
                            attachment_w403_http_fixture:roundtrip(
                                S#{leaf => Leaf}, Surface, Driver
                            )
                        after
                            report_evidence(Leaf)
                        end
                    end}
                }
             || Surface <- [ordinary, enterprise_internal, enterprise_seat, cs_widget],
                Driver <- [disk, garage]
            ]
        end}.

%% Separate from the fixed 64-leaf matrix: the declared hash and size match the
%% real body, but a disallowed text control byte lies beyond the first 64 KiB.
sealed_body_validation_test_() ->
    {setup, fun attachment_w403_http_fixture:setup/0, fun attachment_w403_http_fixture:cleanup/1,
        fun(S) ->
            [
                {
                    atom_to_list(Surface) ++ "/" ++ atom_to_list(Driver),
                    {timeout, 180, fun() ->
                        Leaf =
                            <<"sealed-body/", (atom_to_binary(Surface))/binary, "/",
                                (atom_to_binary(Driver))/binary>>,
                        try
                            Prefix = binary:copy(<<"a">>, 65537),
                            Bytes = <<Prefix/binary, 1, "tail">>,
                            T = attachment_w403_http_fixture:register(
                                S#{leaf => Leaf},
                                Surface,
                                Driver,
                                Bytes
                            ),
                            Mod = driver(Driver),
                            meck:new(Mod, [passthrough, no_link]),
                            try
                                Result = attachment_w403_http_fixture:put(T, Bytes),
                                ?assertNot(attachment_w403_http_fixture:success(Result)),
                                ?assertEqual(0, put_calls(Mod)),
                                ?assertEqual(
                                    {error, not_found}, attachment_w403_http_fixture:read(T)
                                )
                            after
                                meck:unload(Mod)
                            end
                        after
                            report_evidence(Leaf)
                        end
                    end}
                }
             || Surface <- [enterprise_seat, cs_widget], Driver <- [disk, garage]
            ]
        end}.

report_evidence(Leaf) ->
    io:format(user, "W403_HTTP_EVIDENCE ~s~n", [
        jsone:encode(#{
            leaf => Leaf,
            requests => attachment_w403_http_fixture:evidence(Leaf)
        })
    ]).

w403_driver_matrix_test_() ->
    {setup, fun attachment_w403_http_fixture:setup/0, fun attachment_w403_http_fixture:cleanup/1,
        fun(S) ->
            [
                {
                    atom_to_list(Surface) ++ "/" ++ atom_to_list(Driver) ++ "/" ++
                        atom_to_list(Scenario),
                    {timeout, 180, fun() -> run_leaf(S, Surface, Driver, Scenario) end}
                }
             || Surface <- [ordinary, enterprise_internal, enterprise_seat, cs_widget],
                Driver <- [disk, garage],
                Scenario <- scenarios()
            ]
        end}.
scenarios() ->
    [
        confirm_old_put_different_bytes,
        stat_then_different_put,
        inflight_same_bytes_after_confirm,
        put_paused_before_dispatch_until_expiry,
        same_key_different_worker_payload,
        provider_bypass_writer_denied,
        max_supported_file_gateway_budget,
        disk_put_delete_lock_order_or_garage_expiry_fence
    ].

-define(F, attachment_w403_http_fixture).
-define(BODY, <<"w403 original registered content">>).

run_leaf(S0, Surface, Driver, Scenario) ->
    Leaf = <<
        (atom_to_binary(Surface))/binary,
        "/",
        (atom_to_binary(Driver))/binary,
        "/",
        (atom_to_binary(Scenario))/binary
    >>,
    S = S0#{leaf => Leaf},
    try
        scenario(S, Surface, Driver, Scenario)
    after
        cleanup_jobs(),
        report_evidence(Leaf)
    end.

scenario(S, Surface, Driver, confirm_old_put_different_bytes) ->
    T = ?F:register(S, Surface, Driver, ?BODY),
    Mod = driver(Driver),
    meck:new(Mod, [passthrough, no_link]),
    try
        ?assert(?F:success(?F:put(T, ?BODY))),
        ?assert(?F:success(?F:confirm(T))),
        Before = put_calls(Mod),
        ?assertNot(?F:success(?F:put(T, <<"different old capability content">>))),
        ?assertEqual(Before, put_calls(Mod)),
        %% Fresh same-content PUT after confirmation is read-only/idempotent or
        %% rejected. Neither outcome permits another provider write.
        _ = ?F:put(T, ?BODY),
        ?assertEqual(Before, put_calls(Mod)),
        ?assertEqual({ok, ?BODY}, ?F:read(T))
    after
        meck:unload(Mod)
    end;
scenario(S, Surface, Driver, stat_then_different_put) ->
    T = ?F:register(S, Surface, Driver, ?BODY),
    ?assert(?F:success(?F:put(T, ?BODY))),
    with_read_barrier(T, fun(Barrier) ->
        Job = start_job(fun() -> ?F:confirm(T) end),
        Worker = await_barrier(Barrier),
        case Surface of
            enterprise_internal ->
                serialized_competing_put(T, Barrier, Worker, Job);
            _ ->
                Competing = ?F:put(T, <<"replacement after observed bytes">>),
                ?assertNot(?F:success(Competing)),
                ?F:overlap(maps:get(leaf, T), Worker, Competing),
                release(Barrier, Worker),
                ?assert(?F:success(finish_job(Job)))
        end,
        ?assertEqual({ok, ?BODY}, ?F:read(T))
    end);
scenario(S, Surface, Driver, inflight_same_bytes_after_confirm) ->
    T = ?F:register(S, Surface, Driver, ?BODY),
    ?assert(?F:success(?F:put(T, ?BODY))),
    with_put_barrier(T, fun(Barrier) ->
        Job = start_job(fun() -> ?F:put(T, ?BODY) end),
        Worker = await_barrier(Barrier),
        Confirmed = ?F:confirm(T),
        ?assert(?F:success(Confirmed)),
        ?F:overlap(maps:get(leaf, T), Worker, Confirmed),
        release(Barrier, Worker),
        Result = finish_job(Job),
        ?assert(?F:success(Result) orelse lists:member(maps:get(status, Result), [409, 410])),
        ?assertEqual({ok, ?BODY}, ?F:read(T))
    end);
scenario(S, Surface, Driver, put_paused_before_dispatch_until_expiry) ->
    T = short_registration(S, Surface, Driver, ?BODY),
    with_put_barrier(T, fun(Barrier) ->
        Job = start_job(fun() -> ?F:put(T, ?BODY) end),
        Worker = await_barrier(Barrier),
        wait_expired(T),
        release(Barrier, Worker),
        ?assertNot(?F:success(finish_job(Job))),
        ?assertEqual({error, not_found}, ?F:read(T)),
        ?assertNot(?F:success(?F:confirm(T)))
    end);
scenario(S, Surface, Driver, same_key_different_worker_payload) ->
    T = ?F:register(S, Surface, Driver, ?BODY),
    with_put_barrier(T, fun(Barrier) ->
        First = start_job(fun() -> ?F:put(T, ?BODY) end),
        Worker = await_barrier(Barrier),
        %% The first worker has committed its receipt, but not dispatched I/O.
        Second = start_job(fun() -> ?F:put(T, <<"different competing worker payload">>) end),
        Competing = finish_job(Second),
        ?assertNot(?F:success(Competing)),
        ?F:overlap(maps:get(leaf, T), Worker, Competing),
        ?assertEqual({error, not_found}, ?F:read(T)),
        release(Barrier, Worker),
        ?assert(?F:success(finish_job(First))),
        ?assert(?F:success(?F:confirm(T))),
        ?assertEqual({ok, ?BODY}, ?F:read(T))
    end);
scenario(S, Surface, Driver, provider_bypass_writer_denied) ->
    T = ?F:register(S, Surface, Driver, ?BODY),
    ?assert(?F:success(?F:put(T, ?BODY))),
    ?assert(?F:success(?F:confirm(T))),
    bypass_denied(T),
    ?assertEqual({ok, ?BODY}, ?F:read(T));
scenario(S, Surface, Driver, max_supported_file_gateway_budget) ->
    Size = ?F:max_bytes(Surface),
    Bytes = binary:copy(<<"a">>, Size),
    T = ?F:register(S, Surface, Driver, Bytes),
    erlang:garbage_collect(),
    Baseline = erlang:memory(binary),
    Parent = self(),
    Ref = make_ref(),
    Sampler = spawn_link(fun() -> sample_memory(Parent, Ref, Baseline) end),
    Started = erlang:monotonic_time(millisecond),
    Response =
        try
            ?F:put(T, Bytes)
        after
            Sampler ! {stop, Ref}
        end,
    Elapsed = erlang:monotonic_time(millisecond) - Started,
    Peak =
        receive
            {memory_peak, Ref, P} -> P
        after 1000 ->
            unlink(Sampler),
            exit(Sampler, kill),
            erlang:error(memory_sampler_timeout)
        end,
    Delta = max(0, Peak - Baseline),
    io:format(user, "W403_GATEWAY_BUDGET ~s~n", [
        jsone:encode(#{
            leaf => maps:get(leaf, T),
            payload_bytes => Size,
            peak_binary_delta_bytes => Delta,
            elapsed_ms => Elapsed,
            throughput_bytes_per_second => (Size * 1000) div max(1, Elapsed),
            peak_binary_delta_per_payload => Delta / Size
        })
    ]),
    %% Preserve the configured maximum size and HTTP timeout. Memory is recorded
    %% as evidence; do not invent a new SLO for the retained httpc binary path.
    ?assert(?F:success(Response)),
    ?assert(Elapsed < 120000),
    ?assertEqual({ok, Bytes}, ?F:read(T)),
    ?assert(?F:success(?F:confirm(T)));
scenario(S, Surface, disk, disk_put_delete_lock_order_or_garage_expiry_fence) ->
    disk_delete_order(short_registration(S, Surface, disk, ?BODY));
scenario(S, Surface, garage, disk_put_delete_lock_order_or_garage_expiry_fence) ->
    garage_signed_expiry(short_registration(S, Surface, garage, ?BODY)).

%% Barriers only pause a real operation for this unique key. Authentication,
%% persistence, signatures and provider responses always remain production code.
serialized_competing_put(T, Barrier, ConfirmWorker, ConfirmJob) ->
    %% Internal confirm holds its identity/session transaction across the read.
    %% Observe the competing authenticated request before its real admission
    %% waits for that lock, then release confirm and require the final rejection.
    Key = maps:get(key, T),
    Bytes = <<"replacement after observed bytes">>,
    Hash = binary:encode_hex(crypto:hash(sha256, Bytes), lowercase),
    Parent = self(),
    Admission = make_ref(),
    ok = meck:new(attach_pending_repo, [passthrough, no_link]),
    try
        ok = meck:expect(attach_pending_repo, register_content_pin, fun(K, Receipt) ->
            case K =:= Key andalso maps:get(<<"sha256">>, Receipt) =:= Hash of
                true -> Parent ! {w403_admission_entered, Admission, self()};
                false -> ok
            end,
            meck:passthrough([K, Receipt])
        end),
        PutJob = start_job(fun() -> ?F:put(T, Bytes) end),
        PutWorker =
            receive
                {w403_admission_entered, Admission, Pid} -> Pid
            after 10000 -> erlang:error(w403_admission_not_reached)
            end,
        RequestId = ?F:inflight_overlap(maps:get(leaf, T), ConfirmWorker, PutWorker),
        ?assertEqual(0, put_calls(driver(maps:get(driver, T)))),
        release(Barrier, ConfirmWorker),
        ?assert(?F:success(finish_job(ConfirmJob))),
        Competing = finish_job(PutJob),
        ?assertEqual(RequestId, maps:get(request_id, Competing)),
        ?assertEqual(409, maps:get(status, Competing)),
        ?assertNot(?F:success(Competing)),
        ?assertEqual(0, put_calls(driver(maps:get(driver, T))))
    after
        release(Barrier, ConfirmWorker),
        cleanup_jobs(),
        meck:unload(attach_pending_repo)
    end.

with_read_barrier(T, Fun) ->
    Mod = driver(maps:get(driver, T)),
    Key = maps:get(key, T),
    Parent = self(),
    Barrier = make_ref(),
    Gate = ets:new(w403_barrier, [public, set]),
    meck:new(Mod, [passthrough, no_link]),
    try
        meck:expect(Mod, read_snapshot, fun(I) ->
            Result = meck:passthrough([I]),
            case maps:get(key, I) =:= Key andalso Result =:= {ok, ?BODY} of
                true -> pause_once(Parent, Barrier, Gate);
                false -> ok
            end,
            Result
        end),
        Fun(Barrier)
    after
        release_all(Barrier, Gate),
        cleanup_jobs(),
        meck:unload(Mod)
    end.

with_put_barrier(T, Fun) ->
    Mod = driver(maps:get(driver, T)),
    Key = maps:get(key, T),
    Parent = self(),
    Barrier = make_ref(),
    Gate = ets:new(w403_barrier, [public, set]),
    meck:new(Mod, [passthrough, no_link]),
    try
        case maps:get(driver, T) of
            garage ->
                meck:expect(Mod, put_snapshot, fun(I, Source, Mime, Ctx) ->
                    maybe_pause(I, Key, Parent, Barrier, Gate),
                    meck:passthrough([I, Source, Mime, Ctx])
                end);
            disk ->
                meck:expect(Mod, put_snapshot, fun(I, Source, Expiry, Check) ->
                    maybe_pause(I, Key, Parent, Barrier, Gate),
                    meck:passthrough([I, Source, Expiry, Check])
                end)
        end,
        Fun(Barrier)
    after
        release_all(Barrier, Gate),
        cleanup_jobs(),
        meck:unload(Mod)
    end.

maybe_pause(I, Key, Parent, Barrier, Gate) ->
    case maps:get(key, I) =:= Key of
        true -> pause_once(Parent, Barrier, Gate);
        false -> ok
    end.
pause_once(Parent, Barrier, Gate) ->
    Entered =
        try
            ets:insert_new(Gate, {worker, self()})
            %% The owning leaf is already cleaning up.
        catch
            error:badarg -> false
        end,
    case Entered of
        false ->
            ok;
        true ->
            Parent ! {w403_barrier, Barrier, self()},
            receive
                {w403_continue, Barrier} -> ok
            after 30000 -> erlang:error(w403_barrier_timeout)
            end
    end.
await_barrier(Barrier) ->
    receive
        {w403_barrier, Barrier, Pid} ->
            erlang:put({w403_waiter, Barrier}, Pid),
            Pid
    after 10000 -> erlang:error(w403_barrier_not_reached)
    end.
release(Barrier, Pid) ->
    Pid ! {w403_continue, Barrier},
    ok.
release_all(Barrier, Gate) ->
    [release(Barrier, Pid) || {worker, Pid} <- ets:tab2list(Gate)],
    erlang:erase({w403_waiter, Barrier}),
    ets:delete(Gate),
    ok.
start_job(Fun) ->
    Parent = self(),
    Ref = make_ref(),
    {Pid, Monitor} = spawn_opt(
        fun() ->
            Result =
                try
                    {ok, Fun()}
                catch
                    Class:Reason:Stack -> {error, Class, Reason, Stack}
                end,
            Parent ! {w403_result, Ref, Result}
        end,
        [link, monitor]
    ),
    Job = {Pid, Monitor, Ref},
    erlang:put(w403_jobs, [Job | jobs()]),
    Job.
finish_job({Pid, Monitor, Ref} = Job) ->
    try
        receive
            {w403_result, Ref, {ok, Value}} ->
                erlang:demonitor(Monitor, [flush]),
                Value;
            {w403_result, Ref, {error, Class, Reason, Stack}} ->
                erlang:demonitor(Monitor, [flush]),
                erlang:raise(Class, Reason, Stack);
            {'DOWN', Monitor, process, Pid, Reason} ->
                erlang:error({w403_worker_exit, Reason})
        after 130000 ->
            unlink(Pid),
            exit(Pid, kill),
            erlang:error(w403_worker_timeout)
        end
    after
        erlang:put(w403_jobs, lists:delete(Job, jobs()))
    end.
jobs() ->
    case erlang:get(w403_jobs) of
        L when is_list(L) -> L;
        _ -> []
    end.
cleanup_jobs() ->
    %% Release real-operation barriers before waiting here. Do not unload meck
    %% while an owned client or its server-side operation is still returning.
    [
        try
            finish_job(Job)
        catch
            _:_ -> ok
        end
     || Job <- jobs()
    ],
    erlang:erase(w403_jobs),
    ok.
driver(disk) -> attachment_storage_disk;
driver(garage) -> attachment_storage_garage.
put_calls(Mod) -> meck:num_calls(Mod, put_snapshot, '_').

%% Shorten only this leaf's initial persisted expiry, before production signing.
%% This is a real short-lived session, not a forged token or mocked PG result.
short_registration(S, Surface, Driver, Bytes) ->
    Cid = elib_uuid:gen_v7(),
    Parent = self(),
    meck:new(elib_pg, [passthrough, no_link]),
    T =
        try
            meck:expect(elib_pg, query, fun(Sql, Params) ->
                meck:passthrough([Sql, short_expiry(Sql, Params, Cid, Parent)])
            end),
            meck:expect(elib_pg, query, fun(Conn, Sql, Params) ->
                meck:passthrough([Conn, Sql, short_expiry(Sql, Params, Cid, Parent)])
            end),
            ?F:register(S#{registration_cid => Cid}, Surface, Driver, Bytes)
        after
            meck:unload(elib_pg)
        end,
    receive
        {short_expiry, Cid, Expiry, OriginalDeadline} ->
            {Expiry, OriginalDeadline} = persisted_times(T),
            ?assertEqual(Expiry, expiry(maps:get(<<"expires_at">>, maps:get(registration, T)))),
            T#{expiry => Expiry}
    after 1000 -> erlang:error(short_expiry_not_persisted)
    end.

short_expiry(Sql0, Args, Cid, Parent) when is_list(Args) ->
    Sql = iolist_to_binary(Sql0),
    Positions =
        case
            {
                binary:match(Sql, <<"fn_v5_register_pending(">>),
                binary:match(Sql, <<"fn_v5_register_eb(">>)
            }
        of
            {{_, _}, _} when length(Args) =:= 12 -> {5, 7, 8};
            {_, {_, _}} when length(Args) =:= 9 -> {5, 8, 9};
            _ -> none
        end,
    case Positions of
        {C, D, E} ->
            case lists:nth(C, Args) =:= Cid of
                true ->
                    Deadline = expiry(lists:nth(D, Args)),
                    Original = expiry(lists:nth(E, Args)),
                    Short = min(erlang:system_time(second) + 3, min(Original, Deadline)),
                    Parent ! {short_expiry, Cid, Short, Deadline},
                    replace_nth(E, iso(Short), Args);
                false ->
                    Args
            end;
        none ->
            Args
    end;
short_expiry(_, Args, _, _) ->
    Args.
replace_nth(N, V, List) ->
    {A, [_ | B]} = lists:split(N - 1, List),
    A ++ [V | B].
iso(T) -> list_to_binary(calendar:system_time_to_rfc3339(T, [{unit, second}, {offset, "Z"}])).
expiry(T) when is_integer(T) -> T;
expiry(T) when is_binary(T) ->
    try
        binary_to_integer(T)
    catch
        error:badarg ->
            calendar:rfc3339_to_system_time(binary_to_list(T), [{unit, second}])
    end;
expiry(T) when is_list(T) -> calendar:rfc3339_to_system_time(T, [{unit, second}]).
session_table(#{surface := Surface}) when Surface =:= ordinary; Surface =:= enterprise_internal ->
    <<"attach_pending">>;
session_table(_) ->
    <<"enterprise_asset">>.
persisted_times(#{key := Key} = T) ->
    Table = session_table(T),
    {ok, [#{<<"expiry">> := E, <<"deadline">> := D}]} = elib_pg:query(
        <<"SELECT extract(epoch from upload_expires_at)::bigint AS expiry, extract(epoch from session_deadline)::bigint AS deadline FROM ",
            Table/binary, " WHERE object_key=$1">>,
        [Key]
    ),
    {E, D}.
wait_expired(T) ->
    Expiry = maps:get(expiry, T),
    Delay = max(0, Expiry * 1000 - erlang:system_time(millisecond) + 50),
    timer:sleep(Delay),
    ?assert(erlang:system_time(second) >= Expiry),
    Table = session_table(T),
    {ok, [#{<<"expired">> := true}]} = elib_pg:query(
        <<"SELECT clock_timestamp()>=upload_expires_at AS expired FROM ", Table/binary,
            " WHERE object_key=$1">>,
        [maps:get(key, T)]
    ).

bypass_denied(#{driver := garage, location := I} = T) ->
    %% The actual provider receives an unsigned write, not a mocked denial.
    Key = maps:get(key, I),
    match = re:run(Key, <<"^[a-zA-Z0-9/_.-]+$">>, [{capture, none}]),
    Endpoint = string:trim(binary_to_list(maps:get(endpoint, I)), trailing, "/"),
    Url = Endpoint ++ "/" ++ binary_to_list(maps:get(container, I)) ++ "/" ++ binary_to_list(Key),
    {ok, {{_, Code, _}, _, _}} = httpc:request(
        put,
        {Url, [], "text/plain", <<"unsigned bypass writer">>},
        [{timeout, 30000}, {autoredirect, false}],
        [{body_format, binary}]
    ),
    ?assert(lists:member(Code, [401, 403])),
    bypass_evidence(T, #{provider_http_status => Code});
bypass_denied(#{driver := disk, location := I} = T) ->
    %% Same-UID filesystem writers are explicitly outside the isolation boundary.
    %% Prove rejection by a genuinely different OS principal instead.
    Exe =
        case os:find_executable("runuser") of
            false -> erlang:error({no_go, unprivileged_writer_tool_missing, runuser});
            Found -> Found
        end,
    Id =
        case os:find_executable("id") of
            false -> erlang:error({no_go, unprivileged_writer_tool_missing, id});
            IdPath -> IdPath
        end,
    {0, OwnerText} = command(Id, ["-u"]),
    Owner = binary_to_integer(string:trim(OwnerText)),
    Writer =
        case command(Exe, ["-u", "nobody", "--", Id, "-u"]) of
            {0, UidText} -> binary_to_integer(string:trim(UidText));
            _ -> erlang:error({no_go, unprivileged_writer_unavailable})
        end,
    ?assert(Writer > 0),
    ?assertNotEqual(Owner, Writer),
    <<"file://", Root/binary>> = maps:get(endpoint, I),
    Path = filename:join([
        binary_to_list(Root),
        binary_to_list(maps:get(container, I)),
        binary_to_list(maps:get(key, I))
    ]),
    {Code, Output} = command(Exe, [
        "-u",
        "nobody",
        "--",
        "/bin/sh",
        "-c",
        "printf '%s' \"$1\" > \"$2\"",
        "w403",
        "unprivileged bypass writer",
        Path
    ]),
    ?assert(Code =/= 0),
    ?assertNotEqual(nomatch, binary:match(Output, <<"Permission denied">>)),
    bypass_evidence(T, #{
        unprivileged_writer_exit => Code,
        owner_uid => Owner,
        writer_uid => Writer
    }).
bypass_evidence(T, Detail) ->
    io:format(user, "W403_PROVIDER_BYPASS ~s~n", [jsone:encode(Detail#{leaf => maps:get(leaf, T)})]).
command(Exe, Args) ->
    Port = open_port({spawn_executable, Exe}, [
        binary,
        exit_status,
        use_stdio,
        stderr_to_stdout,
        {args, Args},
        {env, [{"LC_ALL", "C"}]}
    ]),
    command_result(Port, <<>>).
command_result(Port, Acc) ->
    receive
        {Port, {data, Data}} -> command_result(Port, <<Acc/binary, Data/binary>>);
        {Port, {exit_status, Code}} -> {Code, Acc}
    after 5000 ->
        port_close(Port),
        erlang:error(unprivileged_writer_timeout)
    end.

sample_memory(Parent, Ref, Peak0) ->
    Peak = max(Peak0, erlang:memory(binary)),
    receive
        {stop, Ref} -> Parent ! {memory_peak, Ref, Peak}
    after 2 -> sample_memory(Parent, Ref, Peak)
    end.

%% Capture only the actual signer result in memory, then pause the entry upload.
%% The application's failed late upload and Garage's rejection of the unchanged
%% signed URL are separate assertions. A local abort is never a remote drain proof.
garage_signed_expiry(T) ->
    Key = maps:get(key, T),
    Parent = self(),
    Barrier = make_ref(),
    Gate = ets:new(w403_signed_barrier, [public, set]),
    meck:new(elib_s3_sign, [passthrough, no_link]),
    try
        meck:expect(
            elib_s3_sign,
            presign_url_at,
            fun(Method, EP, C, K, Mime, Issued, Ttl, Cfg) ->
                Url = meck:passthrough([Method, EP, C, K, Mime, Issued, Ttl, Cfg]),
                case Method =:= <<"PUT">> andalso K =:= Key of
                    true ->
                        ?assertEqual(
                            maps:get(expiry, T),
                            calendar:datetime_to_gregorian_seconds(Issued) + Ttl - 62167219200
                        ),
                        Parent ! {frozen_provider_url, Barrier, Url, Mime},
                        pause_once(Parent, Barrier, Gate);
                    false ->
                        ok
                end,
                Url
            end
        ),
        Job = start_job(fun() -> ?F:put(T, ?BODY) end),
        Worker = await_barrier(Barrier),
        {Url, Mime} =
            receive
                {frozen_provider_url, Barrier, U, M} -> {U, M}
            after 1000 -> erlang:error(frozen_provider_url_missing)
            end,
        wait_expired(T),
        release(Barrier, Worker),
        %% This proves the real application entry did not accept a late upload.
        ?assertNot(?F:success(finish_job(Job))),
        ?assertEqual({error, not_found}, ?F:read(T)),
        %% Independently prove the real provider enforces its signed deadline.
        %% No URL rewriting, refreshed signature, credential or response mock.
        ProbeId = <<"w403-", (elib_uuid:gen_v7())/binary>>,
        {ok, {{_, Code, _}, _, ErrorBody}} = httpc:request(
            put,
            {
                binary_to_list(Url),
                [{"x-w403-request-id", binary_to_list(ProbeId)}],
                binary_to_list(Mime),
                ?BODY
            },
            [{timeout, 30000}, {autoredirect, false}],
            [{body_format, binary}]
        ),
        ExpiryReason = provider_expiry_reason(Code, ErrorBody),
        io:format(user, "W403_GARAGE_EXPIRY ~s~n", [
            jsone:encode(#{
                leaf => maps:get(leaf, T),
                stage => <<"unchanged_signed_provider_replay">>,
                method => <<"PUT">>,
                request_id => ProbeId,
                status => Code,
                provider_error_code => provider_error_code(ErrorBody),
                provider_expiry_reason => atom_to_binary(ExpiryReason)
            })
        ]),
        ?assertEqual(signed_url_expired, ExpiryReason),
        ?assertEqual({error, not_found}, ?F:read(T)),
        ?assertNot(?F:success(?F:confirm(T)))
    after
        release_all(Barrier, Gate),
        cleanup_jobs(),
        meck:unload(elib_s3_sign)
    end.

%% Garage v2.4.1 (268334bd): signature/payload.rs rejects expired presigned
%% dates as BadRequest; common_error.rs maps that to 400 / InvalidRequest.
%% Require the exact fixed message as well: unrelated 400s and forbidden bad
%% signatures are not expiry evidence. Never return or log the raw message/body.
provider_expiry_reason(400, Body) when is_binary(Body) ->
    case
        {
            provider_error_code(Body),
            binary:match(Body, <<"<Message>Bad request: Date is too old</Message>">>)
        }
    of
        {<<"InvalidRequest">>, {_, _}} -> signed_url_expired;
        _ -> unrecognized
    end;
provider_expiry_reason(_, _) ->
    unrecognized.

provider_expiry_reason_test() ->
    Expired =
        <<"<Error><Code>InvalidRequest</Code>",
            "<Message>Bad request: Date is too old</Message></Error>">>,
    ?assertEqual(signed_url_expired, provider_expiry_reason(400, Expired)),
    ?assertEqual(unrecognized, provider_expiry_reason(403, Expired)),
    ?assertEqual(
        unrecognized,
        provider_expiry_reason(
            400,
            <<"<Error><Code>InvalidRequest</Code><Message>Bad request: Invalid date</Message></Error>">>
        )
    ),
    ?assertEqual(
        unrecognized,
        provider_expiry_reason(
            403,
            <<"<Error><Code>AccessDenied</Code><Message>Forbidden: Invalid signature</Message></Error>">>
        )
    ),
    ?assertEqual(
        unrecognized,
        provider_expiry_reason(
            400,
            <<"<Error><Code>AccessDenied</Code><Message>Bad request: Date is too old</Message></Error>">>
        )
    ),
    ?assertEqual(unrecognized, provider_expiry_reason(400, <<"unclassified">>)).

%% Emit only the provider's symbolic error code, never its URL, headers, body,
%% signature or credentials.
provider_error_code(Body) ->
    case re:run(Body, <<"<Code>([A-Za-z0-9_]{1,64})</Code>">>, [{capture, [1], binary}]) of
        {match, [Code]} -> Code;
        nomatch -> <<"unclassified">>
    end.

disk_delete_order(T) ->
    ?assert(?F:success(?F:put(T, ?BODY))),
    Key = maps:get(key, T),
    Parent = self(),
    Barrier = make_ref(),
    Gate = ets:new(w403_disk_lock_barrier, [public, set]),
    meck:new(attachment_storage_disk, [passthrough, no_link]),
    try
        meck:expect(attachment_storage_disk, put_snapshot, fun(I, Source, Expiry, Check) ->
            LockedCheck = fun() ->
                Result = Check(),
                case maps:get(key, I) =:= Key andalso Result =:= ok of
                    true -> pause_once(Parent, Barrier, Gate);
                    false -> ok
                end,
                Result
            end,
            meck:passthrough([I, Source, Expiry, LockedCheck])
        end),
        meck:expect(attachment_storage_disk, delete_snapshot, fun(I) ->
            case maps:get(key, I) =:= Key of
                true -> Parent ! {disk_delete_started, Barrier};
                false -> erlang:error(unowned_delete_attempt)
            end,
            meck:passthrough([I])
        end),
        Writer = start_job(fun() -> ?F:put(T, ?BODY) end),
        Worker = await_barrier(Barrier),
        wait_expired(T),
        {Drain, QueueSql, QueueArgs} = deletion_intent(T),
        Deleter = start_job(Drain),
        receive
            {disk_delete_started, Barrier} -> ok
        after 5000 -> erlang:error(delete_did_not_reach_lock)
        end,
        %% DELETE has entered the real driver but cannot pass the held OS lock.
        DeleteRef = element(3, Deleter),
        receive
            {w403_result, DeleteRef, _} -> erlang:error(delete_bypassed_put_lock)
        after 100 -> ok
        end,
        ?assertEqual({ok, ?BODY}, ?F:read(T)),
        release(Barrier, Worker),
        ?assertNot(?F:success(finish_job(Writer))),
        ?assertMatch({ok, _}, finish_job(Deleter)),
        ?assertEqual({error, not_found}, ?F:read(T)),
        {ok, [#{<<"job_state">> := <<"done">>}]} = elib_pg:query(QueueSql, QueueArgs),
        ?assertNot(?F:success(?F:put(T, ?BODY))),
        ?assertEqual({error, not_found}, ?F:read(T))
    after
        release_all(Barrier, Gate),
        cleanup_jobs(),
        meck:unload(attachment_storage_disk)
    end.

deletion_intent(#{surface := Surface, key := Key} = T) when
    Surface =:= ordinary; Surface =:= enterprise_internal
->
    %% Ordinary worker lacks a scoped claim. Refuse to run it if another due job
    %% exists; never consume, reset or postpone another run's queue rows.
    {ok, []} = elib_pg:query(
        <<"SELECT path FROM attachment_object_deletion WHERE job_state IN ('ready','retry_wait') AND next_retry_at<=clock_timestamp() AND attempts<8">>,
        []
    ),
    {ok, [_]} = elib_pg:query(
        <<"SELECT fn_v5_intent_pending_delete($1,'w403 owned expired upload','sweep')">>, [Key]
    ),
    ?assert(erlang:system_time(second) >= maps:get(expiry, T)),
    {
        fun attachment_delete_worker:run_batch/0,
        <<"SELECT job_state FROM attachment_object_deletion WHERE path=$1">>,
        [Key]
    };
deletion_intent(#{fixture := S, confirm_body := Body}) ->
    Scope = maps:get(scope, S),
    Org = maps:get(org_id, Scope),
    Ws = maps:get(workspace_id, Scope),
    Id = maps:get(asset_id, Body),
    {ok, [_]} = elib_pg:query(
        <<"SELECT fn_v5_intent_eb_delete($1,$2,$3,'w403 owned expired upload')">>,
        [Org, Ws, Id]
    ),
    {
        fun() -> eb_pg_asset_delete_queue:drain(Org, Ws, 1) end,
        <<"SELECT job_state FROM enterprise_asset_delete_queue WHERE organization_id=$1 AND workspace_id=$2 AND asset_id=$3">>,
        [Org, Ws, Id]
    }.
