%%% elib_tsid_bootstrap_harness_tests — TSID first-boot bootstrap
%%% fault-injection harness tests (scenario level).
%%%
%%% User-approved 6-scenario harness contract. All scenarios are purely
%%% local (per-scenario tmpdir + injected funs) and never touch a real
%%% database. Each test builds the intermediate state directly on disk
%%% and observes the boot outcome through elib_tsid_guard:start_link
%%% (guard stop reasons arrive as gen_server {error, Reason}; the
%%% start_result/1 helper normalizes them to the contract shape
%%% {ok, Pid} | {stop, Reason}).
%%%
%%% Scenarios:
%%%   S1 first_boot_interrupted_after_persist:
%%%        durable slot exists, cutover manifest never written
%%%        -> {stop, blocked_legacy_writer}
%%%   S2 store_lost_after_manifest:
%%%        manifest on disk, both clock slots deleted
%%%        -> {stop, store_lost}
%%%   S3 scan_interrupted_is_idempotent:
%%%        scan_fun error -> stop, no manifest, store unpolluted;
%%%        retry with working scan -> READY, manifest floor == scan value
%%%   S4 legacy_writer_ack_takeover:
%%%        LEGACY_ACK env adopts existing floor, manifest mode=legacy_ack;
%%%        reboot without env -> READY, floor never regresses
%%%   S5 missing_table_bad_column:
%%%        schema-drift scan errors -> stop; catalog validation must
%%%        never silently pass
%%%   S6 manual_floor_paths:
%%%        manual_floor env -> READY, manifest floor ==
%%%        unix_ms - EPOCH (rel-ms domain), store fence >= floor,
%%%        scan_fun provably unused;
%%%        bad floor values -> {stop, {bootstrap_env, _}}
%%%
%%% Note on the exit-style scan injection: the decide contract types
%%% scan_fun as returning {ok, _} | {error, term()}, and does not
%%% specify handling of non-local exits from inside the fun, so S3
%%% injects the contract-shaped {error, killed} instead of exit(bomb).
-module(elib_tsid_bootstrap_harness_tests).

-include_lib("eunit/include/eunit.hrl").

-define(EPOCH_MS, 1735689600000).
-define(DC_BITS, 3).
-define(ACK_VALUE, "I-CONFIRM-OLD-WRITER-STOPPED").

%% One shared combined_node across scenarios, mirroring the guard
%% suite: the global TSID runtime publish is BEAM-wide and only accepts
%% idempotent same-config republish (F-12 anti-silent-reconfig), so a
%% per-scenario node number would fail every boot after the first.
%% Each scenario resets the runtime up front instead (see reset/0).
-define(NODE_S1, 210).
-define(NODE_S2, 210).
-define(NODE_S3, 210).
-define(NODE_S4, 210).
-define(NODE_S5, 210).
-define(NODE_S6, 210).

%% ===================================================================
%% S1 — first boot interrupted between store persist and manifest write
%% ===================================================================

first_boot_interrupted_after_persist_test() ->
    Scene = first_boot_interrupted_after_persist,
    Root = tmp_root("s1"),
    Node = ?NODE_S1,
    %% Durable slot exists (persist already succeeded), cutover manifest
    %% never written: the first boot died in between.
    {ok, S0} = elib_tsid_store:open(store_fresh_cfg(Root, Node)),
    {ok, _S1} = elib_tsid_store:persist(S0, 5000),
    assert_manifest_absent(Scene, manifest_path(Root, Node)),
    %% Conservative semantics: refuse the boot, the old writer has not
    %% been proven stopped.
    StartRes = start_trapped((base_cfg(Root, Node))#{store_bootstrap => existing}),
    expect_stop_reason(Scene, blocked_legacy_writer, StartRes),
    %% The refusal must be side-effect free: floor intact, no manifest.
    {ok, R} = elib_tsid_store:open(store_existing_cfg(Root, Node)),
    assert_safe_before_exact(Scene, 5000, elib_tsid_store:status(R)),
    assert_manifest_absent(Scene, manifest_path(Root, Node)),
    cleanup_root(Root).

%% ===================================================================
%% S2 — manifest survives, both durable clock slots are lost
%% ===================================================================

store_lost_after_manifest_test() ->
    Scene = store_lost_after_manifest,
    Root = tmp_root("s2"),
    Node = ?NODE_S2,
    %% Full pristine first boot with a fake scan: manifest must land.
    ScanF = fun(_Ctx) -> {ok, #{floor_safe_before => 9000}} end,
    Cfg1 = (base_cfg(Root, Node))#{store_bootstrap => fresh, bootstrap_scan_fun => ScanF},
    P1 = expect_ready(Cfg1, Scene),
    StoreDir = elib_tsid_guard:store_dir(P1),
    assert_manifest_present(Scene, manifest_path(Root, Node)),
    stop_guard(P1),
    %% Simulate loss of the durable slots (disk lost, manifest survives).
    remove_if_exists(elib_tsid_store:slot_path(StoreDir, a)),
    remove_if_exists(elib_tsid_store:slot_path(StoreDir, b)),
    %% Restart must refuse: the manifest proves a floor existed and the
    %% store no longer carries it.
    StartRes = start_trapped((base_cfg(Root, Node))#{store_bootstrap => existing}),
    expect_stop_reason(Scene, store_lost, StartRes),
    cleanup_root(Root).

%% ===================================================================
%% S3 — interrupted scan stops cleanly; retry is deterministic
%% ===================================================================

scan_interrupted_is_idempotent_test() ->
    Scene = scan_interrupted_is_idempotent,
    Root = tmp_root("s3"),
    Node = ?NODE_S3,
    %% Contract-shaped error return (see module note on exit-style).
    BombF = fun(_Ctx) -> {error, killed} end,
    R1 = start_trapped((base_cfg(Root, Node))#{
        store_bootstrap => fresh, bootstrap_scan_fun => BombF
    }),
    expect_stop_scan(Scene, R1),
    assert_manifest_absent(Scene, manifest_path(Root, Node)),
    assert_store_unpolluted(Scene, Root, Node),
    %% Same disk state, working scan: boot must succeed deterministically.
    OkF = fun(_Ctx) -> {ok, #{floor_safe_before => 7777}} end,
    Cfg2 = (base_cfg(Root, Node))#{store_bootstrap => fresh, bootstrap_scan_fun => OkF},
    P2 = expect_ready(Cfg2, Scene),
    stop_guard(P2),
    case elib_tsid_bootstrap:read_manifest(manifest_path(Root, Node)) of
        {ok, #{floor_safe_before := 7777}} -> ok;
        Other -> erlang:error({bootstrap_harness_failed, Scene, {manifest_floor, 7777}, Other})
    end,
    {ok, R} = elib_tsid_store:open(store_existing_cfg(Root, Node)),
    assert_safe_before_at_least(Scene, 7777, elib_tsid_store:status(R)),
    cleanup_root(Root).

%% ===================================================================
%% S4 — operator ACK adopts legacy floor; reboot keeps it
%% ===================================================================

legacy_writer_ack_takeover_test() ->
    Scene = legacy_writer_ack_takeover,
    Root = tmp_root("s4"),
    Node = ?NODE_S4,
    %% Legacy state identical to S1: durable floor, no manifest.
    {ok, S0} = elib_tsid_store:open(store_fresh_cfg(Root, Node)),
    {ok, _S1} = elib_tsid_store:persist(S0, 5000),
    %% Only the ACK query answers; MODE/FLOOR queries stay unset.
    AckF =
        fun
            ("IMBOY_TSID_BOOTSTRAP_LEGACY_ACK") -> ?ACK_VALUE;
            (_) -> false
        end,
    Cfg1 = (base_cfg(Root, Node))#{store_bootstrap => existing, bootstrap_env_fun => AckF},
    P1 = expect_ready(Cfg1, Scene),
    F1 =
        case elib_tsid_bootstrap:read_manifest(manifest_path(Root, Node)) of
            {ok, #{mode := legacy_ack, floor_safe_before := F}} ->
                F;
            Other ->
                erlang:error({bootstrap_harness_failed, Scene, {manifest_mode, legacy_ack}, Other})
        end,
    stop_guard(P1),
    %% Reboot with no env at all: manifest present -> proceed_existing.
    P2 = expect_ready((base_cfg(Root, Node))#{store_bootstrap => existing}, Scene),
    stop_guard(P2),
    {ok, R} = elib_tsid_store:open(store_existing_cfg(Root, Node)),
    assert_safe_before_at_least(Scene, F1, elib_tsid_store:status(R)),
    cleanup_root(Root).

%% ===================================================================
%% S5 — schema drift scan errors must never pass silently
%% ===================================================================

missing_table_bad_column_test() ->
    Scene = missing_table_bad_column,
    Root = tmp_root("s5"),
    Node = ?NODE_S5,
    DriftF =
        fun(_Ctx) -> {error, {schema_drift, #{missing => [some_table], wrong_type => []}}} end,
    R1 = start_trapped((base_cfg(Root, Node))#{
        store_bootstrap => fresh, bootstrap_scan_fun => DriftF
    }),
    expect_stop_scan(Scene, R1),
    assert_manifest_absent(Scene, manifest_path(Root, Node)),
    assert_store_unpolluted(Scene, Root, Node),
    BadPkF = fun(_Ctx) -> {error, {unclassified_primary_keys, [x]}} end,
    R2 = start_trapped((base_cfg(Root, Node))#{
        store_bootstrap => fresh, bootstrap_scan_fun => BadPkF
    }),
    expect_stop_scan(Scene, R2),
    assert_manifest_absent(Scene, manifest_path(Root, Node)),
    assert_store_unpolluted(Scene, Root, Node),
    cleanup_root(Root).

%% ===================================================================
%% S6 — manual_floor happy path plus the invalid-value matrix
%% ===================================================================

manual_floor_paths_test() ->
    Scene = manual_floor_paths,
    Root = tmp_root("s6"),
    Node = ?NODE_S6,
    %% EPOCH + 10_000_000 ms: after the epoch, inside the 42-bit range.
    %% floor_safe_before is relative-millisecond (same domain as the
    %% store's safe_before), exactly the manual floor instant.
    UnixMs = ?EPOCH_MS + 10000000,
    ExpectFloor = UnixMs - ?EPOCH_MS,
    %% Provably unused: if the guard scans in manual mode the boot dies.
    NeverScan = fun(_Ctx) -> exit(bootstrap_scan_must_not_run) end,
    %% Frozen clock == the manual floor instant: no catch-up wait, no
    %% renew tick can move the durable floor before we read it back.
    WallF = fun(millisecond) -> UnixMs end,
    Cfg =
        (base_cfg(Root, Node))#{
            store_bootstrap => fresh,
            bootstrap_env_fun => manual_floor_env(integer_to_list(UnixMs)),
            bootstrap_scan_fun => NeverScan,
            wall_clock_ms => WallF
        },
    Pid = expect_ready(Cfg, Scene),
    stop_guard(Pid),
    %% The decision itself is recorded exactly on the manifest...
    {ok, M} = elib_tsid_bootstrap:read_manifest(manifest_path(Root, Node)),
    ?assertEqual(ExpectFloor, maps:get(floor_safe_before, M)),
    %% ...while the store's durable fence is monotonic (boot_ready renews
    %% it above the floor), so >= is the correct store-side invariant.
    {ok, R} = elib_tsid_store:open(store_existing_cfg(Root, Node)),
    assert_safe_before_at_least(Scene, ExpectFloor, elib_tsid_store:status(R)),
    cleanup_root(Root),
    %% Invalid floors: missing / non-integer / garbage / negative /
    %% earlier than the epoch — each refuses before any scan or write.
    Bads = [false, "12.5", "abc", "-5", integer_to_list(?EPOCH_MS - 1)],
    lists:foldl(
        fun(Bad, I) ->
            SubRoot = lists:flatten(io_lib:format("~s-bad-~b", [Root, I])),
            ok = filelib:ensure_dir(SubRoot ++ "/x"),
            SubCfg =
                (base_cfg(SubRoot, Node))#{
                    store_bootstrap => fresh,
                    bootstrap_env_fun => manual_floor_env(Bad),
                    bootstrap_scan_fun => NeverScan
                },
            expect_stop_env({Scene, bad_floor, I}, start_trapped(SubCfg)),
            cleanup_root(SubRoot),
            I + 1
        end,
        1,
        Bads
    ),
    ok.

%% ===================================================================
%% helpers
%% ===================================================================

tmp_root(Prefix) ->
    Dir =
        "/tmp/tsid_bootstrap_harness_" ++ Prefix ++ "_" ++
            integer_to_list(erlang:unique_integer([positive])),
    ok = filelib:ensure_dir(Dir ++ "/x"),
    Dir.

%% Mirrors elib_tsid_guard_tests:fast_cfg/1 (small fence window so the
%% guard's own validity invariant holds; registry lock, no flock need).
base_cfg(Root, Node) ->
    #{
        root => Root,
        combined_node => Node,
        dc_bits => ?DC_BITS,
        names => [user, group_info],
        lock_provider => registry,
        max_logical_lead_ms => 50,
        fence_window_ms => 100,
        fence_renew_margin_ms => 20,
        startup_clock_wait_timeout_ms => 1000,
        capacity_wait_timeout_ms => 5000
    }.

store_fresh_cfg(Root, Node) ->
    #{root => Root, combined_node => Node, dc_bits => ?DC_BITS, store_bootstrap => fresh}.

store_existing_cfg(Root, Node) ->
    #{root => Root, combined_node => Node, dc_bits => ?DC_BITS, store_bootstrap => existing}.

node_dir(Node) ->
    lists:flatten(io_lib:format("node-~4..0B", [Node])).

manifest_path(Root, Node) ->
    filename:join([Root, node_dir(Node), "tsid.bootstrap"]).

%% Cutover manifest env: manual mode with a raw floor value (false =
%% unset, exercising the "missing floor" branch).
manual_floor_env(FloorRaw) ->
    fun
        ("IMBOY_TSID_BOOTSTRAP_MODE") -> "manual_floor";
        ("IMBOY_TSID_BOOTSTRAP_FLOOR_UNIX_MS") -> FloorRaw;
        (_) -> false
    end.

%% Runs start_link inside a trap_exit helper so a failing boot's exit
%% signal cannot kill the test process (same style as
%% elib_tsid_guard_tests:start_trapped/1).
start_trapped(Cfg) ->
    Parent = self(),
    Helper = spawn(fun() ->
        process_flag(trap_exit, true),
        R =
            try
                elib_tsid_guard:start_link(Cfg)
            catch
                _:E -> {error, E}
            end,
        %% Drain the trapped EXIT letter so it cannot leak.
        receive
            {'EXIT', _, _} -> ok
        after 0 -> ok
        end,
        Parent ! {start_trapped, self(), R}
    end),
    receive
        {start_trapped, Helper, R} -> R
    after 10000 ->
        error(helper_timeout)
    end.

%% Normalizes a trapped start result to the contract shape
%% {ok, Pid} | {stop, Reason}: gen_server surfaces an init {stop, R}
%% as {error, R} at the call site.
start_result({ok, Pid}) -> {ok, Pid};
start_result({error, Reason}) -> {stop, Reason};
start_result({stop, Reason}) -> {stop, Reason}.

%% Successful boots MUST start_link directly from the test process:
%% the gen_server's parent has to outlive the guard, and a short-lived
%% start_trapped helper kills the guard the instant it exits (parent
%% EXIT). start_trapped remains for boots expected to stop.
expect_ready(Cfg, Scene) ->
    case elib_tsid_guard:start_link(Cfg) of
        {ok, Pid} ->
            wait_ready(Pid, Scene),
            Pid;
        Other ->
            erlang:error({bootstrap_harness_failed, Scene, {ok, pid}, Other})
    end.

expect_stop_reason(Scene, Reason, StartRes) ->
    case start_result(StartRes) of
        {stop, Reason} -> ok;
        Other -> erlang:error({bootstrap_harness_failed, Scene, {stop, Reason}, Other})
    end.

expect_stop_scan(Scene, StartRes) ->
    case start_result(StartRes) of
        {stop, {bootstrap_scan, _}} ->
            ok;
        Other ->
            erlang:error({bootstrap_harness_failed, Scene, {stop, {bootstrap_scan, '_'}}, Other})
    end.

expect_stop_env(Scene, StartRes) ->
    case start_result(StartRes) of
        {stop, {bootstrap_env, _}} ->
            ok;
        Other ->
            erlang:error({bootstrap_harness_failed, Scene, {stop, {bootstrap_env, '_'}}, Other})
    end.

stop_guard(Pid) ->
    elib_tsid_guard:stop(Pid),
    ok = wait_dead(Pid, 50),
    %% The guard's publish leaves a BEAM-wide runtime behind; the next
    %% scenario must start from a clean slate.
    elib_tsid:reset_for_test().

wait_ready(Pid, Scene) ->
    wait_ready(Pid, Scene, 40).

wait_ready(_Pid, Scene, 0) ->
    erlang:error({bootstrap_harness_failed, Scene, guard_not_ready});
wait_ready(Pid, Scene, N) ->
    case elib_tsid_guard:status(Pid) of
        ready ->
            ok;
        _ ->
            timer:sleep(50),
            wait_ready(Pid, Scene, N - 1)
    end.

wait_dead(_Pid, 0) ->
    error(still_alive);
wait_dead(Pid, N) ->
    case is_process_alive(Pid) of
        false ->
            ok;
        true ->
            timer:sleep(100),
            wait_dead(Pid, N - 1)
    end.

assert_manifest_absent(Scene, Path) ->
    case elib_tsid_bootstrap:read_manifest(Path) of
        absent -> ok;
        Other -> erlang:error({bootstrap_harness_failed, Scene, manifest_absent, Other})
    end.

assert_manifest_present(Scene, Path) ->
    case elib_tsid_bootstrap:read_manifest(Path) of
        {ok, _} -> ok;
        Other -> erlang:error({bootstrap_harness_failed, Scene, manifest_present, Other})
    end.

%% Proves a failed boot left the store without any durable floor.
assert_store_unpolluted(Scene, Root, Node) ->
    case elib_tsid_store:open(store_existing_cfg(Root, Node)) of
        {error, no_valid_slot} -> ok;
        Other -> erlang:error({bootstrap_harness_failed, Scene, {store_open, no_valid_slot}, Other})
    end.

assert_safe_before_exact(Scene, Want, Status) ->
    case Status of
        #{safe_before := Want} -> ok;
        Other -> erlang:error({bootstrap_harness_failed, Scene, {safe_before, Want}, Other})
    end.

assert_safe_before_at_least(Scene, Min, Status) ->
    case Status of
        #{safe_before := SB} when SB >= Min -> ok;
        Other -> erlang:error({bootstrap_harness_failed, Scene, {safe_before_at_least, Min}, Other})
    end.

remove_if_exists(Path) ->
    case file:delete(Path) of
        ok -> ok;
        {error, enoent} -> ok;
        {error, R} -> erlang:error({bootstrap_harness_failed, remove_failed, Path, R})
    end.

%% Best-effort recursive tmpdir cleanup; errors ignored (the dir may
%% already be gone, or an assertion aborted before anything existed).
cleanup_root(Path) ->
    case file:list_dir(Path) of
        {ok, Entries} ->
            lists:foreach(fun(E) -> cleanup_root(filename:join(Path, E)) end, Entries),
            _ = file:del_dir(Path);
        {error, _} ->
            _ = file:delete(Path)
    end,
    ok.
