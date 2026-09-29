%%% elib_tsid_bootstrap_tests — first-boot bootstrap state machine tests
%%%
%%% All tests run against a tmpdir with injected funs (env / scan /
%%% wall clock); no database connection is ever made. Covers every
%%% branch of the user-approved state matrix plus manifest encoding
%%% boundaries and the durable-write protocol surface.
-module(elib_tsid_bootstrap_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

-define(NODE, 7).
-define(EPOCH_MS, 1735689600000).
-define(MAX_REL_TS, ((1 bsl 42) - 1)).
-define(ENV_MODE, "IMBOY_TSID_BOOTSTRAP_MODE").
-define(ENV_FLOOR, "IMBOY_TSID_BOOTSTRAP_FLOOR_UNIX_MS").
-define(ENV_ACK, "IMBOY_TSID_BOOTSTRAP_LEGACY_ACK").
-define(ACK_VALUE, "I-CONFIRM-OLD-WRITER-STOPPED").

%% ===================================================================
%% Helpers
%% ===================================================================

%% 32-byte digest with the same shape as elib_tsid_catalog:digest().
digest(N) ->
    <<N:256/unsigned-big>>.

tmp_path() ->
    Dir = "/tmp/tsid_bootstrap_test_" ++ integer_to_list(erlang:unique_integer([positive])),
    ok = filelib:ensure_dir(Dir ++ "/x"),
    filename:join(Dir, "tsid.bootstrap").

env(Map) ->
    fun(Name) ->
        case maps:find(Name, Map) of
            {ok, V} ->
                V;
            error ->
                false
        end
    end.

%% Default ctx: pristine shape (no store error, floor 0, no manifest).
ctx(Overrides) ->
    Base = #{
        store_floor => 0,
        catalog_digest => digest(1),
        combined_node => ?NODE,
        manifest_path => tmp_path(),
        env_fun => env(#{}),
        scan_fun => fun(_Opts) -> {ok, #{floor_safe_before => 100}} end,
        wall_clock_fun => fun(millisecond) -> ?EPOCH_MS + 5000 end
    },
    maps:merge(Base, Overrides).

manifest_map(Floor) ->
    #{
        mode => auto_scan,
        floor_safe_before => Floor,
        combined_node => ?NODE,
        catalog_digest => digest(1),
        created_at_rel_ms => 5000
    }.

%% ===================================================================
%% Pristine branches
%% ===================================================================

pristine_auto_scan_test() ->
    C = ctx(#{scan_fun => fun(#{catalog := _Cat}) -> {ok, #{floor_safe_before => 12345}} end}),
    R = elib_tsid_bootstrap:decide(C),
    ?assertMatch(
        {ok, #{
            action := proceed_floor,
            floor_safe_before := 12345,
            mode := auto_scan,
            combined_node := ?NODE,
            created_at_rel_ms := 5000
        }},
        R
    ),
    {ok, #{catalog_digest := D}} = R,
    ?assertEqual(digest(1), D).

pristine_auto_scan_empty_db_test() ->
    %% Empty database: scan floor 0 is legal (guard starts from wall clock).
    C = ctx(#{scan_fun => fun(_) -> {ok, #{floor_safe_before => 0}} end}),
    ?assertMatch(
        {ok, #{action := proceed_floor, floor_safe_before := 0, mode := auto_scan}},
        elib_tsid_bootstrap:decide(C)
    ).

scan_receives_catalog_opt_test() ->
    Self = self(),
    C =
        ctx(#{
            scan_fun =>
                fun(Opts) ->
                    Self ! {scan_opts, Opts},
                    {ok, #{floor_safe_before => 1}}
                end
        }),
    {ok, _} = elib_tsid_bootstrap:decide(C),
    receive
        {scan_opts, Opts} ->
            ?assert(maps:is_key(catalog, Opts))
    after 1000 ->
        error(no_scan_call)
    end.

pristine_manual_floor_test() ->
    UnixMs = ?EPOCH_MS + 1000,
    ExpFloor = 1000,
    C =
        ctx(#{
            env_fun =>
                env(#{?ENV_MODE => "manual_floor", ?ENV_FLOOR => integer_to_list(UnixMs)})
        }),
    R = elib_tsid_bootstrap:decide(C),
    ?assertMatch(
        {ok, #{action := proceed_floor, floor_safe_before := ExpFloor, mode := manual_floor}},
        R
    ).

%% ===================================================================
%% Legacy writer blocking / LEGACY_ACK adoption
%% ===================================================================

blocked_legacy_writer_test() ->
    C = ctx(#{store_floor => 5000}),
    ?assertEqual({stop, blocked_legacy_writer}, elib_tsid_bootstrap:decide(C)).

legacy_ack_adopt_test() ->
    C = ctx(#{store_floor => 5000, env_fun => env(#{?ENV_ACK => ?ACK_VALUE})}),
    R = elib_tsid_bootstrap:decide(C),
    ?assertMatch(
        {ok, #{
            action := adopt_existing,
            floor_safe_before := 5000,
            mode := legacy_ack,
            combined_node := ?NODE,
            created_at_rel_ms := 5000
        }},
        R
    ),
    {ok, #{catalog_digest := D}} = R,
    ?assertEqual(digest(1), D).

adopt_then_write_manifest_then_restart_test() ->
    %% Guard integration shape: the adopt map feeds write_manifest/2
    %% verbatim; the next decide (manifest present, floor present)
    %% must be proceed_existing.
    C1 = ctx(#{store_floor => 5000, env_fun => env(#{?ENV_ACK => ?ACK_VALUE})}),
    {ok, Adopt} = elib_tsid_bootstrap:decide(C1),
    ok = elib_tsid_bootstrap:write_manifest(maps:get(manifest_path, C1), Adopt),
    C2 = ctx(#{store_floor => 5000, manifest_path => maps:get(manifest_path, C1)}),
    ?assertMatch({ok, #{action := proceed_existing}}, elib_tsid_bootstrap:decide(C2)).

proceed_floor_then_restart_test() ->
    %% Normal first boot: proceed_floor map feeds write_manifest/2
    %% verbatim (extra `action` key ignored), restart is proceed_existing.
    C1 = ctx(#{}),
    {ok, Proceed} = elib_tsid_bootstrap:decide(C1),
    ok = elib_tsid_bootstrap:write_manifest(maps:get(manifest_path, C1), Proceed),
    C2 = ctx(#{store_floor => 100, manifest_path => maps:get(manifest_path, C1)}),
    ?assertMatch({ok, #{action := proceed_existing}}, elib_tsid_bootstrap:decide(C2)).

%% ===================================================================
%% store_lost / store_corrupt / identity / catalog_changed
%% ===================================================================

store_lost_floor_cleared_test() ->
    C = ctx(#{}),
    P = maps:get(manifest_path, C),
    ok = elib_tsid_bootstrap:write_manifest(P, manifest_map(100)),
    ?assertEqual({stop, store_lost}, elib_tsid_bootstrap:decide(C)).

%% no_valid_slot (both slots absent) is manifest-aware: without a
%% bootstrap manifest the node dir is pristine and boot proceeds via
%% the scan path; with a manifest it is store_lost.
pristine_no_valid_slot_is_pristine_test() ->
    C =
        ctx(#{
            store_open_error => no_valid_slot,
            scan_fun => fun(_) -> {ok, #{floor_safe_before => 7}} end
        }),
    ?assertMatch(
        {ok, #{action := proceed_floor, floor_safe_before := 7, mode := auto_scan}},
        elib_tsid_bootstrap:decide(C)
    ).

store_lost_no_valid_slot_with_manifest_test() ->
    %% Manifest present + both slots absent -> the durable floor is gone.
    C0 = ctx(#{}),
    P = maps:get(manifest_path, C0),
    ok = elib_tsid_bootstrap:write_manifest(P, manifest_map(100)),
    C = C0#{store_open_error => no_valid_slot},
    ?assertEqual({stop, store_lost}, elib_tsid_bootstrap:decide(C)).

store_corrupt_test() ->
    ?assertEqual(
        {stop, store_corrupt},
        elib_tsid_bootstrap:decide(ctx(#{store_open_error => corrupt_no_valid}))
    ),
    ?assertEqual(
        {stop, store_corrupt},
        elib_tsid_bootstrap:decide(ctx(#{store_open_error => split_brain}))
    ).

store_identity_mismatch_store_internal_test() ->
    ?assertEqual(
        {stop, store_identity_mismatch},
        elib_tsid_bootstrap:decide(ctx(#{store_open_error => {manifest, bad_manifest}}))
    ),
    ?assertEqual(
        {stop, store_identity_mismatch},
        elib_tsid_bootstrap:decide(ctx(#{store_open_error => {config_mismatch, #{}}}))
    ).

store_identity_mismatch_manifest_test() ->
    C = ctx(#{store_floor => 100}),
    P = maps:get(manifest_path, C),
    ok = elib_tsid_bootstrap:write_manifest(P, (manifest_map(100))#{combined_node => 9}),
    ?assertEqual({stop, store_identity_mismatch}, elib_tsid_bootstrap:decide(C)).

catalog_changed_test() ->
    C = ctx(#{store_floor => 100}),
    P = maps:get(manifest_path, C),
    ok = elib_tsid_bootstrap:write_manifest(P, (manifest_map(100))#{catalog_digest => digest(2)}),
    ?assertEqual({stop, catalog_changed}, elib_tsid_bootstrap:decide(C)).

proceed_existing_test() ->
    C = ctx(#{store_floor => 100}),
    P = maps:get(manifest_path, C),
    ok = elib_tsid_bootstrap:write_manifest(P, manifest_map(100)),
    ?assertMatch({ok, #{action := proceed_existing}}, elib_tsid_bootstrap:decide(C)).

decide_manifest_corrupt_test() ->
    C0 = ctx(#{}),
    P = maps:get(manifest_path, C0),
    ok = file:write_file(P, <<"garbage-not-a-manifest">>),
    C = ctx(#{store_floor => 100, manifest_path => P}),
    ?assertMatch({stop, {manifest_corrupt, _}}, elib_tsid_bootstrap:decide(C)).

%% ===================================================================
%% Env validation
%% ===================================================================

env_bad_mode_test() ->
    %% Trailing space must NOT be trimmed: exact match only.
    C = ctx(#{env_fun => env(#{?ENV_MODE => "auto "})}),
    ?assertEqual({stop, {bootstrap_env, {bad_mode, "auto "}}}, elib_tsid_bootstrap:decide(C)).

env_missing_floor_test() ->
    C = ctx(#{env_fun => env(#{?ENV_MODE => "manual_floor"})}),
    ?assertEqual({stop, {bootstrap_env, floor_unix_ms_missing}}, elib_tsid_bootstrap:decide(C)).

env_bad_floor_test() ->
    lists:foreach(
        fun(Bad) ->
            C =
                ctx(#{env_fun => env(#{?ENV_MODE => "manual_floor", ?ENV_FLOOR => Bad})}),
            Exp = {stop, {bootstrap_env, {bad_floor_unix_ms, Bad}}},
            ?assertEqual(Exp, elib_tsid_bootstrap:decide(C))
        end,
        ["", " 100", "100 ", "+100", "-100", "1_000", "0x10", "abc"]
    ).

env_floor_range_test() ->
    Low = ctx(#{
        env_fun => env(#{
            ?ENV_MODE => "manual_floor",
            ?ENV_FLOOR => integer_to_list(?EPOCH_MS - 1)
        })
    }),
    ?assertMatch(
        {stop, {bootstrap_env, {floor_unix_ms_out_of_range, _}}},
        elib_tsid_bootstrap:decide(Low)
    ),
    High =
        ctx(#{
            env_fun =>
                env(#{
                    ?ENV_MODE => "manual_floor",
                    ?ENV_FLOOR => integer_to_list(?EPOCH_MS + ?MAX_REL_TS + 1)
                })
        }),
    ?assertMatch(
        {stop, {bootstrap_env, {floor_unix_ms_out_of_range, _}}},
        elib_tsid_bootstrap:decide(High)
    ),
    MaxFloor = (1 bsl 42) - 1,
    MaxOk =
        ctx(#{
            env_fun =>
                env(#{
                    ?ENV_MODE => "manual_floor",
                    ?ENV_FLOOR => integer_to_list(?EPOCH_MS + ?MAX_REL_TS)
                })
        }),
    R1 = elib_tsid_bootstrap:decide(MaxOk),
    ?assertMatch(
        {ok, #{action := proceed_floor, floor_safe_before := MaxFloor, mode := manual_floor}},
        R1
    ),
    EpochOk =
        ctx(#{
            env_fun =>
                env(#{
                    ?ENV_MODE => "manual_floor",
                    ?ENV_FLOOR => integer_to_list(?EPOCH_MS)
                })
        }),
    R2 = elib_tsid_bootstrap:decide(EpochOk),
    ?assertMatch({ok, #{action := proceed_floor, floor_safe_before := 0}}, R2).

env_bad_ack_blocked_test() ->
    %% Wrong case and trailing space both fail: exact match only.
    lists:foreach(
        fun(BadAck) ->
            C = ctx(#{store_floor => 5000, env_fun => env(#{?ENV_ACK => BadAck})}),
            ?assertMatch(
                {stop, {bootstrap_env, {bad_legacy_ack, _}}},
                elib_tsid_bootstrap:decide(C)
            )
        end,
        ["i-confirm-old-writer-stopped", "I-CONFIRM-OLD-WRITER-STOPPED ", "yes"]
    ).

env_bad_ack_pristine_test() ->
    %% Pristine branch also rejects a wrongly-valued ACK (fail-closed,
    %% never silently ignored).
    C = ctx(#{env_fun => env(#{?ENV_ACK => "I-CONFIRM-OLD-WRITER-STOP"})}),
    ?assertMatch(
        {stop, {bootstrap_env, {bad_legacy_ack, _}}},
        elib_tsid_bootstrap:decide(C)
    ).

%% ===================================================================
%% scan_fun failure propagation
%% ===================================================================

scan_error_propagates_test() ->
    C = ctx(#{scan_fun => fun(_) -> {error, conn_refused} end}),
    ?assertEqual({stop, {bootstrap_scan, conn_refused}}, elib_tsid_bootstrap:decide(C)).

scan_invalid_result_test() ->
    C1 = ctx(#{scan_fun => fun(_) -> {ok, #{}} end}),
    ?assertMatch(
        {stop, {bootstrap_scan, {invalid_scan_result, _}}},
        elib_tsid_bootstrap:decide(C1)
    ),
    C2 = ctx(#{scan_fun => fun(_) -> {ok, #{floor_safe_before => -1}} end}),
    ?assertMatch(
        {stop, {bootstrap_scan, {invalid_floor, _}}},
        elib_tsid_bootstrap:decide(C2)
    ).

%% ===================================================================
%% mode_from_env / floor_from_env direct contracts
%% ===================================================================

mode_from_env_direct_test() ->
    ?assertEqual(auto_scan, elib_tsid_bootstrap:mode_from_env(fun(_) -> false end)),
    ?assertEqual(
        manual_floor,
        elib_tsid_bootstrap:mode_from_env(fun
            ("IMBOY_TSID_BOOTSTRAP_MODE") ->
                "manual_floor";
            (_) ->
                false
        end)
    ),
    ?assertMatch(
        {stop, {bootstrap_env, {bad_mode, "nope"}}},
        elib_tsid_bootstrap:mode_from_env(fun(_) -> "nope" end)
    ).

floor_from_env_direct_test() ->
    ?assertMatch(
        {stop, {bootstrap_env, floor_unix_ms_missing}},
        elib_tsid_bootstrap:floor_from_env(fun(_) -> false end)
    ),
    {ok, Floor} =
        elib_tsid_bootstrap:floor_from_env(fun
            ("IMBOY_TSID_BOOTSTRAP_FLOOR_UNIX_MS") ->
                integer_to_list(?EPOCH_MS + 2048);
            (_) ->
                false
        end),
    ?assertEqual(2048, Floor).

%% ===================================================================
%% Manifest encoding / decoding / durable write
%% ===================================================================

manifest_roundtrip_all_modes_test() ->
    P = tmp_path(),
    Cases =
        [{auto_scan, 0}, {manual_floor, 1000}, {legacy_ack, 4398046511103}],
    lists:foreach(
        fun({Mode, Floor}) ->
            ok = elib_tsid_bootstrap:write_manifest(P, (manifest_map(Floor))#{mode => Mode}),
            {ok, R} = elib_tsid_bootstrap:read_manifest(P),
            ?assertEqual(
                #{
                    version => 1,
                    mode => Mode,
                    combined_node => ?NODE,
                    floor_safe_before => Floor,
                    created_at_rel_ms => 5000,
                    catalog_digest => digest(1)
                },
                R
            )
        end,
        Cases
    ).

manifest_file_mode_0600_test() ->
    P = tmp_path(),
    ok = elib_tsid_bootstrap:write_manifest(P, manifest_map(100)),
    {ok, #file_info{mode = Mode}} = file:read_file_info(P),
    ?assertEqual(8#600, Mode band 8#777).

manifest_overwrite_test() ->
    P = tmp_path(),
    ok = elib_tsid_bootstrap:write_manifest(P, manifest_map(100)),
    ok = elib_tsid_bootstrap:write_manifest(P, manifest_map(200)),
    {ok, #{floor_safe_before := 200}} = elib_tsid_bootstrap:read_manifest(P).

manifest_no_temp_leftover_test() ->
    P = tmp_path(),
    ok = elib_tsid_bootstrap:write_manifest(P, manifest_map(100)),
    ok = elib_tsid_bootstrap:write_manifest(P, manifest_map(300)),
    Files = [filename:basename(F) || F <- filelib:wildcard(filename:dirname(P) ++ "/*")],
    ?assertEqual(["tsid.bootstrap"], lists:sort(Files)).

manifest_crc_corrupt_rejected_test() ->
    P = tmp_path(),
    ok = elib_tsid_bootstrap:write_manifest(P, manifest_map(100)),
    {ok, Bin} = file:read_file(P),
    %% Flipping any single byte (body or crc area) must yield bad_crc.
    lists:foreach(
        fun(Pos) ->
            <<A:Pos/binary, Byte, B/binary>> = Bin,
            Bad = <<A/binary, (Byte bxor 16#FF), B/binary>>,
            BadPath = tmp_path(),
            ok = file:write_file(BadPath, Bad),
            ?assertEqual({error, bad_crc}, elib_tsid_bootstrap:read_manifest(BadPath))
        end,
        [0, 9, 12, 30, 61, 63]
    ).

manifest_length_corrupt_rejected_test() ->
    P = tmp_path(),
    ok = elib_tsid_bootstrap:write_manifest(P, manifest_map(100)),
    {ok, Bin} = file:read_file(P),
    Trunc = binary:part(Bin, 0, byte_size(Bin) - 1),
    ok = file:write_file(P, Trunc),
    ?assertEqual({error, bad_length}, elib_tsid_bootstrap:read_manifest(P)),
    ok = file:write_file(P, <<Bin/binary, 0>>),
    ?assertEqual({error, bad_length}, elib_tsid_bootstrap:read_manifest(P)).

manifest_bad_magic_version_mode_range_test() ->
    P = tmp_path(),
    Body1 = <<"XXXXXXXXX", 1:16/unsigned-big, 0:8, 7:16, 0:64, 0:64, (digest(1))/binary>>,
    ok = file:write_file(P, <<Body1/binary, (erlang:crc32(Body1)):32>>),
    ?assertEqual({error, bad_magic}, elib_tsid_bootstrap:read_manifest(P)),
    Body2 = <<"IMBTSIDB1", 2:16/unsigned-big, 0:8, 7:16, 0:64, 0:64, (digest(1))/binary>>,
    ok = file:write_file(P, <<Body2/binary, (erlang:crc32(Body2)):32>>),
    ?assertEqual({error, bad_version}, elib_tsid_bootstrap:read_manifest(P)),
    Body3 = <<"IMBTSIDB1", 1:16/unsigned-big, 3:8, 7:16, 0:64, 0:64, (digest(1))/binary>>,
    ok = file:write_file(P, <<Body3/binary, (erlang:crc32(Body3)):32>>),
    ?assertEqual({error, bad_mode}, elib_tsid_bootstrap:read_manifest(P)),
    Body4 = <<"IMBTSIDB1", 1:16/unsigned-big, 0:8, 2000:16, 0:64, 0:64, (digest(1))/binary>>,
    ok = file:write_file(P, <<Body4/binary, (erlang:crc32(Body4)):32>>),
    ?assertEqual({error, bad_range}, elib_tsid_bootstrap:read_manifest(P)).

write_manifest_field_validation_test() ->
    P = tmp_path(),
    ?assertMatch(
        {error, {bad_manifest_field, {mode, _}}},
        elib_tsid_bootstrap:write_manifest(P, (manifest_map(100))#{mode => wat})
    ),
    ?assertMatch(
        {error, {bad_manifest_field, {catalog_digest, _}}},
        elib_tsid_bootstrap:write_manifest(P, (manifest_map(100))#{catalog_digest => <<1, 2, 3>>})
    ),
    ?assertMatch(
        {error, {bad_manifest_field, {combined_node, _}}},
        elib_tsid_bootstrap:write_manifest(P, (manifest_map(100))#{combined_node => 2000})
    ),
    ?assertMatch(
        {error, {bad_manifest_field, {floor_safe_before, _}}},
        elib_tsid_bootstrap:write_manifest(P, (manifest_map(100))#{floor_safe_before => "x"})
    ),
    %% Missing fields are typed rejections, not crashes.
    ?assertMatch(
        {error, {bad_manifest_field, {combined_node, missing}}},
        elib_tsid_bootstrap:write_manifest(P, #{mode => auto_scan})
    ).

write_manifest_absent_dir_fails_typed_test() ->
    %% No parent dir: typed temp_create failure, not a crash.
    Base = "/tmp/tsid_bootstrap_absent_dir_" ++ integer_to_list(erlang:unique_integer([positive])),
    P = filename:join([Base, "sub", "tsid.bootstrap"]),
    ?assertMatch(
        {error, {temp_create, _}},
        elib_tsid_bootstrap:write_manifest(P, manifest_map(100))
    ).
