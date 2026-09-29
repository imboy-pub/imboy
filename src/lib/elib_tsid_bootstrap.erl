%%%-------------------------------------------------------------------
%%% @doc TSID 首启自举状态机（pristine-only，用户批准方案·步骤 5）
%%%
%%% 职责边界：本模块只「判定」并产出/校验割接 manifest；floor 的
%%% 持久化（elib_tsid_store:persist，AC-05D 持久化成功才发布）与
%%% fence 发布全部留在 elib_tsid_guard 手里。运行期永不自我升级：
%%% 从旧版 store 状态接管必须走显式的操作员确认（LEGACY_ACK）。
%%%
%%% 割接 manifest：<root>/node-<NNNN>/tsid.bootstrap，二进制定长 + CRC32，
%%% 魔数 IMBTSIDB1；写协议与 elib_tsid_store 的 durable 写一致：
%%% temp exclusive → write → sync → chmod 0600 → rename → dirsync。
%%% 字段：version(16)、mode(8: 0=auto_scan 1=manual_floor 2=legacy_ack)、
%%% combined_node(16)、floor_safe_before(64)、created_at_rel_ms(64)、
%%% catalog_digest(32B SHA-256)、CRC32。
%%%
%%% 状态判定矩阵（全部 FAIL 级：{stop, Reason}，不得降 warning）：
%%%   pristine = 无割接 manifest 且 store 无 durable floor
%%%     auto_scan    → scan_fun 求 floor → proceed_floor
%%%     manual_floor → IMBOY_TSID_BOOTSTRAP_FLOOR_UNIX_MS → proceed_floor
%%%   无 manifest 且 store 有 floor（旧 writer 未证明停写，或首启中断于
%%%     persist 之后、写 manifest 之前）→ {stop, blocked_legacy_writer}
%%%     除非 IMBOY_TSID_BOOTSTRAP_LEGACY_ACK=I-CONFIRM-OLD-WRITER-STOPPED
%%%     （操作员确认旧版停写；补写 manifest 后按 legacy_ack 接管现有 floor）
%%%   有 manifest 且 store floor 丢失/清零        → {stop, store_lost}
%%%   store 打开损坏（corrupt_no_valid/split_brain）→ {stop, store_corrupt}
%%%   身份/布局不符（store 内部 manifest 或本 manifest）
%%%                                              → {stop, store_identity_mismatch}
%%%   manifest 的 catalog_digest ≠ 当前 digest    → {stop, catalog_changed}
%%%   环境变量非法                                → {stop, {bootstrap_env, D}}
%%%   auto_scan 扫描失败                          → {stop, {bootstrap_scan, R}}
%%%
%%% 环境变量（非法值一律 {stop, ...}，不静默取默认）：
%%%   IMBOY_TSID_BOOTSTRAP_MODE          = auto_scan（缺省）| manual_floor
%%%   IMBOY_TSID_BOOTSTRAP_FLOOR_UNIX_MS = 整数 unix 毫秒（manual_floor 必填；
%%%                                        floor_safe_before = ms - EPOCH_MS）
%%%   IMBOY_TSID_BOOTSTRAP_LEGACY_ACK    = I-CONFIRM-OLD-WRITER-STOPPED
%%%
%%% Decision order (English notes below are implementation contract):
%%% store_open_error classification -> manifest read/identity -> digest
%%% -> floor existence -> pristine branch. Env vars are consumed only in
%%% the pristine / blocked branches, so a normal restart
%%% (proceed_existing) never fails on a stale bad env value.
%%%
%%% Unit contract: floor_safe_before lives in the SAME relative-millisecond
%%% domain as elib_tsid_store's safe_before (guard boot_with_floor compares
%%% it against wall-clock rel-ms directly). Scan derives it as
%%% slot_to_ts(max_slot) + 1; manual floor is unix_ms - EPOCH_MS.
%%% ctx.store_floor is caller-owned durable safe_before; decide only
%%% tests zero vs non-zero and passes it through verbatim on
%%% adopt_existing.
%%%
%%% Fail-closed extras beyond the matrix above (all {stop, _}):
%%%   {store_open_error, E}     unknown store open error (e.g. symlink)
%%%   {manifest_corrupt, R}     bootstrap manifest present but undecodable
%%%   {store_floor_invalid, V}  ctx.store_floor not a non-negative integer
%%%   {clock_before_epoch, D}   wall clock before EPOCH at proceed/adopt
%%%-------------------------------------------------------------------
-module(elib_tsid_bootstrap).

-export([decide/1, write_manifest/2, read_manifest/1, mode_from_env/1, floor_from_env/1]).

%% Frozen cross-references to elib_tsid.erl: ?EPOCH_MS / ?MAX_REL_TS.
%% Changing the epoch or the 42-bit width must update both sides.
-define(EPOCH_MS, 1735689600000).
-define(MAX_REL_TS, ((1 bsl 42) - 1)).

%% floor_safe_before is relative-millisecond (same domain as the store's
%% safe_before), so the upper bound is ?MAX_REL_TS itself.

%% Bootstrap manifest (IMBTSIDB1): fixed 66 bytes = 62 body + 4 crc32.
-define(BOOT_MAGIC, <<"IMBTSIDB1">>).
-define(BOOT_VERSION, 1).
-define(BOOT_BODY_SIZE, 62).
-define(BOOT_RECORD_SIZE, 66).
%% Same as elib_tsid_store:?FILE_MODE (durable-write chmod tightening).
-define(FILE_MODE, 8#600).

%% Mode byte encoding inside the manifest.
-define(MODE_AUTO_SCAN, 0).
-define(MODE_MANUAL_FLOOR, 1).
-define(MODE_LEGACY_ACK, 2).

%% Env vars: values must match exactly (no trim / loose parsing).
-define(ENV_MODE, "IMBOY_TSID_BOOTSTRAP_MODE").
-define(ENV_FLOOR_UNIX_MS, "IMBOY_TSID_BOOTSTRAP_FLOOR_UNIX_MS").
-define(ENV_LEGACY_ACK, "IMBOY_TSID_BOOTSTRAP_LEGACY_ACK").
-define(LEGACY_ACK_VALUE, "I-CONFIRM-OLD-WRITER-STOPPED").

%% Ctx：
%%   store_floor      => store 打开后的 durable safe_before（0 = 无）
%%   store_open_error => elib_tsid_store:open 的错误（未提供 = 打开成功）
%%   catalog_digest   => elib_tsid_catalog:digest()
%%   combined_node    => 0..1023
%%   manifest_path    => 割接 manifest 绝对路径
%%   env_fun          => fun((string()) -> false | string())（缺省 os:getenv；测试注入）
%%   scan_fun         => fun((map()) -> {ok, #{floor_safe_before => N}} | {error, term()})
%%                       （缺省 elib_tsid_scan:scan；测试注入）
%%   wall_clock_fun   => fun((millisecond) -> integer())（缺省 system_time；测试注入）
%% 返回：
%%   {ok, #{action := proceed_existing}}                          正常重启
%%   {ok, #{action := proceed_floor, floor_safe_before := N, mode := M,
%%          catalog_digest := D}}                                 pristine 首启
%%   {ok, #{action := adopt_existing, floor_safe_before := S, mode => legacy_ack,
%%          catalog_digest := D}}                                 LEGACY_ACK 接管
%%   {stop, Reason}                                               FAIL 级终止
%% proceed_floor / adopt_existing 返回额外携带 combined_node 与
%% created_at_rel_ms：guard 持久化 floor 成功后可把该 map 原样交给
%% write_manifest/2（action 键被忽略）。
-spec decide(map()) -> {ok, map()} | {stop, term()}.
decide(Ctx) ->
    EnvF = maps:get(env_fun, Ctx, fun os:getenv/1),
    ScanF = maps:get(scan_fun, Ctx, fun elib_tsid_scan:scan/1),
    WallF = maps:get(wall_clock_fun, Ctx, fun erlang:system_time/1),
    case maps:get(store_open_error, Ctx, undefined) of
        undefined ->
            decide_manifest(Ctx, EnvF, ScanF, WallF);
        no_valid_slot ->
            %% Both slots absent: the durable floor is 0 whether the node
            %% dir is pristine or lost. Manifest presence disambiguates —
            %% absent -> pristine first boot, present -> store_lost.
            decide_manifest(Ctx, EnvF, ScanF, WallF);
        Other ->
            classify_store_open_error(Other)
    end.

%% store_open_error classification (matrix step 1). {manifest, _} and
%% {config_mismatch, _} are both store-side identity/layout mismatches;
%% no_valid_slot is handled in decide/1 (manifest-aware); anything
%% unknown fails closed with the raw error.
classify_store_open_error(corrupt_no_valid) ->
    {stop, store_corrupt};
classify_store_open_error(split_brain) ->
    {stop, store_corrupt};
classify_store_open_error({manifest, _}) ->
    {stop, store_identity_mismatch};
classify_store_open_error({config_mismatch, _}) ->
    {stop, store_identity_mismatch};
classify_store_open_error(Other) ->
    {stop, {store_open_error, Other}}.

decide_manifest(Ctx, EnvF, ScanF, WallF) ->
    EffectiveFloor = effective_floor(Ctx),
    case read_manifest(maps:get(manifest_path, Ctx)) of
        {ok, M} ->
            decide_manifest_ok(Ctx, M, EffectiveFloor);
        absent ->
            decide_pristine(Ctx, EnvF, ScanF, WallF, EffectiveFloor);
        {error, R} ->
            {stop, {manifest_corrupt, R}}
    end.

%% Store floor as seen by the state machine: an absent-slot open error
%% counts as floor 0 (see decide/1).
effective_floor(Ctx) ->
    case maps:get(store_open_error, Ctx, undefined) of
        no_valid_slot -> 0;
        _ -> maps:get(store_floor, Ctx, 0)
    end.

%% Manifest present: identity -> digest -> floor existence (matrix order).
decide_manifest_ok(Ctx, M, EffectiveFloor) ->
    case maps:get(combined_node, M) =:= maps:get(combined_node, Ctx) of
        false ->
            {stop, store_identity_mismatch};
        true ->
            case maps:get(catalog_digest, M) =:= maps:get(catalog_digest, Ctx) of
                false ->
                    {stop, catalog_changed};
                true ->
                    case EffectiveFloor of
                        0 ->
                            {stop, store_lost};
                        _ ->
                            {ok, #{action => proceed_existing}}
                    end
            end
    end.

%% No manifest: floor == 0 -> pristine first boot; floor > 0 -> a legacy
%% writer may still be running (or the first boot died between persist
%% and manifest write) -> blocked unless LEGACY_ACK confirms exactly.
decide_pristine(Ctx, EnvF, ScanF, WallF, EffectiveFloor) ->
    case check_legacy_ack(EnvF) of
        {stop, _} = Stop ->
            Stop;
        Ack ->
            case EffectiveFloor of
                0 ->
                    decide_fresh(Ctx, EnvF, ScanF, WallF);
                StoreFloor when is_integer(StoreFloor), StoreFloor > 0 ->
                    adopt_or_block(Ack, Ctx, StoreFloor, WallF);
                Bad ->
                    {stop, {store_floor_invalid, Bad}}
            end
    end.

adopt_or_block(confirmed, Ctx, StoreFloor, WallF) ->
    case now_rel_ms(WallF) of
        {stop, _} = Stop ->
            Stop;
        {ok, NowRel} ->
            {ok, #{
                action => adopt_existing,
                floor_safe_before => StoreFloor,
                mode => legacy_ack,
                catalog_digest => maps:get(catalog_digest, Ctx),
                combined_node => maps:get(combined_node, Ctx),
                created_at_rel_ms => NowRel
            }}
    end;
adopt_or_block(not_set, _Ctx, _StoreFloor, _WallF) ->
    {stop, blocked_legacy_writer}.

decide_fresh(Ctx, EnvF, ScanF, WallF) ->
    case mode_from_env(EnvF) of
        {stop, _} = Stop ->
            Stop;
        auto_scan ->
            fresh_auto_scan(Ctx, ScanF, WallF);
        manual_floor ->
            case floor_from_env(EnvF) of
                {ok, Floor} ->
                    proceed_floor(manual_floor, Floor, Ctx, WallF);
                {stop, _} = Stop ->
                    Stop
            end
    end.

fresh_auto_scan(Ctx, ScanF, WallF) ->
    Catalog = elib_tsid_catalog:primary_keys(),
    case ScanF(#{catalog => Catalog}) of
        {ok, #{floor_safe_before := Floor}} when
            is_integer(Floor), Floor >= 0, Floor =< ?MAX_REL_TS
        ->
            proceed_floor(auto_scan, Floor, Ctx, WallF);
        {ok, #{floor_safe_before := Bad}} ->
            {stop, {bootstrap_scan, {invalid_floor, Bad}}};
        {ok, Other} ->
            {stop, {bootstrap_scan, {invalid_scan_result, Other}}};
        {error, R} ->
            {stop, {bootstrap_scan, R}}
    end.

proceed_floor(Mode, Floor, Ctx, WallF) ->
    case now_rel_ms(WallF) of
        {stop, _} = Stop ->
            Stop;
        {ok, NowRel} ->
            {ok, #{
                action => proceed_floor,
                floor_safe_before => Floor,
                mode => Mode,
                catalog_digest => maps:get(catalog_digest, Ctx),
                combined_node => maps:get(combined_node, Ctx),
                created_at_rel_ms => NowRel
            }}
    end.

now_rel_ms(WallF) ->
    Unix = WallF(millisecond),
    Rel = Unix - ?EPOCH_MS,
    case Rel >= 0 of
        true ->
            {ok, Rel};
        false ->
            {stop, {clock_before_epoch, #{now_unix_ms => Unix}}}
    end.

%% LEGACY_ACK is validated whenever it is set (pristine included): a
%% wrongly-valued ACK means the operator misunderstood the state, which
%% must fail closed instead of being silently ignored.
check_legacy_ack(EnvF) ->
    case EnvF(?ENV_LEGACY_ACK) of
        false ->
            not_set;
        ?LEGACY_ACK_VALUE ->
            confirmed;
        Other ->
            {stop, {bootstrap_env, {bad_legacy_ack, Other}}}
    end.

-spec mode_from_env(fun((string()) -> false | string())) ->
    auto_scan | manual_floor | {stop, term()}.
mode_from_env(EnvF) ->
    case EnvF(?ENV_MODE) of
        false ->
            auto_scan;
        "auto_scan" ->
            auto_scan;
        "manual_floor" ->
            manual_floor;
        Other ->
            {stop, {bootstrap_env, {bad_mode, Other}}}
    end.

-spec floor_from_env(fun((string()) -> false | string())) ->
    {ok, non_neg_integer()} | {stop, term()}.
floor_from_env(EnvF) ->
    case EnvF(?ENV_FLOOR_UNIX_MS) of
        false ->
            {stop, {bootstrap_env, floor_unix_ms_missing}};
        Value ->
            case parse_decimal(Value) of
                {ok, UnixMs} ->
                    rel_floor(UnixMs, Value);
                {error, _} ->
                    {stop, {bootstrap_env, {bad_floor_unix_ms, Value}}}
            end
    end.

%% unix_ms must satisfy 0 <= unix_ms - EPOCH <= MAX_REL_TS;
%% floor_safe_before = unix_ms - EPOCH (relative-millisecond domain,
%% identical to elib_tsid_store's safe_before).
rel_floor(UnixMs, Value) ->
    Rel = UnixMs - ?EPOCH_MS,
    case Rel >= 0 andalso Rel =< ?MAX_REL_TS of
        true ->
            {ok, Rel};
        false ->
            {stop, {bootstrap_env, {floor_unix_ms_out_of_range, Value}}}
    end.

%% Strict decimal parsing: ASCII digits only, no sign, no whitespace,
%% no trim — the value must be exact. The empty string is not a number.
parse_decimal([]) ->
    {error, not_decimal_digits};
parse_decimal(Value) when is_list(Value) ->
    parse_decimal_digits(Value, 0);
parse_decimal(_) ->
    {error, not_a_string}.

parse_decimal_digits([], Acc) ->
    {ok, Acc};
parse_decimal_digits([C | Rest], Acc) when C >= $0, C =< $9 ->
    parse_decimal_digits(Rest, Acc * 10 + (C - $0));
parse_decimal_digits(_, _) ->
    {error, not_decimal_digits}.

%% ===================================================================
%% Bootstrap manifest durable write / read
%% ===================================================================

%% Manifest accepts the exact map decide() returns on proceed_floor /
%% adopt_existing (extra keys such as `action` are ignored); the guard
%% calls this after persisting the floor.
-spec write_manifest(file:filename_all(), map()) -> ok | {error, term()}.
write_manifest(Path, Manifest) ->
    case boot_manifest_body(Manifest) of
        {error, _} = E ->
            E;
        {ok, Body} ->
            Bin = <<Body/binary, (erlang:crc32(Body)):32/unsigned-big>>,
            durable_write(Path, Bin)
    end.

boot_manifest_body(Manifest) ->
    case mode_field(Manifest) of
        {error, _} = E ->
            E;
        {ok, ModeByte} ->
            Fields = [
                node_field(Manifest),
                floor_field(Manifest),
                created_at_field(Manifest),
                digest_field(Manifest)
            ],
            case collect_ok(Fields, []) of
                {ok, [Node, Floor, Created, Digest]} ->
                    {ok,
                        <<?BOOT_MAGIC/binary, ?BOOT_VERSION:16/unsigned-big,
                            ModeByte:8/unsigned-big, Node:16/unsigned-big, Floor:64/unsigned-big,
                            Created:64/unsigned-big, Digest/binary>>};
                {error, _} = E ->
                    E
            end
    end.

field(Manifest, Key) ->
    case maps:find(Key, Manifest) of
        {ok, V} ->
            V;
        error ->
            missing
    end.

mode_field(Manifest) ->
    case field(Manifest, mode) of
        auto_scan ->
            {ok, ?MODE_AUTO_SCAN};
        manual_floor ->
            {ok, ?MODE_MANUAL_FLOOR};
        legacy_ack ->
            {ok, ?MODE_LEGACY_ACK};
        Other ->
            {error, {bad_manifest_field, {mode, Other}}}
    end.

node_field(Manifest) ->
    case field(Manifest, combined_node) of
        V when is_integer(V), V >= 0, V =< 1023 ->
            {ok, V};
        V ->
            {error, {bad_manifest_field, {combined_node, V}}}
    end.

floor_field(Manifest) ->
    case field(Manifest, floor_safe_before) of
        V when is_integer(V), V >= 0, V =< ?MAX_REL_TS ->
            {ok, V};
        V ->
            {error, {bad_manifest_field, {floor_safe_before, V}}}
    end.

created_at_field(Manifest) ->
    case created_at_value(Manifest) of
        V when is_integer(V), V >= 0, V < (1 bsl 64) ->
            {ok, V};
        V ->
            {error, {bad_manifest_field, {created_at_rel_ms, V}}}
    end.

%% created_at_rel_ms (decide return key) wins; `created_at` accepted as
%% alias; when absent, default to the current wall clock.
created_at_value(Manifest) ->
    case maps:find(created_at_rel_ms, Manifest) of
        {ok, V} ->
            V;
        error ->
            case maps:find(created_at, Manifest) of
                {ok, V} ->
                    V;
                error ->
                    erlang:system_time(millisecond) - ?EPOCH_MS
            end
    end.

digest_field(Manifest) ->
    case field(Manifest, catalog_digest) of
        V when is_binary(V), byte_size(V) =:= 32 ->
            {ok, V};
        _ ->
            {error, {bad_manifest_field, {catalog_digest, not_sha256_digest}}}
    end.

collect_ok([{ok, V} | Rest], Acc) ->
    collect_ok(Rest, [V | Acc]);
collect_ok([], Acc) ->
    {ok, lists:reverse(Acc)};
collect_ok([{error, _} = E | _], _Acc) ->
    E.

%% Durable write protocol, mirroring elib_tsid_store's write path:
%% exclusive temp -> write -> sync -> close check -> chmod 0600 ->
%% rename -> dirsync. On any failure the temp of THIS write is removed
%% and any existing manifest stays untouched (rename is atomic).
durable_write(Path, Bin) ->
    Dir = filename:dirname(Path),
    Unique = erlang:unique_integer([positive]),
    Temp = filename:join(Dir, "tsid.bootstrap.tmp." ++ integer_to_list(Unique)),
    case file:open(Temp, [write, exclusive, raw]) of
        {ok, Fd} ->
            case write_sync_close(Fd, Bin) of
                ok ->
                    commit_temp(Dir, Temp, Path);
                {error, _} = E ->
                    _ = file:delete(Temp),
                    E
            end;
        {error, _} = E ->
            {error, {temp_create, E}}
    end.

commit_temp(Dir, Temp, Path) ->
    case file:change_mode(Temp, ?FILE_MODE) of
        ok ->
            case file:rename(Temp, Path) of
                ok ->
                    dir_sync(Dir);
                {error, _} = E ->
                    _ = file:delete(Temp),
                    {error, {rename, E}}
            end;
        {error, _} = E ->
            _ = file:delete(Temp),
            {error, {chmod, E}}
    end.

write_sync_close(Fd, Bin) ->
    case file:write(Fd, Bin) of
        ok ->
            case file:sync(Fd) of
                ok ->
                    case file:close(Fd) of
                        ok ->
                            ok;
                        {error, _} = E ->
                            {error, {close, E}}
                    end;
                {error, _} = E ->
                    _ = file:close(Fd),
                    {error, {sync, E}}
            end;
        {error, _} = E ->
            _ = file:close(Fd),
            {error, {write, E}}
    end.

%% Parent directory sync (same approach as elib_tsid_store:dir_sync,
%% which is not exported; EXT-03 measured ok on macOS APFS and Linux).
-spec dir_sync(file:filename_all()) -> ok | {error, term()}.
dir_sync(Dir) ->
    case file:open(Dir, [read, directory, raw]) of
        {ok, Fd} ->
            R = file:sync(Fd),
            _ = file:close(Fd),
            case R of
                ok ->
                    ok;
                {error, _} = E ->
                    {error, {dir_sync_failed, E}}
            end;
        {error, _} = E ->
            {error, {dir_open_failed, E}}
    end.

%% Returns {ok, Manifest} | absent | {error, Reason}（含 CRC/魔数/长度损坏）。
-spec read_manifest(file:filename_all()) -> {ok, map()} | absent | {error, term()}.
read_manifest(Path) ->
    case file:read_file(Path) of
        {error, enoent} ->
            absent;
        {ok, Bin} ->
            decode_boot_manifest(Bin);
        {error, _} = E ->
            E
    end.

decode_boot_manifest(Bin) when byte_size(Bin) =:= ?BOOT_RECORD_SIZE ->
    <<Body:?BOOT_BODY_SIZE/binary, Crc:32/unsigned-big>> = Bin,
    case erlang:crc32(Body) =:= Crc of
        true ->
            decode_boot_body(Body);
        false ->
            {error, bad_crc}
    end;
decode_boot_manifest(_) ->
    {error, bad_length}.

decode_boot_body(<<Magic:9/binary, Version:16/unsigned-big, Rest:51/binary>>) ->
    case Magic of
        ?BOOT_MAGIC ->
            decode_boot_body_v1(Version, Rest);
        _ ->
            {error, bad_magic}
    end;
decode_boot_body(_) ->
    %% Body size is fixed by decode_boot_manifest; defensive only.
    {error, bad_length}.

decode_boot_body_v1(
    ?BOOT_VERSION,
    <<
        ModeByte:8/unsigned-big,
        Node:16/unsigned-big,
        Floor:64/unsigned-big,
        Created:64/unsigned-big,
        Digest:32/binary
    >>
) ->
    case mode_from_byte(ModeByte) of
        {error, _} = E ->
            E;
        {ok, Mode} ->
            case Node =< 1023 andalso Floor =< ?MAX_REL_TS of
                true ->
                    {ok, #{
                        version => ?BOOT_VERSION,
                        mode => Mode,
                        combined_node => Node,
                        floor_safe_before => Floor,
                        created_at_rel_ms => Created,
                        catalog_digest => Digest
                    }};
                false ->
                    {error, bad_range}
            end
    end;
decode_boot_body_v1(_, _) ->
    {error, bad_version}.

mode_from_byte(?MODE_AUTO_SCAN) ->
    {ok, auto_scan};
mode_from_byte(?MODE_MANUAL_FLOOR) ->
    {ok, manual_floor};
mode_from_byte(?MODE_LEGACY_ACK) ->
    {ok, legacy_ack};
mode_from_byte(_) ->
    {error, bad_mode}.
