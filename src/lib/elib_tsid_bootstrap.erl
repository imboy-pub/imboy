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
%%% 字段：version(16)、mode(8: 0=auto_scan 1=manual_floor 2=legacy_ack
%%%        3=catalog_rebind)、
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
%%%     （默认行为不变；唯一显式例外是下方 catalog rebind 路径）
%%%   环境变量非法                                → {stop, {bootstrap_env, D}}
%%%   auto_scan 扫描失败                          → {stop, {bootstrap_scan, R}}
%%%
%%% == catalog rebind（digest 失配的显式重绑；默认仍 STOP，绝不自动 GO） ==
%%% manifest 绑定的 catalog_digest 与当前 catalog 不符时，唯一放行通道是
%%% 操作员设置 IMBOY_TSID_BOOTSTRAP_REBIND_ACK 显式重绑，且必须同时满足
%%% （实现见 maybe_rebind/6 的 R0..R7）：已取得 lifetime lock
%%% （ctx.lifetime_lock_held）；manifest/CombinedNode/dc_bits/store 身份
%%% 一致（进入该分支前已由矩阵前序检查证毕）；操作员以 ACK 前缀显式确认
%%% 所有旧 writer 已停止；ACK 同时精确绑定 expected old digest 与 current
%%% new digest（交换/过期/错绑 → {bootstrap_env, rebind_ack_digest_mismatch}）；
%%% transition 在 catalog 已验证溯源 allowlist（known_versions ×
%%% verified_rebind_transitions）中，未知 → {stop,
%%% blocked_catalog_transition_unrecognized}；store floor 在（丢失 = store_lost，
%%% 重绑不得借 persist 重建已丢 store）；当前 catalog 全量 schema 校验 +
%%% 高水位扫描成功（失败 → {stop, {rebind_scan, R}}）。授权 ProposedFloor =
%%% max(StoreFloor, ScanFloor)——数据库当前 max 不是已删除历史 ID 的证明，
%%% floor 绝不降低。授权产物 action=rebind_floor 由 guard 执行：先 durable
%%% persist ProposedFloor（fsync/readback、单调不降），再原子写绑定新
%%% digest 的 manifest，最后进入既有 boot_ready。
%%%
%%% rebind 崩溃恢复（各断点均 fail-closed，绝不 fresh reset / 删文件）：
%%%   persist 前崩溃                → manifest 仍绑旧 digest：重启维持
%%%                                  catalog_changed（或携 ACK 显式重试）
%%%   persist 后、manifest 前崩溃   → store floor 已提高：携 ACK 重试时
%%%                                  ProposedFloor = max(已提高 store, 新扫描)，
%%%                                  绝不回退
%%%   manifest durable 后、runtime 发布前崩溃 → 重启走 proceed_existing
%%%                                  （新 manifest + 已提高 store 的正常恢复）
%%%   store/manifest 损坏、身份不符、scan 失败、DB 不可达 → STOP
%%%   第二实例拿不到 lifetime lock  → guard 层 lock_unavailable 拒启
%%%
%%% 环境变量（非法值一律 {stop, ...}，不静默取默认）：
%%%   IMBOY_TSID_BOOTSTRAP_MODE          = auto_scan（缺省）| manual_floor
%%%   IMBOY_TSID_BOOTSTRAP_FLOOR_UNIX_MS = 整数 unix 毫秒（manual_floor 必填；
%%%                                        floor_safe_before = ms - EPOCH_MS）
%%%   IMBOY_TSID_BOOTSTRAP_LEGACY_ACK    = I-CONFIRM-OLD-WRITER-STOPPED
%%%   IMBOY_TSID_BOOTSTRAP_REBIND_ACK    = I-CONFIRM-OLD-WRITER-STOPPED-AND-REBIND:
%%%                                        <old_digest_hex>:<new_digest_hex>
%%%                                        （仅 catalog_changed 悬置时被读取；
%%%                                        其它状态下惰性，不参与判定）
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
-define(MODE_CATALOG_REBIND, 3).

%% Env vars: values must match exactly (no trim / loose parsing).
-define(ENV_MODE, "IMBOY_TSID_BOOTSTRAP_MODE").
-define(ENV_FLOOR_UNIX_MS, "IMBOY_TSID_BOOTSTRAP_FLOOR_UNIX_MS").
-define(ENV_LEGACY_ACK, "IMBOY_TSID_BOOTSTRAP_LEGACY_ACK").
-define(LEGACY_ACK_VALUE, "I-CONFIRM-OLD-WRITER-STOPPED").
-define(ENV_REBIND_ACK, "IMBOY_TSID_BOOTSTRAP_REBIND_ACK").
%% REBIND_ACK 值格式（严格，无 trim）：
%%   I-CONFIRM-OLD-WRITER-STOPPED-AND-REBIND:<old_hex64>:<new_hex64>
%% 前缀本身即「操作员确认所有旧 writer 已停止」的显式确认语句；
%% <old_hex64> 必须精确等于 manifest 当前绑定的 catalog_digest，
%% <new_hex64> 必须精确等于当前 elib_tsid_catalog:digest()——两个
%% digest 都锚定进 ACK，交换/过期/错绑自然 fail-closed。
-define(REBIND_ACK_PREFIX, "I-CONFIRM-OLD-WRITER-STOPPED-AND-REBIND").

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
            decide_manifest_ok(Ctx, M, EffectiveFloor, EnvF, ScanF, WallF);
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
%% digest 不符的默认行为保持不变：无显式 rebind 意图 → {stop, catalog_changed}。
%% 只有 REBIND_ACK 显式给出且全部前置条件满足时才走重绑授权
%% （maybe_rebind/6），且授权绝不降低 store floor、绝不跳过高水位扫描。
decide_manifest_ok(Ctx, M, EffectiveFloor, EnvF, ScanF, WallF) ->
    case maps:get(combined_node, M) =:= maps:get(combined_node, Ctx) of
        false ->
            {stop, store_identity_mismatch};
        true ->
            case maps:get(catalog_digest, M) =:= maps:get(catalog_digest, Ctx) of
                false ->
                    maybe_rebind(Ctx, M, EffectiveFloor, EnvF, ScanF, WallF);
                true ->
                    case EffectiveFloor of
                        0 ->
                            {stop, store_lost};
                        _ ->
                            {ok, #{action => proceed_existing}}
                    end
            end
    end.

%%--------------------------------------------------------------------
%% catalog digest 显式重绑（rebind）授权路径。默认无意图 → catalog_changed
%%（R0，合同钉住）。有 ACK 时按 R1..R7 顺序判定（语义与断言矩阵见模块头
%% 与 elib_tsid_rebind_tests）：格式 → old/new 绑定 → transition 溯源
%% allowlist → lifetime lock → store floor 在 → 当前 catalog 全量扫描成功。
%% ProposedFloor = max(StoreFloor, ScanFloor)（数据库当前 max 不是已删除
%% 历史 ID 的证明，floor 绝不降低）。授权产物由 guard 执行：先 durable
%% persist（fsync/readback、单调不降），再原子写绑定新 digest 的 manifest，
%% 最后进入既有 boot_ready。无法证明 transition 安全时必须 STOP。
%%--------------------------------------------------------------------
maybe_rebind(Ctx, M, StoreFloor, EnvF, ScanF, WallF) ->
    case rebind_ack(EnvF) of
        not_set ->
            %% 默认行为合同：无显式 rebind 意图即 FAIL（不降 warning）。
            {stop, catalog_changed};
        {stop, _} = Stop ->
            Stop;
        {ok, #{old_digest := OldD, new_digest := NewD}} ->
            ManifestD = maps:get(catalog_digest, M),
            CurrentD = maps:get(catalog_digest, Ctx),
            case OldD =:= ManifestD andalso NewD =:= CurrentD of
                false ->
                    {stop,
                        {bootstrap_env,
                            {rebind_ack_digest_mismatch, #{
                                ack_old_digest => OldD,
                                ack_new_digest => NewD,
                                manifest_digest => ManifestD,
                                current_digest => CurrentD
                            }}}};
                true ->
                    rebind_check_transition(Ctx, StoreFloor, ScanF, WallF, OldD, NewD)
            end
    end.

rebind_check_transition(Ctx, StoreFloor, ScanF, WallF, OldD, NewD) ->
    case rebind_transition_verified(OldD, NewD) of
        false ->
            {stop, blocked_catalog_transition_unrecognized};
        true ->
            %% 严格 true 才算持锁；缺失/其它值一律按未持锁 fail-closed。
            case maps:get(lifetime_lock_held, Ctx, false) of
                true ->
                    rebind_check_floor(Ctx, StoreFloor, ScanF, WallF);
                _ ->
                    {stop, rebind_lock_not_held}
            end
    end.

rebind_check_floor(_Ctx, 0, _ScanF, _WallF) ->
    %% manifest 在而 store durable floor 丢失：重绑绝不能借 persist 把
    %% 已丢失的 store「重建」出来——store_lost 优先于一切重绑授权。
    {stop, store_lost};
rebind_check_floor(Ctx, StoreFloor, ScanF, WallF) when is_integer(StoreFloor), StoreFloor > 0 ->
    rebind_scan(Ctx, StoreFloor, ScanF, WallF);
rebind_check_floor(_Ctx, Bad, _ScanF, _WallF) ->
    {stop, {store_floor_invalid, Bad}}.

%% 当前 catalog 口径的全量扫描（与 pristine auto_scan 同一 scan 合同：
%% 内含 schema 校验 + 反向发现 + 高水位），加 rebind 标记供 scan 实现与
%% 测试区分场景。扫描结果与 store floor 取 max——扫描只能抬高 floor。
rebind_scan(Ctx, StoreFloor, ScanF, WallF) ->
    Catalog = elib_tsid_catalog:primary_keys(),
    case ScanF(#{catalog => Catalog, rebind => true}) of
        {ok, #{floor_safe_before := ScanFloor}} when
            is_integer(ScanFloor), ScanFloor >= 0, ScanFloor =< ?MAX_REL_TS
        ->
            rebind_authorize(Ctx, max(StoreFloor, ScanFloor), WallF);
        {ok, #{floor_safe_before := Bad}} ->
            {stop, {rebind_scan, {invalid_floor, Bad}}};
        {ok, Other} ->
            {stop, {rebind_scan, {invalid_scan_result, Other}}};
        {error, R} ->
            {stop, {rebind_scan, R}}
    end.

rebind_authorize(Ctx, ProposedFloor, WallF) ->
    case now_rel_ms(WallF) of
        {stop, _} = Stop ->
            Stop;
        {ok, NowRel} ->
            {ok, #{
                action => rebind_floor,
                floor_safe_before => ProposedFloor,
                mode => catalog_rebind,
                catalog_digest => maps:get(catalog_digest, Ctx),
                combined_node => maps:get(combined_node, Ctx),
                created_at_rel_ms => NowRel
            }}
    end.

%% transition 溯源：两个 digest 都必须是已发布 catalog 版本（known_versions），
%% 且版本对在已验证邻接表中。跨步/降级/未知 digest 一律 false。
rebind_transition_verified(OldD, NewD) ->
    Known = elib_tsid_catalog:known_versions(),
    case {lists:keyfind(OldD, 2, Known), lists:keyfind(NewD, 2, Known)} of
        {{FromV, _}, {ToV, _}} ->
            lists:member({FromV, ToV}, elib_tsid_catalog:verified_rebind_transitions());
        _ ->
            false
    end.

%% REBIND_ACK 读取与解析：只在 catalog digest 失配分支被调用；其它状态
%% （manifest 缺失 / digest 已匹配）下设置该变量是惰性的、不参与判定。
%% 非法值（格式错/段数错/非十六进制/长度错）在失配状态下一律 fail-closed。
rebind_ack(EnvF) ->
    case EnvF(?ENV_REBIND_ACK) of
        false ->
            not_set;
        Value when is_list(Value) ->
            parse_rebind_ack(Value);
        Other ->
            {stop, {bootstrap_env, {bad_rebind_ack, Other}}}
    end.

parse_rebind_ack(Value) ->
    case string:split(Value, ":", all) of
        [?REBIND_ACK_PREFIX, OldHex, NewHex] ->
            case {hex_digest(OldHex), hex_digest(NewHex)} of
                {{ok, OldD}, {ok, NewD}} ->
                    {ok, #{old_digest => OldD, new_digest => NewD}};
                _ ->
                    {stop, {bootstrap_env, {bad_rebind_ack, Value}}}
            end;
        _ ->
            {stop, {bootstrap_env, {bad_rebind_ack, Value}}}
    end.

%% 严格 64 个十六进制字符（大 小写均收，binary:decode_hex 口径）→ 32 字节。
hex_digest(Hex) ->
    HexBin = unicode:characters_to_binary(Hex),
    case is_binary(HexBin) andalso byte_size(HexBin) =:= 64 of
        true ->
            try
                {ok, binary:decode_hex(HexBin)}
            catch
                _:_ -> error
            end;
        false ->
            error
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
        catalog_rebind ->
            {ok, ?MODE_CATALOG_REBIND};
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
mode_from_byte(?MODE_CATALOG_REBIND) ->
    {ok, catalog_rebind};
mode_from_byte(_) ->
    {error, bad_mode}.
