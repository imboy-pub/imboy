# IMBoy WebSocket parse reuse validation (in progress)

Base: `5941171b673b2167b77db80611d42d36c5d491f5` (historical A/B baseline).
OTP 29.1.2 / ERTS 17.1.1, PG18.6. Measured candidate commit `e0b87b5d29e241bbaed5b9d2ddb6d34b4d40e601`, tree `c33f17ad1eb81e49d723ac59e8d402438360c13b` (merged by PR24 as `ee3e151f833bb8bb12e57c6c03cd84c2fd95a4e0`); source/beam hashes retained in logs. Only two WebSocket modules differ in performance A/B. Other regression repairs are separately attributable.

## Completed continuous stress

**CPU and RSS below measure the client and server together in the same BEAM VM, not standalone production-server savings.**
Each variant ran a single uninterrupted 600-second measurement followed by 30-second cooldown. Baseline then candidate; same fixed 500 business messages/sec, four users × eight devices (32 sockets total), eight deliveries/business, 300,000 business messages and 2,400,000 successful device deliveries per variant, zero errors. This is offered-load stability, not saturation throughput. Nine visible logical CPUs were shared, not reserved; cgroup CPU quota was not exposed. BEAM +S4:4 +A4.

8192 raw opaque payload bytes expand to 10924 base64 ciphertext bytes. JSON grows 11218–11223 bytes; v2 frame 11227–11232 bytes. Unique complete-frame matching measures sender timestamp through actual Gun socket receipt; ordering observations are retained, not hidden by FIFO assumptions. Each business invocation creates independent zero-delay delivery timers (`src/ds/message_ds.erl:126`, `imboy_syn:start_delivery_timer/3`); this harness does not establish a global ordering contract across concurrent invocations. Exact bytes/no loss do not certify strict business ordering.

| Metric | Baseline | Candidate |
|---|---:|---:|
| Measurement wall µs | 600001426 | 600001137 |
| Whole VM client+server OS CPU ms | 672460 | 466110 |
| Erlang runtime CPU ms | 616783 | 406800 |
| P95 histogram upper-bound ms | 8.58 | 5.91 |
| P99 histogram upper-bound ms | 10.88 | 8.01 |
| Reductions | 59952455763 | 32241109785 |
| Garbage collections | 12229129 | 6566005 |
| GC words | 19238342092 | 10947434730 |
| Whole-process peak RSS KiB | 89172 | 85896 |
| Peak sampled binary bytes | 7225000 | 8073528 |
| Out-of-order received frames | 20694 | 21672 |

CPU fell 30.7%, P95 31.1%, P99 26.4%; peak sampled binary memory increased 11.7%. Queue samples were zero at drain boundaries, not a proof of zero transient queue. RSS/CPU include the benchmark client in the same VM. Original failed harness attempts are retained and excluded from completed runs.

Actual production modules include message_ds, timers/retry/ACK cache, delivery authorization guard, websocket_handler, Cowboy, codec; actual Gun/TCP sockets. Identity/auth-event fixtures are synthetic; no production login, message database persistence, or full inbound CLIENT_ACK persistence is claimed. Real PG authorization revoke/retry is a separate test using the actual access repository and current migrations.

## Small payload tail confirmation
After long stress, ten alternating rounds, 6000 messages/variant/round, 60000 samples/variant/protocol, zero errors. 512 raw payload bytes, single device. Full per-round distributions in `logs/socket-tail-confirmation-summary.json`.

JSON P99 round median: 1.976 → 2.0765ms (+0.1005ms, +5.1%), five candidate rounds better and five worse. Legacy protobuf P99 round median: 3.2275 → 2.802ms (-13.2%). JSON is not claimed to improve all latency metrics; candidate remains exactly the measured implementation.

## Regression status
- Compile and new nine parse equivalence/security tests passed.
- Baseline and candidate each passed 107 existing WS/message/ACK/codec tests and actual PG discussion authorization revoke/retry test.
- Current focused fixture/boundary repairs: 74 outer EUnit tests passed across ten modules; fresh-peer helpers require real nonzero inner test pass counts.
- Restored Hird/directory/message performance environment: 29 tests passed.
- Actual Garage attachment/OA configuration: 35 tests passed.
- Separate baseline/candidate message performance module: five each passed, including concurrent 50 messages (~1.1sec); the later full-suite timeout exposed unowned workers leaking into the next mocked suite; the follow-up adds bounded ownership and lifecycle regressions.
- Original streaming HEAD body reproduces SQLSTATE23503 `fk_enterprise_asset_workspace`, missing organization/workspace parent, on both WS versions in owned migrated databases. Its original replica-mode cleanup was intentionally not executed; a diagnostic harness disposes owned databases instead.
- FK fixtures passed three additional consecutive runs (6 streaming + 7 attachment leaf tests per run), then the owned marker database residue query was empty.
- Actual enterprise asset driver attachment group: ten leaf tests passed.
- The explicit-config full suite reached natural terminal: 13351 passed, 4 failed, 0 skipped, plus 24 Hird missing-environment setup markers and one concurrent-worker timeout, raw make exit2. Four disk different-OS-principal denial cases remain BLOCKED on this unprivileged cloud. Historical old-base full suite had 13151 passed, 35 failed and cancellation/setup failures, raw make exit2. It is not represented as green.

Real TextGuard source latest `4afdf4089c998a47a968020dc679b6c28d4289ca` built using official Rust1.90, binary SHA256 `cbfc8970a0cb8543ef37413d680412c3570002780f070c1ee03e97513bbc061a`. Real per-suite loopback services passed integration119, business213 and shadow61 leaf tests; all also passed inside the full suite. No real-device acceptance has been performed.

## Reproduction

Build backend and pinned dependencies with the repository Makefile first. With OTP29 on PATH, run `bench/ws_parse_reuse/run.sh short`, `... retry`, or `... stress`. Each invocation compiles non-TEST production artifacts, records beam hashes, uses an owned output directory, and compares the fixed base SHA with the checked-out candidate. Stress defaults to two continuous 600s measurements at 500 business/s, followed by each variant's 30s cooldown. Create the printed output directory's `STOP` file to stop safely. Requires Linux `/proc` for process CPU/RSS observations. The committed harness differs from the measured external copy only in portable launch/output paths; rerunning on other hardware is a new experiment.

The delivered evidence archive `imboy-ws-checkpoint-20261020.tar.gz`, Library version14, SHA256 `e0dcd33bc49d6d8b18b74f4a847f9fa4f477dd3c5a86db6ff01c75cb7cebf8e0`, retains the original measured harness, full raw synthetic measurement records, exact beam hashes, failed harness attempts, and regression logs. Later checkpoints may supersede it.

Automatic real TextGuard test service ownership currently requires Linux `/proc`; this cloud run does not certify macOS. Existing explicit standalone-launcher ownership remains available. Earlier shadow cached-version failures were resolved by an owned single-node cache READY barrier without changing assertions. The full-suite residual failures/cancellations above remain explicit and are not rewritten as passed by subsequent focused runs.


## Follow-up validation / 后续验证

A follow-up against merged main shares only exceptional encoder fallback bodies; its original 600 s result is **not** claimed as a new long test of the refactored helper. Strict error/throw/exit frame-byte regressions were added. Worker ownership now covers normal completion, exception, common-deadline timeout, partial spawn failure and caller death, with confirmed DOWN before returning. Original business success assertions remain unchanged; the outer EUnit timeout includes bounded cleanup.

后续修复保留原业务断言，明确外层 EUnit 预算与业务性能条件的区别；目录 PG 允许显式空口令；本次验证使用 trust 配置，早期非空合成占位符被 trust 忽略，不是密码认证证据。

Focused follow-up results: 36 WS/directory/concurrent-worker/next-suite cases, 16 owned-PG cleanup cases, 41 real Hird P1/P2/P3/offline cases and 3 runner contract tests passed. Cleanup verification found no remaining marker databases after the repaired INTBE02 callers and W203 barrier connection. The different-OS-principal capability remains unavailable; no test was skipped or changed to simulate that proof. Complete raw records and precise commit/beam hashes remain in the delivered evidence checkpoint.

Follow-up fallback refactor also passed the existing107 WS/message/ACK/codec cases and actual-PG revoke/retry case. A new five-round alternating short comparison against merged main recorded96 rows (80 measured), zero delivery errors. Median per-round P99 milliseconds, base→follow-up: JSON512/one device1.761→1.833; v2 JSON512/one1.668→1.975; legacy512/one2.645→2.910; JSON8192/two4.509→4.369; v2 JSON8192/two4.641→4.616; legacy8192/two4.543→4.632; v2 JSON65536/two25.385→25.116; v2 JSON8192/eight9.591→9.605. Small-frame tails worsened in these observations; they are retained without claiming all-protocol improvement or dismissing them as noise. These are separate-round medians, not pooled quantiles.

The fallback-only short comparison is maintenance validation, not a performance improvement claim: small512-byte P99 changes were +4.1% JSON, +18.4% v2 and +10.0% legacy; the8192-byte/eight-device group showed same-VM total CPU +0.5% and P99 +0.1%. All groups used five alternating rounds after warmup. Actual Elvis rerun has not been performed for the deduplicated fallback; the existing dynamic asset-port diagnostic remains unresolved.
