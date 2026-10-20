# IMBoy WebSocket parse reuse validation (in progress)

Base: `5941171b673b2167b77db80611d42d36c5d491f5` (latest fetched main).
OTP 29.1.2 / ERTS 17.1.1, PG18.6. Current candidate uncommitted; source/beam hashes retained in logs. Only two WebSocket modules differ in performance A/B. Other regression repairs are separately attributable.

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
- Separate baseline/candidate message performance module: five each passed, including concurrent 50 messages (~1.1sec); earlier full-suite timeout is not yet reproduced or explained.
- Original streaming HEAD body reproduces SQLSTATE23503 `fk_enterprise_asset_workspace`, missing organization/workspace parent, on both WS versions in owned migrated databases. Its original replica-mode cleanup was intentionally not executed; a diagnostic harness disposes owned databases instead.
- FK fixtures passed three additional consecutive runs (6 streaming + 7 attachment leaf tests per run), then the owned marker database residue query was empty.
- Actual enterprise asset driver attachment group: ten leaf tests passed.
- Latest full unfiltered suite is still pending. Historical old-base full suite had 13151 passed, 35 failed and cancellation/setup failures, raw make exit2. It is not represented as green.

Real TextGuard source latest `4afdf4089c998a47a968020dc679b6c28d4289ca` built using official Rust1.90, binary SHA256 `cbfc8970a0cb8543ef37413d680412c3570002780f070c1ee03e97513bbc061a`. Real loopback service ready; per-suite lifecycle isolation repairs and integration verification are in progress. No real-device acceptance has been performed.

## Reproduction

Build backend and pinned dependencies with the repository Makefile first. With OTP29 on PATH, run `bench/ws_parse_reuse/run.sh short`, `... retry`, or `... stress`. Each invocation compiles non-TEST production artifacts, records beam hashes, uses an owned output directory, and compares the fixed base SHA with the checked-out candidate. Stress defaults to two continuous 600s measurements at 500 business/s, followed by each variant's 30s cooldown. Create the printed output directory's `STOP` file to stop safely. Requires Linux `/proc` for process CPU/RSS observations. The committed harness differs from the measured external copy only in portable launch/output paths; rerunning on other hardware is a new experiment.

The delivered evidence archive `imboy-ws-checkpoint-20261020.tar.gz`, Library version14, SHA256 `e0dcd33bc49d6d8b18b74f4a847f9fa4f477dd3c5a86db6ff01c75cb7cebf8e0`, retains the original measured harness, full raw synthetic measurement records, exact beam hashes, failed harness attempts, and regression logs. Later checkpoints may supersede it.

Automatic real TextGuard test service ownership currently requires Linux `/proc`; this cloud run does not certify macOS. Existing explicit standalone-launcher ownership remains available. At this draft checkpoint, real shadow cached-version cases fail on both HTTP and gRPC, and the latest full suite has not run to completion. This change is not ready to merge.
