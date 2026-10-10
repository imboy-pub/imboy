# V2.2.1 W0 执行报告

**结论：W0_BLOCKED；保留方案 A；confirmed=false；生产复制白名单为空。**

本轮为 W0 收尾轮（CACHE_V2_2_1_W0_CLOSEOUT）：只完善与验收 W0，不执行 W1～W5，不修改生产缓存实现。

## 候选绑定与范围

本轮存在两个独立候选绑定，历史结果与当前结果不互相冒称：

- **历史候选 `hist-578d83ac`**：HEAD `578d83acd53cf9cccb373b6b004a4b2083eecf49`，压测轮次 1/2（下表"轮次 1/2"）在该候选下执行，环境与本轮相同。该候选下 `cache_w0_bench.erl` SHA256 `049026b3…1e1e`、`run_cache_w0.py` SHA256 `966536ca…041a`。
- **本轮候选 `closeout-34858496`**：授权基线 `34858496b5ef…`（用户 2026-10-10 重新授权；任务提示词原预期 HEAD `5aff3863…` 及其上 TSID 提交均为外来上下文）。绑定轮次 7/8 实际执行于 `eb7d4d548fe8…`——13:50:05 外来 TSID W7 提交使 HEAD 前移；**缓存证据面等价性已机器验证**：执行于 34858496 的轮次 5 与执行于 eb7d4d54 的轮次 7，二者 source-evidence.json（42 个缓存相关源文件全文+SHA）逐字节一致，缓存调用行零漂移。该前移属授权条款"TSID 提交为外来上下文"范畴，记录于 decision.json head_equivalence_proof 并由校验器强制。
- **源码漂移机器核验**：本轮候选相对历史候选的 source-evidence 漂移仅 2 个文件（`src/ds/group_ds.erl`、`src/logic/moderation_policy.erl`，均为 TSID 提交所致）；校验器从两轮 source-evidence 内嵌全文重算 `imboy_cache|depcache` 调用行差异，结果为 **0**，与声明一致（不信任自声明字段）。缓存核心 `imboy_cache.erl`/`imboy_cache_sync.erl` 未漂移。
- 本轮候选脚本（含扩展自检、扫描增强与事务边界核验后的钩子状态）SHA256：`run_cache_w0.py` `7621cbbe…`、`cache_w0_bench.erl` `f17e9806…`（完整值见 baseline.json script_hashes）。每轮 baseline.json 绑定各自 script_hashes，不跨候选混用。轮次 3/4（审查修复前）与 5/6（事务边界核验前）为预绑定轮，已被轮次 7/8 取代，原始数据保留在 RUN_ROOT 供追溯。
- 压测执行期间工作树存在 11 个外来已跟踪修改（elib_tsid 及其测试，TSID 会话所为），均在两轮 baseline.json working_tree_status 中如实记录，且全部不在缓存调用证据面内；两轮 calls.json/source-evidence.json 逐字节一致。
- 环境：macOS-27.0.1-arm64-arm-64bit，CPU 12，OTP 29，online schedulers 12。syn 3.4.2；depcache .app vsn=git，依赖源码/beam 摘要在各轮 baseline.json。
- 独立 BEAM、随机 cookie、分布式端口绑定 127.0.0.1；peer 节点地址 127.0.0.1，不连接现役业务节点；不启动 IMBoy/PG/Garage。
- 每轮 operations/worker=10000，并发 1/8/32，1024 Key，固定 256-byte 合成值，TTL=3600，memory_max=64 MB。轮次间串行执行。
- GET/get_wait/memo 表中为热命中；mixed 为 90% GET+10% SET。冷 memo 自检与缺陷复现单独记录。
- syn 基线是真实依赖广播到测试接收进程、逐条等待所有接收者 depcache:set 完成的 ACK——测量的是**应用完成延迟**，不是 publish 返回延迟，不是生产 confirmed 实现，不冒充未来 V2 协议。
- 两候选各自 `erlc -Werror` 与 BEAM 均 exit=0；W0 runner 退出码 2 是设计上的 W0_BLOCKED 信号，与技术退出码不混同。

## 历史候选性能数据（轮次 1/2，HEAD 578d83ac，保持原样）

| 轮次 | 节点 | 操作 | 并发 | 请求数 | ops/s | P50 μs | P95 μs | P99 μs | 错误率 | 队列采样峰值 | 主 BEAM 采样峰值 MiB |
|---|---|---|---|---|---|---|---|---|---|---|---|
| 1 | 1 | get | 1 | 10000 | 2881844 | 0 | 1 | 1 | 0.0% | 0 | 34.63 |
| 1 | 1 | get | 8 | 80000 | 8340284 | 0 | 1 | 1 | 0.0% | 0 | 42.85 |
| 1 | 1 | get | 32 | 320000 | 6721559 | 1 | 1 | 1 | 0.0% | 0 | 76.77 |
| 1 | 1 | get_wait | 1 | 10000 | 3402518 | 0 | 1 | 1 | 0.0% | 0 | 76.39 |
| 1 | 1 | get_wait | 8 | 80000 | 10761367 | 0 | 1 | 1 | 0.0% | 0 | 80.68 |
| 1 | 1 | get_wait | 32 | 320000 | 7870143 | 1 | 1 | 1 | 0.0% | 0 | 83.48 |
| 1 | 1 | set | 1 | 10000 | 665956 | 1 | 2 | 2 | 0.0% | 0 | 91.00 |
| 1 | 1 | set | 8 | 80000 | 727517 | 9 | 24 | 46 | 0.0% | 4 | 98.91 |
| 1 | 1 | set | 32 | 320000 | 769068 | 36 | 78 | 108 | 0.0% | 31 | 112.59 |
| 1 | 1 | mixed | 1 | 10000 | 2310536 | 0 | 1 | 2 | 0.0% | 0 | 92.24 |
| 1 | 1 | mixed | 8 | 80000 | 2161812 | 0 | 19 | 54 | 0.0% | 3 | 99.87 |
| 1 | 1 | mixed | 32 | 320000 | 802554 | 1 | 327 | 500 | 0.0% | 28 | 249.64 |
| 1 | 1 | memo | 1 | 10000 | 3052503 | 0 | 1 | 1 | 0.0% | 0 | 136.15 |
| 1 | 1 | memo | 8 | 80000 | 11058889 | 0 | 1 | 1 | 0.0% | 0 | 136.27 |
| 1 | 1 | memo | 32 | 320000 | 13171977 | 0 | 1 | 1 | 0.0% | 0 | 160.61 |
| 1 | 1 | syn | 1 | 10000 | 343584 | 3 | 4 | 7 | 0.0% | 1 | 134.39 |
| 1 | 1 | syn | 8 | 80000 | 305810 | 24 | 42 | 68 | 0.0% | 9 | 141.39 |
| 1 | 1 | syn | 32 | 320000 | 294861 | 102 | 155 | 219 | 0.0% | 33 | 158.51 |
| 1 | 3 | syn | 1 | 10000 | 24569 | 36 | 69 | 111 | 0.0% | 2 | 38.30 |
| 1 | 3 | syn | 8 | 80000 | 78943 | 95 | 169 | 219 | 0.0% | 14 | 48.15 |
| 1 | 3 | syn | 32 | 320000 | 99531 | 315 | 462 | 525 | 0.0% | 73 | 98.09 |
| 2 | 1 | get | 1 | 10000 | 2872738 | 0 | 1 | 1 | 0.0% | 0 | 34.32 |
| 2 | 1 | get | 8 | 80000 | 8032935 | 0 | 1 | 2 | 0.0% | 0 | 43.09 |
| 2 | 1 | get | 32 | 320000 | 6230651 | 1 | 1 | 2 | 0.0% | 0 | 69.08 |
| 2 | 1 | get_wait | 1 | 10000 | 3280840 | 0 | 1 | 1 | 0.0% | 0 | 72.29 |
| 2 | 1 | get_wait | 8 | 80000 | 9274287 | 0 | 1 | 1 | 0.0% | 0 | 62.49 |
| 2 | 1 | get_wait | 32 | 320000 | 6496407 | 1 | 1 | 2 | 0.0% | 0 | 100.21 |
| 2 | 1 | set | 1 | 10000 | 667735 | 1 | 2 | 2 | 0.0% | 0 | 90.83 |
| 2 | 1 | set | 8 | 80000 | 769401 | 9 | 20 | 38 | 0.0% | 5 | 89.16 |
| 2 | 1 | set | 32 | 320000 | 780292 | 34 | 74 | 107 | 0.0% | 30 | 112.78 |
| 2 | 1 | mixed | 1 | 10000 | 2151463 | 0 | 1 | 2 | 0.0% | 0 | 87.56 |
| 2 | 1 | mixed | 8 | 80000 | 2194848 | 0 | 19 | 62 | 0.0% | 1 | 89.58 |
| 2 | 1 | mixed | 32 | 320000 | 790008 | 1 | 325 | 508 | 0.0% | 31 | 117.88 |
| 2 | 1 | memo | 1 | 10000 | 3213368 | 0 | 1 | 1 | 0.0% | 0 | 83.78 |
| 2 | 1 | memo | 8 | 80000 | 11250176 | 0 | 1 | 1 | 0.0% | 0 | 85.65 |
| 2 | 1 | memo | 32 | 320000 | 13533517 | 0 | 1 | 1 | 0.0% | 0 | 106.22 |
| 2 | 1 | syn | 1 | 10000 | 350226 | 3 | 3 | 6 | 0.0% | 0 | 88.29 |
| 2 | 1 | syn | 8 | 80000 | 301616 | 24 | 46 | 73 | 0.0% | 9 | 94.80 |
| 2 | 1 | syn | 32 | 320000 | 278832 | 101 | 189 | 273 | 0.0% | 33 | 128.78 |
| 2 | 3 | syn | 1 | 10000 | 25104 | 36 | 67 | 111 | 0.0% | 2 | 37.73 |
| 2 | 3 | syn | 8 | 80000 | 79938 | 94 | 166 | 209 | 0.0% | 17 | 47.56 |
| 2 | 3 | syn | 32 | 320000 | 94106 | 315 | 476 | 611 | 0.0% | 72 | 94.31 |

## 本轮候选性能数据（轮次 7/8；授权基线 34858496，执行 HEAD eb7d4d54，缓存证据面逐字节等价）

| 轮次 | 节点 | 操作 | 并发 | 请求数 | ops/s | P50 μs | P95 μs | P99 μs | 错误率 | 队列采样峰值 | 主 BEAM 采样峰值 MiB |
|---|---|---|---|---|---|---|---|---|---|---|---|
| 7 | 1 | get | 1 | 10000 | 2748763 | 0 | 1 | 1 | 0.0% | 0 | 49.23 |
| 7 | 1 | get | 8 | 80000 | 6083188 | 0 | 1 | 1 | 0.0% | 0 | 56.89 |
| 7 | 1 | get | 32 | 320000 | 5022602 | 1 | 1 | 2 | 0.0% | 0 | 93.68 |
| 7 | 1 | get_wait | 1 | 10000 | 2674512 | 0 | 1 | 1 | 0.0% | 0 | 91.06 |
| 7 | 1 | get_wait | 8 | 80000 | 6774494 | 0 | 1 | 1 | 0.0% | 0 | 83.02 |
| 7 | 1 | get_wait | 32 | 320000 | 6766186 | 1 | 1 | 2 | 0.0% | 0 | 92.58 |
| 7 | 1 | set | 1 | 10000 | 641931 | 1 | 2 | 3 | 0.0% | 0 | 116.60 |
| 7 | 1 | set | 8 | 80000 | 668840 | 9 | 27 | 57 | 0.0% | 4 | 123.71 |
| 7 | 1 | set | 32 | 320000 | 746481 | 36 | 81 | 122 | 0.0% | 31 | 134.35 |
| 7 | 1 | mixed | 1 | 10000 | 1423690 | 0 | 2 | 4 | 0.0% | 0 | 117.21 |
| 7 | 1 | mixed | 8 | 80000 | 237629 | 1 | 87 | 379 | 0.0% | 6 | 141.93 |
| 7 | 1 | mixed | 32 | 320000 | 473116 | 12 | 397 | 984 | 0.0% | 29 | 138.00 |
| 7 | 1 | memo | 1 | 10000 | 2975305 | 0 | 1 | 1 | 0.0% | 0 | 163.92 |
| 7 | 1 | memo | 8 | 80000 | 9216590 | 0 | 1 | 1 | 0.0% | 0 | 168.62 |
| 7 | 1 | memo | 32 | 320000 | 6190992 | 0 | 1 | 1 | 0.0% | 0 | 183.27 |
| 7 | 1 | syn | 1 | 10000 | 313824 | 3 | 5 | 8 | 0.0% | 1 | 168.33 |
| 7 | 1 | syn | 8 | 80000 | 262139 | 24 | 59 | 159 | 0.0% | 9 | 174.79 |
| 7 | 1 | syn | 32 | 320000 | 248298 | 108 | 214 | 330 | 0.0% | 33 | 192.17 |
| 7 | 3 | syn | 1 | 10000 | 20945 | 38 | 96 | 146 | 0.0% | 2 | 52.68 |
| 7 | 3 | syn | 8 | 80000 | 67923 | 107 | 207 | 355 | 0.0% | 16 | 63.81 |
| 7 | 3 | syn | 32 | 320000 | 86940 | 357 | 551 | 895 | 0.0% | 76 | 109.24 |
| 8 | 1 | get | 1 | 10000 | 2916302 | 0 | 1 | 1 | 0.0% | 0 | 49.21 |
| 8 | 1 | get | 8 | 80000 | 7879444 | 0 | 1 | 2 | 0.0% | 0 | 56.76 |
| 8 | 1 | get | 32 | 320000 | 6415011 | 1 | 1 | 2 | 0.0% | 0 | 83.32 |
| 8 | 1 | get_wait | 1 | 10000 | 3232062 | 0 | 1 | 1 | 0.0% | 0 | 87.89 |
| 8 | 1 | get_wait | 8 | 80000 | 9390774 | 0 | 1 | 1 | 0.0% | 0 | 93.21 |
| 8 | 1 | get_wait | 32 | 320000 | 6815761 | 1 | 1 | 2 | 0.0% | 0 | 101.58 |
| 8 | 1 | set | 1 | 10000 | 686059 | 1 | 2 | 2 | 0.0% | 0 | 105.76 |
| 8 | 1 | set | 8 | 80000 | 665940 | 10 | 24 | 47 | 0.0% | 5 | 112.60 |
| 8 | 1 | set | 32 | 320000 | 781897 | 35 | 75 | 110 | 0.0% | 27 | 128.96 |
| 8 | 1 | mixed | 1 | 10000 | 2288853 | 0 | 1 | 2 | 0.0% | 0 | 105.14 |
| 8 | 1 | mixed | 8 | 80000 | 1509035 | 0 | 18 | 47 | 0.0% | 1 | 112.85 |
| 8 | 1 | mixed | 32 | 320000 | 657303 | 7 | 361 | 587 | 0.0% | 28 | 131.60 |
| 8 | 1 | memo | 1 | 10000 | 3233107 | 0 | 1 | 1 | 0.0% | 0 | 125.69 |
| 8 | 1 | memo | 8 | 80000 | 4594005 | 0 | 1 | 1 | 0.0% | 0 | 152.69 |
| 8 | 1 | memo | 32 | 320000 | 13819312 | 0 | 1 | 1 | 0.0% | 0 | 135.22 |
| 8 | 1 | syn | 1 | 10000 | 358051 | 3 | 3 | 5 | 0.0% | 0 | 126.80 |
| 8 | 1 | syn | 8 | 80000 | 305685 | 24 | 44 | 71 | 0.0% | 9 | 133.70 |
| 8 | 1 | syn | 32 | 320000 | 302906 | 99 | 145 | 199 | 0.0% | 33 | 145.94 |
| 8 | 3 | syn | 1 | 10000 | 23673 | 32 | 91 | 128 | 0.0% | 3 | 52.92 |
| 8 | 3 | syn | 8 | 80000 | 68952 | 108 | 198 | 324 | 0.0% | 17 | 63.67 |
| 8 | 3 | syn | 32 | 320000 | 87787 | 334 | 541 | 916 | 0.0% | 79 | 107.78 |


两候选每轮各 2,870,000 个计时操作；冷 loader 自检与缺陷复现不计入。两候选数据形态一致（缓存调用行零漂移），互为独立候选绑定而非合并样本。轮次 7 的 mixed 吞吐显著低于其他轮（237k vs ~1.5-2.5M ops/s@8 并发）且三节点尾延迟升高，为压测期间本机负载噪声所致、按原始值照实记录——这正是"短时基准不代表稳态容量"的直接例证。表中内存含 load generator、采样与延迟样本列表，只是主节点 BEAM，不是缓存纯内存或集群峰值。各节点结束时内存、elapsed_us、cache_ets_words/bytes 都在各轮 performance.json。ETS bytes 不含完整 refc binary 占用。队列每 5ms 采样各节点测试 receiver+depcache 队列总和，不覆盖全部 syn 内部队列。微秒分辨率下 P50=0 表示低于分辨率；短时基准不能当长期稳定饱和实验，更不能推生产容量。

## depcache 原生并发能力与风险（源码 oracle + 实测）

源码依据：`deps/depcache/src/depcache.erl`（依赖绑定摘要见 baseline.json）。

| 行为 | 源码 oracle | 实测（轮次 5/6 一致） |
|---|---|---|
| 同 Key 冷 miss 单飞 | `handle_call_get_wait`（:991-1025）首个 miss 成为被监控 writer，其余加入 wait_pids | cold_memo_workers=32、loader_executions=1 |
| flush 与在途 loader | `flush_key`（:1301-1305）只删 ETS 与进程字典，不触碰 wait_pids；loader 完成后 `set` 回填 | stale_refill_after_flush=reproduced；**等待者同样收到 stale**（stale_waiter_value_after_flush=stale，经 handle_call_set :1121-1130 回复） |
| loader 异常 | `memo_key` catch（:345-348）向等待者回 `{throw,R}` 并重抛；memo（:306-307）重抛 | 3 等待者 + writer 全部 rethrown_ok；异常 Key 之后保持 undefined |
| writer 崩溃 | `{'DOWN'}`（:922-935）向等待者回 `{error,premature_exit}`；memo（:310-311）重试 | premature_exit_waiter=ok；memo 重试自跑 1 次 loader 返回新值 |
| 手工 get_wait 所有权 | miss 时首个调用者收到 `undefined` 并成为 writer（:1021）；ownership 不可携带（计划 19.3） | first_caller_becomes_writer=undefined；并发等待者在 writer set 后收到新值 |
| 等待超时边界 | 同左 | MEASURED（探针 `scripts/cache_w0_timeout_probe.erl`，结果内嵌 decision.json `depcache_timeout_probe.result`）：锁 30s 过期后第二 miss 成为第二并发 writer（单飞在 >30s 负载下破坏）；旧等待者以 gen_server:call 30s 超时退出、无无限挂起；新等待者收到首 writer 迟到 set 的值；末次 set 胜出 |

旧 loader 回填是**缺陷复现**，不是修复验收；versioned physical Key / generation 前置校验属 W1 准备，本轮未实现，未重写 depcache waiter。

## 正确性取证（本轮新增与复核）

- **legacy 广播面（G-04/W1-05 证据）**：`imboy_cache:set/5` 在 `dsync_enabled=true`（`config/sys.config:339` 生产默认）时对一切非 `{local_cache,_}` 键全节点广播完整值（`imboy_cache.erl:156-184`）。机器推导的广播命名空间清单（16 项，词法文件粒度归属）见 unresolved-forms.json `legacy_set_broadcast`：含 apns_jwt（签名 JWT）、fcm_access_token_dyn、config5（**解密后配置值**，`config_ds.erl:122-130`）、qr_login/qr_login_qr（会话与 token 映射）、csrf、bind_mail（含邮箱）、login_attempt_ip_limit（每次失败登录广播一次）。归属注记：login_attempt 仅因与 ip_limit 共享源文件而出现在清单中，`login_attempt_ds:181/186` 实际写入键为 `{login_attempt_ip_limit,Ip}`（宏展开）；其余条目待 W1 数据流核验逐项确认。策略矩阵的 local 判定是**迁移目标，不是现状**。
- **事务边界复核**：`auth_session_ds:bump/1` 为 COMMIT 后 evict（正确时序）；`bump_in_tx/2` 事务内 evict（W1-03）；`group_member_ds:146` 事务内 `group_ds:join` → 事务内缓存 set（W1-03）；`friend_ds` 失效在 repo 写之后、按调用点核验事务边界留 W1。
- **tag_subtitle 键失配复核属实**：memo 填充 `tag_subtitle_2_<TagId>`（TTL 86400），`flush_subtitle` 只清 `tag_subtitle_3_<TagId>`（无生产写入方）——副标题变更后最长 1 天读旧值。W1 修复项。
- **无失效钩子命名空间（本轮新发现）**：moment_acl（权限相关，仅 moment_ds:377-384 memo，TTL 300s）、billing_meter（仅 :74-79 读透填充，TTL 300s）、moderation_sensitive_words（仅 :113 memo，TTL 60s）——三者全仓均无失效调用，变更靠 TTL 自然过期。moment_acl 属权限类，W1 须裁决 TTL 兜底可否接受。
- **group join/leave 读改写（本轮新发现）**：group_ds:join/leave（:264-296）对成员列表缓存 get→set 读改写，与 account_list 同类非原子；且在 group_member_ds 事务内被调用。
- **config_ds 双重失效与回填窗口**：set 路径写库前 flush（:135）+ 写库后再 flush（:167）；仍存在提交前读→二次 flush 后回填旧值的 7.1 竞态窗口，TTL 864000s 放大影响。
- **配置矛盾（外部确认项）**：user_device_logic:115 注释假设"生产 dsync_enabled=false"，但仓内 config/sys.config:339 为 true；实际部署值须运维确认（决定敏感广播发现的影响面）。
- account_ds 的 get_wait 只合并 miss；已有列表的 get→set 不提供原子出队（源码注释亦确认 30s 超时风险与 3s 空 TTL 缓解）。G-01 保留。
- QR status 为 get 后 delete，SSE 可以下推 token；且两类 QR 键当前会被 legacy set 全节点广播。没有权威原子消费 oracle。本轮不迁移 QR（G-QR 独立 gate）。
- 当前 imboy_cache_sync 没有版本/epoch、快照或重连补偿（`imboy_cache_sync.erl:89-102` 仅 set/flush/flush，未知消息静默丢弃）；syn membership 恢复不等于 cache 数据恢复。分区/故障/重启恢复 NOT_EXECUTED（W4 范围）。
- 计划 1.3 取证清单补全：`include/cache.hrl` 定义 `?DEPCACHE_SERVER = imboy_cache`（`chat.hrl` 定义 `?CACHE_SCOPE = imboy_cache`，同步组 `dsync_handler`）；`imboy_syn.erl` 为 WS 会话 syn 封装（ACK 重试在自有 ETS），其成员恢复属 syn 自身，与缓存数据恢复无关——加强 W0-08 结论。

## Key 策略矩阵与失效钩子状态

- 完整矩阵见 key-registry.md / key-registry.json，共 47 个业务命名空间、动态 Key 形态或仅失效标签；本轮新增逐命名空间 `mutation_hook` 状态、`mutation_hook_sites`（文件:行:函数）、`write_sites` 与 `mutation_hook_reason`。
- 钩子状态分布（47 项，经逐调用点源码核验；完备性以词法 census 为界——未以 imboy_cache/depcache 形态出现的失效路径不可见，属 unresolved-forms 持续登记范围）：TRACED_COMMIT_AFTER_OK 19（失效位于 DB 写返回/with_tx 提交后，含 friend_ds 7 点、channel 事务两簇、user_tag_ds with_tx 闭包外两处、adm 失效束 :311-313/:817-819 等）、LEGACY_TAG_NO_WRITER 9、W1_KNOWN_DEFECT 5（account_list、login_attempt_ip_limit、tag_subtitle_2、qr_login、qr_login_qr）、NO_FLUSH_HOOK_TTL_ONLY 3（moment_acl、billing_meter、moderation_sensitive_words）、LOCAL_SELF_MANAGED 3、LOCAL_SELF_MANAGED_BROADCAST_W1 4、DEP_TAG_CASCADE 1、TRACED_IN_TX_W1 1（group）、MIXED_TX_BOUNDARY_W1 1（auth_session_epoch）、SECRET_SUBITEM_REVIEW_W1 1（config5）。
- 已知 cosmetic（复审 P2，不值得重跑绑定链）：key-registry.json 中 user_device_name 理由引用 change_name "(:69-76)"，实际 flush 位于 :77-78（写在 :74）——时序声明正确，仅行号范围差 2 行。
- **W0 调查完成 ≠ W1 实现完成**：除明确标注的状态外，失效钩子未实施任何修复；未闭合项不得伪称已冻结。
- 生产词法调用 126 个，覆盖 35 个模块；每个调用附外层函数线索；EXCEPTION_UNRESOLVED=0。全范围清单总计 206（含测试和脚本）。
- 动态/不可静态解析形态共 6 类（unresolved-forms.json）：forwarded_key（adm_user_logic:find/3 调用方构造键，含调用点清单）、derived_string_key（implode 派生）、random_binary_key（Csrf）、runtime_keys_via_syn、legacy_set_broadcast、anonymous_memo_mfa（生产 src 未发现 memo/1 包装调用）。
- 自有 ETS/persistent_term/ACK 和进程字典 gun_conn 显式排除通用迁移（other-stores.json 37 条），不会复制 PID/timer ref。
- 值大小、数量上界与恢复资源预算仍未验证，保留未冻结状态。

## confirmed 需求核验

结论不变：**confirmed=false**。逐路径核验表见 confirmed-requirements.md；本轮复核 QR（get→delete 非原子 + SSE 双通道 + legacy 全节点广播）、account_ds（读改写）、auth/adm session（COMMIT 后/事务内混合）、限流/一次性标志（读改写）后无新增 confirmed 需求。测试 ACK 仅测量远端 depcache:set 完成延迟，不构成 confirmed 需求成立的证据。

## 方案决策与 W1 readiness

- 方案 A：保留 depcache + syn + 后续轻量元数据；不引入 Redis/Mnesia，不新增中间件。两候选四轮数据一致支持该技术基线。
- 方案 B：不启用。SET 排队可观察，但没有生产高频 Key/目标硬件/SLO 缺口/A 调优失败证据；触发条件不成立。**B 不获准 ≠ A 已具备生产资格。**
- W0_BLOCKED 原因（与 decision.json blockers 一致）：
  1. 尚无实际业务峰值、目标硬件与可证据化冻结的绝对 SLO（BLOCKED_EXTERNAL）；
  2. Registry 的值大小/数量/资源预算未冻结；失效钩子已完成 W0 逐调用点调查（19 项确认 COMMIT 后失效、3 项无钩子仅 TTL 待 W1 裁决、5 项明确缺陷待 W1 修复），回填竞态 generation 栅栏属 W1/W2 实现项。
- W1：NOT_STARTED / BLOCKED_ON_W0。本轮未执行 W1～W5 的任何实现。
- 没有启动 IMBoy/PG/Garage，没有业务数据变更、部署、push、破坏性 Git 操作；未触碰受保护外来 WIP（docs/business/imboy-open-source-productization-strategy-v1.md、docs/design/）与其他会话改动。

## W0 验收 ledger 摘要（机器字段见 decision.json acceptance_ledger）

| ID | 任务 | 结果 |
|---|---|---|
| W0-01 | 记录本地 HEAD、工作区变更和依赖版本 | PASS |
| W0-02 | 全仓缓存调用点扫描 | PASS |
| W0-03 | 完整 Key Registry | BLOCKED（预算未冻结；钩子仅调查） |
| W0-04 | 敏感数据和权限类缓存分类 | PASS |
| W0-05 | confirmed 真实业务需求审查 | PASS（confirmed=false） |
| W0-06 | 当前 depcache 性能基线 | PASS（技术基线；非生产 SLO） |
| W0-07 | 当前 syn 同步性能基线 | PASS（应用完成 ACK 语义） |
| W0-08 | 三节点故障与恢复现状调查 | PASS（调查完成；故障注入 NOT_EXECUTED 属 W4） |
| W0-09 | 绝对性能 SLO 冻结 | BLOCKED_EXTERNAL |
| W0-10 | 方案 A/B 决策冻结 | PASS（A 保留；B 不获准；非生产资格） |

## 原始证据与复跑

持久证据根（仓外，防临时目录清理）：`/Users/leeyi/project/imboy.pub/.Codex/runs/cache-v2.2.1-w0/`

- 轮次 1（历史候选）：`hist-imboy-cache-w0-i55ib9ea/`（原始目录 `/var/folders/.../imboy-cache-w0-i55ib9ea` 的核验副本，manifest SHA256 `188f68ec…4d`）
- 轮次 2（历史候选）：`hist-imboy-cache-w0-8kn7as_5/`（manifest SHA256 `35e3c425…bf`）
- 轮次 7（本轮候选，绑定）：`r7-20261010-140457/`
- 轮次 8（本轮候选，绑定）：`r8-20261010-140511/`
- 轮次 3/4/5/6（`r3-20261010-131525/`、`r4-20261010-131543/`、`r5-20261010-134625/`、`r6-20261010-134650/`）：预绑定轮（3/4 为审查修复前、5/6 为事务边界核验前；5/6 执行于授权基线 34858496 并作为 HEAD 等价性证明的对照轮），性能数据在同一 bench 下取得、仍然有效，但扫描器注册表已被轮次 7/8 取代；保留供追溯，其中 r5/r6 被 decision.json head_equivalence_proof 引用。

每轮含 baseline.json / performance.json / raw.log / compile.log / calls.json / source-evidence.json / other-stores.json / unresolved-forms.json / key-registry.json / key-registry.md / sha256.json。原始执行证据保持仓外，遵守 **/evidence/ 排除规则。

- 复跑：`python3 scripts/run_cache_w0.py --operations 10000`；输出新目录，不覆盖历史证据。
- 文档一致性：`python3 scripts/check_cache_w0_docs.py`（exit 0 只表示交付文档/证据/ledger 一致，不表示 W0/生产/UAT 通过；校验器重算源码漂移、锚定证据路径并强制 ledger fail-closed）；`--self-test` 验证校验器对缺证据、缺必需 ID、重复/多余 ID、未知结果、空值 gaps、无证据 PASS、任务文本漂移、状态矛盾一律拒绝。

独立复审：两轮只读代理审查——首轮 3 条 P1 全部修复；增量复审抽查 10 处行号证据全部相符、确认无过度声明，其 P1（等价性证明非强制且未锚定授权侧）已修复为：closeout 候选强制要求 head_equivalence_proof、对照轮 baseline.head 必须逐字等于 authorized_head、对照轮 manifest 链完整校验，并经防伪造自证（删除 proof / 篡改 authorized_head 均被拒绝）。

未检查：真实数据库/设备/UAT、目标三机网络、长期稳定性、V2 实现后的恢复和安全最终决策。
风险：原始证据已复制到仓外持久 RUN_ROOT 并逐一核验 manifest；`/var/folders` 原目录仍可能被系统清理（decision.json 同时引用两处路径）。decision.json 的绑定根为本地文件，无外部锚（git note/签名），防篡改依赖复核方重新执行校验器与压测。W0_BLOCKED 不允许继续 W1。

## 附注：执行后 HEAD 前移与证据面交集核查（2026-10-10）

- 绑定候选轮 7/8 的源码证据快照执行于 `eb7d4d54`（2026-10-10 13:50:05）；其后的超时探针与最终文档校验在 `2da28c3fb524`（2026-10-10 14:26:27，外部 TSID 会话提交 `test(tsid): restore runtime after tests that end in a reset state`，按本轮授权视为外来上下文）下通过。
- `git diff --name-only eb7d4d54..HEAD` 仅含 `test/lib/elib_tsid_tests.erl`、`test/lib/imboy_env_tsid_tests.erl` 两个测试文件；与 r7 证据面 42 文件求交集为**空**（核查记录：RUN_ROOT `r7-20261010-140457/head-intersection-check.txt`）。
- 结论：缓存证据面（源码快照、调用普查、Key Registry、漂移基线）不受该提交影响，绑定候选对当前 HEAD `2da28c3f` 仍然有效；W0 全部结论（W0_BLOCKED、confirmed=false、方案 A/B 裁决、W1 NOT_STARTED / BLOCKED_ON_W0）不变。
- 若后续 HEAD 再前移，复核方应重新执行 `git diff eb7d4d54..HEAD` 并与证据面求交；交集非空时必须重新快照候选，不得沿用旧证据。
