# Limiter V1 AC-16 旧基线——冻结输入表（L0，HEAD 0d28ad4f）

计划依据：docs/plans/2026-10-13-limiter-v1-local-evidence-and-plan.md §7.6 / 附录 C.9。
本表在测量运行前冻结；所有测量按本表执行，缺值标 INPUT_PENDING。

## 硬件 / 软件平台

| 项 | 值 |
|---|---|
| 硬件 | Apple Silicon（arm64）12 性能核在线（schedulers_online=12）、36GB 内存 |
| OS | macOS 27.0.1（sw_vers -productVersion 实测，见 JSON env.os_version） |
| OTP | 29（erts 17.0.2；实测见 JSON env.otp/env.erts） |
| 编译档 | erlfmt 后 `make compile` 默认档（+O2 默认，无额外 ERLC_OPTS） |
| 被测代码 | git HEAD `0d28ad4f`，工作区无 WIP（干净 checkout） |
| 测量节点 | 独立 escript VM（-noshell +A 8/-A 4），非共享节点，无 DB 依赖（login_attempt_ds + imboy_cache/depcache + throttle 最小启动；探针先验证启动合同） |

## 输入（Key 与分布）

| 项 | 基线 B（同有效记账工作量） | 基线 A（E2E 旧真实成本） |
|---|---|---|
| 账户键 | 10000 个 `acct_bench_NNNNNN`（16B）均匀轮转 | 500 个 `acct_e2e_NNNNNNN`（16B）唯一账户各 1 次 |
| IP 键 | 1000 个 `10.0.XX.YYY`（9-10B）均匀轮转；check_ip 每 IP ≤100 次=默认 `login_max_ip_attempts_per_hour`(100) 限内 → 全部事件有效记账 | 旧代码 Ip 恒 `<<>>`（do_login_verify/4 硬编码，真实本地直连形态） |
| throttle 规则 | `throttle:setup(passport_per_ip, 10, per_minute)`（生产默认） | n/a（HTTP 层不在链内） |
| 账户域规则 | `login_max_attempts`=5、`login_lock_duration_minutes`=30（代码默认） | 同左 |
| 事件规模 | 100k/场景；混合=80% check_ip + 15% record_failure + 5% reset（reset 仅对本轮 record 过的键 → 全部有效删除） | N=500 错误密码（真实 verify_user 失败路径） |
| 并发 | 1 worker 与 8 worker（8×12.5k；spawn_monitor+ref 收齐，不 sleep） | 1 worker 顺序 HTTP 语义（函数级直调） |
| warmup | 10k（record + check_ip + throttle 各覆盖） | 20 次 do_login |
| 独立重复 | 每场景 3 次 | 3 次 |
| 认证耗时模型 | n/a（不 mock 被测状态层；认证不在 B 链内） | 真实 elib_password verify（default_md5+salt 档，~µs 级）；mock 边界仅 user_ds find_by_*（内存 stub 返回合成用户 status=1） |
| 计时 | erlang:monotonic_time(nanosecond) 每事件一对时间戳 | 同左 |
| 采集口径 | 调用者进程 reductions 差值/事件（含每事件计时开销，各场景一致口径）；ets:all() 总 memory 差值；混合 8w 后 owner mailbox_len | 同左；另记录 get_attempts 记账数与 IP Cache 键实际写入值 |

## 已知偏差（如实记录，不虚构）

1. 基线 B `check_ip_rate_limit`/混合场景中 `imboy_cache:flush()` 用于场景间清零；flush 语义为 depcache 全实例清理（诊断脚本验证生效）。
2. 基线 B `throttle_check` 场景 ok=9000/deny=91000（每 IP 前 9-10 次允许；跨重复的 reset_state 对 throttle_ets `scope_counters` 表清理后首 run 残留 warmup 计数——允许/拒绝成本同路径，分位数不受影响）。
3. 基线 B 混合场景 `record_failure` 部分键达到 5 次锁定阈值后旧实现返回 `{ok, N, locked}` 三元组——仍计入 ok（记账发生），不并入 denied。
4. 基线 A 未跑真实 Cowboy HTTP handler（任务授权：escript 函数级真实链 P→S→D→verify_user）；真实 handler 链由 AC-01 REST case（新实现侧）覆盖。
5. max 值受 BEAM 调度/GC 抖动影响（观测到个别 ~10ms 尖峰），已保留原始 3 次重复区间，不取单次最值做结论。
