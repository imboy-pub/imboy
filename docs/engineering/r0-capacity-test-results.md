# R0 容量测试结果（r0-capacity-test-results）

> 版本 1.0 | 2026-11-24 | R0 阶段 F/G 交付物
> 环境：本机隔离（worktree release `1.0.0-alpha.84` @ :9821 + 一次性 PG 容器 `r0-capacity-pg` @127.0.0.1:15432 + python3.15 venv `websockets 17.4`）
> 严禁外推：本报告全部数字**只代表本机隔离环境**，不构成任何生产容量承诺。

## 1. 机制验证矩阵（实现正确性）

| # | 验证项 | 结果 | 证据 |
|---|---|---|---|
| V1 | 并发准入竞争不超额（原子 admit） | ✅ | EUnit `concurrent_admission_race_never_over_admits_test`（30 进程抢 cap=8 → 恰 8 准入 22 拒绝，计数终值=8）； OTP 29.0.2 阈值式 `{Pos,Incr,Threshold,Set,Reset}` 实证恒 badarg → 2-op 形态（/tmp 探针 50 轮×8 进程零违例） |
| V2 | 超上限升级前 503 + Retry-After | ✅ | 隔离节点 cap=5：满载后 3 条晚到连接全部 `HTTP 503 retry-after=5 body={"code":503,"msg":"ws_capacity_full"}`；cap=1500 节点：计数垫到 1500 后真实 WS → `HTTP_503 retry-after=5` |
| V3 | 并发风暴窗口无超额准入（升级后兜底） | ✅ | cap=5 同时 8 条：恰好 5 条准入、3 条被升级后 1013 Try Again Later 关闭，`rejected_total=3`，已提交名额=5 |
| V4 | 正常关闭释放 | ✅ | bench 拆线后 `imboy_ws_admission_current` → 0（多轮）；单连接保持 18s 全程可观测、关闭即回落 |
| V5 | 异常退出可靠清理 | ✅ | `exit(Pid, kill)` 硬杀 ws 进程 → 周期 sweep 5s 内 `{ws_admission_swept_dead_slots,1,1}`，current 2→1 |
| V6 | 计数泄漏自愈 | ✅ | EUnit 漂移修复用例（超计/欠计双向 repair + 负值夹取） |
| V7 | 服务重启计数恢复 | ✅ | 表随 VM 重建为 0 = 真实在线状态；重启后 `[capacity] … ws_admitted=0` 且新连接正常准入 |
| V8 | 多 listener 共用单节点总预算 | ✅ | EUnit `budget_is_node_global_across_surfaces_test`（全局表，cap=1 时另一"listener"连接被拒） |
| V9 | 注册（鉴权）失败回滚 | ✅ | CT `admission_slot_released_on_auth_failure`（cap=1 下 401 后立即可准入）；单元 `handshake_failure_rolls_back_slot_test` |
| V10 | 指标正确性（建立/关闭/异常/重启） | ✅ | `imboy_ws_admission_current/limit/rejected_total` + `imboy_ws_connections_current`（syn 换源）在上述各场景实测值正确；recorder 显示 admission 与 syn 在采样规模内逐点一致（±2 = agent 成员） |
| V11 | 80% 水位告警 | ✅（规则面）/ ⚠️ 未在真实 Prometheus 触发 | 规则 `ImBoyWSAdmissionHigh`（ratio>0.8 for 2m）已入 `imboy-alerts.yml`，指标名与导出按名对账（静态测试）；未搭 Prometheus 栈做端到端触发验证 |
| V12 | 现有登录/REST/WS 消息回归 | ✅ | CT 19/19（含真实 token 建连、心跳、C2C 收发、CLIENT_ACK、重连、并发、清理 6 组）；隔离节点 REST 登录 200、WS 消息链路（CT 覆盖）；全量 EUnit 14973 过 |
| V13 | Release 参数一致性（T8） | ✅ | `_rel/imboy/releases/1.0.0-alpha.84/vm.args` 与 `config/vm.args` 逐字一致；启动 `erlang:system_info` 读数 = 65536/65536/2；`ERL_FLAGS="+P 32768 +Q 32768"` 运行期覆盖实测生效（32768/32768），回滚通道可用 |

## 2. 容量阶梯（阶段 F，隔离环境实测）

节点：`+P/+Q 65536`、`+S 2`、`schedulers_online=2`、macOS arm64、单客户端节点（bench 50/s 匀速爬坡）。

| 档 | 目标 | 客户端结果 | 服务端实况 | 判读 |
|---|---|---|---|---|
| 基线 | — | — | processes=360 ports=12 mem≈117MB RSS≈259MB | 空闲基线 |
| F1a | 2,000 | 2000/2000 连接+保持 8s；握手 p50=1.5ms p95=6.2ms | 未采峰（采样窗口错位） | 机制正常 |
| F1b | 5,000 | 5000/5000 连接+保持 12s；p50=2.0 p95=20.8 p99=27.1 | 未采峰；拆线后 RSS≈590MB 回落中 | 机制正常 |
| F1c | 10,000 | 10000/10000 连接+保持 12s；p50=2.0 p95=20.9 | 采样器 erpc 超时自毁，无平台期样本 | **客户端口径**；服务端稳态未证实 |
| F2 | 12,000 | 12000/12000 连接+保持 15s；p50=8.0 p95=24.6 | 峰值采样 processes≈1212 ports≈865（爬坡中段 admission≈899） | **客户端口径** |
| F3 依据 | — | — | **节点内 recorder 三轮复现：服务端可持续稳态 admission ≈1.6–1.8k**（1759/1755/1708，行存活率 99.3%） | **本机 rig 对真实 imboy 的实际承载上限** |
| F4 | cap=1500、推 3,000 | 3000/3000（客户端口径） | 闸门直测：计数垫 1500 → 真实 WS 得 `HTTP 503 retry-after=5`；rejected/上限语义正确 | 超限行为在 rig 可承载规模上验证 |
| F5 | cap=5 全套 | — | V2/V3/V5 全过 | 超限行为小规模完备验证 |

### 2.1 必须如实声明的限制

1. **20,000 并发上限值未在本机验证**——rig 服务端稳态天花板 ≈1.7k，无法承载 2 万级稳态压测。20,000 是批准的**预算参数**（预算模型推导），不是实测容量。
2. `bench_websocket.sh` 在 ≥3k 并发时 `connected/held` 是**客户端幻觉**（服务端已不能维持，客户端无流量感知半开连接）；本报告 ≥3k 档全部标注"客户端口径"。
3. 真实 imboy 每连接有鉴权（DB）+ `user_server` 投递 + syn 注册成本，与第一轮裸 cowboy 探针（16.2k）不可比；`user_server_shard_overloaded`（单 uid 3000 连接打爆分片队列 1000）在压测中大量出现——**单 uid 海量设备属于压测形态特例**，不构成产品缺陷证据。
4. 蓝绿/native-shell 路径未实测（数值本批未改，由 T1 下限一致性静态测试守住）。

## 3. 六态验收总账（属主要求的最终报告口径）

| 状态 | 项目 |
|---|---|
| **已实现** | 应用层 WS 准入（软闸 503 + 幸存进程原子认领 + 1013 兜底 + 清道夫 + 对账自愈）；监听器显式钉住 1024×10（map 形态）；死键删除；指标换源 + 3 个新 gauge + 80% 水位告警；VM 派生默认值 65536/65536；install.sh/compose/env.example 注入链；回滚文档；设计勘误 |
| **已编译** | worktree `make compile` 全绿（多次增量）；`make rel` 产物 alpha.84 |
| **已测试** | EUnit：`ws_admission_tests` 17/17、`imboy_capacity_config_tests` 11/11、`imboy_app_tests` 9/9（契约更新后）；全量 `make eunit`（隔离 DB）：**14973 通过 / 31 失败**，其中 2 失败为本分支引入（imboy_app 契约，已修复复绿）、8 失败经 A/B 基线（stash 对照）证实为**分支基线既有**、其余 21 为全量跑池态污染（聚焦复现即消失）；CT `websocket_connection_flow` **19/19**（含 2 个新准入用例）；T7 install.sh 沙箱 6/6 |
| **已压测** | 隔离环境阶梯：2k/5k/10k/12k（客户端口径）+ 稳态天花板 ≈1.7k（服务端 recorder）+ 超限直测 cap=1/5/1500 |
| **已验证** | V1–V10、V12、V13（见 §1） |
| **尚未验证** | ① 20,000 绝对并发（本机不可能）② Prometheus 端到端告警触发（未搭栈）③ 生产节点（PRODUCTION_NOT_VERIFIED 状态维持，属主未回填 §2 只读命令）④ >3k 稳态服务端口径（rig 限制）⑤ docker 镜像构建后的 vm.args 检视（无 backend 镜像构建环节，静态 T1 + release T8 已覆盖同源文件） |

## 4. 过程事故记录（如实披露）

1. **prodready_e2e 误杀与恢复**：清理隔离节点时 `pkill -f "_rel/imboy/bin/imboy"` 模式误中主检出节点（argv 同含该路径；此前 `pgrep -fl beam` 因 argv 重写漏检误判"无运行节点"）。主检出节点（alpha.75/prodready_e2e）被杀两次（第二次为 heart 误清连锁），均已按原参数（NAME/COOKIE/heart）从主检出 release 原样拉起恢复，`ping=pong`、`/healthz=200` 终态确认。未触及 imboy_v1@4323 数据库内容。
2. **eunit 配置险情**：`make eunit` 默认经 `sys.runtime.config` 指向共享 dev 库 `imboy_v1@4323`；迁移守卫以 `out_of_order [275]` 拒绝带病迁移（保护生效，零写入）。全量 EUnit 改经 `IMBOY_PG_*` env 覆盖指向一次性测试库 `imboy_test_v1@5432`（重锚 + 五扩展 + 角色同步）。
3. **worktree 曾被外部清理**（11-23→11-24 之间）：分支 ref 被删、worktree 目录消失；共享对象库未 GC，按属主"保留两个已有签名提交"要求原样恢复（同路径/同名/同 SHA `c910ea2cc`+`8caaa1c5e`）。
