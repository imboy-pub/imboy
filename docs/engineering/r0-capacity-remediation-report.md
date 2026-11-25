# R0 容量治理总结报告（r0-capacity-remediation-report）

> 版本 1.1 | 2026-11-25 | P1-1 修复收口 / IMBoy Runtime Risk Remediation R0（v1.1：PR #78 review 修正，见 §2.1 / §3.1）
> 分支 `fix/r0-capacity-limits-20261123`（基于 main `a5c74219f`）· PR #78（review 修正见 §2.1；合并状态以 GitHub PR 为准）
> 六态总账与逐项证据：`r0-capacity-test-results.md` §3 / 机制矩阵 §1

## 1. 交付总览（D0–D8 批准 → 实施对照）

| 决策 | 批准内容 | 实施 | 状态 |
|---|---|---|---|
| D0 | 新增应用层 WS 准入 | `ws_admission.erl` + `websocket_handler` 两段式闸（init/2 软闸 503 + websocket_init 原子认领 + 1013 兜底 + 5s 清道夫 + 对账自愈） | ✅ 已验证（V1–V7, V9） |
| D1 | 单节点 WS 上限 20,000 | `IMBOY_MAX_WS_CONNECTIONS`（默认 20000，1..1000000 校验，非法值启动即拒） | ✅ 已实现；**绝对并发值未验证**（rig 上限 ≈1.7k，见 test-results §2.1） |
| D2 | Docker/dev `+P/+Q` 65536 | `config/vm.args`、`scripts/start_node.sh`；release 烘焙逐字一致 + 启动读数一致 | ✅ 已验证（V13/T8/T1） |
| D3 | `+S` 保持 2 + 可注入 | vm.args 保持 `+S 2`；`IMBOY_MAX_SCHEDULERS`（空=沿用，1..1024 校验）经 install.sh 生成 `+S N` 进 ERL_FLAGS | ✅ 已实现 + T7；`.env` 注入链经一键安装套件覆盖（V14，review 修正） |
| D4 | HTTP 阶段显式 1024×10 | `imboy_app:listener_socket_opts/1` **map 形态**钉住（ranch 只把 list 当 socket_opts——本批新发现的实现陷阱，已注释 + T2 防回归） | ✅ 已验证（启动 ranch:info 读数） |
| D5 | 升级前 503 + Retry-After | 满载软闸 `503 retry-after=5 body={"code":503,"msg":"ws_capacity_full"}`（实测）；竞态窗口兜底 = 升级后 1013 关闭；Cowboy 语义与 Flutter 退避均按批准稿核实 | ✅ 已验证（V2/V3） |
| D6 | 指标修复 + 80% 告警 | `imboy_ws_connections_current` ← `imboy_syn:count()`（真实在线注册，含 AI agent 成员——语义注记已写入代码）；新增 `imboy_ws_admission_current/limit/rejected_total`；`ImBoyWSAdmissionHigh`（ratio>0.8，随注入上限自适应） | ✅ 已验证（V10/V11 规则面；Prometheus 端到端未搭栈） |
| D7 | 删 compose 死 `ERL_MAX_PORTS` | community/demo 已删 + 注入链；**主检出未跟踪的 docker-compose.prod.yml 未动**（属主部署入口，按属主指示单独处理） | ✅ 已实现（T7 静态断言） |
| D8 | cookie 另立任务 | 未纳入本批（vm.args 占位 cookie 维持，W6-A02 口径） | ✅ 范围守约 |

**属主强制约束逐条落实对照**：见设计文档 §13.1（含实施勘误：准入执行点移至幸存进程）。

## 2. 本批最重要的两个技术发现（均已实证并落勘误）

1. **cowboy supervisor 模式下升级接管会替换进程**（推翻第一轮审计 §0"原地接管"）：init/2 进程在 101 后 ~1s 内退出，真实 ws 循环在另一幸存进程。第一版准入把名额挂在 init 进程上，被清道夫在 5s 内漏排活连接的槽位（cap 静默失效）——隔离节点 recorder 实测捕获后重构为两段式（提交 `6825219c2`）。**若未经隔离节点实测直接上生产，容量上限会静默失效。**
2. **ranch 对 list 形态 TransportOpts 只当 socket_opts**（`normalize_opts/1` 把 list 包成 `#{socket_opts => list}`）：把 `max_connections` 追加进既有 `[{port, Port}]` list 会误传给 gen_tcp 使监听器起不来。钉住必须用 map 形态（TLS 证书拼装同步重构进 socket_opts）。

其余勘误与发现（ets 阈值式 badarg、bench 幻觉、rig 天花板、metrics JSON 直方图既有缺陷等）见 `r0-capacity-remediation-design.md` §11 与 `r0-capacity-test-results.md` §2.1/§4。

### 2.1 PR #78 review 追加的两项发现（2026-11-25）

3. **安装器生成的 `.env` 必须能被 bash `source`**：首版 `install.sh` 把 `IMBOY_ERL_FLAGS=+P 65536 +Q 65536` 不带引号写入。`nginx/init-letsencrypt.sh` 与 `scripts/sanity_check.sh` 以 `set -a; . ./.env` 读回，bash 把该行当作"前缀赋值 + 命令 `65536`"（`command not found`，exit 127，变量不会生效）——开了 `-e` 的 `init-letsencrypt.sh` **当场中断，全新安装的首次 TLS 签发失败**；`sanity_check.sh` 没有 `-e`，只多一行报错后继续。由 PR 的 CI（仓库自带一键安装离线套件，7 FAIL）抓到；本车道自写的 T7 沙箱只对文件做文本断言，没有以脚本的真实用法消费生成物，所以创建 PR 前的所有本地验证都没发现。修正：带引号落盘（bash 读回完整、compose 解析时剥引号，已用真实 `docker compose config` 核对），并把断言补进仓库套件（提交 `282c3c57c`）。**教训：验证部署脚本时，必须用它下游脚本的真实消费方式去读产物。**
4. **`timer:apply_interval/4` 的句柄不是 reference**：返回的是 `{interval, Ref}` 元组。照 `msg_rate_logic` 惯用法写的 `OldRef when is_reference(OldRef) -> timer:cancel(OldRef)` 守卫恒假，重复 `init_table` 从不取消前一个清道夫定时器（注释承诺的"防多个并行清道夫"并不成立），且触发 dialyzer 的 opaque/契约告警。生产启动只调一次 init，故线上无影响；修正为经带契约的读取函数直接取消，并加回归用例（提交 `ff15a3eae`）。自审同时修正了 `ensure_table` 惰性建表路径的一处覆写竞态（并发首连的计数可能被初始化覆写回 0）。

## 3. 验证账本（摘要）

- 编译：worktree `make compile` 全绿；`make rel` alpha.84。
- 单元/契约：`ws_admission_tests` 17/17、`imboy_capacity_config_tests` 11/11、`imboy_app_tests` 9/9、T7 install.sh 沙箱 6/6。
- 全量 EUnit（隔离一次性测试库）：**14973 过 / 31 败**——2 败为本分支引入（监听器契约，已修复复绿）；8 败经 stash A/B 对照证实为**基线既有**（attachment OS env 门、cs 白名单 2、imboy_syn 1、user_region 1 等与本分支无关）；其余 21 为全量跑池态污染（聚焦复现即消失）。
- CT：`websocket_connection_flow` **19/19**（新增 2 个真实 handler 准入用例）。
- 隔离节点冒烟：登录 200、WS 建连/关闭、满载 503+Retry-After、风暴 1013 无超额、硬杀 5s 回收、重启恢复、ERL_FLAGS 运行期覆盖、指标四族实测正确。
- 压测：阶梯 2k/5k/10k/12k（客户端口径）+ **服务端稳态天花板 ≈1.6–1.8k（recorder 三轮复现）**；超限在 cap=1/5/1500 直测。
- **明确未验证**：20,000 绝对并发、Prometheus 端到端触发、生产节点（PRODUCTION_NOT_VERIFIED 维持）、>3k 稳态服务端口径。

### 3.1 review 修正后的重跑（2026-11-25）

逐项归因与证据见 `r0-capacity-test-results.md` §5。PR 首版的 7 项 CI 失败中，**本分支引入的是 3 类**：安装套件 7 FAIL（回归）、elvis `ws_admission.erl` 3 条、dialyzer `ws_admission.erl` 2 条；其余（契约导出漂移、redocly、模块边界、其他文件的 elvis/dialyzer、超大文件 68 个）与 main 基线逐指纹相同，未做改动、如实上报。

修正后：编译 / erlfmt 通过；elvis、dialyzer 在本分支文件上均为 0 条（两者都做了"修复前会报、修复后不报"的阴性对照）；EUnit 聚焦 38/38（`ws_admission_tests` 18/18）；CT 19/19；一键安装套件在 ubuntu-24.04 容器 128/0（修复前 117/11，逐字复现 CI 的 7 条）；隔离 release 节点真实 WS 9/9 + 定时器清道夫 7/7。全量 EUnit 重跑结果记于 test-results §5.3。

## 4. 回滚

`r0-capacity-rollback.md`：7 个逻辑单元各自可 revert；运行期回退通道（`IMBOY_MAX_*` → install.sh 重算 ERL_FLAGS，无需重建镜像）已实测（V13）。

## 5. 遗留与建议（不阻塞 R0 收口）

1. **syn 陈旧行观察**：长寿命节点上发现数小时前的 syn 成员行（bench-000000）未被清理——syn 的本地成员清理在特定退出形态下可能滞后；建议另立观测任务（与本批准入无关：准入名额自带进程存活对账）。
2. **metrics JSON 端点既有缺陷**：`elib_metric` 直方图 float 桶键使 jsone 编码崩（`/metrics` 默认 JSON 分支 500；`Accept: text/plain` 正常）——第一轮审计未覆盖，建议另立修复任务。
3. **bench_websocket.sh 与现行鉴权契约脱节**：登录签发的 token 绑定 did（Task 10 后 did 以 token 为权威），脚本假 did 头失效 → 同设备踢下线风暴；建议升级脚本按 legacy token 或多 token 模式重建。
4. `imboy_online_users` 指标用 `syn:registry_count(imboy)`（registry 计数）而非 `imboy_syn:count_user/0`（group 计数）——语义待属主裁夺，本批未动。
5. 属主可选：在生产节点执行设计 §2 只读命令回填，把 PRODUCTION_NOT_VERIFIED 升级。
6. **`msg_rate_logic:init_table/0` 的同款定时器守卫**（review 相邻发现，非本批引入）：`OldRef when is_reference(OldRef)` 对 `timer:apply_interval/4` 的 `{interval, Ref}` 句柄恒假，重复 init 不取消旧定时器；该模块以 `-dialyzer({nowarn_function, [init_table/0]})` 消音了对应告警。生产只 init 一次，无实际影响；建议另立小任务对齐 `ws_admission` 的写法并撤掉消音。
7. **main 基线 CI 红灯**（非 R0 范围）：自 2026-11-24 08:08（`a9494cc37`）起 API Contract Gate / imboy Quality Gate / Backend CI 为 failure——`personal-history` 路由未做 `make contract-export`、redocly 2 errors、`user_handler` 直接引用 `geo_regions_ds`（模块边界）、elvis NEW 7 文件、dialyzer NEW 5 条。属其他车道在 main 上的既有状态，建议属主另行分派。

## 6. 事故披露

见 `r0-capacity-test-results.md` §4（prodready_e2e 误杀×2 均已按原参数恢复至 ping=pong/healthz=200；eunit 险些指向共享 dev 库被迁移守卫拦下、零写入；worktree 外部清理后原样恢复）；review 期间的披露见同文件 §4 第 4–5 条（PR 创建后 CI 抓到本分支回归并已修正；隔离节点首启时节点名未按预期隔离约 1 分钟，随即纠正，无影响）。
