# 工程守则（跨车道硬规则 / engineering-guardrails）

> 版本 1.0 | 2026-11-25 | 源出 R0 容量治理车道（P1-1）实证
> 证据链：同目录 `r0-capacity-remediation-design.md`（§11 勘误）、`r0-capacity-test-results.md`（六态总账）、`r0-capacity-status.json`；实施 PR #78（merge commit `62ebb54ed`）。
> 根级 `AGENTS.md`「AI 使用指引」第 9 条指向本文件。修改任一条前先复核证据是否仍然成立。

## 1. cowboy/WS 生命周期改动：必须隔离节点真实链路验证

**规则**：凡涉及 cowboy websocket handler、ranch listener 生命周期（升级、接管、supervisor 模式、listener opts）的改动，合并前必须在隔离 release 节点用真实升级链路验证；EUnit/meck mock 不能作为此类行为的证据。

**背景（R0 实证）**：

- cowboy supervisor 模式升级会**替换进程**：`init/2` 的 pid 在 101 升级后 ~1s 内退出，ws 循环跑在另一个幸存进程。R0 第一版准入名额挂在 init pid 上，EUnit 全绿、cap 仍静默失效——只有隔离节点 recorder 抓到（勘误与修复见 `6825219c2`）。
- ranch `normalize_opts/1` 把 bare-list 形态 TransportOpts **只当 socket_opts**：钉住 `max_connections`/`num_conns_sups` 必须 map 形态（TLS 证书也要放进 socket_opts），否则误传 gen_tcp。

**操作要求**：

- 隔离 release 节点：复制 vm.args 改 `-name`，以 `VMARGS_PATH=<副本>` 启动（`NAME=` 在 `foreground` 模式不生效）。
- 停止节点只用 RPC `init:stop`（release 带 `-heart`，强杀会被拉起）。
- 验证点必须覆盖真实 WS 握手 → 升级 → 幸存进程认领 → 释放的全程。

## 2. 容量数字口径：预算参数 ≠ 实测容量

**规则**：20,000 并发连接是属主批准的**预算参数**，不是实测生产承载能力；任何文档、报告、对外材料不得写成"实测/已验证的生产容量"。交付报告必须区分 已实现/已编译/已测试/已压测/已验证/尚未验证。

**背景（R0 实证）**：本机 rig 对真实 imboy 的服务端稳态天花板 ≈1.6–1.8k（recorder 三轮复现）；`bench_websocket.sh` 在 ≥3k 时报的 connected/held 是客户端半开假象（裸 cowboy 可到 16.2k；真实服务含 per-conn 鉴权/DB/user_server 成本）。

**操作要求**：引用容量数字时写明口径（预算参数/客户端口径/服务端实测）；压测前先认清 rig 天花板。

## 3. EUnit 库纪律：一次性库 + 聚焦枚举

**规则**：PG 相关测试禁止打共享 dev 库；"全量绿"结论必须先确认枚举范围。

**背景（R0 实证）**：裸 `make eunit` 经 `config/sys.runtime.config` 默认连共享 dev 库 imboy_v1@4323——迁移守卫（out_of_order[275]）拦截后零写入，但那是守卫在挡，不是流程在挡。另外裸 `make eunit` 依 erlang.mk 枚举（剔除与 src 模块同名的 `*_tests`），**新增测试模块根本不在全量跑里**。

**操作要求**：

- 用 `IMBOY_PG_*` env 覆盖指向一次性测试库（容器 `imboy/pg18:3.7.0-1`，12 扩展按 `test/common/inttest_marker_db.erl` 顺序安装）。
- 分支自有覆盖须 `EUNIT_MODS=<mod>` 聚焦跑 + CT 定向跑；不要把"全量没跑过它"误读成"全量过了它"。
- 失败集比对：与上一次 triage 清单逐项 `comm` 比对再下"零新增"结论；CI 基线红灯逐 job 双侧取证（PR 侧 + main 侧各跑一次同 job）。

## 4. 进程/端口清理：只用精确 PID

**规则**：禁止用路径/名称模式杀进程（`pkill -f <模式>`、宽匹配 `pgrep` 管道）。

**背景（R0 实证）**：`pkill -f "_rel/imboy/bin/imboy"` 两次误杀主检出 prodready_e2e（worktree 与主检出 argv 同含该路径；`pgrep -fl beam` 因 argv 重写漏检）；heart 连带清掉后 beam 自停。

**操作要求**：先 `lsof -ti :<端口>` 等精确手段枚举 → 逐 PID 核对 cwd/argv → 按 PID 处理；停止 release 节点用 RPC `init:stop`（见 §1）。
