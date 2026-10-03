# LiveKit 单服务迁移收口计划 V2

> 日期：2026-09-24
> 状态：`READY_FOR_AUTHORIZED_CLOSURE`
> 当前结论：`PARTIAL`
> 发布结论：`RELEASE=NO_GO`
> 前序计划：`docs/plans/2026-09-21-livekit-single-service-migration-deployment-plan-v1.md`
> 前序计划 SHA-256：`28aa35a4c8439266f2ee6f84764b373af586aecb243c5945d7cf20e55a2d337e`

本文是迁移代码合入 `main` 后的收口执行合同，不是部署证明、发布授权或旧 TURN 删除授权。
执行者必须以当前源码、现场只读采样和本计划的 Acceptance 为准，不得用旧报告中的 `PASS`
替代重新验证。

## 1. 已合并基线与当前事实

### 1.1 Git 基线

以下 SHA 是 2026-09-24 完成快进合并后的**实现基线**，不包含本文档的后续文档提交：

| 仓库 | 分支 | 合并前 SHA | LiveKit 实现合并后 SHA |
|---|---|---|---|
| `imboy` | `main` | `64dd8060e0e7bf3b3e4d4bc78546a5ca80ad8f6a` | `85bc0654118bbb6e6bb4b003de5d72bccd671339` |
| `imboyapp` | `main` | `eacc5301137e9353b0ffd5de73c4dae3fa444ab5` | `e38ac726b7a0b922d4671b9b9c92985077357efb` |

两个仓库均通过 `git merge --ff-only` 合入。前序 run 的 12 个 worktree 和 12 个本地分支已删除；
证据目录保留：

```text
/Users/leeyi/project/imboy.pub/.Codex/runs/
  livekit-single-service-20260921T101700Z-622bf9b2-3527a2a8/
```

本文提交后，`imboy/main` 将自然前移到文档提交 SHA。后续执行不得要求
`imboy/main == 85bc...`，而应同时验证：

1. `85bc0654...` 是执行时 `imboy/main` 的祖先；
2. `e38ac726...` 是执行时 `imboyapp/main` 的祖先；
3. 从上述基线到执行 HEAD 的增量已经单独审计；
4. 执行时把两个完整 HEAD 和本计划文件 SHA-256 写入新 run 的 `control/baseline.json`。

### 1.2 当前生产事实

以 2026-09-24 前序 run 的只读证据为限：

- LiveKit v1.13.7 信令服务和 `wss://rtc.imboy.pub` 已上线并完成 join 证据；
- 生产 LiveKit embedded TURN **尚未启用**；
- 旧 eturnal 仍承担 `3478/5349/50201-50500`，不得视为已退出；
- 严格 TURN/TLS `:443` 仍为 `BLOCKED_TURN_443`；
- 生产媒体面、真实 relay selected candidate 和双真机完整通话矩阵没有闭环；
- 旧 TURN 的停止、卸载和残留清理均未执行；
- push、tag、PR、发布均未授权。

所以当前必须保持：

```text
LIVEKIT_SINGLE_SERVICE=PARTIAL
TURN_DOMAIN_TLS_AUTO_RENEWAL=BLOCKED_TURN_443
IMBOYAPP_C2C_AND_GROUP_RTC=PARTIAL(BLOCKED_DEVICE)
ETURNAL_COTURN_REMOVAL=NOT_EXECUTED
RELEASE=NO_GO
```

> 注记（2026-10-02）：2026-10-01 服务器曾有一轮安装器外手动 SNI 实施（历史采样
> 陈述，待服务器授权后重新采样绑定，见 1.4 节）；在重新采样完成前，上述
> 2026-09-24 清单中的生产事实陈述保持原样，不因该手动实施而自动更新。

### 1.3 当前验证实况

合并候选已完成的本地验证：

- 后端 `gmake compile`：通过；
- `rtc_room_logic_tests`：10/10 通过；
- `lk_preflight_rtc_turn_test.sh`：48/48 通过；
- `lk_turn_hook_install_test.sh`：48/48 通过；
- 模块边界检查和相关 Bash 语法检查：通过；
- App LiveKit/状态机/悬浮窗聚焦测试：37/37 通过；
- App 67 个相关 Dart 文件格式检查：零变更；
- App analyze：候选与当时 `main` 均为 27 个既有问题，LiveKit 改动未新增命中。

后端全量 `gmake eunit` **未通过**：`5117 passed / 37 failed / 0 skipped`，另有测试被取消，
退出码 2。主要阻塞包括独立 worktree 缺少本地 PostgreSQL/HTTP 配置、
`127.0.0.1:4393` 拒绝连接及已有导出/审计测试红项。该结果不得写成 PASS，也不得由聚焦测试替代。

旧计划 50 个正式 Acceptance 的当前统计为：

| 状态 | 数量 |
|---|---:|
| `PASS` | 39 |
| `PARTIAL` | 3 |
| `PARTIAL(BLOCKED_LOCAL_ENV)` | 1 |
| `BLOCKED_DEVICE` | 1 |
| `BLOCKED_TURN_443` | 1 |
| `NOT_EXECUTED` | 5 |

旧 `control/acceptance.tsv` 另含 RESULT、执行期新增 A06、LEGACY_CLIENT 等管理行；旧 TSV、
`acceptance-matrix.md` 和 `RESULT.json` 存在时间点与候选 SHA 不一致，不能继续并列充当真源。

### 1.4 L4 SNI 加固阶段记录（2026-10-02，LOCAL_HARDENING 产物）

依据 `docs/plans/2026-10-01-l4-sni-hardening-relay-verification-plan-v1.md`（L4 SNI
运维加固与真实 relay 验证计划）完成的本地候选阶段产物登记。以下为**候选实现提交
事实**（后端 base `4d9ffba2`，app 仓候选另计），不代表服务器已部署、生产已变更，
也不改变本计划任何 Acceptance ID 的取值：

- **listen 漂移巡检**：`scripts/check_l4_sni_listen.sh`，fail-closed 检测受管 vhost
  配置文本（退出码 `0` 健康/`1` 漂移含受管配置缺失/`2` 输入解析错误/`3` 推送失败；
  `--strict` 默认与 `--pre-switch` 两模式；`L4_SNI_ENV_FILE` 配置发现），107 项
  行为测试（imboy `76b09f27`、`abec6b5f`）。只检测配置文件文本，不证明运行配置
  或媒体健康。
- **指标推送助手严格模式**：`scripts/lib/metrics_push.sh` opt-in `METRICS_PUSH_STRICT=1`
  （失败/缺 URL 返回非零）与 `METRICS_PUSH_LAST_STATUS` 结果观测，默认语义零改变，
  38+16 项测试（imboy `71a68cf5`）。
- **安装器宝塔实例兼容与回滚行为**：`deploy/install-livekit-l4-sni.sh` 增加
  `resolve_nginx_instance`（`NGINX_BIN` 显式优先→宝塔路径→`PATH`，必须匹配运行
  master，歧义/自定义 `-g` 即 `BLOCKED_ENV`）、全阶段同二进制同 `-c`/`-p`、reload
  改为 pidfile 校验 + SIGHUP 兜底、备份 format=2（manifest + 逐文件 sha256 + 服务
  状态快照，**与旧 format=1 tar 断代不兼容**）、overlay 感知回滚与有效配置哈希
  校验、重复 apply 保护保留，95 项沙箱测试（imboy `a6ba4595`）。
- **App RTC 连接级探针**：`integration_test/rtc/rtc_relay_realdevice_test.dart`
  （连接级证据，evidence_level 标注）与 `rtc_relay_stats_judge.dart` 纯判定器
  （selected pair 优先、nominated 不单独成立、remote relay 不替代 local、缺字段
  即 `BLOCKED_EVIDENCE`；无 candidate.port=443 断言），37 项判定测试 + 11 项 p2p
  回归（imboyapp `b69e0d7c`）。
- **巡检接线与告警**：`deploy/cron/imboy-ops.cron` 每 5 分钟 `--strict --push` 巡检
  行与 `deploy/prometheus/rules/imboy-alerts.yml` 告警组 `imboy.l4_sni` 四条规则
  （`L4SNIListenDrift`/`L4SNICheckFailed` critical，`L4SNIStaleMetrics`/
  `L4SNIMetricsMissing` warning），promtool 50 规则 + 22 断言（imboy `f5e320cc`）。

本阶段**未执行**（均为后续 Wave 任务，未启动或未获授权）：服务器采样与候选上传
（Task 7）、上线巡检与告警管路验证（Task 9）、TLS443 relay 真机证据（Task 10，
探针未在任何真机运行）、双真机设备矩阵（Task 11）。LOCAL_HARDENING 阶段的最终
状态以该 run 的 acceptance 台账为准，不由本文档宣布；即便其判 PASS，也只说明
本地合同通过。

历史现场事实：2026-10-01 服务器曾有一轮**安装器外手动 SNI 实施**（证书、vhost、
eturnal 停用及 livekit.env/overlay 变更；历史采样陈述，采样日期 2026-10-01，待
服务器授权后重新采样绑定）。处于该手动启用状态时 `--apply` 会被重复 apply 保护
拒绝，不能通过补跑 apply 幂等接管。

本条目不改变 1.2 节任何状态常量，也不改变本计划任何 Acceptance ID
（`CL-TEST-01-A01..A06`、`CL-DEVICE-01-A01..A07`、`CL-NET-01-*` 等）的取值；原表
仍按原合同另行验收，不因本阶段本地完成而自动 PASS。运维候选细节与"待服务器核实"
清单见 `docs/guides/operations/deployment/livekit-turn-443-l4-sni.md`（候选实现/
待上线口径）。

**2026-10-03 后续记录**：上述为 Wave A 时点。后续服务器只读复采确认巡检
cron 正常，现役巡检脚本与 main hash 一致；实际 LiveKit 使用单文件 Compose
与 standalone `docker-compose`，手动恢复候选已按此来源整理。监控按用户
决定只完成默认关闭的开关及步骤，不启动、不对接。设备组合确认为 Android
MRD AL00 + macOS App；macOS 严格入口缺参负例退出 1，不算 relay/媒体证据。
真实连接、双向媒体与完整矩阵仍待合成账号和资源条件。当前证据 run 为
`.Codex/runs/20261003-l4-sni-main-completion/`；本补记不改变原 Acceptance ID
或原合同状态，RELEASE=NO_GO。

## 2. 目标、边界与授权

### 2.1 唯一目标

在不掩盖降级项的前提下，完成生产安全整改、TURN 443 模式决策、历史客户端影响评估、
Linux 真实 relay、双真机通话矩阵、证据一致性和旧 TURN 最终处置，使所有 Required Acceptance
有可复现证据，并给出诚实的 `PASS` 或 `BLOCKED` 终态。

### 2.2 本计划不授权

本文不授权以下动作；每一类都必须由用户对明确目标单独授权：

- push、tag、PR、合并远端分支、应用商店或任何形式发布；
- SSH 写操作、生产部署、重启、停机、生产迁移；
- DNS、安全组、EIP、CLB、L4 SNI、证书签发或防火墙变更；
- 停止、卸载、删除 eturnal/coturn，删除生产进程、容器、文件或数据；
- 联系、通知、@提及任何第三方，或设置/使用联系方式；
- 使用真实用户、真实用户数据或 PII 做测试；
- 将 secret 原值写入命令行、日志、证据、提交或对话。

`继续` 仅表示继续做最小安全的本地/只读收口，不扩大上述权限。

### 2.3 Owner 与写权限

| 角色 | 职责 | 独占写路径/资源 |
|---|---|---|
| A0 / Coordinator | 基线、租约、集成、Acceptance、最终判定 | 新 run 的 `control/`、`FINAL/`；唯一可更新最终状态 |
| SEC owner | Cookie/epmd 预案与获授权后的执行证据 | `agents/SEC/`；生产变更窗口需单独授权 |
| NET owner | TURN 443 决策材料与所选网络合同验证 | `agents/NET/`；不得代用户选择模式 |
| COMPAT owner | 历史客户端覆盖率和最低版本策略 | `agents/COMPAT/`；只读数据或匿名聚合 |
| TEST owner | Linux relay 与服务端 allocation 证据 | `agents/TEST/`；独占测试端口和容器名 |
| DEVICE owner | 两台授权真机的 C2C/群通话矩阵 | `agents/DEVICE/`；独占设备租约和合成账号 |
| REMOVE owner | 旧 TURN 处置预案；获二次删除授权后执行 | `agents/REMOVE/`；未授权时只读 |

如果未启用并行 agent，由同一执行者按上述角色顺序串行完成，路径和授权边界不变。
A0 是共享路径、最终状态和任何生产写入的唯一集成者。

## 3. 新 run、SHA 绑定与证据合同

### 3.1 初始化

在工作区根执行，先确认根目录不是 Git 仓库：

```bash
cd /Users/leeyi/project/imboy.pub
test "$(git -C imboy rev-parse --show-toplevel)" = "/Users/leeyi/project/imboy.pub/imboy"
test "$(git -C imboyapp rev-parse --show-toplevel)" = "/Users/leeyi/project/imboy.pub/imboyapp"

RUN_ID="$(date -u +%Y%m%dT%H%M%SZ)-livekit-closure-v2"
RUN_ROOT="/Users/leeyi/project/imboy.pub/.Codex/runs/$RUN_ID"
OLD_RUN_ROOT="/Users/leeyi/project/imboy.pub/.Codex/runs/livekit-single-service-20260921T101700Z-622bf9b2-3527a2a8"
mkdir -p "$RUN_ROOT"/{control,agents,FINAL}
sha256sum imboy/docs/roadmap/2026-09-24-livekit-single-service-closure-plan-v2.md \
  > "$RUN_ROOT/control/plan.sha256"
git -C imboy rev-parse HEAD
git -C imboyapp rev-parse HEAD
git -C imboy status --short --branch
git -C imboyapp status --short --branch
```

macOS 无 `sha256sum` 时使用：

```bash
shasum -a 256 imboy/docs/roadmap/2026-09-24-livekit-single-service-closure-plan-v2.md \
  > "$RUN_ROOT/control/plan.sha256"
```

### 3.2 单一事实源

新 run 的唯一状态真源是：

```text
$RUN_ROOT/control/acceptance.tsv
```

固定列：

```text
task_id	acceptance_id	required	status	evidence_sha256	evidence_path	note
```

规则：

1. `acceptance.tsv` 只允许 A0 更新；每个 Acceptance 恰好一行；
2. `status` 只允许 `PENDING/PASS/BLOCKED/WAIVED_BY_OWNER/FAIL`；
3. Required 项只有 `PASS` 才能进入总 `PASS`；
4. 若选择 `NO_STRICT_443`，仅对应的严格 443 产品能力可标 `WAIVED_BY_OWNER`，
   但“决策已记录并按所选合同验证”这一 Required Acceptance 仍须 `PASS`；
5. `acceptance-matrix.md`、`RESULT.json`、`FINAL/REPORT.md` 必须从 TSV 机械生成；
6. 生成后必须反向校验行数、状态计数、证据文件存在性和 SHA-256；任一不一致即 `FAIL`；
7. 证据只记录 secret 的存在性、长度和新旧指纹，不记录原值。

### 3.3 基线漂移停止条件

任一条件成立即停止写操作并报告 `BLOCKED_BASELINE_DRIFT`：

- `85bc0654...` 不再是 `imboy` 执行 HEAD 的祖先；
- `e38ac726...` 不再是 `imboyapp` 执行 HEAD 的祖先；
- 两仓存在未归属的工作树改动、未知 worktree 或重叠 writer；
- 本计划文件 SHA 与 `control/plan.sha256` 不一致；
- 生产资源 owner、端口、DNS、证书或现役版本相对前序证据发生变化且尚未重新评估。

## 4. 依赖 DAG 与执行波次

```text
CL-00
  +--> CL-SEC-01 ------------------------+
  +--> CL-NET-01 --> CL-TEST-01 ---------+
  +--> CL-LEGACY-01 ---------------------+--> CL-REMOVE-01 --> CL-EVIDENCE-01 --> CL-FINAL
  +--> CL-TEST-01 --> CL-DEVICE-01 ------+
  +--------------------------------------+
```

| Wave | 任务 | 启动条件 | 可结束状态 |
|---|---|---|---|
| W0 | `CL-00` | 无 | `PASS/BLOCKED_BASELINE_DRIFT` |
| W1 | `CL-SEC-01`、`CL-NET-01`、`CL-LEGACY-01` | `CL-00=PASS` | `PASS/BLOCKED_AUTH/BLOCKED_DECISION` |
| W2 | `CL-TEST-01` | `CL-00=PASS`；网络合同已知 | `PASS/BLOCKED_ENV` |
| W3 | `CL-DEVICE-01` | `CL-TEST-01=PASS`，双设备租约有效 | `PASS/BLOCKED_DEVICE` |
| W4 | `CL-REMOVE-01` | 所有硬门 PASS + 用户再次明确删除授权 | `PASS/NOT_EXECUTED/BLOCKED_AUTH` |
| W5 | `CL-EVIDENCE-01`、`CL-FINAL` | 前置任务终态 | `PASS/BLOCKED/FAIL` |

## 5. 执行卡

### CL-00：重建基线与证据真源

**Owner**：A0
**依赖**：无
**目标**：以当前两个 `main`、本计划 SHA 和现场只读采样建立新 run，废止旧报告的并列真源地位。

执行要点：

- 双采样两个仓库的 HEAD、status、worktree、submodule，间隔至少 60 秒；
- 验证 `85bc...`、`e38ac...` 的祖先关系并审计其后的增量；
- 重新统计前序 50 个正式 Acceptance，但只作为历史输入；
- 对生产只做获授权的只读采样；不得复用旧端口 owner、证书到期日或服务状态冒充当前值；
- 初始化新 `acceptance.tsv`，预登记本计划全部 Acceptance。

Acceptance：

| ID | Required | 通过条件 | 证据 |
|---|---|---|---|
| `CL-00-A01` | Yes | 两仓双采样稳定，工作树归属清楚，无重叠 writer | `agents/A0/baseline-git.json` |
| `CL-00-A02` | Yes | 两个实现 SHA 均为当前 HEAD 祖先，增量审计完成 | `agents/A0/head-binding.txt` |
| `CL-00-A03` | Yes | 计划 SHA、run 路径、owner/lease、资源名登记完成 | `control/plan.sha256`、`control/leases.json` |
| `CL-00-A04` | Yes | 生产 LiveKit、eturnal、端口、证书、epmd 只读现状重新采样 | `agents/A0/prod-readonly.json` |
| `CL-00-A05` | Yes | 新 TSV 行唯一、Required 完整，旧矩阵未被当作新真源 | `control/acceptance.tsv` |

回滚与停止：只创建新证据，不改生产。发现漂移、未知 WIP、凭证不足或需要写生产时立即停止。

### CL-SEC-01：轮换已泄漏 Cookie 并收敛公网 epmd

**Owner**：SEC owner；生产写入仅 A0
**依赖**：`CL-00`
**现状**：11 字符生产 Erlang Cookie 曾进入两份证据，虽已本地脱敏，仍必须按已泄漏处理；
同时前序证据显示 epmd 监听 `0.0.0.0:4369`。权威事件记录：
`$OLD_RUN_ROOT/FINAL/SECRET-INCIDENT-20260924.md`。

执行前必须获得用户对以下精确范围的单独生产授权：停机窗口、目标节点、Cookie 轮换、
epmd 方案和回滚方式。不得在文档中预选用户尚未确认的 epmd 方案。

最低执行合同：

1. 只在目标服务器安全生成高熵新 Cookie；不通过命令行参数或证据传递原值；
2. 记录变更前节点/端口/健康快照和可恢复配置备份；
3. 在授权窗口轮换现役节点并验证节点启动、HTTP/WS 健康和部署控制命令；
4. 采用用户批准的防火墙或 loopback 绑定方案限制公网 4369；
5. 从授权外网络验证 4369 不可达，从允许路径验证业务无回归；
6. 全证据扫描旧/新 Cookie 原值零命中，只保留长度和 SHA-256 前缀指纹。

Acceptance：

| ID | Required | 通过条件 | 证据 |
|---|---|---|---|
| `CL-SEC-01-A01` | Yes | 用户对目标、窗口、epmd 方案和回滚作出明确授权 | `agents/SEC/authorization.md` |
| `CL-SEC-01-A02` | Yes | 现役节点 Cookie 已轮换，旧值认证失败，新值业务健康 | `agents/SEC/rotation-result.md` |
| `CL-SEC-01-A03` | Yes | 公网 4369 不可达，允许路径和业务节点正常 | `agents/SEC/epmd-result.md` |
| `CL-SEC-01-A04` | Yes | repo/run/日志 secret 扫描零原值，权限符合合同 | `agents/SEC/secret-scan.txt` |
| `CL-SEC-01-A05` | Yes | 回滚演练或可执行回滚检查通过 | `agents/SEC/rollback-check.md` |

回滚：恢复加密/受限备份中的旧运行配置、恢复原网络规则并重启原节点；旧 Cookie 已泄漏，
回滚后状态必须立即降为 `BLOCKED_SECURITY`，不能长期运行。

停止条件：授权不完整、无法确认节点归属、备份不可恢复、健康检查失败、发现第三方依赖或
任何 secret 将进入日志。

### CL-NET-01：TURN 443 模式由用户定案并验证

**Owner**：NET owner；决策者为用户
**依赖**：`CL-00`
**目标**：用户必须从以下模式中明确选择，执行者不得代选：

| 模式 | 含义 | 必须验证 |
|---|---|---|
| `DEDICATED_PUBLIC_IP` | LiveKit/TURN 独占公网 IP 的 TCP 443 | IP owner、DNS、证书、443/TCP relay、续期 |
| `EXISTING_L4_SNI` | 现有四层入口按 TLS SNI 分流 | SNI 路由、透传、证书、故障隔离、续期 |
| `NO_STRICT_443` | 明确接受不提供 TURN/TLS:443 | 书面降级、3478/5349 合同、客户端回退、风险说明 |

先做只读端口/IP/DNS/证书采样和三方案影响报告，再请求决策。任何 DNS、安全组、EIP、CLB、
证书签发或网络写入都需目标级单独授权。

Acceptance：

| ID | Required | 通过条件 | 证据 |
|---|---|---|---|
| `CL-NET-01-A01` | Yes | 三方案的成本、停机、回滚和风险基于当前现场给出 | `agents/NET/options.md` |
| `CL-NET-01-A02` | Yes | 用户明确记录一个 `TURN_443_MODE` | `agents/NET/decision.md` |
| `CL-NET-01-A03` | Yes | 所选模式获授权后按合同实施；若无需变更则记录 N/A 原因 | `agents/NET/implementation.md` |
| `CL-NET-01-A04` | Yes | 对所选合同完成真实外网 relay、证书和续期验证 | `agents/NET/verification.md` |
| `CL-NET-01-A05` | Yes | 故障注入和回滚路径验证，不影响 rtc/pro/eturnal 现役路径 | `agents/NET/rollback.md` |

停止条件：用户未决策、443 owner 不清、云资源范围不清、需新增联系方式、证书签发要求未知联系信息，
或变更将影响未授权第三方。

### CL-LEGACY-01：历史客户端覆盖率与最低支持版本

**Owner**：COMPAT owner
**依赖**：`CL-00`
**目标**：证明停止旧 TURN 不会切断仍依赖 `/api/v1/user/credential`、eturnal URL 或旧
`RTCPeerConnection` C2C 链的受支持客户端。

只允许使用已有的匿名版本聚合、发布记录、协议能力和合成客户端；需要真实用户数据、商店后台、
外部统计或通知用户时必须另行授权。

Acceptance：

| ID | Required | 通过条件 | 证据 |
|---|---|---|---|
| `CL-LEGACY-01-A01` | Yes | 列出历史版本、发布时间、RTC 实现和 credential 依赖 | `agents/COMPAT/version-matrix.tsv` |
| `CL-LEGACY-01-A02` | Yes | 给出匿名覆盖率；无可靠数据时明确 `BLOCKED_DATA`，不得猜测 | `agents/COMPAT/coverage.md` |
| `CL-LEGACY-01-A03` | Yes | 用户批准最低支持版本、强更/宽限/回滚策略 | `agents/COMPAT/min-version-decision.md` |
| `CL-LEGACY-01-A04` | Yes | 最低受支持版本在旧 TURN 停止模拟下仍可完成 C2C/群通话 | `agents/COMPAT/compat-test.md` |
| `CL-LEGACY-01-A05` | Yes | 对低于最低版本的产品影响和恢复办法有明确记录 | `agents/COMPAT/fallback.md` |

停止条件：覆盖率来源无法合法访问、需要 PII、最低版本需用户决策、发现仍受支持版本只能使用旧链。

### CL-TEST-01：Linux 宿主真实 relay 与 LiveKit allocation

**Owner**：TEST owner
**依赖**：`CL-00`；`CL-NET-01-A02` 至少已确定目标合同
**目标**：消除 macOS Docker Desktop vpnkit 改写 UDP 源地址造成的本地环境阻塞，在 Linux 宿主
取得真实 selected candidate 和服务端 allocation 双边证据。

测试必须使用隔离端口、唯一容器名、合成账号和 scratch 数据库。先只跑本地/隔离 Linux；
生产测试必须另获授权。

建议入口：

```bash
cd /path/to/imboy
RTC_E2E_KEEP=0 \
RTC_E2E_HOST_NET=1 \
RTC_ADVERTISE_IP="$AUTHORIZED_LINUX_IP" \
bash scripts/rtc_e2e_test.sh
```

Acceptance：

| ID | Required | 通过条件 | 证据 |
|---|---|---|---|
| `CL-TEST-01-A01` | Yes | Linux 内核、网络、端口、镜像 digest 和资源 owner 已冻结 | `agents/TEST/linux-baseline.json` |
| `CL-TEST-01-A02` | Yes | 客户端 selected candidate 明确为 `relay`，非 host/srflx | `agents/TEST/selected-candidate.json` |
| `CL-TEST-01-A03` | Yes | 同一会话在 LiveKit 服务端可见 TURN allocation/participant/track | `agents/TEST/livekit-allocation.json` |
| `CL-TEST-01-A04` | Yes | 双向音频/视频 publish-subscribe 有真实包计数和持续时间 | `agents/TEST/media-flow.json` |
| `CL-TEST-01-A05` | Yes | 过期/篡改 token、端口阻断、证书错误均 fail-closed | `agents/TEST/negative-matrix.md` |
| `CL-TEST-01-A06` | Yes | 退出后容器、端口、scratch DB、合成账号残留为零 | `agents/TEST/cleanup.txt` |

回滚：停止仅属于本 run 的容器/进程并删除本 run 的 scratch 数据；不得清理 foreign/unknown 资源。

停止条件：不能证明 Linux 资源归属、端口冲突、测试目标指向生产但无授权、任何 required degradation
被脚本当成 exit 0、selected candidate 或服务端 allocation 任一缺失。

### CL-DEVICE-01：双真机完整通话矩阵

**Owner**：DEVICE owner
**依赖**：`CL-TEST-01=PASS`；两台授权真机、合成账号和设备租约有效
**目标**：两台物理设备完成 C2C 和群通话；模拟器结果不计入 Required Acceptance。

最小矩阵：

| 维度 | Required 场景 |
|---|---|
| 网络 | Wi-Fi↔Wi-Fi、Wi-Fi↔蜂窝、蜂窝↔蜂窝；至少一腿强制 relay |
| 媒体 | 语音、视频、静音、扬声器、前后摄像头切换 |
| 会话 | 主叫/被叫互换、拒绝、取消、忙线、超时、挂断 |
| 生命周期 | 前台→后台→前台、悬浮窗、锁屏/解锁（平台允许范围） |
| 故障恢复 | 短时断网、网络切换、重连、晚到信令、重复 accept |
| 房间 | C2C 对称同房、群通话加入/离开、远端轨道真实订阅 |

Acceptance：

| ID | Required | 通过条件 | 证据 |
|---|---|---|---|
| `CL-DEVICE-01-A01` | Yes | 两台设备、构建 SHA、App 包、合成账号与租约绑定 | `agents/DEVICE/device-baseline.json` |
| `CL-DEVICE-01-A02` | Yes | C2C 语音/视频双向矩阵全部 PASS | `agents/DEVICE/c2c-matrix.tsv` |
| `CL-DEVICE-01-A03` | Yes | 群通话加入/发布/订阅/离开无退化 | `agents/DEVICE/group-matrix.tsv` |
| `CL-DEVICE-01-A04` | Yes | Wi-Fi/蜂窝/强制 relay 均有 candidate 和媒体证据 | `agents/DEVICE/network-matrix.tsv` |
| `CL-DEVICE-01-A05` | Yes | 前后台、断网重连和负例矩阵全部 PASS | `agents/DEVICE/recovery-matrix.tsv` |
| `CL-DEVICE-01-A06` | Yes | 远端轨道真实可听/可见且与服务端 participant/track 对齐 | `agents/DEVICE/media-evidence.md` |
| `CL-DEVICE-01-A07` | Yes | 测试未触碰真实用户/生产数据，合成资源已清理 | `agents/DEVICE/cleanup.md` |

停止条件：任一设备不是物理真机、设备未授权、账号不确定是否为合成数据、构建 SHA 不一致、
远端轨道未订阅、仅凭 UI 截图或“连接成功”文本判定媒体成功。

### CL-REMOVE-01：旧 eturnal/coturn 最终处置

**Owner**：REMOVE owner；生产写入仅 A0
**依赖**：`CL-SEC-01`、`CL-NET-01`、`CL-LEGACY-01`、`CL-TEST-01`、`CL-DEVICE-01`
全部 Required Acceptance 为 PASS
**额外硬门**：即使前置全绿，也必须再次获得用户对明确主机、服务、软件包、配置、端口和不可恢复项的
**删除授权**。本计划、前序部署授权和本次本地 worktree/分支删除授权均不能替代该授权。

执行顺序：

1. 只读快照 eturnal/coturn unit、进程、package、容器、端口、配置、证书 hook 和恢复来源；
2. 先停止旧服务但不卸载，验证新路径和完整双真机矩阵；
3. 观察窗口内保持可快速恢复；
4. 再请求最终卸载确认；获确认后才删除已列明的旧组件；
5. 重跑端口 owner、源码/配置/API/文档残留审计和全链验收；
6. 任一新路径失败立即恢复旧服务，状态回到 `PARTIAL`。

Acceptance：

| ID | Required | 通过条件 | 证据 |
|---|---|---|---|
| `CL-REMOVE-01-A01` | Yes | 前置硬门全 PASS，删除清单和恢复源完整 | `agents/REMOVE/preflight.md` |
| `CL-REMOVE-01-A02` | Yes | 用户对精确删除对象再次授权 | `agents/REMOVE/delete-authorization.md` |
| `CL-REMOVE-01-A03` | Yes | 停旧服务后新 TURN/SFU 和双真机矩阵仍全绿 | `agents/REMOVE/stop-observation.md` |
| `CL-REMOVE-01-A04` | Yes | 授权对象已处置，3478/5349/443/relay 端口 owner 符合所选合同 | `agents/REMOVE/removal-result.md` |
| `CL-REMOVE-01-A05` | Yes | unit/package/container/cron/hook/config 无未解释旧 TURN 残留 | `agents/REMOVE/residual-scan.txt` |
| `CL-REMOVE-01-A06` | Yes | 删除后完整 C2C/群/relay/重连/证书续期复验通过 | `agents/REMOVE/post-removal-test.md` |
| `CL-REMOVE-01-A07` | Yes | 回滚来源、不可恢复项和观察窗口结果归档 | `agents/REMOVE/rollback-record.md` |

回滚：按快照恢复 package/config/unit/hook，启动旧服务并恢复原端口合同；复验旧客户端和新客户端。

停止条件：任一前置非 PASS、删除授权不精确、恢复源不可用、端口 owner 不明、历史客户端仍依赖旧链、
新路径媒体/relay/证书任一失败。

### CL-EVIDENCE-01：证据一致性与机械生成

**Owner**：A0
**依赖**：上述任务均有终态
**目标**：消除旧 run 中 TSV、matrix、RESULT 的 SHA 和状态不一致。

Acceptance：

| ID | Required | 通过条件 | 证据 |
|---|---|---|---|
| `CL-EVIDENCE-01-A01` | Yes | 每个计划 Acceptance 在 TSV 恰好一行，无未知 ID | `control/acceptance.tsv` |
| `CL-EVIDENCE-01-A02` | Yes | 每个 PASS 的证据存在、非空且 SHA-256 匹配 | `FINAL/evidence-check.txt` |
| `CL-EVIDENCE-01-A03` | Yes | matrix、RESULT、REPORT 由同一 TSV 机械生成 | `FINAL/generation.log` |
| `CL-EVIDENCE-01-A04` | Yes | 三份产物的状态计数、HEAD、计划 SHA、镜像 digest 完全一致 | `FINAL/consistency-check.txt` |
| `CL-EVIDENCE-01-A05` | Yes | secret/PII 扫描通过，未把旧报告陈述成当前事实 | `FINAL/content-safety.txt` |

停止条件：生成产物被手工编辑、证据缺失、SHA 不匹配、同一 ID 多行、状态词超出枚举。

### CL-FINAL：最终复核与诚实终态

**Owner**：A0
**依赖**：`CL-EVIDENCE-01=PASS`
**目标**：按当前 HEAD 和真实外部 oracle 作最终判断，不把本地候选绿灯冒充生产/发布完成。

Acceptance：

| ID | Required | 通过条件 | 证据 |
|---|---|---|---|
| `CL-FINAL-A01` | Yes | 两仓 HEAD、计划 SHA、镜像/证书/设备证据均绑定 | `FINAL/RESULT.json` |
| `CL-FINAL-A02` | Yes | 后端聚焦测试、App 聚焦测试、Linux relay、双真机、生产复验全绿 | `FINAL/acceptance-matrix.md` |
| `CL-FINAL-A03` | Yes | 全量 EUnit 红项已修复或经独立基线对照证明与本变更无新增且有明确 owner | `FINAL/backend-baseline.md` |
| `CL-FINAL-A04` | Yes | 三轮独立复核无未处置 P0/P1，P2 有 owner/期限 | `FINAL/reviews.md` |
| `CL-FINAL-A05` | Yes | 本 run 临时容器、端口、数据库、账号、设备租约残留为零 | `FINAL/cleanup.txt` |
| `CL-FINAL-A06` | Yes | 所有 Required Acceptance 均为 PASS，状态计数机械一致 | `FINAL/consistency-check.txt` |

最终判定规则：

```text
if 所有 Required Acceptance == PASS:
    CLOSURE=PASS
else:
    CLOSURE=BLOCKED 或 FAIL（按真实原因）

RELEASE 始终保持 NO_GO，直到用户另行授权并完成独立发布门禁。
```

以下均不得单独推出 `CLOSURE=PASS`：代码已在 `main`、HTTP 200、WSS join、单机 publish、
本地脚本 PASS、截图、mock E2E、旧报告 PASS、服务端口监听、用户接受降级但未验证所选合同。

## 6. 必跑本地回归

从执行时 `main` 新建干净 worktree，串行运行；不得在同一 worktree 并行编译和 EUnit：

```bash
cd /path/to/imboy
gmake compile
gmake eunit t=rtc_room_logic_tests
bash scripts/test/lk_preflight_rtc_turn_test.sh
bash scripts/test/lk_turn_hook_install_test.sh
bash scripts/check_module_boundaries.sh
bash -n scripts/rtc_e2e_test.sh
bash -n deploy/preflight.sh
bash -n deploy/install.sh
git diff --check
```

```bash
cd /path/to/imboyapp
dart format --output=none --set-exit-if-changed \
  lib/page/chat/p2p_call_screen \
  test/unit_test/page/chat \
  integration_test/rtc
flutter analyze --no-pub
flutter test \
  test/unit_test/page/chat/p2p_call_livekit_test.dart \
  test/unit_test/page/chat/p2p_call_state_machine_test.dart \
  test/unit_test/page/chat/p2p_call_floating_test.dart
git diff --check
```

全量后端基线必须单独运行并保存完整退出码和汇总：

```bash
cd /path/to/imboy
gmake eunit 2>&1 | tee "$RUN_ROOT/agents/A0/full-eunit.log"
test "${PIPESTATUS[0]}" -eq 0
```

若仍出现配置缺失或数据库拒绝连接，状态是 `BLOCKED_LOCAL_ENV`，不是 PASS。若测试失败数相对受控
基线增加，立即 `FAIL_REGRESSION`。

## 7. 总停止条件

出现以下任一项，A0 必须停止当前波次、保持旧服务和 `RELEASE=NO_GO`，不得自行扩大范围：

- 用户尚未决定 `TURN_443_MODE` 或尚未批准最低支持版本；
- Cookie/epmd、网络、证书、部署、旧 TURN 删除缺少目标级授权；
- 需要联系方式、第三方通知、真实用户/PII 或外部账号；
- 双真机、Linux 宿主、网络条件或生产窗口不可用；
- selected candidate、服务端 allocation、远端轨道或真实媒体任一缺证；
- 证据之间状态、SHA、计数不一致；
- 回滚不可执行，或发现可能影响未知/foreign 资源；
- 任何 Required Acceptance 为 `PENDING/BLOCKED/WAIVED_BY_OWNER/FAIL`。

## 8. 完成定义

本计划只在以下条件同时满足时完成：

1. `CL-00` 至 `CL-FINAL` 的 Required Acceptance 全部 PASS；
2. 已泄漏 Cookie 完成轮换，公网 epmd 风险闭合；
3. 用户选择的 TURN 443 合同已经实施并由真实外网验证；
4. 历史客户端最低版本和覆盖率决策有证据；
5. Linux relay、LiveKit allocation、双真机 C2C/群通话矩阵全绿；
6. 旧 TURN 只在二次明确授权后完成处置，且删除后全链复验通过；
7. TSV、matrix、RESULT、REPORT 机械一致，临时资源残留为零；
8. 最终报告仍把本地完成、生产完成和发布授权分开陈述。

在此之前，合法且诚实的终态是 `BLOCKED_*` 或 `PARTIAL`，不是 `PASS`；发布始终为 `NO_GO`。
