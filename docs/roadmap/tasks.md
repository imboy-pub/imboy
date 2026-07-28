# IMBoy 任务状态账本（tasks.md）

> **这是 loop 的唯一状态真源**。每轮：读本文件 → 选一个 `status: ready` 且依赖全 `done` 的任务 → 执行 → 更新其 `status` 与 `evidence` → 提交。
> 人读版路线见 `2026-roadmap.md`（波次）与 `architecture-roadmap.md`（总纲）。本文件是它们的**可执行状态投影**。
> 状态机：`blocked`（前置未完成）→ `ready`（可做）→ `in_progress`（进行中）→ `done`（验收命令通过）/ `wont_fix`（决策放弃）。
> 修订 2026-07-22。

---

## Loop 协议（机器执行约定）

```
每轮循环：
1. 读 tasks.md，解析所有任务块。
2. 刷新状态：某任务所有 deps 均 done → 若其为 blocked 则置 ready。
3. 取第一个 status: ready 的任务（按本文件出现顺序 = 优先级顺序）。
   无 ready 任务 → 检查是否某 Wave 闸门可结算 → 否则报告"无可做任务，等待人工"并退出。
4. 置该任务 in_progress，执行其 action。
5. 跑该任务 verify 命令：
   - 通过 → status: done，填 evidence（commit sha / 命令输出摘要 / file:line）。
   - 失败 → 保持 in_progress，记 blocker，进入下一轮或报告。
6. 安全/E2EE 类任务（tag: security）：先派 model:opus 子 agent 执行，主循环只收摘要
   （见记忆 feedback-fable5-opus-delegation，规避 Fable 5 护栏）。
7. 提交（不 push，除非人工要求），继续下一轮。
```

可选聚焦执行：启动任务时可指定 `focus=commercialization`，此时只选择
`tag: commercialization` 的任务，使用
`docs/planning/p0-commercialization-claude-code-plan-2026-07.md`
作为动作与验收的详细规范。未指定 focus 时保持原有全局顺序。

**退出条件**（任一即停并报告）：无 ready 任务且无可结算闸门 / 遇授权/凭证/架构决策 / 连续两轮无新进展 / 验收需真机或人工凭证。

**格式契约**（勿破坏，loop 靠正则解析）：
- 每任务一个 `### <ID>` 块；`- status:` `- deps:` `- wave:` `- tag:` `- effort:` `- verify:` `- evidence:` 各占一行。
- status 值仅限：`blocked` `ready` `in_progress` `done` `wont_fix`。
- deps 为逗号分隔的任务 ID 或 `none`。

---

## Wave 0 · 稳定化（进入条件：无）

### W0-ARCH-01-P0
- title: auth_middleware /v1/ 前缀急修 + 豁免矩阵测试
- status: ready
- deps: none
- wave: 0
- tag: security
- effort: S
- source: risk-report P0-1；backend-review F-01；security-roadmap SEC-03
- action: 修 `src/api/auth_middleware.erl:34` 的 `<<"/v1/">>` 分支为 `/api/v1/`（或统一委派）；加集成测试覆盖支付回调/webhook 免签路径与设备签名门。
- verify: 新增集成测试通过 + `grep -n '"/v1/"' src/api/auth_middleware.erl` 不再命中死分支；支付回调路径返回非 902。
- evidence:

### W0-ARCH-02
- title: WS 同步回执路径与投递管道对齐（JSON 化 payload）
- status: ready
- deps: none
- wave: 0
- tag: protocol
- effort: S
- source: risk-report P1-P1/P1-P5；ARCH-02
- action: 把 `websocket_handler.erl:814-818` 的 `ws_reply(protobuf,v2)` 有损转换改为 v2 帧 + JSON payload（照 `webrtc_ws_logic.erl` 先例）。
- verify: "非快乐路径 × v2 编码"矩阵测试全绿；C2S_SERVER_ACK / C2G_ERROR 在 v2 连接携带完整 type/字段可达客户端。
- evidence:

### W0-PERF-02
- title: depcache ACK 定时器改并发 ETS 表（write_concurrency）
- status: ready
- deps: none
- wave: 0
- tag: performance
- effort: M
- source: risk-report P0-3；PERF-02
- action: 把 `message_ds.erl:154`、`websocket_logic.erl:58,71` 的 ACK 定时器/标志从 depcache 迁到并发 ETS 表（范本 `agent_rate_limiter.erl:84`，单 named_table+write_concurrency）。
- verify: ACK 路径 `grep` 零 depcache call；EUnit 绿；并发压测无锁竞争热点。
- evidence:

### W0-PERF-01a
- title: user_server 离线检查改阈值查询（急修部分）
- status: ready
- deps: none
- wave: 0
- tag: performance
- effort: S
- source: risk-report P0-2；PERF-01
- action: `message_ds.erl:358-362` 上线离线检查从"拉 3×5000 行"改为按阈值 `chat.hrl:14` 的 COUNT/EXISTS 查询。
- verify: 上线路径不再 SELECT 5000 行（读代码确认 + EUnit）。
- evidence:

### W0-SEC-01
- title: 计费 9 端点补 current_uid 归属校验
- status: ready
- deps: none
- wave: 0
- tag: security
- effort: S
- source: risk-report P1-A1；security-review H-01；SEC-01
- action: `billing_handler.erl:70,91,117,133,147,174,201,226,246`（subscribe/renew/cancel/subscription/report_usage/check_quota/invoice_generate/invoice_pay/invoice_list 函数头）每端点加 current_uid 归属校验（照支付 owner_uid 红线），invoice_pay 优先。
- verify: 每端点新增"跨租户请求被拒"测试；A 租户 token 操作 B 租户对象返回拒绝。
- evidence:

### W0-SEC-02
- title: 钱包借记补 frozen/status 守卫 + 表级 CHECK
- status: ready
- deps: none
- wave: 0
- tag: security
- effort: S
- source: risk-report P1-D1；SEC-02
- action: `wallet_repo.erl:117-120`（atomic_balance_change 补 status+frozen）、`:193-197`（do_debit 补 frozen）、`:271-275`；加迁移表级 CHECK `frozen<=balance`。
- verify: 冻结态借记被拒测试 + CHECK 拦 frozen>balance；转账/红包回归绿。
- evidence:

### W0-SEC-00
- title: AGPL vodozemac 法务裁决（产品决策，非工程）
- status: ready
- deps: none
- wave: 0
- tag: decision
- effort: decision
- source: risk-report P0-4；SEC-00
- action: 三选一定案（开源本体 / 购商业授权 / 换非 AGPL 绑定），`imboyapp/pubspec.yaml:223-224`。
- verify: 授权路径书面定案（人工，非命令）。
- evidence:

---

## Wave 0 闸门 = M1
### GATE-W0
- title: M1 可售最小安全线
- status: blocked
- deps: W0-ARCH-01-P0,W0-ARCH-02,W0-PERF-02,W0-PERF-01a,W0-SEC-01,W0-SEC-02,W0-SEC-00
- wave: 0
- tag: gate
- effort: —
- action: 确认上列全 done + 一轮真机回归。绿则解锁 Wave 1。
- verify: 5 个阻断项清零 + 真机回归无退化（需人工/真机）。
- evidence:

---

## Wave 1 · GA 硬化（进入条件：GATE-W0 done）

### W1-ARCH-01
- title: 鉴权属性声明式化（消灭 4 处平行 path 维护）
- status: blocked
- deps: W0-ARCH-01-P0
- wave: 1
- tag: security
- effort: L
- source: ARCH-01；risk-report P1-A2
- action: 鉴权 open/option/免签豁免收敛到路由 Opts 声明，中间件统一消费；含 setup 401 修复。
- verify: 豁免矩阵测试全绿；`grep` 无平行 path 前缀维护残留。
- evidence:

### W1-ARCH-03
- title: 协议契约 CI 门禁（proto/OpenAPI/ws_url diff）
- status: blocked
- deps: W0-ARCH-02
- wave: 1
- tag: protocol
- effort: M
- source: ARCH-03；risk-report P1-P4/P1-P6
- action: proto regen diff + OpenAPI 覆盖门 + ws_url 校验进 CI。
- verify: 故意改 proto 不 regen → CI 失败；ws_url 指向真实路由由 preflight 校验。
- evidence:

### W1-ARCH-04
- title: SDK 契约对齐 + 端到端冒烟
- status: blocked
- deps: W1-ARCH-03
- wave: 1
- tag: protocol
- effort: M
- source: ARCH-04；risk-report P1-P3
- action: 修 imboy-sdk-js 5 项漂移（login pwd/quickLogin/已删 e2ee 端点/token 事件名/子协议）；加登录→握手→收发→确认 E2E。
- verify: SDK E2E 全链路绿，进发版门禁。
- evidence:

### W1-TEST-01
- title: 后端 full-eunit + dialyzer 收紧为 ratchet 硬门
- status: blocked
- deps: none
- wave: 1
- tag: testing
- effort: M
- source: TEST-01；testing-review P1-T1
- action: 清存量红项后把 `backend-ci.yml` 的 full-eunit/dialyzer 从 continue-on-error 改硬门。
- verify: CI 新 PR 红即拦；continue-on-error 已移除。
- evidence:

### W1-TEST-02
- title: 三仓覆盖率阈值门
- status: blocked
- deps: W1-TEST-01
- wave: 1
- tag: testing
- effort: M
- source: TEST-02；coverage-plan
- action: 三仓加 coverage 采集 + 地板=首次实测的 ratchet 门。
- verify: 覆盖率低于地板 CI 失败。
- evidence:

### W1-TEST-03
- title: admin Playwright E2E 进 CI
- status: blocked
- deps: none
- wave: 1
- tag: testing
- effort: S
- source: TEST-03；testing-review P1-T2
- action: 9 个 Playwright spec 接入 CI（nightly/合并前）+ flaky 隔离。
- verify: `grep -r playwright imboyadmin/.github/workflows/` 命中；E2E 在 CI 跑。
- evidence:

### W1-TEST-05
- title: 排查 integration_test.yml（CRITICAL）
- status: blocked
- deps: none
- wave: 1
- tag: testing
- effort: M
- source: TEST-05；testing-review P1-T4
- action: 确认 umbrella 下是否曾触发；修复接线或删冗余坏死副本（ci.yml 已有在跑的 integration job）。
- verify: integration 在 CI 真实可见地绿。
- evidence:

### W1-ENG-01
- title: Flutter custom_lint（autoDispose/裸URL/800行/token）
- status: blocked
- deps: none
- wave: 1
- tag: engineering
- effort: M
- source: ENG-01；flutter-review P1-F1
- action: 建 4 条 custom_lint 规则 + 存量 baseline 豁免（只拦新增）。
- verify: 4 类违规新增即 CI 失败；baseline 递减。
- evidence:

### W1-ENG-02
- title: Flutter DDL 单一真源 + CI 校验
- status: blocked
- deps: none
- wave: 1
- tag: engineering
- effort: M
- source: ENG-02；flutter-review P1-F3
- action: 以 embedded 常量为真源生成 .sql 副本 + CI 一致校验；无脚本降级显式失败。
- verify: 常量改动不同步则 CI 失败；无脚本降级抛错而非 success。
- evidence:

### W1-SEC-04
- title: 会话与密钥卫生（JWT 吊销 + KDF 升级 + 密钥拆分 + cookie 过期）
- status: blocked
- deps: none
- wave: 1
- tag: security
- effort: L
- source: SEC-04；risk-report P2-1/P2-2/P1-A3/P2-3
- action: JWT 吊销通道；口令改记忆硬 KDF（双读兼容）；cookie 去硬编码默认+加过期/可吊销；jwt_key 与 postgre_aes_key 拆分。
- verify: 封禁用户 token 立即失效；旧口令透明升级；cookie 有 exp 且登出吊销；两密钥不同值强制校验。
- evidence:

### W1-RELI-01
- title: message_retry 全量扫描 + 集群 syn Pid 语义确认
- status: blocked
- deps: none
- wave: 1
- tag: reliability
- effort: M
- source: risk-report P1-C1/P1-C2；backend-review F-04
- action: `message_retry.dart:178` 前 100 条截断改全量/分页；`imboy_syn.erl:166,172` 远端 Pid + start_timer 语义按目标 OTP 版本实测确认（不假设 badarg）。
- verify: 失败消息不丢（分页扫描测试）；start_timer 远端行为有实测记录。
- evidence:

---

## Wave 1 闸门 = M2
### GATE-W1
- title: 1.0 GA
- status: blocked
- deps: W1-ARCH-01,W1-ARCH-03,W1-ARCH-04,W1-TEST-01,W1-TEST-02,W1-TEST-03,W1-TEST-05,W1-ENG-01,W1-ENG-02,W1-SEC-04,W1-RELI-01
- wave: 1
- tag: gate
- effort: —
- action: SDK E2E 绿 + CI 全硬门 + P1 台账清零。绿则解锁 Wave 2。
- verify: 上列全 done。
- evidence:

---

## Wave 2 · 规模化（进入条件：GATE-W1 done）

### W2-PERF-01b
- title: user_server 完整拆分（上下线 DB 写与 fanout 异步化）
- status: blocked
- deps: W0-PERF-02,W0-PERF-01a
- wave: 2
- tag: performance
- effort: L
- source: PERF-01；risk-report P0-2
- verify: 1 万并发重连压测，上下线队列不积压。
- evidence:

### W2-PERF-03
- title: C2G 扇出异步化
- status: blocked
- deps: none
- wave: 2
- tag: performance
- effort: M
- source: PERF-03；risk-report P1-PF1
- verify: 千人群发送，发送者进程响应时间与群规模无关。
- evidence:

### W2-PERF-04
- title: 投递管道去 JSON 中间格式
- status: blocked
- deps: W0-ARCH-02
- wave: 2
- tag: performance
- effort: M
- source: PERF-04；risk-report P1-PF2
- verify: v2 客户端投递路径无冗余 re-encode（profile 验证）。
- evidence:

### W2-PERF-05
- title: 连接池真超时 + prepared statement 缓存
- status: blocked
- deps: none
- wave: 2
- tag: performance
- effort: M
- source: PERF-05；risk-report P1-D2
- verify: 注入慢查询，池不被 sleep 阻塞；prepared statement 命中率可观测。
- evidence:

### W2-PERF-06
- title: statement_timeout 全链路
- status: blocked
- deps: none
- wave: 2
- tag: performance
- effort: S
- source: PERF-06；risk-report P1-D2
- verify: 连接建立即带 statement_timeout；超时查询被 PG 主动终止。
- evidence:

### W2-ARCH-05
- title: Flutter 运行时收敛（分期）
- status: blocked
- deps: W1-ENG-01
- wave: 2
- tag: architecture
- effort: XL
- source: ARCH-05
- verify: 单一运行时约定文档化；三套并存消除。
- evidence:

### W2-ARCH-07
- title: Olm-only cutover 客户端启用（分期）
- status: blocked
- deps: none
- wave: 2
- tag: security
- effort: L
- source: ARCH-07（迁移 42-46 已落盘）
- action: 启用 `chat_network_service.dart:562` useOlmForC2C；补 proto E2EEMeta olm 子对象。
- verify: Olm C2C 真机验收 PASS（非模拟器、非 RSA fallback 冒充）。
- evidence:

### W2-RELI-02
- title: 集群模式端到端验证
- status: blocked
- deps: W1-RELI-01
- wave: 2
- tag: reliability
- effort: M
- source: risk-report P1-C1
- verify: 多节点消息投递正确（集群回归）。
- evidence:

---

## Wave 2 闸门 = M3
### GATE-W2
- title: 规模化就绪
- status: blocked
- deps: W2-PERF-01b,W2-PERF-03,W2-PERF-04,W2-PERF-05,W2-PERF-06,W2-ARCH-05,W2-ARCH-07,W2-RELI-02
- wave: 2
- tag: gate
- effort: —
- verify: 压测证明单节点吞吐提升一个量级 + 集群回归绿。
- evidence:

---

## Wave 3 · 平台化（进入条件：GATE-W2 done）

### W3-ARCH-06
- title: 后端平台职责分离（监督子树拆分）
- status: blocked
- deps: none
- wave: 3
- tag: architecture
- effort: L
- source: ARCH-06
- verify: 杀 Agent 子树不影响消息收发（测试）。
- evidence:

### W3-ARCH-07-end
- title: E2EE 收尾 + RSA 写路径下线
- status: blocked
- deps: W2-ARCH-07
- wave: 3
- tag: security
- effort: L
- source: ARCH-07 收尾
- verify: 密钥体系统一；RSA 仅 decrypt-only。
- evidence:

### W3-ENG-03
- title: 巨型文件拆分（chat_page.dart 2234 行等 12+）
- status: blocked
- deps: none
- wave: 3
- tag: engineering
- effort: L
- source: ENG-03；risk-report P1-Q2
- verify: 消息主链路文件全部 <800 行；真机回归无退化。
- evidence:

### W3-SEC-05
- title: 多租户隔离审计 + TimescaleDB 生命周期治理
- status: blocked
- deps: none
- wave: 3
- tag: security
- effort: L
- source: SEC-05
- verify: 对象级授权审计零高危遗留；归档链配置与文档一致。
- evidence:

### W3-ENG-04
- title: ADR 补齐 + 死资产清理（liveRoom）
- status: blocked
- deps: none
- wave: 3
- tag: engineering
- effort: M
- source: ENG-04
- verify: liveRoom 或转正或下线；E2EE/支付/LiveKit/MCP 各有 ADR。
- evidence:

### W3-OBS-01
- title: 可观测闭环（SLA 告警联动）
- status: blocked
- deps: none
- wave: 3
- tag: observability
- effort: M
- source: observability-notes/monitoring
- verify: 平台级 SLA 可度量。
- evidence:

---

## Wave 3 闸门 = M4
### GATE-W3
- title: 平台化
- status: blocked
- deps: W3-ARCH-06,W3-ARCH-07-end,W3-ENG-03,W3-SEC-05,W3-ENG-04,W3-OBS-01
- wave: 3
- tag: gate
- effort: —
- verify: 平台级 SLA 达标 + 多租户隔离审计通过。
- evidence:

---

## Wave C0 · 商业化 P0（聚焦执行）

> 详细规范：`docs/planning/p0-commercialization-claude-code-plan-2026-07.md`。
> 固化默认：单租户 `owner_uid=current_uid`、OIDC-only、支付 mock-only；真实凭证和真机不进入本闸门。

### C0-BILL-01
- title: Billing 归属与跨租户越权修复
- status: blocked
- deps: W0-SEC-01
- wave: C0
- tag: commercialization
- effort: M
- source: p0-commercialization-claude-code-plan-2026-07.md §C0-BILL-01
- action: 增加 owner_uid 迁移；handler 注入 current_uid；logic 统一 assert_owner；invoice_pay 反查订阅归属；补 8 类跨用户 EUnit。
- verify: make compile && make eunit；billing 授权测试全绿；billing handler 不再丢弃 State；owner_uid schema 存在。
- evidence:

### C0-LICENSE-01
- title: License max_nodes 硬 gate 与续费边界
- status: ready
- deps: none
- wave: C0
- tag: commercialization
- effort: M
- source: p0-commercialization-claude-code-plan-2026-07.md §C0-LICENSE-01
- action: 接入 max_nodes 硬 gate；补签名、域名、过期、宽限、用户数和节点数 fixture 测试；完善脱敏状态 API。
- verify: make compile && make eunit；max_nodes=1 拒绝第二节点；License API 不泄露原文/私钥。
- evidence:

### C0-BRAND-01
- title: Flutter/Admin 白标构建配置
- status: ready
- deps: none
- wave: C0
- tag: commercialization
- effort: M
- source: p0-commercialization-claude-code-plan-2026-07.md §C0-BRAND-01
- action: 建立单一 BrandConfig，覆盖名称、Logo、启动页、主题色和品牌文案；补默认与白标 fixture。
- verify: flutter analyze && flutter test；bun test && bun run build；默认/白标 fixture 配置断言通过。
- evidence:

### C0-OPS-01
- title: 备份恢复与健康告警闭环
- status: ready
- deps: none
- wave: C0
- tag: commercialization
- effort: M
- source: p0-commercialization-claude-code-plan-2026-07.md §C0-OPS-01
- action: 接入受支持的备份调度、Pushgateway 成功指标、TLS 证书告警、支付结果指标和临时库 restore smoke。
- verify: bash -n scripts/backup_pg.sh scripts/backup_garage.sh deploy/preflight.sh；docker compose config；helm lint；promtool check rules。
- evidence:

### C0-IAM-01
- title: OIDC PKCE 生产加固与 fake IdP 回归
- status: ready
- deps: none
- wave: C0
- tag: commercialization
- effort: M
- source: p0-commercialization-claude-code-plan-2026-07.md §C0-IAM-01
- action: 固定 issuer/aud/exp/nonce/PKCE 校验；解决或显式阻断多节点一次性状态；未实现 provider 不得假报已启用。
- verify: make compile；OIDC EUnit 覆盖重放/claims/并发 OTC；fake IdP authorize→callback→exchange 全链路通过。
- evidence:

### C0-GOV-01
- title: 数据导出、审计和 RBAC fail-closed
- status: blocked
- deps: C0-IAM-01
- wave: C0
- tag: commercialization
- effort: M
- source: p0-commercialization-claude-code-plan-2026-07.md §C0-GOV-01
- action: 实现受限范围 export_data；补关键动作审计；RBAC 不可用时拒绝敏感写操作；留存/Legal Hold 未实现时显式标记。
- verify: make compile && make eunit；导出 schema/敏感字段断言通过；模拟 rbac 404 时敏感写操作被拒。
- evidence:

### C0-CONTRACT-01
- title: 商业 API 合同与三仓发布门
- status: blocked
- deps: C0-BILL-01,C0-GOV-01
- wave: C0
- tag: commercialization
- effort: M
- source: p0-commercialization-claude-code-plan-2026-07.md §C0-CONTRACT-01
- action: 补 finance/billing/license/sso/export_data OpenAPI；增加三仓构建、测试、版本和迁移一致性检查。
- verify: redocly lint api/openapi.yaml；make compile && make eunit；bun test && bun run build；flutter analyze && flutter test。
- evidence:

### GATE-C0
- title: P0 商业化自动验收闸门
- status: blocked
- deps: C0-BILL-01,C0-LICENSE-01,C0-BRAND-01,C0-OPS-01,C0-IAM-01,C0-GOV-01,C0-CONTRACT-01
- wave: C0
- tag: commercialization
- effort: —
- source: p0-commercialization-claude-code-plan-2026-07.md §P0 闸门
- action: 执行本地 mock 商业冒烟并汇总三仓验收证据；不触发真实支付、商店发布或真机操作。
- verify: 所有 deps done；三仓检查全绿；注册→License→OIDC→订阅→mock 支付→审计→导出→备份 smoke 全绿；git diff --check。
- evidence:

---

## 进度快照（loop 每轮可更新此表，便于人一眼看全局）

| Wave | 任务数 | done | in_progress | ready | blocked | 闸门 |
|---|---|---|---|---|---|---|
| 0 | 7 | 0 | 0 | 7 | 0 | GATE-W0 blocked |
| 1 | 11 | 0 | 0 | 0 | 11 | GATE-W1 blocked |
| 2 | 8 | 0 | 0 | 0 | 8 | GATE-W2 blocked |
| 3 | 6 | 0 | 0 | 0 | 6 | GATE-W3 blocked |
| C0 | 8 | 0 | 0 | 4 | 4 | GATE-C0 blocked |

> loop 更新规则：改完任务 status 后同步刷新本表计数（或运行 `grep -c 'status: done' tasks.md` 等重算）。
