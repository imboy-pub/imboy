# imboy 执行任务清单 / Execution Checklist

> 版本 2026-07-02 | 汇总本次会话四份分析文档为单一可执行清单。
> 注意：loop 任务状态唯一真源是 `docs/roadmap/tasks.md`（2026-07-22 修订，更新于本清单之后）；冲突以 tasks.md 为准。
> **本清单只做调度**（ID/依赖/分工/验收一句话/阻塞状态），详细方案见各源文档：
> - 全项目巡检：[project-audit-roadmap-2026-07.md](./project-audit-roadmap-2026-07.md)
> - P0-A WebRTC：[p0-webrtc-relay-rootcause-2026-07.md](./p0-webrtc-relay-rootcause-2026-07.md)
> - P0-B billing：[p0-billing-multitenant-authz-2026-07.md](./p0-billing-multitenant-authz-2026-07.md)
> - P1 E2EE 恢复：[p1-e2ee-device-recovery-protocol-2026-07.md](./p1-e2ee-device-recovery-protocol-2026-07.md)

## 分工图例
- **[glm]** glm-5.2 可独立盲执行（方案已达可执行精度）
- **[Fable→glm]** 需 Fable 先出详细签名/表结构，再 glm 执行
- **[真机]** 阻塞于真机验证（禁模拟器）
- **[拍板]** 阻塞于人工决策，不排日程
- **[外部]** 阻塞于商户账号/第三方凭证

## ⚠️ glm 通用执行陷阱（每个后端任务都适用）
erlfmt pre-commit 会格式化；`make format` 有全量副作用（污染 160+ 文件需 restore）；PostToolUse auto-stage → commit 前 `git restore --staged .` 清空再精确 `git add`；DCO 必 `-s`；sandbox 模式 commit 不落地（commit 用默认环境）；迁移序号 8 位连续、strict 已开必须递增（当前最新 `00000018`，下一个 `00000019`）；`stash@{0}: pre-dedup-stash` 是他会话工作勿动。

---

## Wave 0 — 已完成（勿重复执行）

- [x] **SEC-02** 群任务/投票/日程/相册读侧 IDOR — 已由并发会话提交 `5cb86897`（同批 `2262c91f`/`dc3af70a`/`8900cf0e`/`aab32d57`/`67ba0103`）

---

## Wave 1 — 立即可做（无阻塞，纯代码，[glm] 独立）

按性价比排序，互相无依赖，可任意并行。

- [ ] **WRTC-00** WebRTC 信令断链修复（真根因）— 改 `imboyapp/lib/service/message_webrtc.dart:128` `fireData` → `AppEventBus.fire(WebRTCSignalingEvent(data: data))` + 补信令收链测试 · 验收：flutter analyze 绿（端到端需 [真机]）· ~1h
- [ ] **SEC-03** red_packet_detail 越权读 — `wallet_handler.erl:240` 去 `_CurrentUid` 下划线 + `red_packet_logic:detail/1`→`/2` 加 ViewerUid 归属校验 · 验收：eunit 断言非相关 uid 返回 no_permission · ~1h
- [ ] **BILL-01** billing 管理端路由迁移 — `imboy_router.erl` 删 `/v1/billing/plan`+`/plan/update`（adm 侧已有 finance:write 门）· 验收：非 admin 打 /v1/billing/plan → 404 · ~1h
- [ ] **FEAT-01(user-device)** admin 设备管理补路由 — `imboy_router.erl` /adm 段加 `/adm/user/devices|device/kick|kick-all`（后端逻辑已存在只挂路由）· 验收：admin 页面拉到设备列表 · ~2h
- [ ] **FEAT-03(b)** E2EE 死开关短期隐藏 — 设置页条件隐藏 E2EE toggle + 注释说明由后端 policy 控制 · 验收：flutter analyze 绿（policy 加密需 [真机]）· ~1h
- [ ] **CONTRACT-01** OpenAPI 补 finance/billing/license 全域 — 对照 `adm_finance_handler`/`adm_stats_handler` 补 `api/openapi.yaml`（servers 用 127.0.0.1，lint 忽略走 .redocly.lint-ignore.yaml）· 验收：redocly lint 零新增警告 · ~3h
- [ ] **CONTRACT-02** admin payload 二次 parse — 先 verify `messageRenderingHelpers.tsx:8` payload 是否含 64-bit id，含则 `JSON.parse`→`safeParseBigIntJson` · 验收：tsc 绿 + 大 id 不失真 · ~1h
- [ ] **CONTRACT-04(admin)** safeParseBigIntJson 阈值 — `src/lib/safeParseBigIntJson.ts:20` 正则 `\d{16,}`→`\d{17,}`（TSID 实际 18-19 位）· 验收：tsc 绿 · ~0.5h
- [ ] **PERF-02** mention 无界查询加分页 — `mention_repo.erl` find_by_uid/find_by_group_and_uid 加 page/size + LIMIT · 验收：eunit 分页断言 · ~2h
- [ ] **OPS-01** 备份自动调度 + Pushgateway 上报 — compose 加 cron sidecar 跑 backup_pg.sh + 脚本末尾 curl 推 `imboy_backup_last_success_timestamp` · 验收：告警 IMBoyBackupNotRunning 变绿 · ~2h
- [ ] **OPS-03** sys.config 重复 kernel 键 + 弱口令 — 删 `:417` 重复 kernel 段（保 `:395` logger_level=info）+ `:174,190` 弱口令改占位 · 验收：`grep -c "{kernel,"`→1 + make run 正常 · ~1h

---

## Wave 2 — 需 Fable 先出方案（[Fable→glm]）

- [ ] **ARCH-01** messaging_logic 越界 cowboy_req 重构 — 8 个函数的 cowboy_req 解析上移 msg_handler（Fable 出逐函数新签名映射）· 验收：`grep cowboy_req src/logic/messaging_logic.erl`→0 + eunit 绿
- [ ] **FEAT-01(moderation)** 敏感词+审核队列后端 — 新建 sensitive_word 表(迁移 00000019)+adm_moderation_handler+logic+ds+repo（Fable 出表结构+接口）· 验收：敏感词 CRUD 往返
- [ ] **OPS-02(payment 指标)** 支付失败率告警 — metrics_handler 加 `imboy_payment_*_total{status}` 计数器（Fable 定 label 规范）+ rules 告警 · 验收：/metrics 可见 payment 指标
- [ ] **OPS-02(cert 告警)** TLS 证书到期告警 — compose 加 blackbox_exporter + alerts 加 `probe_ssl_earliest_cert_expiry` 阈值（这条 [glm] 可独立）· 验收：blackbox 有 cert 指标

---

## Wave 3 — 待拍板解锁（[拍板]，见文末决策清单）

- [ ] **BILL-02~05** billing 租户端 IDOR 完整修复 — 依赖 **BLK-BILL 归属模型**拍板。骨架见 billing 文档；拍板后 Fable 定签名 → glm 落地 owner_uid 列+迁移+9 端点 assert_owner+10 个 authz eunit
- [ ] **E2EE-BK-01~03 + CLI-01~04** E2EE 换设备恢复第四链路 — 依赖 **BLK-E2EE 协议方向**拍板。含后端 3 端点+客户端 4 集成点+`e2ee_service.dart:344` did→kid 匹配修复（否则恢复也解不开）
- [ ] **SEC-05** 免鉴权路由收紧 — 依赖 Fable 确认 `/user/show`/`/conversation/online` 字段收窄范围（产品可见性判断）→ glm 移出 open/0 · 验收：无 token 打 → 401
- [ ] **CONTRACT-03** admin 权限 fail-open→fail-closed — 依赖 **拍板**（安全 vs 可用权衡，H11 是有意设计）

---

## Wave 4 — 真机阻塞（[真机]，禁模拟器，需连续真机窗口）

- [ ] **WRTC-00 验证** — flutter clean 重编后双真机跨网通话，日志证伪法定案（10 分钟）
- [ ] **WRTC-02b** 响铃期候选丢失 — 候选缓冲提前到 handleWebRTC 层/响铃期建占位 session（修 WRTC-00 后必然暴露）
- [ ] **WRTC-01 诊断** — curl /v1/user/credential 看 turn_urls 非空 + trickle-ice 测 relay 候选（若 WRTC-00 修复后 relay 仍 0 才需）；env override 加固部分 [glm] 可先做
- [ ] **WRTC-03/04** 单次投递无重试 / glare — 次要，真机确诊后再定是否改
- [ ] **FEAT-03(a)** E2EE 密钥漂移根因修复 — 需真机调试重装后重新协商/拉设备公钥
- [ ] **r_upgrade / amap 禁改区越界还原** — `plugin/r_upgrade`(UpgradeManager.java)+`plugin/amap_flutter_location_plus` 越界改动须还原，Android 13 receiver 合规改走上游 fork 或 gradle 层；还原后真机打包确认高德重复类不复发

---

## Wave 5 — 外部阻塞（[外部]，不排日程）

- [ ] **BIZ-02** 真实支付网关对接 — 阻塞 Stripe/支付宝/微信商户账号+凭证
- [ ] **BIZ-03 / FEAT-01(sso)** 白标 SSO — 阻塞 SSO 协议选型拍板（OIDC/SAML？自建 vs 对接企业 IdP）
- [ ] **SEC-04** compliance key E2EE 语义 — 阻塞合规定位拍板（等保要求 or 死路径？），属披露非代码 bug
- [ ] **OPS-04** CI full-eunit/dialyze ratchet — 需先跑 CI 收基线 + 人工定阈值
- [ ] **ARCH-02** 超 800 行文件拆分（后端 11+Flutter 11）— 技术债，需 Fable 出拆分边界，非阻塞可插空
- [ ] **Flutter 债务** 194 处 Colors 硬编码 token 化 + 测试套件 8 编译错修复 — 真机批次

---

## 决策清单（阻塞 Wave 3，需人工拍板）

| ID | 决策 | 推荐 | 阻塞任务 |
|---|---|---|---|
| **BLK-BILL** | tenant↔uid 归属模型 | 单租户简化 owner_uid=current_uid（YAGNI） | BILL-02~05 |
| **BLK-E2EE** | 换设备恢复协议方向 | Matrix-4S 风格 Recovery Key（零操作+强安全） | E2EE-BK 全套 |
| **BLK-SEC05** | /user/show 字段收窄范围 | 仅昵称/头像，去手机号/邮箱 | SEC-05 |
| **BLK-CONTRACT03** | admin 权限 fail-open 是否改 fail-closed | 安全性优先改 fail-closed + 有限重试 | CONTRACT-03 |
| **BLK-SEC04** | compliance key 合规定位 | 需你确认是否等保要求 | SEC-04 披露 |
| **BLK-SSO** | 白标 SSO 协议 | 需你确认 OIDC/SAML + 自建 vs 对接 | SSO 后端 |

---

## 推荐执行顺序（给调度用）

1. **先清 Wave 1**（全部 [glm] 独立，无风险，一天可扫大半）——优先 WRTC-00（真根因一行改）、SEC-03、BILL-01 三个安全项。
2. **拍板 BLK-BILL + BLK-E2EE** 解锁 Wave 3 两个大件。
3. **Wave 2** Fable 出方案后 glm 跟进。
4. **凑真机窗口**批量做 Wave 4（WRTC 全链验证 + r_upgrade 还原）。
5. Wave 5 随外部条件到位插入。
