# imboy.pub 任务优先级批次索引（2026-07-06）

> 来源：用户从 `imboy/docs/analysis/project-audit-roadmap-2026-07.md` + `imboy/ROADMAP.md` 勾选确认
> 决策：**18 项全部纳入"现在做"池**（用户全勾）。本文按依赖关系排出**执行批次**（先后，非排后）。
> 主任务：**FEAT-03 E2EE 死开关**（plan: `imboyapp/.claude/PRPs/plans/feat-03-e2ee-dead-toggle.plan.md`）

---

## 批次总览

| 批次 | 主题 | 项数 | 阻塞 | 主仓库 |
|---|---|---|---|---|
| 0 | 止血（glm 独立可执行） | 5 | 无 | imboyapp + imboy |
| 1 | CRITICAL 安全/契约（先行） | 2 | SEC-01 阶段2 需拍板 | imboy + imboyadmin |
| 2 | 性能/架构（可并行） | 3 | 无 | imboy + imboyapp |
| 3 | 低优/需拍板 | 1（合并包） | 产品/Fable 拍板 | imboy + imboyapp |
| 4 | GA 发布收尾 | 5 | 仅 CONTRACT-01 ← 批次1 SEC-01；Sentry/配套/上架无硬依赖 | 跨三仓 |

---

## 批次 0 — 止血（立即可做，[MODEL] glm 可独立执行）

与主任务 FEAT-03 无依赖，可并行启动。

| ID | 任务 | 仓库 | 优先级 | 备注 |
|---|---|---|---|---|
| **FEAT-03** | E2EE 死开关与 UI 脱节 | imboyapp(+imboy 文档) | HIGH | **plan ready，待实施**（`imboyapp/.claude/PRPs/plans/feat-03-e2ee-dead-toggle.plan.md`） |
| ~~SEC-03~~ | red_packet_detail 越权读 | imboy | MEDIUM | **✅ 已完成**（commit `0d80be05`），核实见 project-audit-roadmap-2026-07.md |
| SEC-05 | 免鉴权路由收紧（/user/show、/conversation/online） | imboy | MEDIUM | 需 Fable 确认字段收窄范围 |
| ~~ARCH-01~~ | messaging_logic 越界操作 cowboy_req | imboy | MEDIUM | **✅ 已完成**（commit `ecbbce8d`），核实见 project-audit-roadmap-2026-07.md |
| ~~CONTRACT-02~~ | admin payload 二次 JSON.parse 绕过 TSID | imboyadmin + imboy | MEDIUM | **✅ 已完成**（commit `98db3f7`，imboyadmin 仓），核实见 project-audit-roadmap-2026-07.md |

---

## 批次 1 — CRITICAL 安全/契约（先行）

| ID | 任务 | 仓库 | 优先级 | 备注 |
|---|---|---|---|---|
| **SEC-01** | billing 全端点零鉴权 | imboy(+imboyadmin) | CRITICAL | 阶段1 管理端动作迁移 glm 可做；阶段2 租户归属需产品拍板（方案 `imboy/docs/analysis/p0-billing-multitenant-authz-2026-07.md`） |
| ~~FEAT-01~~ | admin 三组页面契约断裂（moderation/user-device/sso 404） | imboy + imboyadmin | CRITICAL | **✅ 已完成**（三子域分别 commit `8090e8ae`/`c557e4f1`/`eb3d75d4`），核实见 project-audit-roadmap-2026-07.md。注：sso 仅管理端配置契约，真实 OIDC/SAML 联邦登录仍是 BIZ-03（BLOCKED） |

---

## 批次 2 — 性能/架构（可并行，长期）

| ID | 任务 | 仓库 | 优先级 | 备注 |
|---|---|---|---|---|
| PERF-01 | 群消息扇出 >10000 静默截断 | imboy | MEDIUM | 后端限流+告警 |
| PERF-02 | mention 等无界列表查询 | imboy | MEDIUM | 后端加 LIMIT/分页 |
| ARCH-02 | 11+11 文件超 800 行拆分 | imboy + imboyapp | MEDIUM | 纯重构、风险低收益慢，长期消化。数据源：`project-audit-roadmap-2026-07` ARCH-02（11 后端 + 11 Flutter）；`100-task-backlog` 旧快照（7 / 10+）已过期，勿引用 |

---

## 批次 3 — 低优/需拍板

| ID | 任务 | 仓库 | 优先级 | 备注 |
|---|---|---|---|---|
| 合并包 | FEAT-02 export_data 501 (GDPR) + FEAT-04 占位清理 + SEC-04 compliance key | imboy + imboyapp | LOW/MED | FEAT-02 需 Fable 出隐私范围方案；SEC-04 需产品/法务拍板合规方向（披露部分已在 FEAT-03 plan Task 5 落地） |

---

## 批次 4 — GA 发布收尾（1.0.0 GA）

依赖批次 1 完成（CONTRACT-01 依赖 SEC-01 billing 域补全）。

| ID | 任务 | 仓库 | 依赖 | 备注 |
|---|---|---|---|---|
| CONTRACT-01 | OpenAPI 漏 finance/billing/license 全域 | imboy | 批次1 SEC-01 billing 域补全 | HIGH，GA 必做 |
| API schema 冻结 | openapi.yaml + asyncapi.yaml 标 stable | imboy | CONTRACT-01 | schema 标 stable |
| Sentry DSN 文档化 | 前后端 source map + SENTRY_DSN 指南 | 跨三仓 | 无（可并行） | GA 前做 |
| 发布配套包 | seed_demo / runbook / DCO CI / README.en | imboy + imboyapp | 无（可并行） | 与上述并行 |
| iOS/Play 上架 | App Store + Play Internal Test | imboyapp | 上述全部完成 | 发布动作，非代码 |

---

## 建议执行序

1. **本周**：FEAT-03（主任务，已出 plan）→ 并行启动 SEC-03 / SEC-05 / ARCH-01（后端独立，glm 可做）
2. **下周**：SEC-01 阶段1（billing 管理端鉴权，glm 可做）——FEAT-01 已完成，从本序列移除
3. **滚动**：ARCH-02 大文件拆分（每次 PR 拆 1-2 个，长期）+ PERF-01/02
4. **GA 冲刺**：CONTRACT-01 → API schema 冻结 → Sentry DSN → 发布配套 → 上架
5. **拍板后**：SEC-01 阶段2 / SEC-04 / FEAT-02（等产品/Fable 输入）

> ROADMAP 中期/长期项（消息翻译/搜索/语音转文字、Helm、Loki、PG 只读副本、Windows/Linux 客户端、联邦协议、OTel、AI 助理、多租户 SaaS）**默认排后**，不在 18 项"现在做"池内。
