# API 契约对账 325 条差异处置方案（Remediation Plan）

> 生成日期 / Date: 2026-09-30
> 输入 / Input: [api-contract-reconciliation.md](./api-contract-reconciliation.md)（对账报告，2026-09-30）
> 真源 / Sources: `src/imboy_router.erl`（路由）· `api/openapi-{api,adm,internal}.yaml` + `api/paths/**`（契约编辑真源）· `api/openapi.yaml`（聚合产物，codegen 输入）· `scripts/contract_gate.py`（contract-export）
> 性质 / Nature: **分析与处置方案文档，不含任何已执行的代码/契约变更**。所有修复动作均待人工拍板后另行 PR。

---

## 1. 口径核算（325 之辨）

对账报告 §2 的"合计 325"与各分项直接相加（220+86+13+16=335）不符，需先钉死口径，避免后续 PR 验收时数字对不上：

| 分项 | 条数 | 是否计入 325 | 依据 |
|---|---:|---|---|
| C1a REST 真缺口 | 220 | ✅ | 报告 §3.1 |
| C1b feature 门控（CS 56 + EB 30） | 86 | ✅ | 报告 §3.2 |
| C2 契约陈旧 | 16 | ✅ | 报告 §4 |
| C1c dev-only | 3 | ✅（325−220−86−16=3，唯一自洽解释） | 报告 §3.3 |
| C1c infra 6 + static 4 | 10 | ❌（视为"无需契约"，非差异） | 报告 §3.3 处置建议 |
| **合计** | **325** | | |

**复核验证**（2026-09-30 重跑全量 diff，脚本口径=报告 §6 第 1 条"源码字面"）：
路由 860 / 契约 557 / 双方共有 541 / 路由有契约无 **319**（= REST 220 + CS 56 + EB 30 + dev 3 + infra 6 + static 4，与报告 §2/§3 完全一致）/ 契约有路由无 **16**（与报告 §4 全列一致）。数字复现无偏差。

---

## 2. 归因框架与总账

四类归因定义（按任务裁定）：

| 类 | 定义 | 判定标准 | 条数 |
|---|---|---|---:|
| **A** | openapi 漏写：端点存在且稳定（handler 活、有消费方） | 路由注册 + `src/api/` 有 handler + 前端/App 有调用 | **216** |
| **B** | openapi 陈旧：端点已删或改形，契约未同步 | 路由未注册 + handler 不存在 + git 有下线记录 | **16** |
| **C** | 门控导出口径差异：非文档问题（feature 编译门 / dev 运行时门导致"存在但非恒在"） | 路由位于 `-ifdef` 门控 helper 或 `is_dev_env/0` 门内 | **89**（86 feature + 3 dev） |
| **D** | 端点应废弃待确认：活路由但疑无消费方（别名族） | 路由注册但双端 grep 无调用 | **4** |
| 合计 | | | **325** |

---

## 3. B 类 16 条：契约陈旧（端点已删，契约漏删）— 证据最硬，优先处置

**归因结论：全部 16 条为 B。实现已删、契约漏删，删除动作零外部破坏**（对外页面从未有人能调通这些端点）。

### 3.1 e2ee/social + e2ee/transfer（14 条）——git 实锤

- 下线 commit：**`52d22a4c`（2026-07-14）"refactor(e2ee): 下线自研社交恢复/设备转移后端，密钥托管收敛云备份"**，commit message 明言"删除 handler/logic/ds/repo 各层 e2ee_social_*/e2ee_transfer_*/e2ee_shard_* 共 11 模块 + 10 测试 + e2ee_cleanup_worker；**移除 14 条路由**；迁移 00000038 drop 四张自研表"。
- handler 验证（2026-09-30）：`src/api/` 现存 e2ee 相关仅 `e2ee_handler.erl`/`e2ee_backup_handler.erl`/`e2ee_trust_handler.erl`，全 `src/` grep `e2ee_social|e2ee_transfer|social_shard` 零命中。
- 契约残留位置：`api/openapi-api.yaml:622-633`（transfer 6 条）、`:634-649`（social 8 条），端点文件在 `api/paths/api/v1/e2ee-transfer/`、`api/paths/api/v1/e2ee-social/`（三受众重组 commit `7b18cbe2` 迁入，早于 52d22a4c 的下线，故漏删）。
- 现行替代方案：`/api/v1/e2ee/recovery/start` + `compliance_key`（server_backup 云备份线，对账报告 §4 备注），与迁移 00000038 一致。

### 3.2 `/api/v1/auth/assets`（GET+POST）与 `/api/adm/attach/auth`（POST）——前身替换

- 契约位置：`api/openapi-api.yaml:94`、`api/openapi-adm.yaml:143-144`。
- handler 验证：全 `src/` grep `auth/assets`、`attach/auth` 零命中；现行等价物为 `/api/v1/attachment/*`（`attachment_handler`，router 注册 4 条，其中 3 条已有契约、`/api/v1/attachment/upload` 属 A 类待补）与 `/api/adm/storage/*`（`storage_handler`，8 条注册、7 条有契约）。对账报告 §4 判断"疑似前身"成立。

### 3.3 B 类处置动作与影响面

| 项 | 内容 |
|---|---|
| 处置动作 | 删 `api/paths/api/v1/e2ee-social/`（8 文件）+ `e2ee-transfer/`（6 文件）+ `auth/assets.yaml` + `adm/v1/attach/auth.yaml` → 改两个 root（openapi-api.yaml / openapi-adm.yaml 各删对应 `$ref` 行）→ `redocly lint` → `python3 api/gen_aggregate.py` 重新聚合（流程见 `api/README.md:74`） |
| 外部影响面 | **docs.imboy.com / GitHub Pages 契约页**（渲染渠道见 `api/README.md:57`）减少 16 path；`openapi.yaml` 为 codegen 输入（`api/README.md:95`），下游 codegen 产物同步缩减。因端点从未实现，属纠错不属 breaking，但需在 CHANGELOG 声明"清理未实现端点" |
| 工作量 | **0.5–1 人日**（删文件+改 root+重新聚合+lint+报告数字更新），无需写任何新 schema |

---

## 4. C 类 89 条：门控口径差异（非文档漏写）——需先立标注规范，再分批补

### 4.1 成因拆解

1. **CS 56 / EB 30（编译门）**：路由在 `imboy_router.erl` 底部 `-ifdef(IMBOY_FEATURE_CUSTOMER_SERVICE)`（:2048-2442）/ `-ifdef(IMBOY_FEATURE_ENTERPRISE_BUSINESS)`（:1763-1974）helper 内，未选中的部署物理裁剪。契约侧从未收录，属"feature 面从未入契约"的产品决策空白，**不是漏写单个端点**。
2. **dev-only 3（运行行门）**：`/api/v1/test/req_get`、`/api/v1/test/req_post`、`/api/v1/agent_task/demo` 仅 `is_dev_env/0`（`imboy_router.erl:1579-1580`）为真时注册。
3. **导出工具口径（关联机制，非差异本身）**：`contract_gate.py` 的 `extract_routes/1` 按固定文本窗口提取——EB/CS 门控段显式并入窗口（`scripts/contract_gate.py:162-180`），moment helper 刻意排除（`scripts/contract_gate.py:160-162` 注释明言"不放宽窗口（那会把 moment 等其它 helper 一并摄入、改变既有契约）"；moment helper 本体在 `imboy_router.erl:1623-1663`）。故 `.contract/api_contract.json`（endpoints：main 8 / adm 294 / api_v1 495 / api_internal 26 / test_dev_only 3，合计 826）中 moment 恒 0 条——实证：json 全文 `moment` 仅 1 次出现且在 notes 字符串内。**对账以 router 源码字面为准（860），export 数字不可直接比对**（对账报告 §6 已注明，本方案再次实证）。

### 4.2 关键先例：moment 门控面是可以入契约的

MOMENT 同为编译门 feature（`-ifdef(IMBOY_FEATURE_MOMENT)`，`imboy_router.erl:1623`），其 18 条路由（`/api/v1/moment` 12 + `/api/adm/moment` 6）**契约全覆盖**（对账报告 §2"覆盖最好的域"）。这证明"feature 门控"与"契约收录"不冲突——**建议 CS/EB 依 moment 先例补契约，加 `x-feature` 标注声明部署依赖**，而非在契约中整体排除。当前 openapi 三 root 无任何 `x-feature`/`x-gated` 先例（grep 仅 `api/openapi-api.yaml:84` 一行注释提到"deprecated 旧 alias"概念），标注规范需新立。

### 4.3 C 类处置动作与影响面

| 项 | 内容 |
|---|---|
| 处置动作 | ① 新立 `x-feature: customer_service | enterprise_business` 标注约定（root info.description 说明语义：未启用该 feature 的部署返回 404）；② 86 条分 5 个 PR 补契约并打标（见 §7）；③ dev-only 3 条不补契约，进排除面声明（见 §5）；④ 可选工具项：`contract_gate.py` 增补 moment 窗口使 export 与源码字面口径对齐（改动约 10 行，属工具非契约，单独立项） |
| 外部影响面 | 契约页面新增 86 path（带 feature 标注）。**风险点**：未启用 feature 的私有部署购买者按契约页调用将 404——靠 `x-feature` 标注 + root description 声明缓解；EB 面已有 admin 前端消费（imboyadmin `enterprise-business` 相关 9 文件），CS 面有 `src/modules/customer_service/` 模块，消费方真实存在 |
| 工作量 | 标注规范 0.5 天；86 条端点契约 ≈ **8–12 人日**（会话/坐席/工单类端点 schema 较重）；工具对齐 0.5 天（可选） |

---

## 5. 排除面声明（infra 6 + static 4 + dev 3，非差异、防再误报）

13 条（`/metrics`、`/healthz`、`/livez`、`/readyz`、`/api/v1/mcp`、`/.well-known/agent.json`；`/privacy-policy`、`/account-deletion`、`/static/[...]`、`/static/admin/[...]`；dev 3 条见 §4.1）建议**在 `api/README.md` 与三 root 的 info.description 追加"契约排除面"清单**，一次性了断后续对账的反复误报。工作量 0.5 人日，外部影响面为零（纯文档）。`/api/v1/mcp` 与 `/.well-known/agent.json` 已有 MCP/A2A 协议文档承接（对账报告 §3.3）。

---

## 6. A 类 216 条 + D 类 4 条：REST 真缺口——按消费方分 P0/P1/P2

### 6.1 D 类 4 条（别名族，先确认再动手）

`/api/adm/role/delete`、`/api/adm/role/disable`、`/api/adm/roles/delete`、`/api/adm/roles/disable`：router 双注册并存（role 单数族 11 条中 9 条有契约、roles 复数族 7 条中 5 条有契约），imboyadmin 全量 grep `adm/role/delete|adm/roles/delete|adm/role/disable|adm/roles/disable` **零调用**。**处置：人工确认（单数族与复数族谁是正身、另一族是否历史别名）→ 要么补正身契约+废弃别名族路由（改 router），要么双族都补契约。在确认前不补不删**。确认成本 0.5 天（含翻 git 历史与 curl 面板验证）。

### 6.2 A 类 216 条的消费方证据与分级

消费方核查（2026-09-30 全量 grep imboyadmin `src/` 与 imboyapp `lib/`）：

| 分级 | 域（缺失条数） | 消费方证据 | 理由 |
|---|---|---|---|
| **P0** | e2ee 9（olm/devices 线）、wallet 5（recharge/confirm、red_packet 3、transfer/accept）、passport 2（alipay_login、alipay_authinfo） | imboyapp `lib/service/olm_session_service.dart`、`e2ee_bootstrap.dart`、`lib/page/wallet/red_packet_{send,detail}_page.dart` 等 | **App 端线上功能正在调用但契约页查不到**——对外契约可信度的最大黑洞 |
| **P1** | organizations 28+23、workspaces 17+5、ai_agent 15、enterprise 12、finance 11、channel 11+2、mcp 8、moderation 5、agent 6、bot 6、agent_task 2、user 3、admin 4、appeal 5（v1 3+adm 2）、report_action 3、stats 2、storage 1、feedback 1、operation_logs 1 | imboyadmin：organizations 41 文件、workspaces 29 文件、enterprise-business 9、moderation 7、ai_agent 6、enterprise/organizations 4、appeal 4（`lib/page/mine/appeal/appeal_page.dart`）；report_action `src/modules/ops_governance/api/reports.ts` | **admin 管理面正在消费的管理域 API**，契约页缺失影响买方二次开发与鉴权核对 |
| **P2** | moya 22、agent-card 1、auth/wechat-mini 1、wechat/mini/events 1、oa/sso 1、payment/callback 1、attachment/upload 1 | moya（墨芽习字微信小程序，`imboy_router.erl:81-96、537-548` 注释明确）admin 零消费、独立小程序端交付；payment/callback 属网关回调（erlang_pay 消费，URL 带凭据） | 独立端/回调面，影响面小于双主力端；moya 域建议与小程序端联调补齐 |

> UNVERIFIED 项：`/api/v1/oa/sso/code`、`/api/v1/agent-card` 的直接消费方未在双端 grep 命中（handler 活跃：`imboy_router.erl` 注册在案），归 A、列 P2，补契约前可再核。

### 6.3 A 类处置动作模板与工作量

| 项 | 内容 |
|---|---|
| 处置动作（每端点） | 新建 `api/paths/<surface>/v1/<域>/<端点>.yaml`（遵循 46 个既有域目录组织）→ root `$ref` 一行 → `redocly lint` → `gen_aggregate.py` 重新聚合 → 纳入 `test/rest/contracts.tsv`（8 条既有行结构：method/path/handler/auth/spec/suite/case_ids/openapi_operation，见文件头）获得持续门禁 |
| 外部影响面 | 契约页/codegen 新增 path。**方法集（C3）风险**：路由层无方法真源（cowboy 纯 path 匹配，对账报告 §5），补契约时方法必须逐 handler 抽 `cowboy_req:method/1` 分派分支核实，禁止按路径形状臆测（`check_rest_contract_coverage.sh` 头注释同样规定"Methods are NEVER inferred from the path shape"） |
| 工作量 | 简单端点 0.5h/条，重 schema 端点 1–1.5h/条：P0 16 条 ≈ **2 人日**；P1 170 条 ≈ **15–20 人日**；P2 30 条 ≈ **3 人日** |

---

## 7. 修复路线（P0→P2）与 PR 拆分（每 PR ≤30 文件）

补一个端点 = 1 新 paths 文件 + root 1 行 + TSV 1 行 + 聚合产物 1–2 个（`gen_aggregate.py` 生成后提交）≈ 4–5 文件/端点。删 B 类 = 1 paths 文件删除 + root 删行 + 聚合。

| 序 | PR 主题 | 内容 | 文件数 | 级 |
|---|---|---|---:|---|
| 1 | `docs(contract): 声明契约排除面` | §5：api/README.md + 三 root description + 本方案落档 | ~5 | **P0** |
| 2 | `fix(contract): 删除已下线 e2ee social/transfer + 前身资产的 16 条陈旧契约` | §3：删 16 paths 文件 + 2 root + 聚合 + CHANGELOG | ~21 | **P0** |
| 3 | `feat(contract): 补 App 端在用核心端点（e2ee olm/wallet/passport-alipay）16 条` | §6.2 P0：16 paths + 1 root(api) + TSV + 聚合 | ~20 | **P0** |
| 4 | `feat(contract): 补 organizations v1 面 28 条` | 拆 departments/members 族与 invitations/invite_code 族两段提交 | ~33→拆 2 提交或 25+25 双 PR | **P1** |
| 5 | `feat(contract): 补 adm organizations 23 + workspace 5` | | ~29 | P1 |
| 6 | `feat(contract): 补 workspaces v1 17 条` | | ~22 | P1 |
| 7 | `feat(contract): 补 ai_agent 15 + agent 6 + agent_task 2 + agent-card 1` | | ~28 | P1 |
| 8 | `feat(contract): 补 enterprise 治理面 12 + stats 2 + storage 1 + operation_logs 1` | | ~20 | P1 |
| 9 | `feat(contract): 补 finance 11 + bot 6 + mcp 8` | | ~28 | P1 |
| 10 | `feat(contract): 补 channel 13 + user 3 + admin 4 + feedback 1` | | ~28 | P1 |
| 11 | `feat(contract): 补 appeal 5 + report_action 3 + moderation 5` | | ~18 | P1 |
| 12 | `chore(contract): 确认 role/roles 别名族并收敛（D 类 4 条）` | 人工确认后：补正身契约 / 或提删别名路由的独立 PR（动 router，需另行评审） | ~6 | P1 |
| 13 | `feat(contract): 立 x-feature 标注规范` | root info + redocly lint 规则（可选）+ README | ~4 | **P2** |
| 14–16 | `feat(contract): 补 CS 面 56 条`（widget 14 / tenant 26 / adm+坐席 16） | 每段一 PR | ~25×3 | P2 |
| 17–18 | `feat(contract): 补 EB 面 30 条`（tenant 18 / adm 12） | | ~24+20 | P2 |
| 19 | `feat(contract): 补 moya 22 + 第三方端 7（wechat-mini/wechat-events/oa-sso/payment-callback/attachment）` | moya 与小程序端联调 | ~30 | P2 |
| 20 | `chore(tooling): contract_gate 增补 moment 窗口，export 对齐源码字面` | 可选工具项（见 §4.1） | ~2 | P2 |

**依赖关系**：PR-1、2、3 可立即并行；PR-13（标注规范）必须先于 PR-14–18；PR-4~12 之间无依赖可并行。**验收口径**：每 PR 合入后重跑对账（报告 §6 流程），对应域缺失数归零；最终 `only_router` 应收敛到 infra/static/dev 13 条 + 未决 D 4 条。

---

## 8. 附：325 条逐条归因清单

> 归因列：A=漏写待补 · B=契约陈旧待删 · C=门控口径（feature 补契约+x-feature / dev 排除声明）· D=废弃待确认。router 行号以 2026-09-30 `08c5597e` 版本 `src/imboy_router.erl` 为准。

### 8.1 B 类 16 条（契约有路由无，全列，处置=删契约）

| 路径 | 契约位置 | 归因 | 证据 |
|---|---|---|---|
| `/api/v1/e2ee/transfer/create` | openapi-api.yaml:622 | B | handler 随 52d22a4c 删除 |
| `/api/v1/e2ee/transfer/accept` | openapi-api.yaml:624 | B | 同上 |
| `/api/v1/e2ee/transfer/confirm` | openapi-api.yaml:626 | B | 同上 |
| `/api/v1/e2ee/transfer/cancel` | openapi-api.yaml:628 | B | 同上 |
| `/api/v1/e2ee/transfer/info` | openapi-api.yaml:630 | B | 同上 |
| `/api/v1/e2ee/transfer/pending` | openapi-api.yaml:632 | B | 同上 |
| `/api/v1/e2ee/social/contacts` | openapi-api.yaml:634 | B | 同上 |
| `/api/v1/e2ee/social/contacts/add` | openapi-api.yaml:636 | B | 同上 |
| `/api/v1/e2ee/social/contacts/remove` | openapi-api.yaml:638 | B | 同上 |
| `/api/v1/e2ee/social/create_shards` | openapi-api.yaml:640 | B | 同上 |
| `/api/v1/e2ee/social/shards` | openapi-api.yaml:642 | B | 同上 |
| `/api/v1/e2ee/social/recover` | openapi-api.yaml:644 | B | 同上 |
| `/api/v1/e2ee/social/proxy_shards` | openapi-api.yaml:646 | B | 同上 |
| `/api/v1/e2ee/social/decrypt_shard` | openapi-api.yaml:648 | B | 同上 |
| `/api/v1/auth/assets` | openapi-api.yaml:94 | B | 全 src/ 零注册；attachment/* 前身 |
| `/api/adm/attach/auth` | openapi-adm.yaml:143 | B | 全 src/ 零注册；adm/storage/* 前身 |

### 8.2 A 类 216 条 + D 类 4 条（REST 真缺口，分域逐条；D 类 4 条已标出）

#### /api/v1/organizations（28 条）

| 路径 | router 行 | handler | 归因 |
|---|---|---|---|
| `/api/v1/organizations/deletion-preflight` | imboy_router.erl:706 | organization_handler | A |
| `/api/v1/organizations/invitations/mine` | imboy_router.erl:709 | organization_api_handler | A |
| `/api/v1/organizations/invite_code/join` | imboy_router.erl:718 | organization_api_handler | A |
| `/api/v1/organizations/invite_code/preview` | imboy_router.erl:715 | organization_api_handler | A |
| `/api/v1/organizations/{organization_id}/archive` | imboy_router.erl:748 | organization_handler | A |
| `/api/v1/organizations/{organization_id}/default-workspace` | imboy_router.erl:821 | organization_api_handler | A |
| `/api/v1/organizations/{organization_id}/departments` | imboy_router.erl:773 | organization_api_handler | A |
| `/api/v1/organizations/{organization_id}/departments/{department_id}` | imboy_router.erl:776 | organization_api_handler | A |
| `/api/v1/organizations/{organization_id}/departments/{department_id}/archive` | imboy_router.erl:784 | organization_api_handler | A |
| `/api/v1/organizations/{organization_id}/departments/{department_id}/members` | imboy_router.erl:788 | organization_api_handler | A |
| `/api/v1/organizations/{organization_id}/departments/{department_id}/members/{user_id}` | imboy_router.erl:792 | organization_api_handler | A |
| `/api/v1/organizations/{organization_id}/departments/{department_id}/members/{user_id}/admin` | imboy_router.erl:796 | organization_api_handler | A |
| `/api/v1/organizations/{organization_id}/departments/{department_id}/move` | imboy_router.erl:780 | organization_api_handler | A |
| `/api/v1/organizations/{organization_id}/directory/departments` | imboy_router.erl:803 | organization_directory_handler | A |
| `/api/v1/organizations/{organization_id}/directory/me` | imboy_router.erl:811 | organization_directory_handler | A |
| `/api/v1/organizations/{organization_id}/directory/members` | imboy_router.erl:807 | organization_directory_handler | A |
| `/api/v1/organizations/{organization_id}/directory/search` | imboy_router.erl:815 | organization_directory_handler | A |
| `/api/v1/organizations/{organization_id}/invitations` | imboy_router.erl:756 | organization_api_handler | A |
| `/api/v1/organizations/{organization_id}/invitations/accept` | imboy_router.erl:759 | organization_api_handler | A |
| `/api/v1/organizations/{organization_id}/invitations/{invitation_id}/reject` | imboy_router.erl:763 | organization_api_handler | A |
| `/api/v1/organizations/{organization_id}/invitations/{invitation_id}/revoke` | imboy_router.erl:767 | organization_api_handler | A |
| `/api/v1/organizations/{organization_id}/invite_code` | imboy_router.erl:830 | organization_api_handler | A |
| `/api/v1/organizations/{organization_id}/invite_code/join` | imboy_router.erl:833 | organization_api_handler | A |
| `/api/v1/organizations/{organization_id}/members/{user_id}/offboard` | imboy_router.erl:737 | organization_member_handler | A |
| `/api/v1/organizations/{organization_id}/members/{user_id}/restore` | imboy_router.erl:735 | organization_member_handler | A |
| `/api/v1/organizations/{organization_id}/members/{user_id}/suspend` | imboy_router.erl:733 | organization_member_handler | A |
| `/api/v1/organizations/{organization_id}/members/{user_id}/workspaces` | imboy_router.erl:741 | organization_member_handler | A |
| `/api/v1/organizations/{organization_id}/restore` | imboy_router.erl:751 | organization_handler | A |

#### /api/adm/organizations（23 条）

| 路径 | router 行 | handler | 归因 |
|---|---|---|---|
| `/api/adm/organizations` | imboy_router.erl:1127 | adm_organization_handler | A |
| `/api/adm/organizations/{organization_id}` | imboy_router.erl:1128 | adm_organization_handler | A |
| `/api/adm/organizations/{organization_id}/archive` | imboy_router.erl:1143 | adm_organization_handler | A |
| `/api/adm/organizations/{organization_id}/departments` | imboy_router.erl:1137 | adm_organization_handler | A |
| `/api/adm/organizations/{organization_id}/departments/{department_id}/archive` | imboy_router.erl:1194 | adm_organization_handler | A |
| `/api/adm/organizations/{organization_id}/departments/{department_id}/move` | imboy_router.erl:1192 | adm_organization_handler | A |
| `/api/adm/organizations/{organization_id}/departments/{department_id}/rename` | imboy_router.erl:1190 | adm_organization_handler | A |
| `/api/adm/organizations/{organization_id}/invitations` | imboy_router.erl:1134 | adm_organization_handler | A |
| `/api/adm/organizations/{organization_id}/invitations/{invitation_id}/cancel` | imboy_router.erl:1181 | adm_organization_handler | A |
| `/api/adm/organizations/{organization_id}/invite_code` | imboy_router.erl:1185 | adm_organization_handler | A |
| `/api/adm/organizations/{organization_id}/members` | imboy_router.erl:1131 | adm_organization_handler | A |
| `/api/adm/organizations/{organization_id}/members/{user_id}/remove` | imboy_router.erl:1179 | adm_organization_handler | A |
| `/api/adm/organizations/{organization_id}/members/{user_id}/restore` | imboy_router.erl:1177 | adm_organization_handler | A |
| `/api/adm/organizations/{organization_id}/members/{user_id}/suspend` | imboy_router.erl:1175 | adm_organization_handler | A |
| `/api/adm/organizations/{organization_id}/owner-activation` | imboy_router.erl:1155 | adm_owner_activation_handler | A |
| `/api/adm/organizations/{organization_id}/owner-activation/consume` | imboy_router.erl:1167 | adm_owner_activation_handler | A |
| `/api/adm/organizations/{organization_id}/owner-activation/reactivate` | imboy_router.erl:1163 | adm_owner_activation_handler | A |
| `/api/adm/organizations/{organization_id}/owner-activation/resend` | imboy_router.erl:1159 | adm_owner_activation_handler | A |
| `/api/adm/organizations/{organization_id}/owner-transfer` | imboy_router.erl:1149 | adm_organization_handler | A |
| `/api/adm/organizations/{organization_id}/owner-transfer-by-phone` | imboy_router.erl:1171 | adm_owner_activation_handler | A |
| `/api/adm/organizations/{organization_id}/restore` | imboy_router.erl:1146 | adm_organization_handler | A |
| `/api/adm/organizations/{organization_id}/review/{review}` | imboy_router.erl:1188 | adm_organization_handler | A |
| `/api/adm/organizations/{organization_id}/workspaces` | imboy_router.erl:1140 | adm_organization_handler | A |

#### /api/v1/moya（22 条）

| 路径 | router 行 | handler | 归因 |
|---|---|---|---|
| `/api/v1/moya/assignments` | imboy_router.erl:541 | moya_assignment_handler | A |
| `/api/v1/moya/assignments/{id}` | imboy_router.erl:542 | moya_assignment_handler | A |
| `/api/v1/moya/assignments/{id}/submissions` | imboy_router.erl:545 | moya_assignment_handler | A |
| `/api/v1/moya/classes/{id}/invite-code` | imboy_router.erl:600 | moya_invite_handler | A |
| `/api/v1/moya/classes/{id}/learners` | imboy_router.erl:584 | moya_roster_handler | A |
| `/api/v1/moya/context/switch` | imboy_router.erl:540 | moya_context_handler | A |
| `/api/v1/moya/contexts` | imboy_router.erl:539 | moya_context_handler | A |
| `/api/v1/moya/invite/info` | imboy_router.erl:603 | moya_invite_handler | A |
| `/api/v1/moya/invite/join` | imboy_router.erl:606 | moya_invite_handler | A |
| `/api/v1/moya/learners/{id}/bind` | imboy_router.erl:576 | moya_learner_bind_handler | A |
| `/api/v1/moya/learners/{id}/history` | imboy_router.erl:568 | moya_assignment_handler | A |
| `/api/v1/moya/learners/{id}/history/unread-count` | imboy_router.erl:571 | moya_assignment_handler | A |
| `/api/v1/moya/learners/{id}/unbind` | imboy_router.erl:579 | moya_learner_bind_handler | A |
| `/api/v1/moya/review-queue` | imboy_router.erl:567 | moya_review_handler | A |
| `/api/v1/moya/submissions/{id}` | imboy_router.erl:548 | moya_assignment_handler | A |
| `/api/v1/moya/submissions/{id}/ai-draft` | imboy_router.erl:558 | moya_review_handler | A |
| `/api/v1/moya/submissions/{id}/review-draft` | imboy_router.erl:561 | moya_review_handler | A |
| `/api/v1/moya/submissions/{id}/review-workbench` | imboy_router.erl:554 | moya_review_handler | A |
| `/api/v1/moya/submissions/{id}/reviews/publish` | imboy_router.erl:564 | moya_review_handler | A |
| `/api/v1/moya/submissions/{id}/withdraw` | imboy_router.erl:551 | moya_assignment_handler | A |
| `/api/v1/moya/subscribe/report` | imboy_router.erl:594 | moya_subscribe_handler | A |
| `/api/v1/moya/tasks` | imboy_router.erl:591 | moya_task_handler | A |

#### /api/v1/workspaces（17 条）

| 路径 | router 行 | handler | 归因 |
|---|---|---|---|
| `/api/v1/workspaces/join` | imboy_router.erl:839 | workspace_handler | A |
| `/api/v1/workspaces/mine` | imboy_router.erl:838 | workspace_handler | A |
| `/api/v1/workspaces/{workspace_id}` | imboy_router.erl:840 | workspace_handler | A |
| `/api/v1/workspaces/{workspace_id}/archive` | imboy_router.erl:877 | workspace_handler | A |
| `/api/v1/workspaces/{workspace_id}/branding` | imboy_router.erl:844 | workspace_handler | A |
| `/api/v1/workspaces/{workspace_id}/channels` | imboy_router.erl:850 | workspace_handler | A |
| `/api/v1/workspaces/{workspace_id}/groups` | imboy_router.erl:853 | workspace_handler | A |
| `/api/v1/workspaces/{workspace_id}/invite_code` | imboy_router.erl:871 | workspace_handler | A |
| `/api/v1/workspaces/{workspace_id}/invite_code/revoke` | imboy_router.erl:874 | workspace_handler | A |
| `/api/v1/workspaces/{workspace_id}/members` | imboy_router.erl:856 | workspace_handler | A |
| `/api/v1/workspaces/{workspace_id}/members/invite` | imboy_router.erl:859 | workspace_handler | A |
| `/api/v1/workspaces/{workspace_id}/members/remove` | imboy_router.erl:862 | workspace_handler | A |
| `/api/v1/workspaces/{workspace_id}/members/role` | imboy_router.erl:865 | workspace_handler | A |
| `/api/v1/workspaces/{workspace_id}/members/transfer_owner` | imboy_router.erl:868 | workspace_handler | A |
| `/api/v1/workspaces/{workspace_id}/overview` | imboy_router.erl:847 | workspace_handler | A |
| `/api/v1/workspaces/{workspace_id}/restore` | imboy_router.erl:880 | workspace_handler | A |
| `/api/v1/workspaces/{workspace_id}/update` | imboy_router.erl:841 | workspace_handler | A |

#### /api/adm/ai_agent（15 条）

| 路径 | router 行 | handler | 归因 |
|---|---|---|---|
| `/api/adm/ai_agent/create` | imboy_router.erl:1040 | adm_ai_agent_handler | A |
| `/api/adm/ai_agent/detail` | imboy_router.erl:1039 | adm_ai_agent_handler | A |
| `/api/adm/ai_agent/knowledge_config` | imboy_router.erl:1052 | adm_ai_agent_handler | A |
| `/api/adm/ai_agent/list` | imboy_router.erl:1038 | adm_ai_agent_handler | A |
| `/api/adm/ai_agent/mandate_create` | imboy_router.erl:1065 | adm_ai_agent_handler | A |
| `/api/adm/ai_agent/role/create` | imboy_router.erl:1058 | adm_ai_agent_handler | A |
| `/api/adm/ai_agent/role/detail` | imboy_router.erl:1057 | adm_ai_agent_handler | A |
| `/api/adm/ai_agent/role/draft` | imboy_router.erl:1059 | adm_ai_agent_handler | A |
| `/api/adm/ai_agent/role/list` | imboy_router.erl:1056 | adm_ai_agent_handler | A |
| `/api/adm/ai_agent/role/publish` | imboy_router.erl:1060 | adm_ai_agent_handler | A |
| `/api/adm/ai_agent/role/set_status` | imboy_router.erl:1061 | adm_ai_agent_handler | A |
| `/api/adm/ai_agent/roles` | imboy_router.erl:1044 | adm_ai_agent_handler | A |
| `/api/adm/ai_agent/set_status` | imboy_router.erl:1042 | adm_ai_agent_handler | A |
| `/api/adm/ai_agent/update` | imboy_router.erl:1041 | adm_ai_agent_handler | A |
| `/api/adm/ai_agent/upload_avatar` | imboy_router.erl:1046 | adm_ai_agent_handler | A |

#### /api/adm/enterprise（12 条）

| 路径 | router 行 | handler | 归因 |
|---|---|---|---|
| `/api/adm/enterprise/organizations/{org_id}/applications` | imboy_router.erl:1999 | adm_enterprise_application_handler | A |
| `/api/adm/enterprise/organizations/{org_id}/applications/{application_id}` | imboy_router.erl:2002 | adm_enterprise_application_handler | A |
| `/api/adm/enterprise/organizations/{org_id}/applications/{application_id}/audit-logs` | imboy_router.erl:2032 | adm_enterprise_application_handler | A |
| `/api/adm/enterprise/organizations/{org_id}/applications/{application_id}/credentials` | imboy_router.erl:2011 | adm_enterprise_application_handler | A |
| `/api/adm/enterprise/organizations/{org_id}/applications/{application_id}/credentials/{credential_id}` | imboy_router.erl:2017 | adm_enterprise_application_handler | A |
| `/api/adm/enterprise/organizations/{org_id}/applications/{application_id}/credentials/{credential_id}/rotate` | imboy_router.erl:2014 | adm_enterprise_application_handler | A |
| `/api/adm/enterprise/organizations/{org_id}/applications/{application_id}/deliveries` | imboy_router.erl:2029 | adm_enterprise_application_handler | A |
| `/api/adm/enterprise/organizations/{org_id}/applications/{application_id}/delivery-stats` | imboy_router.erl:2026 | adm_enterprise_application_handler | A |
| `/api/adm/enterprise/organizations/{org_id}/applications/{application_id}/grants` | imboy_router.erl:2020 | adm_enterprise_application_handler | A |
| `/api/adm/enterprise/organizations/{org_id}/applications/{application_id}/grants/{grant_id}` | imboy_router.erl:2023 | adm_enterprise_application_handler | A |
| `/api/adm/enterprise/organizations/{org_id}/applications/{application_id}/scopes` | imboy_router.erl:2008 | adm_enterprise_application_handler | A |
| `/api/adm/enterprise/organizations/{org_id}/applications/{application_id}/status` | imboy_router.erl:2005 | adm_enterprise_application_handler | A |

#### /api/adm/finance（11 条）

| 路径 | router 行 | handler | 归因 |
|---|---|---|---|
| `/api/adm/finance/payment-transactions` | imboy_router.erl:1305 | adm_finance_handler | A |
| `/api/adm/finance/payment-transactions/refund` | imboy_router.erl:1308 | adm_finance_handler | A |
| `/api/adm/finance/recharge-orders` | imboy_router.erl:1301 | adm_finance_handler | A |
| `/api/adm/finance/recharge-orders/refund` | imboy_router.erl:1302 | adm_finance_handler | A |
| `/api/adm/finance/wallet/{user_id}/transactions` | imboy_router.erl:1298 | adm_finance_handler | A |
| `/api/adm/finance/wallets` | imboy_router.erl:1297 | adm_finance_handler | A |
| `/api/adm/finance/wallets/freeze` | imboy_router.erl:1311 | adm_finance_handler | A |
| `/api/adm/finance/wallets/unfreeze` | imboy_router.erl:1312 | adm_finance_handler | A |
| `/api/adm/finance/withdrawals` | imboy_router.erl:1324 | adm_finance_handler | A |
| `/api/adm/finance/withdrawals/complete` | imboy_router.erl:1325 | adm_finance_handler | A |
| `/api/adm/finance/withdrawals/reject` | imboy_router.erl:1328 | adm_finance_handler | A |

#### /api/v1/channel（11 条）

| 路径 | router 行 | handler | 归因 |
|---|---|---|---|
| `/api/v1/channel/order/cancel` | imboy_router.erl:479 | channel_handler_order | A |
| `/api/v1/channel/order/refund` | imboy_router.erl:481 | channel_handler_order | A |
| `/api/v1/channel/qrcode` | imboy_router.erl:367 | channel_handler | A |
| `/api/v1/channel/{channel_id}/archive` | imboy_router.erl:375 | channel_handler | A |
| `/api/v1/channel/{channel_id}/comment/{comment_id}/delete` | imboy_router.erl:440 | channel_handler_comment | A |
| `/api/v1/channel/{channel_id}/comment/{comment_id}/like` | imboy_router.erl:444 | channel_handler_comment | A |
| `/api/v1/channel/{channel_id}/comment/{comment_id}/unlike` | imboy_router.erl:447 | channel_handler_comment | A |
| `/api/v1/channel/{channel_id}/message/{message_id}/comment` | imboy_router.erl:436 | channel_handler_comment | A |
| `/api/v1/channel/{channel_id}/message/{message_id}/comments` | imboy_router.erl:432 | channel_handler_comment | A |
| `/api/v1/channel/{channel_id}/restore` | imboy_router.erl:376 | channel_handler | A |
| `/api/v1/channel/{channel_id}/webhook/{webhook_id}/rotate` | imboy_router.erl:495 | channel_webhook_handler | A |

#### /api/v1/e（9 条）

| 路径 | router 行 | handler | 归因 |
|---|---|---|---|
| `/api/v1/e2ee/devices` | imboy_router.erl:201 | olm_handler | A |
| `/api/v1/e2ee/devices/batch_claim` | imboy_router.erl:202 | olm_handler | A |
| `/api/v1/e2ee/group_history_grant` | imboy_router.erl:175 | e2ee_handler | A |
| `/api/v1/e2ee/olm/claim` | imboy_router.erl:198 | olm_handler | A |
| `/api/v1/e2ee/olm/fallback_key` | imboy_router.erl:196 | olm_handler | A |
| `/api/v1/e2ee/olm/get_identity` | imboy_router.erl:197 | olm_handler | A |
| `/api/v1/e2ee/olm/identity` | imboy_router.erl:194 | olm_handler | A |
| `/api/v1/e2ee/olm/prekey_count` | imboy_router.erl:199 | olm_handler | A |
| `/api/v1/e2ee/olm/prekeys` | imboy_router.erl:195 | olm_handler | A |

#### /api/adm/mcp（8 条）

| 路径 | router 行 | handler | 归因 |
|---|---|---|---|
| `/api/adm/mcp/audit` | imboy_router.erl:1073 | adm_mcp_handler | A |
| `/api/adm/mcp/clients` | imboy_router.erl:1066 | adm_mcp_handler | A |
| `/api/adm/mcp/clients/approve` | imboy_router.erl:1068 | adm_mcp_handler | A |
| `/api/adm/mcp/clients/create` | imboy_router.erl:1067 | adm_mcp_handler | A |
| `/api/adm/mcp/clients/grants` | imboy_router.erl:1071 | adm_mcp_handler | A |
| `/api/adm/mcp/clients/grants/set` | imboy_router.erl:1072 | adm_mcp_handler | A |
| `/api/adm/mcp/clients/reject` | imboy_router.erl:1069 | adm_mcp_handler | A |
| `/api/adm/mcp/clients/revoke` | imboy_router.erl:1070 | adm_mcp_handler | A |

#### /api/adm/bot（6 条）

| 路径 | router 行 | handler | 归因 |
|---|---|---|---|
| `/api/adm/bot/deliveries` | imboy_router.erl:1075 | adm_bot_delivery_handler | A |
| `/api/adm/bot/deliveries/replay` | imboy_router.erl:1074 | adm_bot_delivery_handler | A |
| `/api/adm/bot/detail` | imboy_router.erl:1262 | adm_bot_handler | A |
| `/api/adm/bot/disable` | imboy_router.erl:1263 | adm_bot_handler | A |
| `/api/adm/bot/enable` | imboy_router.erl:1264 | adm_bot_handler | A |
| `/api/adm/bot/list` | imboy_router.erl:1261 | adm_bot_handler | A |

#### /api/v1/agent（6 条）

| 路径 | router 行 | handler | 归因 |
|---|---|---|---|
| `/api/v1/agent/categories` | imboy_router.erl:144 | ai_agent_handler | A |
| `/api/v1/agent/discover` | imboy_router.erl:142 | ai_agent_handler | A |
| `/api/v1/agent/mandate/active` | imboy_router.erl:652 | agent_mandate_handler | A |
| `/api/v1/agent/mandate/authorize` | imboy_router.erl:650 | agent_mandate_handler | A |
| `/api/v1/agent/mandate/revoke` | imboy_router.erl:651 | agent_mandate_handler | A |
| `/api/v1/agent/search` | imboy_router.erl:143 | ai_agent_handler | A |

#### /api/adm/moderation（5 条）

| 路径 | router 行 | handler | 归因 |
|---|---|---|---|
| `/api/adm/moderation/review-queue` | imboy_router.erl:1382 | adm_moderation_handler | A |
| `/api/adm/moderation/review-queue/{id}/moderate` | imboy_router.erl:1379 | adm_moderation_handler | A |
| `/api/adm/moderation/sensitive-words` | imboy_router.erl:1376 | adm_moderation_handler | A |
| `/api/adm/moderation/sensitive-words/import` | imboy_router.erl:1370 | adm_moderation_handler | A |
| `/api/adm/moderation/sensitive-words/{id}` | imboy_router.erl:1373 | adm_moderation_handler | A |

#### /api/adm/workspace（5 条）

| 路径 | router 行 | handler | 归因 |
|---|---|---|---|
| `/api/adm/workspace/archive` | imboy_router.erl:1122 | adm_workspace_handler | A |
| `/api/adm/workspace/detail` | imboy_router.erl:1120 | adm_workspace_handler | A |
| `/api/adm/workspace/list` | imboy_router.erl:1119 | adm_workspace_handler | A |
| `/api/adm/workspace/members` | imboy_router.erl:1121 | adm_workspace_handler | A |
| `/api/adm/workspace/restore` | imboy_router.erl:1123 | adm_workspace_handler | A |

#### /api/v1/wallet（5 条）

| 路径 | router 行 | handler | 归因 |
|---|---|---|---|
| `/api/v1/wallet/recharge/confirm` | imboy_router.erl:636 | wallet_handler | A |
| `/api/v1/wallet/red_packet/open` | imboy_router.erl:640 | wallet_handler | A |
| `/api/v1/wallet/red_packet/send` | imboy_router.erl:639 | wallet_handler | A |
| `/api/v1/wallet/red_packet/{id}/detail` | imboy_router.erl:641 | wallet_handler | A |
| `/api/v1/wallet/transfer/accept` | imboy_router.erl:645 | wallet_handler | A |

#### /api/adm/admin（4 条）

| 路径 | router 行 | handler | 归因 |
|---|---|---|---|
| `/api/adm/admin/config/feedback-workflow` | imboy_router.erl:993 | adm_admin_handler | A |
| `/api/adm/admin/config/product-experience` | imboy_router.erl:976 | adm_admin_handler | A |
| `/api/adm/admin/config/sidebar` | imboy_router.erl:992 | adm_admin_handler | A |
| `/api/adm/admin/ux/events` | imboy_router.erl:997 | adm_stats_handler | A |

#### /api/adm/report_action（3 条）

| 路径 | router 行 | handler | 归因 |
|---|---|---|---|
| `/api/adm/report_action/execute` | imboy_router.erl:1357 | adm_report_action_handler | A |
| `/api/adm/report_action/list` | imboy_router.erl:1359 | adm_report_action_handler | A |
| `/api/adm/report_action/reverse` | imboy_router.erl:1358 | adm_report_action_handler | A |

#### /api/adm/user（3 条）

| 路径 | router 行 | handler | 归因 |
|---|---|---|---|
| `/api/adm/user/device/kick` | imboy_router.erl:1106 | adm_user_handler | A |
| `/api/adm/user/devices` | imboy_router.erl:1105 | adm_user_handler | A |
| `/api/adm/user/force_logout` | imboy_router.erl:1104 | adm_user_handler | A |

#### /api/v1/appeal（3 条）

| 路径 | router 行 | handler | 归因 |
|---|---|---|---|
| `/api/v1/appeal/actions` | imboy_router.erl:615 | appeal_handler | A |
| `/api/v1/appeal/create` | imboy_router.erl:613 | appeal_handler | A |
| `/api/v1/appeal/my` | imboy_router.erl:614 | appeal_handler | A |

#### /api/adm/appeal（2 条）

| 路径 | router 行 | handler | 归因 |
|---|---|---|---|
| `/api/adm/appeal/list` | imboy_router.erl:1386 | adm_appeal_handler | A |
| `/api/adm/appeal/review` | imboy_router.erl:1387 | adm_appeal_handler | A |

#### /api/adm/channel（2 条）

| 路径 | router 行 | handler | 归因 |
|---|---|---|---|
| `/api/adm/channel/order/refund` | imboy_router.erl:1266 | adm_channel_handler | A |
| `/api/adm/channel/{channel_id}/price` | imboy_router.erl:1293 | adm_channel_handler | A |

#### /api/adm/role（2 条）

| 路径 | router 行 | handler | 归因 |
|---|---|---|---|
| `/api/adm/role/delete` | imboy_router.erl:1036 | adm_role_handler | D |
| `/api/adm/role/disable` | imboy_router.erl:1034 | adm_role_handler | D |

#### /api/adm/roles（2 条）

| 路径 | router 行 | handler | 归因 |
|---|---|---|---|
| `/api/adm/roles/delete` | imboy_router.erl:1037 | adm_role_handler | D |
| `/api/adm/roles/disable` | imboy_router.erl:1035 | adm_role_handler | D |

#### /api/adm/stats（2 条）

| 路径 | router 行 | handler | 归因 |
|---|---|---|---|
| `/api/adm/stats/finance` | imboy_router.erl:1412 | adm_stats_handler | A |
| `/api/adm/stats/finance/report` | imboy_router.erl:1413 | adm_stats_handler | A |

#### /api/v1/agent_task（2 条）

| 路径 | router 行 | handler | 归因 |
|---|---|---|---|
| `/api/v1/agent_task/approve` | imboy_router.erl:532 | agent_task_handler | A |
| `/api/v1/agent_task/reject` | imboy_router.erl:533 | agent_task_handler | A |

#### /api/v1/passport（2 条）

| 路径 | router 行 | handler | 归因 |
|---|---|---|---|
| `/api/v1/passport/alipay_authinfo` | imboy_router.erl:74 | passport_handler | A |
| `/api/v1/passport/alipay_login` | imboy_router.erl:73 | passport_handler | A |

#### /api/adm/feedback（1 条）

| 路径 | router 行 | handler | 归因 |
|---|---|---|---|
| `/api/adm/feedback/status` | imboy_router.erl:1025 | adm_feedback_handler | A |

#### /api/adm/operation_logs（1 条）

| 路径 | router 行 | handler | 归因 |
|---|---|---|---|
| `/api/adm/operation_logs` | imboy_router.erl:1107 | adm_operation_log_handler | A |

#### /api/adm/storage（1 条）

| 路径 | router 行 | handler | 归因 |
|---|---|---|---|
| `/api/adm/storage/download` | imboy_router.erl:1091 | adm_attach_handler | A |

#### /api/v1/agent-card（1 条）

| 路径 | router 行 | handler | 归因 |
|---|---|---|---|
| `/api/v1/agent-card` | imboy_router.erl:61 | agent_card_handler | A |

#### /api/v1/attachment（1 条）

| 路径 | router 行 | handler | 归因 |
|---|---|---|---|
| `/api/v1/attachment/upload` | imboy_router.erl:683 | attach_handler | A |

#### /api/v1/auth（1 条）

| 路径 | router 行 | handler | 归因 |
|---|---|---|---|
| `/api/v1/auth/wechat-mini/login` | imboy_router.erl:82 | moya_auth_handler | A |

#### /api/v1/oa（1 条）

| 路径 | router 行 | handler | 归因 |
|---|---|---|---|
| `/api/v1/oa/sso/code` | imboy_router.erl:91 | enterprise_oa_sso_handler | A |

#### /api/v1/payment（1 条）

| 路径 | router 行 | handler | 归因 |
|---|---|---|---|
| `/api/v1/payment/callback/{gateway}` | imboy_router.erl:655 | payment_callback_handler | A |

#### /api/v1/user（1 条）

| 路径 | router 行 | handler | 归因 |
|---|---|---|---|
| `/api/v1/user/deletion_status` | imboy_router.erl:155 | user_handler | A |

#### /api/v1/wechat（1 条）

| 路径 | router 行 | handler | 归因 |
|---|---|---|---|
| `/api/v1/wechat/mini/events` | imboy_router.erl:96 | moya_wechat_msg_handler | A |

### 8.3 C 类 89 条（86 feature 门控 + 3 dev-only，逐条）

#### CUSTOMER_SERVICE（56 条）

| 路径 | router 行 | handler | 归因 |
|---|---|---|---|
| `/api/adm/customer-service/organizations/{org_id}/provisioning` | imboy_router.erl:2349 | cs_platform_handler | C |
| `/api/adm/customer-service/organizations/{org_id}/seats` | imboy_router.erl:2341 | cs_platform_handler | C |
| `/api/adm/customer-service/organizations/{org_id}/seats/{id}/resume` | imboy_router.erl:2380 | cs_platform_handler | C |
| `/api/adm/customer-service/organizations/{org_id}/seats/{id}/suspend` | imboy_router.erl:2374 | cs_platform_handler | C |
| `/api/adm/customer-service/organizations/{org_id}/sessions` | imboy_router.erl:2356 | cs_platform_handler | C |
| `/api/adm/customer-service/organizations/{org_id}/sessions/{id}` | imboy_router.erl:2369 | cs_platform_handler | C |
| `/api/adm/customer-service/organizations/{org_id}/sessions/{id}/close` | imboy_router.erl:2391 | cs_platform_handler | C |
| `/api/adm/customer-service/organizations/{org_id}/sessions/{id}/transfer` | imboy_router.erl:2385 | cs_platform_handler | C |
| `/api/adm/customer-service/organizations/{org_id}/stats/sessions` | imboy_router.erl:2364 | cs_platform_handler | C |
| `/api/adm/customer-service/seat-consoles` | imboy_router.erl:2416 | cs_platform_handler | C |
| `/api/adm/customer-service/seat-consoles/{id}` | imboy_router.erl:2421 | cs_platform_handler | C |
| `/api/adm/customer-service/seat-consoles/{id}/revoke` | imboy_router.erl:2426 | cs_platform_handler | C |
| `/api/adm/customer-service/seats` | imboy_router.erl:2336 | cs_platform_handler | C |
| `/api/adm/customer-service/widget-installations` | imboy_router.erl:2397 | cs_platform_handler | C |
| `/api/adm/customer-service/widget-installations/{id}` | imboy_router.erl:2402 | cs_platform_handler | C |
| `/api/adm/customer-service/widget-installations/{id}/revoke` | imboy_router.erl:2407 | cs_platform_handler | C |
| `/api/v1/cs/me/seat-contexts` | imboy_router.erl:2056 | cs_tenant_handler | C |
| `/api/v1/cs/organizations/{org_id}/seat-limit` | imboy_router.erl:2178 | cs_tenant_handler | C |
| `/api/v1/cs/organizations/{org_id}/seats` | imboy_router.erl:2108 | cs_tenant_handler | C |
| `/api/v1/cs/organizations/{org_id}/seats/me/events` | imboy_router.erl:2237 | cs_tenant_handler | C |
| `/api/v1/cs/organizations/{org_id}/seats/me/heartbeat` | imboy_router.erl:2195 | cs_tenant_handler | C |
| `/api/v1/cs/organizations/{org_id}/seats/me/presence` | imboy_router.erl:2204 | cs_tenant_handler | C |
| `/api/v1/cs/organizations/{org_id}/seats/presence` | imboy_router.erl:2211 | cs_tenant_handler | C |
| `/api/v1/cs/organizations/{org_id}/seats/sessions` | imboy_router.erl:2220 | cs_tenant_handler | C |
| `/api/v1/cs/organizations/{org_id}/seats/{id}/resume` | imboy_router.erl:2118 | cs_tenant_handler | C |
| `/api/v1/cs/organizations/{org_id}/seats/{id}/suspend` | imboy_router.erl:2113 | cs_tenant_handler | C |
| `/api/v1/cs/organizations/{org_id}/sessions/queue` | imboy_router.erl:2062 | cs_tenant_handler | C |
| `/api/v1/cs/organizations/{org_id}/sessions/{id}` | imboy_router.erl:2148 | cs_tenant_handler | C |
| `/api/v1/cs/organizations/{org_id}/sessions/{id}/claim` | imboy_router.erl:2068 | cs_tenant_handler | C |
| `/api/v1/cs/organizations/{org_id}/sessions/{id}/close` | imboy_router.erl:2080 | cs_tenant_handler | C |
| `/api/v1/cs/organizations/{org_id}/sessions/{id}/context` | imboy_router.erl:2158 | cs_tenant_handler | C |
| `/api/v1/cs/organizations/{org_id}/sessions/{id}/read-cursor` | imboy_router.erl:2169 | cs_tenant_handler | C |
| `/api/v1/cs/organizations/{org_id}/sessions/{id}/transfer` | imboy_router.erl:2074 | cs_tenant_handler | C |
| `/api/v1/cs/organizations/{org_id}/shop-keys` | imboy_router.erl:2125 | cs_tenant_handler | C |
| `/api/v1/cs/organizations/{org_id}/shop-keys/{id}/revoke` | imboy_router.erl:2130 | cs_tenant_handler | C |
| `/api/v1/cs/organizations/{org_id}/stats/sessions` | imboy_router.erl:2186 | cs_tenant_handler | C |
| `/api/v1/cs/organizations/{org_id}/transfer-targets` | imboy_router.erl:2228 | cs_tenant_handler | C |
| `/api/v1/cs/organizations/{org_id}/visit-tokens` | imboy_router.erl:2136 | cs_tenant_handler | C |
| `/api/v1/cs/organizations/{org_id}/visit-tokens/{id}/revoke` | imboy_router.erl:2141 | cs_tenant_handler | C |
| `/api/v1/cs/sessions` | imboy_router.erl:2087 | cs_tenant_handler | C |
| `/api/v1/cs/sessions/{id}/messages` | imboy_router.erl:2091 | cs_tenant_handler | C |
| `/api/v1/cs/sessions/{id}/rating` | imboy_router.erl:2095 | cs_tenant_handler | C |
| `/api/v1/cs/widget/bootstrap` | imboy_router.erl:2249 | cs_widget_handler | C |
| `/api/v1/cs/widget/frame/{installation_id}` | imboy_router.erl:2260 | cs_widget_frame_handler | C |
| `/api/v1/cs/widget/identity/exchange` | imboy_router.erl:2253 | cs_widget_handler | C |
| `/api/v1/cs/widget/sessions` | imboy_router.erl:2290 | cs_widget_handler | C |
| `/api/v1/cs/widget/sessions/{id}/assets/confirm` | imboy_router.erl:2306 | cs_widget_handler | C |
| `/api/v1/cs/widget/sessions/{id}/assets/presign` | imboy_router.erl:2302 | cs_widget_handler | C |
| `/api/v1/cs/widget/sessions/{id}/assets/upload` | imboy_router.erl:2313 | cs_widget_handler | C |
| `/api/v1/cs/widget/sessions/{id}/assets/{asset}/content` | imboy_router.erl:2319 | cs_widget_handler | C |
| `/api/v1/cs/widget/sessions/{id}/events` | imboy_router.erl:2298 | cs_widget_handler | C |
| `/api/v1/cs/widget/sessions/{id}/messages` | imboy_router.erl:2294 | cs_widget_handler | C |
| `/api/v1/cs/widget/sessions/{id}/rating` | imboy_router.erl:2323 | cs_widget_handler | C |
| `/api/v1/enterprise/conversations/{conversation_id}/messages` | imboy_router.erl:2100 | cs_tenant_handler | C |
| `/seat/{public_seat_console_id}` | imboy_router.erl:2286 | cs_seat_console_handler | C |
| `/w/{public_widget_id}` | imboy_router.erl:2273 | cs_widget_handler | C |

#### ENTERPRISE_BUSINESS（30 条）

| 路径 | router 行 | handler | 归因 |
|---|---|---|---|
| `/api/adm/enterprise-business/organizations/{org_id}/assets/{id}/content` | imboy_router.erl:1919 | eb_platform_handler | C |
| `/api/adm/enterprise-business/organizations/{org_id}/contacts` | imboy_router.erl:1897 | eb_platform_handler | C |
| `/api/adm/enterprise-business/organizations/{org_id}/contacts/{id}` | imboy_router.erl:1902 | eb_platform_handler | C |
| `/api/adm/enterprise-business/organizations/{org_id}/conversations/{id}/messages` | imboy_router.erl:1907 | eb_platform_handler | C |
| `/api/adm/enterprise-business/organizations/{org_id}/identities` | imboy_router.erl:1892 | eb_platform_handler | C |
| `/api/adm/enterprise-business/organizations/{org_id}/members/{uid}/suspend` | imboy_router.erl:1925 | eb_platform_handler | C |
| `/api/adm/enterprise-business/organizations/{org_id}/messages/{message_id}` | imboy_router.erl:1913 | eb_platform_handler | C |
| `/api/adm/enterprise-business/organizations/{org_id}/offboarding/cases` | imboy_router.erl:1951 | eb_platform_handler | C |
| `/api/adm/enterprise-business/organizations/{org_id}/offboarding/cases/{id}` | imboy_router.erl:1957 | eb_platform_handler | C |
| `/api/adm/enterprise-business/organizations/{org_id}/offboarding/{id}/execute` | imboy_router.erl:1931 | eb_platform_handler | C |
| `/api/adm/enterprise-business/organizations/{org_id}/offboarding/{id}/finalize` | imboy_router.erl:1943 | eb_platform_handler | C |
| `/api/adm/enterprise-business/organizations/{org_id}/offboarding/{id}/verify` | imboy_router.erl:1937 | eb_platform_handler | C |
| `/api/v1/enterprise/organizations/{org_id}/assets/confirm` | imboy_router.erl:1835 | eb_tenant_handler | C |
| `/api/v1/enterprise/organizations/{org_id}/assets/presign` | imboy_router.erl:1829 | eb_tenant_handler | C |
| `/api/v1/enterprise/organizations/{org_id}/assets/{id}/content` | imboy_router.erl:1842 | eb_tenant_handler | C |
| `/api/v1/enterprise/organizations/{org_id}/business-identities` | imboy_router.erl:1772 | eb_tenant_handler | C |
| `/api/v1/enterprise/organizations/{org_id}/business-identities/{id}/assign` | imboy_router.erl:1777 | eb_tenant_handler | C |
| `/api/v1/enterprise/organizations/{org_id}/contacts` | imboy_router.erl:1783 | eb_tenant_handler | C |
| `/api/v1/enterprise/organizations/{org_id}/contacts/{id}` | imboy_router.erl:1789 | eb_tenant_handler | C |
| `/api/v1/enterprise/organizations/{org_id}/contacts/{id}/notes` | imboy_router.erl:1795 | eb_tenant_handler | C |
| `/api/v1/enterprise/organizations/{org_id}/conversations` | imboy_router.erl:1801 | eb_tenant_handler | C |
| `/api/v1/enterprise/organizations/{org_id}/conversations/{id}/messages` | imboy_router.erl:1807 | eb_tenant_handler | C |
| `/api/v1/enterprise/organizations/{org_id}/conversations/{id}/messages/{message_id}/ack` | imboy_router.erl:1822 | eb_tenant_handler | C |
| `/api/v1/enterprise/organizations/{org_id}/members/{uid}/suspend` | imboy_router.erl:1848 | eb_tenant_handler | C |
| `/api/v1/enterprise/organizations/{org_id}/offboarding` | imboy_router.erl:1853 | eb_tenant_handler | C |
| `/api/v1/enterprise/organizations/{org_id}/offboarding/cases` | imboy_router.erl:1877 | eb_tenant_handler | C |
| `/api/v1/enterprise/organizations/{org_id}/offboarding/cases/{id}` | imboy_router.erl:1882 | eb_tenant_handler | C |
| `/api/v1/enterprise/organizations/{org_id}/offboarding/{id}/execute` | imboy_router.erl:1858 | eb_tenant_handler | C |
| `/api/v1/enterprise/organizations/{org_id}/offboarding/{id}/finalize` | imboy_router.erl:1868 | eb_tenant_handler | C |
| `/api/v1/enterprise/organizations/{org_id}/offboarding/{id}/verify` | imboy_router.erl:1863 | eb_tenant_handler | C |

#### dev-only（3 条）

| 路径 | router 行 | handler | 归因 |
|---|---|---|---|
| `/api/v1/agent_task/demo` | imboy_router.erl:1599 | agent_task_demo_handler | C |
| `/api/v1/test/req_get` | imboy_router.erl:1595 | test_handler | C |
| `/api/v1/test/req_post` | imboy_router.erl:1596 | test_handler | C |

TOTAL: 319
