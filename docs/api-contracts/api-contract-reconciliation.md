# API 契约对账报告（imboy_router.erl ↔ api/openapi-*.yaml）

> 生成日期 / Generated: 2026-09-30
> 真源 A / Source A: `src/imboy_router.erl`（`get_routes/0` 全部路由注册，860 条）
> 真源 B / Source B: `api/openapi-api.yaml`（353 path）+ `api/openapi-adm.yaml`（178）+ `api/openapi-internal.yaml`（26），合计 557 path / 577 operation（聚合文件 `api/openapi.yaml` 与三真源并集一致）
> 关联机制 / Related: `scripts/check_rest_contract_coverage.sh`（RTF-04，curated TSV 门禁）、`scripts/contract_gate.py`、`.contract/api_contract.json`（contract-export）

<!-- AUTO-GENERATED: START (source: src/imboy_router.erl + api/*.yaml) -->

## 1. 口径与局限 / Method & Limitations

- **路径归一化**：cowboy `:param` 段与 OpenAPI `{param}` 段视为等价后逐条比对。
- **方法（C 类）局限**：cowboy 路由是纯 path 匹配（同路径双语义由 handler 内 `cowboy_req:method/1` 分派，见 `imboy_router.erl` 内注释），**路由层无方法真源**，方法不一致无法机械判定；本次仅做 handler 抽样（见 §4）。
- **feature 编译门**：`IMBOY_FEATURE_MOMENT` / `IMBOY_FEATURE_ENTERPRISE_BUSINESS` / `IMBOY_FEATURE_CUSTOMER_SERVICE` 门控路由在未选中时物理裁剪（不进 beam）；本次按"源码字面注册"口径统计并单独分组（§3.2）。
- **dev-only 路由**：`/api/v1/test/*` 与 `/api/v1/agent_task/demo` 仅非生产注册（`is_dev_env/0` 运行时门）。
- **plugin 动态路由**：`imboy_router_registry` ETS 运行时注册，静态不可枚举，不在本次对账范围（与 coverage gate 口径一致）。
- B 类（契约多余）逐条经 `grep -rn` 全 `src/` 复核，**均无任何静态或动态注册**。

## 2. 总览统计 / Summary

| 类别 | 定义 | 条数 | 处置倾向 |
|---|---|---|---|
| C1a | 路由有、契约缺（REST，非 feature 门、非 dev/infra/static） | **220** | 补契约（分域分批） |
| C1b | 路由有、契约缺（feature 门控：CS 56 + EB 30；MOMENT 0） | 86 | 需人工确认（补契约并标 feature，或声明排除） |
| C1c | 路由有、契约缺（infra 6 / static 4 / dev-only 3） | 13 | 无需契约（建议在契约根文件声明排除面） |
| C2 | 契约有、路由无（疑似已删/未实现） | **16** | 删契约（需人工确认） |
| C3 | 方法不一致 | 未判定 | 需逐 handler 专项审计 |
| 合计 | — | 325 | >50，本文按汇总+样例呈现 |

覆盖最好的域（对照，前缀字面量口径）：`/api/v1/moment*` 18/18 全覆盖（12 条 `/api/v1/moment` + 6 条 `/api/adm/moment`）；`/api/v1/channel*` 46/57（单数前缀 37/48 + 复数 `channels/*` 9 条全覆盖）；`/api/adm/admin` 17/21。

## 3. C1：路由有而契约缺（309 条）

### 3.1 REST 真缺口 220 条（分域统计 + 样例）

| 域 | 域内路由总数 | 契约缺失 | 缺失样例（最多3条） |
|---|---|---|---|
| `/api/v1/organizations` | 35 | 28 | `/api/v1/organizations/deletion-preflight`、`.../invitations/mine`、`.../invite_code/preview` |
| `/api/adm/organizations` | 23 | 23 | `/api/adm/organizations`、`.../organizations/:organization_id`、`.../members` |
| `/api/v1/moya` | 22 | 22 | `/api/v1/moya/contexts`、`.../context/switch`、`.../assignments` |
| `/api/v1/workspaces` | 19 | 17 | `/api/v1/workspaces/mine`、`.../join`、`.../:workspace_id` |
| `/api/adm/ai_agent` | 16 | 15 | `/api/adm/ai_agent/list`、`.../detail`、`.../create` |
| `/api/adm/enterprise` | 12 | 12 | `/api/adm/enterprise/organizations/:org_id/applications`、`.../:application_id`、`.../status` |
| `/api/v1/channel*` | 57 | 11 | `/api/v1/channel/qrcode`、`.../:channel_id/archive`、`.../restore`（57 含复数 `channels/*` 9 条，均已有契约） |
| `/api/adm/finance` | 16 | 11 | `/api/adm/finance/wallets`、`.../wallet/:user_id/transactions`、`.../recharge-orders` |
| `/api/v1/e2ee` | 21 | 9 | `/api/v1/e2ee/group_history_grant`、`.../olm/identity`、`.../olm/prekeys` |
| `/api/adm/mcp` | 8 | 8 | `/api/adm/mcp/clients`、`.../create`、`.../approve` |
| `/api/v1/agent` | 7 | 6 | `/api/v1/agent/discover`、`.../search`、`.../categories` |
| `/api/adm/bot` | 6 | 6 | `/api/adm/bot/deliveries/replay`、`.../deliveries`、`.../list` |
| `/api/v1/wallet` | 13 | 5 | `/api/v1/wallet/recharge/confirm`、`.../red_packet/send`、`.../red_packet/open` |
| `/api/adm/workspace` | 5 | 5 | `/api/adm/workspace/list`、`.../detail`、`.../members` |
| `/api/adm/moderation` | 5 | 5 | `/api/adm/moderation/sensitive-words/import`、`.../:id`、`.../sensitive-words` |
| `/api/adm/admin` | 21 | 4 | `.../config/product-experience`、`.../config/sidebar`、`.../config/feedback-workflow` |
| `/api/v1/appeal` | 3 | 3 | `/api/v1/appeal/create`、`.../my`、`.../actions` |
| `/api/adm/user` | 19 | 3 | `.../force_logout`、`.../devices`、`.../device/kick` |
| `/api/adm/report_action` | 3 | 3 | `.../execute`、`.../reverse`、`.../list` |
| `/api/v1/passport` | 14 | 2 | `.../alipay_login`、`.../alipay_authinfo` |
| `/api/v1/agent_task` | 3 | 2 | `.../approve`、`.../reject` |
| `/api/adm/role` + `/api/adm/roles` | 11 | 4 | `.../role/disable`、`.../roles/delete`（别名族） |
| `/api/adm/channel` | 20 | 2 | `.../order/refund`、`.../:channel_id/price` |
| `/api/adm/appeal` | 2 | 2 | `.../list`、`.../review` |
| `/api/adm/stats` | 8 | 2 | `.../finance`、`.../finance/report` |
| `/api/v1/agent-card` | 1 | 1 | `/api/v1/agent-card` |
| `/api/v1/auth` | 4 | 1 | `.../auth/wechat-mini/login` |
| `/api/v1/oa` | 1 | 1 | `.../oa/sso/code` |
| `/api/v1/wechat` | 1 | 1 | `.../wechat/mini/events` |
| `/api/v1/user` | 12 | 1 | `.../deletion_status` |
| `/api/v1/payment` | 1 | 1 | `.../payment/callback/:gateway` |
| `/api/v1/attachment` | 4 | 1 | `.../attachment/upload` |
| `/api/adm/feedback` | 4 | 1 | `.../feedback/status` |
| `/api/adm/storage` | 8 | 1 | `.../storage/download` |
| `/api/adm/operation_logs` | 1 | 1 | `/api/adm/operation_logs` |

**处置建议**：补契约。整域缺失（缺失率 100%：`/api/adm/organizations`、`/api/v1/moya`、`/api/adm/enterprise`(FULL-08 治理面)、`/api/adm/mcp`、`/api/adm/workspace`、`/api/adm/moderation`、`/api/v1/appeal`、`/api/adm/report_action`）建议按域整批补齐；高覆盖域（channel/wallet/e2ee/passport）按单端点补齐。补契约前需人工拍板拆分到对应 surface（api/adm）。

### 3.2 feature 门控缺口 86 条

- **CUSTOMER_SERVICE**：56 条（`/api/v1/cs/*` 37、`/api/adm/customer-service/*` 16、`/w/:public_widget_id`、`/seat/:public_seat_console_id`、`/api/v1/enterprise/conversations/:conversation_id/messages`）；样例：`/api/v1/cs/me/seat-contexts`、`/api/v1/cs/organizations/:org_id/sessions/queue`、`/api/v1/cs/organizations/:org_id/seats`
- **ENTERPRISE_BUSINESS**：30 条（`/api/v1/enterprise/organizations/:org_id/*` 18、`/api/adm/enterprise-business/*` 12）；样例：`.../business-identities`、`.../contacts`、`.../offboarding/cases`
- **MOMENT**：0 条缺失（moment 18 条路由契约全覆盖）。

**处置建议**：需人工确认——feature 未选中的部署不注册这些路由；要么补契约并加 `x-feature`/`deprecated-if-disabled` 类标注，要么在契约根文件 description 声明"feature 门控面不入契约"。不建议默认全量补入。

### 3.3 infra / static / dev-only 13 条

`/metrics`、`/healthz`、`/livez`、`/readyz`、`/api/v1/mcp`、`/.well-known/agent.json`（infra）；`/privacy-policy`、`/account-deletion`、`/static/[...]`、`/static/admin/[...]`（static）；`/api/v1/test/req_get`、`/api/v1/test/req_post`、`/api/v1/agent_task/demo`（dev-only）。

**处置建议**：无需 REST 契约。`/api/v1/mcp` 与 `/.well-known/agent.json` 属 MCP/A2A 协议面（另有协议文档），`/healthz` 等属运维探针。建议在 `api/README.md` 或契约根文件声明排除面清单，避免后续对账反复误报。

## 4. C2：契约有而路由无（16 条，全列）

经全 `src/` grep 复核（含动态注册路径）均无实现：

| 契约路径 | 方法 | surface | 处置建议 |
|---|---|---|---|
| `/api/v1/e2ee/social/shards` | GET | api | 删契约（需人工确认） |
| `/api/v1/e2ee/social/contacts` | GET | api | 删契约（需人工确认） |
| `/api/v1/e2ee/social/contacts/add` | POST | api | 删契约（需人工确认） |
| `/api/v1/e2ee/social/contacts/remove` | POST | api | 删契约（需人工确认） |
| `/api/v1/e2ee/social/create_shards` | POST | api | 删契约（需人工确认） |
| `/api/v1/e2ee/social/decrypt_shard` | POST | api | 删契约（需人工确认） |
| `/api/v1/e2ee/social/proxy_shards` | GET | api | 删契约（需人工确认） |
| `/api/v1/e2ee/social/recover` | POST | api | 删契约（需人工确认） |
| `/api/v1/e2ee/transfer/create` | POST | api | 删契约（需人工确认） |
| `/api/v1/e2ee/transfer/pending` | GET | api | 删契约（需人工确认） |
| `/api/v1/e2ee/transfer/info` | GET | api | 删契约（需人工确认） |
| `/api/v1/e2ee/transfer/accept` | POST | api | 删契约（需人工确认） |
| `/api/v1/e2ee/transfer/confirm` | POST | api | 删契约（需人工确认） |
| `/api/v1/e2ee/transfer/cancel` | POST | api | 删契约（需人工确认） |
| `/api/v1/auth/assets` | GET+POST | api | 删契约（疑似 `attachment/*` 前身） |
| `/api/adm/attach/auth` | POST | adm | 删契约（疑似 `adm/storage/*` 前身） |

备注：`e2ee/social/*` 疑似旧"社交恢复分片"方案残留（现行注册的是 `/api/v1/e2ee/recovery/start` + `compliance_key` 线）；`e2ee/transfer/*` 无任何对应 handler。以上 16 条的契约修订需人工拍板后改 `api/paths/` 端点文件并重跑 `gen_aggregate.py`。

## 5. C3：方法不一致（未判定，局限说明）

- 路由层（cowboy）只有 path+handler+action，无方法；方法真源在各 handler 的 method 分派分支。
- 抽样验证：`organization_member_handler:handle_action(member, ...)` 仅允许 DELETE，与契约 `/api/v1/organizations/{organization_id}/members/{user_id}` 只有 DELETE 一致——即契约方法集并非必然滞后，需逐 handler 审计才能定论。
- 建议：后续专项按 handler 逐个提取 method 分派（可扩展 `scripts/check_rest_contract_coverage.sh` 或 `contract_gate.py`），本次不对 C3 下结论。

<!-- AUTO-GENERATED: END -->

## 6. 复核方式 / How to Reproduce

1. 路由清单：`imboy_router:get_routes/0` 运行时结果，或 `.contract/api_contract.json`（contract-export，含 main/adm/api_v1/api_internal/test_dev_only 五组 path+handler+action）。⚠️ 注意两者口径不等价：export 由 `contract_gate.py` 的 `extract_routes/1` 按**固定文本窗口**纯静态提取，不感知编译门状态——EB/CS 门控段被显式并入窗口（`scripts/contract_gate.py:162-180`），moment helper 则被刻意排除在窗口外（同文件 :156-160），故 moment 在 export 中**恒为 0 条、与编译门开闭无关**；复现本文数字须回 `src/imboy_router.erl` 源码字面口径。
2. 契约清单：解析三份 root 的 `paths` 段（path item 为 `$ref` 指向 `api/paths/**` 端点文件，需二级解析取方法）。
3. 现有门禁：`bash scripts/check_rest_contract_coverage.sh`（curated TSV 子集门禁，非全量对账）；本文为全量一次性对账，差异处置完成后建议把新补端点纳入该 TSV 以获得持续门禁。

## 相关文档 / Related

- 端点总目录：[../reference/rest-api-v1-catalog.md](../reference/rest-api-v1-catalog.md)
- 三端对齐：[three-platform-alignment.md](./three-platform-alignment.md)
- 契约生成与聚合：`api/README.md`、`api/gen_aggregate.py`
