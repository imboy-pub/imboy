# T02 契约冻结记录 / Contract freeze record

## 修订10增量候选 — DRAFT_NOT_ACCEPTED

当前用户批准豆包ASR2.0+GLM-5.3-Flash，权威增量为DOUBAO-API.md，原型为design-preview-doubao.html，profile fixture SHA256为`d723bf03c21bf27f6d35a38d6134c2cdf33ebe4a6700b35c8bd98ec27fc4cfab`（profile语义hash，不是fixture文件hash）。当前BE R7冻结manifest为`ce5c604fea8cd5129ea7be5b70279b026fa8fc6f96c2bba7a0c753d92bde848b`，App R8冻结manifest `app-doubao-recovery-r8-final-manifest.json` SHA256为`c45400a33c21b308c2eec59c528555b7e8930d5c86e4bd98055683a18622d2a9`，独立审查及候选范围另核；这些不是当前计划accepted证据。候选迁移222/223/224、OpenAPI和实际源码绑定由主控制器核对，224 up hash `1d93316541f90bb75e00149331d555f1977c163ebd78e9a072d142ce091783ff`、down hash `2dae588aa2c0eeeaafd9423d488861aecce5e66ab826d2150a37d9a62f1577ac`。

plan-manifest仅冻结本次文档候选内容；仓外STATE仍revision9，须审查和显式迁移。旧runner绑定、历史PASS/FAIL及下文20261020快照完全保留，不因更新hash提升状态。当前模型真实质量、供应商可达取音窗口、适用全量、真机与视觉验收不得由静态原型或合成回执替代。

## 原冻结历史（不追改哈希）

修订9的历史边界（2026-10-22）：以下为2026-10-20冻结快照，保留当时文件哈希、行号和迁移分配事实，不表示当前源码仍与快照相同。主仓已合并的面聊迁移实际为211—220；新增清理重试迁移221仍属当前任务候选，尚未合并或完整验收。最新API以本目录 BACKEND-API.md 及其修订9 manifest 为准，清理重试不在旧冻结快照内；须在最终候选契约复核时补验。下列视觉发现仍是实施必须满足的要求，不能用本次文档整理视为已关闭。

修订9采用已批准BYOK方案；本文件下列哈希仍是历史冻结快照，不代表新原型已完成视觉验收或新方案已实现。

日期：2026-10-20。执行 run：`20261020-193000-talkv1-193415`（授权来源：用户会话指令，原文与 RESUME-PROMPT.md 一致，sha256 `d2376873…c01a`）。
本文件是 PLAN.md T02 的产出：冻结原型版本、登记与当前源码约定的对齐/差异、固定实施约束。**本文件不修改 DESIGN.md / BACKEND-API.md / UI-SPEC.md 的既有内容**；实施以本文登记的冻结结论为准。

## 1. 冻结的原型与设计版本

| 文件 | sha256（冻结） |
|---|---|
| design-preview.html | `da1876b820c55a1f99c191c026bd11b3bb1b6cfe5adc81bc241ee461b92bddcc` |
| DESIGN.md | `19d6e8e5db02a400a6c0af519e3ed325f42bb7ac7a4856ab344fcf764fe01433` |
| UI-SPEC.md | `91fff2a0bc0cda9225eb77146d7990333bded9cfe8c9261948a8faa527072963` |
| BACKEND-API.md | `4cca8619c1fd22a863bd0beac5590226d10dfed59b78fda22cdbbab09f5492ba` |
| design-tokens.json | `ecf855261a1fdd50eed4d8247d146c8420e32b2a5f18b423c882f0fd8060c107` |

## 2. API 契约与源码约定的对齐结论（后端仓取证 2026-10-20）

已核对 `src/imboy_router.erl`、`src/lib/elib_response.erl`、`include/error_code.hrl`、`src/api/enterprise_internal_idempotency.erl`、`src/api/app_manifest_handler.erl`、`priv/migrations/`、`api/openapi-api.yaml`。结论：

1. **信封复用确认**：现有信封为 `{code, msg, sv_ts, payload}`（`elib_response.erl:214-252`，success code=0）。talk_record 全部端点沿用，不另建信封。BACKEND-API.md 中"外层沿用项目 envelope"成立。
2. **路由注册**：仅 `/api/v1/*`；新增 `/api/v1/talk-records` 及 `/api/v1/talk-record-cards`（如实施需要）按 `{Path, talk_record_handler, #{action => X}}` 三元组注册进 `imboy_router:get_routes/0`；与既有 `/api/v1/group/face2face`（位置建群，另一功能）无路径冲突。中间件链（auth/throttle/feature_gate）自动生效，handler 内不再重复鉴权传输层。
3. **OpenAPI 同步义务**：`api/openapi-api.yaml`（3.1.0，354 path）是机器真源，`.contract/api_contract.json` 为配套。每个新端点实施时必须同步登记 path+schema，缺 OpenAPI 条目视为 T05+ 验收不通过。
4. **分页差异（有意决策）**：现有惯例是 `page/size + total`（`elib_pg:page_with_total/6` 返回 `#{total,page,size,list}`）；BACKEND-API.md 为 talk_record 选择 **opaque cursor + limit(默认20,最大100)**。冻结：talk_record 域使用 cursor 分页（列表、segments、grants 候选等游标资源），不复用 page/size；两者在后端并存，客户端 T08 按域区分。
5. **幂等**：沿用 enterprise_internal_idempotency 的 digest 模式（SHA-256(method+path+canonical_json_body)），但 scope 按设计为 `(actor_id, method, resource, key)`，持久表 `talk_record_idempotency`（设计 §2）。错误码复用 `ERR_IDEMPOTENCY_CONFLICT=5460` / `ERR_IDEMPOTENCY_KEY_REQUIRED=5461`（`error_code.hrl:515-518`）。
6. **ETag/If-Match（新增能力，无业务先例）**：现有仅 GET+`If-None-Match` 304（`app_manifest_handler.erl`）与内部 TextGuard CAS。`If-Match`/428/412 乐观并发是 talk_record 首次引入的业务面模式——T05 起实现须在 elib_response 层补充 `428 Precondition Required` 与 `412 Precondition Failed` 的 status 映射；客户端 T08 须新增 If-Match 处理（现无先例，`app_manifest_service.dart` 仅 304）。
7. **错误码**：业务错误码实施时从 `include/error_code.hrl` 的 5xxx 分域分配器登记（建议新域 55xx=talk_record，实施时以当时文件实际空闲段为准并同步 `imboyapp/scripts/generate_error_code.dart` 再生成）。HTTP 语义映射按 BACKEND-API.md §4（401/403/404/409/412/413/415/422/428/429/503）。
8. **迁移编号**：当前最大 `00000208`（`priv/migrations/`，199 被跳过）。talk_record 首个迁移由主控制器持 migration-number 锁分配 **00000209** 起；up/down 成对、隔离测试库验证。
9. **i18n**：后端无请求级 i18n（错误消息中文硬编码于 ERROR_MSG_MAP）；客户端 slang（`assets/i18n/*.i18n.yaml` → `strings.g.dart`）。talk_record 文案走客户端 slang，后端仅错误码。
10. **四层架构**：`src/api/talk_record_handler.erl` → `src/logic/talk_record_logic.erl` → `src/ds/talk_record_ds.erl` → `src/repo/talk_record_repo.erl`（及子资源 repo），严格单向；测试对齐各层 CLAUDE.md 惯例。

## 3. App 客户端契约对齐（imboyapp 取证）

1. API 客户端：`HttpClient`（dio，`http_client.dart:99`）+ `IMBoyInterceptor`；talk_record 新建 `lib/store/api/talk_record_api.dart extends HttpClient`，URL 常量入 `lib/config/const.dart`。
2. 信封解析复用 `http_transformer.dart`（code/msg/payload + `_enrichPayload`）；`throwIfFailed` / `payloadAsMap` 等。
3. 大整数：统一 `EntityId`/`entityIdOf`（`workspace_model.dart:25-35`，等价 imboyadmin safeParseBigIntJson；禁 int.parse）。JSON integer 传输。
4. Repository：接口+实现分离（模块 domain 接口 + sqflite_sqlcipher 实现）；ViewModel 用 Riverpod codegen（`@riverpod`）。
5. 路由：`goRouterProvider` 分域 spread 模式；talk_record 页面新建 `config/router/routes/talk_record_routes.dart`，页面经 `pages_barrel.dart`；feature 门用 `RouteFeatureGuard`。
6. If-Match：客户端无先例，T08 新增（响应头捕获 `ETag`，写操作带 `If-Match`，412 → 冲突 UI 状态）。

## 4. 视觉冻结与实施约束（详见 run 证据 T02/visual-sweep-results.json）

原型 13 页 × {320/390/1280 明色, 320 暗色, 320 × 1.3em 复合字号} 扫描：无横向溢出、无文本裁切，除以下登记项；角色矩阵（owner/editor/viewer）与 UI-SPEC §3 一致。**以下为实施强制约束（T10/T16/T18/T21 施工时必须满足，原型缺陷不得复刻）**：

- **C-F1**：暗色选中态分段文字 #2474E5-on-#142947 = 3.27:1（<4.5）。施工时选中态分段控件对比须 ≥4.5:1（或文字≥18.66px 粗体）。
- **C-F2**：主按钮白字 on #2474E5 = 4.46:1。施工时主按钮文字对比须达 4.5:1（微调按钮底色/文字字号字重）。
- **C-F3**：记录管理页提示文字 nowrap 在字号放大后溢出 105px。施工时提示文字必须可换行，禁止单行截断。
- **C-F4**：播放器进度轨道视觉高 16px 可接受，但触达区须 ≥44pt。
- 其余既定约束（44/48pt、320 可达、键盘/安全区、文本可换行）按 UI-SPEC §1 冻结。

## 5. 决策门登记

- `target_platforms`：按 T03 base_readiness 取证后冻结（见 decisions 流程）；UI 侧已按 imboyapp 现有平台（iOS/Android）为准，无新增平台义务。
- `retention_and_export_policy`：V1 不硬编码天数（DESIGN §6 冻结：部署策略给出、保存前展示、UI 显示实际规则）；导出 txt/md 两格式冻结。

## 6. 验收对照

- `ui_contract`：§1-§4 + 角色矩阵 + 原型版本冻结 ✔（证据 visual-sweep-results.json + 2 截图 sha256）
- `api_contract`：§2/§3 源码逐条对齐 ✔（取证文件:行号在 Explore 报告，run 归档）
- `visual_layout`：320/390/宽屏/明暗/大字(1.3em 复合) 扫描完成，4 项发现已转为实施约束 ✔
