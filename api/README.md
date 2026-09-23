# `imboy/api/` — IMBoy 三端 API 契约真源（机器可处理）

> **关联**：三端工程约定 [`docs/CONVENTIONS.md`](../docs/CONVENTIONS.md)（历史迭代 quality-loop v1.3 T3.1~T3.6，计划文件已不存在）
> **角色**：本目录是 **API 与协议契约的代码生成驱动真源（Source of Truth）**
> **创建**：2026-05-09 / iteration 65 / T3.1 落地

---

## 真源原则（v1.1 决策 B / 2026-05-08）

- imboy = 服务端 + API 契约真源仓
- imboyapp（Flutter）/ imboy-admin-frontend（React）通过 **codegen 脚本**从本目录拉取并生成代码
- 修改 API 契约 **只能改本目录**；客户端代码从此再生
- 严禁手写 DTO 与 service 直连后端 — 必须经 codegen

---

## 目录结构（三受众布局，2026-09-22 重组）

```
imboy/api/
├── README.md           # 本文件（总览）
├── openapi-api.yaml        # 编辑真源 — surface api（业务 v1 端点桥接）
├── openapi-adm.yaml        # 编辑真源 — surface adm（管理后台端点桥接）
├── openapi-internal.yaml   # 编辑真源 — surface internal（企业集成 v1，见 internal/v1/）
├── openapi.yaml        # 生成式聚合桥接（gen_aggregate.py 产出，勿手改）
├── gen_aggregate.py    # 聚合生成/校验脚本（--check 供门禁用）
├── asyncapi.yaml       # D-cleanup 产出 — WebSocket / 实时通讯（AsyncAPI 3.0，机器消费真源）
├── paths/              # OpenAPI path item 文件，三级布局：
│   ├── api/v1/<domain>/<action>.yaml      # surface api（业务端点）
│   ├── adm/v1/<domain>/<action>.yaml      # surface adm（原 adm-* 目录去前缀）
│   └── internal/v1/<domain>/<action>.yaml # surface internal（企业集成）
├── components/         # 共享 schemas / parameters（三 surface 通用）
├── internal/v1/        # internal surface 人类集成文档（README/endpoints/Postman）
├── proto/              # T3.3 产出 — 实时通讯 Protobuf（asyncapi.yaml 的字节级真源）
│   ├── imboy_v2_frame.proto    # WS 顶层封装帧
│   ├── imboy_s2c.proto         # Server→Client action payload
│   └── ...
├── codegen/            # T3.4 产出 — 代码生成脚本
│   ├── erlang.sh       # imboy 自身（gpb plugin via erlang.mk）
│   ├── dart.sh         # imboyapp（protoc + dart-protoc-plugin）
│   └── typescript.sh   # imboy-admin-frontend（protoc + ts-proto）
└── redocly.yaml        # T3.5 产出 — OpenAPI 渲染配置
```

---

## 工件用途速查

| 工件 | 协议 | 消费方 | 工具链 |
|------|------|-------|--------|
| `openapi-{api,adm,internal}.yaml` | HTTP REST（三受众编辑真源） | 后端实现校验 + 前端 axios client + 文档 | Redocly + openapi-generator |
| `openapi.yaml` | 三 root 聚合并集（GENERATED） | 契约校验脚本 / 单文件导入方 | `python3 api/gen_aggregate.py` |
| `asyncapi.yaml` | WebSocket / 实时事件 | 客户端 WS 消息分发 codegen + 文档 | AsyncAPI Generator + Studio |
| `proto/*.proto` | WebSocket（imboy.v2 frame）| Erlang gpb / Dart protoc / TypeScript ts-proto | protoc + plugins |
| `codegen/*.sh` | 调用 | Makefile / npm scripts / pubspec 依赖 | shell + 各 plugin |
| `redocly.yaml` | OpenAPI lint/渲染 | CI + docs.imboy.com / GitHub Pages | redocly cli |

---

## 三受众布局与映射规则（2026-09-22 重组）

**surface 三值**（按受众划分，不按 URL 前缀机械切）：

- `api` — 业务端点（移动端/Web 客户端消费；含无 `/v1` 前缀的旧路径如 `/user_tag/change_name`）
- `adm` — 管理后台端点（URL 多为 `/api/adm/...`；目录自旧 `adm-*` 前缀目录去前缀而来）
- `internal` — 企业集成端点（URL `/api/internal/v1/...`；`ib_int_*` 凭证认证）

**目录版本段 `v1` 是契约定位**，不强制等于 URL 前缀：`paths/api/v1/user/show.yaml`
对应 URL `/api/v1/user/show`，但 `paths/api/v1/user_tag/change_name.yaml` 对应
URL `/api/user_tag/change_name`（历史无 v1 前缀路径，归 surface api）。

**修改流程**：改对应 surface 的 root（或其桥接的 `paths/<surface>/v1/...` 端点文件）
→ `redocly lint` 三 root → `python3 api/gen_aggregate.py` 重新生成聚合文件并提交。
`openapi.yaml`（聚合桥接）头部标 GENERATED，手改会被下次生成覆盖。

**internal 双层结构**：`api/internal/v1/` 是人类集成文档
（README / endpoints / Postman collection），`api/paths/internal/v1/` 是机器契约
（OpenAPI path item），两者互为指路、内容同源（handler 实证）。

**旧路径 → 新路径映射**：一次性迁移映射表存档于
`.Codex/runs/api-surface-reorg-20260922T182750/migration-map.tsv`（528 文件）。
git 历史可通过 rename 追溯。

---

## 与 `imboy/docs/api/`（人类可读契约）的关系

**`imboy/docs/api/`**（已存在 10+ 文件）：
- 含 **手写设计文档**：`websocket-api-2.md` / `channel-api-contract-v1.md` / `moment-api-contract-v1.md` / `e2ee-server-persisted-shard-contract-v1.md` / `tsid-field-convention.md` / `tsid-field-matrix.md` / `rest-api.md`
- 含 **历史 yaml**：`openapi.yaml`（340 行）/ `asyncapi.yaml` / `envelope.schema.json`
- 角色：**人类可读的设计与决策记录**（叙述性、含背景与权衡）

**`imboy/api/`**（本目录，T3.1 起新建）：
- 含 **机器可处理的契约**：`openapi.yaml`（codegen 输入）/ `proto/*.proto` / `redocly.yaml`
- 角色：**代码生成驱动真源**（结构化、由工具消费）

**T3.2 / T3.3 迁移策略**：
- T3.2 把 `docs/api/openapi.yaml`（340 行手写）改写到 `api/openapi.yaml` 作为代码生成真源；旧位置降级为参考链接
- T3.3 把 `imboy/proto/imboy.proto` + `imboy/src/imboy.proto`（已存在 360 行 v2 frame proto）拆分整理至 `api/proto/`
- 手写设计文档（websocket-api-2.md 等）**保留** 在 `docs/api/`，与本目录并存

---

## 关联约定

- **CONVENTIONS**：`imboy/docs/CONVENTIONS.md`（v1.3 T1.3 落地的 6 条不可妥协规则）
- **TSID**：所有 ID 字段在 OpenAPI 用 `string`（避免 JS Number 精度丢失），见 `imboy/docs/CONVENTIONS.md` §1
- **错误响应**：统一信封 `{code, message, details, traceId}`，见 §4
- **分页**：cursor-based 默认，offset 仅限管理后台，见 §5
- **TSID 字段映射**：参考 `imboy/docs/api/tsid-field-matrix.md`（哪些字段是 TSID）

---

## 任务追踪

| 任务 | 产出 | 状态 |
|------|------|------|
| T3.1 | 目录骨架 + 本 README | ✅ 2026-05-09 |
| T3.2 | `openapi.yaml`（5 高频端点）| ⏳ 待执行 |
| T3.3 | `proto/*.proto`（整理 v2 frame）| ⏳ 待执行 |
| T3.4 | `codegen/{erlang,dart,typescript}.sh` | ⏳ 待执行 |
| T3.5 | `redocly.yaml` + `docs/api.html` | ⏳ 待执行 |
| T3.6 | 三端各替换 1 个端点 DTO 为生成代码 | ⏳ 待执行 |

---

> **警告**：禁止在本目录之外写新的 API/协议结构定义。任何 PR 添加新端点 → 必须先改 `openapi.yaml` 或 `proto/*.proto`，CI 校验通过后才允许实现。
> 设计层文档可继续写到 `imboy/docs/api/`。

---

## 已排除路由 / Explicitly excluded routes

下列路由在 `src/imboy_router.erl` 中存在，但**有意不挂载**到 `openapi.yaml`：

| 路由 | Handler | 排除原因 / Exclusion rationale |
|------|---------|--------------------------------|
| `/account-deletion` | `index_handler` (HTML) | 合规需要的账号注销页，HTML 响应而非 JSON API |
| `/privacy-policy` | `index_handler` (HTML) | 隐私政策页，HTML 响应 |
| `/static/[...]` | `cowboy_static` | 静态资源中间件，不属于业务契约 |
| `/static/admin/[...]` | `cowboy_static` | 管理后台前端 SPA 资源 |
| `/test/req_get` | `test_handler` | 开发调试端点，不暴露给客户端 codegen |
| `/test/req_post` | `test_handler` | 同上 |

如未来需要把这些挂入 OpenAPI（例如静态资源需 SDK 化、或测试端点需契约化），可参考 `paths/api/v1/system/help.yaml` 的 `text/html` content type 模式。

---

## 历史契约缺陷（已于 D-cleanup phase 27 修复）

phase 1 从 `docs/api/openapi.yaml`（340 行历史冻结契约，2026-04-15 之前的手写版）迁移时带入的虚构端点，**router 中不存在**，已在 D-cleanup 中删除：

| 已删除契约路径 | router 实际路径 | 替代方案 |
|---|---|---|
| ~~`/passport/refresh`~~ | `/refreshtoken` + `/v1/refreshtoken` | 使用 `paths/api/v1/auth/refreshtoken.yaml`（D-extras） |
| ~~`/user/current`~~ | 不存在 | 客户端从 JWT 解出自身 uid 后调 `/user/show?id=...` |
| ~~`/user/{uid}`~~ | 不存在 | 使用 `/user/show?id=...`（`paths/api/v1/user/show.yaml`） |

注：删除属破坏性 path 变更（oasdiff 会报 ERR），但本质上是 contract bug fix —— 这些 path 在后端不存在，client SDK 调用必然 404。删除前若有 client 已按 phase 1 契约生成代码并实际调用过这些 path，应在收到 404 时回退到上表"替代方案"。

---

## 路由覆盖统计 (截至 D-cleanup 2026-05-17)

- Router 路径总数：**512**
- OpenAPI 已挂载：**401**（D-extras 404 - 删除 3 个虚构）
- V1 段重复路径（已等价覆盖）：**107**
- 业务路径覆盖率：**100%**（506 真实业务 path 全覆盖；6 个有意排除）

历史 commit 系列：见 `changelog.md` "T3.2 split phase 1–27"。
