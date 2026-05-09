# `imboy/api/` — IMBoy 三端 API 契约真源（机器可处理）

> **关联**：`.claude/plans/quality-loop.md` v1.3 T3.1 ~ T3.6 (PHASE_W3_CONTRACT)
> **角色**：本目录是 **API 与协议契约的代码生成驱动真源（Source of Truth）**
> **创建**：2026-05-09 / iteration 65 / T3.1 落地

---

## 真源原则（v1.1 决策 B / 2026-05-08）

- imboy = 服务端 + API 契约真源仓
- imboyapp（Flutter）/ imboy-admin-frontend（React）通过 **codegen 脚本**从本目录拉取并生成代码
- 修改 API 契约 **只能改本目录**；客户端代码从此再生
- 严禁手写 DTO 与 service 直连后端 — 必须经 codegen

---

## 目录结构（T3.1 骨架）

```
imboy/api/
├── README.md           # 本文件（总览）
├── openapi.yaml        # T3.2 产出 — HTTP REST API（OpenAPI 3.1）
├── proto/              # T3.3 产出 — 实时通讯 Protobuf
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
| `openapi.yaml` | HTTP REST | 后端实现校验 + 前端 axios client + 文档 | Redocly + openapi-generator |
| `proto/*.proto` | WebSocket（imboy.v2 frame）| Erlang gpb / Dart protoc / TypeScript ts-proto | protoc + plugins |
| `codegen/*.sh` | 调用 | Makefile / npm scripts / pubspec 依赖 | shell + 各 plugin |
| `redocly.yaml` | OpenAPI 静态文档 | docs.imboy.com / GitHub Pages | redocly cli |

---

## 与 `imboy/doc/api/`（人类可读契约）的关系

**`imboy/doc/api/`**（已存在 10+ 文件）：
- 含 **手写设计文档**：`websocket-api-2.md` / `channel_api_contract_v1.md` / `moment_api_contract_v1.md` / `e2ee_server_persisted_shard_contract_v1.md` / `tsid-field-convention.md` / `tsid-field-matrix.md` / `rest-api.md`
- 含 **历史 yaml**：`openapi.yaml`（340 行）/ `asyncapi.yaml` / `envelope.schema.json`
- 角色：**人类可读的设计与决策记录**（叙述性、含背景与权衡）

**`imboy/api/`**（本目录，T3.1 起新建）：
- 含 **机器可处理的契约**：`openapi.yaml`（codegen 输入）/ `proto/*.proto` / `redocly.yaml`
- 角色：**代码生成驱动真源**（结构化、由工具消费）

**T3.2 / T3.3 迁移策略**：
- T3.2 把 `doc/api/openapi.yaml`（340 行手写）改写到 `api/openapi.yaml` 作为代码生成真源；旧位置降级为参考链接
- T3.3 把 `imboy/proto/imboy.proto` + `imboy/src/imboy.proto`（已存在 360 行 v2 frame proto）拆分整理至 `api/proto/`
- 手写设计文档（websocket-api-2.md 等）**保留** 在 `doc/api/`，与本目录并存

---

## 关联约定

- **CONVENTIONS**：`imboy/docs/CONVENTIONS.md`（v1.3 T1.3 落地的 6 条不可妥协规则）
- **TSID**：所有 ID 字段在 OpenAPI 用 `string`（避免 JS Number 精度丢失），见 `imboy/docs/CONVENTIONS.md` §1
- **错误响应**：统一信封 `{code, message, details, traceId}`，见 §4
- **分页**：cursor-based 默认，offset 仅限管理后台，见 §5
- **TSID 字段映射**：参考 `imboy/doc/api/tsid-field-matrix.md`（哪些字段是 TSID）

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
> 设计层文档可继续写到 `imboy/doc/api/`。
