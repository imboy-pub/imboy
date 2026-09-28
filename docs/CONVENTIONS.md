# IMBoy 工程约定（CONVENTIONS）

> **真源（Source of Truth）**：本文件位于 `imboy/docs/CONVENTIONS.md`，由 imboy（API/契约真源仓）维护，三端共同遵守。
>
> **不可妥协（Non-Negotiable）**：以下 6 条规则适用于三端（Erlang 后端 / Flutter 客户端 / React 管理后台）。任何 PR 违反任一条 → 必须修复或显式标记 `convention-exception` 标签并在 PR 描述中说明充分理由。
>
> **版本**：v1.1 · **最后更新**：2026-09-19（对齐代码事实：错误码整数形态、WS action 命名、ID 进路径现状；v1.0 中与代码相反的三处已修正，详见各节）

---

## 1. ID 规则（IDs）

**规则**：所有跨端 ID 一律使用 **TSID（Time-Sorted Unique Identifier，64-bit）**。禁止新增 `BIGSERIAL` / 自增整数 / UUIDv4 作为业务主键。

**理由**：
- 时间有序 → 索引友好、分页稳定（迁移 2026-04-07 完成，全量替换 187 文件）
- 64-bit → JSON `number` 范围内（> 2^53 的值需 `string` 包装传输，详见下方「字段命名」章节）
- 单点生成无协调成本（雪花算法变种），分布式安全

**实现**：
- 后端（Erlang）：`elib_tsid:generate/1` 生成；DB 列类型 `BIGINT`（不是 `BIGSERIAL`）
- DB 迁移：所有主键改 `BIGINT` + 应用层填值
- 跨语言互操作：见「字段命名」章节的 string-encoded id 约定

**禁止**：
- 新增 `BIGSERIAL` / `SERIAL` / `IDENTITY` 列
- 业务主键使用 `UUID`（仅 idempotency-key 等场景例外）
- 把**公开不可猜测**能力建在 TSID 之上——TSID 允许出现在 URL 路径（现行 API 全量如此，如 `/:organization_id`），但对外分发、需要防枚举的标识必须另发专用公开标识/令牌（如 `channel.custom_id`、挂件 `public_widget_id`、邀请 token，且令牌只存摘要）

---

## 2. 时间规则（Timestamps）

**规则**：所有时间字段在传输与存储中统一为 **UTC，毫秒精度，ISO-8601 字符串**（带 `Z` 后缀）。

**理由**：
- 时区转换在客户端展示层处理，避免后端业务逻辑被时区污染
- 毫秒精度对齐 IM 消息排序需求
- ISO-8601 跨语言库支持成熟

**格式**：`YYYY-MM-DDTHH:mm:ss.sssZ`（合成示例：`2026-05-08T01:30:42.137Z`）

**字段命名**：
- 创建时间：`created_at`（数据库列）/ `createdAt`（JSON）
- 更新时间：`updated_at` / `updatedAt`
- 软删除：`deleted_at` / `deletedAt`（`null` 表示未删除）
- 业务时间：`{action}_at`（如 `published_at`、`expired_at`）

**实现**：
- 后端（Erlang）：`imboy_dt:utc_now/0` 返回 `binary()`；DB 列类型 `TIMESTAMPTZ DEFAULT NOW()`
- Flutter：`DateTime.parse(...).toUtc()` 解析；展示层用 `intl` 转本地时区
- React：`dayjs.utc(...).local()` 转换

**禁止**：
- 传输 Unix epoch 整数（除非协议层明确约定，如 v2 frame 协议头）
- 字段使用 `time` / `date` / `timestamp` 这类不带语义的命名
- 任何场景在 DB 用 `TIMESTAMP`（无时区）类型

---

## 3. 字段命名规则（Field Naming）

**规则**：按场景分层使用三套命名约定，互转由 codegen 与序列化层自动完成。

| 场景 | 约定 | 合成示例 |
|------|------|---------|
| 数据库列、Erlang record/map key | `snake_case`（atom 或 binary） | `created_at`、`user_id` |
| JSON 传输（HTTP / WS payload） | `camelCase` | `createdAt`、`userId` |
| Protobuf 字段 | `snake_case`（生成代码自动转 camelCase） | `created_at` |
| Erlang 模块 / 函数 | `snake_case` | `imboy_user`、`get_by_id/1` |
| Erlang atom 常量 | `snake_case` | `ok`、`not_found` |
| Dart / TS 类与类型 | `PascalCase` | `UserProfile`、`AuthToken` |
| Dart / TS 函数与变量 | `camelCase` | `fetchUser`、`isAuthorized` |
| React 组件 | `PascalCase` | `<UserCard />` |
| 常量 | `UPPER_SNAKE_CASE` | `MAX_FILE_SIZE` |

**ID 跨端编码**：
- 64-bit TSID 在 JSON 中作为 **`string`** 传输（避免 JS `number` 精度丢失）
- 字段名固定为 `id` / `xxxId`（如 `userId`、`messageId`）
- 后端反序列化时显式 `binary_to_integer/1` 解码

**禁止**：
- DB 列名用 `camelCase`
- JSON 字段用 `snake_case`（除非协议明确指定，如 OAuth2 `access_token`）
- 单字母变量（除循环索引 `I` / `N`）
- 缩写命名（`usr_id` ✗ → `user_id` ✓）

---

## 4. 错误响应规则（Error Response）

> v1.1 修正：v1.0 曾规定字符串错误码（如 `"USER_NOT_FOUND"`）并禁止整数——**与代码事实相反**。现行代码（`elib_response`、`include/error_code.hrl`、三端客户端解析）自始至终使用**整数错误码**。本节以代码为准重写。

**规则**：所有 HTTP / WS 响应使用统一信封，由整数 `code` 驱动客户端错误处理逻辑；错误码定义的唯一真源是 `include/error_code.hrl`（详见 [错误码使用规范](./reference/error-codes.md)）。

**响应信封**（实现：`elib_response:reply_json/4,5`）：

```json
{
  "code": 0,
  "msg": "success",
  "payload": { }
}
```

- `code`：整数。`0` 成功；`1` 通用错误；`4xx/5xx/9xx` 区间语义参考 HTTP（客户端错误/服务端错误/IM 业务专用）。
- `msg`：人类可读消息（后端中文须 `/utf8`）；客户端**绝不**基于 `msg` 文本做分支判断。
- `payload`：业务数据；管理面响应另有 `sv_ts`（服务器时间戳）。
- HTTP 状态码：主流模式为 **HTTP 200 + 信封 code 表达业务结果**（如计费类端点无权时 HTTP 200 + code=403）；少数端点使用真实 HTTP 状态码（如 WS 握手 401）。**每个端点的具体语义以 `api/openapi.yaml` 与契约产物为准，不凭状态码猜。**

**错误码维护**：

- 新错误码加入 `include/error_code.hrl` 宏定义，并同步 [错误码使用规范](./reference/error-codes.md)。
- 客户端生成的错误码常量由脚本从 `error_code.hrl` 派生（产物勿手改）。

**禁止**：

- 客户端解析 `msg` 字符串做逻辑分支
- 把 stacktrace / SQL 错误透传到客户端（`payload` 需脱敏）
- 未经契约评审新增与既有区间冲突的错误码

---

## 5. 分页规则（Pagination）

**规则**：所有列表 API 默认使用 **cursor-based** 分页；`offset/limit` 仅用于管理后台的内部查询且必须有 `WHERE` 索引覆盖。

**Cursor 分页请求**（合成示例）：
```
GET /api/v1/messages?cursorBefore=123456789012345678&limit=50
GET /api/v1/messages?cursorAfter=123456789012345678&limit=50
```

**Cursor 分页响应**（合成示例）：
```json
{
  "data": [],
  "pageInfo": {
    "hasMore": true,
    "nextCursor": "123456789012345678",
    "prevCursor": "987654321098765432"
  }
}
```

**约束**：
- `limit` 默认 20，最大 100（超过 → 400 错误，`code=PAGE_LIMIT_EXCEEDED`）
- Cursor 值即 TSID（时间有序），客户端不做语义解析
- 服务端 SQL：`WHERE id < {cursor} ORDER BY id DESC LIMIT {limit + 1}`（多取 1 条判断 `hasMore`）
- 不返回 `total`（高基数列表禁止 `COUNT(*)`，必要时给「估算 total」专用接口）

**Offset 分页（仅限管理后台）**：
- 必须配合 `WHERE` 过滤条件使用，禁止全表 offset
- 字段：`page`（1-based）、`pageSize`（默认 20，最大 200）
- 响应：`{ data, pageInfo: { page, pageSize, total } }`，`total` 来自带索引的 `COUNT`

**禁止**：
- 客户端 API 用 offset/limit 分页（性能不可预测，深翻页 OOM 风险）
- 列表接口不返回 cursor（无法翻页）
- 分页参数从 query string 之外的位置读取（如 header）

---

## 6. URL / 模块 / 端点命名规则（Naming）

**规则**：HTTP 端点使用 RESTful 资源命名 + 显式版本前缀；Erlang 模块按功能域分组；WS action 按 `domain.verb` 命名。

**HTTP 端点**：
- 版本前缀：`/api/v{N}/`（当前 `v1`，破坏性变更升 `v2`）
- 资源用复数名词：`/api/v1/users`、`/api/v1/messages`
- 嵌套不超过 2 层：`/api/v1/users/{id}/contacts`（✓）；`/api/v1/users/{id}/contacts/{cid}/messages`（✗，应拆出独立资源）
- 动作用 RPC-style 子路径：`/api/v1/users/{id}/actions/block`（✓，比 `PATCH /users/{id}` 加 body 字段更显式）
- 路径段用 `kebab-case`（如 `/api/v1/qr-login`）

**HTTP 方法**：
| 方法 | 语义 |
|------|------|
| GET | 查询，幂等，无副作用 |
| POST | 创建 / 触发动作（非幂等） |
| PUT | 整体替换（幂等） |
| PATCH | 部分更新（需 `If-Match` ETag 防并发） |
| DELETE | 删除（幂等） |

**WS Action 命名**（v1.1 修正：现行 action 为 snake_case，不是点分式）：
- 格式：`{domain}_{verb}` 小写下划线（如 `message_revoke`、`e2ee_room_key`、`group_member_join`）
- 注册表：`src/lib/imboy_ws_action_registry.erl`（ETS，插件可扩展）；未注册 action 回 `unknown_action`
- 客户端常量表：imboyapp `lib/service/message_type_constants.dart`（S2C 侧）
- 新增 action 必须双侧同步登记，并更新 [WebSocket 协议](./reference/ws-protocol-contract.md)

**Erlang 模块命名**：
- 业务域前缀 `imboy_`（如 `imboy_user`、`imboy_msg_router`）
- 工具模块前缀 `elib_`（如 `elib_tsid`、`elib_pg`、`elib_str`）
- 一个模块单一职责，超 800 行 → 拆分（`{domain}_query` / `{domain}_command` / `{domain}_handler`）
- 行为模块（gen_server / gen_statem）后缀加 `_srv` / `_sm`（如 `imboy_user_srv`）

**禁止**：
- 端点路径混用 `_` 与 `-`（统一 kebab-case）
- 缩写域名（`/api/v1/usr` ✗ → `/api/v1/users` ✓）
- WS action 用驼峰（`msgSend` ✗ → `message_send` ✓）或点分式（`msg.send` ✗）
- Erlang 模块名带连字符（atom 语法不支持）

---

## 附录：例外申请流程

如个别场景必须违反某条规则，PR 必须：

1. 在描述中声明「**Convention Exception: §X.Y**」并附理由
2. 添加 GitHub label `convention-exception`
3. 由 2 名 maintainer 评审通过方可合并
4. 在例外台账追加一行记录（PR # / 时间 / 例外条款 / 范围）。台账文件尚未建立（原引用 `docs/CONVENTIONS_EXCEPTIONS.md` 不存在）；建立前以 PR 描述为准。

未走例外流程的违反 → 强制阻塞合并（`quality.yml` GHA + `code-reviewer` agent 双重检查）。

---

## 工具链联动

- **lefthook pre-commit**：`commitlint` + `gitleaks` + 各端 formatter
- **lefthook pre-push**：推送前质量门（Gradualizer ratchet 等）。⚠️ 生效前提是分支 upstream 可被 `HEAD @{push}` 解析——新 clone 后必须执行 `git config remote.pushDefault gitcode && git branch --set-upstream-to=gitcode/main main`，否则 lefthook 会以 "no matching push files" **静默跳过全部门禁**（2026-09-29 三仓实证修复，详见各仓 lefthook.yml 注释）
- **CI（quality.yml）**：本约定的自动化检查由 `oasdiff` / `eslint` / `elvis` / `dart_code_metrics` 落地
- **codegen**：`imboy/api/codegen/` 生成的客户端代码自动符合「字段命名」§3 与「错误响应」§4
- **codemap**：变更后由 `doc-updater` agent 同步至 `src/*/CLAUDE.md` 模块文档（旧 `docs/archive/CODEMAPS/` 因数据失真已于 2026-07-25 删除）

---

> **最后**：本文件不是一次性产物。每次发现新约定空白 → PR 修订本文件并通过例外流程评审。
