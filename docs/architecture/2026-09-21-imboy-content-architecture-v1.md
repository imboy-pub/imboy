# IMBoy Content Architecture V1.1 — Deferred Architecture Proposal

> 基于对 AppFlowy / AFFiNE / Docmost / Outline 四个开源项目的源码级取证，与 IMBoy 后端 / Flutter / Admin 三端代码及现有架构文档的完整对齐，设计的 IMBoy 内容（文档/知识）能力 V1 架构。

- **Status**: **DEFERRED / NOT READY FOR IMPLEMENTATION**（保留为研究与方向决策材料；不得据此直接开工）
- **Date**: 2026-09-21（v1.0 初稿 + v1.1 评审修订，同日）
- **性质**: 延期架构提案。本轮**未修改任何业务代码、未执行任何数据库迁移、未改变任何现有 API**。
- **方法**: Evidence → Model → Decision → Contract → Scope。所有关键结论可追溯至源码文件、迁移文件、官方 LICENSE 或仓内架构文档（见 §30 证据索引）。

> **延期声明**：本文以下 Architecture Decisions、Domain Contract 与 Acceptance Matrix 均为研究时点的候选设计，当前不具备冻结效力。恢复实施前必须基于届时 `imboy`、`imboyapp`、`imboyadmin` 的实际 HEAD 重新取证、修订并评审；本文中的路径、迁移号、API、权限入口、附件机制和验收命令均可能过期。

> **修订历史**：v1.0（外部评审 8.7/10 CONDITIONAL APPROVAL）→ v1.1 吸收当时 8 项修订；后续代码复核发现仍有实施阻断项，现降级为延期提案。历史评审结果不代表当前实现就绪。

## Deferred Handling（当前有效状态）

本文保留竞品取证、产品边界与技术方向，不继续投入实施级细化。恢复实施时不得直接沿用本文的 DDL、API 或验收矩阵，必须先完成重新基线。

**研究基线（仅代表 2026-09-21 取证时点）**：

| 仓库 | HEAD |
|------|------|
| `imboy` | `9f5a5fc68452716c32becf6b4c2ecbe3c5b9c701` |
| `imboyapp` | `60bc4925a03f9ca97d1436e35fe4e5f6b0693325` |
| `imboyadmin` | `553790dc9b6e6def0c331637d7f2619f34bc2170` |

**Known Blockers（重新立项前必须关闭）**：

1. Content 的 Feature / Product 归属及 `organization_id` 显式贯穿尚未按 ADR-0007 闭合；
2. fractional indexing 缺少数据库唯一性、固定 collation 与并发防环契约；
3. archived Workspace 的事务内写守卫尚未进入 Content 设计；
4. attachment scope CHECK、孤儿 GC 与 `object_key` 渲染假设和当前代码不一致；
5. CAS 尚未覆盖删除/恢复，409 后的安全合并或人工冲突处理未定义；
6. 非成员 403/404、E2EE `doc_card`、软删保留期与 FTS 删除可见性尚未统一。

**重新启动条件**：Content 进入正式路线图后，先重新确认三仓 HEAD、已采纳 ADR、数据库迁移序列、附件/权限/WS 当前实现，再产出新的架构评审结论与 Acceptance Matrix；只有用户再次明确批准，状态才可改为 `ARCHITECTURE_ACCEPTED / READY_FOR_IMPLEMENTATION`。

## Decision Summary（12 个研究阶段结论，非冻结）

| # | 问题 | 结论 |
|---|------|------|
| 1 | IMBoy 是否应该增加 Content？ | **YES**——定位为「工作区知识层」（Workspace Docs），非消息替代品 |
| 2 | Content 是否一等业务对象？ | **YES**——从身份/生命周期/权限/持久化/引用/搜索/审计/API 八维论证（§5.4） |
| 3 | 属于 Organization 还是 Workspace？ | **Workspace**（组织经 workspace 传导；工作区是既有权限边界） |
| 4 | Project / Channel / Group 与 Content 的关系？ | V1 仅做「文档卡片分享进聊天」；Project 集成 / Channel 收敛 / Group 全部 DEFER（§21） |
| 5 | Page 是否一等对象？ | **YES**——`doc_page` 单实体（Page 即 Document，一页一行） |
| 6 | Block 是否独立持久化？ | **NO**——四竞品无一做 block 行存储；V1 整页 Markdown 源文存储 |
| 7 | V1 是否需要 Database（Notion 式）？ | **NO**（选项 A：完全不做，且不预留表结构） |
| 8 | V1 是否需要 CRDT？ | **NO / DEFER**——单人编辑 + 版本 CAS + WS 变更通知；V2 迁移路径明确（§16.4） |
| 9 | V1 是否需要实时多人编辑？ | **NO**——仅服务端主动推送 doc_updated 通知（复用现有 WS 基建） |
| 10 | Flutter Editor 路线？ | **B：Markdown 源文编辑**（升级现有 channel compose 链路）；非 WebView+Tiptap、非自研 Block |
| 11 | V1 做到什么程度？ | 工作区树状文档 + Markdown 编辑/渲染 + PG FTS 搜索 + 附件全复用 + 软删除回收 + WS 通知 |
| 12 | 明确延期什么？ | CRDT/多人协作/Block 模型/Notion Database/白板/AI 写作/页面 ACL/公开分享/版本历史/评论/收藏/导入导出/离线/Admin 面/无条件覆盖保存（§26） |

> 候选验收矩阵见 **§32 Acceptance Matrix**。该矩阵当前不生效；重新立项后须先按届时代码重写并批准，方可用于 Content V1 完成判定。产品定位名暂记为 **Content V1 — Workspace Document MVP**。

---

## 1. Problem Definition

IMBoy 是开箱即用的即时通讯平台（C2C/C2G 消息、WebSocket 长连接、E2EE、Garage S3 附件直传）。当前不存在任何统一的「长文内容」能力，但存在四个烟囱式内容形态和一处 AI 预留位：

[EVIDENCE E-01]
- Source: imboy 后端仓
- Location: `priv/migrations/00000003_channel.up.sql:421`（channel_message.content text + payload jsonb）、`priv/migrations/00000001_foundation.up.sql:4790`（group_notice.body text）、`priv/migrations/00000004_social.up.sql:10`（moment_post.content text）、`priv/migrations/00000078_project_foundation.up.sql:30`（project.description text）
- Observation: 四类「正文」各自独立建表、独立 API、独立编辑 UI，无共享内容基座；`src/` 与 `priv/migrations/` 全文扫描 content/document/page/article/post/wiki/knowledge/note/block/editor/rich_text，**不存在任何 `doc_*`、`page_*`、`article_*`、`wiki_*` 业务表或模块**（"block" 仅命中 user_denylist 黑名单，"page" 仅分页参数）。
- Implication: Content V1 是**从零起建的新业务域**，无遗留迁移负担；同时意味着四个既有形态的收敛**不是** V1 目标（明确 DEFER，避免范围爆炸）。

[EVIDENCE E-02]
- Source: imboy docs
- Location: `docs/adr/2026-08-08-ai-agent-role-inheritance.md` 决策 5：「**独立知识库表留作后续扩展**」
- Observation: AI Agent 知识库目前是配置文本注入（`src/logic/ai_agent_kb_logic.erl`），无文档实体；roadmap（8 个文件全扫）与 product/packaging-contract 中均无 content/wiki 规划。
- Implication: Content 是全新业务域，无既有产品规划冲突；AI 知识库是 V2/V3 的潜在消费方而非约束方。

本架构要回答的核心问题：**在不破坏 IMBoy 现有 Organization / Workspace / Project / Group / Channel / Attachment / Permission 架构的前提下，Content 是否应该成为下一层核心能力；如果是，V1 的最小、稳定、可演进领域模型是什么。**

## 2. Product Boundary

**Content V1 是**：工作区（Workspace）内的树状长文文档能力——团队公告沉淀、项目说明、内部知识页，形态为 Markdown 长文，挂在 workspace_shell 之下。

**Content V1 不是**：
- 不是消息的替代品（消息时间线模型 `conv_seq` 权威、30 天热存 + 归档，与文档生命周期完全不同，`docs/concepts/messaging-model.md` Constraints）；
- 不是频道帖/群公告/动态的重写（四个烟囱保持原样，收敛是 V2+ 独立提案）；
- 不是 E2EE 产品（V1 文档对服务端可见；E2EE 文档是明确的 Non-Goal，§26）；
- 不是 Notion 克隆（无 Database/白板/公式，§26）。

## 3. Research Scope

**外部四项目（源码级，浅克隆于取证时点）**：

| 项目 | 取证 HEAD | 日期 | 服务端形态 |
|------|-----------|------|-----------|
| AppFlowy | `5cf3a36` | 2026-06-26 | 独立仓 AppFlowy-Cloud（本次未克隆，客户端仓取证，服务端部分标 EVIDENCE_GAP） |
| AFFiNE | `d897bb3` | 2026-09-20 | 同仓 `packages/backend/server`（NestJS + Prisma + Rust native） |
| Docmost | `7bef7b1` | 2026-09-20 | 同仓 `apps/server`（NestJS + Kysely） |
| Outline | `c3d2ae5`（1.10.1） | 2026-09-20 | 同仓 `server/`（Node + Sequelize） |

**IMBoy 三端**：`imboy/`（Erlang/OTP 后端，268 个迁移文件，最高迁移号 00000135）、`imboyapp/`（Flutter，android/ios/macos/web 四端）、`imboyadmin/`（React 19 管理后台）。

**证据规则**：外部结论以克隆源码文件为准；IMBoy 结论以代码 + 迁移文件 + 已采纳 ADR 为准；文档与代码冲突时**以代码 + 最新有效契约为准并记录冲突**（§5.5）。

## 4. External Architecture Evidence（四项目取证矩阵）

### 4.1 能力借鉴矩阵

| Capability | AppFlowy | AFFiNE | Docmost | Outline | IMBoy V1 决策 |
|-------------|----------|--------|---------|---------|---------------|
| Workspace | 客户端实体（SQLite `user_workspace_table`） | **服务端实体**（Prisma `workspaces`） | 实例多 workspace（`workspaces` 表） | Team = workspace（`teams`） | 复用现有 `workspace` 表（迁移 76） |
| Page | View（元数据树）与 Document（正文）分离、共享同一 ID | Doc = 一个 Y.Doc；服务端仅投影元数据（`workspace_pages`） | `pages` 表一行一页（Tiptap JSON + ydoc 双存） | `documents` 表一行一页（三列并存） | **`doc_page` 一行一页**（Docmost/Outline 形态） |
| Block | block map 存于 Yjs 文档内（nanoid(10) 稳定 ID + children 数组序） | block=Y.Map(sys:id/flavour/children)，**无 parent 字段** | Tiptap 节点树（存在 content jsonb 内） | ProseMirror 节点树（存在 content jsonb 内） | **不做 Block 持久化**（四家均无 block 行表） |
| Rich Text | 自研 appflowy_editor（Node 树 ↔ block map） | BlockSuite block tree | Tiptap JSON（协作真源是 ydoc） | ProseMirror JSON + Markdown 交换格式 | **Markdown 源文**（CommonMark 子集） |
| Database | 一 Database 一 collab + **每行一独立 collab**（复杂度之源） | database block（存于 doc 内） | 无（有 bases 雏形迁移） | 无 | **完全不做**（选项 A） |
| Collaboration | Yjs(yrs 0.21.3) + WS，60s 无响应重连 | Yjs + socket.io + y-protocols awareness | Yjs + **@hocuspocus/server**（debounce 10s/45s） | Yjs + Hocuspocus + Redis 扩展 + y-indexeddb | **V1 不做**；REST + CAS + WS 通知 |
| Version | collab_snapshot 表（BLOB） | SnapshotHistory（压实时自动存历史，按套餐保留） | page_history（BullMQ 异步去重快照，5min/60s 双档） | revisions（编辑会话 done 时落库，相同则跳过） | V1 不做；V2 采 Docmost 式去重快照 |
| Permission | workspace 角色 + 页面共享 access_level(10/20/30/50) + space 公私 + publish | workspace 4 角色 + doc 6 角色 + DocGrant ACL + **permission generation 失效** | workspace 3 角色 + space 3 角色(open/private) + page ACL(restricted 覆盖) + shares | team 4 角色 + collection permission + membership sourceId 继承链 | **复用 workspace 三角色**（owner/member/guest），无页面级 ACL |
| Search | 本地 Tantivy + 云端搜索回退 | Rust memory-indexer（jieba-rs 分词） | **PG tsvector + unaccent + pg_trgm**（title A/text B 权重） | PG tsvector 插件化（title A/text B-D） | **PG FTS + pg_jieba**，沿用仓内 FTS Projection Pattern |
| Attachment | S3 multipart（5MB 分片）+ 断点续传表 | Blob 元数据表 + 预约制上传 + 引用保护 GC | attachment 表（local/S3/Azure 驱动抽象）+ page_id 软关联 | attachments 表 + presigned PUT + expiresAt | **全量复用 attachment + Garage presign**，仅扩 scope |
| Reference | — | — | backlinks 迁移存在 | relationships 物化表（BacklinksProcessor 异步写） | V1 不做；V2 采 Outline 物化表模式 |
| Wiki 结构 | Folder collab 内 View 树（Space=带 extra 标记的 View） | 客户端根 Y.Doc meta（Collection/Tag 纯客户端） | **邻接表 parent_page_id + fractional index position** + 递归 CTE | parentDocumentId + documentStructure JSONB **双轨（历史包袱）** | **邻接表 + fractional index**（Docmost 模式；拒绝 Outline 双轨） |
| Offline | RocksDB 本地全量 + 增量同步 | IndexedDB 优先源 + BroadcastChannel | 无离线持久化（EVIDENCE_GAP） | y-indexeddb（一文档一库） | V1 不做（与 workspace/project 现状一致） |
| CRDT | Yjs | Yjs | Yjs | Yjs | **V1 不引入**（DEFER） |

### 4.2 Block / Page 数据模型七问（汇总）

- **A 内容格式**：四家全部是「结构化文档 + CRDT」组合——AppFlowy/AFFiNE = block tree 存于 Yjs 文档；Docmost/Outline = ProseMirror/Tiptap JSON 快照 + Yjs 二进制真源 + 派生纯文本。**无一家以 HTML 为存储格式**；Markdown 在 Outline 中是**交换格式**（导入导出用 `commonMark:true` 可移植模式，`shared/editor/lib/markdown/serializer.ts:1-10` 文件头注释自认 fork 自 prosemirror-markdown 加表格支持）。
- **B Block 独立持久化**：全部否。AFFiNE 服务端**没有任何 block 级存储/更新 API**，服务端读 block 靠 Rust loader 解析快照（`packages/backend/native/src/doc_loader.rs`）；Docmost/Outline 的 block（节点）都在整页 JSON/二进制里。
- **C Block 稳定 ID**：AppFlowy nanoid(10)（`frontend/rust-lib/flowy-document/src/parser/json/parser.rs:21`）；AFFiNE `sys:id`；Docmost/Outline 依赖 PM 节点位置（协作态靠 Yjs mark/RelativePosition 换算，`apps/server/src/core/comment/yjs.util.ts`）。
- **D 排序**：AppFlowy = children **数组顺序**（非分数索引）；AFFiNE = children Y.Array 顺序（**fractional indexing 仅用于 edgeless 画布**，`framework/std/src/utils/fractional-indexing.ts`）；Docmost = **fractional-indexing-jittered 字符串 + C collation 排序**（`apps/server/src/core/page/services/page.service.ts:200-216`、`page.repo.ts:730`）；Outline = documentStructure 数组顺序 + collection.index 字符串列（曾发生索引碰撞需专门迁移修复 `20250327062414`）。
- **E 移动**：AppFlowy 用 `prev_view_id` 锚点（`flowy-folder/src/entities/view.rs:557-620`）；Docmost/Outline 改 position/index 键，移动只改 O(1) 个键。
- **F 删除**：Docmost 软删除 `deleted_at` + 递归恢复 + 「父仍在删除态则脱离父级」规则（`page.repo.ts:261-313`）+ 定期清理；Outline DefaultScope 过滤 + documentRestorer。**无一家物理即删**。
- **G 嵌套**：全部用 parent 指针或 children 数组（邻接/文档内树），无 closure table、无物化路径。

[EVIDENCE E-03]
- Source: Docmost / Outline 源码
- Location: Docmost `apps/server/src/database/migrations/20240324T086300-pages.ts`（parent_page_id 自引用 FK + position varchar + content jsonb + ydoc bytea + text_content + tsv + deleted_at）；Outline `server/models/Document.ts:288-470`（text TEXT @deprecated / content JSONB / state BLOB 三列并存）
- Observation: 「协作真源 + 结构化快照 + 派生搜索文本」三列是 Docmost/Outline 双验证的成熟形态；但两家的 ydoc/state 列存在的原因都是**实时协作**。
- Implication: IMBoy V1 无协作，可裁掉 CRDT 列；保留「源格式 + 派生纯文本供 FTS」两层即够。

### 4.3 编辑器架构链对比

| 项目 | Editor → Document Model → Sync → Persistence → API |
|------|------------------------------------------------------|
| AppFlowy | appflowy_editor（独立 git 包）→ Flutter Node 树 → DocumentDataPB(protobuf/FFI) → collab_document(Yjs) → RocksDB + SyncPlugin(WS) → 19 个 FFI 事件 |
| AFFiNE | BlockSuite → Y.Map/Y.Array block tree → socket.io `space:push-doc-update` → PG updates 增量 + snapshots 快照（读时惰性压实 squashUpdatesToSnapshot） |
| Docmost | Tiptap React → y-prosemirror → Hocuspocus WS（debounce 10s 持久化）→ ydoc bytea + content jsonb + text_content 三写（isDeepStrictEqual 跳过无变化）→ REST 只管元数据，正文走协作通道 |
| Outline | 自研 PM editor（shared/editor 同构 schema，服务端 stub Editor）→ y-prosemirror → Hocuspocus（可独立进程）→ state BLOB + content JSONB 快照 + text 派生（DocumentUpdateTextTask）→ REST `documents.update` 整篇/patch 模式 |

### 4.4 实时协作对比

| 维度 | AppFlowy | AFFiNE | Docmost | Outline |
|------|----------|--------|---------|---------|
| 传输 | WS（client-api crate） | socket.io（自管重连 reconnection:false） | WS `/collab`（Hocuspocus）+ 独立 Socket.IO 事件通道 | WS `/collaboration`（独立服务）+ socket.io `/realtime` |
| 引擎 | Yjs(yrs) | Yjs + y-protocols | Yjs + Hocuspocus | Yjs + Hocuspocus + Redis |
| 冲突解决 | CRDT | CRDT | CRDT | CRDT（API 写走 documentCollaborativeUpdater + PermanentUserData） |
| 光标/Presence | 文档级 awareness（selection + 用户色） | awareness 二进制透传 | awareness（collaboration-caret） | yCursorPlugin + awareness 身份校验 |
| 离线 | RocksDB 全量本地 | IndexedDB 优先源 | 无离线持久化 | y-indexeddb 一文档一库 |
| 版本/快照 | collab_snapshot 表 | 压实时顺手存历史 | BullMQ 去重快照 | 会话结束落 revision，相同跳过 |

**关键观察**：四家为了「多人同时编辑」全部引入了 Yjs 全家桶 + 独立 WebSocket 协作通道 + 二进制文档状态。这是**以实时协作为核心卖点**的产品的合理成本；对以 IM 为核心的 IMBoy，V1 引入等于新增一个大型 runtime（见 §16 成本分析）。

## 5. IMBoy Current Architecture Evidence

### 5.1 实体与权限模型（必须尊重的现状）

[EVIDENCE E-04]
- Source: `docs/concepts/collaboration-hierarchy.md` + 迁移文件
- Location: 组织（`organization`，迁移 95/113-131）→ 工作区（`workspace`/`workspace_member`，迁移 76，role CHECK `owner|member|guest` 且**注释明确禁止扩展为通用 RBAC**）→ 项目（`project`，迁移 78，`UNIQUE(id, workspace_id)` 复合键模式）→ 群组/频道（scope=personal|workspace 两态，迁移 77 XOR CHECK）
- Observation: 工作区是「协作与资源容器」与权限边界；各层角色独立正交不跨层继承；资源表带复合唯一键 + 复合 FK 强制同 workspace 是既有惯例（`00000078:40`、`00000081:39`）。
- Implication: Content 挂 workspace、复用三角色、沿用复合键惯例，零新权限概念。

### 5.2 可复用基建清单（Content V1 不重建这些）

[EVIDENCE E-05]
- Source: imboy 后端
- Location:
  - 附件：`src/logic/attach_logic.erl` presign/confirm/authorize（scope 六分支读鉴权 fail-closed）+ `src/lib/elib_s3_sign.erl`（Garage SigV4）+ attachment 表 scope/scope_ref（迁移 13）+ anchor 列（迁移 108）；
  - 搜索：PG FTS + pg_jieba（`to_tsvector('public.jiebacfg',...)`），影子表模式 `fts_user`/`fts_group`/`fts_channel`（迁移 68/69：title A 权重 / description B 权重 + GIN + 触发器）；消息 FTS 排除 E2EE 密文（迁移 33）；
  - WS：`src/lib/imboy_syn.erl` publish/2,3（服务端推送原语）+ `src/ds/msg_s2c_ds.erl:send/7`（落库/免落库两态）+ ephemeral 直推先例 `src/lib/llm_stream.erl:90-119`（stream_delta 节流帧）；
  - 权限：`src/logic/workspace_logic.erl:my_role/2` / ensure_member 模式 + DB 触发器 fail-closed 双保险惯例；
  - REST：envelope `{code:0,msg,payload}`（`src/lib/elib_response.erl`）、`success_rfc3339`、分页 `elib_param:page/1` → `{total,page,size,list}`；
  - ID：`src/lib/elib_tsid.erl` 命名生成器（register/1 + generate/1，每表独立序列）；
  - 迁移：erlang_migrate（`src/lib/imboy_migrate.erl`），8 位零填充编号，当前最高 **00000135**，外层单事务、文件内禁 BEGIN/COMMIT。
- Observation: 附件直传链、FTS 影子表模式、WS 推送、成员校验、REST 规范、TSID、迁移体系全部成熟且有多个业务域复用先例。
- Implication: Content V1 的增量只有「文档实体表 + FTS 影子表 + 编辑器 + 路由/UI」；附件/搜索/推送/权限全部复用。

### 5.3 Content 雏形普查（六态分类）

| 状态 | 命中 | 判定依据 |
|------|------|---------|
| 现有生产能力（烟囱） | channel_message、group_notice、moment_post、announcement、enterprise_note（企业联系人备注，密文） | 各自独立表 + API + UI |
| 部分能力（配置型） | ai_agent_kb_logic（知识=配置文本注入，无文档实体） | `src/logic/ai_agent_kb_logic.erl` |
| 遗留/弃用 | 无 | — |
| 未发布/计划/死代码/仅文档 | 无 | 全文扫描零命中 |
| **通用 Content 基座** | **不存在** | 无 doc_/page_/article_/wiki_ 表、handler、路由 |

### 5.4 Content 是否一等业务对象（八维论证）

| 维度 | 作为 Project.description 之类的附属字段 | 作为一等实体 doc_page | 结论 |
|------|----------------------------------------|----------------------|------|
| Identity | 无独立 ID，无法被引用/收藏/分享 | TSID 主键，可深链可引用 | 一等 |
| Lifecycle | 随宿主创建/删除，无独立软删/恢复 | active/deleted 状态 + 恢复 | 一等 |
| Permission | 继承宿主，无法区分读写 | 复用 workspace 角色可判读写 | 一等 |
| Persistence | text 列内嵌，无版本无审计 | 独立表 + version CAS + 时间戳/操作人审计 | 一等 |
| Reference | 无法被消息/项目/频道引用 | doc_id 可被卡片消息、项目关联表引用 | 一等 |
| Search | 随宿主字段偶然可搜 | fts_doc 影子表 + 标题/正文分权重 | 一等 |
| Audit | 无 | created_by/updated_by + WS 通知事件 | 一等 |
| API/Client | 无独立 API | 完整 CRUD + 路由 + 深链 | 一等 |

### 5.5 文档-代码冲突记录（以代码 + 最新契约为准）

1. **`content` 词根已被占用**：`docs/architecture/module-map.md:20` 存在 `channel_content` 域（频道内容）。→ 新域命名采用 **`doc`** 词根（`src/features/doc/`、`doc_` 前缀、`doc_page` 表），与频道内容明确切割。
2. **module-map.md 过时**（2026-03-15，只列 8 域，无 workspace/project/features 体系；其 `<domain>_logic` 公开入口口径与 ADR-0007 铁律 3 的 `<bc>_facade` 冲突）。→ Content 按 ADR-0007 执行，不按 module-map。
3. **overview.md 落后于 ADR-0006/0007**（分层表无 features/products）。→ 同上。
4. **msg_type 清单三处文档不一致**（messaging-model vs message_api_contract vs 代码 imboy_codec 8 种）。→ Content 引用消息类型时以 `src/lib/imboy_codec.erl` 为准。
5. **ADR-0004 仍是 Proposed**（origin 命名空间未落地）。→ doc_page 不加 origin_id 列，不依赖未实施 ADR。
6. **glossary 未收录 Feature Slice 新术语**。→ 实施时随 Content 一并补 `doc` 词条（后续 PR，非本文档职责）。

## 6. Product Model（IMBoy Content 的实体分层）

```
Organization（不动）
└── Workspace（不动）
    ├── Project / Group / Channel（不动）
    └── Docs（新）：树状长文文档集合
        └── DocPage（一等实体）：一页 = 一行 = 一篇 Markdown 长文
```

| 概念 | 定位 |
|------|------|
| DocPage | **first-class persistence entity**（表 doc_page） |
| 页面树 | parent_id 邻接表 + position（Docmost 模式） |
| 收藏/最近/模板/标签 | **UI 概念，V1 不实现**（AFFiNE 把 Collection/Tag 放客户端根文档的思路佐证了它们不是核心持久化实体） |
| Block | **V1 不存在此概念**（编辑体验由 Markdown 承载） |
| Space/分区 | **V1 不引入**（工作区即分区；AppFlowy 的 Space=带 extra 标记的 View，证明它非必需实体） |

## 7. Domain Model

```
Workspace 1 ── N DocPage（树，自引用）
DocPage N ── N Attachment（经现有 attachment.scope='doc' + scope_ref，无新关联表）
DocPage 1 ── N DocPageRevision（V2，本期不建表）
Message → DocPage（卡片消息引用，payload 携 doc_id，无外键）
Project ↔ DocPage（V2：project_doc_rel，镜像现有 project_channel_rel 模式）
```

候选领域规则（重新立项时复核）：
1. doc_page.workspace_id NOT NULL；访问前置 `workspace_logic:my_role/2` 检查；
2. parent 必须同 workspace 且非自身后代（防环，复用 organization_department 防环先例）；
3. 删除 = 标记子树 deleted（递归 UPDATE），恢复遵循 Docmost 规则：递归恢复，若父仍处删除态则与父脱离；
4. 正文修改必须带 expected_version（CAS），失败返回 409；
5. **禁止**页面级 ACL、公开分享、跨工作区移动（全部 V2+）。

## 8. Page Model

`doc_page` 单表承载（详细 DDL 见 §18）：

- **标识**：`id` TSID（`elib_tsid:register(doc_page)` 独立序列）；**无 slug**（V1 深链直接用 TSID；Outline urlId / Docmost slug_id 服务的「人类可读公开 URL」在公开分享落地时才需要，V2 决策）；
- **树**：`parent_id bigint NULL`（同表自引用 FK + 复合 `(id, workspace_id)` 唯一键使 FK 可强制同 workspace）+ `position varchar(64)` fractional indexing 键。**position 语义 = 同一父节点下兄弟间的全序**（与 Docmost 一致，非全局序）；子树随父移动时后代 position 无需重写。
- **position 参数契约（v1.1，实施首日冻结）**：① 字母表 = 62 字符（`0-9A-Za-z`），具体排序在实施首日冻结于 domain 模块（建议 Figma 式交替序防退化）头注释，**一经冻结禁改**（改动=全量键重生成迁移）；② 初始兄弟键经哨兵键 `before(min)/between(a,b)/after(max)` 生成，首尾插入不退化为边界追加；③ 碰撞（并发产生同键）→ 调用方以 `between(碰撞键, 其右邻)` 确定性重试，上限 3 次；④ 重平衡触发：键长 > 48 **或** 重试 3 次仍碰撞 → 该父节点下全量兄弟均分重生成（单事务、保序、version CAS 保护）；⑤ 硬上限 varchar(64)，理论不可达（48 触发重平衡）。性质测试绑定 TREE-POS-001~005（§32）。
- **内容**：`title varchar(500)`、`body_md text`（CommonMark 子集）。**大小契约（v1.1 双限制）**：`length(body_md) ≤ 1,000,000`（Unicode scalar values，PG `length()`）**且** `octet_length(body_md) ≤ 4,194,304`（4 MiB UTF-8 字节）——两限制任一超出即 422；客户端计数（Dart UTF-16 code units）仅作 UX 预检，服务端为权威；`plain_text text`（写入时由纯函数从 body_md 剥离语法生成，供 FTS；同样受双限制约束）；
- **并发**：`version int NOT NULL DEFAULT 1`（每次成功 PATCH +1，CAS 依据）；
- **审计**：`created_by` / `updated_by` / `created_at` / `updated_at`；
- **软删**：`deleted_at timestamptz NULL` + `deleted_by`（工作区表多用 status 列，但文档需要「回收站 + 恢复 + 树语义」，deleted_at 更贴合，且与 Docmost/Outline 双验证一致）。

[EVIDENCE E-06]
- Source: Docmost 源码
- Location: `apps/server/src/core/page/services/page.service.ts:20,200-216`（fractional-indexing-jittered）、`apps/server/src/database/repos/page/page.repo.ts:218-229`（递归 CTE 树查询）、`:730`（C collation 排序）
- Observation: 邻接表 + fractional index 排序是纯 PG、零额外服务的成熟树方案，移动 O(1)。
- Implication: IMBoy 直接采用；fractional indexing 算法（Figma 公开算法，MIT 实现）移植为 Erlang 纯函数放 domain 层（铁律 4：零 mock 可测）。

## 9. Block Model

**结论：V1 无 Block 模型。** 依据：
1. 四竞品无一持久化 block 行（§4.2-B）——block 概念只在「结构化编辑器 + 协作」语境下才有必要；
2. IMBoy 现有富文本心智是 Markdown（聊天 GptMarkdown 渲染、频道图文、markdown_page 三路渲染成品）；
3. Block 表的查询/排序/移动/孤儿清理成本（AppFlowy 每行一 collab 的复杂度是反面教材，`flowy-database2/src/services/database/database_editor.rs:793-830`）在单人 Markdown 场景零收益。

V2 若升级结构化，路线是「整页结构化 JSON + 编辑器原生支持」，而非补 block 行表（§27）。

## 10. Editor Model

**决策：路线 B——Markdown 源文编辑（升级现有链路）。**

| 路线 | 评估 | 结论 |
|------|------|------|
| A. Flutter 原生结构化 editor（super_editor/flutter_quill） | 新依赖 + 新文档格式（quill delta / super_editor 树）与 Markdown 存储不匹配；多端一致性风险 | Rejected（V1） |
| B. Markdown（TextField + 工具条 + 预览） | 复用 `markdown_format.dart` 纯函数 + `channelMarkdownBody` 渲染 + GptMarkdown；四端零新渲染器 | **Recommended (V1)** |
| C. Quill | delta 格式锁死，需 md↔delta 双向转换层 | Rejected |
| D. WebView + Tiptap | 能力最强但引入 JS 桥/离线/体积/一致性成本，且 V1 无协作无需 PM 语义 | **V2 触发式候选**（§27.2） |
| E. 自研 Block Editor | AppFlowy 投入了独立包级工作量；与 IMBoy 体量不匹配 | Rejected |

[EVIDENCE E-07]
- Source: imboyapp
- Location: `lib/page/channel/channel_compose_page.dart`（1096 行，TextField(maxLines:null) + `_MarkdownToolbar`，正文 2000 字/≤9 图）；`lib/page/channel/widgets/markdown_format.dart`（7 种操作纯函数，文件头自认无 toggle 语义）；`lib/page/channel/widgets/channel_markdown.dart`（主题化渲染 + 授权图片）；pubspec：`flutter_markdown_plus ^1.0.7`、`webview_flutter ^4.14.0`、无任何 editor 框架依赖
- Observation: 「Markdown 撰写 + 渲染」链路在频道图文已是成品；缺的是 toggle 语义、标题层级、撤销栈、草稿自动保存、预览切换。
- Implication: Content 编辑器 = channel compose 模式的定向升级（补齐上述五项），而非新框架选型。

V1 编辑器规格（SHOULD 全列表见 §25）：toggle 语义（重复操作取消标记）、h1-h3、列表/引用/行内代码/代码块/链接/图片（经 BatchUploadController 复用附件链）、UndoHistoryController、3s 防抖自动保存（PATCH + CAS）、编辑/预览切换。

## 11. Attachment Model

**决策：全量复用现有 attachment / Garage 体系，禁止重建文件存储。**

[EVIDENCE E-08]
- Source: imboy 后端
- Location: `priv/migrations/00000013_*.sql`（attachment.scope CHECK `public|private|c2c|group|channel|moment` 后扩 teaching + scope_ref）；`src/logic/attach_logic.erl:authorize/1-3` 六分支 fail-closed 读鉴权
- Observation: scope 枚举是开放扩展点（teaching 已加过一次），presign→PUT→confirm 直传链完整，读侧授权按 scope+scope_ref 收敛。
- Implication: 新增 scope 值 `doc`（一条 ALTER CHECK 迁移）+ scope_ref=page_id；`authorize` 加 doc 分支（workspace 成员即放行读，fail-closed 兜底）即完成集成。

图片在 Markdown 中的引用沿用频道图文现状：正文嵌 object_key 引用，渲染端经 `AssetsService.viewUrl` + `cachedImageProvider` 授权注入（`imboyapp/lib/service/assets.dart`；chat_page 注释明确「object_key 必须经授权 provider」是设计约束）。**禁止**在正文持久化带签名的临时 URL（企业侧 `enterprise_asset` 的「禁持久化 URL」CHECK 是同一原则的更强表达，迁移 118）。

**生命周期契约（v1.1）**：
- **doc 软删/恢复不触碰 attachment 行**——文档删除 ≠ 附件删除；回收站恢复后图片必然可用（附件行从未被删除），杜绝「文档恢复后图片没了」；
- 附件清理完全跟随**现有** attachment GC 体系（`attach_cleanup_logic` 孤儿统计/清理）：scope='doc' 的 scope_ref 指向**永久删除**（而非软删）的 page 时才可判孤儿；
- 物理清理（页面永久删除）V1 不提供端点（回收站即终态）；V2 引入时孤儿判定须与「历史快照/卡片消息仍引用该附件」做引用保护（AFFiNE blob GC「引用保护型删除」先例，§4.1）。

## 12. Reference Model

- **V1**：文档间引用 = 普通 Markdown 链接（`[标题](imboy://imboy.pub/workspace/{ws}/docs/{id})`），客户端拦截 scheme 跳转；服务端不解析不物化。
- **title_snapshot 非真相属性（v1.1）**：doc_card 中 **doc_id 是唯一 canonical 身份，title_snapshot 仅是发送时刻的展示快照（缓存）**——打开卡片一律 `GET /docs/:id` 做权限校验并取当前标题；历史消息里快照与当前标题不一致属预期行为（与消息 reply_snippet 同语义，不构成缺陷）。
- **Backlink（V2）**：采 Outline 物化表模式——`doc_page_relation(page_id, ref_page_id, type)` 由异步 processor 在保存时解析正文链接写入（`outline/server/queues/processors/BacklinksProcessor.ts:11-55` 先例），查询侧零开销。
- **消息 → 文档卡片（V1 SHOULD）**：custom 消息 payload `{kind:"doc_card", doc_id, title_snapshot}`，客户端 message_type_registry 新增 builder（注册表 `lib/plugins/registry/message_type_registry.dart`），打开时走深链 + 权限校验。**E2EE 会话中的文档引用只带元数据**（E2EE 正文服务端不可见，迁移 33 FTS 排除密文同理）。

## 13. Permission Model

| workspace 角色 | 读 | 写（创建/编辑/移动/删除） |
|---------------|----|--------------------------|
| owner | ✅ | ✅ |
| member | ✅ | ✅ |
| guest | ✅ | ❌（403） |

依据与边界（**guest 语义已代码实证，非假设**——v1.1 将 §13 由「V1 假设」升级为契约）：

[EVIDENCE E-12]
- Source: imboy 后端
- Location: `src/logic/workspace_logic.erl:9`（头注：「Guest: 只读（W0 控制面；不改变其在已加入 Group/Channel 中的既有能力）」）、`:104`（工作区详情 Owner/Member/Guest 同权读）、`:771`（active 成员校验三角色均通过）、`:776-782`（可创建校验：Guest → 403「Guest 角色不能创建工作区资源」）；`src/logic/project_member_logic.erl:191-201`（内容写权限：ws_role=guest → 一切写 403「Guest 角色为只读」）；`src/logic/project_logic.erl:7-9,42,103,143`、`src/logic/project_task_logic.erl:7,204`、`src/logic/channel_logic.erl:326-343`
- Observation: 「**读 = ensure_member 三角色同权；写 = role ≠ guest**」是 workspace 资源域（project/task/channel）已代码化且头注文档化的一致模式，非文档孤本。
- Implication: doc_page 完全对齐该模式：读 = active 成员（三角色同权），写 = owner/member（guest 403）；实现镜像 `ensure_content_write` 模式。E-12 同时满足 fail-closed 方向（guest 越权一律 403）。
- `workspace_member.role` CHECK 注释明确**禁止扩展为通用 RBAC**（迁移 76）——V1 不做页面级 ACL 是遵守既有约束，不是遗漏；
- Docmost 的「space 角色 → restricted 页面 ACL 覆盖」与 AFFiNE 的 DocGrant + permission generation 是 V2 引入页面级权限时的成熟参照（Docmost `page-access.service.ts` 回落模型、AFFiNE `gateway.ts:1302-1323` 代数失效）；
- **无公开分享链接、无 guest 页面授权、无跨组织可见**（Non-Goal）；权限默认 fail-closed（对齐 ADR《privacy-defaults》：设置缺失 = 最保守）。

## 14. Search Model

**决策：PG 原生 FTS + pg_jieba，沿用仓内既有 FTS Projection Pattern**（fts_user / fts_group / fts_channel 三先例；复用的是架构模式，非 schema 复制）。**契约（v1.1）**：`doc_page` 是唯一真相，`fts_doc` 是**可抛弃投影（disposable projection）**——未来 `DROP + REBUILD` 全量重建必须不影响任何业务数据（重建脚本列 SHOULD 交付物，绑定 FTS-004）。

[EVIDENCE E-09]
- Source: imboy 迁移 + Docmost 迁移
- Location: imboy `priv/migrations/00000069_*.sql:27-51`（fts_channel：token tsvector + GIN + 触发器，name A / description B 权重）；Docmost `migrations/20240324T086800-pages-tsvector-trigger.ts`（setweight(title,'A') || setweight(text_content,'B')）与 `20250729T213756`（unaccent + pg_trgm + 1M 截断）
- Observation: 两家在不同技术栈上收敛到同一形态：**写入时派生纯文本 + 触发器维护 tsvector + 标题/正文分权重**；IMBoy 已有 jieba 中文配置与三个影子表先例。
- Implication: 新建 `fts_doc` 影子表（page_id PK / workspace_id / title / plain_text / token tsvector + GIN）+ doc_page 触发器；查询按 workspace 成员过滤，复用 `{total,page,size,list}` 分页响应。

外部引擎（ES/Meilisearch/Tantivy）：四家中仅 AppFlowy（本地 Tantivy）与 AFFiNE（自研 Rust 内存索引）自建，均为「客户端可离线搜索」驱动；Docmost/Outline 均纯 PG。IMBoy 服务端搜索场景 PG 足够，**不引入新存储引擎**。

## 15. Version Model

**决策：V1 不做历史版本（无 revision 表）；并发用 version CAS。**

- `doc_page.version`（int，PATCH 需带 expected_version，不匹配返回 409 + 当前版本）；
- 软删除回收站覆盖主要事故面（误删可恢复）；
- V2 revision 配方（已验证）：Docmost 式异步去重快照——保存事件入队（jobId=page_id 去重）、与上一快照 isDeepStrictEqual 相同则跳过、新页 60s/老页 5min 双档节流（`apps/server/src/collaboration/constants.ts` + `processors/history.processor.ts`）；恢复 = 客户端 setContent 后走正常保存链生成新版本（零服务端特殊逻辑）。

## 16. Realtime Model

### 16.1 V1 架构

```
V1 = REST CRUD + 乐观锁 CAS + 服务端 WS 变更通知（不做协同）
编辑：PATCH /docs/:id (expected_version) → 409 时客户端重载/合并
通知：doc 变更 → imboy_syn:publish 对 workspace 在线成员推 ephemeral 帧
      {type:"doc", action:"doc_updated", payload:{workspace_id,page_id,version,updated_by,updated_at}}
      （ephemeral 免落库，llm_stream stream_delta 先例；客户端收帧 → invalidate provider → 重拉）
```

### 16.2 为什么 V1 不需要 CRDT

[EVIDENCE E-10]
- Source: 四项目源码（§4.4 对比表）
- Location: Docmost `apps/server/src/collaboration/collaboration.gateway.ts`（Hocuspocus debounce 10s/45s + Redis 扩展）、Outline `server/services/collaboration.ts`（独立进程 + 6 个扩展）、AFFiNE `packages/backend/server/src/core/sync/gateway.ts`（1400+ 行 socket.io 网关）
- Observation: 实时协作的成本 = CRDT runtime（Yjs）+ 二进制文档状态列 + 独立 WS 协作通道 + 水平扩展组件（Redis）+ 离线持久化层 + 权限代数失效。四家全部为「多人同时编辑」核心卖点支付了这笔成本。
- Implication: IMBoy 的核心卖点是 IM（WS/消息/E2EE 已有），文档 V1 是知识沉淀而非协同编辑；单人编辑 + CAS + 通知已满足「别人改了我要看到」。**CRDT = NO（DEFER）**。

### 16.3 冲突处理（V1 现实风险）

单人编辑为主、偶发双开。**CAS 语义（v1.1 候选）**：
- 所有写操作（title/body_md/移动）**MUST CAS**——API 无 force/overwrite 分支，V1 不存在「无条件覆盖保存」；
- 409 客户端唯一合法流程：**Reload 最新 → 本地变更重放/合并 → 以新 expected_version 重试**；放弃编辑 = 显式取消（本地丢弃）；
- 不静默 last-write-wins：缺 expected_version 的写一律 422 拒绝；
- force overwrite（管理恢复语义）→ V2+ 可选且仅限 workspace owner、独立端点 + 审计；V1 列入 Non-Goals（§26）。

### 16.4 V2 升级路径（明确，不留模糊）

- **触发条件**：同一文档 ≥2 人并发编辑成为高频场景，或 Web 端团队明确要求协同。
- **技术路线**：存储层加 `state bytea`（Yjs 真源）列，body_md 降级为快照（Docmost 三列模式）；编辑器换 WebView+Tiptap（y-prosemirror）或 super_editor+yjs；协作服务为**独立部署单元**（Node @hocuspocus/server，实施前核验其许可证），复用 IMBoy JWT。
- **数据迁移**：Markdown → ProseMirror JSON → Y.Doc 种子。双先例：Outline `server/scripts/20231119000000-backfill-document-content.ts`（Markdown→CRDT→JSON 两代演进）、Docmost `TiptapTransformer.toYdoc`（onLoadDocument 无 ydoc 时从 JSON 重建）。V1 严格 CommonMark 子集的纪律使该转换无损可控。
- **降级方案**（若不想引入 Node sidecar）：文档级编辑锁 + 在线编辑者 presence（经现有 WS 即可，成本约为 CRDT 的 1/10），覆盖 90% 团队场景。V2 启动时在两方案间做独立决策。

## 17. API Contract（设计，不实现）

风格遵循：新式资源嵌套（`/api/v1/workspaces/:workspace_id/...` 一代，如 organizations 家族）+ envelope `{code:0,msg,payload}` + `page/size` 分页 + success_rfc3339。

| Method | Path | 语义 | 级别 |
|--------|------|------|------|
| GET | `/api/v1/workspaces/:workspace_id/docs?parent_id=&page=&size=&include_deleted=` | 列表（flat + parent_id，客户端组树；含回收站过滤参数） | MUST |
| POST | `/api/v1/workspaces/:workspace_id/docs` | 创建（title, parent_id?, body_md?, position 锚点） | MUST |
| GET | `/api/v1/docs/:id` | 详情（body_md, version, 祖先链 breadcrumb） | MUST |
| PATCH | `/api/v1/docs/:id` | 更新 title/body_md（带 expected_version，CAS）/ 移动（parent_id+position） | MUST |
| DELETE | `/api/v1/docs/:id` | 软删（子树递归标记） | MUST |
| POST | `/api/v1/docs/:id/restore` | 恢复（Docmost 递归 + 脱离死父规则） | SHOULD |
| GET | `/api/v1/fts/doc?workspace_id=&q=&page=&size=` | 全文搜索（fts 家族路由风格，与 fts/msg 一致） | SHOULD |
| — | `POST /api/v1/attachment/presign / confirm`（现有端点） | scope=doc + scope_ref=page_id 复用 | MUST（零新端点） |
| WS | s2c ephemeral `doc_updated` 帧 | 在线成员失效缓存 | SHOULD |

错误约定：403（非 workspace 成员读 / 任何角色 guest 写）/ 404（不存在或不属于该 workspace——**不泄漏存在性**，对齐铁律 6 负例）/ 409（version 冲突，payload 带当前 version；客户端唯一合法处理 = §16.3 重载-重放-重试）/ 422（parent 防环、字符/字节双限制超限、**缺 expected_version**）。**PATCH 无 force/overwrite 分支**（v1.1 候选，§16.3）。

路由注册：静态加入 `imboy_router.erl` ApiV1Routes（非插件动态路由，ADR-0003）；是否挂 feature gate（BUILD-00R 裁剪）实施时登记 `imboy_feature` + policy catalog（packaging-contract 机制）。

## 18. Database Model（设计，不执行迁移）

### 18.1 表：`doc_page`（迁移号 = ASSIGNED_AT_IMPLEMENTATION_TIME，*Implementation Note*）

```sql
CREATE TABLE doc_page (
  id           bigint PRIMARY KEY,                       -- TSID：elib_tsid:register(doc_page)
  workspace_id bigint NOT NULL REFERENCES workspace(id),
  parent_id    bigint,                                   -- 自引用，见下
  title        varchar(500) NOT NULL DEFAULT '',
  body_md      text NOT NULL DEFAULT '',                 -- CommonMark 子集，≤1,000,000 字符
  plain_text   text NOT NULL DEFAULT '',                 -- 写入时派生（domain 纯函数）
  position     varchar(64) NOT NULL,                     -- fractional indexing（C collation 排序）
  version      int NOT NULL DEFAULT 1,                   -- CAS
  created_by   bigint NOT NULL REFERENCES "user"(id),
  updated_by   bigint REFERENCES "user"(id),
  created_at   timestamptz NOT NULL DEFAULT now(),
  updated_at   timestamptz NOT NULL DEFAULT now(),
  deleted_at   timestamptz,
  deleted_by   bigint,
  CONSTRAINT uk_doc_page_id_ws UNIQUE (id, workspace_id),
  CONSTRAINT fk_doc_page_parent FOREIGN KEY (parent_id, workspace_id)
      REFERENCES doc_page (id, workspace_id)             -- 复合 FK 强制同 workspace（迁移78/81 惯例）
);
CREATE INDEX idx_doc_page_ws_parent ON doc_page (workspace_id, parent_id)
    WHERE deleted_at IS NULL;
CREATE INDEX idx_doc_page_fts_join ON doc_page (workspace_id);  -- fts_doc 联表过滤
```

### 18.2 表：`fts_doc`（迁移号 = ASSIGNED_AT_IMPLEMENTATION_TIME；与触发器同迁移，*Implementation Note*）

照 `00000069` fts_channel 模式：`(page_id PK, workspace_id, title, plain_text, token tsvector)` + doc_page BEFORE INSERT/UPDATE 触发器同步 + GIN(token)，权重 title A / plain_text B，jieba 配置。

### 18.3 迁移：attachment scope 扩展（与 doc_page 迁移同批实施，*Implementation Note*）

`DROP CONSTRAINT + ADD CONSTRAINT` 扩 `scope` CHECK 增加 `'doc'`（该 CHECK 先例已扩过 teaching；毫秒级 DDL，仓内惯例不用 CONCURRENTLY，`00000081:30-31`）。

### 18.4 不建的表

`content_block`（否，§9）、`doc_space`（否，工作区即分区）、`doc_share`（V2）、`doc_page_revision`（V2）、`user_doc_favorite`（V2）、`doc_page_relation`（V2）。

### 18.5 迁移纪律

- 编号 8 位零填充顺延，up/down 成对，文件内禁 BEGIN/COMMIT，头注释自洽（ADR-0002 + imboy_migrate 契约）；
- **迁移号 = ASSIGNED_AT_IMPLEMENTATION_TIME（v1.1，架构不预设编号）**：实施 PR 落号前必须以当时 main 的实际最高号顺延——本文写作时 main 最高号为 00000135，但存在多条未合并候选分支（REST/CSWW 等）可能已占用后续号；绑定 DB-002 验收门；
- expand-first：不加回填列；不触碰任何历史迁移（Organization 契约 C18）。

## 19. Flutter Architecture

| 项 | 设计 | 复用证据 |
|----|------|---------|
| 路由 | `/workspace/:workspaceId/docs`（列表树）+ `/workspace/:workspaceId/docs/:pageId`（查看/编辑）；name `workspace_docs` / `workspace_doc_detail`；深链 `imboy://imboy.pub/...`（Android scheme 已声明 `AndroidManifest.xml:92`） | go_router 分域路由文件惯例（`lib/config/router/routes/workspace_routes.dart` 风格） |
| 入口 | workspace_shell 新增第 6 个 Tab「文档」（SHOULD；改 `workspace_shell_nav_items.dart` + `_DestinationStack`） | 壳扩展点已在 `workspace_shell_page.dart:88-112` 文档化 |
| 编辑器 | DocEditorPage：升级版 channel compose（§10 规格）；渲染复用 `channelMarkdownBody`（主题/授权图片/安全外链全继承） | `markdown_format.dart` 纯函数 + `BatchUploadController` |
| 数据层 | `DocApi extends HttpClient`；`FutureProvider.autoDispose.family` + 写后 invalidate | `workspace_data_providers.dart` 模板（含用法契约注释） |
| 消息卡片 | message_type_registry 注册 doc_card builder，懒取标题，点击深链 | `lib/plugins/registry/message_type_registry.dart` + 15 个既有 builder 先例 |
| 离线 | **不做**（与 workspace/project 现状一致：纯在线 FutureProvider） | `workspace_data_providers.dart` 头注释「全部走后端」 |
| i18n | slang 词条新增（`assets/i18n/<locale>/*.i18n.yaml`） | — |
| 平台 | android / ios / macos / web 同步可用（纯 Dart Markdown 链路四端无损） | pubspec 平台目录 |

## 20. Admin Architecture

**决策：Content Admin 第一期不存在**（V1 无任何 `/api/adm` 侧 Content 端点与页面）。这不是遗漏，而是定位裁定：

[EVIDENCE E-11]
- Source: imboyadmin
- Location: `src/services/api/client.ts:6`（`BASE_URL = '/api/adm'`）；`src/components/shared/adminRoles.ts`（6 个内置角色全部为平台侧角色：超级/运营/审计/审核/安全/客服管理员）；`src/modules/organization/api/public.ts` 头注（「Platform Admin 不映射 org owner/admin……创建组织/改名等 App 面旅程在平台面不存在」）；`package.json` dependencies（grep editor/tiptap/quill/prosemirror/lexical/markdown **零命中**，长文本只有 `src/components/ui/textarea.tsx`）；`src/pages/content-moderation/`（SensitiveWord / ContentReviewQueue / AppealReview 三页——治理面先例）
- Observation: imboyadmin 是**平台运营治理面**，不是租户自助管理面；而 Content V1 是租户侧（工作区内）的内容**生产**能力，与 Admin 定位正交。把生产工具放进 Admin 会重蹈 organization 模块头注明确避开的边界冲突，且需引入全新富文本依赖面（XSS/消毒）。
- Implication: V1 的内容生产只发生在 App/Flutter 与（未来的）Web Shell；Admin 保持零 Content 面。

**触发式进入条件（V2+，满足其一才立项）**：
1. 平台级内容治理需求出现（敏感词命中、举报申诉、下架/恢复）→ 按 `content-moderation` 既有模式扩展，复用 `PermissionRoute` + `DataTablePagination`（`src/components/shared/DataTable.tsx:206`，强制规范：默认 size=10、页变重置 page=1）+ TanStack Query + `useListQueryState` 脚手架，新增 `src/modules/content/` 模块（module_map.md 硬规则）；
2. 平台运营需要只读审计视图（谁建了什么文档）→ 仅列表/详情只读页，复用 TSID/`EntityId` 管道（`src/types/common.ts:8` + `safeParseBigIntJson` 已在 axios `transformResponse` 全局注入）。

不做「租户管理员在 Admin 写文档」这条线——那是 App/Web Shell 的职责（§19）。

## 21. Content 与现有对象关系（V1 必须 / 可选 / Future / 禁止）

| 关系 | 级别 | 说明 |
|------|------|------|
| Content ↔ Workspace | **V1 必须** | 外键 + 权限 + 复合键，全部复用 |
| Content ↔ User | **V1 必须** | created_by/updated_by 审计；Agent（account_type=1）作为作者是能力预留但 V1 无生成入口 |
| Content ↔ Attachment | **V1 必须** | scope='doc' 复用，禁新建存储 |
| Message → Content（卡片分享） | **V1 可选（SHOULD）** | custom payload，E2EE 会话仅元数据 |
| Content ↔ Project（主页/关联） | **Future（V2）** | project_doc_rel 镜像 project_channel_rel；project.description 不迁移 |
| Task link Content | Future（V2+） | 任务描述维持现状 |
| Channel publish Content | Future（V2+） | channel_message 烟囱不动；收敛独立提案 |
| Message → create Content（沉淀） | Future（V2+） | E2EE 正文服务端不可见，只能客户端侧导出 |
| Content ↔ Agent（AI 写作/AI 知识源） | Future（V2/V3） | ai_agent_kb_logic 后续可读 fts_doc；AI 写作 Non-Goal |
| Content ↔ Group | **禁止（V1）** | 群公告烟囱不动 |

## 22. Security

1. **认证**：全部端点经 `auth_middleware_api_v1`（JWT + 设备签名），非白名单路由；
2. **授权**：每个 handler 入口 `workspace_logic:ensure_member`；跨工作区访问 404 不泄漏存在性（铁律 6 负例）；DB 层复合 FK 兜底；
3. **XSS/渲染安全**：Markdown 渲染禁 raw HTML（flutter_markdown_plus 默认不渲染内联 HTML，实施时以测试钉死）；外链走 `SafeLauncher.safeLaunchUrl` 白名单模式（channel_markdown 现状继承）；
4. **附件**：object_key 经 `AssetsService.viewUrl` 授权；正文禁持久化签名 URL；attachment authorize doc 分支 fail-closed；
5. **E2EE 边界**：文档**不在** E2EE 域（服务端可读可索引）；E2EE 会话中的文档卡片只带元数据——与消息域「服务器只见密文与元数据」一致；
6. **隐私默认**：fail-closed（权限缺失 = 最保守）；**不搞「界面隐藏但服务端仍保存」的假删除**（privacy-defaults ADR 红线）；软删除是真实删除标记 + 回收站语义，非隐藏；
7. **审计**：created_by/updated_by + doc_updated 通知；平台级内容审核（敏感词/审核队列）接入为 **DEFER**（工作区文档是否进平台审核需独立产品决策）。

## 23. License Considerations

**IMBoy 自身许可背景**：imboy 与 imboyadmin 已切换 **BSL 1.1**（Change License MPL 2.0），imboyapp 为 MulanPSL-2.0（仓内 LICENSE 与 CHANGELOG）。

| 项目 | License 证据 | 可学架构 | 可复实现 | 可抄代码/Schema/UI | 风险 |
|------|-------------|---------|---------|-------------------|------|
| AppFlowy | 根 LICENSE = **AGPL-3.0 全文**（661 行）；子包 LICENSE 多为 "TODO: Add your license here." 占位（`frontend/appflowy_flutter/packages/*/LICENSE`） | ✅ | ✅（独立实现） | ❌（AGPL 传染 + 子包授权不明） | **高** |
| AFFiNE | 根 LICENSE 分段：`packages/backend` + `packages/common/native` = **EE（生产需订阅，CE 分发部分回落 MPL2.0）**；其余（含 blocksuite）= **MIT**（LICENSE-MIT） | ✅ | ✅ | ❌ 服务端代码；前端 MIT 代码抄进 BSL 仓需保留版权声明且方向为「BSL 吸收 MIT」可行但需逐文件声明 | 中-高 |
| Docmost | 核心 **AGPL-3.0**；`apps/server/src/ee` 等目录 = 企业订阅许可（`packages/ee/LICENSE`，目录实际为空壳） | ✅ | ✅ | ❌（AGPL 传染） | **高** |
| Outline | **BSL 1.1**（Licensed Work "Outline 1.10.1"；Additional Use Grant 禁止用于第三方 Document Service；Change Date 2030-09-09 → Apache 2.0） | ✅ | ✅ | ❌（IMBoy 商业 IM 若吸收其代码直接违反 AUG） | **高** |

**结论（技术/许可证据，非法律意见）**：
1. **Can Learn / Can Reimplement：全部可以**——本 V1 架构即纯模式借鉴（Docmost 的树+FTS、Outline 的派生文本列、AFFiNE 的「服务端不存 block」），无一字代码复制；
2. **Can Copy Code：全部禁止**——AppFlowy/Docmost AGPL、AFFiNE 后端 EE、Outline BSL：**在未经许可证条件审查的情况下，禁止直接复制或引入任何相关代码**；任何候选依赖引入前必须完成逐依赖许可证核验（以下为技术/许可证据整理，非法律结论，发布前需法务复核）；
3. **依赖引入时逐个核验**：若 V2 采用 Hocuspocus/Tiptap/yjs，须在实施前核验其许可证与商业条款（Tiptap 有商业双许可成分）；fractional indexing 算法实现选 MIT 且保留声明；
4. **Need Legal Review**：BSL 1.1 的 Licensed Work 定义与「IMBoy 商业化分发形态」的相互作用，建议发布前法务复核（与既有 BSL 迁移遗留问题同批处理）。

## 24. Architecture Candidates（候选方案比较）

> 本节为任务规格要求的候选方案对比；**不用「最好/最优」类无依据措辞**，结论统一表述为 Recommended for IMBoy V1 并给出依据。

**Option A — Minimal Content**：不做独立内容域。仅增强既有对象的长文字段（如 `project_task` 加 description、`project.description` 富化），内容永远是别的实体的附属属性。
**Option B — Workspace Content**：工作区级树状长文文档（独立一等实体 `doc_page` + 邻接表树 + Markdown 源文存储 + 复用 workspace 权限/附件/FTS）。即本架构所推荐者。
**Option C — Notion-like Content**：Block 模型（结构化 JSON + 稳定 block ID）+ Notion 式 Database 雏形 + 协作预留（CRDT 列/网关）。

| 维度 | Option A | Option B | Option C |
|------|----------|----------|----------|
| Complexity | 最低（无新域） | 中（1 个 feature 切片 + 2 张表 + 编辑器升级） | 最高（Block 模型 + Database + 协作 runtime，参照 AppFlowy 行级 collab 复杂度证据） |
| Value | 低——只解决单点长文，无法承载「工作区知识层」；四烟囱痛点不变 | 高——填补从零起建的空白（E-01），可服务公告/说明/知识沉淀三类场景 | 潜在最高，但 V1 无对应真实需求（roadmap/product 零 content 规划，E-02），属为竞品清单开发 |
| Migration（后续演进成本） | 死路：附属字段永远长不出树/引用/搜索 | 好：CommonMark 子集 + version CAS 是 C 的无损前身（§16.4 迁移配方双先例） | — |
| Backend Cost | ≈0 | 2 张表 + 6 个端点 + 1 张 FTS 影子表 + scope CHECK 扩展 | 新增 block 存储/协作网关/水平扩展组件（E-10 成本清单），Erlang 侧无 Yjs 等价生态，需引入第二语言 runtime |
| Flutter Cost | ≈0 | channel compose 定向升级（补 5 项能力，零新框架） | 需自研或引入结构化编辑器框架（E-07：现状零依赖，AppFlowy 为此维护独立包级投入） |
| Realtime Cost | 0 | WS 通知复用现有基建（§16.1） | CRDT 全家桶 + 独立协作通道 + Redis（E-10：四竞品为协作核心卖点支付的成本） |
| Future Extensibility | 差（无实体则无演进起点） | 好（§27 三段路线：revision/ACL/协作轨各自独立触发） | 好但以一次性支付 C 的全部成本为前提 |

**结论**：**Recommended for IMBoy V1: Option B**（其中内容存储/编辑采用 A 级别的最小纪律——Markdown 源文，见 D-05；C 的升级路径在 §16.4/§27 保持畅通且无锁死）。选 B 的决定性依据：Content 需求真实存在（四烟囱 + AI 预留位，E-01/E-02）使 A 被否；而 V1 无协作需求与团队规模约束使 C 的成本无法被当前任何已证实的用例偿付（E-10 + roadmap 零规划）。A 并非全无价值——其「最小纪律」被吸收进 B 的存储决策。

## 25. V1 Scope — Workspace Document MVP（MUST / SHOULD / DEFER）

> **规模校准（v1.1）**：Content V1 定名 **Workspace Document MVP**，是**中等规模 Feature Slice**（约 2 张表 + 6 个端点 + 编辑器升级 + 三端接线），不是「小功能」——Flutter 侧（树 + 编辑器 + 自动保存 + 撤销 + 预览 + 附件 + 深链 + 壳集成）已是一套完整文档产品 MVP 的工作量。验收以 §32 为准，建议实施计划按 后端切片 → Flutter 只读 → Flutter 编辑 三段交付。

**后端 MUST**：doc_page + fts_doc 迁移；features/doc 切片（domain: markdown→plain_text 纯函数 + fractional indexing 纯函数 + 树防环；application: CRUD/移动/删除/恢复用例；interfaces: handler + facade；infrastructure: repo）；路由注册；workspace 三角色鉴权；CAS。
**后端 SHOULD**：restore 端点；fts/doc 搜索端点；WS doc_updated 通知；feature gate 登记。
**Flutter MUST**：路由 + 文档树列表页 + 查看页（渲染）；编辑器升级（toggle/标题/撤销/自动保存/预览）；深链。
**Flutter SHOULD**：workspace_shell 第 6 Tab；doc_card 消息卡片；web_shell 深链参数。
**Admin**：无（§26）。
**DEFER**：全部见 §26。

## 26. Explicit Non-Goals（NOT IN V1）

CRDT / Yjs / OT / Automerge；实时多人编辑与光标 presence；**无条件覆盖保存（force overwrite，含其隐式 PATCH 分支；管理恢复语义留 V2 评估）**；Block 模型与 content_block 表；Notion 式 Database（表格/看板/日历/公式/rollup/relation）；画布/白板/edgeless；AI 写作/AI 摘要；页面级 ACL；公开分享链接与 web 发布（含 slug）；访客页面授权；评论（行内/整页）；文档内 @mention；收藏/最近/模板/标签；backlink 物化与图谱；文件导入（docx/html/md zip）与导出包；离线缓存/本地 SQLite 文档表；版本历史 UI（revision 表与界面）；Admin 管理面；跨工作区移动/组织级 wiki；E2EE 文档；协同锁；敏感词/审核接入；Channel/Group/Project 既有内容形态的收敛迁移。

**Admin 不做的证据化理由**：imboyadmin 是平台运营面（`/api/adm` 前缀 + 6 个平台侧角色 + 组织模块头注明确剥离租户语义），且**零编辑器依赖**（package.json 全量 grep 无 editor/tiptap/quill/markdown）——租户侧内容生产放 Admin 与其定位正交；平台治理需求（内容审核）出现时再按 moderation 模式接入。

## 27. V2/V3 Extension Path

### 27.1 V2（按触发逐项立项，非整批）
- **doc_page_revision**：Docmost 去重快照配方（§15）；
- **收藏/最近**：user_doc_favorite；
- **Project 集成**：project_doc_rel（镜像 project_channel_rel）+ 项目主页约定；
- **分享链接**：slug_id + doc_share 表；**补齐 Outline 缺失的密码保护与过期字段**（Outline Share.ts grep 无 password/expires——竞品缺口即机会）；
- **页面级 ACL**：Docmost「restricted 覆盖 + 默认回落 space 角色」模式；
- **backlink**：doc_page_relation 物化 processor（Outline 模式）；
- **导入导出**：md zip 导入（Outline 相对链接解析先例）/ CommonMark 导出；
- **Admin 治理面**：如产品要求（平台审核/下架）。

### 27.2 协作轨（条件触发，§16.4 已定路径与迁移配方；先评估文档级锁的降级方案）

### 27.3 V3
Notion-lite 结构化（若立项：独立结构化文档 kind + 整页 JSON 模型，**绝不**回头补 block 行表）；AI 知识库消费 fts_doc；组织级 wiki 跨工作区聚合。

## 28. Migration Strategy

1. **代码落位**：全部新代码进 `src/features/doc/`（ADR-0007 铁律 1；模块前缀 `doc_` 登记 glossary 前缀注册表，与 cs_/eb_ 并列）；`make arch-check` 随首个 PR 接线；handler 静态路由进 imboy_router（ADR-0003）。
2. **数据库**：纯新增（doc_page + fts_doc + attachment CHECK 扩展，迁移号实施时分配），无数据迁移、无回填、不碰历史迁移（C18）。
3. **API**：只增不改；现有端点零变更。
4. **灰度**：feature gate 默认关 → 单工作区验证 → 按 packaging-contract profile 放开。
5. **既有烟囱**：明确**不迁移**；未来收敛（如 channel_message → doc）须独立提案并设计双向兼容。

## 29. Risks

| # | 风险 | 等级 | 缓解 |
|---|------|------|------|
| R1 | Markdown 表达天花板（复杂版式/嵌入物） | 中 | V1 严格 CommonMark 子集纪律保证未来可无损升级结构化（§16.4 迁移配方已验证双先例） |
| R2 | 编辑器体验与竞品差距 | 中 | 接受——V1 目标「可用可沉淀」非「所见即所得」；channel compose 已提供现成 Markdown 撰写链路，可作为 V1 编辑体验的实现基础（「用户接受度」未经产品数据验证，不作为依据） |
| R3 | 团队场景双开编辑 409 摩擦 | 低-中 | 明确冲突 UX（重载/覆盖）；V2 文档锁方案成本 1/10 |
| R4 | attachment CHECK 变更碰热表 | 低 | 毫秒级 DDL 仓内惯例（00000081:30）；teaching 先例 |
| R5 | 大文档 FTS 写放大 | 低 | 1M 截断（Outline/Docmost 同款）；触发器仅依赖列变化时重算（Outline 2026-09 触发器收窄先例） |
| R6 | `doc` 命名与 channel_content/AI kb 语义混淆 | 低 | glossary 立 `doc_` 词条；「知识库」一词保留给 AI 域 |
| R7 | 许可污染（实施时无意抄代码/引依赖） | 高 | §23 红线 + 依赖引入前许可证核验步骤写进实施 PR 模板 |
| R8 | Erlang fractional indexing 实现缺陷 | 低 | 纯函数 + property-based 测试（domain 层零 mock 可测，铁律 4） |
| R9 | 期望膨胀（企业买家预期 Notion 级） | 中 | 以本文 Non-Goals 为对外沟通基线；V2 路线图透明化 |
| R10 | WS doc_updated 噪音 | 低 | ephemeral 免落库 + 仅 workspace 在线成员 + 客户端节流 invalidate |

## 30. Evidence Index

**外部源码（克隆于 /tmp/content-arch，取证时点 HEAD）**
- AppFlowy `5cf3a36`：LICENSE(AGPL-661 行)；`flowy-folder/src/entities/view.rs:40,184,345,557-620,727-766`；`flowy-document/src/document_data.rs:1-76`、`parser/json/parser.rs:13-21`；`flowy-database2/src/services/database/database_editor.rs:793-830`；`flowy-search-pub/src/{tantivy_state,schema}.rs`；`collab_builder.rs:193-231,289-362`；`Cargo.toml:90-97`（collab/yrs）。服务端=独立仓 EVIDENCE_GAP。
- AFFiNE `d897bb3`：根 LICENSE + LICENSE-MIT + `packages/backend/server/LICENSE`(EE)；`schema.prisma:181-220,462-481,639-711,1213-1234`（workspaces/workspace_pages/updates/snapshots/blob）；`blocksuite/framework/store/src/model/block/types.ts:1-10`；`framework/std/src/utils/fractional-indexing.ts`；`packages/backend/server/src/core/sync/gateway.ts`（sync 网关/权限代数）；`core/doc/storage/doc.ts:135-218`（squash 压实）；`framework/sync/src/doc/impl/indexeddb.ts`。
- Docmost `7bef7b1`：README License 节 + `packages/ee/LICENSE`；`migrations/20240324T086300-pages.ts`、`20240324T086800-pages-tsvector-trigger.ts`、`20250729T213756`、`20260224T233803-page-permissions.ts`、`20250408T191830-shares.ts`；`collaboration/{collaboration.gateway,extensions/persistence.extension,constants}.ts`；`core/page/services/page.service.ts:200-216`、`database/repos/page/page.repo.ts:218-313,730`；`core/page/page-access/page-access.service.ts`；`core/search/search.service.ts`。
- Outline `c3d2ae5`：LICENSE（BSL 1.1，Change Date 2030-09-09）；`server/models/{Document,Collection,UserMembership,Share,Revision,Attachment}.ts`；`server/models/helpers/DocumentHelper.tsx`；`server/services/collaboration.ts` + `server/collaboration/*`；`server/queues/processors/{Revisions,Backlinks}Processor.ts`；`plugins/search-postgres/server/PostgresSearchProvider.ts`；`shared/editor/lib/markdown/serializer.ts`；`server/migrations/20160711071958-search-index.js`、`20250327062414`。

**IMBoy 代码**
- 迁移：00000001(foundation: attachment/group_notice/announcement)、00000003(channel/channel_message)、00000004(moment_post)、00000005/06(msg_c2c/c2g)、00000013(scope)、00000033(FTS 排 E2EE)、00000068/69(fts_group/fts_channel)、00000076(workspace)、00000077(scope XOR)、00000078/81(project)、00000095/113(organization)、00000108(anchor)、00000115(enterprise_note)、00000118(enterprise_asset)、00000135(当前最高)。
- 代码：`src/imboy_router.erl`；`src/api/auth_middleware_api_v1.erl`；`src/logic/{workspace_logic,attach_logic,fts_logic,ai_agent_kb_logic}.erl`；`src/lib/{elib_response,elib_param,elib_tsid,elib_s3_sign,imboy_syn,imboy_ws_action_registry,llm_stream}.erl`；`src/ds/msg_s2c_ds.erl`；`src/features/{agent,customer_service,enterprise_business}/`。
- 文档：`docs/adr/0001-0007` + 2026-08-08×2 + 2026-09-07；`docs/architecture/feature-slice-rules.md`、`module-map.md`、`overview.md`；`docs/concepts/{collaboration-hierarchy,messaging-model,accounts-and-actors}.md`；`docs/glossary.md`；`docs/product/packaging-contract.md`。
- Flutter：`pubspec.yaml`；`lib/config/router/app_router.dart` + `routes/workspace_routes.dart`；`lib/page/channel/{channel_compose_page.dart,widgets/markdown_format.dart,widgets/channel_markdown.dart}`；`lib/page/workspace_shell/workspace_shell_page.dart` + `workspace_shell_nav_items.dart`；`lib/store/api/attachment_api.dart:159`；`lib/service/assets.dart`；`lib/page/workspace/workspace_data_providers.dart`；`lib/plugins/registry/message_type_registry.dart`；`lib/service/sqlite.dart:48`（_dbVersion=32）。
- Admin：`package.json`；`src/services/api/client.ts:6`；`src/components/shared/{DataTable.tsx:206,adminRoles.ts}`；`src/modules/organization/api/public.ts`；`docs/module_map.md`。

## 31. Decision Log

| # | 决策 | 选择 | 否决项与理由 | 证据 |
|---|------|------|--------------|------|
| D-01 | 是否新增 Content | YES，工作区知识层 | 「不做」被四烟囱痛点 + AI 预留位否决 | E-01/E-02 |
| D-02 | 领域归属 | Workspace（org 经传导） | Org 级（权限模型不支持、无组织级容器先例） | E-04 |
| D-03 | 一等实体 | doc_page 单实体 | 附属字段（八维全败，§5.4） | §5.4 |
| D-04 | 命名 | `doc` 词根（features/doc、doc_ 前缀、doc_page 表） | `content`（channel_content 占用）、`kb`（AI kb 占用） | E-01/§5.5-1 |
| D-05 | Block 存储 | **E：Markdown 源文 + 派生 plain_text** | A 整页 JSONB（V1 无结构化编辑器，JSON 无消费方）；B Block 行（四竞品零先例）；C PM JSON（强绑 Tiptap/PM 生态）；D 混合（两套真相） | E-03/E-06/E-07 |
| D-06 | 树结构 | 邻接表 + fractional index | Outline 双轨（documentStructure 同步包袱 + 碰撞修复迁移）；整数序号（全量重排） | E-06 |
| D-07 | Editor | B：Markdown 升级 | A/C（格式不匹配/新依赖）；D WebView+Tiptap（V2 触发式）；E 自研（体量） | E-07 |
| D-08 | 附件 | 复用 attachment scope='doc' | 新建存储（违反禁止重复建设） | E-08 |
| D-09 | 权限 | workspace 三角色，guest 只读 | 页面 ACL（迁移76 禁 RBAC 扩展；Docmost/AFFiNE 模式留 V2） | E-04 |
| D-10 | 搜索 | fts_doc 影子表 + jieba | 外部引擎（四家中两家纯 PG 足够先例） | E-09 |
| D-11 | 版本/历史 | V1 仅 version CAS + 软删 | revision 表（V2 Docmost 配方已备） | §4.1/§15 |
| D-12 | Realtime | REST + CAS + WS 通知 | CRDT（新 runtime 成本，四家均为协作核心卖点支付） | E-10 |
| D-13 | Database（Notion 式） | A 完全不做，不预留表 | B 预留（违反禁止空壳/提前抽象）；C 最小表（无消费方）；D 完整（AppFlowy 行级 collab 复杂度反例） | §4.1 |
| D-14 | API 风格 | 新式资源嵌套 + envelope + CAS 409 | 老式动词路径（group_notice 风格为遗留代） | E-05 |
| D-15 | Admin | V1 无 | 平台运营面定位 + 零编辑器依赖 | §26 |
| D-16 | 软删 | deleted_at + 子树递归 + Docmost 恢复规则 | status 列（树语义/回收站不适配）；物理删 | §4.2-F |
| D-17 | 消息集成 | V1 仅 doc_card 卡片（SHOULD） | Channel/Message 收敛（独立提案） | §21 |
| D-18 | 许可纪律 | 只学不抄；依赖逐个核验 | 直接复制任何四家代码（AGPL/EE/BSL，未经审查引入被禁止） | §23 |
| D-19 | CAS 语义（v1.1 候选） | 所有写 MUST CAS；409 = 重载-重放-重试；无 force 分支 | 普通成员隐式覆盖保存（绕过 CAS 安全模型，与「不静默 LWW」自相矛盾） | §16.3 / API-004~005 |
| D-20 | guest 权限（v1.1 实证） | 读=三角色同权、写=owner/member（guest 403），镜像 ensure_content_write 模式 | 带未验证假设进入实现（已由 E-12 代码证据消解） | E-12 / AUTH-001~002 |
| D-21 | 验收契约（v1.1 候选） | 重新立项后重写并批准 §32，才能成为完成判定真源 | 以当前过期候选矩阵直接 gate | §32 / Deferred Handling |
| D-22 | 文档分层（v1.1 候选） | 架构/领域/验收三层在重新立项时分别复核；迁移号等 Implementation Note 实施时分配 | 将研究时点实施细节误作当前冻结契约 | Deferred Handling / DB-002 |

## 32. Acceptance Matrix（候选验收矩阵，当前不生效）

> 本节保留 v1.1 研究时点的候选 Gate，**不是当前有效的完成判定真源**。重新立项后必须基于届时三仓 HEAD 逐条重写、验证并经用户批准，才能激活为实施 Gate。激活后，每条 Gate 才按 Requirement + 可执行验证 + Oracle + 期望结果落地证据（命令 + 退出码 + 日志路径）。
> 分级：**[M]** = MUST-Gate（FAIL 不可合并）；**[S]** = SHOULD-Gate（FAIL 记偏差，不阻断合并但阻断发布）。

### 32.1 ARCH（架构门）

| ID | 级 | 要求 | 验证 | Oracle | 期望 |
|----|----|------|------|--------|------|
| ARCH-001 | M | 全部新后端代码位于 `src/features/doc/`（路由注册与迁移除外），零文件落入旧四层目录 | `git diff --name-only` 审查 + `make arch-check` | arch-check 输出 | exit 0、零违规行 |
| ARCH-002 | M | 模块前缀 `doc_`；无 content_/kb_ 前缀 | 命名清单 grep | 模块清单 | 全部 doc_* |
| ARCH-003 | M | 跨单元调用仅经 facade；domain 层零 mock 可测 | arch-check 引用边矩阵 + domain 单测依赖审查 | 门输出 + eunit | 无越界边；domain 单测无 meck |
| ARCH-004 | M | 路由静态注册于 imboy_router（ADR-0003，非动态插件路由） | grep imboy_router.erl | 路由清单 | 6 端点在列 |
| ARCH-005 | M | 零新增 hex/npm/pub 依赖 | 三 manifest diff | git diff rebar.lock/pubspec.yaml/package.json | 零新增行 |

### 32.2 DB（数据门）

| ID | 级 | 要求 | 验证 | Oracle | 期望 |
|----|----|------|------|--------|------|
| DB-001 | M | 迁移 up/down 成对、幂等、文件内无 BEGIN/COMMIT | `make migrations-check` + down→up 二次执行 | 门退出码 + DB 状态 | exit 0；幂等 |
| DB-002 | M | 迁移号以实施时 main 实际最高号顺延 | 文件名 vs 当时 main 最高号 | 文件名 | 严格顺延，无撞号 |
| DB-003 | M | parent 复合 FK 强制同 workspace | 跨 ws parent 写入负例 | epgsql 错误 | FK 违反拒绝 |
| DB-004 | M | 防环：后代设为祖先的 parent → 422，树无环 | API 负例 | HTTP + 递归 CTE 检查 | 422；无环 |
| DB-005 | M | 软删标记整棵子树；恢复递归 + 死父脱离 | repo 层 eunit | DB 状态断言 | 子树全标；恢复挂接正确 |
| DB-006 | M | 未修改任何历史迁移文件 | `git diff priv/migrations` | diff 清单 | 仅新增文件 |
| DB-007 | M | 双限制生效：>1M 字符 或 >4MiB 字节 → 422 | 两形态 API 负例（纯中文超字节 / 超长拼接超字符） | HTTP + length()/octet_length() | 422 |
| DB-008 | M | attachment scope CHECK 扩展后存量零变化 | ALTER 前后全表 count/值抽查 | SQL 对比 | 行数/值不变 |

### 32.3 TREE / TREE-POS（树与排序门）

| ID | 级 | 要求 | 验证 | Oracle | 期望 |
|----|----|------|------|--------|------|
| TREE-001 | M | 移动子树 = O(1) 键变更，后代 position 不重写 | eunit + UPDATE 影响行数 | SQL 计数 | 仅移动节点 1 行变更 |
| TREE-002 | M | 同父兄弟序 = position C collation 字典序 | repo 排序测试 | 结果序 | 与插入序一致 |
| TREE-POS-001 | M | `between(a,b)` 生成键恒严格落于 a、b 之间 | EUnit 属性测试（随机键对 ≥10⁴ 组） | 属性断言 | 100% 成立 |
| TREE-POS-002 | M | 同侧重复插入 ≥10⁴ 次键长有界：>48 触发重平衡、恒 <64 | 属性测试 | 键长序列 | 无违例 |
| TREE-POS-003 | M | 碰撞恢复确定性：同输入重试路径唯一可复现 | 固定种子单测 | 重试结果 | 可复现 |
| TREE-POS-004 | M | 重平衡单事务全或无且保序 | eunit + 回滚注入 | DB 状态 | 全或无 |
| TREE-POS-005 | M | 字母表首日冻结：模块头注释与实现一致且冻结后 diff 为空 | 代码审查 + 注释存在性 | 模块头 | 冻结标记在 |

### 32.4 API / AUTH（接口与权限门）

| ID | 级 | 要求 | 验证 | Oracle | 期望 |
|----|----|------|------|--------|------|
| API-001 | M | CRUD happy path 全通 | smoke 家族脚本新增 docs 段 | HTTP + envelope | code=0、契约字段齐 |
| API-002 | M | 分页响应 `{total,page,size,list}` | 冒烟 | payload 形状 | 契约一致 |
| API-003 | S | 时间字段 RFC3339（success_rfc3339） | 冒烟 | 字段格式 | 契约一致 |
| API-004 | M | PATCH 缺 expected_version → 422；不匹配 → 409 + 当前 version | API 双负例 | HTTP | 两态分明 |
| API-005 | M | 无任何 force/overwrite 分支 | 路由 + handler grep | 代码 | 零命中 |
| AUTH-001 | M | guest 读 OK（列表/详情/搜索） | 三角色矩阵冒烟 | HTTP | 200 |
| AUTH-002 | M | guest 任意写（建/改/移/删/恢复）→ 403 | 负例全集 | HTTP | 403 |
| AUTH-003 | M | 非 workspace 成员 → 404（不泄漏存在性） | 跨 ws 负例 | HTTP | 404（非 403） |
| AUTH-004 | M | 鉴权先于 handler（auth_middleware 链不变） | `imboy_app.erl:99-109` 顺序审查 | 中间件链 | 顺序不变 |
| AUTH-005 | M | 每端点至少一个跨工作区负例（铁律 6） | 测试清单审查 | 用例枚举 | 6 端点全覆盖 |

### 32.5 EDITOR / FTS / ATTACH / WS / SEC

| ID | 级 | 要求 | 验证 | Oracle | 期望 |
|----|----|------|------|--------|------|
| EDITOR-001 | M | 工具条 toggle 语义（重复点击取消标记） | markdown_format 升级版纯函数单测 | 输入/输出对 | 全操作 toggle |
| EDITOR-002 | M | 撤销栈恢复上一步一致 | widget 单测（UndoHistoryController） | 状态 | 恢复一致 |
| EDITOR-003 | M | 自动保存 3s 防抖 + CAS 重试链（409→重载→重放→重试） | 集成测试（mock API） | 请求序列 | 契约序列 |
| EDITOR-004 | S | 页面关闭/崩溃后 3s 内输入有本地暂存兜底 | 真机手工（Android/iOS 各一） | 操作录屏 | 无丢失 |
| EDITOR-005 | M | 正文图片一律经授权 provider（AssetsService.viewUrl） | 渲染链代码审查 | 调用链 | 零直接 URL |
| FTS-001 | M | plain_text 派生纯函数：给定 body_md 输出确定纯文本 | eunit（零 mock，固定用例集） | 断言 | 一致 |
| FTS-002 | M | fts_doc 触发器随写同步（title A / plain_text B，jieba） | DB 集成测试 | tsvector 内容 | 权重与分词正确 |
| FTS-003 | M | 搜索仅返回请求 workspace 可见文档 | 跨 ws 双库负例 | 结果集 | 零泄漏 |
| FTS-004 | S | fts_doc 可抛弃：DROP+REBUILD 后业务数据零影响 | 重建脚本演练 | 行数对比 | 一致 |
| ATTACH-001 | M | scope='doc' 全链复用现有 presign/confirm/view_url | 冒烟 | HTTP | 零新端点 |
| ATTACH-002 | M | authorize doc 分支 fail-closed：非本 ws 成员取 URL 拒绝 | 负例 | 授权函数 | false |
| ATTACH-003 | M | body_md 中零持久化签名 URL | 写入侧校验单测 | 内容扫描 | 零命中 |
| ATTACH-004 | M | 文档软删/恢复不触碰 attachment 行 | eunit | 行计数 | 不变 |
| WS-001 | M | doc_updated ephemeral 帧仅发本 ws 在线成员 | 双 ws 在线负例 | 帧捕获 | 单 ws 收到 |
| WS-002 | M | 帧不落库（msg_s2c 零新行） | DB 断言 | 表查询 | 零行 |
| WS-003 | S | 客户端收帧 → provider invalidate → 数据刷新 | 集成测试 | 状态 | 刷新发生 |
| SEC-001 | M | Markdown 渲染不执行 raw HTML（注入 `<script>` 不生效） | 渲染单测 | 输出 | 不执行不渲染 |
| SEC-002 | M | 外链仅经 SafeLauncher 白名单 | 渲染链代码审查 | 调用链 | 零裸 launch |
| SEC-003 | M | 全部端点 JWT + 设备签名（非白名单） | 路由×中间件矩阵 | 清单 | 无裸路由 |

### 32.6 验收状态机

```
DEFERRED → 重新基线 + 修订评审 + 用户批准 → READY_FOR_IMPLEMENTATION
         → 实施中逐 Gate 附证据（命令/退出码/日志路径）
        → 全部 [M] PASS + [S] 偏差登记在案 → CONTENT_V1_ACCEPTED（可宣布完成）
        → 任一 [M] FAIL = 不可合并；[S] FAIL = 可合并不发布
```

---

> **延期提醒（v1.1）**：本文档当前只用于保存研究成果，禁止作为实施任务直接派发。重新立项时须：① 重新基线三仓 HEAD 与已采纳 ADR；② 关闭 Deferred Handling 的 Known Blockers；③ 重写并验证 §32；④ 由用户明确批准状态切换为 `ARCHITECTURE_ACCEPTED / READY_FOR_IMPLEMENTATION`；⑤ 再分配迁移号、登记术语/特性并制定实施计划。未经上述步骤，不得宣布 Content V1 已具备实施或完成条件。
