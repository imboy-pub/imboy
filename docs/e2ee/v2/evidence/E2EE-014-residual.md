# E2EE-014 残留 — OpenAPI 同步 / 客户端 trust UI 边界 / 并发压测可行性

- **Date**: 2026-07-20
- **依据**: E2EE-014 evidence 「残留」三项：客户端 trust UI、OpenAPI 同步、100 并发重放集成压测。
- **本轮处置**: A 完成；C 划定边界（大部分 BLOCKED）；D 判定受阻于 DB 准备。

## A — OpenAPI 同步 `/e2ee/trust/record`（DONE）

**发现**: 该端点从未进过 OpenAPI（`api/paths/e2ee/` 无 trust 文件），非「同步新字段」而是**从零补文档**。

**改动**:
- 新建 `api/paths/e2ee/trust-record.yaml`：13 字段请求体（含 §3.3.1 canonical 11 签名字段说明 + 幂等/单调/freshness 语义）、200 `{success:true}`、4XX 泛化错误原因清单（`invalid_signature`/`stale_event`/`identity_version_rollback`/`event_id_conflict`/`actor_device_revoked` 等，声明不泄漏签名 oracle）。
- `api/openapi.yaml` 挂载 `/api/v1/e2ee/trust/record`。
- 结构完全镜像既有通过 lint 的 `report-device-key.yaml`（同 Envelope/4XX 模式）。

**验证**: YAML 语法 `yaml.safe_load` 通过；本机无 redocly（npx 离线拉不动），CI redocly 会跑；`actor_uid` 明确标注取自会话不接受 body。

## C — 客户端 trust UI 边界（划定：大部分 BLOCKED，不实施）

依 ADR 16：
- **已解锁（§3.3.1 scoped waiver）**: 设备用**自身 Ed25519 私钥**签 11 字段 canonical 记录「A 信任 B」的 trust-event → POST `/e2ee/trust/record`，及验证 `e2ee_trust_changed` 广播。
- **BLOCKED（§5 Cross-signing，仍 Proposed）**: 账号 Master Key / 委派 device-signing key 授权另一设备成 verified（§46）、device manifest `account_signature`（§4.1）、透明度日志 inclusion proof（DT-05）。五方签字未过不得实施。

**不实施理由（除边界外）**: 即便只做已解锁子集，仍是大型客户端特性（vodozemac Ed25519 签名暴露 + 与服务端逐字节一致的 canonical builder + e2ee_api + 广播验证 + safety-number UI）。其 wire 契约正确性**必须靠运行中的后端 + 真机验证**——canonical 逐字节失配即全拒。无后端/真机时 mock 协议边界只会产出「幻影正确」代码（既往教训）。建议作为独立会话，配真机 + 运行后端落地。

## D — 100 并发重放集成压测（受阻于 DB 准备）

**现状**: 本地 imboy_v1 在 migration **37**，`trust_audit` 表不存在（`to_regclass('public.trust_audit')` 为空）。`insert_event` 真 PG 测试需 trust_audit（migration 44 建表 + 47 加 freshness 列）+ user_device（actor_device_state 跨表读）。

**受阻**: 在共享 dev 库 imboy_v1 应用未来迁移（44/47）有风险——迁移 41 乱序、43-46 renumber 未定，且本地节点/smoke 依赖 imboy_v1 现状。不宜为压测污染 dev 库。

**当前保证**: 「100 并发只 1 审计 + 1 状态变化」已由 **migration 47 `event_id` partial UNIQUE** + 本轮安全修复的 **`pg_advisory_xact_lock` per-target 串行化 + 冲突归属核对**在代码层保证；logic 层单测覆盖 inserted/duplicate/rollback/conflict 四路返回。真并发压测属集成层，**需独立 throwaway 测试库**（跑 44+47 建表后压测）或专用 CI DB，**不建议改 imboy_v1**。

**建议**: D 作为独立任务，用一次性测试库（`createdb` → 应用 44+47 → 100 并发 harness → dropdb），而非在 dev 库上做。
