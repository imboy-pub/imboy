# S1 Security Review — E2EE-014 / 016 / 015 commits

- **Date**: 2026-07-20
- **Reviewer**: security-reviewer agents (2×, 并行) + 人工核实 + 修复
- **Scope**: imboy `da1cb358`（E2EE-014 trust-event 后端）、imboyapp `f944dfc8`（E2EE-016 backup parser）、imboyapp `e3646285`（E2EE-015 logout 清理）
- **依据**: 计划 E2EE-019 G1 安全出口要求「reviewer 对 S1 commits 做范围审查并记录 findings；Critical/High = 0，Medium 有 owner/期限」

## 结论

| 级别 | 数量 | 处置 |
|---|---|---|
| CRITICAL | 1 | **已修复**（E2EE-015 quitLogin 控制流回归） |
| Medium | 3 | 2 已修复（后端 event_id 抢占、版本 TOCTOU）、1 已澄清注释（RSA 传输密钥有意排除） |
| Low | 3 | 记录；2 择机、1 属移动端信任边界固有限度 |

修复后 imboy trust eunit 21/21 绿、imboyapp `dart analyze lib` 零问题 + 相邻回归全绿。

---

## CRITICAL-1 — logout purge 失败跳过物理资源收尾，换号复用旧账号加密库

- **来源**: E2EE-015（`e3646285`，我方本会话引入）。
- **文件**: `imboyapp/lib/store/repository/user_repo_local.dart` quitLogin / `lib/service/sqlite.dart` db getter。
- **问题**: quitLogin 中 `purgeAll()` 失败即 `return false`，跳过其后的 `SecureTokenStorageService.clear()` / `WebSocketService.closeSocket` / `SqliteService.close()`。而 `SqliteService.db` getter 仅按 `isOpen` 复用缓存句柄、不校验 uid。flutter_secure_storage 的 `readAll/delete` 在 Android Keystore 失效 / iOS Keychain 冲突时会真实抛 PlatformException（单测已覆盖该分支），触发后：currentUid 已清空（UI 认为已登出），但旧账号 SQLCipher 句柄仍打开、旧 JWT 未清、旧 WS 未断。随后登录另一账号，loginAfter 只补 E2EE purge 不 close DB，db getter 按 isOpen 复用旧句柄 → **新账号读写落进上一账号加密库**（跨账号数据串号）。比修复前更差（修复前 clearCache 失败只 log 后继续）。
- **修复**:
  1. `quitLogin`：purge 失败改为置 `purgeFailed` 标记，**不中断**后续 token/WS/DB 强制收尾；末尾按标记返回 false。物理资源无条件关闭。
  2. `SqliteService`（纵深防御）：句柄绑定 `_openUid`；db getter 发现 `_openUid != currentUid` 强制 `close()` 后重开；`close()`/`setDbForTest` 同步维护 `_openUid`。即使某路径漏 close，uid 漂移也不会复用旧库。
- **验证**: 新增 `test/service/sqlite_uid_isolation_test.dart`（2 测试：uid 一致复用、uid 漂移关旧不复用）绿。

## MEDIUM-1 — event_id 全局唯一可被抢占，静默吞掉合法信任事件

- **来源**: E2EE-014 后端（`da1cb358`）。
- **文件**: `imboy/src/repo/trust_audit_repo.erl` insert_event / migration 47 `uk_trust_audit_event_id`。
- **问题**: `ON CONFLICT (event_id) DO NOTHING` 唯一约束是全局 event_id，不含 actor/target 归属。任一认证用户可用**猜到/观测到**的 event_id 抢先自签一条合法事件插入；真正 owner 的请求命中冲突 → 返回 `{ok, duplicate}` → handler 回 200，而真实的信任/撤销决策从未落库或广播。直接击穿本 commit 要防的「撤销不得被吞」。
- **修复**: insert_event 在 event_id 冲突（DO NOTHING 命中）时**回读既有行**核对 `(actor_uid, target_uid, target_device_id, to_state)`：一致才是合法幂等重放 `{ok, duplicate}`；不一致 → `{error, event_id_conflict}` 拒绝，不静默成功。logic 层透传该 binary 语义错误。
- **验证**: 新增 eunit `event_id_conflict_rejected_test` 绿。

## MEDIUM-2 — 版本单调校验 check-then-insert TOCTOU（防回退竞态）

- **来源**: E2EE-014 后端（`da1cb358`）。
- **文件**: `imboy/src/logic/e2ee_trust_logic.erl` check_version_then_write（原）/ `trust_audit_repo.erl`。
- **问题**: 原实现「logic 层 SELECT MAX(version) → 校验 → 另起 INSERT」两次非事务往返。同一 target device 的并发请求（一条旧版本重放 + 一条新版本）可读到相同 pre-update MAX 同时通过 `>=` 校验，使旧版本事件在密钥轮换后仍被写入/广播——仅拒顺序回退，不拒竞态回退。
- **修复**: 版本校验**下沉进 repo 单事务**并原子化：`with_tx` 内先 `pg_advisory_xact_lock(hashtext(target_uid:device_id))` 按 target 串行化，锁内读 MAX 后决定是否 `identity_version_rollback`，再幂等 INSERT + 冲突归属核对，全部在同一事务内完成。删除 logic 层非原子预检与死代码 `max_target_identity_version`（DS+repo）。
- **验证**: `identity_version_rollback_rejected_test` 改为验证 insert_event 返回该语义错误、被调用 1 次；21 eunit 全绿。

## MEDIUM-3 — RSAService 登录密码传输密钥未纳入 logout 清单

- **来源**: E2EE-015（`e3646285`）。
- **文件**: `imboyapp/lib/service/rsa.dart`（`Keys.publicKey/privateKey`）。
- **判定**: 该 keypair 是**设备级登录密码传输密钥**（仅对登录/改密的明文密码做一次性 RSA 加密，服务端持对应公钥），非账号级 E2EE 身份秘密，不随账号切换轮换。清理无安全收益且会迫使下次登录重生成+重报公钥。
- **处置**: 在 `e2ee_secret_inventory.dart` `secretKeyPrefixes` 文档显式声明**有意排除** RSA 传输密钥与 `secure_token*`，消除「RSA 私钥全清理」措辞歧义，防后续维护者误判遗漏。行为不变。

## LOW（记录）

- **L-1** `imboyapp/e2ee_local_backup_service.dart` `minBackupBytes` 为硬编码字面量，未从 `E2EECryptoService.saltLength/ivLength/authTagLength` 派生；未来常量漂移有 RangeError 风险。**owner/期限**：E2EE-016 后续硬化，随下次触及该文件时改为派生表达式。
- **L-2** `e2eePurgePending` 标记存于非加密 shared_preferences；具沙盒任意读写能力的攻击者（越狱/root）可置 false 绕过补救 purge。属移动端 fail-closed 机制固有强度上限，非本 commit 新增漏洞；不单独改造存储层（收益有限）。
- **L-3** 后端 `to_int/1` 对 client 数字串 `binary_to_integer` 无长度上限（CPU-cost DoS 杠杆），依赖 Cowboy/proxy 层全局 body-size 限制兜底；非本 commit 引入。**owner/期限**：确认反代层 body 上限，纳入 E2EE-019 G1 核对项。

## 已复核无发现（后端）

签名验证（11 字段全绑定、fail-closed try/catch）、freshness 窗口边界（-1/0/+1ms 与 spec 一致、统一 stale_event 无 timing oracle）、撤销/代数校验（status=1 且 gen 精确匹配）、SQL 参数化（全 `$N` 占位）、路由鉴权（`/e2ee/trust/record` 不在 open allowlist、ActorUid 取自会话不可伪造）、错误消息 hygiene（泛化 binary，无密钥/栈泄漏）。

## 已复核无发现（客户端 backup parser）

notes_length 32 字节头 offset 22 布局与 reader 严格对齐、旧启发式彻底删除、资源上限在 readAsBytes/KDF/大分配前生效且统一 ArgumentError、物理 DID 不再被 `_applyRestoredKeys` 覆盖（全仓 setDeviceId 调用点核实无旁路）、10k fuzz 只 ArgumentError。
