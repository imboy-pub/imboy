# Plan v2: E2EE 密钥托管自动备份闭环（Key Vault）

> 版本：v2（全面版，2026-06-13）| 取代 v1
> 关联：`e2ee-cross-device-recovery-assessment-2026-06.md`（评估）、`e2ee-key-backup-implementation-plan.md`（方案）
> 范本：Matrix 4S (SSSS) + Signal SVR 高熵 Recovery Key

## Summary
新增"密钥托管自动备份"：客户端用高熵 Recovery Key 加密 E2EE 私钥包（私钥+deviceId+keyId），上传自有 Garage S3 零知识托管；换设备/重装后输入 Recovery Key 还原旧密钥，进而解开服务端 `msg_store` 已存的历史密文。把"换机历史恢复"从"需用户事前操作"升级为"装好 App 输一个 Key 就回来"。

## User Story
作为 imboy 用户，我想在换新手机或删除重装后，输入一个恢复密钥就能自动看回全部历史消息，这样我不必担心丢失聊天记录，也无需在换机前做任何准备。

## Problem → Solution
**现状**：服务端已永久归档历史密文（`msg_store`），但新设备生成全新 deviceId+密钥对，旧密文 `e2ee.recipients[]` 里没有给新 did 包装的 AES 密钥 → 拉回也解不开，落 `_e2ee_failed`「[加密消息]」。现有三套恢复（设备直传/社交恢复/本地备份）全需用户**事前主动操作**。
**目标**：增加唯一"零事前操作"恢复路径——零知识加密的服务端密钥托管。核心洞察：**只需备份"能解开密文的密钥"（几十字节），无需备份消息库**。

## Metadata
- **Complexity**: XL（imboy 后端 + imboyapp 客户端 + 加固，约 20+ 文件）
- **分阶段交付**: Stage A 加固(解耦) → Stage B 后端 Vault → Stage C 客户端 → Stage D 真机回归
- **Estimated Files**: 后端 ~10，客户端 ~7，加固 ~4

---

## ⚠️ Phase 0 — 实施前必做核实清单（BLOCKING）

> v1 计划基于 agent 提取的代码模式，存在 3 处未证实假设。**实施者必须先逐项核实并回填，再开工**。任何一项与下述假设冲突，须先更新本计划对应 Task。

| # | 待核实事实 | 核实命令 | 假设值（v1） | 若不符的影响 |
|---|---|---|---|---|
| V1 | 迁移文件命名约定 | `ls imboy/priv/migrations/` | 现存是 `0000000N_语义名` 8位序号（**与项目记忆"时间戳版本号"冲突**） | 决定 Task B1 新建文件名格式；erlang_migrate strict 乱序检测可能拒绝错误序号 |
| V2 | `msg_archive_enabled` "默认 false" 文档位置 | `grep -rn "msg_archive_enabled" imboy/ --include=*.md` | **imboy/CLAUDE.md 全词无命中**（仅 msg_archive_repo 子串）；真正写 false 的文档位置未定位 | 决定 Task A3 改哪个文件；可能在 `src/logic/CLAUDE.md` 或 `src/ds/CLAUDE.md` |
| V3 | 好友关系校验函数 | `grep -rn "is_friend\|are_friends" imboy/src/ds/` | 假设 `friend_ds:is_friend/2` 存在（**未证实**） | 决定 Task A2 调用名；若不存在需用 `friend_ds` 实际 API 或 `friend_repo` |
| V4 | `validate_history_params` 真实行号 | `grep -n "validate_history_params" imboy/src/logic/messaging_logic.erl` | 子句在 **172/175/178/180/182 行**（已确认） | 已确认，直接用 |
| V5 | storage_secure 现状 | 已读 `storage_secure.dart:37` | `const FlutterSecureStorage()` 无 options（已确认） | 已确认，直接用 |
| V6 | flutter_secure_storage v10 默认 Android 行为 | 查包文档/源码 | 假设 v10 默认 `encryptedSharedPreferences` 行为 | 决定 Task A1 是否需显式设 AndroidOptions |
| V7 | E2EE 密文字段名 | 对照 `imboyapp service/CLAUDE.md` | **是 `recipients[]` 含 `{did,kid,ek}`**，非 v1 写的 `keys[]` | 影响恢复后解密复用逻辑的字段引用 |

---

## 核心设计决策

### D1：备份"密钥"而非"消息"
服务端已有 `msg_store` 永久密文归档，历史密文始终在服务器。Vault payload 仅托管：
```json
{ "version": 1, "private_key": "<PEM>", "public_key": "<PEM>",
  "device_id": "<旧did>", "key_id": "<旧kid>", "created_at": "<ISO8601>",
  "checksum": "sha256:<hex>" }
```
字段名与现有 `e2ee_transfer_service` / `e2ee_local_backup_service` 的 backupData **完全一致**，复用还原逻辑。

### D2：恢复凭据用 48 字符高熵 Recovery Key（不用口令）
客户端 CSPRNG 生成 30 字节 → Base32(RFC4648 无填充) → 48 字符，分组展示 `XXXX-XXXX-...`（Element X 教训：放弃 passphrase 避免弱口令）。

### D3：MVP 复用现有 PBKDF2-310k，不引入 Argon2id
**理由**：恢复凭据是 ~240-bit 高熵 Key，本身不可暴力破解，Argon2id 抗 GPU 特性非瓶颈；现有 `E2EECryptoService.deriveKey`（PBKDF2-HMAC-SHA256, 310k, OWASP 2021）已足够。引入 Argon2id 需新增 Dart 依赖，违反 KISS/YAGNI。**Argon2id 列入 NOT Building（Phase 2）**。

### D4：服务端零知识 + 复用 attach_logic 三段式
后端只存元数据（salt/checksum/object_key）+ Garage 托管密文 blob，**永不接触私钥/Recovery Key**。Garage 上传复用 `attach_logic` 的 presign→confirm(HEAD核实)→view_url(fail-closed)。

### D5：还原旧 deviceId/keyId 是关键
恢复必须把旧 did/kid 写回 secure storage（复用 `_decryptAndSaveKey`），否则拉回的旧密文（`recipients[]` 按旧 did 包装）仍解不开。

---

## 数据流

### 备份流
```
首次开启E2EE/密钥轮换后
  客户端: genRecoveryKey(若无) → genSalt/IV
        → deriveKey(RecoveryKey,salt)[PBKDF2-310k]
        → payload(私钥包JSON) → encryptAesGcm → {ct,authTag}
  POST /v1/e2ee/vault/init {} → 后端返回 {salt, presigned_put_url, object_key}
  PUT 密文blob → Garage(presigned, 直传, ≤300s过期)
  POST /v1/e2ee/vault/commit {object_key, key_checksum, blob_size, alg_ver}
        → 后端 upsert e2ee_key_vault (uid主键)
  客户端: 强制用户记录+二次确认 Recovery Key（明示丢失不可恢复）
```
### 恢复流
```
新设备登录(other_device_count>0 或 vault/status存在) → 引导弹窗 → /e2ee_key_recovery
  用户选「用 Recovery Key 恢复」→ 输入 Key
  GET /v1/e2ee/vault/status → {exists:true, alg_ver}
  GET /v1/e2ee/vault/fetch → {salt, key_checksum, presigned_get_url}
  客户端: deriveKey(Key,salt) → 本地比对 checksum
        ├ 不匹配 → 即时报「恢复密钥错误」，不下载 (省流量、不暴露明文)
        └ 匹配 → GET blob ← Garage → decryptAesGcm → payload
              → 还原 private_key/public_key/device_id/key_id 到 secure storage
              → 重启依赖私钥的服务实例
              → E2EEHealthCheckService.to.retryFailedMessages() (重解_e2ee_failed)
              → chat_archive_service.loadHistory() (拉msg_store历史密文→解密回填)
```

---

## API 契约

| Method | Path | Req Body | Resp payload | 鉴权 |
|---|---|---|---|---|
| POST | `/v1/e2ee/vault/init` | `{}` | `{salt:hex, object_key, presigned_put_url, expires_in}` | JWT，uid取自token |
| POST | `/v1/e2ee/vault/commit` | `{object_key, key_checksum, blob_size, alg_ver}` | `{committed:true, vault_version}` | JWT |
| GET | `/v1/e2ee/vault/status` | — | `{exists:bool, alg_ver, vault_version, updated_at}` | JWT |
| GET | `/v1/e2ee/vault/fetch` | — | `{salt:hex, key_checksum, presigned_get_url, expires_in}` | JWT |
| DELETE | `/v1/e2ee/vault/delete` | `{}` | `{deleted:bool}` | JWT |

约束：blob_size ≤ 16384(16KB)；presigned URL ≤300s；object_key 由后端生成 `vault/{uid}/{version}.enc`，**不接受客户端传 uid**（防越权）；commit 须校验 object_key 归属当前 uid。

---

## UX Design

### Before → After
```
Before: 换机/重装→登录→引导→/e2ee_key_recovery→[设备转移|社交恢复|本地备份导入]
        三入口全需"换机前已准备"，否则历史永久显示「[加密消息]」
After:  开启E2EE→引导生成+确认 Recovery Key（自动随密钥轮换更新备份）
        换机/重装→登录→引导→/e2ee_key_recovery
        →[★用 Recovery Key 恢复(置顶·零操作) | 设备转移 | 社交恢复 | 本地备份导入]
        →输入 Key→30s内历史自动解密回填
```
### Interaction Changes
| Touchpoint | Before | After | Notes |
|---|---|---|---|
| 开启 E2EE 后 | 无备份引导 | 引导生成+确认 RecoveryKey | 强制二次确认+明示不可恢复 |
| `/e2ee_key_recovery` | 3 入口 | 4 入口 | 新入口置顶标"推荐·零操作" |
| 恢复成功 | — | 自动拉历史+失败重试 | 复用 loadHistory + retryFailedMessages |

---

## Mandatory Reading

| 优先级 | 文件 | 行 | 为什么 |
|---|---|---|---|
| P0 | `imboy/src/api/e2ee_handler.erl` | 1-92, 347-371 | init 分发 + GET/DELETE 模板 + capability gate |
| P0 | `imboy/src/repo/e2ee_local_backup_repo.erl` | 101-118, 211-255, 281-291 | Repo 参数化SQL + TSID + row_to_map |
| P0 | `imboy/src/logic/attach_logic.erl` | 16-135 | Garage presign→confirm→view_url 全链路 |
| P0 | `imboyapp/lib/service/e2ee_local_backup_service.dart` | 60-124, 194-291 | 加密备份/还原骨架（新服务的上云版） |
| P0 | `imboyapp/lib/service/e2ee_crypto_service.dart` | 30-49, 70-106, 126-241, 284-311 | KDF/AES-GCM/随机数原语 |
| P0 | `imboyapp/lib/service/e2ee_transfer_service.dart` | 105-143 | `_decryptAndSaveKey` 还原私钥+did |
| P1 | `imboy/src/ds/e2ee_local_backup_ds.erl` | 14-28 | DS 薄透传模板 |
| P1 | `imboy/src/lib/elib_oss.erl` | 98-142, 346-358 | presign_put/get + garage_config |
| P1 | `imboy/priv/migrations/00000004_social.up.sql` | 612-661 | e2ee_local_backups 表参照 |
| P1 | `imboyapp/lib/store/api/e2ee_api.dart` | 78-90 | API client GET/POST 模板 |
| P1 | `imboyapp/lib/service/storage_secure.dart` | 31-45, 145-221 | secure storage 单例 + E2EE key 存取 |
| P1 | `imboyapp/lib/page/chat/chat/services/chat_archive_service.dart` | 210-253 | loadHistory 复用 |
| P1 | `imboyapp/lib/service/e2ee_health_check_service.dart` | 445-619 | retryFailedMessages 复用 |
| P2 | `imboy/src/imboy_router.erl` | 256-284 | 路由注册语法 |
| P2 | `imboy/test/logic/e2ee_logic_tests.erl` | 17-40 | EUnit + meck 模板 |
| P2 | `imboyapp/test/service/e2ee_backup_restore_test.dart` | 1-30 | flutter test 模板 |

---

## Patterns to Mirror

### 后端 ERROR_HANDLING（handler→client）
```erlang
%% SOURCE: e2ee_handler.erl:77-92 — 注意 helper 是 elib_response 不是 imboy_response
case e2ee_logic:user_keys(CurrentUid, TargetUid) of
    {ok, Payload}      -> elib_response:success(Req0, Payload);
    {error, Msg, Code} -> elib_response:error(Req0, Msg, Code)
end.
%% 取 uid: auth_ds:current_uid(State)
%% capability gate: ensure_e2ee_enabled(Req0) 先过闸门
```
### 后端 REPOSITORY_PATTERN（参数化SQL + TSID + 越权判定）
```erlang
%% SOURCE: e2ee_local_backup_repo.erl:231-255
Id = elib_tsid:generate(e2ee_key_vault),
Sql = <<"INSERT INTO ... VALUES ($1,$2,...) RETURNING ...">>,
case elib_pg:query(Sql, [Id, Uid, ...]) of
    {ok, _, [{Result}]} -> {ok, row_to_map(Result)};
    {error, Reason}     -> ?ERROR_LOG([...]), {error, Reason}
end.
%% DELETE: {ok,1,_}->ok; {ok,0,_}->{error,not_found}
```
### 后端 LOGGING
```erlang
?INFO_LOG([e2ee_vault_repo, vault_committed, Uid, Version]),
?ERROR_LOG([e2ee_vault_repo, commit_failed, Reason]),
```
### 客户端 SERVICE_PATTERN（全静态工具类）
```dart
// SOURCE: e2ee_local_backup_service.dart:26 / e2ee_key_service.dart:79
/// @author Imboy Team  /// @since 2026-06-13
class E2EEKeyVaultService { /* static Future<...> */ }
// 失败: throw Exception('上传密钥备份失败: $e'); 返回: Map<String,dynamic>
```
### 客户端 CRYPTO_REUSE（直接复用，勿重造）
```dart
// SOURCE: e2ee_crypto_service.dart
final salt = E2EECryptoService.generateSalt();              // 16B
final iv   = E2EECryptoService.generateIV();                // 12B
final key  = await E2EECryptoService.deriveKey(recoveryKey, salt); // 32B
final enc  = await E2EECryptoService.encryptAesGcm(plaintext, key, iv); // {ciphertext,authTag}
final csum = E2EECryptoService.calculateChecksum(plaintext); // sha256 hex
```
### 客户端 KEY_RESTORE（恢复核心）
```dart
// SOURCE: e2ee_transfer_service.dart:105-143
final storage = StorageSecureService.to;
await storage.savePrivateKey(privateKeyStr);
await storage.savePublicKey(publicKeyStr);
if (deviceId != null) await storage.setDeviceId(deviceId); // 还原旧 did 是关键
if (keyId != null) await storage.setKeyId(keyId);
```
### 客户端 API_CLIENT
```dart
// SOURCE: e2ee_api.dart:78-90 (GET 返空兜底) / e2ee_plus_api.dart:31-43 (POST 失败抛Exception)
IMBoyHttpResponse resp = await post(API.e2eeVaultCommit, data: {...});
if (!resp.ok) throw Exception(resp.msg);
return resp.payload as Map<String,dynamic>;
```
### TEST_STRUCTURE
```erlang
%% 后端: e2ee_logic_tests.erl:17-40
?WITH_MECK(e2ee_vault_ds, [{'commit',2,fun(_,_)->{ok,#{}}end}], fun()->
  ?assertMatch({ok,_}, e2ee_vault_logic:commit_vault(123,#{...})) end).
```
```dart
// 客户端: e2ee_crypto_service_test.dart:9-56 — 不用 ProviderScope, mock path_provider channel
TestWidgetsFlutterBinding.ensureInitialized();
test('vault encrypt/decrypt roundtrip', () async { ... });
```

---

## Files to Change

### Stage A 加固（解耦可先行）
| File | Action | 依赖核实 |
|---|---|---|
| `imboyapp/lib/service/storage_secure.dart` | UPDATE | V5,V6 + 兼容性(见 Task A1 GOTCHA) |
| `imboy/src/logic/messaging_logic.erl` | UPDATE | V3,V4 |
| `<msg_archive_enabled 默认值文档>` | UPDATE | V2（位置待定位） |
| `imboy/deploy/preflight.sh` | UPDATE | — |

### Stage B 后端 Vault
| File | Action |
|---|---|
| `imboy/priv/migrations/<命名待V1>_e2ee_vault.up.sql` / `.down.sql` | CREATE |
| `imboy/src/repo/e2ee_vault_repo.erl` | CREATE |
| `imboy/src/ds/e2ee_vault_ds.erl` | CREATE |
| `imboy/src/logic/e2ee_vault_logic.erl` | CREATE |
| `imboy/src/api/e2ee_handler.erl` | UPDATE（加 vault_* action + do_vault_*） |
| `imboy/src/imboy_router.erl` | UPDATE（注册 5 路由） |
| `imboy/include/error_code.hrl` | UPDATE（?ERR_E2EE_VAULT_* 5030+） |
| `imboy/test/logic/e2ee_vault_logic_tests.erl` | CREATE |
| `imboy/config/sys*.config` | UPDATE（vault bucket + e2ee_vault_enabled） |

### Stage C 客户端
| File | Action |
|---|---|
| `imboyapp/lib/service/e2ee_key_vault_service.dart` | CREATE |
| `imboyapp/lib/service/recovery_key_codec.dart` | CREATE（Base32，项目无现成） |
| `imboyapp/lib/store/api/e2ee_vault_api.dart` | CREATE |
| `imboyapp/lib/config/const.dart` | UPDATE（vault 路径常量） |
| `<恢复中心页面>`（grep /e2ee_key_recovery） | UPDATE（第4入口） |
| `imboyapp/lib/i18n/zh-CN.i18n.yaml`(+各语言) | UPDATE（t.chat.e2eeVault*，跑 slang） |
| `imboyapp/test/service/e2ee_key_vault_service_test.dart`, `recovery_key_codec_test.dart` | CREATE |

## NOT Building
- Argon2id（D3，Phase 2）/ 服务端双因子派生 / HSM·SGX 飞地（企业版）
- 完整消息库云备份（msg_store 已存密文）
- 切换 Signal Protocol（独立改造）/ 媒体单独备份（已在 Garage）

---

## Step-by-Step Tasks

### Stage A — 加固（低风险解耦，建议独立提交）

#### Task A1: storage_secure 显式配置卸载/平台行为
- **ACTION**: `storage_secure.dart:37` 给 `FlutterSecureStorage` 配置 iOS/Android options
- **IMPLEMENT**: `FlutterSecureStorage(aOptions: AndroidOptions(encryptedSharedPreferences: true), iOptions: IOSOptions(accessibility: KeychainAccessibility.first_unlock_this_device))`
- **MIRROR**: 现有 `read/write` 已透传 options 形参（storage_secure.dart:60-138）
- **GOTCHA（关键，v1 遗漏）**: ⚠️ **向后兼容风险**——iOS 默认 accessibility 是 `whenUnlocked`，已存的 `e2ee_private_key` 条目保留其原属性，**改 options 只影响新写入**，新旧条目属性不一致可能导致部分读取异常。安全做法：① 先评估是否需一次性 re-write 迁移（读出→用新 options 重写）；② `first_unlock_this_device` 的 `ThisDeviceOnly` 会**禁止 iCloud Keychain 同步**（这是安全增强，但若产品依赖跨设备 Keychain 同步则冲突，需确认）。**实施前必须在真机验证现有用户密钥仍可读**。
- **VALIDATE**: 真机：老用户升级后 `hasE2EEKeys()` 仍 true、能正常收发加密消息；新装用户重装后行为符合预期

#### Task A2: msg/history 增好友/群成员关系校验
- **ACTION**: `messaging_logic.erl` `validate_history_params`（172/175 行子句）增关系校验
- **IMPLEMENT**: c2c 子句（172行）取得 PeerId 后增 `friend_ds:is_friend(CurrentUid, PeerId)` 校验，非好友返回 `{error,<<"forbidden">>,403}`；c2g 子句（175行）增 `group_ds:is_member(CurrentUid, Gid)` 校验
- **MIRROR**: `e2ee_logic.erl:23-40` 的 `group_ds:is_member` + forbidden 返回模式
- **IMPORTS**: friend_ds（**V3 待核实真实函数名**）、group_ds
- **GOTCHA**: 仅密文泄露面收敛（拉回仍需密钥才能解），但属正确鉴权；勿误伤合法会话；确认 group_ds:is_member/2 签名
- **VALIDATE**: `make eunit` 新增非好友/非成员返回 403 用例

#### Task A3: 更正 msg_archive_enabled 文档 + preflight 强校验
- **ACTION**: 定位并更正"默认 false"描述 + preflight 增校验
- **IMPLEMENT**: ① 按 V2 核实结果定位文档（**imboy/CLAUDE.md 全词无命中，真实位置待定**），改为"配置默认 true（sys*.config 全部已置 true），代码兜底值为 false"；② `deploy/preflight.sh` 增 `grep -q "{msg_archive_enabled, true}" config/sys.pro.config || { echo "ERROR: msg_archive_enabled 未开启，换设备将无法恢复历史"; exit 1; }`
- **MIRROR**: preflight.sh 现有校验块风格
- **GOTCHA**: 先 `grep -rn msg_archive_enabled imboy/ --include=*.md` 找到真实文档；preflight 校验目标配置文件名以实际生产配置为准
- **VALIDATE**: `bash deploy/preflight.sh` 通过；故意改 false 时报错

### Stage B — 后端 Vault

#### Task B1: migration 建表
- **ACTION**: 建 `e2ee_key_vault` 表
- **IMPLEMENT**:
```sql
CREATE TABLE public.e2ee_key_vault (
    uid bigint NOT NULL,
    vault_version integer NOT NULL DEFAULT 1,
    salt bytea NOT NULL,
    key_checksum character varying(64) NOT NULL,
    object_key character varying(255) NOT NULL,
    blob_size integer NOT NULL,
    alg_ver integer NOT NULL DEFAULT 1,
    created_at timestamp with time zone DEFAULT now(),
    updated_at timestamp with time zone DEFAULT now(),
    CONSTRAINT e2ee_key_vault_uid_check CHECK ((uid > 0)),
    CONSTRAINT e2ee_key_vault_blob_size_check CHECK ((blob_size >= 0 AND blob_size <= 16384))
);
ALTER TABLE ONLY public.e2ee_key_vault ADD CONSTRAINT e2ee_key_vault_pkey PRIMARY KEY (uid);
```
  down.sql: `DROP TABLE IF EXISTS public."e2ee_key_vault" CASCADE;`
- **MIRROR**: `00000004_social.up.sql:612-661`
- **GOTCHA**: ⚠️ **V1 命名约定 BLOCKING**——`ls priv/migrations/` 确认序号 vs 时间戳；PK 用 uid（每用户一份覆盖式 upsert）；blob ≤16KB；若用 erlang_migrate strict，序号须严格递增
- **VALIDATE**: 迁移后 `\d e2ee_key_vault`

#### Task B2: Repo 层
- **ACTION**: `e2ee_vault_repo.erl`：`upsert/1`、`find_by_uid/1`、`delete_by_uid/1`
- **IMPLEMENT**: upsert 用 `INSERT ... ON CONFLICT (uid) DO UPDATE SET salt=$,key_checksum=$,object_key=$,blob_size=$,alg_ver=$,vault_version=vault_version+1,updated_at=now()`；find SELECT salt/checksum/object_key/version/alg_ver；delete 受影响行数判 not_found
- **MIRROR**: REPOSITORY_PATTERN（e2ee_local_backup_repo.erl:231-255）
- **IMPORTS**: `elib_pg:query/2`
- **GOTCHA**: salt 是 bytea 传 binary；ON CONFLICT 覆盖更新；uid 已是主键无需 tsid
- **VALIDATE**: `make eunit`（?TEST_WITH_DB）

#### Task B3: DS 层
- **ACTION**: `e2ee_vault_ds.erl` 薄透传 repo（导出 upsert/find_by_uid/delete_by_uid）
- **MIRROR**: `e2ee_local_backup_ds.erl:14-28`
- **VALIDATE**: `make compile`

#### Task B4: Logic 层
- **ACTION**: `e2ee_vault_logic.erl`：`init_vault/1`(幂等salt+presign PUT)、`commit_vault/2`(校验size+归属+写库)、`fetch_vault/1`(presign GET+salt/checksum)、`delete_vault/1`(删库+删blob)、`status/1`
- **IMPLEMENT**: object_key=`<<"vault/",(integer_to_binary(Uid))/binary,"/",(integer_to_binary(Ver))/binary,".enc">>`；presign 调 `elib_oss:presign_put_for_key/3`(≤300s)
- **MIRROR**: `attach_logic.erl:22-135`
- **IMPORTS**: `elib_oss`、`e2ee_vault_ds`
- **GOTCHA**: salt 幂等（已存复用旧 salt，否则旧 RecoveryKey 失效！）；delete 同步删 Garage blob 防孤儿；commit 校验 object_key 前缀 `vault/<uid>/` 归属
- **VALIDATE**: `make eunit`（meck e2ee_vault_ds + elib_oss）

#### Task B5: Handler + 路由 + 错误码
- **ACTION**: `e2ee_handler.erl` init 增 5 action + do_vault_*；router 注册；error_code.hrl 加 5030+
- **IMPLEMENT**: 端点见 API 契约；每个先过 `ensure_e2ee_enabled`；uid 用 `auth_ds:current_uid(State)`
- **MIRROR**: `e2ee_handler.erl:50-92, 347-371`
- **GOTCHA**: ⚠️ helper 是 `elib_response` 非 imboy_response；uid 用 `auth_ds:current_uid` 非 proplists；**禁接受 uid 参数**；新错误码补 `imboy_error:error_msg/1` 映射
- **VALIDATE**: `make compile && make eunit && make ctl ARGS="smoke all"`

### Stage C — 客户端

#### Task C1: Recovery Key 编解码
- **ACTION**: `recovery_key_codec.dart`：`generate()`(30B CSPRNG→Base32→48字符分组)、`parse()`(去分隔符+校验)、`format()`(XXXX-XXXX 展示)
- **IMPLEMENT**: 复用 E2EECryptoService FortunaRandom 套路生成随机字节；Base32 RFC4648 无填充自实现
- **GOTCHA**: 展示分组、派生用去分隔符原值；大小写归一
- **VALIDATE**: `flutter test test/service/recovery_key_codec_test.dart`

#### Task C2: Vault API client
- **ACTION**: `e2ee_vault_api.dart` extends HttpClient：init/commit/status/fetch/delete；const.dart 加 5 路径常量
- **MIRROR**: `e2ee_api.dart:78-90`、`e2ee_plus_api.dart:31-43`
- **GOTCHA**: blob 上传走 presigned PUT（Dio 直传 Garage，不经 HttpClient 拦截器）；业务 API 的 token 由拦截器自动注入
- **VALIDATE**: `flutter analyze`

#### Task C3: Vault 服务 — 备份
- **ACTION**: `e2ee_key_vault_service.dart` `backup()`：组 payload→genSalt/IV→deriveKey→encryptAesGcm→init→PUT→commit
- **MIRROR**: `e2ee_local_backup_service.dart:60-124`（导出骨架，写文件换成上传S3）
- **GOTCHA**: RecoveryKey/私钥/masterKey **绝不**入日志(注意 iPrint 是空壳，用 debugPrint 时也禁打印密钥)/请求体；密钥轮换后自动重新 backup
- **VALIDATE**: `flutter test`（加解密往返）

#### Task C4: Vault 服务 — 恢复
- **ACTION**: `restore(recoveryKey)`：status→fetch→deriveKey→**本地比对checksum(错则不下载)**→GET blob→decrypt→还原did/kid→拉历史+重试
- **MIRROR**: `e2ee_transfer_service.dart:105-143`、`e2ee_health_check_service.dart:445-619`、`chat_archive_service.dart:210-253`
- **GOTCHA**: 还原后重启依赖私钥的服务实例；末尾调 retryFailedMessages + loadHistory；**V7：密文字段是 recipients[] 含 ek**
- **VALIDATE**: `flutter test`；真机三场景

#### Task C5: UI 接入
- **ACTION**: 开启E2EE后引导生成+确认RecoveryKey；`/e2ee_key_recovery` 页加第4入口；i18n
- **IMPLEMENT**: grep `/e2ee_key_recovery` 定位恢复中心页加按钮→RecoveryKey输入页；文案 `t.chat.e2eeVault*` 后 `dart run slang`
- **MIRROR**: `e2ee_recovery_guide_dialog.dart:33-68`
- **GOTCHA**: 入口在路由页非 dialog；强制二次确认；颜色/间距走 AppColors/AppSpacing Token（DESIGN.md）；破坏性操作用 iosRed
- **VALIDATE**: 真机 UI 走查

### Stage D — 真机端到端回归
- **ACTION**: 换机/删除重装/升级三场景全链路（禁模拟器）
- **VALIDATE**: 历史 30s 内可读，无「[加密消息]」残留；输错 Key 即时报错；升级无需恢复行为不变

---

## Testing Strategy

### 单元测试矩阵
| 层 | Test | Input | Expected | Edge |
|---|---|---|---|---|
| 客户端 | RecoveryKey 生成 | — | 48字符Base32, ~240bit | |
| 客户端 | 加解密往返 | payload+Key | 解密==原文 | |
| 客户端 | checksum 校验 | 错误Key | 本地报错不下载 | ✓ |
| 客户端 | 还原写回 | payload | did/kid 入 secure storage | |
| 后端 | Vault upsert | 同uid二次 | 覆盖+version+1 | ✓ |
| 后端 | 越权 fetch | 他人uid token | 403 | ✓ |
| 后端 | blob 超限 | >16KB | commit 拒绝 | ✓ |
| 后端 | salt 幂等 | 二次 init | 复用旧salt | ✓ |
| 后端 | history 非好友 | 非好友 PeerId | 403 | ✓ |

### Edge Cases
- [ ] RecoveryKey 错误（checksum 不匹配）
- [ ] 无 Vault（status false → 提示走旧路径兜底）
- [ ] Garage 不可用（presign/上传失败 → 明确报错不静默）
- [ ] 并发备份（ON CONFLICT 一致）
- [ ] 超 1 年 TTL 消息（部分不可恢复，向用户明示）
- [ ] iOS 老用户密钥兼容（Task A1 GOTCHA）

---

## Validation Commands
```bash
# 后端
cd imboy && make compile && make dialyze && make eunit && make ctl ARGS="smoke all"
# 客户端
cd imboyapp && flutter analyze && flutter test test/service/e2ee_key_vault_service_test.dart test/service/recovery_key_codec_test.dart
# DB
cd imboy && IMBOYENV=local make run   # 迁移自动执行, psql \d e2ee_key_vault
# 加固
cd imboy && bash deploy/preflight.sh
# 真机（禁模拟器）：换机/删除重装/升级 三场景
```
EXPECT: 后端 eunit+dialyze 绿；客户端 analyze 无新增警告；真机三场景验收通过

---

## 安全威胁模型
| 威胁 | 缓解 |
|---|---|
| 服务端泄露 → 读历史 | 零知识：服务端无 RecoveryKey，blob 是 AES-256-GCM 密文 |
| RecoveryKey 暴力破解 | ~240bit 高熵，不可行（故 D3 免 HSM） |
| 越权读他人 Vault | JWT + uid 取自 token + object_key 归属校验，禁 uid 参数 |
| presigned URL 泄露 | ≤300s 过期 + 绑定 uid 路径 |
| checksum 反推明文 | 仅 vault_enc_key SHA-256 前缀，单向 |
| 密钥进日志 | 禁止；iPrint 空壳，debugPrint 也禁打印密钥 |
| 孤儿 blob | delete 同步删 Garage |

## Risks
| Risk | 可能性 | 影响 | 缓解 |
|---|---|---|---|
| 迁移命名约定不明(V1) | 高 | 中 | Phase 0 BLOCKING 核实 |
| storage accessibility 破坏老用户密钥(A1) | 中 | 高 | 真机验证 + 评估 re-write 迁移；可先只配 Android |
| friend_ds 函数名不符(V3) | 中 | 低 | Phase 0 核实 |
| 客户端无 Garage 直传先例 | 中 | 中 | presigned URL + Dio 直传，后端 attach_logic 已验证签名 |
| RecoveryKey 丢失=数据丢失 | 中 | 高 | 强制二次确认+明示+保留旧三套恢复兜底 |
| salt 非幂等致旧 Key 失效 | 低 | 高 | B4 GOTCHA：已存复用旧 salt |

## Completion Checklist
- [ ] Phase 0 七项核实完成并回填
- [ ] 遵循模式（elib_response/auth_ds/elib_pg参数化/static service）
- [ ] 错误处理匹配（三元组+elib_response）
- [ ] 日志 ?INFO_LOG/?ERROR_LOG（后端）/debugPrint（客户端，禁打印密钥）
- [ ] 无硬编码（有效期/上限/颜色用常量/Token）
- [ ] i18n 走 slang；UI 走 AppColors/AppSpacing
- [ ] 无不必要 scope（Argon2id/HSM 排除）
- [ ] 私钥/RecoveryKey 全程不入日志/服务端
- [ ] 自包含，实现无需再搜索

## Notes
- 跨 imboy + imboyapp 两仓，分仓 commit（工作区根非 git 仓库）。用户未要求前**不做 git 操作**。
- Stage A 解耦可独立先行；Stage B/C 有依赖（C 依赖 B 的 API）。
- Phase 2 演进：Argon2id 双因子派生、HSM/SGX（企业版卖点）、receiver-side 多设备分发。
- **置信度自评：6/10**（v1 为 7，下调因 Phase 0 暴露 3 处未证实假设 + storage 兼容性风险，核实后回升）。
