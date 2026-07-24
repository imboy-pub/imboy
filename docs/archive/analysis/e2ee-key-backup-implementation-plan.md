# imboy E2EE 自动化密钥托管备份 — 技术实施方案

> 生成日期：2026-06-13
> 目标：补齐"换设备/重装后历史消息平滑、零操作恢复"的最后一块拼图
> 关联评估：[e2ee-cross-device-recovery-assessment-2026-06.md](./e2ee-cross-device-recovery-assessment-2026-06.md)
> 范本：Matrix 4S (SSSS) + Signal SVR 双因子派生

---

## 0. 核心设计决策（先读）

### 决策一：备份"密钥"而非"消息"

imboy 服务端已有 `msg_store` 永久密文归档（TTL 1 年），历史密文始终在服务器。因此**无需上传消息库**，只需托管"能解开密文的密钥材料"：

```
备份内容（KeyVault payload，明文态仅几十字节）:
{
  "rsa_private_key": "<PEM>",   // 旧设备 RSA-2048 私钥
  "device_id": "<旧 did>",       // 关键：还原旧 did 才能匹配密文 keys[]
  "key_id": "<旧 kid>",
  "created_at": <ts>,
  "alg_ver": 1
}
```

**收益**：备份体积极小、上传瞬时、无需增量同步、无媒体存储成本。这是 imboy 相比 WhatsApp 方案的结构性优势，应在 ToB 文档中作为卖点。

### 决策二：恢复凭据用高熵 Recovery Key（不用 passphrase）

采纳 Element X 教训：放弃用户口令选项，只用客户端生成的 **48 字符 Base32 Recovery Key**（约 240 bit 熵）。避免弱口令，也就**无需服务端 HSM/限速**——因为高熵 Key 本身暴力破解不可行。

> 可选增强（Phase 2）：叠加服务端随机种子做双因子派生，进一步对冲"Recovery Key 被部分泄露"风险。MVP 不必须。

### 决策三：复用现有基础设施

| 能力 | 复用现有 |
|------|---------|
| 密文 blob 存储 | Garage S3（已部署，presign 直传链路已实现） |
| KDF / AES-GCM | `e2ee_crypto_service.dart`（PBKDF2/AES-GCM 已有） |
| 元数据表模式 | 参照 `e2ee_local_backups` 表 |
| 还原旧 did/kid | `e2ee_transfer_service.dart:121-138`（逻辑已存在，抽取复用） |
| 历史拉取 | `GET /v1/msg/history`（已实现，conv_seq 游标） |
| 恢复引导 UI | `e2ee_recovery_guide_dialog.dart`（已有，增加一个入口） |

---

## 1. 加密设计

### 1.1 密钥层级

```
Recovery Key (48 char Base32, 客户端 CSPRNG 生成, 用户保管)
        │  Argon2id (m=64MB, t=3, p=1, salt=服务端下发的随机salt)
        ▼
backup_master_key (256-bit)
        │  HKDF-SHA256(info="imboy-keyvault-v1")
        ▼
vault_enc_key (256-bit, AES-256-GCM)
        │  加密
        ▼
KeyVault payload 密文 (存 Garage S3)
```

- **Recovery Key 生成**：`CSPRNG → 30 字节 → Base32 编码 → 48 字符`，分组展示 `XXXX-XXXX-...`（参照 Apple 28 字符、Signal 64 字符）。
- **salt**：服务端为每个 uid 生成并持久化 16 字节随机 salt，恢复时下发。salt 不是秘密，但保证 Recovery Key → master_key 不可预计算。
- **校验值**：存 `vault_enc_key` 的 SHA-256 前 8 字节作 `key_checksum`，用于"输入 Recovery Key 后立即判定对错"，不暴露明文。

### 1.2 零知识保证

- 服务端只存：加密后的 blob（Garage S3）+ 元数据（salt、checksum、版本、size、ts）。
- 服务端**无法**解密：没有 Recovery Key 就推不出 vault_enc_key。
- Recovery Key 永不上传。丢失则不可恢复（UI 必须明示）。

---

## 2. 数据模型（后端）

### 2.1 新增表 `e2ee_key_vault`（参照 `e2ee_local_backups`）

```sql
CREATE TABLE e2ee_key_vault (
    uid           bigint   NOT NULL,
    vault_version int      NOT NULL DEFAULT 1,
    salt          bytea    NOT NULL,           -- Argon2id salt (16B)
    key_checksum  varchar(32) NOT NULL,        -- vault_enc_key SHA-256 前缀(hex)
    object_key    varchar(255) NOT NULL,       -- Garage S3 中密文 blob 的 key
    blob_size     int      NOT NULL,
    alg_ver       int      NOT NULL DEFAULT 1, -- 加密套件版本
    created_at    bigint   NOT NULL,
    updated_at    bigint   NOT NULL,
    PRIMARY KEY (uid)                          -- 每用户一份当前 vault（覆盖式更新）
);
```

> 不存私钥、不存明文、不存 Recovery Key。仅托管元数据 + S3 指针。

### 2.2 S3 对象布局

```
bucket: imboy-e2ee-vault (私有, 禁公开读)
key:    vault/{uid}/{vault_version}.enc
```

恢复时后端签发 presigned GET URL，客户端直接从 Garage 下载密文。

---

## 3. API 设计（后端 Erlang，遵循 Handler→Logic→DS→Repo）

新增 `src/api/e2ee_vault_handler.erl`，路由注册到 `imboy_router.erl`：

| Method | Path | 用途 |
|--------|------|------|
| POST | `/v1/e2ee/vault/init` | 获取/创建 salt，返回 presigned PUT URL（上传密文 blob） |
| POST | `/v1/e2ee/vault/commit` | blob 上传完成后提交元数据（checksum/size/version） |
| GET  | `/v1/e2ee/vault/status` | 查询是否存在 vault + 元数据（salt/checksum/version） |
| GET  | `/v1/e2ee/vault/fetch` | 返回 presigned GET URL + salt + checksum（恢复用） |
| DELETE | `/v1/e2ee/vault/delete` | 删除 vault（用户主动注销/重置） |

**鉴权**：全部走 JWT，`uid` 从 token 取，禁止跨用户访问（不接受 uid 参数）。

**Logic 层**：`src/logic/e2ee_vault_logic.erl`
- `init_vault/1`：幂等创建 salt（已存在则复用），调用 Garage 模块签发 PUT presign。
- `commit_vault/2`：写 `e2ee_key_vault`（覆盖式 upsert），校验 blob_size 上限（如 ≤ 16KB，防滥用）。
- `fetch_vault/1`：签发 GET presign + 返回 salt/checksum。

**DS/Repo**：`e2ee_vault_ds.erl` / `e2ee_vault_repo.erl`，标准 CRUD，SQL 全参数化。

---

## 4. 客户端流程（Flutter）

### 4.1 新增 `e2ee_key_vault_service.dart`

```
备份 (backup):
  1. 已有 Recovery Key? 无则 generateRecoveryKey()
  2. vault/init → 拿 salt + presigned PUT URL
  3. master_key = Argon2id(recoveryKey, salt)
  4. vault_enc_key = HKDF(master_key)
  5. payload = {rsa_private_key, device_id, key_id, ...}
  6. ciphertext = AES-256-GCM(vault_enc_key, payload)
  7. PUT ciphertext → Garage (presigned)
  8. vault/commit(checksum, size, version)
  9. 引导用户保存/确认 Recovery Key（强制二次确认）

恢复 (restore):
  1. vault/status 判定存在
  2. 用户输入 Recovery Key
  3. vault/fetch → salt + checksum + presigned GET URL
  4. master_key = Argon2id(recoveryKey, salt)
  5. vault_enc_key = HKDF(master_key)
  6. 本地校验 checksum 匹配 → 否则提示 Key 错误（不下载）
  7. GET ciphertext ← Garage
  8. payload = AES-256-GCM-decrypt(vault_enc_key, ciphertext)
  9. 还原 rsa_private_key + device_id + key_id 到 secure storage
     （复用 e2ee_transfer_service 的 _decryptAndSaveKey 还原逻辑）
  10. 触发 ChatArchiveService.loadHistory() 拉 msg_store 历史
  11. 历史密文用还原的旧 did 解密 → 重试 _e2ee_failed 占位消息
      （复用 e2ee_health_check_service 的失败重试逻辑）
```

### 4.2 触发时机
- **备份**：首次生成 E2EE 密钥后引导开启；密钥轮换后自动更新 vault。
- **恢复**：新设备登录检测到 `other_device_count > 0` 或 `vault/status` 存在时，恢复引导弹窗新增"用 Recovery Key 恢复"入口（现有 `e2ee_recovery_guide_dialog.dart` 增加第 4 个选项）。

### 4.3 依赖
- Argon2id：需引入 `package:argon2`（或 `cryptography` 包，已含 Argon2id），评估纯 Dart 性能（移动端 64MB 内存参数约数百 ms，可接受）。
- 其余复用 `e2ee_crypto_service.dart` 现有 AES-GCM / HKDF。

---

## 5. 任务拆解（建议分 3 个 Sprint）

### Sprint 1 — 后端 Vault 基础（约 3-4 天）
- [ ] T1 migration：新增 `e2ee_key_vault` 表（时间戳版本号，参照迁移系统规范）
- [ ] T2 Garage：新增私有 bucket `imboy-e2ee-vault`，禁公开读，presign PUT/GET 封装
- [ ] T3 Repo/DS：`e2ee_vault_repo.erl` / `e2ee_vault_ds.erl` CRUD（参数化 SQL）
- [ ] T4 Logic：`e2ee_vault_logic.erl`（init/commit/fetch/delete，blob 上限校验）
- [ ] T5 Handler + 路由：`e2ee_vault_handler.erl`，注册 5 个端点，JWT 鉴权
- [ ] T6 EUnit：Logic 70% / Repo 80% 覆盖

### Sprint 2 — 客户端备份/恢复（约 4-5 天）
- [ ] T7 引入 Argon2id 依赖，封装 `deriveBackupMasterKey()`
- [ ] T8 `e2ee_key_vault_service.dart`：generateRecoveryKey / backup / restore
- [ ] T9 抽取 `e2ee_transfer_service` 的 `_decryptAndSaveKey` 还原逻辑为可复用方法
- [ ] T10 UI：Recovery Key 生成/确认页 + 恢复输入页 + 引导弹窗新入口
- [ ] T11 恢复后串联 `ChatArchiveService.loadHistory` + 失败消息重试
- [ ] T12 flutter test（密钥派生、加解密往返、checksum 校验）

### Sprint 3 — 加固与回归（约 2-3 天）
- [ ] T13 修复 `storage_secure.dart` Keychain accessibility / Android 清除策略显式配置
- [ ] T14 修复 `msg/history` 好友/群成员关系校验
- [ ] T15 更正 CLAUDE.md `msg_archive_enabled` 默认值描述，纳入 `deploy/preflight.sh` 强校验
- [ ] T16 真机 E2E：换机 / 删除重装 / 升级三场景全链路回归（必须真机，禁模拟器）
- [ ] T17（可选 Phase 2）双因子派生：叠加服务端随机种子

---

## 6. 安全审查要点（提交前必过）

- [ ] Recovery Key / 私钥 / master_key **绝不**出现在日志、网络请求体、服务端存储
- [ ] vault blob 上限校验（防存储滥用 DoS）
- [ ] presigned URL 短期过期（≤ 5 min）、绑定 uid 路径
- [ ] checksum 仅用于"Key 对错预判"，不可逆推明文
- [ ] vault/* 全部 JWT 鉴权，uid 取自 token，拒绝跨用户
- [ ] Argon2id 参数固定且随 alg_ver 版本化，便于未来升级
- [ ] 删除 vault 时同步删除 Garage blob（避免孤儿对象）

---

## 7. 验收标准

| 场景 | 验收 |
|------|------|
| 新手机首登 | 输入 Recovery Key → 30s 内历史消息可读，无「[加密消息]」残留 |
| 删除重装 | 同上，且不依赖 iOS Keychain 残留 |
| APP 升级 | 行为不变，无需恢复（密钥本地仍在） |
| Recovery Key 错误 | 本地 checksum 即时报错，不下载 blob |
| 无 Recovery Key | 明确提示历史不可恢复，可走旧的设备直传/社交恢复兜底 |

---

## 8. 范围外（明确不做 / 后续演进）

- HSM / SGX 飞地防暴力破解 → 高熵 Recovery Key 已规避需求，留作企业版卖点
- 完整消息库云备份 → 无必要（msg_store 已存密文）
- 切换到 Signal Protocol（Double Ratchet/前向保密）→ 独立大改造，本方案不涉及
- 媒体文件单独备份 → 媒体已在 Garage，恢复 viewUrl 授权即可访问
