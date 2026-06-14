# E2EE 零信任重设计方案 —— transfer 与 social.decrypt_shard

> 立项日期：2026-06-14 ｜ 关联审计：[e2ee-backend-audit-final.md](./e2ee-backend-audit-final.md)（#5 待决项）
> 状态：**后端已实施（方案1）**，客户端/OpenAPI/测试待跟进（2026-06-14）
> 目标：消除"服务端持有/中转用户明文私钥"的零知识违反，统一为"私钥永不落库、加解密仅在客户端、服务端只中转密文"。

---

## 1. 问题陈述（现状违反零信任的两处）

### 1.1 transfer：服务端中转明文私钥
`e2ee_transfer_logic:create_transfer/5`（`:35-79`）当前流程：
```
handler 取发送方明文私钥 get_sender_private_key(CurrentUid)   ← 服务端持有明文私钥
      → get_receiver_public_key(ToUid)
      → create_transfer(..., PrivateKeyPem, ToPublicKeyPem)
            → encrypt_private_key(PrivateKeyPem, ToPublicKeyPem)  ← 服务端执行加密(:47)
            → 存 encrypted_key_bundle
```
**服务端在 `:47` 同时持有明文私钥并执行加密 → 彻底违反零知识。**

### 1.2 social：服务端解密代理分片
`e2ee_social_handler:do_decrypt_shard/2`（`:232-284`）：
```
get_proxy_shard(ShardId, CurrentUid) → extract_encrypted_shard
   → get_proxy_private_key(CurrentUid)              ← 服务端取代理私钥(:252)
   → elib_cipher:decrypt_rsa_oaep(Shard, PrivKey)   ← 服务端解密(:255)
   → 返回 decrypted_shard
```
**服务端取私钥并解密 → 违反零知识。**

> 现状两端点均因 `user_device` 表无 `private_key` 列、取私钥链路恒返回 `{error}` 而**永久失败**——无任何可用客户端依赖，故重设计**无存量兼容负担**。

---

## 2. 零信任原则（重设计契约）

| 资产 | 谁持有 | 服务端可见 |
|------|--------|-----------|
| 用户 RSA 私钥 | 仅本设备本地 | ❌ 永不 |
| 接收方/代理 RSA 公钥 | 任意方可取（`user_keys` 端点） | ✅（本就公开） |
| 加密密钥包 / 加密分片 | 服务端只存/转**密文** | ✅ 仅密文 |
| 加/解密运算 | 仅客户端 | ❌ 永不 |

---

## 3. transfer 重设计

### 3.1 API 契约变更：`POST /v1/e2ee/transfer/create`
**改动前** Body：`{to_uid}`（服务端去取私钥+公钥+加密）
**改动后** Body：
```json
{
  "to_uid": 123,
  "from_device_id": "<发送方设备ID>",
  "encrypted_key_bundle": "<客户端用接收方公钥加密自身私钥后的密文(base64)>",
  "key_wrap_alg": "RSA-OAEP-256"
}
```
- 发送方客户端职责（新增）：①调 `GET /v1/e2ee/user_keys?uid=to_uid` 取接收方公钥 → ②本地用该公钥加密自身私钥 → ③上传密文。
- 服务端职责（简化）：仅校验 `to_uid` 存在 + `from_uid≠to_uid` + 非空密文 → 直接存 `encrypted_key_bundle`。**不再取私钥、不再加密。**

### 3.2 后端改动清单
| 文件 | 改动 |
|------|------|
| `e2ee_transfer_handler:create_transfer/2`(`:70-144`) | 删除 `get_sender_private_key`(`:106`)、`get_receiver_public_key`(`:111`) 调用；从 Body 读 `encrypted_key_bundle`/`from_device_id`；非空校验 |
| `e2ee_transfer_logic:create_transfer`(`:35`) | 签名 `create_transfer(FromUid, FromDeviceId, ToUid, EncryptedBundle)`（去掉 `PrivateKeyPem`/`ToPublicKeyPem`）；删除 `encrypt_private_key/2` 调用(`:47`)，直接用入参 `EncryptedBundle` |
| `e2ee_transfer_logic:encrypt_private_key/2` | **删除**（服务端不再加密） |
| `e2ee_transfer_handler` | 删除 import/export `get_sender_private_key`、`get_receiver_public_key/1` 内部函数 |

- `accept_transfer`/`confirm_transfer`/`cancel`/`info`/`pending` 流程**不变**：接收方客户端 `accept` 取回 `encrypted_key_bundle`，用本地私钥解密（客户端侧），服务端只流转状态。
- 表 `e2ee_transfer_sessions.encrypted_key_bundle text`（迁移 `00000004:1072`）已支持，**无需迁移**。

---

## 4. social.decrypt_shard 重设计

### 4.1 API 契约变更：`POST /v1/e2ee/social/decrypt_shard`
**语义反转**：服务端不再解密，改为"取出加密分片供代理客户端本地解密"。
- 响应字段 `decrypted_shard` → `encrypted_shard`。
- 代理客户端拿到 `encrypted_shard` 后，用本地私钥 `RSA-OAEP` 解密（客户端侧），再把明文分片回传给恢复发起方（既有 Shamir 客户端本地重组路径 `recover_key`/`validate_shards`）。

> 可选：若 `get_proxy_shards`(`handle_action get_proxy_shards`) 已能返回 `encrypted_shard` 列表，则 `decrypt_shard` 端点可整体废弃、从路由移除。需先确认客户端是否单独依赖按 `shard_id` 取单片——建议保留端点改语义，降低客户端改动面。

### 4.2 后端改动清单
| 文件 | 改动 |
|------|------|
| `e2ee_social_handler:do_decrypt_shard/2`(`:232-284`) | 删除 `get_proxy_private_key`(`:252`)+`decrypt_rsa_oaep`(`:255`) 分支；`extract_encrypted_shard` 成功后直接 `success(#{<<"encrypted_shard">> => EncryptedShard})` |
| `e2ee_social_handler:get_proxy_private_key/1`(`:422-424`) | **删除** + export(`:5`)/dialyzer(`:5`) |
| `e2ee_social_logic:get_proxy_private_key/1`(`:227-240`) | **删除** + export(`:35`) |
| `e2ee_social_handler:get_sender_private_key/1`(`:426-437`) | **删除** + export(`:27`)（transfer 改造后无调用方） |

---

## 5. 统一删除清单（私钥取用链路）

重设计完成后，以下"取服务端私钥"函数全部无调用方，统一删除：
- `user_device_repo:get_private_key/2`（`:181-196`）、`update_private_key/3`（`:203-211`）+ export(`:20`)
- `user_device_ds:get_private_key/2`（`:176-177`）、`update_private_key/3`（`:179-182`）+ export(`:26-27`)
- `e2ee_social_handler:get_proxy_private_key/1`、`get_sender_private_key/1`
- `e2ee_social_logic:get_proxy_private_key/1`
- `e2ee_transfer_logic:encrypt_private_key/2`

> `user_device_repo:get_public_by_uid/1` 保留（已在审计中去掉 `private_key` 列），transfer/social 取设备公钥列表仍需要它。

---

## 6. 客户端配套（imboyapp / imboy-sdk-js）

| 端点 | 客户端新增职责 |
|------|---------------|
| transfer/create | 先取接收方公钥 → 本地加密自身私钥 → 传 `encrypted_key_bundle` |
| transfer/accept | 取回 `encrypted_key_bundle` → 本地私钥解密得发送方私钥 |
| social/decrypt_shard | 改读响应 `encrypted_shard` → 本地私钥解密 → 回传明文分片给恢复流程 |

- SDK（`imboy-sdk-js` e2ee 子模块）与 Flutter 端需同步更新请求/响应类型。
- 建议同时在 `docs/api/` 更新 E2EE 端点契约文档与 OpenAPI（关联记忆 `imboy-openapi-contract-sync`）。

---

## 7. 兼容与上线策略

- **无存量兼容负担**：两端点现状永久失败，无可用客户端，可直接切换为零信任契约，无需双跑/灰度。
- 建议在同一个 release 内：后端契约变更 + 客户端实现 + OpenAPI 更新一并交付。
- 上线前：`make compile && make eunit && make dialyze`；补 `e2ee_transfer`/`e2ee_social` 的契约层 EUnit（create 收密文、decrypt_shard 返密文、proxy_uid 校验、私钥函数已删后无 undefined 引用）。

---

## 8. 风险

| 风险 | 缓解 |
|------|------|
| 客户端未同步即上线 → transfer/social 不可用 | 因现状本就不可用，无回归；但需在 release notes 标注需客户端 ≥ 对应版本 |
| 删除函数遗漏调用方 → 编译 undefined | 删除后 `make dialyze` + 全量 `grep` 调用方核对（清单见 §5） |
| `decrypt_shard` 语义反转致字段名变更 | 客户端按新字段 `encrypted_shard` 解析；旧字段无人依赖 |

---

## 9. 工作量评估（后端）

- transfer：handler + logic 改造 + 删 `encrypt_private_key`，约 2 处函数重写。
- social：`do_decrypt_shard` 简化 + 删 2 个私钥函数。
- 统一删除：repo/ds/handler/logic 私钥函数链 8 处。
- 后端净改动适中（多为删除），主要成本在**客户端实现 + 契约文档/测试**。

---

## 10. 后端实施记录（2026-06-14，erlc +strong_validation 通过，未 git 提交）

| 模块 | 改动 |
|------|------|
| `e2ee_transfer_logic` | `create_transfer/5`→`/4`（去 `PrivateKeyPem`/`ToPublicKeyPem`，收 `EncryptedBundle`）；删 `encrypt_private_key/2`（服务端不再加密） |
| `e2ee_transfer_handler:do_create_transfer` | 改读 `from_device_id`+`encrypted_key_bundle`（非空校验），调 `create_transfer/4`；删 `get_sender_private_key/1`、`get_receiver_public_key/1` 及失效 dialyzer 属性 |
| `e2ee_social_handler:do_decrypt_shard` | 删服务端 `get_proxy_private_key`+`decrypt_rsa_oaep`，改为返回 `encrypted_shard`；删 `get_proxy_private_key/1`、`get_sender_private_key/1` + export/dialyzer |
| `e2ee_social_logic` | 删 `get_proxy_private_key/1` + export |
| `user_device_repo` | 删 `get_private_key/2`、`update_private_key/3` + export |
| `user_device_ds` | 删 `get_private_key/2`、`update_private_key/3` wrapper + export |

**验证**：6 文件 erlc 无 error；全仓 grep 确认无悬空调用（`user_device:*private_key` 调用清零、`create_transfer` 统一 `/4`）。`elib_cipher:encrypt_private_key/2` 为独立通用库函数，未触碰。

**结论**：后端已无"持有/中转明文私钥"的代码路径，零知识违反已消除。两端点在客户端实现新契约前仍不可用（与改造前一致，安全态势净改善）。

### 待跟进（未做）
1. **客户端**（imboyapp + imboy-sdk-js）：transfer/create 改传 `encrypted_key_bundle`+`from_device_id`；transfer/accept 本地解密；social/decrypt_shard 改读 `encrypted_shard` 后本地解密。
2. **OpenAPI/契约文档**：更新两端点请求/响应（关联 `imboy-openapi-contract-sync`）。
3. **测试**：补 `e2ee_transfer`/`e2ee_social` 契约层 EUnit；上线前跑 `make compile && make eunit && make dialyze`。
