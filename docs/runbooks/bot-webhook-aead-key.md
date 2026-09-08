# Runbook — Bot verify_token AEAD 主密钥：轮换 / 丢失恢复（WH-01-A07）

> 关联：PDT-01 webhook 契约 §2.2、ADR 信任边界、migration 00000092。
> 密文格式：`elib_cipher:aes_gcm_encrypt(plain, Key)`（AES-256-GCM，
> base64(salt16+iv12+ct+tag16)）；Key = sha256(postgre_aes_key)。
> fail-closed：主密钥缺失或解密失败 → 投递标 dead（`no_key`/`not_encrypted`/
> 解密错误），绝不降级读明文（明文列 `verify_token` 已在迁移截止期 v1.1 删除）。

## 1. 计划内轮换（双密钥窗）

1. 生成新密钥 `NEW`；将 `postgre_aes_key` 配置更新为 `NEW`（滚动重启节点前，
   先在灰度节点验证解密成功率）。
2. **双读窗**：`bot_repo:get_verify_token/1` 优先解 `verify_token_enc`；
   解密失败且行 `token_migrated=false` 时，触发一次性重加密：
   `bot_repo:set_verify_token_enc(BotId, LegacyPlain)`（LegacyPlain 仅存在于
   截止期前的明文列）。窗口期结束后执行第 3 步。
3. 批量重加密：`bot_webhook_delivery_logic:reencrypt_all()`（WH-01 后续补齐）
   或手工 SQL 导出明文 → 应用侧逐行 `set_verify_token_enc` → 置
   `token_migrated=true`。
4. 验证：`SELECT count(*) FROM bot WHERE token_migrated = false` = 0。
5. 旧密钥退役：旧节点全部滚动完成后，旧密钥从密管（1Password/ Vault）撤销。

## 2. 密钥丢失（fail-closed 演练）

1. 停止投递 worker（`gen_server:stop(bot_webhook_delivery_worker)`），
   避免解密失败风暴产生死信。
2. 确认影响面：所有 `verify_token_enc` 行不可解密 → Bot webhook 出站暂停
   （产品语义：出站通道不可用，IM 主功能不受影响）。
3. 恢复（仅当旧密钥可找回时）：找回旧密钥 → 逐 bot 轮换：
   `set_verify_token_enc(BotId, NewPlain)` 并同步通知开发者更新 verify secret
   （双端同换）。
4. 旧密钥无法找回：对每个受影响 Bot 执行「凭证重置」——生成新 verify secret
   （一次性展示）+ `set_verify_token_enc`，由运营通知开发者更新；未在 SLA 内
   更新者置 `disabled=true`。

## 3. 演练清单（每次发布前）

- [ ] 模拟密钥缺失（清空 postgre_aes_key）：worker 将到期的交付标 dead 且
      错误类 `no_key`，无明文降级路径被触发。
- [ ] 模拟密文损坏（UPDATE 一行密文为垃圾值）：该行解密失败 fail-closed，
      其余行不受影响。
- [ ] 演练轮换：按 §1 步骤在灰度环境完成一轮完整轮换并抽查签名可用。
