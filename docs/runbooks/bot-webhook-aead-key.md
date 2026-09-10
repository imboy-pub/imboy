# Runbook — Bot verify_token AEAD 主密钥：轮换 / 丢失恢复（WH-01-A07）

> 关联：PDT-01 webhook 契约 §2.2、ADR 信任边界、migration 00000092。
> 密文格式：`elib_cipher:aes_gcm_encrypt(plain, Key)`（AES-256-GCM，
> base64(salt16+iv12+ct+tag16)）；Key = sha256(postgre_aes_key)。
> fail-closed：主密钥缺失或解密失败 → 投递标 dead（`no_key`/`not_encrypted`/
> 解密错误），绝不降级读明文。`IMBOY_POSTGRE_AES_KEY_OLD` 只在 Bot 密钥轮换窗
> 内用于解密旧 AEAD 密文，不用于新写入，也不开放给其他密文消费者。

## 1. 计划内轮换（双密钥窗）

1. 生成 `NEW`，在全部节点同时设置 `IMBOY_POSTGRE_AES_KEY=NEW` 和
   `IMBOY_POSTGRE_AES_KEY_OLD=OLD`，再滚动重启。新写入只使用 `NEW`。
2. **双读窗**：`bot_repo:get_verify_token/1` 先用 `NEW` 解密；仅认证失败时尝试
   `OLD`。旧 key 解密成功后必须立即用 `NEW` 重加密并恰好更新 1 行；更新失败
   返回 `reencrypt_failed`，不得继续发送 webhook。
3. 在单个受控节点执行 `bot_webhook_delivery_logic:reencrypt_all()`。只有返回
   `{ok, #{processed := N, failed_bot_ids := []}}` 才允许继续；失败列表不得忽略。
4. 确认所有服务节点均已运行新版本和 `NEW`，再移除
   `IMBOY_POSTGRE_AES_KEY_OLD` 并滚动重启。旧 key 不得长期驻留。

禁止用 SQL 导出 verify secret 明文；`token_migrated` 只表示历史明文列已清理，
不能作为本次 AEAD key 轮换完成的证据。

## 2. 密钥丢失（fail-closed 演练）

1. 停止投递 worker（`gen_server:stop(bot_webhook_delivery_worker)`），
   避免解密失败风暴产生死信。
2. 确认影响面：所有 `verify_token_enc` 行不可解密 → Bot webhook 出站暂停
   （产品语义：出站通道不可用，IM 主功能不受影响）。
3. 恢复（仅当丢失 key 可找回时）：恢复为 `IMBOY_POSTGRE_AES_KEY_OLD`，配置一把
   新当前 key，执行 §1 的双密钥批量重加密；不需要读取或导出 secret 明文。
4. 旧密钥无法找回：对每个受影响 Bot 执行「凭证重置」——生成新 verify secret
   （一次性展示）+ `set_verify_token_enc`，由运营通知开发者更新；未在 SLA 内
   更新者置 `disabled=true`。

## 3. 演练清单（每次发布前）

- [ ] 模拟密钥缺失（清空 postgre_aes_key）：worker 将到期的交付标 dead 且
      错误类 `no_key`，无明文降级路径被触发。
- [ ] 模拟密文损坏（UPDATE 一行密文为垃圾值）：该行解密失败 fail-closed，
      其余行不受影响。
- [ ] 模拟旧 key 命中但重加密写入失败：返回 `reencrypt_failed`，不发送 webhook。
- [ ] 演练轮换：`reencrypt_all/0` 失败列表为空，移除旧 key 后签名仍可用。
