# P1 · E2EE 换设备历史消息恢复协议 — 设计方案

> 版本 2026-07-02 | 方法：两路只读 agent 独立 trace（证据互印）+ 主会话核实。全程未改代码。
> 执行分工：Fable 出方案，glm-5.2 盲执行。**协议方向须人工拍板，见文末 D。**

---

## 事实基础（已确证）

**密钥模型（per-device）**：
- `user_device` 表每 (uid, device_id) 一行，含 `public_key`(PEM) + `key_id`(kid)，**无 private_key 列**（`user_device_repo.erl:170-179` 注释"私钥永不落库"）。私钥只存客户端 `flutter_secure_storage`（`e2ee_private_key`）。
- 换设备/重装：`user_device_ds:update_public_key/5` 是 **UPDATE 覆盖**，旧公钥不留历史版本。

**消息加密信封（核心矛盾所在）**：
- 每条消息 = 一次性 AES-256 加密正文，AES 密钥用**每个接收方设备的 RSA 公钥逐个 wrap** 进 `e2ee.keys[]`（`{did, kid, wrap_alg:'RSA-OAEP-256', ek}`）。构建端 `e2ee_service.dart:203-280`。
- 解密端 `e2ee_service.dart:344` **按 `k['did'] == myDid` 匹配**自己的 entry，用 `getPrivateKeyByKid(kid)` 取私钥解 ek。
- **换设备的两道坎（确证）**：
  1. **数学坎**：历史消息的 `keys[]` 里只有"发送时刻"已知设备的 wrap 条目。新 device 的新密钥是发送后才生成的，**新私钥在数学上解不开任何历史密文**（`keys[]` 里没有新 did 的条目）。
  2. **匹配坎**：即使把旧私钥恢复到新设备，`e2ee_service.dart:344` 按 **did** 匹配（新设备 did 不同）→ 仍报 `'No key found for device'`。**恢复方案必须同时解决"旧私钥可达"+"解密按 kid 而非 did 匹配"**。

**密文历史已就绪**：
- `msg_store` / `msg_archive` 永久存储密文+完整 `e2ee` jsonb 信封（`msg_archive_enabled` 生产+本地实测均 `true`，`sys.config:94`）。
- 拉取端点 `/v1/msg/history`（游标 `after_seq`）原样透传 e2ee 字段。→ **历史密文+同步基础设施已完备，第四条恢复链路只缺"密钥"一环**。

**三条现有恢复链路都需外部锚点，目标场景必然落空**（`e2ee_recovery_logic.erl`）：
| 链路 | 前提 | 目标场景失效原因 |
|---|---|---|
| device_transfer (P1) | 旧设备在线且持旧私钥主动发起 | 无第二设备 → 失效 |
| social_recovery (P2) | ≥2 可信联系人提前设分片 | 无联系人 → 失效 |
| local_backup (P3) | 用户手里有 `.enc` 文件（服务端只存元数据） | 无本地文件 → 失效 |

→ "换机+无设备+无联系人+无文件" → `get_recovery_options/1` 返回 `[]` → `ERR_E2EE_RECOVERY_NO_OPTIONS`。**这是设计空白，非 bug。缺一条"不依赖任何外部锚点、靠服务端持有密钥"的路径。**

**可复用基础设施（确证）**：
- **Garage S3 presign 全链路现成**：`elib_s3_sign.erl`（presign_put/get/delete）+ `elib_oss.erl` 多桶封装 + `IMBOY_GARAGE_*` env override。`/v1/attachment/presign` 已用此做客户端直传。→ 备份密文直传/下载零新造。
- **compliance_key 表证明"服务端存加密私钥"模式已过审**：`compliance_key.private_key_encrypted` 列 + CRUD/revoke（`compliance_key_repo`）。客户端"给信封追加一个受托 recipient"的双 wrap 模式已跑通（`e2ee_service.dart:244-266`）。**但语义是"全局单一合规密钥"，非"每用户一份托管备份密钥"，复用需新表新语义。**
- **客户端本地备份加密格式可直接复用**：`e2ee_local_backup_service.dart` PBKDF2-HMAC-SHA256（310000 迭代）+ AES-GCM 加密私钥成 `.enc`。恢复入口 `e2ee_recovery_logic:get_recovery_options/1` + `start_auto_recovery/3` 加一个 `server_backup` 分支即可挂入现有编排。
- **客户端已有 kid→私钥历史链**（`storage_secure.dart:170` `getPrivateKeyByKid`），但只在本机 secure storage，换设备即断——**这条链就是自动备份要救的最小载荷**。

---

## 推荐方案：服务端零操作加密密钥备份（Recovery-Key 模式）

**核心思路**：给现有三条链路补第四条 `server_backup`，语义 = "客户端用一个用户持有的 Recovery Key 加密其私钥历史链，密文自动备份到服务端（Garage S3），换设备时用 Recovery Key 解回。服务端全程只存密文，永不接触明文私钥或 Recovery Key"——延续本项目"零信任服务端"一贯语义。

### 为什么是这个方向（对比拍板选项见 D）

| 维度 | 方案 X: Matrix-4S 风格 Recovery Key | 方案 Y: 私钥直导出到 S3（密码保护） |
|---|---|---|
| 安全 | 高。Recovery Key 是高熵随机串（用户抄写保管），服务端零知识 | 中。依赖用户设的密码强度，弱密码可暴力破解 |
| 可用 | 中。用户须妥善保管 Recovery Key（丢了仍不可恢复，但这是 E2EE 固有权衡） | 高。用户只需记密码 |
| 实现成本 | 中。复用 PBKDF2/AES-GCM 格式 + Garage presign + recovery 编排分支 | 低。几乎就是现有 local_backup 改"文件存 S3" |
| 零操作 | **是**（首次登录自动生成 Recovery Key 并提示抄写，之后每次密钥轮换自动增量备份） | 否（需用户主动设密码导出） |

**推荐方案 X**：它是唯一满足"零操作自动备份"（题目核心诉求）且保持强安全的路径。方案 Y 本质是现有 local_backup 的服务端托管版，可作为过渡/降级选项。

### 后端协议端点骨架（新增，挂入现有 e2ee 路由段 `imboy_router.erl:259-287`）

- **E2EE-BK-01 · 上传备份**：`POST /v1/e2ee/key_backup/upload`
  - 入参：`{backup_version, key_checksum, blob_object_key}`（客户端已把加密 blob 经 `/v1/attachment/presign` 直传 Garage，这里只登记元数据+object_key）。
  - Handler `e2ee_handler` 新增 action → `e2ee_key_backup_logic:save_backup(CurrentUid, Meta)` → 新 DS `e2ee_key_backup_ds` → 新表 `e2ee_key_backups`。
  - **表结构**（迁移 `00000019`，参照 `e2ee_local_backups` 但加 `object_key` 指向 Garage）：`id, uid, backup_version, key_checksum, object_key varchar, kdf_params jsonb, created_at`。**不存密文本体**（本体在 Garage，服务端零接触）。
- **E2EE-BK-02 · 拉取备份元数据**：`GET /v1/e2ee/key_backup/latest` → 返回 `{object_key, kdf_params, key_checksum}` + 一个 presigned GET url（`elib_oss:presign_get_for_key`）供客户端下载加密 blob。
- **E2EE-BK-03 · 挂入恢复编排**：`e2ee_recovery_logic.erl` 的 `get_recovery_options/1` 加 `check_server_backup_available/1`（查 `e2ee_key_backup_ds:find_latest(Uid)` 非空），优先级设为 **priority 2.5**（介于 social 和 local 之间：比社交省事，但需用户持 Recovery Key）。`start_auto_recovery/3` 加 `<<"server_backup">>` 分支返回 object_key + presigned url。

### 客户端集成点（imboyapp）

- **CLI-01 · 首次生成 Recovery Key**：首次登录/首次开 E2EE 时生成高熵随机 Recovery Key（如 12 词助记或 base58 串），弹窗提示用户**抄写保管**（一次性，之后不再显示明文）。复用 `e2ee_crypto_service.dart` 的 PBKDF2 从 Recovery Key 派生备份加密密钥。
- **CLI-02 · 零操作增量备份**：每次 `regenerateAndReportDeviceKey()`（`storage_secure.dart:167`）密钥轮换后，把最新 kid→privateKeyPem 历史链用备份密钥 AES-GCM 加密 → 经 `/v1/attachment/presign` 直传 Garage → 调 E2EE-BK-01 登记。载荷 = `storage_secure.dart:170` 的 kid→私钥链。
- **CLI-03 · 换设备恢复**：新设备触发恢复横幅（现有 `report_device_key` 的 `OtherDeviceCount>0` 已驱动）→ 走 `server_backup` 分支 → 提示用户输入 Recovery Key → 下载 blob 解密 → 把 kid→私钥链写入本机 secure storage 历史链。
- **CLI-04 · 修复"匹配坎"（关键，否则恢复也解不开）**：`e2ee_service.dart:344` 解密匹配逻辑从**按 did** 改为**按 kid 兜底**：先按 did 匹配（快路径），未命中则遍历 `keys[]` 用 `getPrivateKeyByKid(k['kid'])` 尝试解（历史消息路径）。这是恢复后能真正解开历史密文的必要改动。

### 验收 gate（EUnit + 真机）

- **后端 EUnit**（`test/logic/e2ee_key_backup_logic_tests.erl`，meck DS）：
  - `test_save_backup_persists_metadata_only`：save_backup 后 DS 存了 object_key/checksum，**断言无 private_key/blob 字段落库**（零信任验证）。
  - `test_latest_returns_presigned_url`：find_latest 返回含 object_key，logic 生成 presigned GET url 非空。
  - `test_recovery_options_includes_server_backup`：有备份时 `get_recovery_options` 含 `server_backup`；无备份时不含。
  - `test_recovery_options_empty_when_no_anchors`：四链路全无 → 返回 `[]`（回归保护，确认不破坏现有三链路）。
  - `make compile && make eunit` 绿。
- **迁移**：`00000019_e2ee_key_backups.up/down.sql` 真 PG 往返；序号递增（strict）。
- **客户端**：`flutter analyze` 零问题。
- **真机端到端（BLOCKED 真机）**：设备 A 发若干 E2EE 消息 → 卸载重装为"设备 B" → 输入 Recovery Key 恢复 → **拉历史消息能解密**（验证 CLI-04 kid 匹配生效）。这是唯一能证明"数学坎+匹配坎"双解的 gate。

### 边界（不能动）

- 不改现有三条恢复链路（device_transfer/social/local_backup）逻辑，只**新增**第四条。
- 服务端**永不接触**明文私钥/Recovery Key/备份密码——`e2ee_key_backups` 表零密文本体，blob 在 Garage 为客户端加密态。这是本项目零信任红线（`e2ee_local_backup_repo.erl:11-15` 已立此规矩）。
- 不复用 compliance_key 表（语义是全局合规监听，混用会破坏 E2EE 语义，见 P0 巡检 SEC-04）。
- `e2ee_social_ds:create_key_shares/5` 那条"服务端收明文私钥做 Shamir"的死代码路径（仅 EUnit 调用）**不要激活**，且本次可顺手标注为待删死代码。

### glm 执行陷阱

- 迁移 `00000019` 须带 down + 真 PG 往返；序号递增（erlang_migrate strict）。
- 客户端改 `e2ee_service.dart:344` 匹配逻辑属加密核心路径，改后必须真机验证历史消息解密（禁模拟器）；颜色/间距若涉 UI 走 token。
- 后端 erlfmt/DCO `-s`/`git restore --staged .` 精确 add；Garage bucket/前缀新建须同步 `deploy/` 配置。
- **勿把 blob 存进 PG**（YAGNI + 违背零信任存储边界），走 Garage object_key。

### 回滚条件

- 第四条链路是纯新增，不影响现有三链路，恢复编排失败即摘除 `server_backup` 分支回滚。
- CLI-04 匹配逻辑改动若真机暴露解密回归，回退为"仅 did 匹配"（恢复功能失效但不破坏现有加密），单独排查。

---

## D. 必须人工拍板（协议方向）

**BLK-E2EE · 换设备恢复协议方向** —— 三个选项（详见对话拍板问题）：

- **选项 A（推荐）· Matrix-4S 风格 Recovery Key**：高熵 Recovery Key + 服务端零知识加密备份到 Garage。零操作、强安全，实现成本中。满足题目"零操作自动备份"核心诉求。
- **选项 B · 密码保护私钥导出到 S3**：现有 local_backup 的服务端托管版，用户设密码。实现最省，但非零操作、弱密码风险，安全性依赖用户密码强度。
- **选项 C · 暂不做服务端备份，仅强化现有三链路引导**：换设备前强制引导用户设社交联系人/导出本地备份（提前布防）。零服务端存储风险，但仍无法覆盖"什么都没设就换机"场景——治标不治本。

**次要决策**：Recovery Key 形态（12 词助记 vs base58 串）、备份触发时机（每次轮换 vs 定时）、是否同时提供选项 B 作为降级路径。这些可在选定 A 后由 Fable 细化，不阻塞骨架。
