# IMBoy E2EE 安全审计报告（唯一事实源）

日期：2026-09-07
状态：阶段 A 已完成；P0 与部分 P1/P2 本地修复已完成，授权后的攻击复测及其余 P1/P2 修复待执行。
配套执行清单：[`E2EE_ATTACK_MATRIX.md`](./E2EE_ATTACK_MATRIX.md)

本文件统一承载基线、消息路径、密钥所有权、Findings、修复状态和发布结论。旧报告、注释、测试名称及历史 PASS/GO 均不自动继承。

## 1. 证据与基线

证据等级：A=本轮隔离环境真实客户端→真实后端→真实存储→真实接收端；B=当前源码/配置/schema/可复现集成证据；C=单元或协议测试；D=文档/注释/历史报告。明确可达的安全边界违反直接判 FAIL；缺少必要运行证据判 UNKNOWN/PARTIAL/BLOCKED。

状态机：`OPEN → ROOT_CAUSE_CONFIRMED → FIXED → REGRESSION_PASS → ATTACK_RETEST_PASS → CLOSED`。

| 仓库 | 分支与 HEAD | 阶段 A worktree |
|---|---|---|
| `imboy` | `main` / `850af103d41d2f24ad8cf7827f816b9c7ca7d0ef` | dirty 8 项，含既有 E2EE 文档改动和未跟踪 `docs/security/` |
| `imboyapp` | `main` / `924347d011e84716a1b89a03cff10ac391010e69` | dirty 79 项，均按既有工作保留 |

依赖：Flutter 3.47.2、Dart 3.13.2、OTP 29、ERTS 17.0.2、GNU Make 3.81、`flutter_vodozemac 0.8.1`、`vodozemac 0.8.0`、`sqflite_sqlcipher 3.4.1`。销售 Compose/Helm 默认 `required`，策略无权威值时回落 `disabled`；运行节点实际模式未查，记 UNKNOWN。被忽略的 `config/sys.pro.config` 含敏感赋值特征但不在 HEAD，本轮未读取其值。

| 本轮证据 | 结果 | 等级与边界 |
|---|---|---|
| Flutter E2EE 定向测试 | 72/72 | C；PFv3、TOFU、策略、附件 AEAD、replay、fan-out |
| vodozemac 原生 FFI | 5/5 | C；Olm 建链、PFS/PCS ratchet、room-key-over-Olm |
| 后端定向 EUnit | 124 PASS / 1 FAIL | C；失败为产品 feature 顺序，非密码学断言，但门禁并非全绿 |
| 当前源码/配置/schema 链路追踪 | 完成 | B；不能替代 A 级黑盒证据 |
| Finding 001 定向回归 | 50/50 PASS，编译与格式检查通过 | C；真实 PostgreSQL/前成员攻击复测仍 BLOCKED |
| Finding 002 定向回归 | 28/28 PASS | C；工作区移除后的真机通知、rotation 与旧 session 攻击复测仍 BLOCKED |
| Finding 003 定向回归 | 11/11 PASS；定向 analyze 与 diff check 通过 | C；强刷空结果/异常均不复用旧设备，真实撤销攻击复测仍 BLOCKED |
| 群会话回归 | 25/25 PASS | C；既有合规测试夹具意外访问默认生产只读端点，故不作为隔离环境或 A 级证据 |
| Finding 004 定向回归 | logic 12/12、handler 11/11 PASS；编译与格式检查通过 | C；非活跃成员在归档查询前被拒绝，真实前成员 API 复测仍 BLOCKED |
| Finding 010 定向回归 | 上传接线 18/18、策略/封装 20/20 PASS；定向 analyze 通过 | C；策略未知在上传前抛错，真实对象存储 Canary 复测仍 BLOCKED |
| Finding 008 定向回归 | Safety Number 入口 3/3、关联威胁模型 24/24 PASS；定向 analyze 与 diff check 通过 | C；双方真实 device ID/Olm identity 聚合且验证状态绑定当前号码，双真机换钥与设备增删复测仍 BLOCKED |
| Finding 014 定向回归 | 日志边界守卫 1/1、关联 WS/ACK/离线解密 27/27 PASS；脱敏后守卫+WS 复跑 21/21 PASS；定向 analyze 通过 | C；消息链路已禁止记录原始帧、完整异常、明文 payload/preview、会话对象与标题；真机与后端日志 Canary 扫描仍 BLOCKED |
| Finding 009 定向回归 | SQLCipher 边界 13/13、数据库迁移/快照/schema/uid 隔离关联回归 59/59 PASS；定向 analyze 与 diff check 通过 | C；加密平台不再无密码探测/回退，不再创建或自动清理明文迁移备份，错钥/明文/损坏统一保留原库并停止初始化；Android 真机错钥、文件字节与 sidecar 复测待目标设备确认 |

## 2. 真实消息路径

**C2C 发送**：Composer → `MessageModel` → `ChatNetworkService.sendWsMsg/sendMessage` → `_shouldEncryptOutbound` → `encryptPayload` → `_encryptC2COlmFanOut` → `E2eeOutboundRouter.encryptV3` → `OlmProtocol.encrypt` → `OlmSessionService.encryptC2CMessage` → 最终 `Session.encrypt`（`imboyapp/lib/service/olm_session_service.dart:642`）→ WS。

**C2C 接收**：WS → `MessageService` → `E2EEService.decryptInboundV3` → `OlmProtocol.decrypt` → `OlmSessionService.decryptC2CMessage` → prekey 首包 `Account.createInboundSession` / 普通包最终 `Session.decrypt`（`:869`）→ 本地 DB/UI。

**C2G 发送**：同一入口 → 群级策略门 → `GroupSessionService.ensureOutboundSession` → `E2eeOutboundRouter.encryptV3` → `MegolmProtocol.encrypt` → 最终 `GroupSession.encrypt`（`imboyapp/lib/service/group_session_service.dart:209`）→ WS；room key 对每个设备经 Olm 包裹分发。

**C2G 接收**：WS → `MessageService` → `E2EEService.decryptInboundV3` → `MegolmProtocol.decrypt` → `GroupSessionService.decryptGroupMessage` → 最终 `InboundGroupSession.decrypt`（`:605`）→ 本地 DB/UI。

**服务端/离线**：WS handler → `message_router_logic` → `msg_c2c_logic/msg_c2g_logic` → deployment/group policy gate → `msg_store` staging → worker → 正式表/归档 → 在线投递或离线拉取 →客户端 ACK。schema 支持密文不等于实际数据库、日志、备份和 Push 已无明文。

## 3. 实际状态机与安全上限

- C2C：设备生成 Olm Account → 上报 identity/OTK/签名 fallback → claim OTK（耗尽时用签名 fallback）→ 验签与 TOFU → 建立/恢复 per-device session → PFv3+Olm 加密；ratchet 与 outbox、dedupe 与接收 ratchet 分别在 SQLCipher 事务中提交。设计上不降级到 RSA/明文。
- C2G：发送端内存维护 outbound Megolm → 导出 room key → Olm 逐设备包裹 → 接收端保存 inbound pickle → 按成员/设备变化、100 条、7 天或进程重启轮换。当前源码已补齐成员撤销传播并使强刷失败/空结果 fail-closed，本地回归通过；真实设备撤销链仍待 A 级复测。
- Megolm rotation 不等于 Double Ratchet PCS。离群后保密依赖“成员撤销传播→新设备快照→下一条消息前轮换”完整成立；当前仅有 B/C 级实现与回归证据。
- 新成员/重新加入是否可读历史尚无明确产品策略，记安全设计缺口。

## 4. 密钥所有权

| 材料 | 客户端生成/存储 | 服务端可见 | 恢复与生命周期 |
|---|---|---|---|
| Olm identity/account 私态 | vodozemac；Secure Storage 中 account pickle/pickle key | Ed25519/Curve25519 公钥 | 不入备份；清数据重建；logout 清理 |
| OTK/签名 fallback 私态 | account pickle | 公钥、签名、claim 状态 | OTK 原子消费；fallback 轮换 |
| Olm session/message key | SQLCipher CryptoStore / ratchet 瞬态 | 预期不可见 | session 不备份，防棘轮分叉；message key 不导出 |
| Megolm outbound | 发送设备内存 | 预期不可见 | 轮换或重启重建 |
| Megolm inbound/room key | 接收设备 Secure Storage | 仅见 Olm 包裹帧 | 保留解历史；进入加密备份 |
| 附件 content key | 客户端 CSPRNG；descriptor 应在 PFv3 内 | 新链路预期不可见 | 每附件独立；解密产物可进入普通缓存 |
| 备份 key | 用户口令 KDF 派生 | 密文、salt、KDF 参数 | 派生 key 不保存；备份不含 Olm account/session/TOFU pin |
| legacy RSA 私钥 | Secure Storage + 加密备份 | 公钥；历史迁移窗口可能可解 | 仅历史解密意图 |
| compliance key | 客户端拉取并 TOFU pin 公钥 | 公钥 | 私钥实际保管方和运行保护 UNKNOWN |
| SQLCipher key | Secure Storage | 预期不可见 | 当前源码已移除无密码探测/回退及明文备份生成；历史备份 artifact 的盘点/清理由设备与数据范围授权后执行 |

## 5. Findings 与修复状态

| ID | 级别 | 根因/影响 | 状态与关闭条件 |
|---|---|---|---|
| E2EE-2026-001 | P0 | `group_member_repo:find/3` 与成员列表未过滤 `status=1`；停用行被群消息、群密钥、附件和多类群 ACL 当成活跃成员 | REGRESSION_PASS；共享查询和列表已限定 active，显式重入改为原子恢复；移除成员攻击复测待授权 |
| E2EE-2026-002 | P0 | 工作区移除只停用下属群记录，未逐群发 leave、清缓存或触发 session stale | REGRESSION_PASS；事务提交后已逐群清缓存并发布持久 leave 通知；工作区移除与旧 session 攻击复测待授权 |
| E2EE-2026-003 | P0 | 群设备密钥强刷失败/为空时复用最长 30 分钟旧缓存 | REGRESSION_PASS；强刷空结果覆盖旧缓存，最终异常清空整组缓存并抛出；旧设备/旧 session 攻击复测待授权 |
| E2EE-2026-004 | P1 | C2G history 仅构造 `c2g:<gid>`，不校验当前活跃成员 | REGRESSION_PASS；共享 history 入口已要求当前 active 成员并在归档查询前拒绝；前成员 API 攻击复测待授权 |
| E2EE-2026-005 | P1 | 客户端在 PFv3 认证、解密、持久化前 ACK；实时与离线两条路径同源 | ROOT_CAUSE_CONFIRMED；必须与 006/007 共同建立入站原子提交后再 ACK，不能只删 WS ACK→离线/畸形帧复测 |
| E2EE-2026-006 | P1 | PFv3 解密失败未形成有界、可重启恢复的密文状态；Olm ratchet 已提交而消息明文尚未提交时存在崩溃丢信窗口 | ROOT_CAUSE_CONFIRMED；原始密文、ratchet/dedupe 与最终消息形成可恢复提交协议→重启恢复回归 |
| E2EE-2026-007 | P1 | C2G 无持久 replay 状态；C2C `dedupeAndPersistSession` 捕获全部异常并返回 duplicate，把存储事故与重放混同 | ROOT_CAUSE_CONFIRMED；分离错误并统一持久防重，且与 005/006 的提交顺序一起修复→replay/存储故障复测 |
| E2EE-2026-008 | P1 | Safety Number 入口把 legacy RSA key/kid 当 Olm identity/device ID，本端 device ID 为空，且本地“已验证”未绑定当前号码 | REGRESSION_PASS；双方均取活跃 Olm device ID，identity 走本地权威或签名+TOFU 路径，验证值绑定当前聚合码；双真机换钥/增删设备攻击复测待授权 |
| E2EE-2026-009 | P1 | SQLCipher 密码打开失败后尝试 `password:null`；明文探测成功后复制 `.plain.bak` 并删除原库，备份最长保留 7 天 | REGRESSION_PASS；加密平台已有库只用当前 key 验证，失败保留原库并终止；已移除无密码探测/二次打开、备份生成和自动清理；Android 真机错钥/明文库/sidecar/历史 artifact 取证复测待明确设备与数据范围 |
| E2EE-2026-010 | P1 | 附件策略查询异常返回“不封装”，先明文上传、后由消息门拒发 | REGRESSION_PASS；不再吞策略异常，策略未知会在调用上传 API 前中止；对象存储 Canary 攻击复测待授权 |
| E2EE-2026-011 | P1 | 备份不含 Olm account/session/TOFU pin，清数据后身份连续性与 C2C 历史丢失 | ROOT_CAUSE_CONFIRMED；明确产品边界，禁止克隆 ratchet |
| E2EE-2026-012 | P1 | 新成员/重入群历史访问策略未定义 | OPEN；产品拍板→实现/文档→生命周期复测 |
| E2EE-2026-013 | P2 | 规范声明 Signed Capabilities，双端未完整实现 | OPEN；降级为未实现声明或补签名/验签 |
| E2EE-2026-014 | P2 | debug 路径可记录完整 WS/解密后 Conversation payload，解析异常文本也可携带输入片段 | REGRESSION_PASS；已移除完整 WS 开关并将消息/会话/离线/S2C/解密日志收窄为非敏感元数据和异常类型；真实 Canary 日志扫描待授权 |

安全边界 finding 未完成对应攻击复测不得 CLOSED。

## 6. 声明仲裁与文档漂移

| 声明 | 当前结论 |
|---|---|
| C2C Olm/PFv3/per-device | B/C 支持；当前双真机 A 级证据缺失，PARTIAL |
| C2G Megolm/room-key-over-Olm | B/C 级协议、成员撤销传播、密钥强刷与 history ACL 回归通过；A 级撤销复测缺失且新成员历史策略未定义，PARTIAL |
| required/compliance 全链拒绝明文 | 消息门、群密钥强刷和附件上传前策略本地路径均 fail-closed；真实 Transport/对象存储证据仍缺失 |
| legacy RSA 仅历史解密 | 编排符合意图；新消息降级攻击未复测，PARTIAL |
| 附件上传前 AES-256-GCM | B/C 级内核与策略异常 fail-closed 回归通过；对象存储、缩略图和临时文件 A 级证据缺失，PARTIAL |
| Push 固定占位 | B 级正文占位；昵称/群名 metadata 和真实 provider payload 待查，PARTIAL |
| 服务端零知识 | B 级源码/schema 支持；实际 DB/日志/备份/历史窗口未查，UNKNOWN |
| 私钥保护、Replay、MITM、多设备 | SQLCipher 无密码降级与新明文备份路径已修复并通过 C 级回归，但历史 artifact 和 Android/iOS 真机提取抗性未复测；C2G replay 仍有明确缺口；Safety Number 入口本地回归通过，但双真机换钥/设备增删及“聚合号码验证只上报首个设备”边界未完成 A 级验证；多设备整体仍为 PARTIAL |

漂移：旧红队 GO 早于当前 PFv3 群协议；旧审计声称 P0=0/P1=0 和全部降级 fail-closed；Safety Number 文档声称全设备聚合；部分规范仍描述旧 RSA/vodozemac；销售默认值不能证明运行配置。当前架构声明不使用 Redis，除非授权目标额外引入，否则 Redis 项为 N/A。

## 7. 运行缺口与发布门

未获授权前，下列项保持 UNKNOWN/BLOCKED：两用户 C2C、四用户 C2G 真机；Canary 搜索 Transport/PG/队列/日志/备份/对象存储/Push；篡改、MITM、replay、乱序、重连、重启；identity/prekey/session/设备撤销；群加入/退出/移除/重入；Android Keystore、iOS Keychain、附件临时文件和缓存。现有账号、设备、端口、进程或生产地址均不构成默认授权。

当前发布姿态：**NO-GO**。原因是三个 P0 均未完成攻击复测、P1/P2 未解决、当前后端门禁非全绿、A 级证据缺失。最终固定 verdict 仅在修复、回归和授权后的攻击复测完成后写入；此前本文件不提供任何最终发布 PASS。
