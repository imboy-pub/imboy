# ADR 19 — MLS 1.0 群聊目标架构与 Go/No-Go

> **状态**：Proposed
> **拟替代**：ADR 09 R3“本轮不实现 MLS”的决定；ADR 02/04/11 的 MLS 占位与群聊长期路线
> **保留**：Protocol Registry 解耦思想；Megolm 历史 decrypt-only 与迁移期 Strong Preview
> **规范依据**：RFC 9420、RFC 9750

---

## 1. Revisit 条件已满足

ADR 09 R3 允许在成熟实现可用或业务需要大型群高效成员变更时重开。当前目标已从“完成 v2 基础能力”提升为“达到行业顶级产品水平”，而 sender-key/Megolm 难以为群组提供可扩展、可证明的 PCS；RFC 9420 已标准化，OpenMLS 等维护中的 Rust 实现可用于受控 FFI spike。

因此决定：**MLS 1.0 是 Strict Group 的 GA 目标**。在 Go/No-Go 验证和安全门完成前，不直接承诺生产切换。

---

## 2. 协议边界

### 2.1 成员模型

- 一个 MLS leaf 对应一个物理设备，不对应一个账号。
- leaf credential 绑定 ADR 16 的 `(uid, device_id, device_generation, identity_version)` 和 Device Manifest hash。
- 加人/移除授权由 IMBoy 群成员策略决定，但 Proposal/Commit 必须由合法 MLS member 签名并验证服务端业务授权证明。
- 同账号新增设备按新增 leaf 处理；不得复制已有 leaf 私钥/state。

### 2.2 组件职责

| 组件 | 职责 | 禁止 |
|---|---|---|
| Flutter/Dart | 产品状态机、UI、网络调度、CryptoStore 事务边界 | 实现 tree math/自研密码学 |
| Rust crypto core | RFC 9420 编解码、key schedule、proposal/commit、secret tree | 网络、token、业务权限判断 |
| Erlang Delivery Service | KeyPackage/Welcome/Proposal/Commit/应用消息存储转发、配额 | 解密、生成 leaf 私钥、替客户端提交 Commit |
| Authentication Service | Device Manifest/credential 验证与透明度证明 | 以服务端声明替代账号签名 |

建议在 `imboyapp/packages/imboy_crypto` 内建立 Rust core，通过 `flutter_rust_bridge` 或等价窄 FFI 暴露；是否采用 OpenMLS 必须由 Spike 结果决定，不在 ADR 中盲目锁库。

---

## 3. Go/No-Go Spike

Spike 不接业务 UI，只验证最难、最可能失败的条件：

| Gate | Go 条件 | No-Go 条件 |
|---|---|---|
| 标准一致性 | RFC 官方/项目维护向量全通过 | 需修改协议语义或跳过向量 |
| 独立互操作 | 与另一 RFC 9420 实现完成 create/add/update/remove/app round-trip | 只能自身互操作 |
| 移动支持 | iOS/Android 真机静态/动态库构建、后台恢复、升级均通过 | 依赖不支持目标 ABI/平台 |
| 持久化 | state 可加密序列化，事务失败可恢复，无 secret clone | 必须先提交内部状态才可知道输出且无法回滚 |
| 性能 | 1000 leaf Commit p95 ≤2s，峰值内存可接受 | 超预算且无清晰优化路径 |
| 维护性 | 依赖有活跃维护、安全响应、可锁定/审计许可证 | abandoned、不可审计生成代码或许可证冲突 |
| fuzz | FFI/codec 10 万输入无 crash/OOM/UB | 可从网络输入触发 panic/越界 |

No-Go 不表示回到不安全宣传：Megolm 继续 `Strong Preview`，公开缺少群 PCS 的限制，同时评估另一个成熟 MLS 实现。禁止自研 MLS 替代。

---

## 4. MLS Profile

最终 cipher suite 由 Spike 和两端硬件能力选定，必须是 RFC 9420 注册套件且依赖库完整支持。Profile 冻结以下行为：

1. `group_id` 为随机 256-bit 值，不直接使用可枚举 gid；受认证映射绑定业务 gid。
2. 应用消息使用 PrivateMessage；握手消息默认 PrivateMessage，必要公开控制消息需单独 ADR。
3. Protected Frame header hash 放入 `authenticated_data`。
4. KeyPackage 一次性消费，绑定 device manifest/identity version，设置过期时间和 ciphersuite。
5. 每次成员/设备增加、移除、撤销必须形成 Commit；移除设备不能收到新 epoch secrets。
6. 定期 Update：至少按消息数、时间或安全事件触发；具体阈值经性能/风险基准冻结。
7. past epoch secrets 仅按有界乱序窗口保留，超过后安全删除；历史长期读取走 ADR 17 的只读归档策略。
8. Welcome 只能发给已授权、透明度验证通过的设备 credential。

---

## 5. 顺序、分叉与事务

- 每群每设备维护唯一活跃 `(group_id, epoch, state_hash)`。
- Proposal 可排队但有总数/大小/时效上限；Commit 严格验证 confirmed/interim transcript hash。
- 收到 epoch gap：在小窗口内拉取缺失 handshake；超限执行受认证 resync，不盲目导入服务端快照。
- 同 epoch 冲突 Commit：按 RFC 状态机处理；业务层不得“最后写入覆盖”。分叉产生安全事件。
- 生成 Commit/Application ciphertext、更新 MLS state、写 outbox 必须在一个 CryptoStore 事务中提交；网络发送只读已提交 outbox。
- ACK 只更新传输状态，不回滚 crypto state；重发复用已提交密文，不重新加密。

---

## 6. 服务端接口与抗 DoS

服务端新增协议无关对象 API：

- publish/claim KeyPackage；
- append/fetch ordered handshake messages；
- fetch Welcome by authorized target device；
- append/fetch application ciphertext；
- query group delivery cursor，不返回客户端私密 state。

硬限制：单 KeyPackage/handshake/application 尺寸、每群 pending proposals、epoch gap、每设备发布量、claim 速率、Welcome 保存期。所有写入要求 device-bound session、operation signature 和 idempotency key。

服务端业务成员变更与 MLS Commit 的关系要可审计：服务端可以拒绝非法群操作，但不能制造一个客户端会接受的未签名成员变更。

---

## 7. Megolm → MLS 迁移

1. 客户端先发布 MLS read/write capability，但默认关闭。
2. 仅新建测试群启用；达到 Spike/CI/外审门禁后扩展到新建群。
3. 既有群迁移创建新的随机 MLS group，由当前已验证设备集合生成首次 Commit/Welcome。
4. 群内发送受认证 `migration_event`，绑定旧 gid/session 与新 MLS group/epoch；达到成员确认门槛后停止新 Megolm 写入。
5. Megolm session 永久 decrypt-only 用于历史；不得将 Megolm sender key 注入 MLS PSK 作为信任捷径。
6. 回滚只能暂停 MLS 新写入并修复/恢复同一 MLS state；不得在同一安全会话中静默回写 Megolm。若必须新开 Megolm Preview 会话，UI 必须显示安全等级下降并由产品负责人批准。

---

## 8. 验收测试

| ID | 用例 | 通过条件 |
|---|---|---|
| MLS-01 | RFC/实现维护的全部适用向量 | 100% 通过，无本地特殊分支 |
| MLS-02 | 与独立实现 create/add/update/remove/application | 双向互操作 100% |
| MLS-03 | 新成员加入 | 不能解密加入前应用消息 |
| MLS-04 | 成员/设备移除 | 不能解密 Commit 后消息 |
| MLS-05 | 设备状态泄漏后完成安全 Update/Commit | 泄漏状态不能解密恢复后的消息，PCS 测试可证明 |
| MLS-06 | replay/reorder/delay/duplicate | 按窗口接受或拒绝，无重复业务提交/epoch rollback |
| MLS-07 | 同 epoch 冲突 Commit/fork | 检测并阻断应用消息，不最后写覆盖 |
| MLS-08 | 恶意服务端替换 leaf credential/Welcome/group_id | manifest、签名或 context 验证失败 |
| MLS-09 | kill 注入 Commit 的每个持久化边界 | state/outbox 原子，无 key reuse/永久分叉 |
| MLS-10 | 1/10/100/1000 leaf 真机性能 | 满足 ADR 14 预算并归档曲线 |
| MLS-11 | KeyPackage 重复 claim/过期/耗尽 | 不重复使用，不降级，限流可观测 |
| MLS-12 | FFI/codec fuzz 100,000 样本 | 无 crash、UB、OOM、secret 进入日志 |
| MLS-13 | iOS/Android 前后台、升级、数据库迁移 | 历史/当前 epoch 一致，失败可恢复 |
| MLS-14 | 合规模式 | 仅采用 ADR 18 批准模型；无自定义 secret export 旁路 |

---

## 9. 外审与生效

MLS 代码在进入生产 Strict Group 前必须由具备 MLS/现代消息协议经验的独立团队审计，至少覆盖 Rust core、FFI、credential binding、持久化事务、迁移和恢复。

- [ ] Spike 全部 Go；报告含依赖版本、平台、基准和未解决风险
- [ ] Cipher suite/Profile 另行冻结并签字
- [ ] MLS-01..14 Critical 用例 0 skip
- [ ] 外审 Critical/High 未修复为 0
- [ ] Megolm 迁移与紧急停止 runbook 演练通过

接受本 ADR 只授权 Spike 和后续受门禁实现，不等于立即宣告 MLS GA。

---

## 10. 参考

- [RFC 9420 — The Messaging Layer Security Protocol](https://www.rfc-editor.org/rfc/rfc9420.html)
- [RFC 9750 — The Messaging Layer Security Architecture](https://www.rfc-editor.org/rfc/rfc9750.html)
- [OpenMLS — Rust MLS implementation](https://github.com/openmls/openmls)
- [Matrix.org — libolm deprecation and vodozemac direction](https://matrix.org/blog/2024/08/libolm-deprecation/)
