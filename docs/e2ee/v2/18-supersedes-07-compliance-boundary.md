# ADR 18 — Strict 与 Compliance 的信任边界

> **状态**：Proposed
> **拟替代**：ADR 07/13 中合规接收方的信任与 fallback 语义
> **保留**：服务端不持有合规私钥、服务端不解密消息的既有决定
> **依赖**：ADR 14–17

---

## 1. 决策

`strict` 和 `compliance` 是两种不同的安全产品，不共享同一个“顶级 E2EE”徽章：

| 模式 | 内容接收方 | 信任根 | 缺钥/过期行为 | 可对外表述 |
|---|---|---|---|---|
| Strict | 会话中已授权用户设备 | 用户账号根 + 设备透明度 | 拒发 | 端到端加密 |
| Compliance | 上述设备 + 明确列出的组织审计设备 | 用户账号根 + 组织合规根 | 拒发 | 带组织审计接收方的端到端加密 |
| Optional | 由用户明确选择 | 视具体消息 | 可显式明文 | 不得显示 E2EE 徽章 |

Compliance 不是“后门”：审计接收方是消息的显式端点，私钥只在审计端点；但它的可读主体比 Strict 多，因此不能宣传为等价隐私边界。

---

## 2. Compliance Key Manifest

组织策略必须提供受认证清单：

```text
ComplianceKeyManifest {
  version,
  tenant_id,
  policy_id,
  key_id,
  algorithm,
  public_key,
  valid_from_ms,
  expires_at_ms,
  purpose,
  jurisdiction,
  previous_manifest_hash,
  signer_key_id,
  signature
}
```

- 根公钥通过 MDM、管理员现场 QR 或应用构建 pin 引导；不能只从同一 API 下载后立即信任。
- 轮换 manifest 由当前有效 key 或离线组织根签名，并形成哈希链。
- 客户端验证租户、用途、算法、key 长度、有效期、链和签名；缓存只在有效期内使用。
- 撤销/到期/无法获取新清单时 fail-closed；不得使用陈旧 key 静默发送。
- 服务端仅保存 public manifest，不允许任何 schema/API 字段接收私钥。

---

## 3. 消息绑定与用户可见性

Compliance 的 Protected Frame 必须绑定：

- `mode=compliance`；
- `tenant_id/policy_id`；
- `compliance_key_id` 和 manifest version/hash；
- 审计 wrap/MLS member 的目标身份；
- 用户设备接收方集合摘要。

客户端在发送前、会话详情、导出和转发界面持续显示“组织审计方可读取”，不能只在首次登录弹一次提示。模式改变视为安全边界改变，必须中断会话并生成可见系统事件。

---

## 4. 密码学接入

- C2C Olm：业务内容使用随机 content key 加密；content key 分别封装给用户设备和合规公钥，二者均绑定同一 Protected Frame hash。不得把 Olm plaintext 单独复制后使用未绑定 RSA blob。
- Megolm Preview：合规 entry 与用户 room key entry 同属受认证 inner frame；缺少或无效即整条消息拒发。
- MLS：合规审计方应建模为明确的组织审计 leaf/bot device，或使用经独立审计的外部发送/接收扩展；未完成标准安全评审前不得自创 MLS 密钥导出旁路。
- 服务端不执行 unwrap/decrypt，不接收合规私钥，不提供“管理员临时解密”API。

合规审计公钥若为长期静态 RSA/HPKE key，则相对于审计方不具备与 Strict 用户设备相同的 FS/PCS：审计私钥未来泄漏可能暴露其被封装的历史 content keys。产品必须公开此差异；若组织要求更强属性，应把审计方实现为可轮换、可撤销的独立设备/MLS leaf，并缩短密钥周期，不能用文案掩盖。

---

## 5. 策略初始化和离线行为

1. 应用启动先加载并验证本地最高版本策略，再允许构造任何 E2EE 消息。
2. 策略状态只有 `valid / unavailable / expired / rollback / signature_error`；后四者在 strict/compliance 均拒发。
3. 离线时只可使用未过期且已验证的缓存 manifest；过期后拒发，不以“网络不好”降级。
4. 收到低于本地最高 version 的策略或 manifest 视为 rollback。
5. 时间判断同时参考安全服务器时间偏移和本地单调时钟，防止简单修改系统时间绕过到期。

---

## 6. 审计与运维

- 记录 manifest 发布/轮换/撤销、客户端采用版本、拒发错误类别；不记录 content key、私钥、明文或完整密文。
- 合规私钥操作应在独立审计端点/HSM 或等价受控环境，访问必须有双人授权和不可变审计；这是部署要求，不进入 IM 服务进程。
- 租户关闭 Compliance 并切换 Strict 时，新消息立即不再包含审计接收方；历史消息的既有接收权限不能被密码学撤回，UI/文档要诚实说明。

---

## 7. 验收测试

| ID | 用例 | 通过条件 |
|---|---|---|
| CB-01 | policy service 未初始化/超时 | strict/compliance 均拒发，optional 需显式确认 |
| CB-02 | manifest 缺失、过期、签名错误、tenant 不符、版本回滚 | 全部拒发，不读陈旧 fallback |
| CB-03 | 服务端替换 public key 并重签 API 响应 | 因组织根签名/pin 不匹配而拒绝 |
| CB-04 | 篡改 Protected Frame 的 mode/key_id/policy_id | 解密/上下文验证失败 |
| CB-05 | Compliance 正常 round-trip | 用户设备与审计端点解得同一 payload；服务端不能解 |
| CB-06 | Strict 正常 round-trip | wire 中无 compliance wrap/member，审计私钥无法解密 |
| CB-07 | 密钥轮换边界前后消息 | 各自仅由声明 key 解开，旧 key 不用于到期后新消息 |
| CB-08 | 搜索 server DB/log/config/crashdump | 无 private key/content key/明文 |
| CB-09 | 模式切换 | 会话被中断并产生用户可见事件，无静默切换 |
| CB-10 | 离线跨过 expires_at | 到期时开始拒发，不无限使用缓存 |

---

## 8. 生效条件

- [ ] 法务/产品确认 Compliance 的准确用户文案和安全声明
- [ ] 组织根引导、轮换、吊销和灾难恢复 runbook 已评审
- [ ] CB-01..10 全部可自动化或真机验证
- [ ] 合规审计端点的密钥托管不在 IMBoy 服务端进程/数据库

接受后在 ADR 07/13 顶部标注：服务端零私钥决定保留，合规 trust/fallback 由 ADR 18 替代。
