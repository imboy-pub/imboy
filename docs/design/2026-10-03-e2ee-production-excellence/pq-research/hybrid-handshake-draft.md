# C16 Hybrid Handshake 设计草案（X3DH + ML-KEM，PROTOTYPE 上限）

run-20261003-094804 / C16 / 2026-10-03 / 状态：**设计草案，未实现、未批准生产接线**

目标：按 Signal PQXDH 语义给现有 Olm 会话建立叠加 ML-KEM-768 混合层，且**不修改 vodozemac 内部**（上游无注入口，见 selection-matrix.md R2）。组合安全目标（AC-32）：**经典 X25519 层与 PQ 层任一不被攻破，明文即保密**。

## 1. 语义模型：双层包封（envelope）而非密钥注入

PQXDH 规范把 `SK = KDF(DH1‖DH2‖DH3‖SS)` 交给 Double Ratchet [确证]。vodozemac 不允许外部注入 SK [确证（API 实测）]，故等价改写为**双层密文**：

```
发送（新 suite 'pq-olm.v1'，仅当双方 capability 匹配）：
  inner_ct = Olm.encrypt(plaintext)                    # 现有链路原样（X3DH + DR 不动）
  pq_mk    = PQ_ratchet.messageKey()                   # PQ 层自己的对称棘轮（见 §3）
  frame    = header ‖ AEAD_Encrypt(pq_mk, inner_ct, AD)

AD = suite_tag ‖ sender_uid ‖ sender_device ‖ peer PQ pin ‖ epoch_id ‖ seq
```

解密必须先外层 PQ AEAD 通过，再内层 Olm 通过；任何一层失败即整体失败（fail-closed，见 pq-profile.md 负例清单）。攻击者要读明文必须**同时**攻破 X25519 层（Olm）与 ML-KEM 层（PQ ratchet 根）——这正是「任一不被攻破即安全」的 KEM 组合安全定义 [推断：与 Signal Triple Ratchet 的 KDF 混合论证、X-Wing 混合密钥论证同构]。

代价（如实声明）：密文为两层之和；PQ 层不提供逐消息的**量子**级前向保密（见 §3 与 pq-profile.md「持续 PQ ratchet 评估」）。

## 2. 握手（prekey bundle 扩展）

对照 PQXDH 规范的 PQ prekey 结构（PQSPK = signed last-resort PQ prekey、PQOPK = one-time PQ prekey，均由身份键签名）[确证]，映射到本仓现有服务端 claim 流：

### 2.1 服务端 prekey 模型扩展（草案，后端迁移归 C12 lease，本卡不写）

现有：`claimKey` 返回 `identity{curve25519_key,ed25519_key,signature}` + `key_base64`（one-time 优先，fallback 兜底）[确证（olm_session_service.dart:710-717）]。

扩展字段（新增，不覆盖旧字段——旧端忽略未知字段即可保持兼容）：

```json
{
  "pq_kem": {
    "alg": "ML-KEM-768",
    "pubkey_b64": "<1184B 公钥>",
    "key_id": "<PQ prekey 标识（PQXDH IdKEM 语义）>",
    "kind": "one_time | fallback",
    "signature_b64": "<设备 ed25519 对 canonical(pq_kem.pubkey‖key_id‖kind) 的签名>"
  }
}
```

- 签名复用 E2EE-062 fallback_key_signature 的 canonical + golden vector 模式 [确证（本仓既有机制）]，域分离常量 `imboy.pqprekey.v1`。
- PQ one-time prekey 池复用现有 OTK 补传水位机制（低水位 5 / 目标 50）扩展出 `pq_otk` 一类 [推断：沿用 otk_refill_policy]。
- **服务端只存公钥侧**（与现有 prekey 相同，无秘密新增）[确证（服务端语义既有）]。

### 2.2 Alice（发起方）流程

1. claim：`requestId` 幂等键机制复用（OlmClaimRequestId）[确证]。
2. 验证 identity 签名 + TOFU pin（现有）→ 再验证 `pq_kem.signature`（新断言点：失败拒绝建会话）。
3. **PQ pin**：`(peer_uid, peer_device_id)` 的 PQ 公钥指纹独立 TOFU 首钉；后续不匹配 → `IdentityChangedException` 同语义 fail-closed。
4. `(_ct, _kem_ss) = ML-KEM-768.Encaps(pq_kem.pubkey)`；`_ct`(1088B) 随首条 prekey 消息携带。
5. 建立内层 Olm 会话（现有 `createOutboundSession` 原样）。
6. **PQ 根密钥混合**：
   `pq_root_0 = HKDF-SHA256(ikm = kem_ss ‖ olm_session_id ‖ first_inner_ct_hash,
                            salt = ss_hash, info = "imboy.pqxdh.v1.root")`
   —— PQXDH 里「初始密文充当认证」[确证（规范 §）]；此处以首条内层密文的哈希作为等效认证绑定，防未认证 KEM 公钥替换（SEAD/重封装防护的弱化形式：完整 SEAD 防护要求把 `EncodeKEM(PQPK)` 绑进 AD [确证（规范）]，本设计 AD 已含 peer PQ pin，等效达成）[推断]。
7. 发送 `suite='pq-olm', version=1` 的首条消息：metadata 带 `pq_ct_b64`、`pq_key_id`、`pq_epoch=0`。

### 2.3 Bob（响应方）流程

1. 收到 messageType=0 且 suite='pq-olm' → 走 PQ 入站分支（与纯 Olm 入站分叉，互不干扰）。
2. 用 `pq_key_id` 定位本地 PQ 私钥（one-time 优先消费；fallback 常驻），`kem_ss = Decaps(sk, pq_ct)`。
3. 验证对端 PQ pin（若 Bob 侧先发起过则已有 pin；否则在本向建立时同样首钉）。
4. 内层 Olm inbound 建立并解出首条明文 → 计算 `first_inner_ct_hash` → 同式派生 `pq_root_0`（两侧一致）。
5. 之后进入 §3 的 PQ ratchet。

### 2.4 与现有 Olm 流程的映射总表

| 现有 Olm 步骤（spec §4） | PQXDH 语义 | 本草案落点 |
|---|---|---|
| identity bundle（ed25519 签 curve25519） | 同 + 签 PQ prekey | `pq_kem.signature`（identity 键签） |
| one-time/fallback prekey claim | + PQ one-time/fallback prekey | claim 响应新增 `pq_kem` 字段 |
| E_A 临时 DH | + Alice 一次性 KEM encaps（ct 随首条消息） | metadata `pq_ct_b64` |
| SK=KDF(DH1..4) 交 DR | SK=KDF(DH1..3‖SS) 交 DR | 双层包封 + pq_root 混合（§1） |
| TOFU pin curve25519 | 同 | + 独立 PQ pin |
| OTK 原子 claim 防重放 | 同（PQ OTK 同池语义） | 服务端 claim 幂等复用 |

## 3. PQ ratchet（持续混合层）

握手后 PQ 层维护**独立对称棘轮 + 稀疏 KEM 再封装**（SPQR 风格简化版）[确证（SPQR 语义）+ 推断（工程简化）]：

- 每方向消息：`pq_mk_i = HKDF(pq_chain, "imboy.pq.msg")`，`pq_chain` 前向步进——与 spec §5.1 同构，因此**继承 §5.3 同链前向性边界**（同链内攻破 chain 可前推；靠 pq_mk 即时销毁）[确证（与现有合同一致的边界声明，AC-32 要求如实声明）]。
- epoch 换钥：每 `N` 条消息（默认 N=100，PROTOTYPE 可配）或方向变化阈值触发新 KEM 交换：发送方新 EK 公钥 1184B / 对端回 CT 1088B [确证（ML-KEM-768 参数）]。PROTOTYPE 采用**整块携带**（不分块搭便车）——SPQR 的分块+擦除码（EK 37 块 / CT 34 块）作为**后续优化**选项，不进首版 [推断：先正确后优化，减少状态机面积]。
- 新 `pq_root_{e+1} = HKDF(kem_ss_{e+1} ‖ pq_root_e, info="imboy.pq.epoch")` —— **密钥链式混合**：即使 KEM 未来被破，历史 epoch 根仍受旧熵保护；反之亦然 [推断：混合链组合论证]。
- 串行 epoch（一次只推进一个，未完成不启动下一个）——SPQR 明确不并行多 epoch 的安全理由（并行需同时保存多个 DK，一次泄露暴露多 epoch）[确证（SPQR 公告）]。

## 4. Downgrade 防护（衔接 C02）

- capability 名：`'pq-olm.v1'` 加入 `DeviceManifest.capabilities` 集合（C02 已有签名 manifest + 过期校验 + 链式哈希 [确证（C02 决策报告/已集成代码）]）。
- `securityRank` 扩展为 `['pq-olm', 'olm', 'megolm', 'rsa-oaep']`（pq-olm 最高位）。
- 策略（与 C02「固定套件、结构上不可降级」一致的精神）：
  1. **每对端设备首次协商即固定**所选 suite 并 HWM 记录；已 HWM=pq-olm 的对端再出现无 pq capability 的 manifest → `CapabilityDowngradeException`（fail-closed）。
  2. 发送链只在「本端开启（默认 off）∧ 对端 manifest 含 pq-olm.v1 ∧ 验签通过」三条件同时成立时选 pq-olm；否则退回纯 Olm——**回退只发生在「从未以 pq-olm 与该设备通信」的场景**，一旦建立过 pq 会话则不允许回退（HWM 门）。
  3. claim 响应**缺** `pq_kem` 字段但 manifest 声称含 pq capability → 视为服务端不一致/剥离攻击，拒绝建会话（不静默降级为纯 Olm）——这是「混合组件替换/剥离 fail-closed」断言点之一。
- 旧客户端（无 pq capability）收到 pq-olm 消息 → 走「未知 suite」既有拒绝路径（不尝试解密）[确证（e2ee_protocol.dart fromMetadata/registry 语义）]。

## 5. 会话与密钥命名空间隔离（AC-33，详见 pq-profile.md §6）

- ProtocolSuite：`ProtocolSuite('pq-olm', 1, cipher)` 独立注册（registry 按 protocol 键分发 [确证（e2ee_protocol.dart:216-229）]）。
- 存储：PQ 层状态独立前缀 `crypto_pq_state:<peer_uid>:<peer_device_id>`（chain key、epoch 计数、DK 私钥、PQ pin），不与 Olm session pickle 混表。
- **旧 Olm 会话不自动升级**：已存在的 olm 会话继续按 olm 收发直到会话自然结束；pq-olm 仅用于**新建**会话且受默认 off 的开关控制（D-04）。

## 6. 本草案的明确不做项

- 不 fork / 不改 vodozemac；不写任何 KEM/AEAD 原语（用选型库）。
- 不做分块搭便车与擦除码（PROTOTYPE 整块携带，见 §3）。
- 不做 PQ 群聊（Megolm PQ 化）——单聊之外属后续卡。
- 不声称逐消息量子 FS、不声称主动量子攻击者下的认证安全（PQXDH 规范明示认证非量子安全 [确证]）。
