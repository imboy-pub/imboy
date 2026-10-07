# C16 Vectors 方案（AC-32 独立实现 interop）

run-20261003-094804 / C16 / 2026-10-03 / 状态：方案文档，fixture 生成实现属后续 PROTOTYPE 卡

AC-32 原文要求：**独立实现 interop 与官方 vectors/算法对应**。Signal 未发布 PQXDH 官方 test vectors（X3DH/PQXDH 规范均无 vectors 附录）[确证]，因此按三层组织：官方算法 vectors（ML-KEM 层）+ 自建协议 golden vectors（语义层）+ Python 独立实现双源生成/验证（沿用 C09 的 generate/verify 双源模式 [确证（本目录 fixtures/ 既有该模式：generate.escript + vectors.json + verify_python.py）]）。

## 1. 第 1 层：官方算法 vectors（ML-KEM 原语层）

- 来源 A：NIST ACVP ML-KEM（FIPS 203）KAT（keyGen/encaps/decaps，含 decapsulation-invalid 负例集）。
- 来源 B：选型实现自带 KAT（RustCrypto ml-kem crate 测试集）。
- 通过标准：候选 FFI 绑定对全部 ACVP KAT 逐字节一致；任何 `decapsulation-invalid` 样本必须产生显式失败而非静默错值。

## 2. 第 2 层：协议语义 golden vectors（自建、双源）

向量族（每个向量含 inputs/expected 两段，全部确定性种子驱动）：

| 族 | 覆盖 | 数量（首版） |
|---|---|---|
| V1 prekey-bundle | PQ prekey canonical 序列化 + ed25519 签名/验签（含错签、错 key_id、kind 篡改负例） | 8 |
| V2 handshake-positive | Alice/Bob 全流程：kem_ss 一致、pq_root_0 两侧一致、首条 frame 可解 | 4 |
| V3 handshake-negative | PQ 签名失败 / PQ pin 不匹配 / 缺 pq_kem 字段 / pq_ct 指向错误 key_id / SEAD 式换绑（AD 换 peer pin）→ 全部 fail-closed | 10 |
| V4 ratchet | epoch 0..3 正向推进；N 条消息链 key 派生序列；epoch 换钥后旧 pq_mk 解新消息必失败 | 6 |
| V5 replay/out-of-order | 旧 epoch CT 重放、frame seq 回退、重复 frame digest、跨 epoch 块 | 8 |
| V6 mixing-boundary | 双层解密序：外层过内层败、内层过外层败、双层乱序裁剪 | 6 |

生成/验证（双源，防自证）：
- 源 1（Dart/测试侧）：Flutter test 内用被测实现生成并校验。
- 源 2（Python 独立）：`liboqs-python`（ML-KEM-768）+ `cryptography`（HKDF/AEAD/Ed25519）按**本草案文档**独立写出 mini oracle，从 vectors.json 读入复算。两源对 expected 逐字节一致才 PASS——满足「独立实现 interop」。
- （可选加固）源 3：aws-lc-rs 或 libcrux 对 KEM 层三向对拍，作为 ml-kem crate「无审计」缓解的证据链。

## 3. 第 3 层：与官方规范的对应性声明表

AC-32 要求「与官方 vectors/算法对应」——算法层对应 ACVP（§1）；协议层无官方 vectors，则以**规范条文编号映射表**替代：本草案每个行为（prekey 结构、SEAD AD 绑定、初始密文认证、SK 交付语义、认证非量子安全的边界）逐条给出 PQXDH 规范章节引用（见 hybrid-handshake-draft.md §2.4 与 pq-profile.md §3.4 声明），并由独立 reviewer 核对「无一条声称超出规范保证」。

## 4. 交付物与流程

```
docs/design/2026-10-03-e2ee-production-excellence/
  pq-research/vectors-plan.md          ← 本文（方案）
  fixtures/pq/                          ← 后续卡生成（不在本 profile 卡内）
    generate.escript / generate.dart    ← 源 1 生成器
    vectors.json                        ← 种子驱动的向量集（合成数据，无真实秘密）
    verify_python.py                    ← 源 2 独立 oracle（requirements: liboqs-python, cryptography）
```

- vectors.json 仅含合成种子与预期值，无 PII/真实密钥（plan §7 约束）。
- CI/命令合同：verify 脚本以非零退出码表达任一向量不一致；skip 不算 PASS（plan §4）。
- vectors 随草案变更强制再生（草案 version 字段进向量头部，版本不匹配 → 拒绝加载旧向量）。
