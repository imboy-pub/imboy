# C15 — 标准 Test Vectors 获取方案与清单

> run: run-20261003-094804 | card: C15 | 获取时间：2026-10-03
> 来源：IETF MLS 工作组公开仓库 `mlswg/mls-implementations`（test-vectors/ 目录）。
> 性质：公开仓库公开数据，仅用于互操作测试（非外发、非生产数据）。

## 1. 来源与许可证

- 仓库：https://github.com/mlswg/mls-implementations （IETF MLS WG 工作区仓库）
- 目录：`test-vectors/`，共 **15 个 JSON 文件**（GitHub API 列目录核实）
- 格式说明：同仓库 `test-vectors.md`（JSON 数组；MLS struct 以 TLS 规范编码后 hex 表示；
  HPKE/Signature 公私钥编码规则在文档中逐条定义）
- **许可证注意**：该仓库根目录**无独立 LICENSE 文件**【确证：GitHub API 目录列表】。
  IETF WG 贡献通常按 IETF 知识产权条款处置；RFC 9420 文本中的代码组件按修订版 BSD
  许可发布，但 vectors 文件本身未附显式许可证。**用作互操作测试输入为社区普遍实践**
  （OpenMLS/mls-rs/mlspp 的 CI 均消费该目录）【确证：社区实现消费惯例】；
  **生产分发/再发布前需法务确认（UNVERIFIED）**——本轮仅本地测试用途，无再发布。

## 2. 本轮已下载（vectors/ 目录，10 个核心文件 + SHA256）

| 文件 | 大小(B) | SHA256 | 覆盖内容 |
|---|---|---|---|
| tree-math.json | 91,438 | 27f04891f56106593b74b674445f01b845e173953770c084deb2f8cf5592e2fc | 树数学（root/parent/left/right/间接） |
| crypto-basics.json | 20,116 | 17cfcf89af9f51d0f2aa7af77f6f9ec99376a039214b6d42a6f11646b83e8c29 | derive_secret/derive_tree_secret/encrypt_with_label/expand_with_label/ref_hash |
| key-schedule.json | 101,795 | 05aa9a68bd2538ace72d8c53375984cc728ef62220ebf314df675708546d97a7 | epoch 密钥调度（joiner/epoch/auth/membership/confirmation… secret 派生链） |
| message-protection.json | 30,812 | f7c1ae62ce63c3003e526d539c99f2b1444f65ee0a484d3a5667d46867526c45 | MLSCiphertext/PrivateMessage 保护（签名+加密+MAC） |
| secret-tree.json | 211,298 | 08f92e6272452e2c832e32d38e16cf0c4aa28967e47d3842c60bc354c6b67a94 | ratchet tree → 每 leaf/世代加密密钥派生（**PCS 证据链核心**） |
| transcript-hashes.json | 7,328 | 58046b58fbd98519b4e60953a105f550e4b427e2b140ea67dedcb751260eecb8 | confirmed/interim transcript hash（commit 链完整性） |
| welcome.json | 16,815 | 06be9d5c99817ef2545e4b15b8e73fd9b604685a8e55b59ca168eda98e236502 | Welcome 消息/加密 GroupInfo/新成员加入 |
| psk_secret.json | 155,437 | 2b534969dba0b65a04b7d790496af5c0ccdb472b3fc4ca25c8c82df3e8523784 | PSK（external/resumption）注入 |
| deserialization.json | 934 | 1e394706e79f77df71454e5970f2aa736d4ab8b6c7e3219dd5609992388a953e | 反序列化拒绝向量 |
| tree-operations.json | 50,670 | 9a25f8720714d1256ba2dad8d83a660ba0bae0d33ed15c682b9d74a5d67aad0a | 树操作（add/remove/update leaf 后的树演化） |

覆盖面：除 treekem/messages/passive-client/tree-validation 四个大文件外的全部类别；
每文件按 ciphersuite 0x0001–0x0007 共 7 组入口（抽样核实 tree-math=10 树、
crypto-basics/message-protection/welcome/transcript-hashes 均为 7 条目/套件）。

## 3. 未下载的大文件（获取命令记录，按需再取）

| 文件 | 大小(B) | 用途 | 获取命令 |
|---|---|---|---|
| messages.json | 2,685,714 | 全消息语法序列化 | `curl -LO https://raw.githubusercontent.com/mlswg/mls-implementations/main/test-vectors/messages.json` |
| treekem.json | 1,970,683 | TreeKEM update path/路径秘密 | 同上模式（改文件名） |
| tree-validation.json | 1,383,087 | 树哈希/父哈希校验 | 同上 |
| passive-client-handling-commit.json | 1,285,850 | 被动客户端处理 commit | 同上 |
| passive-client-random.json | 1,948,393 | 被动客户端随机树 | 同上 |
| passive-client-welcome.json | 719,346 | 被动客户端 Welcome | 同上 |

说明：为控制计划目录体积（>10MB 不入设计文档目录），大文件按需在
prototype 阶段拉取到本地构建产物目录（不入仓）。

## 4. 与实现的 vector 集（第二互操作来源）

- **OpenMLS**：仓库内 `compat_tests/`（消费上述官方 vectors）+ 自有测试 vectors
  （Rust 单测内嵌）。PoC 用其 CI 同款数据即与本目录 vectors 同源【确证】。
- **mls-rs**：README 声称以预计算 vectors 验证 RFC 9420 符合性【确证】。
- AC-30「标准 vectors 互操作」执行定义：选定实现（OpenMLS）对上述 10 文件全部
  通过 + 我们的第二实现（Dart 绑定层之上）对同一批文件复算一致——对齐 C09
  AC-19 的「两个独立实现 + golden vectors」验收模式。

## 5. 复现命令（本轮实际执行）

```bash
BASE="https://raw.githubusercontent.com/mlswg/mls-implementations/main/test-vectors"
DEST="<本目录>/vectors"
for f in tree-math crypto-basics key-schedule message-protection secret-tree \
         transcript-hashes welcome psk_secret deserialization tree-operations; do
  curl -sL -o "$DEST/$f.json" "$BASE/$f.json"
done
shasum -a 256 "$DEST"/*.json   # 与 §2 表核对
```

网络状态：可用（GitHub raw 直连成功，无代理）。
