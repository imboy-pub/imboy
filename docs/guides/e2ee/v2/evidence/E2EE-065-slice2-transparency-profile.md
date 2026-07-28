# E2EE-065 Slice 2：Transparency Profile 冻结草案

- **Slice**：`28-e2ee-065-066-key-transparency-research.md` §4 切片计划 **Slice 2**
- **会话**：`20260729-0800-claude-code`
- **仓库**：`imboy`（文档）
- **状态**：草案完成；**profile 未签字**，E2EE-065/066 整体仍为 `PENDING`
- **交付物**：[`29-e2ee-065-transparency-profile-v1.md`](../29-e2ee-065-transparency-profile-v1.md)
- **本刀不改任何生产代码，未新增任何测试模块**

---

## 1. 做了什么

playbook E2EE-033 第 1 步要求「先冻结 transparency profile：hash、leaf/node domain
separation、空树值、canonical event bytes、tree-head 签名输入、proof wire、
signing-key 轮换」，且验收标准写着 **「transparency profile 无 `TBD`，
domain separation 与签名输入有跨实现 golden vector」**。

本刀产出该 profile 的**冻结草案**，九项冻结条目**逐条给出确定值，无一处 `TBD`**，
并附**跨实现已核验**的 golden vector。

**接受动作必须人工**（playbook 原文「由安全 reviewer 接受」），loop 只出草案。

---

## 2. 跨实现 golden vector：已核验

用 **Erlang（`crypto:hash/2`）与 Python（`hashlib`）两套独立实现**分别计算
同一组向量，**逐字节一致**：

| 项 | 值 |
|---|---|
| 空树根 | `e3b0c442…7852b855` |
| `leaf_hash(E1)` | `de22f9f5…68e602e1` |
| `MTH([E1,E2])` | `bbd5b8a6…13a77ae0` |
| `MTH([E1,E2,E3])` | `6beeef5d…e3acb962` |
| `SHA-256(0x02 ‖ head)` | `34760542…04cd1f3d` |

（完整 64 位 hex 见 profile §8。）

### 2.1 两重独立自校验（本刀的"对照组"）

本刀没有生产代码可改，因而没有传统意义的 RED。替代它的是**两重自校验**——
若任一不成立，说明计算器本身错了，后面所有向量都不必看：

1. **空树根 == 公认的 `SHA-256("")`** ——
   `e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855` 是全世界都
   知道的常量。算不出它，说明这套代码连标准 SHA-256 都不对。
2. **`MTH([E1]) == leaf_hash(E1)`** —— RFC 6962 的定义要求。
   两值在结果里确实相等。

这两条**不依赖我自己的任何假设**，是外部可判定的，因此能起到对照组的作用。

### 2.2 n=3 是刻意选的

三叶树是**最小的非平衡情形**（k=2，左子树 2 叶、右子树 1 叶）。
只用 n=1/2/4 的向量无法区分「实现了 RFC 6962 的分裂规则」与
「实现了朴素的两两配对」——两者在 2 的幂上结果相同。
向量必须包含至少一个非 2 的幂，否则等于没验分裂规则。

### 2.3 canonical bytes 的**长度**也是向量的一部分

profile 把 `E1` 的 canonical bytes 长度（96 字节）也列为向量。
长度对不上说明编码规则理解错了（多/少了尾随换行、字段序不对），
此时再比 hash 只会得到「不一致」这个无信息量的结论。

---

## 3. 几处需要说明的取舍

### 3.1 复用既有 canonical 方案，不发明第三套

项目已有两套：`CanonicalCbor`（PFv3）与 `key=value\n` ASCII 字典序
（trust event，Dart 与 Erlang 双语言对齐且带 fail-closed 非单射守卫）。

KT 事件是**平坦键值**，用第二套即可；引入第三套 = 第三份 golden vector +
第三处跨实现漂移面。profile §9 已把该判断与「为何不用 CBOR」一并写明。

### 3.2 domain separation 用 `0x00/0x01/0x02` 三个前缀

`0x00`/`0x01` 是 RFC 6962 的 leaf/node 分离，防 second-preimage
（否则可令 `leaf(x) == node(a,b)`，对同一 root 构造两棵不同的树）。
额外的 `0x02` 用于 tree head，防止一条 tree-head 签名被当作 leaf 数据复用。

### 3.3 `domain` 是显式字段，不依赖字母序巧合

tree-head canonical bytes 里 `domain=imboy.kt.v1.tree_head` 恰好按 ASCII 序排最前，
但 profile **不依赖这个巧合**——它把 `domain` 列为**必需的冻结字段**。
依赖「d 恰好小于 l/r/t」是一种会在字段集变更时静默失效的写法。

### 3.4 签名 key 过期 → **fail-closed**，与 E2EE-062 第七刀方向相反

E2EE-062 第七刀对 `rate_not_set` 选择了 **fail-open + 打日志**，理由是
「scope 缺失是配置错误，拒掉全部 claim 会让 E2EE 建会话不可用」。

这里选**相反**方向：两把 signing key 都过期时**拒绝** tree head。
理由是后果不对称——那里放行只是「限流暂时失效」，这里放行是
**「透明度机制完全静默失效」**，而透明度的唯一价值就是不被静默绕过。
profile §7 已写明该对比，避免后来者误以为两处不一致是疏漏。

---

## 4. RED 记录

**不适用**——本刀是纯文档交付，无生产代码、无行为缺陷可复现。
替代验收见 §2.1 的两重独立自校验。

---

## 5. 验收命令与结果

```
$ escript <计算脚本>      # Erlang 侧，已删除临时脚本
EMPTY_ROOT=e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855
E1_LEAF=de22f9f514db9c0faa1c57e53668678cfbf93f3b166eafd1212e254368e602e1
ROOT_N3=6beeef5d57749b14c1f9d4b090ebcc0eaa35422a7b19bdde36863dc8e3acb962
HEAD_SIGNING_INPUT_SHA256=34760542818964fc8f23ad1a09dca6c5a9d4388561cfa87ae8110e9c04cd1f3d

$ python3 - <<'EOF' ... EOF   # Python 侧独立复算
EMPTY_ROOT = e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855
E1_LEAF    = de22f9f514db9c0faa1c57e53668678cfbf93f3b166eafd1212e254368e602e1
ROOT_N3    = 6beeef5d57749b14c1f9d4b090ebcc0eaa35422a7b19bdde36863dc8e3acb962
HEAD_SIG_IN= 34760542818964fc8f23ad1a09dca6c5a9d4388561cfa87ae8110e9c04cd1f3d
```

**逐字节一致。** Python 版复算脚本已完整写进 profile §8.4，任何人可离线重跑
（不依赖项目任何代码）。Erlang 侧临时脚本已删除（`/tmp`，未入库）。

两侧验收命令（`make e2ee-verify` / flutter）本刀**不适用**：
未改生产代码、未增删测试模块、未动 Makefile。

---

## 6. 残留风险

1. **golden vector 未被测试钉死** —— 目前只存在于文档里。
   钉死它属 **Slice 4（Merkle 纯函数实现）**，而 Slice 4 属实施范畴，
   与队列第 6 项「只产出调研与设计文档」的界定冲突，**是否放行需人工确认**。
   **认识论状态：向量值本身已跨实现核验（已实证）；「实现会持续符合它们」未实证。**
2. **profile 未经安全 reviewer 接受** —— playbook 第 1 步的硬性要求，
   **loop 不得自我接受**。这是本 profile 生效的前置条件。
3. **leaf index 分配机制仍未定** —— 只有 Slice 1 定下的约束
   「不得用 `bigserial`」，两阶段 sequencer 的具体形态属 Slice 3。
4. **Consistency proof 路径构造未展开** —— wire 格式已冻结，
   算法沿用 RFC 6962，实现与向量属 Slice 4。
5. ADR 16 的 transparency log 部分**仍为 Proposed**，实施受其阻塞。
6. E2EE-062 既有残留不变。

---

## 7. 认识论状态汇总

| 结论 | 状态 |
|---|---|
| 空树根 = `SHA-256("")` | **已实证**（两套实现 + 公认常量） |
| `MTH([E1]) == leaf_hash(E1)`（RFC 6962 定义要求） | **已实证** |
| 三条事件的 leaf hash 与 n=1/2/3 的 root | **已实证**（Erlang + Python 逐字节一致） |
| tree-head 签名输入 hash | **已实证**（同上） |
| canonical bytes 长度（E1=96、head=168） | **已实证** |
| profile 无 `TBD` | **已实证**（逐条通读） |
| 「本 profile 足以让 DT-05/06/07 成立」 | **设计推理，未实证** |
| 「实现会持续符合这些向量」 | **未实证** —— 需 Slice 4 的测试 |
| profile 已被安全 reviewer 接受 | **否** —— 待人工 |

---

## 8. 未做

- **未实施任何生产代码**；未新增测试模块；未动 Makefile 门禁清单。
- 未新增迁移、依赖、配置项。
- 未改 ADR / 协议规范；未代签任何 ADR；**未自我接受本 profile**。
- 未动 E2EE-012/023/024/025/029 状态标记。
- 不 push、不部署、不访问生产、不通知第三方。
