# E2EE-065 Slice 1：`bigserial` 能否充当 KT leaf index —— 真 PG 实证

- **Slice**：`28-e2ee-065-066-key-transparency-research.md` §4 切片计划 **Slice 1**
- **会话**：`20260729-0600-claude-code`
- **仓库**：`imboy`
- **状态**：Slice 1 完成；**E2EE-065/066 整体仍为 `PENDING`**
- **本刀不改任何生产代码**，只新增一个真 PG 探针测试模块

---

## 1. 做了什么

上一刀的设计文档 §2.1 把 KT 日志的 `identity_log.seq` 写成 `bigserial`
（与既有 `trust_audit` 同范式），同时把它与 playbook E2EE-033 验收标准
「并发 append 1000 events 得到**唯一连续**位置」的冲突标为 **未实证**，
并明确要求「不得凭 PostgreSQL 通例推断，须在真 PG 上验证后再定表结构」。

本刀就是那次验证。

新增 `test/integration/kt_seq_contiguity_probe_tests.erl`（3 例）：
建一张一次性探针表 `public.kt_seq_probe`，测完 `DROP`。
**不在** `e2ee-verify` 硬门禁清单内（真 PG 集成模块，无 DB 时会 skip）。

---

## 2. 探针结果

```
$ IMBOYENV=local make eunit t=kt_seq_contiguity_probe_tests \
    EUNIT_ERL_OPTS="-config config/sys.local -pa ebin -pa test"
  All 3 tests passed.
```

| # | 探针问题 | 结果 |
|---|---|---|
| 1 | **对照组**：顺序提交时 seq 连续？ | **是**（相邻差恒为 1） |
| 2 | 事务回滚后序号回收？ | **否** —— 留下**永久空洞**，那一行永不出现 |
| 3 | 分配顺序 = 提交可见顺序？ | **否** —— 见 §2.2 |

### 2.1 ⚠️ 对照组第一次就红了 —— 停下重估，未接着钻

首次运行 `Failed: 3, Passed: 0`，**含对照组**。
按铁律「对照组红 = harness 缺陷，立刻停下重估」，没有去调整被测断言，
而是先查 harness：

```
**error:{badmatch,{ok,[#{<<"seq">> => 2}]}}
```

根因：`elib_pg:query/2` 返回 **`{ok, [Map]}`（map 列表）**，
不是我假设的 epgsql 三元组 `{ok, Cols, Rows}`。

修正取值形状后重跑，对照组转绿、三例全绿。
**若当时不看对照组、直接把后两条的断言改到"能过"，会得到一个测什么都对的探针。**
该坑已写进探针模块的注释（`insert_one/1` 上方）。

### 2.2 核心发现：**空洞会追溯填上**

用例 `visibility_hole_backfills`：
事务 A 先取号但**不提交**；事务 B 后取号并提交。

- 此刻按 `seq` 扫描：**只见 SeqB，不见 SeqA** —— 出现一个洞；
- A 提交后再扫：**`[SeqA, SeqB]`** —— 那个低序号**追溯出现**。

对 Merkle 日志这是**致命**的，不只是"不好看"：

```
t1 扫描 [.., SeqB]        → 算出 root R1，tree size = N
t2 扫描 [.., SeqA, SeqB]  → 算出 root R2，tree size = N   （R2 ≠ R1）
```

**同一 tree size 先后算出不同 root**——这与设计文档 §2.4 要检出的
**split view 形状完全一致**。也就是说：日志会**自己制造出无法与真实攻击区分的告警**。
consistency proof 同样直接失效。

---

## 3. 据此定案（取安全那一侧）

**leaf index 必须与 `bigserial` 解耦。**

append 走两阶段：先提交行，再由**单一串行化 sequencer** 分配 leaf index，
且 sequencer 只处理**已提交可见**的行。这同时满足 playbook 的「唯一连续位置」。

具体机制留给 Slice 3，但设计文档 §2.1 已写死约束：
**不得再把 `bigserial` 直接当 leaf index。**

取舍理由：另两条路都更差——
- 「接受空洞、把空洞当合法 leaf」：树里有洞，inclusion proof 语义崩坏；
- 「靠加锁让 `bigserial` 连续」：等价于串行化写入，却把串行化藏在序列分配里，
  比显式 sequencer 更难审计，且回滚仍会留洞（探针 #2 已实证）。

---

## 4. 验收命令与结果

```
$ IMBOYENV=local make eunit t=kt_seq_contiguity_probe_tests \
    EUNIT_ERL_OPTS="-config config/sys.local -pa ebin -pa test"
  All 3 tests passed.

$ erlfmt --check test/integration/kt_seq_contiguity_probe_tests.erl
  All matched files use erlfmt code style!

$ git diff --check
  （通过）
```

探针表清理已核实（另起一次 erl 查询，查完即删脚本）：

```
PROBE_TABLE_CHECK={ok,[#{<<"t">> => <<"DROPPED">>}]}
```

`make e2ee-verify` 未跑：本刀不改任何生产代码，也未向门禁清单增删模块。

---

## 5. 残留风险

1. **并发规模未压到 playbook 要求的 1000** —— 探针用 2 个并发事务证明了
   **性质**（分配序 ≠ 可见序）。1000 并发是**吞吐与唯一性**的验收，属 Slice 3/5，
   不是本刀要答的问题。**认识论状态：性质已实证，规模未验。**
2. **两阶段 sequencer 的具体机制未设计** —— 只定了约束（不得用 bigserial 当
   leaf index），机制留给 Slice 3。其本身的并发正确性需再次真 PG 验收。
3. **探针表建在本地开发库** —— 已 `DROP` 并核实。若该测试在 CI 上跑，
   需确认 CI 库允许建表；本模块**不在**硬门禁内，默认不会被 CI 触发。
4. E2EE-065/066 的**实施**仍受三重阻塞（PFv3 改协议、ADR 16 待签字、
   上游 064 BLOCKED），见 `evidence/E2EE-065-066-research-and-design.md` §3。
5. E2EE-062 既有残留不变。

---

## 6. 认识论状态汇总

| 结论 | 状态 |
|---|---|
| 顺序提交时 `bigserial` 连续 | **已实证**（对照组） |
| 回滚不回收序号，留永久空洞 | **已实证** |
| **分配顺序 ≠ 提交可见顺序，空洞会追溯填上** | **已实证** |
| 同一 tree size 可先后算出不同 root | **已实证**（由上一条直接构造） |
| 该现象与 split view 形状一致、无法区分 | **推论**（由上一条 + 设计 §2.4 定义） |
| leaf index 必须与 `bigserial` 解耦 | **已定案** |
| 1000 并发下的唯一性与吞吐 | **未验** —— 属 Slice 3/5 |
| 两阶段 sequencer 的并发正确性 | **未设计、未验** |
| 探针表已从本地库清除 | **已实证**（`to_regclass` 返回 `DROPPED`） |

---

## 7. 未做

- **未实施 KT 任何生产代码**；未新增迁移、依赖、配置项。
- 未改 ADR / 协议规范；未代签任何 ADR；未动 E2EE-012/023/024/025/029 状态标记。
- 未向 `e2ee-verify` 门禁清单增删模块（真 PG 集成模块按规定不入硬门禁）。
- 未删除或 skip 任何测试。
- 不 push、不部署、不访问生产、不通知第三方。
