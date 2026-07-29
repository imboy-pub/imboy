# E2EE-065 Slice 4 —— Merkle 树与 inclusion / consistency proof（纯函数）

> **会话**：20260730-0000-claude-code ｜ **仓库**：imboy ｜ **状态**：Slice 4 完成，E2EE-065 整体仍 `PENDING`
> **授权依据**：用户放行「纯函数实施刀」（`30-...` §2 决策四）。
>
> ⚠️⚠️ **实现 profile ≠ 接受 profile**。`29-e2ee-065-transparency-profile-v1.md`
> 仍是**未签字的冻结草案**；playbook 明写 profile 须由安全 reviewer 人工接受，
> **loop 不得自我接受**。本刀的存在不构成对该 profile 的任何形式批准，
> 也不改变 `human_gate.adr_14_19: BLOCKED`。

---

## 1. 做了什么

新增 `src/lib/e2ee_kt_merkle.erl`（纯函数）+ `test/lib/e2ee_kt_merkle_tests.erl`（24 例，已入门禁）：

| 能力 | profile 出处 |
|---|---|
| `canonical_event_bytes/1`、`canonical_head_bytes/1` | §3、§5 |
| `leaf_hash/1`、`node_hash/2`（`0x00`/`0x01` domain separation） | §2、§4 |
| `mth/1`（RFC 6962 Merkle Tree Hash） | §2 |
| `inclusion_path/2` + `verify_inclusion/5` | §6 |
| `consistency_path/2` + `verify_consistency/5` | §6 |
| `tree_head_signing_input/1`（`0x02` 前缀） | §5 |

**未做**：不接 DB、不接 HTTP、**不产生也不持有签名私钥**（profile §7：私钥不在
DB/repo/日志/API）。sequencer（Slice 1 已定案：leaf index 必须与 `bigserial` 解耦）、
proof API、gossip、独立 monitor 分别属其后各刀。

---

## 2. 方法：生成侧直译规范，验证侧迭代，两者穷举交叉核验

- **proof 生成**照 RFC 6962 §2.1.1/§2.1.2 的递归定义直译（`PATH` / `SUBPROOF`）——
  它是本模块里最接近规范原文、因而最可逐行核对的一段；
- **proof 验证**必须走迭代算法（验证方**拿不到整棵树**，这正是 KT 的意义）；
- 两者互为对照：测试穷举 n≤16 的**全部** `(index, size)` 与 `(m, n)` 组合。

**这个方法立刻见效——它抓到了本刀的真 bug。**

---

## 3. 穷举交叉核验抓到的真 bug（本刀最有价值的部分）

首次运行：`Failed: 1. Passed: 23`，红的正是 `consistency_exhaustive_accepts_test`。

定位（不猜，跑出失败集）：

```
FAIL pairs: [{5,6},{9,10},{9,11},{10,11},{9,12},{10,12},{11,12},{13,14}]
```

⚠️ **全部是非平衡情形**。根因：`verify_consistency` 里左兄弟分支的判据我写成了
`Node band 1 =:= 1`（只判奇），**漏掉了 `orelse Node =:= Last`**。
当 `Node =:= Last` 时，一个**左兄弟**被当成右兄弟处理，
**旧根那一路（`Fr`）就此永远不再更新**。

以 m=5, n=6 为例：走到第二段 path（`MTH(d0..d3)`，是左兄弟）时 `Node=2, Last=2`，
错误判据把它送进右兄弟分支，`Fr` 停在 `leaf(d4)`，永远等不到 `Root1`。

> ⚠️ **中途我还走错过一次方向**：第一次修复时我判断「不该有沿右脊爬升那一步」，
> 把它删掉——**仍然红**，失败集一模一样。爬升步骤是必需的，错的只有判据。
> 这一步记在这里是因为它正是本项目反复点名的失效模式：
> **静态判断"哪里不对"不可靠，得让穷举告诉你**。

修复后：

| 范围 | 结果 |
|---|---|
| 门禁内 n≤16 | 24/24 全绿 |
| 额外扫描 n≤64（**2080 个 `(m,n)` + 2080 个 `(index,size)`**） | consistency FAIL=0、inclusion FAIL=0 |

n≤64 的扫描是**一次性核实**，未进门禁（跑起来更慢，且 n≤16 已覆盖全部
非平衡分支形态）。复跑命令见 §7。

---

## 4. 空验证（negative control）

五条，四条精确变红、**一条全绿（第三条真发现）**：

| 空验证 | 手法 | 结果 |
|---|---|---|
| A | leaf 与 node 共用同一前缀 | `Failed: 3` —— domain separation + 两组 golden vector |
| B | 去掉 `\n`/`\r` 守卫 | `Failed: 3` —— 三条 canonical 拒收用例 |
| C | 把 consistency 判据改回只判奇 | `Failed: 1` —— 即 §3 那个 bug，**可复现** |
| D | **去掉最终 `Last =:= 0` 校验** | **All 24 passed** ⚠️ |
| E | tree head 前缀改成与 leaf 相同 | `Failed: 2` |

恢复后 24/24，无漂移。

### 4.1 关于 D（未被任何用例区分的一道校验）

`Last =:= 0` 要求「path 恰好把树高走完」。删掉它测试全绿，说明**没有任何用例
能把它与其它防线区分开**。

**未据此删除**，也**未编造一个能让它变红的用例**。理由是推理层面的：
删掉后接受仍需 `Fr =:= Root1` 且 `Sr =:= Root2`；path 被截短意味着少做若干次
`node_hash`，此时 `Fr` 是某个低层子树哈希，要它等于 `Root1` 属于 SHA-256 碰撞。
故该分支在抗碰撞假设下**不可达**，留着是零成本的纵深防御。

⚠️ **该结论是推理，不是实证**——与本文件其它「已实证」条目性质不同，
特此标注（见 §8）。

---

## 5. 覆盖矩阵（24 例）

| 组 | 覆盖 |
|---|---|
| 1 对照组（2） | 空树根 == 公认 `SHA-256("")`；`MTH([E1]) == leaf(E1)`。**两重外部自校验**：它们红则后面所有向量都不必看 |
| 2 golden vectors（4） | `E1` canonical **长度 96**；三条 leaf hash；`MTH` 于 n=1/2/**3**；tree head canonical **长度 168** 与签名输入。**n=3 是最小非平衡树**，用它才能区分 RFC 6962 分裂规则与朴素两两配对 |
| 3 canonical 编码（6） | 与 golden 逐字节相等（入参 map 书写顺序**刻意打乱**，证明按字典序而非书写序）；无尾随换行；`\n`/`\r` 在 value 与 key 均拒；key 含 `=` 拒；**非单射注入的具体形态**；空 map / 非 map |
| 4 domain separation（2） | 把内部节点的两个子哈希拼成一条 64 字节「事件」，`leaf(x) != node(a,b)`；head 前缀 != leaf 前缀 |
| 5 inclusion（4） | **穷举 n≤16 全部叶子必须验通过**（正向锚点）；换叶子/换 root/path 增删/挪 index 全拒；**逐段**篡改 path（n=11 的每个 M、每一段）；参数畸形 |
| 6 consistency（6） | **穷举 0<m≤n≤16 必须验通过**；**分叉历史必须拒**（把旧树末叶换掉再拿原 proof 验，n=9 全部 m）；逐段篡改（n=13）；同尺寸要求空 path 且两根相同；m<n 时空 path 不得蒙混；参数畸形（含 m>n 回滚） |

> 第 6 组的「分叉历史必须拒」是 KT 真正要防的东西：**日志不得悄悄改写历史**。

---

## 6. 验收

```
IMBOYENV=local make eunit t=e2ee_kt_merkle_tests → All 24 tests passed
make e2ee-verify                                 → All 385 tests passed（上轮 361，+24）
erlfmt --check（两个新文件）                      → All matched files use erlfmt code style
git diff --check                                 → 通过
```

新模块已加入 `Makefile` 的 `e2ee-verify` Modules 清单（纯函数、无 DB 依赖，
适合进硬门禁）。imboyapp 侧未改动。

---

## 7. 复算 / 复跑

```bash
# 门禁
IMBOYENV=local make eunit t=e2ee_kt_merkle_tests

# n≤64 一次性穷举扫描（不在门禁内）
erl -noshell -pa ebin -eval '
Ev = fun(I) -> <<"k=", (integer_to_binary(I))/binary>> end,
Evs = fun(N) -> [Ev(I) || I <- lists:seq(1,N)] end,
BadC = [{M,N} || N <- lists:seq(1,64), M <- lists:seq(1,N),
   begin Ds=Evs(N), R1=e2ee_kt_merkle:mth(lists:sublist(Ds,M)),
   R2=e2ee_kt_merkle:mth(Ds), P=e2ee_kt_merkle:consistency_path(M,Ds),
   not e2ee_kt_merkle:verify_consistency(M,N,P,R1,R2) end],
io:format("consistency FAIL=~p~n",[length(BadC)]), init:stop().'
```

profile §8.4 的 Python 独立复算脚本不变，仍可用于第三方核对。

---

## 8. 残留风险

1. ⚠️ **profile 未签字** —— 本刀实现的是**未被接受的草案**。若安全 reviewer
   在接受时改动任何冻结项（hash / 前缀 / 字段集 / canonical 规则），
   **本模块与全部 golden vector 都要跟着改**；
2. ⚠️ **未接线** —— 无 DB、无 API、无 sequencer。
   `olm_identity_repo.erl:46` 至今仍是 `ON CONFLICT DO UPDATE` **就地覆盖身份键**
   （调研文档已实证），**服务端替换身份键后数据库不留痕迹这一问题，本刀一分未动**；
3. **签名与验签未实现** —— `tree_head_signing_input/1` 只算到签名输入为止。
   Ed25519 签名、`key_id`、双签过渡窗口、「两把都过期 → fail-closed」（profile §7）
   全部未做；
4. **`Last =:= 0` 校验无用例区分**（§4.1）—— 推理层面判定不可达，**非实证**；
5. **未做恒定时间比较** —— root/hash 比较用 `=:=`。这些都是**公开值**
   （root hash 本就要对外发布），不构成 oracle；但该性质是**推理**，未做侧信道分析；
6. **n>64 未扫描**，超大树（真实日志会远超 64 叶）只由算法结构保证，未逐一实证；
7. **未与第三方 CT 实现做互操作** —— golden vector 目前是「本实现 + profile 文档里的
   Erlang/Python 两套」三方一致，**尚未与 Trillian 等外部实现对拍**。

---

## 9. 认识论状态

| 结论 | 状态 |
|---|---|
| profile §8 全部 golden vector 与本实现逐字节一致 | **已实证**（测试钉死，`29-...` §10 残留 1 就此关闭） |
| inclusion / consistency 在 n≤64 上生成与验证自洽 | **已实证**（2080 + 2080 组合，0 失败） |
| 判据漏 `Node =:= Last` 会让非平衡树的 consistency 验不过 | **已实证**（真 bug + 空验证 C 可复现） |
| 「不该有沿右脊爬升那一步」 | **已被推翻**（删掉后失败集一模一样） |
| leaf/node/head 三处 domain separation 各自生效 | **已实证**（空验证 A/E） |
| `Last =:= 0` 在抗碰撞假设下不可达 | **推理，未实证**（§4.1） |
| 哈希比较不构成 oracle | **推理，未实证**（§8 残留 5） |
| 本刀对生产的影响 | **零** —— 无调用方，无 DB/HTTP 接线 |

---

## 10. 未做

- 不接受 profile、不改 ADR、不改任何既有任务的状态标记、不改发布等级。
- 不 push、不部署、不访问生产、不通知第三方。
