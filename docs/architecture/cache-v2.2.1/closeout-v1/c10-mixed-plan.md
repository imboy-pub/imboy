# C10 — 混合版本 dcheck 按冻结协议更新（CACHE_V221_BACKEND_CLOSEOUT_V1）

> 更新时间：2026-10-12（Asia/Shanghai）
> 状态：CODE_UPDATED_COMPILE_VERIFIED（编译 0 error / 0 warning；按红线未运行）
> 唯一 owner 文件：`scripts/cache_v21_mixed_dcheck.erl`、本文件
> 协议依据：`protocol-budget-frozen.md`（C01 冻结，含 2026-10-11 实施的 r1/r2 修订）

## 1. INTEGRATION_BASE 机制确认（无 BLOCKED）

混合组网机制在脚本内完整存在，三个新场景均直接复用：

- 旧版本节点 = INTEGRATION_BASE `3a01829d`（V2 引入前的 main）经 git worktree
  + `make app` 独立构建树（构建方式见 `w1-w5-report-20261011.md` W5-05 节），
  其 ebin 路径经环境变量 `LEGACY_EBIN` 传入 `main/1` → `start_mixed/2`。
- 新版本节点 = 当前构建树，经 `NEW_EBIN` 传入。
- `peer_boot/4` 与 `restart_peer_as/2` 按角色（legacy/new）选择构建树的
  `ebin` + `deps/{depcache,syn}/ebin`；driver beam（本脚本模块）经 `-pa DD`
  同时挂在新旧节点 code path，因此 peer 端辅助函数在两棵树上均可加载。
- frozen 协议断言只 rpc 到新节点（B/C）执行——proto 层
  （`imboy_cache_sync_proto`）仅存在于新树；旧节点 A 在同场断言
  catch-all 兼容（sync 存活、不受 V2/漂移事件影响）。

## 2. 新增场景表（插入位置：M6 之后、R1 之前——此时 A=legacy、B/C=new
   的混合拓扑未被 R 系列回滚破坏）

| 名称 | 协议依据（frozen 文档节号） | 断言 oracle | 编译产物验证记录 |
|---|---|---|---|
| M7 hash mismatch: drifted-node events rejected (reject_total++, value frozen) | 第 1 节（config_hash/registry_hash 不一致 → 事件整体拒绝 `{reject, hash_mismatch}`，计数 reject_total，peer 标记 hash_divergent） | C 改 `cache_registry_overrides` 漂移后 put：B 上值保持 v1；B 侧 `reject_total` 较场景内清零基线 ≥1；`imboy_cache_sync_proto:hash_divergent(C)` = true；A（legacy）sync 存活；恢复配置后 v3_heal 在 B 收敛（一致事件自动清除 divergent 标记 + 显式删除兜底，同 C9b 风格） | erlc 单文件编译通过，见第 4 节 |
| M8 epoch shift: stale-epoch events rejected after peer bump | 第 1 节（epoch 小于本节点 → 拒绝 `{reject, stale_epoch}`）+ 第 3 节（epoch 单调推进；旧 epoch 重放一律拒绝） | B `bump_epoch()` → 1（`epoch()` 读数复核 =1）；C（epoch=0）put 旧 epoch 事件：B 值保持 v1、`reject_total` ≥1、A 存活；`{sync_epoch, 0}` 回落后 v3_heal 收敛（不污染 R1-R3）。invalidate 不参与 epoch 比较的豁免（第 1 节）由 src 既有单测覆盖，不在混合场景重复 | erlc 单文件编译通过，见第 4 节 |
| M9 old incarnation replay in grace: accepted-but-no-resurrect (r1) | 第 2 节（收口修订 r1：宽限集合内旧化身（W=720s）→ ok；旧值防复活由 Lamport revision + epoch 承担，不依赖化身） | 前提 oracle：C 重启换化身后，B 侧宽限条目 `{grace_incarnation, {C, OldBoot}}` 存在（ets member）；重放（`inject_v2_set_frozen`，origin=C 旧化身 + 旧 revision）：B 值保持 v2_newinc（revision older → 不复活）；接受性证明：同一宽限化身携带更高 revision（counter+100000）的 `grace_accepted` 在 B 收敛——直接证明化身层不拒绝（r1 修订动机：拒绝旧化身将使重启 peer 全部事件被永久拒绝）；A 存活 | erlc 单文件编译通过，见第 4 节 |
| M10 r2 handshake: 3-tuple frame, legacy 2-tuple still answered | 第 4 节（收口修订 r2：`version_request` 扩为 `{cache_version_request, FromPid, FromNode}` 3 元组；旧 2 元组仍被应答走 syn 视图反查，兼容 C10 混合矩阵旧节点；`version_report` 9 元组尾增发送者 sync pid） | `rpc(B, imboy_cache_sync_proto, version_request_frame, [self()])` 精确等于 `{cache_version_request, SelfPid, B}`；2 元组兼容路径：删除 C 侧 `{peer_incarnation, B}` 后向 B sync 发送 `{cache_version_request, C}`，converge（≤10s 有界轮询）断言 C 侧注册重建——即 9 元组 report 经视图反查到达且被 `handle_version_report` 处理的可观察副作用；B/C/A sync 全存活 | erlc 单文件编译通过，见第 4 节 |

当前验收矩阵共 **16 个场景**：主集群依次运行 M1-M3、U1、L1、L2、M6-M10、
R1-R3（14 个）；M4/M5 因会销毁所有新版内存副本，在独立尾部集群再次运行（2 个）。
U1 与 L1/L2 覆盖升级、保留权威离开及回归；M4/M5 不再由其他场景隐式替代，失败均进入
同一结果 JSON 并使驱动非零退出。历史 13/13 证据不覆盖当前矩阵。

## 3. 代码变更清单（仅 `scripts/cache_v21_mixed_dcheck.erl`）

| 变更 | 说明 |
|---|---|
| 新增场景 M7-M10 | 见第 2 节；场景函数 m7_hash_mismatch/3、m8_epoch_shift/3、m9_old_incarnation_replay/3、m10_r2_handshake/3 |
| 新增 peer 端导出 | `inject_v2_set_frozen/5`（C01 冻结字段信封：epoch/config_hash/registry_hash 由执行节点现场计算，OriginBootId 可指定）；`reject_reset/0` / `reject_read/0`（elib_metric 节点计数器探针）；`read_rev_value/1`（Lamport revision 读取） |
| 改造 `inject_v2_set/4` | 原构造缺 C01 新增字段，冻结协议下会被结构性拒绝（missing_protocol_field）；现转调 frozen 版，boot_id 用 `<<0:128>>`（第 2 节快照回放合法特例） |
| 新增 driver 端辅助 | `ensure_new_ready/1`（C10 场景前置，同 C9 ensure_c9_ready 语义）；`converge_true/2`（谓词有界轮询，等待协议副作用而非固定 sleep） |
| 时间戳真实化 | `iso_utc/0` 由秒级 + 固定 `.000` 占位升级为 `calendar:system_time_to_rfc3339(erlang:system_time(millisecond), [{unit, millisecond}, {offset, "Z"}])`（毫秒级真实 UTC）；场景内 `expires_at_ms` 均为 `erlang:system_time(millisecond) + TTL`，无伪造占位 |
| 既有 warning 清零 | `restart_peer_as` 未用变量 `FullName` → `_FullName`；`stop_all` 中弃用 `catch` 表达式 → `try ... catch` （基线 2 warning → 0） |
| 头注释 | 补 M7-M10 场景说明与 frozen 文档节号引用 |

## 4. 编译产物验证记录

```text
命令（仓库根目录）：
  erlc -I include -o /tmp/mixed_dcheck_build scripts/cache_v21_mixed_dcheck.erl

结果：0 error，0 warning
产物：/tmp/mixed_dcheck_build/cache_v21_mixed_dcheck.beam（23000 bytes，2026-10-12 07:09）
基线对照：更新前同命令编译为 0 error / 2 warning（未用变量、弃用 catch），本轮一并清零。
```

按任务红线：未运行 dcheck、未 `make`/`make eunit`、未启动任何 Erlang 节点、
未占用 4323/46xxx/48xxx 端口、无 git add/commit；未修改
`scripts/cache_v21_dcheck.erl` 与 `src/` 下任何文件。真实 BEAM 混合组网
执行（`LEGACY_EBIN`/`NEW_EBIN` 两树 + `erl -noshell -pa ... -eval
'cache_v21_mixed_dcheck:main([...])'`）留待后续互斥验证窗口；完整 16 场景均需通过，
两轮独立运行并绑定同一冻结候选。
