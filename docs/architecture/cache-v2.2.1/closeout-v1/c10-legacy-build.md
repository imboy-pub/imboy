# C10 — 旧版本构建树（INTEGRATION_BASE 3a01829d）预构建记录

> 构建时间：2026-10-12（Asia/Shanghai）
> 状态：BUILD_OK（make app 退出码 0；按红线只编译、未运行任何节点/测试）
> 消费方：`scripts/cache_v21_mixed_dcheck.erl`（W5-05 混合组网 + C10 M7-M10 场景）
> 机制依据：`c10-mixed-plan.md` 第 1 节（旧节点 = worktree + `make app` 独立构建树）

## 当前重建材料（2026-10-13）

原 `.ai/c10-legacy-worktree` 已不存在。冻结提交仍可读取，已在仓外
`/private/var/folders/8m/wbjj0qmn4ml0mgn56wm56j2m0000gn/T/imboy-c10-legacy-jgg4llal`
从 `3a01829d32c94bdb15f80150445d0c1b71b88a8b` 归档重建，独立 `make -j1 app`
退出码为 0，生成 904 个 BEAM。依赖使用现有依赖的独立副本，清除了副本中的
旧构建缓存；共享依赖未修改。新旧 `include/deps.mk` 相同，运行时关键依赖
depcache/syn 位于 2.0.0/3.4.2 tag；依赖复制不等于重新从上游验证全部依赖。

`imboy_cache`、`imboy_cache_sync`、`elib_metric` 与依赖定义的源码 Git blob
均匹配冻结提交；旧缓存/同步/指标 BEAM 存在，V2 proto/snapshot BEAM 不存在。
日志为仓外 `/tmp/cache-c10-legacy-rebuild.log`。这只恢复构建前置，当前候选
C10 混合矩阵、两轮验证和 C12 证据绑定尚未完成。以下保留原构建的历史记录。

当前混合启动诊断（2026-10-13，非 C12 候选）：原驱动同时连接三节点后
初始化，新版无 READY 权威，150 秒仍 RECOVERING。调整为 B 自然单节点
READY 后接入 C 和旧 A，C 在约 17 秒内 DEGRADED：旧提交完全没有版本
报告能力，当前接收端等待所有来源分类，不能开始快照。不能把静默对端
直接排除，否则会将失联新版误当旧版。
驱动现先建立 B/C 新版权威组再接入 A；隔离 bootstrap 诊断通过并清理节点，
只证明场景准备可运行，不证明 M3 混合重启恢复。启动异常改为写 FAIL JSON
且清理已创建 peer，PID 在可达性等待前登记。完整 C10 仍未通过。

M1/M2 真实旧/新节点定向诊断通过：未分类旧 SET 可传播且不创建版本行；
受管键旧 SET 经同发送者到同接收 PID 的 ping_ok 屏障确认处理后，现有值和
五字段版本行不变，拒绝计数增长；新版事件不会使旧节点崩溃，旧版仍可传播
未分类键。这不是完整混合矩阵、生产网络或 C12 候选绑定证据。
在同一真实混合组网内完成 M1/M2 后执行 M3，重启 C 实际返回 DEGRADED，
READY 断言失败；混合恢复缺口已由原场景路径复现。旧节点实际加载独立树
的 syn_pg BEAM；该树与当前依赖的 syn_pg 源码摘要相同，直接 PID 发送实现
支持上述处理屏障。诊断日志在仓外，不计为完整 C10 PASS。

加入 `membership_capability_gate_v1` 后重新使用真实旧/新三节点诊断：
旧节点在本地 syn 视图中未声明 V2，公开 put/delete 和直接 V2 SET 广播均
拒绝 `incompatible_peers`；原 M6 旧节点失效场景仍通过。此前允许复制成功
的 M1/M2 诊断是门禁前记录，后续完整矩阵需按 C02 能力交集要求修订，
不能删除旧节点或跳过拒绝来制造 PASS。

## 1. Worktree

| 项 | 值 |
|---|---|
| 路径 | `/Users/leeyi/project/imboy.pub/imboy/.ai/c10-legacy-worktree` |
| HEAD | `3a01829d32c94bdb15f80150445d0c1b71b88a8b`（detached） |
| 提交 | `test(tsid): 批量跨块预算守恒黑盒回归（慢墙钟编排）`（2026-10-11 00:49:43 +0800） |
| 创建命令 | `git worktree add .ai/c10-legacy-worktree 3a01829d` |

## 2. 构建

| 项 | 值 |
|---|---|
| 命令 | `cd .ai/c10-legacy-worktree && make app` |
| 退出码 | 0 |
| 耗时 | 254.33s real（user 104.84s / sys 53.58s，含全量 deps 拉取编译） |
| 编译日志 | `/tmp/c10_legacy_build.log`（一次性产物，不入仓） |

## 3. 关键产物验证

| 产物 | 结果 |
|---|---|
| `ebin/imboy_cache_sync.beam` | 存在（4436 bytes；`ebin/` 共 904 个 beam） |
| `ebin/imboy_cache.beam` | 存在（6752 bytes） |
| `ebin/elib_metric.beam` | 存在（10488 bytes；`init_on_peer/0` 依赖，旧树自身 src 内） |
| `deps/depcache/ebin/depcache.beam` | 存在 |
| `deps/syn/ebin/syn.beam` | 存在 |
| `ebin/imboy_cache_sync_proto.beam` / `imboy_cache_sync_snapshot.beam` | **不存在（正确）** —— V2 模块在 3a01829d（V2 引入前 main）尚未诞生 |

## 4. LEGACY_EBIN 应设的确切路径

```bash
LEGACY_EBIN=/Users/leeyi/project/imboy.pub/imboy/.ai/c10-legacy-worktree
```

**注意：指向 worktree 根目录，不是 `ebin/` 子目录。**
变量名虽叫 `LEGACY_EBIN`，但脚本语义是"构建树根"：
`main/1` → `start_mixed(LegacyRoot, NewRoot)` → `peer_args/2` 取
`filename:join(Root, "ebin")` 与 `filename:join(Root, "deps/" ++ D ++ "/ebin")`
（D ∈ `[depcache, syn]`）。只给 `ebin/` 子目录会让 peer 起不来（缺 deps 路径）。

## 5. 旧树版本特征（只读验证）

### 5.1 协议形态：纯 legacy，无 version 握手

旧树 `src/lib/imboy_cache_sync.erl` 仅 102 行，协议为：

- 广播：`syn:publish(imboy_cache, dsync_handler, {cache_sync, Message})`
- 接收：`handle_sync_message/1` 只认
  `{set, Key, Data, MaxAge, Depend}` / `{flush, Key}` / `flush`，
  其余 **catch-all 忽略**（M2/R 场景的兼容性来源）
- `version_request` / `version_report` **完全不存在**——3a01829d 是
  "V2 引入前的 main"，比 V2 早期帧更老的基线

与任务预期描述（"旧 2 元组请求 / 8 元组 report"）的差异说明：
"旧 2 元组请求"是 **V2 系列早期版本** 的帧格式，并非 3a01829d 树的帧。
M10 的 2 元组兼容路径由 driver **模拟发送给新树 B**（脚本注释"模拟旧版
V2 节点"；新树 `src/lib/imboy_cache_sync.erl:431` 有 2 元组走视图反查的
兼容处理），不依赖旧树 A 具备该帧。旧树 A 在混合矩阵中只承担 legacy
set/flush 通路与 catch-all 忽略——与脚本头注释"INTEGRATION_BASE
（V2 引入前的 main）"完全一致，**基线选择正确**。

### 5.2 组网前提（混合场景可用性）

| 宏 / 常量 | 旧树 | 新树（当前 main） | 一致性 |
|---|---|---|---|
| `?CACHE_SCOPE`（syn scope） | `imboy_cache`（include/chat.hrl:4） | `imboy_cache`（include/chat.hrl:4） | 一致 |
| group 名 | `dsync_handler`（模块内宏） | `dsync_handler` | 一致 |
| `?DEPCACHE_SERVER` | `imboy_cache`（include/cache.hrl:3） | `imboy_cache`（include/cache.hrl:3） | 一致 |

脚本 `init_on_peer/0` 的 `syn:add_node_to_scopes([imboy_cache])` 与
`depcache:start_link(imboy_cache, ...)` 与上述取值吻合。

### 5.3 场景文件

旧树 `scripts/` 下无任何 `cache_v2*` 场景文件（grep 无匹配，正确——
C9 场景 `cache_v21_dcheck.erl` 与 `cache_v21_mixed_dcheck.erl` 均为
3a01829d 之后产物；主仓现有
`cache_v2_bench.erl` / `cache_v21_dcheck.erl` / `cache_v21_mixed_dcheck.erl`）。

## 6. 注意事项

1. **worktree 不要删**：C10 混合 dcheck 执行时（`LEGACY_EBIN`/`NEW_EBIN`
   两树组网）直接复用本构建树；删除后需按本文第 1-2 节重建（约 4 分钟）。
2. **`.ai/` 原本未被 `.gitignore` 覆盖**（任务前提偏差，实测主
   `.gitignore`、`.git/info/exclude`、全局 excludes 均无该条目），构建后
   主仓 `git status` 一度出现 `?? .ai/`。已追加 `.ai/` 到
   `.git/info/exclude`（纯本地管理文件、不入库、不影响工作树；删除该行
   即回滚）。若要根治建议后续把 `.ai/` 写进 tracked 的 `.gitignore`
   （需另行确认，本次未动 tracked 文件）。
3. worktree 处于 **detached HEAD**，勿在其中做任何 commit / branch /
   checkout 操作，保持 3a01829d 原样。
4. beam 为本机 OTP 工具链编译产物；若执行环境 Erlang/OTP 大版本变更
   （beam 兼容性断裂），需 `make app` 重建。
5. 按红线：本次仅编译，未启动任何 Erlang 节点、未运行 dcheck/eunit、
   未触碰主仓工作树文件、无 git add/commit、未修改
   `scripts/cache_v21_mixed_dcheck.erl`。
