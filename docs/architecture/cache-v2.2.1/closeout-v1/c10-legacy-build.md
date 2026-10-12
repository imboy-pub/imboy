# C10 — 旧版本构建树（INTEGRATION_BASE 3a01829d）预构建记录

> 构建时间：2026-10-12（Asia/Shanghai）
> 状态：BUILD_OK（make app 退出码 0；按红线只编译、未运行任何节点/测试）
> 消费方：`scripts/cache_v21_mixed_dcheck.erl`（W5-05 混合组网 + C10 M7-M10 场景）
> 机制依据：`c10-mixed-plan.md` 第 1 节（旧节点 = worktree + `make app` 独立构建树）

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
