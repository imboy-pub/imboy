# 清理日志 / Cleanup Log

> 记录 2026-06-13「可发布 v1 打包」附带的中等清理。原则：只删无歧义垃圾，**不做 git 操作**（删除留工作区，可 `git checkout` 回滚）；有歧义的一律记录待决，不自动删。

---

## 1. 已删除：无歧义垃圾文件（15 个，均未被 git 跟踪）

| 类型 | 文件 | 证据 |
|---|---|---|
| macOS 垃圾 | `.DS_Store` ×13（根、docker/、test/、deploy/、docs/、priv/、api/、src/、docker/imboy_pg18/** 等） | Finder 自动产物，非源码，未跟踪 |
| 崩溃转储 | `./erl_crash.dump`、`./priv/erl_crash.dump` | Erlang 崩溃转储，运行期产物，未跟踪 |

删除前已逐个 `git ls-files --error-unmatch` 校验：**全部未被 git 跟踪**，删除不影响版本库。

---

## 2. 未删除：xref 结果不可靠（不作为删除依据）

`make xref` 输出 ~38 条 "Undefined function ... called by ..."，但研判为**假阳性**，不可作死代码依据：

- 关键反证：`imboy_policy:origins_view/1` 与 `imboy_policy_view:origins_view/1` **互相**报 undefined，但两个模块（`src/lib/imboy_policy.erl`、`src/lib/imboy_policy_view.erl`）**都真实存在且互相调用**。互报 undefined 说明 xref 运行在**不完整的 beam 集**上（部分模块未编译/未加载进 xref path），而非真的函数缺失。
- 结论：当前 xref 输出**不能用于删代码**。正确做法是先 `make`（完整 build）成功、确保全部 beam 就位，再跑 `make xref` 复核。

---

## 3. 待决清单：可能的断裂调用（需完整 build 后复核，未改动）

以下调用点 xref 报 undefined，**部分可能是真实 bug、部分是 xref 假阳性**，需完整 build + 人工核对后再定，本次**未删未改**：

- `cowboy_req:qs_val/3`（被 `messaging_logic:history/2`、`reaction_list/2`、`read_stats/2` 调用）—— `qs_val/3` 在 Cowboy 2.x 已移除，**若该路径仍在用则为真实 bug**；需确认 messaging_logic 是否仍被路由。
- `wallet_ds:topup/3`（被 `wallet_logic:topup/3` 调用）—— 钱包充值链路，需确认。
- `elib_cnv:to_integer/1`、`to_binary/1`、`to_list/1`（多处）—— elib_cnv 迁移后这些 helper 是否还在？
- `elib_pg:update/3`、`elib_pg:delete/3`（announcement_ds、group_schedule_repo）。
- `auth_logic:verify_for_assets/4`、`verify_for_open/3`（auth_handler:assets/2）。
- `channel_logic:get_message_reactions/2`、`get_pinned_messages/1`、`refund_order/2`。
- `lager:add_sink/2`、`syn:count/1` 等第三方 API（大概率 xref 未加载 deps beam 的假阳性）。

> 建议：作为独立「死代码/断裂调用治理」任务，先 `make` 全量编译通过后再 `make xref`，逐条 `grep` 确认调用方是否仍被路由/apply 间接调用，再决定删除或修复。Erlang 的 xref 对 handler/router 间接调用与 behaviour 回调有系统性误报，切勿据原始输出直接删。
