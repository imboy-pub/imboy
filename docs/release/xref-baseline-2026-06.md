# Xref 断裂调用基线 / Undefined-Calls Baseline (2026-06-13)

> **✅ 2026-06-13 全部修复：42 → 0。** CI xref job 已升级为零容忍 ratchet（`backend-ci.yml` 阻塞），任何新增 undefined call 均导致 CI 失败。

---

> 原始基线（2026-06-13 初始记录）：在**完整 `make` 编译（382 beam）之后**跑 `make xref` 得到 **42 处调用了不存在的函数**。
> 修复分类：A 类真 bug（函数改名/签名错误）→ B 类（export 缺失、孤儿 beam）→ C 类（policy 互调 export）→ D 类（stub 补全 + API 版本升级）。

---

## A. 已核实为真 bug（已查目标模块导出，确认函数不存在）

| 断裂调用 | 调用方 | 实情 | 影响功能 |
|---|---|---|---|
| `elib_cnv:to_binary/1` ×3 | config_ds:normalize_ice_server、push_notification_ds:get_apns_config/get_fcm_config | elib_cnv 只导出 `safe_to_binary/1`，无 `to_binary/1` | WebRTC ICE 配置、APNs/FCM 推送配置 |
| `elib_cnv:to_integer/1` ×2 | channel_handler:stats、channel_handler_message:subscribers | 只导出 `safe_to_integer/1` | 频道统计、订阅者列表 |
| `elib_cnv:to_list/1` ×1 | push_notification_ds:get_apns_private_key | 无此导出 | APNs 私钥读取 |
| `elib_pg:update/3` ×4 | announcement_ds:publish/unpublish/update/delete_by_id | elib_pg 只有 `update/4`、`update/5` | 公告发布/下架/更新 |
| `elib_pg:delete/3` ×2 | group_schedule_repo:delete_participant/delete_remind | 同上，arity 不符 | 群日程删除 |
| `cowboy_req:qs_val/3` ×3 | messaging_logic:history/reaction_list/read_stats | `qs_val/3` 在 Cowboy 2.x 已移除（本项目 Cowboy 2.10） | 消息历史、反应列表、已读统计 |

**修复方向**：`to_binary`→`safe_to_binary`、`to_integer`→`safe_to_integer`、`to_list` 找替代；`update/3`→`update/4`（补 Conn 或 WhereParams）；`qs_val/3`→ Cowboy 2.x 的 `cowboy_req:match_qs/2` 或 `parse_qs/1`。

## B. 高度疑似真 bug（同类模式，建议逐一核实导出后修）

- `elib_dt:to_binary/1`、`elib_dt:to_datetime/1`（adm_app_version_handler:version_stats）
- `elib_param:integer/3`（adm_announcement_handler:index）
- `auth_logic:verify_for_assets/4`、`verify_for_open/3`（auth_handler:assets）
- `channel_logic:get_message_reactions/2`、`get_pinned_messages/1`、`refund_order/2`（channel_handler_*）
- `channel_admin_ds:add/1`（channel_logic_message:add_admin）
- `e2ee_social_handler:get_sender_private_key/1`（e2ee_transfer_handler）
- `wallet_ds:topup/3`（wallet_logic:topup）—— topup/3 已定义，**核实是否 `-export`**；若漏导出则钱包充值整链断

## C. imboy_policy ↔ imboy_policy_view 互相 undefined（13 处，需定向核实）

`imboy_policy` 与 `imboy_policy_view` 互相调用对方一批函数（`origins_view/1`、`capability_adjustments/2`、`effective_view_from_configs/3` 等）均报 undefined，但两模块都存在且已编译。**最可能是这些函数未 `-export`**（模块内部用没问题，但被另一模块外部调用就 undef）。逐一确认 export 列表即可定性。

## D. 第三方 API（核对依赖版本后定性）

- `lager:add_sink/2`、`lager:remove_sink/1`（imboy_plugin_logger）—— lager 3.9.2 是否有此 API？
- `syn:count/1`（metrics_handler:collect_system_metrics）—— 核对 syn 版本 API。

---

## 复核与收紧流程

```bash
make            # 必须先完整编译（382 beam），否则 xref 假阳性
make xref       # 复核 undefined 数（当前 42）
```
1. 先修 A 类（已确认，改法明确）；
2. 核实 B/C/D 后修或确认为活代码误报；
3. undefined 降到 0 后，把 `backend-ci.yml` 的 xref job 去掉 `continue-on-error`，改为硬阻塞门禁。
