# E2EE-013 实施计划 — Crypto API 设备所有权（token DID 绑定）

> 状态：Planned（用户已选方向=DID 绑定进 token）。因认证子系统高 blast-radius + 会话成本高，建议新会话执行。
> 授权：用户已授权作为 P0 漏洞修复；git 身份 leeyi <leeyisoft@qq.com>。

## 根因（已核实）
- `token_ds:decrypt_token` 仅返回 `{ok, Uid, Exp, Sub}`，**token 不含 DID**。
- `did` 来自 HTTP header（`auth_ds:verify_sign` 做 HMAC），但**未进认证 State**。
- `olm_handler` 写端点（report_identity/prekeys/fallback）`device_id` **取自 body** → 同账号任一设备 token 可覆盖另一设备密钥。

## 方向（用户定）：DID 绑定进 token
写端点从认证上下文取 DID，body device_id 仅一致性校验；legacy 无 DID token 对 crypto 写 fail-closed，读保持兼容。

## Blast radius（精确）
- `src/ds/token_ds.erl`：加 `did` claim。
  - 新 `encrypt_token(ID, Did, Second, Sub)`，`did => Did`；旧 `encrypt_token(ID,Second,Sub)` 委托 Did=<<>>；加 `encrypt_token(ID,Did)`、`encrypt_refreshtoken(ID,Did)`。
  - `decrypt_token` 提取 `maps:get(did, Payload, <<>>)`，返回 `{ok, ID, Exp, Sub, Did}`（4→5 元组）。
- 更新 3 个 decrypt_token 调用点加 `_Did`/`Did` 到模式：`src/ds/auth_ds.erl`(verify_token)、`src/ds/websocket_ds.erl`、`src/api/passport_handler.erl`(刷新)。
- `src/ds/auth_ds.erl`：`verify_token` 返回 `{ok, Uid, Did}`；`do_authorization` 写 `current_did` 进 State handler_opts；加 `current_did/1` 访问器。
- 4 个出令牌点传 `did` header：`src/api/qr_login_handler.erl`、`src/api/passport_handler.erl`(login)、`src/logic/passport_logic.erl`(token+refreshtoken) —— 需把 `did` header 从 Req 线程进 passport_logic（确认签名）。
- `src/api/olm_handler.erl`：写端点 `report_identity/prekeys/fallback` 用 `auth_ds:current_did(State)`；body device_id 必须等于 current_did 否则 403；current_did 为空（legacy token）对写 fail-closed（403）。
- refresh 流程：用旧 token 的 DID 重签新 token（DID/generation 不变）。

## 验收（DT-01/02）
- A(设备D1)token 写 body device_id=D2 → 403，identity/OTK/fallback 表无变化。
- body DID 缺失/空/重复字段/超长/Unicode 混淆均不绕过。
- 已撤销/禁用设备（user_device 非 active）不能上传 → 需接 user_device active 校验。
- 分层不反向依赖；SQL 全参数化。
- `make compile`、相关 EUnit、`make format-check` 全绿。
- ⚠️兼容影响：现有已登录用户 token 无 DID → crypto 写 fail-closed 直到重新登录/刷新（计划 S1.3 接受）。

## 依赖后续
- E2EE-014（trust freshness，加 migration 00000047 + event_id/issued_at/expires_at）同 Batch B03，但改 trust schema，需另行确认作 P0 修复。
- 完整 device-bound session（可撤销）= E2EE-030（需 ADR 16 签字）。
