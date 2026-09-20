# API: Group 域回归（`/api/v1/group/*`、`/api/v1/group_member/*`）

## Contract

| Field | Value |
| --- | --- |
| Handler | `group_handler`（`add/detail/dissolve`）、`group_member_handler`（`join/leave/page/role`） |
| Routes | `POST /api/v1/group/add`、`GET /api/v1/group/detail?gid=`、`POST /api/v1/group/dissolve`、`POST /api/v1/group_member/join`、`POST /api/v1/group_member/leave`、`GET /api/v1/group_member/page?gid=`、`POST /api/v1/group_member/role` |
| Content-Type | `application/json`（page/detail 为 GET，query string 传 `gid`） |
| Authentication | JWT 必带（Bearer token）+ device-sign 头（`api_auth_switch=on`，`auth_ds:verify_sign/2`） |
| Response | HTTP 200 + `{code,msg,sv_ts,payload}` envelope；仅认证边界（无/坏 token）返回真实 HTTP 401 |
| Throttle | `group/add` 按 uid 每用户 3 秒 1 次；`group_member/join|leave` 按 caller `{group_member, uid}` 3 秒 1 次；`group/dissolve` 按 `{group, gid}` 每小时 1 次；`group_member/role` 按 `{group_member_role, uid}` 3 秒 1 次。suite 用独立用户/独立 gid 设计规避限流误伤 |
| Executable suite | `test/rest/suites/api_v1_group_SUITE.erl` |

行为事实源（取证基线）：

- `src/api/group_handler.erl` / `src/api/group_member_handler.erl` — 参数、限流位置、守卫顺序
- `src/logic/group_logic.erl` / `src/logic/group_member_logic.erl` — `add/5`（personal scope）、`dissolve/2`、`validate_role_permission/3`（管理员 3..副群主 5）
- `src/ds/group_ds.erl` — `dissolve_group/4`（非群主 → owner-only 错误；成功 → 硬删除群行）；`group_member_ds:join_group/5`（upsert 幂等）
- `src/lib/elib_pg.erl` `one/2` + `src/repo/group_repo.erl` — **缺行返回 `{ok, #{}}`（空 map），不是 error**
- `include/group_role.hrl` — 角色矩阵（1 成员 / 3 管理员 / 4 群主 / 5 副群主）

## 取证确认的真实行为（写进断言，不臆造）

1. **缺行 ≠ 错误**：`group/detail` 对不存在的 gid 返回 HTTP 200 + `code=0` + `payload={}`（handler 只把 SQL 失败 `{error,_}` 映射为 `"群组不存在"`，缺行走 `elib_pg:one` 默认空 map → success 空载荷）。
2. **dissolve 不存在 gid**：`owner_uid` 取默认 0 ≠ 操作者 → `code=1`，`msg="只有拥有者才能够解散该群，或者群已解散"`（与"非群主解散"同一错误）。
3. **join 不存在 gid**：空群行使 `member_max=0/member_count=0`，容量差 0 → `code=1`，`msg="群成员已满。"`（不是 `"群组不存在"`）。
4. **重复 join 幂等**：handler 先查 `list_member` 命中已有成员 → 直接回 `code=0` + 现有成员列表；DS 层 `upsert_active` 亦幂等。
5. **leave 恒成功**：`group_member/leave` 无论自退还是被踢（无权限时被吞）都回 `code=0`；退群效果通过后续 `group_member/page` 的 `"你不是群成员"` 证明。
6. **非成员可读 detail**：personal 群的 `group/detail` 无成员门（代码注释 P1-7b 明示任何持 JWT 用户可读，仅 workspace 群有 403 守卫）——因此群域的 Authorization 断言落在 dissolve/role/join 上，不对 detail 臆造 403。

## Cases

### GROUP-001: Happy Path — 建群 → detail → 邀请 → 成员页 → 退群 → 解散

#### Given

Fixture 创建 owner、member 两个用户并各自登录。

#### When

1. owner `POST /api/v1/group/add` `{}`；
2. owner `GET /api/v1/group/detail?gid=<gid>`；
3. owner `POST /api/v1/group_member/join` `{gid, member_uids: [<member>]}`；
4. member `GET /api/v1/group_member/page?gid=<gid>`；
5. member `POST /api/v1/group_member/leave` `{gid, member_uids: [<member>]}`；
6. member 再查 `group_member/page?gid=<gid>`；
7. owner `POST /api/v1/group/dissolve` `{gid}`；
8. owner 再查 `group/detail?gid=<gid>`。

#### Then

- 步骤 1：`code=0`，`payload.group.id`/`payload.group.owner_uid` 为当次 TSID gid 与 owner uid；`payload.member_list` 含 owner。
- 步骤 2：`code=0`，`payload.id`/`payload.owner_uid` 与 gid/owner 一致。
- 步骤 3：`code=0`，`payload.member_list` 含 member（uid 以 JSON 字符串传入，logic 层转 integer）。
- 步骤 4：`code=0`，成员列表含 member。
- 步骤 5：`code=0`，`payload.gid` 回显。
- 步骤 6：`code=1`，`msg="你不是群成员"`（退群生效的真实证明）。
- 步骤 7：`code=0`，`payload.gid` 回显；群行被硬删除（`DELETE FROM "group"`）。
- 步骤 8：`code=0`，`payload={}`（缺行空载荷语义，见上文行为 1）。

### GROUP-002: Authentication — 无 token / 坏 token

#### Given / When / Then

同 FRIEND-002 形态，目标是 `GET /api/v1/group/detail?gid=<随机不存在的 gid>`：无 authorization → HTTP 401 + `code=401` + `"未登录，请先登录"`；垃圾 Bearer → HTTP 401 + `code=706`。

### GROUP-003: Authorization — 非群主不可解散；非成员不可邀请他人

#### Given

owner 建群并邀请 member；outsider 已登录但不在群内。

#### When

1. outsider `join` `{gid, member_uids: [<member>]}`；
2. member `dissolve` `{gid}`；
3. member `GET detail?gid=<gid>`。

#### Then

- 步骤 1：`code=1`，`msg="你不是群成员"`（邀请他人必须自己是成员；outsider 邀的是 member 而非自己，故触发成员门而非 self-join 通道）。
- 步骤 2：`code=1`，`msg="只有拥有者才能够解散该群，或者群已解散"`，`payload={}`。
- 步骤 3：`code=0`，`payload.id` 仍为该 gid —— 被拒绝的解散未删除群。

### GROUP-004: Not Found — 从未存在的 gid

#### Given

owner 已登录；gid 为当次随机生成（< 2^62）。

#### When

1. `GET /api/v1/group/detail?gid=<ghost>`；
2. owner `POST /api/v1/group/dissolve` `{gid: <ghost>}`；
3. owner `POST /api/v1/group_member/join` `{gid: <ghost>, member_uids: [<owner>]}`（self-join 通道）。

#### Then

- 步骤 1：HTTP 200，`code=0`，`payload={}`（真实契约：缺群返回空成功）。
- 步骤 2：`code=1`，`msg="只有拥有者才能够解散该群，或者群已解散"`。
- 步骤 3：`code=1`，`msg="群成员已满。"`（容量门落到空群行）。

### GROUP-005: Role 管理 — 成员越权 / 群主提权 / 角色值越界

#### Given

owner 建群并邀请 member（role=1）；stranger 已登录、不在群内。

#### When

1. member `role` `{gid, user_id: <member>, role: 3}`；
2. owner `role` `{gid, user_id: <member>, role: 3}`；
3. stranger `role` `{gid, user_id: <member>, role: 9}`。

#### Then

- 步骤 1：`code=1`，`msg="你没有权限修改群成员角色"`（普通成员 < 管理员 3）。
- 步骤 2：`code=0`，`msg="success."`，`payload={gid, user_id}`（群主提权成功）。
- 步骤 3：`code=1`，`msg="角色值必须在1-3之间"`（参数守卫先于成员资格守卫，故非成员也得到参数错误）。

### GROUP-006: Validation — join 参数错误；重复 join 幂等

#### Given

三个独立 caller（空列表/非列表/gid=0 各一，规避 `{group_member, uid}` 3 秒限流）+ owner、member。

#### When

1. `join` `{gid: <ghost>, member_uids: []}`；
2. `join` `{gid: <ghost>, member_uids: 42}`（非列表）；
3. `join` `{gid: 0, member_uids: [...]}`；
4. owner 建群并邀请 member；
5. member 自身再 `join` `{gid, member_uids: [<member>]}`。

#### Then

- 步骤 1：`code=1`，`msg="member_uids 不能为空"`。
- 步骤 2：`code=1`，`msg="member_uids 必须是list"`。
- 步骤 3：`code=1`，`msg="group id 格式有误"`。
- 步骤 4：`code=0`，成员列表含 member。
- 步骤 5：`code=0`，`payload.member_list` 仍含 member —— 重复加入幂等（handler 命中已有成员直接回成功），登记为真实 Idempotency 语义。

## 无法覆盖项（本批不做及原因）

- `group/qrcode`（302 重定向 + `exp/tk` md5 签名依赖 `solidified_key` 配置）与 `face2face/face2face_save`：涉及二维码签名与地理位置，留待专门任务。
- workspace 群（scope=workspace 的 403/409/980 语义）：依赖 workspace 域 fixture，超出 RTF-07 范围。
- `group/edit`、`group/page`、`transfer`、`mute/unmute`、`alias`：首批最小闭环不含；角色 Authorization 已由 `role`/`dissolve` 覆盖。
