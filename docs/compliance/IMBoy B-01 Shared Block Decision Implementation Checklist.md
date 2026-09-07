# IMBoy B-01 Shared Block Decision — Implementation Checklist

> 供实施会话直接开工的地基勘察与任务拆解。2026-09-05 由 D-04/R-02/L-01 收官会话产出。
> ⚠️ 前置：**拉黑方向性矩阵为 pending-owner 产品决策**（见文末选项），开工前需 owner
> 拍板；矩阵之外的本清单工程项可先行（尤其 P0 缓存缺口修复）。

## 地基现状（已勘察）

- **模块**：`user_denylist_ds/repo/logic/handler` 四件套齐全；
  `user_denylist_logic:in_denylist/2`（Uid 是否拉黑了 DeniedUserId）为判定入口，
  `imboy_cache:memo` **缓存 10 天**（864000 秒）。
- **已覆盖边界**：C2C 发消息（`msg_c2c_logic` in_denylist reject + S2C 提示）、
  通话（`webrtc_ws_logic`）、好友申请（`friend_logic` blocked 拒绝 + friend_ds
  pending_status 派生 none|pending|friends|blocked，拉黑>好友>申请中）。
- **未覆盖边界（Gap Matrix 所指旁路）**：mention（群内 @）、invite（群/频道/
  workspace 邀请）、profile 查看、search 搜索、频道评论/动态互动。

## P0 缺口（✅ 已修复，imboy `ad14434e`）

~~拉黑/解除拉黑后缓存 10 天不失效~~ **定性修正**：主路径 in_denylist
本就即时 flush（验收满足）；真缺口为 check_relationship3 旁路缓存
（TTL 300s）不被拉黑操作失效——转发等旁路场景最长 5 分钟漏拦。

**已修**：user_denylist_logic add/remove 成功后调
`friend_ds:invalidate_cache/2`（覆盖 is_friend2/check_relationship3
双向与依赖标签）。denylist 套件 3/3。

## 待矩阵拍板后的工程项

1. **决策模块**：`user_block_decision.erl`（单点判定：can_dm/can_call/
   can_friend/can_mention/can_invite——读矩阵常量，边界全部改调此模块，
   不再各自散查 in_denylist）。**雏形已先行**：`user_denylist_logic:blocked_between/2`
   （2026-09-07，双向任一拉黑即 true + DB 异常 fail-closed），矩阵拍板后并入。
2. **边界接线**：mention（群消息 @ 解析处）、invite（group/channel/workspace
   邀请 handler）、profile/search（按矩阵决定是否仅隐藏入口）。
   **invite 中不依赖矩阵的部分已接线**（见下节）。
3. **WS 消息路径**：~~需勘察~~ **已勘察（2026-09-07）：同链无需接线**——
   WS 消息经 `message_router_logic` 路由到 `msg_c2c_logic:c2c/3`，与 HTTP
   共用 check_relationship 拦截；websocket_handler 仅引用 c2c_client_ack。
4. **测试矩阵**：A↔B 双向 × DM/call/friend/mention/invite × 拉黑/解除；
   DB 失败 fail-closed（直连场景判定异常时默认拒绝）。**已完成 invite 维度**
   （channel/workspace 双向 + fail-closed 用例）；其余维度随矩阵接线补。
5. **B-02 UX**：被拦截方的可预期错误文案（复用 in_denylist S2C 既有提示）、
   拉黑/解除入口状态展示。

## 2026-09-07 补充勘察与接线（不依赖矩阵的部分）

- **AI 主动消息（ai_agent_proactive）无缺口**：仅 E2EE 门无拉黑门，但其三个
  调用场景均不构成拉黑绕行——注册欢迎（新用户不可能预拉黑 agent）、运营报告
  （运营者自配通道）、agent 回复（用户先主动发起；若拉黑则发起本身已被
  msg_c2c_logic 拦截，回复无从触发）。
- **invite 接线（已完成）**：频道邀请 `channel_logic_invitation:create_invitation`
  与工作区邀请 `workspace_logic:invite` 是点对点直接接触（与好友申请同级），
  「存在拉黑关系就不撮合」在矩阵三个选项下语义一致，先行接线：
  - 新增 `user_denylist_logic:blocked_between/2`（双向判定；与 check_relationship3
    旁路的 fail-open 不同，邀请非关键路径按 B-01 口径 **fail-closed**）；
  - 命中返回 `{error, {403, <<"存在拉黑关系，无法(发送频道)邀请…"/utf8>>}}`；
  - EUnit：blocked_between 3 用例（双向+fail-closed）+ channel_logic_tests
    补门放行 mock 与 blocked 用例（201/201）+ workspace_logic_tests 同（29/29）。
- **仍卡矩阵的旁路（勿先接线）**：mention（群内 @ 属「共同群可见性」范畴）、
  群拉人/自主加入群（拉黑后被拉入新群 vs 既有共同群保留，三选项未回答）、
  profile 查看、search（checklist 原判：按矩阵决定是否仅隐藏入口）。

## 拉黑矩阵选项（pending-owner，建议默认「对称阻断直连」）

| 选项 | 语义 | 影响 |
|---|---|---|
| **对称阻断直连**（建议默认） | 拉黑后双向私聊/通话/好友申请/邀请全阻断；共同群可见性保留 | 实现最简（in_denylist 双向查询）、群聊体验无损 |
| 仅阻断接收侧 | B 发送成功无感知，A 端静默丢弃 | 收件侧逐条过滤，成本高 |
| 全场景隐藏 | 含共同群互不可见 | 破坏群聊体验（成员突然消失），成本最高 |

## 环境注意

- 本地验收后端 9801（main 热更）或干净重编；user_denylist 表结构与既有
  add/remove/page 契约以 `user_denylist_repo` 为准。
- 缓存键格式 `{in_denylist, Uid, DeniedUserId}`（imboy_cache:memo）。
