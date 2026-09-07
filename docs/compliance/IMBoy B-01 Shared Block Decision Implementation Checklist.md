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

## P0 缺口（不依赖矩阵决策，可先行修复）

**拉黑/解除拉黑后缓存 10 天不失效**——add/remove 时未 flush `in_denylist`
缓存键，拉黑后最长 10 天内 B 仍可发消息给 A（验收明确要求
"cache invalidates immediately"）。修法：

```erlang
%% user_denylist_logic:add/2 与 remove/2 成功后：
imboy_cache:flush({in_denylist, Uid, DeniedUserId}),
imboy_cache:flush({in_denylist, DeniedUserId, Uid})   %% 若矩阵为对称阻断
```
EUnit：拉黑后立即 in_denylist 翻转、解除后立即翻转（mock imboy_cache 或
用真 memo 的 flush 验证）。

## 待矩阵拍板后的工程项

1. **决策模块**：`user_block_decision.erl`（单点判定：can_dm/can_call/
   can_friend/can_mention/can_invite——读矩阵常量，边界全部改调此模块，
   不再各自散查 in_denylist）。
2. **边界接线**：mention（群消息 @ 解析处）、invite（group/channel/workspace
   邀请 handler）、profile/search（按矩阵决定是否仅隐藏入口）。
3. **WS 消息路径**：WS 直发消息（若与 msg_c2c_logic 不同链）需同款拦截。
4. **测试矩阵**：A↔B 双向 × DM/call/friend/mention/invite × 拉黑/解除；
   DB 失败 fail-closed（直连场景判定异常时默认拒绝）。
5. **B-02 UX**：被拦截方的可预期错误文案（复用 in_denylist S2C 既有提示）、
   拉黑/解除入口状态展示。

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
