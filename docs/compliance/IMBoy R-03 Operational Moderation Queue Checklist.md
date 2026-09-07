# IMBoy R-03 Operational Moderation Queue Checklist

> 实施计划 Task R-03。前置：R-02 处置动作执行器（已完成）、A-01 消息访问分层（已完成）。
> 状态：**已实施（2026-09-07）**。

## Goal（计划原文要点）

让非 E2EE/公开内容的审核队列真实运转，绝不扫描 E2EE 明文；确定性关键词规则优先；按内容面定义 queue/quarantine 语义；定义误报处理与 SLA 字段；不引入新 AI provider。验收=受支持的非 E2EE UGC 产生可追溯判定；E2EE 消息发送永不向服务端审核暴露明文。

## 现状勘察（起点）

- 敏感词黑名单 + review_queue 表 + Admin CRUD（R 前置已有，脚手架）；
- **全仓无任何生产写入方**——队列永远为空（Gap Matrix：「生产内容不入队，reject 不处置」）。

## 实施内容

### 1. 唯一 policy 入口（src/logic/moderation_policy.erl，新模块）

- `inspect(Surface, Text)` 三态：`allow` / `{blocked, Hits}` / `{queued, Hits}`；
  - 归一化：小写 + 全角 ASCII 变体（U+FF01..FF5E → 半角）+ 零宽字符剥离（U+200B/200C/200D/FEFF），词表同归一化，防变体绕过；
  - **severity=high → blocked**（quarantine：发布前直接拒绝，对用户返回「内容包含违规词汇，发布失败」）；
  - **medium/low → queued**（queue：先发后审，内容照常发布并写人工复核队列）；
  - 词表 60s memo 缓存；**词表读取/缓存故障一律 fail-open**（放行 + ERROR LOG）——检查设施故障≠违规证据，不把公开内容发布面整体打挂；
  - 非 binary/空文本直接 allow；损坏 UTF-8 字节丢弃不放过检查。
- `enqueue/7` 写 `review_queue`（零迁移复用：msg_type=surface、to_type=channel/moment、hit_words 逗号分隔、status=pending）；失败返回 {error,_} 由调用方执行 fail-open。

### 2. 内容面接入（两个公开/半公开 UGC 面）

| 面 | 接入点 | blocked | queued |
|---|---|---|---|
| channel_message | channel_logic_message:do_publish_message | 落库**前**拦截 | 落库后入队（含 MessageId），入队失败 fail-open 发布不受影响 |
| moment_post | moment_logic:create_post | 落库前拦截（validate 后） | 发帖成功后入队（含 PostId），fail-open |

### 3. reject 联动处置（adm_moderation_logic，修「reject 不处置」缺口）

- reject 且 surface=channel_message → `channel_message_ds:delete` + 广播消息删除；
- reject 且 surface=moment_post → `moment_ds:delete_post_by_admin`；
- approve → 误报放行，内容保留（同时建议 Admin 删除对应误报词表项）；
- 撤下为尽力而为：review_find/删除/通知任何故障仅 ERROR LOG，**不回滚已落库的审核判定**（truthful），内容删除幂等可重试。

### 4. SLA 与误报

- 队列行新增 `overdue` 标记（pending 且 created_at 超 24h）——Admin 分页接口自动附带，按 SLA 优先级处理；
- 误报处理：approve 流转 + hit_words 记录命中词（便于回溯并清理误报词条）。

### 5. E2EE 边界（验收红线）

- **接入面仅 channel/moment**（公开/半公开明文）；C2C/C2G 私信发送路径（msg_c2c_logic/msg_c2g_logic/websocket）**零改动**——strict 模式下服务端只有密文，无明文可审；
- E2EE 内容违规面走 R-01 举报证据模型（客户端主动提供明文证据），本任务不触碰。

## 测试证据（2026-09-07）

| 门 | 结果 |
|---|---|
| moderation_policy_tests（新） | **7/7**：归一化（全角/大小写/零宽）、allow 空文本不打词表、high blocked、medium queued、词表故障 fail-open、入队参数形态、入队失败透传 |
| adm_moderation_logic_tests | **12/12**（+4：reject 撤频道帖、approve 不撤、reject 撤动态、overdue SLA 标记三种状态） |
| moment_logic_tests 回归 | 15/15（fail-open 修复后） |
| channel_publish_idempotency_tests 回归 | 通过 |

## 已知边界与后续

- ~~**profile 面（昵称/简介）本轮未接入**~~ → **R-03.1 已接入（2026-09-08，见下节）**。
- 队列 Admin 页的 overdue 列展示与「按 overdue 排序」属 imboyadmin UI 打磨，接口字段已就绪。
- 词表命中为朴素包含匹配（人工维护词表量级几十~几千），超大规模词表再考虑 AC 自动机——当前不过度设计。
- 踩坑记录（第 4 次）：heredoc/Python 写入的测试中文 binary 字面量再次缺 /utf8（fun 头 pattern 里的中文同样要 /utf8）；`elib_dt:now/0` 返回 rfc3339 binary 不是整数，毫秒基准用 `elib_dt:millisecond/0`；跨模块复用返回结构前核对 key 形态（review_page 是 atom key `list`，不是 `<<"list">>`）。

## R-03.1：profile 文本公开面接入（2026-09-08）

计划 R-03 Files 要求 "one policy entry point called by enabled profile/channel/moment writes"，首轮接入 channel/moment 两面，本批补齐 profile 面。

**实施**（零新表、零迁移，复用 review_queue）：

1. `moderation_policy.erl`：新增 surface `profile_field`；入队形态 `{profile_field, FieldBin}` 把字段名编码进 `review_queue.msg_type`（`profile_field:sign`），`to_type=profile`。
2. `user_logic.erl`：`update/3` 的 `set_field` 分支接 `profile_review_gate/3`——文本公开字段白名单 `nickname/sign/profession/school/interests`（透传白名单的非文本字段 avatar/background/region/birthday 不审）；high 命中拒绝保存（`{error, {1, <<"">>, Msg}}`，handler 透出）；medium/low 先保存后入队；入队失败 fail-open。
3. `adm_moderation_logic.erl`：`remove_surface_content/1` 加 profile 分支——reject 时按 msg_type 后缀定位字段、清空该字段（msg_id 复用 uid）。清空而非恢复原值：reject 时原值语义已不可信，交由用户重新设置；清空失败仅记日志不回滚审核判定（与 channel/moment 容错同口径）。

**测试**：

| 套件 | 结果 |
|---|---|
| user_logic_tests | +5（high 拒绝零落库 / queued 保存+入队参数 / allow 零入队 / 非文本字段跳过 / 入队失败 fail-open）；39 passed，3 failed 为注销链既有环境红（elib_tsid 注册+PG 池，HEAD 对照同红，与本批无关） |
| moderation_policy_tests | +1（profile 入队 msg_type 编码断言）→ 8/8 |
| adm_moderation_logic_tests | +2（reject 清字段 / 清空失败不回滚）→ 14/14 |
| 回归 moment_logic_tests | 15/15 |
| 回归 channel_logic_message_tests | 12/12 |

**边界**：改名频率风控（per-field rate limit）不在本批范围；blocked 文案与 moment/channel 同串（"内容包含违规词汇"族）。
