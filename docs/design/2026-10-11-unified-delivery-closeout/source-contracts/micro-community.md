# IMBoy 极简微社群落地与验收计划 / Minimal Micro-Community Delivery Plan

日期：2026-10-08

版本：V1

状态：`PARTIAL_LOCAL_CANDIDATE_CLOSED（2026-10-11 收口）`——26 项：MC-AC-21 PASS（候选 a7dc619b 双 GREEN：r4+r11）、MC-AC-23 PASS，其余 24 项如实 PARTIAL（blocker 在台账行内）；verifier INTEGRITY_FAIL=protected WIP drift 14 文件（主树并发演进，结构性不可修）；verdict 见 `~/.codex/micro-community-runs/mc-closure-20261010T060003Z/final/`
本轮授权：编写计划与验证计划完整性；不实施业务代码、不运行迁移、不创建执行分支、不提交、不 push、不部署。

## 1. 目标与边界 / Objective and Scope

产品形态：一个微社群由一个 `channel` 内容区和一个 `group` 讨论区组成。频道承载帖子、订阅与付费权益，群承载成员讨论。复用现有消息、评论、反应、附件及角色能力，不建立 `community` 独立表族。

本计划 Required 范围：

1. 单频道绑定单讨论群；反向也最多关联一个频道。
2. 新增 `channel.discussion_group_id`；本版同时实施 `channel_message.is_essence`，不以“可选”绕过验收。
3. 免费、私有、付费频道的讨论准入及持续授权；退订、退款、到期、停用后的撤权。
4. 复用频道内容，补精华、帖子标签契约与频道内帖子正文检索。
5. Flutter 现有频道入口增加讨论入口；Admin 现有频道详情增加绑定管理，不新增社区管理导航。
6. 本地真实 PostgreSQL、真实 Admin 浏览器、Android 真机的业务闭环；不据此宣称 iOS/macOS 或生产资格。

极简约束：首版只允许 `scope=personal` 的频道与群绑定；企业工作区资源维持现状，绑定 workspace 资源明确拒绝，不自动转换归属。不做多群、多对多关系、逐帖自动转发到群、跨群线程映射、算法推荐、课程、会员等级、AI 或新支付网关。

这不是“完整复制 Telegram”。Telegram 对外暴露关联讨论群标识，但公开 API 字段不能证明其内部数据库结构。IMBoy 的关联、授权与迁移必须以本仓实际契约为准。

## 2. 已核实基线 / Verified Baseline

以下是计划编写时快照，不是执行候选。MC-00 必须重新采样。

| 独立仓库 | HEAD | 已观察到的受保护 WIP |
|---|---|---|
| `imboy` | `96a06a37fc0d53f6cf7081fde063f99f855d0276` | 未跟踪的 `docs/business/imboy-open-source-productization-strategy-v1.md` |
| `imboyapp` | `192d106e0869fde4359594d521ce1d1415356a5c` | 下列 4 个文件有修改 |
| `imboyadmin` | `62c40f27ba498049dd911e9f7d4e9d1929620c66` | 本次采样未见工作树修改 |

App 受保护路径：

- `lib/page/channel/widgets/channel_publish_bar.dart`
- `lib/page/chat/chat/attachment_handler.dart`
- `lib/page/chat/chat/chat_page.dart`
- `lib/page/group/group_discovery/group_discovery_page.dart`

后端受保护文档的本次 SHA256：`30daec1253a9828613b1952a2dbacfb99a96e6d04a3cce3311668764e9e9d29c`。执行时新增 WIP 也必须纳入保护；不得复制外来 WIP 当成自己的实现。

| 能力或原判断 | 当前证据 | 分类与处理 |
|---|---|---|
| 频道帖子、JSON payload、置顶、浏览、反应 | `priv/migrations/00000003_channel.up.sql:421` | 已有结构，`GAP_ACCEPTANCE`；复用，不重建 |
| 频道标签 | 同文件 `channel.tags` | 已有结构；帖子标签仍需校验、读写、筛选契约 |
| “fts_channel 已实现内容检索” | `00000069_channel_discovery.up.sql:35` 只索引频道名称与简介 | `CONTRACT_CONFLICT`；帖子正文检索为 `GAP_IMPLEMENTATION` |
| “进群只看 channel_subscription 状态” | `channel_logic_common:ensure_channel_content_access/2`；付费分支调用 `channel_order_ds:has_purchased/2` | `CONTRACT_CONFLICT`；不得用订阅记录替代购买权益 |
| 付费权益有效期 | `channel_order_repo:has_purchased/2` 读取 paid 状态及 `subscription_end_at` | 已有判定，`GAP_ACCEPTANCE`；订单付款超时 `expires_at` 不等于会员到期 |
| 讨论群绑定、精华 | 本次 source/API/schema 搜索未发现目标字段 | `GAP_IMPLEMENTATION`；执行前再次确认无并行实现 |
| 群成员及角色 | `00000001_foundation.up.sql:4424` | 已有结构，需补关联后的完整权限回归 |
| “支付只差通电” | 本轮未验证真实网关签名、入账、退款及对账 | `BLOCKED_EXTERNAL`（真实支付资格）；不能宣传已完成 |

“90% 复用”仅作为方向，不是量化完成率；字段存在不等于接口、端侧、权限与投产均已验收。

## 3. 固定设计契约 / Frozen Design Contract

### 3.1 最小数据改动

计划 SQL 示例，不是可直接运行的生产迁移；MC-00 分配不冲突的迁移编号。

```sql
ALTER TABLE public.channel
    ADD COLUMN discussion_group_id bigint
    REFERENCES public."group"(id) ON DELETE RESTRICT;

CREATE UNIQUE INDEX i_channel_discussion_group
    ON public.channel (discussion_group_id)
    WHERE discussion_group_id IS NOT NULL;

ALTER TABLE public.channel_message
    ADD COLUMN is_essence boolean NOT NULL DEFAULT false;
```

- 反向查询索引建立在存储绑定字段的 `channel` 上，不是 `group` 上。
- 旧频道默认无绑定，旧帖子默认非精华；不批量迁移旧群、不更改已有 `type/scope` 含义。
- FK 防悬空、唯一索引防一群多频道；跨表归属和操作权限在事务中校验，不用跨表 CHECK 假装保证。
- 禁止删除群时自动 SET NULL 使受限讨论群失去门槛；删除、停用及解绑走明确业务流程。
- 正文 FTS 如需索引，优先使用 `channel_message.content` 的 GIN 表达式索引，不新增内容表或平行 `fts_channel_message` 表。`jiebacfg` 需在隔离库确认可用；缺失不能悄悄改成频道名称检索。

### 3.2 绑定及生命周期

- 操作者同时具备既有频道管理权限与群管理权限；平台管理员沿用现有受审计授权路径，不新增万能绕过。
- 只绑定 personal 域、启用状态、尚未被绑定的新讨论群。群须私有且仅邀请加入；初次绑定时只有拥有者一个成员，且没有既有聊天、公告、文件等成员内容，避免接管旧群及暴露历史数据。
- 默认要求频道拥有者与群拥有者一致；继承既有 creator/owner 的权威判定，不按客户端传来的 user_id 授权。
- 普通订阅者不得通过标签、payload、群邀请或参数篡改改变绑定和门槛。
- 绑定、同值重试、解绑、替换均事务化；冲突有稳定错误码，成功有现有审计记录。锁顺序在 MC-00 冻结，重试不重复副作用。
- 首版不允许对已有普通成员或内容的绑定执行解绑/换群。空绑定可解除；非空返回冲突并保持原关系。不得通过停用频道后解除关联逃避此限制。
- 软删除/停用频道保留绑定并拒绝讨论访问；物理删除关联频道不得直接解除付费门槛。实际删除入口必须执行相同守卫。拥有者转移也须维护双端管理约束。

### 3.3 有效权限，而不是一次性进群校验

绑定群的有效动作权限 = 既有群动作权限 + 关联频道有效性 + 频道权益校验。

- 普通成员须有有效群成员身份、有效频道订阅，并通过 `ensure_channel_content_access/2` 对应的权威规则；私有邀请与 paid 订单有效期继续由已有流程决定。
- 公共免费频道的“能看公开内容”不等于“已加入讨论”：先走现有订阅，再显式申请/加入群；订阅本身不自动进群，退群不自动退订频道。
- 付费规则使用 paid 且未过 `subscription_end_at` 的订单；订阅脏行、待付款、退款、取消、支付超时订单均不授予讨论权限。
- 管理例外必须同时满足既有频道管理与群管理规则；“只有频道管理员”不自动成为群管理员。
- 所有 REST/WS 发送、历史读取、成员与附件读取、通知、在线广播、离线拉取、旧入口都受同一生效判定约束；缓存不是最终授权源。不得只把 UI 按钮隐藏。
- 退款/退订/停用事务提交后，以及 DB 时钟越过权益截止后，新授权动作不得继续通过。撤权与发送/广播竞争需要明确线性化点：授权和对应写入不能用无锁的分离查询留下 TOCTOU；退款提交后发起的推送授权不得通过，队列需在交付边界重验。
- 复用现有退群/通知机制，不硬删除聊天历史或成员审计。残留成员行不能作为授权，成员名单/人数须区分有效成员与历史记录。
- 已经到达用户设备的数据无法远程保证抹除；验收针对撤权后的新获取与新投递。撤权前已授权且在传输中的数据需在证据中区分，不能伪称可追回。
- 不修改 E2EE 协议、不静默降级。MC-00 冻结允许组合；若既有密钥撤权流程不足以支持绑定 E2EE 群，则该组合明确拒绝，不假装完成加密资格。

### 3.4 内容契约与产品门禁

- `is_essence` 与 `is_pinned` 相互独立；频道管理员可以增删精华，普通作者不因是作者获得管理权限。
- 帖子标签沿用 `payload.tags`：数组最多 5 项；每项 trim 后 1–20 个 Unicode 字符；去重；拒绝非数组、非字符串、超限输入。编辑标签保留原 payload 其他字段，未提交 tags 不清空它。
- 帖子检索在明确频道内检索正文；支持精华和标签筛选，继续采用现有分页约定。排除撤回/删除内容；未授权时不返回正文、片段、总数或私有标签。外部 ID、游标用现有 TSID 契约，不引入 JS number 精度损失。
- `fts_channel` 继续用于找频道，不改变其语义。逐帖评论保留既有 `channel_comment`，不另外建立讨论线程映射。
- 仅在已有 channel 与关联能力均启用的产品包中显示入口。`phase1_delivery` 的频道发现/订单等交付锁继续 fail-closed；旧缓存或远程配置不能重新打开。不得为这项功能自动改全局 feature manifest。

## 4. 任务卡与依赖 / Tasks and Dependencies

责任角色仅划分所有权，不代表本轮启动 Agent；默认串行执行。未经独立授权不启动并行子 Agent。

| 任务 | 依赖 | 唯一责任范围 | 交付物与完成条件 |
|---|---|---|---|
| MC-00 基线与契约冻结 | 无 | 集成负责人；三仓只读盘点、资源、验收清单 | `baseline.json`、WIP 哈希、入口清单、`requirements.json`、资源 lease、加密/产品包矩阵；确认 26 个 Required IDs 无删减 |
| MC-01 数据与 API 契约 | MC-00 | 后端数据负责人；新迁移、channel/message 序列化、OpenAPI/AsyncAPI 与三端 DTO 契约 | 绑定/精华两列及索引；fresh/upgrade/down 测试；稳定字段及错误码，不覆盖并行迁移 |
| MC-02 绑定生命周期 | MC-01 | 后端；既有 `channel_logic*`、DS/Repo、管理接口与审计 | 绑定/同值重试/受限解绑、删除与转移守卫；权限/归属/并发/失败事务测试 |
| MC-03 讨论持续授权 | MC-02 | 后端；共享群权限判定及全部实际调用入口 | 全入口覆盖矩阵；复用频道订单权限；REST/WS/附件/推送/离线撤权与竞态测试，非绑定群行为不变 |
| MC-04 内容能力收敛 | MC-01、MC-03 | 后端；频道消息精华/标签/检索、评论与反应回归 | 单一路径内容读写；真实 FTS、标签筛选、分页及 IDOR 测试；不新建帖子表 |
| MC-05 Flutter 极简入口 | MC-02、MC-03、MC-04 | App 负责人；现有频道详情/内容/API/model/router/i18n及测试 | 频道→订阅/权益提示→讨论群→返回内容；精华/标签/检索；旧缓存、深链、撤权处理；受保护 WIP 路径无 handoff 则 `BLOCKED_NO_HANDOFF` |
| MC-06 Admin 管理闭环 | MC-02、MC-04 | Admin 负责人；现有频道详情/内容页、API/types及测试 | 同权限范围空群选择、绑定确认、冲突提示、精华管理；复用现有组件与 `EntityId`，不新增社区导航 |
| MC-07 集成与证据 | MC-03至MC-06 | 集成负责人；隔离 PG、全门、真实浏览器/真机、验收脚本 | 26 IDs 实测；API/DB/WS/端侧证据同一候选；迁移恢复演练；异常不能以 mock 替代 |
| MC-08 独立终审 | MC-07 | 只读验收负责人；重算候选、证据、ID集、WIP | 独立重跑 verifier，核验副作用与范围；只在全部 Required PASS 时签 `LOCAL_CANDIDATE_PASS` |

执行顺序：MC-00 → MC-01 → MC-02 → MC-03 → MC-04 → MC-05 → MC-06 → MC-07 → MC-08。计划不引入新插件引擎、通用权益平台或大型新测试框架。

### 4.1 实施影响面必须盘点完整

MC-00 输出具体路径和函数，不以本节候选列表替代调用点调查：

- 数据：`priv/migrations/`、`channel_repo/channel_message_repo`、各 SELECT 安全列、DS 缓存键及失效。
- 业务：`channel_logic_common`、`channel_logic_subscription`、`channel_logic_order`、`group_member_logic`、群创建/邀请/扫码/面对面入口、`msg_c2g_logic` 与所有成员权限消费者。
- 群附件、文件、相册、公告、reaction、pinned、mention、RTC、红包、Bot/MCP/internal 等入口：未在产品包启用可验证 fail-closed，但不得留下可直接调用的绕过。对 personal 资源不适用的 internal/workspace 入口须验证拒绝。
- 传输：WebSocket、S2C、在线群广播、重试、离线拉取、撤权前生成的通知任务。
- 契约：`api/openapi-api.yaml`、`api/openapi-adm.yaml`、必要 AsyncAPI、三端 EntityId/TSID 解析、错误码/i18n、产品交付锁与文档。
- App/Admin：调用方、model/API、列表/详情、导航、测试 fixtures、旧数据缓存；不要改 `ios/*`、`macos/*`、`plugin/r_upgrade` 或 `erlang.mk`。

## 5. 固定验收清单 / Required Acceptance

Required ID 精确集合为 `MC-AC-01` 至 `MC-AC-26`。每个 ID 的全部子场景通过才算 PASS；不允许用协调任务、mock、截图或 HTTP 200 顶替。

| ID | 场景与行为 oracle | 必要证据 |
|---|---|---|
| MC-AC-01 | 三仓基线、候选可追溯；受保护 WIP 内容未改变、未混入交付 | HEAD/tree/WIP 摘要与最终差异 |
| MC-AC-02 | 空库和旧 schema 升级成功；旧频道绑定 NULL、旧帖子精华 false，旧内容/订单/成员不丢 | 真实 PG schema、迁移日志、升级前后断言 |
| MC-AC-03 | 非存在群 FK 拒绝；同群第二频道冲突；多个 NULL 合法；超过 2^53 的 ID 跨三端不变 | 真实约束错误、API/DTO 往返、DB 行 |
| MC-AC-04 | 双端拥有者能绑定；普通成员、单端管理员、其他圈主不能绑定，失败零副作用 | API业务码、关系/审计前后查询 |
| MC-AC-05 | workspace、跨域、禁用/删除群、公开群、非空旧群、非兼容 E2EE 组合明确拒绝 | 参数矩阵及 DB 不变断言 |
| MC-AC-06 | 两频道并发绑定同群仅一胜；同值重试不重复写/审计；故障注入全部回滚 | 并发 barrier、事务日志、唯一关系 |
| MC-AC-07 | 空绑定可解绑；非空解绑/换群及直接删除受限；停用不变免费；转移不破坏双端权限 | 生命周期 API、DB、有效访问验证 |
| MC-AC-08 | 开启包入口正常；锁定包 UI/API/深链/缓存均 fail-closed；旧频道/群不被自动绑定 | 两类包配置、端侧和 API 证据 |
| MC-AC-09 | 现有文本/图片/附件发布、评论、反应、置顶、撤回正常，不另建内容数据源 | 频道现有接口及 DB回归；媒体授权 |
| MC-AC-10 | 精华与置顶四种组合独立；普通作者无权管理精华；撤回/删除后精华列表无泄露 | 接口、列表、DB两列断言 |
| MC-AC-11 | tags 边界、去重、删除标签、部分编辑成立；其他 payload 字段不丢；非法输入零写入 | 参数化用例与完整 payload 比较 |
| MC-AC-12 | 只在帖子正文出现的中文词可检索；频道名称匹配不充当正文命中；标签/精华组合有效 | 真实 jiebacfg/PG结果与查询计划 |
| MC-AC-13 | 检索/内容/精华分页无重复遗漏；篡改 message/channel/cursor ID、撤回和私有数据不泄露正文/片段/总数 | 多页 oracle、跨频道IDOR矩阵 |
| MC-AC-14 | 免费订阅者显式入群；私有频道邀请接受前拒绝、接受后允许；邀请/扫码/旧入群 API 均无绕过 | 所有入群入口清单、API/DB成员行 |
| MC-AC-15 | paid未到期可进；pending/refunded/cancelled/expired拒绝；仅脏订阅行仍拒绝；会员到期与付款超时不混淆 | 隔离 PG 合成订单及真实权限查询 |
| MC-AC-16 | 退订、移除订阅、退款、频道停用后，发送/历史/成员/公告/文件/相册等新访问拒绝；有效名单计数一致 | 全入口权限表、API/DB副作用查询 |
| MC-AC-17 | 已连接/已缓存/离线账户撤权后，广播、重试、通知和离线拉取不交付新内容；授权竞争按冻结线性化点成立 | 两个真实 WS、收件序列、竞态日志 |
| MC-AC-18 | DB时钟跨 subscription_end_at 后无需重登即失权；管理例外仅双端权限成立；加密策略不被降级 | 截止时间、边界前后读写、角色矩阵 |
| MC-AC-19 | DB不可用、权益查询异常、退款与发送/绑定并发时 fail-closed；无误发、重复成员或半绑定 | 故障注入、并发 oracle、最终 DB 状态 |
| MC-AC-20 | 未绑定的普通群、独立频道、现有 workspace和单聊行为不变；同一组 fixtures 跑基线/候选 | 回归日志与基线对照 |
| MC-AC-21 | Android真机完成浏览帖子→订阅→讨论→返回→精华/标签/检索；数据对应同候选后端 | device/platform、版本、操作日志、API/DB |
| MC-AC-22 | Android旧深链、缓存、弱网重连及正在聊天时撤权正确；拒绝后无新内容残留或虚假成功 | 真机错误旅程、WS/API、端侧状态 |
| MC-AC-23 | 真实 Admin 浏览器完成选群/绑定/冲突/精华管理；普通管理员越权拒绝；禁止拦截伪造业务响应 | 浏览器trace、API业务码、DB前后验证 |
| MC-AC-24 | 合成数据隔离库可升级、备份、旧候选回退演练；有数据时 down 不作为无损回滚；没有写正式库 | 本地恢复日志、行数/哈希与资源身份 |
| MC-AC-25 | 同一冻结候选全EUnit连续两次退出0；Admin test/lint/typecheck/deadcode/build、App analyze/test通过；无 Required skip/cancel | 原始日志、命令/exit、版本指纹 |
| MC-AC-26 | 独立 verifier 检出缺ID、多ID、重复ID、错SHA、非零exit、假PASS/缺证据、错误设备、WIP混入；正常包重验通过 | verifier负例自测与终审输出 |

## 6. 测试产物与命令 / Test Artifacts and Commands

下列新增测试/脚本路径是实施交付目标，目前不存在，不能声称已可运行；同义既有套件若可扩展，MC-00 固定映射并保留同一验收 ID。

- Backend：`test/logic/micro_community_binding_tests.erl`、`micro_community_access_tests.erl`、`micro_community_content_tests.erl`；`test/integration/micro_community_pg_integration_tests.erl`。集成套件需完整 OTP/TSID 与独立 PG 初始化，不用 meck 当最终证据。
- App：`test/micro_community/` 与 `integration_test/micro_community_flow_test.dart`，后者必须启动实际 App 并操作真实 UI，不只是从设备调用 API。
- Admin：扩展现有 ChannelDetail/ChannelMessage API与页面测试；`tests/e2e/micro-community.spec.ts` 使用实际登录、后端与 DB，不 mock 业务 API。
- 验收：`scripts/verify_micro_community_acceptance.py`，对应 `test/scripts/test_verify_micro_community_acceptance.py`；复用现有 verifier 辅助函数，不生成另一个通用验收框架。

### 6.1 执行准备

只在用户后续授权实施后执行。MC-00 先确定各仓独立工作树及合成数据资源；不默认占用主树服务和数据库，不 reset/clean/stash、不 blanket-stage。

```bash
set -euo pipefail
export WORKSPACE_ROOT=/Users/leeyi/project/imboy.pub
export RUN_ROOT="$(mktemp -d /tmp/imboy-micro-community.XXXXXX)"
for repo in imboy imboyapp imboyadmin; do
  git -C "$WORKSPACE_ROOT/$repo" rev-parse --show-toplevel
  git -C "$WORKSPACE_ROOT/$repo" rev-parse HEAD
  git -C "$WORKSPACE_ROOT/$repo" status --porcelain=v1
  git -C "$WORKSPACE_ROOT/$repo" worktree list --porcelain
done
```

MC-00 保存 `baseline.json` 和包含 staged/unstaged/untracked 字节哈希的 `protected-wip.sha256`，分配测试数据库、relay/HTTP/adm/Admin端口和设备 lease。执行目录写入下述变量，密码/Token不写计划、日志或仓库。

`RUN_ROOT` 只能保存合成账号数据与脱敏证据；无真实用户 PII。对外通知、账户/联系方式、真实支付、push、发布、部署或生产迁移均需另行人工确认。

### 6.2 专项及全量门

MC-00 生成资源环境 `run-env.sh`，包括 `BACKEND_WORKTREE`、`ADMIN_WORKTREE`、`APP_WORKTREE`、`EUNIT_RELAY_PORT`、`EUNIT_RELAY_TARGET`、`EUNIT_HTTP_PORT`、`EUNIT_HTTP_ADM_PORT`、浏览器及真机本地地址与 Android device ID。PG须独立且具备 pg_jieba；核对 Makefile 实际读取的 pg_conf，禁止仅改无效环境变量。

执行前 `flutter devices --machine` 记录真实设备，拒绝模拟器、Chrome、unknown或平台声明不符。手机使用明确的本地局域网后端地址，不把手机上的 127.0.0.1 当电脑后端。

```bash
set -euo pipefail
: "${RUN_ROOT:?MC-00 required}"
source "$RUN_ROOT/run-env.sh"
: "${BACKEND_WORKTREE:?}" "${ADMIN_WORKTREE:?}" "${APP_WORKTREE:?}"
: "${EUNIT_RELAY_PORT:?}" "${EUNIT_RELAY_TARGET:?}"
: "${EUNIT_HTTP_PORT:?}" "${EUNIT_HTTP_ADM_PORT:?}"
: "${ANDROID_DEVICE_ID:?}" "${IMBOY_TEST_API_BASE_URL:?}"
: "${IMBOY_ADMIN_E2E_BASE_URL:?}"

cd "$BACKEND_WORKTREE"
make compile
for suite in micro_community_binding_tests micro_community_access_tests micro_community_content_tests micro_community_pg_integration_tests; do
  IMBOYENV=local make eunit-local t="$suite" \
    EUNIT_RELAY_PORT="$EUNIT_RELAY_PORT" EUNIT_RELAY_TARGET="$EUNIT_RELAY_TARGET" \
    EUNIT_HTTP_PORT="$EUNIT_HTTP_PORT" EUNIT_HTTP_ADM_PORT="$EUNIT_HTTP_ADM_PORT"
done
python3 -m unittest discover -s test/scripts -p 'test_verify_micro_community_acceptance.py'

cd "$ADMIN_WORKTREE"
bun run test
bun run lint
bun run typecheck
bun run deadcode
bun run build
IMBOY_ADMIN_E2E_BASE_URL="$IMBOY_ADMIN_E2E_BASE_URL" \
  PLAYWRIGHT_DISABLE_WEBSERVER=1 \
  bun run test:e2e tests/e2e/micro-community.spec.ts --workers=1 --retries=0

cd "$APP_WORKTREE"
flutter analyze
flutter test
flutter test integration_test/micro_community_flow_test.dart \
  -d "$ANDROID_DEVICE_ID" \
  --dart-define="IMBOY_TEST_API_BASE_URL=$IMBOY_TEST_API_BASE_URL"
```

MC-00须确认 integration fixture 实际读取该 dart-define，不能传一个无人使用的变量。Admin真实后端地址、登录与数据库 oracle 同样必须由 fixture 明确消费并校验；不使用生产 .env.e2e。

冻结三仓候选后串行执行两次完整后端门，不使用 t 过滤、不并发修改同一数据库：

```bash
set -euo pipefail
source "$RUN_ROOT/run-env.sh"
cd "$BACKEND_WORKTREE"
for pass in 1 2; do
  IMBOYENV=local make eunit-local \
    EUNIT_RELAY_PORT="$EUNIT_RELAY_PORT" EUNIT_RELAY_TARGET="$EUNIT_RELAY_TARGET" \
    EUNIT_HTTP_PORT="$EUNIT_HTTP_PORT" EUNIT_HTTP_ADM_PORT="$EUNIT_HTTP_ADM_PORT" \
    2>&1 | tee "$RUN_ROOT/backend-full-$pass.log"
done
python3 scripts/verify_micro_community_acceptance.py --run-root "$RUN_ROOT"
```

命令日志须由 `commands.sh/commands.jsonl` 捕获开始/结束、实际 argv、目录、资源身份、exit及日志SHA256；tee或最后一个命令成功不能掩盖前面失败。存量失败在相同隔离基线上复现并记录，不能改签 PASS、擅自扩大修复范围或跳过全量门。

浏览器工具依工作区要求先尝试 `autoglm-browser-agent`；不可用时记录原因后才使用 Playwright等fallback。自动化 spec须按相同真实页面动作与API/DB oracle验收；不向子Agent委托浏览器操作。

## 7. 证据包与终态 / Evidence and Verdict

MC-07 的最小证据包位于 `RUN_ROOT`：

- `baseline.json`、`protected-wip.sha256`、`leases.json`、`requirements.json`、`entrypoints.json`。
- `candidate-manifest.json`：计划SHA256、三仓基准/候选HEAD与tree、工作树补丁/未跟踪文件哈希、feature profile、schema版本、App构建版本、设备平台与资源ID。未提交树须有可独立重建的补丁，单有HEAD不算冻结。
- `commands.sh`、`commands.jsonl`、`acceptance.tsv`、`recovery.jsonl`、原始测试/API/DB/WS/浏览器/真机日志与 `artifacts.sha256`。
- `final/verdict.json`、`final/report.md`；报告只使用脱敏合成账号标识。

`acceptance.tsv` 至少包含：`id/status/candidate_manifest_sha256/command_id/oracle/evidence_path/evidence_sha256/reviewer/blocker`。API/DB负例必须证明未产生消息、成员、绑定或扣款副作用。证据数量和截图不能代替行为 oracle。

Verifier 必须独立重算 26 IDs 的精确集合与唯一性、候选/计划/证据哈希、日志实际结果和 Required skip/cancel、设备类别与范围，检查文件路径不逃出证据根。文件存在或 ledger 里写 PASS 都不足以通过；业务 oracle由真实测试断言证明，MC-08复核关键权限与竞态原始证据，不声称哈希能证明任意文本真假。

终态：

| 状态 | 条件 |
|---|---|
| `PLAN_READY / NOT_EXECUTED` | 本轮只完成可执行计划及结构检查 |
| `WORKER_PASS` | 任务自测通过；不是三仓集成通过 |
| `PARTIAL` | 任一 Required 未完成/失败/无证据/被跳过/取消 |
| `BLOCKED_NO_HANDOFF` | 必需路径有外来 WIP且无明确交接 |
| `BLOCKED_RESOURCE_AUTH` | 没有隔离资源或真机使用权；不得自行接管设备/账户 |
| `BLOCKED_EXTERNAL` | 所需真实支付/外部依赖无授权或不可验证 |
| `LOCAL_CANDIDATE_PASS` | 26 IDs全PASS、全门成功、WIP未混入且独立终审通过 |

所有本地终态均保持：`PRODUCTION_PAYMENT=NOT_EXECUTED`、`PRODUCTION_QUALIFIED=NO_GO`、`PRODUCTION_DEPLOYED=NOT_EXECUTED`；真实回款/退款/对账另立授权与验收，不用合成 paid 订单冒充支付资格。iOS/macOS资格另立平台矩阵，不由Android结果推导。

## 8. 恢复、停止与实施授权 / Recovery and Authority

- 迁移只在合成数据隔离库演练，先备份。回退优先停写并恢复备份/兼容候选；有实际绑定或精华数据时，删除列的 down 会丢元数据，不能称无损回滚。
- 任一权限泄露、误发、权益异常、并行覆盖、跨域绑定或资源身份不符立即停止相关任务，保留原始证据，不自动修改生产或别人的主树。
- 解除/替换非空绑定、迁移现有真实圈子、启用真实付费、开放workspace、多群/加密组合扩围，均先修订契约和验收集合，不在实现中自行拍板。
- 后续实施获得明确授权后，验证通过的代码按独立功能本地提交，身份 command-scoped `leeyi <leeyisoft@qq.com>`；不得混入无关WIP。不改全局Git配置，不 push/部署。未获实施授权前不执行任务卡。
- 建议本地提交按依赖闭合：数据契约→绑定→持续权限→内容→端侧→集成验收；中间提交不能引用后续才新增的字段/模块。

## 9. 参考来源 / References

- 本地权威：本计划第2节列出的 schema、`src/logic/channel_logic_common.erl`、`src/repo/channel_order_repo.erl`、`docs/product/packaging-contract.md`、当前 Makefile 与三端规范。
- PostgreSQL 18约束文档：`https://www.postgresql.org/docs/18/ddl-constraints.html`；FK、UNIQUE与跨表CHECK的边界须遵循实际数据库行为。
- Telegram讨论组说明：`https://core.telegram.org/api/discussion`；只作为交互参照，不作为IMBoy schema或支付门槛的证据。

本计划刻意跳过：新增community表族、多对多绑定、新支付网关、逐帖转发线程、企业范围扩展、全面导航重写。未检查：真实支付、生产容量、安全外审与iOS/macOS平台资格。
