# E2EE-2026-012 群历史语义决策包

原决策包日期：2026-09-09；当前源码覆盖核验：2026-09-11

状态：`CURRENT_IMPLEMENTATION_PARTIAL / DECISION_RECORDED / D3_DETAIL_RECORDED / A_LEVEL_ATTACK_RETEST=BLOCKED`

发布姿态：`NO-GO`

本文是 F/R/D/M 产品与安全决策的唯一工件。用户于 2026-09-11 书面确认 `F=F2，R=R2，D=D3，M=M1，AI-ID=B`，其中本文件记录 `F2/R2/D3/M1`；又于 2026-09-12 书面确认 D3 采用分开授权：archive ciphertext download 继承账号获权 generation 范围，historical room-key grant 默认不随新设备自动授予，只能经显式恢复并绑定群、generation、epoch、`conv_seq` 范围和审计记录。当前源码已有对应方向的部分实现；生产 C2G staging 的原子 `conv_seq`/权限/recipient snapshot、群聊附件 generation ACL 和 offline timeline generation 过滤均已本地修复。D3 细节已记录，但 room-key grant 与 backup epoch metadata 尚未实现，不得据此标记 `ATTACK_RETEST_PASS/CLOSED`。

## 0A. 2026-09-11 CURRENT-HEAD OVERRIDE

本节覆盖下述 2026-09-09 Base 的代码现状描述；选项定义、原子 cutover 契约和外部门保持有效。

```text
imboy      a7d76cc23b17600b246921e36e634bc091cad1f2 + scoped patch
imboyapp   8ebe49e355ee66b79386f40739432d0bf34c13df + scoped patch
verdict    LOCAL_SECURITY_GATE_FAIL
governance DECISION_RECORDED / D3_DETAIL_RECORDED
A-level    BLOCKED
release    NO-GO
```

当前实现与证据：

1. 迁移 101 新增 `group_member_generation` 和 staging `conv_seq`；history 与 batch sync 复用 generation 下界。首次加入/重入建立新世代，leave/remove/workspace remove 关闭世代；迁移对 legacy active member 使用 M1 `start_seq=1`。
2. 2026-09-11 补齐建群创建者和解散群两个旁路：创建者走统一 `join_group/5` 建立首个 open generation；解散在同一序列锁下关闭该群全部 open generation；授权还要求 member 与 group 都是 active。
3. scratch PostgreSQL 已用实际 `erlang_migrate` 完成 `107 -> 100 -> 107`。legacy C2G backlog 分群按 staging id 稳定回填，C2C 保持 `NULL`；M1 仅回填 active member；唯一 open generation 与 interval 约束均实测拒绝违规写入；重复 `up(all)` 前后状态哈希一致。
4. 生产 `msg_c2g_logic` 的三个 staging 分支现统一传整数 GID 与最低发送者角色进入 `stage/12`。Repo 在 `msg_store_seq` 顺序锁事务内重新验证 active group/sender/role，读取最多 `5000+1` 个 active recipients，并在同一次提交中写入 `to_id`、`to_id_list` 和 `conv_seq`；旧 C2G UID 列表入口明确返回 `c2g_group_id_required`。
5. worker 的 `claim_pending/2` 与恢复查询现显式选择 `conv_seq`，archive 搬运 staging 固化值而不再二次分配。`staging_prealloc_test_` 已改为经生产 claim 查询取行，避免 `SELECT *` 掩盖漏列。2026-09-12 marker scratch-PG harness 已补跑生产 `stage → enqueue → msg_store_worker → msg_c2g/msg_c2g_timeline → msg_store archive` 管道，2/2 PASS，并断言 formal message 与 archive 保持 E2EE envelope、timeline 与 archive 使用同一 staging 固化 `conv_seq`。首次真库重放发现 duplicate identity 查询保留未使用参数导致 PostgreSQL `42P18 indeterminate_datatype`；`verify_c2g_duplicate_identity/2` 改为连续 6 参数后，`msg_store_repo_tests` 48/48、`msg_store_worker_tests` 35/35 和完整 harness 均 PASS，scratch 数据库残留为 0。独立增量复审为 `APPROVE`（0 CRITICAL / 0 HIGH / 0 MEDIUM / 0 LOW）。这是当前本地 B/C 级证据，不等同生产负载或 A 级证据。
6. `msg_c2g_request_ledger` 负责 durable MsgId 与稳定请求身份，`msg_c2g_recipient_snapshot` 负责 immutable recipient set 与 action 授权，两者不再混用。稳定 hash 覆盖 msg_type/E2EE/业务 payload/sender device，剔除顶层和嵌套 payload 的服务端时间字段；action 单独比较。在线投递、Push、mention、Agent 与 Bot 继续复用 committed recipient snapshot。当前 7 个定向 EUnit suite **171/171 PASS**；此前不同代码形状下的旧汇总计数不再作为当前补丁证据。
7. room-key 公钥枚举已单独修复为 active group/caller/recipient/device 的单 statement snapshot，成员和设备条目都以 `4096 + 1` 探针拒绝截断集合；定向 60/60 PASS，PG 18.4 只读 `EXPLAIN` 确认未授权分支不执行成员/设备扫描，授权分支不再全站设备外侧扫描或 limit 前全量排序。2026-09-12 已让现有 marker scratch-PG harness 直接执行生产 SQL，覆盖未授权、预置 inactive recipient、active recipient 动态撤销、inactive caller/group/device、无有效 key，以及 4096/4097 active member/device sentinel；真库矩阵 PASS、残留数据库为 0，最新独立复审为 `APPROVE`（0 CRITICAL / 0 HIGH / 0 MEDIUM / 0 LOW），关闭此前唯一 MEDIUM 及随后指出的 recipient 动态撤销用例缺口。查询完成后到客户端包裹、再到 C2G 中继之间仍没有 membership generation 绑定，因此不能据此关闭历史 room-key grant 或 C2G 撤销竞态。
8. migration 108 已区分两类群媒体：聊天附件用客户端最终 `messageId` 声明 `anchor_msg_id`，C2G staging 在同一顺序锁事务内绑定权威 `anchor_conv_seq`，下载按 active group/member/current open generation 的 `start_seq` 授权；独立 `group_file` 用 `group_file_id` 明确关联并保持当前成员共享语义。legacy 聊天附件只给 M1 `start_seq=1` 世代兼容，未知或未绑定记录 fail-closed。App 八个生产上传入口均传同一最终 ID，视频本体与缩略图共锚。108 up/down/backfill/ACL 已在 scratch PostgreSQL 通过；旧客户端发布、最低版本/强制升级和真实阻断仍为 `BLOCKED_EXTERNAL`。退出/移除只能阻止再次签发，已签发 GET URL 最长 600 秒内仍有效。
9. migration 109 已给 `msg_c2g_timeline` 增加权威 `conv_seq`；worker 拒绝无 seq 的 C2G staging，`/msg/offline` 的 list/count 共用 current open generation 下界，NULL legacy 行 fail-closed。scratch PostgreSQL 已覆盖全量 1→109、legacy 回填、重复执行、稳定 hash、身份冲突、malformed/GID mismatch 和 up/down/up。首次上线使用停止旧节点后的 boot migration 与 cutover marker；109 dirty 在停旧节点前阻断并要求人工恢复。ledger 370 天后且正式消息/staging 均消失才以每轮最多 10×1000 条清理。App 备份仍无 generation/seq 范围 metadata；生产规模 DDL/cutover、room-key epoch token、historical grant 和 A 级 leave/rejoin 仍未完成。

当前实现与决策选项的对应关系：

| 选项 | 当前源码状态 | 决策状态 |
|---|---|---|
| F2 | 首次加入以新 generation 下界限制 archive | 用户选择已记录；generation/history 与生产 staging 原子 cutover 已实现，定向回归和 scratch 真库矩阵通过；生产规模 cutover 尚未完成 |
| R2 | 重入新建 generation，离开时关闭旧 generation | 用户选择已记录；generation 生命周期与 staging 顺序锁边界已实现并有定向回归；真实锁竞争/A 级生命周期未测 |
| D3 | archive ACL 按账号继承 generation；历史 key 应显式恢复 | 分开授权已记录；archive ACL 部分存在，恢复 UI 有显式口令操作，但备份 session 无 generation/epoch/seq 范围，历史 room-key grant 与审计未闭环 |
| M1 | legacy active member 回填 `start_seq=1`，inactive 不回填 | 用户选择已记录；迁移和 scratch 真库证据存在；grandfathered 全量历史例外是既定迁移代价，不代表发布验收完成 |

仍不能关闭 012：migration 108/109 的 scratch 真库矩阵虽已通过，但生产规模 DDL/cutover 尚未完成；旧客户端群附件 rollout 尚未执行；room-key epoch token、historical grant 和 backup metadata 尚未证明只覆盖获权 epoch；真实成员/设备生命周期和攻击复测均未执行。当前 012 必须写 `LOCAL_SECURITY_GATE_FAIL`，不允许写 `ATTACK_RETEST_PASS` 或发布 `GO`。

## 0B. 2026-09-09 历史 Base 重验声明（LT-02-C）

冻结基线（决策包绑定的代码事实全部在该 Base 重验成立）：

```text
imboy      63747f8d7a0f9bc27bce4c540549a032141fbc3a
imboyapp   0152560aa741b69411e484cc84c1c220f565b2af
imboyadmin 8c2b8615c292d82257886ad51445db87c366d719
```

历史重验结果（锚点见 §1）：`/msg/history` 与 batch sync 当时仍为 boolean membership + 整群归档读取；`group_member` 当时仍无 join boundary/generation 列；消息仍先进 staging（无 `conv_seq`），worker 异步 archive 阶段才经 `next_conv_seq/1` 分配序列。相关 C 级回归（后端 9 模块 124 PASS/0 FAIL，含 messaging_logic 12、msg_c2s_logic 35、group_member_repo 21）只证明当时行为；这些代码事实已被 §0A 的当前实现覆盖。

## 1. 已确认事实（2026-09-09 历史 Base；由 §0A 覆盖代码现状）

1. `/msg/history`（`src/logic/messaging_logic.erl` `history/5` → `validate_history_params` → `group_ds:is_member/2`）与 batch sync（`src/logic/msg_c2s_logic.erl` `handle_sync` → `group_ds:is_member/2`）当时只验证 active membership，然后读取整个 `c2g:<gid>` archive。授权结果是 boolean，不能给查询提供不可绕过的 lower bound。
2. archive 以 `conv_seq` 游标分页。客户端可传 `seq=0`；当时的 active member 因而可请求群创建以来的全部归档。
3. 当时的 `group_member` 列为 id/group_id/user_id/role/is_join/join_mode/status/created_at/updated_at（`priv/migrations/00000001_foundation.up.sql:4424`），没有不可变的 join boundary 或 membership generation。主动退出删除行，workspace removal 置 inactive，重入路径语义不一致；mutable `updated_at` 和旧行 `created_at` 都不能可靠表达本次入群。
4. 消息当时先进入 staging（`src/ds/msg_store_ds.erl`，无 `conv_seq`），`msg_store_worker`（`src/ds/msg_store_worker.erl:200`）之后才异步调用 `msg_archive_repo:archive/1`（内部 `next_conv_seq/1`，`src/repo/msg_archive_repo.erl:37/73/111`）分配序列。membership transition 与序列分配不在同一事务，且 sequence 表示 archive 执行顺序而不是消息持久接受顺序；入群前 backlog 可在入群后才取得 seq。
5. archive ACL 与解密能力是两道门。服务端返回旧密文不表示新设备有旧 Megolm inbound key；分发旧 room key 会主动扩大历史可见面。
6. 已经交付到设备的明文、密文、room key、截图或导出不能远程追回。移除成员最多保护未来消息。

## 2. 必须冻结的共享数据流

```mermaid
flowchart LR
    A[Membership transition\njoin / leave / remove / rejoin] --> B[Immutable generation + boundary\nstart_seq / end_seq]
    B --> C[Shared history authorization\nidentity + eligible generations + authorized intervals]
    C --> D1[/msg/history\nserver clamps cursor]
    C --> D2[batch sync\nserver clamps every cursor]
    B --> E[Room-key policy\nrotation + historical key grant]
    B --> F[Attachment authorization\nmessage boundary + membership generation]
    D1 --> G[Authorized archive ciphertext]
    D2 --> G
    E --> H[Decryptable history on device]
    F --> I[Authorized attachment object]
```

所有读取面必须复用同一个授权结果，而不是分别重建规则：

```text
authorize_group_history(user_id, group_id, device_id?)
  -> {allow, authorized_intervals=[
       {generation_id, start_seq, end_seq | open}
     ]}
  -> deny
```

F1/F2/R2 通常只返回一个 interval；R3 必须返回多个不连续 epoch，禁止把它们压成一个最小 start/最大 end。`/msg/history` 与 batch sync 仅查询 `conv_seq` 落在任一授权 interval 且大于 client cursor 的行；`next_seq` 是最后一条已返回授权消息，`has_more` 只按其后仍存在的授权消息计算，跨越离群 gap 时不能回传或计数 gap 内消息。未知、重叠或不一致 interval 必须 fail-closed。

附件下载必须把对象绑定到 anchor message 的 `conv_seq`/generation，并检查其落在授权 interval；不能只检查当前 active membership。room-key grant 必须限定到获权 epoch，且每个 membership 边缘强制 rotation。若历史 Megolm session 横跨获权与未获权区间，该 key 不可选择性披露，必须拒绝 grant 或走用户批准的迁移规则。只修一个 history endpoint 不能关闭 012。

## 3. 首次入群 F（三选一）

| 选项 | 用户语义 | 数据与实现影响 | 安全与体验权衡 |
|---|---|---|---|
| **F1 全量历史** | 新成员可请求群创建以来全部 archive | boundary 可从 1 开始；如需可解，还要向新设备授予历史 room keys | 体验最完整，管理员误邀会暴露全部可用历史 |
| **F2 仅本次入群后** | 只返回 `conv_seq >= immutable_join_seq` | 每个 generation 固化 lower bound；history/sync/附件/key grant 同源执行 | 最少披露且可验证；新成员看不到加入前上下文 |
| **F3 群级可配置** | 群主选择全量、入群后或指定窗口 | policy 必须版本化；窗口在入群事务中解析成 seq 并固化，不能查询时按 timestamp 漂移 | 灵活但 UI、审计、迁移与误配置成本最高 |

2026-09-11 用户已选择 **F2**，作为首次入群语义。

## 4. 退出或移除后重入 R（三选一）

| 选项 | 用户语义 | 数据与实现影响 | 安全与体验权衡 |
|---|---|---|---|
| **R1 复用最初 boundary** | 重入后恢复旧历史，并看到离群期间 archive | 单 current row 最简单 | 削弱移除语义，离群期间内容被重新授权，不推荐 |
| **R2 每次重入新 generation** | 服务端只重发本次重入后的历史；设备本地旧数据不删除 | leave/remove 关闭当前 generation，rejoin append 新 generation | 最少重新披露，语义简单；旧设备已有数据仍无法追回 |
| **R3 仅曾在群内区间** | 恢复过去参与期，排除离群区间 | append-only `[start_seq,end_seq]` epochs；授权返回 interval 集合，history/sync 跨 gap 分页；附件按 anchor seq 校验；每个 epoch 边缘 rotation，历史 key 只授予完整落在获权区间的 session | 体验更好但复杂，区间合并、cursor、`has_more` 或跨 epoch key grant 错误容易越权 |

2026-09-11 用户已选择 **R2**。只有未来另有明确业务决策时才评估 R3。

## 5. 同账号新设备 D（三选一）

| 选项 | 用户语义 | 数据与实现影响 | 安全与体验权衡 |
|---|---|---|---|
| **D1 自动继承账号历史与 keys** | 新旧设备尽量看到相同历史 | account ACL + 自动历史 key 转移/恢复 | 换机最顺，设备接管时历史暴露最大 |
| **D2 新设备仅看 activation 后** | 新设备默认只看未来 | 需要 device-level grant/boundary，轮换后只发新 key | 最少披露，换机历史体验最差 |
| **D3 继承账号 archive boundary，历史 key 显式恢复** | 可下载账号获权密文；默认只能解未来，用户确认后恢复备份内部分群历史 | archive ACL 仍按账号；key grant 记录来源、范围和 Safety Number 变化 | 平衡体验与披露；不能恢复未备份的 Megolm，也不恢复 C2C Olm ratchet/TOFU |

2026-09-11 用户已选择 **D3**；2026-09-12 又确认「可下载旧密文」和「可获得旧 key」分开授权。账号可按获权 generation 下载密文；新设备默认只取得未来 key，历史 key 必须经显式恢复，且 grant 必须限定群、generation、epoch、`conv_seq` 范围并记录审计。

## 6. 旧数据迁移 M（三选一）

旧 membership 没有可信 join sequence，禁止用 `updated_at` 或推测的 `created_at` 猜安全边界。

| 选项 | 回填规则 | 优点 | 代价与风险 |
|---|---|---|---|
| **M1 兼容回填** | 现有 active 成员 `start_seq=1`，新加入/重入开始严格执行 | 迁移与体验影响最小 | 老成员永久保留 grandfathered 全量历史例外 |
| **M2 安全 cutover** | 现有 active 成员从部署 cutover seq+1 开始 | 最少披露、无需猜历史 | 现有设备失去服务端旧历史回填，破坏最大 |
| **M3 群级人工回填** | 可靠外部审计可精确回填，其余群显式采用 M1 或 M2 | 可在有证据的群恢复精度 | 需要授权数据源、逐群审计和异常处理 |

2026-09-11 用户已选择 **M1**，并保留现有 active 老成员的 grandfathered 全量历史例外；inactive 老成员不回填。

## 7. 原子 cutover 契约（staging backlog）

历史缺口有两层竞态：membership transition 与 `next_conv_seq/1` 使用不同事务；消息进入 staging 后直到异步 archive 才分配 seq。2026-09-11 当前补丁已把 seq 分配、发送者/角色重验和 recipient snapshot 收进 staging 事务，并让 worker 搬运该 seq；本段保留为设计根因，不再描述当前实现。

目标边界定义：**消息在服务端持久接受到 staging 的顺序**，而不是异步 archive 完成时间或客户端 timestamp。消息接受事务在写 staging 时预分配并保存 `conv_seq`；worker 归档只能搬运既定 seq，不能再次分配。membership transition 与 staging 接受对同一 `conv_key` 使用同一数据库锁顺序：join/rejoin 记录锁内当前序列的下一值为 `start_seq`，leave/remove 记录锁内当前值为 `end_seq`。事务回滚造成的 seq gap 允许存在，history/sync 不能假设连续。

实现前必须满足：

1. membership generation 的 open/close 与 `c2g:<gid>` cutover seq 在同一事务、同一锁顺序内确定。
2. sequence API 必须接受 staging 事务连接；staging 保存预分配 `conv_seq`，archive 使用该值。现有 archive 阶段 `next_conv_seq/1` 契约不足。
3. 群消息持久接受和 membership transition 对同一 `conv_key` 使用一致锁顺序，避免 deadlock 与边界漂移；异步 archive 不再参与边界排序。
4. staging 在取得该顺序锁后必须用同一事务连接重新验证 active sender，并查询 active recipient snapshot；事务外的 `is_member/2` 和缓存成员列表只能作早期快速拒绝，不能作为最终授权或最终收件人集合。
5. staging 成功必须把事务内 recipient snapshot 返回给发送逻辑；在线投递、离线时间线、Push、mention/Bot/Agent 旁路只能消费该快照，禁止继续使用事务外旧列表。
6. leave/admin remove/workspace remove 都关闭当前 generation；rejoin 必须新建 generation。重复事件需幂等，未知状态 fail-closed。
7. boundary 以 `conv_seq` 表达，不使用 wall-clock timestamp。迁移需可逆、可审计并记录选择的 M 策略。
8. 启用新语义前，legacy staging backlog 必须在隔离维护窗排空，或原子补齐能证明原接受顺序的 seq；无法证明时 fail-closed，不得把 archive 时间冒充接受时间。

最小实现方向是在现有 repository/transaction 模式内把 sequence 分配前移到 staging 写事务，并复用返回 interval 集合的 shared history authorization；不新建服务或引入依赖，除非压力测试证明现有 PG 锁模型不够。

## 8. 决策后的验收清单

### 本地隔离测试棒

- 后端 EUnit 必须指向任务专属 scratch PG，并逐模块串行：`EUNIT_CONFIG=<scratch-config> make eunit-local t=messaging_logic_tests`，同法运行 `msg_c2s_logic_tests` 与 `group_member_repo_tests`。
- 并发 join/message、leave/message、rejoin/message：断言以 staging 接受顺序分配边界；构造 pre-transition staging backlog 延迟 archive，证明不会被错分；记录 scratch 清理为 0。
- 并发 leave/admin remove/workspace remove 与 send：若撤销事务先取得顺序，发送必须拒绝且 staging/在线投递/Push/旁路均为 0；若发送先取得顺序，持久化 `conv_seq` 与 recipient snapshot 必须一致，后续撤销关闭在该 seq 之后。测试必须经 `msg_c2g_logic:c2g/3` 真实调用形状进入 staging，不得直接传整数 GID 代替生产参数。
- history 与 batch sync：`seq=0`、负数、溢出、伪造 `conv_key`、边界前后、空页、多页、重复 cursor 均同源过滤；R3 覆盖多个 epoch、跨 gap cursor 与 `has_more`。
- 生命周期：首次加入、主动退出、管理员移除、workspace removal、重入；附件 ACL、offline list/count 和 room-key grant 与批准的 F/R/D 一致。必须覆盖旧世代未 ACK room-key → leave → rejoin → offline 不返回、新世代正常返回、NULL legacy 不返回、list/count 一致。
- 迁移：M 策略、migration 108/109 的 up/down、回填、幂等、并发与失败回滚；不得读取共享或生产数据。附件矩阵必须分别验证聊天附件 anchor generation、legacy M1、未绑定 fail-closed 与独立 `group_file` 当前成员共享。

证据记录：Run ID、三仓 SHA、diff hash、scratch 资源名、禁网状态、迁移版本、命令/退出码、断言数、期望/实际、证据等级、清理结果。上述最多形成 B/C 级证据。

### 外部 BLOCKED 门

真实账号、真机、建群/退群/踢人、旧 session、room-key 导出、抓包/MITM/replay、真实 DB/log/backup/object store/Push、Keychain/Keystore、生产和第三方操作均需另行授权。群附件 rollout 还必须严格按“发布携带 `anchor_msg_id` 的客户端 → 配置现有 `app_version` 最低版本/强制升级 → 验证旧客户端已无法进入群附件上传（若只提示则先补服务端版本门）→ 部署强制 anchor 后端”执行；任一步缺证据均为 `BLOCKED_EXTERNAL`，不得让无 anchor 的新聊天附件退回普通 active-membership ACL。只有 A 级生命周期与攻击复测完成后，012 才可进入 `ATTACK_RETEST_PASS`。

## 9. 已记录决策与实现待办

```text
用户原文：确认 F=F2，R=R2，D=D3，M=M1，AI-ID=B
用户原文：确认 D3 采用分开授权
本文件生效：F=F2 / R=R2 / D=D3 / M=M1
治理状态：DECISION_RECORDED / D3_DETAIL_RECORDED
D3 合同：账号按获权 generation 下载 archive ciphertext；historical room key 仅经显式、限范围、可审计的恢复授权
```

`F2 / R2 / D3 / M1` 与 D3 分开授权均已有用户书面选择；这关闭产品决策，不关闭实现或验收。当前 D3 仅有账号级 archive ACL，历史 room-key 显式恢复、限范围 grant、审计和 backup epoch metadata 尚未闭环。

在 D3 historical grant 实现、migration 108/109 生产规模 DDL/cutover 与 A 级复测、旧客户端 rollout、room-key epoch/backup metadata 等剩余验收完成前：`E2EE-2026-012=LOCAL_SECURITY_GATE_FAIL/DECISION_RECORDED/D3_DETAIL_RECORDED/A_LEVEL_ATTACK_RETEST=BLOCKED`，`RELEASE_POSTURE=NO-GO`。
