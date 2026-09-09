# E2EE-2026-012 群历史语义决策包

日期：2026-09-09（当前 Base 重验并回填；决策包语义与 2026-09-09 架构稿设计一致，代码锚点已在当前冻结 Base 逐项重验。REV-1：仅修正本行错别字「架构棒」→「架构稿」，选项语义与全部其余内容零改动）

状态：`DECISION_PACKAGE_READY / WAITING_USER_DECISION`

发布姿态：`NO-GO`

本文只提交产品与安全决策，不批准实现。F/R/D/M 任一选择都必须由用户书面确认；确认前不得修改 schema、迁移、产品代码或测试，也不得把 012 标为 `FIXED`。

## 0. 当前 Base 重验声明（2026-09-09，LT-02-C）

冻结基线（决策包绑定的代码事实全部在该 Base 重验成立）：

```text
imboy      63747f8d7a0f9bc27bce4c540549a032141fbc3a
imboyapp   0152560aa741b69411e484cc84c1c220f565b2af
imboyadmin 8c2b8615c292d82257886ad51445db87c366d719
```

重验结果（锚点见 §1）：`/msg/history` 与 batch sync 仍为 boolean membership + 整群归档读取；`group_member` 仍无 join boundary/generation 列；消息仍先进 staging（无 `conv_seq`），worker 异步 archive 阶段才经 `next_conv_seq/1` 分配序列。相关 C 级回归（后端 9 模块 124 PASS/0 FAIL，含 messaging_logic 12、msg_c2s_logic 35、group_member_repo 21）只证明现状行为，不改变缺口判定。跨 SHA 后本决策包的代码事实需重新核对。

## 1. 已确认事实（当前 Base 重验）

1. `/msg/history`（`src/logic/messaging_logic.erl` `history/5` → `validate_history_params` → `group_ds:is_member/2`）与 batch sync（`src/logic/msg_c2s_logic.erl` `handle_sync` → `group_ds:is_member/2`）当前只验证 active membership，然后读取整个 `c2g:<gid>` archive。授权结果是 boolean，不能给查询提供不可绕过的 lower bound。
2. archive 以 `conv_seq` 游标分页。客户端可传 `seq=0`；当前 active member 因而可请求群创建以来的全部归档。
3. `group_member` 列为 id/group_id/user_id/role/is_join/join_mode/status/created_at/updated_at（`priv/migrations/00000001_foundation.up.sql:4424`），没有不可变的 join boundary 或 membership generation。主动退出删除行，workspace removal 置 inactive，重入路径语义不一致；mutable `updated_at` 和旧行 `created_at` 都不能可靠表达本次入群。
4. 消息先进入 staging（`src/ds/msg_store_ds.erl`，无 `conv_seq`），`msg_store_worker`（`src/ds/msg_store_worker.erl:200`）之后才异步调用 `msg_archive_repo:archive/1`（内部 `next_conv_seq/1`，`src/repo/msg_archive_repo.erl:37/73/111`）分配序列。membership transition 与序列分配不在同一事务，且 sequence 表示 archive 执行顺序而不是消息持久接受顺序；入群前 backlog 可在入群后才取得 seq。
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

推荐但未批准：**F2**，作为安全默认最小且边界清楚。

## 4. 退出或移除后重入 R（三选一）

| 选项 | 用户语义 | 数据与实现影响 | 安全与体验权衡 |
|---|---|---|---|
| **R1 复用最初 boundary** | 重入后恢复旧历史，并看到离群期间 archive | 单 current row 最简单 | 削弱移除语义，离群期间内容被重新授权，不推荐 |
| **R2 每次重入新 generation** | 服务端只重发本次重入后的历史；设备本地旧数据不删除 | leave/remove 关闭当前 generation，rejoin append 新 generation | 最少重新披露，语义简单；旧设备已有数据仍无法追回 |
| **R3 仅曾在群内区间** | 恢复过去参与期，排除离群区间 | append-only `[start_seq,end_seq]` epochs；授权返回 interval 集合，history/sync 跨 gap 分页；附件按 anchor seq 校验；每个 epoch 边缘 rotation，历史 key 只授予完整落在获权区间的 session | 体验更好但复杂，区间合并、cursor、`has_more` 或跨 epoch key grant 错误容易越权 |

推荐但未批准：**R2**。只有明确业务需求要求恢复旧参与期时再评估 R3。

## 5. 同账号新设备 D（三选一）

| 选项 | 用户语义 | 数据与实现影响 | 安全与体验权衡 |
|---|---|---|---|
| **D1 自动继承账号历史与 keys** | 新旧设备尽量看到相同历史 | account ACL + 自动历史 key 转移/恢复 | 换机最顺，设备接管时历史暴露最大 |
| **D2 新设备仅看 activation 后** | 新设备默认只看未来 | 需要 device-level grant/boundary，轮换后只发新 key | 最少披露，换机历史体验最差 |
| **D3 继承账号 archive boundary，历史 key 显式恢复** | 可下载账号获权密文；默认只能解未来，用户确认后恢复备份内部分群历史 | archive ACL 仍按账号；key grant 记录来源、范围和 Safety Number 变化 | 平衡体验与披露；不能恢复未备份的 Megolm，也不恢复 C2C Olm ratchet/TOFU |

推荐但未批准：**D3**；高敏部署可选 D2。用户还必须明确「可下载旧密文」和「可获得旧 key」是否分开授权。

## 6. 旧数据迁移 M（三选一）

旧 membership 没有可信 join sequence，禁止用 `updated_at` 或推测的 `created_at` 猜安全边界。

| 选项 | 回填规则 | 优点 | 代价与风险 |
|---|---|---|---|
| **M1 兼容回填** | 现有 active 成员 `start_seq=1`，新加入/重入开始严格执行 | 迁移与体验影响最小 | 老成员永久保留 grandfathered 全量历史例外 |
| **M2 安全 cutover** | 现有 active 成员从部署 cutover seq+1 开始 | 最少披露、无需猜历史 | 现有设备失去服务端旧历史回填，破坏最大 |
| **M3 群级人工回填** | 可靠外部审计可精确回填，其余群显式采用 M1 或 M2 | 可在有证据的群恢复精度 | 需要授权数据源、逐群审计和异常处理 |

推荐但未批准：普通升级通常选 **M1 并明确 grandfathered 风险**；高敏新部署或空数据环境可选 M2。Architect 不代替用户选择。

## 7. 原子 cutover 契约（staging backlog）

当前存在两层竞态：membership transition 与 `next_conv_seq/1` 使用不同事务；更重要的是，消息进入 staging 后由 worker 异步 archive，直到 archive 才分配 seq。即使 transition 与当前 `next_conv_seq/1` 增加相同锁，入群前已接受但排队中的消息仍可在入群后取得 seq，被错误归为入群后历史。

目标边界定义：**消息在服务端持久接受到 staging 的顺序**，而不是异步 archive 完成时间或客户端 timestamp。消息接受事务在写 staging 时预分配并保存 `conv_seq`；worker 归档只能搬运既定 seq，不能再次分配。membership transition 与 staging 接受对同一 `conv_key` 使用同一数据库锁顺序：join/rejoin 记录锁内当前序列的下一值为 `start_seq`，leave/remove 记录锁内当前值为 `end_seq`。事务回滚造成的 seq gap 允许存在，history/sync 不能假设连续。

实现前必须满足：

1. membership generation 的 open/close 与 `c2g:<gid>` cutover seq 在同一事务、同一锁顺序内确定。
2. sequence API 必须接受 staging 事务连接；staging 保存预分配 `conv_seq`，archive 使用该值。现有 archive 阶段 `next_conv_seq/1` 契约不足。
3. 群消息持久接受和 membership transition 对同一 `conv_key` 使用一致锁顺序，避免 deadlock 与边界漂移；异步 archive 不再参与边界排序。
4. leave/admin remove/workspace remove 都关闭当前 generation；rejoin 必须新建 generation。重复事件需幂等，未知状态 fail-closed。
5. boundary 以 `conv_seq` 表达，不使用 wall-clock timestamp。迁移需可逆、可审计并记录选择的 M 策略。
6. 启用新语义前，legacy staging backlog 必须在隔离维护窗排空，或原子补齐能证明原接受顺序的 seq；无法证明时 fail-closed，不得把 archive 时间冒充接受时间。

最小实现方向是在现有 repository/transaction 模式内把 sequence 分配前移到 staging 写事务，并复用返回 interval 集合的 shared history authorization；不新建服务或引入依赖，除非压力测试证明现有 PG 锁模型不够。

## 8. 决策后的验收清单

### 本地隔离测试棒

- 后端 EUnit 必须指向任务专属 scratch PG，并逐模块串行：`EUNIT_CONFIG=<scratch-config> make eunit-local t=messaging_logic_tests`，同法运行 `msg_c2s_logic_tests` 与 `group_member_repo_tests`。
- 并发 join/message、leave/message、rejoin/message：断言以 staging 接受顺序分配边界；构造 pre-transition staging backlog 延迟 archive，证明不会被错分；记录 scratch 清理为 0。
- history 与 batch sync：`seq=0`、负数、溢出、伪造 `conv_key`、边界前后、空页、多页、重复 cursor 均同源过滤；R3 覆盖多个 epoch、跨 gap cursor 与 `has_more`。
- 生命周期：首次加入、主动退出、管理员移除、workspace removal、重入；附件 ACL 和 room-key grant 与批准的 F/R/D 一致。
- 迁移：M 策略的 up/down、幂等、并发与失败回滚；不得读取共享或生产数据。

证据记录：Run ID、三仓 SHA、diff hash、scratch 资源名、禁网状态、迁移版本、命令/退出码、断言数、期望/实际、证据等级、清理结果。上述最多形成 B/C 级证据。

### 外部 BLOCKED 门

真实账号、真机、建群/退群/踢人、旧 session、room-key 导出、抓包/MITM/replay、真实 DB/log/backup/object store/Push、Keychain/Keystore、生产和第三方操作均需另行授权。只有 A 级生命周期与攻击复测完成后，012 才可进入 `ATTACK_RETEST_PASS`。

## 9. 待用户书面选择

```text
F = F1 | F2 | F3   # 首次入群历史
R = R1 | R2 | R3   # 退出/移除后重入
D = D1 | D2 | D3   # 同账号新设备
M = M1 | M2 | M3   # 既有数据迁移

可选补充：
- D 场景是否分别控制 archive ciphertext download 与 historical room-key grant
- F3/M3 是否需要，以及谁有权配置或提供审计数据
```

推荐但未批准：`F2 / R2 / D3 / M1`。

在四项选择完成前：`E2EE-2026-012=ROOT_CAUSE_CONFIRMED/BLOCKED_DECISION`，`RELEASE_POSTURE=NO-GO`。
