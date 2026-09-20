# IMBoy E2EE 攻击矩阵

原矩阵日期：2026-09-07
状态：`2026-09-11 CURRENT-HEAD OVERRIDE` 的 AI 明文身份门、C2G staging 权威快照、群聊附件 generation ACL、`/msg/offline` 旧世代过滤、D3 有限 historical room-key grant/backup metadata 与 server-authoritative Megolm session attestation 已形成本地实现候选；migration 1→112 和 108/109/111/112 当前 scratch PostgreSQL 真库矩阵已通过。用户已选择 `F2/R2/D3/M1/AI-ID=B`、明确接受 AI-ID=B 的恶意/被攻陷运行时服务端伪造 Agent 剩余风险，并于 2026-09-12 确认 D3 采用 archive ciphertext 与 historical room-key grant 分开授权。migration 112 在 C2G staging 顺序锁事务内固化 sender/device、recipient generation 集合与单调 `conv_seq` 范围，historical grant 只从该账本签发；原 session attestation `HIGH / OPEN` 已在本地候选关闭。当前状态仍为 `LOCAL_SECURITY_GATE_FAIL / DECISION_RECORDED / RISK_ACCEPTED / D3_DETAIL_RECORDED / A_LEVEL_ATTACK_RETEST=BLOCKED / E2EE_RELEASE=NO-GO`：生产规模 DDL/cutover、真实 App/账号/群、旧客户端/旧 session、抓包、篡改、密钥与外部服务证据仍为 BLOCKED。Findings 与发布结论见 [`E2EE_AUDIT_REPORT.md`](./E2EE_AUDIT_REPORT.md)，群历史决策（E2EE-2026-012）待 F/R/D/M 产品决策（原决策包为历史计划文档，已移除）。

历史 Base 冻结基线（2026-09-09 LT-02-C，已被下述 override 覆盖）：

```text
imboy      63747f8d7a0f9bc27bce4c540549a032141fbc3a
imboyapp   0152560aa741b69411e484cc84c1c220f565b2af
imboyadmin 8c2b8615c292d82257886ad51445db87c366d719
```

本文件不得保存账号口令、token、私钥、session secret、真实消息、PII、完整敏感 payload、真实设备 ID 或可复用 Canary。

## 0. 2026-09-11 CURRENT-HEAD OVERRIDE

```text
imboy      e9ff7ed48d5e12efac477b1948e26768af806590 + scoped patch (non-doc content sha256 e3486787cb97478fdf86d1155013e6e8c6114b835b47f06c729f7e0436c521cb)
imboyapp   bc33e4b2e297b9aa2fab15840d478ab209d99585 + scoped patch (non-doc content sha256 edeacce073f5cc5b87a00c6be116e9eabc3b7d561afdf0dc307e54080ad5c390)
evidence   scripts/test/run_group_attachment_acl_pg.sh + current command ledger
```

| 范围 | 当前本地证据 | 不得提升的边界 |
|---|---|---|
| E2EE-2026-012 | 迁移 101、legacy backlog/M1/约束/幂等、创建者首世代与解散关闭世代已有历史证据；生产 C2G 现以整数 GID + required role 进入顺序锁事务，分别固化 durable request ledger 与 immutable recipient snapshot，worker/投递/Push/mention/Agent/Bot 复用；migration 108 绑定聊天附件 anchor，migration 109 绑定 offline timeline/current generation，migration 111 补齐 C2G request ledger/recipient snapshot，migration 112 绑定 Megolm session/sender device/recipient generations/seq interval；D3 有限 grant、backup metadata、恢复确认/审计和 trusted-seq 门已本地接线 | `LOCAL_SECURITY_GATE_FAIL / DECISION_RECORDED / D3_DETAIL_RECORDED`：用户已选择 F2/R2/D3/M1，并确认 archive ciphertext 与 historical room-key grant 分开授权。当前后端核心 **194/194 PASS**；`msg_c2g_ds_tests` 4 PASS / 3 `BLOCKED_ENV`（缺 `pg_conf`，不计入 194）；Flutter合并相关回归 **72 PASS / 1 SKIP** 且 scoped analyze PASS。scratch PostgreSQL 完成 migration 1→112、群附件/历史边界、D3 session 生命周期、生产 stage 冲突矩阵和 C2G pipeline，同轮 PASS、marker residual=0；deploy sequence 51/51、migrate gate 6/6 PASS。migration 112 真库完整 schema predicate=1，错误长度及同名错误 PK/UNIQUE/FK/CHECK 均为 0；已有 marker 的常规发布在健康后 schema 漂移时拒绝切流。固定快照 follow-up 复审 **APPROVE（0 CRITICAL / 0 HIGH / 0 MEDIUM / 0 LOW）**。原 session attestation `HIGH / OPEN` 已本地关闭；生产规模 DDL/cutover、生产负载、旧客户端 rollout 和 A 级攻击复测仍未完成 |
| LT02-SEC-01 | 裸 agent badge 不再授权明文；消息/附件/重试经共享门；五元组显式绑定 owner UID，已有记录读取、确认或保存期间账号/badge/deployment/identity 变化均 fail-closed，保存后二次复核失败会撤销确认；五文件 **90/90 PASS**、scoped analyze 零问题 | 用户已选择 AI-ID=B，并明确接受恶意/被攻陷运行时服务端可伪造 Agent 的剩余风险；deployment-scoped 派生 identity 仍不是独立签名信任锚，真实传输/存储未做 A 级复测，finding 不得 CLOSED |
| 环境 | 当前唯一 loopback scratch PG 证据可复现 | `run_group_attachment_acl_pg.sh` 创建唯一 marker DB、全量迁移、运行 108/109/111/112 migration、ACL、D3 session 生命周期、生产 stage 冲突矩阵及异步 C2G worker 管道并 trap 删除；同库事务化 schema 反例验证错误 varchar 长度和同名错误 PK/UNIQUE/FK/CHECK 均被 predicate 拒绝，当前同轮 PASS、marker residual=0。该结果仅为 B 级本地真库，不替代生产规模或 A 级证据 |

本节覆盖下文所有把 2026-09-09 Base 称作“当前”的描述，但不把旧 B/C 证据升级成 A 级证据。矩阵中的 A 级行继续保持 `BLOCKED`，012 与 LT02-SEC-01 均不得标 `ATTACK_RETEST_PASS/CLOSED`。

## 1. 每轮授权单

| 项 | 必须由用户明确确认的值 |
|---|---|
| 后端/PG/对象存储 | `<AUTHORIZED_ISOLATED_BACKEND>` / `<AUTHORIZED_ISOLATED_DATABASE>` / `<AUTHORIZED_ISOLATED_OBJECT_STORE>` |
| Push | `<AUTHORIZED_TEST_PUSH_TARGET>` 或 `NOT_IN_SCOPE` |
| C2C 用户 | `<CONTROLLED_USER_A>`、`<CONTROLLED_USER_B>` |
| C2G 用户 | `<CONTROLLED_USER_A>`、`<CONTROLLED_USER_B>`、`<CONTROLLED_USER_C>`、`<CONTROLLED_USER_D>` |
| 真机 | `<AUTHORIZED_REAL_DEVICE_LIST>` |
| 群与成员操作 | `<AUTHORIZED_GROUP_AND_MEMBERSHIP_SCOPE>` |
| 数据查询 | `<AUTHORIZED_DB_LOG_BACKUP_OBJECT_PUSH_SCOPE>` |
| 抓包/篡改/replay | `<AUTHORIZED_CAPTURE_MITM_REPLAY_SCOPE>` |
| 密钥/App 数据实验 | `<AUTHORIZED_IDENTITY_SESSION_BACKUP_MUTATION_SCOPE>` |
| 时间与留存 | `<AUTHORIZED_TIME_WINDOW_AND_RETENTION>` |

不得复用历史账号、口令、token、设备、端口或地址；连接中的设备和监听进程不代表授权；不得操作生产、共享或第三方环境；模拟器不构成 Flutter 安全验收。

本轮已授权记录（脱敏）：物理 Android 9 真机；仅安装并运行测试包，只在应用随机临时目录创建、读取、错钥打开并删除本轮 SQLCipher 测试库；不登录账号、不连接后端、不读取或修改现有 IMBoy App 数据。设备序列号和 Canary 明文不入库。（该轮为阶段 A 历史授权，产生于旧 SHA；当前 Base 未重签。）

历史 Base 重验已授权记录（2026-09-09，LT-02-C）：三仓冻结 SHA 的隔离 detached worktree；C 级 EUnit/Flutter 单测、scoped analyze 与静态源码对账；任务专属 scratch PostgreSQL（marker DB 用后即删，residual=0）；Flutter 侧离线依赖、无真机、无模拟器、无账号、无真实网络出访。2 个 integration_test 设备文件尝试后记 ENV_BLOCKED_ATTEMPTED，未执行设备分支。

当前本地授权记录（2026-09-11）：仅上述两个任务专属 worktree、仓库外 scratch PostgreSQL、静态检查、迁移往返和本地单元/协议测试；无真机、账号、真实群操作、抓包、外部服务或生产数据。Docker Engine EOF 后未重启 Docker Desktop，避免影响其他会话容器。

获授权后，每次运行临时生成唯一 Canary，仅在证据中保留 SHA-256 和脱敏前缀：

```bash
CANARY="IMBOY-E2EE-$(date +%s)-$(openssl rand -hex 24)"
printf '%s' "$CANARY" | shasum -a 256
```

## 2. C2C

| ID | 攻击/流程 | PASS 条件 | 目标等级 | 当前状态 |
|---|---|---|---|---|
| C2C-01 | Canary 全表面搜索 | HTTP/WS、staging、消息/归档、队列、日志、备份、对象存储、Push 均无明文 | A | BLOCKED |
| C2C-02 | 服务端 root 恢复 | 无设备秘密时不能恢复历史或新消息 | A | BLOCKED |
| C2C-03 | PFv3/header/ciphertext/tag/nonce/metadata 篡改 | 全部认证失败，且不作为已交付 ACK | A | BLOCKED；005/007 已 REGRESSION_PASS，同 ID 改密文与同密文换 ID 有 C 级拒绝回归，真实 Transport 篡改待授权 |
| C2C-04 | replay/duplicate/乱序/丢包/重传 | 不重复推进 ratchet/UI；合法乱序在协议界限内恢复；不降级明文 | A | BLOCKED；006/007 已 REGRESSION_PASS，ratchet/dedupe/digest 原子提交和完成态幂等有 C 级回归，真实乱序/丢包/重连待授权 |
| C2C-05 | 离线/重连/App 与后端重启 | 密文可恢复，认证并持久化后才 ACK | A | BLOCKED；005/006 已 REGRESSION_PASS，REST `msg_id` 归一化、可恢复解密结果及 ACK fail-closed 有 C 级回归；Android 真机 SQLCipher stage→关闭/重开句柄→恢复为限定 B PASS，但真实 App 进程/backend 重启仍待授权 |
| C2C-06 | identity/prekey/fallback 替换 | 签名错误拒绝；pin 变化阻断并给出有效告警 | A | BLOCKED；008 已 REGRESSION_PASS，Safety Number 双端聚合与换钥失效有 C 级回归，真实替换/告警复测待授权 |
| C2C-07 | identity/session state 泄露 | 精确证明 FS/PCS 恢复界限，不用协议单测替代真机结论 | C+A | C PARTIAL；A BLOCKED |
| C2C-08 | 新设备/重装/清数据/多设备/撤销 | 每设备合法 fan-out；撤销设备不再收到信封；历史行为符合产品策略 | A | BLOCKED；008 已证明设备集合变化会使本地 Safety Number 验证失效（C）；011 已证明备份不克隆 Olm account/session/TOFU pin，秘密收集失败时拒绝导出，Megolm 写入失败时不报告完整恢复（C）；真实换机恢复与设备生命周期仍待授权 |
| C2C-09 | RSA/Megolm/未知套件降级 | 新消息不能被迫降到旧协议或明文 | C+A | C PARTIAL；A BLOCKED |

## 3. C2G

| ID | 攻击/流程 | PASS 条件 | 目标等级 | 当前状态 |
|---|---|---|---|---|
| C2G-01 | A/B/C/D 四用户真机收发 | 每个授权设备获得自己的合法 room key 并解出一致消息 | A | BLOCKED |
| C2G-02 | 新成员/重入群 | rotation 和历史访问符合已批准策略 | A | BLOCKED；生产 staging 已在 sequence 锁事务内固化 `conv_seq`、request identity 和 recipient snapshot，worker 不再二次分配；群聊附件与 offline timeline 已绑定同一 seq/current generation；D3 历史 key 显式恢复、有限 grant、backup metadata、trusted-seq 门和 migration 112 session attestation 已形成 B/C 级候选。scratch 生命周期已证明 C 加入后旧 session 内容及旧 grant 被拒绝、B leave/rejoin 后旧 grant 被拒绝；生产规模 DDL/cutover、旧客户端 rollout 为 `BLOCKED_EXTERNAL`，真实首次加入/重入攻击复测未闭环 |
| C2G-03 | 主动退出/管理员移除/解散 | 旧成员不能取新 key、发消息、读 history/附件或解新密文 | A | BLOCKED；staging 事务已权威重验 active sender/`@all` role并把同一 recipient snapshot 贯穿全部下游；附件下载按 anchor seq，offline list/count 按当前 open generation 过滤，NULL legacy timeline fail-closed。真实前成员 API、附件、旧 session、解散生命周期及锁竞争攻击复测仍待授权，不能升级为 A 级 PASS |
| C2G-04 | 工作区级移除 | 所有下属群撤销、缓存失效，并在下一消息前 rotate | A | BLOCKED；002 已 REGRESSION_PASS，攻击复测待授权 |
| C2G-05 | 设备增加/撤销/离线恢复 | key 集合只覆盖当前授权设备，不恢复被撤销访问 | A | BLOCKED；003 已 REGRESSION_PASS；room-key 公钥查询改为 active group/caller/recipient/device 单 statement snapshot，成员/设备 4096 上限用 4097 探针 fail-closed，定向 60/60 PASS。2026-09-12 marker scratch-PG fixture 直接执行生产 SQL，覆盖未授权、预置 inactive recipient、active recipient 动态撤销、inactive caller/group/device、无有效 key 和 4096/4097 member/device sentinel，PASS 且残留 0；PG 18.4 只读 EXPLAIN 与最新独立复审均为 APPROVE（0 CRITICAL / 0 HIGH / 0 MEDIUM / 0 LOW），关闭此前 MEDIUM 和动态撤销用例缺口。旧世代未 ACK room-key 经 `/msg/offline` 回流的路径已本地按 timeline seq 修复；historical grant/backup metadata 与服务端 session attestation 已接线，但真实 leave/rejoin/自动导入复测仍未完成 |
| C2G-06 | 旧 session 攻击 | 旧 inbound 不能解 required rotation 后的密文 | A | BLOCKED；客户端 restored grant 对 generation/start/end fail-closed，并可按同 generation、同 start 在线延展；服务端 migration 112 已要求 room-key 与 PFv3 内容共用同 sender/device、recipient generation 集合和单调 seq 范围。真库生产 `stage/12` 矩阵已拒绝 membership 边缘后的旧 session、同 session 更换 room-key MsgId、未知 session、sender UID/DID 变化和非单调 extend；重复同消息保持幂等，所有拒绝路径均断言 sequence/staging/ledger/attestation 不推进，原 attestation HIGH 已本地关闭。真实旧 session/修改版客户端攻击仍未执行，不能标 A 级 PASS |
| C2G-07 | 恶意成员注入 | replay、伪造 sender、旧 sid、跨群 room key 和 metadata 篡改全部拒绝 | A | BLOCKED；005/006/007 已 REGRESSION_PASS，C2G 持久 digest、跨群绑定、room-key 安全存储后 ACK 有 C 级回归，真实恶意成员攻击待授权 |
| C2G-08 | rotation 阈值 | 成员/设备集合、100 条、7 天和重启触发符合实现 | A | BLOCKED |
| C2G-09 | FS/PCS 上限 | 报告 sender-chain/rotation 实际保证，不把 Megolm rotation 称为 Double Ratchet PCS | A | BLOCKED |

## 4. 独立面

| ID | 范围 | PASS 条件 | 当前状态 |
|---|---|---|---|
| X-01 | 附件原文件/缩略图 | 上传对象均为认证密文；独立 key/nonce；content key 只在认证 E2EE 内容内；聊天附件下载遵守 anchor generation | BLOCKED；010 已 REGRESSION_PASS；八个生产上传入口复用最终 message ID 作为 `anchor_msg_id`，视频本体/缩略图同锚，后端本地绑定权威 seq 并按当前 generation 授权。migration 108 scratch 真库矩阵已完成；旧客户端先升级后启用强制 anchor 的 rollout、真实对象存储与缩略图 Canary 复测仍待完成。独立 `group_file` 继续按当前成员共享，不冒充聊天历史。退出/移除只能阻止再次签发，既有 GET URL 最长 600 秒内仍有效，不能宣称即时撤销 |
| X-02 | 附件 metadata/本地文件 | 披露文件名、MIME、URL、size；temp 清理；长期明文缓存有明确策略 | BLOCKED |
| X-03 | Push | Provider/Gateway 无消息明文；记录昵称/群名/类型 metadata | BLOCKED |
| X-04 | Android/iOS 密钥保护 | Keystore/Keychain accessibility、备份迁移与提取抗性符合声明 | B PARTIAL；A BLOCKED |
| X-05 | SQLCipher/文件系统 | 不存在无密码回退、明文备份或泄漏 side file | C PASS（当前 Base 重验 2026-09-09：主机侧 89 PASS/0 FAIL）/ 阶段 A Android 限定范围 B PASS（旧 SHA，D/superseded）；物理 Android 9 真机使用每轮 `Random.secure()` Canary，8/8 PASS：错钥拒绝、原文件字节不变、正确密钥及 inbox 解密结果可重开恢复、数据库文件字节无 Canary、完成态/replay 分类正确、未生成新 `.plain.bak`/`.pre_encrypt.bak`，临时目录已清理且测试包已卸载（历史轮次描述保留）。Secure Storage 为 mock；旧明文库、WAL/SHM、历史备份 artifact 与真实 Keystore 未覆盖，009 保持 REGRESSION_PASS |
| X-06 | Database/日志/备份/WAL | required 模式仅有允许的密文/metadata，无 Canary 或设备/session secret | BLOCKED；014 已 REGRESSION_PASS（当前 Base 重验 2026-09-09：日志边界组 68 PASS/0 FAIL）；011 备份边界当前 Base 重验 52 PASS/1 declared SKIP（REV-1 补跑 server_backup_service 6/6 后计入），破坏性恢复 harness（文件缺失记 BLOCKED_DRIFT）在专用测试包、隔离后端/数据库、受控账号/设备和资源级清理所有权闭环前不得执行或作为发布证据；真实客户端/后端日志、DB、备份与 WAL Canary 扫描待授权 |
| X-07 | Compliance | 明确私钥保管方、授权解密边界、轮换确认和 zero-knowledge 例外 | BLOCKED |
| X-08 | Redis | 仅目标部署实际使用 Redis 时检查 | 当前声明架构 N/A |
| X-09 | AI 明文身份授权 | 真人不能因可污染 badge 误入明文域；agent 身份、用户确认和 deployment 变化均不可绕过 | C `LOCAL_REGRESSION_PASS / AI_ID_B_SELECTED / RISK_ACCEPTED`；用户已明确接受 B 的恶意/被攻陷运行时服务端伪造 Agent 剩余风险，但独立签名身份锚仍缺失，恶意服务端/真实传输 A 级复测 BLOCKED；LT02-SEC-01 不得 CLOSED |

## 5. 证据格式与停止条件

每项记录：随机 Run ID、精确代码/产物哈希、脱敏环境和用户/设备标签、Canary SHA-256、策略与 key/session 前置状态、脱敏复现步骤、期望/实际、PASS/FAIL/PARTIAL/UNKNOWN/BLOCKED/N/A、证据路径及清理结果。

原始证据含敏感内容时放在仓库外；Git 内禁止保存完整抓包、数据库导出、私钥、session pickle、token、账号、PII 或 Canary 明文。

以下任一条件成立立即停止：目标或账号归属不明；发现真实用户/生产数据；操作将通知或影响第三方；授权范围与实际环境不一致；证据可能泄露秘密；需要删除、替换、导出或恢复未获授权的数据/密钥。

只有对应 Finding 完成 `FIXED → REGRESSION_PASS → ATTACK_RETEST_PASS` 才能关闭；缺少 A 级条件不得用历史 PASS、mock 或协议单测替代。
