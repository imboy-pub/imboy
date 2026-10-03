# E2EE 投产攻击矩阵与真实旅程合同（AC-22 / AC-23）

- **计划**: [plan.md](./plan.md)（run-20261003-094804）
- **Owner 卡**: C11（攻击矩阵与真实旅程测试）；真实旅程执行另需 C13 设备授权
- **历史基线**: [`docs/security/audits/e2ee-2026-09-07/E2EE_ATTACK_MATRIX.md`](../../security/audits/e2-2026-09-07/E2EE_ATTACK_MATRIX.md)（C2C-01..09 / C2G-01..09 / X-01..09，全部映射见 §8）
- **编写日期**: 2026-10-03 Asia/Shanghai
- **状态**: 本文件定义 AC-22/AC-23 的逐场景合同与 PASS 条件；**尚未执行任何真实旅程**。执行前置依赖清单见 §9。

## 0. 目的与边界

1. 本矩阵把 AC-22（两账号真实 C2C）与 AC-23（四账号 C2G）拆成可逐格验收的场景，每格定义：前置 fixture、攻击/操作步骤、双方 oracle、PASS 条件。
2. 冒烟层（`imboyapp/integration_test/e2ee_release/`，本地嵌入式服务器替身 + vodozemac 真实加解密）只用于 harness 自检，**不构成 AC-22/AC-23 证据**（计划 §4：跨平台脚本目前是合成 interop，不满足 AC-22/23）。
3. 真实旅程必须满足：真实授权后端 + 合成账号 + 真机（Android/iOS/macOS 按支持矩阵）+ 双端 UI/解密 oracle + 服务端密文存储对应关系。只 UI 截图不算 PASS（计划 AC-23 原文），模拟器不充当真机。
4. 本文件不得保存账号口令、token、私钥、session secret、真实消息、PII、完整敏感 payload、真实设备 ID 或可复用 Canary 明文。

## 1. 授权单与 Canary（沿用历史矩阵 §1）

每次真实旅程运行前，用户按历史矩阵 §1 逐项确认：后端/PG/对象存储、Push、C2C 用户 A/B、C2G 用户 A/B/C/D、真机清单、群与成员操作范围、数据查询范围、抓包/篡改/replay 范围、密钥/App 数据实验范围、时间与留存。不得复用历史账号、口令、token、设备、端口或地址。

Canary 每轮临时生成，证据只留 SHA-256 与脱敏前缀（沿用历史矩阵 §1 的生成方式）：

```bash
CANARY="IMBOY-E2EE-$(date +%s)-$(openssl rand -hex 24)"
printf '%s' "$CANARY" | shasum -a 256
```

停止条件沿用历史矩阵 §5：目标/账号归属不明、发现真实用户/生产数据、操作影响第三方、授权范围与环境不一致、证据可能泄露秘密、需删除/替换/导出未授权数据 —— 任一成立立即停止并记 `BLOCKED_USER_AUTH`。

## 2. 合成 fixture contract（摘要）

完整可执行定义：`imboyapp/integration_test/e2ee_release/fixture_contract.dart`（随 C11 commit 入仓）。命名规范与结构：

| 类别 | 命名规范 | 示例 |
|---|---|---|
| C2C 账号 | `c11-c2c-{a\|b}-{runTag}` | `c11-c2c-a-r1003` |
| C2G 账号 | `c11-c2g-{a\|b\|c\|d}-{runTag}` | `c11-c2g-c-r1003` |
| 设备 | `{ownerAccount}-dev{n}` | `c11-c2c-b-r1003-dev2` |
| 群组 | `c11-c2g-group-{runTag}` | `c11-c2g-group-r1003` |
| 消息 ID | `c11msg-{scene}-{seq}` | `c11msg-s01-0003` |
| canary 载荷 | 见 §1，消息文本内嵌 canary | 仅 SHA-256 入证据 |

规则：runTag 每轮随机（≤12 位 `[a-z0-9]`），全小写；禁止使用任何真实账号/设备序列号/脚本默认值（如历史脚本里的 `XWE6R19916004085` 只是旧默认，不得沿用）；口令等秘密由本地环境注入，不入仓。合成账号只在授权隔离后端创建，旅程结束按授权范围清理。

## 3. 双 oracle 定义（所有场景通用）

每个场景同时采集两类 oracle，缺一不可：

### 3.1 设备显示 oracle（端侧）

- 真机：旅程结束时每台设备会话页的实际渲染内容（Patrol/集成测试文本断言为主，截图仅补充）；附件场景需实际打开并校验内容可读。
- 断言口径：接收端显示文本 == 发送端发送文本 == 预期明文（含 canary）；解密失败必须以明确失败态呈现（`_e2ee_failed` 分类/占位），不得静默丢弃或显示乱码冒充成功。

### 3.2 服务端密文 oracle（服务端）

在授权范围内查询（`<AUTHORIZED_DB_LOG_BACKUP_OBJECT_PUSH_SCOPE>`）：

- DB 消息表/归档表中该 `message_id` 行的 payload/e2ee 字段仅含密文与允许元数据（协议、版本、设备枚举），canary 明文扫描 = 0 命中；
- 对象存储（Garage S3）：附件对象为认证密文（独立 content key/nonce + 认证 descriptor），消息体与对象元数据均无明文；
- 日志/队列/Push 下发记录：无 canary 明文（昵称/群名/类型等允许 metadata 除外，按 AC-11/AC-34 口径）；
- **对应关系**：每个设备显示的消息 ↔ 服务端存储行通过 `message_id`（C2C）/ `anchor_msg_id`+seq（C2G/附件）一一对应，且服务端密文无法在无端侧秘密时还原明文（C2C-02 面复测）。

### 3.3 PASS 条件模板

| 代号 | 条件 |
|---|---|
| P1 | 双端（或多端）显示 oracle 一致且等于发送输入 |
| P2 | 服务端密文 oracle 全表面 canary 0 命中，对应关系成立 |
| P3 | 攻击场景 fail-closed：认证失败/拒绝/告警，不降级明文、不重复推进 ratchet/UI、不虚报 ACK |
| P4 | 平台矩阵（§5）双向覆盖，无缺平台（未授权平台记 `BLOCKED_USER_AUTH`，不得 N/A 抹掉） |
| P5 | C2G 逐会话/seq 校验：每个成员按其授权世代/区间实际可解/不可解与策略一致 |

## 4. AC-22 场景矩阵（两账号真实 C2C）

fixtures 基线：`F_C2C = {A: c11-c2c-a-*, B: c11-c2c-b-*}`；A、B 各 ≥1 台真机；场景 S06 另加 B 的第二设备。每格默认含 P1+P2；攻击场景另含 P3。

| 场景 ID | 场景 | 前置 fixture | 攻击/操作步骤 | oracle（设备 + 服务端） | PASS |
|---|---|---|---|---|---|
| AC22-S01 | 文字发送（A→B） | F_C2C，双向已完成密钥交换/互信 | A 发含 canary 的文本 | B 显示 == 发送文本；服务端消息行/归档/日志 canary 扫描 0 命中，`message_id` 对应 | P1+P2 |
| AC22-S02 | 文字接收与回复（B→A，双向） | S01 通过 | B 回复含 canary2 的文本 | A 显示 == 回复文本；服务端同上；两个方向密文行独立 | P1+P2 |
| AC22-S03 | 附件（图片+文件） | F_C2C + 授权对象存储 | A 发图片与文件（各含 canary 嵌入像素/字节）；B 下载并打开 | B 端打开内容 == 原文件；对象存储对象为密文（独立 key/nonce/descriptor 认证）；消息行仅允许元数据 | P1+P2（缩略图同面） |
| AC22-S04 | 离线 | F_C2C，B 先登出/断网 | B 离线期间 A 发 N≥3 条（含 canary）；B 重新上线拉取 | B 上线后按序解出全部 N 条且无重复；离线期间服务端仅存密文；ACK 在认证并持久化之后（C2C-05 面） | P1+P2 |
| AC22-S05 | 重连/后端重启 | F_C2C | 传输中断→重连；期间 A 重试发送（同 msg_id 去重） | 重连后 B 收到且仅收到一份；ratchet/UI 不重复推进；服务端无未持久化密文被交付 | P1+P2+P3(重复帧) |
| AC22-S06 | 多设备 | F_C2C + B-dev2（经可信旧设备批准加入） | A 发 per-device fan-out 消息；随后撤销 B-dev2 再发 | B 两设备各自解出；撤销后 dev2 不再收到信封，dev1 正常；服务端 fan-out 信封按设备独立密文 | P1+P2（+AC-15/AC-04 联合） |

### 4.1 AC-22 恶意服务器代理模式（可控 MITM）

**插入方式（本地 proxy 配置点）**：App 构建以 dart-define 把 API/WS 端点指向本地可控代理（监听 loopback），代理上游指向 `<AUTHORIZED_ISOLATED_BACKEND>`；WS 帧与 REST 目录响应在代理处按场景改写。注入点分类：

- **W1 WS 帧级**：对 `e2ee` 信封字节做翻转/截断/替换、重复投递、重排投递；
- **W2 REST 目录级**：改写 identity/prekey/fallback/room-key/设备清单端点响应（换公钥、删字段、回滚旧版本）；
- **W3 流量调度**：丢弃、延迟、乱序、洪泛（耗尽）；
- **W4 回滚**：重发旧 manifest/旧 room-key/旧 session attestation。

| 场景 ID | 攻击 | 注入点 | 操作 | PASS（P3 细则） |
|---|---|---|---|---|
| AC22-M01 | replay/重复 | W1 | 同一密文帧重复投递 ≥2 次 | B 端去重：UI 只显示一次，ratchet 不重复推进；不产生新 ACK |
| AC22-M02 | 乱序 | W1/W3 | 打乱多条密文投递顺序（含跨 ratchet 间隔） | 合法乱序在协议界限内恢复；越界乱序明确失败态，不降级明文 |
| AC22-M03 | 篡改 | W1 | 密文字节翻转/截断、header/nonce/tag/metadata 替换、同 ID 换密文、同密文换 ID | 全部认证失败且不作为已交付 ACK；PFv3 外层 hash/CBOR 校验拒绝（C2C-03 面） |
| AC22-M04 | 身份目录攻击 | W2/W4 | 替换 B 的 identity/prekey/fallback、伪造设备清单、回滚旧 manifest | 签名/信任链验证失败拒绝；pin/safety-number 变化阻断并有效告警（C2C-06 面）；KT 证明缺失按策略阻断（AC-21） |
| AC22-M05 | 耗尽 | W3 | 洪泛垃圾帧/超大帧、OTK 耗尽诱导 | 客户端限流/拒绝且不崩溃；OTK 耗尽可观测不泄露 uid 等攻击择时信息（AC-26 口径） |

## 5. AC-22 平台矩阵（双向覆盖）

按 C00 冻结的客户端支持矩阵执行；下表为计划口径，执行时以 C00 快照为准。任一格未获设备授权记 `BLOCKED_USER_AUTH`，不得以另一平台替代。

| 方向 | Android 真机 | iOS 真机 | macOS |
|---|---|---|---|
| A→B（每平台发送） | S01–S06 | S01–S06 | S01–S06 |
| B→A（每平台接收+回复） | S01–S06 | S01–S06 | S01–S06 |
| 跨平台对（如 Android→iOS） | 按支持矩阵抽样 ≥1 组/平台对 | 同左 | 同左 |

## 6. AC-23 场景矩阵（四账号 C2G）

fixtures 基线：`F_C2G = {A,B,C,D: c11-c2g-*}` + 群 `c11-c2g-group-{runTag}`；A 为群主。每格默认含 P1+P2+P5。

| 场景 ID | 场景 | 前置 fixture | 攻击/操作步骤 | oracle（设备 + 服务端） | PASS |
|---|---|---|---|---|---|
| AC23-G01 | 建群四端基础收发 | F_C2G 建群，A/B/C/D 各真机 | A 发含 canary 文本；B/C/D 各回一条 | 四端显示一致；服务端 C2G 行密文+generation/seq 元数据；每成员用自己的 room key 解出（C2G-01 面） | P1+P2+P5 |
| AC23-G02 | 新成员加入 | G01 后 C 退群再加入（或 D 二次入群） | 加入前消息密文留存；加入后新消息 | 加入后 C 能解加入后消息；C 不能解加入前消息（除非 D3 显式 grant，且 grant 区间准确）；服务端 recipient snapshot/世代记录与之一致 | P1+P2+P5（C2G-02 面） |
| AC23-G03 | 踢人 | G01 后群主移除 D | 移除后 A 发新消息 | D 端不能取新 key、不能解新密文、不能读新 history/附件；A/B/C 正常；服务端新世代 recipient 集不含 D 的设备 | P1+P2+P5（C2G-03 面） |
| AC23-G04 | 重入 | G03 后 D 重新申请入群 | 重入前后消息 | 重入后 D 的旧 grant/session 失效，仅按新授权区间可解；服务端 grant 账本区间与 D 实际解密能力一致 | P1+P2+P5（C2G-02 重入面） |
| AC23-G05 | 成员离线 | G01 后 C 离线 | 离线期间 ≥10 条消息（跨 rotation 阈值更佳）；C 上线 | C 上线后按 seq/generation 依序解出其授权区间内全部消息；区间外（如被踢前世代）不可解；服务端 offline timeline 按当前世代过滤 | P1+P2+P5 |
| AC23-G06 | 换机 | G01 后 D 弃旧设备换新设备 | 旧设备撤销 + 新设备经信任流程加入 | 新设备可解；旧设备不再收到任何新信封；服务端设备清单/密钥枚举一致（C2C-08/C2G-05 面） | P1+P2+P5 |

### 6.1 AC-23 恶意成员/服务端攻击场景（沿用 §4.1 注入点 + 恶意成员客户端）

| 场景 ID | 攻击 | 注入点/操作 | PASS（P3 细则） |
|---|---|---|---|
| AC23-M01 | 伪造 sender/跨群 room key | 恶意成员客户端用他群 room key 或伪造 sender UID/DID 发帧 | 接收端 session/群绑定校验拒绝；不进入 UI；服务端 attestation/世代账本不推进 |
| AC23-M02 | 旧 session/旧 grant 重放 | W4：重放被踢成员旧 session 密文与旧 grant | 旧 inbound 不能解撤销后密文；restored grant 对 generation/start/end fail-closed（C2G-06 面） |
| AC23-M03 | seq 越界/伪造 attestation | W2：伪造 session attestation、非单调 seq、跨 generation 重放 | 服务端顺序锁拒绝；客户端 trusted-seq 门拒绝；ledger 不推进 |
| AC23-M04 | 恶意成员注入（C2G-07 面） | 恶意成员发 metadata 篡改/旧 sid 帧 | 全部拒绝且持久 digest 不被污染 |
| AC23-M05 | rotation 竞态 | 成员变更与发送/密钥分发并发 | 撤销后消息不被旧成员解出；失败触发刷新/rotate 或拒发（AC-17） |

## 7. harness 与冒烟层边界（防伪声明）

- `imboyapp/integration_test/e2ee_release/` 的本地嵌入式服务器替身 + 模拟设备冒烟层：vodozemac 真实加解密、内存消息总线、注入点枚举，仅证明 harness 自身可用（fixture 合法、oracle 断言器有效、注入器可触发 fail-closed）。
- 该层**不登录、不连真实后端、不产生 AC-22/AC-23 证据**；真实层入口在代码中一律 `REQUIRES_AUTHORIZED_BACKEND` 标注并在未授权时抛错。
- 历史脚本 `scripts/run_cross_platform_e2ee_interop*.sh` 属合成 interop（计划 §4 已认定不满足 AC-22/23），仅作能力参考。

## 8. 历史矩阵映射（C2C/C2G/X 全部行 → 本计划 AC）

历史状态列保持原判（`BLOCKED` 等），本表只建立映射，不改判；结案仍按历史矩阵 §5 `FIXED→REGRESSION_PASS→ATTACK_RETEST_PASS` 链。

| 历史行 | 历史状态 | 映射到本计划 | 承接场景/说明 |
|---|---|---|---|
| C2C-01 Canary 全表面搜索 | BLOCKED | AC-28（C13）+ AC-22 P2 | §3.2 服务端密文 oracle 即其真实旅程版 |
| C2C-02 服务端 root 恢复 | BLOCKED | AC-07/AC-08（C03） | 无设备秘密不能恢复；AC22-M04 目录攻击辅助验证 |
| C2C-03 PFv3/密文篡改 | BLOCKED（C 级已回归） | AC-10（C04）+ AC22-M03 | M03 即其 A 级复测场景 |
| C2C-04 replay/乱序/丢包 | BLOCKED（C 级已回归） | AC-16（C07）+ AC22-M01/M02 | 真实 Transport 复测 |
| C2C-05 离线/重连/重启 | BLOCKED（C 级已回归） | AC22-S04/S05 + AC-26 | 含真实 App 进程/backend 重启 |
| C2C-06 identity/prekey 替换 | BLOCKED（C 级已回归） | AC-21（C10）+ AC22-M04 | KT 证明链 + safety-number 阻断 |
| C2C-07 FS/PCS 泄露界限 | C PARTIAL / A BLOCKED | AC-15/AC-16（C07）+ AC-33（C16） | 真机泄露实验归 C07/C16，AC-22 不重复主张 |
| C2C-08 新设备/重装/多设备/撤销 | BLOCKED | AC-04（C01）+ AC-15 + AC22-S06 | 真实换机恢复归 C07/C13 联合 |
| C2C-09 套件降级 | C PARTIAL / A BLOCKED | AC-05（C02）+ AC-32（C16） | 能力协商 fail-closed |
| C2G-01 四用户真机收发 | BLOCKED | AC23-G01 | 本矩阵直接承接 |
| C2G-02 新成员/重入 | BLOCKED（B/C 级候选） | AC18（C08）+ AC23-G02/G04 | 生产规模与真实攻击复测在此闭环 |
| C2G-03 退出/移除/解散 | BLOCKED | AC-17（C08）+ AC23-G03 | 解散场景作为 G03 扩展步 |
| C2G-04 工作区级移除 | BLOCKED（C 级已回归） | AC-04 + AC-17 | 工作区面在 AC23-G03 扩展（授权群范围内） |
| C2G-05 设备增加/撤销/离线恢复 | BLOCKED（B/C 级已过） | AC-04 + AC23-G05/G06 | 4096 上限等已由后端覆盖，本矩阵验真实端到端 |
| C2G-06 旧 session 攻击 | BLOCKED | AC-17 + AC23-M02 | staging 矩阵已 B 级，A 级复测在此 |
| C2G-07 恶意成员注入 | BLOCKED（C 级已回归） | AC-18 + AC23-M01/M04 | |
| C2G-08 rotation 阈值 | BLOCKED | AC-17 + AC-26 | 阈值触发面在 AC23-G05 跨阈值离线中覆盖 |
| C2G-09 FS/PCS 上限报告 | BLOCKED | AC-17 报告口径 + AC-31（C15） | 不把 Megolm rotation 称 PCS（沿用原判） |
| X-01 附件密文/anchor | BLOCKED | AC-09（C04）+ AC22-S03 | 真对象存储 + 缩略图 canary 复测 |
| X-02 附件 metadata/本地文件 | BLOCKED | AC-10 + AC-12（C05） | 下载/缓存/临时文件策略 |
| X-03 Push 无明文 | BLOCKED | AC-11 + AC-22 P2（Push 面入 canary 扫描） | 授权 `<AUTHORIZED_TEST_PUSH_TARGET>` 或 NOT_IN_SCOPE |
| X-04 密钥保护 | B PARTIAL / A BLOCKED | AC-12 + AC-27（C13） | 真机 Keychain/Keystore 取证 |
| X-05 SQLCipher/文件系统 | C PASS / B 旧轮 | AC-11 + AC-27 | 冻结候选重验 |
| X-06 DB/日志/备份/WAL | BLOCKED | AC-11 + AC-28 | 真实面 canary 扫描归 C13 |
| X-07 Compliance 边界 | BLOCKED | AC-13/AC-14（C06） | |
| X-08 Redis | N/A | AC-34 复核声明 | 目标部署不用 Redis 时维持 N/A（只在启用时检查） |
| X-09 AI 明文身份门 | C 级已过 / A BLOCKED | AC-13/AC-14（C06） | AI-ID=B 风险接受不冒充完成，独立签名锚归 C06/C09 |

## 9. 真实旅程执行前置依赖清单（AC-22/AC-23 完整运行）

1. **后端与基础设施授权**（`<AUTHORIZED_ISOLATED_BACKEND>` / DATABASE / OBJECT_STORE）：隔离后端部署冻结候选 imboy 后端 + PG + Garage S3 + Push stub；配套授权查询范围（DB/日志/备份/对象/Push 五面只读）。
2. **前置卡完成**：C07（多设备身份/安全码/生产接线）、C08（群成员/room-key/历史授权）、C10（KT/交叉签名，AC22-M04 需要）；C12 迁移门通过（后端 schema 冻结）。
3. **合成账号**：C2C×2、C2G×4，按 §2 命名在授权后端注册；口令/凭据本地环境注入。
4. **真机设备授权**：Android/iOS/macOS 按 §5 支持矩阵的设备清单（禁模拟器、禁沿用脚本默认设备 ID）；设备需可安装测试包 + 取证访问。
5. **MITM 代理授权**：本地可控代理的抓包/篡改/replay 范围授权（§4.1 注入点 W1–W4）。
6. **密钥/数据实验授权**：身份目录改写、OTK 耗尽、设备撤销/换机操作范围。
7. **C13 联动**：服务端 canary 全表面搜索与端侧取证由 C13 执行（AC-27/AC-28），AC-22/AC-23 的 P2 oracle 数据来自同一授权窗口。
8. 上述任一缺失时，对应场景记 `BLOCKED_USER_AUTH` / `BLOCKED_INFRA`，不降级为合成证据。

## 10. 证据格式

每场景记录：场景 ID、Run ID、两仓 candidate SHA、config hash、脱敏环境与设备标签、canary SHA-256、前置密钥/会话状态、注入点与参数（攻击场景）、双方 oracle 原始输出（设备断言文本/服务端查询结果）、PASS/FAIL/BLOCKED、证据路径与清理结果。原始敏感证据放仓库外 evidence 根（`.Codex/evidence/e2ee-excellence/<run_id>/cards/C11/`），Git 内禁入完整抓包、私钥、pickle、token、PII 或 Canary 明文。
