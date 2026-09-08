# IMBoy E2EE 攻击矩阵

日期：2026-09-07  
状态：静态/协议检查可本地执行；本轮仅授权一台物理 Android 9 真机上的 SQLCipher 随机临时库测试且已通过；账号、消息、群、真实 App 数据、数据查询、抓包、篡改、密钥和外部服务操作仍为 BLOCKED。Findings 与发布结论见 [`E2EE_AUDIT_REPORT.md`](./E2EE_AUDIT_REPORT.md)。

本文件不得保存账号口令、token、私钥、session secret、真实消息、PII、完整敏感 payload、真实设备 ID 或可复用 Canary。

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

本轮已授权记录（脱敏）：物理 Android 9 真机；仅安装并运行测试包，只在应用随机临时目录创建、读取、错钥打开并删除本轮 SQLCipher 测试库；不登录账号、不连接后端、不读取或修改现有 IMBoy App 数据。设备序列号和 Canary 明文不入库。

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
| C2G-02 | 新成员/重入群 | rotation 和历史访问符合已批准策略 | A | BLOCKED / SECURITY DESIGN GAP；012 已 ROOT_CAUSE_CONFIRMED：history 与批量 sync 只按当前 active membership 开放整个群归档，现有成员行无稳定的本次入群边界；首次加入、退出/移除后重入、同账号新设备策略待人工拍板 |
| C2G-03 | 主动退出/管理员移除 | 旧成员不能取新 key、发消息、读 history/附件或解新密文 | A | BLOCKED；001/003/004 已 REGRESSION_PASS，真实前成员 API、附件与旧 session 攻击复测待授权 |
| C2G-04 | 工作区级移除 | 所有下属群撤销、缓存失效，并在下一消息前 rotate | A | BLOCKED；002 已 REGRESSION_PASS，攻击复测待授权 |
| C2G-05 | 设备增加/撤销/离线恢复 | key 集合只覆盖当前授权设备，不恢复被撤销访问 | A | BLOCKED；003 已 REGRESSION_PASS，强刷空结果/异常本地 fail-closed，真实设备复测待授权 |
| C2G-06 | 旧 session 攻击 | 旧 inbound 不能解 required rotation 后的密文 | A | BLOCKED |
| C2G-07 | 恶意成员注入 | replay、伪造 sender、旧 sid、跨群 room key 和 metadata 篡改全部拒绝 | A | BLOCKED；005/006/007 已 REGRESSION_PASS，C2G 持久 digest、跨群绑定、room-key 安全存储后 ACK 有 C 级回归，真实恶意成员攻击待授权 |
| C2G-08 | rotation 阈值 | 成员/设备集合、100 条、7 天和重启触发符合实现 | A | BLOCKED |
| C2G-09 | FS/PCS 上限 | 报告 sender-chain/rotation 实际保证，不把 Megolm rotation 称为 Double Ratchet PCS | A | BLOCKED |

## 4. 独立面

| ID | 范围 | PASS 条件 | 当前状态 |
|---|---|---|---|
| X-01 | 附件原文件/缩略图 | 上传对象均为认证密文；独立 key/nonce；content key 只在认证 E2EE 内容内 | BLOCKED；010 已 REGRESSION_PASS，真实对象存储与缩略图 Canary 复测待授权 |
| X-02 | 附件 metadata/本地文件 | 披露文件名、MIME、URL、size；temp 清理；长期明文缓存有明确策略 | BLOCKED |
| X-03 | Push | Provider/Gateway 无消息明文；记录昵称/群名/类型 metadata | BLOCKED |
| X-04 | Android/iOS 密钥保护 | Keystore/Keychain accessibility、备份迁移与提取抗性符合声明 | B PARTIAL；A BLOCKED |
| X-05 | SQLCipher/文件系统 | 不存在无密码回退、明文备份或泄漏 side file | C PASS / Android 限定范围 B PASS；物理 Android 9 真机使用每轮 `Random.secure()` Canary，8/8 PASS：错钥拒绝、原文件字节不变、正确密钥及 inbox 解密结果可重开恢复、数据库文件字节无 Canary、完成态/replay 分类正确、未生成新 `.plain.bak`/`.pre_encrypt.bak`，临时目录已清理且测试包已卸载。Secure Storage 为 mock；旧明文库、WAL/SHM、历史备份 artifact 与真实 Keystore 未覆盖，009 保持 REGRESSION_PASS |
| X-06 | Database/日志/备份/WAL | required 模式仅有允许的密文/metadata，无 Canary 或设备/session secret | BLOCKED；014 已 REGRESSION_PASS，消息链路日志脱敏有 C 级回归；011 备份包、收集失败和恢复写失败边界有 57/57 C 级回归，备份导入 widget 的 cloud probe 已改为本地注入并 7/7 PASS；破坏性恢复 harness 默认门只得到 SKIP，仍缺专用 App 容器、受控账号/后端和不会误删其他版本的清理所有权，未执行 destructive 分支；真实客户端/后端日志、DB、备份与 WAL Canary 扫描待授权 |
| X-07 | Compliance | 明确私钥保管方、授权解密边界、轮换确认和 zero-knowledge 例外 | BLOCKED |
| X-08 | Redis | 仅目标部署实际使用 Redis 时检查 | 当前声明架构 N/A |

## 5. 证据格式与停止条件

每项记录：随机 Run ID、精确代码/产物哈希、脱敏环境和用户/设备标签、Canary SHA-256、策略与 key/session 前置状态、脱敏复现步骤、期望/实际、PASS/FAIL/PARTIAL/UNKNOWN/BLOCKED/N/A、证据路径及清理结果。

原始证据含敏感内容时放在仓库外；Git 内禁止保存完整抓包、数据库导出、私钥、session pickle、token、账号、PII 或 Canary 明文。

以下任一条件成立立即停止：目标或账号归属不明；发现真实用户/生产数据；操作将通知或影响第三方；授权范围与实际环境不一致；证据可能泄露秘密；需要删除、替换、导出或恢复未获授权的数据/密钥。

只有对应 Finding 完成 `FIXED → REGRESSION_PASS → ATTACK_RETEST_PASS` 才能关闭；缺少 A 级条件不得用历史 PASS、mock 或协议单测替代。
