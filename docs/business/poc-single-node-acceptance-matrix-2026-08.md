# 单节点 PoC 验收矩阵 / Single-Node PoC Acceptance Matrix

> **版本 / Version**: 2026-08 v1.0 | **配套**：[PoC 范围表](./poc-single-node-scope-2026-08.md) ｜ [客户签字记录模板](./poc-customer-signoff-template-2026-08.md)
> **适用环境**：客户授权的单节点 Docker Compose 部署（社区版随仓 compose，或商务版 prod.yml + sales-policy overlay）。
> **执行纪律**：
> 1. 每项记录 PASS / FAIL / BLOCKED 三态之一，附证据（命令输出、截图、日志路径）；BLOCKED 必须注明是否属于客户侧外部依赖（`blocked_external_dependency`）。
> 2. 所有操作在**客户环境目标真机/服务器**上执行；不得以我方本地演示输出充当验收证据。
> 3. E2EE 相关项必须区分 `required`（纯端到端）与 `compliance`（含审计第二收件人）模式，两者不得混称为"端到端加密"。
> 4. 涉及停机的演练（A8/A9）须先取得客户确认的维护窗口。
> 5. 验收结果只代表本次环境与版本，不构成对性能、集群或其他版本的承诺。

---

## 验收项总览

| ID | 验收项 | 对应里程碑 | 结果（填写） | 证据编号 |
|---|---|---|---|---|
| A1 | 安装部署 | D2 | ☐ PASS ☐ FAIL ☐ BLOCKED | |
| A2 | 两账号 C2C 消息 | D6 | ☐ PASS ☐ FAIL ☐ BLOCKED | |
| A3 | C2G 群聊消息 | D6 | ☐ PASS ☐ FAIL ☐ BLOCKED | |
| A4 | E2EE 端到端加密 | D8 | ☐ PASS ☐ FAIL ☐ BLOCKED | |
| A5 | 附件上传（Garage S3） | D6 | ☐ PASS ☐ FAIL ☐ BLOCKED | |
| A6 | 群 / 频道 / 项目空间 | D3–D4 | ☐ PASS ☐ FAIL ☐ BLOCKED | |
| A7 | Admin 管理后台 | D5 | ☐ PASS ☐ FAIL ☐ BLOCKED | |
| A8 | 备份与恢复 | D7 | ☐ PASS ☐ FAIL ☐ BLOCKED | |
| A9 | 升级与回滚 | D7 | ☐ PASS ☐ FAIL ☐ BLOCKED | |

---

## A1 安装部署

**前置**：服务器就绪（Linux x86_64、内存 ≥ 8 GB、Docker 24+ / Compose v2.23.1+）；两个域名已解析到本机；80/443 可公网访问。

**步骤**：

```bash
cd deploy
cp .env.example .env && $EDITOR .env     # 人工仅填 API_DOMAIN / ADMIN_DOMAIN / CERTBOT_EMAIL
bash preflight.sh                        # 前置检查全 PASS（注意：.env 缺失时 preflight 直接退出）
bash install.sh --edition community      # 商务版：--edition business（prod.yml 需经商务渠道获取）
```

**通过判据**（全部满足）：
1. `install.sh` 正常结束，并打印 **Release Identity 三元组**（`IMBOY_VERSION` / `IMBOY_GIT_SHA` / `IMBOY_IMAGE_DIGEST`），抄录进签字记录；
2. 容器健康：`docker compose -f docker-compose.community.yml ps` 显示 `imboy_pg18`、`imboy_garage`、`imboy_backend`、`imboy_admin`、`imboy_nginx` 为 running/healthy；
3. 后端健康检查：`curl -s https://<API_DOMAIN>/api/v1/init` 返回 `{"code":0,...}`；
4. 管理后台 `https://<ADMIN_DOMAIN>` 可打开登录页；超管已通过网页 `/setup` 向导或 `install.sh --admin-phone/--admin-password` 建立；
5. 超管凭据移交客户方保管，我方不留存。

## A2 两账号 C2C 消息

**前置**：A1 通过；两台目标真机已安装客户端并指向 `https://<API_DOMAIN>`。

**步骤**：
1. 真机甲注册账号 U1（邮箱 + 验证码），真机乙注册账号 U2；
2. U1 添加 U2 为好友，U2 通过；
3. U1 → U2 发送文本消息，U2 回复一条。

**通过判据**：双向消息在双方真机即时可见；断网重连后消息不丢失、不重复；会话列表与未读计数正确。

## A3 C2G 群聊消息

**前置**：A2 通过；已按主场景创建至少一个群（含 U1、U2）。

**步骤**：U1 在群内发送文本；U2 在群内回复；第三成员（如适用）仅看到自己加入后的消息。

**通过判据**：群消息按成员正确分发；新成员历史可见性符合管理端配置；群内 @提及（如启用）通知正常。

## A4 E2EE 端到端加密

**前置**：A2/A3 通过；Admin 后台可将 E2EE 模式切换为 `required`（纯端到端）；已向客户书面披露 `compliance` 模式的第二收件人机制。

**步骤**：
1. 将 E2EE 模式设为 `required`；U1 → U2 发送一条含敏感样例词的消息；
2. U2 正常解密阅读；
3. 在服务器上取证数据库密文：

```bash
docker exec imboy_pg18 psql -U "$POSTGRES_USER" -d "$POSTGRES_DB" \
  -c "SELECT payload, e2ee FROM message ORDER BY created_at DESC LIMIT 1;"
```

**通过判据**（全部满足）：
1. 目标真机双方收发正常；
2. `payload` 为密文（不含明文样例词）；
3. 我方可出示服务端零知识不变量的验证方式（后端仓 `make e2ee-verify` 全量加密安全测试通过记录，作为**研发侧证据**，不替代客户环境取证）；
4. 若客户选择评估 `compliance` 模式：确认客户已知晓持合规私钥者可解密该模式全部消息、合规私钥由客户自行生成保管、服务端永不接收；未选择则跳过本条。

## A5 附件上传（Garage S3）

**前置**：A2 通过；社区版内置 Garage 对象存储已随 A1 启动（商务版确认对应 S3 配置就绪）。

**步骤**：
1. U1 在 C2C 会话发送图片与文档各一份；U2 预览/下载；
2. 在群内重复一次；
3. 尝试绕过客户端直接访问对象存储原始 URL（未经授权路径）。

**通过判据**：
1. 附件上传成功，接收方可在线预览并完整下载，文件字节一致（可对比哈希）；
2. 附件访问走授权 URL（经 `AssetsService.viewUrl` 机制），**未经授权的原始 URL 不能读取内容**；
3. 可选用 `scripts/garage_e2e_test.sh` 做端到端自检并留存输出。

## A6 群 / 频道 / 项目空间

**前置**：A1 通过；主场景涉及的业务对象已在 D3–D4 配置。

**步骤**：
1. 创建群（≥ 2 成员）：发公告、发消息；
2. 创建频道：发布频道消息/公告，成员订阅可见；
3. 创建项目空间（workspace / project）：在项目下建任务，指派成员、更新状态。

**通过判据**（按主场景裁剪，未启用的对象记 N/A）：
1. 三类对象的创建、成员可见范围与客户确认的组织结构一致；
2. 未被授权的账号（用 U2 反向验证）看不到未授权对象；
3. 任务状态流转在成员端可见。

## A7 Admin 管理后台

**前置**：A1 通过；超管账号由客户方保管。

**步骤**：
1. 以超管登录 `https://<ADMIN_DOMAIN>`；
2. 用户管理：检索 U1/U2，核对注册信息；
3. 系统设置：查看/切换 E2EE 模式（disabled/optional/required/compliance），并复述各模式含义给客户确认；
4. 群/频道治理：查看 A6 创建的对象与成员。

**通过判据**：四步操作全部完成且权限表现符合客户治理规则；普通用户账号无法访问 Admin 后台；管理操作有审计/日志可查（按当前版本实际能力如实记录）。

## A8 备份与恢复

**前置**：A1–A5 已产生测试数据；客户已确认演练窗口；手册：[backup-restore.md](../guides/operations/deployment/backup-restore.md)，操作链见[运维操作链索引](../guides/operations/single-node-ops-chain-index-2026-08.md)。

**步骤**：
1. 全量备份：`bash scripts/backup_pg.sh --full`（pg_dump 自定义格式）；
2. 附件备份：`bash scripts/backup_garage.sh`（rclone 同步 Garage bucket，需预配置 remote）；
3. 验证备份可读：`pg_restore --list <备份文件>`；
4. 在客户指定的恢复目标（独立实例或维护窗口内的原环境）执行 `bash scripts/restore_pg.sh`（内含恢复后行数抽样校验）；
5. 恢复后抽验 A4 的那条加密消息仍在且仍为密文、附件可下载。

**通过判据**：
1. 备份文件生成且可列出清单；
2. 恢复过程完成，行数抽样校验通过；
3. 恢复后核心数据（用户、消息、附件）可访问且 E2EE 密文属性不变；
4. 全程耗时已记录（对照 RTO 目标 < 30 分钟**如实记录实测值**，不达标不判 FAIL 但必须写入限制说明）。

## A9 升级与回滚

**前置**：A8 通过（升级前必有可用备份）；版本与升级说明真相源：[RELEASES.md](../../RELEASES.md)。

**步骤**：
1. 升级前：比对运行环境三元组与 Release 说明的三元组；
2. 按目标版本在 [RELEASES.md](../../RELEASES.md) 对应「升级说明」小节执行升级（正式版发布后另有 [upgrade-runbook.md](../guides/operations/upgrade-runbook.md)，含回滚章节）；
3. 升级后：重跑 A1 判据 2–3（容器健康 + API 健康检查）与 A2 冒烟；
4. 回滚演练：按 upgrade-runbook 的回滚路径（PITR + 切回旧版本）演练一次，或在当前版本做"重装 + A8 备份恢复"等效演练。

**通过判据**：
1. 升级后版本三元组正确变更，健康检查与冒烟通过；
2. 回滚演练后系统恢复到演练前版本与数据状态；
3. 升级/回滚耗时已记录。

> ⚠️ **当前版本状态如实披露**：`RELEASES.md` 记载首个正式 semver 版本尚未发布（Golden Install + Golden Upgrade 双门禁未全绿），`1.0.0-alpha.*` 为内部版本。PoC 若在正式版发布前执行，A9 只能按"跨 alpha 内部版本升级 + 备份恢复等效回滚"演练并**如实记录该限制**，不得宣称"正式版升级路径已验证"。

---

## 结论状态（四选一，与 PoC 范围表 §7 对应）

| 状态 | 含义 |
|---|---|
| `accepted` | A1–A9 无 FAIL（N/A 与已披露限制除外），硬性安全门（A4 判据 4、A5 判据 2、A7 权限判据）全部通过 |
| `rework` | 存在可复现 FAIL，列出修复项与复测日期 |
| `blocked_external_dependency` | 客户环境/授权/设备未就绪导致关键项 BLOCKED |
| `no-go` | 出现越权、未授权处理、数据丢失，或客户判定价值不足 |

执行记录汇总后填入[客户签字记录模板](./poc-customer-signoff-template-2026-08.md)。
