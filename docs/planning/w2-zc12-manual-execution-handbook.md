# ZC-12 人工操作执行手册 — 真机、真人、干净部署与 Release Gate

> 版本：1.0 | 日期：2026-08-29 | Owner：总控 Agent（技术准备）+ 人工操作者（执行）
> 代码态：imboy `db41789a` / imboyapp `a18e89e5` / imboyadmin `46c7e10`（+ 各仓 W2 未提交产物，commit/push 待 H4 授权）
> 版本：后端 `1.0.0-alpha.70` / App `1.0.0-alpha.16+6` / Admin `1.0.0-alpha.16`

## 〇、技术前置（已由总控验证，2026-08-29）

✅ **空库全链部署演练通过**：空 PG 库 + 12 个扩展（postgis, pgrouting, postgis_topology, fuzzystrmatch, postgis_tiger_geocoder, address_standardizer, pg_jieba, pg_trgm, timescaledb, pg_stat_statements, pgcrypto, vector）→ app 启动自动迁移 00000001→00000081 一次通过（dirty=false）→ healthz 200 → API 层校验逻辑正常响应。

⚠️ **部署前置清单（干净部署操作者必读）**：
1. PG 库必须**先装扩展再起 app**（顺序不可反——首试迁移中断会留 dirty 记录，需 `UPDATE schema_migrations SET dirty=false` 后重试）；
2. 生产密钥必须显式提供：`IMBOY_SOLIDIFIED_KEY`(32B)/`IMBOY_SOLIDIFIED_KEY_IV`(16B)/`IMBOY_PASSWORD_SALT` 等（dev 默认链仅 IMBOYENV=local 生效；生产 fail-fast 是有意设计）；
3. 注册通道依赖验证码（短信/邮箱）——未配置前 signup 返回"验证码无效"，属预期 fail-closed。

### 外部依赖配置就绪度（ZC-12 静态核查，2026-08-29）

| 依赖 | 现态 | 对 ZC-12 的影响 |
|---|---|---|
| 邮箱验证码（SMTP） | ✅ QQ relay 已配（sys.local.config `smtp_option`） | 干净部署/注册冒烟可走邮箱验证码 |
| 短信（SMS） | ⚠️ switch=off，`default_code=6666`（测试万能码） | 测试环境注册可用 6666；生产需开通阿里云短信 |
| Push（JPush） | ❌ 未配置（local/pro 均无 jpush 段） | **§二 Push 验证受阻**——需注册极光账号并配置，或改用应用内通知替代验证 |
| 音视频（LiveKit） | ❌ pro 配置密钥为占位符（`required-at-runtime-please-configure`） | **§二 音视频验证受阻**——需真实 LiveKit/eturnal 凭据 |
| 附件（Garage/S3） | ✅ 链路存在（Admin avatar 测试有真实上传行为） | 可验证；失败则为既有环境问题（见 avatar 红灯） |
| 升级提示（r_upgrade） | 依赖服务端版本记录接口 | 发布高于本机 version 的记录即可验证 |

## 一、两台真机 Demo A/B（H2，⛔ 待真机接入）

**前置**：两台 Android/iOS 真机（不同网络更佳：一台 WiFi 一台蜂窝）；安装 `1.0.0-alpha.16` 包（构建命令：`cd imboyapp && flutter build apk --release`）；测试账号 ×2（生产对测号 15001/15002@imboy.pub 或新建）。

| # | 走查项 | 设备 A | 设备 B | 通过标准 |
|---|---|---|---|---|
| 1 | 登录 → Workspace Experience 进入 | | | 无崩溃 |
| 2 | 创建 Workspace → 创建 Project | | | 成员列表自动含 Owner |
| 3 | 邀请 B 为 Workspace 成员 → B 加入 Project | | | B 非成员直访 403 文案 |
| 4 | Project 成员页：邀请/移除/转移 Owner | | | 幂等反馈；409 场景（新 Owner 有未完成任务） |
| 5 | 里程碑：create → reach → 重复 reach | | | 单向状态；重复达成无报错 |
| 6 | 频道关联：link → 重复 link → unlink | | | 幂等反馈；personal 频道不可选 |
| 7 | 四聚合 Tab：置顶/资源/动态/相关帖 | | | 空态正常；动态无消息正文 |
| 8 | Workspace 归档后 W2 写操作 | | | 980 稳定错误文案 |
| 9 | Guest 场景：B 降为 Guest | | | 只读态；写入口隐藏 |
| 10 | C2C/C2G 消息回归（ChatShell） | | | 收发正常 |

## 二、Push / 音视频 / 附件 / 升级提示（H2）

| 项 | 步骤 | 通过标准 |
|---|---|---|
| Push | A 发消息 → B 退后台收推送 | 到达且点击跳转正确会话 |
| 音视频 | RTC 通话一轮（号 117/118 或真实账号） | 接通、音视频双向、挂断正常 |
| 附件 | 发送图片/文件 → 对端经 viewUrl 查看 | 加载成功、无原始 URL 泄露 |
| 升级提示 | 服务端发布高于本机 version 的版本记录 | 弹升级提示且不阻断使用 |

## 三、3 人 30 秒理解测试（H2 真人）

- 人员：3 名未参与项目的真实用户；
- 话术：「这是一个团队协作 IM。请只看这个项目页面 30 秒，然后回答：① 这是什么？② 你能做什么？③ 你想先点哪里？」
- 通过标准：≥2/3 人 30 秒内答对 ①②；③ 的第一直觉点与设计入口一致或可解释；
- 记录：每人原话 + 用时 + 是否需要引导（证据模板见 §六）。

## 四、干净部署（未参与实现者操作）

1. 新机器/新目录 clone 三仓（imboy + imboyapp + imboyadmin，**当前须提供产物访问方式**——见 §七 H4 前置）；
2. `cd imboy/deploy && cp .env.example .env && $EDITOR .env`（按 imboy/deploy/README.md）；
3. PG 前置：建库 + 装 §〇 的 12 个扩展；
4. `bash preflight.sh` → `docker compose -f docker-compose.prod.yml up -d`（或本机等价流程）；
5. 冒烟：healthz 200、注册一个账号（需已配置验证码通道）、登录进 Workspace；
6. 记录：耗时、卡点、文档偏差。

## 五、生产等价数据量迁移与回滚演练（H3，⛔ 待授权）

- 数据源：生产等价规模快照（**脱敏**，PII 严禁离开授权环境）；
- 步骤：快照恢复到演练库 → `make` 迁移至 alpha.70 链（00000081）→ 应用冒烟 → **回滚**（00000081.down + 应用回退）→ 复核数据一致性；
- 记录：各阶段时长、锁窗口、回滚后行数对账（迁移 81 的 down 会拒收 W2 事件行——见 down 文件头说明，属安全特性）。

## 六、证据记录模板（每项必填）

```
项目：<项名> | 设备/环境：<型号/规格> | 网络：<WiFi/蜂窝/内网>
版本：App alpha.16+6 / 后端 alpha.70 | HEAD：imboy db41789a(+产物) / app a18e89e5 / admin 46c7e10
时间：<ISO8601> | 操作人：<姓名> | 结果：PASS/FAIL | 证据：<截图/录屏/日志路径>
```

## 七、H4 Release Gate（最后人工确认）

- [ ] git author/committer 身份确认（用户本人）
- [ ] 目标远端选择（三仓 origin=gitee / github / gitcode 多远端，逐仓指定）
- [ ] 提交切片确认（ledger §6 各卡建议 message+pathspec）
- [ ] push 授权（含三仓历史 268+ 笔未 push 提交是否随行）
- [ ] tag 名与推送（v1.0.0-alpha.70 / v1.0.0-alpha.16）
- [ ] 发布渠道（制品/公告——外向操作逐项确认）

## 判定

以上全部 PASS 且无阻断缺陷 → 输出 `READY_FOR_ALPHA_RELEASE`；任一阻断缺陷或证据缺失 → `BLOCKED` 并列精确缺口。
