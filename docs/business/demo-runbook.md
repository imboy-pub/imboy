# IMBoy 5 分钟现场演示脚本

> **类型**：操作手册 · **读者**：售前/创始人 · **版本**：v1.0 · **最后更新**：2026-07-27
> 目标受众：企业 IT 决策者、安全负责人、CTO
> 核心信息：**数据在你手里，密钥在用户手里，我们什么都看不到。**

---

## 演示前准备（提前 10 分钟）

### 环境启动

```bash
# 方案 A：干净 Docker 环境（推荐，客户面前用）
cd deploy
docker compose -f docker-compose.demo.yml up -d
# 等待 ~30s，确认后端健康：
curl -s http://127.0.0.1:9800/api/v1/init | python3 -m json.tool

# 方案 B：已有本地开发环境
curl -s http://127.0.0.1:9800/api/v1/init
# 看到 {"code":0, "msg":"success.", ...} 即可
```

### 准备清单

- [ ] 终端窗口（后端 API 演示用）
- [ ] 手机 × 2（装好 IMBoy App，连同一 WiFi）
- [ ] 浏览器打开管理后台（`http://<IP>:8080` 或本地 `bun run dev`）
- [ ] 浏览器打开文档站（https://imboy-pub.github.io/imboy/）
- [ ] 备用：录屏（万一现场网络出问题）

### IP 配置同步（演示失败的第一大原因）

演示前 **必须** 确认以下两处 IP 一致且为当前局域网 IP：

```bash
# 查看当前 IP
ifconfig | grep "inet " | grep -v 127.0.0.1
# 假设输出 192.168.0.24
```

**① 后端 `config/sys.local.config`**（WebSocket 地址）：

```erlang
{ws_url, <<"ws://192.168.0.24:9800/api/v1/ws">>},
```

修改后重启后端：`IMBOYENV=local make run`

**② 客户端 `imboyapp/.env.local`**（HTTP API 地址）：

```
API_BASE_URL=http://192.168.0.24:9800
```

修改后重新运行 App：`flutter run --dart-define=APP_ENV=local`

**③ 手机与电脑在同一 WiFi / 子网**，手机 App 设置中确认服务器地址为上述 IP。

> ⚠️ 切换网络（如从办公室到客户现场）后 IP 会变，必须重新执行上述步骤。

---

## 演示流程（5 分钟）

### 第一幕：30 秒 — 开场定位

> **说**：「IMBoy 是一个企业私有化即时通讯平台。和钉钉、飞书最大的区别是——
> 部署在你自己的服务器上，数据库是你的，密钥在用户手机上，
> 我们的代码里没有任何解密函数。这不是承诺，是 CI 门禁强制保证的。」

**动作**：打开文档站 → E2EE 协议规范 → §11 Server Zero-Knowledge Invariant

### 第二幕：60 秒 — 注册与登录

> **说**：「我们看最基础的用户流程。」

**动作**（手机 A）：

1. 打开 App → 注册 → 输入邮箱 `alice@demo.company`
2. 验证码（本地环境固定 `6666`，Docker demo 环境查日志）
3. 设置密码 → 注册成功 → 自动登录

**动作**（手机 B）：同样注册 `bob@demo.company`

> **说**：「注意，注册信息直接写入你机房里的 PostgreSQL，
> 不经过任何第三方。密码用 PBKDF2-SHA256 三十一次迭代存储，
> 即使数据库被拖库，暴力破解成本极高。」

### 第三幕：90 秒 — E2EE 消息收发（核心卖点）

> **说**：「现在 Alice 给 Bob 发一条端到端加密消息。」

**动作**（手机 A）：

1. 添加 Bob 为好友
2. 打开对话 → 输入「下季度预算已批准，请安排采购」
3. 发送

**动作**（手机 B）：收到消息，正常显示

**动作**（终端）：

```bash
# 展示数据库中存储的是密文
docker exec imboy_demo_pg18 psql -U imboy_user -d imboy_demo \
  -c "SELECT payload, e2ee FROM message ORDER BY created_at DESC LIMIT 1;"
```

> **说**：「看数据库里存的内容——payload 是密文，e2ee 字段是密钥信封。
> 服务器只做路由转发，从头到尾不碰明文。
> 即使有人拿到数据库备份，也解不出任何一个字。」

**加分项**（如果时间允许）：

```bash
# 一键跑 265 项后端安全测试
cd imboy && make e2ee-verify
# === E2EE verify ALL PASSED ===
```

> **说**：「这不是 PPT 上的'我们很安全'。265 项自动化测试，
> 覆盖密钥协商、棘轮前向保密、降级防护、审计日志完整性——
> 每次代码提交都跑，任何一项失败，合并被阻断。」

### 第四幕：60 秒 — 管理后台

> **说**：「管理员视角。」

**动作**（浏览器）：

1. 打开管理后台 → 登录（首启向导创建的超管账号）
2. 用户管理 → 展示刚注册的两个用户
3. 系统设置 → 展示 E2EE 模式开关（disabled/optional/required/compliance）

> **说**：「合规模式下，所有消息强制加密，同时支持合规公钥双包装——
> 满足金融、政务场景的监管审计需求，但审计方拿到的是
> 经用户设备授权才能解密的包装密钥，不是明文。」

### 第五幕：30 秒 — 部署与收尾

> **说**：「部署只需要一条命令。」

**动作**（终端）：

```bash
# 展示生产级 compose（12 个服务，含监控）
cat deploy/docker-compose.prod.yml | head -20
```

> **说**：「Docker Compose 一键起：PostgreSQL、后端、管理后台、
> Nginx TLS、Prometheus 监控、Grafana 仪表盘——全部在你自己的机器上。
> 也支持 Helm Chart 部署到 K8s。
> 社区版免费，100 用户以内不限量。需要更多，聊 License。」

**收尾**：递上安全简报（`docs/business/e2ee-security-brief.md` 的 PDF 打印版）

---

## 常见提问应对

| 问题 | 回答要点 |
|------|---------|
| 「和钉钉/飞书比有什么优势？」 | 数据主权 + E2EE + 无 SaaS 依赖。他们做不到服务端零知识——商业模式不允许。 |
| 「性能怎么样？」 | Erlang/OTP 原生并发，单节点 10 万+ WebSocket 连接。可集群水平扩展。 |
| 「支持哪些平台？」 | iOS / Android / macOS（Flutter），管理后台 Web（React），JS SDK 供第三方集成。 |
| 「加密算法是什么？」 | X3DH 密钥协商 + Olm/Megolm 双棘轮（vodozemac 实现，与 Signal/Matrix 同源），AES-256-GCM。 |
| 「怎么证明你们看不到？」 | CI 脚本 `check_server_zero_crypto.sh` grep 全部后端代码，发现解密函数调用即构建失败。开源可审计。 |
|「私有化部署需要什么配置？」 | 4C8G 起步（100 人），Docker 环境，一个域名。10 分钟部署完成。 |
|「数据迁移怎么做？」 | PostgreSQL 标准备份恢复 + Garage S3 rsync。提供迁移脚本和文档。 |

---

## 演示后跟进

1. 当天发送：安全简报 PDF + 文档站链接 + 试用 License（50 用户 / 30 天）
2. 第 3 天：跟进邮件「是否需要协助部署到贵司环境？」
3. 第 7 天：提供技术支持通道（微信群 / 邮件）

---

## 附录：演示环境重置

```bash
# 完全重置（清除所有演示数据）
cd deploy
docker compose -f docker-compose.demo.yml down -v
docker compose -f docker-compose.demo.yml up -d

# 重置后首次访问管理后台会触发 /setup 向导
```
