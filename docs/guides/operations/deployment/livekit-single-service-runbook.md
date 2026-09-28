# LiveKit 单服务运行手册（rtc WSS + SFU + embedded TURN）

> 面向读者：**不熟悉 LiveKit 的运维**。本手册逐行给出命令、所在机器/目录、预期输出与失败停止点，照抄即可完成部署、验证、升级、续期与回滚。
>
> 适用版本：imboy 集成分支（LiveKit `livekit/livekit-server:v1.13.7`，精确 tag 锁定，禁止 `latest`/浮动 tag）。
>
> 本文描述的是 **imboy 仓库内已实现的部署形态**（`deploy/` 目录）。本文是手册不是部署证明：生产切换（W4/W5）由运维按 迁移计划(docs/plans/2026-09-21-livekit-single-service-migration-deployment-plan-v1.md,存于项目工作区) 执行并另行留存证据。
>
> 相关文档：[deployment.md](./deployment.md)（完整部署参考）｜[day1-quickstart.md](./day1-quickstart.md)（5 分钟上手）｜[production-architecture.md](./production-architecture.md)（生产架构快照）

---

## 运维处置卡片（owner / 止损 / 回滚）· W3-A04

| 要素 | 值 |
|---|---|
| **Owner** | IMBoy Ops（待用户指名，指名后替换本行） |
| **Escalation** | ① IMBoy Ops 值班 → ② 待指名平台负责人（IM/电话占位）→ ③ LiveKit 层故障查 LiveKit 官方 GitHub/社区（版本 tag 已锁定，附版本号报障） |

### 止损线（量化，触发即回滚）

- **升级 LiveKit 版本（§7）后 15 分钟观察窗**内：通话建立失败/超时明显上升
  （内测群 ≥2 人报告进房失败，或 rtc WSS 握手 5xx）→ 立即把镜像 tag 切回
  上一个锁定版本并 `docker compose up -d imboy_livekit`。
- **开启 TURN overlay（§5）后**：UDP 媒体端口（50000-50200）不通或 ICE
  失败率上升 → `LIVEKIT_TURN_ENABLED=false` 降回纯公网 IP 直连形态
  （媒体直连不受影响，仅弱网/NAT 用户体验降级）。
- **通用止血原则**：通话类故障优先保"能通"——先回形态（§8 eturnal 退路），
  再排障，不在故障态上调参。

### 回滚

- LiveKit 版本回退：§7 末尾（改回旧 tag 重建容器）。
- TURN 层退路：§8 回滚到旧 eturnal（共存期 W4 方案）。
- 整体停用：§9 停止与卸载（保留数据目录，便于复盘后重拉）。

---

## 0. 一句话理解拓扑

IMBoy 的全部通话（1:1 与群通话）都走 **一个 LiveKit 容器**（服务名 `imboy_livekit`）：

```text
App 客户端
  ├── wss://rtc.imboy.pub            WebSocket 信令（443，TLS 由 nginx/OpenResty 终结，反代 127.0.0.1:7880）
  ├── 106.53.76.53:50000-50200/udp   WebRTC 媒体（ICE/UDP，直连容器）
  ├── 106.53.76.53:7881/tcp          ICE/TCP 兜底（UDP 被限时）
  ├── turn.imboy.pub:3478/udp        LiveKit embedded TURN/STUN（W5 终态，需开启 overlay）
  └── turn.imboy.pub:5349/tcp        LiveKit embedded TURN/TLS（W5 终态，需开启 overlay）
```

- **没有** eturnal、**没有** coturn、**没有** Redis、**没有** Egress/录制 —— 这些都不是本形态的依赖。
- 后端只做一件事：`POST /api/v1/rtc/room/join` 校验好友/群成员资格后签发 LiveKit JWT（TTL 600 秒），返回 `ws_url = wss://rtc.imboy.pub`。
- 旧的 `/api/v1/user/credential`（eturnal TURN 凭证）端点已删除，客户端不再调用。

### 端口合同（冻结，引用 LK-01 合同）

| 端口/段 | 协议 | 共存期归属（W4，旧 eturnal 未删） | 终态归属（W5，eturnal 已删） |
|---|---|---|---|
| 443 | TCP | nginx（含 `wss://rtc.imboy.pub` 反代） | 不变 |
| 7880 | TCP | LiveKit 信令，**仅绑定 127.0.0.1** | 不变 |
| 7881 | TCP | LiveKit ICE/TCP | 不变 |
| 50000-50200 | UDP | LiveKit 媒体段（`rtc.port_range`） | 不变 |
| 50201-50500 | UDP | 旧 eturnal 有效 relay 段 | **LiveKit TURN relay**（`turn.relay_range`） |
| 3478 | UDP+TCP | **旧 eturnal**（LiveKit 禁止占用，不得双活） | LiveKit `turn.udp_port`（UDP；TCP 空置） |
| 5349 | TCP | **旧 eturnal**（TLS） | LiveKit `turn.tls_port`（TLS） |

> 共存期说明：旧 eturnal 配置的 relay 段是 50000-50500，但 50000-50200 已被 LiveKit 的 docker-proxy 先绑定，eturnal 实际只能用 50201-50500，物理不冲突（已长期共存验证）。

### TURN/TLS@443 现状（BLOCKED_TURN_443，如实声明）

`turn.imboy.pub:443/TCP` 上的 TURN/TLS **不部署**：443 只归 nginx，LiveKit 的 TURN/TLS 固定监听 **5349**。在「只放行 443」的严格企业网络里，客户端回退链为：

```text
UDP 媒体段 50000-50200  →  ICE/TCP 7881  →  TURN/TLS 5349
```

若 5349 也不可达，通话退到 7881 ICE/TCP —— 这是冻结时点已接受的边界，不是缺陷。解除该限制的条件（三选一，任一成立才可修订端口合同）：① 有了第二个公网 IP；② 书面授权 443 四层 SNI 分流改造；③ 书面接受「仅 443 网络无 TURN/TLS」的降级。

### 容量与运维边界（如实声明）

| 维度 | 值 |
|---|---|
| 部署形态 | 单节点、无 Redis（standalone 模式） |
| 重启影响 | **LiveKit 重启 = 全部进行中通话立即中断**（房间状态在进程内存），客户端 SDK 会自动重连恢复 |
| 并发 participant 硬上限 | 约 100（媒体段 201 个 UDP 端口 ÷ 每人 2 端口） |
| 运营建议上限 | **50**（为视频转发 CPU 与共存服务留余量；超过先观测再扩容） |
| 容器内存限额 | 768M（`LIVEKIT_MEM_LIMIT` 可调 512M-768M） |
| TURN relay 容量 | 段 300 端口，每用户 allocation 配额 12（默认） |

---

## 1. 前置条件检查

**机器**：部署目标 Linux 服务器（本文示例为 Ubuntu 22.04+，Docker Compose v2.23.1+）。
**目录**：任意有写权限的目录，下文统一用 `/opt/imboy-stack`。

```bash
# [机器: 部署机] [目录: ~] 检查 Docker 与 Compose 版本
docker --version
docker compose version
# 预期输出: Docker version 24+ / Docker Compose version v2.23.1+
# 失败停止点: Compose < v2.23.1 时 garage 内联配置不生效，先升级 Docker（get.docker.com 脚本安装的版本均满足）
```

```bash
# [机器: 部署机] [目录: ~] 检查端口占用（以 root 或 sudo）
sudo ss -lntup | grep -E ':(80|443|7880|7881|5432|9800)\b' || echo "关键端口空闲"
sudo ss -lnup | grep -E ':(3478|5349)\b' || echo "TURN 端口空闲"
# 预期输出: 若 3478/5349 有监听且进程为旧 eturnal（beam.smp），说明处于共存期（W4）——
#           此时必须保持 LIVEKIT_TURN_ENABLED=false（见 §5），不要试图同时开两套 TURN。
```

**DNS（人工步骤，在域名服务商控制台完成）**：两条 A 记录都指向本机公网 IP——

| 域名 | 用途 |
|---|---|
| `rtc.example.com`（下文称 RTC_DOMAIN） | WSS 信令域，nginx 443 反代 7880 |
| `turn.example.com`（下文称 TURN_DOMAIN） | TURN 域；80 端口仅用于 ACME 证书签发，TLS 5349 由 LiveKit 直接监听 |

```bash
# [机器: 部署机] [目录: ~] 验证 DNS 已生效
getent hosts rtc.example.com turn.example.com
# 预期输出: 两行，均解析为本机公网 IP
# 失败停止点: DNS 未生效时 certbot 签发必然失败，先等解析传播（通常几分钟到 1 小时）
```

**云安全组（人工步骤，在云厂商控制台完成）**：放行 `7881/tcp`、`50000-50500/udp`；开启 TURN overlay 后另需 `3478/udp`、`5349/tcp`。服务器本机 ufw 放行不等于云侧放行，两侧都要核对。

---

## 2. 形态 A：Docker Compose 社区栈部署（推荐新部署者）

适用：全新机器，用仓库自带的 `deploy/install.sh` 一键部署（LiveKit 已是社区栈第 7 个核心服务）。

### 2.1 获取部署包

```bash
# [机器: 部署机] [目录: /opt] 克隆仓库（或从发布包解压）
cd /opt && git clone https://github.com/imboy-pub/imboy.git imboy-stack
cd /opt/imboy-stack/deploy
# 预期输出: 目录内可见 install.sh / preflight.sh / docker-compose.community.yml / .env.example / nginx/
```

### 2.2 生成并填写 .env

```bash
# [机器: 部署机] [目录: /opt/imboy-stack/deploy] 生成 600 权限的 .env 模板并自动补齐密钥
bash install.sh --edition community
# 预期输出: 首次运行会生成 .env（自动填好随机密钥，包括 LIVEKIT_API_KEY / LIVEKIT_API_SECRET），
#           然后提示缺少人工变量并退出 —— 这是设计行为（不猜测域名/邮箱）。
```

```bash
# [机器: 部署机] [目录: /opt/imboy-stack/deploy] 人工填写 6 个必填项
# 用任意编辑器打开 .env，修改以下变量（示例域名请换成自己的）：
#   API_DOMAIN=api.example.com
#   ADMIN_DOMAIN=admin.example.com
#   CS_WIDGET_DOMAIN=cs.example.com
#   RTC_DOMAIN=rtc.example.com        # LiveKit 信令域（本手册 §1 的 DNS）
#   TURN_DOMAIN=turn.example.com      # LiveKit TURN 域
#   CERTBOT_EMAIL=ops@example.com     # 证书到期通知邮箱（必须人工填写，脚本不会猜）
${EDITOR:-vi} .env
```

> 保持 `LIVEKIT_TURN_ENABLED=false`（默认）。全新机器没有旧 eturnal，也可以在证书签发完成后直接按 §5 开启 TURN overlay。

### 2.3 前置检查 + 启动

```bash
# [机器: 部署机] [目录: /opt/imboy-stack/deploy] 前置检查
bash preflight.sh --docker
# 预期输出: 全部 ok。任何 err 都要先解决（域名格式、端口占用、密钥长度等），
#           preflight 是 fail-closed 的：有 err 时直接退出非零。
# 失败停止点: 按 err 提示修正 .env 后重跑，直到无 err。
```

```bash
# [机器: 部署机] [目录: /opt/imboy-stack/deploy] 启动社区栈（含 imboy_livekit）
docker network create imboy-network 2>/dev/null || true
docker compose -f docker-compose.community.yml up -d
# 预期输出: 各容器 Started，imboy_livekit 处于 running。
docker ps --format 'table {{.Names}}\t{{.Image}}\t{{.Status}}' | grep livekit
# 预期输出: imboy_livekit  livekit/livekit-server:v1.13.7  Up ...
# 失败停止点: 容器反复重启时先看日志: docker logs imboy_livekit --tail 50
```

### 2.4 首次签发 TLS 证书（两域必签）

```bash
# [机器: 部署机] [目录: /opt/imboy-stack/deploy] 一次性签发全部域名（含 RTC_DOMAIN 与 TURN_DOMAIN）
bash nginx/init-letsencrypt.sh
# 预期输出: 逐域签发，最后输出 "TLS 证书签发完成: ..."，其中包含 rtc 与 turn 两域。
# 失败停止点: 常见原因是 DNS 未生效或 80 端口被占 —— 回到 §1 检查后再重跑（脚本幂等）。
```

### 2.5 验证

```bash
# [机器: 部署机] [目录: 任意] 验证 LiveKit 信令经 nginx 可达（应返回 404/400 而非 502/超时）
curl -sS -o /dev/null -w '%{http_code}\n' https://rtc.example.com/
# 预期输出: 404（根路径无页面是正常的；502 = 反代失败，检查 imboy_livekit 是否 running）
```

```bash
# [机器: 部署机] [目录: 任意] 验证媒体端口已发布
sudo ss -lnup | grep -E ':(50000|50200)\b' && sudo ss -lntp | grep ':7881\b'
# 预期输出: docker-proxy 监听 50000-50200/udp 与 7881/tcp。
```

```bash
# [机器: 部署机] [目录: /opt/imboy-stack] 端到端冒烟（真实信令 + token + 房间）
bash scripts/rtc_e2e_test.sh
# 预期输出: group/c2c 合同与 TURN 路径断言全部 PASS。
# 失败停止点: 按脚本输出的第一个 FAIL 项排查（后端未起 / LIVEKIT_API_SECRET 不匹配最常见）。
```

---

## 3. 形态 B：宿主机 nginx/OpenResty 生产机部署

适用：生产机（宝塔 OpenResty 布局，参考快照 `deploy/nginx/prod-vhosts/`）。该形态下 443 由宿主机 OpenResty 承载，LiveKit 容器照常由 compose 启动（或按现有生产方式运行）。

### 3.1 部署 rtc 域 vhost（先签发、后启用 443）

```bash
# [机器: 生产机] [目录: 仓库的 deploy/nginx/prod-vhosts/] 将 rtc vhost 复制到 nginx 配置目录
# ⚠️ 第一步先部署「纯 80 形态」：暂时注释掉文件里的 443 server 块（证书还不存在，
#    直接放开会让 nginx -t 失败）。文件头注释有同样的操作说明。
cp rtc.imboy.pub.conf /www/server/panel/vhost/nginx/rtc.example.com.conf   # 域名换成自己的
# 用编辑器: 1) 把 server_name 改成自己的 RTC_DOMAIN; 2) 注释掉 listen 443 的 server 块
nginx -t && nginx -s reload
# 预期输出: syntax is ok / test is successful
```

```bash
# [机器: 生产机] [目录: 任意] 签发 rtc 域证书（webroot 模式）
certbot certonly --webroot -w /var/www/certbot -d rtc.example.com
# 预期输出: Successfully received certificate ...（证书落在 /etc/letsencrypt/live/rtc.example.com/）
# 失败停止点: 报 DNS/超时 → 回 §1 查 DNS；报 webroot 404 → 确认 vhost 的
#             /.well-known/acme-challenge/ location 指向 /var/www/certbot。
```

```bash
# [机器: 生产机] [目录: /www/server/panel/vhost/nginx/] 放开 443 server 块（反代 127.0.0.1:7880）
# 编辑 rtc.example.com.conf: 取消注释 443 块，证书路径改为
#   /etc/letsencrypt/live/rtc.example.com/fullchain.pem 与 privkey.pem
nginx -t && nginx -s reload
# 预期输出: test is successful
# 验证: curl -sS -o /dev/null -w '%{http_code}\n' https://rtc.example.com/   → 404 即通
```

### 3.2 部署 turn 域 vhost（纯 80，仅用于证书）

turn 域 **没有 443**（TURN/TLS 5349 由 LiveKit 直接监听，见 §0 的 BLOCKED_TURN_443 说明）：

```bash
# [机器: 生产机] [目录: 仓库的 deploy/nginx/prod-vhosts/] 复制 turn vhost（纯 80 ACME webroot）
cp turn.imboy.pub.conf /www/server/panel/vhost/nginx/turn.example.com.conf   # 域名换成自己的
nginx -t && nginx -s reload
certbot certonly --webroot -w /var/www/certbot -d turn.example.com
# 预期输出: 证书落在 /etc/letsencrypt/live/turn.example.com/
```

### 3.3 安装 TURN 证书续期分发 hook（W5 前必须完成）

TURN 证书不由 nginx 使用，续期后必须原子分发到 `/etc/imboy/livekit-certs/` 并重启 LiveKit：

```bash
# [机器: 生产机] [目录: 仓库的 deploy/nginx/] 安装 hook
install -m 0755 livekit-turn-cert-deploy-hook.sh \
  /etc/letsencrypt/renewal-hooks/deploy/livekit-turn-cert.sh
# 预期输出: 无输出即成功。hook 只处理 TURN_DOMAIN 的续期，其它域名续期会被它直接跳过。
```

```bash
# [机器: 生产机] [目录: 任意] 手工演练一次（不触发真实续期，不动生产证书目录也可）
TURN_DOMAIN=turn.example.com LIVEKIT_TURN_CERT_TARGET=/tmp/lk-certs \
  bash /etc/letsencrypt/renewal-hooks/deploy/livekit-turn-cert.sh \
  /etc/letsencrypt/live/turn.example.com
# 预期输出: 分发成功日志；/tmp/lk-certs/ 下出现 fullchain.pem 与 privkey.pem。
# ⚠️ 注意: hook 会 docker restart imboy_livekit（容器在跑时）—— 即全部通话中断，
#          演练请选低峰期。
```

---

## 4. 验证 embedded TURN（开启 overlay 后）

前提：已完成 §5 开启（`LIVEKIT_TURN_ENABLED=true`）。

```bash
# [机器: 部署机] [目录: 任意] 端口归属验证
sudo ss -lnup | grep ':3478\b'      # 预期: docker-proxy（LiveKit）
sudo ss -lntp | grep ':5349\b'      # 预期: docker-proxy（LiveKit）
sudo ss -lnup | grep -E ':(50201|50500)\b'   # 预期: relay 段监听
```

```bash
# [机器: 部署机] [目录: 任意] TURN 真实 relay 验证（端口可达不算数，必须有 relay 证据）
docker logs imboy_livekit 2>&1 | grep -i 'turn' | tail -20
# 预期输出: 出现 TURN allocation 相关日志；客户端侧 ICE candidate 的 selected 候选应为
#           relay 类型。完整自动化验证跑 scripts/rtc_e2e_test.sh（其 TURN 断言即按此标准）。
```

---

## 5. 开启 TURN overlay（LIVEKIT_TURN_ENABLED）

> **前置条件（缺一不可，preflight 会逐项 fail-closed 检查）**：
> 1. 旧 TURN（eturnal/coturn）已停止并删除，`ss -lntup` 确认 3478/5349 **无残留监听**（不得双活：两套 TURN 同抢 3478 会 bind 冲突、容器崩溃）；
> 2. TURN_DOMAIN 证书已签发且就绪（overlay 挂载目录下有 `fullchain.pem`/`privkey.pem`）；
> 3. 云安全组已放行 `3478/udp`、`5349/tcp`、`50201-50500/udp`。

```bash
# [机器: 部署机] [目录: deploy/] 第一步：确认旧 TURN 无残留（共存期看到 eturnal 监听 = 停，勿继续）
sudo ss -lntup | grep -E ':(3478|5349)\b' || echo "3478/5349 空闲，可以开启"
```

```bash
# [机器: 部署机] [目录: deploy/] 第二步：.env 打开开关（编辑器修改）
#   LIVEKIT_TURN_ENABLED=true
${EDITOR:-vi} .env
```

```bash
# [机器: 部署机] [目录: deploy/] 第三步：重跑安装器（幂等收敛，自动叠加 overlay 并展开证书目录）
bash install.sh --edition community
# 预期输出: 安装摘要中 LiveKit TURN 显示 turn(s):<TURN_DOMAIN>:3478|5349
# 失败停止点: preflight 阶段任何 err（端口被占 / 证书缺失 / 开关值非法）都会拒绝继续，按提示修复。
```

```bash
# [机器: 部署机] [目录: 任意] 第四步：按 §4 验证端口与 relay。
```

---

## 6. 证书续期（两域）

### 形态 A（compose certbot 容器）

- certbot 容器每 12h 自动 `certbot renew`，nginx 容器每 6h 自动 reload —— 无需人工干预。
- rtc 域证书只被 nginx 使用（7880 无 TLS），续期后 reload 即生效。
- turn 域证书被 LiveKit 挂载使用：compose 形态下挂载的就是 certbot live 目录，**容器需要重启才加载新证书**（见 §7 的中断说明；低峰操作）。

```bash
# [机器: 部署机] [目录: 任意] 验证续期机制（不真正续期）
docker exec imboy_certbot certbot renew --dry-run
# 预期输出: The dry run was successful（所有域）
```

### 形态 B（宿主机 certbot）

```bash
# [机器: 生产机] [目录: 任意] 验证续期 + hook 链路
certbot renew --dry-run
systemctl list-timers | grep -Ei 'certbot|acme'   # 确认定时器存在
ls -l /etc/letsencrypt/renewal-hooks/deploy/livekit-turn-cert.sh   # 确认 hook 已装（§3.3）
# 预期输出: dry run successful；timer 在列；hook 文件存在且可执行。
```

---

## 7. 升级 LiveKit 版本

```bash
# [机器: 部署机] [目录: deploy/] 修改 docker-compose.community.yml 中 imboy_livekit 的 image tag
#   （从 v1.13.7 改为目标版本；只允许精确 tag，禁止 latest/v1.13 浮动写法）
${EDITOR:-vi} docker-compose.community.yml
docker compose -f docker-compose.community.yml up -d imboy_livekit
# 预期输出: 容器以新镜像重建并 running。
docker logs imboy_livekit --tail 20   # 确认无 panic/配置错误
```

> ⚠️ **升级即重启 = 全部通话中断**（单节点、房间状态在内存）。客户端会自动重连，但请选低峰窗口。
> 开启了 TURN overlay 的部署，注意 `docker-compose.livekit-turn.yml` 的 `LIVEKIT_CONFIG` 与基础文件是**整体覆盖**关系：改基础配置必须同步改 overlay（overlay 文件头有同样警示）。

版本回滚（回到 v1.13.7）：把 image tag 改回 `livekit/livekit-server:v1.13.7` 再 `up -d imboy_livekit` 即可，无需额外操作。

---

## 8. 回滚到旧 eturnal（TURN 层退路）

适用场景：W5 切换到 LiveKit embedded TURN 后发现严重问题，需要临时回到旧 eturnal（仅在旧 eturnal 尚未被删除、或留有完整备份时可执行）。

> 前提：旧 eturnal 以 systemd 服务运行（unit 名 `eturnal`，安装于 `/opt/eturnal`，配置 `/etc/eturnal/eturnal.yaml`，证书 `/etc/eturnal/tls/`）。若已按删除流程 purge，需先用备份恢复 `/opt/eturnal`、`/etc/eturnal` 与 systemd unit。

```bash
# [机器: 生产机] [目录: deploy/] 第一步：关掉 LiveKit TURN（先让路，避免 3478/5349 争抢）
# 编辑 .env: LIVEKIT_TURN_ENABLED=false，然后重跑安装器收敛
${EDITOR:-vi} .env
bash install.sh --edition community
# 预期输出: 安装摘要中 LiveKit TURN 显示「未启用」；3478/5349 不再被 docker-proxy 监听。
sudo ss -lntup | grep -E ':(3478|5349)\b' || echo "3478/5349 已空闲"
```

```bash
# [机器: 生产机] [目录: 任意] 第二步：恢复并启动旧 eturnal
sudo systemctl unmask eturnal 2>/dev/null || true
sudo systemctl enable --now eturnal
sudo systemctl status eturnal --no-pager | head -5
# 预期输出: active (running)，且 ss 显示 3478/5349 回到 beam.smp（eturnal）名下。
```

```bash
# [机器: 生产机] [目录: 任意] 第三步：验证客户端 TURN 恢复
# 旧客户端走 eturnal 需要后端恢复 /api/v1/user/credential —— 该端点已删除，
# 因此回滚仅对「仍内置旧 TURN 地址的存量客户端版本」有效；新版本客户端
# 的 fallback 链（UDP → 7881 → 无 TURN）不受影响、仍可通话（可能无中继）。
# 结论：回滚 eturnal 只是媒体面兜底，不是完整功能回滚 —— 重大故障优先考虑
#       回滚整个后端版本（见 deployment.md 回滚流程）。
```

---

## 9. 停止与卸载 LiveKit

```bash
# [机器: 部署机] [目录: deploy/] 停止（通话立即中断）
docker compose -f docker-compose.community.yml stop imboy_livekit
# 开启了 TURN overlay 时:
# docker compose -f docker-compose.community.yml -f docker-compose.livekit-turn.yml stop imboy_livekit

# 彻底移除（含 overlay 与 hook）
docker compose -f docker-compose.community.yml rm -f imboy_livekit
sudo rm -f /etc/letsencrypt/renewal-hooks/deploy/livekit-turn-cert.sh   # 形态 B 的 hook
sudo rm -rf /etc/imboy/livekit-certs                                    # TURN 证书分发目录
```

> 卸载后端侧依赖同步清理：`.env` 中 `LIVEKIT_API_KEY/LIVEKIT_API_SECRET` 置空会导致后端 join 返回 `livekit_not_configured` 受控错误（不会崩 500）；按需保留或清理。

---

## 10. 故障排查速查

| 现象 | 可能原因 | 处理 |
|---|---|---|
| `docker logs imboy_livekit` 反复重启 | 7881/媒体端口被占，或 keys 缺失 | `ss -lntup` 查占用；确认 `.env` 的 `LIVEKIT_API_KEY/SECRET` 非空且 secret ≥32 字符 |
| `https://rtc.example.com` 返回 502 | imboy_livekit 未运行 / nginx upstream 错 | 确认容器 running 且 vhost 反代 `127.0.0.1:7880`；`nginx -t` 后 reload |
| WSS 握手失败（App 连不上信令） | nginx 缺 `Upgrade/Connection` 头或超时过短 | 对照 `deploy/nginx/prod-vhosts/rtc.imboy.pub.conf` 的 location 写法（3600s 超时 + buffering off） |
| 客户端入会报 `livekit_not_configured` | 后端三键（ws_url/api_key/api_secret）任一为空 | 检查后端 env（compose 形态为 `IMBOY_LIVEKIT_*`）后重启后端 |
| 客户端入会报「不是好友/不是群成员」 | 业务 ACL 拒绝 | 属正常防线；核对好友/群成员关系 |
| 开启 TURN 后容器起不来 | 3478/5349 仍被旧 eturnal 占用（双活禁止） | 回 §5 前置条件；先退场旧 TURN |
| TURN/TLS 连不上 | 云安全组未放行 5349/tcp，或证书未挂载 | 控制台放行；确认 `LIVEKIT_TURN_CERT_DIR` 下有 fullchain/privkey |
| 通话质量差/掉线 | 达到容量上限（>50 并发） | 查容器内存/CPU 与 participant 数；超 50 属超合同运营，先扩容再排查 |
| 续期后 TURN/TLS 报证书错误 | 证书更新但容器未重启 | 形态 A：低峰 `docker restart imboy_livekit`；形态 B：确认 §3.3 hook 已安装 |

---

## 11. 监控要点

```bash
# [机器: 部署机] [目录: 任意] 日常巡检
docker stats --no-stream imboy_livekit          # 内存应 < 768M 限额
docker logs imboy_livekit --since 1h 2>&1 | grep -ciE 'error|panic' || true   # 错误计数
sudo ss -lntup | grep -cE ':(7881|3478|5349)\b' # 监听端口数（按形态为 1-3）
```

监控栈（`--profile monitoring`）的告警规则与面板见 [monitoring.md](./monitoring.md)。
