# LiveKit TURN/TLS 443 一键切换手册

本手册用于一台公网 IP 上已经由宿主机 Nginx/OpenResty 占用 `443/TCP` 的部署。目标是
在不购买新 EIP 的前提下，让普通 HTTPS 与 `turn.<域名>:443` 共用入口：Nginx stream
按 TLS SNI 分流，HAProxy 只在回环地址把 PROXY v1 转成 LiveKit 支持的 PROXY v2。

```text
公网 :443
  -> Nginx stream ssl_preread
     -> 普通 HTTPS -> 127.0.0.1:10443 (Nginx HTTP TLS vhost)
     -> TURN SNI   -> 127.0.0.1:15442 (HAProxy, PROXY v1 -> v2)
                    -> 127.0.0.1:15443 (LiveKit embedded TURN/TLS)
```

脚本只处理这次 L4 SNI/TURN 切换。社区版整栈仍由 `deploy/install.sh` 安装。脚本会停止
并禁用 eturnal，但**不会卸载或删除 eturnal/coturn**；回滚时可恢复原 eturnal 状态。

> **候选实现状态（2026-10-02 采样）**：本手册描述的安装器行为（第三章）、备份与
> 回滚合同（第七章）以及巡检/监控链路（第八章）均为**候选实现/待上线**——服务器
> 尚未部署本候选。2026-10-01 服务器曾按同类思路做过一轮**安装器外手动 SNI 实施**
> （证书、vhost、eturnal 停用及 livekit.env/overlay 变更；此为历史采样陈述，待
> 服务器授权后重新采样绑定时间、镜像 digest 与配置摘要，不当作当前已验证状态）。
> 若当前生产仍处于该安装器外手动启用状态：
>
> - `--apply` 会被重复 apply 保护拒绝（`L4 SNI appears already active outside
>   this installer; refusing a duplicate apply`），**手动模式不能通过补跑 apply
>   让安装器幂等接管**；如需迁移到安装器管理，属独立接管决策任务，须先完成
>   preview、备份格式与恢复语义设计并单独获授权；
> - 手动现场**没有**安装器 format=2 备份，`--rollback` 对其不可用；
> - 第八章巡检/监控同样未上线，`drift=0` 一类指标当前在服务器上并不存在。

## 一、前置条件

1. Debian/Ubuntu，root 或 sudo 权限。Nginx 由 systemd 或宝塔面板管理均可，但
   安装器会钉定唯一 nginx 实例（见第三章"安装器行为要点"），无法唯一确定时以
   `BLOCKED_ENV` 拒绝，不会猜测继续。
2. 宿主机 Nginx 已运行，编译时包含 `stream` 和 `stream_ssl_preread` 模块。可用
   `NGINX_BIN` 显式指定二进制；显式指定的路径不可执行时安装器直接失败，不会
   悄悄回落到其他 nginx。
3. Docker、Docker Compose、OpenSSL、curl、Python 3、`ss` 已安装。
4. LiveKit 容器已运行，信令健康地址可访问。
5. `turn.<域名>` 已解析到本机；证书目录包含匹配的 `fullchain.pem` 和 `privkey.pem`。
6. 云安全组和主机防火墙已放行 `443/TCP`、`3478/UDP`、`50201-50500/UDP`。
7. 把所有当前监听 443 的 HTTPS vhost 和它们切换前的预期状态码写入配置。

不要把 LiveKit API secret、Cookie 或私钥写入本手册或 L4 配置。脚本只读取现有
Compose env 文件，执行过程不会打印其内容。

## 二、准备配置

在仓库目录执行：

```bash
cd /path/to/imboy
sudo install -m 600 deploy/livekit-l4-sni.env.example /etc/imboy/livekit-l4-sni.env
sudo editor /etc/imboy/livekit-l4-sni.env
```

逐项检查域名、Nginx vhost 路径、Compose 文件、证书目录和
`HTTPS_HEALTH_CONTRACTS`。已有的 `404` 或 `502` 基线不要擅自改成 `200`，脚本比较的
是“切换前后是否一致”，不是替业务修复历史状态。

如果 LiveKit 容器连接了多个 Docker 网络，脚本会拒绝猜测代理来源。此时根据现场
路由确认实际网关，并在配置中填写单个 `LIVEKIT_PROXY_TRUSTED_IP`；脚本仍只生成该
地址的 `/32`，不能为了省事信任整个 Docker 网段。

## 三、一条命令安装

先做只读检查：

```bash
sudo bash deploy/install-livekit-l4-sni.sh --check
```

预期最后一行包含：

```text
CHECK_PASS: prerequisites, certificate, Nginx modules, Compose, and LiveKit are valid
```

确认无报错后执行一键切换：

```bash
sudo bash deploy/install-livekit-l4-sni.sh --apply
```

脚本将依次安装 HAProxy、备份配置、动态检测 LiveKit 所在 Docker 网络网关并只信任
该 IP 的 `/32`、生成 Compose/Nginx/HAProxy 配置、停止并禁用 eturnal、启动 LiveKit
embedded TURN、切换 443、执行真实 TLS 与 HTTP 健康检查。任一步失败都会自动回滚。

成功时最后两行应包含：

```text
VERIFY_PASS: HTTPS baselines, backend, TURN TLS, ports, and PROXY protocol
APPLY_PASS: backup=... rollback=...
```

HAProxy 仅安装 Debian 官方包，不监听公网；典型新增磁盘占用约 5 MB，不产生云资源
月费。配置中故意不使用 HAProxy TCP `check`，否则裸 TCP 探针会持续制造 LiveKit
`TLS handshake failed: EOF` 日志。

### 安装器行为要点（候选实现）

- **nginx 实例钉定**：`NGINX_BIN` 显式指定优先，其次依次探测宝塔安装路径与
  `PATH`；选定二进制必须与当前运行 master 的实际二进制一致，存在歧义或 master
  带无法识别的自定义 `-c`/`-p`/`-g` 参数时以 `BLOCKED_ENV` 拒绝，绝不猜一个
  继续执行。
- **全阶段同实例同参数**：preflight、backup、install、verify、restore（含
  verify_switch）全程使用同一二进制与同一 `-c`/`-p` 参数。`nginx -T` 配置导出、
  漂移检测与 reload 都作用于钉定实例，不会因另一个 systemd unit 下同名 nginx
  返回成功而误判宝塔实例已重载。
- **reload 方式**：不再依赖 `systemctl reload nginx`；先校验候选配置与 pid 文件，
  再对钉定 master 发 `SIGHUP` 兜底，reload 后核验有效配置、端口 owner 与 HTTPS
  基线。
- **重复 apply 保护保留**：检测到安装器外已启用的 SNI 现场时拒绝再次 `--apply`
  （见文首状态注记），不提供"补跑接管"路径。

## 四、安装后检查

```bash
sudo systemctl is-active nginx haproxy
sudo systemctl is-active eturnal || true
sudo ss -lntup | grep -E ':(443|10443|15442|15443|3478|50201|50500)\b'
openssl s_client -connect turn.imboy.pub:443 -servername turn.imboy.pub \
  -verify_hostname turn.imboy.pub -brief </dev/null
sudo docker logs --since 5m imboy_livekit 2>&1 | tail -100
```

期望：Nginx 占公网 443 和回环 10443；HAProxy 只占回环 15442；Docker 只把 LiveKit
TURN/TLS 映射到回环 15443；eturnal 为 inactive；证书输出包含
`Verification: OK`。端口在线不是最终通话证明，发布前仍须用 Android 与 iPhone 真机
确认 ICE selected candidate 为 `relay`，并覆盖 Wi-Fi、蜂窝、前后台和断网重连。

宝塔面板管理的 nginx 不由 systemd unit 承载，`systemctl is-active nginx` 不适用；
此时以 `ss` 的端口归属和钉定 master 进程存活为准（HAProxy 仍为 systemd 服务）。

## 五、证书续期

安装器会把 `deploy/nginx/livekit-turn-cert-deploy-hook.sh` 安装到 certbot deploy hook。
它只在 `turn.<域名>` 证书真正续期后原子更新证书并重启 LiveKit，不会启动 eturnal。
LiveKit 重启会中断正在进行的通话，dry-run 和真实续期应放在低峰窗口：

```bash
# 新版 Certbot 直接在 dry-run 成功后执行已安装的 deploy hook；旧版拆成两步
if certbot renew --help all 2>&1 | grep -q -- '--run-deploy-hooks'; then
  sudo certbot renew --dry-run --run-deploy-hooks --no-random-sleep-on-renew \
    --cert-name turn.imboy.pub
else
  sudo certbot renew --dry-run --no-random-sleep-on-renew \
    --cert-name turn.imboy.pub
  sudo env RENEWED_LINEAGE=/etc/letsencrypt/live/turn.imboy.pub \
    RENEWED_DOMAINS=turn.imboy.pub \
    /etc/letsencrypt/renewal-hooks/deploy/livekit-turn-cert.sh
fi

sudo systemctl is-active eturnal || true
sudo openssl x509 -in /etc/letsencrypt/live/turn.imboy.pub/fullchain.pem \
  -noout -subject -issuer -enddate
```

只执行普通 `--dry-run` 不会调用 deploy hook。支持 `--run-deploy-hooks` 的版本必须带上
该参数；不认识该参数的旧版（例如 Debian 12 的 Certbot 2.1.0）必须在 dry-run 成功后
显式调用已安装的 hook。两种方式都会让 hook 使用当前有效证书（不是 staging 临时证书）
重启 LiveKit，因此会中断进行中的通话，必须安排在低峰窗口。命令会访问 Let's Encrypt
staging，执行前还要遵守本组织的第三方交互授权规则。人工演练使用
`--no-random-sleep-on-renew` 跳过 Certbot 为定时任务设计的随机延迟；系统定时续期保持
默认随机调度。

## 六、重启后验证

服务器重启后重新执行只读检查和 TLS 验证：

```bash
cd /path/to/imboy
sudo bash deploy/install-livekit-l4-sni.sh --check
openssl s_client -connect turn.imboy.pub:443 -servername turn.imboy.pub \
  -verify_hostname turn.imboy.pub -brief </dev/null
```

同时确认 `nginx`、`haproxy`、`docker` 为 active，`eturnal` 仍为 inactive。若 Docker
网络被重建且网关地址变化，不能扩大为一个网段信任；应先回滚，再重新 `--apply`，让
脚本生成新的精确 `/32`。该"回滚后重新 apply"仅适用于**安装器管理的现场**（有
format=2 备份可回滚）；若当前为安装器外手动启用状态，`--apply` 会被重复 apply
保护拒绝（见文首状态注记），迁移决策须另立任务，不在本手册流程内。

## 七、一键回滚

> 本章节描述的备份/恢复合同为候选实现（见文首状态注记），服务器尚未部署本候选。

```bash
cd /path/to/imboy
sudo bash deploy/install-livekit-l4-sni.sh --rollback
```

脚本校验备份 manifest 与逐文件 SHA-256 后恢复 Nginx、HAProxy、Compose 和证书
hook，并恢复切换前的 eturnal active/enabled 状态。显式恢复某次备份可使用：

```bash
sudo bash deploy/install-livekit-l4-sni.sh --rollback \
  --backup /root/imboy-livekit-l4-sni-backups/<UTC时间戳>
```

**备份格式断代（format=2）**：候选实现的备份目录包含 `manifest.txt`、逐文件
SHA-256 与服务状态快照（format=2），恢复程序只认新格式。旧的 format=1 tar 备份
与恢复程序**不兼容（断代）**：显式 `--backup` 只接受安装器 format=2 的有效备份，
普通 tar 包不能直接传给 `--backup`；旧 format=1 备份须按当时的旧恢复程序处理。
跨实例恢复（备份记录的 nginx 二进制或 conf 路径与当前钉定实例不一致）同样被拒绝。

**overlay 感知回滚**：回滚保存并恢复切换时实际生效的 Compose 组合（基础文件 +
运维 overlay）与 overlay 文件存在性，还原后校验非 secret 有效配置哈希与切换前
一致；不允许"只恢复磁盘文件却用缺 overlay 的基础 Compose 重建"。

**回滚可用性必须由恢复行为证明**：format=2 备份的恢复与失败回滚行为已在沙箱
测试中覆盖（95 项，含手动 overlay 状态与首次切换失败回滚），**生产环境未演练**；
在服务器完成一次真实恢复演练之前，不得声称生产回滚已就绪，也不得声称"保持手动
状态即零风险"。

回滚不会自动卸载 HAProxy；包本身不监听公网。只有确认 Nginx 已恢复直接承载 443、
eturnal 已恢复且不再需要该转换器后，才可另行人工卸载 HAProxy。旧 eturnal/coturn 的
卸载和配置删除是独立的破坏性操作，不属于本脚本，也不能仅凭本脚本成功就执行。

## 八、listen 漂移巡检与监控（候选实现/待上线）

> 本章节链路**尚未部署到服务器**；候选实现位于本仓库，待服务器授权后按
> `docs/plans/2026-10-01-l4-sni-hardening-relay-verification-plan-v1.md`
> Task 7–9 上线并验证告警管路后，才能回写"已上线"。

### 巡检脚本

`scripts/check_l4_sni_listen.sh` 检查受管 vhost **配置文件文本**的 listen 合同：
每个受管 HTTPS server 的 HTTPS listener 必须为 loopback:10443 且含独立
`proxy_protocol` token；任何直连 443（裸 `443`、指定 IPv4 地址、`0.0.0.0`、`*`、
IPv6 形式）均为漂移。注释中的指令不算有效配置。它只检查文本，**不证明实际运行
配置、端口归属或媒体健康**。

```bash
sudo bash scripts/check_l4_sni_listen.sh --strict --push
```

- 模式：`--strict`（默认，切换后合同）/ `--pre-switch`（切换前允许直连 443，
  但文件结构仍须可判定）。
- 退出码：`0` 健康；`1` 发现漂移，**含受管配置缺失**（受检文件无任何受管 HTTPS
  server 定义——空文件、纯注释、仅 HTTP server——按漂移 fail-closed，不猜通过）；
  `2` 输入/解析错误（文件缺失/不可读、include、跨行指令、未知 listen 参数等，
  诊断打印 file:line）；`3` 要求推送但 Pushgateway URL 缺失或推送失败。
- 配置发现优先级：显式传入的文件参数优先（跳过自动发现）；否则从
  `L4_SNI_ENV_FILE`（默认 `/etc/imboy/livekit-l4-sni.env`）读取
  `NGINX_VHOST_DIR` 与 `HTTPS_VHOST_FILES`。
- 指标（Pushgateway 文本格式，job=`imboy_l4_sni_check`）：
  `imboy_l4_sni_listen_drift{config_file,server}`（1=漂移 0=健康）、
  `imboy_l4_sni_check_success`、`imboy_l4_sni_last_check_timestamp_seconds`。

### 定时巡检与日志（候选 cron 行）

仓库 `deploy/cron/imboy-ops.cron`（部署到 `/etc/cron.d/imboy-ops`）候选行，每
5 分钟执行一次：

```cron
*/5 * * * * root set -a; . /etc/imboy/ops.env; set +a; cd /opt/imboy && L4_SNI_ENV_FILE=/etc/imboy/livekit-l4-sni.env bash scripts/check_l4_sni_listen.sh --strict --push >> /var/log/imboy/l4_sni_listen.log 2>&1
```

日志写入 `/var/log/imboy/l4_sni_listen.log`（诊断含 file:line）。`set -a` 确保
`/etc/imboy/ops.env` 中的 `PUSHGATEWAY_URL` 等变量导出给子进程；巡检行不使用
`|| true` 隐藏失败。人工受控 reload 前也应执行同一检测。

### 告警规则（`deploy/prometheus/rules/imboy-alerts.yml`，group `imboy.l4_sni`）

| 告警 | 表达式要点 | for | 级别 |
|---|---|---|---|
| `L4SNIListenDrift` | `imboy_l4_sni_listen_drift > 0` | 10m | critical |
| `L4SNICheckFailed` | `imboy_l4_sni_check_success == 0` | 10m | critical |
| `L4SNIStaleMetrics` | `time() - imboy_l4_sni_last_check_timestamp_seconds > 900` | 5m | warning |
| `L4SNIMetricsMissing` | Pushgateway 抓取存活（`up=1`）时 `absent_over_time` + 按 instance 的 offset 消失检测 | 0m | warning |

**语义链：`drift=0` 不是健康证明，新鲜时间戳才是巡检存活证明。** 推送持续失败
（脚本 exit 3）时指标停留在 Pushgateway 旧值，旧 `drift=0` 永不自动消失（Pushgateway
无 TTL），只有 `imboy_l4_sni_last_check_timestamp_seconds` 持续刷新才能证明巡检
活着；`L4SNIStaleMetrics`（>900s 未刷新，约 3 轮巡检）即为此兜底。
`L4SNIMetricsMissing` 仅在 Pushgateway 抓取存活时判定指标从未推送或被整体删除，
Pushgateway 自身宕机（`up=0`）由部署侧 scrape 存活验证负责，不属本告警组职责。

### 推送助手严格模式（opt-in）

`scripts/lib/metrics_push.sh` 新增可选严格模式：设置 `METRICS_PUSH_STRICT=1` 时
推送失败或 URL 缺失返回非零，每次尝试后 `METRICS_PUSH_LAST_STATUS`
（`ok`/`fail`/`skipped`）可观测。默认行为零改变（未开启时失败仍返回 0），既有
调用方语义不变。巡检脚本 `check_l4_sni_listen.sh` 自带独立推送路径（`--push`，
失败 exit 3），不依赖该 helper 的严格模式。

## 九、已知限制与待服务器核实清单（2026-10-02）

以下事项在本地候选阶段未闭环，属**待服务器核实**项（计划 Task 7/9 范围），
不得提前当作已解决：

1. **存量 cron 环境加载**：现役 cron 配置中已有 4 条任务行以
   `. /etc/imboy/ops.env` 方式 source 环境但无导出，其指标推送可能一直被静默
   跳过；待服务器核实实际效果。候选巡检行已用 `set -a; . …; set +a` 显式导出。
2. **Pushgateway 自身存活**：Pushgateway 宕机（scrape `up=0`）目前没有专属
   告警，依赖部署侧 scrape/up 验证；是否补专属告警待上线阶段决策。
3. **多主机推送覆盖**：巡检推送未显式携带 `instance` 标签，多主机同时推送会
   互相覆盖；当前单机部署不构成问题，扩容前须补标签方案。
4. **巡检日志轮转**：`/var/log/imboy/l4_sni_listen.log` 尚无 logrotate 配置
   先例，上线时应一并配置。
5. **既有测试基线漂移**：`lk_preflight_rtc_turn_test.sh` 有 3 项在本阶段 base
   上即失败的既有 fixture 漂移（非本次候选引入），处置另行登记。
6. **生产回滚未演练**：回滚可用性必须由恢复行为证明；目前仅沙箱证明，生产
   恢复演练待服务器授权后安排（见第七章）。
7. **手动模式现场状态未绑定**：2026-10-01 手动 SNI 实施后的实际 nginx/LiveKit/
   eturnal 状态为历史采样陈述，待授权后重新采样绑定（见文首状态注记）。
