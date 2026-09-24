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

## 一、前置条件

1. Debian/Ubuntu，使用 systemd，root 或 sudo 权限。
2. 宿主机 Nginx 已运行，编译时包含 `stream` 和 `stream_ssl_preread` 模块。
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
脚本生成新的精确 `/32`。

## 七、一键回滚

```bash
cd /path/to/imboy
sudo bash deploy/install-livekit-l4-sni.sh --rollback
```

脚本校验备份 SHA-256 后恢复 Nginx、HAProxy、Compose 和证书 hook，并恢复切换前的
eturnal active/enabled 状态。显式恢复某次备份可使用：

```bash
sudo bash deploy/install-livekit-l4-sni.sh --rollback \
  --backup /root/imboy-livekit-l4-sni-backups/<UTC时间戳>
```

回滚不会自动卸载 HAProxy；包本身不监听公网。只有确认 Nginx 已恢复直接承载 443、
eturnal 已恢复且不再需要该转换器后，才可另行人工卸载 HAProxy。旧 eturnal/coturn 的
卸载和配置删除是独立的破坏性操作，不属于本脚本，也不能仅凭本脚本成功就执行。
