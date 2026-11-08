# 环境、网络、域名与证书

## 1. 主机要求

首个演练目标为专用 Ubuntu 24.04 x86_64 主机和同平台构建的二进制。准备起点为 4 vCPU、16 GiB 内存、至少 40 GiB 可用 SSD；这些数值是本包小规模演练预算，不是承诺的用户数、吞吐量或生产容量。磁盘还需容纳附件增长、数据库/WAL、日志、备份与恢复演练的临时空间。

目标环境应具备 systemd、Bash 4.4+、GNU coreutils、tar、gzip、支持 `--aws-sigv4` 的 curl、OpenSSL、jq、awk、sed、grep、find、util-linux、iproute2、file、Nginx，以及候选二进制需要的动态库。发布方冻结同平台候选主机软件包的精确版本；环境方可预装，也可在有软件安装授权且批准源可达时使用 `install.sh --apply --prepare-host`。最终依赖以无缓存主机实测为准，不从其他主机随意复制 `.so`。

应用 release 自带 ERTS。客户不需要 Erlang 开发环境、Git、编译器或 Python。纯 Shell 指交付流程与辅助工具的实现；业务仍由 Erlang/OTP、PostgreSQL、Garage 和 Nginx 二进制运行。

主机必须完成时间同步。安装器记录 UTC 时间，后端运行配置中的 `TZ` 默认为 `Asia/Shanghai`，会影响沿用上游的本地时区定时任务；日志与演练记录均注明时区，不能把这一设置擅自当作 UTC。预签名 URL 与 TLS 依赖合理时钟；大量偏移会使请求过期或尚未生效。

## 2. 三域名与流量关系

| 外部域名 | 后端去向 | 必须保留的语义 |
| --- | --- | --- |
| `api.example.com` | 本机 IMBoy HTTP 9800 | `/api/v1/ws` 的 Upgrade/Connection；真实客户端 IP；HTTPS 来源 |
| `s3.example.com` | 本机 Garage S3 API 3900 | 原 Host、路径、查询串和签名头；不增添 `/s3` 前缀 |
| `files.example.com` | 本机 Garage Website 3902 | Host 映射到公开桶；仅提供公开桶读取 |

`IMBOY_GARAGE_PUBLIC_ENDPOINT` 的示例值固定为 **`https://s3.example.com`**，不再使用 `https://api.example.com/s3`。`FILES_DOMAIN` 对应 `garage.public_base_url`；两者用途不同，不能互换。私有附件不通过公开网站域名访问。

三个域名的 A 记录指向入口 IPv4。当前生成的 Nginx 模板只配置 IPv4 监听，默认不发布 AAAA；IPv6 要由发布方补充监听、路由与防火墙并单独演练，不能只新增 DNS。DNS TTL、变更负责人、内外网解析差异、代理/CDN 都记录在现场表中。

本包不处理多台后端、数据库高可用、Garage 多节点或既有共享反向代理。这些拓扑需要另行验证节点标识、路由、对象一致性和故障切换。

## 3. 入站端口

| 端口/协议 | 来源 | 用途 | 默认约定 |
| --- | --- | --- | --- |
| 443/TCP | 许可的客户端网络 | HTTPS、WSS、S3 签名请求、公开文件 | 对外开放 |
| 80/TCP | 许可的客户端网络 | HTTPS 跳转；是否用于证书挑战由现场确定 | 按 Nginx 配置使用 |
| SSH 管理端口/TCP | 管理网络/VPN/跳板机 | 安装与运维 | 由现场控制，不由脚本修改 |
| 9800/TCP | 127.0.0.1 | IMBoy 内部 HTTP | 不对公网开放 |
| 5432/TCP | 127.0.0.1 | 本机后端访问 PostgreSQL | 不对公网开放 |
| 3900/TCP | 127.0.0.1 | Garage S3 API | 仅经 Nginx 对外 |
| 3901/TCP | 127.0.0.1 | Garage 单节点 RPC | 不对公网开放 |
| 3902/TCP | 127.0.0.1 | Garage 公开桶 Website | 仅经 Nginx 对外 |
| 3903/TCP | 127.0.0.1 | Garage 管理 | 不对公网开放 |
| 4369 / 9100 TCP | 127.0.0.1 | EPMD / Erlang 本机运行控制 | 不对公网开放 |

本包不需要 TURN、LiveKit 媒体端口、Kubernetes 端口等未纳入合同的入口。安装器不修改现场防火墙；环境负责人配置并留存验证证据。

## 4. 出站与离线范围

应用与 PG/Garage 通过本机地址通讯。是否访问短信、SMTP、推送、IdP、更新仓库取决于启用范围；域名解析、授时和证书续期服务应可用。

**二进制随包供应不等于整台操作系统完全离线安装。** `--prepare-host` 需要访问现场已批准、能提供清单中精确版本的发行版源。完全离线时，由环境方先准备匹配的本地源或安装好系统依赖，再不带该选项安装并复验。不能把构建机缓存当作客户依赖已齐，也不能把“源里有更新版本”当作冻结版本可用。

发布方构建阶段的源码下载与工具联网不得转嫁给客户执行者；在版本冻结表记录构建来源即可。

## 5. TLS 文件核对

`TLS_DIR` 下有 `api/`、`s3/`、`files/` 三个目录，每个包含 `fullchain.pem` 与 `privkey.pem`。证书覆盖对应域名，完整中间链与私钥匹配。证书最少剩余 7 天有效，交付建议 30 天以上。CA 根证书通过可信渠道安装；应用私钥不作为 CA 材料分发。

以下只读命令不输出私钥正文。路径和域名替换为现场值：

```bash
openssl x509 -in fullchain.pem -noout -subject -issuer -dates
openssl x509 -in fullchain.pem -noout -checkhost api.example.com
openssl x509 -in fullchain.pem -noout -checkend 2592000
openssl x509 -in fullchain.pem -pubkey -noout | openssl pkey -pubin -outform DER | sha256sum
openssl pkey -in privkey.pem -pubout -outform DER | sha256sum
```

`-checkhost` 要查看输出，明确出现 `does match certificate` 才算域名匹配；部分 OpenSSL 版本即使不匹配也可能返回 0，不能只看退出码。安装器也显式检查匹配输出。最后两行公钥摘要应一致。`-checkend 2592000` 检查未来 30 天是否过期。本包不接受需要口令解密的 TLS 私钥，使用受限文件权限保护现场无口令 PEM；其他私钥托管方式需另做对应部署方案。

本包不调用 Certbot，不自动签发证书。证书由企业 CA、外部运维流程或经批准的 Shell ACME 工具提供。续期责任人应按现场计划更新证书、执行 `nginx -t`、受控 reload 并从外网验证；失败时保留已知可用证书。证书轮换不需要重建数据库或重新生成应用 RSA。

## 6. S3 与浏览器检查

预签名请求包含精确 Host、路径、查询串和有效期。代理不能重写签名路径、移除查询参数、改写 Host 或把上传重定向到其他域名。CDN/WAF/企业网关若限制 PUT、DELETE、OPTIONS、分块上传或大请求体，应按实际上传边界逐项验证。

浏览器附件验收需要真实前端 origin 与 CORS 配置。只测 `curl` 不能证明浏览器跨域上传成功。本包仅接受 `https://真实域名` 形式的默认 443 origin，不允许显式端口、路径或通配符；自定义端口需由发布方调整并重新验收。没有交付 Web 前端时可以按范围记录 `NOT_APPLICABLE`，不能记为已通过。

TLS 与网络至少从服务器、管理网络、真实用户接入网络分别验证。S3 使用临时测试对象；报告不保留可访问业务附件的完整预签名 URL。
