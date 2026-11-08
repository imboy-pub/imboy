# 部署配置示例与生成结果

本节供发布审阅、网络准备和故障定位。客户按 `01_INSTALL.md` 运行安装器即可，不必手工拼装本节内容。示例域名属于占位符，含 `GENERATED_ON_TARGET` 的项不是可运行秘密。

## 1. 配置示例清单

| 文件 | 内容 | 如何使用 |
| --- | --- | --- |
| `templates/site.env.example` | 七个现场非秘密参数 | 复制到包根目录 `site.env` 后填写 |
| `templates/backend.env.example` | 后端实际接收的运行环境变量 | 对照定位；不直接覆盖已初始化的运行文件 |
| `templates/nginx.conf.example` | 三域名 HTTP/TLS/WSS/S3/公开文件代理 | 从本包生成函数导出，无真实证书私钥 |
| `templates/systemd/imboy-postgresql.service` | 原生 PG18 单元 | 同安装器逻辑，仅供审阅 |
| `templates/systemd/imboy-garage.service` | Garage 单节点单元 | 同安装器逻辑，仅供审阅 |
| `templates/systemd/imboy-backend.service` | 自包含 Erlang release 前台单元 | 环境指向 `/etc/imboy` |
| `templates/garage.toml.example` | Garage 目录、端口、region 与 Website 合同 | 秘密由现场生成，不从示例复制 |
| `templates/postgresql.conf.example` | 专用 PG18 配置片段 | 生成的 `imboy.conf` 参考，不是完整集群配置 |

这些示例未在真实服务器独立启动测试。安装器生成具体值，Nginx 语法检查通过后再加载；文件可读不等于目标机运行通过。

## 2. 三入口映射

| 客户 URL | 内部服务 | 关键配置 |
| --- | --- | --- |
| `https://api.example.com` | `127.0.0.1:9800` | API 保留 Host；WSS Upgrade 支持 `/api/v1/ws` |
| `https://s3.example.com` | `127.0.0.1:3900` | `proxy_pass` 不追加 URI；Host、查询串、签名路径不变 |
| `https://files.example.com` | `127.0.0.1:3902` | Host 改为 `imboy-public.garage.localhost`；只允许 GET/HEAD |

S3 公网 endpoint 为 `https://s3.example.com`，内部 endpoint 为 `http://127.0.0.1:3900`。公开文件 base URL 为 `https://files.example.com`，由 Website 读取独立公开桶。三者不能合并成带 `/s3` 的代理路径。

Nginx 示例的 API/S3 请求体上限为 `500m`。这只是代理配置，不是已承诺的产品上传大小；后端、客户端、网关上限仍需一起核对，超限与中断重试须真实验收。

默认访问日志不记录查询串，但其他日志仍可能含敏感字段，分享前必须审阅脱敏。公网 `/metrics` 与 `/api/adm/setup/` 在代理层拒绝，管理员初始化走本机入口。

代理语义参考：[Nginx proxy_set_header / proxy_pass](https://nginx.org/en/docs/http/ngx_http_proxy_module.html)。

## 3. systemd 与运行目录

后端由 `imboy` 用户执行 `/opt/imboy/current/bin/imboy foreground`，systemd 读取 `/etc/imboy/backend.env`。本版本 relx 运行配置入口为：

```ini
Environment=RELX_CONFIG_PATH=/etc/imboy/sys.config
Environment=VMARGS_PATH=/etc/imboy/vm.args
Environment=RELX_REPLACE_OS_VARS=
```

`RELX_REPLACE_OS_VARS` 必须为空值；该版本按非空判断，字符串 `false` 也会启用替换。持久写入放在 `/var/lib/imboy/backend`，release 由 root 管理。启动模板证据见 `09_SOURCE_EVIDENCE.md`。

PG 与 Garage 使用各自服务账号和目录。服务依赖表达启动顺序，实际 readiness 由安装器继续检查；`After=` 不是数据库已可接受业务查询的证明。

本包采用单 Erlang 节点。EPMD/分布端口只在本机使用；不能复制同一 TSID 身份启动另一个同时承载写入的实例。多节点需另做节点登记、共享状态与故障验证。

## 4. 数据库身份与迁移窗口

集群使用本机专用 socket 和受限 TCP。安装/升级只在本包专用 PG 集群内临时将 `imboy` 角色提升为 SUPERUSER，应用公网入口在此窗口保持关闭。成功核对后收回权限；失败路径停止入口/后端并撤销临时授权。运行验证检查该角色是非 superuser。异常断电等情况下恢复操作仍须核对实际角色状态，不对公网开放数据库。

迁移和扩展正确性需要真实 PG18 执行证明。静态 SQL、库文件存在或源码搜索不能代替迁移成功记录。
