# 从零安装手册

适用对象：第一次接触 IMBoy、能使用 SSH 和基本 Linux 命令的执行者。目标是按材料部署，无需阅读后端业务代码。执行前先读 `00_DELIVERY_STATUS.md`，确认拿到的是**发布方已组装 payload 的候选目录**。

## 1. 接收材料

必须同时收到：完整安装目录、发行物版本与哈希、非秘密现场参数、三域名证书交接说明、范围表、验收表、发布联系人。仅收到模板材料 ZIP 时，先由发布方执行组装流程。

在无生产数据的目标机解压后，进入安装目录。下面命令均在这个目录运行。不要复制单个 `install.sh` 到其他位置执行；它依赖同目录下的 `lib/`、`templates/`、`metadata/` 与 `payload/`。

Linux 建议接收 `.tar.gz` 格式。假设发布方把候选放在 `/srv/deliveries/imboy-candidate.tar.gz`，可用 `tar -xzf /srv/deliveries/imboy-candidate.tar.gz -C /srv/deliveries` 解压，再进入解压后含 `install.sh` 的目录。文件名和候选目录以发布方实际交付为准。ZIP 也提供相同内容，需要本机具备 ZIP 解压工具。

```bash
bash ./install.sh --help
sha256sum --check CHECKSUMS.sha256
```

校验失败时先核对传输是否完整、候选版本是否一致，不要自行重新生成清单掩盖差异。完整性清单验证文件没有意外变化；发行方身份与可信渠道需要另行确认。

## 2. 填写现场参数

按 `02_NETWORK_TLS.md` 完成环境检查，复制并编辑模板：

```bash
cp templates/site.env.example site.env
chmod 600 site.env
```

`site.env` 只存非秘密值；不要放数据库口令、JWT、RSA 私钥、S3 secret 或 IdP client secret。模板中的占位符必须替换。域名填写主机名，不能带 `https://`、路径或尾斜杠；由配置生成器组合 URL。

| 参数 | 示例 | 用途 |
| --- | --- | --- |
| `API_DOMAIN` | `api.example.com` | HTTPS 与 `wss://api.example.com/api/v1/ws` |
| `S3_DOMAIN` | `s3.example.com` | Garage S3 API、公网预签名上传/下载 |
| `FILES_DOMAIN` | `files.example.com` | 独立公开桶的网站读取 |
| `TLS_DIR` | `/srv/imboy-tls` | 现场已准备的三域名证书目录 |

`TLS_DIR` 下必须有 `api/fullchain.pem`、`api/privkey.pem`、`s3/fullchain.pem`、`s3/privkey.pem`、`files/fullchain.pem`、`files/privkey.pem`。证书至少剩余 7 天有效期，且覆盖各自域名。建议交付时预留 30 天以上余量。相同的 SAN 证书可以覆盖三域，但三个目录仍按本包合同准备。私钥从现场安全渠道取得，不随通用包分发。

## 3. 只读预检查

```bash
sudo bash ./install.sh --check
```

预检查核对发行物、版本/操作系统/架构、冻结的主机软件包版本、系统命令和动态库、systemd、现场参数与证书。它不访问真实 DNS/TLS 链路、不启动服务。已有安装目录、专用账号、私有端口等接管保护在 `--apply` 写入之前再次执行；`--check` 通过不保证主机在正式安装时仍为空。预检查不等同于运行验收。

有阻塞就先修正对应项目再检查，无需反复盲试 `--apply`。如果只是主机缺少候选清单已冻结的系统依赖，可使用下一节的 `--prepare-host` 安装方式。记录故障编号、命令、退出码、时间和脱敏输出，按 `06_TROUBLESHOOTING.md` 处理。

## 4. 一键初始化安装

环境负责人确认这是一台专用的新主机、数据目录没有已有业务后，选择一种方式。系统依赖已经齐备时运行：

```bash
sudo bash ./install.sh --apply
```

如果需要同时安装候选包冻结的系统工具、Nginx 和运行库，目标机能访问现场批准的发行版软件源，并且本次操作包含安装这些软件的授权，则使用：

```bash
sudo bash ./install.sh --apply --prepare-host
```

`--prepare-host` 只与 `--apply` 一起使用。候选包、现场参数和已有安装/续跑身份检查通过后，入口调用 `tools/prepare-host.sh --apply`，再进行其余主机检查与初始化。依赖齐备时不用此选项。它会实际安装系统软件，不是只读检查；精确版本不可取得时应处理依赖阻塞，不临时换版本。后续初始化失败不会自动卸载已安装的软件。离线目标应预装匹配依赖，不使用需要联网取包的方式。

安装负责以下动作：

1. 核对候选包与目标环境，建立本安装的状态和目录。
2. 安装随包提供的运行时和后端 release，准备专用服务账号。
3. 在本机生成内部秘密与 RSA 材料，并设置受限权限。
4. 初始化专用 PostgreSQL 集群、业务身份和必需扩展。
5. 初始化 Garage 单节点布局、私有桶、公开桶、访问密钥及桶权限。
6. 生成应用运行配置、systemd 单元和 Nginx 三域名 TLS 配置。
7. 迁移期间保持应用公网入口关闭，临时提升专用 PG 集群中的 `imboy` 角色；成功后撤销授权，失败处理也停止入口并执行撤销。异常断电或数据库不可用时，恢复操作必须核对实际角色状态。成功检查数据库/Garage/后端 readiness 后才开放 TLS 入口并执行基础验证。

以程序返回码与验证报告判断结果。某一阶段“完成”不代表后续阶段完成。数据库迁移、对象存储权限和应用配置错误都必须保留为失败。

秘密只在现场生成或通过现场渠道提供。程序退出、SSH 断开或重启后，不要删除 `/etc/imboy` 或重新生成密钥；这些文件与数据库、对象存储共同构成安装身份。

## 5. 中断后续跑

保留原候选目录、原 `site.env`、状态和全部数据，核对错误原因后运行：

```bash
sudo bash ./install.sh --apply --resume
```

续跑应复用已有安装身份和秘密。候选版本、现场配置或证书发生变化时，先记录并确认脚本是否允许续跑，不要用新目录冒充同一次安装。无法确认某一步是否执行完成时，先检查实际服务和数据；不能仅依据日志最后一行猜测。

`install.sh --resume` 只用于这次首次安装。实例一旦进入升级或恢复阶段，入口会拒绝用旧安装包继续，以免把旧二进制切回已经迁移的数据。升级失败按 `04_OPERATIONS.md` 保留现场并处理，不能用首次安装的续跑命令代替。

## 6. 管理员与客户端接入

安装第七步会调用本机管理员初始化入口，需要交互终端。管理员初始化仍是独立验收项目；非交互运行或中断后，可单独执行：

```bash
sudo bash ./admin-init.sh
```

按提示输入管理员身份与口令，口令隐藏输入。本包入口要求 12–64 个可打印 ASCII 字符、无空格、至少包含字母和数字；允许标点，并需二次确认。邮箱或手机号必须符合后端校验。不要把口令附加到命令行、Shell 历史或工单。已有管理员时入口保留原账号和口令，不执行重置。没有预置通用管理员密码。

后端实际提供 `/api/adm/setup/status` 与 `/api/adm/setup/init`，本包使用本机入口并限制公网访问初始化路径。初始化管理员不代表管理后台网页已经安装；管理后台 UI 不属于本包。

客户端候选包由项目方提供，需配置同一 API 地址、WebSocket 路径和产品范围。真实登录若依赖短信/SMTP/IdP，先完成供应商配置；不得关闭认证以换取“验收通过”。

源码证据：[实际路由](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/src/imboy_router.erl)、[管理员初始化与校验](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/src/logic/adm_setup_logic.erl)。

## 7. 验证与交接

```bash
sudo bash ./verify.sh
sudo bash ./verify.sh --s3-write
curl --fail --silent --show-error https://api.example.com/healthz
```

`--s3-write` 通过本机 Nginx 的 HTTPS S3 域名入口发送真实 SigV4 请求，写入、读取、比对并删除唯一的临时测试对象；默认验证不会写对象。该检查将域名解析到回环，只证明本机 TLS/代理/S3 往返，不能代替外网 DNS、客户网络或真实 APP 预签名/确认业务。最后一条命令的域名换成现场值，并从真实用户接入网络再执行一次。不要添加 `-k` 绕过 TLS。内网 CA 应在测试客户端及本机 curl 的信任库中正确安装。

基础检查通过后，按 `05_ACCEPTANCE.md` 完成真实登录、双用户消息、私有/公开附件、重启、备份恢复、升级与允许回退。健康端点只检查后端、数据库连通性与 TSID readiness，不能替代附件签名、短信、推送或业务验收。

交接时保留候选版本、配置摘要、脱敏结果与实际命令，交接备份位置、密钥保管人、证书续期责任人和维护窗口。不要将 `/etc/imboy`、完整日志或数据库备份上传到公共问题跟踪系统。
