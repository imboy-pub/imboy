# 原生 systemd 安装 / Native systemd installation

本交付形态在 Linux 单机上用 systemd 管理 IMBoy、PostgreSQL、Garage，由 Nginx 提供 HTTPS。工具包冻结于 alpha.83、迁移头 243。当前 main 的原生安装兼容性仍待验证。[验证范围 / Validation scope](docs/10_VALIDATION.md)

This delivery kit uses systemd for IMBoy, PostgreSQL, and Garage on one Linux host, with Nginx providing HTTPS. It is frozen at alpha.83 and migration head 243. Native compatibility with current main still needs validation.

## 拿到包后 / After receiving the package

查看 `metadata/release.env`。若 `PAYLOAD_READY=no`，由发布方先组装运行文件。若为 `yes`，核对候选和现场演练记录后再安装。

Read `metadata/release.env`. With `PAYLOAD_READY=no`, the publisher must assemble the runtime files. With `yes`, confirm the candidate identity and rehearsal evidence before installing.

在已组装包根复制 `templates/site.env.example` 为 `site.env`，权限设为 `0600`，填写现场值并准备 API、S3、公开文件域的证书。先只读检查，获准后安装。

At the assembled package root, copy `templates/site.env.example` to `site.env`, set mode `0600`, and supply the site settings and certificates for API, S3, and public files. Run the read-only check first and install once authorized.

```bash
sudo bash ./install.sh --check
sudo bash ./install.sh --apply
```

| 要做什么 / Task | 从这里开始 / Guide |
|---|---|
| 安装、继续中断的安装 / Install or resume | [安装 / Installation](docs/01_INSTALL.md) |
| 填写域名、证书和站点参数 / Configure DNS, TLS, and site settings | [网络 / Network](docs/02_NETWORK_TLS.md) · [配置 / Configuration](docs/03_CONFIGURATION.md) |
| 验收登录和附件 / Verify login and attachments | [验收 / Acceptance](docs/05_ACCEPTANCE.md) |
| 升级、备份、恢复和故障排查 / Upgrade, back up, restore, and troubleshoot | [运维 / Operations](docs/04_OPERATIONS.md) · [排障 / Troubleshooting](docs/06_TROUBLESHOOTING.md) |
| 组装运行文件和校验清单 / Assemble binaries and checksums | [发布方 / Publisher](tools/README_PUBLISHER.md) · [制包 / Assembly](docs/07_RELEASE_ASSEMBLY.md) |

包内 `CHECKSUMS.sha256` 覆盖全部交付文件。`site.env` 由现场维护。日志、备份和验收记录保存在包外。

The package manifest covers the delivered files. The operator owns `site.env` and stores logs, backups, and acceptance records outside the package.

[完整中文手册 / Full Chinese reference](MANUAL.zh-CN.md)
