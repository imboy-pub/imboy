# 验证方法与结果记录 / Verification and results

本文件定义检查方法及其证据范围，不预填某次制包或客户现场的通过结果。每次交付先记录 ZIP 的 SHA-256、`SOURCE_COMMIT`、工具版本、执行时间与退出码，再填写 [验收记录模板](ACCEPTANCE_TEMPLATE.md) 的副本。生成 ZIP 本身不代表真实安装成功。

## 1. 在解压包中复跑离线检查

按 [安装手册](INSTALL.md) 准备 Bash、jq、OpenSSL 与基本 Unix 工具。在包根运行：

```bash
(
  set -euo pipefail
  umask 077
  mkdir -p records
  chmod 700 records
  bash scripts/run_offline_checks.sh 2>&1 \
    | tee "records/offline-$(date +%Y%m%d-%H%M%S).log"
)
```

| 检查 | 命令 | 能证明的范围 |
|---|---|---|
| 包完整性、权限、Shell 语法 | `bash scripts/check_delivery.sh` | 清单文件存在、字节/权限匹配、Bash 语法可解析 |
| 原安装控制流回归 | `bash scripts/test/install_one_click_test.sh` | 临时目录中模拟 Docker/网络等外部行为，检查初始化、顺序、失败传播与文件保留 |
| preflight 规则自测 | `bash deploy/preflight.sh --self-test` | 规则函数的正反用例；不读取客户现场配置 |
| 入口保护回归 | `bash scripts/test/delivery_guard_test.sh` | 模拟安装器下的配置范围、已有数据/凭据保护、并发锁和退出码传播 |

统一入口依次运行上述检查。PASS 数以本次日志为准；文档语法、mock 和静态路径检查不能证明真实容器、数据库或客户端业务已运行。客户安装链无需宿主机 Python；宿主机是否恰好安装 Python，与脚本是否依赖它是两回事。

维护者另在源码仓库运行 `bash scripts/test/package_community_test.sh`，验证制包清单、归档内容、解压权限、路径、凭据排除和重复输出保护。它不属于客户安装依赖，也不要求客户获取源码或 Git。

## 2. 现场检查必须另记

| 项目 | 初始状态 | 现场入口 / 条件 |
|---|---|---|
| 实际 `docker compose config -q` | NOT_RUN | 填写并初始化的 `.env`、真实 Compose |
| 镜像拉取、digest、容器启动与健康 | NOT_RUN | 已发布镜像、拉取权限、网络、Docker |
| Nginx `-t` 与 RSA 文件读取 | NOT_RUN | 真实容器；见现场验收命令 |
| 五域 ACME、外网 TLS 信任、续期与加载 | NOT_RUN | DNS、80/443、证书维护流程；TURN 另验 5349 |
| 数据库迁移和清理 LOGIN 认证/权限 | NOT_RUN | 运行镜像迁移清单、PG、DBA；正确/错误密码两方向验证 |
| 管理员初始化与真实用户登录 | NOT_RUN | 匹配客户端、正式开户/验证条件 |
| 双端 WebSocket 消息、回执与重连 | NOT_RUN | 两个账号、两台客户端、客户网络 |
| 私有附件与公开头像/图片 | NOT_RUN | Garage 桶/授权、实际上传下载与内容比对 |
| RTC/TURN、monitoring、短信/SMTP | NOT_RUN | 按本次交付范围逐项准备和执行 |
| 真实重复安装、升级、备份恢复、陌生人演练 | NOT_RUN | 独立演练服务器、备份、维护窗口和接收方 |

执行入口见 [ACCEPTANCE_COMMANDS.md](ACCEPTANCE_COMMANDS.md)、[DB_CLEANUP_ACCOUNT.md](DB_CLEANUP_ACCOUNT.md)、[UPGRADE_AND_RECOVERY.md](UPGRADE_AND_RECOVERY.md)。未执行项保持 `NOT_RUN`；缺少镜像、域名、账号或设备等明确前提时写 `BLOCKED`、原因与负责人。已有 PASS 在配置、镜像或包内容变化后应复验对应范围。

## 3. 记录和完整性边界

`MANIFEST.sha256` 与 `FILE_MODES.tsv` 覆盖不可变交付文件。`deploy/.env`、`deploy/data/`、安装锁和 `records/` 属于运行或记录内容，不纳入不可变清单；不可在修改脚本、Compose 或模板后自行重算清单掩盖差异。需要调整静态文件时，回到源码提交、审阅、重新制包和验证。

离线日志与现场日志分别保存到客户受控记录目录；外发前脱敏。不要把真实 `.env`、证书私钥、数据库备份、客户数据或未过滤的容器环境纳入仓库或重新分发的 ZIP。
