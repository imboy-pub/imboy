# 社区版交付验收结果记录模板 / Acceptance record

**模板状态：未填写；真实服务器与业务结果均为 `NOT_RUN`。** 复制本文件后填写，不要把包内离线检查结论直接粘成现场通过结果。

## 1. 本次验收身份

| 字段 | 值 |
|---|---|
| 记录编号 | |
| 客户 / 实例代号（避免真实个人信息） | |
| 包文件名 / 外层 SHA-256 | |
| 包内文件清单校验结果 / 证据 | |
| 源码基线（包内 `SOURCE_COMMIT`） | |
| 包内文件来源清单 / 本次发布说明 | |
| `.env` 的 `IMBOY_VERSION` | |
| backend 实际镜像引用 / image ID / RepoDigest | |
| admin 实际镜像引用 / image ID / RepoDigest | |
| widget 实际镜像引用 / image ID / RepoDigest | |
| pg18 实际镜像引用 / image ID / RepoDigest | |
| Garage / Nginx / Certbot / LiveKit 实际 digest | |
| 客户端版本 / 平台 / 构建身份 | |
| OS / 架构 / CPU / 内存 / 可用磁盘 | |
| Docker / Compose / Bash / jq / OpenSSL 版本 | |
| DATA_DIR / 安装目录 | |
| 五域 / 公网入口（客户保管） | |
| 已启用 profile / TURN / 其他可选项 | |
| 执行人 / 复核人 | |
| 开始 / 结束时间与时区 | |
| 服务器操作授权 / 维护窗口 | |

不要在此表记录密码、私钥、token、Erlang cookie、短信 / SMTP 凭据或签名下载 URL。

## 2. 结果状态

`PASS`：实际通过且有证据。`FAIL`：实际失败。`BLOCKED`：缺前置条件。`NOT_RUN`：未执行。`N/A`：经确认不在交付范围。WARN 单独记录，不自动折算为 PASS。

## 3. 文件、离线检查与现场前置

| ID | 检查项 | 状态 | 实际命令 / 证据位置 / 退出码 / 说明 |
|---|---|---|---|
| P01 | ZIP 校验值与解压目录完整性 | NOT_RUN | |
| P02 | `scripts/check_delivery.sh` | NOT_RUN | |
| P03 | 安装控制流离线回归 | NOT_RUN | |
| P04 | preflight `--self-test` | NOT_RUN | |
| P05 | Shell / Unix 工具齐全，客户安装路径不依赖宿主 Python | NOT_RUN | 不把“宿主机装了 Python”混同于“脚本依赖 Python” |
| P06 | OS / x86_64 / 资源 / 磁盘与权限 | NOT_RUN | |
| P07 | Docker / Compose 版本及 daemon 权限 | NOT_RUN | |
| P08 | 五域 A / AAAA 与公网入口一致 | NOT_RUN | |
| P09 | 五域 HTTP-01 网络条件 | NOT_RUN | |
| P10 | 实际镜像可拉取与版本身份 | NOT_RUN | |
| P11 | 清理 LOGIN、客户凭据及客户端条件齐备 | NOT_RUN | |

## 4. 安装与基础设施

| ID | 检查项 | 状态 | 证据 / 实际结果 |
|---|---|---|---|
| I01 | 原安装器执行与退出码 | NOT_RUN | |
| I02 | 实际 `docker compose config -q` | NOT_RUN | 记录完整 `-f` 组合，不附展开后的密钥 |
| I03 | 镜像拉取及 edge 镜像文件检查 | NOT_RUN | |
| I04 | 9 个核心服务都在运行，适用健康检查正常 | NOT_RUN | 明确包含 widget 与 edge |
| I05 | PostgreSQL 就绪及扩展可用 | NOT_RUN | |
| I06 | 迁移记录与本次发布预期一致 | NOT_RUN | 单行 head、dirty=false、history 与镜像迁移集合一致 |
| I07 | 独立清理 LOGIN 最小授权与实际 TCP 密码登录 | NOT_RUN | |
| I08 | RSA 文件可读、挂载正确、公私钥匹配 | NOT_RUN | 不记录私钥内容 |
| I09 | Nginx 实际配置 `nginx -t` | NOT_RUN | |
| I10 | API / Admin / Widget / RTC 域证书受信任且域名匹配 | NOT_RUN | 不使用 `-k` 或关闭校验 |
| I11 | TURN 证书文件及链接目标有效 | NOT_RUN | 默认未启用 TURN 时也须完成签证 |
| I12 | 外网 API `/healthz` 与管理页、Widget 静态入口 | NOT_RUN | |
| I13 | 原 `sanity_check.sh` 全部结果及 WARN 处理 | NOT_RUN | |
| I14 | 证书自动续期与加载方案 | NOT_RUN | 需要实际演练，运行容器不等于已验证续期 |
| I15 | 日志、磁盘、备份策略和恢复责任 | NOT_RUN | |

## 5. 客户端业务

| ID | 操作 | 状态 | 账号代号 / 设备 / 时间 / 结果 / 证据 |
|---|---|---|---|
| B01 | 首次管理员初始化并登录 | NOT_RUN | |
| B02 | 初始化后再次访问 `/setup` 不可重复建立首次管理员 | NOT_RUN | |
| B03 | 普通账号正式注册或约定开户、登录 | NOT_RUN | 记录是否依赖实际短信 / SMTP |
| B04 | A → B 单聊消息发送、接收与回执 | NOT_RUN | |
| B05 | B → A 单聊消息发送、接收与回执 | NOT_RUN | |
| B06 | 断线后重新连接并恢复应收消息 | NOT_RUN | |
| B07 | 文件附件上传、发送、另一端下载及内容校验 | NOT_RUN | 原件 / 下载件 SHA-256 或 `cmp` 结果 |
| B08 | 图片附件显示、下载；按范围测试头像 / 群图 | NOT_RUN | 不用文件附件一项替代所有公开资源流程 |
| B09 | 重复安装 / 受控重启后登录、历史数据和附件仍可用 | NOT_RUN | |
| B10 | Widget 真实访客到坐席对话（若交付） | NOT_RUN | |
| B11 | 一对一 RTC 双端音视频（若交付） | NOT_RUN | 双向媒体、拒绝 / 挂断、重连等范围写明 |
| B12 | TURN 受限网络回退（若启用） | NOT_RUN | 记录客户端实际使用 relay 的证据 |
| B13 | SMTP / 短信真实投递（若启用） | NOT_RUN | |
| B14 | 监控采集、Grafana、实际告警通知（若启用） | NOT_RUN | |

WebSocket 返回 101、TCP 端口可达、HTTP 返回 200、证书签发成功都不能替代 B04–B12 的实际业务结果。

## 6. 重复安装、升级与恢复

| ID | 检查项 | 状态 | 证据 / 说明 |
|---|---|---|---|
| R01 | 重复安装前后 `.env` / RSA 指纹一致 | NOT_RUN | 指纹保留在客户受控记录，不附原文 |
| R02 | 已有数据库、Garage、证书及数据标记保留 | NOT_RUN | |
| R03 | 配置遗失而数据存在时，入口拒绝重新初始化 | NOT_RUN | 只在独立演练副本测试，不破坏客户实例 |
| R04 | 并发安装锁拒绝第二个安装进程 | NOT_RUN | |
| R05 | 升级前数据库 / 角色 / 配置 / 密钥 / 对象存储备份 | NOT_RUN | 备份位置、时间点、限制权限 |
| R06 | 升级后 backend / admin / widget / edge 对齐，PG 镜像未误变更 | NOT_RUN | |
| R07 | 适用数据库迁移成功且旧数据可用 | NOT_RUN | |
| R08 | 在独立环境实际恢复备份并复验业务 | NOT_RUN | “备份文件能列目录”不能填此项 PASS |

## 7. 失败、告警与未验证项

| ID | 状态 | 问题 / 缺少条件 | 影响 | 下一步与验证方式 | 负责人 | 目标日期 |
|---|---|---|---|---|---|---|
| | | | | | | |

## 8. 陌生人演练干预记录

| 时间 | 所在步骤 | 原命令 / 错误 | 文档是否足够 | 额外指导或操作 | 介入人 / 耗时 | 应修改哪份材料 |
|---|---|---|---|---|---|---|
| | | | | | | |

## 9. 本次结论

- 本次必需项范围：
- PASS 数：
- FAIL 数：
- BLOCKED 数：
- NOT_RUN 数：
- 经确认 N/A 项及原因：
- 未关闭 WARN 及接受依据：
- 结论：`未验收 / 有条件验收 / 验收通过`（选择并写明依据）：
- 客户确认人 / 时间：
- 交付方确认人 / 时间：
- 原始证据受控存放位置与脱敏交接记录：

只有逐项执行后才能改变上述状态；没有真实环境时保留 `NOT_RUN` 或 `BLOCKED`。
