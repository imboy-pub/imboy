# l4_sni_installer_sandbox — install-livekit-l4-sni.sh 自包含离线沙箱（Task 3）

被 `scripts/test/livekit_l4_sni_installer_test.sh` 消费的隔离 fixture。**零真实
nginx / systemd / docker / 网络访问**：全部外部命令通过 PATH 注入本目录 stub；
真实系统只用到 bash / python3 / coreutils（cp/mkdir/chmod/sed/awk/grep 等）。
零 secret、零真实证书私钥（turn-certs 下的 PEM 为无意义填充文本，由 harness
在临时沙箱内生成）。

## 布局

| 路径 | 用途 |
|---|---|
| `bin/nginx-core` | 假 nginx 内核：`-V/-t/-T/-s reload`，行为由 `$L4_SB_STATE/ctl/*` 钩子控制；`-T` 输出带 `# configuration file <path>:` 头并展开一级 include |
| `nginx-bt/sbin/nginx` | “宝塔实例”包装器（tag=nginx-bt，prefix=nginx-bt/），exec nginx-core |
| `nginx-distro/sbin/nginx` | “发行版实例”包装器（tag=nginx-distro），用于错误实例/自动探测场景 |
| `nginx-*/conf/nginx.conf.template` | 主配置模板（`__HOME__` 占位符，harness 复制时渲染出绝对路径的 `pid`/include） |
| `nginx-bt/conf/vhosts/` | 两个 443 直连 vhost（api/dual），apply 时被安装器改写为 10443 |
| `bin/systemctl` | 假 systemctl：active/enabled 状态映射 + 变更副作用（restart haproxy 会按 `$L4_SB_HAPROXY_CONF` 是否含 `bind 127.0.0.1:15442` 增删 15442 监听行） |
| `bin/docker` | 假 docker/compose：`config` 输出确定性的逐文件 sha 摘要（备份/恢复前后可比对）；`up` 记录精确 `-f` 组合到 `ctl/compose-last-up`，并按组合是否含 generated overlay 增删 docker-proxy 监听行 |
| `bin/ss`、`bin/ps`、`bin/curl`、`bin/openssl`、`bin/stat`、`bin/find`、`bin/install`、`bin/sha256sum`、`bin/haproxy`、`bin/id` | 其余 GNU/系统命令替身（macOS/BSD 差异也由它们抹平） |
| `bin/check-stub` | Task-1 检测脚本 stub：按冻结接口合同的 CLI/退出码语义（`--strict`/`--pre-switch` + config_file...；rc 由 `ctl/check-strict-rc` / `ctl/check-pre-switch-rc` 控制） |

## 约定

- 所有 stub 读取环境变量 `L4_SB_STATE`（场景状态目录）；每个调用追加一行到
  `$L4_SB_STATE/calls.log`（`nginx:<tag> ...` / `systemctl ...` / `docker ...` /
  `check mode=...`），供测试断言“同一实例贯穿 preflight→backup→install→verify→restore”。
- “运行中的 nginx master”是一个真实存在、忽略 SIGHUP 的 sleep 进程：fake ps
  按状态表报告其为 `nginx: master process <bt-or-distro binary>`，因此安装器的
  `kill -HUP <pid>` / 存活检查走真实系统调用。
- 场景钩子（`ctl/` 下）：`nginx-fail-t-on=N`（第 N 次 `-t/-T` 失败，一次性）、
  `nginx-fail-reload-on=N`（第 N 次 `-s reload` 失败）、`nginx-V-no-modules`、
  `check-strict-rc` / `check-pre-switch-rc`、`compose-up-rc`、`openssl-rc`、
  `curl-codes`、`curl-ok-urls`、`turn-domain`、`systemctl-mainpid-nginx` 等。
- 本目录内容 **永不** 被测试修改：harness 每次 `cp -R` 到临时目录后操作副本。
