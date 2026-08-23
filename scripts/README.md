# imboy 运维脚本 / Operations Scripts

> 后端开发、部署、备份、诊断脚本汇总。生产部署完整流程见
> [deploy/README.md](../deploy/README.md) 与 [docs/guides/operations/deployment/](../docs/guides/operations/deployment/)。

## 开发环境

| 脚本 | 用途 |
|------|------|
| `dev_setup.sh` | 本地开发环境初始化 |
| `seed_demo.sh` / `seed_ai_personas.sh` | 灌入演示数据 / AI 角色数据 |
| `start_node.sh` / `stop_node.sh` | 本地节点启停 |
| `onboarding_setup.sh` / `verify_onboarding.sh` | 新部署初始化与自检 |

## 部署与节点控制

| 脚本 | 用途 |
|------|------|
| `deploy.sh` | 生产部署入口（配合 `deploy/` 目录使用） |
| `imboy-deploy.sh` | 蓝绿部署：`all` / `api` / `admin` / `migrate` / `rollback` |
| `imboy_ctl` | 节点 CLI（迁移、冒烟、状态），环境变量 `IMBOY_CTL_NODE` / `IMBOY_CTL_COOKIE` |

```bash
export IMBOY_CTL_NODE=imboy_dev@127.0.0.1
export IMBOY_CTL_COOKIE=imboycookie
escript scripts/imboy_ctl db migrate
```

### 蓝绿发布的迁移时序

`imboy-deploy.sh all/api` 会在新节点启动前执行 `.env.deploy` 中
`DEPLOY_EXPAND_MIGRATIONS` 列出的可加性迁移；本版本必须包含
`00000064_msg_store_sender_did.up.sql`，并验证 `public.msg_store.sender_did`
已存在。随后以 `IMBOY_AUTO_MIGRATE=false` 启动新节点，避免 application boot
抢先执行完整迁移；该值也会写入 release 的 `sys.config`，普通重启不会恢复
自动迁移。新节点通过 `/healthz`、切换 Nginx 后，脚本停止旧节点并确认端口
关闭（Nginx reload 不会断开既有 WebSocket），最后才显式执行完整 `db migrate`。

`DEPLOY_EXPAND_MIGRATIONS` 只允许放入已经完成兼容性评审的 expand SQL，不能
把删除列/表等 contract 迁移提前。

非蓝绿的首次安装或本地启动仍默认自动迁移；如需自行编排迁移时序，可显式设置
`IMBOY_AUTO_MIGRATE=false`，并负责在节点启动后调用 `imboy_ctl db migrate`。
统一入口的 `all`/`api` 模式均由底层 `deploy.sh` 在同一流程完成切流、停止旧节点
和显式迁移；`migrate` 模式仅用于独立补跑，不参与 `all` 的正常时序。独立补跑前
会校验 Nginx 正指向目标版本、另一蓝绿端口已关闭；无法可靠证明时拒绝迁移。

`deploy.sh --no-migrate` 只用于发布对当前 schema 完全兼容的代码：它会强制保留旧节点，
不会授权紧接着执行完整迁移。若之后使用独立 `migrate` 补跑，必须先停止另一色旧节点，
入口会重新核对监听状态。反之，正常完整迁移必须停止旧节点及既有长连接，因此不接受
`IMBOY_DEPLOY_STOP_OLD=false`；迁移开始后若失败，须先核对已应用 schema，再决定是否
人工恢复旧版本，不能自动切回。

管理后台远端目录固定限制在 `/www/wwwroot/<站点>`，且部署前必须由管理员在真实目录
创建 `.imboy-admin-root` 标记文件；上传脚本会校验 `realpath` 与标记，并在同步时保留
该标记，避免 `--delete` 或清理回退误作用于系统目录或符号链接目标。

## 备份与恢复

| 脚本 | 用途 |
|------|------|
| `backup_pg.sh` / `restore_pg.sh` | PostgreSQL 备份/恢复（内置 timescaledb pre/post_restore 包裹） |
| `backup_garage.sh` | Garage S3 附件备份 |
| `backup_imboy_db.sh` | 业务库备份 |
| `restore_smoke.sh` | 备份恢复冒烟（验证可恢复性） |

> 恢复演练记录见 [restore-drill-2026-06.md](../docs/guides/operations/deployment/restore-drill-2026-06.md)。

## Garage S3

`garage-install.sh`（安装）、`garage-local-setup.sh`（本地桶/密钥初始化）、`garage_e2e_test.sh`（端到端验证）。

## 测试与冒烟

`smoke/`（c2c/ws/ctl 冒烟）、`smoke_extended.sh`、`run_feature_flag_smoke.sh`、`rtc_e2e_test.sh`、`sso_oidc_e2e.sh`、`bench_websocket.sh`（WS 压测）、`payment_credit_in_tx_it.sh`（支付集成测试）、`paid_channel_fixture.sh`（付费频道闭环 fixture）、`verify_device_api_sql.sh`。

## 校验与诊断

`check_module_boundaries.sh`（四层边界门禁）、`check_dco.sh`、`check_duplicate_modules.sh`、`check_server_zero_crypto.sh`、`check_release_consistency.sh`（商业化发布一致性门禁）、`check_tls_expiry.sh`（TLS 证书到期检查）、`validate_p5_manifest.sh`、`sanity_check.sh`、`erl_crashdump_analyzer.sh`（崩溃转储分析）、`websocket_diagnose.sh`（WS 连接逐层诊断：端口→HTTP→握手→在线数）。

钱包约束有两级数据库门禁：`verify_wallet_constraint_sql.sh` 在一次性 PostgreSQL 18
合成实例验证 SQL 语义；`verify_wallet_constraint_clone.sh` 默认只读预检，只有在显式
确认后才对生产规模的隔离可写克隆执行 65/66 正反迁移、锁兼容与恢复演练。后者的操作
手册见 [wallet-constraint-clone-acceptance.md](../docs/guides/operations/wallet-constraint-clone-acceptance.md)。
其离线 fail-closed 守卫已纳入 `check_release_consistency.sh` 和 Backend CI；CI 只证明
脚本守卫与编排，不能代替生产规模隔离克隆证据。

API 契约门禁（P3-C2）：`contract_gate.py` 从 `src/imboy_router.erl`、
`priv/migrations` CHECK 约束、`include/error_code.hrl` 静态导出
`.contract/api_contract.json`（确定性产物，内容不变则文件不变），并校验
admin/flutter 枚举镜像与 EntityId/TSID 规则。用法：`make contract-export`
（合法变更时同 PR 重导出）/ `make contract-check`（漂移非零退出；可传
`ADMIN_DIR=` / `FLUTTER_DIR=`，缺省自动探测同级仓）。CI 见
`.github/workflows/contract-gate.yml`。

## 其他

`channel_daily_digest.sh`（频道日报）、`plugin_install.sh`（插件安装）、`gen_license.escript`（License 生成）、`sso/`（SSO 相关）、`fdfs_reference_census.sql` + `migrate_fdfs_avatars.erl`（FastDFS 历史迁移残留，一次性用途）。

发布门禁与审计：`evidence_manifest.sh`（发布证据清单，审计就绪包附录 B）、`license_inventory.sh`（第三方依赖许可证清单，D3 门）、`validate_sales_release_config.escript`（销售版功能策略门禁，不读取密钥）、`recrypt_user_collect.escript`（A-06 主密钥轮换，`user_collect.info` 清洗）。`test/` 为脚本自测目录（alertmanager 渲染、异地备份、恢复守卫等）。
