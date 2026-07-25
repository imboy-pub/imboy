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

## 备份与恢复

| 脚本 | 用途 |
|------|------|
| `backup_pg.sh` / `restore_pg.sh` | PostgreSQL 备份/恢复（内置 timescaledb pre/post_restore 包裹） |
| `backup_garage.sh` | Garage S3 附件备份 |
| `backup_imboy_db.sh` | 业务库备份 |

> 恢复演练记录见 [restore-drill-2026-06.md](../docs/guides/operations/deployment/restore-drill-2026-06.md)。

## Garage S3

`garage-install.sh`（安装）、`garage-local-setup.sh`（本地桶/密钥初始化）、`garage_e2e_test.sh`（端到端验证）。

## 测试与冒烟

`smoke/`（c2c/ws/ctl 冒烟）、`smoke_extended.sh`、`run_feature_flag_smoke.sh`、`rtc_e2e_test.sh`、`sso_oidc_e2e.sh`、`bench_websocket.sh`（WS 压测）、`payment_credit_in_tx_it.sh`（支付集成测试）、`verify_device_api_sql.sh`。

## 校验与诊断

`check_module_boundaries.sh`（四层边界门禁）、`check_dco.sh`、`check_duplicate_modules.sh`、`check_server_zero_crypto.sh`、`validate_p5_manifest.sh`、`sanity_check.sh`、`erl_crashdump_analyzer.sh`（崩溃转储分析）。

## 其他

`channel_daily_digest.sh`（频道日报）、`plugin_install.sh`（插件安装）、`gen_license.escript`（License 生成）、`sso/`（SSO 相关）、`fdfs_reference_census.sql` + `migrate_fdfs_avatars.erl`（FastDFS 历史迁移残留，一次性用途）。
