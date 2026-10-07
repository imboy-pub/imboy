# 新机器运行手册模板（AC27 四入口）

> 状态：current（模板） | 本文件是**脱敏模板**：所有 `__PLACEHOLDER____` 由部署者替换，
> 绝不在此或任何交付物中填写真实值。

## 配置入口（Configure）

1. `cp config/sys.example.config config/sys.local.config`（或按部署环境选 pro 模板），
   替换以下占位：

```erlang
%% config/sys.local.config（节选，全量键见 config/sys.example.config 注释）
{imboy, [
    {http_port, __HTTP_PORT__},
    {pg_conf, #{
        host => "__PG_HOST__",
        port => __PG_PORT__,
        database => "__PG_DB__",
        username => "__PG_USER__",
        password => "__PG_PASSWORD__"
    }},
    {jwt_key, "__JWT_SECRET__"},
    {solidified_key, "__SOLIDIFIED_KEY__"}
    %% 其余键：redis/garage/push 等，见 example 注释
]}
```

2. 数据库初始化：`make migrate`（erlang_migrate 驱动；回滚策略见「恢复入口」）。
3. 附件存储：参照 `deploy/` 与 `scripts/garage-local-setup.sh`（生产拓扑见 deploy/README.md）。

## 启动入口（Start）

```bash
IMBOYENV=local make run          # 本地
IMBOYENV=pro make rel            # 生产 release 打包（产物不含 sync 等开发依赖）
# 生产容器化：cd deploy && cp .env.example .env && $EDITOR .env && bash preflight.sh \
#   && docker compose -f docker-compose.prod.yml up -d
```

健康确认：`make ctl ARGS="smoke all"`（冒烟）与 `scripts/smoke/` 套件。

## 验收入口（Verify）

- 静态门：`make compile && make dialyze`
- 单测（targeted）：`make eunit-local t=<模块>`
- 全量回归与终验门：`scripts/test/phase1_gate_all.sh --list`（二十门总控；执行细则见
  `scripts/test/phase1_gates/`，需绑定隔离库与端口表后运行）
- 接口冒烟：REST/WS 契约见 `docs/reference/`（rest-api-v1-catalog、websocket-api-2）。

## 恢复入口（Recover）

- 备份：`scripts/backup_pg` / `scripts/backup_imboy_db` / `scripts/backup_garage`
- 恢复：`scripts/restore_pg`（演练记录见 `docs/guides/operations/deployment/backup-restore.md`）
- 升级/回退：`docs/guides/operations/upgrade-runbook.md`（迁移不可逆时走备份恢复路线，
  不假称 down 脚本即回滚）
- 故障排查：`scripts/websocket_diagnose`、`scripts/erl_crashdump_analyzer`

## 四入口之外的纪律

- 真实凭据只存在于 `config/sys.local*`（永不入 git 与交付包，见
  `scripts/deliver/make_delivery_pack.sh` 排除规则 X1）。
- 变更交付候选后必须重新走 Q12 冻结流程（AC30：三仓 SHA/构建/配置脱敏 hash 重录）。
