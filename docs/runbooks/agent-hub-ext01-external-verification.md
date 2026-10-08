# Agent Hub 隔离环境操作 / Agent Hub isolated-environment operations

本手册说明 MCP 客户端诊断脚本的鉴权与运行条件。Agent Hub 属于第二期；一期交付保持入口关闭。本手册不声明任何候选版本已经通过验收。

This guide describes authentication and runtime conditions for the MCP client diagnostic script. Agent Hub belongs to phase two and remains disabled in phase-one deliveries. This guide does not certify any candidate.

## §0 运行条件 / Runtime conditions

- 在已授权的隔离环境内使用自有合成测试账号；外部地址、设备及外向动作须在对应授权范围内。
- webhook receiver 指向隔离环境，避免触达第三方。真实用户、生产数据和凭证不得写入仓库。
- 当前候选必须重新验证本地前置条件，不能沿用历史通过声明。

- Use owned synthetic accounts in an authorized isolated environment; addresses, devices, and external actions must remain within their authorization scope.
- Keep webhook receivers inside the isolated environment. Keep real-user data, production data, and credentials outside Git.
- Recheck local prerequisites for the current candidate rather than relying on historical claims.

### 0.2 admin 签名 cookie 对（ADM_UID / ADM_SIG）

admin API 鉴权为 cookie 对：`adm_user_id` + `adm_user_sig`。签名值是
**elib_hasher:hmac_sha256 的 base64 输出（88 字符 ASCII，含 +/=）**，可直接经
环境变量传递。两种获取方式：

1. **浏览器 DevTools**：用管理员账号登录 admin 控制台 → DevTools → Application →
   Cookies → 复制 `adm_user_id` 与 `adm_user_sig` 的值。
2. **服务端铸造**（在目标环境节点同版本代码上执行；必须与运行节点用同一份
   ebin，否则签名口径可能不一致）：
   ```bash
   erl -noshell -boot no_dot_erlang -config config/sys.<env> \
     -pa ebin $(for d in deps/*/ebin; do echo -n "-pa $d "; done) \
     -eval 'application:load(imboy),
            io:format("~s~n", [adm_auth_middleware:sign_admin_cookie(<<"<管理员uid>">>)]),
            halt(0).'
   ```

脚本注入：`export ADM_UID=<uid> ADM_SIG=<签名>`（或 `ADM_SIG_HEX` 传 hex 形态的
任意字节值）；凭证只经环境变量，不落盘。

Authentication uses the administrator cookie pair `adm_user_id` and `adm_user_sig`. Obtain it from an authorized isolated-environment login or generate it with that environment's matching server code. Pass credentials through environment variables and avoid printing them in logs.

## MCP 脚本 / MCP script

```bash
python3 scripts/agent_hub_ext01_mcp_client_smoke.py --self-test
```

自检验证脚本自身，不能证明产品链路通过。实际运行前阅读脚本参数及目标环境授权要求，设置 `IMBOY_BASE_URL`、`ADM_UID`、`ADM_SIG`，将 `--out` 指向仓库外的受控目录。非 loopback 目标须具备对应授权，才可设置 `EXT01_AUTHORIZED=yes`。

The self-test checks the script itself. It does not qualify the product. Review arguments and target authorization before supplying `IMBOY_BASE_URL`, `ADM_UID`, and `ADM_SIG`. Direct `--out` to a controlled directory outside Git. Set `EXT01_AUTHORIZED=yes` only when a non-loopback target is authorized.

## 安装与恢复 / Installation and recovery

安装以 `deploy/README.md` 为准；数据库迁移以目标版本实际迁移文件为准，不使用固定历史版本号判定升级完成。备份和恢复使用 `scripts/backup_pg.sh`、`scripts/backup_garage.sh` 与 `scripts/restore_pg.sh`，并在隔离环境核对数据库、附件及业务状态的一致性。

Follow `deploy/README.md` for installation and the target release's migration files for upgrades. Use the PostgreSQL and Garage backup scripts and `scripts/restore_pg.sh`, then verify database, attachment, and business-state consistency in isolation.

## 运行记录 / Run records

执行计划、测试材料、截图、日志、数据库导出及验收报告全部保存在仓库外。记录实际候选 SHA、命令、退出码、行为断言与产物哈希；未执行或缺少证据的检查不能填为通过。导出前移除凭证和个人信息。

Keep execution plans, test materials, screenshots, logs, database exports, and acceptance reports outside Git. Record actual candidate SHAs, commands, exit codes, behavioral assertions, and artifact hashes. Never mark unexecuted or unsupported checks as passed. Remove credentials and personal information before exporting records.
