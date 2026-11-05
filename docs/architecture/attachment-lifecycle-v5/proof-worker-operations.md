# 企业附件独立扫描 Worker

企业 direct 资产必须有独立 Worker 扫描 winner 后写入的 proof，应用绑定事务再按锁后状态核对 proof。应用数据库角色不能写 proof，也不能取得 proof 身份。扫描未完成、失败或证明过期时保持待确认。

Worker 必须由独立 OS 用户或等价隔离单元启动，应用用户不能读取其环境、配置、进程和临时目录。单独注入 PostgreSQL 登录和只读 Garage 凭据；不要把凭据放入命令行、日志或仓库。数据库管理员在完成权限审查后，配置 `imboy_eb_proof_worker` 登录（NOINHERIT、无超级用户及角色管理权限），只授 `eb_direct_proof_worker` 成员资格。运行时角色及其登录不得直接或间接成为这两个角色的成员。

启动 `scripts/enterprise_direct_proof_worker.sh`。Worker 环境需要 `EB_PROOF_PG_HOST`、`EB_PROOF_PG_PORT`、`EB_PROOF_PG_DB`、`EB_PROOF_PG_USER`、`EB_PROOF_PG_PASSWORD`、`EB_PROOF_PG_SSL`，以及 `EB_PROOF_HEALTH_KEY`（该 bucket 中由运维预置、proof 可读的健康探针对象），`EB_PROOF_STORAGE_NAMESPACE`、`EB_PROOF_STORAGE_ENDPOINT`、`EB_PROOF_STORAGE_CONTAINER`、`EB_PROOF_STORAGE_ACCESS_KEY`、`EB_PROOF_STORAGE_SECRET_KEY`。对象存储定位必须与资产冻结 registry 一致。`EB_PROOF_PG_SSL` 明确设置为 `true` 或 `false`；仅隔离的本地测试使用 `false`。

源码目录默认查找 `deps/*/ebin`。OTP release 通过 `EB_PROOF_SCRIPT` 指定当前 release 的 `lib/imboy-<version>/priv/workers/enterprise_direct_proof.escript`，并通过 `EB_PROOF_CODE_PATHS` 指定当前 release 依赖 ebin 路径，使用冒号分隔；不得混入旧版本。配置及进程隔离需由部署环境实际验证。

失败任务记录在数据库并按退避时间重试；后续任务不被前 20 个失败项长期阻塞。Worker 日志只记录失败阶段，不记录凭据、签名 URL 或附件内容。

容器部署通过独立服务运行，Compose 使用 `enterprise-direct` profile；Helm 设置 `enterpriseProof.enabled=true` 并指定独立 `enterpriseProof.existingSecret`，Secret 的键为上述 `EB_PROOF_*` 环境名，不能复用后端 Secret。须为该身份另建只读存储凭据和仅有 `eb_direct_proof_worker` 成员资格的数据库登录。企业 direct 启用时必须同时启用此服务；缺配置、身份不安全或扫描异常时不能生成 proof，确认维持待处理。

`fn_eb_direct_worker_ready(namespace, locator_sha256)` 是数据库可读的短时在线信号，仅由 proof 身份刷新；它不能替代内容扫描证明。启用企业 direct 的控制链必须同时检查该信号，缺失或过期时失败关闭。Helm 将 `enterpriseProof.enabled` 同时映射给后端开关并启动独立 Deployment。社区 Compose 默认关闭 direct；若启用 direct，必须同时启用 `enterprise-direct` profile 并提供独立凭据。
