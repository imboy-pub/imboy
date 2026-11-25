# R0 容量治理回滚方案（r0-capacity-rollback）

> 版本 1.0 | 2026-11-24 | R0 阶段 G 交付物
> 对应实施：分支 `fix/r0-capacity-limits-20261123`（基于 main `a5c74219f`）
> 设计依据：`r0-capacity-remediation-design.md` §10 + 属主批准（附录 §13）

## 0. 原则

- 全程按**逻辑单元**拆分提交，每个单元可独立 `git revert`；
- 不使用 `git reset --hard` / `git clean`；
- 运行期回退优先于代码回退（不重建镜像即可生效的通道 = `ERL_FLAGS` 注入与 `IMBOY_MAX_WS_CONNECTIONS`）。

## 1. 各逻辑单元的回滚方式

| 逻辑单元 | 提交内容 | 代码回退 | 运行期回退（无需重建） |
|---|---|---|---|
| E1 ws_admission 准入模块 + 测试 | 新增 `src/lib/ws_admission.erl`、`test/lib/ws_admission_tests.erl` | `git revert <commit>`（纯新增文件，revert 即删除） | **不可运行期关闭**（无"无限制"取值，属属主约束）；上限可经 `IMBOY_MAX_WS_CONNECTIONS` 调大至合法上界 1000000 |
| E2 handler 接入 + 监听器钉住 | `websocket_handler.erl`（准入闸/503/释放/删死键）、`imboy_app.erl`（钉 1024×10/校验/init_table/start_tls map 重构）、`imboy_env.erl`（整数覆盖） | `git revert <commit>`（行为回到现状等价态） | 同上；WS 上限调大即接近放行全部 |
| E3 指标修复 + 水位告警 | `metrics_handler.erl`（换源 + 准入 gauge）、`deploy/prometheus/rules/imboy-alerts.yml` | `git revert <commit>`；注意换源后 `imboy_ws_connections_current` 语义从 ~0 跳变为真实连接数，`ImBoyWSConnectionsDrop`（较 10min 前跌 >50% 且 >10）随之恢复有效——面板阈值如曾按失真值调过需同步复核 | 告警可在 Prometheus 侧临时 silence，不必回滚代码 |
| E4 VM 参数默认值 | `config/vm.args`、`scripts/start_node.sh`（+P/+Q 20480→65536） | `git revert` + 重建镜像 | **运行期临时回退**：`.env` 设 `IMBOY_MAX_PROCESSES=20480 IMBOY_MAX_PORTS=20480` 后重跑 `deploy/install.sh`（经 `ERL_FLAGS` 生效，实测 ERL_FLAGS 覆盖 args_file，无需重建镜像） |
| E5 compose 注入 + install.sh | `docker-compose.community.yml`、`docker-compose.demo.yml`（删死 `ERL_MAX_PORTS` + 注入）、`deploy/.env.example`、`deploy/install.sh`（`ensure_capacity_vars`） | `git revert`；已写入 `.env` 的 `IMBOY_ERL_FLAGS` 需手工删除该行（或置空） | compose 不经 install.sh 直接 up 时 `ERL_FLAGS: ${IMBOY_ERL_FLAGS:-}` 为空 → 使用镜像内置 vm.args 派生默认值（同为 65536），两条路径一致 |
| E5b review 修正：`.env` 可 source | `deploy/install.sh`（带引号写 `IMBOY_ERL_FLAGS`）、`deploy/.env.example` 注释、`scripts/test/install_one_click_test.sh` 新增断言（提交 `282c3c57c`） | **不要单独 revert**：回退会恢复"全新安装首次 TLS 签发中断"的回归（提交 45778a1a4 的首版行为）。若要撤掉整条注入链，按 E5 整体回退 | 无需（行为修正） |
| E2b review 修正：`ws_admission` | `src/lib/ws_admission.erl`、`test/lib/ws_admission_tests.erl`（提交 `ff15a3eae`） | `git revert ff15a3eae`：准入语义不变，仅回到"重复 init 不取消旧定时器"的首版（生产只 init 一次，无运行期差异），并重新引入 elvis 3 条 / dialyzer 2 条 | — |
| 测试扩展 | `websocket_connection_flow_SUITE.erl` admission 组、`test/lib/imboy_capacity_config_tests.erl` | `git revert`（无生产影响） | — |
| 文档 | 设计/状态/回滚/测试报告/RESUME | `git revert` | — |

## 2. 快速回退矩阵（生产事故口径）

| 症状 | 第一响应（运行期） | 代码回退 |
|---|---|---|
| 误拒风暴（503 过多） | `.env` 调大 `IMBOY_MAX_WS_CONNECTIONS`（≤1000000）→ 重启 backend 容器 | revert E2 恢复无准入闸 |
| 进程/端口压力（+P 撞顶风险） | `.env` 调大 `IMBOY_MAX_PROCESSES/PORTS`（≤134217727）→ 重跑 install.sh → 重启 | revert E4 回 20480（实际 32768） |
| install.sh 注入异常 | 手工删 `.env` 中 `IMBOY_ERL_FLAGS` 行后直接 `docker compose up -d` | revert E5 |
| 告警误报 | Prometheus silence `ImBoyWSAdmissionHigh` | revert E3 |
| 启动失败（invalid_config max_ws_connections） | `.env` 修正为 1..1000000 内整数 | revert E1+E2 |

## 3. 回滚验证清单

1. `bin/imboy` 启动后日志含 `[capacity] process_limit=… port_limit=… max_ws_connections=…` 行，数值与期望一致；
2. `/metrics`（内网）含 `imboy_ws_admission_current/limit`；
3. 一条真实 WS 可建立、可关闭，`imboy_ws_admission_current` 回落；
4. REST API（登录）正常。
5. 安装器写入的 `IMBOY_ERL_FLAGS` 能被脚本原样读回：`bash -c 'set -eu; set -a; . ./.env; echo "$IMBOY_ERL_FLAGS"'` 输出完整的 `+P … +Q …`（值在 `.env` 里带一对双引号；不带引号时 `nginx/init-letsencrypt.sh` 首签会中断）。

## 4. 与其他部署路径的关系（本批不改，记录口径）

- 蓝绿（`blue_green_deploy.sh`，+P/+Q 1048576）与 native-shell（`render.sh`，262144/65536）：数值高于派生下限，本批不改；回滚本分支不影响这两条路径。
- `docker-compose.prod.yml` 为主检出未跟踪文件（属主部署入口）：本分支未改动；如需套用同款注入（删 `ERL_MAX_PORTS` + `ERL_FLAGS`/`IMBOY_MAX_WS_CONNECTIONS` 透传），按属主指示单独处理。
- `config/vm_local.args` 为机器本地未跟踪文件，本分支未改动。
