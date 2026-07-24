# imboy

基于 Cowboy、Erlang/OTP 与 PostgreSQL 的 IMBoy 即时通讯后端服务。

长期维护的架构、接口与运维说明以仓库内核心文档为准，不再在根 README 堆积一次性计划、模板教程或环境专属样例。

许可证：木兰宽松许可证，第 2 版，见 `./LICENSE`。

## 核心入口

- 架构概览：`docs/architecture/overview.md`
- 数据访问：`docs/architecture/database-access.md`
- 文档索引：`docs/README.md`
- 测试说明：`test/README.md`
- REST API：`docs/reference/rest-api.md`
- WebSocket API：`docs/reference/websocket-api-2.md`

## 环境依赖

- Erlang/OTP 28+
- PostgreSQL 18+
- 依赖管理：`Makefile` + `include/deps.mk` + `erlang.mk`
- 示例配置：`config/sys.config.example`

依赖与运行说明详见 `docs/guides/operations/dependencies.md`。

## 常用开发命令

```bash
make compile
make eunit
make ct
make tests
make dialyze
bash script/check_module_boundaries.sh

IMBOYENV=local make run
IMBOYENV=local make rel
IMBOYENV=local make relup
```

说明：

- `make eunit` 默认使用 `config/sys.config`，可通过 `EUNIT_CONFIG=config/sys.local.config` 指定配置。
- 提交迁移前，按 `docs/reference/engineering/migration_naming.md` 做命名自检。
- 功能开关 smoke 校验可直接使用 `script/run_feature_flag_smoke.sh`，或通过 `make feature-smoke` 触发。

## Architecture Gates / 架构门禁

- 后端边界检查：`bash script/check_module_boundaries.sh`
- 编译门禁：`make compile`
- 约束范围：已迁移的 HTTP handler 只能依赖当前登记的领域 facade 或兼容层，新增跨域 `*_repo` / `*_ds` / `*_logic` 直连会直接失败

## 工程化基线

- CI 工作流：`.github/workflows/backend-ci.yml`
- 发布辅助脚本：`script/deploy.sh`
- 功能开关 smoke：`script/run_feature_flag_smoke.sh`
- 节点启停脚本：`script/start_node.sh`、`script/stop_node.sh`

默认门禁以 `make compile`、`make eunit`、`make ct`、`make dialyze` 为准。

## 远程 Shell 与运行维护

连接 release 节点：

```bash
_rel/imboy/bin/imboy remote_console
```

本地开发常见维护动作：

```erlang
config_ds:reload().
config_ds:local_reload().

Routes = imboy_router:get_routes(),
Dispatch = cowboy_router:compile(Routes),
cowboy:set_env(imboy_listener, dispatch, Dispatch).
```

说明：

- `config_ds:reload/0` 与 `config_ds:local_reload/0` 用于重新加载配置。
- 路由热更新仅适用于明确知道影响范围的开发调试场景。

## 分布式节点调试

按仓库现有脚本启动节点：

```bash
make start node=node1 port=9801
make start node=node2 port=9802 cookie=imboycookie
make stop node=node1
```

验证节点连通性：

```erlang
net_adm:ping('node2@127.0.0.1').
net_adm:names().
```

`imboy_syn` 相关联调可直接在 shell 中调用：

```erlang
imboy_syn:init().
imboy_syn:join(1, <<"ios">>, self(), <<"did11">>).
imboy_syn:list_by_uid(1).
imboy_syn:publish(1, <<"hello from node2">>).
```

## 测试入口

推荐统一走 `Makefile` 和现有测试目录，不再新增仓库级包装脚本。

```bash
make eunit
make ct
make tests

make eunit EUNIT_MODS='router_consistency_tests app_feature_handler_tests'
make ct-msg_ack_logic
```

补充脚本入口：

- `test/api/test_api.sh`
- `test/api/test_edge_cases.sh`
- `test/logic/run_device_session_tests.sh`

详细约定见 `test/README.md`。

## 发布

构建 release：

```bash
IMBOYENV=prod make rel
IMBOYENV=prod make relup
```

使用发布脚本：

```bash
./script/deploy.sh <host> <new_version> <old_version>
```

手工部署 release 产物：

```bash
mkdir -p /usr/local/imboy
cp ./_rel/imboy/imboy-<version>.tar.gz /usr/local/imboy/
cd /usr/local/imboy
tar -xzf imboy-<version>.tar.gz

bin/imboy daemon
bin/imboy remote_console
bin/imboy restart
bin/imboy stop
```

版本升级常见命令：

```bash
./bin/imboy versions
./bin/imboy upgrade <new_version>
./bin/imboy downgrade <old_version>
./bin/imboy uninstall <old_version>
```

具体部署约束与环境变量说明见 `docs/guides/deployment.md`。

## 相关仓库

- 移动端：`../imboyapp`
- 管理后台：`../imboy-admin-frontend`
