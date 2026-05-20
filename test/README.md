# ImBoy 测试说明

> Last Updated: 2026-03-09  
> Scope: 当前仓库测试目录、推荐入口与维护约定

## 测试形态

当前仓库主要有三类测试：

- `EUnit`：覆盖 `adm/`、`api/`、`logic/`、`ds/`、`repo/`、`lib/` 等模块级测试；
- `Common Test`：根目录下的 `*_SUITE.erl` 流程测试；
- 手工脚本：少量补充脚本放在 `test/api/`、`test/logic/` 中，用于特定联调或烟测。

## 目录结构

```text
test/
├── adm/                           # 后台 Handler / Middleware / Logic 测试
├── api/                           # App 侧 HTTP / WebSocket Handler 测试
├── common/                        # 测试辅助模块
├── docs/                           # 历史压测图文资料与现场记录
├── ds/                            # DS 层测试
├── integration/                   # 端到端 / 集成场景测试（EUnit 风格）
├── lib/                           # 公共库测试
├── logic/                         # 业务逻辑测试
├── performance/                   # 性能相关测试模块
├── repo/                          # Repo 层测试
├── *_SUITE.erl                    # Common Test suites
├── test_quality_improvement_tasklist.md  # 历史测试质量专项记录
└── README.md                      # 本文档
```

当前仓库可见的 Common Test suites：

- `friend_management_flow_SUITE.erl`
- `group_management_flow_SUITE.erl`
- `messaging_flow_SUITE.erl`
- `msg_ack_logic_SUITE.erl`
- `msg_delivery_SUITE.erl`
- `user_auth_flow_SUITE.erl`
- `websocket_connection_flow_SUITE.erl`

## 历史资料

仓库保留了少量历史资料，便于容量评估、问题回溯和测试方法复用：

- `test/docs/test1.md`：100 万级 WebSocket 在线连接压测记录，含服务端 / 客户端监控截图
- `test/test_quality_improvement_tasklist.md`：早期测试质量专项记录，适合作为治理背景材料，不作为当前实时统计口径

说明：历史资料属于归档保留内容，不直接作为 2026 年对外交付 SLA、容量承诺或当前门禁覆盖率口径。

## 推荐入口

### 1. 运行全部 EUnit

```bash
make eunit
```

### 2. 聚焦运行少量模块

```bash
make eunit EUNIT_MODS='app_feature_handler_tests adm_admin_feature_config_tests router_consistency_tests'
```

### 3. 指定测试配置

```bash
make eunit EUNIT_CONFIG=config/sys.local.config
```

说明：`Makefile` 会把 `EUNIT_CONFIG` 转成 `erl -config` 所需格式，并在测试启动前 `load` 应用、设置 `env=test`。

### 4. 运行全部 Common Test

```bash
make ct
```

### 5. 运行单个 Common Test suite

```bash
make ct-msg_ack_logic
make ct-msg_delivery
```

### 6. 运行全部测试

```bash
make tests
```

## 补充脚本

当前仓库仍保留少量定向脚本：

- `test/api/test_api.sh`
- `test/api/test_edge_cases.sh`
- `test/logic/run_device_session_tests.sh`

这些脚本不再作为统一测试入口；新增测试时，优先补到 `EUnit` 或 `Common Test`。

## 性能测试说明

性能相关模块位于 `test/performance/`，例如：

- `msg_send_performance_tests.erl`
- `db_query_performance_tests.erl`
- `websocket_performance_tests.erl`
- `channel_perf_benchmark.erl`
- `channel_ws_push_benchmark.erl`

建议按模块聚焦运行，而不是依赖额外包装脚本，例如：

```bash
make eunit EUNIT_MODS='msg_send_performance_tests db_query_performance_tests websocket_performance_tests'
```

## 编写约定

- 测试文件命名保持 `<module>_tests.erl`；
- API / Logic / Repo / DS 测试尽量放回对应层级目录；
- 流程性、多步骤、跨模块验证优先使用 `Common Test`；
- 新增测试入口时，优先复用 `make eunit`、`make ct`、`make tests`，避免重新引入仓库级包装脚本。

## 维护规则

- 如果删除某个测试脚本或入口，必须同步更新本文档；
- 如果新增长期保留的 suite 或目录结构，优先补本文档，不单独新建“临时测试说明”；
- 如果某类测试只服务当前阶段，优先记录在阶段性文档或 issue，不长期挂在测试主说明里。
