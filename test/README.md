# ImBoy 测试套件

> 版本: 1.0.0
> 最后更新: 2026-02-17

---

## 概述

本测试套件为 ImBoy 即时通讯系统提供完整的测试覆盖，包括：

- **单元测试**: 验证单个函数和模块的正确性
- **集成测试**: 验证功能模块之间的协作
- **性能测试**: 评估系统各项性能指标
- **压力测试**: 测试系统在高负载下的稳定性

---

## 目录结构

```
test/
├── integration/                    # 集成测试
│   ├── msg_forward_integration_tests.erl      # 消息转发测试
│   ├── msg_reply_integration_tests.erl        # 消息引用回复测试
│   ├── conversation_pin_delete_integration_tests.erl  # 会话置顶/删除测试
│   ├── msg_reaction_integration_tests.erl     # 表情回应测试
│   ├── group_notice_integration_tests.erl     # 群公告测试
│   ├── mention_integration_tests.erl          # @提及测试
│   ├── group_member_role_integration_tests.erl # 群成员角色测试
│   └── group_category_tag_integration_tests.erl # 群分组/标签测试
│
├── performance/                    # 性能测试
│   ├── msg_send_performance_tests.erl         # 消息发送性能
│   ├── db_query_performance_tests.erl         # 数据库查询性能
│   └── websocket_performance_tests.erl        # WebSocket性能
│
├── stress/                         # 压力测试
│   ├── high_concurrency_stress_tests.erl      # 高并发测试
│   └── group_member_limit_stress_tests.erl    # 群成员上限测试
│
├── api/                            # API Handler 测试
├── logic/                          # Logic 层测试
├── ds/                             # DS 层测试
├── repo/                           # Repo 层测试
├── lib/                            # Lib 层测试
│
├── common/                         # 测试公共模块
├── imboy_test_suite.erl            # 测试套件主模块
│
└── README.md                       # 本文档

scripts/
├── run_tests.sh                    # 测试运行脚本
└── benchmark.sh                    # 性能基准测试脚本
```

---

## 快速开始

### 运行所有单元测试

```bash
make eunit
```

### 运行快速测试（不需要数据库）

```bash
make test-fast
```

### 运行集成测试

```bash
./scripts/run_tests.sh integration
```

### 运行性能测试

```bash
./scripts/run_tests.sh performance
```

### 运行压力测试

```bash
./scripts/run_tests.sh stress
```

### 运行所有测试套件

```bash
./scripts/run_tests.sh all
```

### 生成覆盖率报告

```bash
./scripts/run_tests.sh coverage
```

### 运行性能基准测试

```bash
# 交互式菜单
./scripts/benchmark.sh

# 直接运行特定测试
./scripts/benchmark.sh msg      # 消息发送
./scripts/benchmark.sh db       # 数据库查询
./scripts/benchmark.sh ws       # WebSocket
./scripts/benchmark.sh all      # 所有测试
```

---

## 测试详情

### 集成测试

| 测试模块 | 测试内容 | 用例数 |
|---------|---------|--------|
| msg_forward_integration_tests | 消息转发（C2C/C2G，批量，溯源） | 10 |
| msg_reply_integration_tests | 消息引用回复（单聊/群聊，嵌套引用） | 6 |
| conversation_pin_delete_integration_tests | 会话置顶/删除/恢复 | 10 |
| msg_reaction_integration_tests | 消息表情回应（添加/移除/统计） | 9 |
| group_notice_integration_tests | 群公告（创建/更新/删除/置顶） | 8 |
| mention_integration_tests | @提及（单个/多个/所有人） | 8 |
| group_member_role_integration_tests | 群成员角色（管理员/禁言） | 9 |
| group_category_tag_integration_tests | 群分组/标签管理 | 9 |

### 性能测试

| 测试模块 | 测试内容 | 性能阈值 |
|---------|---------|---------|
| msg_send_performance_tests | 消息发送性能 | < 100ms/条 |
| db_query_performance_tests | 数据库查询性能 | < 50ms/查询 |
| websocket_performance_tests | WebSocket性能 | > 500 msg/s |

### 压力测试

| 测试模块 | 测试内容 | 负载规模 |
|---------|---------|---------|
| high_concurrency_stress_tests | 高并发消息 | 100用户 x 50消息 |
| group_member_limit_stress_tests | 群成员上限 | 1000+ 成员群组 |

---

## EUnit 测试编写

### 方法 1: 使用头文件宏（推荐）

```erlang
-module(my_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("test/include/eunit_setup.hrl").

% 简单测试（不需要应用）
my_simple_test() ->
    ?TEST_SIMPLE(fun() ->
        Result = elib_pg_sql:public_tablename(<<"user">>),
        ?assertEqual(<<"public.user">>, Result)
    end).

% 需要应用的测试
my_test_with_app() ->
    ?TEST_WITH_APP(fun() ->
        Result = my_repo:find(1),
        ?assertMatch({ok, _}, Result)
    end).

% 需要数据库的测试
my_test_with_db() ->
    ?TEST_WITH_DB(fun() ->
        Result = my_repo:find(1),
        ?assertMatch({ok, _}, Result)
    end).
```

### 方法 2: 测试夹具（Fixture）

```erlang
-module(feature_integration_tests).
-include_lib("eunit/include/eunit.hrl").

feature_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      {"测试用例1", fun test_case_1/0},
      {"测试用例2", fun test_case_2/0}
     ]
    }.

setup() ->
    application:set_env(imboy, env, test),
    % 初始化测试数据
    #{user1 => create_test_user(), group => create_test_group()}.

cleanup(_Context) ->
    % 清理测试数据
    ok.

test_case_1() ->
    ?assert(true).
```

---

## 提供的辅助功能

### test/include/eunit_setup.hrl

提供以下宏和函数：

**宏**:
- `?TEST_WITH_APP(TestFun)` - 自动启动应用的测试
- `?TEST_WITH_DB(TestFun)` - 需要数据库的测试（不可用时跳过）
- `?TEST_SIMPLE(TestFun)` - 简单测试（不需要应用）

**函数**:
- `eunit_setup/0` - 启动所有必要的应用
- `eunit_cleanup/1` - 清理资源
- `eunit_try_db/0` - 尝试建立数据库连接

### test/common/eunit_runner.erl

运行器模块：
- `eunit_runner:run()` - 运行所有测试
- `eunit_runner:run(Modules)` - 运行指定模块
- `eunit_runner:run_fast()` - 快速测试（不需要数据库）

---

## 在 Erlang Shell 中运行测试

```erlang
% 启动 Erlang Shell
erl -pa ebin -pa test

% 运行所有测试
imboy_test_suite:run_all().

% 运行集成测试
imboy_test_suite:run_integration().

% 运行性能测试
imboy_test_suite:run_performance().

% 运行压力测试
imboy_test_suite:run_stress().

% 运行单个测试模块
eunit:test(msg_forward_integration_tests, [verbose]).

% 运行单个测试用例
msg_forward_integration_tests:test_c2c_to_c2c_forward().
```

---

## 性能基准

基于测试结果，系统应达到以下性能基准：

| 指标 | 目标值 |
|-----|--------|
| 单聊消息吞吐量 | > 1000 msg/s |
| 群聊消息吞吐量 | > 500 msg/s |
| WebSocket 连接吞吐量 | > 500 conn/s |
| 数据库查询响应时间 | < 50ms (P99) |
| 系统可用性 | > 99.9% |

---

## 测试报告

测试运行后，报告将保存在 `test_reports/` 目录：

```
test_reports/
├── integration_test_report.txt    # 集成测试报告
├── performance_test_report.txt    # 性能测试报告
├── stress_test_report.txt         # 压力测试报告
└── coverage_report.html           # 覆盖率报告
```

---

## 最佳实践

1. **分类测试**:
   - 不需要数据库的测试：直接写 `test()` 函数
   - 需要应用的测试：使用 `?TEST_WITH_APP`
   - 需要数据库的测试：使用 `?TEST_WITH_DB`

2. **测试命名**:
   - 简单测试：`<function>_test()`
   - 需要 setup：`<function>_test_()`

3. **快速开发**:
   - 开发时使用 `make test-fast` 快速反馈
   - 提交前使用 `make eunit` 完整测试

4. **测试隔离**:
   - 每个测试都是独立的
   - 使用 setup/cleanup 管理测试数据

---

## 故障排查

### 测试失败常见原因

1. **数据库连接失败**
   - 检查 PostgreSQL 是否运行
   - 检查数据库配置

2. **测试超时**
   - 增加测试超时时间
   - 检查系统资源使用情况

3. **依赖模块未加载**
   - 运行 `make compile` 重新编译
   - 检查 ebin 目录是否在代码路径中

### 查看详细日志

```bash
# 启用详细日志
erl -pa ebin -pa test -eval "application:set_env(imboy, env, test), eunit:test(Module, [verbose])"
```

---

## 清理测试产物

```bash
make eunit-clean
```

---

## 贡献指南

### 添加新测试

1. 在相应目录创建测试文件
2. 遵循命名规范: `*_tests.erl`
3. 使用 EUnit 测试框架
4. 在 `imboy_test_suite.erl` 中注册测试

---

## 联系方式

如有问题，请联系开发团队或提交 Issue。
