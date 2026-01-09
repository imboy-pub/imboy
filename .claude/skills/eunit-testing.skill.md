# EUnit Testing Skill for Imboy

## Description
imboy 项目的 EUnit 单元测试技能集，包含测试框架使用、问题诊断和最佳实践。

## 🚀 快速开始

### 运行所有测试
```bash
make eunit
```

### 运行单个模块测试
```erlang
% 在 Erlang shell 中运行
eunit_runner:run([config_ds_tests]).
eunit_runner:run([imboy_pg_sql_tests]).
```

### 运行快速测试（不需要数据库）
```erlang
eunit_runner:run_fast().
```

## 📁 测试文件结构

```
test/
├── api/              # API Handler 层测试
├── ds/               # Data Service 层测试
├── logic/            # Logic 层测试
├── repo/             # Repository 层测试
├── lib/              # 基础库测试
├── common/           # 通用测试辅助模块
│   ├── eunit_runner.erl    # 测试运行器
│   ├── test_helper.erl     # 测试辅助函数
│   └── meck_helper.erl     # Mock 辅助
└── include/
    └── eunit_setup.hrl     # 测试宏定义
```

## 📝 编写测试

### 基础模板

```erlang
-module(my_module_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% ===================================================================
%% 简单测试（不需要应用启动）
%% ===================================================================

my_simple_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = my_module:my_function(),
        ?assertEqual(expected, Result)
    end).

%% ===================================================================
%% 需要应用的测试
%% ===================================================================

my_app_test_() ->
    ?TEST_WITH_APP(fun() ->
        Result = my_module:my_function(),
        ?assertMatch({ok, _}, Result)
    end).

%% ===================================================================
%% 需要数据库的测试（数据库不可用时自动跳过）
%% ===================================================================

my_db_test_() ->
    ?TEST_WITH_DB(fun(Conn) ->
        Result = my_repo:find(1, Conn),
        ?assertMatch({ok, _}, Result)
    end).

%% ===================================================================
%% 使用 Mock 的测试
%% ===================================================================

my_mock_test_() ->
    ?WITH_MECK(my_dep, [
        {'my_function', 1, fun(Arg) -> {ok, Arg} end}
    ], fun() ->
        Result = my_module:call_dep(),
        ?assertMatch({ok, _}, Result)
    end).
```

## 🔧 测试宏说明

### 头文件提供的宏 (`include/eunit_setup.hrl`)

| 宏 | 用途 | 是否需要数据库 |
|-----|------|----------------|
| `?TEST_SIMPLE(Fun)` | 简单测试，不需要应用 | ❌ |
| `?TEST_WITH_APP(Fun)` | 启动应用的测试 | ❌ |
| `?TEST_WITH_DB(Fun)` | 需要数据库连接 | ✅ |
| `?TEST_WITH_CONN(Fun)` | 提供数据库连接参数 | ✅ |
| `?WITH_MECK(Module, Expects, Fun)` | 单个模块 Mock | ❌ |
| `?WITH_MECKS(Configs, Fun)` | 多个模块 Mock | ❌ |

## 🛠️ 辅助模块

### eunit_runner

```erlang
% 运行所有测试
eunit_runner:run().

% 运行指定模块
eunit_runner:run([user_repo_tests, group_repo_tests]).

% 快速测试（不需要数据库）
eunit_runner:run_fast().

% Setup 函数（用于手动 setup）
eunit_runner:eunit_setup().
eunit_runner:eunit_cleanup(State).
eunit_runner:eunit_setup_with_db().
```

### test_helper

提供通用的测试辅助函数。

### meck_helper

提供简化的 Mock 设置和清理：

```erlang
% 设置 Mock
meck_helper:setup_mock(Module, Expectations).

% 清理 Mock
meck_helper:cleanup_mock(Module).
```

## ⚠️ 常见问题与解决方案

### 问题 1: make eunit 卡死

**根本原因：**
`make eunit` 会尝试运行所有测试模块，包括需要数据库连接的测试。当测试数量多或某些测试有问题时，可能导致：
- 数据库连接池耗尽
- 应用启动超时
- 某些测试有死锁或无限循环

**诊断步骤：**

```bash
# 1. 检查数据库连接
psql -h localhost -U imboy_user -d imboy_v1 -c "SELECT 1;"

# 2. 使用项目提供的诊断脚本
./diagnose_eunit.escript
```

**解决方案：**

**方案 A：使用快速测试脚本（推荐）**
```bash
# 交互式测试运行
./run_quick_tests.sh

# 选项：
# 1) 快速测试（不需要数据库）
# 2) 单个模块测试
# 3) 全部测试
```

**方案 B：运行单个模块**
```erlang
% 在 Erlang shell 中
erl -pa ebin -pa test/ds -pa test/lib -pa deps/*/ebin

% 运行单个测试
eunit:test(config_ds_tests, [verbose]).
```

**方案 C：使用 eunit_runner（不需要数据库）**
```erlang
% 快速测试
eunit_runner:run_fast().

% 指定模块
eunit_runner:run([imboy_pg_sql_tests]).
```

**方案 D：修改测试避免卡死**
- 使用 `?TEST_SIMPLE` 代替 `?TEST_WITH_DB`
- 添加超时参数：`eunit:test(Module, [{timeout, 10}])`

### 问题 2: 编译警告

**常见警告：**
- `variable 'X' is unused` - 未使用的变量
- `evaluation of operator '-'/2 will fail` - meck 选项错误

**修复方法：**

```erlang
% 错误示例
meck:new(my_module, [passthrough, no-link]).  % no-link 应该是 nolink

% 正确示例
meck:new(my_module, [passthrough, nolink]).

% 或者使用大写形式
meck:new(my_module, [passthrough, {no_link, true}]).
```

### 问题 3: 测试依赖

确保 Makefile 中有测试依赖：

```makefile
TEST_DEPS = sync meck
```

## 📋 测试命名规范

### 测试函数命名

| 类型 | 命名格式 | 示例 |
|-----|---------|------|
| 简单测试 | `<function>_test()` | `tablename_test()` |
| 生成器测试 | `<function>_test_()` | `find_by_id_test_()` |
| 测试组 | `<function>_<scenario>_test_()` | `find_by_id_success_test_()` |

### 测试文件命名

- 模块: `my_module.erl`
- 测试: `my_module_tests.erl` 或 `my_module_test.erl`

## 🎯 测试最佳实践

### 1. 测试分类

```erlang
% 无依赖测试 - 最快
my_util_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(10, my_math:add(5, 5))
    end).

% 需要应用 - 中等
my_logic_test_() ->
    ?TEST_WITH_APP(fun() ->
        Result = my_logic:process(),
        ?assertMatch({ok, _}, Result)
    end).

% 需要数据库 - 最慢
my_repo_test_() ->
    ?TEST_WITH_DB(fun(Conn) ->
        Result = my_repo:find(1),
        ?assertMatch({ok, _}, Result)
    end).
```

### 2. Mock 使用

**何时使用 Mock：**
- 测试 Logic 层时 Mock Repository 层
- 测试 Handler 层时 Mock Logic 层
- 隔离外部依赖（API、数据库）

**何时不用 Mock：**
- 测试 Repository 层直接用测试数据库
- 集成测试

### 3. 测试数据管理

```erlang
% 每个测试独立
setup_test_data_test_() ->
    ?TEST_WITH_DB(fun(Conn) ->
        % 插入测试数据
        ok = my_repo:insert(#{id => 1, name => <<"test">>}, Conn),
        % 执行测试
        Result = my_repo:find(1, Conn),
        ?assertMatch({ok, #{name := <<"test">>}}, Result),
        % 测试结束自动回滚或使用事务
        ok
    end).
```

### 4. 断言选择

```erlang
% 精确匹配
?assertEqual(Expected, Actual).

% 模式匹配（推荐用于复杂结构）
?assertMatch({ok, #{id := Id}}, Result).

% 异常测试
?assertThrow(badarg, my_function()).

% 条件断言
?assert(Condition).
```

## 📊 测试覆盖率

```bash
# 生成覆盖率报告
make cover

# 或者
erl -eval "cover:compile_beam_directory(\".ebeam\"), cover:write_to_file()."
```

## 🔍 调试测试

### 在测试中输出调试信息

```erlang
my_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = my_module:my_function(),
        ?debugFmt("Result: ~p~n", [Result]),
        ?assertEqual(expected, Result)
    end).
```

### 运行特定测试

```bash
# 运行单个测试文件
erl -noshell -eval "eunit:test(my_module_tests, [verbose]), halt()."

# 运行特定测试函数
erl -noshell -eval "eunit:test({my_module_tests, my_test}, [verbose]), halt()."
```

## 📚 参考资源

- **EUnit 官方文档**: http://erlang.org/doc/apps/eunit/chapter.html
- **项目测试说明**: `test/README.md`
- **示例测试**:
  - `test/lib/imboy_pg_sql_tests.erl` - 简单测试
  - `test/ds/config_ds_tests.erl` - DS 层测试
  - `test/repo/user_repo_tests.erl` - Repository 测试

## 💡 开发工作流

1. **编写测试** - 按照模板创建测试文件
2. **快速验证** - `eunit_runner:run_fast()` 或 `eunit_runner:run([single_module])`
3. **完整测试** - `make eunit` (提交前)
4. **覆盖率检查** - `make cover`

## ✅ 测试检查清单

在提交代码前，确保：
- [ ] 所有新功能都有对应的测试
- [ ] 测试命名符合规范
- [ ] Mock 在测试后正确清理
- [ ] 没有未使用的变量警告
- [ ] 数据库测试使用事务隔离
- [ ] `make eunit` 通过
