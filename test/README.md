# EUnit 测试使用说明

## 🚀 快速开始

### 运行所有测试

```bash
make eunit
```

### 运行快速测试（不需要数据库）

```bash
make test-fast
```

### 清理测试产物

```bash
make eunit-clean
```

## 📝 测试文件编写

### 方法 1: 使用头文件宏（推荐）

在测试文件中添加：

```erlang
-module(my_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("test/include/eunit_setup.hrl").

% 简单测试（不需要应用）
my_simple_test() ->
    ?TEST_SIMPLE(fun() ->
        Result = imboy_pg_sql:public_tablename(<<"user">>),
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

### 方法 2: 手动 setup

```erlang
-module(my_repo_tests).
-include_lib("eunit/include/eunit.hrl").

% 不需要任何 setup 的简单测试
tablename_test() ->
    Result = my_repo:tablename(),
    ?assertEqual(<<"public.my_table">>, Result).

% 需要启动应用的测试
my_test_() ->
    {setup,
     fun eunit_setup/0,
     fun eunit_cleanup/1,
     fun() ->
         Result = my_repo:find(1),
         ?assertMatch({ok, _}, Result)
     end}.
```

## 📖 提供的辅助功能

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

## 💡 最佳实践

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

## ⚠️ 注意事项

1. **数据库依赖**: 需要数据库的测试会在数据库不可用时自动跳过
2. **应用启动**: `make eunit` 会自动启动应用，无需手动操作
3. **测试隔离**: 每个测试都是独立的，不会相互影响

## 📚 示例

查看现有测试文件作为参考：
- `test/apps/imlib/src/imboy_pg_sql_tests.erl` - SQL 构造测试
- `test/apps/imrepo/src/user_repo_tests.erl` - Repository 测试
