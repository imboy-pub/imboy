# EUnit 测试基础学习技能

## Description
面向 EUnit 初学者的基础学习指南，涵盖测试框架使用、编写测试、Mock 和最佳实践。

---

## 🎯 学习目标

完成本指南后，您将能够：
- 理解 EUnit 测试框架的基本概念
- 编写单元测试和测试生成器
- 使用 Mock 隔离依赖
- 调试失败的测试

---

## 📖 第一部分：EUnit 基础

### 1. 什么是 EUnit？

EUnit 是 Erlang/OTP 内置的**单元测试框架**，灵感来自 JUnit。

**特点：**
- ✅ 内置在 Erlang/OTP 中，无需额外安装
- ✅ 简洁的测试语法
- ✅ 支持测试生成器
- ✅ 自动发现测试函数

### 2. 基本测试文件结构

```erlang
-module(my_module_tests).
-include_lib("eunit/include/eunit.hrl").

%% 测试函数名必须以 _test() 或 _test_() 结尾

%% 简单测试：不使用 fixture
simple_add_test() ->
    ?assertEqual(3, my_module:add(1, 2)).

%% 测试生成器：返回测试列表
add_test_() ->
    [
        ?_assertEqual(3, my_module:add(1, 2)),
        ?_assertEqual(0, my_module:add(-1, 1)),
        ?_assertEqual(-2, my_module:add(-1, -1))
    ].
```

**命名规则：**
| 类型 | 命名格式 | 说明 |
|------|---------|------|
| 简单测试 | `xxx_test()` | 直接执行，无参数 |
| 测试生成器 | `xxx_test_()` | 返回测试列表 |

---

## 🧪 第二部分：编写测试

### 1. 基本断言

```erlang
%% 相等断言
?assertEqual(Expected, Actual).
%% 示例
?assertEqual(4, 2 * 2).

%% 模式匹配断言
?assertMatch(Pattern, Value).
%% 示例
?assertMatch({ok, _}, my_module:process()).

%% 异常断言
?assertThrow(Exception, Expression).
%% 示例
?assertThrow(badarg, lists:flatten("abc", "def")).

%% 条件断言
?assert(Condition).
%% 示例
?assert(5 > 3).

%% 否定断言
?assertNot(Condition).
?assertNotEqual(3, 2 + 2).

%% 失败断言（用于标记未完成）
?fail("Not implemented yet").
```

### 2. 测试生成器详解

```erlang
%% 返回测试列表
string_operations_test_() ->
    [
        {"测试反转", ?_assertEqual("cba", lists:reverse("abc"))},
        {"测试长度", ?_assertEqual(3, length("abc"))}
    ].

%% 使用 foreach 为每个输入运行测试
foreach_test_() ->
    Lists = [[1,2], [3,4], [5,6]],
    lists:map(fun(L) ->
        {"列表 " ++ io_lib:format("~p", [L]),
         ?_assertEqual(hd(L), lists:last(L) - 1)}
    end, Lists).
```

### 3. Setup 和 Cleanup

```erlang
%% 使用 setup 进行初始化
with_setup_test_() ->
    {setup,
     fun() ->        % setup 函数
         io:format("Setup...~n"),
         {ok, Pid} = my_server:start_link(),
         Pid
     end,
     fun(Pid) ->     % cleanup 函数
         io:format("Cleanup...~n"),
         gen_server:stop(Pid)
     end,
     fun(_Pid) ->    % 实际测试
         [
             ?_test(my_server:do_something())
         ]
     end}.
```

---

## 🎭 第三部分：使用 Mock

### 1. 什么是 Mock？

Mock 用于**隔离外部依赖**，让测试更快速、更可靠。

**何时使用 Mock：**
- ✅ 测试 Logic 层时 Mock Repository 层
- ✅ 测试 Handler 层时 Mock Logic 层
- ❌ 测试 Repository 层时（应该用真实数据库）

### 2. 使用 meck 库

Imboy 项目使用 `meck` 库进行 Mock。

```erlang
%% 安装 meck（已在项目中）
%% make: TEST_DEPS = meck

%% 基本用法
with_mock_test_() ->
    meck:new(user_repo, [no_link]),
    meck:expect(user_repo, find, fun(1) -> {ok, #{id => 1, name => <<"Alice">>}} end),

    Result = user_logic:get_name(1),

    ?assertEqual({ok, <<"Alice">>}, Result),

    meck:unload(user_repo).
```

### 3. Imboy 项目的 Mock 辅助宏

```erlang
%% 使用项目提供的宏
-include("eunit_setup.hrl").

%% Mock 单个模块
with_mock_test_() ->
    ?WITH_MECK(user_repo, [
        {'find', 1, fun(Id) -> {ok, #{id => Id}} end}
    ], fun() ->
        ?_assertMatch({ok, _}, user_logic:get_user(1))
    end).

%% Mock 多个模块
with_multiple_mocks_test_() ->
    ?WITH_MECKS([
        {user_repo, [
            {'find', 1, fun(_) -> {ok, #{id => 1}} end}
        ]},
        {cache_ds, [
            {'get', 1, fun(_) -> undefined end}
        ]}
    ], fun() ->
        ?_assertMatch({ok, _}, user_logic:get_user(1))
    end).
```

---

## 🗄️ 第四部分：数据库测试

### 1. 测试数据库连接

```erlang
%% 使用 Imboy 提供的宏
with_db_test_() ->
    ?TEST_WITH_DB(fun(Conn) ->
        %% Conn 包含: #{host => ..., database => ..., user => ...}
        ?_assertMatch({ok, _, _}, imboy_pg:query("SELECT 1", [], Conn))
    end).
```

### 2. 测试 Repository

```erlang
%% 完整的 Repository 测试示例
user_repo_test_() ->
    {setup,
     fun() ->
         %% 创建测试表
         {ok, _, _} = imboy_pg:query(
             "CREATE TEMP TABLE users (id INT, name TEXT)",
             []
         )
     end,
     fun(_) ->
         %% 清理
         ok
     end,
     fun(_) ->
         [
             ?_test(begin
                 %% 插入测试数据
                 {ok, _, _} = imboy_pg:query(
                     "INSERT INTO users VALUES ($1, $2)",
                     [1, <<"Alice">>]
                 ),
                 %% 测试查询
                 {ok, _, [{Row}]} = user_repo:find(1),
                 ?assertEqual(1, maps:get(id, Row))
             end)
         ]
     end}.
```

---

## 🚀 第五部分：运行测试

### 1. 命令行运行

```bash
# 运行所有测试
make eunit

# 运行单个模块
erl -noshell -eval "eunit:test(user_repo_tests, [verbose]), init:stop()."

# 在 Erlang shell 中运行
erl
> eunit:test(user_repo_tests, [verbose]).

# 使用 Imboy 的测试运行器
# 在 shell 中运行
> eunit_runner:run([user_repo_tests]).
> eunit_runner:run().  % 运行所有测试
> eunit_runner:run_fast().  % 快速测试（无数据库）
```

### 2. 测试分类

```erlang
%% Imboy 项目中的测试分类

%% 1. 简单测试（不需要应用）
simple_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(2, 1 + 1)
    end).

%% 2. 需要应用启动
with_app_test_() ->
    ?TEST_WITH_APP(fun() ->
        ?assertMatch({ok, _}, some_ds:config())
    end).

%% 3. 需要数据库
with_db_test_() ->
    ?TEST_WITH_DB(fun(Conn) ->
        ?assertMatch({ok, _, _}, imboy_pg:query("SELECT 1", [], Conn))
    end).
```

---

## 🔍 第六部分：调试测试

### 1. 查看详细输出

```erlang
%% 使用 verbose 模式
eunit:test(my_module, [verbose]).

%% 在测试中输出调试信息
debug_test() ->
    Result = my_module:func(),
    ?debugFmt("Result: ~p~n", [Result]),
    ?assertEqual(expected, Result).
```

### 2. 常见错误

| 错误 | 原因 | 解决方案 |
|------|------|---------|
| `undefined function` | 测试文件未编译 | 运行 `make compile` |
| `variable 'X' is unused` | 未使用的变量 | 用 `_X` 或 `_` 前缀 |
| `badmatch` | 模式匹配失败 | 检查返回值格式 |
| `timeout` | 测试运行超时 | 增加 timeout 参数 |

```erlang
%% 增加 timeout（秒）
my_slow_test_() ->
    {timeout, 60, [
        ?_test(begin
            timer:sleep(50000),
            ok
        end)
    ]}.
```

---

## ✅ 最佳实践清单

### 测试命名
- [ ] 测试文件名：`my_module_tests.erl`
- [ ] 测试函数名：`function_name_test()` 或 `function_name_test_()`
- [ ] 描述清晰：`should_return_ok_when_user_exists_test()`

### 测试结构
```erlang
%% AAA 模式：Arrange（准备）→ Act（执行）→ Assert（断言）
user_exists_test_() ->
    %% Arrange：准备测试数据
    UserId = 1,

    %% Act：执行被测函数
    Result = user_repo:find(UserId),

    %% Assert：验证结果
    ?assertMatch({ok, #{id := 1}}, Result).
```

### Mock 规则
- [ ] 每个测试独立设置 Mock
- [ ] 测试后清理 Mock (`meck:unload`)
- [ ] 只 Mock 外部依赖
- [ ] 使用 `?WITH_MECK` 宏简化代码

### 数据库测试
- [ ] 使用临时表或事务
- [ ] 每个测试独立数据
- [ ] 测试后清理数据
- [ ] 使用 `?TEST_WITH_DB` 宏

---

## 📚 Imboy 项目测试示例

### Repository 测试

```erlang
%% test/repo/user_repo_tests.erl
-module(user_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

find_by_id_test_() ->
    ?TEST_WITH_DB(fun(Conn) ->
        [
         {"查找存在的用户",
          ?_test(begin
              %% 插入测试数据
              Sql = "INSERT INTO users (id, username) VALUES ($1, $2)",
              {ok, _, _} = imboy_pg:query(Sql, [1, <<"alice">>], Conn),

              %% 测试查找
              {ok, Columns, [Row]} = user_repo:find_by_id(1, Conn),

              %% 验证结果
              ?assertEqual(1, maps:get(id, Row))
          end)},
         {"查找不存在的用户",
          ?_assertEqual({error, not_found}, user_repo:find_by_id(999, Conn))}
        ]
    end).
```

### Logic 测试（使用 Mock）

```erlang
%% test/logic/user_logic_tests.erl
-module(user_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

get_user_test_() ->
    ?TEST_WITH_APP(fun() ->
        ?WITH_MECK(user_repo, [
            {'find_by_id', 1, fun(_) -> {ok, #{id => 1, name => <<"Alice">>}} end}
        ], fun() ->
            ?_assertMatch({ok, #{name := <<"Alice">>}}, user_logic:get_user(1))
        end)
    end).
```

---

## 🎯 学习路径

```
第 1 周：基础
├── 学习 EUnit 基本语法
├── 编写简单的断言测试
└── 运行 `make eunit`

第 2 周：进阶
├── 使用测试生成器
├── 学习 Setup/Cleanup
└── 测试复杂逻辑

第 3 周：Mock
├── 学习 meck 库
├── 使用 ?WITH_MECK 宏
└── 测试 Logic 层

第 4 周：集成测试
├── 数据库测试
├── 使用 ?TEST_WITH_DB
└── 端到端测试
```

---

## 📚 参考资源

### 官方文档
- [EUnit 官方文档](http://erlang.org/doc/apps/eunit/chapter.html)
- [meck 文档](https://github.com/eproxus/meck)

### 项目资源
- `test/include/eunit_setup.hrl` - Imboy 测试宏定义
- `test/common/eunit_runner.erl` - 测试运行器
- `.claude/skills/eunit-testing.skill.md` - 项目 EUnit 进阶指南

---

## 🔍 常见问题

### Q: 测试文件应该放在哪里？
**A:** `test/` 目录下，按层级组织：
```
test/
├── repo/     % Repository 测试
├── logic/    % Logic 测试
├── ds/       % DS 测试
└── api/      % API 测试
```

### Q: 如何跳过某个测试？
**A:** 临时重命名测试函数（加下划线前缀）：
```erlang
_skip_wip_test() ->
    ?assert(true).
```

### Q: make eunit 卡住怎么办？
**A:** 使用快速测试或单个模块：
```bash
# 快速测试（无数据库）
erl -eval "eunit_runner:run_fast()."

# 单个模块
erl -eval "eunit:test(user_repo_tests, [verbose])."
```

---

## 🎯 适用场景

当您需要以下操作时，使用此技能：
- 编写新的单元测试
- Mock 外部依赖
- 运行测试套件
- 调试失败的测试
- 理解 Imboy 项目的测试结构
