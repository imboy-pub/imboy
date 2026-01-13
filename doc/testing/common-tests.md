# Common Test 使用指南

## erlang.mk 原生支持

erlang.mk 原生支持 Common Test，只需在 `test/` 目录下创建 `*_SUITE.erl` 文件即可。

## 运行方式

### 基本命令

```bash
# 运行所有 Common Test suites
make ct

# 运行特定 suite
make ct-msg_ack_logic

# 运行特定测试用例（不分组）
make ct-msg_ack_logic c=c2c_ack_deletes_offline_msg

# 运行特定 group
make ct-msg_delivery t=full_flow

# 运行特定 group 中的特定测试
make ct-msg_delivery t=full_flow:send_c2c_message

# 运行所有测试（EUnit + Common Test）
make tests
```

## 文件命名规范

| 文件名 | Suite 名称 | 说明 |
|--------|-----------|------|
| `test/msg_ack_logic_SUITE.erl` | `msg_ack_logic` | ACK 处理测试 |
| `test/msg_delivery_SUITE.erl` | `msg_delivery` | 消息投递测试 |

## 测试套件结构

```erlang
-module(your_suite).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%% ===================================================================
%% Suite 回调
%% ===================================================================

%% 必须导出：返回所有测试用例和测试组
all() ->
    [
     test_case_1,
     test_case_2,
     {group, group_name}
    ].

%% 可选：定义测试组
groups() ->
    [
     {group_name, [], [test_case_3, test_case_4]}
    ].

%% 可选：Suite 初始化
init_per_suite(Config) ->
    % 启动应用、设置环境等
    Config.

end_per_suite(_Config) ->
    % 清理资源
    ok.

%% 可选：Group 初始化
init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

%% 可选：测试用例初始化
init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    % 清理 Mock
    meck:unload(),
    ok.


%% ===================================================================
%% 测试用例
%% ===================================================================

test_case_1(_Config) ->
    % 测试逻辑
    ok = some_function(),
    {comment, "测试成功"}.

test_case_2(Config) ->
    % 使用 Config 传递数据
    Val = ?config(key, Config),
    ?assertEqual(expected, Val).
```

## 常用 CT 宏和函数

### 日志输出

```erlang
ct:log("信息: ~p", [Data]),
ct:pal("可配置日志: ~p", [Data]),  % 可通过 verbose 控制
ct:print("始终输出: ~p", [Data]).
```

### 断言

```erlang
?assert(Condition)                           % 布尔断言
?assertEqual(Expected, Actual)               % 相等断言
?assertMatch(Pattern, Value)                 % 模式匹配断言
?assertNot(Condition)                        % 否定断言
?assertException(ExceptionClass, Pattern, Fun)  % 异常断言
```

### Config 操作

```erlang
% 保存数据
Config2 = lists:keystore(key, 1, Config, {key, Value}),

% 读取数据
Value = proplists:get_value(key, Config),
Value = ?config(key, Config),
```

## 当前测试套件

### msg_ack_logic_SUITE

测试 `msg_ack_logic` 模块的 ACK 处理功能：

| 测试用例 | 说明 |
|---------|------|
| `c2c_ack_deletes_offline_msg` | C2C ACK 删除离线消息 |
| `c2c_ack_with_no_msg` | C2C ACK 处理不存在的消息 |
| `c2g_ack_marks_timeline` | C2G ACK 标记 timeline |
| `s2c_ack_deletes_offline_msg` | S2C ACK 删除离线消息 |
| `c2s_ack_uses_parameterized_query` | C2S ACK 使用参数化查询 |
| `unknown_msg_type_handles_gracefully` | 未知消息类型优雅处理 |

### msg_delivery_SUITE

测试消息投递完整流程：

| 测试组 | 测试用例 | 说明 |
|--------|---------|------|
| `full_flow` | `send_c2c_message` | 发送 C2C 消息 |
| | `message_delivered_to_online_user` | 投递给在线用户 |
| | `message_ack_cleanup` | ACK 清理 |
| `offline_storage` | `store_offline_message` | 存储离线消息 |
| | `retrieve_offline_message_on_user_online` | 用户上线获取离线消息 |
| `retry_mechanism` | `retry_on_first_failure` | 首次失败后重试 |
| | `retry_intervals_configuration` | 重试间隔配置 |
| `multi_device` | `deliver_to_all_devices` | 投递到所有设备 |
| | `ack_from_single_device` | 单设备 ACK |

## 创建新的测试套件

### 1. 创建测试文件

在 `test/` 目录下创建 `{name}_SUITE.erl`：

```bash
touch test/your_module_SUITE.erl
```

### 2. 编写测试

```erlang
-module(your_module_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() ->
    [
     your_test_case
    ].

init_per_suite(Config) ->
    application:set_env(imboy, env, test),
    Config.

end_per_suite(_Config) ->
    ok.

your_test_case(_Config) ->
    % 测试逻辑
    ?assertEqual(true, your_module:your_function()),
    {comment, "测试通过"}.
```

### 3. 运行测试

```bash
make ct-your_module
```

## 配置选项

### CT_OPTS

设置额外的 Common Test 选项：

```makefile
CT_OPTS = -ct_hooks your_ct_hook
```

### CT_LOGS_DIR

设置日志输出目录：

```makefile
CT_LOGS_DIR = logs/ct
```

### CT_SUITES

手动指定测试套件（通常不需要，erlang.mk 会自动发现）：

```makefile
CT_SUITES = msg_ack_logic msg_delivery
```

## 查看测试结果

### HTML 报告

```bash
make ct

# 查看报告
open logs/ct/index.html
```

### 命令行输出

```bash
# 详细输出
make ct ct_verbose=true

# 持续输出（不缓冲）
make ct ct_keep_logs=true
```

## 故障排除

### 测试套件未被发现

确保：
1. 文件以 `_SUITE.erl` 结尾
2. 文件在 `test/` 目录
3. 文件编译到 `ebin/`

### 数据库连接问题

在 `init_per_suite` 中初始化：

```erlang
init_per_suite(Config) ->
    application:set_env(imboy, env, test),
    {ok, _} = application:ensure_all_started(imboy),
    Config.
```

### Mock 未清理

在 `end_per_testcase` 中清理：

```erlang
end_per_testcase(_TestCase, _Config) ->
    meck:unload(),
    ok.
```

## 参考资料

- [Common Test 用户指南](https://www.erlang.org/doc/apps/common_test/chapter.html)
- [erlang.mk CT 文档](https://erlang.mk/guide/ct.html)
- [Erlang 测试工具](https://www.erlang.org/doc/testing_tools.html)
