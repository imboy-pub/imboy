# Erlang/OTP 28 基础学习技能

## Description
面向 Erlang/OTP 初学者的基础学习指南，涵盖语法、核心概念和 OTP 设计原则。

---

## 🎯 学习目标

完成本指南后，您将能够：
- 理解 Erlang 基本语法和数据类型
- 掌握进程和消息传递
- 了解 OTP 设计原则
- 能够编写简单的 GenServer

---

## 📖 第一部分：Erlang 基础语法

### 1. 数据类型

```erlang
% 1. 原子 (Atom)
% 小写开头，或用单引号
ok.
error.
'atom-with-dashes'.

% 2. 整数和浮点数
42.
3.14.

% 3. 字符串 (实际上是整数列表)
"hello".  % 等同于 [104,101,108,108,111]
<<"hello">>.  % 二进制格式（推荐）

% 4. 元组 (Tuple)
% 固定大小，用花括号
{ok, Value}.
{error, Reason}.
{user, 123, "Alice"}.

% 5. 列表 (List)
% 可变大小，用方括号
[1, 2, 3].
[Head | Tail] = [1, 2, 3].  % Head=1, Tail=[2,3]

% 6. Map (字典)
% 键值对，推荐用于复杂数据
User = #{id => 123, name => "Alice"}.
maps:get(id, User).  % 获取值

% 7. PID (进程标识符)
self().  % 当前进程的 PID
```

### 2. 模式匹配

```erlang
% 模式匹配是 Erlang 的核心

% 1. 变量绑定（大写开头）
Name = "Alice".

% 2. 元组模式匹配
{ok, Result} = some_function().
{error, Reason} = some_function().  % 如果返回 {ok, _} 会匹配失败

% 3. 列表模式匹配
[First | Rest] = [1, 2, 3, 4].  % First=1, Rest=[2,3,4]
[H1, H2 | Tail] = [1, 2, 3, 4].  % H1=1, H2=2, Tail=[3,4]

% 4. 在函数中使用模式匹配
handle_result({ok, Value}) ->
    io:format("Success: ~p~n", [Value]);
handle_result({error, Reason}) ->
    io:format("Error: ~p~n", [Reason]).
```

### 3. 函数定义

```erlang
-module(my_math).
-export([add/2, factorial/1]).  % 导出函数：模块名/参数个数

% 简单函数
add(A, B) ->
    A + B.

% 使用模式匹配和子句
factorial(0) -> 1;
factorial(N) when N > 0 ->
    N * factorial(N - 1).

% 使用 case 表达式
parse(Input) ->
    case Input of
        {ok, Value} -> Value;
        {error, _} -> 0
    end.

% 使用 if 表达式
check_number(N) ->
    if
        N > 0 -> positive;
        N < 0 -> negative;
        true -> zero
    end.
```

### 4. 变量作用域

```erlang
% 变量只能赋值一次（单次赋值）
X = 1.
X = 1.  % OK：相同值
% X = 2.  % ERROR：不能重新赋值

% 在不同作用域可以重新绑定
case something() of
    {ok, X} -> X;  % 这里的 X 是新的绑定
    {error, _} -> 0
end.
```

---

## 🔄 第二部分：并发编程

### 1. 创建进程

```erlang
% spawn/3：创建新进程
% spawn(Module, Function, Arguments)
Pid = spawn(module, function, [arg1, arg2]).

% 示例：简单的计数器进程
-counter() ->
    receive
        increment ->  % 接收消息
            counter();
        {get, From} ->
            From ! 0,  % 发送消息
            counter()
    end.

% 启动进程
Pid = spawn(counter).
```

### 2. 发送和接收消息

```erlang
% 发送消息 (! 操作符)
Pid ! {hello, "World"}.
Pid ! increment.

% 接收消息 (receive)
loop() ->
    receive
        {msg, Content} ->
            io:format("Got: ~p~n", [Content]),
            loop();
        stop ->
            ok
    after 5000 ->  % 超时（毫秒）
        timeout
    end.
```

### 3. 进程注册

```erlang
% 注册进程（给它一个名字）
register(my_process, Pid).

% 向注册的进程发送消息
my_process ! {msg, "Hello"}.

% 查找进程
whereis(my_process).  % 返回 PID 或 undefined
```

### 4. 完整示例：客户端-服务器

```erlang
-module(server).
-export([start/0, request/1, loop/0]).

% 启动服务器
start() ->
    spawn(server, loop, []).

% 发送请求
request(Request) ->
    server ! {self(), Request},
    receive
        {response, Reply} ->
            Reply
    end.

% 服务器循环
loop() ->
    receive
        {From, {add, A, B}} ->
            From ! {response, A + B},
            loop();
        {From, {mul, A, B}} ->
            From ! {response, A * B},
            loop()
    end.

% 使用：
% Pid = server:start().
% server:request({add, 1, 2}).  % 返回 3
```

---

## 🏗️ 第三部分：OTP 设计原则

### 1. 什么是 OTP？

OTP (Open Telecom Platform) 是一套**设计原则和行为模块**，用于构建可靠、可容错的应用程序。

**核心思想：**
- **监督树** - 进程分层组织，自动重启失败的进程
- **行为模块** - gen_server, gen_fsm, supervisor 等
- **应用** - 代码组织和打包

### 2. GenServer (通用服务器)

GenServer 是最常用的 OTP 行为，提供了标准的客户端-服务器接口。

```erlang
-module(counter).
-behaviour(gen_server).

% === 导出回调函数 ===
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

% === 导出 API 函数 ===
-export([start_link/0, increment/0, get_count/0]).

% === 状态定义 ===
-record(state, {count = 0}).

% ============================================================
% API 函数（客户端调用）
% ============================================================

% 启动计数器
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% 增加计数（异步）
increment() ->
    gen_server:cast(?MODULE, increment).

% 获取计数（同步）
get_count() ->
    gen_server:call(?MODULE, get_count).

% ============================================================
% GenServer 回调函数（服务端实现）
% ============================================================

% 初始化
init([]) ->
    {ok, #state{}}.

% 处理同步调用 (call)
handle_call(get_count, _From, State) ->
    {reply, State#state.count, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

% 处理异步调用 (cast)
handle_cast(increment, State) ->
    NewCount = State#state.count + 1,
    {noreply, State#state{count = NewCount}};
handle_cast(_Msg, State) ->
    {noreply, State}.

% 处理其他消息
handle_info(_Info, State) ->
    {noreply, State}.
```

**使用示例：**
```erlang
% 1. 启动
counter:start_link().

% 2. 使用
counter:increment().
counter:increment().
counter:get_count().  % 返回 2
```

### 3. Supervisor (监督者)

Supervisor 管理子进程，当子进程崩溃时自动重启。

```erlang
-module(counter_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

% 启动监督者
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

% 初始化（定义子进程规范）
init([]) ->
    SupFlags = #{
        strategy => one_for_one,  % 重启策略
        intensity => 10,          % 最多重启 10 次
        period => 60              % 在 60 秒内
    },

    ChildSpecs = [
        #{
            id => counter,
            start => {counter, start_link, []},
            restart => permanent,  % 永久重启
            shutdown => 5000,     % 关闭超时
            type => worker,       % 工作进程
            modules => [counter]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
```

**重启策略：**
| 策略 | 说明 |
|------|------|
| `one_for_one` | 只重启崩溃的子进程 |
| `one_for_all` | 重启所有子进程 |
| `rest_for_one` | 重启崩溃的和之后启动的子进程 |

---

## 🎯 Imboy 项目中的 OTP 应用

### 典型模块结构

```
imboy/
├── src/
│   ├── api/              # HTTP Handler (Cowboy)
│   ├── logic/            # 业务逻辑 (可能使用 GenServer)
│   ├── ds/               # 数据服务
│   └── repo/             # 数据仓库
└── sup/
    └── imboy_sup.erl     # 顶层监督者
```

### 数据库连接池示例

```erlang
% 使用 pooler 管理 PostgreSQL 连接池
% 在 supervisor 中配置：

ChildSpecs = [
    #{
        id => pg_pool,
        start => {pooler, start_link, [
            [{name, pg_pool},
             {size, 10},
             {max_overflow, 20}]
        ]},
        ...
    }
].
```

---

## ✅ 最佳实践清单

### 代码风格
- [ ] 模块名小写，使用下划线
- [ ] 变量名大写开头
- [ ] 原子小写开头
- [ ] 使用模式匹配而非 if/case
- [ ] 未使用的参数用 `_` 前缀

### GenServer 开发
- [ ] 使用 `-behaviour(gen_server).`
- [ ] 导出所有回调函数
- [ ] 同步操作用 `handle_call`
- [ ] 异步操作用 `handle_cast`
- [ ] 保持状态最小化

### 错误处理
- [ ] 使用 `try...catch` 处理异常
- [ ] "Let it crash" - 让监督者重启
- [ ] 使用 `{ok, Result}` 和 `{error, Reason}` 元组

---

## 📚 学习资源

### 官方文档
- [Erlang 官方文档](https://www.erlang.org/doc/)
- [OTP 设计原则](https://www.erlang.org/doc/system/design_principles.html)
- [GenServer 指南](https://www.erlang.org/doc/man/gen_server.html)

### 推荐阅读
1. **Learn You Some Erlang** - 在线免费教程
2. **Erlang in Anger** - 生产环境调试指南
3. **Erlang and OTP in Action** - 实战书籍

### 练习项目
1. 实现一个简单的键值存储服务器
2. 创建一个聊天室服务器
3. 实现一个任务队列系统

---

## 🔍 常见问题

### Q: 什么时候用进程，什么时候不用？
**A:** 需要**并发**、**状态隔离**、**容错性**时用进程。简单计算不需要。

### Q: handle_call 和 handle_cast 有什么区别？
**A:**
- `handle_call`: 同步，等待响应
- `handle_cast`: 异步，不等待响应

### Q: 如何调试进程？
**A:**
```erlang
% 查看进程信息
erlang:process_info(Pid).

% 使用 observer
observer:start().
```

---

## 🎯 适用场景

当您需要以下操作时，使用此技能：
- 编写新的 Erlang 模块
- 创建 GenServer
- 设计监督树
- 理解 Imboy 项目架构
- 调试并发问题
