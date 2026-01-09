# 全栈学习路线图

## Description
Erlang/OTP、PostgreSQL、Flutter 全栈开发的学习路线图，针对 Imboy 项目定制。

---

## 🎯 学习目标

本路线图将帮助您从零开始，逐步掌握：
1. **Erlang/OTP 28** - 后端服务开发
2. **PostgreSQL 18** - 数据库设计与优化
3. **EUnit** - 单元测试
4. **Flutter** - 移动应用开发

---

## 📊 学习路径总览

```
第 1 阶段：基础入门 (4-6 周)
├── Erlang 基础语法 ──────────┐
├── PostgreSQL SQL 基础 ───────┤
└── Dart 语言基础 ─────────────┤

第 2 阶段：后端开发 (6-8 周)
├── OTP 设计原则 ─────────────┐
├── GenServer 编程 ────────────┤
├── PostgreSQL 高级特性 ───────┤
└── EUnit 测试 ────────────────┘

第 3 阶段：前端开发 (4-6 周)
├── Flutter Widget 系统 ──────┐
└── Flutter 状态管理 ─────────┘

第 4 阶段：项目实战 (持续)
└── Imboy 项目开发 ────────────→ 熟练掌握
```

---

## 📅 第 1 阶段：基础入门 (4-6 周)

### 第 1-2 周：Erlang 基础

**学习内容：**
- ✅ 数据类型（原子、元组、列表、Map）
- ✅ 模式匹配
- ✅ 函数定义
- ✅ 进程和消息传递

**实践项目：**
```erlang
% 实现一个简单的计算器
calculator() ->
    receive
        {add, A, B, From} ->
            From ! A + B,
            calculator();
        {mul, A, B, From} ->
            From ! A * B,
            calculator()
    end.

% 测试
Pid = spawn(fun calculator/0).
Pid ! {add, 1, 2, self()}.
receive Result -> io:format("Result: ~p~n", [Result]) end.
```

**资源：**
- [Erlang 基础学习技能](.claude/skills/erlang-basics.skill.md)
- [Learn You Some Erlang](http://learnyousomeerlang.com/)

---

### 第 3-4 周：PostgreSQL 基础

**学习内容：**
- ✅ SQL 基础（SELECT、INSERT、UPDATE、DELETE）
- ✅ 表设计与约束
- ✅ 索引基础
- ✅ JOIN 查询

**实践项目：**
```sql
-- 创建用户和消息表
CREATE TABLE users (
    id BIGSERIAL PRIMARY KEY,
    username VARCHAR(50) NOT NULL UNIQUE,
    email VARCHAR(100) NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE TABLE messages (
    id BIGSERIAL PRIMARY KEY,
    from_id BIGINT NOT NULL REFERENCES users(id),
    to_id BIGINT NOT NULL REFERENCES users(id),
    content TEXT NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW()
);

-- 查询用户的所有消息
SELECT u.username, m.content, m.created_at
FROM messages m
JOIN users u ON m.from_id = u.id
WHERE m.to_id = 1
ORDER BY m.created_at DESC;
```

**资源：**
- [PostgreSQL 基础学习技能](.claude/skills/postgresql-basics.skill.md)

---

### 第 5-6 周：Dart 语言基础

**学习内容：**
- ✅ 变量与数据类型
- ✅ 函数与闭包
- ✅ 类与对象
- ✅ 异步编程（Future、async/await）
- ✅ 空安全

**实践项目：**
```dart
// 实现一个简单的用户模型
class User {
  final String name;
  final String email;
  final int age;

  User({required this.name, required this.email, required this.age});

  factory User.fromJson(Map<String, dynamic> json) {
    return User(
      name: json['name'] as String,
      email: json['email'] as String,
      age: json['age'] as int,
    );
  }

  Map<String, dynamic> toJson() {
    return {'name': name, 'email': email, 'age': age};
  }

  bool get isAdult => age >= 18;
}

// 测试
void main() async {
  final user = User(name: 'Alice', email: 'alice@example.com', age: 25);
  print(user.isAdult);  // true
  print(user.toJson());  // {name: Alice, email: alice@example.com, age: 25}
}
```

**资源：**
- [Flutter 基础学习技能](.claude/skills/flutter-basics.skill.md)
- [Dart 语言导览](https://dart.dev/guides)

---

## 📅 第 2 阶段：后端开发 (6-8 周)

### 第 7-9 周：OTP 设计原则

**学习内容：**
- ✅ GenServer 基础
- ✅ Supervisor 监督树
- � ️ 应用结构
- ✅ 错误处理

**实践项目：**
```erlang
% 实现一个计数器服务器
-module(counter).
-behaviour(gen_server).

%% API
-export([start_link/0, increment/0, get_count/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2]).

-record(state, {count = 0}).

%% API functions
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

increment() ->
    gen_server:cast(?MODULE, increment).

get_count() ->
    gen_server:call(?MODULE, get_count).

%% Callbacks
init([]) ->
    {ok, #state{}}.

handle_call(get_count, _From, State) ->
    {reply, State#state.count, State}.

handle_cast(increment, State) ->
    NewCount = State#state.count + 1,
    {noreply, State#state{count = NewCount}}.
```

**资源：**
- [Erlang 基础学习技能](.claude/skills/erlang-basics.skill.md)
- [OTP 设计原则文档](https://www.erlang.org/doc/system/design_principles.html)

---

### 第 10-11 周：EUnit 测试

**学习内容：**
- ✅ EUnit 基础语法
- ✅ 测试生成器
- ✅ Mock 使用（meck）
- ✅ 数据库测试

**实践项目：**
```erlang
%% 测试计数器服务器
-module(counter_tests).
-include_lib("eunit/include/eunit.hrl").

%% 简单测试
increment_test() ->
    {ok, Pid} = counter:start_link(),
    counter:increment(),
    counter:increment(),
    ?assertEqual(2, counter:get_count()),
    gen_server:stop(Pid).

%% 使用 Mock
user_logic_test_() ->
    {setup,
     fun() -> meck:new(user_repo, [unstick]), ok end,
     fun(_) -> meck:unload(user_repo) end,
     fun() ->
         meck:expect(user_repo, find, fun(1) -> {ok, #{id => 1}} end),
         ?assertEqual({ok, #{id => 1}}, user_logic:get_user(1))
     end}.
```

**资源：**
- [EUnit 基础学习技能](.claude/skills/eunit-basics.skill.md)

---

### 第 12-14 周：PostgreSQL 高级特性

**学习内容：**
- ✅ 索引优化
- ✅ JSON/JSONB
- ✅ 事务处理
- ✅ PG 18 新特性（AIO、UUIDv7）

**实践项目：**
```sql
-- 使用 JSONB 存储用户配置
CREATE TABLE user_settings (
    user_id BIGINT PRIMARY KEY,
    config JSONB NOT NULL DEFAULT '{}',
    updated_at TIMESTAMPTZ DEFAULT NOW()
);

-- 创建 GIN 索引
CREATE INDEX idx_user_settings_config ON user_settings USING GIN (config);

-- 查询配置
SELECT * FROM user_settings WHERE config @> '{"theme": "dark"}';

-- 更新配置
UPDATE user_settings
SET config = jsonb_set(config, '{theme}', '"light"')
WHERE user_id = 1;

-- 使用 UUIDv7
CREATE TABLE events (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    event_type VARCHAR(50) NOT NULL,
    payload JSONB NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW()
);
```

**资源：**
- [PostgreSQL 基础学习技能](.claude/skills/postgresql-basics.skill.md)

---

## 📅 第 3 阶段：前端开发 (4-6 周)

### 第 15-17 周：Flutter Widget 系统

**学习内容：**
- ✅ Widget 基础
- ✅ 布局（Row、Column、Stack）
- ✅ ListView 和 GridView
- ✅ 导航与路由

**实践项目：**
```dart
// 实现一个用户列表页面
class UserListPage extends StatelessWidget {
  const UserListPage({super.key});

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: const Text('Users')),
      body: ListView.builder(
        itemCount: users.length,
        itemBuilder: (context, index) {
          final user = users[index];
          return ListTile(
            leading: CircleAvatar(child: Text(user.name[0])),
            title: Text(user.name),
            subtitle: Text(user.email),
            onTap: () {
              Navigator.push(
                context,
                MaterialPageRoute(
                  builder: (context) => UserDetailPage(user: user),
                ),
              );
            },
          );
        },
      ),
    );
  }
}
```

---

### 第 18-20 周：Flutter 状态管理

**学习内容：**
- ✅ setState
- ✅ Provider
- ✅ 网络请求
- ✅ 错误处理

**实践项目：**
```dart
// 使用 Provider 的用户管理
class UserModel extends ChangeNotifier {
  List<User> _users = [];
  bool _isLoading = false;

  List<User> get users => _users;
  bool get isLoading => _isLoading;

  Future<void> loadUsers() async {
    _isLoading = true;
    notifyListeners();

    try {
      _users = await ApiService().fetchUsers();
    } catch (e) {
      print('Error: $e');
    } finally {
      _isLoading = false;
      notifyListeners();
    }
  }
}
```

**资源：**
- [Flutter 基础学习技能](.claude/skills/flutter-basics.skill.md)

---

## 📅 第 4 阶段：项目实战 (持续)

### Imboy 项目实战

**后端任务：**
1. 理解项目架构（4 层架构）
2. 阅读现有代码（Handler → Logic → DS → Repo）
3. 编写单元测试
4. 实现新功能

**前端任务：**
1. 创建 Flutter 项目
2. 实现用户界面
3. 连接 WebSocket
4. 实现消息收发

---

## 📚 学习资源汇总

### Erlang/OTP
| 资源 | 类型 | 链接 |
|------|------|------|
| Erlang 基础学习技能 | 技能文件 | `.claude/skills/erlang-basics.skill.md` |
| Learn You Some Erlang | 在线教程 | http://learnyousomeerlang.com/ |
| Erlang 官方文档 | 官方文档 | https://www.erlang.org/doc/ |
| OTP 设计原则 | 官方文档 | https://www.erlang.org/doc/system/design_principles.html |

### PostgreSQL
| 资源 | 类型 | 链接 |
|------|------|------|
| PostgreSQL 基础学习技能 | 技能文件 | `.claude/skills/postgresql-basics.skill.md` |
| PostgreSQL Tutorial | 在线教程 | https://www.postgresqltutorial.com/ |
| PG 18 新特性 | 文章 | https://neon.com/postgresql/postgresql-18-new-features |

### EUnit
| 资源 | 类型 | 链接 |
|------|------|------|
| EUnit 基础学习技能 | 技能文件 | `.claude/skills/eunit-basics.skill.md` |
| EUnit 官方文档 | 官方文档 | http://erlang.org/doc/apps/eunit/chapter.html |

### Flutter
| 资源 | 类型 | 链接 |
|------|------|------|
| Flutter 基础学习技能 | 技能文件 | `.claude/skills/flutter-basics.skill.md` |
| Flutter 官方文档 | 官方文档 | https://docs.flutter.dev/ |
| Flutter 实战 | 电子书 | https://book.flutterchina.club/ |

---

## ✅ 学习检查清单

### Erlang/OTP
- [ ] 能够编写基本的 Erlang 函数
- [ ] 理解进程和消息传递
- [ ] 能够实现 GenServer
- [ ] 能够设计监督树
- [ ] 理解 Imboy 项目架构

### PostgreSQL
- [ ] 能够编写基本 SQL 查询
- [ ] 能够设计表结构
- [ ] 理解索引原理
- [ ] 能够使用 JSON/JSONB
- [ ] 了解 PG 18 新特性

### EUnit
- [ ] 能够编写单元测试
- [ ] 能够使用 Mock
- [ ] 能够编写数据库测试
- [ ] 理解测试最佳实践

### Flutter
- [ ] 理解 Widget 系统
- [ ] 能够实现基本布局
- [ ] 理解状态管理
- [ ] 能够实现网络请求
- [ ] 能够构建完整页面

---

## 🎯 学习建议

### 时间分配
| 阶段 | 时间 | 重点 |
|------|------|------|
| 基础入门 | 4-6 周 | 理解基本概念 |
| 后端开发 | 6-8 周 | OTP + PostgreSQL |
| 前端开发 | 4-6 周 | Flutter + 状态管理 |
| 项目实战 | 持续 | Imboy 项目开发 |

### 学习方法
1. **理论学习** - 阅读技能文件和官方文档
2. **动手实践** - 完成每个阶段的小项目
3. **代码阅读** - 阅读 Imboy 项目源码
4. **测试驱动** - 先写测试，再写功能

### 常见问题

**Q: 学习顺序可以调整吗？**
A: 可以。如果您已有某方面基础，可以跳过或调整顺序。

**Q: 需要全部学完才能做项目吗？**
A: 不需要。掌握基础后就可以开始参与项目，在实践中学习。

**Q: 遇到问题怎么办？**
A: 1) 查阅对应技能文件；2) 查看官方文档；3) 查看 Imboy 代码示例

---

## 📝 学习笔记模板

建议为每个技术创建学习笔记：

```markdown
# Erlang 学习笔记

## 第 1 周
- [x] 数据类型
- [x] 模式匹配
- [ ] 进程通信

## 练习项目
- [ ] 简单计算器
- [ ] 进程通信示例

## 问题记录
- Q: 元组和列表的区别？
- A: 元组固定大小，列表可变大小...
```

---

**祝您学习顺利！🚀**

如有问题，随时查阅对应的技能文件或询问我。
