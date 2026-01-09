# Flutter 基础学习技能

## Description
面向 Flutter 初学者的基础学习指南，涵盖 Dart 语法、Widget 系统、状态管理和最佳实践。

---

## 🎯 学习目标

完成本指南后，您将能够：
- 理解 Dart 语言基础
- 掌握 Flutter Widget 系统
- 实现状态管理
- 构建简单的 Flutter 应用

---

## 📖 第一部分：Dart 语言基础

### 1. 基本语法

```dart
// 变量声明
String name = 'Alice';
var age = 25;           // 类型推断
final PI = 3.14;        // 运行时常量
const MAX = 100;        // 编译时常量

// 数据类型
int count = 42;
double price = 3.99;
bool isActive = true;
String message = 'Hello';
List<int> numbers = [1, 2, 3];
Set<String> uniqueNames = {'Alice', 'Bob'};
Map<String, int> scores = {'Alice': 95, 'Bob': 87};

// 字符串插值
String greeting = 'Hello, $name!';
String info = 'Name: $name, Age: $age';
```

### 2. 函数

```dart
// 基本函数
int add(int a, int b) {
  return a + b;
}

// 箭头函数（单行）
int multiply(int a, int b) => a * b;

// 可选参数
void greet(String name, {String? title, int age = 0}) {
  print('Hello, ${title ?? ''} $name, age $age');
}

greet('Alice');                           // Hello,  Alice, age 0
greet('Bob', title: 'Dr.', age: 30);      // Hello, Dr. Bob, age 30

// 位置可选参数
String format(String first, String last, [String? middle]) {
  return middle != null
      ? '$first $middle $last'
      : '$first $last';
}
```

### 3. 异步编程

```dart
// Future：表示一个异步操作
Future<String> fetchUserData() {
  return Future.delayed(
    Duration(seconds: 2),
    () => 'User Data',
  );
}

// async/await
Future<void> loadData() async {
  print('Loading...');
  String data = await fetchUserData();
  print('Loaded: $data');
}

// 错误处理
Future<void> safeLoad() async {
  try {
    String data = await fetchUserData();
    print('Success: $data');
  } catch (e) {
    print('Error: $e');
  } finally {
    print('Done');
  }
}
```

### 4. 类与对象

```dart
class User {
  // 属性
  String name;
  int age;

  // 构造函数
  User(this.name, this.age);

  // 命名构造函数
  User.guest() : name = 'Guest', age = 0;

  // 方法
  void introduce() {
    print('I am $name, $age years old');
  }

  // Getter
  bool get isAdult => age >= 18;

  // 运算符重载
  @override
  String toString() => 'User($name, $age)';
}

// 使用
var alice = User('Alice', 25);
alice.introduce();
print(alice.isAdult);  // true
```

### 5. 空安全（Null Safety）

```dart
// 可空类型
String? name;

// 使用前检查
if (name != null) {
  print(name.length);
}

// ?? 空值合并运算符
String displayName = name ?? 'Guest';

// ?. 空值安全调用
print(name?.length);  // 如果 name 为 null，返回 null

// ! 断言非空（确定不为 null 时使用）
print(name!.length);  // 如果 name 为 null，抛出异常
```

---

## 🎨 第二部分：Flutter Widget 系统

### 1. 一切皆 Widget

Flutter 的核心思想是：**UI 由 Widget 组成的树**。

```dart
import 'package:flutter/material.dart';

// 最简单的应用
void main() {
  runApp(
    const Center(
      child: Text(
        'Hello, Flutter!',
        textDirection: TextDirection.ltr,
      ),
    ),
  );
}
```

### 2. StatelessWidget（无状态 Widget）

```dart
class MyCard extends StatelessWidget {
  const MyCard({super.key});

  @override
  Widget build(BuildContext context) {
    return Card(
      child: Padding(
        padding: const EdgeInsets.all(16.0),
        child: Column(
          children: [
            const Text('Title', style: TextStyle(fontSize: 20)),
            const SizedBox(height: 8),
            Text('Content', style: TextStyle(color: Colors.grey[700])),
          ],
        ),
      ),
    );
  }
}
```

### 3. StatefulWidget（有状态 Widget）

```dart
class Counter extends StatefulWidget {
  const Counter({super.key});

  @override
  State<Counter> createState() => _CounterState();
}

class _CounterState extends State<Counter> {
  int _count = 0;

  void _increment() {
    setState(() {
      _count++;
    });
  }

  @override
  Widget build(BuildContext context) {
    return Column(
      children: [
        Text('Count: $_count'),
        ElevatedButton(
          onPressed: _increment,
          child: const Text('Increment'),
        ),
      ],
    );
  }
}
```

**StatefulWidget 生命周期：**
```
initState() → didChangeDependencies() → build() → setState() → build()
                    ↓
              deactivate() → dispose()
```

### 4. 常用 Widget

| Widget | 说明 | 示例 |
|--------|------|------|
| `Container` | 容器，可设置装饰、内边距 | `Container(padding: EdgeInsets.all(8))` |
| `Row` | 水平布局 | `Row(children: [Icon(), Text()])` |
| `Column` | 垂直布局 | `Column(children: [Text(), Text()])` |
| `Stack` | 层叠布局 | `Stack(children: [Image(), Positioned()])` |
| `ListView` | 列表 | `ListView(children: [...])` |
| `GridView` | 网格 | `GridView.count(crossAxisCount: 2)` |
| `SizedBox` | 固定尺寸空白 | `SizedBox(height: 20)` |
| `Padding` | 内边距 | `Padding(padding: EdgeInsets.all(8))` |
| `Center` | 居中 | `Center(child: Text())` |

---

## 📦 第三部分：布局

### 1. Flex 布局

```dart
// Row：水平排列
Row(
  children: [
    Expanded(flex: 1, child: Container(color: Colors.red)),
    Expanded(flex: 2, child: Container(color: Colors.blue)),
  ],
)

// Column：垂直排列
Column(
  crossAxisAlignment: CrossAxisAlignment.start,
  children: [
    const Text('Title'),
    const Text('Subtitle'),
  ],
)
```

### 2. 对齐方式

```dart
// MainAxisAlignment：主轴对齐
// Row 的主轴是水平方向
Row(
  mainAxisAlignment: MainAxisAlignment.spaceEvenly,
  children: [Text('A'), Text('B'), Text('C')],
)

// CrossAxisAlignment：交叉轴对齐
// Row 的交叉轴是垂直方向
Row(
  crossAxisAlignment: CrossAxisAlignment.center,
  children: [Text('A'), Text('B')],
)
```

**对齐选项：**
| MainAxisAlignment | 说明 |
|-------------------|------|
| `start` | 从开始位置排列 |
| `end` | 从结束位置排列 |
| `center` | 居中 |
| `spaceBetween` | 两端对齐，中间均匀分布 |
| `spaceAround` | 每个元素两侧空间相等 |
| `spaceEvenly` | 所有空间均匀分布 |

---

## 🔄 第四部分：状态管理

### 1. 基础状态管理（setState）

```dart
class TodoList extends StatefulWidget {
  const TodoList({super.key});

  @override
  State<TodoList> createState() => _TodoListState();
}

class _TodoListState extends State<TodoList> {
  final List<String> _todos = [];
  final TextEditingController _controller = TextEditingController();

  void _addTodo() {
    if (_controller.text.isNotEmpty) {
      setState(() {
        _todos.add(_controller.text);
        _controller.clear();
      });
    }
  }

  @override
  Widget build(BuildContext context) {
    return Column(
      children: [
        TextField(
          controller: _controller,
          decoration: const InputDecoration(labelText: 'Enter todo'),
        ),
        ElevatedButton(onPressed: _addTodo, child: const Text('Add')),
        Expanded(
          child: ListView.builder(
            itemCount: _todos.length,
            itemBuilder: (context, index) => ListTile(title: Text(_todos[index])),
          ),
        ),
      ],
    );
  }
}
```

### 2. 状态管理方案对比

| 方案 | 复杂度 | 适用场景 |
|------|--------|----------|
| `setState` | ⭐ 简单 | 小型应用、单页面状态 |
| `InheritedWidget` | ⭐⭐ 中等 | 中型应用、组件共享 |
| `Provider` | ⭐⭐ 中等 | 推荐、社区标准 |
| `Riverpod` | ⭐⭐⭐ 较复杂 | 大型应用、类型安全 |
| `Bloc` | ⭐⭐⭐ 复杂 | 大型应用、复杂业务逻辑 |

### 3. Provider 基础

```dart
// 1. 创建数据模型
class Counter extends ChangeNotifier {
  int _count = 0;
  int get count => _count;

  void increment() {
    _count++;
    notifyListeners();
  }
}

// 2. 提供数据
void main() {
  runApp(
    ChangeNotifierProvider(
      create: (_) => Counter(),
      child: const MyApp(),
    ),
  );
}

// 3. 消费数据
class CounterWidget extends StatelessWidget {
  const CounterWidget({super.key});

  @override
  Widget build(BuildContext context) {
    final counter = Provider.of<Counter>(context);
    return Column(
      children: [
        Text('Count: ${counter.count}'),
        ElevatedButton(
          onPressed: counter.increment,
          child: const Text('Increment'),
        ),
      ],
    );
  }
}
```

---

## 🌐 第五部分：路由与导航

### 1. 基本导航

```dart
// 跳转到新页面
Navigator.push(
  context,
  MaterialPageRoute(builder: (context) => const SecondPage()),
);

// 返回上一页
Navigator.pop(context);

// 跳转并等待结果
final result = await Navigator.push(
  context,
  MaterialPageRoute(builder: (context) => const SelectionPage()),
);
print('Selected: $result');
```

### 2. 命名路由

```dart
// 1. 定义路由
MaterialApp(
  routes: {
    '/': (context) => const HomePage(),
    '/details': (context) => const DetailsPage(),
    '/settings': (context) => const SettingsPage(),
  },
  initialRoute: '/',
)

// 2. 跳转
Navigator.pushNamed(context, '/details');

// 3. 传递参数
Navigator.pushNamed(
  context,
  '/details',
  arguments: {'id': 123},
);

// 4. 获取参数
final args = ModalRoute.of(context)!.settings.arguments as Map;
final id = args['id'];
```

---

## 📡 第六部分：网络请求

### 1. 使用 http 包

```yaml
# pubspec.yaml
dependencies:
  http: ^1.2.0
```

```dart
import 'package:http/http.dart' as http;
import 'dart:convert';

class ApiService {
  static const String baseUrl = 'https://api.example.com';

  // GET 请求
  Future<Map<String, dynamic>> getUser(int id) async {
    final response = await http.get(
      Uri.parse('$baseUrl/users/$id'),
    );

    if (response.statusCode == 200) {
      return jsonDecode(response.body) as Map<String, dynamic>;
    } else {
      throw Exception('Failed to load user');
    }
  }

  // POST 请求
  Future<Map<String, dynamic>> createUser(
    String name,
    String email,
  ) async {
    final response = await http.post(
      Uri.parse('$baseUrl/users'),
      headers: {'Content-Type': 'application/json'},
      body: jsonEncode({'name': name, 'email': email}),
    );

    if (response.statusCode == 201) {
      return jsonDecode(response.body) as Map<String, dynamic>;
    } else {
      throw Exception('Failed to create user');
    }
  }
}
```

### 2. 错误处理与加载状态

```dart
class UserPage extends StatefulWidget {
  const UserPage({super.key, required this.userId});

  final int userId;

  @override
  State<UserPage> createState() => _UserPageState();
}

class _UserPageState extends State<UserPage> {
  Map<String, dynamic>? _user;
  bool _isLoading = true;
  String? _error;

  @override
  void initState() {
    super.initState();
    _loadUser();
  }

  Future<void> _loadUser() async {
    try {
      final user = await ApiService().getUser(widget.userId);
      setState(() {
        _user = user;
        _isLoading = false;
      });
    } catch (e) {
      setState(() {
        _error = e.toString();
        _isLoading = false;
      });
    }
  }

  @override
  Widget build(BuildContext context) {
    if (_isLoading) {
      return const Center(child: CircularProgressIndicator());
    }

    if (_error != null) {
      return Center(child: Text('Error: $_error'));
    }

    return Scaffold(
      body: Column(
        children: [
          Text('Name: ${_user!['name']}'),
          Text('Email: ${_user!['email']}'),
        ],
      ),
    );
  }
}
```

---

## ✅ 最佳实践清单

### 代码规范
- [ ] 使用 `const` 构造函数
- [ ] Widget 拆分到单独的文件
- [ ] 使用 `private` 变量（`_variable`）
- [ ] 添加 `key` 参数
- [ ] 使用 `async/await` 而非 `.then()`

### 性能优化
- [ ] 使用 `const Widget` 减少重建
- [ ] 使用 `ListView.builder` 而非 `ListView(children: [])`
- [ ] 避免在 `build()` 中创建大量对象
- [ ] 使用 `RepaintBoundary` 隔离重绘区域

### 状态管理
- [ ] 单页面状态使用 `setState`
- [ ] 跨页面状态使用 Provider/Riverpod
- [ ] 复杂业务逻辑使用 Bloc

---

## 📚 学习资源

### 官方文档
- [Flutter 官方文档](https://docs.flutter.dev/)
- [Flutter 性能最佳实践](https://docs.flutter.dev/perf/best-practices)
- [Flutter 应用架构指南](https://docs.flutter.dev/app-architecture/guide)
- [Dart 语言导览](https://dart.dev/guides)

### 推荐教程
- [Flutter 实战](https://book.flutterchina.club/)
- [Flutter 示例](https://gallery.flutter.dev/)

### 社区资源
- [Flutter 中文网](https://flutter.cn/)
- [Pub.dev](https://pub.dev/) - Flutter 包仓库

---

## 🔍 常见问题

### Q: StatelessWidget 和 StatefulWidget 什么时候用？
**A:**
- `StatelessWidget`：内容不变化（静态页面）
- `StatefulWidget`：内容会变化（计数器、表单）

### Q: 如何选择状态管理方案？
**A:**
- 小型项目：`setState` 足够
- 中型项目：Provider
- 大型项目：Riverpod 或 Bloc

### Q: const 构造函数有什么用？
**A:** 使用 `const` 可以：
- 避免重复创建 Widget
- 减少 rebuild 次数
- 提升性能

```dart
const Text('Hello'),  // ✅ 推荐
Text('Hello'),        // ❌ 避免在树中重复创建
```

---

## 🎯 适用场景

当您需要以下操作时，使用此技能：
- 开发 Flutter 应用
- 理解 Flutter 架构
- 实现状态管理
- 优化 Flutter 应用性能
- 学习 Dart 语言
