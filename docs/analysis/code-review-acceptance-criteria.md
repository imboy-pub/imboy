# imboy.pub 修复评估验收标准

> 配套文档：[code-review-fix-plan.md](./code-review-fix-plan.md)  
> 版本：1.0  
> 评审人：独立评审员（不能是修复者本人）

---

## 验收原则

1. **命令可重现**：每条验收标准都提供可直接执行的命令，输出结果即为通过/失败证明。
2. **零例外原则**：P0/P1 项不允许以"已知问题"理由豁免，必须全部通过。
3. **回归保护**：修复后必须保证原有测试套件全部通过，不允许删测试来使验收绿灯。
4. **双人原则**：P0 安全项由另一名工程师独立验证，不接受自我验收。

---

## CRITICAL 验收（T-01）

### AC-01：源码中无硬编码加密密钥

**验收命令**：

```bash
# 检查 Erlang 源码
grep -r "pLV8yWGUUnd3Y2gaHP5a\|e6Z8KuBnGCi2t7we" \
  /Users/leeyi/project/imboy.pub/imboy/src \
  --include="*.erl"

# 检查 Flutter 源码
grep -r "pLV8yWGUUnd3Y2gaHP5a\|e6Z8KuBnGCi2t7we\|signKey\s*=\s*[\"']" \
  /Users/leeyi/project/imboy.pub/imboyapp/lib \
  --include="*.dart"

# 检查 TypeScript 源码
grep -r "pLV8yWGUUnd3Y2gaHP5a\|e6Z8KuBnGCi2t7we" \
  /Users/leeyi/project/imboy.pub/imboy-admin-frontend/src \
  --include="*.ts" --include="*.tsx"
```

**通过条件**：以上三条命令均**无输出**（返回 0 行）。

**验收功能测试**：

```bash
# 验证 dev 环境不配置密钥时仍能正常启动
cd /Users/leeyi/project/imboy.pub/imboy
unset IMBOY_SOLIDIFIED_KEY IMBOY_SOLIDIFIED_KEY_IV
IMBOYENV=local make run &
sleep 5
# 应看到 WARNING 日志而非 ERROR/crash
grep "solidified_key not set" _build/default/rel/imboy/log/erlang.log.1
kill %1
```

**通过条件**：日志中出现 WARNING 行，服务正常启动，不 crash。

**CI 门禁验收**：

```bash
# 确认 CI workflow 包含此检查
grep -A5 "Reject hardcoded" \
  /Users/leeyi/project/imboy.pub/.github/workflows/ci.yml
```

**通过条件**：grep 返回包含 `pLV8yWGUUnd3Y2gaHP5a` 的检查步骤。

---

## HIGH 验收（T-02、T-03、T-04）

### AC-02：Web 桌面 ChatPanel 功能完整

**验收步骤**（需真机/模拟器）：

```bash
cd /Users/leeyi/project/imboy.pub/imboyapp
flutter analyze lib/page/chat/chat/chat_panel.dart
# 应零错误
```

**功能验收清单**（手动测试，需截图存档）：

| 验收项 | 操作 | 预期结果 |
|--------|------|---------|
| AC-02-1 | macOS 桌面版打开任一 C2C 会话 | 右侧面板显示消息列表，不显示 "TODO Phase 2.1" 占位文字 |
| AC-02-2 | 发送文本消息 | 消息出现在列表底部，WebSocket ACK 正常 |
| AC-02-3 | 接收消息 | 对端发送消息后，右侧面板实时更新 |
| AC-02-4 | 点击关闭按钮 | 面板收起，webShellProvider 状态清除 |
| AC-02-5 | 切换到 C2G 群聊 | 群聊消息列表正常显示 |

**代码验收**：

```bash
# 确认 chat_panel.dart 中不含 TODO Phase 2 占位符
grep "TODO Phase 2" \
  /Users/leeyi/project/imboy.pub/imboyapp/lib/page/chat/chat/chat_panel.dart
```

**通过条件**：grep **无输出**。

---

### AC-03：debugPrint 门禁生效

**验收命令**：

```bash
# 统计未受 guard 保护的裸 debugPrint（排除生成文件）
grep -rn "debugPrint(" \
  /Users/leeyi/project/imboy.pub/imboyapp/lib \
  --include="*.dart" \
  | grep -v "\.g\.dart" \
  | grep -v "assert(" \
  | grep -v "kDebugMode" \
  | wc -l
```

**通过条件**：输出数字 **≤ 30**（允许少量合理的调试输出，但须全部有 `kDebugMode` guard）。

**高风险文件专项验收**：

```bash
# 高风险文件中不含裸 debugPrint
for f in \
  "lib/component/location/amap_helper.dart" \
  "lib/page/settings/e2ee_social_create_page.dart" \
  "lib/page/contact/contact/contact_provider.dart"; do
  count=$(grep -c "debugPrint(" \
    /Users/leeyi/project/imboy.pub/imboyapp/$f 2>/dev/null || echo 0)
  guarded=$(grep -c "kDebugMode\|assert(" \
    /Users/leeyi/project/imboy.pub/imboyapp/$f 2>/dev/null || echo 0)
  echo "$f: $count debugPrint, $guarded guards"
done
```

**通过条件**：高风险文件中每处 `debugPrint` 均在同文件有对应 `kDebugMode` guard。

**lint 配置验收**：

```bash
grep "no_debug_print\|avoid_print" \
  /Users/leeyi/project/imboy.pub/imboyapp/analysis_options.yaml
```

**通过条件**：grep 返回非空（lint 规则已配置）。

---

### AC-04：amap_helper 使用统一 HTTP 客户端

**验收命令**：

```bash
# 不应存在裸 Dio() 实例化（排除 HttpClient 自身初始化）
grep -n "= Dio()" \
  /Users/leeyi/project/imboy.pub/imboyapp/lib/component/location/amap_helper.dart
```

**通过条件**：grep **无输出**。

**功能验收**：

```bash
# 运行相关测试
cd /Users/leeyi/project/imboy.pub/imboyapp
flutter test test/component/location/ --reporter=expanded 2>/dev/null \
  || echo "SKIP: 若无 location 测试则跳过，需在修复时补充"
```

---

## MEDIUM 验收（T-05、T-06、T-07、T-08、T-09）

### AC-05：chat_provider.dart 文件大小合规

**验收命令**：

```bash
# 检查拆分后所有文件均在 600 行以内
for f in \
  /Users/leeyi/project/imboy.pub/imboyapp/lib/page/chat/chat/chat_provider.dart \
  /Users/leeyi/project/imboy.pub/imboyapp/lib/page/chat/chat/chat_state.dart \
  /Users/leeyi/project/imboy.pub/imboyapp/lib/page/chat/chat/chat_network_service.dart \
  /Users/leeyi/project/imboy.pub/imboyapp/lib/page/chat/chat/chat_local_service.dart; do
  lines=$(wc -l < "$f" 2>/dev/null || echo "MISSING")
  echo "$lines $f"
  [ "$lines" = "MISSING" ] && echo "ERROR: file not found" && exit 1
  [ "$lines" -gt 600 ] && echo "ERROR: exceeds 600 lines" && exit 1
done
echo "PASS"
```

**通过条件**：输出 `PASS`，四个文件均存在且不超过 600 行。

**回归验收**：

```bash
cd /Users/leeyi/project/imboy.pub/imboyapp
flutter analyze lib/page/chat/ 2>&1 | grep -E "error|warning" | wc -l
```

**通过条件**：输出 **0**。

---

### AC-06：chat_page.dart 文件大小合规

```bash
for f in \
  /Users/leeyi/project/imboy.pub/imboyapp/lib/page/chat/chat/chat_page.dart \
  /Users/leeyi/project/imboy.pub/imboyapp/lib/page/chat/chat/chat_input_bar.dart \
  /Users/leeyi/project/imboy.pub/imboyapp/lib/page/chat/widget/chat_message_list.dart; do
  lines=$(wc -l < "$f" 2>/dev/null || echo "MISSING")
  echo "$lines $f"
  [ "$lines" = "MISSING" ] && echo "ERROR: file not found" && exit 1
  [ "$lines" -gt 700 ] && echo "ERROR: exceeds 700 lines" && exit 1
done
echo "PASS"
```

**通过条件**：输出 `PASS`。

---

### AC-07：message.dart 迁移完成

```bash
# 旧路径不再被任何业务代码 import
grep -r "import.*service/message\.dart" \
  /Users/leeyi/project/imboy.pub/imboyapp/lib \
  --include="*.dart" \
  | grep -v "service/message\.dart" \
  | wc -l
```

**通过条件**：输出 **0**（无外部引用）。

---

### AC-08：imboy_policy.erl 行数达标

```bash
wc -l /Users/leeyi/project/imboy.pub/imboy/src/lib/imboy_policy.erl
wc -l /Users/leeyi/project/imboy.pub/imboy/src/lib/imboy_policy_persistence.erl 2>/dev/null \
  || echo "ERROR: persistence module not created"
```

**通过条件**：
- `imboy_policy.erl` ≤ 1000 行
- `imboy_policy_persistence.erl` 存在且 ≤ 300 行

**Erlang 编译验收**：

```bash
cd /Users/leeyi/project/imboy.pub/imboy
make compile 2>&1 | grep -E "^src.*error" | wc -l
```

**通过条件**：输出 **0**。

---

### AC-09：VERSION 与 CHANGELOG 一致

```bash
version_file=$(cat /Users/leeyi/project/imboy.pub/VERSION | tr -d '[:space:]')
changelog_version=$(grep "^## \[" /Users/leeyi/project/imboy.pub/CHANGELOG.md \
  | grep -v "Unreleased" | head -1 | grep -oP '\[\K[^\]]+')
echo "VERSION file: $version_file"
echo "CHANGELOG latest: $changelog_version"
[ "$version_file" = "$changelog_version" ] && echo "PASS" || echo "FAIL: mismatch"
```

**通过条件**：输出 `PASS`。

---

## LOW 验收（T-10）

### AC-10：移除生产代码中 io:format 调试输出

```bash
# 检查非测试 Erlang 源码中的裸 io:format
grep -rn "io:format(" \
  /Users/leeyi/project/imboy.pub/imboy/src \
  --include="*.erl" \
  | grep -v "test\|_SUITE\|eunit_runner\|elib_async"
```

**通过条件**：grep **无输出**（或仅剩 elib_async 等工具类，有注释说明用途）。

---

## 全量回归验收（所有任务完成后必须执行）

### AR-01：Erlang 后端全量编译与测试

```bash
cd /Users/leeyi/project/imboy.pub/imboy

# 编译
make compile 2>&1 | tail -5

# 单元测试
make eunit 2>&1 | grep -E "^(All|Failed|Passed|Skipped)" | tail -3

# Dialyzer 类型检查（允许有已知 nowarn，但不允许新增错误）
make dialyze 2>&1 | grep "^imboy_src" | wc -l
```

**通过条件**：
- 编译无 `error` 输出
- `Failed: 0`
- Dialyzer 错误数 ≤ 修复前基准值（需记录修复前数值）

---

### AR-02：Flutter 静态分析与测试

```bash
cd /Users/leeyi/project/imboy.pub/imboyapp

# 静态分析
flutter analyze 2>&1 | tail -5

# 单元测试
flutter test --reporter=compact 2>&1 | tail -3
```

**通过条件**：
- `flutter analyze`：`No issues found!` 或仅剩修复前已有的 info 级别警告
- `flutter test`：`All tests passed!`

---

### AR-03：前端构建与测试

```bash
cd /Users/leeyi/project/imboy.pub/imboy-admin-frontend

# TypeScript 类型检查
npx tsc --noEmit 2>&1 | wc -l

# 单元测试
bun test 2>&1 | tail -5

# 构建
bun run build 2>&1 | tail -3
```

**通过条件**：
- tsc 错误数 **0**
- bun test 全绿
- build 成功（无 error 退出）

---

### AR-04：冒烟测试（端到端）

```bash
# 后端节点必须运行
cd /Users/leeyi/project/imboy.pub/imboy
make ctl ARGS="smoke all" 2>&1 | grep -E "^(PASS|FAIL|ERROR)" | sort | uniq -c
```

**通过条件**：无 `FAIL` 或 `ERROR` 行（或与修复前基准一致）。

---

## 验收矩阵总览

| 任务 | 优先级 | 验收命令 | 通过条件 | 功能测试 | 回归测试 |
|------|--------|---------|---------|---------|---------|
| T-01 密钥清除 | P0 | AC-01 | 零 grep 输出 + CI 门禁 | 启动 WARNING 不 crash | AR-01/02 |
| T-02 ChatPanel | P1 | AC-02 | 无 TODO 占位 | 手动截图 5 项 | AR-02 |
| T-03 debugPrint | P1 | AC-03 | ≤30 裸输出 | — | AR-02 |
| T-04 amap HTTP | P1 | AC-04 | 无裸 Dio() | 地图搜索正常 | AR-02 |
| T-05 provider 拆分 | P2 | AC-05 | 4 文件均 ≤600 | — | AR-02/04 |
| T-06 page 拆分 | P2 | AC-06 | 3 文件均 ≤700 | — | AR-02/04 |
| T-07 message 迁移 | P2 | AC-07 | 零 import | — | AR-02 |
| T-08 policy 拆分 | P2 | AC-08 | ≤1000 + 新文件 | — | AR-01 |
| T-09 VERSION | P2 | AC-09 | 输出 PASS | — | — |
| T-10 io:format | P3 | AC-10 | 零裸输出 | — | AR-01 |

---

## 验收签字区

| 任务 | 修复工程师 | 验收工程师 | 验收日期 | 结论 |
|------|-----------|-----------|---------|------|
| T-01 | | | | ☐ PASS / ☐ FAIL |
| T-02 | | | | ☐ PASS / ☐ FAIL |
| T-03 | | | | ☐ PASS / ☐ FAIL |
| T-04 | | | | ☐ PASS / ☐ FAIL |
| T-05 | | | | ☐ PASS / ☐ FAIL |
| T-06 | | | | ☐ PASS / ☐ FAIL |
| T-07 | | | | ☐ PASS / ☐ FAIL |
| T-08 | | | | ☐ PASS / ☐ FAIL |
| T-09 | | | | ☐ PASS / ☐ FAIL |
| T-10 | | | | ☐ PASS / ☐ FAIL |
| **全量回归** | | | | ☐ PASS / ☐ FAIL |
