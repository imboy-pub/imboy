# imboy.pub 代码审查修复计划

> 来源：2026-06-09 批判性代码审查报告  
> 版本：1.0  
> 负责人：待分配

---

## 文档说明

本文档将审查报告中的所有问题按执行顺序组织为可追踪的任务，每项任务包含：
- 问题定位（文件 + 行号）
- 修复方案（最小变更原则）
- 验收命令（可直接运行）

验收标准见配套文档 `code-review-acceptance-criteria.md`。

---

## P0 — 紧急（当天完成，阻断发布）

### T-01：移除 solidified_key 硬编码密钥

**问题**：`imboy/src/imboy_app.erl:403-404` 将 AES 密钥以明文字面量写入源码，且与 Flutter 客户端共享同一副本。一旦仓库公开，任何人均可解密 solidified 数据。

**根因**：开发便利性妥协——"dev 默认值"逃避了配置强制要求。

**修复方案**：

```erlang
%% 修改前（imboy_app.erl:403-404）
DevDefaultKey = <<"pLV8yWGUUnd3Y2gaHP5aggZ7wnKT9DqL">>,
DevDefaultIV  = <<"e6Z8KuBnGCi2t7we">>,

%% 修改后：从文件或随机生成，禁止字面量
ensure_solidified_keys() ->
    case normalize_secret(config_ds:env(solidified_key, <<>>)) of
        <<>> ->
            %% 非生产环境：生成稳定 dev key（基于节点名哈希，重启不变）
            Seed = erlang:phash2(node()),
            DevKey = base64:encode(crypto:hash(sha256, integer_to_binary(Seed))),
            DevIV  = binary:part(DevKey, 0, 16),
            ok = application:set_env(imboy, solidified_key, DevKey),
            ok = application:set_env(imboy, solidified_key_iv, DevIV),
            logger:warning(
                "[imboy] solidified_key not set — generated node-local dev key. "
                "MUST set IMBOY_SOLIDIFIED_KEY in production."
            ),
            ok;
        _ ->
            %% key 已配置，检查 iv
            case normalize_secret(config_ds:env(solidified_key_iv, <<>>)) of
                <<>> ->
                    Seed = erlang:phash2(node()),
                    DevKey = base64:encode(crypto:hash(sha256, integer_to_binary(Seed))),
                    DevIV = binary:part(DevKey, 0, 16),
                    ok = application:set_env(imboy, solidified_key_iv, DevIV),
                    logger:warning("[imboy] solidified_key_iv not set — using derived dev iv."),
                    ok;
                _ ->
                    ok
            end
    end.
```

同步在 Flutter 侧 `imboyapp/lib/service/encrypter.dart` 中移除对应硬编码值，改为从后端 `/init` 接口获取或通过环境注入。

**CI 门禁（加入 `.github/workflows/ci.yml`）**：

```yaml
- name: Reject hardcoded secrets
  run: |
    if grep -r "pLV8yWGUUnd3Y2gaHP5a\|e6Z8KuBnGCi2t7we" . \
       --include="*.erl" --include="*.dart" --include="*.ts"; then
      echo "BLOCKED: hardcoded solidified key found"
      exit 1
    fi
```

**预计工时**：2h（Erlang 1h + Flutter 0.5h + CI 0.5h）

---

## P1 — 高优先级（本迭代内完成）

### T-02：完成 ChatPanel Phase 2.1.b/c（Web 桌面聊天面板）

**问题**：`imboyapp/lib/page/chat/chat/chat_panel.dart:230` 是纯占位符，Web Shell 桌面端无法收发消息。版本已到 rc.3，此功能不应是未完成状态。

**修复方案**：

分三个子任务顺序完成：

**T-02a（1d）：接入 ChatMessageList**

```dart
// chat_panel.dart — 替换占位 body
body: messages != null
    ? ChatMessageList(
        messages: messages!,
        currentUid: currentUid,
        chatType: chatType,
      )
    : const _PlaceholderBody(),
```

**T-02b（0.5d）：接入 ChatInput**

```dart
// chat_panel.dart — 底部输入框
bottom: ChatInput(
  peerId: peerId,
  chatType: chatType,
  onSend: onSend,
),
```

**T-02c（0.5d）：接入 chatProvider 状态层**

```dart
// chat_panel.dart — 改为 ConsumerStatefulWidget，监听 chatProvider
final chatState = ref.watch(chatProvider(ChatKey(peerId: peerId, chatType: chatType)));
```

**预计工时**：2d

---

### T-03：Flutter debugPrint lint 门禁

**问题**：858 处 `debugPrint` 未受 `kDebugMode` 保护，在 profile 构建中泄露内部状态，`amap_helper.dart` 还在 debug 输出中包含 API 调用参数。

**修复方案**：

**步骤 1**：在 `analysis_options.yaml` 添加自定义规则

```yaml
# imboyapp/analysis_options.yaml
analyzer:
  plugins:
    - custom_lint
custom_lint:
  rules:
    - avoid_print: true      # 已有
    - no_debug_print_in_prod: true  # 新增，见下方
```

**步骤 2**：批量修复高风险的裸 debugPrint

优先处理以下包含敏感信息的文件：
- `amap_helper.dart:275,300` — 含 API key 参数，改为条件输出
- `contact_provider.dart:148,157,164,178` — 含 uid
- `e2ee_social_create_page.dart:271,278` — 含 E2EE shard 数量

修复模式：
```dart
// 修改前
debugPrint("amapapi_getAmapPoi ${queryParameters.toString()}");

// 修改后
assert(() {
  debugPrint("amapapi_getAmapPoi ${queryParameters.toString()}");
  return true;
}());
// 或直接删除不必要的 debug 输出
```

**步骤 3**：CI 门禁

```bash
# 脚本加入 CI：检查 release 模式不含裸 debugPrint
grep -rn "debugPrint(" lib/ --include="*.dart" \
  | grep -v "\.g\.dart" \
  | grep -v "assert(" \
  | grep -v "kDebugMode" \
  | wc -l | xargs -I{} sh -c '[ {} -lt 50 ] || (echo "Too many unguarded debugPrint: {}" && exit 1)'
```

**预计工时**：1d（批量修复 0.5d + lint 配置 0.5d）

---

### T-04：amap_helper 注入统一 HttpClient

**问题**：`amap_helper.dart:275,300` 直接 `Dio()` 实例化，绕过项目统一的重试/超时/监控拦截器，且无法在测试中 mock。

**修复方案**：

```dart
// 修改前（amap_helper.dart）
return await Dio().get("https://restapi.amap.com/...", ...);

// 修改后：通过构造函数注入或静态 getter
class AmapApi {
  const AmapApi({Dio? dio}) : _dio = dio ?? HttpClient.instance.dio;
  final Dio _dio;

  Future<Response<dynamic>> getAmapPoi(...) async {
    return _dio.get(
      "https://restapi.amap.com/v5/place/around",
      queryParameters: queryParameters,
    );
  }
}
```

**预计工时**：1h

---

## P2 — 中优先级（下一迭代）

### T-05：拆分 chat_provider.dart（2613 行 → 4 文件）

**问题**：`chat_provider.dart` 2613 行，混合了消息状态、网络调用、本地持久化、UI 事件处理，严重违反单一职责原则。

**拆分方案**：

```
chat_provider.dart (2613行) 拆分为：
├── chat_state.dart           (~200行) — 纯状态模型 + Freezed
├── chat_network_service.dart (~400行) — 网络请求（send/recall/edit）
├── chat_local_service.dart   (~300行) — SQLite 读写
└── chat_provider.dart        (~600行) — Riverpod provider + 协调层
```

**执行步骤**：
1. 先抽取 `ChatState` 和相关 Freezed 模型到 `chat_state.dart`
2. 抽取所有 `_sendMessage`/`_recallMessage`/`_editMessage` 到 `chat_network_service.dart`
3. 抽取所有 SQLite 操作到 `chat_local_service.dart`
4. 保持 `chat_provider.dart` 作为协调层，通过 `ref.read()` 调用上述服务

**验收**：每个文件不超过 600 行，`flutter analyze` 零错误。

**预计工时**：2d

---

### T-06：chat_page.dart 拆分（2433 行 → 3 文件）

**问题**：`chat_page.dart` 2433 行包含消息列表、输入框、工具栏、权限申请等多个 Widget。

**拆分方案**：

```
chat_page.dart (2433行) 拆分为：
├── chat_page.dart          (~400行) — 页面骨架 + Scaffold
├── chat_message_list.dart  (~600行) — 消息列表 Widget（已有但未充分利用）
├── chat_input_bar.dart     (~400行) — 输入框 + 附件工具栏
└── chat_toolbar.dart       (~300行) — 顶部工具栏 + 操作菜单
```

**预计工时**：1.5d

---

### T-07：完成 message.dart → messaging/ 迁移

**问题**：`message.dart`（1535 行）文件头注释说明需迁移到 `modules/messaging/public.dart`，但旧文件仍是主实现，双头维护。

**执行步骤**：
1. 审计 `message.dart` 中哪些函数已在 `messaging/` 中有对应实现
2. 逐函数将调用方切换到 `messaging/public.dart`
3. 用 `dart fix` 批量修改 import
4. 当 `message.dart` 仅剩 re-export 时，可安全删除

**验收**：`grep -r "import.*service/message.dart" lib/` 返回 0 结果。

**预计工时**：1.5d

---

### T-08：imboy_policy.erl §4 持久化层独立

**问题**：`imboy_policy.erl` 1285 行，文件头注释已规划拆分但 §4 Save/persist（约 225 行）仍在主文件。

**拆分方案**：

```
imboy_policy_persistence.erl (新建, ~225行)
  - save_admin_config/1
  - save_profile_settings/1
  - persist_feature_overrides/1
```

**验收**：`imboy_policy.erl` 降至 1000 行以内，`make eunit` 全绿。

**预计工时**：2h

---

### T-09：修复 VERSION 与 CHANGELOG 不一致

**问题**：`VERSION` 文件显示 `1.0.0-rc.3`，但 `CHANGELOG.md` 已有 `## [1.0.0] - 2026-04-14` 的 GA 条目。

**修复**：

```bash
# 确认当前正确版本后执行
echo "1.0.0" > /Users/leeyi/project/imboy.pub/VERSION
```

同时将 `deploy/.env.example` 中的 `IMBOY_VERSION=1.0.0-rc.1` 更新为当前版本。

**预计工时**：30min

---

## P3 — 低优先级（技术债偿还）

### T-10：移除 group_ds.erl 调试用 io:format

**问题**：`imboy/src/ds/group_ds.erl:54` 残留调试输出，在生产环境会污染标准输出。

```erlang
%% 删除此行
io:format("is_member/2  Uid ~p, Gid ~p, Res ~p, Size ~p\n", [Uid, Gid, Res, map_size(Res)])
%% 若需保留，替换为：
?DEBUG_LOG([Uid, Gid, Res, map_size(Res)])
```

**预计工时**：15min

---

### T-11：websocket_handler.erl 职责拆分（可选，下下迭代）

**问题**：856 行 handler 同时处理握手、消息分发、心跳、ACK，建议拆分但非紧急。

**方案参考**：
```
websocket_handler.erl (~250行) — 握手 + 路由分发
websocket_message_handler.erl (~300行) — 业务消息处理
websocket_heartbeat.erl (~100行) — 心跳 + 超时管理
```

**预计工时**：1d

---

### T-12：增加 Repo 层真实 DB 集成测试

**问题**：367 个测试文件中 161 个依赖 meck，mock 行为与真实 PG 实现可能漂移。

**方案**：选择 5 个核心 Repo 模块（user_repo、msg_c2c_repo、conversation_repo、channel_repo、attachment_repo），针对每个编写不使用 meck 的真实 DB 测试，使用测试事务回滚保证幂等性。

**预计工时**：3d

---

## 执行时间线

```
Week 1（当前迭代）
  Day 1:  T-01（安全密钥）+ T-09（VERSION）+ T-10（io:format）
  Day 2-3: T-02（ChatPanel Phase 2.1）
  Day 4:  T-03（debugPrint lint）
  Day 5:  T-04（amap_helper）

Week 2（下一迭代）
  Day 1-2: T-05（chat_provider 拆分）
  Day 3-4: T-06（chat_page 拆分）
  Day 5:   T-08（imboy_policy §4）

Week 3（技术债偿还）
  T-07（message.dart 迁移完成）
  T-12（Repo 集成测试）
  T-11（websocket 拆分，可选）
```

---

## 相关文档

- 验收标准：[code-review-acceptance-criteria.md](./code-review-acceptance-criteria.md)
- 原始审查报告：2026-06-09 审查会话记录
- 安全规范：[security-auth-middleware-audit.md](./security-auth-middleware-audit.md)
