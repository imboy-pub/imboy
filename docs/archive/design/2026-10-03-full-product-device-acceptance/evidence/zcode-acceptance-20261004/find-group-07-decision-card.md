# Decision Card: FIND-GROUP-07 解阻选项 (a) — 群应用 service 可测试性注入钩子

> 提案状态：**PENDING_USER**（DEC-2 选项 a，见 decision-requests-log.json）
> 提案人：ZCode（验收协调）| 2026-10-04
> 性质：测试基建最小改动（`@visibleForTesting`，零生产行为变化）；**未获批准不实施**。

## 1. 问题

group_vote/task/schedule/file/album/vote_detail 等页面的数据链走
`GroupVoteService.to` 等 **`static final` 单例**（私有构造 + 私有 `_api` 字段），
widget 测试的 FakeAsync zone 无法注入 Fake——约 70+ 行 TARGET_MISSING 验收用例
无法本地构造（f00-group-widget-calibration-batch4.json 受阻定性）。

## 2. 精确方案（逐文件）

对每个涉事 service（GroupVoteService、GroupTaskService、GroupScheduleService、
GroupFileService、GroupAlbumService，及 detail 页对应者）做同构两行级改动：

```dart
// 现状
class GroupVoteService {
  static final GroupVoteService to = GroupVoteService._privateConstructor();
  GroupVoteService._privateConstructor();
  final GroupVoteApi _api = GroupVoteApi();
  ...
}

// 改为（仅增两行，不动既有任何行）
class GroupVoteService {
  static final GroupVoteService to = GroupVoteService._privateConstructor();
  GroupVoteService._privateConstructor();

  @visibleForTesting
  static GroupVoteService? testOverride;   // 新增：仅测试装配点

  static GroupVoteService get toOrDefault =>
      testOverride ?? to;                    // 新增：读取点
  ...
}
```

- 调用方（页面/provider）**不改**：仍引用 `XxxService.to` 的地方逐文件评估——
  若页面直引 `.to`，则测试经由 provider Spy 已绕开 service 的场景不受影响；
  需要 service 真实编排逻辑（如 vote 的分页封装）的用例，读取点改用
  `toOrDefault`（同样两行级、生产语义 = `to`）。
- `import 'package:meta/meta.dart'`（或 flutter foundation）按文件补。
- **生产路径行为变化：零**（`testOverride` 默认 null，`toOrDefault === to`）。

## 3. 影响面与回归

| 项 | 内容 |
|---|---|
| 触碰文件 | 5-8 个 service 文件，每文件 +2~4 行；0 个页面/逻辑文件 |
| 静态检查 | `dart analyze lib test/unit_test --fatal-infos`（L3 口径） |
| 回归 | 现有全部单测套件（group 10 套件 + contact 5 套件 + 服务既有单测）全绿 |
| 新增 | 每页一套 widget 套件（ffi no-isolate + Spy/override 已定型模式），批量补齐 70+ 行判定 |
| 回滚 | 单 commit revert，无迁移/无数据/无配置 |

## 4. 不做什么

- 不改任何 API 语义、不改任何 UI、不改任何存储；
- 不触碰 E2EE 计划 lease 域（olm/megolm/crypto 线）；
- 不在本卡内实施——实施以本卡获批准（回复「②a」）为唯一触发。

## 5. 批准后的执行序列

1. 上述两行级改动逐 service 提交（独立功能 commit，身份
   `leeyi <leeyisoft@qq.com>`）；
2. 按 ffi no-isolate + Spy 模式逐页落套件（vote → task → schedule → file →
   album → detail 页），每套件独立 commit；
3. 校准注记 batch5+ 登记全部收敛行；
4. 更新 w3-domain-summary.md 覆盖表。
