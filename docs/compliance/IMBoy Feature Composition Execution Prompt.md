# IMBoy Feature Composition - New Session Execution Prompt

你是 IMBoy 项目的资深 Erlang/OTP、Flutter、React/Vite、构建系统与产品架构工程师。请在当前代码库中执行“单一 Feature Manifest 驱动三端可组合构建”的第一阶段闭环。

## 目标

一个人工维护的 canonical product-feature manifest 决定：

- Backend release 编译/注册哪些业务功能；
- Flutter 构建包含哪些功能页面、路由、可选依赖与资产；
- Admin 只包含并展示 Base + 已编译且运行时启用功能的管理模块；
- 三个产物内嵌相同 manifest schema version/hash，防止组合漂移。

本会话默认只完成 `F-00 -> F-01 -> F-02`。完成并验收后停止，不要直接展开 `F-03..F-09`。计划来源：

`imboy/docs/compliance/IMBoy Overseas Compliance Implementation Plan.md`

## 开始前必须做

1. `/Users/leeyi/project/imboy.pub` 是 umbrella workspace，不是 Git 仓库。分别检查：

```bash
git -C /Users/leeyi/project/imboy.pub/imboy rev-parse --show-toplevel
git -C /Users/leeyi/project/imboy.pub/imboyapp rev-parse --show-toplevel
git -C /Users/leeyi/project/imboy.pub/imboyadmin rev-parse --show-toplevel
git -C /Users/leeyi/project/imboy.pub/imboy status --short
git -C /Users/leeyi/project/imboy.pub/imboyapp status --short
git -C /Users/leeyi/project/imboy.pub/imboyadmin status --short
```

2. 完整读取并遵守：

- 根级 `AGENTS.md`
- `imboy/CLAUDE.md`，以及存在时的 `imboy/AGENTS.md`
- `imboyapp/AGENTS.md` 与 `imboyapp/CLAUDE.md`
- `imboyadmin/CLAUDE.md`，以及存在时的 `imboyadmin/AGENTS.md`
- 上述 Implementation Plan 和 Gap Matrix

3. 保护三个仓库的现有脏工作树。不得 reset/clean/checkout 覆盖用户修改；冲突时调整独占文件，无法隔离则停止并报告。

## 已知可复用基础，必须先验证当前 HEAD

- Backend：`src/lib/imboy_policy.erl`、`src/lib/imboy_policy_catalog.erl`、`src/lib/imboy_profile_preset.erl`、`imboy_feature:feature_names/0`、feature dependencies、plugin manifests、capabilities/profile/effective features。
- Flutter：`lib/app_core/feature_flags/app_feature_registry.dart`、`app_manifest_service.dart`、`route_feature_guard.dart`。
- Admin：`src/components/auth/FeatureRoute.tsx`、`src/hooks/useAdminFeatures.ts`、`src/components/layout/Sidebar.tsx`、`src/App.tsx`。

不要因为文件或字段存在就认定能力完整。追踪 manifest/config -> generator/registry -> import/route -> runtime enforcement -> build artifact。

## 必须遵守的架构边界

1. `compiled_features` 是构建时上限；`runtime_enabled_features` 只能继续关闭：

```text
effective_features = compiled_features intersect runtime_enabled_features
```

2. Flutter/Admin manifest 不是授权依据。Backend 必须继续执行 authentication、authorization、RBAC、report/block/safety enforcement。
3. 运行时隐藏菜单、`if (enabled)`、route guard 不等于构建裁剪。关闭的 Flutter 功能不得被 Base 静态 import 拉入 Dart 编译图；关闭的 Admin 模块不得进入 Vite module graph/chunk。
4. Backend 关闭功能的 REST/WS 路由和 worker 不注册；无法安全剥离的共享模块先列入 Base，禁止为减包重写 Message/Group/Channel/Workspace/E2EE。
5. 数据库默认保持兼容 schema superset，不为每个组合生成 migration 分叉。功能关闭不等于删除既有数据，也不能关闭依法或按政策需要的 retention/deletion job。
6. 不新增通用插件平台、规则引擎、模板引擎或构建框架。优先复用仓库已有语言、脚本、catalog 和生成模式；不得随意添加依赖。
7. Base 不可随意关闭。至少调查并决定：account/auth/session/device、authorization、health/config/upgrade、security audit/logging、privacy notice/account deletion；只要包含 UGC/互动能力，还必须包含 report/block/moderation foundations。每项须写出不可关闭理由和代码证据。
8. 不硬编码国家分支。地区 policy profile 与产品编译组合是不同维度。

## 本会话任务

### F-00 Feature Boundary Inventory

先输出并写入计划指定位置的 inventory，覆盖所有现有产品功能及：Backend routes/WS/actions/workers/modules、Flutter routes/screens/import roots、Admin routes/menu/modules、SDK/assets/permissions、tables、依赖和 Base/optional/shared/unknown 分类。

验收：每个 feature 有 owner、依赖闭包、实际文件/函数证据；无法分离的项标记 Architecture Gap，不猜测。

### F-01 Canonical Product Feature Manifest

基于仓库现有 config/schema 规范选择路径，不要先假定文件名。定义最小、版本化、可验证的单一 manifest。依赖关系尽量引用现有 catalog，不得复制出第二套真源。包含 canonical ordering 和 manifest hash 规则，不放 secrets。

验收：Base-only、selected-feature 两个 fixture 可表达；unknown/duplicate、缺依赖、试图关闭 Base、版本错误全部 fail closed。

### F-02 Generator and Dependency Validation

实现一个最小 deterministic generator，输出三端所需的 sorted generated contract/registry 和相同 schema version/hash，并提供 `--check` 或等效 stale-output 检查。

验收：相同输入 byte-stable；依赖闭包/cycle/stale/hash mismatch 有 runnable checks；本阶段不声称已经完成实际 Backend/Flutter/Admin build slicing。

## 工作方式与验收记录

每个任务开始前记录：base SHA、当前脏文件、任务 owner、独占修改文件、依赖和 stop condition。每个任务结束记录：exact commands、exit code、测试数量/结果、证据路径、diff 和 residual risk。

最小测试必须覆盖：

- Base-only manifest；
- full-selected 或代表性 selected manifest；
- unknown/duplicate feature；
- missing/cyclic dependency；
- Base disable；
- stale generated output；
- deterministic hash/output；
- 三端生成 contract 的 schema version/hash 一致。

禁止把 skip、测试体执行前的基础设施失败、mock-only 或静态扫描记为 PASS。结束结论只允许 `PASS`、`PARTIAL`、`BLOCKED`、`NO-GO`。

## 禁止操作

- 不 push、不 deploy、不发布、不操作商店/生产账号；
- 不发送邮件、消息、通知或使用任何联系方式；
- 不修改联系人、Git author/committer；
- 未经用户明确授权不创建 Git commit；
- 不修改 E2EE、安全边界或现有产品定位；
- 不为了未来需求增加 KYC、AI moderation、支付、Creator Economy；
- 不把 runtime feature flag 当成 build slicing 完成证据。

## 最终输出

按 `F-00/F-01/F-02` 分别报告：状态、文件、实现、测试命令与结果、验收证据、未完成项、下一任务依赖。最后明确说明当前是否仅完成“manifest/generator 基础”，以及尚未完成的 `F-03 Backend`、`F-04 Flutter`、`F-05 Admin`、`F-06 dependencies/assets/permissions` 和 `F-07 artifact consistency`，不得声称已经实现三端按需打包。
