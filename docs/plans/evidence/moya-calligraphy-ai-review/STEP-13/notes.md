# STEP-13 决策、冻结清单与风险 — moya 公共壳

> Agent D（MOYA）。对 STEP-04 冻结契约编程；UX 依 2026-09-09-moya-ux-spec.md §2.1/§3/§4.1。

## 关键决策与取舍

1. **导航：自绘 tabbar + redirectTo/reLaunch**（不用原生 tabBar、不用 custom tabBar 字段）
   - 原生 tabBar 单一 list≤5：家长 3 + 老师 4 无法共存；custom tabBar（app.json "tabBar.custom": true）必须配原生 tabBar 骨架，页面仍需注册为 tabBar page（switchTab 语义），双模式清栈语义反而复杂。
   - 采用：app.json 无 tabBar；`components/mode-tabbar` 自绘；模式内 `wx.redirectTo`（替换当前页，栈不累积）；跨模式/身份切换 `wx.reLaunch`（清栈重建，满足 MOYA-ROLE-01"切换后清理不属于新上下文的缓存"）；tab 集由 `core/nav.ts` 冻结表 + contexts 驱动。
2. **token 过期恢复：静默 wx.login 重登**。契约未定义 refresh 端点（login 响应 refresh_token 为可选、复用 imboy 机制但未给端点）→ 请求层 401 单飞（single-flight）调 session.refresh = 静默重登，重放原请求一次（幂等键不变）。后续后端若提供 refresh 端点，只改 session.silentRelogin 内部。
3. **可测性：platform.ts 依赖注入**。core 层仅依赖 WxLike 最小接口（request/login/storage 三件）；运行时自动绑全局 wx，测试绑内存 mock → 全部登录/刷新/多身份逻辑真实单测，无需模拟微信环境框架。
4. **API base URL**：`moya.debug.api_base` storage 覆盖 > 占位常量（env.example 同源）；开发者工具控制台可切换本地后端；真实值不入 Git。
5. **org_owner 归老师模式**（机构管理入口在老师端），记录于 nav.ts/modeForContext。
6. **tsconfig.build 启用 rewriteRelativeImportExtensions**：源码 ESM 风格 `.ts` 后缀 import（node 测试需要）与 tsc emit（CJS 产物）兼容。

## 公共壳冻结清单（给 Wave 3 Parent/Teacher Agent）

### core 模块 API（冻结，业务页面禁止绕过）

- `request<T>({path, method?, data?, auth?, idempotencyKey?, timeout?, allowRetry?}) → Promise<T>`：返回 envelope.payload；错误统一 `ApiError{kind: network|unauthorized|business|badResponse, code, message}`。业务码常量 `ERR`（errors.ts）。
- `login()/currentToken()/isLoggedIn()/logout()/ensureSession()`（session.ts）：页面只需 `ensureSession()`（index 已做，页面一般不用再调）。
- `fetchContexts()/switchContext(c)/pickStoredSelection(ctxs)/handleContextInvalid(err)/modeForContext(c)/clearStoredSelection()`（context.ts）：TeachingContext 全 TSID string。
- `PARENT_TABS/TEACHER_TABS/homePathForMode/tabsForContext/buildRoute/decodeRoute/switchTab/relaunchMode/relaunchIdentityPicker`（nav.ts）：新增页面不改导航表结构，页面注册进 app.json 后 build。
- `platform.ts`：`setWxImplementation()` 仅供测试；业务代码不得直接调 wx.request。

### 目录与页面骨架

- `src/pages/parent/{home,growth,profile}`（tab：作业/成长/我的）、`src/pages/teacher/{home,tasks,classes,profile}`（tab：待点评/作业/班级/我的）——四件套齐、已挂 mode-tabbar，替换 page-placeholder 即为业务页。
- `pages/index`（启动编排）、`pages/identity-picker`、`pages/no-identity`（勿动，属公共壳）。
- `src/components/`：`full-screen-loading`（唯一全屏态）、`state-view`（empty/error+retry）、`page-placeholder`、`mode-tabbar`（勿改结构）。
- 品牌 Token 在 `src/app.wxss`（page 级 CSS 变量：--ink/--paper/--sprout/--sprout-deep/--seal/--grid 等，UX §4.1 冻结值）+ 通用 class（.card/.btn-primary/.page-body）。

### 测试约定

新业务逻辑测试继续用 `tests/helpers/mock-wx.ts`（createMockWx + okEnvelope/errEnvelope）；每个 mock 请求必须显式声明响应（队列耗尽即断言失败，防假绿）。

## 已知风险

1. **后端联调未发生**（Agent B Wave 2 实现中）：请求层对契约编程，端点路径/envelope 形态若实现有偏差，Step 17 联调时修 request 层单点。
2. **微信工具导入/真机未验证**（BLOCKED_EXTERNAL）： redirectTo/reLaunch 流、自绘 tabbar 安全区、storage 序列化行为需工具/真机确认。
3. **隐私指引前置**：wx.login 在未配置隐私指引的小程序会被拦（README 已记录）。
4. **expires_in 精度**：存储 expiresAt 用 Date.now()+expires_in*1000（秒级字段），毫秒时钟漂移以 60s skew 缓冲；时钟严重漂移的设备会提前/延后重登（重登幂等，无害）。
5. **多身份记忆的宽松匹配**：快照缺可选字段（如无 workspace_id 的 org_owner）时按"空值不参与匹配"，理论上同 type+org+learner 的两个上下文若都缺 group_id 可能歧义命中第一个；真实数据 group_id 恒存在（教学上下文必挂班级），风险低，Step 17 用真实 contexts 复核。
6. **console 全禁**：Wave 3 若需调试日志，必须先在 core 建统一 logger（脱敏）再解禁对应 biome 规则，不得直接 console。
