# STEP-13 测试证据 — moya 公共壳

> `npm test`：52 tests / 15 suites / 52 pass / 0 fail（node:test，全 mock；真实联调属 Step 17）
> mock 基建：`tests/helpers/mock-wx.ts`（内存 storage + 可编程 request 队列 + login code 注入），经 `core/platform.ts` 依赖注入，core 层无一行依赖真实 wx。

## 验收项 → 测试映射

### MOYA-AUTH-01（模拟登录/刷新 + token 不入日志）

- `session-context.test.ts::session`：
  - wx.login code → POST /auth/wechat-mini/login（断言免 Bearer、body 只带 code）→ token 持久化（moya.auth.* key 隔离 + refresh_token 预留）；
  - 登录业务失败 5402 → ApiError(business) 不落会话；
  - expires_in=0 → ensureSession 静默重登（第二次 login 请求发生）→ 新 token 生效；
  - logout 清 token/refresh/会话态。
- `request.test.ts`：
  - HTTP 401 + envelope code=5420 → unauthorized（401 判定先于 envelope，不被业务码遮蔽）；
  - 401 → 单飞刷新（重登）→ 原请求重放，幂等键与 Authorization 更新断言（Bearer t1-old → t2）；
  - 刷新后仍 401 → unauthorized；
  - 错误对象 JSON 序列化不含 token/Bearer/原始 body 字段。
- `no-token-log.test.ts`（grep 断言）：src/ 无任何 console.*；Error/ApiError 构造行无 token 插值拼接；core 层 token 字样仅白名单语义（头注入/存储/类型/注释）。

### MOYA-ROLE-01（单身份直达/多身份显式切换/无权限不可见 + 后端拒绝呈现）

- 单身份：fetchContexts 1 条 → 无记忆也可直接取用（列表第 1 个）；
- 多身份：switch 落记忆（五元组）→ 重新拉取命中上次选择；身份被移除 → 失配自动放弃（null）；switch 5420 失败不落记忆；
- 失效恢复：5420/5421 → reselect（清记忆）；unauthorized → relogin（已断言 logout 后 isLoggedIn=false）；
- 无权限入口不可见：tabsForContext(guardian)=PARENT_TABS(3)、tabsForContext(teacher)=TEACHER_TABS(4)（导航由 contexts 驱动）；
- 直接触达未授权页：mock 403/5424 → ApiError(business).message 可展示（"非本班任课老师"）。

### MOYA-ID-01（最大 64-bit TSID 全链路往返）

`id-roundtrip.test.ts`："9223372036854775807"（上界）与 "9223372036854775806"：
响应解析 → storage 记忆（含 JSON 序列化往返）→ 记忆匹配读回 → buildRoute/decodeRoute 路由参数 → 渲染数据（setData 语义 = JSON 往返），每步严格等于原字符串；并以 `BigInt(Number(MAX)) !== BigInt(MAX)` 证明 number 载体必然丢精度。另：DTO 边界把 number 形态 organization_id 直接拒绝（badResponse）。

### 契约判定顺序（硬约束）

`request.test.ts::判定顺序`：401 先于 envelope（真实状态码语义）；HTTP 200 + code!=0 → business 携带码；code=0 → payload；非 JSON/缺字段 → badResponse。

### 基础工具回归

tsid/format 的 Step 12 用例全部保留通过（52 = Step 12 的 26 + Step 13 新增 26）。
