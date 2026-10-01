# 客服坐席真实工作台与扫码 SSE 修复 / Real Seat Console and QR SSE fix

状态 / Status: **PARTIAL**. 本次完成访客 Widget 与两个坐席工作台的真实网页主链，不代表全部客服验收或六项目标已完成。
The real visitor and two Seat Console pages pass their primary journey. Full customer-service acceptance and the six-goal delivery remain incomplete.

生产缺陷 / Production defect: `cowboy_req:stream_body/3` 返回 `ok`；旧 QR SSE handler 将其当成 Cowboy request 回传，导致 scanned/confirmed/heartbeat 回调崩溃。页面靠 QR status 轮询回退仍能登录，原单测的 stream_body 替身错误地返回了 request map，因而掩盖缺陷。现发送后保留原 request，所有相关回调统一修复。
Cowboy stream_body returns ok. The handler incorrectly passed that value to cowboy_loop as the request, crashing event/heartbeat handling. Polling fallback masked the failure in the browser, and an inaccurate unit mock masked it in tests. The handler now retains the original request after sending.

验证 / Validation:

- 修正替身合同后，旧实现 6 项失败、12 项通过；实现修复后 18/18 通过，无跳过。
  Corrected mock: old implementation failed 6 of 18 checks; fixed implementation passed all 18.
- 真实 Chromium 中两个独立坐席浏览器上下文，经各自二维码与真实 scan/confirm HTTP 完成登录；二维码确认通过 SSE 到达，QR status 请求次数为零，浏览器存储无 JWT。扫码手机侧使用夹具签发的合成身份，不冒充真机扫码。
  Separate Seat browser contexts complete real QR HTTP confirmation and SSE login, without polling or JWT storage. Synthetic mobile credentials are used; physical-device scanning is not proven.
- 访客页面同意告知并发文本；A 页面点击接单、输入回复、选择 B 并转接；B 页面打开会话、回复、点击结束；访客页面收到两条回复并评价 5 星。坐席会话写动作均由构建后的工作台按钮触发，不再由测试调用写接口代替。
  All Seat session writes are triggered through actual workbench controls.
- 数据库严格断言：一条访客和两条坐席 canonical 消息，client IDs 不重复；会话 closed、rating 5；opened/claimed/transferred/closed/rated 事件齐全。
  Canonical database facts verify message uniqueness, lifecycle and rating.
- 接口/PG/QR 独立回归 26 项通过，四种真实身份响应符合合同。最终浏览器后端日志无旧 cowboy_loop(ok) 崩溃。
  The isolated regression passes 26 checks and four identity response contracts; the old callback crash is absent.

复现 / Reproduce: 先在 Admin 仓 `bun run build:widget`；再在后端仓运行：

```sh
IMBOY_DEPS_ROOT=/path/to/independent/dependency/root \
IMBOY_CS_BROWSER_RUNNER=/Users/leeyi/project/imboy.pub/.Codex/worktrees/gz-enterprise-admin/scripts/test/customer-service-browser-journey.mjs \
bash scripts/test/customer_service_internal_http_gate.sh
```

去掉 browser runner 环境变量执行独立接口/PG/QR 回归。门禁每次新建并清理自有 Docker PG、全量迁移 marker 库、原生 Cowboy listener 和临时端口，不接入共享历史库。异常转储落自有运行目录。最终运行分别为 `/tmp/imboy-seat-http.y9MTIB` 和 `/tmp/imboy-seat-http.c51CvA`，退出均为 0。
Without the browser runner variable, the gate runs the separate regression. Each run owns its disposable PG container, migrated marker database and ephemeral ports; crash dumps are confined to its run directory.

证据 / Evidence: [metadata](evidence/cs-seat-workbench-browser-2026-10-01/run.json), [DB facts](evidence/cs-seat-workbench-browser-2026-10-01/browser-db-proof.json), [Seat screenshot](evidence/cs-seat-workbench-browser-2026-10-01/seat-workbench.png), [hashes](evidence/cs-seat-workbench-browser-2026-10-01/sha256.json). Exact unchanged build files reuse the previous immutable Widget browser archive; metadata binds its manifest hash. No fixture JWT is archived. Initial TSID registration failure was a fixture issue; the baseline browser PASS with backend crashes was not accepted as final evidence. An initial attempt to read the browser-consumed SSE body could not retrieve it after the client abort; the final oracle uses successful SSE response, authenticated workspace and zero status-poll requests.

待完成 / Pending: real mobile/device scanning, actual object-storage attachments, two-browser concurrent claim, browser reconnect/revocation, remaining organization/OA/Internal gates, and production readiness.
