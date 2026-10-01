# 坐席并发接单与真实断流恢复 / Seat concurrency and TCP recovery

状态 / Status: **PARTIAL**. CS-02 的并发接单与断流恢复取得真实网页证据；完整 CS-02 尚缺浏览器撤权验证，也不代表六项目标或生产就绪已完成。
Concurrent claim and stream recovery are verified in real browsers. Browser credential revocation and other delivery gates remain open.

两个独立 Chromium 坐席上下文通过真实 QR SSE 登录，并对同一 queued 会话同时点击接单。两条真实 POST 响应严格为一个 200、一个 409；测试按实际赢家继续流程，不固定 A 必须胜出。数据库只有一条 session.claimed 事件。
Two real Seat pages click claim concurrently. Exactly one request returns 200 and the other 409; the actual winner continues. PostgreSQL contains exactly one claim event.

仅切换 Chromium offline 不能证明已有 SSE 连接已断。因此测试将赢家上下文设为离线，并切断自有测试网关全部 TCP 连接，等待该页面实际收到 SSE requestfailed 后，访客继续通过真实页面发消息。恢复赢家网络后，不刷新页面、不重新登录、不手动重试，页面自动发起新 SSE 并显示离线期间消息，正文精确匹配只有一处。
Offline mode alone did not terminate the existing stream. The test cuts owned gateway TCP connections and verifies request failure before sending a visitor message. The winner reconnects and restores the message automatically without refresh/login/manual retry, rendering it once. All gateway streams are cut; visitor and loser may reconnect while the winner is held offline. This is not a seat-only gateway failure or a long-duration outage test.

继续通过页面转接、回复、结束和五星评价；数据库严格验证四条 canonical 消息（两个 contact、两个 business_identity）、四个唯一 client IDs、closed/rating 5 和完整生命周期事件。
The journey continues through transfer, reply, close and rating. Canonical database facts verify four unique messages and the completed lifecycle.

额外修复测试代理连接泄漏：客户端响应关闭时销毁上游请求，已关闭响应不再写错误包。原生 HTTP/SSE 检查在旧代理上失败，修复后 3/3 通过，包含真实流取消与上游 TCP 关闭；没有模拟客服响应或引入新依赖。
The test proxy now closes upstream requests when their consumer disconnects. A native SSE transport regression fails on the old host and passes after the fix; all three transport checks pass. No customer-service response is mocked.

复现 / Reproduce: 在 Admin 仓先 `bun run build:widget`；后端仓执行：

```sh
IMBOY_DEPS_ROOT=/path/to/independent/dependency/root \
IMBOY_CS_BROWSER_RUNNER=/Users/leeyi/project/imboy.pub/.Codex/worktrees/gz-enterprise-admin/scripts/test/customer-service-browser-journey.mjs \
bash scripts/test/customer_service_internal_http_gate.sh
```

Admin 仓运行 `node --test scripts/test/customer-service-static-host.test.mjs` 检查代理。最终浏览器运行 `/tmp/imboy-seat-http.5t14yq` exit 0；复用已核对字节一致的 immutable build，未重跑无关的全仓测试。旧 offline-only 运行因未观察到连接失败而正确失败；TCP-cut 的首次通过运行尚未包含代理清理修复，因此归档最终重跑结果。
The final browser run exits 0. Unchanged build bytes are verified against the previous archive. The offline-only attempt failed its disconnect oracle; the final run includes the proxy cleanup fix. No unrelated full-repository test is claimed.

证据 / Evidence: [metadata](evidence/cs-seat-concurrency-recovery-2026-10-01/run.json), [responses](evidence/cs-seat-concurrency-recovery-2026-10-01/browser-responses.json), [DB facts](evidence/cs-seat-concurrency-recovery-2026-10-01/browser-db-proof.json), [reconnected Seat screenshot](evidence/cs-seat-concurrency-recovery-2026-10-01/seat-reconnected.png), [file hashes](evidence/cs-seat-concurrency-recovery-2026-10-01/sha256.json). No fixture JWT is archived; all accounts/data/resources are synthetic and isolated.

待完成 / Pending: browser credential revocation, real object-storage attachments, physical devices, remaining organization/OA/Internal acceptance and release qualification.
