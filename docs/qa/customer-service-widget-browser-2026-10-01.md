# 客服真实访客网页闭环 / Real visitor Widget journey

状态 / Status: **PARTIAL**. 客服六项目标尚未完成；本证据不等同于 CS-01/02/03 完整验收或生产就绪。
The six-goal delivery remains incomplete. These checks do not close the full CS acceptance contract or establish production readiness.

真实构建 Widget → 原生 Chromium → 隔离全迁移 PostgreSQL → 生产 Cowboy 路由/认证/客服应用，完成同意告知、访客发文本、坐席 A 接单回复、转接 B、B 回复、结束和页面五星评价。坐席操作通过真实 HTTP，使用隔离夹具签发的合成 JWT/设备签名，不经过登录页面或坐席工作台按钮。
The built Widget runs in native Chromium against production routes and a disposable migrated database. The journey covers consent, visitor text, Seat A claim/reply, transfer to B, B reply, close, and five-star rating. Seat controls use actual HTTP with synthetic fixture JWT/device signatures; login and Seat Console clicks are not covered.

数据库断言 / Database oracle: exactly three canonical messages (one contact, two business identities with the expected unique client IDs); one closed session, rating 5; opened/claimed/transferred/closed/rated events present. 页面同时显示访客及两条坐席回复，且无 pageerror。
The page displays the visitor message and both replies with no page errors. Database facts confirm the lifecycle and rating.

复现 / Reproduce (first build Widget in the Admin repository):

```sh
cd /Users/leeyi/project/imboy.pub/.Codex/worktrees/gz-enterprise-admin
bun run build:widget
cd /Users/leeyi/project/imboy.pub/.Codex/worktrees/gz-enterprise-backend
IMBOY_DEPS_ROOT=/path/to/independent/dependency/root \
IMBOY_CS_BROWSER_RUNNER=/Users/leeyi/project/imboy.pub/.Codex/worktrees/gz-enterprise-admin/scripts/test/customer-service-browser-journey.mjs \
bash scripts/test/customer_service_internal_http_gate.sh
```

`IMBOY_DEPS_ROOT` must contain installed `deps/*/ebin` and `ebin/imboy.app` metadata. This run used `/tmp/gz-oa-revoke-deps.1t0g2u2x`; every product module was compiled from current candidate source into the owned run directory. No shared ebin was rebuilt. Docker image: `imboy/pg18:3.6.1-2`; the run creates and removes its own container/volume. Ports are ephemeral; binding fails on a collision rather than attaching to an existing server.

浏览器模式与原有接口模式互斥，前者不冒充原有全接口回归；去掉 `IMBOY_CS_BROWSER_RUNNER` 可运行原模式。本次两种模式分别运行成功：浏览器命令 exit 0，原有八个顶层检查及四种身份响应合同检查通过。数据库隔离于同一 PG 服务上的其他任务。
Browser mode is separate from Internal conformance mode. Both ran successfully; the normal mode passed eight top-level checks and four identity response contract checks. There were no real accounts, contacts, external notifications or production writes.

证据 / Evidence: [run metadata](evidence/cs-widget-browser-2026-10-01/run.json), [database proof](evidence/cs-widget-browser-2026-10-01/browser-db-proof.json), [file hashes](evidence/cs-widget-browser-2026-10-01/sha256.json), [chat screenshot](evidence/cs-widget-browser-2026-10-01/visitor-chat.png), [rating screenshot](evidence/cs-widget-browser-2026-10-01/visitor-rated.png). The archive includes exact Widget/Seat build artifacts and manifest, but excludes fixture JWTs. The initial exploratory run also passed, before the stronger database assertions were added; the archived final run is authoritative.

未覆盖 / Pending: actual Seat Console browser interactions, object-storage attachments, simultaneous two-seat browser claims, reconnect/revocation journeys in this browser runner, native devices and remaining organization/OA/Internal delivery gates.
