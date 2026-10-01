# 坐席停用与恢复：真实网页验证

状态：**PARTIAL**。本轮取得 CS-02 的坐席停用网页与写接口证据；六项目标及生产就绪仍未完成。

两个真实 Chromium 坐席页面经 QR SSE 登录，完成并发接单（200/409）、真实 TCP 断流恢复、转接及回复。在会话仍 active 时，以隔离夹具的可信测试进程调用实际 cs_seat_app 停用当前经办坐席。使用该页面实际持有的旧 Web 凭证发送有效写请求，后端返回 403；页面无需刷新即移除回复入口并说明权限状态。数据库恰好四条消息，拒绝写入没有落库。

随后通过同一实际应用层恢复坐席，创建独立浏览器上下文重新 QR SSE 登录，结束原会话并由访客页面完成五星评价。数据库证明 closed/rating 5、唯一一次接单及 seat.suspended/seat.resumed 事件。

首次检查失败的原因是对已移除输入框调用 isEnabled，定位器等待耗尽了轮询预算；改为先检查节点是否存在，再读取 enabled。最终运行 /tmp/imboy-seat-http.U1JF4k exit 0，未修改产品代码，也未放宽业务断言。

复现：Admin 仓先 bun run build:widget；后端仓执行：

```sh
IMBOY_DEPS_ROOT=/path/to/independent/dependency/root \
IMBOY_CS_BROWSER_RUNNER=/path/to/imboyadmin/scripts/test/customer-service-browser-journey.mjs \
bash scripts/test/customer_service_internal_http_gate.sh
```

证据：[运行元数据](evidence/cs-seat-revocation-browser-2026-10-01/run.json)、[真实响应](evidence/cs-seat-revocation-browser-2026-10-01/browser-result.json)、[数据库事实](evidence/cs-seat-revocation-browser-2026-10-01/browser-db-proof.json)、[停用后网页](evidence/cs-seat-revocation-browser-2026-10-01/seat-revoked.png)、[文件哈希](evidence/cs-seat-revocation-browser-2026-10-01/sha256.json)。复用已逐文件核对一致的静态产物；不归档夹具 JWT 或页面捕获的凭证。测试数据和数据库均隔离。

边界：停用与恢复通过实际应用层执行，不证明管理后台按钮或治理 HTTP 的授权链；坐席事实停用不是 JWT 永久吊销。真实对象存储附件、真机、其余企业/OA/Internal 验收、全量发布资格仍待完成。本轮人工检查测试代码与断言，不宣称独立代理审查。
