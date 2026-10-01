# 客服网页真实附件与当前经办权限

状态：PARTIAL。客服网页附件及本轮鉴权修复取得真实链路证据；六项目标及投产资格仍未完成。

此前坐席 B 抢单成功仍因 not_assignee 无法下载。根因是附件把企业会话入口身份当作客服当前经办，而客服 claim/transfer 的权威身份位于 customer_service_session。现在通过客服 facade 读取同企业、工作区、会话的当前经办及 enabled 事实，附件的 presign、PUT 重鉴权、confirm、content 共用 eb_asset_scope 校验。入口身份保持稳定，不迁移历史消息或客户归属；销售会话继续使用企业经办。没有托管客服 session 的企业客服会话保持原 ACL；有托管但尚未分配、坐席缺失或停用均拒绝；查询错误不回退入口身份。裁剪 customer_service 时客服分支拒绝，销售不调用被裁模块。

同时补齐 presign/confirm/content 三个精确浏览器路径的设备签名豁免，JWT、企业、职能和经办门不变；额外尾路径和治理路径不得命中。Widget iframe 只增加 allow-downloads，不给导航、弹窗、表单权限。Seat/Widget 内容通道拒绝 HTTP 200 JSON 错误信封，不再把错误保存成文件。

最终 /tmp/imboy-seat-http.vXDbMH 与 /tmp/imboy-asset-garage.Z4tryP exit 0，真实 Garage v2.3.0、PostgreSQL 和 Chromium：

- 访客上传 → 坐席下载 29 字节；坐席上传 → 访客下载 26 字节，两方向逐字节相等。
- 匿名访问真实 content 路径 401；转接后旧坐席 403、新坐席 200，后者字节仍与原件一致；坐席停用后旧实际浏览器凭证下载 403。
- 实际 QR SSE 登录无轮询回退；两个网页并发抢单恰一成功一冲突；断开真实网关 TCP 后自动续流；转接、停用恢复、新 QR 登录、结束、访客评分均通过。
- PG 最终 6 条消息、6 个唯一 client_msg_id、双方各 3 条；2 个 active 且绑定消息的附件，哈希与两份合成原件相等；会话 closed、评分 5，claim 事件恰一条。

72 项客户端相关测试通过；宿主原生检查 3/3；类型检查、针对产品改动 lint、新构建/资产扫描及显式后端配对通过。默认 HTTP 回归 /tmp/imboy-seat-http.XuTJY3 exit 0，顶层 26/26 和四种实际身份 schema 校验通过。真实 Garage + PG 应用回归 /tmp/imboy-seat-http.4wjGu9 exit 0，原存储合同六例显式替身回归、非托管企业客服 ACL、重复/并发 PUT、元数据预留和缺失配置拒绝通过。未宣称全仓 EUnit/所有旧附件套件已执行。

复现：

```sh
IMBOY_DEPS_ROOT=/path/to/independent/deps \
IMBOY_CS_BROWSER_ATTACHMENTS=1 \
IMBOY_CS_BROWSER_RUNNER=/path/to/imboyadmin/scripts/test/customer-service-browser-journey.mjs \
bash scripts/test/enterprise_asset_garage_gate.sh
```

新完整构建保存在 evidence/cs-attachment-authority-2026-10-01/build。manifest 的 source_head 是构建前 base，实际测试源以 run.json 的变更文件哈希绑定，不能称为完整冻结候选验收。证据不含生成存储凭证、浏览器 fixture JWT、上传凭证或自签 HTTPS 私钥。HTTPS 自签忽略仅用于隔离测试；没有修改产品 TLS 验证。停用通过真实应用 facade 控制，不证明治理 HTTP/UI 的权限链。

仍需自动 pending 清理调度、历史对象迁移决策、完整相关回归、真机及六项目标总体验收。未 push、部署或生产迁移。本轮人工审查调用链与源码，不宣称独立代理审查。
