# 企业附件默认接入 Garage 与重复上传保护

状态：**PARTIAL**。生产默认对象存储已改为 Garage，并取得真实 Garage + PostgreSQL 应用链证据；客服网页附件全流程及生产资格尚未完成。

原装配直接调用进程内替身，服务重启后对象会丢失。现在 eb_asset_store 默认使用 eb_asset_object_garage，复用现有私桶、配置和签名；测试必须显式设置 eb_asset_object_store=stub，并在结束后恢复先前配置。未配置 Garage 不会回退内存，未知选择值明确拒绝。对象前缀与元数据 key 使用同一构造函数，上传响应的 adapter 字段改为 private_object_store。

同时修复真实数据丢失缺陷：原流程先 PUT 再登记元数据，重复提交同一凭证时登记冲突后的补偿 DELETE 会删掉第一次成功上传的对象。回归在旧顺序下真实失败（对象 not_found）。现在先用现有数据库唯一约束预留 pending_confirm 元数据，再写对象；重复和并发请求在触碰对象前被拒绝。两个实际应用请求共用同一上传凭证，严格只有一个成功、一个失败；再重复提交后，原字节仍存在并能正常确认、授权读取。

存储失败或响应不确定时保留该 pending 记录，交给现有有界清理流程；不自动重放、不删除前次请求拥有的对象。元数据登记失败不会写对象。这个顺序不声称数据库和 S3 构成分布式事务；自动清理调度和故障恢复仍需独立验收。

最终隔离运行 /tmp/imboy-seat-http.f0NhaV 与 /tmp/imboy-asset-garage.E60DJu exit 0：真实应用 facade 的 presign → PUT → confirm → content，通过真实经办和企业 ACL；跨企业及无权限主体拒绝；缺失配置返回明确错误并保留 pending；非法存储选择拒绝。另复用原存储合同的六个用例，在本次独立数据库与显式替身上下文内 6/6 通过。修改过的旧套件均编译，但没有宣称它们全部执行通过。

默认 HTTP 回归 /tmp/imboy-seat-http.irhb2i exit 0：Internal/工作区/QR 三组顶层检查 26/26，通过四种实际身份响应的封闭 schema 校验。负例 SQL 约束错误是主动注入的回滚验证，不是被忽略的测试失败。

复现：

```sh
IMBOY_DEPS_ROOT=/path/to/independent/deps \
IMBOY_ASSET_GARAGE_PG_CHECK=1 \
bash scripts/test/enterprise_asset_garage_gate.sh

IMBOY_DEPS_ROOT=/path/to/independent/deps \
bash scripts/test/customer_service_internal_http_gate.sh
```

默认使用已有 Garage v2.3.0 镜像；可指定另一个已安装镜像版本。本轮不证明 v2.4.1。数据库、容器、私桶、合成密钥和端口均独立；trap 清理容器，不使用真实凭证，不归档生成密钥、配置或上传凭证。

证据：[元数据](evidence/enterprise-asset-garage-assembly-2026-10-01/run.json)、[真实附件应用链](evidence/enterprise-asset-garage-assembly-2026-10-01/asset-pg.log)、[重复上传旧逻辑失败](evidence/enterprise-asset-garage-assembly-2026-10-01/duplicate-upload-red.log)、[HTTP 回归](evidence/enterprise-asset-garage-assembly-2026-10-01/regression.log)、[文件哈希](evidence/enterprise-asset-garage-assembly-2026-10-01/sha256.json)。本轮人工检查源代码、所有直接调用与断言，不宣称独立代理审查。

边界：没有自动迁移旧替身对象；已有仅存于内存的对象不会因此自动出现在 Garage。仍需完成真实 HTTP/客服网页附件收发和下载、自动清理调度、完整相关套件、真机及总体验收。未部署、未迁移生产数据。
