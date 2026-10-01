# 企业附件真实 Garage 存储验证

状态：**PARTIAL**。企业附件默认装配仍使用 eb_asset_object_stub，重启会丢失进程内对象；本轮新增真实 Garage 实现及可运行门禁，尚未切换业务默认装配。

实现复用已有 elib_oss 的私桶、上传和配置，以及 elib_s3_sign 的签名能力。调用方只得到对象字节和摘要；签名 URL、存储配置与凭证留在基础设施层。跨前缀请求在网络访问前被拒绝，缺失凭证返回明确错误；读取与删除禁止自动重定向。

隔离门禁创建独立 Garage 容器、私桶与随机合成密钥，端口只绑定随机回环地址。写入含二进制字节的合成对象后，重启 Garage 并启动新的 Erlang VM，验证完整字节一致；匿名访问 403、跨企业前缀拒绝、错误签名 403、删除后 not_found、缺失配置拒绝。最终 /tmp/imboy-asset-garage.5OnRqU exit 0，trap 删除该容器；未触碰共享 Garage、真实数据或凭证。

首次重启验证发现 Docker 随机映射端口在重启后需要重新查询，门禁已重新读取映射并等待真实 S3 接口，再执行字节断言。

复现：`bash scripts/test/enterprise_asset_garage_gate.sh`。默认使用当前本机已有 dxflrs/garage:v2.3.0，镜像必须已存在；可通过 IMBOY_TEST_GARAGE_IMAGE 指定另一已安装版本。不据此声明 v2.4.1 或生产验收通过。

证据：[元数据](evidence/enterprise-asset-garage-transport-2026-10-01/run.json)、[实际运行结果](evidence/enterprise-asset-garage-transport-2026-10-01/result.log)、[文件哈希](evidence/enterprise-asset-garage-transport-2026-10-01/sha256.json)。不归档生成的密钥、配置或私有 CLI 输出。

下一步：替换 eb_asset_store 的直接替身调用，让生产默认使用真实存储、测试显式选择替身；保持元数据登记失败后的对象回收语义；验证真实 PG 的上传确认、客服收发、授权下载与跨企业拒绝。当前浏览器文本闭环不能证明附件已投产。本轮人工检查源代码和门禁，不宣称独立代理审查。
