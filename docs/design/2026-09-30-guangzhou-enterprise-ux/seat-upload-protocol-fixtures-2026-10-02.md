# 客服坐席附件上传协议夹具闭环

源码基线 `3a429c4e7f4726a073f071e42d70fab5d6320016`。

正确生产配置模块来源下执行 `eb_seat_send_protocol_tests`：初始运行 `/tmp/imboy-seat-http.RpvTIm` 退出 1，5 项通过、1 项失败，失败为读取缺失的 `upload.url`。生产接口仅在配置 HTTPS API 基址时下发代理上传地址；本地合成配置未提供基址。修正后第二轮 `/tmp/imboy-seat-http.wATzN4` 退出 1，6 项通过、1 项失败，PUT 返回 500：套件声明使用本地替身存储，却未显式选择它。

## 改动

- 正常上传链显式设置 `https://synthetic-seat.invalid`，结束时恢复原基址。仍检查 URL 形状、查询参数编码、短 TTL、不泄漏对象 key/存储签名，并按返回路径执行真实 HTTP PUT。
- 新增未配置基址与 HTTP 基址两种负例：presign 成功响应不得包含上传 URL。
- 复用 `eb_pg_test_fixture:select_asset_stub/0` 和 `restore_asset_store/1`，与邻近 `eb_tenant_handler_tests` 使用同一装配和恢复方式；生产端没有默认回退到替身。
- 保留 confirm、空正文附件发送、附件白名单回显、幂等重放、跨企业/工作区凭证拒绝、无身份/无权限 PUT 拒绝检查。

只修改该测试模块。已检查全部相关调用和邻近装配模式，未进行独立代理审查；生产上传、签名和权限实现未改。

## 证据与限制

`/tmp/gz-seat-attachment-protocol-native-gate.sh` 逐项核对未变化源码哈希，重新编译变化测试模块；运行前检查真实生产配置模块来源、应用启动和 PostgreSQL 时间编解码。

运行 `/tmp/imboy-seat-http.o1tRkC` 退出 0，全部 7 项通过，0 项跳过。HTTP 路径、facade、数据库及消息/附件投影为真实实现；身份事实为测试 probe，对象存储为显式本地替身。合成 HTTPS 地址只用于地址合同检查，请求按其路径发到本地 HTTP 监听器，没有对该域名发起网络请求；不代表 TLS、真实 Garage 或完整生产认证链验收。

整体投产仍未证实，完整回归、专项隔离、三端旅程、真机和外部 OA 验收尚未完成。
