# macOS 企业流程与文件审计验收

当前后端 d9391727、App a67e5655 上运行已有企业原生旅程；使用全量迁移的独立 PostgreSQL 和独立 Garage，仅合成账号与数据。

原生执行退出 0；登录、个人四栏/企业切换、组织部门管理、企业群真实消息、频道讨论、无 OA 官网入口、加入/退出企业通过。上传 32 字节文件后原生 WebView 收到授权内容 HTTP 200；删除后旧授权链接拒绝。数据库检查同一个实际 group_file 的上传、删除审计分别恰一条，限定企业、实际操作用户及 Human 角色。

`result.json` 记录两端候选、冻结源码/迁移/验收脚本摘要、日志摘要和四项数据库结果。验收执行期间源码未改变。

复现入口：`python3 /tmp/gz-macos-current-audit-sequential.py`，已有输出时须使用新前缀保留历史结果；临时脚本摘要在 result.json 中。原生测试入口为 imboyapp 的 `integration_test/enterprise/enterprise_scope_native_test.dart`。

本记录不证明 iOS、客户 OA 联调、预览画面像素、企业未读原生清除或阅读队列并发；完整目标仍待最终跨域与连续两次全量后端回归。
