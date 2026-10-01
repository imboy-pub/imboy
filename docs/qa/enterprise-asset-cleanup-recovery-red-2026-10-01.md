# 企业附件清理失败无法重试

状态：历史 FAIL 证据。后续修复及验证见 [恢复与并发保护](enterprise-asset-cleanup-recovery-2026-10-01.md)；保留本次旧逻辑失败的原始记录。

生产 eb_asset_app:cleanup_pending 的 cleanup_delete 先调用 cleanup_asset，将 pending 元数据推进为 deleted，再调用 delete_private。对象删除失败虽返回 skipped，但状态已不可再进入 cleanup_gate（只接受 pending_confirm），导致恢复存储后的清理仍跳过该对象。默认 purge 的孤儿候选同样只接受 pending_confirm，因此不能靠自动 purge 恢复这个墓碑对象。

真实隔离复现 /tmp/imboy-seat-http.wjy3b6 exit 1：向 Garage 写入合成 pending 文件，在 PostgreSQL 将创建时间设为两小时前；临时移除本测试的存储配置，清理明确失败；恢复配置后重试。结果 status_after_failure=deleted、retry_deleted=[]、retry_skip_count=1、object_remains=true。最后严格要求元数据仍可重试 pending_confirm 的断言失败。首次调试的证据序列化错误已修正；此页仅记录修正后的产品缺陷运行。

修复须保证删除失败或响应不确定仍有持久重试依据，并在真正删除前重新保护 pending 状态、TTL、保留与 hold，避免并发 confirm 或新 hold 导致误删。不能简单交换两个调用顺序，也不能让所有 deleted 对象无条件进入清理。现有默认 purge 还存在事务外对象预删，不能据此宣布并发安全或直接开自动调度。

没有运行生产清理。独立容器、数据库、私桶、端口和生成凭证由门禁管理并已清理；归档不含存储配置、凭证或上传引用。证据目录：evidence/enterprise-asset-cleanup-recovery-red-2026-10-01。
