# 群文件受控打开 / Authorized group-file opening

日期 / Date: 2026-10-01。基线 / Base: Backend `b1c4b4a0d33af7a9c6637b48a8bde2ba5c2bb59f`; App `02cd5bb0949bbced207e20a836f2702a3fdb5c4f`。本地候选验证，不是投产验收 / Local candidate validation, not production acceptance.

## 行为 / Behavior

- 群文件详情、列表、搜索和分类列表增加可空 `object_key`，仅来自未删除附件的真实 `group_file_id`、`scope=group` 和匹配的群绑定；保留历史 `file_url` 字段，不自动猜测回填旧记录。
- 下载 Logic 先通过 DS 取得有效文件及绑定，再调用已有 `attach_logic:view_url` 重验群成员、世代和父级资格。无绑定、跨群绑定、附件删除、成员世代关闭均拒绝，不返回裸 `file_url`。
- App 优先采用绑定引用，每次主动打开都向既有 view_url 请求重新授权，不复用缓存或正在进行的旧签发。鉴权失败不会进入预览或外部回退；服务器明确返回空绑定时不退回裸 URL。
- 顶栏显示原生进度指示，防止重复打开；账号 / 群上下文变化后丢弃异步结果。旧服务端未提供绑定字段时仍沿用既有 Assets 授权路径，不把这种兼容分支当作当前企业权限闭环。
- API 下载合同修正为成功 302 Location、业务失败 HTTP 200 信封，与现有 handler 一致。索引迁移 157 对齐绑定查找，仅添加可回滚索引。

File reads expose a nullable canonical object key. Download reuses attachment authorization instead of the raw stored URL. The App fetches fresh authorization on explicit opening and never falls back to a raw URL after denial or an explicitly absent binding. Native progress and context checks prevent stale opens. Legacy servers retain the existing Assets path. The download contract documents 302 success and HTTP-200 business errors. Migration 157 adds only a reversible binding index.

## 验证 / Verification

- RED: 两个 download Logic 鉴权断言在修复前失败，日志 `/tmp/gz-group-file-download-red.log`。
- Backend EUnit: 文件 Repo / DS / Logic、受控下载、范围失权、附件与频道及相关归档案例，共 131 项通过，退出码 0；`/tmp/gz-group-file-controlled-unit.log`。
- 新建 PostgreSQL: `group_file_atomic_pg_tests:run(Socket)` 七项通过，退出码 0；`/tmp/gz-group-file-controlled-pg.log`。实际 SQL 验证元数据和绑定投影、错误绑定、删除、关闭世代、失权、上传事务；读取实际 157 up/down 文件验证回滚、重复 up 和数据不变。禁用 seqscan 的 EXPLAIN 只证明索引可用于该查询，不是吞吐基准。
- App: `flutter test test/unit_test/service/asset_url_resolver_test.dart test/unit_test/page/group/file/group_file_page_test.dart` 34 项通过，退出码 0；`/tmp/gz-group-file-controlled-app.log`。覆盖缓存 / inflight 不复用、鉴权失败零回退、空绑定拒绝、异步群切换丢弃、文档 / 图片 / 音视频与旧链接兼容。
- 相关 Dart 及 tests 的定向 analyze 为 `No issues found`，退出码 0；`/tmp/gz-group-file-controlled-analyze.log`。Erlfmt、diff 检查、三份 YAML 解析和迁移命名配对门通过（312 文件 / 156 对）。

PG 使用新建 Unix socket 合成库，真实 DS / Repo / 授权 SQL、元数据 INSERT 和迁移 DDL。OSS 上传、签名函数、ID 分配、预检成员缓存和连接池定位为替身；下载计数后台任务在该 fixture 跳过，另由 EUnit 覆盖。App 页面和签发请求为替身，不是实际 API / H5 / 存储或真机联调。

PG executes real SQL and migration files in a fresh synthetic database. Storage/signing, ID allocation, preflight cache and pool lookup are mocked; the fixture skips asynchronous download counting, which has separate EUnit coverage. App tests use fake page data and signing responses. No live storage, HTTP or device acceptance is claimed.

## 未闭环 / Remaining

已签发 Garage URL 在 600 秒有效期内不承诺即时撤销；旧 go-fastdfs 路径仍沿用现有授权兼容。旧版 App 的裸 URL、旧服务端兼容分支、已打开或本地缓存文件的实时撤权、无可靠绑定的历史记录恢复、独立群成员 / 管理角色并发撤销、孤立对象清理、完整迁移 / HTTP / 真机 / 真实存储均待验证。尚未应用迁移到业务数据库，也未发布三端。

Issued URLs are not instantly revocable; legacy storage and old-client compatibility remain. Historic unbound-record recovery, active-preview/cache revocation, concurrent membership/role revocation, orphan cleanup, full migrations and real HTTP/device/storage journeys are unfinished. No business database migration or release occurred.
