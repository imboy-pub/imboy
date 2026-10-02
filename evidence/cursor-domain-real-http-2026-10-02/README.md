# Human / Internal 游标隔离真实 HTTP 验收

真实 Cowboy、JWT / Application 凭据和独立 PostgreSQL；同一生产签名密钥，不替换游标实现。两域正常续页后交叉使用游标均返回 400 invalid_request；第二位合法 Human 也拒绝第一位 Human 的游标。39 项相关测试通过，独立只读审查 APPROVE。

仅证明本证据列出的分页与绑定行为，不宣称全目录无漏遍历。原始日志留在受限临时目录，不提交令牌。
