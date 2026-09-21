-- 迁移 00000138 down: owner_activation_invite（GZAPP-06）
-- 安全回滚：只删本迁移引入的表与索引（随表消失）；不动 organization /
-- organization_member / "user" 的任何行（预创建 Owner user 行与 org 归属
-- 由业务侧治理，回滚本表不级联）。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

DROP TABLE IF EXISTS owner_activation_invite;
