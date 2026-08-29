-- CI-00：迁移链与代码的 schema 漂移修复
-- user_repo:find_by_uid/1 的列清单引用 updated_at，但迁移链（00000001~
-- 00000079）从未创建该列（存量库经手工运维获得）。任何按迁移链新建的
-- 库上，用户查询都会因 unknown column 失败（db_query_performance_tests
-- 全样本 not_found 即此根因）。补齐列定义；IF NOT EXISTS 对已有库幂等。
ALTER TABLE public."user" ADD COLUMN IF NOT EXISTS updated_at timestamp with time zone;
