-- 迁移 000060: 删除引用不存在列的 updated_at 触发器
-- Drop triggers that reference a non-existent updated_at column.
--
-- 背景 / Background:
--   00000009 给 39 张表统一挂了 BEFORE UPDATE ... set_updated_at() 触发器，
--   但以下 6 张表建表时（00000001 foundation）没有 updated_at 列。
--   set_updated_at() 函数体引用 NEW.updated_at，触发器执行时（即每次 UPDATE）
--   必抛 "record 'new' has no field 'updated_at'" → 该表所有 UPDATE 静默失败。
--   真机实锤：群分组重命名（UPDATE user_group_category）永远失败。
--   修复选择删触发器而非加列：这些表的 schema 设计本就没有时间戳列，
--   触发器是 00000009 批量挂载时的失误，删除即恢复表的正常更新语义。
--
-- 受影响的表 / Affected tables (6):
--   user_group_category   - 群分组：重命名/删除/排序/移动 全部失效（真机验证）
--   user_friend_category  - 好友分组：重命名等 UPDATE 失效
--   user_denylist         - 黑名单：含状态更新的操作失效
--   group_log             - 群操作日志：UPDATE 失效
--   group_random_code     - 群随机码：UPDATE 失效
--   group_tag             - 群标签：UPDATE 失效

DROP TRIGGER IF EXISTS trg_user_friend_category_updated_at ON public.user_friend_category;
DROP TRIGGER IF EXISTS trg_user_denylist_updated_at ON public.user_denylist;
DROP TRIGGER IF EXISTS trg_group_log_updated_at ON public.group_log;
DROP TRIGGER IF EXISTS trg_group_random_code_updated_at ON public.group_random_code;
DROP TRIGGER IF EXISTS trg_user_group_category_updated_at ON public.user_group_category;
DROP TRIGGER IF EXISTS trg_group_tag_updated_at ON public.group_tag;
