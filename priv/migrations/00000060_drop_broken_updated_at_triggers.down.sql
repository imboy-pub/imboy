-- 回滚 000060: 重建误挂的 updated_at 触发器
-- Rollback: recreate the broken updated_at triggers.
--
-- 注意：表仍无 updated_at 列，重建后这些表的 UPDATE 将再次失败。
-- 仅用于迁移回滚，不要在生产执行。

CREATE TRIGGER trg_user_friend_category_updated_at BEFORE UPDATE ON public.user_friend_category FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();
CREATE TRIGGER trg_user_denylist_updated_at BEFORE UPDATE ON public.user_denylist FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();
CREATE TRIGGER trg_group_log_updated_at BEFORE UPDATE ON public.group_log FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();
CREATE TRIGGER trg_group_random_code_updated_at BEFORE UPDATE ON public.group_random_code FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();
CREATE TRIGGER trg_user_group_category_updated_at BEFORE UPDATE ON public.user_group_category FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();
CREATE TRIGGER trg_group_tag_updated_at BEFORE UPDATE ON public.group_tag FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();
