-- 迁移 000026: 附件文件哈希字段 md5 -> file_hash256（算法 MD5 -> SHA-256）
--
-- 背景：md5 字段被后端注释标为「仅作完整性参考，不作安全边界」，但客户端
--   计算的 MD5 已被密码学攻破。改为 SHA-256，字段更名 file_hash256 以名副其实。
--
-- ⚠️ 存量数据：历史行的哈希值无法回算 SHA-256，故本迁移只做「重命名 + 拓宽列宽」，
--   保留旧值不动（旧行仍是 32 字符 MD5，新上传写 64 字符 SHA-256，列宽 64 兼容两者）。
--   引用计数触发器（user_collect.attach_file_hash256 <-> attachment.file_hash256）
--   按值 JOIN：同代数据（旧 md5<->旧 md5、新 sha256<->新 sha256）仍正确匹配；
--   仅「旧收藏引用了后来被重新上传成 sha256 的文件」这种跨代场景引用计数会失配
--   （已与业务确认接受，属极少数）。

-- 1) attachment.md5 -> file_hash256，并拓宽到 64 字符容纳 SHA-256 hex
ALTER TABLE public.attachment RENAME COLUMN md5 TO file_hash256;
ALTER TABLE public.attachment ALTER COLUMN file_hash256 TYPE character varying(64);
COMMENT ON COLUMN public.attachment.file_hash256 IS '附件文件哈希（SHA-256 hex；历史行为旧 MD5，仅作完整性参考）';

-- 2) user_collect.attach_md5 -> attach_file_hash256（存收藏附件哈希列表，逗号分隔）
ALTER TABLE public.user_collect RENAME COLUMN attach_md5 TO attach_file_hash256;
COMMENT ON COLUMN public.user_collect.attach_file_hash256 IS '收藏记录文件哈希,多个用逗号分割';

-- 3) 更新引用计数触发器函数：按新列名 JOIN
CREATE OR REPLACE FUNCTION public.imboy_user_collect_fun() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  IF (TG_OP = 'DELETE' OR TG_OP = 'TRUNCATE') THEN
    UPDATE public.attachment SET referer_time = referer_time - 1
      WHERE file_hash256 = any(string_to_array(OLD.attach_file_hash256, ','));
    RETURN OLD;
  ELSIF (TG_OP = 'UPDATE') THEN
    -- user_collect 业务上不会有单独修改哈希的可能性，忽略
    RETURN NEW;
  ELSIF (TG_OP = 'INSERT') THEN
    RETURN NEW;
  END IF;
end;
$$;

-- 4) 触发器监听列从 attach_md5 改为 attach_file_hash256（列清单不可 ALTER，须重建）
DROP TRIGGER IF EXISTS imboy_for_user_collect ON public.user_collect;
CREATE TRIGGER imboy_for_user_collect
    AFTER INSERT OR DELETE OR UPDATE OF attach_file_hash256 ON public.user_collect
    FOR EACH ROW EXECUTE FUNCTION public.imboy_user_collect_fun();
