-- 回滚 000026: file_hash256 -> md5（仅结构回退，不回算哈希值）
-- ⚠️ 若存量已有 64 字符 SHA-256 值，回退列宽到 40 会截断/失败，需先自行清理。

-- 4) 触发器改回监听 attach_md5
DROP TRIGGER IF EXISTS imboy_for_user_collect ON public.user_collect;

-- 3) 触发器函数改回旧列名
CREATE OR REPLACE FUNCTION public.imboy_user_collect_fun() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  IF (TG_OP = 'DELETE' OR TG_OP = 'TRUNCATE') THEN
    UPDATE public.attachment SET referer_time = referer_time - 1
      WHERE md5 = any(string_to_array(OLD.attach_md5, ','));
    RETURN OLD;
  ELSIF (TG_OP = 'UPDATE') THEN
    RETURN NEW;
  ELSIF (TG_OP = 'INSERT') THEN
    RETURN NEW;
  END IF;
end;
$$;

-- 2) user_collect 列改回
ALTER TABLE public.user_collect RENAME COLUMN attach_file_hash256 TO attach_md5;
COMMENT ON COLUMN public.user_collect.attach_md5 IS '收藏记录Md5,多个用逗号分割';

-- 1) attachment 列改回（先窄回 40 再改名）
ALTER TABLE public.attachment ALTER COLUMN file_hash256 TYPE character varying(40);
ALTER TABLE public.attachment RENAME COLUMN file_hash256 TO md5;
COMMENT ON COLUMN public.attachment.md5 IS '附件MD5';

-- 重建触发器
CREATE TRIGGER imboy_for_user_collect
    AFTER INSERT OR DELETE OR UPDATE OF attach_md5 ON public.user_collect
    FOR EACH ROW EXECUTE FUNCTION public.imboy_user_collect_fun();
