-- 迁移 0000227 down：恢复迁移前状态。
--   1) 删除 user_collect_key_state 表；
--   2) 恢复 imboy_user_collect_fun 为迁移 26 终版（UPDATE 分支无差分）；
--   3) 删除 generation / key_version 列。
-- 注意：down 会丢失 sealed 行版本元数据与高水位，仅用于开发/测试回滚；
--   生产回滚需先评估存量 sealed 数据。

DROP TABLE IF EXISTS public.user_collect_key_state;

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

ALTER TABLE public.user_collect DROP COLUMN IF EXISTS key_version;
ALTER TABLE public.user_collect DROP COLUMN IF EXISTS generation;
