-- FUNCTION: public.imboy_user_collect_fun()

-- DROP FUNCTION IF EXISTS public.imboy_user_collect_fun();

CREATE OR REPLACE FUNCTION public.imboy_user_collect_fun()
    RETURNS trigger
    LANGUAGE 'plpgsql'
    COST 100
    VOLATILE NOT LEAKPROOF
AS $BODY$
begin
  IF (TG_OP = 'DELETE' OR TG_OP = 'TRUNCATE') THEN
    UPDATE public.attachment SET referer_time = referer_time - 1 WHERE md5 = any(string_to_array(OLD.attach_md5, ','));
    RETURN OLD;
  ELSIF (TG_OP = 'UPDATE') THEN
    -- user_collect 业务上不会有单独修改md5的可能性
    -- 所以不要考虑md5字段修改的情况
    RETURN NEW;
  ELSIF (TG_OP = 'INSERT') THEN
  return NEW;
  END IF;
end;
$BODY$;

ALTER FUNCTION public.imboy_user_collect_fun()
    OWNER TO imboy_user;
