-- FUNCTION: public.imboy_user_tag_fun()

-- DROP FUNCTION IF EXISTS public.imboy_user_tag_fun();

CREATE OR REPLACE FUNCTION public.imboy_user_tag_fun()
    RETURNS trigger
    LANGUAGE 'plpgsql'
    COST 100
    VOLATILE NOT LEAKPROOF
AS $BODY$
begin
  IF (TG_OP = 'DELETE' OR TG_OP = 'TRUNCATE') THEN
    UPDATE public.tag SET referer_time = referer_time - 1 WHERE scene = OLD.scene and id = OLD.tag_id;
    RETURN OLD;
  ELSIF (TG_OP = 'UPDATE') THEN
    -- user_tag 业务上不会有单独修改name的可能性
    -- 所以不要考虑name字段修改的情况
    RETURN NEW;
  ELSIF (TG_OP = 'INSERT') THEN
    UPDATE public.tag SET referer_time = referer_time + 1 WHERE scene = NEW.scene and id = NEW.tag_id;
  return NEW;
  END IF;
end;
$BODY$;

ALTER FUNCTION public.imboy_user_tag_fun()
    OWNER TO imboy_user;
