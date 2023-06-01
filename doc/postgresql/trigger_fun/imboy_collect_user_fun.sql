-- FUNCTION: public.imboy_collect_user_fun()

-- DROP FUNCTION IF EXISTS public.imboy_collect_user_fun();

CREATE OR REPLACE FUNCTION public.imboy_collect_user_fun()
    RETURNS trigger
    LANGUAGE 'plpgsql'
    COST 100
    VOLATILE NOT LEAKPROOF
AS $BODY$
begin
  IF (TG_OP = 'DELETE') THEN
    UPDATE public.collect_resource SET referer_time = referer_time - 1 WHERE kind_id = OLD.kind_id;
    RETURN OLD;
  ELSIF (TG_OP = 'UPDATE') THEN
    RETURN NEW;
  ELSIF (TG_OP = 'INSERT') THEN
  return NEW;
  END IF;
end;
$BODY$;

ALTER FUNCTION public.imboy_collect_user_fun()
    OWNER TO imboy_user;
