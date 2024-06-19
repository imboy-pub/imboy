-- FUNCTION: public.imboy_msg_c2g_fun()

-- DROP FUNCTION IF EXISTS public.imboy_msg_c2g_fun();

CREATE OR REPLACE FUNCTION public.imboy_msg_c2g_fun()
    RETURNS trigger
    LANGUAGE 'plpgsql'
    COST 100
    VOLATILE NOT LEAKPROOF
AS $BODY$
begin
  IF (TG_OP = 'DELETE' OR TG_OP = 'TRUNCATE') THEN
    DELETE FROM public.msg_c2g_timeline WHERE msg_id = OLD.msg_id;
    return OLD;
  ELSIF (TG_OP = 'UPDATE') THEN
    return NEW;
  ELSIF (TG_OP = 'INSERT') THEN
  return NEW;
  END IF;
end;
$BODY$;

ALTER FUNCTION public.imboy_msg_c2g_fun()
    OWNER TO imboy_user;
