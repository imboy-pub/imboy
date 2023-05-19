-- FUNCTION: public.imboy_user_for_fts_fun()

-- DROP FUNCTION IF EXISTS public.imboy_user_for_fts_fun();

CREATE OR REPLACE FUNCTION public.imboy_user_for_fts_fun()
    RETURNS trigger
    LANGUAGE 'plpgsql'
    COST 100
    VOLATILE NOT LEAKPROOF
AS $BODY$
begin
  IF (TG_OP = 'DELETE') THEN
    DELETE FROM public.fts_user WHERE user_id = OLD.id;
    DELETE FROM public.user_setting WHERE user_id = OLD.id;
    RETURN OLD;
  ELSIF (TG_OP = 'UPDATE') THEN
    INSERT INTO public.fts_user (user_id, allow_search, token) VALUES (new.id, 2, setweight(to_tsvector('jiebacfg', new.nickname), 'A') ||
          setweight(to_tsvector('jiebacfg', new.sign), 'B') ||
          setweight(to_tsvector('jiebacfg', new.region), 'C'))
    ON CONFLICT (user_id) DO UPDATE SET token = setweight(to_tsvector('jiebacfg', new.nickname), 'A') ||
          setweight(to_tsvector('jiebacfg', new.sign), 'B') ||
          setweight(to_tsvector('jiebacfg', new.region), 'C');
    -- UPDATE public.fts_user SET token = setweight(to_tsvector('jiebacfg', new.nickname), 'A') ||
    --       setweight(to_tsvector('jiebacfg', new.sign), 'B') ||
    --       setweight(to_tsvector('jiebacfg', new.region), 'B') WHERE user_id=NEW.id;
    RETURN NEW;
  ELSIF (TG_OP = 'INSERT') THEN
    INSERT INTO public.fts_user (user_id, allow_search, token) VALUES (new.id, 2, setweight(to_tsvector('jiebacfg', new.nickname), 'A') ||
          setweight(to_tsvector('jiebacfg', new.sign), 'B') ||
          setweight(to_tsvector('jiebacfg', new.region), 'C'));
  return new;
  END IF;
end;
$BODY$;

ALTER FUNCTION public.imboy_user_for_fts_fun()
    OWNER TO imboy_user;
