
CREATE OR REPLACE FUNCTION imsch.unix_now() returns BIGINT LANGUAGE SQL STABLE as $$ SELECT floor(EXTRACT(epoch FROM ((CURRENT_TIMESTAMP - ('1970-01-01 00:00:00'::timestamp without time zone)::timestamp with time zone) * (1000)::double precision)))::BIGINT $$;
