

CREATE OR REPLACE FUNCTION public.timezone_offset() returns BIGINT LANGUAGE SQL STABLE as $$ SELECT (EXTRACT(TIMEZONE_HOUR FROM CURRENT_TIMESTAMP) * 3600 + EXTRACT(TIMEZONE_MINUTE FROM CURRENT_TIMESTAMP) * 60)*1000 $$;
