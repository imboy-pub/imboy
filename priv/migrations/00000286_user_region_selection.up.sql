CREATE FUNCTION public.fn_user_region_selection_valid(selection jsonb)
RETURNS boolean
LANGUAGE sql IMMUTABLE STRICT
SET search_path = pg_catalog
AS $$
    SELECT CASE WHEN jsonb_typeof(selection) = 'object'
        AND jsonb_typeof(selection->'pathIds') = 'array'
        AND (NOT selection ? 'displaySnapshot'
             OR jsonb_typeof(selection->'displaySnapshot') = 'array')
    THEN (
        selection->'protocolVersion' = '"1.0"'::jsonb
        AND jsonb_typeof(selection->'datasetId') = 'string'
        AND length(selection->>'datasetId') >= 1
        AND jsonb_typeof(selection->'countryCode') = 'string'
        AND (selection->>'countryCode') COLLATE "C" ~ '^[A-Z]{2}$'
        AND jsonb_typeof(selection->'dataRevision') = 'string'
        AND (selection->>'dataRevision') COLLATE "C" ~ '^s-[a-f0-9]{16}$'
        AND jsonb_array_length(selection->'pathIds') >= 1
        AND selection->'pathIds'->0 = to_jsonb('cc:' || (selection->>'countryCode'))
        AND NOT EXISTS (
            SELECT FROM jsonb_array_elements(selection->'pathIds') AS node(value)
            WHERE jsonb_typeof(value) <> 'string' OR value = '""'::jsonb
        )
        AND NOT EXISTS (
            SELECT FROM jsonb_array_elements(selection->'pathIds') AS node(value)
            GROUP BY value HAVING count(*) > 1
        )
        AND NOT EXISTS (
            SELECT FROM jsonb_array_elements(coalesce(selection->'displaySnapshot', '[]')) AS label(value)
            WHERE jsonb_typeof(value) <> 'string'
        )
        AND (NOT selection ? 'displayLocale'
             OR (jsonb_typeof(selection->'displayLocale') = 'string'
                 AND length(selection->>'displayLocale') BETWEEN 2 AND 255))
        AND (selection - ARRAY[
            'protocolVersion', 'datasetId', 'countryCode', 'dataRevision', 'pathIds',
            'displaySnapshot', 'displayLocale'
        ]) = '{}'::jsonb
    ) IS TRUE ELSE false END
$$;

ALTER TABLE public."user"
    ADD COLUMN region_selection jsonb,
    ADD COLUMN region_selection_legacy varchar(80),
    ADD CONSTRAINT ck_user_region_selection_binding CHECK (
        (region_selection IS NULL AND region_selection_legacy IS NULL)
        OR (
            region_selection IS NOT NULL
            AND region_selection_legacy IS NOT NULL
            AND region_selection_legacy COLLATE "C" = region COLLATE "C"
            AND public.fn_user_region_selection_valid(region_selection)
        )
    );

CREATE FUNCTION public.fn_user_region_selection_clear_legacy()
RETURNS trigger
LANGUAGE plpgsql
SET search_path = pg_catalog
AS $$
BEGIN
    IF NEW.region COLLATE "C" IS DISTINCT FROM OLD.region COLLATE "C"
       AND NEW.region_selection IS NOT DISTINCT FROM OLD.region_selection
       AND NEW.region_selection_legacy COLLATE "C" IS NOT DISTINCT FROM OLD.region_selection_legacy COLLATE "C"
    THEN
        NEW.region_selection := NULL;
        NEW.region_selection_legacy := NULL;
    END IF;
    RETURN NEW;
END
$$;

CREATE TRIGGER trg_user_region_selection_clear_legacy
    BEFORE UPDATE OF region ON public."user"
    FOR EACH ROW EXECUTE FUNCTION public.fn_user_region_selection_clear_legacy();
