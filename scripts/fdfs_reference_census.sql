-- go-fastdfs 引用普查（只读）
-- 用途：迁移前查清 fastdfs 历史 URL（/img/、/audio/、/files/ 等）散落在哪些表/列。
-- 运行（人工，经堡垒机）：
--   docker exec -i prod_imboy_pg18 psql -U imboy_user -d imboy_pro -f - < scripts/fdfs_reference_census.sql
-- 输出 NOTICE 行：fastdfs_ref <表>.<列> = <条数>
-- 通用扫描 public schema 所有 text/varchar 列，匹配 ^/<word>/<year> 形态的相对 fastdfs 路径。

DO $$
DECLARE
    r record;
    n bigint;
    pat text := '^/[a-z]+/2[0-9]{3}';
BEGIN
    FOR r IN
        SELECT table_name, column_name
        FROM information_schema.columns
        WHERE table_schema = 'public'
          AND data_type IN ('text', 'character varying')
    LOOP
        BEGIN
            EXECUTE format('SELECT count(*) FROM public.%I WHERE %I ~ %L',
                           r.table_name, r.column_name, pat)
            INTO n;
            IF n > 0 THEN
                RAISE NOTICE 'fastdfs_ref %.% = %', r.table_name, r.column_name, n;
            END IF;
        EXCEPTION WHEN others THEN
            NULL;  -- 跳过无法扫描的列
        END;
    END LOOP;
END $$;
