-- TimescaleDB 唯一索引必须包含分区列 created_at，无法单靠索引阻止同一
-- msg_id 使用不同时间戳重放。触发器用事务级 advisory lock 串行化同表同
-- msg_id 的并发插入，再跨 chunk 判重；不创建永久增长的旁路去重表。
CREATE OR REPLACE FUNCTION public.fn_message_cross_timestamp_dedup()
RETURNS trigger
LANGUAGE plpgsql
AS $$
DECLARE
    existing_created_at timestamptz;
BEGIN
    PERFORM pg_advisory_xact_lock(
        hashtextextended(TG_ARGV[0] || ':' || NEW.msg_id, 0)
    );

    EXECUTE format(
        'SELECT created_at FROM public.%I WHERE msg_id = $1 ORDER BY created_at ASC LIMIT 1',
        TG_ARGV[0]
    ) INTO existing_created_at USING NEW.msg_id;

    IF existing_created_at IS NULL OR existing_created_at = NEW.created_at THEN
        RETURN NEW;
    END IF;
    RETURN NULL;
END;
$$;
--;
CREATE TRIGGER trg_msg_c2c_cross_timestamp_dedup
BEFORE INSERT ON public.msg_c2c
FOR EACH ROW EXECUTE FUNCTION public.fn_message_cross_timestamp_dedup('msg_c2c');
--;
CREATE TRIGGER trg_msg_c2g_cross_timestamp_dedup
BEFORE INSERT ON public.msg_c2g
FOR EACH ROW EXECUTE FUNCTION public.fn_message_cross_timestamp_dedup('msg_c2g');
--;
CREATE TRIGGER trg_msg_c2s_cross_timestamp_dedup
BEFORE INSERT ON public.msg_c2s
FOR EACH ROW EXECUTE FUNCTION public.fn_message_cross_timestamp_dedup('msg_c2s');
--;
CREATE TRIGGER trg_msg_s2c_cross_timestamp_dedup
BEFORE INSERT ON public.msg_s2c
FOR EACH ROW EXECUTE FUNCTION public.fn_message_cross_timestamp_dedup('msg_s2c');
--;
-- 先安装四个触发器，再扫描存量数据，避免检查与安装之间的写入窗口。
-- 如发现存量重复，迁移事务失败并一并回滚本次函数和触发器 DDL。
DO $$
DECLARE
    message_table text;
    has_duplicates boolean;
BEGIN
    FOREACH message_table IN ARRAY ARRAY['msg_c2c', 'msg_c2g', 'msg_c2s', 'msg_s2c']
    LOOP
        EXECUTE format(
            'SELECT EXISTS ('
            'SELECT 1 FROM public.%I GROUP BY msg_id '
            'HAVING min(created_at) <> max(created_at) LIMIT 1)',
            message_table
        ) INTO has_duplicates;

        IF has_duplicates THEN
            RAISE EXCEPTION 'cross-timestamp duplicate msg_id exists in public.%', message_table
                USING ERRCODE = 'unique_violation';
        END IF;
    END LOOP;
END;
$$;
--;
