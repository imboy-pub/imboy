-- 修复频道消息点赞报错（42846 cannot_coerce）：
-- 00000084 把空对象兜底改为 '{}'::jsonb，但聚合仍用返回 json 类型的聚合函数，
-- COALESCE(json, jsonb) 无公共类型，触发器在首次点赞时即抛
-- "COALESCE could not convert type jsonb to json" 并回滚整个反应事务。
-- channel_message.reaction_summary 列类型为 jsonb，此处统一为 jsonb 聚合。
CREATE OR REPLACE FUNCTION public.fn_update_channel_message_reaction_summary() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    UPDATE public.channel_message cm
    SET reaction_summary = COALESCE((
        SELECT jsonb_object_agg(reaction_type, cnt)
        FROM (
            SELECT reaction_type, COUNT(*) as cnt
            FROM public.channel_reaction
            WHERE message_id = COALESCE(NEW.message_id, OLD.message_id)
            GROUP BY reaction_type
        ) sub
    ), '{}'::jsonb)
    WHERE id = COALESCE(NEW.message_id, OLD.message_id);
    RETURN COALESCE(NEW, OLD);
END;
$$;
--;
