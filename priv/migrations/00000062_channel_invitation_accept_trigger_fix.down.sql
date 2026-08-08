-- 回滚 000062: 重建损坏的频道邀请接受触发器
-- Rollback: recreate the broken invitation accept trigger.
--
-- 注意：触发器 INSERT 仍无 id 列，重建后接受邀请将再次失败。
-- 仅用于迁移回滚，不要在生产执行。

CREATE FUNCTION public.fn_channel_invitation_accept() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    -- 当邀请被接受时，自动创建订阅关系
    IF NEW.status = 1 AND OLD.status = 0 THEN
        INSERT INTO public.channel_subscription (channel_id, user_id, subscribed_at, status)
        VALUES (NEW.channel_id, NEW.invitee_uid, NOW(), 1)
        ON CONFLICT (channel_id, user_id)
        DO UPDATE SET status = 1, subscribed_at = NOW();

        -- 更新邀请人的 accepted_at
        NEW.accepted_at := NOW();
    END IF;
    RETURN NEW;
END;
$$;

CREATE TRIGGER tr_channel_invitation_accept BEFORE UPDATE ON public.channel_invitation FOR EACH ROW EXECUTE FUNCTION public.fn_channel_invitation_accept();
