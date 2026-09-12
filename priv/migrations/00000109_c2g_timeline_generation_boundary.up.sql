-- 迁移 00000109: C2G 离线时间线绑定权威会话序号
--
-- 不回填历史行：created_at/现有 ACK 状态都不能证明成员世代。NULL 必须由读取侧
-- fail-closed，避免退群后重入时重新取得旧世代消息，尤其是 Megolm room key。

ALTER TABLE public.msg_c2g_timeline
    ADD COLUMN conv_seq bigint;

ALTER TABLE public.msg_c2g_timeline
    ADD CONSTRAINT chk_msg_c2g_timeline_conv_seq_positive
    CHECK (conv_seq IS NULL OR conv_seq >= 1);

CREATE INDEX idx_c2g_timeline_generation_pending
    ON public.msg_c2g_timeline (to_uid, to_gid, conv_seq, created_at)
    WHERE client_ack = false AND conv_seq IS NOT NULL;

COMMENT ON COLUMN public.msg_c2g_timeline.conv_seq
    IS 'C2G persistent-accept sequence; NULL legacy rows are not eligible for offline delivery';
