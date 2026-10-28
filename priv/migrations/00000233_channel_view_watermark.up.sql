-- 迁移 233: 频道查看更新边界水位（channel-group-ux-v2.2 W08，plan §6.8 C4/D3）
--
-- 单一更新边界语义（不是逐帖阅读记录）：
--   * channel.publish_seq       每频道已提交发布更新的单调序列计数器；
--   * channel_message.update_seq 发布时同事务从计数器取值（编辑/置顶不分配）；
--   * channel_subscription.view_seq
--                               订阅当前激活周期已查看边界 V，推进一律
--                               GREATEST（晚到旧边界不倒退，J13.d）；
--   * channel_subscription.activation_version
--                               激活版本：退订后重新订阅在状态变更事务内
--                               递增并重置 view_seq（复用订阅行 id 不代表
--                               复用旧查看资格，J13.e）。
--
-- 回填口径（plan §6.8）：
--   * 旧帖按当前稳定展示顺序（id 升序）一次性分配 1..N；
--   * 已有订阅 view_seq 保留 0——旧聚合 unread_count 不可推导逐帖看过
--     事实，宁可提示存量更新，也不从聚合数反推已看过；既有 unread_count
--     保留至新版权威重算完成，不把存量未读洗零。

ALTER TABLE public.channel
    ADD COLUMN publish_seq bigint DEFAULT 0 NOT NULL;

ALTER TABLE public.channel_message
    ADD COLUMN update_seq bigint;

ALTER TABLE public.channel_subscription
    ADD COLUMN view_seq bigint DEFAULT 0 NOT NULL;

ALTER TABLE public.channel_subscription
    ADD COLUMN activation_version bigint DEFAULT 0 NOT NULL;

-- 回填：旧帖按 (channel_id, id) 升序分配 1..N。
UPDATE public.channel_message m
SET update_seq = sub.rn
FROM (
    SELECT id,
           ROW_NUMBER() OVER (PARTITION BY channel_id ORDER BY id ASC) AS rn
    FROM public.channel_message
) sub
WHERE m.id = sub.id;

-- 频道计数器对齐到已回填最大序号（无帖频道保持 0）。
UPDATE public.channel c
SET publish_seq = sub.max_seq
FROM (
    SELECT channel_id, MAX(update_seq) AS max_seq
    FROM public.channel_message
    WHERE update_seq IS NOT NULL
    GROUP BY channel_id
) sub
WHERE c.id = sub.channel_id;

-- 水位查询索引：未查看更新按 (channel_id, update_seq) 过滤。
CREATE INDEX i_channel_message_channel_update_seq
    ON public.channel_message USING btree (channel_id, update_seq);

COMMENT ON COLUMN public.channel.publish_seq IS
    'W08 查看水位：每频道发布更新单调序列计数器（发帖事务内分配）';
COMMENT ON COLUMN public.channel_message.update_seq IS
    'W08 查看水位：发布时从频道 publish_seq 取值的单调序号（编辑/置顶不分配）';
COMMENT ON COLUMN public.channel_subscription.view_seq IS
    'W08 查看水位：当前激活周期已查看边界 V（GREATEST 推进，退订重订重置）';
COMMENT ON COLUMN public.channel_subscription.activation_version IS
    'W08 激活版本：重新订阅递增；旧 descriptor 迟到写按版本拒绝';
