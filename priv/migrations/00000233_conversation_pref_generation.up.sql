-- 迁移 233: 会话偏好（pin/delete）世代绑定（channel-group-ux-v2.2 W09.b，plan §6.7）
--
-- plan §6.7："现有 pin/delete 行需保持与当前世代绑定；本期选择在现有
-- pin/delete 行增加适用 generation_id（C2C=0，C2G=真实当前成员世代），
-- 同时更新唯一键/旧写适配。旧行仅可绑定核实为 active 的当前世代，
-- 否则保留为不应用的历史。"
--
-- 世代真源复用 group_member_generation（id/start_seq；open = end_seq
-- IS NULL）。C2C 无成员世代恒 0（始终适用）；C2G 行仅当 generation_id
-- 等于该成员当前 active 世代才适用——"旧值不能用于新世代"由读取投影
-- 过滤承载，不靠客户端筛选。

ALTER TABLE public.conversation_pin
    ADD COLUMN generation_id bigint NOT NULL DEFAULT 0;

ALTER TABLE public.conversation_delete
    ADD COLUMN generation_id bigint NOT NULL DEFAULT 0;

-- 旧行一次性回填：C2G 旧行存在 active 世代（end_seq IS NULL）则绑定，
-- 否则保留 0（不应用的历史）。
UPDATE public.conversation_pin p
SET generation_id = g.id
FROM public.group_member_generation g
WHERE p.conversation_type = 'c2g'
  AND p.generation_id = 0
  AND g.group_id = p.conversation_id
  AND g.user_id = p.user_id
  AND g.end_seq IS NULL;

UPDATE public.conversation_delete d
SET generation_id = g.id
FROM public.group_member_generation g
WHERE d.conversation_type = 'c2g'
  AND d.generation_id = 0
  AND g.group_id = d.conversation_id
  AND g.user_id = d.user_id
  AND g.end_seq IS NULL;

CREATE INDEX i_conversation_pin_user_gen
    ON public.conversation_pin USING btree (user_id, generation_id);

CREATE INDEX i_conversation_delete_user_gen
    ON public.conversation_delete USING btree (user_id, generation_id);

COMMENT ON COLUMN public.conversation_pin.generation_id IS
    'W09 世代绑定：C2C=0（恒适用）；C2G=写入时的 active 成员世代，'
    '不等于当前世代则不适用（旧行不用于新世代）';
COMMENT ON COLUMN public.conversation_delete.generation_id IS
    'W09 世代绑定：语义同 conversation_pin.generation_id';
