-- 为 group_member 表添加 remark 字段（群备注，仅自己可见）
-- 与 alias（群内别名）区别：alias 是用户在群内的显示名称，remark 是用户对该群的个人备注
ALTER TABLE public.group_member
    ADD COLUMN IF NOT EXISTS remark varchar(200) DEFAULT '';

COMMENT ON COLUMN public.group_member.remark IS '群备注，仅该成员自己可见';
