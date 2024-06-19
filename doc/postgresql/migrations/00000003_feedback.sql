-- Table: public.feedback

-- DROP TABLE IF EXISTS public."feedback";

-- 导入数据后，需要更新自增长ID
-- select setval('"feedback_id_seq"', (select max(id) from public."feedback"));

CREATE TABLE IF NOT EXISTS public."feedback"
(
    id BIGSERIAL PRIMARY KEY,
    user_id bigint NOT NULL,
    device_id varchar(40) NOT NULL,
    client_operating_system varchar(80),
    client_operating_system_vsn varchar(680),
    feedback_md5 varchar(40),
    app_vsn varchar(40),
    type varchar(40),
    rating varchar(40),
    contact_detail varchar(200),
    body text not null,
    attach json,

    reply_count int NOT NULL DEFAULT 0,
    status smallint NOT NULL DEFAULT 1,
    updated_at bigint DEFAULT 0,
    created_at bigint NOT NULL,
    CONSTRAINT uk_FeedbackMd5 UNIQUE  (feedback_md5)
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.feedback OWNER to imboy_user;

COMMENT ON TABLE public.feedback IS '用户反馈表';

COMMENT ON COLUMN public.feedback.id IS '主键 自增长ID 反馈ID';

COMMENT ON COLUMN public.feedback.user_id IS '反馈用户ID';

COMMENT ON COLUMN public.feedback.client_operating_system IS '设备类型 web ios android macos windows';

COMMENT ON COLUMN public.feedback.device_id IS '设备ID web设备留空';
COMMENT ON COLUMN public.feedback.client_operating_system_vsn IS '设备版本 {"baseOS":"HUAWEI/CLT-AL00/HWINE:8.1.0/HUAWEICLT-AL00/173(C00):user/release-keys","sdkInt":27,"release":"8.1.0","codename":"REL","incremental":"176(C00)","previewSdkInt":0,"securityPatch":"2018-10-01"}';

COMMENT ON COLUMN public.feedback.type IS '反馈类型 bugReport featureRequest';
COMMENT ON COLUMN public.feedback.rating IS '反馈评级 bad neutral good';
COMMENT ON COLUMN public.feedback.contact_detail IS '联系方式';
COMMENT ON COLUMN public.feedback.attach IS '反馈内容附件';
COMMENT ON COLUMN public.feedback.body IS '反馈内容';
COMMENT ON COLUMN public.feedback.feedback_md5 IS '反馈唯一标识 md5(user_id + device_id + app_vsn + type + body)';
COMMENT ON COLUMN public.feedback.reply_count IS '回复数量，回复的回复也计算在内';
COMMENT ON COLUMN public.feedback.status IS '状态: -1 删除  0 禁用  1 启用 (待回复）  2 已回复  3 已完结（不允许回复了）';
COMMENT ON COLUMN public.feedback.created_at IS '创建记录Unix时间戳毫秒单位';
COMMENT ON COLUMN public.feedback.updated_at IS '更新记录Unix时间戳毫秒单位';
