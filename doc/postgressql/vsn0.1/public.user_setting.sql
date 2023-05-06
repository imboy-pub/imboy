-- Table: public.user_setting

-- DROP TABLE IF EXISTS public.user_setting;

CREATE TABLE IF NOT EXISTS public.user_setting
(
    user_id bigint NOT NULL,
    setting json,
    updated_at bigint NOT NULL,
    CONSTRAINT pk_user_setting_uid PRIMARY KEY (user_id)
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.user_setting
    OWNER to imboy_user;

COMMENT ON TABLE public.user_setting
    IS '用户设置表';

COMMENT ON COLUMN public.user_setting.user_id
    IS '主键 用户表自增长ID';

COMMENT ON COLUMN public.user_setting.setting
    IS '更多设置：json 数据，不同的业务不用的key( add_friend_type 加我方式： mobile 手机号; account 账号; qrcode 二维码; group 群聊; visit_card 名片)';

COMMENT ON COLUMN public.user_setting.updated_at
    IS '更新记录Unix时间戳毫秒单位';
