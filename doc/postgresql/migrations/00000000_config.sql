-- Table: public.config

-- DROP TABLE IF EXISTS public.config;

CREATE TABLE IF NOT EXISTS public.config
(
    tab varchar(20) COLLATE pg_catalog."default" NOT NULL,
    key varchar(40) COLLATE pg_catalog."default" NOT NULL,
    value text NOT NULL,
    title varchar(40) COLLATE pg_catalog."default" NOT NULL,
    sort integer NOT NULL DEFAULT 20,
    remark varchar(200) COLLATE pg_catalog."default" NOT NULL,
    system smallint NOT NULL DEFAULT 0,
    status smallint NOT NULL DEFAULT 1,
    created_at bigint NOT NULL DEFAULT 0,
    updated_at bigint NOT NULL DEFAULT 0,
    CONSTRAINT config_pkey PRIMARY KEY (key)
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.config OWNER to imboy_user;

COMMENT ON TABLE public.config IS '系统配置';

COMMENT ON COLUMN public.config.tab IS '配置选项，便于后台分类浏览';

COMMENT ON COLUMN public.config.key IS '主键';

COMMENT ON COLUMN public.config.title IS '标题';

COMMENT ON COLUMN public.config.sort IS '排序 降序排序，大的值在前面';

COMMENT ON COLUMN public.config.system IS '是否为系统配置，系统配置不可删除';

COMMENT ON COLUMN public.config.status IS '状态: -1 删除  0 禁用  1 启用';

COMMENT ON COLUMN public.config.created_at IS '创建记录Unix时间戳毫秒单位';

COMMENT ON COLUMN public.config.updated_at IS '更新记录Unix时间戳毫秒单位';

-- index



-- data


INSERT INTO public.config (tab, key, value, title, sort, remark, system, status)
VALUES
    ('sys', 'login_pwd_rsa_encrypt', 'aes_cbc_i2jC226tIyltl1R6FXFF2A==', '登录密码使用RSA算法加密', 1, '系统登录开启RSA加密', 1, 1),
    ('sys', 'login_rsa_priv_key', 'aes_cbc_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', '登录RSA算法加密私钥', 3, '', 1, 1),
    ('sys', 'site_name', 'aes_cbc_5xkwbfQ60Rx3XjJO6zmM3g==', '前端站点名称', 20, 'ddd3', 1, 1),
    ('sys', 'login_rsa_pub_key', 'aes_cbc_RpwIhFje4KyGf1JKyeKZJg0Ua/ZCSpvSz+fscrIQPk+zmuko9GX6/pAlDKQMAoRS7R1CZGAHb8LS
Gqz2H1KoVEn8jrq7qdd06CrUQYHMFWUJGvOJ/5yOmIS8wiiob8TuzQrll898PN2xPWm7BeNLyldb
XZK5io7GOrPeECDjhHSs4LXtCBYI0fXXfaMbWr60upAHOlfRGUd1IOgj/eSi16PSIx2zExX66Kpl
7HQ7I/lPEnYSd01c2DE6inV8vUqofy3hZJXTNTViB3/FGZUYCpmxaqUat4nQD0jfMLzd+NurU0kf
8mRZvom/BLnGqlJ47DyTGtFbfOMOqYCrh746nM0LbGen1v/vCoEoJrhGISkCsgra0SOHXDZ4GTdq
QybT5Dj7BcWt8CaSYI2geZnMvTeOadgZtmINLd5mZC5cxhEOJM+fpeVEccx2bYOju8e0RKP6/6LH
hLjTpm5alO/LLthyQggoa8fhJ8Oi3Q595vTLIuu2O0TLw/x3sQuZQfAxDIdwTw7nvyvY2C2/rUgK
hAP/xFrVD8YMFOs4R7/VChbXtv+nROKnbAx6i009odojZ4/8a60tIHnixz/VmYW2ylEB4qNl8SuB
i3a6v4/TOeUKLJwa3YU2Hg4GhbN1DRKwmhSEnFrLMwPFNIuHHL7g8AT9npq3ascFf3CQ1BndWzEw
X1V/WnLv8BhzlwA5RrZ/9DGn3rauA0hl1ueo4dVoM7tzuD8t9QhSXZ/0RerVEH5gaZGWvlWpdDVh
HFsHDUoVbT9MT55IFcGSNpOg0w1cst0y59ixiV5JX5AwJHm2Wag=', '登录RSA算法加密公钥', 2, '系统登录RSA加密公钥', 1, 1);
