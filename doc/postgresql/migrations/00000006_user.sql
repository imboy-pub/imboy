-- Table: public.user

-- DROP TABLE IF EXISTS public."user";

-- 导入数据后，需要更新自增长ID
-- select setval('"user_id_seq"', (select max(id) from public."user"));


CREATE TABLE IF NOT EXISTS public."user"
(
    id BIGSERIAL PRIMARY KEY,
    level_id bigint NOT NULL DEFAULT 1,
    nickname varchar(80) COLLATE pg_catalog."default" NOT NULL DEFAULT '',
    password varchar(800) COLLATE pg_catalog."default" NOT NULL,
    account varchar(80) COLLATE pg_catalog."default" NOT NULL,
    mobile varchar(40) COLLATE pg_catalog."default",
    email varchar(80) COLLATE pg_catalog."default",
    region varchar(80) COLLATE pg_catalog."default" NOT NULL DEFAULT '',
    gender int4 NOT NULL DEFAULT 0,
    experience bigint NOT NULL DEFAULT 0,
    avatar varchar(320) COLLATE pg_catalog."default" NOT NULL DEFAULT '',
    sign varchar(320) COLLATE pg_catalog."default" NOT NULL DEFAULT '',
    ref_user_id bigint NOT NULL DEFAULT 0,
    status smallint NOT NULL DEFAULT 1,
    created_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL,
    reg_ip varchar(40) COLLATE pg_catalog."default" NOT NULL,
    reg_cosv varchar(320) COLLATE pg_catalog."default" NOT NULL
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.user OWNER to imboy_user;

CREATE UNIQUE INDEX uk_Account ON public.user (account);
CREATE UNIQUE INDEX uk_Mobile ON public.user (mobile);
CREATE UNIQUE INDEX uk_Email ON public.user (email);

COMMENT ON TABLE public.user
    IS '用户表';

COMMENT ON COLUMN public.user.id
    IS '主键 自增长ID';

COMMENT ON COLUMN public.user.level_id
    IS '会员等级ID';

COMMENT ON COLUMN public.user.nickname
    IS '用户昵称';

COMMENT ON COLUMN public.user.password
    IS '经过加盐的密码 imboy_password:generate(imboy_hasher:md5("admin888")).';

COMMENT ON COLUMN public.user.account IS '会员账号';
COMMENT ON COLUMN public.user.mobile IS '手机号码';
COMMENT ON COLUMN public.user.email IS '会员注册Email';
COMMENT ON COLUMN public.user.region IS '地区：广东 深圳';
COMMENT ON COLUMN public.user.gender IS '性别 1 男  2 女  3 保密';
COMMENT ON COLUMN public.user.experience IS '经验值';
COMMENT ON COLUMN public.user.avatar IS '头像';
COMMENT ON COLUMN public.user.sign IS '用户签名';
COMMENT ON COLUMN public.user.ref_user_id IS '推荐人ID，0表示无推荐人';

COMMENT ON COLUMN public.user.status
    IS '状态: -1 删除  0 禁用  1 启用  2 申请注销中';

COMMENT ON COLUMN public.user.created_at
    IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';

COMMENT ON COLUMN public.user.reg_ip IS '注册IP';
COMMENT ON COLUMN public.user.reg_cosv IS '客户端操作系统版本，例如： Linux 5.11.0-1018-gcp #20~20.04.2-Ubuntu SMP Fri Sep 3 01:01:37 UTC 2021 | "Windows 10 Pro" 10.0 (Build 19043)';
