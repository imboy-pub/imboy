-- Table: public.user

-- DROP TABLE IF EXISTS public."user";

-- 导入数据后，需要更新自增长ID
-- select setval('"user_id_seq"', (select max(id) from public."user"));

CREATE TABLE IF NOT EXISTS public."user" (
    id bigserial NOT NULL, -- 主键 自增长ID
    level_id int8 DEFAULT 1 NOT NULL, -- 会员等级ID
    nickname varchar(80) DEFAULT ''::character varying NOT NULL, -- 用户昵称
    "password" varchar(800) NOT NULL, -- 经过加盐的密码，首启向导或业务注册时由 elib_password:generate/1 生成
    account varchar(80) NOT NULL, -- 会员账号
    mobile varchar(40) NULL, -- 手机号码
    email varchar(80) NULL, -- 会员注册Email
    region varchar(80) DEFAULT ''::character varying NOT NULL, -- 地区：广东 深圳
    gender int4 DEFAULT 0 NOT NULL, -- 性别 1 男  2 女  3 保密
    experience int8 DEFAULT 0 NOT NULL, -- 经验值
    avatar varchar(320) DEFAULT ''::character varying NOT NULL, -- 头像
    sign varchar(320) DEFAULT ''::character varying NOT NULL, -- 用户签名
    ref_user_id int8 DEFAULT 0 NOT NULL, -- 推荐人ID，0表示无推荐人
    status int2 DEFAULT 1 NOT NULL, -- 状态: -1 删除  0 禁用  1 启用  2 申请注销中
    created_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL, -- 创建记录时间 2025-02-21 08:33:16.268288+08:00
    reg_ip varchar(40) NOT NULL, -- 注册IP
    reg_cosv varchar(320) NOT NULL, -- 客户端操作系统版本，例如： Linux 5.11.0-1018-gcp #20~20.04.2-Ubuntu SMP Fri Sep 3 01:01:37 UTC 2021 | "Windows 10 Pro" 10.0 (Build 19043)
    ref_parent_user_id int8 DEFAULT 0 NOT NULL, -- 推荐人的推荐人user id
    "source" varchar(80) DEFAULT ''::character varying NOT NULL, -- 注册来源标记
    CONSTRAINT user_pkey PRIMARY KEY (id)
);
CREATE UNIQUE INDEX IF NOT EXISTS uk_account ON public."user" USING btree (account);
-- 去重：保留每组重复 email 中 id 最小的那条，其余置 NULL
DELETE FROM public."user" a USING public."user" b
WHERE a.email = b.email AND a.email IS NOT NULL AND a.email <> '' AND a.id > b.id;
CREATE UNIQUE INDEX IF NOT EXISTS uk_email ON public."user" USING btree (email) WHERE email IS NOT NULL AND email <> '';
-- 去重：保留每组重复 mobile 中 id 最小的那条，其余置 NULL
DELETE FROM public."user" a USING public."user" b
WHERE a.mobile = b.mobile AND a.mobile IS NOT NULL AND a.mobile <> '' AND a.id > b.id;
CREATE UNIQUE INDEX IF NOT EXISTS uk_mobile ON public."user" USING btree (mobile) WHERE mobile IS NOT NULL AND mobile <> '';
COMMENT ON TABLE public."user" IS '用户表';

-- Column comments

COMMENT ON COLUMN public."user".id IS '主键 自增长ID';
COMMENT ON COLUMN public."user".level_id IS '会员等级ID';
COMMENT ON COLUMN public."user".nickname IS '用户昵称';
COMMENT ON COLUMN public."user"."password" IS '经过加盐的密码，由 elib_password:generate/1 生成（HMAC-SHA512）';
COMMENT ON COLUMN public."user".account IS '会员账号';
COMMENT ON COLUMN public."user".mobile IS '手机号码';
COMMENT ON COLUMN public."user".email IS '会员注册Email';
COMMENT ON COLUMN public."user".region IS '地区：广东 深圳';
COMMENT ON COLUMN public."user".gender IS '性别 1 男  2 女  3 保密';
COMMENT ON COLUMN public."user".experience IS '经验值';
COMMENT ON COLUMN public."user".avatar IS '头像';
COMMENT ON COLUMN public."user".sign IS '用户签名';
COMMENT ON COLUMN public."user".ref_user_id IS '推荐人ID，0表示无推荐人';
COMMENT ON COLUMN public."user".status IS '状态: -1 删除  0 禁用  1 启用  2 申请注销中';
COMMENT ON COLUMN public."user".created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';
COMMENT ON COLUMN public."user".reg_ip IS '注册IP';
COMMENT ON COLUMN public."user".reg_cosv IS '客户端操作系统版本，例如： Linux 5.11.0-1018-gcp #20~20.04.2-Ubuntu SMP Fri Sep 3 01:01:37 UTC 2021 | "Windows 10 Pro" 10.0 (Build 19043)';
COMMENT ON COLUMN public."user".ref_parent_user_id IS '推荐人的推荐人user id';
COMMENT ON COLUMN public."user"."source" IS '注册来源标记';
