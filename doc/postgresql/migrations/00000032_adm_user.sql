-- Table: public.adm_user

-- DROP TABLE IF EXISTS public."adm_user";

-- 导入数据后，需要更新自增长ID
-- select setval('"adm_user_id_seq"', (select max(id) from public."adm_user"));


CREATE TABLE IF NOT EXISTS public."adm_user"
(
    id BIGSERIAL PRIMARY KEY,
    account varchar(80) COLLATE pg_catalog."default" NOT NULL,
    mobile varchar(40) COLLATE pg_catalog."default",
    email varchar(80) COLLATE pg_catalog."default",
    nickname varchar(80) COLLATE pg_catalog."default" NOT NULL DEFAULT '',
    password varchar(800) COLLATE pg_catalog."default" NOT NULL,
    avatar varchar(320) COLLATE pg_catalog."default" NOT NULL DEFAULT '',
    role_id integer[],
    login_count bigint NOT NULL DEFAULT 0,
    last_login_ip varchar(40) COLLATE pg_catalog."default" NOT NULL DEFAULT '',
    last_login_at bigint NOT NULL DEFAULT 0,
    status integer NOT NULL DEFAULT 1,
    created_at bigint NOT NULL
)

-- imboy_db:assemble_value(#{mobile => "13692177080", password => "password", account => "13692177080", "status" => 1, "role_id" => [1,3], "nickname" => "大大大"}).

-- insert public."adm_user" (mobile, password, account, status, role_id,nickname) value ('13692177080', 'password', '13692177080', 1, '[1,3]', '大大大').
TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.adm_user OWNER to imboy_user;

CREATE UNIQUE INDEX uk_Adm_Account ON public.adm_user (account);
CREATE UNIQUE INDEX uk_Adm_Mobile ON public.adm_user (mobile);
CREATE UNIQUE INDEX uk_Adm_Email ON public.adm_user (email);

COMMENT ON TABLE public.adm_user IS '运营用户表';
COMMENT ON COLUMN public.adm_user.id IS '主键 自增长ID';

COMMENT ON COLUMN public.adm_user.account IS '会员账号';
COMMENT ON COLUMN public.adm_user.mobile IS '手机号码';
COMMENT ON COLUMN public.adm_user.email IS '会员注册Email';

COMMENT ON COLUMN public.adm_user.nickname IS '用户昵称';

COMMENT ON COLUMN public.adm_user.password IS '经过加盐的密码 imboy_password:generate(imboy_hasher:md5("admin888")).';
COMMENT ON COLUMN public.adm_user.avatar IS '头像';
COMMENT ON COLUMN public.adm_user.role_id IS '角色ID';
COMMENT ON COLUMN public.adm_user.login_count IS '登陆次数';
COMMENT ON COLUMN public.adm_user.last_login_ip IS '最后登陆IP';
COMMENT ON COLUMN public.adm_user.last_login_at IS '最后登录时间';

COMMENT ON COLUMN public.adm_user.status
    IS '状态: -1 删除  0 禁用  1 启用';

COMMENT ON COLUMN public.adm_user.created_at
    IS '创建记录Unix时间戳毫秒单位';
