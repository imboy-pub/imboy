-- Table: public.adm_role

-- DROP TABLE IF EXISTS public."adm_role";

-- 导入数据后，需要更新自增长ID
-- select setval('"adm_role_id_seq"', (select max(id) from public."adm_role"));

CREATE TABLE IF NOT EXISTS public."adm_role"
(
    id BIGSERIAL PRIMARY KEY,
    parent_id bigint NOT NULL DEFAULT 0,
    sort bigint NOT NULL DEFAULT 100,
    role_name varchar(80) COLLATE pg_catalog."default" NOT NULL,
    status integer NOT NULL DEFAULT 1,
    updated_at bigint NOT NULL,
    created_at bigint NOT NULL
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.adm_role OWNER to imboy_user;

COMMENT ON TABLE public.adm_role IS '运营用户角色表';
COMMENT ON COLUMN public.adm_role.id IS '主键 自增长ID 角色ID';

COMMENT ON COLUMN public.adm_role.parent_id IS '父级角色ID 0 未顶级角色';
COMMENT ON COLUMN public.adm_role.role_name IS '角色名称';
COMMENT ON COLUMN public.adm_role.sort IS '排序(数字越小越靠前)';


COMMENT ON COLUMN public.adm_role.status IS '状态: -1 删除  0 禁用  1 启用';
COMMENT ON COLUMN public.adm_role.updated_at IS '修改记录Unix时间戳毫秒单位';
COMMENT ON COLUMN public.adm_role.created_at IS '创建记录Unix时间戳毫秒单位';
