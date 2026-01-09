-- Table: public.app_ddl

-- DROP TABLE IF EXISTS public."app_ddl";

-- 导入数据后，需要更新自增长ID
-- select setval('"app_ddl_id_seq"', (select max(id) from public."app_ddl"));

CREATE TABLE IF NOT EXISTS public."app_ddl"
(
    id BIGSERIAL PRIMARY KEY,
    ddl text not null, -- 因为DDL语句可能有顺序要求，所以用 text类型，而不用 json 数据类型
    down_ddl text not null, -- 因为DDL语句可能有顺序要求，所以用 text类型，而不用 json 数据类型
    admin_user_id bigint NOT NULL DEFAULT 0,
    old_vsn int NOT NULL DEFAULT 0,
    new_vsn int NOT NULL DEFAULT 0,
    status smallint NOT NULL DEFAULT 1,
    updated_at timestamptz DEFAULT CURRENT_TIMESTAMP NULL,
    created_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT uk_OldVsn_NewVsn UNIQUE  (old_vsn, new_vsn)
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.app_ddl OWNER to imboy_user;

CREATE INDEX i_ddl_Status_NewVsn ON public.app_ddl (status, new_vsn);

COMMENT ON TABLE public.app_ddl IS 'APP sqlite3 数据库 DDL 语句版本管理表';
COMMENT ON COLUMN public.app_ddl.id IS '主键 自增长ID 反馈ID';
COMMENT ON COLUMN public.app_ddl.old_vsn IS '版本号： 整形数字';
COMMENT ON COLUMN public.app_ddl.new_vsn IS '版本号： 整形数字';
COMMENT ON COLUMN public.app_ddl.ddl IS '需要更新的DDL语句文本，每个SQL半角逗号 ; 分割，因为DDL语句可能有顺序要求，所以用 text类型，而不用 json 数据类型';
COMMENT ON COLUMN public.app_ddl.status IS '状态: -1 删除  0 禁用  1 启用';
COMMENT ON COLUMN public.app_ddl.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';
COMMENT ON COLUMN public.app_ddl.updated_at IS '最后更新记录时间 2025-02-21 08:33:16.268288+08:00';
