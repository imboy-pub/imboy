-- Table: public.app_version

-- DROP TABLE IF EXISTS public."app_version";

-- 导入数据后，需要更新自增长ID
-- select setval('"app_version_id_seq"', (select max(id) from public."app_version"));

CREATE TABLE IF NOT EXISTS public."app_version"
(
    id BIGSERIAL PRIMARY KEY,
    region_code varchar(40) NOT NULL DEFAULT 'cn', -- https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2
    type varchar(40) NOT NULL, -- android ios web
    package_name varchar(80),
    app_name varchar(80),
    vsn varchar(40),
    sign_key varchar(80),
    download_url varchar(320),
    description text not null,

    app_db_vsn int NOT NULL DEFAULT 0,
    force_update int NOT NULL DEFAULT 0,
    status smallint NOT NULL DEFAULT 1,
    updated_at bigint DEFAULT 0,
    created_at bigint NOT NULL,
    CONSTRAINT uk_Type_Vsn UNIQUE  (type, vsn)
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.app_version OWNER to imboy_user;

CREATE INDEX i_vsn_Status_Type_RegionCode ON public.app_version (status, type, region_code);

COMMENT ON TABLE public.app_version IS 'APP版本管理表';
COMMENT ON COLUMN public.app_version.id IS '主键 自增长ID 反馈ID';
COMMENT ON COLUMN public.app_version.region_code IS 'The two-letter country code cn en  参考 https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2';
COMMENT ON COLUMN public.app_version.type IS 'APP类型 web ios android macos windows';
COMMENT ON COLUMN public.app_version.package_name IS '包名 | Bundle ID';
COMMENT ON COLUMN public.app_version.app_name IS 'APP名称';
COMMENT ON COLUMN public.app_version.vsn IS '版本号： x.y.z格式';
COMMENT ON COLUMN public.app_version.app_db_vsn IS 'APP的Sqlite3数据的版本序号 为正整数';

COMMENT ON COLUMN public.app_version.sign_key IS 'APP签名密码';
COMMENT ON COLUMN public.app_version.download_url IS '下载地址';
COMMENT ON COLUMN public.app_version.description IS '描述';
COMMENT ON COLUMN public.app_version.force_update IS '是否强制升级 1 是  2 否';
COMMENT ON COLUMN public.app_version.status IS '状态: -1 删除  0 禁用  1 启用';
COMMENT ON COLUMN public.app_version.created_at IS '创建记录Unix时间戳毫秒单位';
COMMENT ON COLUMN public.group_member.updated_at IS '更新记录Unix时间戳毫秒单位';
