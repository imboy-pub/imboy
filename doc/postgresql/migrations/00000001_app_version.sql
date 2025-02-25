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

    force_update int NOT NULL DEFAULT 0,
    sort int NOT NULL DEFAULT 0,
    status smallint NOT NULL DEFAULT 1,
    updated_at timestamptz DEFAULT CURRENT_TIMESTAMP NULL,
    created_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT uk_Vsn_PkgName_Type UNIQUE  (vsn, package_name, type)
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.app_version OWNER to imboy_user;

COMMENT ON TABLE public.app_version IS 'APP版本管理表';
COMMENT ON COLUMN public.app_version.id IS '主键 自增长ID 反馈ID';
COMMENT ON COLUMN public.app_version.region_code IS 'The two-letter country code cn en  参考 https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2';
COMMENT ON COLUMN public.app_version.type IS 'APP类型 web ios android macos windows';
COMMENT ON COLUMN public.app_version.package_name IS '包名 | Bundle ID';
COMMENT ON COLUMN public.app_version.app_name IS 'APP名称';
COMMENT ON COLUMN public.app_version.vsn IS '版本号： x.y.z格式';

COMMENT ON COLUMN public.app_version.sign_key IS 'APP签名密码';
COMMENT ON COLUMN public.app_version.download_url IS '下载地址';
COMMENT ON COLUMN public.app_version.description IS '描述';
COMMENT ON COLUMN public.app_version.force_update IS '是否强制升级 1 是  2 否';
COMMENT ON COLUMN public.app_version.sort IS '排序，值越大越靠前： major * 1_000_000 + minor * 1_000 + patch';
COMMENT ON COLUMN public.app_version.status IS '状态: -1 删除  0 禁用  1 启用';
COMMENT ON COLUMN public.app_version.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';
COMMENT ON COLUMN public.app_version.updated_at IS '最后更新记录时间 2025-02-21 08:33:16.268288+08:00';

-- index
CREATE INDEX i_vsn_Status_Type_RegionCode ON public.app_version (status, type, region_code);
CREATE INDEX i_vsn_Sort_UpdatedAt ON public.app_version (sort, updated_at);
