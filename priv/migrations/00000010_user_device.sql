-- Table: public.user_device

-- DROP TABLE IF EXISTS public."user_device";

-- 导入数据后，需要更新自增长ID
-- select setval('"user_device_id_seq"', (select max(id) from public."user_device"));

CREATE TABLE public.user_device (
    id bigserial NOT NULL, -- 主键 自增长ID
    user_id int8 NOT NULL, -- 用户ID
    device_type varchar(40) DEFAULT ''::character varying NULL, -- 设备类型 web ios android macos windows
    device_id varchar(40) NOT NULL, -- 设备ID web设备留空
    device_vsn varchar(680) NULL, -- 设备版本 {"baseOS":"HUAWEI/CLT-AL00/HWINE:8.1.0/HUAWEICLT-AL00/173(C00):user/release-keys","sdkInt":27,"release":"8.1.0","codename":"REL","incremental":"176(C00)","previewSdkInt":0,"securityPatch":"2018-10-01"}
    device_name varchar(80) NULL, -- 设备名称（用户可修改的）
    login_count int4 DEFAULT 0 NOT NULL, -- 登陆次数
    last_login_ip varchar(40) DEFAULT ''::character varying NOT NULL, -- 最后登陆IP
    last_login_at timestamptz DEFAULT CURRENT_TIMESTAMP NULL, -- 最后登录UTC时间
    last_active_at timestamptz DEFAULT CURRENT_TIMESTAMP NULL, -- 最近活跃时间
    status int2 DEFAULT 1 NOT NULL, -- 状态: -1 删除  0 禁用  1 启用
    created_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL, -- 创建记录UTC时间
    public_key varchar(2048) DEFAULT ''::character varying NULL, -- 用户设备公钥
    CONSTRAINT uk_status_userid_deviceid UNIQUE (status, user_id, device_id),
    CONSTRAINT user_device_pkey PRIMARY KEY (id)
);
COMMENT ON TABLE public.user_device IS '用户登录设备表';

-- Column comments

COMMENT ON COLUMN public.user_device.id IS '主键 自增长ID';
COMMENT ON COLUMN public.user_device.user_id IS '用户ID';
COMMENT ON COLUMN public.user_device.device_type IS '设备类型 web ios android macos windows';
COMMENT ON COLUMN public.user_device.device_id IS '设备ID web设备留空';
COMMENT ON COLUMN public.user_device.device_vsn IS '设备版本 {"baseOS":"HUAWEI/CLT-AL00/HWINE:8.1.0/HUAWEICLT-AL00/173(C00):user/release-keys","sdkInt":27,"release":"8.1.0","codename":"REL","incremental":"176(C00)","previewSdkInt":0,"securityPatch":"2018-10-01"}';
COMMENT ON COLUMN public.user_device.device_name IS '设备名称（用户可修改的）';
COMMENT ON COLUMN public.user_device.login_count IS '登陆次数';
COMMENT ON COLUMN public.user_device.last_login_ip IS '最后登陆IP';
COMMENT ON COLUMN public.user_device.last_login_at IS '最后登录UTC时间';
COMMENT ON COLUMN public.user_device.last_active_at IS '最近活跃时间';
COMMENT ON COLUMN public.user_device.status IS '状态: -1 删除  0 禁用  1 启用';
COMMENT ON COLUMN public.user_device.created_at IS '创建记录UTC时间';
COMMENT ON COLUMN public.user_device.public_key IS '用户设备公钥';
