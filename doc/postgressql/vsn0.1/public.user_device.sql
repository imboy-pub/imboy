-- Table: public.user_device

-- DROP TABLE IF EXISTS public."user_device";

CREATE TABLE IF NOT EXISTS public."user_device"
(
    id BIGSERIAL PRIMARY KEY,
    user_id bigint NOT NULL,
    device_type varchar(40) DEFAULT '',
    device_id varchar(40) NOT NULL,
    device_vsn varchar(680),
    device_name varchar(80),
    login_count bigint NOT NULL DEFAULT 0,
    last_login_ip varchar(40) NOT NULL DEFAULT '',
    last_login_at bigint DEFAULT 0,
    last_active_at bigint DEFAULT 0,
    status smallint NOT NULL DEFAULT 1,
    created_at bigint DEFAULT 0,
    CONSTRAINT uk_Status_UserID_DeviceID UNIQUE  (status,user_id,device_id)
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.user_device OWNER to imboy_user;

COMMENT ON TABLE public.user_device IS '用户登录设备表';

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
