
ALTER TABLE public.user_device ADD COLUMN IF NOT EXISTS public_key VARCHAR(1024) DEFAULT '';

COMMENT ON COLUMN public.user_device.public_key IS '用户设备公钥';


COMMENT ON COLUMN public.user.status IS '状态: -1 删除  0 禁用  1 启用  2 申请注销中';

COMMENT ON COLUMN public.user_log.type IS '日志类型: 100 用户注销备份  102 用户注销申请记录 110 修改密码';
