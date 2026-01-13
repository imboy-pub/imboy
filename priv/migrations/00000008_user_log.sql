-- Table: public.user_log

-- DROP TABLE IF EXISTS public."user_log";

CREATE TABLE public.user_log (
    ts timestamptz DEFAULT CURRENT_TIMESTAMP(6) NOT NULL,
    "type" int4 NOT NULL, -- 日志类型: 100 用户注销备份  102 用户注销申请记录 110 修改密码
    uid int8 DEFAULT 0 NOT NULL, -- 用户ID
    body text NOT NULL, -- 相关操作类型的json字符串数据
    remark varchar(200) DEFAULT ''::character varying NOT NULL, -- 备注
    created_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL -- 创建记录时间 2025-02-21 08:33:16.268288+08:00
);
CREATE INDEX i_user_log_type_uid_createdat ON public.user_log USING btree (type, uid, created_at);
COMMENT ON TABLE public.user_log IS '用户日志表';

-- Column comments

COMMENT ON COLUMN public.user_log."type" IS '日志类型: 100 用户注销备份  102 用户注销申请记录 110 修改密码';
COMMENT ON COLUMN public.user_log.uid IS '用户ID';
COMMENT ON COLUMN public.user_log.body IS '相关操作类型的json字符串数据';
COMMENT ON COLUMN public.user_log.remark IS '备注';
COMMENT ON COLUMN public.user_log.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';
