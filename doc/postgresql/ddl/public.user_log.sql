-- Table: public.user_log

-- DROP TABLE IF EXISTS public."user_log";


-- 导入数据后，需要更新自增长ID
-- select setval('"user_log_id_seq"', (select max(id) from public."user_log"));

CREATE TABLE IF NOT EXISTS public."user_log"
(
    ts timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    type int NOT NULL,
    uid bigint NOT NULL DEFAULT 0,
    body text NOT NULL,
    remark varchar(200) NOT NULL DEFAULT '',
    created_at bigint NOT NULL
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.user_log OWNER to imboy_user;

COMMENT ON TABLE public.user_log IS '用户日志表';


COMMENT ON COLUMN public.user_log.type IS '日志类型: 100 用户注销备份';
COMMENT ON COLUMN public.user_log.uid IS '用户ID';

COMMENT ON COLUMN public.user_log.body IS '相关操作类型的json字符串数据';
COMMENT ON COLUMN public.user_log.remark IS '备注';

COMMENT ON COLUMN public.user_log.created_at IS '创建记录Unix时间戳毫秒单位';

-- index
CREATE INDEX i_user_log_Type_Uid_CreatedAt ON public.user_log(type, uid, created_at asc);

SELECT create_hypertable('user_log', 'ts', chunk_time_interval => INTERVAL '30 day');
