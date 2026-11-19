-- 00000281 转发请求稳定身份（REL-02/MSG-01/SAFE-05；M-02）
-- request_id 幂等键 + 结算事实（settled_at/result_msg_id）：转发请求重试/重放
-- 命中主键冲突（23505）即不产生重复转发；结算事实使「接受提交后崩溃恢复」
-- 只依赖库态。expand-only：纯新表，不改既有列/触发器/数据；业务接线由后继
-- 转发卡消费（本卡为持久基座，同 M-01 action_version 先例）。
CREATE TABLE public.forward_request_identity (
    request_id     varchar(50) PRIMARY KEY,
    requester_uid  bigint NOT NULL,
    source_type    varchar(10) NOT NULL CHECK (source_type IN ('c2c', 'c2g')),
    source_msg_id  varchar(40) NOT NULL,
    target_type    varchar(10) NOT NULL CHECK (target_type IN ('c2c', 'c2g')),
    target_id      bigint NOT NULL,
    content_sha256 char(64),
    requested_at   timestamptz NOT NULL DEFAULT clock_timestamp(),
    settled_at     timestamptz,
    result_msg_id  varchar(40),
    CONSTRAINT ck_forward_request_identity_content_hash CHECK (
        content_sha256 IS NULL OR content_sha256 ~ '^[0-9a-f]{64}$'
    ),
    CONSTRAINT ck_forward_request_identity_settled CHECK (
        (settled_at IS NULL AND result_msg_id IS NULL)
        OR (settled_at IS NOT NULL AND result_msg_id IS NOT NULL)
    )
);
CREATE INDEX i_forward_request_identity_open
    ON public.forward_request_identity (requester_uid, requested_at DESC)
    WHERE settled_at IS NULL;
-- 运行时角色只读：身份行的写入只能由服务端转发入口事务产生（同 00000258 先例）。
GRANT SELECT ON public.forward_request_identity TO v5_app_runtime;
COMMENT ON TABLE public.forward_request_identity IS
    '转发请求稳定身份：request_id 幂等键 + 结算事实；同 request_id 重试即主键冲突（23505），不产生重复转发。';
