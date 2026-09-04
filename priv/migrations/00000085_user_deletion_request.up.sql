-- 00000085_user_deletion_request.up.sql
-- 账号删除合规链 D-01：注销请求窄记录（Implementation Plan Task D-01）
-- 迁移契约：up=可重复执行，down=安全回滚。禁止 BEGIN/COMMIT——erlang_migrate
--           外层单事务包裹，文件内 COMMIT 会提前提交外层事务（同 00000082 头注）。
--
-- 设计决策：
--   * 窄记录而非在 user 表上加合规字段（D-01 规格明确要求）：请求时间戳/
--     状态/撤销与批准时间都挂本表；user.status(2=申请注销中) 由应用层双写，
--     仅作过渡期兼容（admin list 仍读 user_log）。
--   * user_id UNIQUE：一人一行。重复申请走 ON CONFLICT 原位更新，且已是
--     requested 态时不改 requested_at（幂等：重复请求不重置时间戳，
--     宽限期不被绕过）；撤销/批准后再次申请则原位复活为新请求。
--   * 历史 bug 根治：user 表无 updated_at 列，旧清扫 SQL 引用该列恒错
--     （docs/compliance/IMBoy Account Deletion D-chain Gap Analysis.md 实证），
--     本表 requested_at 是首个可查询的请求时间戳，清扫改 JOIN 本表。
--   * user_id → "user"(id) ON DELETE CASCADE：用户主行删除后请求行随之
--     消失；D-03 job/tombstone 需要的留存另行设计，不在本迁移。
--   * status text：requested（申请中，宽限期内）| cancelled（已撤销）|
--     approved（审批通过注销）；(status, requested_at) 索引服务清扫扫描。

CREATE TABLE IF NOT EXISTS public.user_deletion_request (
    id           bigint                   NOT NULL,  -- TSID
    user_id      bigint                   NOT NULL,
    status       text                     DEFAULT 'requested' NOT NULL,
    requested_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    cancelled_at timestamp with time zone,
    approved_at  timestamp with time zone,
    updated_at   timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT pk_user_deletion_request PRIMARY KEY (id),
    CONSTRAINT uq_user_deletion_request_user UNIQUE (user_id),
    CONSTRAINT fk_user_deletion_request_user FOREIGN KEY (user_id)
        REFERENCES public."user" (id) ON DELETE CASCADE,
    CONSTRAINT ck_user_deletion_request_status
        CHECK (status = ANY (ARRAY['requested', 'cancelled', 'approved']))
);

CREATE INDEX IF NOT EXISTS idx_user_deletion_request_status
    ON public.user_deletion_request (status, requested_at);
--;
