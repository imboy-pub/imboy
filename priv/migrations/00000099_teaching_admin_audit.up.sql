-- 00000099_teaching_admin_audit.up.sql
-- 墨芽习字收官：教学管理动作审计表（Step 16 handoff #2，D 交付；表结构建议 + sentinel uid 0 匿名化策略）
-- 审计范围契约：删除/导出/更正/解绑/绑定等教学管理动作必须留审计记录。
-- 迁移契约：up=可重复执行，down=安全回滚。禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。
--
-- 设计决策（C 采纳口径）：
--   * 裸 bigint 列、无外键（D 给出的二选一中选"取消 FK 改裸列"）：
--     1) 审计是不可变合规日志，必须比被引用实体活得更久——任何 FK（CASCADE 删行/SET NULL 抹操作人/
--        RESTRICT 反向阻塞）都与审计意图相悖；
--     2) FK+sentinel uid 0 需在 "user" 表预插 uid=0 占位行：会触发 sync_fts_user() 等账号触发器、
--        污染账号空间与序列语义，跨环境保证脆弱；
--     3) 裸列使本表天然不进 user_deletion_executor 的级联清单——用户删除不触碰审计行。
--   * sentinel uid 0 语义（对应 00000098 KNOWN_RISKS 的"审计用户匿名化"策略）：
--     - operator_uid / target_user_id 用 0 表示"操作人/目标账号已被删除或匿名化"，
--       严禁用 NULL 抹掉操作主体（NULL 仅用于 target_user_id=该动作本无目标用户的正交语义）；
--     - 应用层匿名化路径：账号删除前 UPDATE teaching_admin_audit SET operator_uid=0
--       WHERE operator_uid=$uid（或 target_user_id 同理）——不删行、不置 NULL。
--     - 建议后续把同策略推广到 homework_submission.withdrawn_by / teacher_review.reviewer_uid
--       （当前 00000097/98 为 FK SET NULL，注销会撞 CHECK fail-closed；改 sentinel 需另立迁移，本波不动）。
--   * action 用 CHECK 枚举（DB 层 fail-closed，仓库一贯风格）：首版集合覆盖已知管理动作
--     bind/unbind（Step 16）与 §9.3 治理动作 learner_archive/data_export/data_delete；
--     新动作经新迁移扩枚举（有意的显式变更）。
--   * detail jsonb 存结构化上下文（role/why/request_id 等）；儿童 PII（display_name/出生年/视频 URL）
--     禁入本表（§9.3：日志与审计不落 PII）。
--   * 只建表不接线：bind/unbind 写入点见 STEP-08-DB/fix-00000099.md 接入建议（B 后续实现）。

CREATE TABLE IF NOT EXISTS teaching_admin_audit (
    id             bigint                       NOT NULL,  -- TSID
    action         text                         NOT NULL,
    operator_uid   bigint                       NOT NULL,  -- 裸列；0=sentinel（操作人已注销/匿名化）
    learner_id     bigint                       NOT NULL,  -- 裸列；审计先于实体存活
    target_user_id bigint,                                 -- 可空=动作无目标用户；0=sentinel（目标已注销）
    detail         jsonb                        DEFAULT '{}'::jsonb NOT NULL,
    created_at     timestamp with time zone     DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT pk_teaching_admin_audit PRIMARY KEY (id),
    CONSTRAINT ck_teaching_admin_audit_action CHECK (
        action = ANY (ARRAY['bind_learner'::text, 'unbind_learner'::text,
                            'learner_archive'::text, 'data_export'::text, 'data_delete'::text])),
    CONSTRAINT ck_teaching_admin_audit_uids CHECK (operator_uid >= 0)
);

COMMENT ON TABLE  teaching_admin_audit             IS '教学管理动作审计（不可变合规日志：绑定/解绑/归档/导出/删除；§9.3）';
COMMENT ON COLUMN teaching_admin_audit.action      IS '动作: bind_learner|unbind_learner|learner_archive|data_export|data_delete（新动作经迁移扩枚举）';
COMMENT ON COLUMN teaching_admin_audit.operator_uid IS '操作人用户ID（裸列无FK）；0=sentinel=操作人账号已删除/匿名化（应用层匿名化时改写为0，禁置NULL）';
COMMENT ON COLUMN teaching_admin_audit.learner_id  IS '学员档案ID（裸列无FK；审计行先于实体存活，learner 物理删除被 RESTRICT 链约束，删除请求走匿名化）';
COMMENT ON COLUMN teaching_admin_audit.target_user_id IS '目标用户ID（bind 目标/解绑原账号）；NULL=动作无目标用户；0=sentinel=目标账号已删除/匿名化';
COMMENT ON COLUMN teaching_admin_audit.detail      IS '结构化上下文 jsonb（role/why/request_id 等）；禁止存 display_name/出生年/视频URL 等儿童 PII';

-- 学员维度追溯（监管导出/家长删除请求按 learner 拉全量动作史）
CREATE INDEX IF NOT EXISTS i_teaching_admin_audit_learner
    ON teaching_admin_audit USING btree (learner_id, created_at DESC);
-- 操作人维度追溯（管理端审计查询）
CREATE INDEX IF NOT EXISTS i_teaching_admin_audit_operator
    ON teaching_admin_audit USING btree (operator_uid, created_at DESC);
