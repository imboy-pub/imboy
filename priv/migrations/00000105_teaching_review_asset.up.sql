-- 00000105_teaching_review_asset.up.sql
-- 墨芽回评媒体 P0-4（MN-MEDIA-01）：teacher_review 多媒体关联表 review_asset
-- 计划契约：docs/plans/2026-09-10-moya-post-security-product-plan.md §P0-4
-- 迁移契约：up=可重复执行，down=安全回滚（fail-closed 预检）。禁止
-- BEGIN/COMMIT——erlang_migrate 外层单事务包裹。
--
-- 设计决策：
--   * 单视频列 → 多媒体集合：既有 teacher_review.video_attachment_id（00000097）
--     保留为兼容读窗口（旧客户端读路径），但不再是新写入真源——新写入真源
--     是本表 review_asset（0-1 feedback_video + 0-3 feedback_image）。
--     写路径由 teaching_review_logic 在 DTO 层从 assets 派生冗余写旧列，
--     保持两读路径一致；down 以旧列可完整表示为回滚前提。
--   * attachment_id 全表 UNIQUE：一个附件至多绑一个 review（一对一，防止
--     同一附件被绑到多个 review 形成跨 review 的读授权歧义）。
--   * 单 review 至多 1 个视频：部分唯一索引（uk_review_asset_one_video_per_review，
--     WHERE kind='feedback_video'）——与 00000097 uk_tr_published_per_submission 同款手法。
--   * 单 review 至多 3 张图片：CONSTRAINT TRIGGER 语句末新鲜快照计数（>3 RAISE），
--     风格参照 00000098 fn_homework_submission_withdraw_guard——多行 INSERT
--     在语句结束时逐行复查，第 4 行即拦截，无需逐语句串行化。
--   * 回填幂等：既有 teacher_review.video_attachment_id 非空且对应 attachment
--     存在（status>=0）的行复制为 feedback_video 关联（sort_order=0，
--     created_by=COALESCE(reviewer_uid,0)，0=sentinel 与 00000100 语义一致）。
--     NOT EXISTS + ON CONFLICT (attachment_id) DO NOTHING 双保险，重复执行零新增。
--     回填行 id 用「当前毫秒(自定义纪元)<<21 | row_number」合成 TSID 同分布值
--     （node 位全 0：应用侧 node 恒非 0，永不与 generate() 撞号）。
--   * created_by 裸列 + CHECK(>=0)：沿用 00000100 sentinel 决策——账号物理删除
--     不阻塞、不抹审计（无 FK SET NULL 抹除风险）。
--   * FK：review_id → teacher_review ON DELETE CASCADE（review 行删除连带解除
--     关联，与 submission_asset→homework_submission 同款）；attachment_id →
--     attachment ON DELETE RESTRICT（附件不可被静默连带删除，fail-closed）。

CREATE TABLE IF NOT EXISTS review_asset (
    id            bigint                       NOT NULL,  -- TSID
    review_id     bigint                       NOT NULL,
    attachment_id bigint                       NOT NULL,
    kind          text                         NOT NULL,
    sort_order    integer                      DEFAULT 0 NOT NULL,
    created_by    bigint                       NOT NULL,
    created_at    timestamp with time zone     DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT pk_review_asset PRIMARY KEY (id),
    CONSTRAINT uk_review_asset_attachment UNIQUE (attachment_id),
    CONSTRAINT ck_review_asset_kind CHECK (kind = ANY (ARRAY['feedback_image'::text, 'feedback_video'::text])),
    CONSTRAINT ck_review_asset_created_by_sentinel CHECK (created_by >= 0)
);

COMMENT ON TABLE  review_asset              IS '老师回评多媒体关联（0-1 feedback_video + 0-3 feedback_image；video_attachment_id 旧列为兼容读窗口，非写入真源）';
COMMENT ON COLUMN review_asset.review_id    IS '回评ID（FK→teacher_review，CASCADE：review 删除连带解除关联）';
COMMENT ON COLUMN review_asset.attachment_id IS '附件ID（全表 UNIQUE：一附件至多绑一 review；FK RESTRICT 保护）';
COMMENT ON COLUMN review_asset.kind         IS '类型: feedback_video 反馈视频（单 review 至多 1）| feedback_image 反馈图片（单 review 至多 3，触发器约束）';
COMMENT ON COLUMN review_asset.created_by   IS '创建人用户ID（审计；0=sentinel 注销语义，禁置NULL——00000100 同款）';

ALTER TABLE review_asset DROP CONSTRAINT IF EXISTS fk_ra_review;
ALTER TABLE review_asset ADD CONSTRAINT fk_ra_review
    FOREIGN KEY (review_id) REFERENCES teacher_review(id) ON DELETE CASCADE;

ALTER TABLE review_asset DROP CONSTRAINT IF EXISTS fk_ra_attachment;
ALTER TABLE review_asset ADD CONSTRAINT fk_ra_attachment
    FOREIGN KEY (attachment_id) REFERENCES attachment(id) ON DELETE RESTRICT;

-- 老师工作台/家长回评视图热路径
CREATE INDEX IF NOT EXISTS i_review_asset_review
    ON review_asset USING btree (review_id, kind, sort_order);

-- 单 review 至多 1 个反馈视频（部分唯一索引兜底；应用层校验先行）
CREATE UNIQUE INDEX IF NOT EXISTS uk_review_asset_one_video_per_review
    ON review_asset USING btree (review_id)
    WHERE kind = 'feedback_video';

-- ============================================================
-- 单 review 至多 3 张反馈图片（CONSTRAINT TRIGGER 语句末新鲜快照计数，
-- 多行 INSERT 第 4 行即拦截；风格参照 00000098 withdraw_guard）
-- ============================================================
CREATE OR REPLACE FUNCTION fn_review_asset_image_cap() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
    v_count integer;
BEGIN
    SELECT count(*) INTO v_count
      FROM review_asset
     WHERE review_id = NEW.review_id AND kind = 'feedback_image';
    IF v_count > 3 THEN
        RAISE EXCEPTION
            '回评图片上限：review % 已关联 % 张反馈图片（上限 3）',
            NEW.review_id, v_count
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_review_asset_image_cap',
                  HINT = '单个回评最多 3 张 feedback_image（P0-4 媒体约束）';
    END IF;
    RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS trg_review_asset_image_cap ON review_asset;
CREATE CONSTRAINT TRIGGER trg_review_asset_image_cap
    AFTER INSERT OR UPDATE OF review_id, kind ON review_asset
    DEFERRABLE INITIALLY IMMEDIATE
    FOR EACH ROW EXECUTE FUNCTION fn_review_asset_image_cap();

COMMENT ON FUNCTION fn_review_asset_image_cap IS
    '回评图片上限兜底：单 review 的 feedback_image 计数 >3 拒绝（语句结束时新鲜快照复查，多行插入同步拦截）';

-- ============================================================
-- 回填：既有 video_attachment_id → feedback_video 关联（幂等）
-- ============================================================
INSERT INTO review_asset (id, review_id, attachment_id, kind, sort_order, created_by)
SELECT
    (((extract(epoch FROM clock_timestamp()) * 1000 - 1735689600000)::bigint) << 21)
        | (row_number() OVER (ORDER BY tr.id)),
    tr.id,
    tr.video_attachment_id,
    'feedback_video',
    0,
    COALESCE(tr.reviewer_uid, 0)
FROM teacher_review tr
JOIN attachment att ON att.id = tr.video_attachment_id AND att.status >= 0
WHERE tr.video_attachment_id IS NOT NULL
  AND NOT EXISTS (
      SELECT 1 FROM review_asset ra WHERE ra.attachment_id = tr.video_attachment_id)
ON CONFLICT (attachment_id) DO NOTHING;
