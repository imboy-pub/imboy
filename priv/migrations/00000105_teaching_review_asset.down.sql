-- 00000105_teaching_review_asset.down.sql
-- 回滚 00000105：删除 review_asset（触发器/索引/表）。
-- fail-closed 预检：存在无法用旧单列 teacher_review.video_attachment_id
-- 表示的数据（任何 feedback_image，或单 review 多于 1 个 feedback_video）
-- 时拒绝回滚——旧列只承载"单视频"语义，多图数据不可逆，不可静默丢弃。
-- 预检通过 = 全部数据可由旧列完整表示（回填的逆操作无损），安全删除。
-- teacher_review.video_attachment_id 列本身不动（00000097 资产，兼容读窗口）。
-- 禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。

DO $$
BEGIN
    IF EXISTS (SELECT 1 FROM review_asset WHERE kind = 'feedback_image') THEN
        RAISE EXCEPTION
            '回滚保护：review_asset 存在 feedback_image 数据，旧单视频列无法表示，拒绝回滚'
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_review_asset_down_guard',
                  HINT = '需先人工清理图片关联或确认丢弃后重试（不可静默删数据）';
    END IF;
    IF EXISTS (
        SELECT 1 FROM review_asset
         WHERE kind = 'feedback_video'
         GROUP BY review_id
        HAVING count(*) > 1
    ) THEN
        RAISE EXCEPTION
            '回滚保护：存在单 review 多于 1 个 feedback_video，旧单视频列无法表示，拒绝回滚'
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_review_asset_down_guard',
                  HINT = '需先人工收敛为单视频后重试';
    END IF;
    -- v3 P1 补第三预检（Round 1 发现）：review_asset 的视频关联与旧列
    -- video_attachment_id 镜像不一致（含旧列为 NULL 而关联存在）——此时
    -- 删表即静默丢失真源视频关联，旧列无法完整表示，fail closed。
    IF EXISTS (
        SELECT 1
          FROM review_asset ra
          JOIN teacher_review tr ON tr.id = ra.review_id
         WHERE ra.kind = 'feedback_video'
           AND tr.video_attachment_id IS DISTINCT FROM ra.attachment_id
    ) THEN
        RAISE EXCEPTION
            '回滚保护：存在 feedback_video 关联与 teacher_review.video_attachment_id 镜像不一致，删表将丢失视频关联，拒绝回滚'
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_review_asset_down_guard',
                  HINT = '需先将旧列与 review_asset 对齐（或人工确认丢弃）后重试';
    END IF;
END;
$$;

DROP TRIGGER IF EXISTS trg_review_asset_image_cap ON review_asset;
DROP FUNCTION IF EXISTS fn_review_asset_image_cap();
DROP TABLE IF EXISTS review_asset;
