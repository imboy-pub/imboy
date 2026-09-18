-- 迁移 00000131 down: 恢复 organization_invitation.invited_by NOT NULL（00000128 原状）。
-- 仅用于结构回滚演练；若库中存在 invited_by IS NULL 的平台创建行，SET NOT NULL
-- 将显式失败——刻意的 fail-loud：不静默丢弃平台邀请数据，回滚前须先业务清理。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

ALTER TABLE organization_invitation
    ALTER COLUMN invited_by SET NOT NULL;
