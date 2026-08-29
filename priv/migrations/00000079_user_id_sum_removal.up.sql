-- 00000079: group.user_id_sum 全量退役
-- 依据: docs/planning/group-user-id-sum-p0-decision-2026-08-29.md（2026-08-29 终局拍板）
-- 背景: user_id_sum = 成员 TSID 算术和，bigint 约 85 人即溢出；溢出点在
--       update_statistics 绑定回写（{integer_overflow,int8} 崩连接 → 加/退群 500，
--       大群"冻结"）。sum 作集合签名数学上可碰撞，属先天缺陷。
-- 终局决策: 同一成员集允许创建多个群（微信/Telegram 同款），不做创建幂等
--       去重——签名载体（sum/hash）整体废弃，列与索引一并移除（PG 元数据
--       操作，瞬时）。载荷字段与客户端存储同版本清理，无过渡期。
DROP INDEX IF EXISTS i_creatorid_memberidsum;
ALTER TABLE "group" DROP COLUMN IF EXISTS user_id_sum;
