-- 00000180_customer_service_ai_agent — down（逆序清理）
-- 先子表/依赖对象，后配置表；purge 角色最后（须先 DROP 其被 GRANT 的函数，
-- 否则 DROP ROLE 因对象依赖失败）。
-- 角色是集群级对象：多库共享同一实例时，单库 down 删除该角色会影响其他
-- 库的 GRANT —— 共享实例场景须先确认无其他库引用（本仓部署形态为单应用库）。
-- No BEGIN/COMMIT: migration runner owns the transaction.

-- 11. purge 权限模型（先函数后角色，消除 GRANT 依赖）
DROP FUNCTION IF EXISTS cs_ai_purge_trace(bigint, timestamptz, text, text, bigint);
DROP TABLE IF EXISTS cs_ai_purge_principal;
DROP ROLE IF EXISTS cs_ai_purge;

-- 9b. 预算调用级账本
DROP TABLE IF EXISTS customer_service_ai_budget_attempt;

-- 9. 成本预算累计视图
DROP TABLE IF EXISTS customer_service_ai_budget;

-- 8b. 消费幂等表
DROP TABLE IF EXISTS customer_service_ai_consume_seen;

-- 8. 作业派生游标
DROP TABLE IF EXISTS customer_service_ai_consume_cursor;

-- 7. AI 作业
DROP TABLE IF EXISTS customer_service_ai_job;

-- 6. AI 投递事实
DROP TABLE IF EXISTS customer_service_ai_delivery;

-- 5. AI 逐轮决策审计（触发器随表 DROP，函数需显式清理）
DROP TRIGGER IF EXISTS trg_csait_append_only ON customer_service_ai_turn;
DROP TABLE IF EXISTS customer_service_ai_turn;
DROP FUNCTION IF EXISTS fn_customer_service_ai_turn_append_only();

-- 4. 会话 AI 状态
DROP TABLE IF EXISTS customer_service_ai_session;

-- 3. FAQ（PG 侧分词触发器）
DROP TRIGGER IF EXISTS trg_csfaq_build_tsv ON customer_service_faq;
DROP TABLE IF EXISTS customer_service_faq;
DROP FUNCTION IF EXISTS fn_csfaq_build_tsv();

-- 2. canary 白名单
DROP TABLE IF EXISTS customer_service_ai_canary;

-- 1. AI 坐席配置
DROP TABLE IF EXISTS customer_service_ai_agent;
