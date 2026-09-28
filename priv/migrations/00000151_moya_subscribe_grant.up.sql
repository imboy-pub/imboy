-- 00000152_moya_subscribe_grant.up.sql
-- 订阅消息一次性授权额度表（W3 服务端依赖之一：老师发布回评 → 家长服务通知）。
-- 迁移契约：up=可重复执行，down=安全回滚（fail-closed 预检）。禁止
-- BEGIN/COMMIT——erlang_migrate 外层单事务包裹。
--
-- 设计决策：
--   * 「一行一次额度」而非「uid+模板计数」：一次性订阅模板每授权一次只能
--     下发一条（微信硬约束），行级记录天然对齐该语义；consume 用条件
--     UPDATE ... WHERE status='pending' 防并发双发，无需额外锁。
--   * 不存 openid：send 时经 sso_identity(provider=wechat_mini) 反查，
--     openid 是 PII，本表只落 uid（与 moya_identity_ds 的 openid 纪律一致）。
--   * consumed_by_submission 留审计线索（哪次发布消费了这条额度），
--     不加 FK：teacher_review 行已由业务保证存在，加约束收益低。
--   * 无过期清理：一次性额度微信侧本身无过期概念（授权即有效，直到被
--     下发消费）；行数 = 授权次数，量级=提交次数，无需分区/清理任务。
--   * rejected/errored 的授权不上报入库（客户端只上报 accepted 列表）。

CREATE TABLE IF NOT EXISTS moya_subscribe_grant (
    id                     bigserial                   PRIMARY KEY,
    uid                    bigint                      NOT NULL,
    template_id            text                        NOT NULL,
    status                 text                        DEFAULT 'pending' NOT NULL,
    created_at             timestamp with time zone    DEFAULT CURRENT_TIMESTAMP NOT NULL,
    consumed_at            timestamp with time zone,
    consumed_by_submission bigint,
    CONSTRAINT ck_moya_subscribe_grant_status
        CHECK (status = ANY (ARRAY['pending'::text, 'consumed'::text]))
);

COMMENT ON TABLE  moya_subscribe_grant                 IS
    '订阅消息一次性授权额度（客户端 requestSubscribeMessage accept 一次记一行 pending；下发成功一行变 consumed）';
COMMENT ON COLUMN moya_subscribe_grant.uid             IS '授权用户（guardian_uid；openid 不入库，send 时反查 sso_identity）';
COMMENT ON COLUMN moya_subscribe_grant.template_id     IS '微信一次性订阅模板 ID（配置而非常量）';
COMMENT ON COLUMN moya_subscribe_grant.status          IS 'pending 未消费 | consumed 已消费（条件 UPDATE 防并发双发）';
COMMENT ON COLUMN moya_subscribe_grant.consumed_at     IS '消费时间（=订阅消息下发成功时间）';
COMMENT ON COLUMN moya_subscribe_grant.consumed_by_submission IS '消费该额度的 homework_submission.id（审计线索）';

ALTER TABLE moya_subscribe_grant DROP CONSTRAINT IF EXISTS fk_moya_subscribe_grant_uid;
ALTER TABLE moya_subscribe_grant ADD CONSTRAINT fk_moya_subscribe_grant_uid
    FOREIGN KEY (uid) REFERENCES "user"(id) ON DELETE CASCADE;

-- 消费路径按 (uid, template_id) 取最近一条 pending（ORDER BY id DESC LIMIT 1）
CREATE INDEX IF NOT EXISTS i_moya_subscribe_grant_consume
    ON moya_subscribe_grant USING btree (uid, template_id, status, id DESC);
