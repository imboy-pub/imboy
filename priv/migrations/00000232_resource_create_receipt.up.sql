-- 迁移 232: 资源创建受理回执表（channel-group-ux-v2.2 W04.b，plan §6.4 C1）
--
-- 身份唯一键 (uid, operation, request_id)：认证用户 + 创建操作 + 客户端意图键。
-- operation 仅允许本计划两类创建（create_group / create_channel）。
--
-- 与 enterprise_internal_idempotency（org/application 域、TTL 过期重置）不同：
-- 本表是面向用户的创建受理事实，**不设 expires_at、永不因 TTL 重置**
-- （§6.4：7d 只约束服务端完整结果的保留义务；最小 receipt 作为去重事实
-- 长期保留，账户删除按既有删除合同清理）。
--
-- 生命周期（同一 PostgreSQL 事务，缺一不可）：
--   1. placeholder：INSERT 占位（uid/operation/request_id/request_digest）；
--   2. 资源创建 + 初始名 + 成员/世代 + 持久通知入队；
--   3. accept：UPDATE 回填 resource_id + server_accepted_at。
-- 占位与回填同事务原子提交——已提交行恒有 resource_id/server_accepted_at，
-- 任何一步失败全部回滚（J23.a：同键可再次首次受理）。
--
-- resource_deleted：群解散/频道删除时置 true 的删除标识（§6.4 最小 receipt
-- 字段）。置位后同键重放不泄露内容、不复活对象，但保留已受理事实（J24.b）。

CREATE TABLE public.resource_create_receipt (
    uid bigint NOT NULL,
    operation character varying(32) NOT NULL,
    request_id character varying(128) NOT NULL,
    request_digest character varying(64) NOT NULL,
    resource_id bigint,
    server_accepted_at timestamp with time zone,
    resource_deleted boolean DEFAULT false NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT pk_resource_create_receipt PRIMARY KEY (uid, operation, request_id),
    CONSTRAINT ck_rcr_operation CHECK (operation IN ('create_group', 'create_channel')),
    CONSTRAINT ck_rcr_request_id CHECK (length(btrim(request_id)) > 0),
    CONSTRAINT ck_rcr_request_digest CHECK (request_digest ~ '^[0-9a-f]{64}$')
);

CREATE INDEX i_rcr_resource ON public.resource_create_receipt (resource_id)
    WHERE resource_id IS NOT NULL;

COMMENT ON TABLE public.resource_create_receipt IS
    '资源创建受理回执：群/频道创建幂等与崩溃恢复（plan v2.2 §6.4 C1；不设 TTL，永不重置）';
COMMENT ON COLUMN public.resource_create_receipt.uid IS '创建发起者用户 ID（认证 uid）';
COMMENT ON COLUMN public.resource_create_receipt.operation IS '创建操作：create_group | create_channel';
COMMENT ON COLUMN public.resource_create_receipt.request_id IS '客户端创建意图键（同键自动/人工重试不变）';
COMMENT ON COLUMN public.resource_create_receipt.request_digest IS '规范载荷 SHA256（排除 request_id/recover_only/两端时间）';
COMMENT ON COLUMN public.resource_create_receipt.resource_id IS '受理资源 ID（群 gid / 频道 cid；占位阶段为 NULL，与回填同事务提交）';
COMMENT ON COLUMN public.resource_create_receipt.server_accepted_at IS '首次成功受理的数据库事务时间（7d 完整结果保留义务的起点）';
COMMENT ON COLUMN public.resource_create_receipt.resource_deleted IS '资源删除标识：置位后重放不泄露内容、不复活对象（J24.b）';
