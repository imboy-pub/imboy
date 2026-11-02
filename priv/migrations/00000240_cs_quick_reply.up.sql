-- CS Quick Reply（客服快捷回复，V3-27 子能力；合同 v3/specs/cs-quick-reply.md §1）
-- 数据模型：workspace 级配置，软删（非 NULL 行对一切读不可见）。
-- 无唯一约束（同 workspace 允许同名重复；防重复由 UI pending 门承担）。
-- 租户列沿用 CS 同族命名 organization_id（语义即 org：admin 面从会话取、
-- seat 面从 :organization_id 路由参数取，铁律6 org 贯穿）。

CREATE TABLE IF NOT EXISTS cs_quick_reply (
    id          bigint PRIMARY KEY,
    organization_id bigint NOT NULL,
    workspace_id   bigint NOT NULL,
    title          varchar(64) NOT NULL,
    content        varchar(2000) NOT NULL,
    sort           int4 NOT NULL DEFAULT 0,
    created_by     bigint NOT NULL,
    created_at     timestamptz NOT NULL DEFAULT now(),
    updated_at     timestamptz NOT NULL DEFAULT now(),
    deleted_at     timestamptz NULL,
    CHECK (length(btrim(title)) > 0),
    CHECK (length(btrim(content)) > 0)
);

CREATE INDEX IF NOT EXISTS idx_cs_quick_reply_scope
    ON cs_quick_reply (organization_id, workspace_id, deleted_at);
