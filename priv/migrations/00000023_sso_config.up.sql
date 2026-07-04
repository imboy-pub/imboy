-- SSO 外部认证配置：每个 provider (ldap|saml|oauth2) 一行，config 存该 provider 全字段
-- SSO external authentication config: one row per provider, full field set stored in jsonb config

CREATE TABLE IF NOT EXISTS sso_config (
    id         bigint       NOT NULL,                          -- TSID
    provider   varchar(16)  NOT NULL,                          -- ldap | saml | oauth2
    enabled    boolean      NOT NULL DEFAULT false,
    config     jsonb        NOT NULL DEFAULT '{}'::jsonb,       -- 该 provider 全字段（含敏感项，MVP 明文存储）
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    PRIMARY KEY (id)
);
CREATE UNIQUE INDEX IF NOT EXISTS uk_sso_config_provider ON sso_config(provider);
COMMENT ON TABLE sso_config IS 'SSO 外部认证配置（每 provider 一行，按 provider upsert）';
