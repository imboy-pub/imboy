-- P0-C: SSO 身份映射表 —— OIDC provider+subject 到 imboy uid 的绑定
-- SSO identity mapping: bind OIDC (provider, subject) to an imboy uid.
-- 首次 SSO 登录时按 email 关联现有账号或自动建号后写入本表；
-- 后续登录按 UNIQUE(provider, subject) 直接命中。

CREATE TABLE IF NOT EXISTS public.sso_identity (
    id         bigint       NOT NULL,                          -- TSID
    provider   varchar(16)  NOT NULL,                          -- oauth2（预留 ldap/saml）
    subject    varchar(255) NOT NULL,                          -- IdP 侧稳定用户标识（sub claim）
    uid        bigint       NOT NULL,                          -- imboy user.id
    email      varchar(255) NOT NULL DEFAULT '',               -- IdP 返回的 email（可为空）
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    PRIMARY KEY (id)
);
CREATE UNIQUE INDEX IF NOT EXISTS uk_sso_identity_provider_subject
    ON public.sso_identity(provider, subject);
CREATE INDEX IF NOT EXISTS idx_sso_identity_uid ON public.sso_identity(uid);
COMMENT ON TABLE public.sso_identity IS 'SSO 身份映射（provider+subject -> uid，OIDC 登录流）';
