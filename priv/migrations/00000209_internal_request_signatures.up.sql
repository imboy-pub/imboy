-- HMAC verification needs an encrypted shared key, never a digest used as a key.
-- Existing digest-only credentials keep legacy Bearer behavior until rotated.
ALTER TABLE public.enterprise_application_credential
    ADD COLUMN signing_secret_enc text;

CREATE TABLE public.enterprise_internal_request_nonce (
    credential_id bigint NOT NULL REFERENCES public.enterprise_application_credential(id),
    nonce varchar(128) NOT NULL,
    expires_at timestamptz NOT NULL,
    PRIMARY KEY (credential_id, nonce)
);
CREATE INDEX idx_internal_request_nonce_expiry
    ON public.enterprise_internal_request_nonce (expires_at);

-- Ticket exchange must remember how its issuer was authenticated; a Human
-- request cannot choose or promote this mode.
ALTER TABLE public.enterprise_oa_login_ticket
    ADD COLUMN issuer_authentication text NOT NULL DEFAULT 'bearer'
    CHECK (issuer_authentication IN ('bearer','hmac'));
