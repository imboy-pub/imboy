-- V5.5 attachment lifecycle M1-expand: frozen identity/state columns.
-- Design contract: docs/architecture/attachment-lifecycle-v5/03-migration-contract.md
-- Expand-only: no GC eligibility granted, existing rows stay NULL / live /
-- registered-compatible; reads keep working. Single transaction via runner.
SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '15min';

ALTER TABLE attachment
    ADD COLUMN storage_driver text,
    ADD COLUMN storage_namespace text,
    ADD COLUMN storage_container text,
    ADD COLUMN storage_locator_sha256 text,
    ADD COLUMN lifecycle_state text NOT NULL DEFAULT 'live',
    ADD COLUMN gc_verified boolean NOT NULL DEFAULT false;

ALTER TABLE attach_pending
    ADD COLUMN storage_driver text,
    ADD COLUMN storage_namespace text,
    ADD COLUMN storage_container text,
    ADD COLUMN storage_locator_sha256 text,
    ADD COLUMN client_upload_id uuid,
    ADD COLUMN upload_registration jsonb,
    ADD COLUMN upload_content jsonb,
    ADD COLUMN session_deadline timestamptz,
    ADD COLUMN scope_ref text,
    ADD COLUMN session_state text NOT NULL DEFAULT 'registered',
    ADD COLUMN upload_expires_at timestamptz,
    ADD COLUMN upload_drained_at timestamptz,
    ADD COLUMN confirmed_attachment_id bigint,
    ADD COLUMN gc_verified boolean NOT NULL DEFAULT false;

ALTER TABLE enterprise_asset
    ADD COLUMN client_upload_id uuid,
    ADD COLUMN upload_registration jsonb,
    ADD COLUMN upload_content jsonb,
    ADD COLUMN session_deadline timestamptz,
    ADD COLUMN upload_expires_at timestamptz,
    ADD COLUMN upload_drained_at timestamptz,
    ADD COLUMN storage_driver text,
    ADD COLUMN storage_namespace text,
    ADD COLUMN storage_container text,
    ADD COLUMN storage_locator_sha256 text,
    ADD COLUMN lifecycle_state text NOT NULL DEFAULT 'live',
    ADD COLUMN gc_verified boolean NOT NULL DEFAULT false;

ALTER TABLE attachment
    ADD CONSTRAINT ck_v5_attachment_lifecycle
        CHECK (lifecycle_state IN ('live', 'delete_intent', 'deleted'));
ALTER TABLE enterprise_asset
    ADD CONSTRAINT ck_v5_eb_lifecycle
        CHECK (lifecycle_state IN ('live', 'delete_intent', 'deleted'));
ALTER TABLE attach_pending
    ADD CONSTRAINT ck_v5_pending_state
        CHECK (session_state IN
            ('registered', 'confirmed', 'rejected', 'expired', 'delete_intent', 'deleted'));

ALTER TABLE attach_pending
    ADD CONSTRAINT fk_v5_pending_confirmed
        FOREIGN KEY (confirmed_attachment_id) REFERENCES attachment(id) ON DELETE RESTRICT;
ALTER TABLE attach_pending
    ADD CONSTRAINT ck_v5_session_link
        CHECK ((session_state = 'confirmed') = (confirmed_attachment_id IS NOT NULL));

-- Location completeness: all-NULL or all-present valid triple (three tables).
ALTER TABLE attachment
    ADD CONSTRAINT ck_v5_attachment_location CHECK (
        (storage_driver IS NULL AND storage_namespace IS NULL AND storage_container IS NULL)
        OR (storage_driver IS NOT NULL AND storage_driver IN ('garage', 'disk')
            AND storage_namespace IS NOT NULL AND storage_namespace <> ''
            AND storage_container IS NOT NULL AND storage_container <> ''));
ALTER TABLE attach_pending
    ADD CONSTRAINT ck_v5_pending_location CHECK (
        (storage_driver IS NULL AND storage_namespace IS NULL AND storage_container IS NULL)
        OR (storage_driver IS NOT NULL AND storage_driver IN ('garage', 'disk')
            AND storage_namespace IS NOT NULL AND storage_namespace <> ''
            AND storage_container IS NOT NULL AND storage_container <> ''));
ALTER TABLE enterprise_asset
    ADD CONSTRAINT ck_v5_eb_location CHECK (
        (storage_driver IS NULL AND storage_namespace IS NULL AND storage_container IS NULL)
        OR (storage_driver IS NOT NULL AND storage_driver IN ('garage', 'disk')
            AND storage_namespace IS NOT NULL AND storage_namespace <> ''
            AND storage_container IS NOT NULL AND storage_container <> ''));

ALTER TABLE attachment
    ADD CONSTRAINT ck_v5_attachment_gc CHECK (
        NOT gc_verified OR (storage_driver IS NOT NULL AND storage_namespace IS NOT NULL
            AND storage_container IS NOT NULL));
ALTER TABLE attach_pending
    ADD CONSTRAINT ck_v5_pending_gc CHECK (
        NOT gc_verified OR (storage_driver IS NOT NULL AND storage_namespace IS NOT NULL
            AND storage_container IS NOT NULL));
ALTER TABLE enterprise_asset
    ADD CONSTRAINT ck_v5_eb_gc CHECK (
        NOT gc_verified OR (storage_driver IS NOT NULL AND storage_namespace IS NOT NULL
            AND storage_container IS NOT NULL));

-- Locators, when present, are 64 lowercase hex.
ALTER TABLE attachment
    ADD CONSTRAINT ck_v5_attachment_locator CHECK (
        storage_locator_sha256 IS NULL OR storage_locator_sha256 ~ '^[0-9a-f]{64}$');
ALTER TABLE attach_pending
    ADD CONSTRAINT ck_v5_pending_locator CHECK (
        storage_locator_sha256 IS NULL OR storage_locator_sha256 ~ '^[0-9a-f]{64}$');
ALTER TABLE enterprise_asset
    ADD CONSTRAINT ck_v5_eb_locator CHECK (
        storage_locator_sha256 IS NULL OR storage_locator_sha256 ~ '^[0-9a-f]{64}$');

-- upload_content pin shape: {version:1, sha256:64hex, size_bytes:non-negative decimal
-- string, mime: non-empty} — NULL allowed until first pin, never partial.
ALTER TABLE attach_pending
    ADD CONSTRAINT ck_v5_pending_content CHECK (
        upload_content IS NULL OR (
            jsonb_typeof(upload_content) = 'object'
            AND upload_content ? 'version' AND (upload_content->>'version') = '1'
            AND upload_content ? 'sha256'
            AND (upload_content->>'sha256') ~ '^[0-9a-f]{64}$'
            AND upload_content ? 'size_bytes'
            AND (upload_content->>'size_bytes') ~ '^(0|[1-9][0-9]*)$'
            AND upload_content ? 'mime' AND length(upload_content->>'mime') > 0));
ALTER TABLE enterprise_asset
    ADD CONSTRAINT ck_v5_eb_content CHECK (
        upload_content IS NULL OR (
            jsonb_typeof(upload_content) = 'object'
            AND upload_content ? 'version' AND (upload_content->>'version') = '1'
            AND upload_content ? 'sha256'
            AND (upload_content->>'sha256') ~ '^[0-9a-f]{64}$'
            AND upload_content ? 'size_bytes'
            AND (upload_content->>'size_bytes') ~ '^(0|[1-9][0-9]*)$'
            AND upload_content ? 'mime' AND length(upload_content->>'mime') > 0));

-- upload_registration snapshot: object with fixed subject/declaration blocks;
-- canonical form is enforced by the register functions, shape here.
ALTER TABLE attach_pending
    ADD CONSTRAINT ck_v5_pending_registration CHECK (
        upload_registration IS NULL OR (
            jsonb_typeof(upload_registration) = 'object'
            AND upload_registration ? 'subject' AND upload_registration ? 'declaration'));
ALTER TABLE enterprise_asset
    ADD CONSTRAINT ck_v5_eb_registration CHECK (
        upload_registration IS NULL OR (
            jsonb_typeof(upload_registration) = 'object'
            AND upload_registration ? 'subject' AND upload_registration ? 'declaration'));

-- V5.4: partial unique recovery ids, old rows stay NULL untouched.
CREATE UNIQUE INDEX uq_v5_pending_client_upload
    ON attach_pending(client_upload_id) WHERE client_upload_id IS NOT NULL;
CREATE UNIQUE INDEX uq_v5_eb_client_upload
    ON enterprise_asset(client_upload_id) WHERE client_upload_id IS NOT NULL;

-- Session deadlines: new V5 registrations must carry both timestamps.
ALTER TABLE attach_pending
    ADD CONSTRAINT ck_v5_pending_times CHECK (
        session_deadline IS NULL OR upload_expires_at IS NULL
        OR upload_expires_at <= session_deadline);
ALTER TABLE enterprise_asset
    ADD CONSTRAINT ck_v5_eb_times CHECK (
        session_deadline IS NULL OR upload_expires_at IS NULL
        OR upload_expires_at <= session_deadline);
