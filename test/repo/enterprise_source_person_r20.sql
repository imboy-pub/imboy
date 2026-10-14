\set ON_ERROR_STOP on
BEGIN;
CREATE SCHEMA r20_source_person_fixture;
SET LOCAL search_path = r20_source_person_fixture;
CREATE TABLE "user" (id bigint PRIMARY KEY, account_type smallint NOT NULL, status smallint NOT NULL);
CREATE TABLE organization (id bigint PRIMARY KEY, owner_id bigint CONSTRAINT fk_organization_owner REFERENCES "user" ON DELETE RESTRICT);
CREATE TABLE enterprise_application (
    id bigint PRIMARY KEY, organization_id bigint NOT NULL REFERENCES organization ON DELETE RESTRICT,
    UNIQUE(organization_id, id)
);
CREATE TABLE organization_member (
    organization_id bigint NOT NULL REFERENCES organization ON DELETE CASCADE,
    user_id bigint NOT NULL REFERENCES "user" ON DELETE CASCADE,
    status text NOT NULL DEFAULT 'active', PRIMARY KEY(organization_id,user_id)
);
CREATE TABLE enterprise_external_identity (
    id bigint PRIMARY KEY, organization_id bigint NOT NULL DEFAULT 1,
    application_id bigint NOT NULL,
    external_user_id text NOT NULL, user_id bigint NOT NULL,
    status text NOT NULL DEFAULT 'active', created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at timestamptz,
    UNIQUE(organization_id,application_id,external_user_id),
    UNIQUE(organization_id,application_id,user_id),
    CONSTRAINT uq_eei_app_external_global UNIQUE(application_id,external_user_id),
    CONSTRAINT fk_eei_organization FOREIGN KEY(organization_id) REFERENCES organization ON DELETE RESTRICT,
    CONSTRAINT fk_eei_application FOREIGN KEY(organization_id,application_id)
        REFERENCES enterprise_application(organization_id,id) ON DELETE RESTRICT,
    CONSTRAINT fk_eei_member FOREIGN KEY(organization_id,user_id)
        REFERENCES organization_member(organization_id,user_id) ON DELETE RESTRICT
);
INSERT INTO "user" SELECT user_id, 0, 1 FROM generate_series(1,12) AS user_id;
INSERT INTO organization VALUES (1,12),(2,12);
INSERT INTO enterprise_application VALUES (1,1);
INSERT INTO organization_member(organization_id,user_id)
SELECT org_id,user_id FROM generate_series(1,2) AS org_id CROSS JOIN generate_series(1,12) AS user_id;
INSERT INTO enterprise_external_identity(id,application_id,external_user_id,user_id)
VALUES (1,1,'legacy',1);
INSERT INTO enterprise_external_identity(id,application_id,external_user_id,user_id,status)
VALUES (11,1,'legacy-removed',11,'removed');
\ir ../../priv/migrations/00000176_enterprise_source_person.up.sql
DO $$ BEGIN
    ASSERT (SELECT status='active' AND user_id=11 FROM enterprise_source_person
        WHERE external_user_id='legacy-removed');
END $$;

CREATE TEMP TABLE patched_trigger AS
SELECT pg_get_functiondef('fn_enterprise_external_identity_source()'::regprocedure) AS definition;
CREATE OR REPLACE FUNCTION fn_enterprise_external_identity_source() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
    INSERT INTO enterprise_source_person
        (application_id, external_user_id, user_id, status, created_at, updated_at)
    SELECT NEW.application_id, NEW.external_user_id, NEW.user_id, NEW.status,
        NEW.created_at, COALESCE(NEW.updated_at, NEW.created_at)
    WHERE NOT EXISTS (SELECT 1 FROM enterprise_source_person
        WHERE application_id=NEW.application_id AND external_user_id=NEW.external_user_id)
    ON CONFLICT(application_id,external_user_id) DO NOTHING;
    UPDATE enterprise_source_person SET status=NEW.status,
        updated_at=COALESCE(NEW.updated_at,NEW.created_at)
    WHERE application_id=NEW.application_id AND external_user_id=NEW.external_user_id
        AND user_id=NEW.user_id AND source_version=0;
    RETURN NEW;
END $$;
INSERT INTO enterprise_source_person(application_id,external_user_id,display_name,source_version)
VALUES (1,'pending','imported',2);
DO $$
DECLARE constraint_name text;
BEGIN
    BEGIN
        INSERT INTO enterprise_external_identity(id,application_id,external_user_id,user_id)
        VALUES (2,1,'pending',2);
        RAISE EXCEPTION 'old trigger unexpectedly accepted pending bind';
    EXCEPTION WHEN foreign_key_violation THEN
        GET STACKED DIAGNOSTICS constraint_name = CONSTRAINT_NAME;
        ASSERT constraint_name='fk_eei_source_person', 'must reproduce exact three-column FK';
        RAISE NOTICE 'PREPATCH_REPRODUCED SQLSTATE=23503 constraint=%', constraint_name;
    END;
    ASSERT (SELECT user_id IS NULL FROM enterprise_source_person WHERE external_user_id='pending');
    EXECUTE (SELECT definition FROM patched_trigger);
END $$;

INSERT INTO enterprise_external_identity(id,application_id,external_user_id,user_id)
VALUES (2,1,'pending',2)
ON CONFLICT(organization_id,application_id,external_user_id) DO UPDATE SET status='active',updated_at=EXCLUDED.updated_at
WHERE enterprise_external_identity.user_id=EXCLUDED.user_id;
DO $$
DECLARE affected_rows integer; constraint_name text;
BEGIN
    ASSERT (SELECT user_id=2 AND display_name='imported' AND source_version=2
        FROM enterprise_source_person WHERE external_user_id='pending');
    INSERT INTO enterprise_external_identity(id,application_id,external_user_id,user_id)
    VALUES (3,1,'pending',3)
    ON CONFLICT(organization_id,application_id,external_user_id) DO UPDATE SET status='active'
    WHERE enterprise_external_identity.user_id=EXCLUDED.user_id;
    GET DIAGNOSTICS affected_rows = ROW_COUNT;
    ASSERT affected_rows=0, 'legacy cross-UID bind must return zero rows';
    BEGIN
        INSERT INTO enterprise_external_identity(id,organization_id,application_id,external_user_id,user_id)
        VALUES (3,2,1,'pending',3);
        RAISE EXCEPTION 'cross-organization rebind unexpectedly succeeded';
    EXCEPTION WHEN check_violation THEN
        GET STACKED DIAGNOSTICS constraint_name = CONSTRAINT_NAME;
        ASSERT constraint_name='ck_eei_source_identity_conflict';
    END;
    ASSERT (SELECT user_id=2 FROM enterprise_source_person WHERE external_user_id='pending');
END $$;

UPDATE enterprise_external_identity SET updated_at=CURRENT_TIMESTAMP WHERE external_user_id='pending';
UPDATE enterprise_source_person SET display_name='next',source_version=3 WHERE external_user_id='pending';
DO $$ BEGIN
    BEGIN
        UPDATE enterprise_source_person SET source_version=2 WHERE external_user_id='pending';
        RAISE EXCEPTION 'version regression unexpectedly succeeded';
    EXCEPTION WHEN check_violation THEN NULL; END;
    BEGIN
        UPDATE enterprise_source_person SET display_name='conflict' WHERE external_user_id='pending';
        RAISE EXCEPTION 'same-version payload replacement unexpectedly succeeded';
    EXCEPTION WHEN check_violation THEN NULL; END;
END $$;
INSERT INTO enterprise_external_identity(id,organization_id,application_id,external_user_id,user_id)
VALUES (7,2,1,'pending',2);
CREATE TEMP TABLE source_before_unbind AS SELECT * FROM enterprise_source_person WHERE external_user_id='pending';
UPDATE enterprise_external_identity SET status='removed' WHERE organization_id=1 AND external_user_id='pending';
DO $$ BEGIN
    ASSERT NOT EXISTS (
        (SELECT * FROM source_before_unbind EXCEPT SELECT * FROM enterprise_source_person WHERE external_user_id='pending')
        UNION ALL
        (SELECT * FROM enterprise_source_person WHERE external_user_id='pending' EXCEPT SELECT * FROM source_before_unbind)
    );
    ASSERT (SELECT status='active' FROM enterprise_external_identity
        WHERE organization_id=2 AND external_user_id='pending');
END $$;
UPDATE enterprise_external_identity SET status='active' WHERE organization_id=1 AND external_user_id='pending';
UPDATE enterprise_source_person SET status='removed',source_version=4 WHERE external_user_id='pending';
DO $$ BEGIN
    BEGIN
        UPDATE enterprise_external_identity SET status='active' WHERE external_user_id='pending';
        RAISE EXCEPTION 'legacy restore unexpectedly succeeded';
    EXCEPTION WHEN check_violation THEN NULL; END;
    BEGIN
        UPDATE enterprise_source_person SET status='active' WHERE external_user_id='pending';
        RAISE EXCEPTION 'same-version status replacement unexpectedly succeeded';
    EXCEPTION WHEN check_violation THEN NULL; END;
END $$;
UPDATE enterprise_source_person SET status='active',source_version=5 WHERE external_user_id='pending';
DO $$ BEGIN
    ASSERT (SELECT user_id=2 AND status='active' AND source_version=5
        FROM enterprise_source_person WHERE external_user_id='pending');
END $$;

INSERT INTO enterprise_source_person(application_id,external_user_id,status,source_version)
VALUES (1,'removed','removed',1),(1,'invalid','active',1),(1,'duplicate','active',1);
UPDATE "user" SET status=0 WHERE id=4;
DO $$ BEGIN
    BEGIN
        INSERT INTO enterprise_external_identity(id,application_id,external_user_id,user_id)
        VALUES (4,1,'removed',3);
        RAISE EXCEPTION 'removed claim unexpectedly succeeded';
    EXCEPTION WHEN check_violation THEN NULL; END;
    BEGIN
        INSERT INTO enterprise_external_identity(id,application_id,external_user_id,user_id)
        VALUES (5,1,'invalid',4);
        RAISE EXCEPTION 'disabled Human claim unexpectedly succeeded';
    EXCEPTION WHEN check_violation THEN NULL; END;
    BEGIN
        INSERT INTO enterprise_external_identity(id,application_id,external_user_id,user_id)
        VALUES (6,1,'duplicate',2);
        RAISE EXCEPTION 'duplicate user claim unexpectedly succeeded';
    EXCEPTION WHEN unique_violation THEN NULL; END;
    ASSERT (SELECT user_id IS NULL FROM enterprise_source_person WHERE external_user_id='duplicate');
END $$;

INSERT INTO enterprise_source_person(application_id,external_user_id,user_id) VALUES (1,'retired',5);
INSERT INTO enterprise_external_identity(id,application_id,external_user_id,user_id)
VALUES (8,1,'retired',5);
INSERT INTO enterprise_external_identity(id,organization_id,application_id,external_user_id,user_id)
VALUES (9,2,1,'retired',5);
DELETE FROM "user" WHERE id=5;
DO $$ BEGIN
    ASSERT NOT EXISTS (SELECT 1 FROM enterprise_external_identity WHERE user_id=5);
    ASSERT (SELECT binding_retired AND user_id IS NULL AND status='removed'
        FROM enterprise_source_person WHERE external_user_id='retired');
    BEGIN
        UPDATE enterprise_source_person SET user_id=6 WHERE external_user_id='retired';
        RAISE EXCEPTION 'retired claim unexpectedly succeeded';
    EXCEPTION WHEN check_violation THEN NULL; END;
    BEGIN
        UPDATE enterprise_source_person SET status='active',source_version=10 WHERE external_user_id='retired';
        RAISE EXCEPTION 'retired restore unexpectedly succeeded';
    EXCEPTION WHEN check_violation THEN NULL; END;
    ASSERT (SELECT count(*)=3 AND bool_and(confdeltype='r') FROM pg_constraint
        WHERE conrelid='enterprise_external_identity'::regclass
            AND conname IN ('fk_eei_organization','fk_eei_application','fk_eei_member'));
END $$;

INSERT INTO enterprise_external_identity(id,application_id,external_user_id,user_id)
VALUES (10,1,'blocked-delete',12);
DO $$ DECLARE constraint_name text; BEGIN
    BEGIN
        DELETE FROM "user" WHERE id=12;
        RAISE EXCEPTION 'owner deletion unexpectedly succeeded';
    EXCEPTION WHEN restrict_violation THEN
        GET STACKED DIAGNOSTICS constraint_name = CONSTRAINT_NAME;
        ASSERT constraint_name = 'fk_organization_owner';
    END;
    ASSERT EXISTS (SELECT 1 FROM enterprise_external_identity WHERE external_user_id='blocked-delete');
    ASSERT (SELECT user_id=12 AND NOT binding_retired
        FROM enterprise_source_person WHERE external_user_id='blocked-delete');
END $$;

-- Load the actual down migration as text; invoke it inside an exception subtransaction.
\set down_sql `cat priv/migrations/00000176_enterprise_source_person.down.sql`
CREATE TEMP TABLE down_migration (sql text NOT NULL);
INSERT INTO down_migration VALUES (:'down_sql');
DO $$ BEGIN
    BEGIN
        EXECUTE (SELECT sql FROM down_migration);
        RAISE EXCEPTION 'lossy down unexpectedly succeeded';
    EXCEPTION WHEN check_violation THEN
        ASSERT SQLERRM='cannot roll back 176 without losing source personnel data';
    END;
    ASSERT to_regclass('enterprise_source_person') IS NOT NULL;
END $$;
DELETE FROM enterprise_external_identity WHERE external_user_id<>'legacy';
DELETE FROM enterprise_source_person WHERE external_user_id<>'legacy';
INSERT INTO enterprise_external_identity(id,organization_id,application_id,external_user_id,user_id)
VALUES (12,2,1,'legacy',1);
DO $$ BEGIN
    BEGIN
        EXECUTE (SELECT sql FROM down_migration);
        RAISE EXCEPTION 'duplicate legacy down unexpectedly succeeded';
    EXCEPTION WHEN check_violation THEN
        ASSERT SQLERRM='cannot roll back 176 without losing source personnel data';
    END;
END $$;
DELETE FROM enterprise_external_identity WHERE organization_id=1;
DO $$ BEGIN
    BEGIN
        EXECUTE (SELECT sql FROM down_migration);
        RAISE EXCEPTION 'cross-organization attribution down unexpectedly succeeded';
    EXCEPTION WHEN check_violation THEN
        ASSERT SQLERRM='cannot roll back 176 without losing source personnel data';
    END;
END $$;
UPDATE enterprise_external_identity SET organization_id=1 WHERE external_user_id='legacy';
\ir ../../priv/migrations/00000176_enterprise_source_person.down.sql
DO $$ BEGIN
    ASSERT to_regclass('enterprise_source_person') IS NULL;
    ASSERT NOT EXISTS (SELECT 1 FROM pg_trigger WHERE tgrelid='"user"'::regclass
        AND tgname='trg_enterprise_source_person_user_delete');
    ASSERT (SELECT user_id=1 FROM enterprise_external_identity WHERE external_user_id='legacy');
    RAISE NOTICE 'R20_SYNTHETIC_SQL_PASS (not full application PG chain)';
END $$;
ROLLBACK;
