-- Run only in a newly created run-owned database with M3 and migration 191.
\set ON_ERROR_STOP on
BEGIN;
DO $$
DECLARE
    cid uuid := 'a1910000-0000-4000-8000-000000000001';
    registration jsonb := '{"subject":{"kind":"user","principal":"99191"},"declaration":{"filename":"a.png","mime":"image/png","scope":"group","scope_ref":"A"}}';
    original attach_pending%ROWTYPE;
    replay record;
BEGIN
    PERFORM * FROM fn_v5_register_pending('v5-replay-test/original', 'fixture', 'group',
        99191, cid, registration, now()+interval '1 day', now()+interval '5 minutes',
        NULL,NULL,NULL,NULL);
    SELECT * INTO STRICT original FROM attach_pending WHERE client_upload_id=cid;
    SELECT * INTO STRICT replay FROM fn_v5_register_pending('v5-replay-test/new-random',
        'fixture','group',99191,cid,registration,now()+interval '2 days',
        now()+interval '10 minutes',NULL,NULL,NULL,NULL);
    ASSERT replay.out_object_key = original.object_key, 'retry changed key';
    ASSERT (SELECT count(*) FROM attach_pending WHERE client_upload_id=cid)=1,
        'retry created another row';
    ASSERT NOT EXISTS (SELECT 1 FROM attach_pending WHERE object_key='v5-replay-test/new-random'),
        'retry reserved proposed key';
    ASSERT (SELECT session_deadline=original.session_deadline AND
        upload_expires_at=original.upload_expires_at FROM attach_pending WHERE client_upload_id=cid),
        'retry extended capability window';
    BEGIN
        PERFORM * FROM fn_v5_register_pending('v5-replay-test/changed-scope','fixture','group',
            99191,cid,jsonb_set(registration,'{declaration,scope_ref}','"B"'),
            now()+interval '1 day',now()+interval '5 minutes',NULL,NULL,NULL,NULL);
        RAISE EXCEPTION 'changed scope_ref accepted';
    EXCEPTION WHEN SQLSTATE '23510' THEN NULL;
    END;
    BEGIN
        PERFORM * FROM fn_v5_register_pending('v5-replay-test/changed-subject','fixture','group',
            99192,cid,registration,now()+interval '1 day',now()+interval '5 minutes',
            NULL,NULL,NULL,NULL);
        RAISE EXCEPTION 'changed principal accepted';
    EXCEPTION WHEN SQLSTATE '23510' THEN NULL;
    END;
    BEGIN
        PERFORM * FROM fn_v5_register_pending(original.object_key,'fixture','group',99191,
            'a1910000-0000-4000-8000-000000000002',registration,now()+interval '1 day',
            now()+interval '5 minutes',NULL,NULL,NULL,NULL);
        RAISE EXCEPTION 'key reassignment accepted';
    EXCEPTION WHEN SQLSTATE '23510' THEN NULL;
    END;
    UPDATE attach_pending SET session_state='rejected' WHERE client_upload_id=cid;
    SELECT * INTO STRICT replay FROM fn_v5_register_pending('v5-replay-test/terminal',
        'fixture','group',99191,cid,registration,now()+interval '1 day',
        now()+interval '5 minutes',NULL,NULL,NULL,NULL);
    ASSERT replay.out_session_state='rejected', 'terminal session was reopened';
    RAISE NOTICE 'UUID_REPLAY_ASSERTIONS_PASS';
END;
$$;
ROLLBACK;
