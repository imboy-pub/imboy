-- A run-owned scratch DB only; all synthetic fixture rows roll back.
\set ON_ERROR_STOP on
BEGIN;
DO $$
DECLARE
    cid uuid;
    k text;
    snap jsonb;
    item integer;
    aid bigint;
    answer bigint;
    before_row jsonb;
    after_row jsonb;
    attack integer;
BEGIN
    FOR item IN 1..4 LOOP
        cid := gen_random_uuid(); k := 'confirm-declaration/' || cid::text;
        aid := 1920000000000 + item;
        IF item <= 2 THEN
            snap := '{"subject":{"kind":"user","principal":"99191"},"declaration":{"scope":"group","scope_ref":"99191"}}';
        ELSE
            snap := '{"subject":{"kind":"application","principal":"1","org_id":"2"},"declaration":{"scope":"enterprise","application_id":1,"org_id":2}}';
        END IF;
        PERFORM * FROM fn_v5_register_pending(k,'fixture',
            CASE WHEN item<=2 THEN 'group' ELSE 'enterprise' END,
            CASE WHEN item<=2 THEN 99191 ELSE 0 END,cid,snap,
            now()+interval '1 day',now()+interval '5 minutes',
            'disk','fixture-ns','fixture',repeat('a',64));
        PERFORM fn_v5_pin_pending_content(k,
            jsonb_build_object('version',1,'sha256',repeat('b',64),'size_bytes',3,'mime','text/plain'));
        INSERT INTO attachment(id,path,creator_user_id,scope,scope_ref,
            storage_driver,storage_namespace,storage_container,storage_locator_sha256)
        VALUES(aid,k,CASE WHEN item<=2 THEN 99191 ELSE 0 END,
            CASE WHEN item<=2 THEN 'group' ELSE 'enterprise' END,
            CASE item WHEN 1 THEN '99192' WHEN 2 THEN '99191'
                      WHEN 3 THEN 'org:2/app:99' ELSE 'org:2/app:1' END,
            'disk','fixture-ns','fixture',repeat('a',64));
        IF item IN (1,3) THEN
            BEGIN
                PERFORM fn_v5_confirm_pending(k,aid);
                RAISE EXCEPTION 'substituted scope_ref accepted';
            EXCEPTION WHEN SQLSTATE '23510' THEN NULL;
            END;
            ASSERT (SELECT session_state='registered' FROM attach_pending WHERE object_key=k),
                'rejected confirm changed session';
        ELSE
            answer := fn_v5_confirm_pending(k,aid);
            ASSERT answer=aid, 'valid original declaration rejected';
            ASSERT fn_v5_confirm_pending(k,aid)=aid, 'replay changed original identity';
        END IF;
    END LOOP;
    -- Every direct writer reaches the BEFORE trigger; rejection is atomic.
    FOR attack IN 1..10 LOOP
        cid := gen_random_uuid(); k := 'direct-confirm/' || cid::text;
        aid := 1921000000000 + attack;
        snap := '{"subject":{"kind":"user","principal":"99191"},"declaration":{"scope":"group","scope_ref":"99191"}}';
        PERFORM * FROM fn_v5_register_pending(k,'fixture','group',99191,cid,snap,
            now()+interval '1 day',now()+interval '5 minutes',
            'disk','fixture-ns','fixture',repeat('a',64));
        IF attack <> 9 THEN
            PERFORM fn_v5_pin_pending_content(k,
                jsonb_build_object('version',1,'sha256',repeat('b',64),'size_bytes',3,'mime','text/plain'));
        END IF;
        INSERT INTO attachment(id,path,creator_user_id,scope,scope_ref,
            storage_driver,storage_namespace,storage_container,storage_locator_sha256,lifecycle_state)
        VALUES(aid,CASE WHEN attack=1 THEN k||'/wrong' ELSE k END,
            CASE WHEN attack IN (2,10) THEN 99192 ELSE 99191 END,'group',
            CASE WHEN attack=3 THEN '99192' ELSE '99191' END,
            CASE WHEN attack=4 THEN 'garage' ELSE 'disk' END,
            CASE WHEN attack=5 THEN 'wrong-ns' ELSE 'fixture-ns' END,
            CASE WHEN attack=6 THEN 'wrong-container' ELSE 'fixture' END,
            CASE WHEN attack=7 THEN repeat('c',64) ELSE repeat('a',64) END,
            CASE WHEN attack=8 THEN 'delete_intent' ELSE 'live' END);
        SELECT to_jsonb(p) INTO before_row FROM attach_pending p WHERE object_key=k;
        BEGIN
            UPDATE attach_pending SET session_state='confirmed',confirmed_attachment_id=aid,
                creator_user_id=CASE WHEN attack=10 THEN 99192 ELSE creator_user_id END
                WHERE object_key=k;
            RAISE EXCEPTION 'direct DML attack % accepted',attack USING ERRCODE='P0001';
        EXCEPTION WHEN SQLSTATE '23510' THEN NULL;
        END;
        SELECT to_jsonb(p) INTO after_row FROM attach_pending p WHERE object_key=k;
        ASSERT before_row=after_row, 'rejected direct DML changed original row';
    END LOOP;
    -- Legal direct confirmation and idempotent replay, followed by forbidden rebinding.
    cid := gen_random_uuid(); k := 'direct-legal/' || cid::text;
    aid := 1921000000100;
    PERFORM * FROM fn_v5_register_pending(k,'fixture','group',99191,cid,snap,
        now()+interval '1 day',now()+interval '5 minutes',
        'disk','fixture-ns','fixture',repeat('a',64));
    PERFORM fn_v5_pin_pending_content(k,
        jsonb_build_object('version',1,'sha256',repeat('b',64),'size_bytes',3,'mime','text/plain'));
    INSERT INTO attachment(id,path,creator_user_id,scope,scope_ref,
        storage_driver,storage_namespace,storage_container,storage_locator_sha256)
    VALUES(aid,k,99191,'group','99191','disk','fixture-ns','fixture',repeat('a',64));
    UPDATE attach_pending SET session_state='confirmed',confirmed_attachment_id=aid WHERE object_key=k;
    ASSERT fn_v5_confirm_pending(k,aid)=aid, 'direct legal confirmation replay rejected';
    SELECT to_jsonb(p) INTO before_row FROM attach_pending p WHERE object_key=k;
    BEGIN
        UPDATE attach_pending SET confirmed_attachment_id=aid+1 WHERE object_key=k;
        RAISE EXCEPTION 'confirmed direct rebinding accepted' USING ERRCODE='P0001';
    EXCEPTION WHEN SQLSTATE '23510' THEN NULL;
    END;
    SELECT to_jsonb(p) INTO after_row FROM attach_pending p WHERE object_key=k;
    ASSERT before_row=after_row, 'rebind changed original row';
    RAISE NOTICE 'FROZEN_CONFIRM_DECLARATION_ASSERTIONS_PASS';
END;
$$;
ROLLBACK;
