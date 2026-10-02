-module(enterprise_human_file_binding_pg_checks).
-include_lib("eunit/include/eunit.hrl").
-define(H, intbe02_http_support).

human_file_binding_test_() -> {timeout, 180, fun run/0}.

run() ->
    S = ?H:setup_all(),
    try
        ok = msg_store_repo:ensure_table_exists(),
        check(S)
    after
        ?H:teardown_all(S),
        inttest_marker_db:release(S)
    end.

check(#{conn := C}) ->
    Msg = <<"synthetic-human-file-binding">>,
    Id = elib_tsid:generate(attachment),
    ok = ?H:sql_exec(
        C,
        <<"INSERT INTO attachment(id,file_hash256,mime_type,name,path,url,size,creator_user_id,scope,scope_ref,anchor_msg_id,status) VALUES($1,'synthetic','text/plain','synthetic','synthetic-human-file-binding','synthetic-human-file-binding',3,995011,'group','995301',$2,1)">>,
        [Id, Msg]
    ),
    Before = snapshot(C),
    ok = ?H:sql_exec(
        C,
        <<"ALTER TABLE enterprise_audit_event ADD CONSTRAINT synthetic_reject_binding CHECK(action<>'file.message_bound') NOT VALID">>
    ),
    ?assertMatch({error, {audit_failed, _}}, stage(Msg)),
    ?assertEqual(Before, snapshot(C)),
    ?assertEqual(
        null,
        maps:get(
            <<"anchor_conv_seq">>,
            ?H:one(C, <<"SELECT anchor_conv_seq FROM attachment WHERE id=$1">>, [Id])
        )
    ),
    ok = ?H:sql_exec(
        C, <<"ALTER TABLE enterprise_audit_event DROP CONSTRAINT synthetic_reject_binding">>
    ),
    ?assertMatch({ok, _, _, _}, stage(Msg)),
    #{<<"anchor_conv_seq">> := Seq} = ?H:one(
        C, <<"SELECT anchor_conv_seq FROM attachment WHERE id=$1">>, [Id]
    ),
    ?assert(is_integer(Seq)),
    #{<<"n">> := 1} = ?H:one(
        C,
        <<"SELECT count(*) AS n FROM enterprise_audit_event WHERE organization_id=995101 AND resource_type='attachment' AND resource_id=$1 AND action='file.message_bound' AND actor_user_id=995011 AND actor_role='human' AND detail->>'msg_id'=$2 AND (detail->>'conv_seq')::bigint=$3">>,
        [Id, Msg, Seq]
    ),
    After = snapshot(C),
    _ = stage(Msg),
    ?assertEqual(After, snapshot(C)).

stage(Msg) ->
    Now = elib_dt:now(),
    msg_store_repo:stage(
        <<"c2g">>,
        Msg,
        <<"file">>,
        <<>>,
        null,
        <<"{\"text\":\"synthetic\"}">>,
        995011,
        995301,
        Now,
        Now
    ).

snapshot(C) ->
    [
        ?H:one(
            C,
            <<"SELECT coalesce(jsonb_agg(to_jsonb(t) ORDER BY to_jsonb(t)::text),'[]'::jsonb)::text AS rows FROM ",
                Table/binary, " t">>
        )
     || Table <-
            [
                <<"attachment">>,
                <<"enterprise_audit_event">>,
                <<"msg_store_staging">>,
                <<"msg_store_seq">>,
                <<"msg_c2g_request_ledger">>,
                <<"msg_c2g_recipient_snapshot">>
            ]
    ].
