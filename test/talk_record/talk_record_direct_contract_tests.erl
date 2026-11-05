-module(talk_record_direct_contract_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

direct_contract_test_() ->
    {timeout, 120,
        {setup, fun setup/0, fun talk_record_t19_support:teardown_all/1, fun(S) ->
            ?_test(run(S))
        end}}.

setup() ->
    S = talk_record_t19_support:setup_all(),
    talk_record_t19_support:seed(S),
    Registry = attachment_storage:registry(),
    Uploads = maps:get(upload_namespaces, Registry),
    Namespace = maps:get(<<"talk_record">>, Uploads),
    application:set_env(
        imboy,
        attachment_storage_registry,
        Registry#{
            upload_namespaces => Uploads#{
                <<"private">> => Namespace, <<"group">> => Namespace, <<"public">> => Namespace
            }
        }
    ),
    S.

run(S) ->
    C = maps:get(conn, S),
    Uid = 700001,
    Sha = binary:copy(<<"a">>, 64),
    Wrong = binary:copy(<<"b">>, 64),
    Cid = <<"8d96abf4-cd50-4d5a-ae0b-b28c71f31f18">>,
    Opts = #{
        <<"cipher">> => <<"AES-256-GCM">>,
        <<"purpose">> => <<"message">>,
        <<"anchor_msg_id">> => <<"group-msg">>
    },
    {ok, #{object_key := Key}} = attach_direct_logic:register(
        Uid, <<"group">>, <<"123">>, Cid, <<"contract">>, Opts
    ),
    {ok, #{object_key := Key}} = attach_direct_logic:register(
        Uid, <<"group">>, <<"123">>, Cid, <<"contract">>, Opts
    ),
    ?assertEqual(
        {error, declaration_conflict},
        attach_direct_logic:register(
            Uid,
            <<"group">>,
            <<"123">>,
            Cid,
            <<"contract">>,
            Opts#{<<"anchor_msg_id">> => <<"other">>}
        )
    ),
    ?assertEqual(
        {error, invalid_attachment_contract},
        attach_direct_logic:register(
            Uid,
            <<"group">>,
            <<"123">>,
            undefined,
            <<"contract">>,
            #{<<"purpose">> => <<"message">>}
        )
    ),
    ?assertEqual(
        {error, unsupported_cipher},
        attach_direct_logic:register(
            Uid,
            <<"private">>,
            undefined,
            undefined,
            <<"contract">>,
            #{<<"cipher">> => <<"unexpected">>}
        )
    ),
    ok = attach_direct_logic:declare(Key, Sha, 10, <<"image/png">>),
    ?assertMatch(
        {error, #error{code = <<"23510">>}},
        elib_pg:query(
            C,
            <<"UPDATE attach_pending SET direct_expected=jsonb_set(direct_expected,'{sha256}',to_jsonb($2::text)) WHERE object_key=$1">>,
            [Key, Wrong]
        )
    ),
    ?assertEqual({error, expected_conflict}, attach_direct_logic:grant(Key, Wrong)),
    {ok, []} = attach_direct_attempt_repo:list_for_session(Key),
    {ok, #{attempt_id := 1, grant := #{required_headers := Headers}}} =
        attach_direct_logic:grant(Key, Sha),
    ?assertEqual(<<"image/png">>, proplists:get_value(<<"Content-Type">>, Headers)),
    ?assertEqual({error, expected_conflict}, attach_direct_logic:renew_grant(Key, Wrong)),
    {ok, Attempts} = attach_direct_attempt_repo:list_for_session(Key),
    ?assertEqual(1, length(Attempts)),
    Part = #{
        <<"part_id">> => 8315001,
        <<"codec">> => <<"aac">>,
        <<"sha256">> => Sha,
        <<"byte_size">> => 10
    },
    {ok, #{object_key := PartKey, attempt_id := 1}} = talk_record_part_upload:prepare(
        Uid, 8315, Part
    ),
    ?assertEqual(
        {error, expected_conflict},
        talk_record_part_upload:prepare(
            Uid, 8315, Part#{<<"sha256">> => Wrong}
        )
    ),
    ?assertEqual(
        {error, expected_conflict},
        talk_record_part_upload:prepare(
            Uid, 8315, Part#{<<"byte_size">> => 11}
        )
    ),
    {ok, PartAttempts} = attach_direct_attempt_repo:list_for_session(PartKey),
    ?assertEqual(1, length(PartAttempts)),
    {ok, #{attempt_id := 2}} = talk_record_part_upload:prepare(Uid, 8315, Part),
    permission_contract(C),
    down_guard(C),
    cid_contract(C, Uid, Key, Cid),
    down_contract(C),
    ok.

permission_contract(C) ->
    Signatures = [
        <<"fn_dv1_register(text,text,text,text,bigint,uuid,text,text,text,text,text,text)">>,
        <<"fn_dv1_register(text,text,text,text,bigint,uuid,text,text,text,text,text,text,text,text,text)">>,
        <<"fn_dv1_register_application(text,text,text,text,text,uuid,text,text,text,text,text,text)">>
    ],
    [
        begin
            {ok, [R]} = elib_pg:query(
                C,
                <<
                    "SELECT has_function_privilege('v5_app_runtime',$1,'EXECUTE') AS app, "
                    "has_function_privilege('v5_worker',$1,'EXECUTE') AS worker, "
                    "has_function_privilege('v5_proof',$1,'EXECUTE') AS proof, "
                    "EXISTS (SELECT 1 FROM pg_proc p, LATERAL aclexplode(coalesce(p.proacl,acldefault('f',p.proowner))) acl "
                    "WHERE p.oid=to_regprocedure($1) AND acl.grantee=0 AND acl.privilege_type='EXECUTE') AS public_exec"
                >>,
                [Sig]
            ),
            ?assertEqual(true, maps:get(<<"app">>, R)),
            ?assertEqual(false, maps:get(<<"worker">>, R)),
            ?assertEqual(false, maps:get(<<"proof">>, R)),
            ?assertEqual(false, maps:get(<<"public_exec">>, R))
        end
     || Sig <- Signatures
    ],
    Denied = [
        {"v5_worker",
            "SELECT fn_dv1_register('x','b','private',NULL,700001,NULL::uuid,'image/png','x','garage','ns','b','d')"},
        {"v5_worker",
            "SELECT fn_dv1_register('x','b','private',NULL,700001,NULL::uuid,'image/png','x','garage','ns','b','d',NULL,'attachment',NULL)"},
        {"v5_proof",
            "SELECT fn_dv1_register_application('x','b','public','1','0',NULL::uuid,'image/png','x','garage','ns','b','d')"}
    ],
    [
        begin
            {ok, [], []} = epgsql:squery(C, "BEGIN"),
            {ok, [], []} = epgsql:squery(C, "SET LOCAL ROLE " ++ Role),
            ?assertMatch({error, #error{code = <<"42501">>}}, epgsql:squery(C, Sql)),
            {ok, [], []} = epgsql:squery(C, "ROLLBACK")
        end
     || {Role, Sql} <- Denied
    ],
    {ok, [], []} = epgsql:squery(C, "BEGIN"),
    {ok, [], []} = epgsql:squery(C, "SET LOCAL ROLE v5_app_runtime"),
    {ok, _, [{Result}]} = epgsql:squery(
        C,
        "SELECT fn_dv1_register('dummy','b','group','123',700001,"
        "'8d96abf4-cd50-4d5a-ae0b-b28c71f31f18'::uuid,'application/octet-stream',"
        "'contract','garage','ns','b','d','AES-256-GCM','message','group-msg')"
    ),
    ?assert(is_binary(Result)),
    {ok, [], []} = epgsql:squery(C, "ROLLBACK"),
    ok.

cid_contract(C, Uid, Key, Cid) ->
    {ok, _} = elib_pg:query(
        C,
        <<"UPDATE attach_pending SET session_state='expired',direct_state='EXPIRED' WHERE object_key=$1">>,
        [Key]
    ),
    ?assertEqual(
        {error, terminal_session_conflict},
        attach_direct_logic:register(
            Uid, <<"group">>, <<"123">>, Cid, <<"contract">>
        )
    ),
    LegacyCid = <<"d38af2e9-b2ad-49cf-8c1a-7207cb740ad7">>,
    {ok, _} = elib_pg:query(
        C,
        <<
            "INSERT INTO attach_pending(object_key,bucket,scope,creator_user_id,client_upload_id,session_state) "
            "VALUES('legacy-contract-key','imboy','private',$1,$2::uuid,'registered')"
        >>,
        [Uid, LegacyCid]
    ),
    ?assertEqual(
        {error, legacy_session_conflict},
        attach_direct_logic:register(
            Uid, <<"private">>, undefined, LegacyCid, <<"legacy.png">>
        )
    ),
    ?assertEqual(
        {error, legacy_session_conflict},
        attach_direct_logic:register_application(
            <<"90123">>, <<"0">>, <<"public">>, LegacyCid, <<"legacy.png">>
        )
    ),
    {ok, _} = elib_pg:query(
        C,
        <<"UPDATE attach_pending SET session_state='expired' WHERE object_key='legacy-contract-key'">>,
        []
    ),
    ?assertEqual(
        {error, legacy_session_conflict},
        attach_direct_logic:register(
            Uid, <<"private">>, undefined, LegacyCid, <<"legacy.png">>
        )
    ),
    AppCid = <<"c6a3cb04-b458-4bc6-a49a-4cb47df76247">>,
    {ok, #{object_key := AppKey}} = attach_direct_logic:register_application(
        <<"90123">>, <<"0">>, <<"public">>, AppCid, <<"avatar.png">>
    ),
    ?assertEqual(
        {error, declaration_conflict},
        attach_direct_logic:register_application(
            <<"90123">>, <<"0">>, <<"private">>, AppCid, <<"avatar.png">>
        )
    ),
    ActiveAppCid = <<"6c207fb3-c823-4197-a5f4-098c980c269f">>,
    {ok, #{object_key := _}} = attach_direct_logic:register_application(
        <<"90124">>, <<"0">>, <<"public">>, ActiveAppCid, <<"avatar.png">>
    ),
    {ok, _} = elib_pg:query(
        C,
        <<"UPDATE attach_pending SET session_state='expired',direct_state='EXPIRED' WHERE object_key=$1">>,
        [AppKey]
    ),
    ?assertEqual(
        {error, terminal_session_conflict},
        attach_direct_logic:register_application(
            <<"90123">>, <<"0">>, <<"public">>, AppCid, <<"avatar.png">>
        )
    ),
    ok.

down_guard(C) ->
    {ok, [], []} = epgsql:squery(C, "BEGIN"),
    {ok, Script} = file:read_file("priv/migrations/00000249_direct_attachment_contract.down.sql"),
    Results = epgsql:squery(C, binary_to_list(Script)),
    ?assertMatch({error, #error{code = <<"P0001">>}}, Results),
    {ok, [], []} = epgsql:squery(C, "ROLLBACK"),
    ok.

down_contract(C) ->
    {ok, _, _} = epgsql:squery(C, "BEGIN"),
    {ok, Script} = file:read_file("priv/migrations/00000249_direct_attachment_contract.down.sql"),
    Results = epgsql:squery(C, binary_to_list(Script)),
    ?assertNot(lists:any(fun(R) -> element(1, R) =:= error end, Results)),
    {ok, [#{<<"app">> := true, <<"worker">> := false, <<"proof">> := false}]} = elib_pg:query(
        C,
        <<
            "SELECT has_function_privilege('v5_app_runtime','fn_dv1_register(text,text,text,text,bigint,uuid,text,text,text,text,text,text)','EXECUTE') AS app, "
            "has_function_privilege('v5_worker','fn_dv1_register(text,text,text,text,bigint,uuid,text,text,text,text,text,text)','EXECUTE') AS worker, "
            "has_function_privilege('v5_proof','fn_dv1_register_application(text,text,text,text,text,uuid,text,text,text,text,text,text)','EXECUTE') AS proof"
        >>,
        []
    ),
    {ok, [], []} = epgsql:squery(C, "SAVEPOINT before_terminal"),
    ?assertMatch(
        {error, #error{code = <<"23510">>}},
        epgsql:squery(
            C,
            "SELECT fn_dv1_register('dummy','b','group','123',700001,"
            "'8d96abf4-cd50-4d5a-ae0b-b28c71f31f18'::uuid,'application/octet-stream',"
            "'contract','garage','ns','b','d')"
        )
    ),
    {ok, [], []} = epgsql:squery(C, "ROLLBACK TO SAVEPOINT before_terminal"),
    {ok, [], []} = epgsql:squery(C, "SAVEPOINT before_scope"),
    ?assertMatch(
        {error, #error{code = <<"23510">>}},
        epgsql:squery(
            C,
            "SELECT fn_dv1_register_application('dummy','imboy','private','90124','0',"
            "'6c207fb3-c823-4197-a5f4-098c980c269f'::uuid,'image/png',"
            "'avatar.png','garage','ns','imboy','digest')"
        )
    ),
    {ok, [], []} = epgsql:squery(C, "ROLLBACK TO SAVEPOINT before_scope"),
    {ok, _, _} = epgsql:squery(C, "ROLLBACK"),
    ok.
