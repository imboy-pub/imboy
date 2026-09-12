%% Migration 108/109, group attachment/action ACL, and room-key member snapshot
%% PostgreSQL integration coverage.
%% The dedicated harness supplies a marker scratch database through
%% IMBOY_GA_TEST_*; normal EUnit runs skip rather than touching a shared DB.

-module(group_attachment_acl_integration_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-define(USER_CURRENT, 91080101).
-define(USER_NEW, 91080102).
-define(GROUP_A, 91080201).
-define(GROUP_B, 91080202).
-define(KEY_CALLER, 91081101).
-define(KEY_OTHER, 91081102).
-define(KEY_GROUP, 91081201).
-define(KEY_MEMBER_LIMIT_GROUP, 91081202).
-define(KEY_DEVICE_LIMIT_GROUP, 91081203).
-define(KEY_PROBE_LIMIT, 4097).

group_attachment_acl_postgres_test_() ->
    {timeout, 240, {setup, fun setup/0, fun cleanup/1, fun run/1}}.

setup() ->
    case os:getenv("IMBOY_GA_TEST_DB") of
        false ->
            skip;
        Db ->
            {ok, _} = application:ensure_all_started(epgsql),
            connect(Db)
    end.

connect(Db) ->
    Host = os:getenv("IMBOY_GA_TEST_HOST", "127.0.0.1"),
    Port = list_to_integer(os:getenv("IMBOY_GA_TEST_PORT", "4323")),
    User = os:getenv("IMBOY_GA_TEST_USER", "imboy_user"),
    Pass = os:getenv("IMBOY_GA_TEST_PASSWORD", "abc54321"),
    case Host of
        "127.0.0.1" -> ok;
        "::1" -> ok;
        _ -> erlang:error({non_loopback_test_database, Host})
    end,
    case lists:prefix("imboy_ga_acl_", Db) of
        true -> ok;
        false -> erlang:error({non_marker_test_database, Db})
    end,
    {ok, Conn} = epgsql:connect(#{
        host => Host,
        port => Port,
        username => User,
        password => Pass,
        database => Db,
        timeout => 5000
    }),
    Conn.

cleanup(skip) ->
    ok;
cleanup(Conn) ->
    epgsql:close(Conn).

run(skip) ->
    [];
run(Conn) ->
    [
        ?_test(begin
            c2g_boundary_migration_matrix(Conn),
            group_session_attestation_migration_matrix(Conn),
            migration_roundtrip_and_backfill(Conn),
            authorization_matrix(Conn),
            action_recipient_matrix(Conn),
            member_public_keys_matrix(Conn)
        end)
    ].

group_session_attestation_migration_matrix(Conn) ->
    Up = read_migration("priv/migrations/00000112_e2ee_group_session_attestation.up.sql"),
    Down = read_migration("priv/migrations/00000112_e2ee_group_session_attestation.down.sql"),
    ok = squery(Conn, Down),
    ?assert(table_exists(Conn, <<"e2ee_group_session_attestation">>)),
    ?assert(table_exists(Conn, <<"e2ee_group_session_member">>)),
    ok = squery(Conn, Up),
    ok = squery(Conn, Up),
    ?assert(table_exists(Conn, <<"e2ee_group_session_attestation">>)),
    ?assert(table_exists(Conn, <<"e2ee_group_session_member">>)).

c2g_boundary_migration_matrix(Conn) ->
    Up = read_migration("priv/migrations/00000111_c2g_request_recipient_boundary.up.sql"),
    Down = read_migration("priv/migrations/00000111_c2g_request_recipient_boundary.down.sql"),

    ok = squery(Conn, Down),
    ?assert(table_column_exists(Conn, <<"msg_c2g_timeline">>, <<"conv_seq">>)),
    ?assert(table_exists(Conn, <<"msg_c2g_request_ledger">>)),
    ?assert(table_exists(Conn, <<"msg_c2g_recipient_snapshot">>)),
    seed_legacy_c2g_staging(Conn),

    ok = squery(Conn, Up),
    ?assert(table_column_exists(Conn, <<"msg_c2g_timeline">>, <<"conv_seq">>)),
    ?assertEqual(
        ?GROUP_A,
        scalar(Conn, <<"SELECT to_id FROM msg_store_staging WHERE msg_id='ga-legacy-c2g'">>)
    ),
    ?assertEqual(
        1,
        scalar(Conn, <<"SELECT conv_seq FROM msg_store_staging WHERE msg_id='ga-legacy-c2g'">>)
    ),
    ?assertEqual(
        [?USER_CURRENT, ?USER_NEW],
        scalar(
            Conn,
            <<"SELECT recipient_uids FROM msg_c2g_recipient_snapshot ",
                "WHERE msg_id='ga-legacy-c2g'">>
        )
    ),
    Hash = ledger_hash(Conn),

    ok = squery(Conn, <<
        "UPDATE msg_store_staging SET "
        "payload=jsonb_set(jsonb_set(payload,'{server_ts}','\"new-top\"'::jsonb),"
        "'{payload,server_ts}','\"new-nested\"'::jsonb), "
        "server_ts='2026-09-11T01:00:00Z' WHERE msg_id='ga-legacy-c2g'"
    >>),
    ok = squery(Conn, Up),
    ?assertEqual(Hash, ledger_hash(Conn)),

    assert_migration_rejects_change(
        Conn,
        Up,
        <<"UPDATE msg_store_staging SET action='message_edit_ack' ",
            "WHERE msg_id='ga-legacy-c2g'">>,
        <<"UPDATE msg_store_staging SET action='' WHERE msg_id='ga-legacy-c2g'">>
    ),
    assert_migration_rejects_change(
        Conn,
        Up,
        <<"UPDATE msg_store_staging SET payload=jsonb_set(payload,'{body}','\"changed\"'::jsonb) ",
            "WHERE msg_id='ga-legacy-c2g'">>,
        <<"UPDATE msg_store_staging SET payload=jsonb_set(payload,'{body}','\"same\"'::jsonb) ",
            "WHERE msg_id='ga-legacy-c2g'">>
    ),
    assert_migration_rejects_change(
        Conn,
        Up,
        <<"UPDATE msg_store_staging SET e2ee='{\"alg\":\"changed\"}'::jsonb ",
            "WHERE msg_id='ga-legacy-c2g'">>,
        <<"UPDATE msg_store_staging SET e2ee=NULL WHERE msg_id='ga-legacy-c2g'">>
    ),
    assert_migration_rejects_change(
        Conn,
        Up,
        <<"UPDATE msg_store_staging SET sender_did='did-changed' ",
            "WHERE msg_id='ga-legacy-c2g'">>,
        <<"UPDATE msg_store_staging SET sender_did='did-legacy' ", "WHERE msg_id='ga-legacy-c2g'">>
    ),
    assert_migration_rejects_change(
        Conn,
        Up,
        <<"UPDATE msg_store_staging SET payload=payload-'to' WHERE msg_id='ga-legacy-c2g'">>,
        <<"UPDATE msg_store_staging SET payload=jsonb_set(payload,'{to}','\"91080201\"'::jsonb) ",
            "WHERE msg_id='ga-legacy-c2g'">>
    ),
    assert_migration_rejects_change(
        Conn,
        Up,
        <<"UPDATE msg_store_staging SET to_id=91080202 WHERE msg_id='ga-legacy-c2g'">>,
        <<"UPDATE msg_store_staging SET to_id=91080201 WHERE msg_id='ga-legacy-c2g'">>
    ),
    assert_formal_staging_conflict_is_rejected(Conn, Up),
    ok = squery(Conn, Up),

    ok = squery(Conn, Down),
    ?assert(table_column_exists(Conn, <<"msg_c2g_timeline">>, <<"conv_seq">>)),
    ?assertEqual(Hash, ledger_hash(Conn)),
    ?assert(table_exists(Conn, <<"msg_c2g_recipient_snapshot">>)),
    ok = squery(Conn, Up),
    ?assertEqual(Hash, ledger_hash(Conn)).

seed_legacy_c2g_staging(Conn) ->
    squery(Conn, <<
        "CREATE TABLE IF NOT EXISTS msg_store_staging ("
        "id bigint PRIMARY KEY,type varchar(10) NOT NULL,msg_id varchar(50) NOT NULL,"
        "msg_type varchar(50),action varchar(50),e2ee jsonb,sender_did varchar(128),"
        "conv_seq bigint,payload jsonb NOT NULL,from_id bigint NOT NULL,to_id bigint,"
        "to_id_list bigint[],created_at timestamptz NOT NULL,server_ts timestamptz NOT NULL,"
        "retry_count integer NOT NULL DEFAULT 0,processed_at timestamptz,"
        "available_at timestamptz NOT NULL DEFAULT now(),error_msg text,"
        "UNIQUE(type,msg_id));"
        "INSERT INTO msg_store_staging "
        "(id,type,msg_id,msg_type,action,e2ee,sender_did,conv_seq,payload,from_id,to_id,"
        "to_id_list,created_at,server_ts) VALUES "
        "(91080601,'c2g','ga-legacy-c2g','text','',NULL,'did-legacy',NULL,"
        "'{\"to\":\"91080201\",\"body\":\"same\",\"server_ts\":\"old-top\","
        "\"payload\":{\"server_ts\":\"old-nested\"}}'::jsonb,"
        "91080101,NULL,ARRAY[91080101,91080102]::bigint[],"
        "'2026-09-11T00:00:00Z','2026-09-11T00:00:00Z')"
    >>).

ledger_hash(Conn) ->
    scalar(
        Conn,
        <<"SELECT encode(request_hash,'hex') FROM msg_c2g_request_ledger ",
            "WHERE msg_id='ga-legacy-c2g'">>
    ).

assert_migration_rejects_change(Conn, Up, ChangeSql, RestoreSql) ->
    ok = squery(Conn, ChangeSql),
    ?assert(migration_fails(Conn, Up)),
    ok = squery(Conn, RestoreSql).

assert_formal_staging_conflict_is_rejected(Conn, Up) ->
    ok = squery(Conn, <<
        "INSERT INTO msg_c2g "
        "(id,topic_id,from_id,to_id,msg_id,msg_type,e2ee,payload,created_at,sender_did) VALUES "
        "(91080701,0,91080101,91080201,'ga-formal-conflict','text',NULL,"
        "'{\"to\":\"91080201\",\"body\":\"same\"}'::jsonb,"
        "'2026-09-11T02:00:00Z','did-formal');"
        "INSERT INTO msg_store_staging "
        "(id,type,msg_id,msg_type,action,e2ee,sender_did,conv_seq,payload,from_id,to_id,"
        "to_id_list,created_at,server_ts) VALUES "
        "(91080702,'c2g','ga-formal-conflict','text','',NULL,'did-formal',2,"
        "'{\"to\":\"91080201\",\"body\":\"same\"}'::jsonb,"
        "91080102,91080201,ARRAY[91080101,91080102]::bigint[],"
        "'2026-09-11T02:00:00Z','2026-09-11T02:00:00Z')"
    >>),
    ?assert(migration_fails(Conn, Up)),
    ok = squery(Conn, <<
        "DELETE FROM msg_store_staging WHERE msg_id='ga-formal-conflict';"
        "DELETE FROM msg_c2g WHERE msg_id='ga-formal-conflict'"
    >>).

migration_fails(Conn, Sql) ->
    case epgsql:squery(Conn, Sql) of
        {error, _} ->
            true;
        Results when is_list(Results) ->
            lists:any(
                fun
                    ({error, _}) -> true;
                    (_) -> false
                end,
                Results
            );
        _ ->
            false
    end.

migration_roundtrip_and_backfill(Conn) ->
    Up = read_migration("priv/migrations/00000108_group_attachment_anchor.up.sql"),
    Down = read_migration("priv/migrations/00000108_group_attachment_anchor.down.sql"),

    ok = squery(Conn, Down),
    ?assertNot(column_exists(Conn, <<"anchor_msg_id">>)),
    ?assertNot(column_exists(Conn, <<"anchor_conv_seq">>)),
    ?assertNot(column_exists(Conn, <<"group_file_id">>)),

    seed_legacy_backfill_rows(Conn),
    ok = squery(Conn, Up),
    assert_migration_objects(Conn),
    ?assertEqual(
        91080401,
        scalar(Conn, <<
            "SELECT group_file_id FROM attachment WHERE id = 91080501"
        >>)
    ),
    ?assertEqual(
        1,
        scalar(Conn, <<
            "SELECT anchor_conv_seq FROM attachment WHERE id = 91080502"
        >>)
    ),

    ok = squery(Conn, Down),
    ?assertNot(column_exists(Conn, <<"anchor_msg_id">>)),
    ok = squery(Conn, Up),
    assert_migration_objects(Conn),
    assert_constraints_reject_invalid_rows(Conn).

assert_constraints_reject_invalid_rows(Conn) ->
    ?assertMatch(
        {error, #error{code = <<"23514">>}},
        epgsql:equery(
            Conn,
            <<"UPDATE attachment SET anchor_conv_seq = 0 WHERE id = 91080502">>
        )
    ),
    ?assertMatch(
        {error, #error{code = <<"23514">>}},
        epgsql:equery(
            Conn,
            <<"UPDATE attachment SET group_file_id = 91080401 WHERE id = 91080502">>
        )
    ).

seed_legacy_backfill_rows(Conn) ->
    ok = squery(Conn, <<
        "INSERT INTO \"user\" (id,password,account,reg_ip,reg_cosv) VALUES "
        "(91080191,'x','ga_migration_owner','127.0.0.1','x') ON CONFLICT DO NOTHING;"
        "INSERT INTO \"group\" (id,owner_uid,creator_uid,title,status) VALUES "
        "(91080291,91080191,91080191,'ga-migration',1) ON CONFLICT DO NOTHING;"
        "INSERT INTO group_file "
        "(id,group_id,file_id,file_name,file_size,file_type,file_category,file_url,uploader_id,status) "
        "VALUES (91080401,91080291,'legacy-prefix','legacy.png',1,'image/png','image',"
        "'legacy-prefix/legacy.png',91080191,0) ON CONFLICT DO NOTHING;"
        "INSERT INTO attachment "
        "(id,file_hash256,path,url,mime_type,creator_user_id,scope,scope_ref,status) VALUES "
        "(91080501,'ga-hash-501','legacy-prefix/legacy.png','legacy-prefix/legacy.png',"
        "'image/png',91080191,'group','91080291',1),"
        "(91080502,'ga-hash-502','legacy-prefix/not-legacy.png','legacy-prefix/not-legacy.png',"
        "'image/png',91080191,'group','91080291',1) ON CONFLICT DO NOTHING;"
    >>).

assert_migration_objects(Conn) ->
    ?assert(column_exists(Conn, <<"anchor_msg_id">>)),
    ?assert(column_exists(Conn, <<"anchor_conv_seq">>)),
    ?assert(column_exists(Conn, <<"group_file_id">>)),
    ?assert(constraint_exists(Conn, <<"ck_attachment_anchor_conv_seq">>)),
    ?assert(constraint_exists(Conn, <<"ck_attachment_group_anchor_kind">>)),
    ?assert(index_exists(Conn, <<"idx_attachment_group_anchor_pending">>)),
    ?assert(index_exists(Conn, <<"idx_gmg_group_text_user_open">>)).

authorization_matrix(Conn) ->
    ok = squery(Conn, <<"BEGIN">>),
    try
        seed_acl_rows(Conn),
        assert_active_generation(Conn),
        assert_leave_and_rejoin(Conn),
        assert_dissolved_group(Conn)
    after
        ok = squery(Conn, <<"ROLLBACK">>)
    end.

seed_acl_rows(Conn) ->
    ok = squery(Conn, <<
        "INSERT INTO \"user\" (id,password,account,reg_ip,reg_cosv) VALUES "
        "(91080101,'x','ga_current','127.0.0.1','x'),"
        "(91080102,'x','ga_new','127.0.0.1','x');"
        "INSERT INTO \"group\" (id,owner_uid,creator_uid,title,status) VALUES "
        "(91080201,91080101,91080101,'ga-a',1),"
        "(91080202,91080101,91080101,'ga-b',1);"
        "INSERT INTO group_member (id,group_id,user_id,role,is_join,status) VALUES "
        "(91080301,91080201,91080101,1,true,1),"
        "(91080302,91080201,91080102,1,true,1);"
        "INSERT INTO group_member_generation "
        "(group_id,user_id,generation_no,start_seq,end_seq) VALUES "
        "(91080201,91080101,1,10,NULL),"
        "(91080201,91080102,1,20,NULL);"
        "INSERT INTO group_file "
        "(id,group_id,file_id,file_name,file_size,file_type,file_category,file_url,uploader_id,status) "
        "VALUES (91080411,91080201,'ga-active','active.pdf',1,'application/pdf','document',"
        "'ga-active/active.pdf',91080101,1),"
        "(91080412,91080201,'ga-deleted','deleted.pdf',1,'application/pdf','document',"
        "'ga-deleted/deleted.pdf',91080101,0),"
        "(91080413,91080202,'ga-cross','cross.pdf',1,'application/pdf','document',"
        "'ga-cross/cross.pdf',91080101,1);"
    >>),
    insert_attachment(Conn, 91080511, <<"ga/chat-before">>, <<"91080201">>, 9, null),
    insert_attachment(Conn, 91080512, <<"ga/chat-at-start">>, <<"91080201">>, 10, null),
    insert_attachment(Conn, 91080513, <<"ga/chat-null">>, <<"91080201">>, null, null),
    insert_attachment(Conn, 91080514, <<"ga/chat-new-generation">>, <<"91080201">>, 30, null),
    insert_attachment(Conn, 91080515, <<"ga/invalid-ref">>, <<"not-a-group">>, 1, null),
    insert_attachment(Conn, 91080516, <<"ga/file-active">>, <<"91080201">>, null, 91080411),
    insert_attachment(Conn, 91080517, <<"ga/file-deleted">>, <<"91080201">>, null, 91080412),
    insert_attachment(Conn, 91080518, <<"ga/file-cross">>, <<"91080201">>, null, 91080413),
    insert_attachment(Conn, 91080519, <<"ga/unbound">>, <<"91080201">>, null, null).

insert_attachment(Conn, Id, Path, ScopeRef, ConvSeq, GroupFileId) ->
    Sql = <<
        "INSERT INTO attachment "
        "(id,file_hash256,path,url,mime_type,creator_user_id,scope,scope_ref,status,"
        "anchor_msg_id,anchor_conv_seq,group_file_id) "
        "VALUES ($1,$2,$3,$3,'application/octet-stream',$4,'group',$5,1,$6,$7,$8)"
    >>,
    AnchorMsgId =
        case GroupFileId of
            null -> <<"msg-", Path/binary>>;
            _ -> null
        end,
    ok = equery(Conn, Sql, [
        Id,
        <<"hash-", Path/binary>>,
        Path,
        ?USER_CURRENT,
        ScopeRef,
        AnchorMsgId,
        ConvSeq,
        GroupFileId
    ]).

assert_active_generation(Conn) ->
    ?assertNot(allowed(Conn, <<"ga/chat-before">>, ?USER_CURRENT)),
    ?assert(allowed(Conn, <<"ga/chat-at-start">>, ?USER_CURRENT)),
    ?assertNot(allowed(Conn, <<"ga/chat-null">>, ?USER_CURRENT)),
    ?assertNot(allowed(Conn, <<"ga/invalid-ref">>, ?USER_CURRENT)),
    ?assert(allowed(Conn, <<"ga/file-active">>, ?USER_CURRENT)),
    ?assertNot(allowed(Conn, <<"ga/file-deleted">>, ?USER_CURRENT)),
    ?assertNot(allowed(Conn, <<"ga/file-cross">>, ?USER_CURRENT)),
    ?assertNot(allowed(Conn, <<"ga/unbound">>, ?USER_CURRENT)),
    ?assertNot(allowed(Conn, <<"ga/chat-at-start">>, ?USER_NEW)),
    ?assertNot(allowed(Conn, <<"ga/file-active">>, 91080999)).

assert_leave_and_rejoin(Conn) ->
    ok = equery(
        Conn,
        <<
            "UPDATE group_member_generation SET end_seq=19 WHERE group_id=$1 AND user_id=$2 "
            "AND generation_no=1"
        >>,
        [?GROUP_A, ?USER_CURRENT]
    ),
    ok = equery(
        Conn,
        <<
            "UPDATE group_member SET status=0 WHERE group_id=$1 AND user_id=$2"
        >>,
        [?GROUP_A, ?USER_CURRENT]
    ),
    ?assertNot(allowed(Conn, <<"ga/chat-at-start">>, ?USER_CURRENT)),
    ?assertNot(allowed(Conn, <<"ga/file-active">>, ?USER_CURRENT)),

    ok = equery(
        Conn,
        <<
            "UPDATE group_member SET status=1 WHERE group_id=$1 AND user_id=$2"
        >>,
        [?GROUP_A, ?USER_CURRENT]
    ),
    ok = equery(
        Conn,
        <<
            "INSERT INTO group_member_generation "
            "(group_id,user_id,generation_no,start_seq,end_seq) VALUES ($1,$2,2,30,NULL)"
        >>,
        [?GROUP_A, ?USER_CURRENT]
    ),
    ?assertNot(allowed(Conn, <<"ga/chat-at-start">>, ?USER_CURRENT)),
    ?assert(allowed(Conn, <<"ga/chat-new-generation">>, ?USER_CURRENT)),
    ?assert(allowed(Conn, <<"ga/file-active">>, ?USER_CURRENT)).

assert_dissolved_group(Conn) ->
    ok = equery(Conn, <<"UPDATE \"group\" SET status=0 WHERE id=$1">>, [?GROUP_A]),
    ?assertNot(allowed(Conn, <<"ga/chat-new-generation">>, ?USER_CURRENT)),
    ?assertNot(allowed(Conn, <<"ga/file-active">>, ?USER_CURRENT)).

action_recipient_matrix(Conn) ->
    ok = squery(Conn, <<"BEGIN">>),
    try
        seed_acl_rows(Conn),
        seed_action_rows(Conn),
        ?assertEqual(
            lists:sort([?USER_CURRENT, ?USER_NEW]),
            lists:sort(action_recipients(Conn, <<"ga-original">>))
        ),
        %% REST offline_ack 删除 timeline 不能改变不可变的原始收件人快照。
        ok = equery(
            Conn,
            <<"DELETE FROM msg_c2g_timeline WHERE msg_id=$1 AND to_uid=$2">>,
            [<<"ga-original">>, ?USER_NEW]
        ),
        ?assertEqual(
            lists:sort([?USER_CURRENT, ?USER_NEW]),
            lists:sort(action_recipients(Conn, <<"ga-original">>))
        ),
        close_current_generation(Conn),
        %% 原发送者离群后不能再编辑/撤回旧消息。
        ?assertEqual([], action_recipients(Conn, <<"ga-original">>)),
        reopen_current_generation(Conn),
        %% 重入开启新世代，也不能恢复旧世代消息的操作权。
        ?assertEqual([], action_recipients(Conn, <<"ga-original">>)),
        seed_second_snapshot_target(Conn),
        ?assertEqual(
            lists:sort([?USER_CURRENT, ?USER_NEW]),
            lists:sort(action_recipients(Conn, <<"ga-second-original">>))
        )
    after
        ok = squery(Conn, <<"ROLLBACK">>)
    end.

seed_action_rows(Conn) ->
    squery(Conn, <<
        "INSERT INTO msg_c2g_recipient_snapshot "
        "(msg_id,from_id,to_gid,conv_seq,recipient_uids,created_at) VALUES "
        "('ga-original',91080101,91080201,20,ARRAY[91080101,91080102]::bigint[],now());"
        "INSERT INTO msg_c2g_timeline "
        "(msg_id,to_uid,to_gid,client_ack,created_at,conv_seq) VALUES "
        "('ga-original',91080101,91080201,false,now(),20),"
        "('ga-original',91080102,91080201,false,now(),20);"
    >>).

close_current_generation(Conn) ->
    ok = equery(
        Conn,
        <<
            "UPDATE group_member_generation SET end_seq=19 "
            "WHERE group_id=$1 AND user_id=$2 AND generation_no=1"
        >>,
        [?GROUP_A, ?USER_CURRENT]
    ),
    equery(
        Conn,
        <<"UPDATE group_member SET status=0 WHERE group_id=$1 AND user_id=$2">>,
        [?GROUP_A, ?USER_CURRENT]
    ).

reopen_current_generation(Conn) ->
    ok = equery(
        Conn,
        <<"UPDATE group_member SET status=1 WHERE group_id=$1 AND user_id=$2">>,
        [?GROUP_A, ?USER_CURRENT]
    ),
    equery(
        Conn,
        <<
            "INSERT INTO group_member_generation "
            "(group_id,user_id,generation_no,start_seq,end_seq) VALUES ($1,$2,2,30,NULL)"
        >>,
        [?GROUP_A, ?USER_CURRENT]
    ).

seed_second_snapshot_target(Conn) ->
    equery(
        Conn,
        <<
            "INSERT INTO msg_c2g_recipient_snapshot "
            "(msg_id,from_id,to_gid,conv_seq,recipient_uids,created_at) "
            "VALUES ('ga-second-original',$1,$2,30,$3,now())"
        >>,
        [?USER_CURRENT, ?GROUP_A, [?USER_CURRENT, ?USER_NEW]]
    ).

action_recipients(Conn, OriginalMsgId) ->
    Sql = msg_store_repo:action_recipient_sql(),
    case elib_pg:query(Conn, Sql, [?GROUP_A, ?USER_CURRENT, 1, OriginalMsgId, 5001]) of
        {ok, Rows} -> [Uid || #{<<"user_id">> := Uid} <- Rows];
        Other -> erlang:error({unexpected_action_recipient_result, Other})
    end.

member_public_keys_matrix(Conn) ->
    ok = squery(Conn, <<"BEGIN">>),
    try
        seed_member_public_keys_rows(Conn),
        assert_member_public_keys_authorization(Conn),
        assert_member_limit_sentinel(Conn),
        assert_device_limit_sentinel(Conn)
    after
        ok = squery(Conn, <<"ROLLBACK">>)
    end.

seed_member_public_keys_rows(Conn) ->
    squery(Conn, <<
        "INSERT INTO \"group\" (id,owner_uid,creator_uid,title,status) VALUES "
        "(91081201,91081101,91081101,'ga-key-auth',1),"
        "(91081202,91081101,91081101,'ga-key-members',1),"
        "(91081203,91081101,91081101,'ga-key-devices',1);"
        "INSERT INTO group_member (id,group_id,user_id,role,is_join,status) VALUES "
        "(91081301,91081201,91081101,1,true,1),"
        "(91081302,91081201,91081102,1,true,1),"
        "(91081305,91081201,91081103,1,true,0),"
        "(91081303,91081202,91081101,1,true,1),"
        "(91081304,91081203,91081101,1,true,1);"
        "INSERT INTO user_device "
        "(id,user_id,device_type,device_id,status,public_key,key_id) VALUES "
        "(91081401,91081101,'android','key-active-a',1,'pk-active-a','kid-active-a'),"
        "(91081402,91081101,'ios','key-inactive',0,'pk-inactive','kid-inactive'),"
        "(91081403,91081101,'web','key-empty',1,'','kid-empty'),"
        "(91081404,91081102,'ios','key-active-b',1,'pk-active-b','kid-active-b'),"
        "(91081405,91081103,'android','key-inactive-member',1,"
        "'pk-inactive-member','kid-inactive-member')"
    >>).

assert_member_public_keys_authorization(Conn) ->
    ?assertEqual([], member_public_key_rows(Conn, ?KEY_GROUP, 91081999)),
    Rows = member_public_key_rows(Conn, ?KEY_GROUP, ?KEY_CALLER),
    ?assertEqual(
        [
            {?KEY_CALLER, <<"key-active-a">>, <<"kid-active-a">>},
            {?KEY_OTHER, <<"key-active-b">>, <<"kid-active-b">>}
        ],
        lists:sort([
            {Uid, DeviceId, KeyId}
         || #{
                <<"user_id">> := Uid,
                <<"device_id">> := DeviceId,
                <<"key_id">> := KeyId,
                <<"member_overflow">> := false
            } <- Rows
        ])
    ),
    ok = equery(
        Conn,
        <<"UPDATE group_member SET status=0 WHERE group_id=$1 AND user_id=$2">>,
        [?KEY_GROUP, ?KEY_OTHER]
    ),
    ?assertMatch(
        [#{<<"user_id">> := ?KEY_CALLER, <<"device_id">> := <<"key-active-a">>}],
        member_public_key_rows(Conn, ?KEY_GROUP, ?KEY_CALLER)
    ),
    ok = equery(
        Conn,
        <<"UPDATE group_member SET status=1 WHERE group_id=$1 AND user_id=$2">>,
        [?KEY_GROUP, ?KEY_OTHER]
    ),
    ok = equery(
        Conn,
        <<"UPDATE group_member SET status=0 WHERE group_id=$1 AND user_id=$2">>,
        [?KEY_GROUP, ?KEY_CALLER]
    ),
    ?assertEqual([], member_public_key_rows(Conn, ?KEY_GROUP, ?KEY_CALLER)),
    ok = equery(
        Conn,
        <<"UPDATE group_member SET status=1 WHERE group_id=$1 AND user_id=$2">>,
        [?KEY_GROUP, ?KEY_CALLER]
    ),
    ok = equery(Conn, <<"UPDATE \"group\" SET status=0 WHERE id=$1">>, [?KEY_GROUP]),
    ?assertEqual([], member_public_key_rows(Conn, ?KEY_GROUP, ?KEY_CALLER)),
    ok = equery(Conn, <<"UPDATE \"group\" SET status=1 WHERE id=$1">>, [?KEY_GROUP]),
    ok = equery(
        Conn,
        <<"UPDATE user_device SET status=0 WHERE user_id IN ($1,$2)">>,
        [?KEY_CALLER, ?KEY_OTHER]
    ),
    ?assertMatch(
        [#{<<"member_overflow">> := false, <<"user_id">> := null}],
        member_public_key_rows(Conn, ?KEY_GROUP, ?KEY_CALLER)
    ).

assert_member_limit_sentinel(Conn) ->
    ok = equery(
        Conn,
        <<
            "INSERT INTO group_member (id,group_id,user_id,role,is_join,status) "
            "SELECT 92000000+n,$1,93000000+n,1,true,1 FROM generate_series(1,4095) n"
        >>,
        [?KEY_MEMBER_LIMIT_GROUP]
    ),
    ?assertMatch(
        [#{<<"member_overflow">> := false, <<"user_id">> := null}],
        member_public_key_rows(Conn, ?KEY_MEMBER_LIMIT_GROUP, ?KEY_CALLER)
    ),
    ok = equery(
        Conn,
        <<
            "INSERT INTO group_member (id,group_id,user_id,role,is_join,status) "
            "VALUES (92004096,$1,93004096,1,true,1)"
        >>,
        [?KEY_MEMBER_LIMIT_GROUP]
    ),
    ?assertMatch(
        [#{<<"member_overflow">> := true, <<"user_id">> := null}],
        member_public_key_rows(Conn, ?KEY_MEMBER_LIMIT_GROUP, ?KEY_CALLER)
    ).

assert_device_limit_sentinel(Conn) ->
    ok = equery(
        Conn,
        <<
            "INSERT INTO user_device "
            "(id,user_id,device_type,device_id,status,public_key,key_id) "
            "SELECT 94000000+n,$1,'android','dev-'||n,1,'pk-'||n,'kid-'||n "
            "FROM generate_series(1,4096) n"
        >>,
        [?KEY_CALLER]
    ),
    ?assertEqual(
        4096,
        length(member_public_key_rows(Conn, ?KEY_DEVICE_LIMIT_GROUP, ?KEY_CALLER))
    ),
    ok = equery(
        Conn,
        <<
            "INSERT INTO user_device "
            "(id,user_id,device_type,device_id,status,public_key,key_id) "
            "VALUES (94004097,$1,'android','dev-4097',1,'pk-4097','kid-4097')"
        >>,
        [?KEY_CALLER]
    ),
    ?assertEqual(
        ?KEY_PROBE_LIMIT,
        length(member_public_key_rows(Conn, ?KEY_DEVICE_LIMIT_GROUP, ?KEY_CALLER))
    ).

member_public_key_rows(Conn, Gid, CurrentUid) ->
    Sql = group_member_repo:authorized_public_keys_sql(),
    case elib_pg:query(Conn, Sql, [Gid, CurrentUid, ?KEY_PROBE_LIMIT]) of
        {ok, Rows} -> Rows;
        Other -> erlang:error({unexpected_member_public_keys_result, Other})
    end.

allowed(Conn, Path, Uid) ->
    Sql = attachment_repo:group_access_sql(<<"public.attachment">>),
    case elib_pg:query(Conn, Sql, [Path, Uid]) of
        {ok, [#{<<"allowed">> := Value}]} -> Value;
        Other -> erlang:error({unexpected_acl_result, Other})
    end.

column_exists(Conn, Column) ->
    table_column_exists(Conn, <<"attachment">>, Column).

table_column_exists(Conn, Table, Column) ->
    scalar(
        Conn,
        <<
            "SELECT EXISTS (SELECT 1 FROM information_schema.columns "
            "WHERE table_schema='public' AND table_name=$1 AND column_name=$2)"
        >>,
        [Table, Column]
    ).

table_exists(Conn, Table) ->
    scalar(
        Conn,
        <<
            "SELECT EXISTS (SELECT 1 FROM information_schema.tables "
            "WHERE table_schema='public' AND table_name=$1)"
        >>,
        [Table]
    ).

constraint_exists(Conn, Name) ->
    scalar(
        Conn,
        <<
            "SELECT EXISTS (SELECT 1 FROM pg_constraint WHERE conname=$1)"
        >>,
        [Name]
    ).

index_exists(Conn, Name) ->
    scalar(
        Conn,
        <<
            "SELECT EXISTS (SELECT 1 FROM pg_indexes WHERE schemaname='public' AND indexname=$1)"
        >>,
        [Name]
    ).

scalar(Conn, Sql) ->
    scalar(Conn, Sql, []).

scalar(Conn, Sql, Params) ->
    case epgsql:equery(Conn, Sql, Params) of
        {ok, _Columns, [{Value}]} -> Value;
        Other -> erlang:error({unexpected_scalar_result, Other})
    end.

equery(Conn, Sql, Params) ->
    case epgsql:equery(Conn, Sql, Params) of
        {ok, _Count} -> ok;
        {ok, _Count, _Columns, _Rows} -> ok;
        Other -> erlang:error({sql_failed, Other})
    end.

squery(Conn, Sql) ->
    case epgsql:squery(Conn, Sql) of
        Results when is_list(Results) ->
            case [Error || Error = {error, _} <- Results] of
                [] -> ok;
                Errors -> erlang:error({sql_failed, Errors})
            end;
        {ok, _} ->
            ok;
        {ok, _, _} ->
            ok;
        Other ->
            erlang:error({sql_failed, Other})
    end.

read_migration(RelativePath) ->
    {ok, Cwd} = file:get_cwd(),
    {ok, Sql} = file:read_file(filename:join(Cwd, RelativePath)),
    Sql.
