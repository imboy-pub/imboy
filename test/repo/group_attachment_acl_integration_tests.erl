%% Migration 108 and group attachment ACL PostgreSQL integration coverage.
%% The dedicated harness supplies a marker scratch database through
%% IMBOY_GA_TEST_*; normal EUnit runs skip rather than touching a shared DB.

-module(group_attachment_acl_integration_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-define(USER_CURRENT, 91080101).
-define(USER_NEW, 91080102).
-define(GROUP_A, 91080201).
-define(GROUP_B, 91080202).

group_attachment_acl_postgres_test_() ->
    {timeout, 120, {setup, fun setup/0, fun cleanup/1, fun run/1}}.

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
            migration_roundtrip_and_backfill(Conn),
            authorization_matrix(Conn)
        end)
    ].

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
        "INSERT INTO group_member (id,group_id,user_id,is_join,status) VALUES "
        "(91080301,91080201,91080101,true,1),"
        "(91080302,91080201,91080102,true,1);"
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

allowed(Conn, Path, Uid) ->
    Sql = attachment_repo:group_access_sql(<<"public.attachment">>),
    case elib_pg:query(Conn, Sql, [Path, Uid]) of
        {ok, [#{<<"allowed">> := Value}]} -> Value;
        Other -> erlang:error({unexpected_acl_result, Other})
    end.

column_exists(Conn, Column) ->
    scalar(
        Conn,
        <<
            "SELECT EXISTS (SELECT 1 FROM information_schema.columns "
            "WHERE table_schema='public' AND table_name='attachment' AND column_name=$1)"
        >>,
        [Column]
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
