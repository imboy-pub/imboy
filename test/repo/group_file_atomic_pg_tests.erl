-module(group_file_atomic_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-export([run/1]).
-define(KEY, <<"tenant/u1/g11/20261001/random/a.txt">>).

run(Socket) ->
    {ok, C} = epgsql:connect(#{
        host => {local, Socket},
        port => 0,
        username => "departure_test",
        database => "postgres",
        codecs => [{epgsql_codec_rfc3339_bin, []}]
    }),
    try
        schema(C),
        mocks(C),
        eunit:test(
            [
                {"file operations recheck current scope", fun() -> verify_scope(C) end},
                {"human confirm audits once and rolls back on audit failure", fun() ->
                    verify_confirm(C, Socket)
                end}
            ] ++
                [
                    {atom_to_list(Mode), fun() -> verify(C, Mode) end}
                 || Mode <- [
                        success,
                        pending_registration_failure,
                        storage_timeout,
                        pending_remove_failure,
                        attachment_failure,
                        file_failure,
                        audit_failure,
                        revoked_during_upload,
                        revoked_group_during_upload
                    ]
                ] ++ [{"binding index reversible", fun() -> verify_index(C) end}],
            [verbose]
        )
    after
        meck:unload(),
        epgsql:close(C)
    end.

verify_scope(C) ->
    sql(C, <<
        "DELETE FROM attachment; DELETE FROM group_file; DELETE FROM attach_pending; DELETE FROM enterprise_audit_event;"
        "UPDATE organization_member SET status='active';"
        "UPDATE group_member SET status=1;"
    >>),
    meck:expect(elib_oss, put_object, fun(<<"private">>, ?KEY, _, _) -> ok end),
    ?assertEqual(
        {ok, <<"test-file">>},
        group_file_ds:upload_file(11, 1, <<"a.txt">>, <<0, 1, 2>>, <<"text/plain">>)
    ),
    ?assertMatch(
        {ok, [#{<<"object_key">> := ?KEY}]},
        group_file_ds:list_files(11, 1, 1, 10)
    ),
    ?assertEqual(
        1,
        count(
            C,
            <<"enterprise_audit_event WHERE action='file.uploaded' AND actor_user_id=1 AND organization_id=10 AND resource_id=101 AND actor_role='human'">>
        )
    ),
    ?assertEqual({error, not_member}, group_file_ds:list_files(11, 2, 1, 10)),
    ?assertEqual({error, not_member}, group_file_ds:download_file(101, 2)),
    ?assertMatch(
        {ok, <<"https://storage.example.com/api/v1/attachment/content?ticket=", _/binary>>},
        group_file_logic:download(101, 1)
    ),
    ?assertMatch(
        {ok, [#{<<"object_key">> := ?KEY}]},
        group_file_ds:search_files(11, <<"a">>, 1, 10, 1)
    ),
    ?assertMatch(
        {ok, [#{<<"object_key">> := ?KEY}]},
        group_file_repo:list_by_category(11, <<"document">>, 1, 10)
    ),
    lists:foreach(
        fun(Change) ->
            sql(C, Change),
            ?assertMatch(
                {ok, [#{<<"object_key">> := null}]},
                group_file_ds:list_files(11, 1, 1, 10)
            ),
            ?assertEqual({error, not_found}, group_file_logic:download(101, 1)),
            sql(C, <<"UPDATE attachment SET scope='group',scope_ref='11',status=1">>)
        end,
        [
            <<"UPDATE attachment SET scope='public'">>,
            <<"UPDATE attachment SET scope_ref='22'">>,
            <<"UPDATE attachment SET status=-1">>
        ]
    ),
    sql(C, <<"UPDATE group_member_generation SET end_seq=10">>),
    ?assertEqual({error, forbidden}, group_file_logic:download(101, 1)),
    sql(C, <<"UPDATE group_member_generation SET end_seq=NULL">>),
    ?assertMatch({ok, [_]}, group_file_logic:get_categories(<<"11">>, 1)),
    lists:foreach(
        fun(Revoke) ->
            sql(C, Revoke),
            ?assertEqual({error, not_member}, group_file_ds:list_files(11, 1, 1, 10)),
            ?assertEqual({error, not_member}, group_file_ds:search_files(11, <<"a">>, 1, 10, 1)),
            ?assertEqual({error, not_member}, group_file_logic:get_categories(<<"11">>, 1)),
            ?assertEqual({error, not_member}, group_file_ds:download_file(101, 1)),
            ?assertEqual({error, not_member}, group_file_ds:delete_file(101, 1)),
            ?assertEqual(1, count(C, <<"group_file WHERE status=1">>)),
            sql(C, <<
                "UPDATE organization_member SET status='active';"
                "UPDATE workspace_member SET status='active'; UPDATE group_member SET status=1"
            >>)
        end,
        [
            <<"UPDATE organization_member SET status='suspended'">>,
            <<"UPDATE workspace_member SET status='removed'">>,
            <<"UPDATE group_member SET status=0">>
        ]
    ),
    sql(C, <<"UPDATE workspace SET status='archived'">>),
    ?assertMatch({ok, [_]}, group_file_ds:list_files(11, 1, 1, 10)),
    ?assertMatch({error, {980, _}}, group_file_ds:delete_file(101, 1)),
    sql(C, <<"UPDATE workspace SET status='active'">>),
    sql(
        C,
        <<"ALTER TABLE enterprise_audit_event ADD CONSTRAINT reject_delete CHECK(action<>'file.deleted') NOT VALID">>
    ),
    ?assertMatch({error, {audit_failed, _}}, group_file_ds:delete_file(101, 1)),
    ?assertEqual(1, count(C, <<"group_file WHERE status=1">>)),
    ?assertEqual(1, count(C, <<"enterprise_audit_event">>)),
    sql(C, <<"ALTER TABLE enterprise_audit_event DROP CONSTRAINT reject_delete">>),
    ?assertEqual(ok, group_file_ds:delete_file(101, 1)),
    ?assertEqual(
        1,
        count(
            C,
            <<"enterprise_audit_event WHERE action='file.deleted' AND actor_user_id=1 AND organization_id=10 AND resource_id=101">>
        )
    ),
    ?assertEqual({error, not_found}, group_file_ds:delete_file(101, 1)),
    ?assertEqual(2, count(C, <<"enterprise_audit_event">>)),
    ?assertEqual({ok, []}, group_file_ds:list_files(11, 1, 1, 10)),
    ?assertEqual({error, not_found}, group_file_ds:download_file(101, 1)).

verify_confirm(C, Socket) ->
    sql(
        C,
        <<"DELETE FROM attachment; DELETE FROM enterprise_audit_event; DELETE FROM attach_pending">>
    ),
    Key = <<"u1/g11/20261002/confirmation.txt">>,
    meck:expect(elib_oss, owner_of_key, fun(Key0) when Key0 =:= Key -> {ok, 1} end),
    meck:expect(elib_oss, head_object, fun(<<"private">>, Key0) when Key0 =:= Key ->
        {ok, #{size => 3, content_type => <<"text/plain">>}}
    end),
    meck:new(group_member_ds, [non_strict, no_link]),
    meck:expect(group_member_ds, is_member, fun(11, 1) -> true end),
    Meta = #{<<"anchor_msg_id">> => <<"confirmed-human-message">>},
    ?assertMatch({ok, _}, attach_logic:confirm(1, Key, <<"group">>, <<"11">>, Meta)),
    ?assertEqual(
        1,
        count(
            C,
            <<"enterprise_audit_event WHERE action='file.confirmed' AND resource_type='attachment' AND resource_id IN (SELECT id FROM attachment) AND actor_user_id=1 AND organization_id=10">>
        )
    ),
    ?assertMatch({ok, _}, attach_logic:confirm(1, Key, <<"group">>, <<"11">>, Meta)),
    ?assertEqual(1, count(C, <<"enterprise_audit_event">>)),
    ?assertEqual(1, count(C, <<"attachment WHERE referer_time=2">>)),
    sql(
        C,
        <<"DELETE FROM attachment; ALTER TABLE enterprise_audit_event ADD CONSTRAINT reject_confirm CHECK(action<>'file.confirmed') NOT VALID">>
    ),
    ?assertMatch(
        {error, {audit_failed, _}}, attach_logic:confirm(1, Key, <<"group">>, <<"11">>, Meta)
    ),
    ?assertEqual(0, count(C, <<"attachment">>)),
    ?assertEqual(1, count(C, <<"enterprise_audit_event">>)),
    sql(C, <<"ALTER TABLE enterprise_audit_event DROP CONSTRAINT reject_confirm">>),
    sql(C, <<"DELETE FROM attachment; DELETE FROM enterprise_audit_event">>),
    ?assert(
        lists:all(
            fun(R) -> element(1, R) =:= ok end,
            parallel_confirm(Socket, Key, Meta, [<<"11">>, <<"11">>], false)
        )
    ),
    ?assertEqual(1, count(C, <<"attachment WHERE referer_time=2">>)),
    ?assertEqual(1, count(C, <<"enterprise_audit_event WHERE action='file.confirmed'">>)),
    verify_cross_scope_confirm(C, Socket, Key, Meta),
    meck:unload(group_member_ds).

verify_cross_scope_confirm(C, Socket, Key, Meta) ->
    sql(C, <<
        "DELETE FROM attachment; DELETE FROM enterprise_audit_event;"
        "INSERT INTO organization_member VALUES(20,1,'active','member');"
        "INSERT INTO workspace VALUES(200,20,'active');"
        "INSERT INTO workspace_member VALUES(200,1,'active');"
        "INSERT INTO \"group\" VALUES(22,1,'workspace',200);"
        "INSERT INTO group_member VALUES(22,1,1)"
    >>),
    meck:expect(group_member_ds, is_member, fun(_, 1) -> true end),
    Parent = self(),
    meck:new(attachment_repo, [passthrough, no_link]),
    meck:expect(attachment_repo, confirmation_row_tx, fun(Conn, ObjectKey) ->
        Result = meck:passthrough([Conn, ObjectKey]),
        case {get(confirm_lookup_seen), Result} of
            {undefined, {ok, not_found}} ->
                put(confirm_lookup_seen, true),
                Parent ! {before_insert, self()},
                receive
                    continue -> ok
                after 5000 -> error(insert_barrier_timeout)
                end;
            _ ->
                ok
        end,
        Result
    end),
    Results = parallel_confirm(Socket, Key, Meta, [<<"11">>, <<"22">>], true),
    ?assertEqual(1, length([R || {ok, _} = R <- Results])),
    ?assertEqual(1, length([R || {error, forbidden} = R <- Results])),
    ?assertEqual(1, count(C, <<"attachment WHERE referer_time=1">>)),
    ?assertEqual(1, count(C, <<"enterprise_audit_event WHERE action='file.confirmed'">>)),
    meck:unload(attachment_repo).

parallel_confirm(Socket, Key, Meta, Refs, Gate) ->
    Parent = self(),
    Workers = [
        spawn_link(fun() ->
            {ok, Conn} = epgsql:connect(#{
                host => {local, Socket},
                port => 0,
                username => "departure_test",
                database => "postgres",
                codecs => [{epgsql_codec_rfc3339_bin, []}]
            }),
            put(atomic_file_conn, Conn),
            Parent ! {ready, self()},
            receive
                go -> ok
            end,
            Result = attach_logic:confirm(1, Key, <<"group">>, Ref, Meta),
            epgsql:close(Conn),
            Parent ! {confirmed, self(), Result}
        end)
     || Ref <- Refs
    ],
    lists:foreach(
        fun(Pid) ->
            receive
                {ready, Pid} -> Pid ! go
            after 5000 -> error(worker_ready_timeout)
            end
        end,
        Workers
    ),
    case Gate of
        true ->
            Waiting = [
                receive
                    {before_insert, Pid} -> Pid
                after 5000 -> error(barrier_timeout)
                end
             || _ <- Workers
            ],
            lists:foreach(fun(Pid) -> Pid ! continue end, Waiting);
        false ->
            ok
    end,
    [
        receive
            {confirmed, Pid, Result} -> Result
        after 5000 -> error(confirm_timeout)
        end
     || Pid <- Workers
    ].

schema(C) ->
    sql(C, <<
        "CREATE TABLE organization(id bigint PRIMARY KEY,status text);"
        "CREATE TABLE organization_member(organization_id bigint,user_id bigint,status text,role text);"
        "CREATE TABLE workspace(id bigint PRIMARY KEY,organization_id bigint,status text);"
        "CREATE TABLE workspace_member(workspace_id bigint,user_id bigint,status text);"
        "CREATE TABLE \"group\"(id bigint PRIMARY KEY,status int,scope text,workspace_id bigint);"
        "INSERT INTO organization VALUES(10,'active'),(20,'active');"
        "CREATE TABLE enterprise_audit_event(id bigint PRIMARY KEY,organization_id bigint,resource_type text,resource_id bigint,"
        "action text,business_identity_id bigint,actor_user_id bigint,actor_role text,detail jsonb,created_at timestamptz DEFAULT now());"
        "INSERT INTO organization_member VALUES(20,2,'active','member');"
        "INSERT INTO organization_member VALUES(10,1,'active','member');"
        "INSERT INTO workspace VALUES(100,10,'active');"
        "INSERT INTO workspace_member VALUES(100,1,'active'),(100,2,'active');"
        "INSERT INTO \"group\" VALUES(11,1,'workspace',100);"
        "CREATE TABLE group_member(group_id bigint,user_id bigint,status int);"
        "INSERT INTO group_member VALUES(11,1,1),(11,2,1);"
        "CREATE TABLE group_member_generation(group_id bigint,user_id bigint,start_seq bigint,end_seq bigint);"
        "INSERT INTO group_member_generation VALUES(11,1,1,NULL),(11,2,1,NULL);"
        "CREATE TABLE group_file(id bigint PRIMARY KEY,group_id bigint,file_id text,file_name text,"
        "file_size bigint CHECK(file_size<>4),file_type text,file_category text,file_url text,"
        "file_hash text,uploader_id bigint,download_count int,status int,"
        "created_at timestamptz,updated_at timestamptz);"
        "CREATE TABLE attach_pending(object_key text PRIMARY KEY,bucket text,scope text,creator_user_id bigint,created_at timestamptz);"
        "CREATE FUNCTION reject_pending_delete() RETURNS trigger LANGUAGE plpgsql AS $$ "
        "BEGIN RAISE EXCEPTION 'injected delete failure'; END $$;"
        "CREATE TABLE attachment(id bigint PRIMARY KEY,file_hash256 text,mime_type text,ext text,"
        "name text,path text UNIQUE,url text,size bigint CHECK(size<>2),info jsonb,"
        "referer_time int,last_referer_user_id bigint,last_referer_at timestamptz,"
        "creator_user_id bigint,scope text,scope_ref text,cipher text,anchor_msg_id text,"
        "group_file_id bigint,anchor_conv_seq bigint,created_at timestamptz,updated_at timestamptz,status int);"
    >>),
    migration(C, "up").

verify_index(C) ->
    Before = count(C, <<"attachment">>),
    sql(C, <<"SET enable_seqscan=off">>),
    {ok, _, Rows} = epgsql:equery(
        C,
        <<
            "EXPLAIN SELECT path FROM attachment WHERE group_file_id=$1 AND scope='group' "
            "AND scope_ref=$2::bigint::text AND status>=0 ORDER BY id LIMIT 1"
        >>,
        [101, 11]
    ),
    Plan = iolist_to_binary([Line || {Line} <- Rows]),
    ?assertNotEqual(nomatch, binary:match(Plan, <<"idx_attachment_group_file_binding">>)),
    sql(C, <<"RESET enable_seqscan">>),
    migration(C, "down"),
    {ok, _, [{null}]} = epgsql:equery(
        C,
        <<"SELECT to_regclass('public.idx_attachment_group_file_binding')">>,
        []
    ),
    ?assertEqual(Before, count(C, <<"attachment">>)),
    migration(C, "up"),
    migration(C, "up"),
    ?assertEqual(Before, count(C, <<"attachment">>)).

%% 从仓库根运行，以执行实际迁移文件而非复制的 DDL。
migration(C, Direction) ->
    Name = "priv/migrations/00000157_group_file_attachment_binding_index." ++ Direction ++ ".sql",
    {ok, Sql} = file:read_file(Name),
    sql(C, Sql).

mocks(C) ->
    meck:new(config_ds, [non_strict, no_link]),
    meck:expect(config_ds, env, fun(sql_driver) -> pgsql end),
    meck:expect(config_ds, env, fun
        (jwt_key, _) -> <<"synthetic-file-ticket-key-32-bytes">>;
        (base_url, _) -> <<"https://storage.example.com">>
    end),
    meck:new(pooler, [non_strict, no_link]),
    meck:expect(pooler, take_member, fun(pgsql) ->
        case get(atomic_file_conn) of
            undefined -> C;
            WorkerConn -> WorkerConn
        end
    end),
    meck:expect(pooler, return_member, fun(pgsql, _) -> ok end),
    meck:expect(pooler, return_member, fun(pgsql, _, _) -> ok end),
    meck:new(group_ds, [non_strict, no_link]),
    meck:expect(group_ds, is_member, fun(_, _) -> true end),
    meck:new(elib_oss, [non_strict, no_link]),
    meck:expect(elib_oss, validate_file_type, fun(_) -> true end),
    meck:expect(elib_oss, max_file_size, fun() -> 1000 end),
    meck:expect(elib_oss, build_object_key, fun(1, <<"group">>, <<"11">>, <<"a.txt">>) -> ?KEY end),
    meck:expect(elib_oss, generate_file_id, fun() -> <<"test-file">> end),
    meck:expect(elib_oss, get_url, fun(?KEY) -> {ok, <<"https://storage.example.com/key">>} end),
    meck:expect(elib_oss, delete_object, fun(<<"private">>, ?KEY) -> ok end),
    meck:expect(elib_oss, get_file_category, fun(_) -> document end),
    meck:expect(elib_oss, get_bucket, fun(<<"group">>) -> <<"private">> end),
    meck:expect(elib_oss, presign_get_for_key, fun(<<"private">>, ?KEY, 600) ->
        <<"https://storage.example.com/signed">>
    end),
    meck:new(workspace_guard, [passthrough, no_link]),
    meck:expect(workspace_guard, write_tx_or_skip, fun(_, _) -> ok end),
    meck:new(elib_tsid, [non_strict, no_link]),
    meck:expect(elib_tsid, registered, fun() -> [enterprise_audit_event] end),
    meck:expect(elib_tsid, generate, fun
        (group_file) -> 101;
        (attachment) -> erlang:unique_integer([positive, monotonic]);
        (enterprise_audit_event) -> erlang:unique_integer([positive, monotonic])
    end).

verify(C, Mode) ->
    sql(C, <<
        "DELETE FROM attachment; DELETE FROM group_file; DELETE FROM attach_pending; DELETE FROM enterprise_audit_event;"
        "UPDATE organization_member SET status='active'; UPDATE group_member SET status=1"
    >>),
    case Mode of
        audit_failure ->
            sql(
                C,
                <<"ALTER TABLE enterprise_audit_event ADD CONSTRAINT reject_upload CHECK(action<>'file.uploaded') NOT VALID">>
            );
        pending_registration_failure ->
            sql(
                C,
                <<"ALTER TABLE attach_pending ADD CONSTRAINT reject_group CHECK(scope<>'group') NOT VALID">>
            );
        pending_remove_failure ->
            sql(
                C,
                <<"CREATE TRIGGER pending_remove_failure BEFORE DELETE ON attach_pending FOR EACH ROW EXECUTE FUNCTION reject_pending_delete()">>
            );
        _ ->
            ok
    end,
    BeforePut = meck:num_calls(elib_oss, put_object, 4),
    meck:expect(elib_oss, put_object, fun(<<"private">>, ?KEY, _, _) ->
        ?assertEqual(
            1,
            count(
                C,
                <<"attach_pending WHERE bucket='private' AND creator_user_id=1 AND scope='group'">>
            )
        ),
        case Mode of
            revoked_during_upload ->
                sql(
                    C,
                    <<"UPDATE organization_member SET status='suspended'">>
                );
            revoked_group_during_upload ->
                sql(C, <<"UPDATE group_member SET status=0">>);
            _ ->
                ok
        end,
        case Mode of
            storage_timeout -> {error, timeout};
            _ -> ok
        end
    end),
    Data =
        case Mode of
            attachment_failure -> <<0, 1>>;
            file_failure -> <<0, 1, 2, 3>>;
            _ -> <<0, 1, 2>>
        end,
    Result = group_file_ds:upload_file(11, 1, <<"a.txt">>, Data, <<"text/plain">>),
    case Mode of
        pending_registration_failure ->
            ?assertMatch({error, _}, Result),
            ?assertEqual(BeforePut, meck:num_calls(elib_oss, put_object, 4)),
            ?assertEqual(0, count(C, <<"attach_pending">>)),
            sql(C, <<"ALTER TABLE attach_pending DROP CONSTRAINT reject_group">>),
            assert_empty(C);
        storage_timeout ->
            ?assertEqual({error, timeout}, Result),
            assert_empty(C);
        Completed when Completed =:= success; Completed =:= pending_remove_failure ->
            ?assertEqual({ok, <<"test-file">>}, Result),
            ?assertEqual(1, count(C, <<"group_file">>)),
            ?assertEqual(1, count(C, <<"attachment">>)),
            ?assertEqual(1, count(C, <<"enterprise_audit_event">>)),
            {ok, _, [{101, 1, <<"group">>, <<"11">>, ?KEY}]} = epgsql:equery(
                C,
                <<"SELECT group_file_id,creator_user_id,scope,scope_ref,path FROM attachment">>,
                []
            );
        audit_failure ->
            ?assertMatch({error, {audit_failed, _}}, Result),
            assert_empty(C),
            ?assertEqual(0, count(C, <<"enterprise_audit_event">>)),
            sql(C, <<"ALTER TABLE enterprise_audit_event DROP CONSTRAINT reject_upload">>);
        attachment_failure ->
            ?assertMatch({error, {attachment_save_failed, _}}, Result),
            assert_empty(C);
        file_failure ->
            ?assertMatch({error, _}, Result),
            assert_empty(C);
        revoked_during_upload ->
            ?assertEqual({error, forbidden}, Result),
            assert_empty(C);
        revoked_group_during_upload ->
            ?assertEqual({error, forbidden}, Result),
            assert_empty(C)
    end,
    verify_pending(C, Mode).

verify_pending(C, Mode) when Mode =:= success; Mode =:= pending_registration_failure ->
    ?assertEqual(0, count(C, <<"attach_pending">>));
verify_pending(C, pending_remove_failure) ->
    ?assertEqual(1, count(C, <<"attach_pending">>)),
    sql(
        C,
        <<"DROP TRIGGER pending_remove_failure ON attach_pending; UPDATE attach_pending SET created_at=NOW()-INTERVAL '3 hours'">>
    ),
    BeforeDelete = meck:num_calls(elib_oss, delete_object, 2),
    ?assertEqual({ok, #{cleaned => 0, errors => 0}}, attachment_ds:pending_cleanup(2)),
    ?assertEqual(BeforeDelete, meck:num_calls(elib_oss, delete_object, 2)),
    ?assertEqual(1, count(C, <<"attachment">>));
verify_pending(C, _FailedMode) ->
    ?assertEqual(1, count(C, <<"attach_pending">>)),
    sql(C, <<"UPDATE attach_pending SET created_at=NOW()-INTERVAL '3 hours'">>),
    meck:expect(elib_oss, delete_object, fun(<<"private">>, ?KEY) -> {error, unavailable} end),
    ?assertEqual({ok, #{cleaned => 0, errors => 1}}, attachment_ds:pending_cleanup(2)),
    ?assertEqual(1, count(C, <<"attach_pending">>)),
    meck:expect(elib_oss, delete_object, fun(<<"private">>, ?KEY) -> ok end),
    ?assertEqual({ok, #{cleaned => 1, errors => 0}}, attachment_ds:pending_cleanup(2)),
    ?assertEqual(0, count(C, <<"attach_pending">>)).

assert_empty(C) ->
    ?assertEqual(0, count(C, <<"group_file">>)),
    ?assertEqual(0, count(C, <<"attachment">>)).

count(C, Table) ->
    {ok, _, [{N}]} = epgsql:equery(C, <<"SELECT count(*) FROM ", Table/binary>>, []),
    N.

sql(C, Q) ->
    R = epgsql:squery(C, Q),
    lists:foreach(
        fun(Item) -> ?assertNotMatch({error, _}, Item) end,
        case is_list(R) of
            true -> R;
            false -> [R]
        end
    ).
