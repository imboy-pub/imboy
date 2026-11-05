%%% Real Garage plus disposable PostgreSQL; production default assembly, no stub.
-module(enterprise_asset_garage_pg_checks).
-export([run/0]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

run() ->
    {ok, _} = application:ensure_all_started(inets),
    Config = #{
        endpoint => env("IMBOY_TEST_ENDPOINT"),
        bucket => env_or("IMBOY_TEST_BUCKET", <<"synthetic-enterprise-assets">>),
        region => <<"garage">>,
        access_key => env("IMBOY_TEST_ACCESS"),
        secret_key => env("IMBOY_TEST_SECRET")
    },
    ok = intbe02_http_support:declare_real_garage(Config),
    H = intbe02_http_support:setup_all(),
    try
        run_with_edge(H, Config)
    after
        intbe02_http_support:teardown_all(H),
        inttest_marker_db:release(H)
    end.

run_with_edge(H, Config) ->
    Edge = intbe02_http_support:start_direct_edge(maps:get(port, H)),
    try
        application:unset_env(imboy, eb_asset_object_store),
        stub_regression(),
        application:set_env(imboy, garage, Config),
        application:set_env(imboy, attachment_storage_registry, #{
            version => 1,
            upload_namespaces => #{
                <<"enterprise">> => <<"eb-garage-fixture">>,
                <<"group">> => <<"eb-garage-fixture">>,
                <<"channel">> => <<"eb-garage-fixture">>
            },
            roots => #{
                <<"eb-garage-fixture">> => #{
                    driver => <<"garage">>,
                    namespace => <<"eb-garage-fixture">>,
                    endpoint => maps:get(endpoint, Config),
                    container => maps:get(bucket, Config)
                }
            }
        }),
        direct_retirement_scope_checks(maps:get(conn, H)),
        direct_bind_atomic_checks(
            maps:get(conn, H), intbe02_http_support:fixture(grp_human, H), 995011
        ),
        direct_bind_revocation_race(
            maps:get(conn, H), intbe02_http_support:fixture(grp_human, H), 995011
        ),
        c08_finalize_personal_race_checks:run(maps:get(conn, H)),
        direct_channel_reference_checks(maps:get(conn, H)),
        human_group_journey(H),
        S = cs_pg_test_fixture:new_scope(),
        ok = cs_pg_test_fixture:exec(
            <<"UPDATE organization_business_identity_assignment SET business_identity_id=$3,function_key='customer_service' WHERE organization_id=$1 AND user_id=$2">>,
            [maps:get(org_id, S), maps:get(actor_user_id, S), maps:get(service_identity_id, S)]
        ),
        journey(S),
        governance_intent_race(maps:get(conn, H)),
        rollback_object(S),
        cleanup_recovery(S, Config),
        cleanup_concurrency(S),
        cleanup_ineligible(S),
        purge_regressions(),
        run_contract_regressions(),
        application:unset_env(imboy, garage),
        Org = maps:get(org_id, S),
        Ws = maps:get(workspace_id, S),
        FailedId = cs_pg_test_fixture:id(),
        ?assertEqual(
            {error, location_changed},
            put_fixture(Org, Ws, #{
                id => FailedId,
                payload => <<"x">>,
                object_hash => eb_asset_content:sha256_hex(<<"x">>)
            })
        ),
        {error, not_found} = eb_asset_store:fetch_asset(Org, Ws, FailedId),
        application:set_env(imboy, eb_asset_object_store, invalid),
        ?assertEqual({error, invalid_object_store}, put_fixture(Org, Ws, #{})),
        io:format(
            "PASS: default Garage assembly; real PG presign/PUT/confirm/content, ACL, duplicate/concurrent PUT safety, metadata reservation, missing and invalid config~n"
        )
    after
        intbe02_http_support:stop_direct_edge(Edge)
    end.

%% Governance and deletion must serialize even for a legacy row without EAR.
governance_intent_race(C) ->
    #{<<"organization_id">> := Org, <<"id">> := App} = intbe02_http_support:one(
        C, <<"SELECT organization_id,id FROM enterprise_application ORDER BY id LIMIT 1">>, []
    ),
    Ctx = #{organization_id => Org, application_id => App},
    {Key, Id} = governance_seed(C, Org, App),
    ok = intbe02_http_support:sql_exec(C, <<"BEGIN">>),
    {ok, _, _} = epgsql:equery(C, "SELECT fn_v5_intent_ordinary_delete($1,'governance race')", [Key]),
    {ok, _, [{Locker}]} = epgsql:equery(C, "SELECT pg_backend_pid()", []),
    Parent = self(),
    _ = spawn(fun() ->
        Result = elib_pg:with_tx(fun(Conn) ->
            {ok, _, [{Pid}]} = epgsql:equery(Conn, "SELECT pg_backend_pid()", []),
            Parent ! {governance_pid, Pid},
            enterprise_asset_retention_logic:governance_tx(Conn, Ctx, #{
                object_key => Key, op => <<"hold">>, reason => <<"synthetic race">>
            })
        end),
        Parent ! {governance_result, Result}
    end),
    Waiter =
        receive
            {governance_pid, Pid} -> Pid
        after 10000 -> error(governance_pid_timeout)
        end,
    ok = await_exact_block(C, Waiter, Locker, 100),
    ok = intbe02_http_support:sql_exec(C, <<"COMMIT">>),
    receive
        {governance_result, Result} ->
            ?assertEqual({error, {<<"invalid_request">>, already_deleting}}, Result)
    after 10000 -> error(governance_result_timeout)
    end,
    #{<<"count">> := 0} = intbe02_http_support:one(
        C,
        <<"SELECT count(*) AS count FROM enterprise_attachment_retention WHERE attachment_id=$1">>,
        [Id]
    ),
    #{<<"count">> := 0} = intbe02_http_support:one(
        C,
        <<"SELECT count(*) AS count FROM enterprise_audit_event WHERE resource_type='attachment' AND action='file.governance.op' AND resource_id=$1">>,
        [Id]
    ),
    {HeldKey, HeldId} = governance_seed(C, Org, App),
    ok = intbe02_http_support:sql_exec(C, <<"BEGIN">>),
    {ok, _} = enterprise_asset_retention_logic:governance_tx(C, Ctx, #{
        object_key => HeldKey, op => <<"hold">>, reason => <<"synthetic hold first">>
    }),
    {ok, _, [{HoldLocker}]} = epgsql:equery(C, "SELECT pg_backend_pid()", []),
    _ = spawn(fun() ->
        Result2 = elib_pg:with_tx(
            fun(Conn) ->
                {ok, _, [{Pid2}]} = epgsql:equery(Conn, "SELECT pg_backend_pid()", []),
                Parent ! {intent_pid, Pid2},
                epgsql:equery(Conn, "SELECT fn_v5_intent_ordinary_delete($1,'hold wins')", [HeldKey])
            end,
            [{reraise, false}]
        ),
        Parent ! {intent_result, Result2}
    end),
    IntentWaiter =
        receive
            {intent_pid, Pid3} -> Pid3
        after 10000 -> error(intent_pid_timeout)
        end,
    ok = await_exact_block(C, IntentWaiter, HoldLocker, 100),
    ok = intbe02_http_support:sql_exec(C, <<"COMMIT">>),
    receive
        {intent_result, IntentResult} ->
            ?assertMatch(
                {error, #error{code = <<"23510">>, message = <<"v5 intent blocked by EAR">>}},
                IntentResult
            )
    after 10000 -> error(intent_result_timeout)
    end,
    #{<<"hold_state">> := <<"held">>} = intbe02_http_support:one(
        C,
        <<"SELECT hold_state FROM enterprise_attachment_retention WHERE attachment_id=$1">>,
        [HeldId]
    ),
    #{<<"count">> := 1} = intbe02_http_support:one(
        C,
        <<"SELECT count(*) AS count FROM enterprise_audit_event WHERE resource_type='attachment' AND action='file.governance.op' AND resource_id=$1">>,
        [HeldId]
    ),
    io:format("PASS: real governance hold and ordinary delete intent serialize in both orders~n").

governance_seed(C, Org, App) ->
    Id = cs_pg_test_fixture:id(),
    Key = iolist_to_binary([
        "eoa/",
        integer_to_binary(Org),
        "/",
        integer_to_binary(App),
        "/synthetic-governance-race/",
        integer_to_binary(Id)
    ]),
    Scope = iolist_to_binary(["org:", integer_to_binary(Org), "/app:", integer_to_binary(App)]),
    ok = intbe02_http_support:sql_exec(
        C,
        <<
            "INSERT INTO attachment(id,file_hash256,mime_type,ext,name,path,url,size,info,referer_time,"
            "last_referer_user_id,last_referer_at,creator_user_id,updated_at,created_at,status,scope,scope_ref,"
            "cipher,legacy_key,anchor_msg_id,anchor_conv_seq,group_file_id,storage_driver,storage_namespace,"
            "storage_container,storage_locator_sha256,lifecycle_state,gc_verified,storage_physical_key)"
            " SELECT $1,file_hash256,mime_type,ext,name,$2,url,size,info,0,last_referer_user_id,"
            "last_referer_at,creator_user_id,now(),created_at,1,'enterprise',$3,cipher,legacy_key,"
            "NULL,NULL,NULL,storage_driver,storage_namespace,storage_container,storage_locator_sha256,"
            "'live',true,$2 FROM attachment ORDER BY id LIMIT 1"
        >>,
        [Id, Key, Scope]
    ),
    {Key, Id}.

%% Confirmed objects survive failed business binding; only the reference and
%% audit must roll back together.
direct_bind_atomic_checks(C, Gid, Uid) ->
    Bytes = <<"synthetic group direct atomic audit">>,
    Name = <<"atomic-audit.txt">>,
    Key = confirmed_group_object(Gid, Uid, Name, Bytes),
    Constraint = <<"reject_direct_file_upload_test">>,
    ok = intbe02_http_support:sql_exec(C, <<
        "ALTER TABLE enterprise_audit_event ADD CONSTRAINT ",
        Constraint/binary,
        " CHECK (action <> 'file.uploaded') NOT VALID"
    >>),
    try
        ?assertMatch(
            {error, {audit_failed, _}},
            group_file_ds:bind_file(Gid, Uid, Name, <<"text/plain">>, Key, byte_size(Bytes), <<>>)
        ),
        #{<<"group_file_id">> := null, <<"lifecycle_state">> := <<"live">>} = intbe02_http_support:one(
            C, <<"SELECT group_file_id,lifecycle_state FROM attachment WHERE path=$1">>, [Key]
        ),
        #{<<"n">> := 0} = intbe02_http_support:one(
            C, <<"SELECT count(*) AS n FROM group_file WHERE file_url=$1">>, [Key]
        )
    after
        ok = intbe02_http_support:sql_exec(
            C, <<"ALTER TABLE enterprise_audit_event DROP CONSTRAINT ", Constraint/binary>>
        )
    end,
    {ok, Ref} = group_file_ds:bind_file(
        Gid, Uid, Name, <<"text/plain">>, Key, byte_size(Bytes), <<>>
    ),
    ?assertEqual(
        {ok, Ref},
        group_file_ds:bind_file(Gid, Uid, Name, <<"text/plain">>, Key, byte_size(Bytes), <<>>)
    ),
    #{<<"n">> := 1} = intbe02_http_support:one(
        C, <<"SELECT count(*) AS n FROM group_file WHERE file_url=$1 AND status=1">>, [Key]
    ),
    #{<<"n">> := 1} = intbe02_http_support:one(
        C,
        <<"SELECT count(*) AS n FROM enterprise_audit_event WHERE action='file.uploaded' AND resource_id=(SELECT group_file_id FROM attachment WHERE path=$1)">>,
        [Key]
    ),
    direct_delete_audit_checks(C, Key, Uid),
    io:format("PASS: direct group bind audit rollback, idempotency, and delete audit rollback~n").

direct_delete_audit_checks(C, Key, Uid) ->
    #{<<"group_file_id">> := FileId} = intbe02_http_support:one(
        C, <<"SELECT group_file_id FROM attachment WHERE path=$1">>, [Key]
    ),
    Constraint = <<"reject_direct_file_delete_test">>,
    ok = intbe02_http_support:sql_exec(C, <<
        "ALTER TABLE enterprise_audit_event ADD CONSTRAINT ",
        Constraint/binary,
        " CHECK (action <> 'file.deleted') NOT VALID"
    >>),
    try
        ?assertMatch({error, {audit_failed, _}}, group_file_ds:delete_file(FileId, Uid)),
        #{<<"status">> := 1} = intbe02_http_support:one(
            C, <<"SELECT status FROM group_file WHERE id=$1">>, [FileId]
        ),
        #{<<"n">> := 0} = intbe02_http_support:one(
            C,
            <<"SELECT count(*) AS n FROM enterprise_audit_event WHERE action='file.deleted' AND resource_id=$1">>,
            [FileId]
        )
    after
        ok = intbe02_http_support:sql_exec(
            C, <<"ALTER TABLE enterprise_audit_event DROP CONSTRAINT ", Constraint/binary>>
        )
    end,
    ?assertEqual(ok, group_file_ds:delete_file(FileId, Uid)),
    ?assertEqual({error, not_found}, group_file_ds:delete_file(FileId, Uid)),
    #{<<"n">> := 1} = intbe02_http_support:one(
        C,
        <<"SELECT count(*) AS n FROM enterprise_audit_event WHERE action='file.deleted' AND resource_id=$1">>,
        [FileId]
    ).

confirmed_group_object(Gid, Uid, Name, Bytes) ->
    confirmed_object(<<"group">>, Gid, Uid, Name, Bytes).

confirmed_object(Scope, Ref, Uid, Name, Bytes) ->
    Sha = binary:encode_hex(crypto:hash(sha256, Bytes), lowercase),
    {ok, #{object_key := Key}} = attach_direct_logic:register(
        Uid, Scope, integer_to_binary(Ref), elib_uuid:gen_v7(), Name
    ),
    ok = attach_direct_logic:declare(Key, Sha, byte_size(Bytes), <<"text/plain">>),
    {ok, #{grant := #{url := Url, required_headers := Hs}, attempt_id := Attempt}} =
        attach_direct_logic:grant(Key, Sha),
    Headers = [{binary_to_list(K), binary_to_list(V)} || {K, V} <- Hs],
    {ok, {{_, Status, _}, _, _}} = httpc:request(
        put,
        {binary_to_list(Url), Headers, "text/plain", Bytes},
        [{timeout, 30000}, {autoredirect, false}],
        [{body_format, binary}]
    ),
    ?assert(Status >= 200 andalso Status < 300),
    {ok, #{state := <<"CONFIRMED">>}} = attach_direct_logic:finalize(Key, Attempt),
    Key.

direct_channel_reference_checks(C) ->
    Channel = 995901,
    Uid = 995011,
    ok = intbe02_http_support:sql_exec(
        C,
        <<"INSERT INTO channel(id,name,creator_uid,scope,workspace_id) VALUES($1,'direct-channel-fixture',$2,'workspace',$3)">>,
        [Channel, Uid, 995201]
    ),
    lists:foreach(
        fun(Shape) -> channel_shape_check(C, Channel, Uid, Shape) end,
        [root, thumb, images]
    ),
    channel_delete_race(C, Channel, Uid),
    io:format(
        "PASS: channel root, thumbnail and image references protected; binding/delete race safe~n"
    ).

channel_payload(root, Key) -> #{<<"uri">> => Key};
channel_payload(thumb, Key) -> #{<<"thumb">> => #{<<"uri">> => Key}};
channel_payload(images, Key) -> #{<<"images">> => [#{<<"uri">> => Key}]}.

channel_publish(Channel, Uid, Payload) ->
    channel_ds:publish_message(Channel, Uid, <<"fixture">>, <<"image">>, Payload).

channel_shape_check(C, Channel, Uid, Shape) ->
    Bytes = iolist_to_binary(io_lib:format("channel-shape-~p", [Shape])),
    Key = confirmed_object(<<"channel">>, Channel, Uid, <<"channel.txt">>, Bytes),
    Payload = channel_payload(Shape, Key),
    ?assertMatch({error, _}, channel_publish(Channel, 995012, Payload)),
    {ok, MessageId} = channel_publish(Channel, Uid, Payload),
    #{<<"blocked">> := true} = intbe02_http_support:one(
        C,
        <<"SELECT fn_v5_channel_reference_blocks($1) AS blocked">>,
        [Key]
    ),
    ?assertEqual(
        {ok, false},
        elib_pg:with_tx(fun(Conn) ->
            attachment_repo:refresh_gc_verified_tx(Conn, Key)
        end)
    ),
    ?assertMatch({ok, 1}, channel_message_ds:delete(MessageId)),
    #{<<"blocked">> := false} = intbe02_http_support:one(
        C,
        <<"SELECT fn_v5_channel_reference_blocks($1) AS blocked">>,
        [Key]
    ).

channel_delete_race(C, Channel, Uid) ->
    BoundKey = confirmed_object(<<"channel">>, Channel, Uid, <<"channel-bound.txt">>, <<"bound">>),
    ok = channel_delete_eligible(C, BoundKey),
    {ok, _} = channel_publish(Channel, Uid, channel_payload(thumb, BoundKey)),
    ok = intbe02_http_support:sql_exec(
        C,
        <<"UPDATE attachment SET gc_verified=true WHERE path=$1">>,
        [BoundKey]
    ),
    ?assertMatch(
        {error, #error{
            code = <<"23510">>, message = <<"v5 intent blocked by channel message reference">>
        }},
        epgsql:equery(C, "SELECT fn_v5_intent_ordinary_delete($1,'bind wins')", [BoundKey])
    ),
    Key = confirmed_object(<<"channel">>, Channel, Uid, <<"channel-race.txt">>, <<"race">>),
    ok = channel_delete_eligible(C, Key),
    Parent = self(),
    ok = meck:new(elib_pg, [passthrough, no_link]),
    try
        ok = meck:expect(elib_pg, with_deadlock_retry_tx, fun(F) ->
            case get(channel_bind_race) of
                true ->
                    meck:passthrough([
                        fun(Conn) ->
                            {ok, _, [{Pid}]} = epgsql:equery(Conn, "SELECT pg_backend_pid()", []),
                            Parent ! {channel_bind_pid, Pid},
                            receive
                                channel_continue -> ok
                            after 10000 -> error(channel_start_timeout)
                            end,
                            F(Conn)
                        end
                    ]);
                _ ->
                    meck:passthrough([F])
            end
        end),
        Binder = spawn(fun() ->
            put(channel_bind_race, true),
            Parent !
                {channel_bind_result, channel_publish(Channel, Uid, channel_payload(root, Key))}
        end),
        BindPid =
            receive
                {channel_bind_pid, P} -> P
            after 15000 -> error(channel_pid_timeout)
            end,
        {ok, _, [{LockerPid}]} = epgsql:equery(C, "SELECT pg_backend_pid()", []),
        ok = intbe02_http_support:sql_exec(C, <<"BEGIN">>),
        try
            {ok, _, _} = epgsql:equery(
                C,
                "SELECT pg_advisory_xact_lock(hashtextextended($1,168))",
                [Key]
            ),
            Binder ! channel_continue,
            ok = await_channel_object_lock(C, BindPid, LockerPid, Key, 100),
            ok = intbe02_http_support:sql_exec(C, <<"COMMIT">>),
            receive
                {channel_bind_result, Result} -> ?assertMatch({ok, _}, Result)
            after 15000 -> error(channel_result_timeout)
            end
        after
            catch intbe02_http_support:sql_exec(C, <<"ROLLBACK">>)
        end
    after
        meck:unload(elib_pg)
    end,
    DeletedKey = confirmed_object(
        <<"channel">>, Channel, Uid, <<"channel-deleted.txt">>, <<"deleted">>
    ),
    ok = channel_delete_eligible(C, DeletedKey),
    {ok, _, _} = epgsql:equery(C, "SELECT fn_v5_intent_ordinary_delete($1,'delete wins')", [
        DeletedKey
    ]),
    ?assertEqual(
        {error, tombstone}, channel_publish(Channel, Uid, channel_payload(root, DeletedKey))
    ),
    #{<<"lifecycle_state">> := <<"delete_intent">>, <<"ref">> := false} =
        intbe02_http_support:one(
            C,
            <<"SELECT lifecycle_state,fn_v5_channel_reference_blocks(path) AS ref FROM attachment WHERE path=$1">>,
            [DeletedKey]
        ).

await_channel_object_lock(_C, _Waiter, _Locker, _Key, 0) ->
    error(channel_object_lock_timeout);
await_channel_object_lock(C, Waiter, Locker, Key, N) ->
    Sql =
        "SELECT EXISTS (SELECT 1 FROM pg_locks w JOIN pg_locks h ON "
        "h.locktype=w.locktype AND h.database=w.database AND h.classid=w.classid "
        "AND h.objid=w.objid AND h.objsubid=w.objsubid "
        "WHERE w.pid=$1 AND h.pid=$2 AND w.locktype='advisory' AND NOT w.granted AND h.granted "
        "AND w.classid::bigint=((hashtextextended($3::text,168) >> 32) & 4294967295) "
        "AND w.objid::bigint=(hashtextextended($3::text,168) & 4294967295))",
    case epgsql:equery(C, Sql, [Waiter, Locker, Key]) of
        {ok, _, [{true}]} ->
            ok;
        _ ->
            timer:sleep(50),
            await_channel_object_lock(C, Waiter, Locker, Key, N - 1)
    end.

channel_delete_eligible(C, Key) ->
    ok = intbe02_http_support:sql_exec(C, <<"BEGIN">>),
    try
        ok = intbe02_http_support:sql_exec(C, <<"SET LOCAL session_replication_role=replica">>),
        ok = intbe02_http_support:sql_exec(
            C,
            <<"UPDATE attach_pending SET upload_expires_at=now()-interval '1 hour' WHERE object_key=$1">>,
            [Key]
        ),
        ok = intbe02_http_support:sql_exec(
            C,
            <<"DELETE FROM workspace_attachment_retention WHERE attachment_id=(SELECT id FROM attachment WHERE path=$1)">>,
            [Key]
        ),
        ok = intbe02_http_support:sql_exec(
            C, <<"UPDATE attachment SET gc_verified=true WHERE path=$1">>, [Key]
        ),
        ok = intbe02_http_support:sql_exec(C, <<"COMMIT">>)
    catch
        Class:Reason:Stack ->
            catch intbe02_http_support:sql_exec(C, <<"ROLLBACK">>),
            erlang:raise(Class, Reason, Stack)
    end.

%% Capture the actual transaction connection after the public prechecks, so
%% pg_blocking_pids cannot accidentally match an unrelated pooled connection.
direct_bind_revocation_race(C, Gid, Uid) ->
    Bytes = <<"synthetic direct binding revocation race">>,
    Name = <<"revoke-race.txt">>,
    Key = confirmed_group_object(Gid, Uid, Name, Bytes),
    Parent = self(),
    ok = meck:new(elib_pg, [passthrough, no_link]),
    try
        ok = meck:expect(elib_pg, with_deadlock_retry_tx, fun(F) ->
            case get(direct_bind_race) of
                true ->
                    meck:passthrough([
                        fun(Conn) ->
                            {ok, _, [{Pid}]} = epgsql:equery(Conn, "SELECT pg_backend_pid()", []),
                            Parent ! {bind_backend, Pid},
                            receive
                                continue_bind -> ok
                            after 10000 -> error(bind_start_timeout)
                            end,
                            F(Conn)
                        end
                    ]);
                _ ->
                    meck:passthrough([F])
            end
        end),
        Binder = spawn(fun() ->
            put(direct_bind_race, true),
            Parent !
                {bind_result,
                    group_file_ds:bind_file(
                        Gid, Uid, Name, <<"text/plain">>, Key, byte_size(Bytes), <<>>
                    )}
        end),
        BindPid =
            receive
                {bind_backend, P} -> P
            after 15000 -> error(bind_backend_timeout)
            end,
        {ok, _, [{LockerPid}]} = epgsql:equery(C, "SELECT pg_backend_pid()", []),
        ok = intbe02_http_support:sql_exec(C, <<"BEGIN">>),
        try
            {ok, _, [_]} = epgsql:equery(
                C,
                "SELECT user_id FROM workspace_member WHERE workspace_id=$1 AND user_id=$2 FOR UPDATE",
                [995201, Uid]
            ),
            Binder ! continue_bind,
            ok = await_exact_block(C, BindPid, LockerPid, 100),
            ok = intbe02_http_support:sql_exec(
                C,
                <<"UPDATE group_member SET status=0 WHERE group_id=$1 AND user_id=$2">>,
                [Gid, Uid]
            ),
            ok = intbe02_http_support:sql_exec(
                C,
                <<"UPDATE workspace_member SET status='removed' WHERE workspace_id=$1 AND user_id=$2">>,
                [995201, Uid]
            ),
            ok = intbe02_http_support:sql_exec(C, <<"COMMIT">>),
            receive
                {bind_result, Result} -> ?assertEqual({error, forbidden}, Result)
            after 15000 -> error(bind_result_timeout)
            end,
            #{<<"n">> := 0} = intbe02_http_support:one(
                C,
                <<"SELECT count(*) AS n FROM group_file WHERE file_url=$1">>,
                [Key]
            ),
            #{<<"group_file_id">> := null, <<"lifecycle_state">> := <<"live">>} =
                intbe02_http_support:one(
                    C,
                    <<"SELECT group_file_id,lifecycle_state FROM attachment WHERE path=$1">>,
                    [Key]
                )
        after
            catch intbe02_http_support:sql_exec(C, <<"ROLLBACK">>),
            intbe02_http_support:sql_exec(
                C,
                <<"UPDATE workspace_member SET status='active' WHERE workspace_id=$1 AND user_id=$2">>,
                [995201, Uid]
            ),
            intbe02_http_support:sql_exec(
                C,
                <<"UPDATE group_member SET status=1 WHERE group_id=$1 AND user_id=$2">>,
                [Gid, Uid]
            )
        end
    after
        meck:unload(elib_pg)
    end.

await_exact_block(_C, _Waiter, _Locker, 0) ->
    error(bind_lock_timeout);
await_exact_block(C, Waiter, Locker, N) ->
    case epgsql:equery(C, "SELECT $2::int = ANY(pg_blocking_pids($1::int))", [Waiter, Locker]) of
        {ok, _, [{true}]} ->
            ok;
        _ ->
            timer:sleep(50),
            await_exact_block(C, Waiter, Locker, N - 1)
    end.

human_group_journey(#{conn := C, port := Port}) ->
    application:set_env(imboy, jwt_key, <<"synthetic-download-gateway-secret-only">>),
    application:set_env(
        imboy, base_url, iolist_to_binary(["http://127.0.0.1:", integer_to_list(Port)])
    ),
    human_group_journey_with_edge(C).

human_group_journey_with_edge(C) ->
    %% setup_all seeds member rows directly; seed their normal open history generation.
    ok = intbe02_http_support:sql_exec(
        C,
        <<"INSERT INTO group_member_generation(group_id,user_id,generation_no,start_seq) VALUES ($1,$2,1,1)">>,
        [995301, 995011]
    ),
    Bytes = <<"synthetic enterprise human group file">>,
    {FileKey, ObjectKey} = intbe02_http_support:direct_group_file(
        <<"995301">>, 995011, <<"synthetic-human.txt">>, Bytes
    ),
    #{<<"id">> := FileId} = group_file_repo:find_by_file_id(FileKey),
    #{<<"path">> := ObjectKey} = intbe02_http_support:one(
        C,
        <<"SELECT path FROM attachment WHERE group_file_id=$1 AND scope='group' AND scope_ref='995301' AND status=1">>,
        [FileId]
    ),
    {ok, Url} = group_file_logic:download(FileId, 995011),
    {ok, {{_, 200, _}, _, Bytes}} = gateway_get(Url, []),
    ?assertEqual({error, not_member}, group_file_logic:download(FileId, 995021)),
    check_gateway_range(Url, Bytes),
    {ok, _} = organization_member_logic:suspend(995001, 995101, 995011),
    {ok, {{_, 403, _}, _, _}} = gateway_get(Url, []),
    assert_revoked_group_file(FileId, ObjectKey, Bytes),
    {ok, _} = organization_member_logic:restore(995001, 995101, 995011),
    {ok, {{_, 200, _}, _, Bytes}} = gateway_get(Url, []),
    ok = group_file_logic:delete(FileId, 995011),
    ?assertEqual({error, forbidden}, attach_logic:view_url(995011, ObjectKey)),
    {ok, {{_, 403, _}, _, _}} = gateway_get(Url, []),
    io:format(
        "PASS: human enterprise group upload and real signed-byte read; cross-tenant and suspended list/search/category/download/upload/delete denied; restore and cleanup~n"
    ).

gateway_get(Url, Headers) ->
    case
        httpc:request(
            get,
            {binary_to_list(Url), Headers},
            [{autoredirect, false}, {timeout, 10000}],
            [{body_format, binary}]
        )
    of
        {ok, {{_, 302, _}, Hs, _}} ->
            EdgeUrl = proplists:get_value("location", Hs),
            httpc:request(get, {EdgeUrl, Headers}, [{autoredirect, false}, {timeout, 10000}], [
                {body_format, binary}
            ]);
        Other ->
            Other
    end.

check_gateway_range(Url, Bytes) ->
    #{path := <<"/api/v1/attachment/content">>} = uri_string:parse(Url),
    {ok, {{_, 302, _}, RedirectHeaders, _}} = httpc:request(
        head, {binary_to_list(Url), []}, [{autoredirect, false}], []
    ),
    {ok, {{_, 200, _}, HeadHeaders, _}} = httpc:request(
        head, {proplists:get_value("location", RedirectHeaders), []}, [{autoredirect, false}], []
    ),
    ?assertEqual(
        integer_to_list(byte_size(Bytes)), proplists:get_value("content-length", HeadHeaders)
    ),
    {ok, {{_, 206, _}, Headers, Part}} = gateway_get(Url, [{"range", "bytes=0-8"}]),
    ?assertEqual(binary:part(Bytes, 0, 9), Part),
    ?assertEqual("private, no-store", proplists:get_value("cache-control", Headers)),
    {ok, {{_, 416, _}, _, _}} = gateway_get(Url, [{"range", "bytes=0-1,3-4"}]),
    Base = config_ds:env(base_url, <<>>),
    {ok, {{_, 403, _}, _, _}} = gateway_get(
        <<Base/binary, "/api/v1/attachment/content?ticket=malformed">>, []
    ),
    {ok, {{_, 403, _}, _, _}} = gateway_get(
        <<Base/binary, "/api/v1/attachment/content">>, []
    ).

assert_revoked_group_file(FileId, ObjectKey, _Bytes) ->
    ?assertEqual({error, forbidden}, attach_logic:view_url(995011, ObjectKey)),
    ?assertEqual({error, not_member}, group_file_logic:download(FileId, 995011)),
    ?assertEqual({error, not_member}, group_file_logic:list(<<"995301">>, 995011, 1, 10)),
    ?assertEqual(
        {error, not_member}, group_file_logic:search(<<"995301">>, <<"synthetic">>, 1, 10, 995011)
    ),
    ?assertEqual({error, not_member}, group_file_logic:get_categories(<<"995301">>, 995011)),
    ?assertEqual({error, not_member}, group_file_logic:delete(FileId, 995011)),
    ?assertEqual(
        {error, not_member},
        group_file_ds:bind_file(
            995301, 995011, <<"denied.txt">>, <<"text/plain">>, ObjectKey, 1, <<>>
        )
    ).

journey(S) ->
    Org = maps:get(org_id, S),
    Ws = maps:get(workspace_id, S),
    Bytes = <<"synthetic enterprise asset in real Garage">>,
    Base = #{
        workspace_id => Ws,
        conversation_id => maps:get(conversation_id, S),
        actor_user_id => maps:get(actor_user_id, S),
        key_ref => cs_pg_test_fixture:key_ref()
    },
    {ok, Presign} = enterprise_business_facade:request_presign(Org, Base#{
        mime => <<"text/plain">>,
        size_bytes => byte_size(Bytes),
        object_hash => eb_asset_content:sha256_hex(Bytes)
    }),
    Ref = maps:get(upload_ref, Presign),
    Id = maps:get(asset_id, Presign),
    ?assertEqual(<<"private_object_store">>, maps:get(adapter, maps:get(upload, Presign))),
    concurrent_put(Org, Base#{upload_ref => Ref, payload => Bytes}),
    ?assertMatch(
        {error, _},
        enterprise_business_facade:put_object(
            Org, Base#{upload_ref => Ref, payload => Bytes}
        )
    ),
    {ok, {content_stream, Bytes}} = eb_asset_store:stream_content(Org, Ws, Id),
    {ok, _} = enterprise_business_facade:confirm_asset(Org, Base#{upload_ref => Ref}),
    {ok, #{redirect_url := ContentUrl}} = enterprise_business_facade:content_stream(
        Org, Base#{asset_id => Id}
    ),
    {ok, {{_, 200, _}, _, Bytes}} = httpc:request(
        get, {binary_to_list(ContentUrl), []}, [{autoredirect, false}], [{body_format, binary}]
    ),
    ?assertEqual(
        {error, not_found},
        enterprise_business_facade:content_stream(
            maps:get(other_org_id, S), Base#{
                workspace_id => maps:get(other_workspace_id, S), asset_id => Id
            }
        )
    ),
    ?assertMatch(
        {error, _},
        enterprise_business_facade:content_stream(
            Org, Base#{actor_user_id => maps:get(peer_user_id, S), asset_id => Id}
        )
    ),
    {ok, Row} = eb_asset_store:fetch_asset(Org, Ws, Id),
    ?assertEqual(active, maps:get(status, Row)),
    ok = eb_asset_store:delete_private(Org, Ws, Id).

rollback_object(S) ->
    Org = maps:get(org_id, S),
    Ws = maps:get(workspace_id, S),
    Id = cs_pg_test_fixture:id(),
    ?assertEqual(
        {error, invalid_asset_descriptor},
        put_fixture(
            Org, Ws, #{id => Id, payload => <<"synthetic rollback">>, mime => <<"text/plain">>}
        )
    ),
    ?assertEqual(
        {error, not_found},
        eb_asset_object_garage:get(
            eb_asset_store:scope_key(Org, Ws, Id), eb_pg_asset_meta:key_prefix(Org, Ws)
        )
    ).

env(Name) -> list_to_binary(os:getenv(Name)).

env_or(Name, Default) ->
    case os:getenv(Name) of
        false -> Default;
        Value -> list_to_binary(Value)
    end.

stub_regression() ->
    Previous = eb_pg_test_fixture:select_asset_stub(),
    Registry = application:get_env(imboy, attachment_storage_registry),
    try
        ok = eunit:test(eb_asset_store_tests:cases({ok, synthetic}), [verbose])
    after
        eb_asset_object_stub:reset(),
        eb_pg_test_fixture:restore_asset_store(Previous),
        case Registry of
            {ok, R} -> application:set_env(imboy, attachment_storage_registry, R);
            undefined -> application:unset_env(imboy, attachment_storage_registry)
        end
    end,
    ?assertEqual(undefined, application:get_env(imboy, eb_asset_object_store)).

concurrent_put(Org, Params) ->
    Parent = self(),
    Workers = [
        spawn(fun() ->
            receive
                start -> Parent ! {self(), enterprise_business_facade:put_object(Org, Params)}
            end
        end)
     || _ <- [1, 2]
    ],
    [Pid ! start || Pid <- Workers],
    Results = [
        receive
            {Pid, Result} -> Result
        after 10000 -> error(upload_timeout)
        end
     || Pid <- Workers
    ],
    ?assertEqual(1, length([ok || {ok, _} <- Results])),
    ?assertEqual(1, length([error || {error, _} <- Results])).

%% Storage failure must leave a retryable pending asset, not a hidden orphan.
cleanup_recovery(_S, _Config) ->
    S = cs_pg_test_fixture:new_scope(),
    Org = maps:get(org_id, S),
    Ws = maps:get(workspace_id, S),
    {Id, Bytes} = expired_pinned(S),
    Params = #{workspace_id => Ws, asset_ids => [Id]},
    {ok, #{deleted := [Id]}} = eb_asset_app:cleanup_pending(Org, Params),
    ?assertEqual(<<"ready">>, queue_state(Org, Ws, Id)),
    ok = meck:new(attachment_storage, [passthrough, no_link]),
    try
        ok = meck:expect(attachment_storage, delete_snapshot, fun(_) ->
            {error, synthetic_failure}
        end),
        ?assertMatch(
            {error, {object_delete_failed, {error, synthetic_failure}}},
            eb_pg_asset_delete_queue:drain(Org, Ws, 10)
        )
    after
        meck:unload(attachment_storage)
    end,
    ?assertEqual(<<"retry_wait">>, queue_state(Org, Ws, Id)),
    purge_queue_rollback_guard(),
    Key = eb_pg_asset_meta:object_key(Org, Ws, Id),
    {ok, #{bytes := Bytes}} = eb_asset_object_garage:get(Key, eb_pg_asset_meta:key_prefix(Org, Ws)),
    ok = wait_queue_retry(Org, Ws, Id, 45),
    {ok, []} = eb_pg_asset_delete_queue:drain(Org, Ws, 10),
    ?assertEqual(<<"done">>, queue_state(Org, Ws, Id)),
    ?assertEqual(
        {error, not_found}, eb_asset_object_garage:get(Key, eb_pg_asset_meta:key_prefix(Org, Ws))
    ).

cleanup_concurrency(_PriorScope) ->
    S = cs_pg_test_fixture:new_scope(),
    Org = maps:get(org_id, S),
    Ws = maps:get(workspace_id, S),
    {Id, Bytes} = expired_pending(Org, Ws),
    Parent = self(),
    Worker = elib_pg:with_tx(fun(Conn) ->
        {ok, [_]} = elib_pg:query(
            Conn,
            <<"SELECT id FROM workspace WHERE organization_id=$1 AND id=$2 FOR UPDATE">>,
            [Org, Ws]
        ),
        Pid = cleanup_worker(Parent, Org, Ws, Id),
        ok = await_cleanup_lock(Conn, 100),
        {ok, [_]} = elib_pg:query(
            Conn,
            <<"UPDATE enterprise_asset SET status='active' WHERE organization_id=$1 AND workspace_id=$2 AND id=$3 AND status='pending_confirm' RETURNING id">>,
            [Org, Ws, Id]
        ),
        Pid
    end),
    assert_cleanup_skipped(Worker),
    {ok, {content_stream, Bytes}} = eb_asset_store:stream_content(Org, Ws, Id),
    {HeldId, HeldBytes} = expired_pending(Org, Ws),
    HoldId = cs_pg_test_fixture:id(),
    HeldWorker = elib_pg:with_tx(fun(Conn) ->
        {ok, [_]} = elib_pg:query(
            Conn,
            <<"INSERT INTO enterprise_retention_hold(id,organization_id,workspace_id,scope_type,reason_code) VALUES($3,$1,$2,'workspace','synthetic-cleanup-hold') RETURNING id">>,
            [Org, Ws, HoldId]
        ),
        Pid = cleanup_worker(Parent, Org, Ws, HeldId),
        ok = await_cleanup_lock(Conn, 100),
        Pid
    end),
    assert_cleanup_skipped(HeldWorker),
    {ok, {content_stream, HeldBytes}} = eb_asset_store:stream_content(Org, Ws, HeldId),
    ok = cs_pg_test_fixture:exec(
        <<"UPDATE enterprise_retention_hold SET released_at=now(),released_by_user_id=$4 WHERE organization_id=$1 AND workspace_id=$2 AND id=$3">>,
        [Org, Ws, HoldId, maps:get(owner_user_id, S)]
    ),
    {ok, #{deleted := [HeldId]}} = eb_asset_app:cleanup_pending(Org, #{
        workspace_id => Ws, asset_ids => [HeldId]
    }),
    io:format(
        "PASS: real cleanup workspace lock observed; concurrent confirmation and new hold prevent object deletion; released hold permits cleanup~n"
    ).

expired_pending(Org, Ws) ->
    Id = cs_pg_test_fixture:id(),
    Bytes = <<"synthetic concurrent pending cleanup">>,
    {ok, _} = put_fixture(Org, Ws, #{
        id => Id, payload => Bytes, object_hash => eb_asset_content:sha256_hex(Bytes)
    }),
    ok = cs_pg_test_fixture:exec(
        <<"UPDATE enterprise_asset SET created_at=now()-interval '2 hours' WHERE organization_id=$1 AND workspace_id=$2 AND id=$3">>,
        [Org, Ws, Id]
    ),
    {Id, Bytes}.

expired_pinned(S) ->
    Org = maps:get(org_id, S),
    Ws = maps:get(workspace_id, S),
    Bytes = <<"synthetic pinned pending cleanup">>,
    ok = cs_pg_test_fixture:exec(
        <<"UPDATE organization_business_identity_assignment SET business_identity_id=$3,function_key='customer_service' WHERE organization_id=$1 AND user_id=$2">>,
        [Org, maps:get(actor_user_id, S), maps:get(service_identity_id, S)]
    ),
    Base = #{
        workspace_id => Ws,
        conversation_id => maps:get(conversation_id, S),
        actor_user_id => maps:get(actor_user_id, S),
        key_ref => cs_pg_test_fixture:key_ref()
    },
    {ok, Presign} = enterprise_business_facade:request_presign(Org, Base#{
        mime => <<"text/plain">>,
        size_bytes => byte_size(Bytes),
        object_hash => eb_asset_content:sha256_hex(Bytes)
    }),
    Id = maps:get(asset_id, Presign),
    {ok, _} = enterprise_business_facade:put_object(Org, Base#{
        upload_ref => maps:get(upload_ref, Presign), payload => Bytes
    }),
    ok = cs_pg_test_fixture:exec(
        <<"UPDATE enterprise_asset SET created_at=now()-interval '2 hours' WHERE organization_id=$1 AND workspace_id=$2 AND id=$3">>,
        [Org, Ws, Id]
    ),
    {Id, Bytes}.

cleanup_worker(Parent, Org, Ws, Id) ->
    spawn(fun() ->
        Parent !
            {self(), eb_asset_app:cleanup_pending(Org, #{workspace_id => Ws, asset_ids => [Id]})}
    end).

await_cleanup_lock(Conn, Attempts) ->
    {ok, _} = elib_pg:query(Conn, <<"SELECT pg_stat_clear_snapshot()">>, []),
    {ok, [#{<<"n">> := N}]} = elib_pg:query(
        Conn,
        <<"SELECT count(*)::integer AS n FROM pg_stat_activity WHERE datname=current_database() AND wait_event_type='Lock' AND query LIKE '%SELECT id FROM workspace%'">>,
        []
    ),
    case N of
        1 -> ok;
        _ when Attempts > 0 -> receive
            after 10 -> await_cleanup_lock(Conn, Attempts - 1)
            end;
        _ -> error(cleanup_did_not_lock_workspace)
    end.

assert_cleanup_skipped(Pid) ->
    receive
        {Pid, {ok, #{deleted := []}}} -> ok;
        {Pid, Other} -> error({cleanup_should_skip, Other})
    after 10000 -> error(cleanup_worker_timeout)
    end.

cleanup_ineligible(S) ->
    Org = maps:get(org_id, S),
    Ws = maps:get(workspace_id, S),
    {Retained, RetainedBytes} = expired_pending(Org, Ws),
    ok = cs_pg_test_fixture:exec(
        <<"UPDATE enterprise_asset SET retain_until=now()+interval '1 day' WHERE organization_id=$1 AND workspace_id=$2 AND id=$3">>,
        [Org, Ws, Retained]
    ),
    {Legacy, LegacyBytes} = expired_pending(Org, Ws),
    ok = eb_asset_store:cleanup_asset(Org, Ws, Legacy),
    {ok, #{deleted := []}} = eb_asset_app:cleanup_pending(Org, #{
        workspace_id => Ws, asset_ids => [Retained, Legacy]
    }),
    [
        begin
            {ok, Row} = eb_asset_store:fetch_asset(Org, Ws, Id),
            ?assertEqual(false, maps:get(pending_object_delete, Row)),
            {ok, #{bytes := Bytes}} = eb_asset_object_garage:get(
                maps:get(object_key, Row), eb_pg_asset_meta:key_prefix(Org, Ws)
            )
        end
     || {Id, Bytes} <- [{Retained, RetainedBytes}, {Legacy, LegacyBytes}]
    ],
    migration_roundtrip(),
    io:format(
        "PASS: future retention and unmarked legacy tombstones keep their real objects; pending intent blocks rollback; empty-intent down/up succeeds~n"
    ).

migration_roundtrip() ->
    {ok, Down} = file:read_file("priv/migrations/00000162_enterprise_asset_cleanup_retry.down.sql"),
    {ok, Up} = file:read_file("priv/migrations/00000162_enterprise_asset_cleanup_retry.up.sql"),
    ok = elib_pg:with_tx(fun(Conn) ->
        [
            begin
                ?assert(element(1, R) =:= ok)
            end
         || R <- epgsql:squery(Conn, Down)
        ],
        [
            begin
                ?assert(element(1, R) =:= ok)
            end
         || R <- epgsql:squery(Conn, Up)
        ],
        ok
    end).

%% Instrument only the scheduling boundary; every read/write/delete stays native.
purge_regressions() ->
    ok = eb_pg_test_fixture:ensure_purge_role(),
    purge_confirm_before_claim(),
    purge_confirm_after_commit(),
    purge_shared_live_reference(),
    hold_after_claim_guard(),
    hold_before_intent_guard(),
    purge_scoped_frozen_physical_key(),
    purge_native_message(),
    purge_failure_recovery(),
    purge_legacy_regression(),
    purge_queue_rollback_guard(),
    io:format(
        "PASS: purge confirmation interleavings; committed queue retries native Garage failure; audit failure leaves metadata and bytes intact~n"
    ).

purge_confirm_before_claim() ->
    S = cs_pg_test_fixture:new_scope(),
    Org = maps:get(org_id, S),
    Ws = maps:get(workspace_id, S),
    {Id, Bytes} = expired_pinned(S),
    Parent = self(),
    Worker = elib_pg:with_tx(fun(Conn) ->
        {ok, [_]} = elib_pg:query(
            Conn,
            <<"SELECT id FROM workspace WHERE organization_id=$1 AND id=$2 FOR UPDATE">>,
            [Org, Ws]
        ),
        Pid = spawn(fun() -> Parent ! {self(), purge_native(Org, Ws)} end),
        ok = await_cleanup_lock(Conn, 100),
        {ok, #{status := active}} = eb_asset_store:confirm_asset(Org, Ws, Id),
        Pid
    end),
    {ok, #{orphan_assets_deleted := 0}} = await_purge(Worker),
    {ok, {content_stream, Bytes}} = eb_asset_store:stream_content(Org, Ws, Id).

purge_confirm_after_commit() ->
    S = cs_pg_test_fixture:new_scope(),
    Org = maps:get(org_id, S),
    Ws = maps:get(workspace_id, S),
    {Id, Bytes} = expired_pinned(S),
    Parent = self(),
    Key = eb_pg_asset_meta:object_key(Org, Ws, Id),
    ok = meck:new(attachment_storage, [passthrough, no_link]),
    try
        ok = meck:expect(attachment_storage, delete_snapshot, fun(Snapshot) ->
            Parent ! {purge_before_object_delete, self()},
            receive
                continue_purge -> meck:passthrough([Snapshot])
            after 10000 -> error(purge_boundary_timeout)
            end
        end),
        Worker = spawn(fun() -> Parent ! {self(), purge_native(Org, Ws)} end),
        receive
            {purge_before_object_delete, Worker} -> ok;
            {Worker, EarlyResult} -> error({purge_stopped_before_object_delete, EarlyResult})
        after 10000 -> error(purge_did_not_reach_object_delete)
        end,
        ?assertEqual({error, not_found}, eb_asset_store:confirm_asset(Org, Ws, Id)),
        ?assertEqual(1, queue_count(Org, Ws)),
        ?assertEqual(
            {error, cleanup_pending},
            put_fixture(Org, Ws, #{
                id => Id, payload => Bytes, object_hash => eb_asset_content:sha256_hex(Bytes)
            })
        ),
        Worker ! continue_purge,
        {ok, #{orphan_assets_deleted := 1}} = await_purge(Worker),
        ?assertEqual(
            {error, not_found},
            eb_asset_object_garage:get(Key, eb_pg_asset_meta:key_prefix(Org, Ws))
        ),
        ?assertEqual(1, queue_count(Org, Ws)),
        ?assertEqual(<<"done">>, queue_state(Org, Ws, Id))
    after
        meck:unload(attachment_storage)
    end.

scoped_rollback_rejects_physical_key() ->
    {ok, Down} = file:read_file("priv/migrations/00000269_enterprise_scoped_physical_key.down.sql"),
    ok = elib_pg:with_tx(fun(Conn) ->
        {ok, [], []} = epgsql:squery(Conn, "SAVEPOINT scoped_rollback"),
        Result = epgsql:squery(Conn, Down),
        Results =
            case Result of
                L when is_list(L) -> L;
                Other -> [Other]
            end,
        ?assert(
            lists:any(
                fun
                    ({error, #error{code = <<"23514">>}}) -> true;
                    (_) -> false
                end,
                Results
            )
        ),
        {ok, [], []} = epgsql:squery(Conn, "ROLLBACK TO SAVEPOINT scoped_rollback"),
        {ok, [#{<<"physical">> := true, <<"worker">> := true, <<"public">> := false}]} =
            elib_pg:query(
                Conn,
                <<
                    "SELECT 'out_storage_physical_key'=ANY(p.proargnames) AS physical,"
                    " has_function_privilege('v5_worker',p.oid,'EXECUTE') AS worker,"
                    " EXISTS(SELECT 1 FROM aclexplode(coalesce(p.proacl,acldefault('f',p.proowner))) e"
                    " WHERE e.grantee=0 AND e.privilege_type='EXECUTE') AS public"
                    " FROM pg_proc p WHERE p.oid='public.fn_v5_claim_eb(uuid,bigint,bigint)'::regprocedure"
                >>,
                []
            ),
        ok
    end).

purge_shared_live_reference() ->
    S = cs_pg_test_fixture:new_scope(),
    Org = maps:get(org_id, S),
    Ws = maps:get(workspace_id, S),
    {Id, Bytes} = expired_pinned(S),
    Key = eb_pg_asset_meta:object_key(Org, Ws, Id),
    Aid = cs_pg_test_fixture:id(),
    Alias = <<Key/binary, ".ordinary-alias">>,
    ok = cs_pg_test_fixture:exec(
        <<
            "INSERT INTO attachment(id,file_hash256,mime_type,ext,name,path,url,size,info,referer_time,"
            "last_referer_user_id,last_referer_at,creator_user_id,updated_at,created_at,status,scope,scope_ref,"
            "cipher,legacy_key,anchor_msg_id,anchor_conv_seq,group_file_id,storage_driver,storage_namespace,"
            "storage_container,storage_locator_sha256,lifecycle_state,gc_verified,storage_physical_key)"
            " SELECT $1,a.file_hash256,a.mime_type,a.ext,a.name,$2,a.url,a.size,a.info,1,"
            "a.last_referer_user_id,a.last_referer_at,a.creator_user_id,now(),a.created_at,1,a.scope,a.scope_ref,"
            "a.cipher,a.legacy_key,NULL,NULL,NULL,e.storage_driver,e.storage_namespace,e.storage_container,"
            "e.storage_locator_sha256,'live',false,$3 FROM attachment a,enterprise_asset e"
            " WHERE e.organization_id=$4 AND e.workspace_id=$5 AND e.id=$6 LIMIT 1"
        >>,
        [Aid, Alias, Key, Org, Ws, Id]
    ),
    {ok, _} = purge_native(Org, Ws),
    ?assertEqual(<<"ready">>, queue_state(Org, Ws, Id)),
    {ok, []} = elib_pg:query(
        <<"SELECT out_asset_id FROM fn_v5_claim_eb($1,$2,$3)">>,
        [elib_uuid:gen_v7(), Org, Ws]
    ),
    ?assertEqual(<<"ready">>, queue_state(Org, Ws, Id)),
    ?assertMatch(
        {ok, #{bytes := Bytes}},
        eb_asset_object_garage:get(Key, eb_pg_asset_meta:key_prefix(Org, Ws))
    ),
    %% Reproduce an inconsistent historical held/deleting row in the isolated fixture.
    ok = elib_pg:with_tx(fun(C) ->
        {ok, _} = elib_pg:query(
            C, <<"SELECT set_config('session_replication_role','replica',true)">>, []
        ),
        {ok, _} = elib_pg:execute(
            C, <<"DELETE FROM workspace_attachment_retention WHERE attachment_id=$1">>, [Aid]
        ),
        {ok, 1} = elib_pg:execute(
            C, <<"UPDATE attachment SET lifecycle_state='delete_intent' WHERE id=$1">>, [Aid]
        ),
        {ok, 1} = elib_pg:execute(
            C,
            <<
                "INSERT INTO enterprise_attachment_retention(attachment_id,organization_id,application_id,retention_until,"
                "hold_state,hold_reason,hold_set_at,purge_state,purged_at) "
                "VALUES($1,$2,$3,clock_timestamp()-interval '1 day','held','synthetic',clock_timestamp(),'purged',clock_timestamp())"
            >>,
            [Aid, Org, Org]
        ),
        {ok, _} = elib_pg:query(
            C, <<"SELECT set_config('session_replication_role','origin',true)">>, []
        ),
        ok
    end),
    {ok, [#{<<"blocked">> := false}]} = elib_pg:query(
        <<"SELECT fn_talk_record_war_blocks($1) AS blocked">>, [Aid]
    ),
    {ok, []} = elib_pg:query(<<"SELECT out_asset_id FROM fn_v5_claim_eb($1,$2,$3)">>, [
        elib_uuid:gen_v7(), Org, Ws
    ]),
    ?assertEqual(<<"ready">>, queue_state(Org, Ws, Id)),
    ?assertMatch(
        {ok, #{bytes := Bytes}},
        eb_asset_object_garage:get(Key, eb_pg_asset_meta:key_prefix(Org, Ws))
    ),
    %% Separately isolate legacy workspace retention after clearing the synthetic hold.
    ok = elib_pg:with_tx(fun(C) ->
        {ok, _} = elib_pg:query(
            C, <<"SELECT set_config('session_replication_role','replica',true)">>, []
        ),
        {ok, 1} = elib_pg:execute(
            C,
            <<"UPDATE enterprise_attachment_retention SET hold_state='none',hold_reason=NULL,hold_set_at=NULL WHERE attachment_id=$1">>,
            [Aid]
        ),
        {ok, 1} = elib_pg:execute(
            C,
            <<"INSERT INTO workspace_attachment_retention(attachment_id,organization_id,workspace_id,scope,scope_ref) VALUES($1,$2,$3,'group','synthetic')">>,
            [Aid, Org, Ws]
        ),
        {ok, _} = elib_pg:query(
            C, <<"SELECT set_config('session_replication_role','origin',true)">>, []
        ),
        ok
    end),
    {ok, [#{<<"blocked">> := true}]} = elib_pg:query(
        <<"SELECT fn_talk_record_war_blocks($1) AS blocked">>, [Aid]
    ),
    {ok, []} = elib_pg:query(<<"SELECT out_asset_id FROM fn_v5_claim_eb($1,$2,$3)">>, [
        elib_uuid:gen_v7(), Org, Ws
    ]),
    ?assertEqual(<<"ready">>, queue_state(Org, Ws, Id)),
    ?assertMatch(
        {ok, #{bytes := Bytes}},
        eb_asset_object_garage:get(Key, eb_pg_asset_meta:key_prefix(Org, Ws))
    ),
    io:format(
        "PASS: shared live, historical held and workspace retention protect enterprise object~n"
    ).

%% Delaying the actual DELETE lets the production hold INSERT race with a leased job.
hold_after_claim_guard() ->
    lists:foreach(fun hold_after_claim_scope/1, [workspace, conversation, message]),
    io:format("PASS: leased delete rejects new scoped holds before real Garage DELETE~n").

hold_after_claim_scope(Scope) ->
    S = cs_pg_test_fixture:new_scope(),
    Org = maps:get(org_id, S),
    Ws = maps:get(workspace_id, S),
    {Id, _Bytes} = expired_pinned(S),
    Conv = maps:get(conversation_id, S),
    Until = eb_system_clock:now() - 120,
    Msg = eb_retention_pg_tests:insert_message(S, <<"synthetic-hold-after-claim">>, Until),
    Key = eb_pg_asset_meta:object_key(Org, Ws, Id),
    ok = cs_pg_test_fixture:exec(
        <<"UPDATE enterprise_asset SET status='active',conversation_id=$4,message_id=$5,retain_until=to_timestamp($6::bigint) WHERE organization_id=$1 AND workspace_id=$2 AND id=$3">>,
        [Org, Ws, Id, Conv, Msg, Until]
    ),
    Hold = #{
        id => cs_pg_test_fixture:id(),
        scope_type => atom_to_binary(Scope),
        scope_conversation_id =>
            case Scope of
                conversation -> Conv;
                _ -> undefined
            end,
        scope_message_id =>
            case Scope of
                message -> Msg;
                _ -> undefined
            end,
        reason_code => <<"synthetic">>
    },
    ok = meck:new(attachment_storage, [passthrough, no_link]),
    try
        ok = meck:expect(attachment_storage, delete_snapshot, fun(I) ->
            ?assertEqual(<<"leased">>, queue_state(Org, Ws, Id)),
            ?assertMatch(
                {error, {sql, <<"23510">>, undefined}}, eb_pg_store:insert_hold(Org, Ws, Hold)
            ),
            {ok, [#{<<"n">> := 0}]} = elib_pg:query(
                <<"SELECT count(*) AS n FROM enterprise_retention_hold WHERE id=$1">>, [
                    maps:get(id, Hold)
                ]
            ),
            meck:passthrough([I])
        end),
        {ok, _} = purge_native(Org, Ws)
    after
        meck:unload(attachment_storage)
    end,
    ?assertEqual(<<"done">>, queue_state(Org, Ws, Id)),
    ?assertEqual(
        {error, not_found}, eb_asset_object_garage:get(Key, eb_pg_asset_meta:key_prefix(Org, Ws))
    ).

hold_before_intent_guard() ->
    lists:foreach(fun hold_before_intent_scope/1, [workspace, conversation, message]),
    io:format("PASS: existing scoped holds block intent and preserve Garage object~n").

hold_before_intent_scope(Scope) ->
    S = cs_pg_test_fixture:new_scope(),
    Org = maps:get(org_id, S),
    Ws = maps:get(workspace_id, S),
    Conv = maps:get(conversation_id, S),
    Until = eb_system_clock:now() - 120,
    Msg = eb_retention_pg_tests:insert_message(S, <<"synthetic-hold-first">>, Until),
    {Id, Bytes} = expired_pinned(S),
    Key = eb_pg_asset_meta:object_key(Org, Ws, Id),
    ok = cs_pg_test_fixture:exec(
        <<"UPDATE enterprise_asset SET status='active',conversation_id=$4,message_id=$5,retain_until=to_timestamp($6::bigint) WHERE organization_id=$1 AND workspace_id=$2 AND id=$3">>,
        [Org, Ws, Id, Conv, Msg, Until]
    ),
    Hold = #{
        id => cs_pg_test_fixture:id(),
        scope_type => atom_to_binary(Scope),
        scope_conversation_id =>
            case Scope of
                conversation -> Conv;
                _ -> undefined
            end,
        scope_message_id =>
            case Scope of
                message -> Msg;
                _ -> undefined
            end,
        reason_code => <<"synthetic">>
    },
    {ok, _} = eb_pg_store:insert_hold(Org, Ws, Hold),
    {ok, _} = purge_native(Org, Ws),
    {ok, [#{<<"n">> := 0}]} = elib_pg:query(
        <<"SELECT count(*) AS n FROM enterprise_asset_delete_queue WHERE organization_id=$1 AND workspace_id=$2 AND asset_id=$3">>,
        [Org, Ws, Id]
    ),
    ?assertMatch(
        {ok, #{bytes := Bytes}},
        eb_asset_object_garage:get(Key, eb_pg_asset_meta:key_prefix(Org, Ws))
    ).

purge_scoped_frozen_physical_key() ->
    S = cs_pg_test_fixture:new_scope(),
    Org = maps:get(org_id, S),
    Ws = maps:get(workspace_id, S),
    {Id, Original} = expired_pinned(S),
    Key = eb_pg_asset_meta:object_key(Org, Ws, Id),
    PhysicalKey = <<Key/binary, ".physical">>,
    Payload = <<"scoped physical identity fixture">>,
    ok = eb_asset_object_garage:put(PhysicalKey, Payload, #{}),
    ok = cs_pg_test_fixture:exec(
        <<"UPDATE enterprise_asset SET storage_physical_key=$4 WHERE organization_id=$1 AND workspace_id=$2 AND id=$3">>,
        [Org, Ws, Id, PhysicalKey]
    ),
    {ok, #{orphan_assets_deleted := 1}} = purge_native(Org, Ws),
    Prefix = eb_pg_asset_meta:key_prefix(Org, Ws),
    ?assertEqual({error, not_found}, eb_asset_object_garage:get(PhysicalKey, Prefix)),
    {ok, #{bytes := Original}} = eb_asset_object_garage:get(Key, Prefix),
    ?assertEqual(<<"done">>, queue_state(Org, Ws, Id)),
    scoped_rollback_rejects_physical_key(),
    io:format(
        "PASS: scoped enterprise purge deletes frozen physical key, preserves logical-key object~n"
    ).

purge_failure_recovery() ->
    S = cs_pg_test_fixture:new_scope(),
    Org = maps:get(org_id, S),
    Ws = maps:get(workspace_id, S),
    {Id, Bytes} = expired_pinned(S),
    ok = meck:new(attachment_storage, [passthrough, no_link]),
    try
        ok = meck:expect(attachment_storage, delete_snapshot, fun(_) ->
            {error, synthetic_failure}
        end),
        ?assertMatch(
            {error, {object_delete_failed, {error, synthetic_failure}}}, purge_native(Org, Ws)
        )
    after
        meck:unload(attachment_storage)
    end,
    ?assertEqual({error, not_found}, eb_asset_store:fetch_asset(Org, Ws, Id)),
    ?assertEqual(<<"retry_wait">>, queue_state(Org, Ws, Id)),
    purge_queue_rollback_guard(),
    Key = eb_pg_asset_meta:object_key(Org, Ws, Id),
    {ok, #{bytes := Bytes}} = eb_asset_object_garage:get(Key, eb_pg_asset_meta:key_prefix(Org, Ws)),
    ok = wait_queue_retry(Org, Ws, Id, 45),
    {ok, #{orphan_assets_deleted := 0, object_delete_failures := []}} = purge_native(Org, Ws),
    ?assertEqual(<<"done">>, queue_state(Org, Ws, Id)),
    ?assertEqual(
        {error, not_found}, eb_asset_object_garage:get(Key, eb_pg_asset_meta:key_prefix(Org, Ws))
    ),
    AuditScope = cs_pg_test_fixture:new_scope(),
    purge_audit_failure(AuditScope).

queue_state(Org, Ws, Id) ->
    {ok, [#{<<"job_state">> := State}]} = elib_pg:query(
        <<"SELECT job_state FROM enterprise_asset_delete_queue WHERE organization_id=$1 AND workspace_id=$2 AND asset_id=$3">>,
        [Org, Ws, Id]
    ),
    State.

wait_queue_retry(_Org, _Ws, _Id, 0) ->
    error(queue_retry_timeout);
wait_queue_retry(Org, Ws, Id, Left) ->
    {ok, [#{<<"due">> := Due}]} = elib_pg:query(
        <<"SELECT next_retry_at <= clock_timestamp() AS due FROM enterprise_asset_delete_queue WHERE organization_id=$1 AND workspace_id=$2 AND asset_id=$3">>,
        [Org, Ws, Id]
    ),
    case Due of
        true ->
            ok;
        false ->
            timer:sleep(1000),
            wait_queue_retry(Org, Ws, Id, Left - 1)
    end.

purge_audit_failure(S) ->
    Org = maps:get(org_id, S),
    Ws = maps:get(workspace_id, S),
    Before = queue_count(Org, Ws),
    {Id, Bytes} = expired_pinned(S),
    ok = meck:new(eb_pg_audit, [passthrough, no_link]),
    try
        ok = meck:expect(eb_pg_audit, append_in, fun(_, _, _) ->
            {error, synthetic_audit_failure}
        end),
        ?assertEqual({error, {audit_failed, synthetic_audit_failure}}, purge_native(Org, Ws)),
        {ok, #{status := pending_confirm}} = eb_asset_store:fetch_asset(Org, Ws, Id),
        {ok, {content_stream, Bytes}} = eb_asset_store:stream_content(Org, Ws, Id),
        ?assertEqual(Before, queue_count(Org, Ws))
    after
        meck:unload(eb_pg_audit)
    end.

purge_native(Org, Ws) ->
    eb_pg_purge:purge_batch(Org, Ws, #{
        now => eb_system_clock:now(),
        batch_limit => 10,
        orphan_asset_age_seconds => 3600
    }).

await_purge(Worker) ->
    receive
        {Worker, Result} -> Result
    after 10000 -> error(purge_completion_timeout)
    end.

queue_count(Org, Ws) ->
    {ok, [#{<<"n">> := N}]} = elib_pg:query(
        <<"SELECT count(*)::integer AS n FROM enterprise_asset_delete_queue WHERE organization_id=$1 AND workspace_id=$2">>,
        [Org, Ws]
    ),
    N.

purge_legacy_regression() ->
    Previous = eb_pg_test_fixture:select_asset_stub(),
    try
        ok = eunit:test(eb_retention_pg_tests:cases({ok, undefined}), [verbose]),
        ok = eunit:test(eb_purge_orphan_asset_pg_tests:cases({ok, undefined}), [verbose])
    after
        eb_pg_test_fixture:restore_asset_store(Previous)
    end.

purge_native_message() ->
    S = cs_pg_test_fixture:new_scope(),
    Org = maps:get(org_id, S),
    Ws = maps:get(workspace_id, S),
    Until = eb_system_clock:now() - 120,
    Msg = eb_retention_pg_tests:insert_message(S, <<"synthetic-native-garage-purge">>, Until),
    {Id, _} = expired_pinned(S),
    ok = cs_pg_test_fixture:exec(
        <<"UPDATE enterprise_asset SET status='active',conversation_id=$4,message_id=$5,retain_until=to_timestamp($6::bigint) WHERE organization_id=$1 AND workspace_id=$2 AND id=$3">>,
        [Org, Ws, Id, maps:get(conversation_id, S), Msg, Until]
    ),
    {ok, #{purged := [Msg], deleted := 1, object_delete_failures := []}} = purge_native(Org, Ws),
    ?assertEqual({error, not_found}, eb_asset_store:fetch_asset(Org, Ws, Id)),
    ?assertEqual(
        {error, not_found},
        eb_asset_object_garage:get(
            eb_pg_asset_meta:object_key(Org, Ws, Id), eb_pg_asset_meta:key_prefix(Org, Ws)
        )
    ),
    ?assertEqual(<<"done">>, queue_state(Org, Ws, Id)).

purge_queue_rollback_guard() ->
    {ok, Down} = file:read_file("priv/migrations/00000163_enterprise_asset_delete_queue.down.sql"),
    [Guard, _] = binary:split(Down, <<"DROP TABLE">>),
    {error, Error} = elib_pg:query(Guard, []),
    ?assertEqual(<<"P0001">>, eb_pg_exec:error_of(Error)).

run_contract_regressions() ->
    ok = eb_ports_tests:ports_contracts_match_declared_callbacks_test(),
    ok = eb_ports_tests:ports_have_explicit_frozen_contracts_test(),
    ok = eb_ports_tests:asset_port_callback_names_cannot_leak_storage_handle_test(),
    ok = eunit:test(eb_message_app_tests:cases({ok, undefined}), [verbose]).

put_fixture(Org, Ws, D) ->
    Now = erlang:system_time(second),
    Ctx = #{
        issued_at => calendar:system_time_to_universal_time(Now, second),
        expires_at => calendar:system_time_to_universal_time(Now + 300, second)
    },
    eb_asset_store:put_private(
        Org, Ws, maps:merge(#{mime => <<"application/octet-stream">>}, D#{signature_context => Ctx})
    ).

%% Synthetic confirmed ledger; retirement, worker claim, hold and S3 I/O are real.
direct_retirement_scope_checks(Conn) ->
    lists:foreach(
        fun(Scope) -> direct_retirement_hold_first(Scope) end,
        [workspace, conversation, message]
    ),
    lists:foreach(
        fun(Scope) -> direct_retirement_delete_first(Conn, Scope) end,
        [workspace, conversation, message]
    ),
    io:format("PASS: enterprise direct retirement ordinary worker scoped hold orders~n").

direct_retirement_fixture() ->
    S = cs_pg_test_fixture:new_scope(),
    Org = maps:get(org_id, S),
    Ws = maps:get(workspace_id, S),
    Conv = maps:get(conversation_id, S),
    Uid = maps:get(actor_user_id, S),
    Msg = eb_retention_pg_tests:insert_message(
        S, <<"direct-retirement-synthetic">>, eb_system_clock:now() - 120
    ),
    {Id, Bytes} = expired_pinned(S),
    Key = eb_pg_asset_meta:object_key(Org, Ws, Id),
    Phys = <<"direct-c/scope-fixture/", (integer_to_binary(Id))/binary>>,
    Source = <<"direct-s/scope-fixture/", (integer_to_binary(Id))/binary>>,
    Aid = cs_pg_test_fixture:id(),
    UUID = elib_uuid:gen_v7(),
    Hash = eb_asset_content:sha256_hex(Bytes),
    ok = eb_asset_object_garage:put(Phys, Bytes, #{}),
    ok = elib_pg:with_tx(fun(C) ->
        {ok, _} = elib_pg:query(C, <<"SET LOCAL session_replication_role=replica">>, []),
        {ok, 1} = elib_pg:execute(
            C,
            <<"UPDATE enterprise_asset SET status='active',conversation_id=$2,message_id=$3,retain_until=(SELECT retain_until FROM enterprise_message WHERE id=$3),upload_registration=coalesce(upload_registration,jsonb_build_object('subject','{}'::jsonb,'declaration','{}'::jsonb)) || jsonb_build_object('protocol','direct-v1') WHERE id=$1">>,
            [Id, Conv, Msg]
        ),
        {ok, 1} = elib_pg:execute(
            C,
            <<"INSERT INTO attachment(id,path,url,scope,creator_user_id,mime_type,file_hash256,size,status,storage_driver,storage_namespace,storage_container,storage_locator_sha256,storage_physical_key,lifecycle_state,gc_verified) SELECT $1,$2,storage_container,'private',$3,mime,object_hash,size_bytes,1,storage_driver,storage_namespace,storage_container,storage_locator_sha256,$4,'live',true FROM enterprise_asset WHERE id=$5">>,
            [Aid, Key, Uid, Phys, Id]
        ),
        {ok, 1} = elib_pg:execute(
            C,
            <<"INSERT INTO attach_pending(object_key,bucket,scope,creator_user_id,session_state,direct_state,confirmed_attachment_id,upload_registration,upload_expires_at,storage_driver,storage_namespace,storage_container,storage_locator_sha256) SELECT $1,storage_container,'private',$2,'confirmed','CONFIRMED',$3,jsonb_build_object('protocol','direct-v1','subject','{}'::jsonb,'declaration','{}'::jsonb),now()-interval '1 hour',storage_driver,storage_namespace,storage_container,storage_locator_sha256 FROM enterprise_asset WHERE id=$4">>,
            [Key, Uid, Aid, Id]
        ),
        {ok, 1} = elib_pg:execute(
            C,
            <<"INSERT INTO attach_direct_attempt(object_key,attempt_no,attempt_state,source_key,target_key,target_driver,target_namespace,target_container,profile_digest,dispatch_cas_uuid,dispatched_at,proof_sha256,proof_size,proof_received_at) SELECT $1,1,'WINNER',$2,$3,storage_driver,storage_namespace,storage_container,storage_locator_sha256,$4,now(),$5,size_bytes,now() FROM enterprise_asset WHERE id=$6">>,
            [Key, Source, Phys, UUID, Hash, Id]
        ),
        {ok, 1} = elib_pg:execute(
            C,
            <<"INSERT INTO enterprise_asset_direct_link(asset_id,object_key,organization_id,workspace_id,conversation_id,subject,binding_state,winner_fence,scan_receipt_sha256,scan_ruleset_digest) SELECT $1,$2,$3,$4,$5,'{}'::jsonb,'bound',jsonb_build_object('attachment_id',$6::bigint,'attempt_no',1,'dispatch_cas_uuid',$7::text,'storage_physical_key',$8::text,'storage_locator_sha256',storage_locator_sha256),repeat('a',64),repeat('b',64) FROM enterprise_asset WHERE id=$1">>,
            [Id, Key, Org, Ws, Conv, Aid, UUID, Phys]
        ),
        {ok, 1} = elib_pg:execute(
            C,
            <<"INSERT INTO enterprise_direct_scan_proof(asset_id,object_key,winner_fence,receipt_sha256,ruleset_sha256,expires_at) SELECT asset_id,object_key,winner_fence,scan_receipt_sha256,scan_ruleset_digest,now()+interval '1 hour' FROM enterprise_asset_direct_link WHERE asset_id=$1">>,
            [Id]
        ),
        {ok, _} = elib_pg:query(C, <<"SET LOCAL session_replication_role=origin">>, []),
        ok
    end),
    {S, Id, Key, Phys, Bytes, Msg}.

direct_scope_hold(S, Msg, Scope) ->
    #{
        id => cs_pg_test_fixture:id(),
        scope_type => atom_to_binary(Scope),
        scope_conversation_id =>
            case Scope of
                conversation -> maps:get(conversation_id, S);
                _ -> undefined
            end,
        scope_message_id =>
            case Scope of
                message -> Msg;
                _ -> undefined
            end,
        reason_code => <<"synthetic-direct">>
    }.

direct_retire(S, Id) ->
    elib_pg:with_tx(fun(C) ->
        {ok, _} = elib_pg:query(C, <<"SELECT set_config('imboy.enterprise_purge','on',true)">>, []),
        case
            elib_pg:query(C, <<"SELECT fn_eb_direct_retire($1,$2,$3)">>, [
                maps:get(org_id, S), maps:get(workspace_id, S), Id
            ])
        of
            {ok, [_]} -> ok;
            {error, R} -> throw({abort_tx, R})
        end
    end).

direct_retirement_hold_first(Scope) ->
    {S, Id, _Key, Phys, Bytes, Msg} = direct_retirement_fixture(),
    {ok, _} = eb_pg_store:insert_hold(
        maps:get(org_id, S), maps:get(workspace_id, S), direct_scope_hold(S, Msg, Scope)
    ),
    {error, RetireError} = direct_retire(S, Id),
    ?assertEqual(<<"23510">>, eb_pg_exec:error_of(RetireError)),
    ?assertMatch(#error{message = <<"enterprise direct retention active">>}, RetireError),
    ?assertMatch({ok, #{bytes := Bytes}}, eb_asset_object_garage:get(Phys, <<"direct-c/">>)).

direct_retirement_delete_first(Conn, Scope) ->
    {S, Id, Key, Phys, _Bytes, Msg} = direct_retirement_fixture(),
    {ok, [#{<<"n">> := 0}]} = elib_pg:query(
        <<"SELECT count(*) AS n FROM attachment_object_deletion WHERE job_state IN ('ready','leased')">>,
        []
    ),
    ok = direct_retire(S, Id),
    ok = elib_pg:with_tx(fun(C) ->
        {ok, _} = elib_pg:query(C, <<"SELECT set_config('imboy.enterprise_purge','on',true)">>, []),
        {ok, 1} = elib_pg:execute(C, <<"DELETE FROM enterprise_asset WHERE id=$1">>, [Id]),
        ok
    end),
    Hold = direct_scope_hold(S, Msg, Scope),
    Org = maps:get(org_id, S),
    Ws = maps:get(workspace_id, S),
    Delete = fun(_Ns, _Container, Path, I) ->
        ?assertEqual(Key, Path),
        {ok, [#{<<"n">> := 1}]} = elib_pg:query(
            <<"SELECT count(*) AS n FROM attachment_object_deletion WHERE path=$1 AND job_state='leased'">>,
            [Key]
        ),
        {ok, [#{<<"n">> := 1}]} = elib_pg:query(
            <<"SELECT count(*) AS n FROM enterprise_direct_retirement_audit WHERE object_key=$1 AND cross_conversation_id=$2 AND cross_message_id=$3">>,
            [Key, maps:get(conversation_id, S), Msg]
        ),
        ?assertMatch(
            {error, {sql, <<"23510">>, undefined}}, eb_pg_store:insert_hold(Org, Ws, Hold)
        ),
        attachment_storage:delete_snapshot(I)
    end,
    ?assertEqual({ok, 1}, attachment_delete_worker:run_batch(Conn, Delete)),
    ?assertEqual({error, not_found}, eb_asset_object_garage:get(Phys, <<"direct-c/">>)).
