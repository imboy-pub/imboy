-module(forward_attachment_target_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

%% This suite runs only against its dedicated 49323 database. Its source winner
%% is synthetic; real HTTP/direct-S3 evidence is a separate requirement.
forward_target_test_() ->
    case os:getenv("C08_FWD_TARGET_DB") of
        false -> [];
        _ -> {timeout, 120, fun run/0}
    end.

run() ->
    Db = os:getenv("C08_FWD_TARGET_DB"),
    true = lists:prefix("c08_fwd_", Db),
    {ok, _} = application:ensure_all_started(epgsql),
    {ok, _} = application:ensure_all_started(pooler),
    application:load(imboy),
    application:set_env(imboy, sql_driver, pgsql),
    Opts = #{
        host => "127.0.0.1",
        port => 49323,
        username => os:getenv("C08_FWD_TARGET_USER"),
        password => os:getenv("C08_FWD_TARGET_PASSWORD"),
        database => Db,
        codecs => [{epgsql_codec_rfc3339_bin, []}]
    },
    {ok, _} = pooler:new_pool(#{
        name => pgsql,
        init_count => 2,
        max_count => 4,
        start_mfa => {epgsql, connect, [Opts]}
    }),
    ok = meck:new(elib_tsid, [passthrough, no_link]),
    TsSeed = 1000000000000000 + binary:decode_unsigned(crypto:strong_rand_bytes(5)) * 1000,
    meck:expect(elib_tsid, generate, fun(_) ->
        TsSeed + erlang:unique_integer([positive, monotonic])
    end),
    try
        Base = 9000000000000 + binary:decode_unsigned(crypto:strong_rand_bytes(5)),
        Uid = Base + 2,
        Sender = Base + 1,
        Peer = Base + 3,
        Gid = Base + 4,
        Source = elib_uuid:gen_v7(),
        Path = <<"synthetic/fwd/", (integer_to_binary(Base))/binary>>,
        Hash = binary:copy(<<"a">>, 64),
        Phys = <<"direct-c/", Path/binary>>,
        Scope =
            <<"c2c:", (integer_to_binary(Sender))/binary, ":", (integer_to_binary(Uid))/binary>>,
        Payload = #{<<"uri">> => Path},
        seed(Base, Uid, Sender, Peer, Gid, Source, Path, Phys, Scope, Hash, Payload),
        c2c_source_role_case(Uid, Sender, Peer, Source, Payload),
        record_failure_rollback_case(Uid, Sender, Peer, Source, Path, Payload),
        C2cTarget = c2c_case(Uid, Sender, Peer, Source, Path, Payload),
        target_edit_liveness_case(Base, Uid, Peer, C2cTarget, Path, Payload, Source),
        {GroupSource, GroupPayload} = c2g_case(Uid, Sender, Peer + 2, Gid, Source, Path, Payload),
        group_edit_liveness_case(Base, Uid, Gid, GroupSource, Path, GroupPayload),
        ReforwardTarget = c2g_source_case(
            Base, Uid, Gid, Peer + 5, GroupSource, Path, GroupPayload
        ),
        target_gc_case(Uid, Peer, Peer + 5, C2cTarget, ReforwardTarget, Path),
        nested_source_payload_case(Uid, Sender, Peer, Source, C2cTarget, Path),
        target_edit_race_case(Base, Uid, Sender, Peer, Source, C2cTarget, Path, Payload),
        target_expiry_lock_case(Base, Uid, Sender, Peer, Source, C2cTarget, Path, Payload),
        source_expiry_target_lock_case(Base, Uid, Sender, Peer, Source, C2cTarget, Path, Payload),
        source_expiry_object_case(Base, Uid, Sender, Peer, Source, C2cTarget, Path, Payload),
        source_expiry_lock_case(Uid, Sender, Source, Payload),
        c2c_source_revoke_case(Uid, Sender, Source, Payload),
        deadlock_retry_case(Base)
    after
        meck:unload(elib_tsid),
        pooler:rm_pool(pgsql)
    end.

seed(Base, Uid, Sender, Peer, Gid, Source, Path, Phys, Scope, Hash, Payload) ->
    Locator = binary:copy(<<"b">>, 64),
    ok = elib_pg:with_tx(fun(C) ->
        {ok, _} = elib_pg:query(C, <<"SET LOCAL session_replication_role=replica">>, []),
        {ok, _} = elib_pg:query(
            C,
            <<"INSERT INTO attachment(id,file_hash256,mime_type,ext,name,path,url,size,info,referer_time,creator_user_id,scope,scope_ref,status,updated_at,created_at,storage_driver,storage_namespace,storage_container,storage_locator_sha256,storage_physical_key) VALUES($1,$2,'image/png','.png','f.png',$3,$3,10,'{}',1,$4,'c2c',$5,1,now(),now(),'garage','fwd','bucket',$6,$7)">>,
            [Base + 10, Hash, Path, Sender, Scope, Locator, Phys]
        ),
        {ok, _} = elib_pg:query(
            C,
            <<"INSERT INTO attach_pending(object_key,bucket,scope,creator_user_id,scope_ref,storage_driver,storage_namespace,storage_container,storage_locator_sha256,session_state,direct_state,confirmed_attachment_id,upload_registration,upload_expires_at,direct_expected) VALUES($1,'bucket','c2c',$2,$3,'garage','fwd','bucket',$4,'confirmed','CONFIRMED',$5,'{\"protocol\":\"direct-v1\",\"subject\":{},\"declaration\":{}}'::jsonb,now()-interval '1 day',jsonb_build_object('version','1','sha256',$6::text,'size_bytes','10','mime','image/png'))">>,
            [Path, Sender, Scope, Locator, Base + 10, Hash]
        ),
        {ok, _} = elib_pg:query(
            C,
            <<"INSERT INTO attach_direct_attempt(object_key,attempt_no,attempt_state,source_key,target_key,target_driver,target_namespace,target_container,profile_digest,dispatch_cas_uuid,dispatched_at,proof_sha256,proof_size,proof_received_at) VALUES($1,1,'WINNER','staging/test',$2,'garage','fwd','bucket',$3,gen_random_uuid(),now(),$4,10,now())">>,
            [Path, Phys, Locator, Hash]
        ),
        {ok, _} = elib_pg:query(
            C,
            <<"INSERT INTO msg_c2c(id,from_id,to_id,msg_id,msg_type,payload,conv_seq) VALUES($1,$2,$3,$4,'image',$5,1)">>,
            [Base + 11, Sender, Uid, Source, jsone:encode(Payload)]
        ),
        {ok, _} = elib_pg:query(
            C,
            <<"INSERT INTO c2c_forward_source_fact(msg_id,sender_id,recipient_id,conv_seq) VALUES($1,$2,$3,1)">>,
            [Source, Sender, Uid]
        ),
        {ok, _} = elib_pg:query(
            C,
            <<"INSERT INTO public.\"group\"(id,owner_uid,creator_uid,scope) VALUES($1,$2,$2,'personal')">>,
            [Gid, Uid]
        ),
        {ok, _} = elib_pg:query(
            C,
            <<"INSERT INTO public.\"group\"(id,owner_uid,creator_uid,scope) VALUES($1,$2,$2,'personal')">>,
            [Base + 40, Uid]
        ),
        lists:foreach(
            fun({Member, Offset}) ->
                {ok, _} = elib_pg:query(
                    C,
                    <<"INSERT INTO group_member(id,group_id,user_id,role,is_join,status) VALUES($1,$2,$3,1,true,1)">>,
                    [Base + Offset, Base + 40, Member]
                ),
                {ok, _} = elib_pg:query(
                    C,
                    <<"INSERT INTO group_member_generation(group_id,user_id,start_seq) VALUES($1,$2,1)">>,
                    [Base + 40, Member]
                )
            end,
            [{Uid, 41}, {Peer + 8, 42}]
        ),
        lists:foreach(
            fun({Member, Offset}) ->
                {ok, _} = elib_pg:query(
                    C,
                    <<"INSERT INTO group_member(id,group_id,user_id,role,is_join,status) VALUES($1,$2,$3,1,true,1)">>,
                    [Base + Offset, Gid, Member]
                ),
                {ok, _} = elib_pg:query(
                    C,
                    <<"INSERT INTO group_member_generation(group_id,user_id,start_seq) VALUES($1,$2,1)">>,
                    [Gid, Member]
                )
            end,
            [{Uid, 20}, {Peer, 21}, {Peer + 2, 22}]
        ),
        ok
    end).

forward_payload(#{<<"msg_type">> := _, <<"payload">> := Inner}, Source, Type) when is_map(Inner) ->
    Inner#{<<"forwarded">> => true, <<"original_msg_id">> => Source, <<"original_type">> => Type};
forward_payload(Payload, Source, Type) ->
    Payload#{<<"forwarded">> => true, <<"original_msg_id">> => Source, <<"original_type">> => Type}.

group_envelope(MsgId, Payload) ->
    #{<<"id">> => MsgId, <<"msg_type">> => <<"image">>, <<"payload">> => Payload}.

ctx(Type, Target, Uid, Sender, Source, Path, Payload) ->
    #{
        source_type => <<"c2c">>,
        source_id => Source,
        source_from => Sender,
        source_target => Uid,
        source_payload => Payload,
        source_msg_type => <<"image">>,
        target_type => Type,
        target_id => Target,
        sender => Uid,
        keys => [Path]
    }.

c2c_source_role_case(Uid, Sender, Outsider, Source, Payload) ->
    ok = elib_pg:with_tx(fun(C) ->
        {ok, _} = elib_pg:query(C, <<"SET LOCAL ROLE v5_app_runtime">>, []),
        Sql =
            <<"SELECT public.fn_forward_source_c2c_validate($1,$2,$3,$4,$5,$6::jsonb) AS allowed">>,
        Args = [Source, Uid, Sender, Uid, <<"image">>, jsone:encode(Payload)],
        {ok, [#{<<"allowed">> := true}]} = elib_pg:query(C, Sql, Args),
        {ok, [#{<<"allowed">> := false}]} = elib_pg:query(
            C,
            Sql,
            [Source, Outsider, Sender, Uid, <<"image">>, jsone:encode(Payload)]
        ),
        {ok, [#{<<"allowed">> := false}]} = elib_pg:query(
            C,
            Sql,
            [Source, Uid, Sender, Uid, <<"image">>, <<"{}">>]
        ),
        ok
    end),
    {ok, [#{<<"worker">> := false, <<"proof">> := false, <<"public">> := false}]} = elib_pg:query(
        <<
            "SELECT has_function_privilege('v5_worker','public.fn_forward_source_c2c_validate(text,bigint,bigint,bigint,text,jsonb)','EXECUTE') AS worker, "
            "has_function_privilege('v5_proof','public.fn_forward_source_c2c_validate(text,bigint,bigint,bigint,text,jsonb)','EXECUTE') AS proof, "
            "EXISTS (SELECT 1 FROM pg_proc p, LATERAL aclexplode(coalesce(p.proacl,acldefault('f',p.proowner))) a "
            "WHERE p.oid='public.fn_forward_source_c2c_validate(text,bigint,bigint,bigint,text,jsonb)'::regprocedure AND a.grantee=0 AND a.privilege_type='EXECUTE') AS public"
        >>,
        []
    ),
    ok.

nested_source_payload_case(Uid, Sender, Peer, Source, Target, Path) ->
    Inner = #{<<"msg_type">> => <<"nested">>, <<"payload">> => #{<<"uri">> => Path}},
    Outer = #{<<"msg_type">> => <<"image">>, <<"payload">> => Inner},
    Forward = Inner#{
        <<"forwarded">> => true, <<"original_msg_id">> => Source, <<"original_type">> => <<"c2c">>
    },
    {rollback, nested_done} = elib_pg:with_tx(fun(C) ->
        {ok, _} = elib_pg:query(C, <<"SET LOCAL session_replication_role=replica">>, []),
        {ok, _} = elib_pg:query(
            C,
            <<"DELETE FROM forward_attachment_reference WHERE target_type='c2c' AND target_msg_id=$1">>,
            [Target]
        ),
        {ok, _} = elib_pg:query(C, <<"UPDATE msg_c2c SET payload=$2 WHERE msg_id=$1">>, [
            Source, jsone:encode(Outer)
        ]),
        {ok, _} = elib_pg:query(
            C,
            <<"UPDATE msg_store_staging SET payload=$2::jsonb WHERE type='c2c' AND msg_id=$1">>,
            [Target, jsone:encode(Forward)]
        ),
        {ok, [#{<<"seq">> := Seq}]} = elib_pg:query(
            C, <<"SELECT conv_seq AS seq FROM msg_store_staging WHERE type='c2c' AND msg_id=$1">>, [
                Target
            ]
        ),
        {ok, _} = elib_pg:query(C, <<"SET LOCAL ROLE v5_app_runtime">>, []),
        {ok, [#{<<"accepted">> := true}]} = elib_pg:query(
            C,
            <<"SELECT public.fn_forward_attachment_bind($1,'c2c',$2,$3,$4,'image',$5::jsonb,'c2c',$6,$4,$7,$8,false) AS accepted">>,
            [Path, Source, Sender, Uid, jsone:encode(Outer), Target, Peer, Seq]
        ),
        throw({rollback, nested_done})
    end),
    ok.

target_expiry_lock_case(Base, Uid, Sender, Peer, Source, Existing, Path, Payload) ->
    Target = elib_uuid:gen_v7(),
    ok = elib_pg:with_tx(fun(C) ->
        {ok, _} = elib_pg:query(C, <<"SET LOCAL session_replication_role=replica">>, []),
        {ok, _} = elib_pg:query(
            C,
            <<"INSERT INTO msg_store_staging SELECT (jsonb_populate_record(NULL::msg_store_staging,to_jsonb(s)||jsonb_build_object('id',$2::bigint,'msg_id',$3::text))).* FROM msg_store_staging s WHERE type='c2c' AND msg_id=$1">>,
            [Existing, Base + 191, Target]
        ),
        {ok, _} = elib_pg:query(
            C,
            <<"UPDATE msg_store_staging SET expire_at=clock_timestamp()+interval '3 seconds' WHERE type='c2c' AND msg_id=$1">>,
            [Target]
        ),
        ok
    end),
    {ok, [#{<<"seq">> := Seq}]} = elib_pg:query(
        <<"SELECT conv_seq AS seq FROM msg_store_staging WHERE type='c2c' AND msg_id=$1">>, [Target]
    ),
    Parent = self(),
    Locker = spawn(fun() ->
        R = elib_pg:with_tx(fun(C) ->
            {ok, _} = elib_pg:query(
                C,
                <<"SELECT msg_id FROM msg_store_staging WHERE type='c2c' AND msg_id=$1 FOR UPDATE">>,
                [Target]
            ),
            Parent ! {target_expiry_locked, self()},
            receive
                release -> ok
            after 10000 -> error(lock_timeout)
            end
        end),
        Parent ! {target_expiry_unlock, self(), R}
    end),
    receive
        {target_expiry_locked, Locker} -> ok
    after 10000 -> error(lock_timeout)
    end,
    Binder = spawn(fun() ->
        R = elib_pg:with_tx(fun(C) ->
            {ok, _} = elib_pg:query(C, <<"SET LOCAL ROLE v5_app_runtime">>, []),
            Parent ! {target_expiry_binding, self()},
            elib_pg:query(
                C,
                <<"SELECT public.fn_forward_attachment_bind($1,'c2c',$2,$3,$4,'image',$5::jsonb,'c2c',$6,$4,$7,$8,false) AS accepted">>,
                [Path, Source, Sender, Uid, jsone:encode(Payload), Target, Peer, Seq]
            )
        end),
        Parent ! {target_expiry_result, self(), R}
    end),
    receive
        {target_expiry_binding, Binder} -> ok
    after 10000 -> error(bind_timeout)
    end,
    wait_named_blocked(<<"%fn_forward_attachment_bind%">>, 100),
    receive
    after 3500 -> ok
    end,
    Locker ! release,
    receive
        {target_expiry_unlock, Locker, ok} -> ok
    after 10000 -> error(unlock_timeout)
    end,
    receive
        {target_expiry_result, Binder, {ok, [#{<<"accepted">> := false}]}} -> ok
    after 10000 -> error(target_expiry_result_timeout)
    end,
    {ok, [#{<<"n">> := 0}]} = elib_pg:query(
        <<"SELECT count(*) AS n FROM forward_attachment_reference WHERE target_msg_id=$1">>, [
            Target
        ]
    ),
    ok.

source_expiry_target_lock_case(Base, Uid, Sender, Peer, Source, Existing, Path, Payload) ->
    Target = elib_uuid:gen_v7(),
    ok = elib_pg:with_tx(fun(C) ->
        {ok, _} = elib_pg:query(C, <<"SET LOCAL session_replication_role=replica">>, []),
        {ok, _} = elib_pg:query(
            C,
            <<"INSERT INTO msg_store_staging SELECT (jsonb_populate_record(NULL::msg_store_staging,to_jsonb(s)||jsonb_build_object('id',$2::bigint,'msg_id',$3::text))).* FROM msg_store_staging s WHERE type='c2c' AND msg_id=$1">>,
            [Existing, Base + 192, Target]
        ),
        {ok, _} = elib_pg:query(
            C,
            <<"UPDATE msg_c2c SET expire_at=clock_timestamp()+interval '3 seconds' WHERE msg_id=$1">>,
            [Source]
        ),
        ok
    end),
    {ok, [#{<<"seq">> := Seq}]} = elib_pg:query(
        <<"SELECT conv_seq AS seq FROM msg_store_staging WHERE type='c2c' AND msg_id=$1">>, [Target]
    ),
    Parent = self(),
    Locker = spawn(fun() ->
        R = elib_pg:with_tx(fun(C) ->
            {ok, _} = elib_pg:query(
                C,
                <<"SELECT msg_id FROM msg_store_staging WHERE type='c2c' AND msg_id=$1 FOR UPDATE">>,
                [Target]
            ),
            Parent ! {source_target_locked, self()},
            receive
                release -> ok
            after 10000 -> error(lock_timeout)
            end
        end),
        Parent ! {source_target_unlocked, self(), R}
    end),
    receive
        {source_target_locked, Locker} -> ok
    after 10000 -> error(lock_timeout)
    end,
    Binder = spawn(fun() ->
        R = elib_pg:with_tx(fun(C) ->
            {ok, _} = elib_pg:query(C, <<"SET LOCAL ROLE v5_app_runtime">>, []),
            Parent ! {source_target_binding, self()},
            elib_pg:query(
                C,
                <<"SELECT public.fn_forward_attachment_bind($1,'c2c',$2,$3,$4,'image',$5::jsonb,'c2c',$6,$4,$7,$8,false) AS accepted">>,
                [Path, Source, Sender, Uid, jsone:encode(Payload), Target, Peer, Seq]
            )
        end),
        Parent ! {source_target_result, self(), R}
    end),
    receive
        {source_target_binding, Binder} -> ok
    after 10000 -> error(bind_timeout)
    end,
    wait_named_blocked(<<"%fn_forward_attachment_bind%">>, 100),
    receive
    after 3500 -> ok
    end,
    Locker ! release,
    receive
        {source_target_unlocked, Locker, ok} -> ok
    after 10000 -> error(unlock_timeout)
    end,
    receive
        {source_target_result, Binder, {ok, [#{<<"accepted">> := false}]}} -> ok
    after 10000 -> error(source_expiry_result_timeout)
    end,
    {ok, [#{<<"n">> := 0}]} = elib_pg:query(
        <<"SELECT count(*) AS n FROM forward_attachment_reference WHERE target_msg_id=$1">>, [
            Target
        ]
    ),
    {ok, _} = elib_pg:query(<<"UPDATE msg_c2c SET expire_at=NULL WHERE msg_id=$1">>, [Source]),
    ok.

target_edit_race_case(Base, Uid, Sender, Peer, Source, Existing, Path, Payload) ->
    Target = elib_uuid:gen_v7(),
    ok = elib_pg:with_tx(fun(C) ->
        {ok, _} = elib_pg:query(C, <<"SET LOCAL session_replication_role=replica">>, []),
        {ok, _} = elib_pg:query(
            C,
            <<"INSERT INTO msg_store_staging SELECT (jsonb_populate_record(NULL::msg_store_staging,to_jsonb(s)||jsonb_build_object('id',$2::bigint,'msg_id',$3::text))).* FROM msg_store_staging s WHERE type='c2c' AND msg_id=$1">>,
            [Existing, Base + 190, Target]
        ),
        ok
    end),
    {ok, [#{<<"seq">> := Seq}]} = elib_pg:query(
        <<"SELECT conv_seq AS seq FROM msg_store_staging WHERE type='c2c' AND msg_id=$1">>, [Target]
    ),
    Parent = self(),
    Locker = spawn(fun() ->
        R = elib_pg:with_tx(fun(C) ->
            {ok, _} = elib_pg:query(
                C, <<"SELECT pg_advisory_xact_lock(hashtextextended($1,168))">>, [Path]
            ),
            Parent ! {edit_locked, self()},
            receive
                release -> ok
            after 10000 -> error(lock_timeout)
            end
        end),
        Parent ! {edit_unlock, self(), R}
    end),
    receive
        {edit_locked, Locker} -> ok
    after 10000 -> error(lock_timeout)
    end,
    Binder = spawn(fun() ->
        R = elib_pg:with_tx(fun(C) ->
            {ok, _} = elib_pg:query(C, <<"SET LOCAL ROLE v5_app_runtime">>, []),
            Parent ! {edit_binding, self()},
            elib_pg:query(
                C,
                <<"SELECT public.fn_forward_attachment_bind($1,'c2c',$2,$3,$4,'image',$5::jsonb,'c2c',$6,$4,$7,$8,false) AS accepted">>,
                [Path, Source, Sender, Uid, jsone:encode(Payload), Target, Peer, Seq]
            )
        end),
        Parent ! {edit_result, self(), R}
    end),
    receive
        {edit_binding, Binder} -> ok
    after 10000 -> error(bind_timeout)
    end,
    wait_named_blocked(<<"%fn_forward_attachment_bind%">>, 100),
    {ok, _} = elib_pg:query(
        <<"UPDATE msg_store_staging SET payload='{}'::jsonb WHERE type='c2c' AND msg_id=$1">>, [
            Target
        ]
    ),
    Locker ! release,
    receive
        {edit_unlock, Locker, ok} -> ok
    after 10000 -> error(unlock_timeout)
    end,
    receive
        {edit_result, Binder, {ok, [#{<<"accepted">> := false}]}} -> ok
    after 10000 -> error(edit_result_timeout)
    end,
    {ok, [#{<<"n">> := 0}]} = elib_pg:query(
        <<"SELECT count(*) AS n FROM forward_attachment_reference WHERE target_msg_id=$1">>, [
            Target
        ]
    ),
    ok.

source_expiry_object_case(Base, Uid, Sender, Peer, Source, Existing, Path, Payload) ->
    Target = elib_uuid:gen_v7(),
    ok = elib_pg:with_tx(fun(C) ->
        {ok, _} = elib_pg:query(C, <<"SET LOCAL session_replication_role=replica">>, []),
        {ok, _} = elib_pg:query(
            C,
            <<"INSERT INTO msg_store_staging SELECT (jsonb_populate_record(NULL::msg_store_staging,to_jsonb(s)||jsonb_build_object('id',$2::bigint,'msg_id',$3::text))).* FROM msg_store_staging s WHERE type='c2c' AND msg_id=$1">>,
            [Existing, Base + 200, Target]
        ),
        ok
    end),
    {ok, [#{<<"seq">> := Seq}]} = elib_pg:query(
        <<"SELECT conv_seq AS seq FROM msg_store_staging WHERE type='c2c' AND msg_id=$1">>, [Target]
    ),
    {ok, _} = elib_pg:query(
        <<"UPDATE msg_c2c SET expire_at=clock_timestamp()+interval '1 second' WHERE msg_id=$1">>, [
            Source
        ]
    ),
    Parent = self(),
    Locker = spawn(fun() ->
        Result = elib_pg:with_tx(fun(C) ->
            {ok, _} = elib_pg:query(
                C, <<"SELECT pg_advisory_xact_lock(hashtextextended($1,168))">>, [Path]
            ),
            Parent ! {object_locked, self()},
            receive
                release -> ok
            after 10000 -> error(lock_timeout)
            end
        end),
        Parent ! {object_unlocked, self(), Result}
    end),
    receive
        {object_locked, Locker} -> ok
    after 10000 -> error(lock_timeout)
    end,
    Reader = spawn(fun() ->
        Parent ! {object_reading, self()},
        Result = elib_pg:with_tx(fun(C) ->
            elib_pg:query(
                C,
                <<"SELECT public.fn_forward_attachment_bind($1,'c2c',$2,$3,$4,'image',$5::jsonb,'c2c',$6,$4,$7,$8,false) AS accepted">>,
                [Path, Source, Sender, Uid, jsone:encode(Payload), Target, Peer, Seq]
            )
        end),
        Parent ! {object_result, self(), Result}
    end),
    receive
        {object_reading, Reader} -> ok
    after 10000 -> error(read_timeout)
    end,
    wait_named_blocked(<<"%fn_forward_attachment_bind%">>, 50),
    receive
    after 1500 -> ok
    end,
    Locker ! release,
    receive
        {object_unlocked, Locker, ok} -> ok
    after 10000 -> error(unlock_timeout)
    end,
    receive
        {object_result, Reader, {ok, [#{<<"accepted">> := false}]}} -> ok
    after 10000 -> error(object_result_timeout)
    end,
    {ok, [#{<<"n">> := 0}]} = elib_pg:query(
        <<"SELECT count(*) AS n FROM forward_attachment_reference WHERE target_msg_id=$1">>, [
            Target
        ]
    ),
    {ok, _} = elib_pg:query(<<"UPDATE msg_c2c SET expire_at=NULL WHERE msg_id=$1">>, [Source]),
    ok.

wait_blocked_by(_, _, 0) ->
    error(expected_blocker_not_observed);
wait_blocked_by(BindPid, LockPid, N) ->
    {ok, [#{<<"blocked">> := Blocked}]} = elib_pg:query(
        <<"SELECT $2::integer = ANY(pg_blocking_pids($1::integer)) AS blocked">>, [BindPid, LockPid]
    ),
    case Blocked of
        true -> ok;
        false -> receive
            after 20 -> wait_blocked_by(BindPid, LockPid, N - 1)
            end
    end.

wait_named_blocked(_, 0) ->
    error(wait_not_observed);
wait_named_blocked(Pattern, N) ->
    {ok, [#{<<"n">> := Count}]} = elib_pg:query(
        <<"SELECT count(*) AS n FROM pg_stat_activity WHERE datname=current_database() AND cardinality(pg_blocking_pids(pid))>0 AND query LIKE $1">>,
        [Pattern]
    ),
    case Count > 0 of
        true -> ok;
        false -> receive
            after 20 -> wait_named_blocked(Pattern, N - 1)
            end
    end.

source_expiry_lock_case(Uid, Sender, Source, Payload) ->
    {ok, _} = elib_pg:query(
        <<"UPDATE msg_c2c SET expire_at=clock_timestamp()+interval '1 second' WHERE msg_id=$1">>, [
            Source
        ]
    ),
    Parent = self(),
    Locker = spawn(fun() ->
        Result = elib_pg:with_tx(fun(Conn) ->
            {ok, _} = elib_pg:query(Conn, <<"SELECT 1 FROM msg_c2c WHERE msg_id=$1 FOR UPDATE">>, [
                Source
            ]),
            Parent ! {expiry_locked, self()},
            receive
                release -> ok
            after 10000 -> error(lock_timeout)
            end
        end),
        Parent ! {expiry_unlocked, self(), Result}
    end),
    receive
        {expiry_locked, Locker} -> ok
    after 10000 -> error(lock_timeout)
    end,
    Reader = spawn(fun() ->
        Parent ! {expiry_reading, self()},
        Result = elib_pg:with_tx(fun(Conn) ->
            elib_pg:query(
                Conn,
                <<"SELECT public.fn_forward_source_c2c_validate($1,$2,$3,$4,'image',$5::jsonb) AS allowed">>,
                [Source, Uid, Sender, Uid, jsone:encode(Payload)]
            )
        end),
        Parent ! {expiry_result, self(), Result}
    end),
    receive
        {expiry_reading, Reader} -> ok
    after 10000 -> error(read_timeout)
    end,
    wait_expiry_blocked(50),
    receive
    after 1500 -> ok
    end,
    Locker ! release,
    receive
        {expiry_unlocked, Locker, ok} -> ok
    after 10000 -> error(unlock_timeout)
    end,
    receive
        {expiry_result, Reader, {ok, [#{<<"allowed">> := false}]}} -> ok
    after 10000 -> error(expiry_result_timeout)
    end,
    {ok, _} = elib_pg:query(<<"UPDATE msg_c2c SET expire_at=NULL WHERE msg_id=$1">>, [Source]),
    ok.

wait_expiry_blocked(0) ->
    error(expiry_wait_not_observed);
wait_expiry_blocked(N) ->
    {ok, [#{<<"n">> := Count}]} = elib_pg:query(
        <<"SELECT count(*) AS n FROM pg_stat_activity WHERE datname=current_database() AND cardinality(pg_blocking_pids(pid))>0 AND query LIKE '%fn_forward_source_c2c_validate%'">>,
        []
    ),
    case Count > 0 of
        true -> ok;
        false -> receive
            after 20 -> wait_expiry_blocked(N - 1)
            end
    end.

c2c_source_revoke_case(Uid, Sender, Source, Payload) ->
    {ok, _} = elib_pg:query(
        <<"INSERT INTO c2c_action_receipt(request_id,original_msg_id,actor_uid,peer_uid,action) VALUES($1,$2,$3,$4,'revoke')">>,
        [elib_uuid:gen_v7(), Source, Sender, Uid]
    ),
    ok = elib_pg:with_tx(fun(C) ->
        {ok, _} = elib_pg:query(C, <<"SET LOCAL ROLE v5_app_runtime">>, []),
        {ok, [#{<<"allowed">> := false}]} = elib_pg:query(
            C,
            <<"SELECT public.fn_forward_source_c2c_validate($1,$2,$3,$4,$5,$6::jsonb) AS allowed">>,
            [Source, Uid, Sender, Uid, <<"image">>, jsone:encode(Payload)]
        ),
        ok
    end).

c2c_case(Uid, Sender, Peer, Source, Path, Payload) ->
    TargetMsg = elib_uuid:gen_v7(),
    Now = elib_dt:to_rfc3339(elib_dt:millisecond(), millisecond, "Z"),
    Ctx = ctx(<<"c2c">>, Peer, Uid, Sender, Source, Path, Payload),
    ?assertEqual(
        {ok, TargetMsg},
        forward_attachment_repo:with_context(Ctx, fun() ->
            case
                msg_store_repo:stage_with_sequence(
                    <<"c2c">>,
                    TargetMsg,
                    <<"image">>,
                    <<>>,
                    null,
                    jsone:encode(forward_payload(Payload, Source, <<"c2c">>)),
                    Uid,
                    Peer,
                    Now,
                    Now,
                    <<>>,
                    Now,
                    null
                )
            of
                {ok, _, _} -> {ok, TargetMsg};
                Error -> Error
            end
        end)
    ),
    {ok, [#{<<"n">> := 1}]} = elib_pg:query(
        <<"SELECT count(*) AS n FROM public.msg_forward WHERE forward_msg_id=$1 AND original_msg_id=$2">>,
        [TargetMsg, Source]
    ),
    c2c_bind_replay_case(Uid, Sender, Peer, Source, TargetMsg, Path, Payload),
    ?assert(forward_attachment_repo:authorize(Path, Peer)),
    ?assertNot(forward_attachment_repo:authorize(Path, Peer + 100)),
    {ok, [#{<<"blocked">> := true}]} = elib_pg:query(
        <<"SELECT fn_v5_forward_reference_blocks($1) AS blocked">>, [Path]
    ),
    TargetMsg.

record_failure_rollback_case(Uid, Sender, Peer, Source, Path, Payload) ->
    TargetMsg = elib_uuid:gen_v7(),
    Now = elib_dt:to_rfc3339(elib_dt:millisecond(), millisecond, "Z"),
    Ctx = ctx(<<"c2c">>, Peer, Uid, Sender, Source, Path, Payload),
    ok = meck:new(msg_forward_repo, [passthrough, no_link]),
    meck:expect(msg_forward_repo, insert_tx, fun(_, _) -> {error, synthetic_record_failure} end),
    try
        ?assertMatch(
            {error, _},
            forward_attachment_repo:with_context(Ctx, fun() ->
                msg_store_repo:stage_with_sequence(
                    <<"c2c">>,
                    TargetMsg,
                    <<"image">>,
                    <<>>,
                    null,
                    jsone:encode(forward_payload(Payload, Source, <<"c2c">>)),
                    Uid,
                    Peer,
                    Now,
                    Now,
                    <<>>,
                    Now,
                    null
                )
            end)
        )
    after
        meck:unload(msg_forward_repo)
    end,
    {ok, [#{<<"n">> := 0}]} = elib_pg:query(
        <<"SELECT count(*) AS n FROM public.msg_store_staging WHERE msg_id=$1">>, [TargetMsg]
    ),
    {ok, [#{<<"n">> := 0}]} = elib_pg:query(
        <<"SELECT count(*) AS n FROM public.forward_attachment_reference WHERE target_msg_id=$1">>,
        [TargetMsg]
    ),
    {ok, [#{<<"n">> := 0}]} = elib_pg:query(
        <<"SELECT count(*) AS n FROM public.msg_forward WHERE forward_msg_id=$1">>, [TargetMsg]
    ).

c2c_bind_replay_case(Uid, Sender, Peer, Source, TargetMsg, Path, Payload) ->
    {ok, [#{<<"seq">> := Seq}]} = elib_pg:query(
        <<"SELECT conv_seq AS seq FROM msg_store_staging WHERE type='c2c' AND msg_id=$1">>, [
            TargetMsg
        ]
    ),
    {ok, [#{<<"can_insert">> := false}]} = elib_pg:query(
        <<"SELECT has_table_privilege('v5_app_runtime','public.forward_attachment_reference','INSERT') AS can_insert">>,
        []
    ),
    ok = elib_pg:with_tx(fun(C) ->
        {ok, _} = elib_pg:query(C, <<"SET LOCAL ROLE v5_app_runtime">>, []),
        Sql =
            <<"SELECT public.fn_forward_attachment_bind($1,'c2c',$2,$3,$4,'image',$5::jsonb,'c2c',$6,$7,$8,$9,false) AS accepted">>,
        Args = [Path, Source, Sender, Uid, jsone:encode(Payload), TargetMsg, Uid, Peer, Seq],
        {ok, [#{<<"accepted">> := false}]} = elib_pg:query(C, Sql, Args),
        {ok, [#{<<"accepted">> := false}]} = elib_pg:query(
            C,
            Sql,
            [Path, Source, Sender, Uid, jsone:encode(Payload), TargetMsg, Peer, Peer, Seq]
        ),
        {ok, [#{<<"accepted">> := false}]} = elib_pg:query(
            C,
            Sql,
            [
                <<"forged/object">>,
                Source,
                Sender,
                Uid,
                jsone:encode(Payload),
                TargetMsg,
                Uid,
                Peer,
                Seq
            ]
        ),
        {ok, [#{<<"accepted">> := false}]} = elib_pg:query(
            C,
            Sql,
            [Path, Source, Sender, Uid, <<"{}">>, TargetMsg, Uid, Peer, Seq]
        ),
        ok
    end).

target_edit_liveness_case(Base, Uid, Peer, Target, Path, Payload, Source) ->
    {rollback, edit_liveness_done} = elib_pg:with_tx(fun(C) ->
        {ok, _} = elib_pg:query(C, <<"SET LOCAL session_replication_role=replica">>, []),
        Sql =
            <<"SELECT public.fn_forward_attachment_authorize($1,$2,false) AS allowed, public.fn_v5_forward_reference_blocks($1) AS blocked">>,
        {ok, [#{<<"allowed">> := true, <<"blocked">> := true}]} = elib_pg:query(C, Sql, [Path, Peer]),
        {ok, _} = elib_pg:query(
            C,
            <<"UPDATE msg_store_staging SET processed_at=now(),error_msg='terminal-fixture' WHERE type='c2c' AND msg_id=$1">>,
            [Target]
        ),
        {ok, [#{<<"allowed">> := true, <<"blocked">> := true}]} = elib_pg:query(C, Sql, [Path, Peer]),
        {ok, _} = elib_pg:query(
            C,
            <<"UPDATE msg_store_staging SET payload='{}'::jsonb WHERE type='c2c' AND msg_id=$1">>,
            [Target]
        ),
        {ok, [#{<<"allowed">> := false, <<"blocked">> := true}]} = elib_pg:query(C, Sql, [
            Path, Peer
        ]),
        {ok, _} = elib_pg:query(
            C,
            <<"UPDATE msg_store_staging SET processed_at=NULL,error_msg=NULL,payload='{}'::jsonb WHERE type='c2c' AND msg_id=$1">>,
            [Target]
        ),
        {ok, [#{<<"allowed">> := false, <<"blocked">> := true}]} = elib_pg:query(C, Sql, [
            Path, Peer
        ]),
        {ok, _} = elib_pg:query(C, <<"SET LOCAL session_replication_role=origin">>, []),
        {ok, _} = elib_pg:query(C, <<"UPDATE attachment SET gc_verified=true WHERE path=$1">>, [
            Path
        ]),
        assert_unknown_gc_block(C, Path),
        {ok, _} = elib_pg:query(C, <<"SET LOCAL session_replication_role=replica">>, []),
        {ok, _} = elib_pg:query(
            C,
            <<"INSERT INTO c2c_action_receipt(request_id,original_msg_id,actor_uid,peer_uid,action,consumed_at,result_payload) VALUES($1,$2,$3,$4,'edit',now(),'{}'::jsonb)">>,
            [elib_uuid:gen_v7(), Target, Uid + 1000, Peer]
        ),
        {ok, _} = elib_pg:query(
            C,
            <<"INSERT INTO c2c_action_receipt(request_id,original_msg_id,actor_uid,peer_uid,action) VALUES($1,$2,$3,$4,'edit')">>,
            [elib_uuid:gen_v7(), Target, Uid, Peer]
        ),
        {ok, _} = elib_pg:query(
            C,
            <<"INSERT INTO c2c_action_receipt(request_id,original_msg_id,actor_uid,peer_uid,action,consumed_at,result_payload) VALUES($1,$2,$3,$4,'edit',now(),'{\"other\":true}'::jsonb)">>,
            [elib_uuid:gen_v7(), Target, Uid, Peer]
        ),
        {ok, [#{<<"allowed">> := false, <<"blocked">> := true}]} = elib_pg:query(C, Sql, [
            Path, Peer
        ]),
        {ok, _} = elib_pg:query(
            C,
            <<"INSERT INTO c2c_action_receipt(request_id,original_msg_id,actor_uid,peer_uid,action,consumed_at,result_payload) VALUES($1,$2,$3,$4,'edit',now(),'{}'::jsonb)">>,
            [elib_uuid:gen_v7(), Target, Uid, Peer]
        ),
        {ok, [#{<<"allowed">> := false, <<"blocked">> := false}]} = elib_pg:query(C, Sql, [
            Path, Peer
        ]),
        Forward = forward_payload(Payload, Source, <<"c2c">>),
        {ok, _} = elib_pg:query(
            C,
            <<"UPDATE msg_store_staging SET payload=$2::jsonb,processed_at=now() WHERE type='c2c' AND msg_id=$1">>,
            [Target, jsone:encode(Forward)]
        ),
        {ok, [#{<<"seq">> := Seq}]} = elib_pg:query(
            C, <<"SELECT conv_seq AS seq FROM msg_store_staging WHERE type='c2c' AND msg_id=$1">>, [
                Target
            ]
        ),
        {ok, _} = elib_pg:query(
            C,
            <<"INSERT INTO msg_c2c(id,from_id,to_id,msg_id,msg_type,payload,conv_seq) VALUES($1,$2,$3,$4,'image',$5,$6)">>,
            [Base + 180, Uid, Peer, Target, jsone:encode(Forward), Seq]
        ),
        {ok, [#{<<"allowed">> := true, <<"blocked">> := true}]} = elib_pg:query(C, Sql, [Path, Peer]),
        {ok, _} = elib_pg:query(C, <<"UPDATE msg_c2c SET payload='{}' WHERE msg_id=$1">>, [Target]),
        {ok, [#{<<"allowed">> := false, <<"blocked">> := false}]} = elib_pg:query(C, Sql, [
            Path, Peer
        ]),
        {ok, _} = elib_pg:query(
            C, <<"UPDATE msg_c2c SET payload='invalid-json' WHERE msg_id=$1">>, [Target]
        ),
        {ok, [#{<<"allowed">> := false, <<"blocked">> := true}]} = elib_pg:query(C, Sql, [
            Path, Peer
        ]),
        {ok, _} = elib_pg:query(C, <<"SET LOCAL session_replication_role=origin">>, []),
        {ok, _} = elib_pg:query(C, <<"UPDATE attachment SET gc_verified=true WHERE path=$1">>, [
            Path
        ]),
        assert_unknown_gc_block(C, Path),
        {ok, _} = elib_pg:query(C, <<"SET LOCAL session_replication_role=replica">>, []),
        {ok, _} = elib_pg:query(C, <<"UPDATE msg_c2c SET payload='null' WHERE msg_id=$1">>, [Target]),
        {ok, [#{<<"allowed">> := false, <<"blocked">> := true}]} = elib_pg:query(C, Sql, [
            Path, Peer
        ]),
        {ok, _} = elib_pg:query(C, <<"SET LOCAL session_replication_role=origin">>, []),
        assert_unknown_gc_block(C, Path),
        throw({rollback, edit_liveness_done})
    end),
    ok.

assert_unknown_gc_block(C, Path) ->
    {ok, _} = elib_pg:query(C, <<"SAVEPOINT malformed_gc">>, []),
    ?assertMatch(
        {error, #error{code = <<"23510">>, message = <<"live forwarded attachment reference">>}},
        elib_pg:query(C, <<"SELECT fn_v5_intent_ordinary_delete($1,'malformed-forward-test')">>, [
            Path
        ])
    ),
    {ok, _} = elib_pg:query(C, <<"ROLLBACK TO SAVEPOINT malformed_gc">>, []),
    ok.

c2g_case(Uid, Sender, Peer, Gid, Source, Path, Payload) ->
    TargetMsg = elib_uuid:gen_v7(),
    Now = elib_dt:to_rfc3339(elib_dt:millisecond(), millisecond, "Z"),
    Ctx = ctx(<<"c2g">>, Gid, Uid, Sender, Source, Path, Payload),
    ?assertNot(forward_attachment_repo:authorize(Path, Peer)),
    ?assertEqual(
        {ok, TargetMsg},
        forward_attachment_repo:with_context(Ctx, fun() ->
            case
                msg_store_repo:stage(
                    <<"c2g">>,
                    TargetMsg,
                    <<"image">>,
                    <<>>,
                    null,
                    jsone:encode(
                        group_envelope(TargetMsg, forward_payload(Payload, Source, <<"c2c">>))
                    ),
                    Uid,
                    Gid,
                    Now,
                    Now,
                    <<>>,
                    1,
                    null
                )
            of
                {ok, _, _, _} -> {ok, TargetMsg};
                Error -> Error
            end
        end)
    ),
    ?assert(forward_attachment_repo:authorize(Path, Peer)),
    ?assertNot(forward_attachment_repo:authorize(Path, Peer + 100)),
    ok = elib_pg:with_tx(fun(C) ->
        {ok, _} = elib_pg:query(C, <<"SET LOCAL ROLE v5_app_runtime">>, []),
        {ok, [#{<<"allowed">> := true}]} = elib_pg:query(
            C,
            <<"SELECT fn_forward_attachment_authorize($1,$2,false) AS allowed">>,
            [Path, Peer]
        ),
        {ok, [#{<<"allowed">> := false}]} = elib_pg:query(
            C,
            <<"SELECT fn_forward_attachment_authorize($1,$2,false) AS allowed">>,
            [Path, Peer + 100]
        ),
        ok
    end),
    {ok, [#{<<"worker">> := false, <<"proof">> := false, <<"public">> := false}]} =
        elib_pg:query(
            <<
                "SELECT has_function_privilege('v5_worker',"
                " 'public.fn_forward_attachment_authorize(text,bigint,boolean)','EXECUTE') AS worker,"
                " has_function_privilege('v5_proof',"
                " 'public.fn_forward_attachment_authorize(text,bigint,boolean)','EXECUTE') AS proof,"
                " EXISTS (SELECT 1 FROM pg_proc p, LATERAL aclexplode(coalesce(p.proacl, acldefault('f',p.proowner))) a"
                " WHERE p.oid='public.fn_forward_attachment_authorize(text,bigint,boolean)'::regprocedure"
                " AND a.grantee=0 AND a.privilege_type='EXECUTE') AS public"
            >>,
            []
        ),
    {ok, _} = elib_pg:query(
        <<"UPDATE group_member_generation SET end_seq=999 WHERE group_id=$1 AND user_id=$2">>,
        [Gid, Peer]
    ),
    ?assertNot(forward_attachment_repo:authorize(Path, Peer)),
    {TargetMsg, group_envelope(TargetMsg, forward_payload(Payload, Source, <<"c2c">>))}.

group_edit_liveness_case(Base, Uid, Gid, Source, Path, Payload) ->
    {rollback, group_edit_done} = elib_pg:with_tx(fun(C) ->
        {ok, [#{<<"seq">> := Seq}]} = elib_pg:query(
            C, <<"SELECT conv_seq AS seq FROM msg_store_staging WHERE type='c2g' AND msg_id=$1">>, [
                Source
            ]
        ),
        Live =
            <<"SELECT public.fn_forward_attachment_target_live('c2g',$1,$2,$3,$4,$5,false) AS readable, public.fn_forward_attachment_target_live('c2g',$1,$2,$3,$4,$5,true) AS blocked">>,
        {ok, [#{<<"readable">> := true, <<"blocked">> := true}]} = elib_pg:query(C, Live, [
            Source, Uid, Gid, Seq, Path
        ]),
        Edit = elib_uuid:gen_v7(),
        Body = #{
            <<"msg_type">> => <<"custom">>,
            <<"payload">> => #{<<"original_msg_id">> => Source, <<"content">> => <<"edited">>}
        },
        {ok, _} = elib_pg:query(
            C,
            <<"INSERT INTO msg_store_staging SELECT (jsonb_populate_record(NULL::msg_store_staging,to_jsonb(s)||jsonb_build_object('id',$2::bigint,'msg_id',$3::text,'msg_type','custom','action','message_edit_ack','payload',$4::jsonb))).* FROM msg_store_staging s WHERE type='c2g' AND msg_id=$1">>,
            [Source, Base + 181, Edit, jsone:encode(Body)]
        ),
        {ok, [#{<<"n">> := 1}]} = elib_pg:query(
            C,
            <<"SELECT count(*) AS n FROM c2g_forward_edit_fact WHERE group_id=$1 AND original_msg_id=$2">>,
            [Gid, Source]
        ),
        Forged = elib_uuid:gen_v7(),
        EncEdit = elib_uuid:gen_v7(),
        EncBody = #{<<"payload">> => #{<<"original_msg_id">> => Forged}},
        EncMeta = #{<<"edit_of">> => Source},
        {ok, _} = elib_pg:query(
            C,
            <<"INSERT INTO msg_store_staging SELECT (jsonb_populate_record(NULL::msg_store_staging,to_jsonb(s)||jsonb_build_object('id',$2::bigint,'msg_id',$3::text,'msg_type','text','action','message_edit','payload',$4::jsonb,'e2ee',$5::jsonb))).* FROM msg_store_staging s WHERE type='c2g' AND msg_id=$1">>,
            [Source, Base + 183, EncEdit, jsone:encode(EncBody), jsone:encode(EncMeta)]
        ),
        {ok, [#{<<"correct">> := 1, <<"forged">> := 0}]} = elib_pg:query(
            C,
            <<"SELECT count(*) FILTER (WHERE original_msg_id=$2) AS correct,count(*) FILTER (WHERE original_msg_id=$3) AS forged FROM c2g_forward_edit_fact WHERE group_id=$1 AND action_msg_id=$4">>,
            [Gid, Source, Forged, EncEdit]
        ),
        {ok, [#{<<"readable">> := false, <<"blocked">> := true}]} = elib_pg:query(C, Live, [
            Source, Uid, Gid, Seq, Path
        ]),
        %% Source C2G edits lack a rewritten authoritative original body.
        {ok, _} = elib_pg:query(C, <<"SET LOCAL session_replication_role=replica">>, []),
        {ok, _} = elib_pg:query(
            C,
            <<"INSERT INTO msg_c2g(id,from_id,to_id,msg_id,msg_type,payload) VALUES($1,$2,$3,$4,'image',$5::jsonb)">>,
            [Base + 182, Uid, Gid, Source, jsone:encode(Payload)]
        ),
        {ok, [#{<<"allowed">> := false}]} = elib_pg:query(
            C,
            <<"SELECT public.fn_forward_source_c2g_validate($1,$2,$2,$3,'image',$4::jsonb,false) AS allowed">>,
            [Source, Uid, Gid, jsone:encode(Payload)]
        ),
        throw({rollback, group_edit_done})
    end),
    ok.

c2g_source_role_case(Uid, Gid, Outsider, Source, Payload) ->
    {ok, [#{<<"can_insert">> := false}]} = elib_pg:query(
        <<"SELECT has_table_privilege('v5_app_runtime','public.c2g_revoke_fact','INSERT') AS can_insert">>,
        []
    ),
    ?assertMatch(
        {error, _},
        elib_pg:with_tx(fun(C) ->
            {ok, _} = elib_pg:query(C, <<"SET LOCAL ROLE v5_app_runtime">>, []),
            case
                elib_pg:query(
                    C,
                    <<"INSERT INTO c2g_revoke_fact(group_id,original_msg_id,action_msg_id,actor_uid,accepted_seq) VALUES($1,$2,$3,$4,999)">>,
                    [Gid, Source, elib_uuid:gen_v7(), Outsider]
                )
            of
                {error, Reason} -> throw({abort_tx, Reason});
                _ -> error(revoke_fact_forgery_allowed)
            end
        end)
    ),
    ok = elib_pg:with_tx(fun(C) ->
        {ok, _} = elib_pg:query(C, <<"SET LOCAL ROLE v5_app_runtime">>, []),
        Sql =
            <<"SELECT public.fn_forward_source_c2g_validate($1,$2,$3,$4,$5,$6::jsonb,false) AS allowed">>,
        {ok, [#{<<"allowed">> := true}]} = elib_pg:query(
            C,
            Sql,
            [Source, Uid, Uid, Gid, <<"image">>, jsone:encode(Payload)]
        ),
        {ok, [#{<<"allowed">> := false}]} = elib_pg:query(
            C,
            Sql,
            [Source, Outsider, Uid, Gid, <<"image">>, jsone:encode(Payload)]
        ),
        {ok, [#{<<"allowed">> := false}]} = elib_pg:query(
            C,
            Sql,
            [Source, Uid, Uid, Gid, <<"image">>, <<"{}">>]
        ),
        ok
    end),
    {ok, [#{<<"worker">> := false, <<"proof">> := false, <<"public">> := false}]} = elib_pg:query(
        <<
            "SELECT has_function_privilege('v5_worker','public.fn_forward_source_c2g_validate(text,bigint,bigint,bigint,text,jsonb,boolean)','EXECUTE') AS worker, "
            "has_function_privilege('v5_proof','public.fn_forward_source_c2g_validate(text,bigint,bigint,bigint,text,jsonb,boolean)','EXECUTE') AS proof, "
            "EXISTS (SELECT 1 FROM pg_proc p, LATERAL aclexplode(coalesce(p.proacl,acldefault('f',p.proowner))) a "
            "WHERE p.oid='public.fn_forward_source_c2g_validate(text,bigint,bigint,bigint,text,jsonb,boolean)'::regprocedure AND a.grantee=0 AND a.privilege_type='EXECUTE') AS public"
        >>,
        []
    ),
    ok.

c2g_workspace_role_case(Base, Uid, Gid, Source, Payload) ->
    %% Changes are rolled back: exercise workspace and organization eligibility
    %% without changing the personal-group fixture used by the remaining tests.
    {rollback, workspace_fixture_done} = elib_pg:with_tx(fun(C) ->
        {ok, _} = elib_pg:query(C, <<"SET LOCAL session_replication_role=replica">>, []),
        {ok, _} = elib_pg:query(
            C,
            <<"INSERT INTO organization(id,name,owner_id,status) VALUES($1,'fwd-test',$2,'active')">>,
            [Base + 100, Uid]
        ),
        {ok, _} = elib_pg:query(
            C,
            <<"INSERT INTO organization_member(organization_id,user_id,role,status) VALUES($1,$2,'member','active')">>,
            [Base + 100, Uid]
        ),
        {ok, _} = elib_pg:query(
            C,
            <<"INSERT INTO workspace(id,organization_id,name,owner_id,status) VALUES($1,$2,'fwd-test',$3,'active')">>,
            [Base + 101, Base + 100, Uid]
        ),
        {ok, _} = elib_pg:query(
            C,
            <<"INSERT INTO workspace_member(workspace_id,user_id,role,status) VALUES($1,$2,'member','active')">>,
            [Base + 101, Uid]
        ),
        {ok, _} = elib_pg:query(
            C,
            <<"UPDATE public.\"group\" SET scope='workspace',workspace_id=$2 WHERE id=$1">>,
            [Gid, Base + 101]
        ),
        {ok, _} = elib_pg:query(C, <<"SET LOCAL ROLE v5_app_runtime">>, []),
        Sql =
            <<"SELECT public.fn_forward_source_c2g_validate($1,$2,$2,$3,'image',$4::jsonb,false) AS allowed">>,
        Args = [Source, Uid, Gid, jsone:encode(Payload)],
        {ok, [#{<<"allowed">> := true}]} = elib_pg:query(C, Sql, Args),
        {ok, _} = elib_pg:query(C, <<"RESET ROLE">>, []),
        {ok, _} = elib_pg:query(
            C,
            <<"UPDATE workspace_member SET status='removed' WHERE workspace_id=$1 AND user_id=$2">>,
            [Base + 101, Uid]
        ),
        {ok, _} = elib_pg:query(C, <<"SET LOCAL ROLE v5_app_runtime">>, []),
        {ok, [#{<<"allowed">> := false}]} = elib_pg:query(C, Sql, Args),
        throw({rollback, workspace_fixture_done})
    end),
    ok.

c2g_discussion_role_case(Base, Uid, Gid, Source, Payload) ->
    {rollback, discussion_fixture_done} = elib_pg:with_tx(fun(C) ->
        {ok, _} = elib_pg:query(C, <<"SET LOCAL session_replication_role=replica">>, []),
        {ok, _} = elib_pg:query(
            C,
            <<"UPDATE public.\"group\" SET type=2,join_limit=3,e2ee_mode=0 WHERE id=$1">>,
            [Gid]
        ),
        {ok, _} = elib_pg:query(
            C,
            <<"INSERT INTO channel(id,name,creator_uid,scope,status,discussion_group_id) VALUES($1,'fwd-test',$2,'personal',1,$3)">>,
            [Base + 110, Uid, Gid]
        ),
        {ok, _} = elib_pg:query(C, <<"SET LOCAL ROLE v5_app_runtime">>, []),
        Sql =
            <<"SELECT public.fn_forward_source_c2g_validate($1,$2,$2,$3,'image',$4::jsonb,$5) AS allowed">>,
        Args = [Source, Uid, Gid, jsone:encode(Payload)],
        {ok, [#{<<"allowed">> := false}]} = elib_pg:query(C, Sql, Args ++ [false]),
        {ok, [#{<<"allowed">> := true}]} = elib_pg:query(C, Sql, Args ++ [true]),
        {ok, _} = elib_pg:query(C, <<"RESET ROLE">>, []),
        {ok, _} = elib_pg:query(C, <<"UPDATE channel SET status=0 WHERE id=$1">>, [Base + 110]),
        {ok, _} = elib_pg:query(C, <<"SET LOCAL ROLE v5_app_runtime">>, []),
        {ok, [#{<<"allowed">> := false}]} = elib_pg:query(C, Sql, Args ++ [true]),
        throw({rollback, discussion_fixture_done})
    end),
    ok.

discussion_subscription_phantom_case(Base, Uid, Gid, Source, Payload) ->
    ChannelId = Base + 120,
    Owner = Base + 121,
    ok = elib_pg:with_tx(fun(C) ->
        {ok, _} = elib_pg:query(C, <<"SET LOCAL session_replication_role=replica">>, []),
        {ok, _} = elib_pg:query(
            C,
            <<"UPDATE public.\"group\" SET owner_uid=$2,type=2,join_limit=3,e2ee_mode=0 WHERE id=$1">>,
            [Gid, Owner]
        ),
        {ok, _} = elib_pg:query(
            C,
            <<"INSERT INTO channel(id,name,creator_uid,scope,status,discussion_group_id,access_type) VALUES($1,'phantom',$2,'personal',1,$3,0)">>,
            [ChannelId, Owner, Gid]
        ),
        ok
    end),
    Parent = self(),
    Locker = spawn(fun() ->
        R = elib_pg:with_tx(fun(C) ->
            {ok, _} = elib_pg:query(
                C, <<"SELECT msg_id FROM msg_c2g WHERE msg_id=$1 FOR UPDATE">>, [Source]
            ),
            Parent ! {subscription_locked, self()},
            receive
                release -> ok
            after 10000 -> error(lock_timeout)
            end
        end),
        Parent ! {subscription_unlocked, self(), R}
    end),
    receive
        {subscription_locked, Locker} -> ok
    after 10000 -> error(lock_timeout)
    end,
    Reader = spawn(fun() ->
        R = elib_pg:with_tx(fun(C) ->
            {ok, _} = elib_pg:query(C, <<"SET LOCAL ROLE v5_app_runtime">>, []),
            Parent ! {subscription_reading, self()},
            elib_pg:query(
                C,
                <<"SELECT public.fn_forward_source_c2g_validate($1,$2,$2,$3,'image',$4::jsonb,true) AS allowed">>,
                [Source, Uid, Gid, jsone:encode(Payload)]
            )
        end),
        Parent ! {subscription_result, self(), R}
    end),
    receive
        {subscription_reading, Reader} -> ok
    after 10000 -> error(read_timeout)
    end,
    wait_named_blocked(<<"%fn_forward_source_c2g_validate%">>, 100),
    ok = elib_pg:with_tx(fun(C) ->
        {ok, _} = elib_pg:query(C, <<"SET LOCAL session_replication_role=replica">>, []),
        {ok, _} = elib_pg:query(
            C,
            <<"INSERT INTO channel_subscription(id,channel_id,user_id,status) VALUES($1,$2,$3,1)">>,
            [Base + 122, ChannelId, Uid]
        ),
        ok
    end),
    Locker ! release,
    receive
        {subscription_unlocked, Locker, ok} -> ok
    after 10000 -> error(unlock_timeout)
    end,
    receive
        {subscription_result, Reader, {ok, [#{<<"allowed">> := false}]}} -> ok
    after 10000 -> error(subscription_phantom_timeout)
    end,
    ok = elib_pg:with_tx(fun(C) ->
        {ok, _} = elib_pg:query(C, <<"SET LOCAL session_replication_role=replica">>, []),
        {ok, _} = elib_pg:query(
            C, <<"UPDATE channel SET discussion_group_id=NULL,status=0 WHERE id=$1">>, [ChannelId]
        ),
        {ok, _} = elib_pg:query(
            C,
            <<"UPDATE public.\"group\" SET owner_uid=$2,type=0,join_limit=0,e2ee_mode=0 WHERE id=$1">>,
            [Gid, Uid]
        ),
        ok
    end).

paid_expiry_target_lock_case(Base, Uid, Gid, Peer, Source, Existing, Path, Payload) ->
    Target = elib_uuid:gen_v7(),
    ChannelId = Base + 220,
    Owner = Base + 221,
    ok = elib_pg:with_tx(fun(C) ->
        {ok, _} = elib_pg:query(C, <<"SET LOCAL session_replication_role=replica">>, []),
        {ok, _} = elib_pg:query(
            C,
            <<"UPDATE public.\"group\" SET owner_uid=$2,type=2,join_limit=3,e2ee_mode=0 WHERE id=$1">>,
            [Gid, Owner]
        ),
        {ok, _} = elib_pg:query(
            C,
            <<"INSERT INTO channel(id,name,creator_uid,scope,status,discussion_group_id,access_type) VALUES($1,'paid-expiry',$2,'personal',1,$3,1)">>,
            [ChannelId, Owner, Gid]
        ),
        {ok, _} = elib_pg:query(
            C,
            <<"INSERT INTO channel_subscription(id,channel_id,user_id,status) VALUES($1,$2,$3,1)">>,
            [Base + 222, ChannelId, Uid]
        ),
        {ok, _} = elib_pg:query(
            C,
            <<"INSERT INTO channel_order(id,channel_id,user_id,order_no,amount,status,expires_at,subscription_end_at) VALUES($1,$2,$3,$4,1,1,clock_timestamp()+interval '1 day',clock_timestamp()+interval '5 seconds')">>,
            [Base + 223, ChannelId, Uid, Target]
        ),
        {ok, _} = elib_pg:query(
            C,
            <<"INSERT INTO msg_store_staging SELECT (jsonb_populate_record(NULL::msg_store_staging,to_jsonb(s)||jsonb_build_object('id',$2::bigint,'msg_id',$3::text))).* FROM msg_store_staging s WHERE type='c2c' AND msg_id=$1">>,
            [Existing, Base + 224, Target]
        ),
        ok
    end),
    {ok, [#{<<"seq">> := Seq}]} = elib_pg:query(
        <<"SELECT conv_seq AS seq FROM msg_store_staging WHERE type='c2c' AND msg_id=$1">>, [Target]
    ),
    {ok, [#{<<"allowed">> := true}]} = elib_pg:query(
        <<"SELECT public.fn_forward_source_c2g_validate($1,$2,$2,$3,'image',$4::jsonb,true) AS allowed">>,
        [Source, Uid, Gid, jsone:encode(Payload)]
    ),
    Parent = self(),
    Locker = spawn(fun() ->
        R = elib_pg:with_tx(fun(C) ->
            {ok, _} = elib_pg:query(
                C,
                <<"SELECT msg_id FROM msg_store_staging WHERE type='c2c' AND msg_id=$1 FOR UPDATE">>,
                [Target]
            ),
            {ok, [#{<<"pid">> := LockPid}]} = elib_pg:query(
                C, <<"SELECT pg_backend_pid() AS pid">>, []
            ),
            Parent ! {paid_target_locked, self(), LockPid},
            receive
                release -> ok
            after 10000 -> error(lock_timeout)
            end
        end),
        Parent ! {paid_target_unlocked, self(), R}
    end),
    LockPid =
        receive
            {paid_target_locked, Locker, LPid} -> LPid
        after 10000 -> error(lock_timeout)
        end,
    Binder = spawn(fun() ->
        R = elib_pg:with_tx(fun(C) ->
            {ok, _} = elib_pg:query(C, <<"SET LOCAL ROLE v5_app_runtime">>, []),
            {ok, [#{<<"pid">> := BindPid}]} = elib_pg:query(
                C, <<"SELECT pg_backend_pid() AS pid">>, []
            ),
            Parent ! {paid_target_binding, self(), BindPid},
            elib_pg:query(
                C,
                <<"SELECT public.fn_forward_attachment_bind($1,'c2g',$2,$3,$4,'image',$5::jsonb,'c2c',$6,$3,$7,$8,true) AS accepted">>,
                [Path, Source, Uid, Gid, jsone:encode(Payload), Target, Peer, Seq]
            )
        end),
        Parent ! {paid_target_result, self(), R}
    end),
    BindPid =
        receive
            {paid_target_binding, Binder, BPid} -> BPid
        after 10000 -> error(bind_timeout)
        end,
    wait_blocked_by(BindPid, LockPid, 100),
    receive
    after 5500 -> ok
    end,
    Locker ! release,
    receive
        {paid_target_unlocked, Locker, ok} -> ok
    after 10000 -> error(unlock_timeout)
    end,
    receive
        {paid_target_result, Binder, {ok, [#{<<"accepted">> := false}]}} -> ok
    after 10000 -> error(paid_expiry_result_timeout)
    end,
    {ok, [#{<<"n">> := 0}]} = elib_pg:query(
        <<"SELECT count(*) AS n FROM forward_attachment_reference WHERE target_msg_id=$1">>, [
            Target
        ]
    ),
    ok = elib_pg:with_tx(fun(C) ->
        {ok, _} = elib_pg:query(C, <<"SET LOCAL session_replication_role=replica">>, []),
        {ok, _} = elib_pg:query(
            C, <<"UPDATE channel SET discussion_group_id=NULL,status=0 WHERE id=$1">>, [ChannelId]
        ),
        {ok, _} = elib_pg:query(
            C,
            <<"UPDATE public.\"group\" SET owner_uid=$2,type=0,join_limit=0,e2ee_mode=0 WHERE id=$1">>,
            [Gid, Uid]
        ),
        ok
    end).

workspace_rebind_race_case(Base, Uid, Gid, Source, Payload) ->
    Org = Base + 130,
    Workspace = Base + 131,
    ok = elib_pg:with_tx(fun(C) ->
        {ok, _} = elib_pg:query(C, <<"SET LOCAL session_replication_role=replica">>, []),
        {ok, _} = elib_pg:query(
            C,
            <<"INSERT INTO organization(id,name,owner_id,status) VALUES($1,'rebind',$2,'active')">>,
            [Org, Uid]
        ),
        {ok, _} = elib_pg:query(
            C,
            <<"INSERT INTO organization_member(organization_id,user_id,role,status) VALUES($1,$2,'member','active')">>,
            [Org, Uid]
        ),
        {ok, _} = elib_pg:query(
            C,
            <<"INSERT INTO workspace(id,organization_id,name,owner_id,status) VALUES($1,$2,'rebind',$3,'active')">>,
            [Workspace, Org, Uid]
        ),
        {ok, _} = elib_pg:query(
            C,
            <<"INSERT INTO workspace_member(workspace_id,user_id,role,status) VALUES($1,$2,'member','active')">>,
            [Workspace, Uid]
        ),
        {ok, _} = elib_pg:query(
            C, <<"UPDATE public.\"group\" SET scope='workspace',workspace_id=$2 WHERE id=$1">>, [
                Gid, Workspace
            ]
        ),
        ok
    end),
    Parent = self(),
    Locker = spawn(fun() ->
        R = elib_pg:with_tx(fun(C) ->
            {ok, _} = elib_pg:query(C, <<"SELECT id FROM organization WHERE id=$1 FOR UPDATE">>, [
                Org
            ]),
            Parent ! {rebind_locked, self()},
            receive
                release -> ok
            after 10000 -> error(lock_timeout)
            end
        end),
        Parent ! {rebind_unlock, self(), R}
    end),
    receive
        {rebind_locked, Locker} -> ok
    after 10000 -> error(lock_timeout)
    end,
    Reader = spawn(fun() ->
        R = elib_pg:with_tx(fun(C) ->
            {ok, _} = elib_pg:query(C, <<"SET LOCAL ROLE v5_app_runtime">>, []),
            Parent ! {rebind_reading, self()},
            elib_pg:query(
                C,
                <<"SELECT public.fn_forward_source_c2g_validate($1,$2,$2,$3,'image',$4::jsonb,false) AS allowed">>,
                [Source, Uid, Gid, jsone:encode(Payload)]
            )
        end),
        Parent ! {rebind_result, self(), R}
    end),
    receive
        {rebind_reading, Reader} -> ok
    after 10000 -> error(read_timeout)
    end,
    wait_named_blocked(<<"%fn_forward_source_c2g_validate%">>, 100),
    ok = elib_pg:with_tx(fun(C) ->
        {ok, _} = elib_pg:query(C, <<"SET LOCAL session_replication_role=replica">>, []),
        {ok, _} = elib_pg:query(
            C, <<"UPDATE public.\"group\" SET scope='personal',workspace_id=NULL WHERE id=$1">>, [
                Gid
            ]
        ),
        ok
    end),
    Locker ! release,
    receive
        {rebind_unlock, Locker, ok} -> ok
    after 10000 -> error(unlock_timeout)
    end,
    receive
        {rebind_result, Reader, {ok, [#{<<"allowed">> := false}]}} -> ok
    after 10000 -> error(rebind_result_timeout)
    end.

c2g_source_case(Base, Uid, Gid, Peer, Source, Path, Payload) ->
    %% The worker normally materializes this staging row; this fixture inserts
    %% a synthetic formal row to exercise repeated forwarding in one database.
    ok = elib_pg:with_tx(fun(C) ->
        {ok, _} = elib_pg:query(C, <<"SET LOCAL session_replication_role=replica">>, []),
        {ok, _} = elib_pg:query(
            C,
            <<
                "INSERT INTO msg_c2g(id,from_id,to_id,msg_id,msg_type,payload)"
                " VALUES($1,$2,$3,$4,'image',$5::jsonb)"
            >>,
            [Base + 30, Uid, Gid, Source, jsone:encode(Payload)]
        ),
        ok
    end),
    c2g_source_role_case(Uid, Gid, Peer, Source, Payload),
    c2g_workspace_role_case(Base, Uid, Gid, Source, Payload),
    c2g_discussion_role_case(Base, Uid, Gid, Source, Payload),
    discussion_subscription_phantom_case(Base, Uid, Gid, Source, Payload),
    workspace_rebind_race_case(Base, Uid, Gid, Source, Payload),
    TargetMsg = elib_uuid:gen_v7(),
    Now = elib_dt:to_rfc3339(elib_dt:millisecond(), millisecond, "Z"),
    Ctx = #{
        source_type => <<"c2g">>,
        source_id => Source,
        source_from => Uid,
        source_target => Gid,
        source_payload => Payload,
        source_msg_type => <<"image">>,
        target_type => <<"c2c">>,
        target_id => Peer,
        sender => Uid,
        keys => [Path]
    },
    ?assertEqual(
        {ok, TargetMsg},
        forward_attachment_repo:with_context(Ctx, fun() ->
            case
                msg_store_repo:stage_with_sequence(
                    <<"c2c">>,
                    TargetMsg,
                    <<"image">>,
                    <<>>,
                    null,
                    jsone:encode(forward_payload(Payload, Source, <<"c2g">>)),
                    Uid,
                    Peer,
                    Now,
                    Now,
                    <<>>,
                    Now,
                    null
                )
            of
                {ok, _, _} -> {ok, TargetMsg};
                Error -> Error
            end
        end)
    ),
    ?assert(forward_attachment_repo:authorize(Path, Peer)),
    paid_expiry_target_lock_case(Base, Uid, Gid, Peer, Source, TargetMsg, Path, Payload),
    group_anchor_scope_case(Uid, Gid, Peer, Source, TargetMsg, Path, Payload),
    GroupTarget = Base + 40,
    GroupMsg = elib_uuid:gen_v7(),
    GroupCtx = Ctx#{target_type => <<"c2g">>, target_id => GroupTarget},
    ?assertEqual(
        {ok, GroupMsg},
        forward_attachment_repo:with_context(GroupCtx, fun() ->
            case
                msg_store_repo:stage(
                    <<"c2g">>,
                    GroupMsg,
                    <<"image">>,
                    <<>>,
                    null,
                    jsone:encode(
                        group_envelope(GroupMsg, forward_payload(Payload, Source, <<"c2g">>))
                    ),
                    Uid,
                    GroupTarget,
                    Now,
                    Now,
                    <<>>,
                    1,
                    null
                )
            of
                {ok, _, _, _} -> {ok, GroupMsg};
                Error -> Error
            end
        end)
    ),
    ?assert(forward_attachment_repo:authorize(Path, Peer + 3)),
    ok = elib_pg:with_tx(fun(C) ->
        {ok, _} = elib_pg:query(C, <<"SET LOCAL session_replication_role=replica">>, []),
        {ok, _} = elib_pg:query(
            C,
            <<"INSERT INTO msg_c2g(id,from_id,to_id,msg_id,msg_type,payload) VALUES($1,$2,$3,$4,'image',$5::jsonb)">>,
            [
                Base + 35,
                Uid,
                GroupTarget,
                GroupMsg,
                jsone:encode(group_envelope(GroupMsg, forward_payload(Payload, Source, <<"c2g">>)))
            ]
        ),
        ok
    end),
    bidirectional_group_case(Uid, Gid, GroupTarget, Source, GroupMsg, Path, Payload, Now),
    {ok, _} = elib_pg:query(
        <<"INSERT INTO c2g_revoke_fact(group_id,original_msg_id,action_msg_id,actor_uid,accepted_seq) VALUES($1,$2,$3,$4,999)">>,
        [GroupTarget, GroupMsg, elib_uuid:gen_v7(), Uid]
    ),
    ?assertNot(forward_attachment_repo:authorize(Path, Peer + 3)),
    {ok, _} = elib_pg:query(
        <<
            "INSERT INTO c2g_revoke_fact(group_id,original_msg_id,action_msg_id,actor_uid,accepted_seq)"
            " VALUES($1,$2,$3,$4,999)"
        >>,
        [Gid, Source, elib_uuid:gen_v7(), Uid]
    ),
    ok = elib_pg:with_tx(fun(C) ->
        {ok, _} = elib_pg:query(C, <<"SET LOCAL ROLE v5_app_runtime">>, []),
        {ok, [#{<<"allowed">> := false}]} = elib_pg:query(
            C,
            <<"SELECT public.fn_forward_source_c2g_validate($1,$2,$3,$4,$5,$6::jsonb,false) AS allowed">>,
            [Source, Uid, Uid, Gid, <<"image">>, jsone:encode(Payload)]
        ),
        ok
    end),
    NewMsg = elib_uuid:gen_v7(),
    ?assertMatch(
        {error, _},
        forward_attachment_repo:with_context(Ctx, fun() ->
            case
                msg_store_repo:stage_with_sequence(
                    <<"c2c">>,
                    NewMsg,
                    <<"image">>,
                    <<>>,
                    null,
                    jsone:encode(forward_payload(Payload, Source, <<"c2g">>)),
                    Uid,
                    Peer,
                    Now,
                    Now,
                    <<>>,
                    Now,
                    null
                )
            of
                {ok, _, _} -> {ok, NewMsg};
                Error -> Error
            end
        end)
    ),
    {ok, [#{<<"n">> := 0}]} = elib_pg:query(
        <<"SELECT count(*) AS n FROM msg_store_staging WHERE type='c2c' AND msg_id=$1">>, [NewMsg]
    ),
    TargetMsg.

bidirectional_group_case(Uid, GidA, GidB, MsgA, MsgB, Path, PayloadA, Now) ->
    Parent = self(),
    Send = fun(SourceGid, TargetGid, SourceMsg, SourcePayload) ->
        TargetMsg = elib_uuid:gen_v7(),
        Ctx = #{
            source_type => <<"c2g">>,
            source_id => SourceMsg,
            source_from => Uid,
            source_target => SourceGid,
            source_payload => SourcePayload,
            source_msg_type => <<"image">>,
            target_type => <<"c2g">>,
            target_id => TargetGid,
            sender => Uid,
            keys => [Path]
        },
        Parent ! {bidir_ready, self()},
        receive
            start -> ok
        after 10000 -> error(bidir_start_timeout)
        end,
        Result = forward_attachment_repo:with_context(Ctx, fun() ->
            case
                msg_store_repo:stage(
                    <<"c2g">>,
                    TargetMsg,
                    <<"image">>,
                    <<>>,
                    null,
                    jsone:encode(forward_payload(SourcePayload, SourceMsg, <<"c2g">>)),
                    Uid,
                    TargetGid,
                    Now,
                    Now,
                    <<>>,
                    1,
                    null
                )
            of
                {ok, _, _, _} -> {ok, TargetMsg};
                Error -> Error
            end
        end),
        Parent ! {bidir_result, self(), TargetGid, TargetMsg, Result}
    end,
    A = spawn(fun() -> Send(GidA, GidB, MsgA, PayloadA) end),
    B = spawn(fun() ->
        Send(GidB, GidA, MsgB, group_envelope(MsgB, forward_payload(PayloadA, MsgA, <<"c2g">>)))
    end),
    receive
        {bidir_ready, A} -> ok
    after 10000 -> error(bidir_ready_a_timeout)
    end,
    receive
        {bidir_ready, B} -> ok
    after 10000 -> error(bidir_ready_b_timeout)
    end,
    A ! start,
    B ! start,
    lists:foreach(
        fun(Pid) ->
            receive
                {bidir_result, Pid, Gid, Msg, {ok, Msg}} ->
                    {ok, _} = elib_pg:query(
                        <<"INSERT INTO c2g_revoke_fact(group_id,original_msg_id,action_msg_id,actor_uid,accepted_seq) VALUES($1,$2,$3,$4,999)">>,
                        [Gid, Msg, elib_uuid:gen_v7(), Uid]
                    );
                {bidir_result, Pid, _, _, Failure} ->
                    error({bidir_failed, Failure})
            after 20000 -> error(bidir_result_timeout)
            end
        end,
        [A, B]
    ).

group_anchor_scope_case(Uid, Gid, Peer, Source, TargetMsg, Path, Payload) ->
    FileId = Gid + 500,
    AlbumDbId = Gid + 501,
    PhotoDbId = Gid + 502,
    AlbumId = elib_uuid:gen_v7(),
    {rollback, anchor_done} = elib_pg:with_tx(fun(C) ->
        {ok, _} = elib_pg:query(C, <<"SET LOCAL session_replication_role=replica">>, []),
        {ok, _} = elib_pg:query(
            C, <<"DELETE FROM forward_attachment_reference WHERE object_key=$1">>, [Path]
        ),
        {ok, _} = elib_pg:query(
            C,
            <<"UPDATE attachment SET scope='group',scope_ref=$2,anchor_conv_seq=1 WHERE path=$1">>,
            [Path, integer_to_binary(Gid)]
        ),
        {ok, _} = elib_pg:query(
            C,
            <<"UPDATE group_member_generation SET start_seq=2 WHERE group_id=$1 AND user_id=$2 AND end_seq IS NULL">>,
            [Gid, Uid]
        ),
        {ok, _} = elib_pg:query(
            C,
            <<"UPDATE c2g_forward_source_fact SET conv_seq=2 WHERE msg_id=$1 AND group_id=$2">>,
            [Source, Gid]
        ),
        {ok, _} = elib_pg:query(
            C,
            <<"UPDATE msg_c2g_recipient_snapshot SET conv_seq=2 WHERE msg_id=$1 AND to_gid=$2">>,
            [Source, Gid]
        ),
        {ok, [#{<<"seq">> := Seq}]} = elib_pg:query(
            C, <<"SELECT conv_seq AS seq FROM msg_store_staging WHERE type='c2c' AND msg_id=$1">>, [
                TargetMsg
            ]
        ),
        {ok, _} = elib_pg:query(C, <<"SET LOCAL ROLE v5_app_runtime">>, []),
        Sql =
            <<"SELECT public.fn_forward_attachment_bind($1,'c2g',$2,$3,$4,'image',$5::jsonb,'c2c',$6,$3,$7,$8,false) AS accepted">>,
        Args = [Path, Source, Uid, Gid, jsone:encode(Payload), TargetMsg, Peer, Seq],
        {ok, [#{<<"accepted">> := false}]} = elib_pg:query(C, Sql, Args),
        {ok, _} = elib_pg:query(C, <<"RESET ROLE">>, []),
        {ok, _} = elib_pg:query(C, <<"UPDATE attachment SET anchor_conv_seq=2 WHERE path=$1">>, [
            Path
        ]),
        {ok, _} = elib_pg:query(C, <<"SET LOCAL ROLE v5_app_runtime">>, []),
        {ok, [#{<<"accepted">> := true}]} = elib_pg:query(C, Sql, Args),
        throw({rollback, anchor_done})
    end),
    ok = elib_pg:with_tx(fun(C0) ->
        {ok, _} = elib_pg:query(C0, <<"SET LOCAL session_replication_role=replica">>, []),
        {ok, _} = elib_pg:query(
            C0,
            <<"INSERT INTO group_file(id,group_id,file_id,file_name,file_size,file_type,file_category,file_url,uploader_id) VALUES($1,$2,$3,'test',1,'image/png','image',$4,$5)">>,
            [FileId, Gid, elib_uuid:gen_v7(), Path, Uid]
        ),
        {ok, _} = elib_pg:query(
            C0,
            <<"INSERT INTO group_album(id,group_id,album_id,album_name,creator_id) VALUES($1,$2,$3,'test',$4)">>,
            [AlbumDbId, Gid, AlbumId, Uid]
        ),
        {ok, _} = elib_pg:query(
            C0,
            <<"INSERT INTO group_album_photo(id,group_id,album_id,photo_id,photo_name,photo_url,photo_size,uploader_id) VALUES($1,$2,$3,$4,'test',$5,1,$6)">>,
            [PhotoDbId, Gid, AlbumId, elib_uuid:gen_v7(), Path, Uid]
        ),
        ok
    end),
    Parent = self(),
    {rollback, {group_anchor_done, Workers}} = elib_pg:with_tx(fun(C) ->
        {ok, _} = elib_pg:query(C, <<"SET LOCAL session_replication_role=replica">>, []),
        {ok, _} = elib_pg:query(
            C, <<"DELETE FROM forward_attachment_reference WHERE object_key=$1">>, [Path]
        ),
        {ok, _} = elib_pg:query(
            C,
            <<"UPDATE attachment SET scope='group',scope_ref=$2,anchor_conv_seq=1 WHERE path=$1">>,
            [Path, integer_to_binary(Gid)]
        ),
        {ok, _} = elib_pg:query(
            C,
            <<"UPDATE group_member_generation SET start_seq=2 WHERE group_id=$1 AND user_id=$2 AND end_seq IS NULL">>,
            [Gid, Uid]
        ),
        {ok, _} = elib_pg:query(
            C,
            <<"UPDATE c2g_forward_source_fact SET conv_seq=2 WHERE msg_id=$1 AND group_id=$2">>,
            [Source, Gid]
        ),
        {ok, _} = elib_pg:query(
            C,
            <<"UPDATE msg_c2g_recipient_snapshot SET conv_seq=2 WHERE msg_id=$1 AND to_gid=$2">>,
            [Source, Gid]
        ),
        {ok, [#{<<"seq">> := Seq}]} = elib_pg:query(
            C, <<"SELECT conv_seq AS seq FROM msg_store_staging WHERE type='c2c' AND msg_id=$1">>, [
                TargetMsg
            ]
        ),
        {ok, _} = elib_pg:query(C, <<"SET LOCAL ROLE v5_app_runtime">>, []),
        Sql =
            <<"SELECT public.fn_forward_attachment_bind($1,'c2g',$2,$3,$4,'image',$5::jsonb,'c2c',$6,$3,$7,$8,false) AS accepted">>,
        Args = [Path, Source, Uid, Gid, jsone:encode(Payload), TargetMsg, Peer, Seq],
        {ok, _} = elib_pg:query(C, <<"RESET ROLE">>, []),
        {ok, _} = elib_pg:query(
            C,
            <<"UPDATE attachment SET group_file_id=$2,anchor_conv_seq=NULL,anchor_msg_id=NULL WHERE path=$1">>,
            [Path, FileId]
        ),
        {ok, _} = elib_pg:query(C, <<"SET LOCAL ROLE v5_app_runtime">>, []),
        {ok, [#{<<"accepted">> := true}]} = elib_pg:query(C, Sql, Args),
        {ok, _} = elib_pg:query(C, <<"RESET ROLE">>, []),
        Workers = [
            spawn(fun() ->
                Parent ! {revoke_start, self()},
                Result = elib_pg:query(Update, [Id]),
                Parent ! {revoke_done, self(), Result}
            end)
         || {Update, Id} <- [
                {<<"UPDATE group_file SET status=0 WHERE id=$1">>, FileId},
                {<<"UPDATE group_album_photo SET status=0 WHERE id=$1">>, PhotoDbId},
                {<<"UPDATE group_album SET status=0 WHERE id=$1">>, AlbumDbId}
            ]
        ],
        lists:foreach(
            fun(Pid) ->
                receive
                    {revoke_start, Pid} -> ok
                after 10000 -> error(revoke_start_timeout)
                end
            end,
            Workers
        ),
        wait_revokes_blocked(C, 3, 500),
        throw({rollback, {group_anchor_done, Workers}})
    end),
    lists:foreach(
        fun(Pid) ->
            receive
                {revoke_done, Pid, {ok, _}} -> ok
            after 10000 -> error(revoke_done_timeout)
            end
        end,
        Workers
    ),
    group_phantom_case(Uid, Gid, Peer, Source, TargetMsg, Path, Payload, PhotoDbId),
    ok.

group_phantom_case(Uid, Gid, Peer, Source, TargetMsg, Path, Payload, ExistingPhoto) ->
    NewAlbum = elib_uuid:gen_v7(),
    NewPhoto = elib_uuid:gen_v7(),
    ok = elib_pg:with_tx(fun(C) ->
        {ok, _} = elib_pg:query(C, <<"SET LOCAL session_replication_role=replica">>, []),
        {ok, _} = elib_pg:query(C, <<"UPDATE group_album_photo SET status=1 WHERE id=$1">>, [
            ExistingPhoto
        ]),
        {ok, _} = elib_pg:query(
            C,
            <<"INSERT INTO group_album(id,group_id,album_id,album_name,creator_id) VALUES($1,$2,$3,'phantom',$4)">>,
            [Gid + 510, Gid, NewAlbum, Uid]
        ),
        ok
    end),
    Parent = self(),
    Locker = spawn(fun() ->
        R = elib_pg:with_tx(fun(C) ->
            {ok, _} = elib_pg:query(
                C, <<"SELECT id FROM group_album_photo WHERE id=$1 FOR UPDATE">>, [ExistingPhoto]
            ),
            Parent ! {phantom_locked, self()},
            receive
                release -> ok
            after 10000 -> error(phantom_lock_timeout)
            end
        end),
        Parent ! {phantom_unlock, self(), R}
    end),
    receive
        {phantom_locked, Locker} -> ok
    after 10000 -> error(phantom_lock_timeout)
    end,
    Binder = spawn(fun() ->
        R = elib_pg:with_tx(fun(C) ->
            {ok, _} = elib_pg:query(C, <<"SET LOCAL session_replication_role=replica">>, []),
            {ok, _} = elib_pg:query(
                C, <<"DELETE FROM forward_attachment_reference WHERE object_key=$1">>, [Path]
            ),
            {ok, _} = elib_pg:query(
                C,
                <<"UPDATE attachment SET scope='group',scope_ref=$2,anchor_conv_seq=1 WHERE path=$1">>,
                [Path, integer_to_binary(Gid)]
            ),
            {ok, _} = elib_pg:query(
                C,
                <<"UPDATE group_member_generation SET start_seq=2 WHERE group_id=$1 AND user_id=$2 AND end_seq IS NULL">>,
                [Gid, Uid]
            ),
            {ok, _} = elib_pg:query(
                C,
                <<"UPDATE c2g_forward_source_fact SET conv_seq=2 WHERE msg_id=$1 AND group_id=$2">>,
                [Source, Gid]
            ),
            {ok, _} = elib_pg:query(
                C,
                <<"UPDATE msg_c2g_recipient_snapshot SET conv_seq=2 WHERE msg_id=$1 AND to_gid=$2">>,
                [Source, Gid]
            ),
            {ok, [#{<<"seq">> := Seq}]} = elib_pg:query(
                C,
                <<"SELECT conv_seq AS seq FROM msg_store_staging WHERE type='c2c' AND msg_id=$1">>,
                [TargetMsg]
            ),
            {ok, _} = elib_pg:query(C, <<"SET LOCAL ROLE v5_app_runtime">>, []),
            Parent ! {phantom_binding, self()},
            {ok, [#{<<"accepted">> := false}]} = elib_pg:query(
                C,
                <<"SELECT public.fn_forward_attachment_bind($1,'c2g',$2,$3,$4,'image',$5::jsonb,'c2c',$6,$3,$7,$8,false) AS accepted">>,
                [Path, Source, Uid, Gid, jsone:encode(Payload), TargetMsg, Peer, Seq]
            ),
            throw({rollback, phantom_done})
        end),
        Parent ! {phantom_result, self(), R}
    end),
    receive
        {phantom_binding, Binder} -> ok
    after 10000 -> error(phantom_bind_timeout)
    end,
    wait_named_blocked(<<"%fn_forward_attachment_bind%">>, 100),
    ok = elib_pg:with_tx(fun(C) ->
        {ok, _} = elib_pg:query(C, <<"SET LOCAL session_replication_role=replica">>, []),
        {ok, _} = elib_pg:query(
            C,
            <<"INSERT INTO group_album_photo(id,group_id,album_id,photo_id,photo_name,photo_url,photo_size,uploader_id) VALUES($1,$2,$3,$4,'late',$5,1,$6)">>,
            [Gid + 511, Gid, NewAlbum, NewPhoto, Path, Uid]
        ),
        ok
    end),
    Locker ! release,
    receive
        {phantom_unlock, Locker, ok} -> ok
    after 10000 -> error(phantom_unlock_timeout)
    end,
    receive
        {phantom_result, Binder, {rollback, phantom_done}} -> ok
    after 10000 -> error(phantom_result_timeout)
    end.

wait_revokes_blocked(_, _, 0) ->
    error(revoke_wait_not_observed);
wait_revokes_blocked(C, Expected, N) ->
    {ok, _} = elib_pg:query(C, <<"SELECT pg_stat_clear_snapshot()">>, []),
    {ok, [#{<<"n">> := Count}]} = elib_pg:query(
        C,
        <<"SELECT count(*) AS n FROM pg_stat_activity WHERE datname=current_database() AND cardinality(pg_blocking_pids(pid))>0 AND query LIKE 'UPDATE group_% SET status=0%'">>,
        []
    ),
    case Count >= Expected of
        true -> ok;
        false -> receive
            after 20 -> wait_revokes_blocked(C, Expected, N - 1)
            end
    end.

target_gc_case(Uid, Peer1, Peer2, Msg1, Msg2, Path) ->
    %% Force a stale cached zero-reference result: intent must still recheck
    %% under the attachment lock and the trigger must reject live forwards.
    {ok, _} = elib_pg:query(<<"UPDATE attachment SET gc_verified=true WHERE path=$1">>, [Path]),
    ?assertMatch(
        {error, _},
        elib_pg:query(
            <<"SELECT fn_v5_intent_ordinary_delete($1,'forward-test')">>, [Path]
        )
    ),
    {ok, [#{<<"state">> := <<"live">>}]} = elib_pg:query(
        <<"SELECT lifecycle_state AS state FROM attachment WHERE path=$1">>, [Path]
    ),
    lists:foreach(
        fun({Msg, Peer}) ->
            {ok, _} = elib_pg:query(
                <<"INSERT INTO c2c_action_receipt(request_id,original_msg_id,actor_uid,peer_uid,action) VALUES($1,$2,$3,$4,'revoke')">>,
                [elib_uuid:gen_v7(), Msg, Uid, Peer]
            )
        end,
        [{Msg1, Peer1}, {Msg2, Peer2}]
    ),
    ?assertNot(forward_attachment_repo:authorize(Path, Peer1)),
    ?assertNot(forward_attachment_repo:authorize(Path, Peer2)),
    {ok, [#{<<"blocked">> := false}]} = elib_pg:query(
        <<"SELECT fn_v5_forward_reference_blocks($1) AS blocked">>, [Path]
    ),
    ok.

%% Two transactions deliberately acquire group-like locks in opposite order.
%% The loser must roll back before the real 40P01 is retried.
deadlock_retry_case(Base) ->
    Parent = self(),
    Run = fun(First, Second) ->
        put(fwd_retry_attempts, 0),
        Result = forward_attachment_repo:with_context(#{}, fun() ->
            forward_attachment_repo:transaction(fun(C) ->
                N = get(fwd_retry_attempts) + 1,
                put(fwd_retry_attempts, N),
                {ok, _} = elib_pg:query(C, <<"SELECT pg_advisory_xact_lock($1)">>, [First]),
                case N of
                    1 ->
                        Parent ! {locked, self()},
                        receive
                            proceed -> ok
                        after 10000 -> error(timeout)
                        end;
                    _ ->
                        ok
                end,
                case elib_pg:query(C, <<"SELECT pg_advisory_xact_lock($1)">>, [Second]) of
                    {ok, _} -> ok;
                    {error, E} -> throw({rollback, E})
                end
            end)
        end),
        Parent ! {finished, self(), Result, get(fwd_retry_attempts)}
    end,
    A = spawn(fun() -> Run(Base + 501, Base + 502) end),
    B = spawn(fun() -> Run(Base + 502, Base + 501) end),
    receive
        {locked, A} -> ok
    after 10000 -> error(lock_a_timeout)
    end,
    receive
        {locked, B} -> ok
    after 10000 -> error(lock_b_timeout)
    end,
    A ! proceed,
    B ! proceed,
    NA =
        receive
            {finished, A, ok, X} -> X
        after 15000 -> error(finish_a_timeout)
        end,
    NB =
        receive
            {finished, B, ok, Y} -> Y
        after 15000 -> error(finish_b_timeout)
        end,
    ?assert(NA > 1 orelse NB > 1).
