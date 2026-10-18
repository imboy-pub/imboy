-module(attachment_upload_receipt_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

%% 真 PG、全链迁移、真实锁等待和生产仓储；任何前置失败均失败，不跳过。
%% Real PostgreSQL, full migrations and production repositories, without skips.
upload_receipt_test_() ->
    {timeout, 900, fun() -> isolated_pg_fixture:run(fun run/0) end}.

run() ->
    elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3}),
    State = inttest_marker_db:provision(#{
        env_prefix => <<"V5_RECEIPT_INTTEST">>,
        connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
    }),
    C = maps:get(conn, State),
    try
        seed_parent(C),
        start_pool(State),
        try
            ordinary_admission(C),
            confirmed_recheck(C),
            enterprise_admission(C),
            pending_intent_transactions(C, State),
            known_error_recovery(C),
            receipt_race(C, State),
            expiry_while_waiting(C, State),
            pending_intent_orders(C, State),
            confirm_orders(C, State),
            application_confirm_orders(C, State),
            enterprise_intent_orders(C, State),
            permissions_and_down(C)
        after
            pooler:rm_pool(pgsql)
        end
    after
        inttest_marker_db:release(State)
    end.

start_pool(State) ->
    application:set_env(imboy, sql_driver, pgsql),
    {ok, _} = application:ensure_all_started(pooler),
    {ok, _} = pooler:new_pool(#{name => pgsql, init_count => 1, max_count => 1,
        start_mfa => {epgsql, connect, [connection_options(State)]}}).

connection_options(State) ->
    Server = maps:get(server, State),
    maps:merge(maps:with([host, port, username, password], Server),
        (maps:get(connect_extra, Server, #{}))#{database => maps:get(db, State)}).

seed_parent(C) ->
    q(C, <<"INSERT INTO \"user\"(id,password,account,account_type,status,reg_ip,reg_cosv)"
           " VALUES(981001,'x','v5-receipt-user',0,1,'127.0.0.1','test')">>, []),
    q(C, <<"INSERT INTO organization(id,name,owner_id) VALUES(981010,'receipt',981001)">>, []),
    q(C, <<"INSERT INTO workspace(organization_id,id,name,owner_id,branding)"
           " VALUES(981010,981011,'receipt',981001,'{}')">>, []).

ordinary_admission(C) ->
    K = <<"receipt/admission">>,
    seed_pending(C, K, 3600),
    R = receipt(<<"test">>),
    ?assertEqual(ok, attach_pending_repo:register_content_pin(K, R)),
    ?assertEqual(ok, attach_pending_repo:register_content_pin(K, R)),
    ?assertEqual({error, content_mismatch},
        attach_pending_repo:register_content_pin(K, receipt(<<"diff">>))),
    ?assertEqual(R, stored_pin(C, <<"attach_pending">>, <<"object_key">>, K)),
    ?assertEqual({error, not_found},
        attach_pending_repo:register_content_pin(<<"receipt/missing">>, R)),
    lists:foreach(fun({Suffix, Change}) ->
        Key = <<"receipt/invalid-", Suffix/binary>>,
        seed_pending(C, Key, 3600),
        ?assertEqual({error, content_mismatch},
            attach_pending_repo:register_content_pin(Key, maps:merge(R, Change))),
        ?assertEqual(null, stored_pin(C, <<"attach_pending">>, <<"object_key">>, Key))
    end, [{<<"mime">>, #{<<"mime">> => <<"image/png">>}},
          {<<"size">>, #{<<"size_bytes">> => <<"5">>}},
          {<<"shape">>, #{<<"extra">> => true}}]),
    E = <<"receipt/expired">>,
    seed_pending(C, E, -1),
    ?assertEqual({error, session_not_renewable}, attach_pending_repo:register_content_pin(E, R)),
    T = <<"receipt/tombstone">>,
    seed_pending(C, T, 3600),
    tombstone(C, T),
    ?assertEqual({error, tombstone}, attach_pending_repo:register_content_pin(T, R)),
    L = <<"receipt/legacy">>,
    q(C, <<"INSERT INTO attach_pending(object_key,bucket,scope,creator_user_id)"
           " VALUES($1,'b','private',981001)">>, [L]),
    ?assertEqual({error, location_changed}, attach_pending_repo:register_content_pin(L, R)).

confirmed_recheck(C) ->
    K = <<"receipt/confirmed">>,
    seed_pending(C, K, 3600),
    R = receipt(<<"test">>),
    ?assertEqual(ok, attach_pending_repo:register_content_pin(K, R)),
    seed_attachment(C, K, 981101),
    q(C, <<"SELECT fn_v5_confirm_pending($1,981101)">>, [K]),
    ?assertEqual({error, session_not_renewable}, attach_pending_repo:register_content_pin(K, R)),
    ?assertEqual(ok, attach_pending_repo:recheck_content_pin(K, R, identity(K))),
    ?assertEqual({error, content_mismatch},
        attach_pending_repo:recheck_content_pin(K, receipt(<<"diff">>), identity(K))),
    ?assertEqual({error, location_changed}, attach_pending_repo:recheck_content_pin(K, R,
        (identity(K))#{locator_digest => binary:copy(<<"c">>, 64)})),
    q(C, <<"UPDATE attachment SET lifecycle_state='delete_intent' WHERE id=981101">>, []),
    ?assertEqual({error, session_not_renewable},
        attach_pending_repo:recheck_content_pin(K, R, identity(K))).

enterprise_admission(C) ->
    D = seed_enterprise(C, 981201, 3600),
    K = eb_pg_asset_meta:object_key(981010,981011,981201),
    {ok, Row} = eb_pg_asset_meta:reserve_registered_put(981010,981011,D),
    ?assertEqual(receipt(<<"test">>), decode(maps:get(upload_content, Row))),
    {ok, _} = eb_pg_asset_meta:reserve_registered_put(981010,981011,D),
    ?assertEqual({error, content_mismatch}, eb_pg_asset_meta:reserve_registered_put(
        981010,981011,D#{payload => <<"diff">>, object_hash => sha(<<"diff">>)})),
    %% A sealed source carries actual bytes without byte_size(tuple)/whole reads.
    ok = attachment_upload_spool:with_source(<<"test">>, fun(Source) ->
        {ok, _} = eb_pg_asset_meta:reserve_registered_put(981010,981011,D#{payload => Source}),
        ok
    end),
    ?assertEqual({error, invalid_descriptor},
        eb_pg_asset_meta:reserve_registered_put(981010,981011,D#{payload => bad})),
    q(C, <<"UPDATE enterprise_asset SET status='active' WHERE id=981201">>, []),
    ?assertEqual({error, session_not_renewable}, eb_pg_asset_meta:reserve_registered_put(981010,981011,D)),
    ?assertEqual(ok, eb_pg_asset_meta:recheck_registered_put(981010,981011,D,identity(K))),
    ?assertEqual({error, location_changed}, eb_pg_asset_meta:recheck_registered_put(
        981010,981011,D,(identity(K))#{container => <<"other">>})),
    E = seed_enterprise(C, 981202, -1),
    ?assertEqual({error, session_not_renewable}, eb_pg_asset_meta:reserve_registered_put(981010,981011,E)),
    ?assertMatch({ok, [#{<<"ok">> := 1}]}, elib_pg:query(<<"SELECT 1 AS ok">>, [])),
    T = seed_enterprise(C, 981203, 3600),
    q(C, <<"SELECT fn_v5_intent_eb_delete(981010,981011,981203,'test')">>, []),
    ?assertEqual({error, session_not_renewable}, eb_pg_asset_meta:reserve_registered_put(981010,981011,T)),
    enterprise_admission_rollback(C).

%% Both post-pin validation and unknown SQL failures must discard the receipt.
%% pin 后校验拒绝与未知 SQL 错误均不得提交 receipt；独立连接读取最终状态。
enterprise_admission_rollback(C) ->
    Mismatch = seed_enterprise(C, 981204, 3600),
    ?assertEqual({error, content_mismatch}, eb_pg_asset_meta:reserve_registered_put(
        981010, 981011, Mismatch#{conversation_id => 981999})),
    ?assertEqual(null, stored_pin(C, <<"enterprise_asset">>, <<"id">>, 981204)),
    Fault = seed_enterprise(C, 981205, 3600),
    q(C, <<"CREATE FUNCTION receipt_eb_post_pin_fault() RETURNS trigger LANGUAGE plpgsql AS $$"
           " BEGIN RAISE EXCEPTION 'receipt fixture post-pin fault' USING ERRCODE='XX001'; END $$">>, []),
    q(C, <<"CREATE TRIGGER receipt_eb_post_pin_fault AFTER UPDATE OF upload_content ON enterprise_asset"
           " FOR EACH ROW WHEN (NEW.id=981205) EXECUTE FUNCTION receipt_eb_post_pin_fault()">>, []),
    try
        ?assertMatch({error, {sql, <<"XX001">>, _}},
            eb_pg_asset_meta:reserve_registered_put(981010, 981011, Fault)),
        ?assertEqual(null, stored_pin(C, <<"enterprise_asset">>, <<"id">>, 981205)),
        ?assertMatch({ok, [#{<<"ok">> := 1}]}, elib_pg:query(<<"SELECT 1 AS ok">>, []))
    after
        q(C, <<"DROP TRIGGER receipt_eb_post_pin_fault ON enterprise_asset">>, []),
        q(C, <<"DROP FUNCTION receipt_eb_post_pin_fault()">>, [])
    end.

%% The pending-intent repository owns only a savepoint, never the outer commit.
%% 独立观察连接验证提交可见性；测试侧不包 SAVEPOINT、不替生产函数恢复错误。
pending_intent_transactions(C, State) ->
    {ok, Observer} = inttest_marker_db:safe_connect(connection_options(State)),
    try
        q(Observer, <<"SET statement_timeout='5s'">>, []),
        q(C, <<"CREATE TABLE pending_receipt_tx_probe(n integer PRIMARY KEY)">>, []),
        pending_intent_commit(C, Observer),
        pending_intent_rollback(C, Observer),
        pending_intent_requires_transaction(C, Observer)
    after
        epgsql:close(Observer)
    end.

pending_intent_commit(C, Observer) ->
    Key = <<"receipt/transaction-commit">>,
    seed_pending(C, Key, 3600),
    pending_transaction(C, fun() ->
        q(C, <<"INSERT INTO pending_receipt_tx_probe VALUES(1)">>, []),
        ?assertEqual(ok, attach_pending_repo:intent_pending_delete_tx(
            C, Key, <<"reject">>, <<"transaction regression">>)),
        ?assertEqual({<<"delete_intent">>, 1, 1}, pending_snapshot(C, Key)),
        ?assertEqual({<<"registered">>, 0, 0}, pending_snapshot(Observer, Key)),
        ?assertEqual(0, pending_sentinel_count(Observer, 1)),
        q(C, <<"COMMIT">>, []),
        ?assertEqual({ok, commit}, epgsql:get_cmd_status(C))
    end),
    ?assertEqual({<<"delete_intent">>, 1, 1}, pending_snapshot(Observer, Key)),
    ?assertEqual(1, pending_sentinel_count(Observer, 1)).

pending_intent_rollback(C, Observer) ->
    Key = <<"receipt/transaction-rollback">>,
    seed_pending(C, Key, 3600),
    pending_transaction(C, fun() ->
        q(C, <<"INSERT INTO pending_receipt_tx_probe VALUES(2)">>, []),
        ?assertEqual(ok, attach_pending_repo:intent_pending_delete_tx(
            C, Key, <<"reject">>, <<"transaction regression">>)),
        ?assertEqual({<<"delete_intent">>, 1, 1}, pending_snapshot(C, Key)),
        ?assertEqual(1, pending_sentinel_count(C, 2)),
        ?assertEqual({<<"registered">>, 0, 0}, pending_snapshot(Observer, Key)),
        ?assertEqual(0, pending_sentinel_count(Observer, 2))
    end),
    ?assertEqual({<<"registered">>, 0, 0}, pending_snapshot(Observer, Key)),
    ?assertEqual(0, pending_sentinel_count(Observer, 2)).

pending_intent_requires_transaction(C, Observer) ->
    Key = <<"receipt/transaction-required">>,
    seed_pending(C, Key, 3600),
    Sql = <<"SELECT * FROM attach_pending WHERE object_key=$1">>,
    Before = q(C, Sql, [Key]),
    ?assertMatch({error, #error{code = <<"25P01">>}},
        attach_pending_repo:intent_pending_delete_tx(
            C, Key, <<"reject">>, <<"transaction regression">>)),
    ?assertEqual(Before, q(Observer, Sql, [Key])),
    ?assertEqual({<<"registered">>, 0, 0}, pending_snapshot(Observer, Key)),
    ?assertEqual([#{<<"probe">> => 42}], q(C, <<"SELECT 42 AS probe">>, [])).

pending_transaction(C, Fun) ->
    q(C, <<"BEGIN">>, []),
    try
        q(C, <<"SET LOCAL statement_timeout='5s'">>, []),
        Fun()
    after
        q(C, <<"ROLLBACK">>, [])
    end.

pending_snapshot(C, Key) ->
    [Row] = q(C,
        <<"SELECT p.session_state,"
          " (SELECT count(*) FROM attachment_object_deletion WHERE path=$1) AS jobs,"
          " (SELECT count(*) FROM attachment_lifecycle_v5_audit"
          " WHERE object_key=$1 AND action='delete_intent') AS audits"
          " FROM attach_pending p WHERE p.object_key=$1">>, [Key]),
    {maps:get(<<"session_state">>, Row), maps:get(<<"jobs">>, Row),
        maps:get(<<"audits">>, Row)}.

pending_sentinel_count(C, N) ->
    [#{<<"n">> := Count}] = q(C,
        <<"SELECT count(*) AS n FROM pending_receipt_tx_probe WHERE n=$1">>, [N]),
    Count.

known_error_recovery(C) ->
    q(C, <<"CREATE TABLE upload_receipt_tx_probe(n integer PRIMARY KEY)">>, []),
    K = <<"receipt/savepoint-conflict">>,
    seed_pending(C,K,3600),
    T = <<"receipt/savepoint-tombstone">>,
    seed_pending(C,T,3600),
    tombstone(C,T),
    Result = elib_pg:with_tx(fun(Tx) ->
        q(Tx, <<"INSERT INTO upload_receipt_tx_probe VALUES(1)">>, []),
        ?assertEqual({error, not_found}, attach_pending_repo:intent_pending_delete_tx(
            Tx,<<"receipt/missing-intent">>,<<"reject">>,<<"test">>)),
        ?assertEqual({error, intent_conflict}, attach_pending_repo:intent_pending_delete_tx(
            Tx,K,<<"sweep">>,<<"test">>)),
        ?assertEqual({error, tombstone}, attach_pending_repo:intent_pending_delete_tx(
            Tx,T,<<"reject">>,<<"test">>)),
        q(Tx, <<"INSERT INTO upload_receipt_tx_probe VALUES(2)">>, []),
        ok
    end),
    ?assertEqual(ok, Result),
    ?assertEqual([#{<<"n">> => 1},#{<<"n">> => 2}],
        q(C, <<"SELECT n FROM upload_receipt_tx_probe ORDER BY n">>, [])),
    q(C, <<"CREATE FUNCTION receipt_test_fault() RETURNS trigger LANGUAGE plpgsql AS $$"
           " BEGIN RAISE EXCEPTION 'test fault' USING ERRCODE='XX001'; END $$">>, []),
    q(C, <<"CREATE TRIGGER receipt_test_fault BEFORE INSERT ON attachment_lifecycle_v5_audit"
           " FOR EACH ROW EXECUTE FUNCTION receipt_test_fault()">>, []),
    q(C, <<"BEGIN">>, []),
    try
        ?assertMatch({error,#error{code = <<"XX001">>}},
            attach_pending_repo:intent_pending_delete_tx(C,K,<<"reject">>,<<"test">>)),
        ?assertMatch({error,#error{code = <<"25P02">>}},
            elib_pg:query(C, <<"SELECT 1">>, []))
    after
        q(C, <<"ROLLBACK">>, []),
        q(C, <<"DROP TRIGGER receipt_test_fault ON attachment_lifecycle_v5_audit">>, []),
        q(C, <<"DROP FUNCTION receipt_test_fault()">>, [])
    end.

receipt_race(C, State) ->
    K = <<"receipt/concurrent">>,
    seed_pending(C,K,3600),
    q(C, <<"BEGIN">>, []),
    pin(C,K,receipt(<<"test">>)),
    W = worker(State, fun(Tx) -> pin(Tx,K,receipt(<<"diff">>)) end),
    try
        wait_blocked(C,W),
        q(C, <<"COMMIT">>, []),
        ?assertMatch({error,#error{code = <<"23510">>}}, result(W)),
        ?assertEqual(receipt(<<"test">>),stored_pin(C,<<"attach_pending">>,<<"object_key">>,K))
    after q(C, <<"ROLLBACK">>, []) end.

expiry_while_waiting(C, State) ->
    K = <<"receipt/expiry-wait">>,
    seed_pending(C,K,2),
    q(C, <<"BEGIN">>, []),
    q(C, <<"SELECT fn_v5_lock_ordinary_upload($1)">>, [K]),
    W = worker(State, fun(Tx) -> pin(Tx,K,receipt(<<"test">>)) end),
    try
        wait_blocked(C,W),
        until(fun() ->
            [#{<<"expired">> := Expired}] = q(C,
                <<"SELECT upload_expires_at <= clock_timestamp() AS expired FROM attach_pending WHERE object_key=$1">>,[K]),
            Expired
        end),
        q(C, <<"COMMIT">>, []),
        ?assertMatch({error,#error{code = <<"55000">>}}, result(W)),
        ?assertEqual(null,stored_pin(C,<<"attach_pending">>,<<"object_key">>,K))
    after q(C, <<"ROLLBACK">>, []) end.

pending_intent_orders(C, State) ->
    lists:foreach(fun(First) ->
        K = <<"receipt/pending-order-",(atom_to_binary(First))/binary>>,
        seed_pending(C,K,3600),
        q(C, <<"BEGIN">>, []),
        {Next, Expected} = case First of
            pin ->
                pin(C,K,receipt(<<"test">>)),
                {fun(Tx) -> pending_intent(Tx,K) end, success};
            intent ->
                pending_intent(C,K),
                {fun(Tx) -> pin(Tx,K,receipt(<<"test">>)) end, <<"55000">>}
        end,
        W = worker(State,Next),
        try
            wait_blocked(C,W),
            q(C, <<"COMMIT">>, []),
            assert_result(Expected,result(W)),
            ?assertEqual([#{<<"session_state">> => <<"delete_intent">>}],
                q(C, <<"SELECT session_state FROM attach_pending WHERE object_key=$1">>,[K]))
        after q(C, <<"ROLLBACK">>, []) end
    end,[pin,intent]).

confirm_orders(C, State) ->
    lists:foreach(fun({First,Id}) ->
        K = <<"receipt/confirm-order-",(atom_to_binary(First))/binary>>,
        seed_pending(C,K,3600),
        pin(C,K,receipt(<<"test">>)),
        seed_attachment(C,K,Id),
        q(C, <<"BEGIN">>, []),
        {Next, Expected} = case First of
            pin ->
                pin(C,K,receipt(<<"test">>)),
                {fun(Tx) -> elib_pg:query(Tx,<<"SELECT fn_v5_confirm_pending($1,$2)">>,[K,Id]) end,success};
            confirm ->
                q(C,<<"SELECT fn_v5_confirm_pending($1,$2)">>,[K,Id]),
                {fun(Tx) -> pin(Tx,K,receipt(<<"test">>)) end,<<"55000">>}
        end,
        W = worker(State,Next),
        try
            wait_blocked(C,W),
            q(C, <<"COMMIT">>, []),
            assert_result(Expected,result(W)),
            ?assertEqual(ok,attach_pending_repo:recheck_content_pin(K,receipt(<<"test">>),identity(K)))
        after q(C, <<"ROLLBACK">>, []) end
    end,[{pin,981111},{confirm,981112}]).

%% Exercise the actual application repository pre-read, not just the final SQL
%% confirm function. Holding only I exposes the former pending -> I inversion.
application_confirm_orders(C, State) ->
    K1 = <<"eoa/981010/981301/receipt/identity-first">>,
    seed_application(C,K1,981121),
    q(C,<<"BEGIN">>,[]),
    q(C,<<"SELECT fn_v5_lock_identity('disk','receipt-root','default',$1)">>,[K1]),
    W1 = worker(State,fun(Tx) ->
        q(Tx,<<"BEGIN">>,[]),
        {ok,_} = enterprise_asset_repo:pending_find_tx(Tx,K1),
        {ok,_} = attach_pending_repo:lock_session_tx(Tx,K1),
        {ok,981121} = attach_pending_repo:confirm_session_tx(Tx,K1,981121),
        q(Tx,<<"COMMIT">>,[]),
        {ok,981121}
    end),
    try
        wait_blocked(C,W1),
        %% This cannot obtain pending if the waiting application already took
        %% the row. The old order produces a genuine PG deadlock here.
        ?assertMatch({ok,[_]},pin(C,K1,receipt(<<"test">>))),
        q(C,<<"COMMIT">>,[]),
        ?assertEqual({ok,981121},result(W1))
    after q(C,<<"ROLLBACK">>,[]) end,
    K2 = <<"eoa/981010/981301/receipt/application-first">>,
    seed_application(C,K2,981122),
    ?assertMatch({ok,[_]},pin(C,K2,receipt(<<"test">>))),
    q(C,<<"BEGIN">>,[]),
    {ok,_} = enterprise_asset_repo:pending_find_tx(C,K2),
    {ok,_} = attach_pending_repo:lock_session_tx(C,K2),
    W2 = worker(State,fun(Tx) -> pin(Tx,K2,receipt(<<"test">>)) end),
    try
        wait_blocked(C,W2),
        ?assertEqual({ok,981122},attach_pending_repo:confirm_session_tx(C,K2,981122)),
        q(C,<<"COMMIT">>,[]),
        ?assertMatch({error,#error{code = <<"55000">>}},result(W2))
    after q(C,<<"ROLLBACK">>,[]) end.

seed_application(C,K,Id) ->
    Reg = #{<<"subject">> => #{<<"kind">> => <<"application">>,
                <<"principal">> => <<"981301">>,<<"org_id">> => <<"981010">>},
            <<"declaration">> => #{<<"mime">> => <<"text/plain">>,
                <<"size_bytes">> => <<"4">>,<<"scope">> => <<"enterprise">>,
                <<"application_id">> => <<"981301">>,<<"org_id">> => <<"981010">>}},
    q(C,<<"SELECT * FROM fn_v5_register_pending($1,'b','enterprise',0,gen_random_uuid(),"
          "$2::jsonb,clock_timestamp()+interval '2 hours',clock_timestamp()+interval '1 hour',"
          "'disk','receipt-root','default',$3)">>,[K,jsone:encode(Reg),digest()]),
    q(C,<<"INSERT INTO attachment(id,path,scope,scope_ref,creator_user_id,storage_driver,"
          "storage_namespace,storage_container,storage_locator_sha256,gc_verified) VALUES"
          "($1,$2,'enterprise','org:981010/app:981301',0,'disk','receipt-root','default',$3,true)">>,
        [Id,K,digest()]).

enterprise_intent_orders(C, State) ->
    lists:foreach(fun({First,Id}) ->
        D = seed_enterprise(C,Id,3600),
        q(C,<<"BEGIN">>,[]),
        {Next,Expected} = case First of
            pin ->
                eb_pin(C,Id,D),
                {fun(Tx) -> elib_pg:query(Tx,<<"SELECT fn_v5_intent_eb_delete(981010,981011,$1,'test')">>,[Id]) end,success};
            intent ->
                q(C,<<"SELECT fn_v5_intent_eb_delete(981010,981011,$1,'test')">>,[Id]),
                {fun(Tx) -> eb_pin(Tx,Id,D) end,<<"55000">>}
        end,
        W = worker(State,Next),
        try
            wait_blocked(C,W),
            q(C,<<"COMMIT">>,[]),
            assert_result(Expected,result(W)),
            ?assertEqual({error,session_not_renewable},eb_pg_asset_meta:reserve_registered_put(981010,981011,D))
        after q(C,<<"ROLLBACK">>,[]) end
    end,[{pin,981211},{intent,981212}]).

permissions_and_down(C) ->
    K = <<"receipt/runtime-role">>,
    seed_pending(C,K,3600),
    q(C,<<"BEGIN">>,[]),
    try
        q(C,<<"SET LOCAL ROLE v5_app_runtime">>,[]),
        ?assertMatch({ok,[_]},pin(C,K,receipt(<<"test">>)))
    after q(C,<<"ROLLBACK">>,[]) end,
    [#{<<"allowed">> := false}] = q(C,
        <<"SELECT has_function_privilege('v5_app_runtime',"
          "'fn_v5_pending_content_gate(text,jsonb,jsonb,boolean)','EXECUTE') AS allowed">>,[]),
    {ok,Down} = file:read_file("priv/migrations/00000196_attachment_lifecycle_v5_upload_receipt.down.sql"),
    q(C,<<"BEGIN">>,[]),
    try
        ?assertMatch({error,#error{}},epgsql:squery(C,Down))
    after q(C,<<"ROLLBACK">>,[]) end.

seed_pending(C,K,Seconds) ->
    Reg = #{<<"subject">> => #{<<"kind">> => <<"user">>,<<"principal">> => <<"981001">>},
            <<"declaration">> => #{<<"mime">> => <<"text/plain">>,<<"size_bytes">> => <<"4">>,<<"scope">> => <<"private">>}},
    q(C,<<"SELECT * FROM fn_v5_register_pending($1,'b','private',981001,gen_random_uuid(),"
          "$2::jsonb,clock_timestamp()+interval '2 hours',clock_timestamp()+($3::integer*interval '1 second'),"
          "'disk','receipt-root','default',$4)">>,[K,jsone:encode(Reg),Seconds,digest()]).

seed_attachment(C,K,Id) ->
    q(C,<<"INSERT INTO attachment(id,path,scope,creator_user_id,storage_driver,storage_namespace,"
          "storage_container,storage_locator_sha256,gc_verified)"
          " VALUES($1,$2,'private',981001,'disk','receipt-root','default',$3,true)">>,[Id,K,digest()]).

seed_enterprise(C,Id,Seconds) ->
    Subject = #{<<"kind">> => <<"seat">>,<<"principal">> => <<"981001">>},
    Reg = #{<<"subject">> => Subject,<<"declaration">> => #{<<"mime">> => <<"text/plain">>,
        <<"size_bytes">> => 4,<<"object_hash">> => sha(<<"test">>),<<"conversation_id">> => null}},
    K = eb_pg_asset_meta:object_key(981010,981011,Id),
    q(C,<<"INSERT INTO enterprise_asset(id,organization_id,workspace_id,object_key,object_hash,mime,"
          "size_bytes,status,storage_driver,storage_namespace,storage_container,storage_locator_sha256)"
          " VALUES($1,981010,981011,$2,$3,'text/plain',4,'pending_confirm','disk','receipt-root','default',$4)">>,
        [Id,K,sha(<<"test">>),digest()]),
    q(C,<<"SELECT * FROM fn_v5_register_eb(981010,981011,$1,$2,gen_random_uuid(),$3::jsonb,'seat',"
          "clock_timestamp()+interval '2 hours',clock_timestamp()+($4::integer*interval '1 second'))">>,
        [Id,K,jsone:encode(Reg),Seconds]),
    #{id => Id,payload => <<"test">>,object_hash => sha(<<"test">>),mime => <<"text/plain">>,
      size_bytes => 4,conversation_id => null,upload_subject => Subject,
      signature_context => #{expires_at => calendar:system_time_to_universal_time(
          erlang:system_time(second)+Seconds,second)}}.

tombstone(C,K) ->
    q(C,<<"INSERT INTO attachment_object_deletion(path,bucket,storage_driver,storage_namespace,"
          "storage_container,storage_locator_sha256,job_state) VALUES($1,'b','disk','receipt-root','default',$2,'ready')">>,[K,digest()]).

pin(C,K,R) -> elib_pg:query(C,<<"SELECT fn_v5_pin_pending_content($1,$2::jsonb)">>,[K,jsone:encode(R)]).
pending_intent(C,K) -> elib_pg:query(C,<<"SELECT fn_v5_intent_pending_delete($1,'test','reject')">>,[K]).
eb_pin(C,Id,D) -> elib_pg:query(C,
    <<"SELECT fn_v5_pin_eb_content(981010,981011,$1,$2::jsonb,$3::jsonb,to_timestamp($4::double precision))">>,
    [Id,jsone:encode(receipt(<<"test">>)),jsone:encode(maps:get(upload_subject,D)),
     calendar:datetime_to_gregorian_seconds(maps:get(expires_at,maps:get(signature_context,D)))-62167219200]).

receipt(Bytes) -> #{<<"version">> => 1,<<"sha256">> => sha(Bytes),
                    <<"size_bytes">> => integer_to_binary(byte_size(Bytes)),<<"mime">> => <<"text/plain">>}.
sha(Bytes) -> binary:encode_hex(crypto:hash(sha256,Bytes),lowercase).
digest() -> binary:copy(<<"b">>,64).
identity(K) -> #{driver => disk,namespace => <<"receipt-root">>,container => <<"default">>,
                 key => K,locator_digest => digest()}.
decode(B) when is_binary(B) -> jsone:decode(B);
decode(M) -> M.
stored_pin(C,Table,Field,Key) ->
    [#{<<"upload_content">> := Pin}] = q(C,
        [<<"SELECT upload_content FROM ">>,Table,<<" WHERE ">>,Field,<<"=$1">>],[Key]),
    decode(Pin).
q(C,Sql,Params) ->
    case elib_pg:query(C,Sql,Params) of
        {ok,Rows} -> Rows;
        Error -> error({receipt_sql_failed,Sql,Error})
    end.

worker(State,Fun) ->
    Parent = self(), Ref = make_ref(),
    {Pid,Monitor} = spawn_monitor(fun() ->
        {ok,Conn} = inttest_marker_db:safe_connect(connection_options(State)),
        [#{<<"pid">> := Backend}] = q(Conn,<<"SELECT pg_backend_pid() AS pid">>,[]),
        Parent ! {Ref,backend,Backend},
        try Parent ! {Ref,result,Fun(Conn)}
        catch Class:Reason:Stack -> Parent ! {Ref,result,{raised,Class,Reason,Stack}}
        after epgsql:close(Conn) end
    end),
    receive {Ref,backend,Backend} -> {Pid,Monitor,Ref,Backend}
    after 10000 -> error(worker_connect_timeout) end.
wait_blocked(C,{_,_,_,Backend}) ->
    until(fun() ->
        [#{<<"blocked">> := Blocked}] = q(C,
            <<"SELECT cardinality(pg_blocking_pids($1)) > 0 AS blocked">>,[Backend]),
        Blocked
    end).
until(F) -> until(F,erlang:monotonic_time(millisecond)+8000).
until(F,Deadline) ->
    case F() of
        true -> ok;
        false ->
            case erlang:monotonic_time(millisecond)<Deadline of
                true -> timer:sleep(10),until(F,Deadline);
                false -> error(pg_barrier_timeout)
            end
    end.
result({_,Monitor,Ref,_}) ->
    receive
        {Ref,result,{raised,C,R,S}} -> erlang:raise(C,R,S);
        {Ref,result,Result} -> erlang:demonitor(Monitor,[flush]),Result;
        {'DOWN',Monitor,process,_,Reason} -> error({receipt_worker_failed,Reason})
    after 10000 -> error(receipt_worker_timeout) end.
assert_result(success,Result) -> ?assertMatch({ok,[_]},Result);
assert_result(Code,Result) -> ?assertMatch({error,#error{code = Code}},Result).
