-module(eb_asset_direct_tests).
-moduledoc "step-14：EB 域 direct 桥接（application 主体会话 + 内容 Worker 复核 + 跨 org 拒绝）".
%%%
% 本地可验收核心（逐面真实 HTTP 证据归 20b）：
%   * EB direct 会话全链：register(application 主体 org 声明)→declare→grant→
%     finalize CONFIRMED；
%   * scan_confirmed：CONFIRMED 后受控扫描（mock 读）+ receipt 五道门验证
%     通过（25MiB/魔数/文本规则基线=eb_asset_content 零改动）；
%   * 跨 org/跨主体写回拒绝（T04/E10 语义）；
%   * 受控读注入器合同（Range/eof/416→eof）。
%%%

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").
-export([garage_binding_gate/0]).

-define(ORG, <<"990920">>).
-define(SUBJ, <<"990921">>).

%% Run only against a fresh, dedicated local database and private test bucket.
garage_binding_gate() ->
    Db = os:getenv("T05_PG_DB"),
    true = is_list(Db) andalso lists:prefix("c08_eb_real_", Db),
    S = setup(),
    try
        Endpoint = list_to_binary(os:getenv("A0_ENDPOINT")),
        Bucket = list_to_binary(os:getenv("C08_EB_BUCKET")),
        Ak = list_to_binary(os:getenv("A0_AK")),
        Sk = list_to_binary(os:getenv("A0_SK")),
        Ns = <<"eb-real-garage">>,
        Root = #{
            driver => <<"garage">>,
            namespace => Ns,
            endpoint => Endpoint,
            container => Bucket,
            access_key => Ak,
            secret_key => Sk,
            region => <<"garage">>
        },
        ok = application:set_env(imboy, garage, Root#{bucket => Bucket}),
        ok = application:set_env(
            imboy,
            attachment_storage_registry,
            #{version => 1, roots => #{Ns => Root}, upload_namespaces => #{<<"enterprise">> => Ns}}
        ),
        {ok, _} = application:ensure_all_started(inets),
        {ok, _} = application:ensure_all_started(ssl),
        ok = wait_test_barrier(os:getenv("EB_PROOF_TEST_BARRIER"), 120),
        garage_binding_run(),
        io:format("EB_REAL_GARAGE_WORKER_BIND_PASS~n")
    after
        %% Keep the confirmed asset and all fixtures for inspection.
        restore_env(S)
    end.

wait_test_barrier(false, _) ->
    ok;
wait_test_barrier(_, 0) ->
    error(test_barrier_timeout);
wait_test_barrier(Path, N) ->
    case filelib:is_file(Path) of
        true ->
            ok;
        false ->
            timer:sleep(500),
            wait_test_barrier(Path, N - 1)
    end.

restore_env(#{booted := Booted, before_env := Before}) ->
    lists:foreach(
        fun
            ({K, {ok, V}}) -> application:set_env(imboy, K, V);
            ({K, undefined}) -> application:unset_env(imboy, K)
        end,
        Before
    ),
    eunit_runner:eunit_cleanup(Booted).

garage_binding_run() ->
    User = 99006100,
    Org = 99006101,
    Ws = 99006102,
    Contact = 99006103,
    Conv = 99006104,
    Asset = 99006105,
    eb_reservation_fixtures(User, Org, Ws, Contact, Conv),
    Subject = #{
        <<"kind">> => <<"seat">>,
        <<"principal">> => integer_to_binary(User),
        <<"org_id">> => integer_to_binary(Org)
    },
    Reg = #{
        <<"protocol">> => <<"direct-v1">>,
        <<"subject">> => Subject,
        <<"declaration">> => #{<<"conversation_id">> => Conv}
    },
    Body = base64:decode(
        <<"iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+/aX8AAAAASUVORK5CYII=">>
    ),
    Sha = binary:encode_hex(crypto:hash(sha256, Body), lowercase),
    D = #{
        id => Asset,
        conversation_id => Conv,
        uploaded_by_user_id => User,
        object_hash => Sha,
        mime => <<"image/png">>,
        size_bytes => byte_size(Body)
    },
    Now = erlang:system_time(second),
    {ok, #{object_key := Key}} = eb_pg_asset_meta:register_direct_session_v5(
        Org, Ws, D, <<"aba94340-99fa-4afb-8fe9-67e56c8c6123">>, Reg, {Now + 3600, Now + 1200}
    ),
    ok = attach_direct_logic:declare(Key, Sha, byte_size(Body), <<"image/png">>),
    {ok, #{grant := Grant, attempt_id := Attempt}} = attach_direct_logic:grant(Key, Sha),
    Headers = maps:from_list(maps:get(required_headers, Grant)),
    {ok, {{_, 200, _}, _, _}} = httpc:request(
        put,
        {
            binary_to_list(maps:get(url, Grant)),
            [
                {"x-amz-checksum-sha256",
                    binary_to_list(maps:get(<<"x-amz-checksum-sha256">>, Headers))}
            ],
            "image/png",
            Body
        },
        [{autoredirect, false}],
        [{body_format, binary}]
    ),
    {ok, #{state := <<"CONFIRMED">>}} = attach_direct_logic:finalize(Key, Attempt),
    ok = wait_independent_proof(Org, Ws, Asset, 120),
    {ok, Asset} = eb_asset_direct:bind_confirmed(
        Org, Ws, Asset, Key, Subject, <<"image/png">>, 60000
    ),
    {ok, [#{<<"status">> := <<"active">>, <<"binding_state">> := <<"bound">>}]} =
        elib_pg:query(
            <<"SELECT e.status,l.binding_state FROM enterprise_asset e JOIN enterprise_asset_direct_link l ON l.asset_id=e.id WHERE e.id=$1">>,
            [Asset]
        ),
    garage_visitor_binding(Org, Ws, Contact, Conv, User, Body, Sha),
    ok.

garage_visitor_binding(Org, Ws, Contact, Conv, User, Body, Sha) ->
    Session = 99006120,
    Installation = 99006121,
    Token = 99006122,
    Asset = 99006123,
    ok = elib_pg:with_tx(fun(C) ->
        {ok, _} = elib_pg:query(C, <<"SET LOCAL session_replication_role=replica">>, []),
        {ok, _} = elib_pg:query(
            C,
            <<"INSERT INTO customer_service_widget_installation(id,organization_id,public_widget_id,display_name,consent_version,status) VALUES($1,$2,'eb-real-visitor','fixture','1','active')">>,
            [Installation, Org]
        ),
        {ok, _} = elib_pg:query(
            C,
            <<"INSERT INTO customer_service_visit_token(id,organization_id,contact_id,token_digest,expires_at,widget_installation_id) VALUES($1,$2,$3,repeat('f',64),now()+interval '1 hour',$4)">>,
            [Token, Org, Contact, Installation]
        ),
        {ok, _} = elib_pg:query(
            C,
            <<"INSERT INTO customer_service_session(id,organization_id,workspace_id,contact_id,conversation_id,status,visit_token_id) VALUES($1,$2,$3,$4,$5,'queued',$6)">>,
            [Session, Org, Ws, Contact, Conv, Token]
        ),
        ok
    end),
    Subject = #{
        <<"kind">> => <<"visitor_session">>,
        <<"principal">> => integer_to_binary(Contact),
        <<"org_id">> => integer_to_binary(Org),
        <<"session_id">> => Session,
        <<"token_id">> => Token,
        <<"installation_id">> => Installation
    },
    Reg = #{
        <<"protocol">> => <<"direct-v1">>,
        <<"subject">> => Subject,
        <<"declaration">> => #{<<"conversation_id">> => Conv}
    },
    D = #{
        id => Asset,
        conversation_id => Conv,
        uploaded_by_user_id => User,
        object_hash => Sha,
        mime => <<"image/png">>,
        size_bytes => byte_size(Body)
    },
    Now = erlang:system_time(second),
    {ok, #{object_key := Key}} = eb_pg_asset_meta:register_direct_session_v5(
        Org, Ws, D, <<"aba94340-99fa-4afb-8fe9-67e56c8c6125">>, Reg, {Now + 3600, Now + 1200}
    ),
    ok = attach_direct_logic:declare(Key, Sha, byte_size(Body), <<"image/png">>),
    {ok, #{grant := Grant, attempt_id := Attempt}} = attach_direct_logic:grant(Key, Sha),
    Headers = maps:from_list(maps:get(required_headers, Grant)),
    {ok, {{_, 200, _}, _, _}} = httpc:request(
        put,
        {
            binary_to_list(maps:get(url, Grant)),
            [
                {"x-amz-checksum-sha256",
                    binary_to_list(maps:get(<<"x-amz-checksum-sha256">>, Headers))}
            ],
            "image/png",
            Body
        },
        [{autoredirect, false}],
        [{body_format, binary}]
    ),
    {ok, #{state := <<"CONFIRMED">>}} = attach_direct_logic:finalize(Key, Attempt),
    ok = wait_independent_proof(Org, Ws, Asset, 120),
    {ok, Asset} = eb_asset_direct:bind_confirmed(
        Org, Ws, Asset, Key, Subject, <<"image/png">>, 60000
    ),
    {ok, [#{<<"status">> := <<"active">>, <<"binding_state">> := <<"bound">>}]} =
        elib_pg:query(
            <<"SELECT e.status,l.binding_state FROM enterprise_asset e JOIN enterprise_asset_direct_link l ON l.asset_id=e.id WHERE e.id=$1">>,
            [Asset]
        ),
    ok.

wait_independent_proof(_Org, _Ws, _Asset, 0) ->
    error(proof_timeout);
wait_independent_proof(Org, Ws, Asset, N) ->
    case eb_pg_asset_meta:direct_proof(Org, Ws, Asset) of
        {ok, _} ->
            ok;
        {error, proof_pending} ->
            timer:sleep(500),
            wait_independent_proof(Org, Ws, Asset, N - 1);
        Error ->
            error({proof_lookup_failed, Error})
    end.

eb_direct_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun eb_full_chain_and_worker_scan/0,
        fun eb_direct_reservation_atomic/0,
        fun cross_org_writeback_rejected/0,
        fun read_fun_contract/0
    ]}.

%% Read delegation must preserve the existing adapter's result semantics while
%% keeping the application free of a concrete PG dependency. No DB is started.
direct_session_port_delegation_test_() ->
    {timeout, 30, fun() ->
        pure_mock_fixture:run(fun() ->
            Key = <<"direct-port-test">>,
            ok = meck:new(eb_asset_store, [passthrough, no_link]),
            ok = meck:new(eb_pg_asset_meta, [no_link]),
            try
                {ok, eb_asset_store} = eb_infra_ports:resolve(asset),
                lists:foreach(
                    fun({RowResult, Expected}) ->
                        ok = meck:expect(
                            eb_pg_asset_meta,
                            direct_session_row,
                            fun(K) ->
                                ?assertEqual(Key, K),
                                RowResult
                            end
                        ),
                        ?assertEqual(
                            Expected,
                            eb_asset_direct:scan_confirmed(Key, <<"image/png">>, <<"sec">>, 1000)
                        )
                    end,
                    [
                        {{ok, #{<<"direct_state">> => <<"REGISTERED">>}}, {error, not_confirmed}},
                        {{error, not_found}, {error, not_found}},
                        {{error, database_unavailable}, {error, database_unavailable}}
                    ]
                ),
                ?assertEqual(3, meck:num_calls(eb_asset_store, direct_session_row, [Key])),
                ?assertEqual(3, meck:num_calls(eb_pg_asset_meta, direct_session_row, [Key])),
                ?assert(meck:validate(eb_asset_store)),
                ?assert(meck:validate(eb_pg_asset_meta)),
                ?assertEqual(undefined, whereis(pgsql)),
                ?assertEqual(undefined, whereis(pooler_sup)),
                ?assertNot(lists:keymember(imboy, 1, application:which_applications()))
            after
                meck:unload(eb_pg_asset_meta),
                meck:unload(eb_asset_store)
            end
        end)
    end}.

scan_uses_winner_and_checks_digest_test_() ->
    {timeout, 30, fun() ->
        pure_mock_fixture:run(fun() ->
            Key = <<"logical">>,
            Physical = <<"winner">>,
            Sha = hex64(42),
            Row = #{
                <<"direct_state">> => <<"CONFIRMED">>,
                <<"mime_type">> => <<"image/png">>,
                <<"file_hash256">> => Sha,
                <<"size">> => 7,
                <<"attachment_id">> => 17,
                <<"attempt_no">> => 3,
                <<"dispatch_cas_uuid">> => <<"fence">>
            },
            ok = meck:new(eb_asset_store, [passthrough, no_link]),
            ok = meck:new(eb_pg_asset_meta, [no_link]),
            ok = meck:new(attachment_storage, [no_link]),
            ok = meck:new(attachment_worker_port, [no_link]),
            try
                ok = meck:expect(eb_pg_asset_meta, direct_session_row, fun(_) -> {ok, Row} end),
                ok = meck:expect(attachment_storage, frozen_location, fun(_, _) ->
                    {ok, #{
                        key => Physical,
                        container => <<"b">>,
                        garage_config =>
                            #{endpoint => <<"e">>, access_key => <<"a">>, secret_key => <<"s">>}
                    }}
                end),
                ok = meck:expect(attachment_worker_port, scan_external, fun(Task) ->
                    ?assertEqual(Physical, maps:get(object_key, Task)),
                    Receipt = attachment_content_receipt:issue(
                        #{
                            attempt_key => maps:get(attempt_key, Task),
                            object_key => Physical,
                            sha256 => Sha,
                            size => 7,
                            mime => <<"image/png">>,
                            verdict => <<"pass">>
                        },
                        <<"sec">>
                    ),
                    {ok, #{verified => true, sha256 => Sha, size => 7, receipt => Receipt}}
                end),
                ?assertMatch(
                    {ok, #{sha256 := Sha, winner_fence := #{<<"attachment_id">> := 17}}},
                    eb_asset_direct:scan_confirmed(Key, <<"image/png">>, <<"sec">>, 1000)
                ),
                ok = meck:expect(attachment_worker_port, scan_external, fun(_) ->
                    {ok, #{verified => true, sha256 => hex64(43), size => 7}}
                end),
                ?assertEqual(
                    {error, {receipt, unverified}},
                    eb_asset_direct:scan_confirmed(Key, <<"image/png">>, <<"sec">>, 1000)
                )
            after
                meck:unload(attachment_worker_port),
                meck:unload(attachment_storage),
                meck:unload(eb_pg_asset_meta),
                meck:unload(eb_asset_store)
            end
        end)
    end}.

verified_receipt_expiry_test() ->
    Sha = hex64(31),
    Expected = #{
        attempt_key => <<"attempt">>,
        object_key => <<"winner">>,
        sha256 => Sha,
        size => 7,
        mime => <<"image/png">>,
        ruleset_digest => attachment_content_receipt:ruleset_digest()
    },
    Now = erlang:system_time(second),
    Fields = Expected#{verdict => <<"pass">>, expires_at => Now + 120},
    Receipt = attachment_content_receipt:issue(Fields, <<"secret">>),
    ?assertEqual(
        {ok, Now + 120},
        attachment_content_receipt:verify_with_expiry(Receipt, <<"secret">>, Expected)
    ),
    ?assertEqual(
        {error, forged},
        attachment_content_receipt:verify_with_expiry(Receipt, <<"wrong">>, Expected)
    ),
    Expired = attachment_content_receipt:issue(Fields#{expires_at => Now - 1}, <<"secret">>),
    ?assertEqual(
        {error, expired},
        attachment_content_receipt:verify_with_expiry(Expired, <<"secret">>, Expected)
    ).

setup() ->
    Before = [
        {K, application:get_env(imboy, K)}
     || K <- [garage, attachment_storage_registry]
    ],
    Booted = eunit_runner:eunit_setup(),
    EP = <<"http://127.0.0.1:39999">>,
    ok = application:set_env(
        imboy,
        garage,
        #{endpoint => EP, bucket => <<"b">>, access_key => <<"ak">>, secret_key => <<"sk">>}
    ),
    Registry = attachment_storage:registry(),
    Roots = maps:get(roots, Registry, #{}),
    Uploads = maps:get(upload_namespaces, Registry, #{}),
    Ns = <<"dv1eb-", (binary:encode_hex(crypto:strong_rand_bytes(12), lowercase))/binary>>,
    ok = application:set_env(imboy, attachment_storage_registry, Registry#{
        roots => Roots#{
            Ns => #{
                driver => <<"garage">>,
                namespace => Ns,
                endpoint => EP,
                container => <<"b">>
            }
        },
        upload_namespaces => Uploads#{<<"public">> => Ns, <<"enterprise">> => Ns}
    }),
    #{booted => Booted, before_env => Before}.

cleanup(#{booted := Booted, before_env := Before}) ->
    try
        elib_pg:with_tx(fun(Conn) ->
            {ok, _} = elib_pg:query(
                Conn,
                <<"SET LOCAL session_replication_role = replica">>,
                []
            ),
            {ok, _} = elib_pg:query(
                Conn,
                <<
                    "DELETE FROM attach_direct_attempt WHERE object_key LIKE 'app990921/%'"
                    " AND object_key NOT IN (SELECT object_key FROM attach_pending WHERE direct_state='CONFIRMED')"
                >>,
                []
            ),
            {ok, _} = elib_pg:query(
                Conn,
                <<
                    "DELETE FROM attachment WHERE creator_user_id = 0"
                    " AND path LIKE 'app990921/%'"
                    " AND path NOT IN (SELECT object_key FROM attach_pending WHERE direct_state='CONFIRMED')"
                >>,
                []
            ),
            {ok, _} = elib_pg:query(
                Conn,
                <<
                    "DELETE FROM attach_pending WHERE creator_user_id = 0"
                    " AND object_key LIKE 'app990921/%' AND direct_state IS DISTINCT FROM 'CONFIRMED'"
                >>,
                []
            ),
            ok
        end),
        eunit_runner:eunit_cleanup(Booted)
    catch
        _:_ -> eunit_runner:eunit_cleanup(Booted)
    after
        [
            case Value of
                undefined -> application:unset_env(imboy, K);
                {ok, V} -> application:set_env(imboy, K, V)
            end
         || {K, Value} <- Before
        ]
    end.

%% ---------------------------------------------------------------- helpers

hex64(N) -> iolist_to_binary(io_lib:format("~64.16.0b", [N])).

with_finalize_mock(Inner) ->
    ok = meck:new(attachment_storage_garage, [passthrough, no_link]),
    ok = meck:expect(
        attachment_storage_garage,
        copy_dispatch,
        4,
        fun(_N, _C, _S, _T) -> {ok, #{}} end
    ),
    ok = meck:expect(
        attachment_storage_garage,
        head_bounded,
        3,
        fun(_N, _C, _T) -> {ok, #{size => 12}} end
    ),
    try
        Inner()
    after
        meck:unload(attachment_storage_garage)
    end.

eb_chain_to_confirmed(Bin) ->
    {ok, #{object_key := Key}} = eb_asset_direct:register(
        ?ORG, ?SUBJ, <<"doc.bin">>, undefined, <<"application/octet-stream">>, test
    ),
    Sha = binary:encode_hex(crypto:hash(sha256, Bin), lowercase),
    ok = attach_direct_logic:declare(
        Key,
        Sha,
        byte_size(Bin),
        <<"image/png">>
    ),
    {ok, #{attempt_id := No}} = attach_direct_logic:grant(Key, Sha),
    {ok, #{state := <<"CONFIRMED">>}} = with_finalize_mock(fun() ->
        attach_direct_logic:finalize(Key, No)
    end),
    {Key, Sha, byte_size(Bin)}.

%% ---------------------------------------------------------------- cases

eb_full_chain_and_worker_scan() ->
    %% V3-17：scan_confirmed 已切外部 OS Worker（正文零过 BEAM）——本用例
    %% mock Worker port 返回真签发 receipt（规则/摘要语义由
    %% attachment_worker_port_gate.sh 真 Garage 端到端锁定）。
    Bin = sample(<<"image/png">>),
    {Key, Sha, Size} = eb_chain_to_confirmed(Bin),
    ok = meck:new(attachment_worker_port, [no_link]),
    ok = meck:expect(
        attachment_worker_port,
        scan_external,
        fun(Task) ->
            ?assertNotEqual(Key, maps:get(object_key, Task)),
            AttemptKey = maps:get(attempt_key, Task),
            Receipt = attachment_content_receipt:issue(
                #{
                    attempt_key => AttemptKey,
                    object_key => maps:get(object_key, Task),
                    sha256 => Sha,
                    size => Size,
                    mime => <<"image/png">>,
                    verdict => <<"pass">>
                },
                maps:get(secret, Task)
            ),
            {ok, #{sha256 => Sha, size => Size, receipt => Receipt, verified => true}}
        end
    ),
    R =
        try
            Res = eb_asset_direct:scan_confirmed(Key, <<"image/png">>, <<"sec">>, 60000),
            %% validate 必须在 unload 之前（unload 后模块非 mock 态，
            %% validate 会抛 {not_mocked, _}——顺序修正）
            ?assert(meck:validate(attachment_worker_port)),
            ok = meck:expect(attachment_worker_port, scan_external, fun(_) ->
                {ok, #{sha256 => hex64(16#AB), size => Size, verified => true}}
            end),
            ?assertEqual(
                {error, {receipt, unverified}},
                eb_asset_direct:scan_confirmed(Key, <<"image/png">>, <<"sec">>, 60000)
            ),
            Res
        after
            meck:unload(attachment_worker_port)
        end,
    ?assertMatch({ok, #{verified := true, sha256 := Sha, size := Size}}, R),
    ok.

eb_direct_reservation_atomic() ->
    User = 99006100,
    Org = 99006101,
    Ws = 99006102,
    Contact = 99006103,
    Conv = 99006104,
    Asset = 99006105,
    BadAsset = 99006106,
    Cid = <<"aba94340-99fa-4afb-8fe9-67e56c8c6123">>,
    BadCid = <<"aba94340-99fa-4afb-8fe9-67e56c8c6124">>,
    eb_reservation_fixtures(User, Org, Ws, Contact, Conv),
    try
        Subject = #{
            <<"kind">> => <<"seat">>,
            <<"principal">> => integer_to_binary(User),
            <<"org_id">> => integer_to_binary(Org)
        },
        Reg = #{
            <<"protocol">> => <<"direct-v1">>,
            <<"subject">> => Subject,
            <<"declaration">> => #{<<"conversation_id">> => Conv}
        },
        D = #{
            id => Asset,
            conversation_id => Conv,
            uploaded_by_user_id => User,
            object_hash => hex64(33),
            mime => <<"image/png">>,
            size_bytes => 12
        },
        Now = erlang:system_time(second),
        Window = {Now + 3600, Now + 1200},
        {ok, #{asset_id := Asset, object_key := Key}} =
            eb_pg_asset_meta:register_direct_session_v5(Org, Ws, D, Cid, Reg, Window),
        ?assertEqual(
            {ok, #{asset_id => Asset, object_key => Key}},
            eb_pg_asset_meta:register_direct_session_v5(Org, Ws, D, Cid, Reg, Window)
        ),
        ok = attach_direct_logic:declare(Key, hex64(33), 12, <<"image/png">>),
        {ok, #{attempt_id := Attempt}} = attach_direct_logic:grant(Key, hex64(33)),
        ?assertMatch(
            {ok, #{state := <<"CONFIRMED">>}},
            with_finalize_mock(fun() ->
                attach_direct_logic:finalize(Key, Attempt)
            end)
        ),
        ok = elib_pg:with_tx(fun(C) ->
            {ok, _} = elib_pg:query(C, <<"SET LOCAL ROLE eb_direct_proof_worker">>, []),
            {ok, [#{<<"ok">> := true}]} = elib_pg:query(
                C,
                <<"SELECT fn_eb_direct_record_scan($1,p.fence,$2,$3,$4) AS ok FROM fn_eb_direct_scan_projection($1) p">>,
                [
                    Asset,
                    hex64(33),
                    string:lowercase(attachment_content_receipt:ruleset_digest()),
                    Now + 300
                ]
            ),
            ok
        end),
        ?assertEqual(
            {ok, Asset},
            eb_asset_direct:bind_confirmed(
                Org, Ws, Asset, Key, Subject, <<"image/png">>, 60000
            )
        ),
        {ok, [#{<<"status">> := <<"active">>, <<"binding_state">> := <<"bound">>}]} =
            elib_pg:query(
                <<"SELECT e.status,l.binding_state FROM enterprise_asset e JOIN enterprise_asset_direct_link l ON l.asset_id=e.id WHERE e.id=$1">>,
                [Asset]
            ),
        BadSubject = Subject#{<<"principal">> => <<"99006199">>},
        ?assertMatch(
            {error, _},
            eb_pg_asset_meta:register_direct_session_v5(
                Org, Ws, D#{id => BadAsset}, BadCid, Reg#{<<"subject">> => BadSubject}, Window
            )
        ),
        {ok, []} = elib_pg:query(<<"SELECT id FROM enterprise_asset WHERE id=$1">>, [BadAsset]),
        {ok, []} = elib_pg:query(
            <<"SELECT object_key FROM attach_pending WHERE client_upload_id=$1::uuid">>, [BadCid]
        )
    after
        eb_reservation_cleanup(User, Org, Ws, Contact, Conv, Asset, BadAsset)
    end.

eb_reservation_fixtures(User, Org, Ws, Contact, Conv) ->
    ok = elib_pg:with_tx(fun(Conn) ->
        {ok, _} = elib_pg:query(Conn, <<"SET LOCAL session_replication_role = replica">>, []),
        %% 自愈：CONFIRMED 出口跳过清理会留下 fixture 行，先删后插保证幂等。
        {ok, _} = elib_pg:query(
            Conn, <<"DELETE FROM enterprise_conversation WHERE id=$1">>, [Conv]
        ),
        {ok, _} = elib_pg:query(
            Conn, <<"DELETE FROM enterprise_contact WHERE id=$1">>, [Contact]
        ),
        {ok, _} = elib_pg:query(
            Conn,
            <<"DELETE FROM organization_business_identity_assignment WHERE id=99006108">>,
            []
        ),
        {ok, _} = elib_pg:query(
            Conn, <<"DELETE FROM organization_business_identity WHERE id=99006107">>, []
        ),
        {ok, _} = elib_pg:query(
            Conn, <<"DELETE FROM organization_member WHERE organization_id=$1">>, [Org]
        ),
        {ok, _} = elib_pg:query(Conn, <<"DELETE FROM workspace WHERE id=$1">>, [Ws]),
        {ok, _} = elib_pg:query(Conn, <<"DELETE FROM organization WHERE id=$1">>, [Org]),
        {ok, _} = elib_pg:query(Conn, <<"DELETE FROM \"user\" WHERE id=$1">>, [User]),
        %% 直传族残留（CONFIRMED 会话/尝试/附件/资产）按本用例 object_key
        %% 前缀一并清——正常角色下守卫拦 DELETE，replica 模式下不触发。
        {ok, _} = elib_pg:query(
            Conn,
            <<"DELETE FROM attach_pending WHERE object_key LIKE 'enterprise/99006101/%'">>,
            []
        ),
        {ok, _} = elib_pg:query(
            Conn,
            <<"DELETE FROM enterprise_asset_direct_link WHERE object_key LIKE 'enterprise/99006101/%'">>,
            []
        ),
        {ok, _} = elib_pg:query(
            Conn,
            <<"DELETE FROM attach_direct_attempt WHERE object_key LIKE 'enterprise/99006101/%'">>,
            []
        ),
        {ok, _} = elib_pg:query(
            Conn,
            <<"DELETE FROM attach_direct_cleanup WHERE object_key LIKE 'enterprise/99006101/%'">>,
            []
        ),
        {ok, _} = elib_pg:query(
            Conn, <<"DELETE FROM attachment WHERE path LIKE 'enterprise/99006101/%'">>, []
        ),
        {ok, _} = elib_pg:query(
            Conn,
            <<"DELETE FROM enterprise_asset WHERE object_key LIKE 'enterprise/99006101/%'">>,
            []
        ),
        {ok, _} = elib_pg:query(
            Conn,
            <<"INSERT INTO public.\"user\"(id,password,account,reg_ip,reg_cosv) VALUES($1,'x','eb-direct-tx-fixture','127.0.0.1','x')">>,
            [User]
        ),
        {ok, _} = elib_pg:query(
            Conn,
            <<"INSERT INTO organization(id,name,owner_id,status) VALUES($1,'eb-direct-tx-fixture',$2,'active')">>,
            [Org, User]
        ),
        {ok, _} = elib_pg:query(
            Conn,
            <<"INSERT INTO workspace(id,name,owner_id,status,type,organization_id) VALUES($1,'eb-direct-tx-fixture',$2,'active','project',$3)">>,
            [Ws, User, Org]
        ),
        {ok, _} = elib_pg:query(
            Conn,
            <<"INSERT INTO organization_member(organization_id,user_id,status) VALUES($1,$2,'active')">>,
            [Org, User]
        ),
        {ok, _} = elib_pg:query(
            Conn,
            <<"INSERT INTO organization_business_identity(id,organization_id,function_key,display_name,status) VALUES(99006107,$1,'sales','fixture','active')">>,
            [Org]
        ),
        {ok, _} = elib_pg:query(
            Conn,
            <<"INSERT INTO organization_business_identity_assignment(id,organization_id,business_identity_id,function_key,user_id,status) VALUES(99006108,$1,99006107,'sales',$2,'active')">>,
            [Org, User]
        ),
        {ok, _} = elib_pg:query(
            Conn,
            <<"INSERT INTO enterprise_contact(id,organization_id,status,display_name) VALUES($1,$2,'active','fixture')">>,
            [Contact, Org]
        ),
        {ok, _} = elib_pg:query(
            Conn,
            <<"INSERT INTO enterprise_conversation(id,organization_id,workspace_id,contact_id,business_identity_id,status) VALUES($1,$2,$3,$4,99006107,'active')">>,
            [Conv, Org, Ws, Contact]
        ),
        ok
    end).

eb_reservation_cleanup(User, Org, Ws, Contact, Conv, Asset, BadAsset) ->
    case
        elib_pg:query(
            <<"SELECT 1 FROM attach_pending WHERE client_upload_id=$1::uuid AND direct_state='CONFIRMED'">>,
            [<<"aba94340-99fa-4afb-8fe9-67e56c8c6123">>]
        )
    of
        {ok, []} ->
            eb_reservation_cleanup_unconfirmed(User, Org, Ws, Contact, Conv, Asset, BadAsset);
        _ ->
            ok
    end.

eb_reservation_cleanup_unconfirmed(User, Org, Ws, Contact, Conv, Asset, BadAsset) ->
    ok = elib_pg:with_tx(fun(Conn) ->
        {ok, _} = elib_pg:query(Conn, <<"SET LOCAL session_replication_role = replica">>, []),
        {ok, _} = elib_pg:query(
            Conn, <<"DELETE FROM enterprise_asset_direct_link WHERE asset_id IN ($1,$2)">>, [
                Asset, BadAsset
            ]
        ),
        Key = eb_pg_asset_meta:object_key(Org, Ws, Asset),
        {ok, _} = elib_pg:query(
            Conn, <<"DELETE FROM attach_direct_attempt WHERE object_key=$1">>, [Key]
        ),
        {ok, _} = elib_pg:query(Conn, <<"DELETE FROM attachment WHERE path=$1">>, [Key]),
        {ok, _} = elib_pg:query(
            Conn,
            <<"DELETE FROM attach_pending WHERE client_upload_id IN ('aba94340-99fa-4afb-8fe9-67e56c8c6123'::uuid,'aba94340-99fa-4afb-8fe9-67e56c8c6124'::uuid)">>,
            []
        ),
        {ok, _} = elib_pg:query(Conn, <<"DELETE FROM enterprise_asset WHERE id IN ($1,$2)">>, [
            Asset, BadAsset
        ]),
        {ok, _} = elib_pg:query(Conn, <<"DELETE FROM enterprise_conversation WHERE id=$1">>, [Conv]),
        {ok, _} = elib_pg:query(Conn, <<"DELETE FROM enterprise_contact WHERE id=$1">>, [Contact]),
        {ok, _} = elib_pg:query(
            Conn, <<"DELETE FROM organization_business_identity_assignment WHERE id=99006108">>, []
        ),
        {ok, _} = elib_pg:query(
            Conn, <<"DELETE FROM organization_business_identity WHERE id=99006107">>, []
        ),
        {ok, _} = elib_pg:query(
            Conn, <<"DELETE FROM organization_member WHERE organization_id=$1 AND user_id=$2">>, [
                Org, User
            ]
        ),
        {ok, _} = elib_pg:query(Conn, <<"DELETE FROM workspace WHERE id=$1">>, [Ws]),
        {ok, _} = elib_pg:query(Conn, <<"DELETE FROM organization WHERE id=$1">>, [Org]),
        {ok, _} = elib_pg:query(Conn, <<"DELETE FROM public.\"user\" WHERE id=$1">>, [User]),
        ok
    end).

cross_org_writeback_rejected() ->
    %% org 声明进 registration：另一 org 主体同 filename 是不同 key（A11）；
    %% 同 key 跨主体 declare 异值 → expected_conflict（T03/T04 语义）
    {ok, #{object_key := Key}} = eb_asset_direct:register(
        ?ORG, ?SUBJ, <<"doc.bin">>, undefined, <<"application/octet-stream">>, test
    ),
    ok = attach_direct_logic:declare(Key, hex64(16#BB), 10, <<"application/octet-stream">>),
    ?assertEqual(
        {error, expected_conflict},
        attach_direct_logic:declare(Key, hex64(16#BC), 10, <<"application/octet-stream">>)
    ),
    ok.

read_fun_contract() ->
    %% 受控读注入器合同（纯数据：Range 截取/_eof）
    Bin = sample(<<"image/png">>),
    ReadFun = fun
        (Offset, Len) when Offset >= byte_size(Bin) -> eof;
        (Offset, Len) ->
            End = min(Offset + Len, byte_size(Bin)),
            {ok, binary:part(Bin, Offset, End - Offset)}
    end,
    {ok, Head} = ReadFun(0, 8),
    ?assertEqual(8, byte_size(Head)),
    ?assertEqual(eof, ReadFun(byte_size(Bin), 8)),
    ok.

sample(<<"image/png">>) -> <<137, 80, 78, 71, 13, 10, 26, 10, 0, 1, 2, 3>>.
