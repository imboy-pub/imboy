-module(eb_direct_purge_mixed_pg_tests).
-include_lib("eunit/include/eunit.hrl").

mixed_purge_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> {timeout, 90, fun mixed_purge/0} end}.

%% 直传链（register→finalize）需要 garage/registry env 指向可解析的 root
%% （endpoint 为假值即可：finalize 的服务端 copy/head 由 with_storage_mock
%% 拦截，不触真实存储）。与 eb_asset_direct_tests:setup/0 同构，测试后还原。
setup() ->
    Before = [
        {K, application:get_env(imboy, K)}
     || K <- [garage, attachment_storage_registry]
    ],
    {ok, ConnPid} = eunit_runner:eunit_setup_with_db(),
    EP = <<"http://127.0.0.1:39999">>,
    ok = application:set_env(
        imboy, garage, #{
            endpoint => EP, bucket => <<"b">>, access_key => <<"ak">>, secret_key => <<"sk">>
        }
    ),
    Registry = attachment_storage:registry(),
    Roots = maps:get(roots, Registry, #{}),
    Uploads = maps:get(upload_namespaces, Registry, #{}),
    Ns = <<"dv1eb-purge-fixture">>,
    ok = application:set_env(imboy, attachment_storage_registry, Registry#{
        roots => Roots#{
            Ns => #{driver => <<"garage">>, namespace => Ns, endpoint => EP, container => <<"b">>}
        },
        upload_namespaces => Uploads#{<<"public">> => Ns, <<"enterprise">> => Ns}
    }),
    {ConnPid, Before}.

cleanup({ConnPid, Before}) ->
    [
        case V of
            undefined -> application:unset_env(imboy, K);
            _ -> application:set_env(imboy, K, V)
        end
     || {K, V} <- Before
    ],
    eunit_runner:eunit_cleanup_db(ConnPid).

mixed_purge() ->
    Org = 99006101,
    Ws = 99006102,
    Asset = 99006105,
    Deferred = 99006301,
    Ready = 99006302,
    ok = seed_direct_asset_fixture(Org, Ws, Asset),
    %% The clone is dedicated to this test: seed synthetic rows with all
    %% production guards re-enabled before the actual purge transaction.
    ok = elib_pg:with_tx(fun(Conn) ->
        {ok, _} = elib_pg:query(Conn, <<"SET LOCAL session_replication_role=replica">>, []),
        %% 自愈：克隆库若被复用，上一 run 的固定 id 行会撞唯一键/attempts
        %% 累积——先删后插保证幂等。
        {ok, _} = elib_pg:execute(
            Conn, <<"DELETE FROM enterprise_message WHERE id IN ($1,$2)">>, [Deferred, Ready]
        ),
        {ok, _} = elib_pg:execute(
            Conn,
            <<"DELETE FROM enterprise_direct_purge_wait WHERE item_kind='message' AND item_id IN ($1,$2)">>,
            [Deferred, Ready]
        ),
        {ok, _} = elib_pg:execute(
            Conn,
            <<
                "INSERT INTO enterprise_message(id,organization_id,workspace_id,conversation_id,"
                "sender_type,sender_contact_id,client_msg_id,retention_days,retain_until)"
                " SELECT $1,c.organization_id,c.workspace_id,c.id,'contact',c.contact_id,$2,1,now()-interval '1 hour'"
                " FROM enterprise_conversation c JOIN enterprise_asset a ON a.conversation_id=c.id"
                " WHERE a.id=$3"
            >>,
            [Deferred, <<"direct-purge-deferred">>, Asset]
        ),
        {ok, _} = elib_pg:execute(
            Conn,
            <<
                "INSERT INTO enterprise_message(id,organization_id,workspace_id,conversation_id,"
                "sender_type,sender_contact_id,client_msg_id,retention_days,retain_until)"
                " SELECT $1,c.organization_id,c.workspace_id,c.id,'contact',c.contact_id,$2,1,now()-interval '1 hour'"
                " FROM enterprise_conversation c JOIN enterprise_asset a ON a.conversation_id=c.id"
                " WHERE a.id=$3"
            >>,
            [Ready, <<"direct-purge-ready">>, Asset]
        ),
        {ok, _} = elib_pg:execute(
            Conn,
            <<"UPDATE enterprise_asset SET message_id=$1,retain_until=now()-interval '1 hour' WHERE id=$2">>,
            [Deferred, Asset]
        ),
        {ok, _} = elib_pg:execute(
            Conn,
            <<
                "UPDATE attachment SET cipher='synthetic' WHERE path="
                "(SELECT object_key FROM enterprise_asset_direct_link WHERE asset_id=$1)"
            >>,
            [Asset]
        ),
        ok
    end),
    Now = erlang:system_time(second),
    {ok, First} = eb_pg_purge:purge_batch(Org, Ws, #{now => Now, batch_limit => 10}),
    ?assertEqual([Ready], maps:get(purged, First)),
    ?assertMatch([{Deferred, {ineligible, direct_retirement_deferred}}], maps:get(skipped, First)),
    ?assertEqual(
        1, scalar(<<"SELECT count(*) AS n FROM enterprise_message WHERE id=$1">>, [Deferred])
    ),
    ?assertEqual(
        0, scalar(<<"SELECT count(*) AS n FROM enterprise_message WHERE id=$1">>, [Ready])
    ),
    ?assertEqual(
        1,
        scalar(
            <<"SELECT count(*) AS n FROM enterprise_direct_purge_wait WHERE item_kind='message' AND item_id=$1">>,
            [Deferred]
        )
    ),
    {ok, Second} = eb_pg_purge:purge_batch(Org, Ws, #{now => Now, batch_limit => 10}),
    ?assertEqual([], maps:get(purged, Second)),
    {ok, _} = elib_pg:execute(
        <<
            "UPDATE enterprise_direct_purge_wait SET next_retry_at=clock_timestamp()-interval '1 second'"
            " WHERE item_kind='message' AND item_id=$1"
        >>,
        [Deferred]
    ),
    {ok, Third} = eb_pg_purge:purge_batch(Org, Ws, #{now => Now, batch_limit => 10}),
    ?assertEqual([], maps:get(purged, Third)),
    ?assertEqual(
        2,
        scalar(
            <<"SELECT attempts AS n FROM enterprise_direct_purge_wait WHERE item_kind='message' AND item_id=$1">>,
            [Deferred]
        )
    ),
    ?assertEqual(
        1,
        scalar(
            <<"SELECT count(*) AS n FROM attach_pending WHERE confirmed_attachment_id IS NOT NULL AND direct_state='CONFIRMED' AND object_key=(SELECT object_key FROM enterprise_asset_direct_link WHERE asset_id=$1)">>,
            [Asset]
        )
    ).

scalar(Sql, Params) ->
    {ok, [#{<<"n">> := Value}]} = elib_pg:query(Sql, Params),
    Value.

%% ---------------------------------------------------------------- fixture
%% 本用例曾静默依赖其他套件（eb_asset_direct_tests）留下的 enterprise_asset
%% 99006105 行：fixture 的 INSERT ... SELECT JOIN enterprise_asset 在资产缺失时
%% 0 行命中不报错，purge_batch 随即空批 → purged=[] 伪失败（nightly 2026-11-07
%% NEW 失败根因）。per-test cleanup 使跨套件复用不可能成立——fixture 必须自足：
%% 基础租户行 + 真实直传链（register→declare→grant→finalize CONFIRMED）。
seed_direct_asset_fixture(Org, Ws, Asset) ->
    User = 99006100,
    Contact = 99006103,
    Conv = 99006104,
    ok = elib_pg:with_tx(fun(Conn) ->
        {ok, _} = elib_pg:query(Conn, <<"SET LOCAL session_replication_role=replica">>, []),
        %% 自愈：直传族残留按 object_key 前缀清；基础租户行按固定 id 清。
        [
            {ok, _} = elib_pg:query(Conn, Sql, [])
         || Sql <- [
                <<"DELETE FROM attach_pending WHERE object_key LIKE 'enterprise/99006101/%'">>,
                <<"DELETE FROM enterprise_asset_direct_link WHERE object_key LIKE 'enterprise/99006101/%'">>,
                <<"DELETE FROM attach_direct_attempt WHERE object_key LIKE 'enterprise/99006101/%'">>,
                <<"DELETE FROM attach_direct_cleanup WHERE object_key LIKE 'enterprise/99006101/%'">>,
                <<"DELETE FROM attachment WHERE path LIKE 'enterprise/99006101/%'">>,
                <<"DELETE FROM enterprise_asset WHERE object_key LIKE 'enterprise/99006101/%'">>
            ]
        ],
        {ok, _} = elib_pg:query(Conn, <<"DELETE FROM enterprise_conversation WHERE id=$1">>, [Conv]),
        {ok, _} = elib_pg:query(Conn, <<"DELETE FROM enterprise_contact WHERE id=$1">>, [Contact]),
        {ok, _} = elib_pg:query(
            Conn, <<"DELETE FROM organization_business_identity_assignment WHERE id=99006108">>, []
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
        {ok, _} = elib_pg:query(
            Conn,
            <<"INSERT INTO public.\"user\"(id,password,account,reg_ip,reg_cosv) VALUES($1,'x','eb-purge-fixture','127.0.0.1','x')">>,
            [User]
        ),
        {ok, _} = elib_pg:query(
            Conn,
            <<"INSERT INTO organization(id,name,owner_id,status) VALUES($1,'eb-purge-fixture',$2,'active')">>,
            [Org, User]
        ),
        %% fn_eb_direct_reserve → fn_eb_direct_scope_lock 的 seat 分支硬性要求
        %% organization_member(org, principal, active)——缺失即 23510
        %% 'enterprise direct subject unauthorized'（PR#42 review P1-1）。
        %% 此行此前唯一来源是 eb_asset_direct_tests 残留，自足性必须显式建。
        {ok, _} = elib_pg:query(
            Conn,
            <<"INSERT INTO organization_member(organization_id,user_id,status) VALUES($1,$2,'active')">>,
            [Org, User]
        ),
        {ok, _} = elib_pg:query(
            Conn,
            <<"INSERT INTO workspace(id,name,owner_id,status,type,organization_id) VALUES($1,'eb-purge-fixture',$2,'active','project',$3)">>,
            [Ws, User, Org]
        ),
        {ok, _} = elib_pg:query(
            Conn,
            <<"INSERT INTO organization_business_identity(id,organization_id,function_key,display_name,status) VALUES(99006107,$1,'sales','fixture','active')">>,
            [Org]
        ),
        %% scope_lock 最后一关：seat 的 identity 必须显式 assignment 到 actor
        %% （organization_business_identity_assignment org+identity+user active）。
        %% 同 member：此前唯一来源是 eb_asset_direct_tests 残留。
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
    end),
    %% 真实直传链（与 eb_asset_direct_tests 同口径；Cid 独立避免与该套件撞
    %% client_upload_id 幂等分支）。会话 deadline 留 1h 裕量：grant 能力窗
    %% now()+?GRANT_TTL_SECONDS(3600) 带微秒，deadline=Now+3600 整秒必溢出
    %% ck_v5_pending_times。
    Now = erlang:system_time(second),
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
    {ok, #{asset_id := Asset, object_key := Key}} = eb_pg_asset_meta:register_direct_session_v5(
        Org, Ws, D, <<"aba94340-99fa-4afb-8fe9-67e56c8c6132">>, Reg, {Now + 7200, Now + 1200}
    ),
    ok = attach_direct_logic:declare(Key, hex64(33), 12, <<"image/png">>),
    {ok, #{attempt_id := Attempt}} = attach_direct_logic:grant(Key, hex64(33)),
    {ok, #{state := <<"CONFIRMED">>}} = with_storage_mock(fun() ->
        attach_direct_logic:finalize(Key, Attempt)
    end),
    ok.

%% finalize 的服务端 copy/head 面与 eb_asset_direct_tests 同构：eunit 无真
%% Garage，mock 派发与有界读（receipt 语义由 attachment_worker_port_gate.sh
%% 真 Garage 端到端锁定）。
with_storage_mock(Inner) ->
    ok = meck:new(attachment_storage_garage, [passthrough, no_link]),
    ok = meck:expect(
        attachment_storage_garage, copy_dispatch, 4, fun(_N, _C, _S, _T) -> {ok, #{}} end
    ),
    ok = meck:expect(
        attachment_storage_garage, head_bounded, 3, fun(_N, _C, _T) -> {ok, #{size => 12}} end
    ),
    try
        Inner()
    after
        meck:unload(attachment_storage_garage)
    end.

hex64(N) -> iolist_to_binary(io_lib:format("~64.16.0b", [N])).
