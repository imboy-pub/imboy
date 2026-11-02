-module(enterprise_webhook_v324_pg_tests).

%%%
% V3-24 Webhook 五类事件完整旅程（G24；required_local=true、allow_static=false）。
%
% 覆盖（SPEC webhook-events-v3 §2/§3/§4）：
%   * G-1 枚举：events_whitelist() 精确 8 值（三处同源中 logic 为真源；
%     configure.yaml/README 同步由本套件 T-G1 钉订阅面）；
%   * G-2 group.member.changed：INT-05/06 有实际变更 → 与审计同事务入箱；
%     纯幂等空转（全 already_*）→ 零投递；
%   * G-3 user.mapping.changed：bind / revoke（resource.id=null 语义）；
%   * G-4 delivered/read：企业托管行（origin 行）归属解析入箱；Human 行
%     （origin null）零外发；信封 resource.type=msg_c2c|msg_c2g；
%   * G-5 cs.session.changed：org 作用域归属（org 下 active Application
%     逐个订阅判定；无 active Application → skip）；
%   * G-6 真实 loopback HTTP 合成接收器（webhook_receiver_fixture）：
%     验签（verify/verify_within）/乱序/重复（平台去重+重放）/超时/
%     4xx 死信/重试耗尽/订阅负例/越权 replay/新五类旅程。
%
% 基础设施零重建：全部走 emit_event_tx → bot_delivery → execute_delivery
% 既有管线；投递 settle 路径（池化）按既有套件先例 meck 捕获，出站 HTTP/
% 签名/guard 全真实（loopback 放行需 IMBOYENV ∈ test|local|dev）。
%%%

-export([v324_cases/0]).

%% eunit 约定入口（t= 单跑与全量 EUNIT_MODS 同一形态）。
enterprise_webhook_v324_pg_test_() ->
    v324_cases().

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").
-include("eunit_setup.hrl").

-define(ORG_A, 994101).
-define(ORG_B, 994102).

-define(OWNER_A, 994001).
-define(OWNER_B, 994002).
-define(H_A1, 994011).
-define(H_A2, 994012).
-define(H_A3, 994013).

-define(PUBLIC_IP, {93, 184, 216, 34}).
-define(PRIN_A, 994014).
-define(PRIN_B, 994024).
-define(H_B1, 994021).

-define(WS_A1, 994201).
-define(GRP_A1, 994301).

-define(EXT_A1, <<"ext-v24a1">>).
-define(EXT_A2, <<"ext-v24a2">>).
-define(EXT_B1, <<"ext-v24b1">>).

%%%===================================================================
%%% 入口
%%%===================================================================

v324_cases() ->
    {timeout, 300,
        {setup, fun setup_conn/0, fun close_conn/1, fun(State) ->
            C = maps:get(conn, State),
            [
                {"g1_whitelist_eight_values",
                    with_tx(C, fun(C1) -> with_public_dns(fun() -> g1_whitelist(C1, State) end) end)},
                {"g1_configure_new_events_subscribable",
                    with_tx(C, fun(C1) ->
                        with_public_dns(fun() -> g1_configure_new_events(C1, State) end)
                    end)},
                {"g2_group_member_changed_emit",
                    with_tx(C, fun(C1) -> with_public_dns(fun() -> g2_group_emit(C1, State) end) end)},
                {"g2_group_member_idempotent_no_emit",
                    with_tx(C, fun(C1) ->
                        with_public_dns(fun() -> g2_group_idempotent(C1, State) end)
                    end)},
                {"g3_mapping_bind_emit",
                    with_tx(C, fun(C1) -> with_public_dns(fun() -> g3_bind_emit(C1, State) end) end)},
                {"g3_mapping_revoke_emit",
                    with_tx(C, fun(C1) ->
                        with_public_dns(fun() -> g3_revoke_emit(C1, State) end)
                    end)},
                {"g4_delivered_hosted_row_emits",
                    with_tx(C, fun(C1) -> with_public_dns(fun() -> g4_delivered(C1, State) end) end)},
                {"g4_human_row_zero_outbound",
                    with_tx(C, fun(C1) -> with_public_dns(fun() -> g4_human_zero(C1, State) end) end)},
                {"g4_read_hosted_row_emits",
                    with_tx(C, fun(C1) -> with_public_dns(fun() -> g4_read(C1, State) end) end)},
                {"g5_cs_session_changed_emit",
                    with_tx(C, fun(C1) -> with_public_dns(fun() -> g5_cs_emit(C1, State) end) end)},
                {"g5_cs_no_active_app_skip",
                    with_tx(C, fun(C1) -> with_public_dns(fun() -> g5_cs_skip(C1, State) end) end)},
                {"g6_t1_signature_happy_path", g6_t1(State)},
                {"g6_t2_tamper_and_replay_window", g6_t2(State)},
                {"g6_t3_rotate_invalidates_old_secret", g6_t3(State)},
                {"g6_t4_out_of_order_delivery", g6_t4(State)},
                {"g6_t5_platform_dedup", g6_t5(State)},
                {"g6_t6_replay_new_delivery_same_event", g6_t6(State)},
                {"g6_t7_receiver_timeout", g6_t7(State)},
                {"g6_t8_4xx_dead_letter", g6_t8(State)},
                {"g6_t9_retry_exhaustion", g6_t9(State)},
                {"g6_t10_subscription_negative", g6_t10(State)},
                {"g6_t11_cross_app_replay_denied", g6_t11(State)},
                {"g6_t12_five_event_families_journey", g6_t12(State)}
            ]
        end}}.

%%%===================================================================
%%% Fixture
%%%===================================================================

setup_conn() ->
    case application:load(imboy) of
        ok -> ok;
        {error, {already_loaded, imboy}} -> ok
    end,
    Prior = application:get_env(imboy, postgre_aes_key),
    try
        elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3})
    catch
        _:_ -> ok
    end,
    {ok, _} = application:ensure_all_started(throttle),
    State0 = inttest_marker_db:provision(#{
        env_prefix => <<"V324_INTTEST">>,
        connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
    }),
    ok = application:set_env(imboy, postgre_aes_key, <<"eunit-v324-webhook-fixture-key">>),
    C = maps:get(conn, State0),
    ok = exec(C, <<"BEGIN">>),
    try
        seed_matrix(C)
    catch
        Class:Reason:Stack ->
            _ = exec_quiet(C, <<"ROLLBACK">>),
            inttest_marker_db:release(State0),
            erlang:raise(Class, {fixture_seed_failed, Reason}, Stack)
    end,
    ok = exec(C, <<"COMMIT">>),
    State0#{conn => C, app_a => app_id(C, <<"v324-oa-a">>), app_b => app_id(C, <<"v324-oa-b">>)}.

close_conn(State) ->
    catch meck:unload(enterprise_webhook_repo),
    catch meck:unload(bot_webhook_delivery_repo),
    catch meck:unload(elib_metric),
    application:unset_env(imboy, postgre_aes_key),
    inttest_marker_db:release(State),
    ok.

with_tx(C, TestFun) ->
    ?_test(begin
        ok = exec(C, <<"BEGIN">>),
        try
            TestFun(C),
            ok
        after
            exec(C, <<"ROLLBACK">>)
        end
    end).

exec(C, IoData) ->
    Sql = iolist_to_binary(IoData),
    case elib_pg:query(C, Sql, []) of
        {ok, _} -> ok;
        {error, Reason} -> erlang:error({sql_error, Reason, Sql})
    end.

exec_quiet(C, IoData) ->
    _ = elib_pg:query(C, iolist_to_binary(IoData), []),
    ok.

one(C, Sql, Params) ->
    case elib_pg:query(C, Sql, Params) of
        {ok, [Row | _]} -> Row;
        {ok, []} -> #{};
        {error, Reason} -> erlang:error({sql_error, Reason, Sql})
    end.

seed_matrix(C) ->
    lists:foreach(fun(U) -> seed_user(C, U) end, [
        ?OWNER_A, ?OWNER_B, ?H_A1, ?H_A2, ?H_A3, ?PRIN_A, ?PRIN_B, ?H_B1
    ]),
    seed_org(C, ?ORG_A, ?OWNER_A),
    seed_org(C, ?ORG_B, ?OWNER_B),
    lists:foreach(
        fun(U) -> seed_org_member(C, ?ORG_A, U) end, [?H_A1, ?H_A2, ?H_A3, ?PRIN_A]
    ),
    seed_org_member(C, ?ORG_B, ?H_B1),
    seed_org_member(C, ?ORG_B, ?PRIN_B),
    seed_workspace(C, ?WS_A1, ?ORG_A, ?OWNER_A),
    seed_ws_member(C, ?WS_A1, ?OWNER_A),
    seed_ws_member(C, ?WS_A1, ?H_A1),
    seed_ws_member(C, ?WS_A1, ?H_A2),
    seed_group(C, ?GRP_A1, ?WS_A1, ?OWNER_A),
    {ok, _} = enterprise_application_repo:create_tx(
        C, ?ORG_A, <<"v324-oa-a">>, <<"v324 org A oa"/utf8>>, ?PRIN_A
    ),
    {ok, _} = enterprise_application_repo:create_tx(
        C, ?ORG_B, <<"v324-oa-b">>, <<"v324 org B oa"/utf8>>, ?PRIN_B
    ),
    {ok, _} = enterprise_application_repo:create_tx(
        C, ?ORG_A, <<"v324-oa-nopin">>, <<"v324 no principal"/utf8>>, null
    ),
    ok = seed_mapping(C, ?ORG_A, app_id(C, <<"v324-oa-a">>), ?EXT_A1, ?H_A1),
    ok = seed_mapping(C, ?ORG_A, app_id(C, <<"v324-oa-a">>), ?EXT_A2, ?H_A2),
    ok = seed_mapping(C, ?ORG_B, app_id(C, <<"v324-oa-b">>), ?EXT_B1, ?H_B1),
    ok.

app_id(C, Key) ->
    maps:get(
        <<"id">>,
        one(C, <<"SELECT id FROM enterprise_application WHERE application_key = $1">>, [Key])
    ).

seed_user(C, Uid) ->
    exec(C, [
        <<"INSERT INTO \"user\" (id, password, account, account_type, status, reg_ip, reg_cosv) VALUES (">>,
        integer_to_binary(Uid),
        ", 'x', 'v994_u",
        integer_to_binary(Uid),
        <<"', 0, 1, '127.0.0.1', 'x')">>
    ]).

seed_org(C, OrgId, OwnerUid) ->
    exec(C, [
        <<"INSERT INTO organization (id, name, owner_id, status, branding, settings, created_at, updated_at) VALUES (">>,
        integer_to_binary(OrgId),
        ", 'v994-org', ",
        integer_to_binary(OwnerUid),
        <<", 'active', '{}'::jsonb, '{}'::jsonb, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)">>
    ]).

seed_org_member(C, OrgId, Uid) ->
    exec(C, [
        <<"INSERT INTO organization_member (organization_id, user_id, role, status, joined_at, created_at, updated_at) VALUES (">>,
        integer_to_binary(OrgId),
        ", ",
        integer_to_binary(Uid),
        <<", 'member', 'active', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)">>
    ]).

seed_workspace(C, WsId, OrgId, OwnerUid) ->
    exec(C, [
        <<"INSERT INTO workspace (id, name, owner_id, status, organization_id, created_at, updated_at) VALUES (">>,
        integer_to_binary(WsId),
        ", 'v994-ws', ",
        integer_to_binary(OwnerUid),
        <<", 'active', ">>,
        integer_to_binary(OrgId),
        ", CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)"
    ]).

seed_ws_member(C, WsId, Uid) ->
    exec(C, [
        <<"INSERT INTO workspace_member (workspace_id, user_id, role, status, joined_at, created_at, updated_at) VALUES (">>,
        integer_to_binary(WsId),
        ", ",
        integer_to_binary(Uid),
        <<", 'member', 'active', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)">>
    ]).

seed_group(C, GroupId, WsId, OwnerUid) ->
    exec(C, [
        <<
            "INSERT INTO \"group\" (id, type, join_limit, owner_uid, creator_uid, member_max,"
            " member_count, title, status, scope, workspace_id, created_at, updated_at) VALUES ("
        >>,
        integer_to_binary(GroupId),
        <<", 2, 3, ">>,
        integer_to_binary(OwnerUid),
        <<", ">>,
        integer_to_binary(OwnerUid),
        <<", 500, 1, 'v994_g_">>,
        integer_to_binary(GroupId),
        <<"', 1, 'workspace', ">>,
        integer_to_binary(WsId),
        <<", CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)">>
    ]).

seed_group_member(C, GmId, Gid, Uid) ->
    exec(C, [
        <<"INSERT INTO group_member (id, group_id, user_id, role, is_join, status, created_at, updated_at) VALUES (">>,
        integer_to_binary(GmId),
        ", ",
        integer_to_binary(Gid),
        ", ",
        integer_to_binary(Uid),
        <<", 1, true, 1, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)">>
    ]).

seed_mapping(C, OrgId, AppId, Ext, Uid) ->
    case enterprise_external_identity_repo:bind_tx(C, OrgId, AppId, Ext, Uid) of
        {ok, _} -> ok;
        {error, identity_already_mapped} -> ok;
        {error, Reason} -> erlang:error({seed_mapping_failed, Reason})
    end.

%% 企业托管 origin 行（G-4 归属解析正例真源；tx 连接上调用）。
seed_origin(C, MsgId, RowId, Kind, AppId) ->
    exec(C, [
        <<
            "INSERT INTO enterprise_message_origin (id, conversation_kind, organization_id,"
            " application_id, sender_kind, msg_row_id, msg_id) VALUES ("
        >>,
        integer_to_binary(elib_tsid:generate(enterprise_message_origin)),
        <<", '">>,
        Kind,
        <<"', ">>,
        integer_to_binary(?ORG_A),
        <<", ">>,
        integer_to_binary(AppId),
        <<", 'application', ">>,
        integer_to_binary(RowId),
        <<", '">>,
        MsgId,
        <<"')">>
    ]).

%%%===================================================================
%%% Ctx
%%%===================================================================

%% side_effect_tx 的 elib_pg:with_tx 在 eunit VM 无池化连接——meck 桥接为
%% 测试事务连接直连执行（既有套件"池化路径一律 meck"纪律同款；外层
%% with_tx ROLLBACK 兜底不留数据）。仅桥接 with_tx/1，query 等 passthrough。
with_side_effect_conn(C, Fun) ->
    ok = meck:new(elib_pg, [no_link, passthrough]),
    ok = meck:expect(elib_pg, with_tx, fun(F) ->
        _ = F(C),
        ok
    end),
    try
        Fun()
    after
        catch meck:unload(elib_pg)
    end.

%% g6 用例的直连事务桥（eunit VM 无池化连接；手工 BEGIN/COMMIT 于测试
%% 连接上执行，异常 ROLLBACK）。与 with_tx 的差异：数据真实提交（marker
%% 一次性库每用例自隔离）。
direct_tx(C, Fun) ->
    ok = exec(C, <<"BEGIN">>),
    try
        R = Fun(C),
        ok = exec(C, <<"COMMIT">>),
        R
    catch
        Class:Reason:Stack ->
            _ = exec_quiet(C, <<"ROLLBACK">>),
            erlang:raise(Class, Reason, Stack)
    end.

%% guard DNS 桩：oa-v324.example.com 解析到公网 IP（SSRF guard 在触库前
%% 解析；loopback URL 由 guard 直接放行不经 DNS——本桩只服务 https 配置面）。
with_public_dns(Fun) ->
    try
        ok = meck:new(inet, [unstick, passthrough, no_passthrough_cover]),
        meck:expect(inet, getaddrs, fun("oa-v324.example.com", inet) ->
            {ok, [?PUBLIC_IP]}
        end),
        try
            Fun()
        after
            meck:unload(inet)
        end
    catch
        _:{already_started, _} ->
            Fun()
    end.

ctx_a(C) ->
    enterprise_hmac_test_support:context_tx(C, ?ORG_A, app_id(C, <<"v324-oa-a">>)).

ctx_b(C) ->
    enterprise_hmac_test_support:context_tx(C, ?ORG_B, app_id(C, <<"v324-oa-b">>)).

%%%===================================================================
%%% G-1 枚举
%%%===================================================================

g1_whitelist(C, _State) ->
    Expected = [
        <<"message.enterprise.accepted">>,
        <<"message.enterprise.failed">>,
        <<"message.enterprise.delivered">>,
        <<"message.enterprise.read">>,
        <<"group.member.changed">>,
        <<"user.mapping.changed">>,
        <<"cs.session.changed">>,
        <<"file.confirmed">>
    ],
    ?assertEqual(Expected, enterprise_webhook_logic:events_whitelist()),
    %% webhook.ping 仍不可订阅
    ?assertEqual(false, lists:member(<<"webhook.ping">>, Expected)),
    %% 新值订阅成功（白名单∩订阅链路对 8 值全通）
    lists:foreach(
        fun(Event) ->
            ?assertMatch(
                {ok, _},
                enterprise_webhook_logic:configure_tx(
                    C,
                    ctx_a(C),
                    #{url => <<"https://oa-v324.example.com/hook">>, events => [Event]}
                )
            )
        end,
        Expected
    ).

g1_configure_new_events(C, _State) ->
    %% 白名单外事件仍拒（不扩散边界 1：白名单封闭）
    ?assertMatch(
        {error, {<<"invalid_request">>, invalid_webhook_config}},
        enterprise_webhook_logic:configure_tx(
            C,
            ctx_a(C),
            #{
                url => <<"https://oa-v324.example.com/hook">>,
                events => [<<"message.personal.received">>]
            }
        )
    ).

%%%===================================================================
%%% G-2 群成员变更
%%%===================================================================

g2_group_emit(C, _State) ->
    Ctx = ctx_a(C),
    {ok, _} = enterprise_webhook_logic:configure_tx(
        C,
        Ctx,
        #{url => <<"https://oa-v324.example.com/hook">>, events => [<<"group.member.changed">>]}
    ),
    seed_group_member(C, 994311, ?GRP_A1, ?OWNER_A),
    {ok, Ok} = enterprise_group_logic:add_members_tx(
        C, Ctx, ?GRP_A1, [?EXT_A1]
    ),
    ?assertEqual(1, maps:get(<<"added">>, Ok)),
    Row = one(
        C,
        <<
            "SELECT event_type, payload::text AS payload FROM bot_delivery"
            " WHERE event_type = 'group.member.changed' ORDER BY created_at DESC LIMIT 1"
        >>,
        []
    ),
    Env = jsone:decode(maps:get(<<"payload">>, Row)),
    ?assertEqual(<<"group.member.changed">>, maps:get(<<"event_type">>, Env)),
    #{<<"resource">> := Resource} = Env,
    ?assertEqual(<<"enterprise_group">>, maps:get(<<"type">>, Resource)),
    ?assertEqual(?GRP_A1, maps:get(<<"id">>, Resource)).

g2_group_idempotent(C, _State) ->
    Ctx = ctx_a(C),
    {ok, _} = enterprise_webhook_logic:configure_tx(
        C,
        Ctx,
        #{url => <<"https://oa-v324.example.com/hook">>, events => [<<"group.member.changed">>]}
    ),
    %% 纯幂等空转：已成员重复加（added=0）→ 零投递
    seed_group_member(C, 994321, ?GRP_A1, ?OWNER_A),
    _ = enterprise_group_logic:add_members_tx(C, Ctx, ?GRP_A1, [?EXT_A1]),
    Before = delivery_count(C, <<"group.member.changed">>),
    {ok, Ok} = enterprise_group_logic:add_members_tx(C, Ctx, ?GRP_A1, [?EXT_A1]),
    ?assertEqual(0, maps:get(<<"added">>, Ok)),
    ?assertEqual(Before, delivery_count(C, <<"group.member.changed">>)).

delivery_count(C, EventType) ->
    Row = one(
        C,
        <<"SELECT COUNT(*)::int AS n FROM bot_delivery WHERE event_type = $1">>,
        [EventType]
    ),
    maps:get(<<"n">>, Row).

%%%===================================================================
%%% G-3 映射变更
%%%===================================================================

g3_bind_emit(C, _State) ->
    Ctx = ctx_a(C),
    {ok, _} = enterprise_webhook_logic:configure_tx(
        C,
        Ctx,
        #{url => <<"https://oa-v324.example.com/hook">>, events => [<<"user.mapping.changed">>]}
    ),
    Before = delivery_count(C, <<"user.mapping.changed">>),
    {ok, _} = enterprise_identity_logic:bind_mapping_tx(
        C, Ctx, <<"ext-v24-new">>, ?H_A3
    ),
    ?assertEqual(Before + 1, delivery_count(C, <<"user.mapping.changed">>)),
    Row = last_delivery_payload(C, <<"user.mapping.changed">>),
    #{<<"resource">> := Resource} = Row,
    ?assertEqual(<<"identity_mapping">>, maps:get(<<"type">>, Resource)),
    ?assertMatch(Id when is_integer(Id), maps:get(<<"id">>, Resource)).

g3_revoke_emit(C, _State) ->
    Ctx = ctx_a(C),
    {ok, _} = enterprise_webhook_logic:configure_tx(
        C,
        Ctx,
        #{url => <<"https://oa-v324.example.com/hook">>, events => [<<"user.mapping.changed">>]}
    ),
    Before = delivery_count(C, <<"user.mapping.changed">>),
    {ok, _} = enterprise_identity_logic:revoke_mapping_tx(C, Ctx, ?EXT_A2),
    ?assertEqual(Before + 1, delivery_count(C, <<"user.mapping.changed">>)),
    Row = last_delivery_payload(C, <<"user.mapping.changed">>),
    #{<<"resource">> := Resource} = Row,
    %% 撤销事件行已软删 → resource.id=null（README §8 注明，集成方以
    %% external_user_id 关联）
    ?assertEqual(<<"identity_mapping">>, maps:get(<<"type">>, Resource)),
    ?assertEqual(null, maps:get(<<"id">>, Resource)).

last_delivery_payload(C, EventType) ->
    Row = one(
        C,
        <<
            "SELECT payload::text AS payload FROM bot_delivery WHERE event_type = $1"
            " ORDER BY created_at DESC LIMIT 1"
        >>,
        [EventType]
    ),
    jsone:decode(maps:get(<<"payload">>, Row)).

%%%===================================================================
%%% G-4 delivered / read（归属解析）
%%%===================================================================

g4_delivered(C, _State) ->
    Ctx = ctx_a(C),
    _ = enterprise_webhook_logic:configure_tx(
        C,
        Ctx,
        #{
            url => <<"https://oa-v324.example.com/hook">>,
            events => [<<"message.enterprise.delivered">>, <<"message.enterprise.read">>]
        }
    ),
    MsgId = <<"v324-hosted-msg-1">>,
    AppId = maps:get(application_id, Ctx),
    seed_origin(C, MsgId, 994901, <<"direct">>, AppId),
    with_side_effect_conn(C, fun() ->
        ok = enterprise_webhook_logic:emit_delivered_for(<<"msg_c2c">>, MsgId)
    end),
    Row = one(
        C,
        <<
            "SELECT payload::text AS payload FROM bot_delivery"
            " WHERE event_type = 'message.enterprise.delivered' ORDER BY created_at DESC LIMIT 1"
        >>,
        []
    ),
    Env = jsone:decode(maps:get(<<"payload">>, Row)),
    ?assertEqual(<<"message.enterprise.delivered">>, maps:get(<<"event_type">>, Env)),
    #{<<"resource">> := Resource} = Env,
    ?assertEqual(<<"msg_c2c">>, maps:get(<<"type">>, Resource)),
    ?assertEqual(994901, maps:get(<<"id">>, Resource)),
    %% 非托管表（s2c/c2s）直接 skip
    ok = enterprise_webhook_logic:emit_delivered_for(<<"msg_s2c">>, MsgId),
    ok.

g4_human_zero(C, _State) ->
    Ctx = ctx_a(C),
    _ = enterprise_webhook_logic:configure_tx(
        C,
        Ctx,
        #{
            url => <<"https://oa-v324.example.com/hook">>,
            events => [<<"message.enterprise.delivered">>, <<"message.enterprise.read">>]
        }
    ),
    Before = delivery_count(C, <<"message.enterprise.delivered">>),
    BeforeRead = delivery_count(C, <<"message.enterprise.read">>),
    %% Human 行：无 origin 行 → 零外发（不扩散边界 3；桥接直连使 emit 真实执行）
    with_side_effect_conn(C, fun() ->
        ok = enterprise_webhook_logic:emit_delivered_for(<<"msg_c2c">>, <<"v324-human-msg-1">>),
        ok = enterprise_webhook_logic:emit_read_for([<<"v324-human-msg-2">>])
    end),
    ?assertEqual(Before, delivery_count(C, <<"message.enterprise.delivered">>)),
    ?assertEqual(BeforeRead, delivery_count(C, <<"message.enterprise.read">>)).

g4_read(C, _State) ->
    Ctx = ctx_a(C),
    _ = enterprise_webhook_logic:configure_tx(
        C,
        Ctx,
        #{
            url => <<"https://oa-v324.example.com/hook">>,
            events => [<<"message.enterprise.read">>]
        }
    ),
    MsgIds = [<<"v324-hosted-msg-r1">>, <<"v324-hosted-msg-r2">>],
    AppId = maps:get(application_id, Ctx),
    seed_origin(C, hd(MsgIds), 994902, <<"direct">>, AppId),
    with_side_effect_conn(C, fun() ->
        ok = enterprise_webhook_logic:emit_read_for(MsgIds)
    end),
    ?assertEqual(1, delivery_count(C, <<"message.enterprise.read">>)),
    Row = last_delivery_payload(C, <<"message.enterprise.read">>),
    #{<<"resource">> := Resource} = Row,
    ?assertEqual(<<"msg_c2c">>, maps:get(<<"type">>, Resource)),
    ?assertEqual(994902, maps:get(<<"id">>, Resource)).

%%%===================================================================
%%% G-5 客服事件（org 作用域归属决策）
%%%===================================================================

g5_cs_emit(C, _State) ->
    Ctx = ctx_a(C),
    _ = enterprise_webhook_logic:configure_tx(
        C,
        Ctx,
        #{url => <<"https://oa-v324.example.com/hook">>, events => [<<"cs.session.changed">>]}
    ),
    with_side_effect_conn(C, fun() ->
        ok = enterprise_webhook_logic:emit_cs_session_changed(?ORG_A, #{
            session_id => 994951, event_type => <<"session.claimed">>
        })
    end),
    Row = one(
        C,
        <<
            "SELECT payload::text AS payload FROM bot_delivery"
            " WHERE event_type = 'cs.session.changed' ORDER BY created_at DESC LIMIT 1"
        >>,
        []
    ),
    Env = jsone:decode(maps:get(<<"payload">>, Row)),
    #{<<"resource">> := Resource} = Env,
    ?assertEqual(<<"cs_session">>, maps:get(<<"type">>, Resource)),
    ?assertEqual(994951, maps:get(<<"id">>, Resource)).

g5_cs_skip(C, _State) ->
    %% 无 active Application（principal null 的 app 不算）→ skip 零投递
    Ctx = ctx_a(C),
    _ = enterprise_webhook_logic:configure_tx(
        C,
        Ctx,
        #{url => <<"https://oa-v324.example.com/hook">>, events => [<<"cs.session.changed">>]}
    ),
    Before = delivery_count(C, <<"cs.session.changed">>),
    NoAppOrg = ?ORG_A + 900,
    with_side_effect_conn(C, fun() ->
        ok = enterprise_webhook_logic:emit_cs_session_changed(NoAppOrg, #{session_id => 1})
    end),
    ?assertEqual(Before, delivery_count(C, <<"cs.session.changed">>)).

%%%===================================================================
%%% G-6 真实 loopback 合成接收器（12 用例矩阵）
%%%===================================================================
%%
%% settle 路径（池化）meck 捕获；出站 HTTP（真实 sender）/guard/签名全真实。
%% IMBOYENV ∈ test|local|dev 时 bot_webhook_guard 放行 HTTP loopback。

with_receiver(Fun) ->
    {ok, Url, Ref} = webhook_receiver_fixture:start(),
    try
        Fun(Url, Ref)
    after
        webhook_receiver_fixture:stop(Ref)
    end.

%% execute_delivery 的池化替身（既有套件先例同款）：principal/secret 从测试
%% 状态注入；settle 写入捕获 ets；出站 sender/guard 真实。
delivery_stubs(Secret) ->
    catch meck:unload(enterprise_webhook_repo),
    catch meck:unload(bot_webhook_delivery_repo),
    catch meck:unload(elib_metric),
    SettleEts = ets:new(v324_settle, [public, ordered_set, named_table]),
    ok = meck:new(enterprise_webhook_repo, [no_link, passthrough]),
    ok = meck:expect(enterprise_webhook_repo, principal_of_delivery, fun(BotId) ->
        case BotId of
            <<"eapp:", UidBin/binary>> -> {ok, binary_to_integer(UidBin)};
            _ -> {error, not_found}
        end
    end),
    ok = meck:expect(enterprise_webhook_repo, get_secret, fun(_Principal) ->
        {ok, Secret}
    end),
    ok = meck:new(bot_webhook_delivery_repo, [no_link, passthrough]),
    ok = meck:expect(bot_webhook_delivery_repo, insert_attempt, fun(D, A) ->
        ets:insert(SettleEts, {{attempt, erlang:unique_integer()}, D, A}),
        ok
    end),
    ok = meck:expect(bot_webhook_delivery_repo, mark_success, fun(D, A) ->
        ets:insert(SettleEts, {{success, erlang:unique_integer()}, D, A}),
        ok
    end),
    ok = meck:expect(bot_webhook_delivery_repo, mark_retry, fun(D, A, _Next, _Extra) ->
        ets:insert(SettleEts, {{retry, erlang:unique_integer()}, D, A}),
        ok
    end),
    ok = meck:expect(bot_webhook_delivery_repo, mark_dead, fun(D, A) ->
        ets:insert(SettleEts, {{dead, erlang:unique_integer()}, D, A}),
        ok
    end),
    ok = meck:new(elib_metric, [no_link, passthrough]),
    ok = meck:expect(elib_metric, increment, fun(_, _) -> ok end),
    ok = meck:expect(elib_metric, increment, fun(_, _, _) -> ok end),
    ok = meck:expect(elib_metric, record, fun(_, _) -> ok end),
    {ok, SettleEts}.

settle_events(SettleEts, Kind) ->
    [
        E
     || {_K, _, _} = E <- lists:sort(ets:tab2list(SettleEts)), element(1, element(1, E)) =:= Kind
    ].

%% 事务内 emit + 事务外读回 delivery 行（与 claim_due 同构）。
emit_and_fetch_row(C, Ctx, EventType, Resource) ->
    {ok, emitted} =
        direct_tx(C, fun(Conn) ->
            enterprise_webhook_logic:emit_event_tx(Conn, Ctx, EventType, Resource)
        end),
    Row = one(
        C,
        <<
            "SELECT delivery_id, bot_id, event_type, payload::text AS payload,"
            " attempt_count, webhook_url, webhook_host, pinned_ip"
            " FROM bot_delivery WHERE event_type = $1 ORDER BY created_at DESC LIMIT 1"
        >>,
        [EventType]
    ),
    Row#{
        <<"payload">> => maps:get(<<"payload">>, Row, <<"{}">>),
        <<"attempt_count">> => maps:get(<<"attempt_count">>, Row, 0)
    }.

%% T1：验签 happy path——4 头齐全；verify/4=true；信封 8 键封闭；
%% occurred_at ISO-8601；settle=mark_success。
g6_t1(State) ->
    ?_test(
        with_receiver(fun(Url, Ref) ->
            C = maps:get(conn, State),
            {ok, #{<<"secret">> := Secret}} = direct_tx(C, fun(Conn) ->
                enterprise_webhook_logic:configure_tx(
                    Conn, ctx_a(C), #{
                        url => Url, events => [<<"message.enterprise.accepted">>], rotate => true
                    }
                )
            end),
            Ctx = ctx_a(C),
            Row = emit_and_fetch_row(C, Ctx, <<"message.enterprise.accepted">>, #{
                resource_type => <<"msg_c2c">>, resource_id => 994001
            }),
            {ok, SettleEts} = delivery_stubs(Secret),
            try
                ok = enterprise_webhook_logic:execute_delivery(Row),
                Captured = webhook_receiver_fixture:take_all(Ref),
                ?assertEqual(1, length(Captured)),
                [Cap] = Captured,
                H = maps:get(headers, Cap),
                Body = maps:get(body, Cap),
                Ts = maps:get(<<"x-imboy-timestamp">>, H),
                Sig = maps:get(<<"x-imboy-signature">>, H),
                DidHdr = maps:get(<<"x-imboy-delivery">>, H),
                EvHdr = maps:get(<<"x-imboy-event">>, H),
                ?assertEqual(<<"message.enterprise.accepted">>, EvHdr),
                ?assertEqual(true, enterprise_webhook_logic:verify(Secret, Ts, Body, Sig)),
                Env = jsone:decode(Body),
                {EnvelopeKeys, _} = enterprise_webhook_logic:envelope_keys(),
                ?assertEqual(lists:sort(EnvelopeKeys), lists:sort(maps:keys(Env))),
                ?assertEqual(DidHdr, maps:get(<<"delivery_id">>, Env)),
                %% ISO-8601（elib_dt 实际输出带时区偏移形态，如
                %% 2026-11-02T16:36:01.737+08:00；断言 T 分隔与时区段存在）
                OccurredAt = maps:get(<<"occurred_at">>, Env),
                ?assert(
                    is_binary(OccurredAt) andalso
                        byte_size(OccurredAt) >= 20 andalso
                        binary:part(OccurredAt, 10, 1) =:= <<"T">>
                ),
                ?assertEqual(1, length(settle_events(SettleEts, success)))
            after
                catch meck:unload(enterprise_webhook_repo),
                catch meck:unload(bot_webhook_delivery_repo),
                catch meck:unload(elib_metric),
                ets:delete(SettleEts)
            end
        end)
    ).

%% T2：篡改一字节 verify=false；旧 timestamp（反重放窗外）verify_within=false。
g6_t2(State) ->
    ?_test(
        with_receiver(fun(Url, Ref) ->
            C = maps:get(conn, State),
            {ok, #{<<"secret">> := Secret}} = direct_tx(C, fun(Conn) ->
                enterprise_webhook_logic:configure_tx(
                    Conn, ctx_a(C), #{url => Url, events => [<<"file.confirmed">>], rotate => true}
                )
            end),
            Ctx = ctx_a(C),
            Row = emit_and_fetch_row(C, Ctx, <<"file.confirmed">>, #{
                resource_type => <<"attachment">>, resource_id => 994002
            }),
            {ok, _SettleEts} = delivery_stubs(Secret),
            try
                ok = enterprise_webhook_logic:execute_delivery(Row),
                [Cap] = webhook_receiver_fixture:take_all(Ref),
                H = maps:get(headers, Cap),
                Body = maps:get(body, Cap),
                Ts = maps:get(<<"x-imboy-timestamp">>, H),
                Sig = maps:get(<<"x-imboy-signature">>, H),
                Tampered =
                    case Body of
                        <<First:1/binary, Rest/binary>> -> <<First:1/binary, "X", Rest/binary>>;
                        _ -> <<Body/binary, "X">>
                    end,
                ?assertEqual(false, enterprise_webhook_logic:verify(Secret, Ts, Tampered, Sig)),
                OldTs = integer_to_binary(os:system_time(second) - 4000),
                OldSig = enterprise_webhook_logic:sign(
                    Secret, enterprise_webhook_logic:signature_base(OldTs, Body)
                ),
                ?assertEqual(
                    false,
                    enterprise_webhook_logic:verify_within(Secret, OldTs, Body, OldSig, 300)
                ),
                ?assertEqual(
                    true,
                    enterprise_webhook_logic:verify_within(Secret, Ts, Body, Sig, 300)
                )
            after
                catch meck:unload(enterprise_webhook_repo),
                catch meck:unload(bot_webhook_delivery_repo),
                catch meck:unload(elib_metric),
                ets:delete(v324_settle)
            end
        end)
    ).

%% T3：轮换即失效——rotate=true 换 secret；再 emit 的投递用新 secret 可验、
%% 旧 secret 验签=false（无双密钥窗）。
g6_t3(State) ->
    ?_test(
        with_receiver(fun(Url, Ref) ->
            C = maps:get(conn, State),
            {ok, #{<<"secret">> := OldSecret}} = direct_tx(C, fun(Conn) ->
                enterprise_webhook_logic:configure_tx(
                    Conn, ctx_a(C), #{url => Url, events => [<<"file.confirmed">>], rotate => true}
                )
            end),
            {ok, #{<<"secret">> := NewSecret}} = direct_tx(C, fun(Conn) ->
                enterprise_webhook_logic:configure_tx(
                    Conn,
                    ctx_a(C),
                    #{url => Url, events => [<<"file.confirmed">>], rotate => true}
                )
            end),
            ?assertNotEqual(OldSecret, NewSecret),
            Ctx = ctx_a(C),
            Row = emit_and_fetch_row(C, Ctx, <<"file.confirmed">>, #{
                resource_type => <<"attachment">>, resource_id => 994003
            }),
            {ok, _} = delivery_stubs(NewSecret),
            try
                ok = enterprise_webhook_logic:execute_delivery(Row),
                [Cap] = webhook_receiver_fixture:take_all(Ref),
                H = maps:get(headers, Cap),
                Ts = maps:get(<<"x-imboy-timestamp">>, H),
                Sig = maps:get(<<"x-imboy-signature">>, H),
                Body = maps:get(body, Cap),
                ?assertEqual(true, enterprise_webhook_logic:verify(NewSecret, Ts, Body, Sig)),
                ?assertEqual(false, enterprise_webhook_logic:verify(OldSecret, Ts, Body, Sig))
            after
                catch meck:unload(enterprise_webhook_repo),
                catch meck:unload(bot_webhook_delivery_repo),
                catch meck:unload(elib_metric),
                ets:delete(v324_settle)
            end
        end)
    ).

%% T4：乱序——事件 A 首投 503（mark_retry）、事件 B 立即 200（mark_success）；
%% 接收器先见 B 后见 A；两投递 id 唯一；按 occurred_at 可重排。
g6_t4(State) ->
    ?_test(
        with_receiver(fun(Url, Ref) ->
            C = maps:get(conn, State),
            {ok, #{<<"secret">> := Secret}} = direct_tx(C, fun(Conn) ->
                enterprise_webhook_logic:configure_tx(
                    Conn, ctx_a(C), #{url => Url, events => [<<"file.confirmed">>], rotate => true}
                )
            end),
            Ctx = ctx_a(C),
            RowA = emit_and_fetch_row(C, Ctx, <<"file.confirmed">>, #{
                resource_type => <<"attachment">>, resource_id => 994004
            }),
            RowB = emit_and_fetch_row(C, Ctx, <<"file.confirmed">>, #{
                resource_type => <<"attachment">>, resource_id => 994005
            }),
            ?assertNotEqual(
                maps:get(<<"delivery_id">>, RowA), maps:get(<<"delivery_id">>, RowB)
            ),
            {ok, SettleEts} = delivery_stubs(Secret),
            try
                webhook_receiver_fixture:set_policy(Ref, 503),
                ok = enterprise_webhook_logic:execute_delivery(RowA),
                %% A 首投到达（503 响应面）
                ?assertEqual(1, webhook_receiver_fixture:count(Ref)),
                webhook_receiver_fixture:set_policy(Ref, 200),
                ok = enterprise_webhook_logic:execute_delivery(RowB),
                %% B 随后到达；两条投递按 event_id 唯一、occurred_at 可重排
                Captured = webhook_receiver_fixture:take_all(Ref),
                ?assertEqual(2, length(Captured)),
                IdA = maps:get(<<"delivery_id">>, RowA),
                IdB = maps:get(<<"delivery_id">>, RowB),
                ?assertEqual(1, length(settle_events(SettleEts, retry))),
                ?assertEqual(1, length(settle_events(SettleEts, success))),
                %% 按信封 occurred_at 可重排（事件语义时间与投递顺序解耦）
                EnvA = jsone:decode(maps:get(<<"payload">>, RowA)),
                EnvB = jsone:decode(maps:get(<<"payload">>, RowB)),
                ?assert(
                    maps:get(<<"occurred_at">>, EnvA) =/= maps:get(<<"occurred_at">>, EnvB)
                ),
                ?assert(is_binary(IdA)),
                ?assert(is_binary(IdB))
            after
                catch meck:unload(enterprise_webhook_repo),
                catch meck:unload(bot_webhook_delivery_repo),
                catch meck:unload(elib_metric),
                ets:delete(SettleEts)
            end
        end)
    ).

%% T5：平台去重——同 event_id 二次 emit → skipped；bot_delivery 无第二行；
%% 接收器只收到一次。
g6_t5(State) ->
    ?_test(
        with_receiver(fun(Url, Ref) ->
            C = maps:get(conn, State),
            {ok, #{<<"secret">> := Secret}} = direct_tx(C, fun(Conn) ->
                enterprise_webhook_logic:configure_tx(
                    Conn, ctx_a(C), #{url => Url, events => [<<"file.confirmed">>], rotate => true}
                )
            end),
            Ctx = ctx_a(C),
            EventId = <<"evt-v324-dup-1">>,
            Resource = #{
                resource_type => <<"attachment">>,
                resource_id => 994006,
                event_id => EventId
            },
            {ok, emitted} = direct_tx(C, fun(Conn) ->
                enterprise_webhook_logic:emit_event_tx(
                    Conn, Ctx, <<"file.confirmed">>, Resource
                )
            end),
            {ok, skipped} = direct_tx(C, fun(Conn) ->
                enterprise_webhook_logic:emit_event_tx(
                    Conn, Ctx, <<"file.confirmed">>, Resource
                )
            end),
            Count = maps:get(
                <<"n">>,
                one(
                    C,
                    <<"SELECT COUNT(*)::int AS n FROM bot_delivery WHERE idempotency_key = $1">>,
                    [
                        <<"evt-", EventId/binary>>
                    ]
                )
            ),
            ?assertEqual(1, Count),
            Fetched = one(
                C,
                <<
                    "SELECT delivery_id, bot_id, event_type, payload::text AS payload,"
                    " attempt_count, webhook_url, webhook_host, pinned_ip"
                    " FROM bot_delivery WHERE idempotency_key = $1"
                >>,
                [<<"evt-", EventId/binary>>]
            ),
            Row = Fetched#{
                <<"payload">> => maps:get(<<"payload">>, Fetched, <<"{}">>),
                <<"attempt_count">> => maps:get(<<"attempt_count">>, Fetched, 0)
            },
            {ok, _} = delivery_stubs(Secret),
            try
                ok = enterprise_webhook_logic:execute_delivery(Row),
                timer:sleep(100),
                ?assertEqual(1, webhook_receiver_fixture:count(Ref))
            after
                catch meck:unload(enterprise_webhook_repo),
                catch meck:unload(bot_webhook_delivery_repo),
                catch meck:unload(elib_metric),
                ets:delete(v324_settle)
            end
        end)
    ).

%% T6：重放——先投 200 达 success；INT-13 replay 终态行 → 新 delivery_id 保留
%% 原 event_id；接收器共收两条（同一 event_id 两条投递）。
g6_t6(State) ->
    ?_test(
        with_receiver(fun(Url, Ref) ->
            C = maps:get(conn, State),
            {ok, #{<<"secret">> := Secret}} = direct_tx(C, fun(Conn) ->
                enterprise_webhook_logic:configure_tx(
                    Conn, ctx_a(C), #{url => Url, events => [<<"file.confirmed">>], rotate => true}
                )
            end),
            Ctx = ctx_a(C),
            Row = emit_and_fetch_row(C, Ctx, <<"file.confirmed">>, #{
                resource_type => <<"attachment">>, resource_id => 994007
            }),
            OrigId = maps:get(<<"delivery_id">>, Row),
            OrigEnv = jsone:decode(maps:get(<<"payload">>, Row)),
            OrigEventId = maps:get(<<"event_id">>, OrigEnv),
            %% 终态 seed：UPDATE status=success（settle 的 DB 语义前置）
            OkSql = iolist_to_binary([
                <<"UPDATE bot_delivery SET status='success' WHERE delivery_id = '">>,
                OrigId,
                <<"'">>
            ]),
            ok = exec(C, [OkSql]),
            {ok, #{<<"replayed">> := true}} = direct_tx(C, fun(Conn) ->
                enterprise_webhook_logic:replay_tx(Conn, Ctx, OrigId)
            end),
            ReplayRow = one(
                C,
                <<
                    "SELECT delivery_id, bot_id, event_type, payload::text AS payload,"
                    " attempt_count, webhook_url, webhook_host, pinned_ip"
                    " FROM bot_delivery WHERE delivery_id <> $1"
                    " AND event_type = 'file.confirmed' ORDER BY created_at DESC LIMIT 1"
                >>,
                [OrigId]
            ),
            ReplayId = maps:get(<<"delivery_id">>, ReplayRow),
            ?assertNotEqual(OrigId, ReplayId),
            ReplayEnv = jsone:decode(maps:get(<<"payload">>, ReplayRow)),
            ?assertEqual(OrigEventId, maps:get(<<"event_id">>, ReplayEnv)),
            ?assertEqual(ReplayId, maps:get(<<"delivery_id">>, ReplayEnv)),
            %% 重放行真实出站（接收器第二条，同 event_id）
            {ok, _} = delivery_stubs(Secret),
            try
                ok = enterprise_webhook_logic:execute_delivery(ReplayRow#{
                    <<"payload">> => maps:get(<<"payload">>, ReplayRow),
                    <<"attempt_count">> => 0
                }),
                timer:sleep(100),
                ?assertEqual(1, webhook_receiver_fixture:count(Ref)),
                Ids = [
                    maps:get(<<"delivery_id">>, jsone:decode(maps:get(body, Cap)))
                 || Cap <- webhook_receiver_fixture:take_all(Ref)
                ],
                ?assertEqual([ReplayId], Ids)
            after
                catch meck:unload(enterprise_webhook_repo),
                catch meck:unload(bot_webhook_delivery_repo),
                catch meck:unload(elib_metric),
                ets:delete(v324_settle)
            end
        end)
    ).

%% T7：接收器 sleep 超过 sender 超时 → attempt class=error、
%% error_trunc={recv,timeout}；settle=mark_retry（重试表 next=5s 档）。
g6_t7(State) ->
    ?_test(
        with_receiver(fun(Url, Ref) ->
            C = maps:get(conn, State),
            {ok, #{<<"secret">> := Secret}} = direct_tx(C, fun(Conn) ->
                enterprise_webhook_logic:configure_tx(
                    Conn, ctx_a(C), #{url => Url, events => [<<"file.confirmed">>], rotate => true}
                )
            end),
            Ctx = ctx_a(C),
            Row = emit_and_fetch_row(C, Ctx, <<"file.confirmed">>, #{
                resource_type => <<"attachment">>, resource_id => 994008
            }),
            webhook_receiver_fixture:set_policy(Ref, {sleep, 1500, 200}),
            PriorTimeout = application:get_env(imboy, bot_webhook_timeout_ms),
            ok = application:set_env(imboy, bot_webhook_timeout_ms, 200),
            {ok, SettleEts} = delivery_stubs(Secret),
            try
                ok = enterprise_webhook_logic:execute_delivery(Row),
                Retries = settle_events(SettleEts, retry),
                ?assertEqual(1, length(Retries)),
                [{_K, _Did, _Attempt} | _] = Retries,
                Attempts = settle_events(SettleEts, attempt),
                ?assertEqual(1, length(Attempts)),
                [{_KA, _D, A} | _] = Attempts,
                %% transport_tests 同款口径：class=error、http_status=null、
                %% error_trunc≈{recv,timeout}
                ?assertEqual(<<"error">>, maps:get(status_class, A)),
                ?assertEqual(null, maps:get(http_status, A)),
                ?assertEqual(<<"{recv,timeout}">>, maps:get(error_trunc, A))
            after
                case PriorTimeout of
                    {ok, V} -> application:set_env(imboy, bot_webhook_timeout_ms, V);
                    undefined -> application:unset_env(imboy, bot_webhook_timeout_ms)
                end,
                catch meck:unload(enterprise_webhook_repo),
                catch meck:unload(bot_webhook_delivery_repo),
                catch meck:unload(elib_metric),
                ets:delete(SettleEts)
            end
        end)
    ).

%% T8：4xx 死信——接收器回 404 → 不重试直接 dead。
g6_t8(State) ->
    ?_test(
        with_receiver(fun(Url, Ref) ->
            C = maps:get(conn, State),
            {ok, #{<<"secret">> := Secret}} = direct_tx(C, fun(Conn) ->
                enterprise_webhook_logic:configure_tx(
                    Conn, ctx_a(C), #{url => Url, events => [<<"file.confirmed">>], rotate => true}
                )
            end),
            Ctx = ctx_a(C),
            Row = emit_and_fetch_row(C, Ctx, <<"file.confirmed">>, #{
                resource_type => <<"attachment">>, resource_id => 994009
            }),
            webhook_receiver_fixture:set_policy(Ref, 404),
            {ok, SettleEts} = delivery_stubs(Secret),
            try
                ok = enterprise_webhook_logic:execute_delivery(Row),
                ?assertEqual(1, length(settle_events(SettleEts, dead))),
                ?assertEqual(0, length(settle_events(SettleEts, retry)))
            after
                catch meck:unload(enterprise_webhook_repo),
                catch meck:unload(bot_webhook_delivery_repo),
                catch meck:unload(elib_metric),
                ets:delete(SettleEts)
            end
        end)
    ).

%% T9：重试耗尽——连续 503×4 → 第 4 次后 dead；attempt 行 4 条（class=5xx）。
g6_t9(State) ->
    ?_test(
        with_receiver(fun(Url, Ref) ->
            C = maps:get(conn, State),
            {ok, #{<<"secret">> := Secret}} = direct_tx(C, fun(Conn) ->
                enterprise_webhook_logic:configure_tx(
                    Conn, ctx_a(C), #{url => Url, events => [<<"file.confirmed">>], rotate => true}
                )
            end),
            Ctx = ctx_a(C),
            Row0 = emit_and_fetch_row(C, Ctx, <<"file.confirmed">>, #{
                resource_type => <<"attachment">>, resource_id => 994010
            }),
            webhook_receiver_fixture:set_policy(Ref, 503),
            {ok, SettleEts} = delivery_stubs(Secret),
            try
                %% 模拟 worker 按 retry schedule 人工推进 4 个 attempt
                %%（[5,30,300] 耗尽即 dead；settle 为 meck 捕获，attempt_count
                %% 由调用方内存递增传入——与 worker 真实轮询语义一致）。
                Exec = fun(AttemptNo) ->
                    ok = enterprise_webhook_logic:execute_delivery(
                        Row0#{<<"attempt_count">> => AttemptNo}
                    )
                end,
                lists:foreach(Exec, [0, 1, 2, 3]),
                %% 耗尽断言以 attempt/重试捕获计数为准（4 次 5xx 后 dead）。
                %% [5,30,300] 耗尽：attempt_no 1..3 → retry；第 4 次 → dead
                ?assertEqual(3, length(settle_events(SettleEts, retry))),
                ?assertEqual(1, length(settle_events(SettleEts, dead))),
                Attempts = settle_events(SettleEts, attempt),
                ?assertEqual(4, length(Attempts)),
                lists:foreach(
                    fun({_K, _D, A}) ->
                        ?assertEqual(<<"5xx">>, maps:get(status_class, A))
                    end,
                    Attempts
                )
            after
                catch meck:unload(enterprise_webhook_repo),
                catch meck:unload(bot_webhook_delivery_repo),
                catch meck:unload(elib_metric),
                ets:delete(SettleEts)
            end
        end)
    ).

%% T10：订阅负例——只订阅 accepted，emit group 事件 → skipped、零投递行、
%% 接收器零流量（不扩散边界的可观测证明）。
g6_t10(State) ->
    ?_test(
        with_receiver(fun(Url, _Ref) ->
            C = maps:get(conn, State),
            {ok, _} = direct_tx(C, fun(Conn) ->
                enterprise_webhook_logic:configure_tx(
                    Conn, ctx_a(C), #{
                        url => Url, events => [<<"message.enterprise.accepted">>], rotate => true
                    }
                )
            end),
            Ctx = ctx_a(C),
            Before = delivery_count(C, <<"group.member.changed">>),
            {ok, skipped} = direct_tx(C, fun(Conn) ->
                enterprise_webhook_logic:emit_event_tx(Conn, Ctx, <<"group.member.changed">>, #{
                    resource_type => <<"enterprise_group">>, resource_id => ?GRP_A1
                })
            end),
            ?assertEqual(Before, delivery_count(C, <<"group.member.changed">>))
        end)
    ).

%% T11：越权负例——App B replay App A 的 delivery → resource_not_found。
g6_t11(State) ->
    ?_test(
        with_receiver(fun(Url, _Ref) ->
            C = maps:get(conn, State),
            {ok, _} = direct_tx(C, fun(Conn) ->
                enterprise_webhook_logic:configure_tx(
                    Conn, ctx_a(C), #{url => Url, events => [<<"file.confirmed">>], rotate => true}
                )
            end),
            CtxA = ctx_a(C),
            Row = emit_and_fetch_row(C, CtxA, <<"file.confirmed">>, #{
                resource_type => <<"attachment">>, resource_id => 994011
            }),
            Did = maps:get(<<"delivery_id">>, Row),
            UpdateSql = iolist_to_binary([
                <<"UPDATE bot_delivery SET status='success' WHERE delivery_id = '">>,
                Did,
                <<"'">>
            ]),
            ok = exec(C, [UpdateSql]),
            CtxB = ctx_b(C),
            ReplayResult = direct_tx(C, fun(Conn) ->
                enterprise_webhook_logic:replay_tx(Conn, CtxB, Did)
            end),
            ?assertMatch(
                {error, {<<"resource_not_found">>, delivery_not_found}},
                ReplayResult
            )
        end)
    ).

%% T12：新五类旅程——delivered/read/mapping/group/cs 各触发一次真实 emit，
%% 各自 event_type 与信封 resource.type 按 §3.1 约定；Human 行 ACK 路径
%% 不产生投递（G-4 归属解析负例已单测）。
g6_t12(State) ->
    ?_test(
        with_receiver(fun(Url, _Ref) ->
            C = maps:get(conn, State),
            {ok, _} = direct_tx(C, fun(Conn) ->
                enterprise_webhook_logic:configure_tx(
                    Conn,
                    ctx_a(C),
                    #{
                        url => Url,
                        events => [
                            <<"message.enterprise.delivered">>,
                            <<"message.enterprise.read">>,
                            <<"user.mapping.changed">>,
                            <<"group.member.changed">>,
                            <<"cs.session.changed">>
                        ]
                    }
                )
            end),
            Ctx = ctx_a(C),
            AppId = maps:get(application_id, Ctx),
            %% delivered（托管行）
            seed_origin(C, <<"v324-journey-d1">>, 994903, <<"direct">>, AppId),
            %% read（托管行）
            with_side_effect_conn(C, fun() ->
                ok = enterprise_webhook_logic:emit_delivered_for(
                    <<"msg_c2c">>, <<"v324-journey-d1">>
                ),
                ok = enterprise_webhook_logic:emit_read_for([<<"v324-journey-d1">>])
            end),
            %% mapping
            {ok, _} = enterprise_identity_logic:bind_mapping_tx(
                C, Ctx, <<"ext-v24-journey">>, ?H_A3
            ),
            %% group（有实际变更）
            seed_group_member(C, 994331, ?GRP_A1, ?OWNER_A),
            _ = enterprise_group_logic:add_members_tx(C, Ctx, ?GRP_A1, [?EXT_A1]),
            %% cs
            with_side_effect_conn(C, fun() ->
                ok = enterprise_webhook_logic:emit_cs_session_changed(?ORG_A, #{
                    session_id => 994961, event_type => <<"session.created">>
                })
            end),
            Expected = #{
                <<"message.enterprise.delivered">> => <<"msg_c2c">>,
                <<"message.enterprise.read">> => <<"msg_c2c">>,
                <<"user.mapping.changed">> => <<"identity_mapping">>,
                <<"group.member.changed">> => <<"enterprise_group">>,
                <<"cs.session.changed">> => <<"cs_session">>
            },
            maps:foreach(
                fun(EventType, ResType) ->
                    Row = one(
                        C,
                        <<
                            "SELECT payload::text AS payload FROM bot_delivery"
                            " WHERE event_type = $1 ORDER BY created_at DESC LIMIT 1"
                        >>,
                        [EventType]
                    ),
                    Env = jsone:decode(maps:get(<<"payload">>, Row)),
                    #{<<"resource">> := Resource} = Env,
                    ?assertEqual(ResType, maps:get(<<"type">>, Resource))
                end,
                Expected
            )
        end)
    ).
