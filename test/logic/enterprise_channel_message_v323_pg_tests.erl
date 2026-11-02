-module(enterprise_channel_message_v323_pg_tests).

%%%
% V3-23 企业应用代发矩阵 backend 验证（G23 工程面）：
%   * A1/A2 白名单扩展：image / link_card / business_action_card
%     × S1 指定员工（direct_tx）/ S2 企业群（group_tx）/ S3 企业频道
%     （INT-50 channel_tx）成功旅程 + payload 形状（wire 值与 flutter B5
%     message_type_constants.dart 的 link_card/business_action_card 对齐）；
%   * B1–B6 企业频道消息通道：迁移 00000241（origin CHECK 扩 channel +
%     enterprise_channel_message 表）、INT-50 logic 成功/负例、
%     origin 账本 channel kind、审计 msg_channel；
%   * E1–E5 NEGATIVE 固化：S4 普通群 org 边界（resource_not_found）、
%     S9 WebRTC 信令通道拒绝（invalid_msg_type）、T23 H-H E2EE 代发拒绝
%     （invalid_msg_type，恒非 E2EE）、跨 org 频道不泄露存在性、
%     human 非订阅者越权、白名单封闭（imageMulti 仍拒——DISCOVERY 面
%     不擅自扩）。
%
% 一次性 marker 库（inttest_marker_db 配方；ID 段 993xxx 与既有套件隔离）。
% 附件 find_confirmed 走 meck 桩（confirm 链为 V3-22 已验面；本卡聚焦
% payload 形状与授权链）。
%%%

-export([v323_cases/0]).

%% eunit 约定入口（t= 单跑与全量 EUNIT_MODS 同一形态；先例
%% enterprise_msg_asset_webhook_pg_tests:enterprise_msg_asset_webhook_pg_test_/0）。
enterprise_channel_message_v323_pg_test_() ->
    v323_cases().

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").
-include("eunit_setup.hrl").

%% ---- 夹具 ID 段（993 段，与 991/992 段既有套件互不冲突） ----

-define(ORG_A, 993101).
-define(ORG_B, 993102).

-define(OWNER_A, 993001).
-define(OWNER_B, 993002).
-define(H_A1, 993011).
-define(H_A2, 993012).
-define(PRIN_A, 993014).
-define(H_B1, 993021).

-define(WS_A1, 993201).
-define(WS_B1, 993211).

-define(GRP_A1, 993301).
-define(GRP_PERSONAL, 993302).

-define(CH_A1, 993401).
-define(CH_B1, 993411).

-define(EXT_A1, <<"ext-v23a1">>).
-define(EXT_A2, <<"ext-v23a2">>).
-define(EXT_B1, <<"ext-v23b1">>).

%%%===================================================================
%%% 入口（t= 单跑：IMBOYENV=local make eunit-local t=enterprise_channel_message_v323_pg_tests）
%%%===================================================================

v323_cases() ->
    {timeout, 120,
        {setup, fun setup_conn/0, fun close_conn/1, fun(State) ->
            C = maps:get(conn, State),
            meck_init(),
            [
                {"ch_text_application_success", with_tx(C, fun(C1) -> ch_text_app(C1, State) end)},
                {"ch_human_subscriber_success",
                    with_tx(C, fun(C1) -> ch_text_human(C1, State) end)},
                {"ch_image_payload_shape", with_tx(C, fun(C1) -> ch_image(C1, State) end)},
                {"ch_link_card_payload_shape", with_tx(C, fun(C1) -> ch_link_card(C1, State) end)},
                {"ch_business_action_card_payload_shape",
                    with_tx(C, fun(C1) -> ch_action_card(C1, State) end)},
                {"direct_new_types_success",
                    with_tx(C, fun(C1) -> direct_new_types(C1, State) end)},
                {"group_new_types_success", with_tx(C, fun(C1) -> group_new_types(C1, State) end)},
                {"neg_personal_group_fail_closed",
                    with_tx(C, fun(C1) -> neg_personal_group(C1, State) end)},
                {"neg_webrtc_msg_type_rejected", with_tx(C, fun(C1) -> neg_webrtc(C1, State) end)},
                {"neg_e2ee_msg_type_rejected", with_tx(C, fun(C1) -> neg_e2ee(C1, State) end)},
                {"neg_cross_org_channel_no_existence_leak",
                    with_tx(C, fun(C1) -> neg_cross_org_channel(C1, State) end)},
                {"neg_human_not_channel_subscriber",
                    with_tx(C, fun(C1) -> neg_not_subscriber(C1, State) end)},
                {"neg_link_card_missing_url",
                    with_tx(C, fun(C1) -> neg_link_card_url(C1, State) end)},
                {"neg_action_params_not_object",
                    with_tx(C, fun(C1) -> neg_action_params(C1, State) end)},
                {"neg_image_unconfirmed_attachment",
                    with_tx(C, fun(C1) -> neg_image_unconfirmed(C1, State) end)},
                {"neg_whitelist_stays_closed",
                    with_tx(C, fun(C1) -> neg_image_multi(C1, State) end)},
                {"origin_channel_kind_persisted",
                    with_tx(C, fun(C1) -> origin_channel(C1, State) end)},
                {"audit_msg_channel_row", with_tx(C, fun(C1) -> audit_channel(C1, State) end)}
            ] ++ []
        end}}.

meck_init() ->
    catch meck:unload(enterprise_asset_repo),
    ok = meck:new(enterprise_asset_repo, [no_link, passthrough]),
    ok = meck:expect(enterprise_asset_repo, find_confirmed_tx, fun(_C, _Org, _App, Key) ->
        {ok, #{
            <<"id">> => 993999001,
            <<"object_key">> => Key,
            <<"name">> => <<"fixture.png">>,
            <<"size">> => 2048,
            <<"mime_type">> => <<"image/png">>
        }}
    end),
    ok.

%%%===================================================================
%%% Fixture
%%%===================================================================

setup_conn() ->
    case application:load(imboy) of
        ok -> ok;
        {error, {already_loaded, imboy}} -> ok
    end,
    try
        elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3})
    catch
        _:_ -> ok
    end,
    {ok, _} = application:ensure_all_started(throttle),
    State = inttest_marker_db:provision(#{
        env_prefix => <<"V323_INTTEST">>,
        connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
    }),
    C = maps:get(conn, State),
    ok = exec(C, <<"BEGIN">>),
    try
        seed_matrix(C)
    catch
        Class:Reason:Stack ->
            _ = exec_quiet(C, <<"ROLLBACK">>),
            inttest_marker_db:release(State),
            erlang:raise(Class, {fixture_seed_failed, Reason}, Stack)
    end,
    ok = exec(C, <<"COMMIT">>),
    AppA = one(C, <<"SELECT id FROM enterprise_application WHERE application_key = $1">>, [
        <<"v323-oa-a">>
    ]),
    State#{conn => C, app_a => maps:get(<<"id">>, AppA)}.

close_conn(State) ->
    catch meck:unload(enterprise_asset_repo),
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
    seed_user(C, ?OWNER_A),
    seed_user(C, ?OWNER_B),
    seed_user(C, ?H_A1),
    seed_user(C, ?H_A2),
    seed_user(C, ?PRIN_A),
    seed_user(C, ?H_B1),
    seed_org(C, ?ORG_A, ?OWNER_A),
    seed_org(C, ?ORG_B, ?OWNER_B),
    seed_org_member(C, ?ORG_A, ?H_A1),
    seed_org_member(C, ?ORG_A, ?H_A2),
    seed_org_member(C, ?ORG_A, ?PRIN_A),
    seed_org_member(C, ?ORG_B, ?H_B1),
    seed_workspace(C, ?WS_A1, ?ORG_A, ?OWNER_A),
    seed_workspace(C, ?WS_B1, ?ORG_B, ?OWNER_B),
    seed_group(C, ?GRP_A1, ?WS_A1, ?OWNER_A),
    seed_group(C, ?GRP_PERSONAL, null, ?OWNER_A),
    seed_channel(C, ?CH_A1, ?WS_A1, ?OWNER_A),
    seed_channel(C, ?CH_B1, ?WS_B1, ?OWNER_B),
    seed_subscription(C, ?CH_A1, ?H_A1),
    {ok, _AppA} = enterprise_application_repo:create_tx(
        C, ?ORG_A, <<"v323-oa-a">>, <<"v323 org A oa"/utf8>>, ?PRIN_A
    ),
    {ok, AppB} = enterprise_application_repo:create_tx(
        C, ?ORG_B, <<"v323-oa-b">>, <<"v323 org B oa"/utf8>>, ?H_B1
    ),
    ok = seed_mapping(C, ?ORG_A, app_id(C, <<"v323-oa-a">>), ?EXT_A1, ?H_A1),
    ok = seed_mapping(C, ?ORG_A, app_id(C, <<"v323-oa-a">>), ?EXT_A2, ?H_A2),
    ok = seed_mapping(C, ?ORG_B, maps:get(<<"id">>, AppB), ?EXT_B1, ?H_B1),
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
        ", 'x', 'v993_u",
        integer_to_binary(Uid),
        <<"', 0, 1, '127.0.0.1', 'x')">>
    ]).

seed_org(C, OrgId, OwnerUid) ->
    exec(C, [
        <<"INSERT INTO organization (id, name, owner_id, status, branding, settings, created_at, updated_at) VALUES (">>,
        integer_to_binary(OrgId),
        ", 'v993-org', ",
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
        ", 'v993-ws', ",
        integer_to_binary(OwnerUid),
        <<", 'active', ">>,
        integer_to_binary(OrgId),
        ", CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)"
    ]).

seed_group(C, GroupId, WsId, OwnerUid) ->
    WsPart =
        case WsId of
            null -> <<"NULL">>;
            _ -> integer_to_binary(WsId)
        end,
    Scope =
        case WsId of
            null -> <<"personal">>;
            _ -> <<"workspace">>
        end,
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
        <<", 500, 1, 'v993_g_">>,
        integer_to_binary(GroupId),
        <<"', 1, '">>,
        Scope,
        <<"', ">>,
        WsPart,
        <<", CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)">>
    ]).

seed_channel(C, ChannelId, WsId, CreatorUid) ->
    exec(C, [
        <<
            "INSERT INTO channel (id, name, creator_uid, status, scope, workspace_id,"
            " visibility, access_type, join_policy, version, created_at, updated_at) VALUES ("
        >>,
        integer_to_binary(ChannelId),
        ", 'v993-channel', ",
        integer_to_binary(CreatorUid),
        <<", 1, 'workspace', ">>,
        integer_to_binary(WsId),
        ", 1, 0, 1, 1, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)"
    ]).

seed_subscription(C, ChannelId, Uid) ->
    exec(C, [
        <<"INSERT INTO channel_subscription (id, channel_id, user_id, status, subscribed_at) VALUES (">>,
        integer_to_binary(elib_tsid:generate(channel_subscription)),
        ", ",
        integer_to_binary(ChannelId),
        ", ",
        integer_to_binary(Uid),
        <<", 1, CURRENT_TIMESTAMP)">>
    ]).

seed_mapping(C, OrgId, AppId, Ext, Uid) ->
    case enterprise_external_identity_repo:bind_tx(C, OrgId, AppId, Ext, Uid) of
        {ok, _} -> ok;
        {error, identity_already_mapped} -> ok;
        {error, Reason} -> erlang:error({seed_mapping_failed, Reason})
    end.

%%%===================================================================
%%% Ctx / 输入
%%%===================================================================

ctx_a(C) ->
    enterprise_hmac_test_support:context_tx(C, ?ORG_A, app_id(C, <<"v323-oa-a">>)).

ctx_b(C) ->
    enterprise_hmac_test_support:context_tx(C, ?ORG_B, app_id(C, <<"v323-oa-b">>)).

channel_input(MsgType, Extra) ->
    maps:merge(
        #{
            sender_mode => <<"application">>,
            msg_type => MsgType
        },
        Extra
    ).

%%%===================================================================
%%% S3 企业频道成功旅程
%%%===================================================================

ch_text_app(C, _State) ->
    Ctx = ctx_a(C),
    {ok, Result} = enterprise_channel_message_logic:channel_tx(
        C, Ctx, ?CH_A1, channel_input(<<"text">>, #{content => <<"hello channel"/utf8>>})
    ),
    RowId0 = maps:get(<<"webhook_resource_id">>, Result),
    ?assert(is_integer(RowId0)),
    ?assertEqual(<<"application">>, maps:get(<<"sender_kind">>, Result)),
    Row = one(
        C,
        <<"SELECT msg_type, payload::text AS payload FROM enterprise_channel_message WHERE msg_id = $1">>,
        [maps:get(<<"msg_id">>, Result)]
    ),
    ?assertEqual(<<"text">>, maps:get(<<"msg_type">>, Row)),
    Payload = jsone:decode(maps:get(<<"payload">>, Row)),
    Origin = maps:get(<<"origin">>, Payload),
    ?assertEqual(
        <<"enterprise_application">>, maps:get(<<"origin">>, Origin, maps:get(<<"kind">>, Origin))
    ),
    ?assertEqual(1, channel_count(C, ?CH_A1)).

ch_text_human(C, _State) ->
    Ctx = ctx_a(C),
    {ok, Result} = enterprise_channel_message_logic:channel_tx(
        C,
        Ctx,
        ?CH_A1,
        channel_input(<<"text">>, #{
            sender_mode => <<"human">>,
            sender_user_id => ?EXT_A1,
            content => <<"human to channel"/utf8>>
        })
    ),
    ?assertEqual(<<"human">>, maps:get(<<"sender_kind">>, Result)),
    ?assertMatch(Id when is_integer(Id), maps:get(<<"sender_user_id">>, Result)).

ch_image(C, _State) ->
    Ctx = ctx_a(C),
    {ok, Result} = enterprise_channel_message_logic:channel_tx(
        C,
        Ctx,
        ?CH_A1,
        channel_input(<<"image">>, #{
            object_key => <<"org/app/fixture.png">>, width => 1920, height => 1080
        })
    ),
    Row = one(
        C,
        <<"SELECT payload::text AS payload FROM enterprise_channel_message WHERE msg_id = $1">>,
        [maps:get(<<"msg_id">>, Result)]
    ),
    Payload = jsone:decode(maps:get(<<"payload">>, Row)),
    Image = maps:get(<<"image">>, Payload),
    ?assertEqual(<<"org/app/fixture.png">>, maps:get(<<"object_key">>, Image)),
    ?assertEqual(1920, maps:get(<<"width">>, Image)),
    ?assertEqual(1080, maps:get(<<"height">>, Image)),
    ?assertEqual(2048, maps:get(<<"size">>, Image)),
    ?assertMatch(#{<<"origin">> := #{<<"kind">> := <<"enterprise_application">>}}, Payload).

ch_link_card(C, _State) ->
    Ctx = ctx_a(C),
    {ok, Result} = enterprise_channel_message_logic:channel_tx(
        C,
        Ctx,
        ?CH_A1,
        channel_input(<<"link_card">>, #{
            title => <<"页面标题"/utf8>>,
            url => <<"https://example.com/page">>,
            description => <<"摘要"/utf8>>,
            thumb => <<"https://cdn.example.com/t.png">>
        })
    ),
    Payload = payload_of(C, Result),
    Card = maps:get(<<"link_card">>, Payload),
    ?assertEqual(<<"页面标题"/utf8>>, maps:get(<<"title">>, Card)),
    ?assertEqual(<<"https://example.com/page">>, maps:get(<<"url">>, Card)),
    ?assertEqual(<<"摘要"/utf8>>, maps:get(<<"description">>, Card)),
    ?assertEqual(<<"https://cdn.example.com/t.png">>, maps:get(<<"thumb">>, Card)),
    ?assert(maps:is_key(<<"origin">>, Payload)).

ch_action_card(C, _State) ->
    Ctx = ctx_a(C),
    {ok, Result} = enterprise_channel_message_logic:channel_tx(
        C,
        Ctx,
        ?CH_A1,
        channel_input(<<"business_action_card">>, #{
            title => <<"审批操作"/utf8>>,
            action_type => <<"approve">>,
            url => <<"https://oa.example.com/approve/1">>,
            action_params => #{<<"order_id">> => <<"A-1">>}
        })
    ),
    Payload = payload_of(C, Result),
    Card = maps:get(<<"business_action_card">>, Payload),
    ?assertEqual(<<"审批操作"/utf8>>, maps:get(<<"title">>, Card)),
    ?assertEqual(<<"approve">>, maps:get(<<"action_type">>, Card)),
    ?assertEqual(#{<<"order_id">> => <<"A-1">>}, maps:get(<<"action_params">>, Card)).

payload_of(C, Result) ->
    Row = one(
        C,
        <<"SELECT payload::text AS payload FROM enterprise_channel_message WHERE msg_id = $1">>,
        [maps:get(<<"msg_id">>, Result)]
    ),
    jsone:decode(maps:get(<<"payload">>, Row)).

channel_count(C, ChannelId) ->
    {ok, N} = enterprise_channel_message_repo:count_for_channel_tx(C, ChannelId),
    N.

%%%===================================================================
%%% A1/A2 × S1/S2（direct/group 新类型）
%%%===================================================================

direct_new_types(C, _State) ->
    Ctx = ctx_a(C),
    Base = #{sender_mode => <<"application">>, recipient_user_id => ?EXT_A2},
    {ok, R1} = enterprise_message_logic:direct_tx(
        C, Ctx, Base#{msg_type => <<"image">>, object_key => <<"org/app/a.png">>}
    ),
    {ok, R2} = enterprise_message_logic:direct_tx(
        C, Ctx, Base#{
            msg_type => <<"link_card">>,
            title => <<"t"/utf8>>,
            url => <<"https://example.com/x">>
        }
    ),
    {ok, R3} = enterprise_message_logic:direct_tx(
        C, Ctx, Base#{
            msg_type => <<"business_action_card">>,
            title => <<"t"/utf8>>,
            action_type => <<"open">>
        }
    ),
    Row1 = direct_payload(C, maps:get(<<"msg_id">>, R1)),
    ?assertMatch(#{<<"image">> := #{<<"object_key">> := <<"org/app/a.png">>}}, Row1),
    Row2 = direct_payload(C, maps:get(<<"msg_id">>, R2)),
    ?assertMatch(#{<<"link_card">> := #{<<"url">> := <<"https://example.com/x">>}}, Row2),
    Row3 = direct_payload(C, maps:get(<<"msg_id">>, R3)),
    ?assertMatch(#{<<"business_action_card">> := #{<<"action_type">> := <<"open">>}}, Row3).

group_new_types(C, _State) ->
    Ctx = ctx_a(C),
    {ok, R1} = enterprise_message_logic:group_tx(
        C,
        Ctx,
        ?GRP_A1,
        #{
            sender_mode => <<"application">>,
            msg_type => <<"image">>,
            object_key => <<"org/app/g.png">>
        }
    ),
    {ok, R2} = enterprise_message_logic:group_tx(
        C,
        Ctx,
        ?GRP_A1,
        #{
            sender_mode => <<"application">>,
            msg_type => <<"link_card">>,
            title => <<"g"/utf8>>,
            url => <<"https://example.com/g">>
        }
    ),
    ?assertMatch(
        #{<<"image">> := #{<<"object_key">> := <<"org/app/g.png">>}},
        group_payload(C, maps:get(<<"msg_id">>, R1))
    ),
    ?assertMatch(
        #{<<"link_card">> := #{<<"url">> := <<"https://example.com/g">>}},
        group_payload(C, maps:get(<<"msg_id">>, R2))
    ).

direct_payload(C, MsgId) ->
    Row = one(C, <<"SELECT payload::text AS payload FROM msg_c2c WHERE msg_id = $1">>, [MsgId]),
    jsone:decode(maps:get(<<"payload">>, Row)).

group_payload(C, MsgId) ->
    Row = one(C, <<"SELECT payload::text AS payload FROM msg_c2g WHERE msg_id = $1">>, [MsgId]),
    jsone:decode(maps:get(<<"payload">>, Row)).

%%%===================================================================
%%% NEGATIVE（E1–E5 + 白名单封闭）
%%%===================================================================

%% E1：S4 普通群 = org 边界 fail-closed（resource_not_found，不泄露存在性）。
neg_personal_group(C, _State) ->
    Ctx = ctx_a(C),
    ?assertMatch(
        {error, {<<"resource_not_found">>, group_not_found}},
        enterprise_message_logic:group_tx(
            C,
            Ctx,
            ?GRP_PERSONAL,
            #{sender_mode => <<"application">>, msg_type => <<"text">>, content => <<"x"/utf8>>}
        )
    ).

%% E2/E4：S9 WebRTC 信令域通道拒绝（msg_type 白名单外 → invalid_msg_type；
%% 应用代发没有 webrtc 通道入口——业务消息不走信令承载）。
neg_webrtc(C, _State) ->
    Ctx = ctx_a(C),
    ?assertMatch(
        {error, {<<"invalid_request">>, invalid_msg_type}},
        enterprise_message_logic:direct_tx(
            C,
            Ctx,
            #{
                sender_mode => <<"application">>,
                recipient_user_id => ?EXT_A2,
                msg_type => <<"webrtcAudio">>
            }
        )
    ),
    ?assertMatch(
        {error, {<<"invalid_request">>, invalid_msg_type}},
        enterprise_channel_message_logic:channel_tx(
            C, Ctx, ?CH_A1, channel_input(<<"webrtcVideo">>, #{})
        )
    ).

%% E3：T23 H-H E2EE 代发拒绝（D02：不生成伪造真人密文；企业托管恒非 E2EE）。
neg_e2ee(C, _State) ->
    Ctx = ctx_a(C),
    ?assertMatch(
        {error, {<<"invalid_request">>, invalid_msg_type}},
        enterprise_message_logic:direct_tx(
            C,
            Ctx,
            #{
                sender_mode => <<"application">>,
                recipient_user_id => ?EXT_A2,
                msg_type => <<"e2ee">>,
                payload => #{<<"ciphertext">> => <<"xx">>}
            }
        )
    ).

%% 跨 org 频道：resource_not_found（不泄露存在性）。
neg_cross_org_channel(C, _State) ->
    Ctx = ctx_a(C),
    ?assertMatch(
        {error, {<<"resource_not_found">>, channel_not_found}},
        enterprise_channel_message_logic:channel_tx(
            C, Ctx, ?CH_B1, channel_input(<<"text">>, #{content => <<"x"/utf8>>})
        )
    ).

%% human sender 非频道订阅者 = 越权（organization_boundary_violation）。
neg_not_subscriber(C, _State) ->
    Ctx = ctx_a(C),
    ?assertMatch(
        {error, {<<"organization_boundary_violation">>, sender_not_channel_subscriber}},
        enterprise_channel_message_logic:channel_tx(
            C,
            Ctx,
            ?CH_A1,
            channel_input(<<"text">>, #{
                sender_mode => <<"human">>,
                sender_user_id => ?EXT_A2,
                content => <<"not subscribed"/utf8>>
            })
        )
    ).

%% link_card 缺 url / 非法 url → invalid_request。
neg_link_card_url(C, _State) ->
    Ctx = ctx_a(C),
    ?assertMatch(
        {error, {<<"invalid_request">>, {invalid_field, <<"url">>}}},
        enterprise_message_logic:direct_tx(
            C,
            Ctx,
            #{
                sender_mode => <<"application">>,
                recipient_user_id => ?EXT_A2,
                msg_type => <<"link_card">>,
                title => <<"t"/utf8>>
            }
        )
    ),
    ?assertMatch(
        {error, {<<"invalid_request">>, {invalid_field, <<"url">>}}},
        enterprise_message_logic:direct_tx(
            C,
            Ctx,
            #{
                sender_mode => <<"application">>,
                recipient_user_id => ?EXT_A2,
                msg_type => <<"link_card">>,
                title => <<"t"/utf8>>,
                url => <<"ftp://example.com/x">>
            }
        )
    ).

%% action_params 非 object → invalid_action_params。
neg_action_params(C, _State) ->
    Ctx = ctx_a(C),
    ?assertMatch(
        {error, {<<"invalid_request">>, invalid_action_params}},
        enterprise_message_logic:direct_tx(
            C,
            Ctx,
            #{
                sender_mode => <<"application">>,
                recipient_user_id => ?EXT_A2,
                msg_type => <<"business_action_card">>,
                title => <<"t"/utf8>>,
                action_type => <<"open">>,
                action_params => <<"not-an-object">>
            }
        )
    ).

%% image 引用未 confirm/跨 org 附件 → resource_not_found（不泄露存在性）。
neg_image_unconfirmed(C, _State) ->
    Ctx = ctx_a(C),
    ok = meck:expect(enterprise_asset_repo, find_confirmed_tx, fun(_C, _Org, _App, _K) ->
        {error, not_found}
    end),
    ?assertMatch(
        {error, {<<"resource_not_found">>, file_not_confirmed}},
        enterprise_message_logic:direct_tx(
            C,
            Ctx,
            #{
                sender_mode => <<"application">>,
                recipient_user_id => ?EXT_A2,
                msg_type => <<"image">>,
                object_key => <<"missing.png">>
            }
        )
    ),
    meck_init().

%% DISCOVERY 面不擅自扩：imageMulti 等其余注册类型仍在白名单外
%% （invalid_msg_type；DISCOVERY 裁决后才升级 MUST-FIX）。
neg_image_multi(C, _State) ->
    Ctx = ctx_a(C),
    ?assertMatch(
        {error, {<<"invalid_request">>, invalid_msg_type}},
        enterprise_message_logic:direct_tx(
            C,
            Ctx,
            #{
                sender_mode => <<"application">>,
                recipient_user_id => ?EXT_A2,
                msg_type => <<"imageMulti">>
            }
        )
    ).

%%%===================================================================
%%% B1/B6：origin 账本 channel kind + 审计 msg_channel
%%%===================================================================

%% origin 行 conversation_kind='channel'（迁移 00000241 CHECK 扩展生效）；
%% 非法 kind 仍 23514。
origin_channel(C, _State) ->
    Ctx = ctx_a(C),
    {ok, Result} = enterprise_channel_message_logic:channel_tx(
        C, Ctx, ?CH_A1, channel_input(<<"text">>, #{content => <<"origin probe"/utf8>>})
    ),
    RowId = maps:get(<<"webhook_resource_id">>, Result),
    Origin = one(
        C,
        <<
            "SELECT conversation_kind, application_id, sender_kind FROM enterprise_message_origin"
            " WHERE msg_id = $1"
        >>,
        [maps:get(<<"msg_id">>, Result)]
    ),
    ?assertEqual(<<"channel">>, maps:get(<<"conversation_kind">>, Origin)),
    ?assertEqual(<<"application">>, maps:get(<<"sender_kind">>, Origin)),
    ?assertEqual(maps:get(application_id, Ctx), maps:get(<<"application_id">>, Origin)),
    %% 非法 kind 仍被 CHECK 拒（23514）——direct|group 保留，channel 只增不改。
    Rejected =
        try
            exec(C, [
                <<
                    "INSERT INTO enterprise_message_origin (id, conversation_kind,"
                    " organization_id, application_id, sender_kind, msg_row_id, msg_id)"
                    " VALUES (993900001, 'bogus', "
                >>,
                integer_to_binary(?ORG_A),
                <<", ">>,
                integer_to_binary(maps:get(application_id, Ctx)),
                <<", 'application', ">>,
                integer_to_binary(RowId),
                <<", 'bogus-msg')">>
            ]),
            no_constraint_violation
        catch
            _:{sql_error, #error{code = <<"23514">>}, _Sql} -> check_violation_rejected
        end,
    ?assertEqual(check_violation_rejected, Rejected).

%% 审计行：resource_type=msg_channel + actor_role=enterprise_application +
%% detail.origin_application_id（INT-50 审计政策）。
audit_channel(C, _State) ->
    Ctx = ctx_a(C),
    {ok, Result} = enterprise_channel_message_logic:channel_tx(
        C, Ctx, ?CH_A1, channel_input(<<"text">>, #{content => <<"audit probe"/utf8>>})
    ),
    RowId = maps:get(<<"webhook_resource_id">>, Result),
    Audit = one(
        C,
        <<
            "SELECT resource_type, action, actor_role, detail::text AS detail"
            " FROM enterprise_audit_event WHERE resource_id = $1"
        >>,
        [RowId]
    ),
    ?assertEqual(<<"msg_channel">>, maps:get(<<"resource_type">>, Audit)),
    ?assertEqual(<<"message.enterprise.accepted">>, maps:get(<<"action">>, Audit)),
    ?assertEqual(<<"enterprise_application">>, maps:get(<<"actor_role">>, Audit)),
    Detail = jsone:decode(maps:get(<<"detail">>, Audit)),
    ?assertEqual(maps:get(application_id, Ctx), maps:get(<<"origin_application_id">>, Detail)).
