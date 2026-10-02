%% enterprise_msg_asset_webhook_pg_tests
%% EPGZ-04 — 企业附件（INT-07/08）/ OA 代发消息（INT-09/10）/ Webhook
%% （INT-12/13）真库集成测试。
%%
%% 一次性 marker 库（inttest_marker_db 配方，env 前缀 EPGZ04_INTTEST，直连
%% imboy_pg18:4323）。setup 阶段 COMMIT 提交双 Org × 双 Workspace 基线夹具
%% （含 principal 绑定）；业务用例每条 BEGIN ... ROLLBACK，不留数据。
%% 池化路径（worker 投递执行 / secret 解密）一律 meck——eunit VM 未起
%% imboy app，池化连接不可用；DB 断言全部走直连事务连接。
%%
%% 覆盖（plan-gz §4.3/§6/§7.1；manifest INV-5/6/7）：
%%   ① INT-07 presign：企业域前缀/pending 登记/参数矩阵
%%   ② INT-08 confirm：HEAD 核实（meck elib_oss）/ownership 前缀校验/
%%      未 presign 拒/对象不存在拒/超限拒（删对象）/attachment 行
%%      scope='enterprise' + origin 元数据/pending 销账
%%   ③ INT-09 direct sender 全矩阵：human/application 成功/跨 org 未映射/
%%      运行时停用/as_user_id 与 actor_user_id 别名拒/缺 sender_user_id/
%%      scope 无隐含包含/无 principal/参数矩阵/file 未 confirm 拒
%%   ④ INT-10 group：成功/跨 org/personal 群/archived ws/sender 非群成员/
%%      sender 非 ws 成员
%%   ⑤ 幂等三态：inserted->complete->replay / digest_conflict / pending
%%   ⑥ 非 E2EE 断言：msg_c2c/msg_c2g e2ee IS NULL + payload 无 e2ee 键 +
%%      OA 路径零调用 msg_store_ds:stage（meck 计数）+ Human E2EE 回归
%%      （required 模式明文 C2C/C2G 仍 policy_violation，门与既有
%%      msg_c2c/c2g_logic_tests 同源 imboy_policy:validate_message_write/5）
%%   ⑦ origin_application_id 持久化断言（audit 行 detail 列级）
%%   ⑧ INT-12 webhook 配置：成功（bot 行 eapp_ 命名空间 + secret 一次）/
%%      HTTPS 强制/SSRF 私网拒（meck inet）/白名单外事件拒/无 principal 拒/
%%      轮换换新 secret/停用后事件跳过
%%   ⑨ 事件入箱：envelope 形态（无 secret/正文/签名 URL）/未订阅跳过/
%%      file.confirmed 随 confirm 原子入箱
%%   ⑩ INT-13 replay：仅本 app（跨 org / 个人 bot 行拒）/新 delivery id
%%      保留 event id/在途拒
%%   ⑪ 投递执行：2xx success/4xx dead/5xx retry + HMAC(secret, ts "." body)
%%      签名头断言/worker 企业分派 hook（eapp: 委托、纯数字不委托）
%%   ⑫ 导出面负例：无 list-all/批量导出形态
-module(enterprise_msg_asset_webhook_pg_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").
-include("eunit_setup.hrl").

%% ---- 夹具（991 段独立 ID，与 A1 987 / A2 988 / A3 989 段互不冲突） ----

-define(ORG_A, 991101).
-define(ORG_B, 991102).

-define(OWNER_A, 991001).
-define(OWNER_B, 991002).
-define(H_A1, 991011).
-define(H_A2, 991012).
-define(H_A3, 991013).
-define(PRIN_A, 991014).
-define(REMOVED_A, 991015).
-define(H_B1, 991021).

-define(WS_A1, 991201).
-define(WS_A_ARCH, 991203).
-define(WS_B1, 991211).

-define(GRP_A1, 991301).
-define(GRP_PERSONAL, 991302).
-define(GRP_ARCH, 991303).
-define(GRP_B1, 991311).

-define(GM_ID_BASE, 991401).

-define(EXT_A1, <<"ext-a1">>).
-define(EXT_A2, <<"ext-a2">>).
-define(EXT_A3, <<"ext-a3">>).
-define(EXT_B1, <<"ext-b1">>).

-define(SCOPES_FULL, [
    <<"application:read">>,
    <<"identities:read">>,
    <<"identities:write">>,
    <<"groups:write">>,
    <<"files:write">>,
    <<"messages:send">>,
    <<"messages:send_as_human">>,
    <<"webhooks:manage">>
]).
-define(SCOPES_APP_SEND_ONLY, [<<"messages:send">>]).
-define(SCOPES_HUMAN_SEND_ONLY, [<<"messages:send_as_human">>]).

-define(PUBLIC_IP, {93, 184, 216, 34}).

%%%===================================================================
%%% Fixture
%%%===================================================================

setup_conn() ->
    try
        elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3})
    catch
        _:_ -> ok
    end,
    lists:foreach(
        fun(Name) ->
            case lists:member(Name, elib_tsid:registered()) of
                true -> ok;
                false -> elib_tsid:register(Name)
            end
        end,
        [group_info, group_member, enterprise_message, enterprise_audit_event, msg_c2c, msg_c2g]
    ),
    {ok, _} = application:ensure_all_started(throttle),
    State = inttest_marker_db:provision(#{
        env_prefix => <<"EPGZ04_INTTEST">>,
        connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
    }),
    C = maps:get(conn, State),
    ok = exec(C, <<"BEGIN">>),
    try
        seed_matrix(C),
        ok = exec(C, <<"COMMIT">>)
    catch
        Class:Reason:Stack ->
            _ = exec_quiet(C, <<"ROLLBACK">>),
            inttest_marker_db:release(State),
            erlang:raise(Class, {fixture_seed_failed, Reason}, Stack)
    end,
    {AppA, AppB, AppNoPin, AppASendOnly, AppAHumanOnly} = app_ids(C),
    State#{
        conn => C,
        app_a => AppA,
        app_b => AppB,
        app_nopin => AppNoPin,
        app_send_only => AppASendOnly,
        app_human_only => AppAHumanOnly
    }.

close_conn(State) ->
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

%% ---- 夹具矩阵 ----

seed_matrix(C) ->
    seed_user(C, ?OWNER_A, 0, 1),
    seed_user(C, ?OWNER_B, 0, 1),
    seed_user(C, ?H_A1, 0, 1),
    seed_user(C, ?H_A2, 0, 1),
    seed_user(C, ?H_A3, 0, 1),
    seed_user(C, ?PRIN_A, 0, 1),
    seed_user(C, ?REMOVED_A, 0, 1),
    seed_user(C, ?H_B1, 0, 1),
    seed_org(C, ?ORG_A, ?OWNER_A, <<"epgz04-org-a">>),
    seed_org(C, ?ORG_B, ?OWNER_B, <<"epgz04-org-b">>),
    seed_org_member(C, ?ORG_A, ?H_A1, <<"active">>),
    seed_org_member(C, ?ORG_A, ?H_A2, <<"active">>),
    seed_org_member(C, ?ORG_A, ?H_A3, <<"active">>),
    seed_org_member(C, ?ORG_A, ?PRIN_A, <<"active">>),
    seed_org_member(C, ?ORG_A, ?REMOVED_A, <<"active">>),
    seed_org_member(C, ?ORG_B, ?H_B1, <<"active">>),
    seed_workspace(C, ?WS_A1, ?ORG_A, ?OWNER_A, <<"active">>),
    seed_workspace(C, ?WS_A_ARCH, ?ORG_A, ?OWNER_A, <<"archived">>),
    seed_workspace(C, ?WS_B1, ?ORG_B, ?OWNER_B, <<"active">>),
    seed_ws_member(C, ?WS_A1, ?OWNER_A),
    seed_ws_member(C, ?WS_A1, ?H_A1),
    seed_ws_member(C, ?WS_A1, ?H_A2),
    seed_ws_member(C, ?WS_A1, ?PRIN_A),
    %% H_A3 不在 WS_A1（ws 边界负例用）
    seed_ws_member(C, ?WS_B1, ?H_B1),
    seed_group(C, ?GRP_A1, ?WS_A1, ?OWNER_A, <<"workspace">>, 1),
    seed_group(C, ?GRP_ARCH, ?WS_A_ARCH, ?OWNER_A, <<"workspace">>, 1),
    seed_group(C, ?GRP_B1, ?WS_B1, ?OWNER_B, <<"workspace">>, 1),
    seed_group(C, ?GRP_PERSONAL, null, ?OWNER_A, <<"personal">>, 1),
    seed_group_member(C, ?GM_ID_BASE, ?GRP_A1, ?H_A1, 1),
    seed_group_member(C, ?GM_ID_BASE + 1, ?GRP_A1, ?OWNER_A, 4),
    seed_group_member(C, ?GM_ID_BASE + 2, ?GRP_PERSONAL, ?OWNER_A, 4),
    {ok, AppA} = enterprise_application_repo:create_tx(
        C, ?ORG_A, <<"epgz04-oa-a">>, <<"epgz04 org A oa"/utf8>>, {?PRIN_A, ?SCOPES_FULL}
    ),
    {ok, AppB} = enterprise_application_repo:create_tx(
        C, ?ORG_B, <<"epgz04-oa-b">>, <<"epgz04 org B oa"/utf8>>, {?H_B1, ?SCOPES_FULL}
    ),
    {ok, _AppNoPin} = enterprise_application_repo:create_tx(
        C, ?ORG_A, <<"epgz04-oa-nopin">>, <<"epgz04 no principal"/utf8>>, {null, ?SCOPES_FULL}
    ),
    {ok, AppSendOnly} = enterprise_application_repo:create_tx(
        C,
        ?ORG_A,
        <<"epgz04-oa-send">>,
        <<"epgz04 send only"/utf8>>,
        {?PRIN_A, ?SCOPES_APP_SEND_ONLY}
    ),
    {ok, AppHumanOnly} = enterprise_application_repo:create_tx(
        C,
        ?ORG_A,
        <<"epgz04-oa-human">>,
        <<"epgz04 human only"/utf8>>,
        {?PRIN_A, ?SCOPES_HUMAN_SEND_ONLY}
    ),
    ok = seed_mapping(C, ?ORG_A, maps:get(<<"id">>, AppA), ?EXT_A1, ?H_A1),
    ok = seed_mapping(C, ?ORG_A, maps:get(<<"id">>, AppA), ?EXT_A2, ?H_A2),
    ok = seed_mapping(C, ?ORG_A, maps:get(<<"id">>, AppA), ?EXT_A3, ?REMOVED_A),
    ok = seed_mapping(C, ?ORG_B, maps:get(<<"id">>, AppB), ?EXT_B1, ?H_B1),
    ok = seed_mapping(C, ?ORG_A, maps:get(<<"id">>, AppSendOnly), ?EXT_A1, ?H_A1),
    ok = seed_mapping(C, ?ORG_A, maps:get(<<"id">>, AppHumanOnly), ?EXT_A1, ?H_A1),
    ok.

seed_user(C, Uid, AccountType, Status) ->
    exec(C, [
        <<"INSERT INTO \"user\" (id, password, account, account_type, status, reg_ip, reg_cosv) VALUES (">>,
        integer_to_binary(Uid),
        ", 'x', 't991_u",
        integer_to_binary(Uid),
        "', ",
        integer_to_binary(AccountType),
        ", ",
        integer_to_binary(Status),
        <<", '127.0.0.1', 'x')">>
    ]).

seed_org(C, OrgId, OwnerUid, Name) ->
    exec(C, [
        <<"INSERT INTO organization (id, name, owner_id, status, branding, settings, created_at, updated_at) VALUES (">>,
        integer_to_binary(OrgId),
        ", '",
        Name,
        "', ",
        integer_to_binary(OwnerUid),
        <<", 'active', '{}'::jsonb, '{}'::jsonb, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)">>
    ]).

seed_org_member(C, OrgId, Uid, Status) ->
    exec(C, [
        <<"INSERT INTO organization_member (organization_id, user_id, role, status, joined_at, created_at, updated_at) VALUES (">>,
        integer_to_binary(OrgId),
        ", ",
        integer_to_binary(Uid),
        ", 'member', '",
        Status,
        <<"', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)">>
    ]).

seed_workspace(C, WsId, OrgId, OwnerUid, Status) ->
    exec(C, [
        <<"INSERT INTO workspace (id, name, owner_id, status, organization_id, created_at, updated_at) VALUES (">>,
        integer_to_binary(WsId),
        ", 't991_ws_",
        integer_to_binary(WsId),
        "', ",
        integer_to_binary(OwnerUid),
        ", '",
        Status,
        "', ",
        integer_to_binary(OrgId),
        <<", CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)">>
    ]).

seed_ws_member(C, WsId, Uid) ->
    exec(C, [
        <<"INSERT INTO workspace_member (workspace_id, user_id, role, status, joined_at, created_at, updated_at) VALUES (">>,
        integer_to_binary(WsId),
        ", ",
        integer_to_binary(Uid),
        <<", 'member', 'active', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)">>
    ]).

seed_group(C, Gid, WsId, OwnerUid, Scope, Status) ->
    WsValue =
        case WsId of
            null -> <<"NULL">>;
            _ -> integer_to_binary(WsId)
        end,
    exec(C, [
        <<"INSERT INTO \"group\" (id, type, join_limit, owner_uid, creator_uid, member_max, member_count, title, status, scope, workspace_id, created_at, updated_at) VALUES (">>,
        integer_to_binary(Gid),
        <<", 2, 3, ">>,
        integer_to_binary(OwnerUid),
        <<", ">>,
        integer_to_binary(OwnerUid),
        <<", 500, 1, 't991_g_">>,
        integer_to_binary(Gid),
        "', ",
        integer_to_binary(Status),
        <<", '">>,
        Scope,
        <<"', ">>,
        WsValue,
        <<", CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)">>
    ]).

seed_group_member(C, GmId, Gid, Uid, Role) ->
    exec(C, [
        <<"INSERT INTO group_member (id, group_id, user_id, role, status, created_at, updated_at) VALUES (">>,
        integer_to_binary(GmId),
        <<", ">>,
        integer_to_binary(Gid),
        <<", ">>,
        integer_to_binary(Uid),
        <<", ">>,
        integer_to_binary(Role),
        <<", 1, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)">>
    ]).

seed_mapping(C, OrgId, AppId, Ext, Uid) ->
    case enterprise_external_identity_repo:bind_tx(C, OrgId, AppId, Ext, Uid) of
        {ok, _} -> ok;
        {error, Reason} -> erlang:error({seed_mapping_failed, Ext, Reason})
    end.

app_ids(C) ->
    A = one(
        C, <<"SELECT id FROM enterprise_application WHERE application_key = 'epgz04-oa-a'">>, []
    ),
    B = one(
        C, <<"SELECT id FROM enterprise_application WHERE application_key = 'epgz04-oa-b'">>, []
    ),
    N = one(
        C, <<"SELECT id FROM enterprise_application WHERE application_key = 'epgz04-oa-nopin'">>, []
    ),
    S = one(
        C, <<"SELECT id FROM enterprise_application WHERE application_key = 'epgz04-oa-send'">>, []
    ),
    H = one(
        C, <<"SELECT id FROM enterprise_application WHERE application_key = 'epgz04-oa-human'">>, []
    ),
    {
        maps:get(<<"id">>, A),
        maps:get(<<"id">>, B),
        maps:get(<<"id">>, N),
        maps:get(<<"id">>, S),
        maps:get(<<"id">>, H)
    }.

%%%===================================================================
%%% Ctx helpers
%%%===================================================================

ctx_a(State) ->
    #{
        organization_id => ?ORG_A,
        application_id => maps:get(app_a, State),
        granted_scopes => ?SCOPES_FULL,
        principal_user_id => ?PRIN_A
    }.

ctx_a_nopin(State) ->
    #{
        organization_id => ?ORG_A,
        application_id => maps:get(app_nopin, State),
        granted_scopes => ?SCOPES_FULL
    }.

ctx_a_send_only(State) ->
    #{
        organization_id => ?ORG_A,
        application_id => maps:get(app_send_only, State),
        granted_scopes => ?SCOPES_APP_SEND_ONLY,
        principal_user_id => ?PRIN_A
    }.

ctx_a_human_only(State) ->
    #{
        organization_id => ?ORG_A,
        application_id => maps:get(app_human_only, State),
        granted_scopes => ?SCOPES_HUMAN_SEND_ONLY,
        principal_user_id => ?PRIN_A
    }.

ctx_b(State) ->
    #{
        organization_id => ?ORG_B,
        application_id => maps:get(app_b, State),
        granted_scopes => ?SCOPES_FULL,
        principal_user_id => ?H_B1
    }.

direct_input_human() ->
    #{
        sender_mode => <<"human">>,
        sender_user_id => ?EXT_A1,
        recipient_user_id => ?EXT_A2,
        msg_type => <<"text">>,
        content => <<"hello from oa"/utf8>>
    }.

direct_input_app() ->
    #{
        sender_mode => <<"application">>,
        recipient_user_id => ?EXT_A2,
        msg_type => <<"text">>,
        content => <<"notice from application"/utf8>>
    }.

%%%===================================================================
%%% Suite
%%%===================================================================

enterprise_msg_asset_webhook_pg_test_() ->
    {timeout, 900,
        {setup, fun setup_conn/0, fun close_conn/1, fun(State) ->
            C = maps:get(conn, State),
            %% inorder：用例间共享全局 meck（inet/elib_oss/池化 repo 桩），
            %% 并行会 already_started 冲突；真库事务连接本身不可并行复用。
            {inorder, [
                %% ① INT-07 presign
                {"presign_success_enterprise_prefix",
                    with_tx(C, fun(C1) -> presign_success(C1, State) end)},
                {"presign_invalid_inputs", with_tx(C, fun(C1) -> presign_invalid(C1, State) end)},
                %% ② INT-08 confirm
                {"confirm_success_attachment_row",
                    with_tx(C, fun(C1) -> confirm_success(C1, State) end)},
                {"confirm_not_presigned_rejected",
                    with_tx(C, fun(C1) -> confirm_not_presigned(C1, State) end)},
                {"confirm_cross_app_key_rejected",
                    with_tx(C, fun(C1) -> confirm_cross_app(C1, State) end)},
                {"confirm_object_not_found",
                    with_tx(C, fun(C1) -> confirm_not_found(C1, State) end)},
                {"confirm_file_too_large_deleted",
                    with_tx(C, fun(C1) -> confirm_too_large(C1, State) end)},
                %% ③ INT-09 direct sender 矩阵
                {"direct_human_success_non_e2ee",
                    with_tx(C, fun(C1) -> direct_human_success(C1, State) end)},
                {"direct_application_success",
                    with_tx(C, fun(C1) -> direct_application_success(C1, State) end)},
                {"direct_cross_org_recipient_not_mapped",
                    with_tx(C, fun(C1) -> direct_cross_org(C1, State) end)},
                {"direct_unmapped_recipient_not_mapped",
                    with_tx(C, fun(C1) -> direct_unmapped(C1, State) end)},
                {"direct_removed_member_not_mapped",
                    with_tx(C, fun(C1) -> direct_removed(C1, State) end)},
                {"direct_disabled_user_not_mapped_runtime",
                    with_tx(C, fun(C1) -> direct_disabled(C1, State) end)},
                {"direct_alias_fields_rejected",
                    with_tx(C, fun(C1) -> direct_alias_rejected(C1, State) end)},
                {"direct_missing_sender_user_id",
                    with_tx(C, fun(C1) -> direct_missing_sender(C1, State) end)},
                {"direct_scope_no_implicit_grant",
                    with_tx(C, fun(C1) -> direct_scope_negative(C1, State) end)},
                {"direct_application_without_principal",
                    with_tx(C, fun(C1) -> direct_no_principal(C1, State) end)},
                {"direct_invalid_msg_params",
                    with_tx(C, fun(C1) -> direct_invalid_params(C1, State) end)},
                {"direct_file_message_requires_confirmed",
                    with_tx(C, fun(C1) -> direct_file_unconfirmed(C1, State) end)},
                {"direct_file_message_with_confirmed_asset",
                    with_tx(C, fun(C1) -> direct_file_confirmed(C1, State) end)},
                %% ④ INT-10 group
                {"group_human_success", with_tx(C, fun(C1) -> group_success(C1, State) end)},
                {"group_cross_org_not_found",
                    with_tx(C, fun(C1) -> group_cross_org(C1, State) end)},
                {"group_personal_not_found", with_tx(C, fun(C1) -> group_personal(C1, State) end)},
                {"group_archived_ws_not_found",
                    with_tx(C, fun(C1) -> group_archived_ws(C1, State) end)},
                {"group_sender_not_member_boundary",
                    with_tx(C, fun(C1) -> group_sender_not_member(C1, State) end)},
                {"group_sender_not_ws_member_boundary",
                    with_tx(C, fun(C1) -> group_sender_not_ws(C1, State) end)},
                %% ⑤ 幂等三态
                {"idempotency_insert_complete_replay",
                    with_tx(C, fun(C1) -> idem_replay(C1, State) end)},
                {"idempotency_digest_conflict",
                    with_tx(C, fun(C1) -> idem_conflict(C1, State) end)},
                {"idempotency_pending_then_retry",
                    with_tx(C, fun(C1) -> idem_pending(C1, State) end)},
                %% ⑥ 非 E2EE 断言 + Human E2EE 回归
                {"non_e2ee_assertions", with_tx(C, fun(C1) -> non_e2ee(C1, State) end)},
                {"human_e2ee_regression_plaintext_still_rejected", human_e2ee_regression_test_()},
                %% ⑦ origin_application_id 持久化断言
                {"origin_application_id_persisted_in_audit",
                    with_tx(C, fun(C1) -> origin_persisted(C1, State) end)},
                %% ⑧ INT-12 webhook 配置
                {"webhook_configure_success", with_tx(C, fun(C1) -> wh_configure(C1, State) end)},
                {"webhook_configure_https_required",
                    with_tx(C, fun(C1) -> wh_https_required(C1, State) end)},
                {"webhook_configure_ssrf_private_rejected", wh_ssrf_test_()},
                {"webhook_configure_events_whitelist",
                    with_tx(C, fun(C1) -> wh_events_whitelist(C1, State) end)},
                {"webhook_configure_no_principal",
                    with_tx(C, fun(C1) -> wh_no_principal(C1, State) end)},
                {"webhook_rotate_new_secret", with_tx(C, fun(C1) -> wh_rotate(C1, State) end)},
                {"webhook_disable", with_tx(C, fun(C1) -> wh_disable(C1, State) end)},
                %% ⑨ 事件入箱
                {"emit_event_envelope_shape", with_tx(C, fun(C1) -> emit_envelope(C1, State) end)},
                {"emit_event_unsubscribed_skipped",
                    with_tx(C, fun(C1) -> emit_unsubscribed(C1, State) end)},
                {"file_confirmed_event_with_confirm",
                    with_tx(C, fun(C1) -> emit_file_confirmed(C1, State) end)},
                %% ⑩ INT-13 replay
                {"replay_success_new_delivery_keeps_event_id",
                    with_tx(C, fun(C1) -> replay_ok(C1, State) end)},
                {"replay_cross_org_not_found",
                    with_tx(C, fun(C1) -> replay_cross_org(C1, State) end)},
                {"replay_in_flight_rejected",
                    with_tx(C, fun(C1) -> replay_in_flight(C1, State) end)},
                %% ⑩.5 repair-f2-high：handler 级真实分派回归（F2 B4 HIGH——
                %% find_tx arity-2 运行时 undef → INT-09/12/23 真实 500；
                %% logic 层直调覆盖不到 handler 壳，这里补上壳面用例）
                {"repair-f2-high INT-09 handler dispatch",
                    with_tx(C, fun(C1) -> repair_int09_handler(C1, State) end)},
                {"repair-f2-high INT-12 webhook configure handler dispatch",
                    with_tx(C, fun(C1) -> repair_int12_handler(C1, State) end)},
                {"repair-f2-high INT-23 webhook deliveries handler dispatch",
                    with_tx(C, fun(C1) -> repair_int23_handler(C1, State) end)},
                %% ⑪ 投递执行 + worker 分派
                {"execute_delivery_2xx_success", with_tx(C, fun(C1) -> exec_2xx(C1, State) end)},
                {"execute_delivery_4xx_dead", with_tx(C, fun(C1) -> exec_4xx(C1, State) end)},
                {"execute_delivery_5xx_retry_and_signature",
                    with_tx(C, fun(C1) -> exec_5xx(C1, State) end)},
                {"worker_dispatch_enterprise_hook", worker_hook_test_()},
                {"worker_dispatch_bot_path_untouched", worker_bot_path_test_()},
                %% ⑫ 导出面
                {"exported_surface_no_list_all", exported_surface_test_()}
            ]}
        end}}.

%%%===================================================================
%%% ① INT-07 presign
%%%===================================================================

presign_success(C, State) ->
    Ctx = ctx_a(State),
    {ok, Result} = enterprise_asset_logic:presign_tx(C, Ctx, #{
        file_name => <<"report.pdf">>, mime_type => <<"application/pdf">>, size_bytes => 1024
    }),
    Key = maps:get(<<"object_key">>, Result),
    Prefix = enterprise_asset_repo:object_key_prefix(?ORG_A, maps:get(app_a, State)),
    PrefixLen = byte_size(Prefix),
    ?assertMatch(true, byte_size(Key) > PrefixLen),
    ?assertEqual(Prefix, binary:part(Key, 0, PrefixLen)),
    %% 个人附件前缀 u<Uid>/ 物理隔离
    ?assertMatch(false, binary:part(Key, 0, 1) =:= <<"u">>),
    ?assertMatch(true, is_binary(maps:get(<<"put_url">>, Result))),
    ?assertMatch(true, is_integer(maps:get(<<"expires_at">>, Result))),
    Row = one(
        C,
        <<"SELECT bucket, scope FROM attach_pending WHERE object_key = $1">>,
        [Key]
    ),
    ?assertEqual(<<"enterprise">>, maps:get(<<"scope">>, Row)).

presign_invalid(C, State) ->
    Ctx = ctx_a(State),
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_asset_logic:presign_tx(C, Ctx, #{
            file_name => <<"a.pdf">>, mime_type => <<"application/x-evil">>
        })
    ),
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_asset_logic:presign_tx(C, Ctx, #{
            file_name => <<>>, mime_type => <<"application/pdf">>
        })
    ),
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_asset_logic:presign_tx(C, Ctx, #{
            file_name => <<"a.pdf">>, mime_type => <<"application/pdf">>, size_bytes => 0
        })
    ),
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_asset_logic:presign_tx(C, Ctx, <<"not-a-map">>)
    ).

%%%===================================================================
%%% ② INT-08 confirm
%%%===================================================================

presigned_key(C, State) ->
    Ctx = ctx_a(State),
    {ok, #{<<"object_key">> := Key}} = enterprise_asset_logic:presign_tx(C, Ctx, #{
        file_name => <<"data.txt">>, mime_type => <<"text/plain">>
    }),
    Key.

confirm_success(C, State) ->
    Key = presigned_key(C, State),
    meck:new(elib_oss, [passthrough, no_passthrough_cover]),
    meck:expect(
        elib_oss,
        head_object,
        fun(_Bucket, _Key) -> {ok, #{size => 128, content_type => <<"text/plain">>}} end
    ),
    try
        {ok, Result} = enterprise_asset_logic:confirm_tx(C, ctx_a(State), #{
            object_key => Key, file_hash256 => hash_of(<<"x">>)
        }),
        ?assertMatch(true, is_integer(maps:get(<<"file_id">>, Result))),
        ?assertEqual(128, maps:get(<<"size">>, Result)),
        Row = one(
            C,
            <<"SELECT scope, scope_ref, info::text AS info, status FROM attachment WHERE path = $1">>,
            [Key]
        ),
        ?assertEqual(<<"enterprise">>, maps:get(<<"scope">>, Row)),
        ?assertEqual(
            enterprise_asset_repo:scope_ref(?ORG_A, maps:get(app_a, State)),
            maps:get(<<"scope_ref">>, Row)
        ),
        Info = jsone:decode(maps:get(<<"info">>, Row)),
        ?assertEqual(maps:get(app_a, State), maps:get(<<"origin_application_id">>, Info)),
        ?assertEqual(<<"enterprise_application">>, maps:get(<<"origin_kind">>, Info)),
        %% pending 销账
        ?assertMatch(
            #{},
            one(C, <<"SELECT 1 AS x FROM attach_pending WHERE object_key = $1">>, [Key])
        )
    after
        meck:unload(elib_oss)
    end.

confirm_not_presigned(C, State) ->
    Key =
        <<"eoa/", (integer_to_binary(?ORG_A))/binary, "/",
            (integer_to_binary(maps:get(app_a, State)))/binary,
            "/20260921/file_1_ab/not-presigned.txt">>,
    meck:new(elib_oss, [passthrough, no_passthrough_cover]),
    meck:expect(
        elib_oss,
        head_object,
        fun(_B, _K) -> {ok, #{size => 1, content_type => <<"text/plain">>}} end
    ),
    try
        ?assertMatch(
            {error, {<<"invalid_request">>, not_presigned}},
            enterprise_asset_logic:confirm_tx(C, ctx_a(State), #{object_key => Key})
        )
    after
        meck:unload(elib_oss)
    end.

confirm_cross_app(C, State) ->
    Key =
        <<"eoa/", (integer_to_binary(?ORG_B))/binary, "/",
            (integer_to_binary(maps:get(app_b, State)))/binary, "/20260921/file_1_ab/other.txt">>,
    ?assertMatch(
        {error, {<<"invalid_request">>, invalid_confirm_input}},
        enterprise_asset_logic:confirm_tx(C, ctx_a(State), #{object_key => Key})
    ).

confirm_not_found(C, State) ->
    Key = presigned_key(C, State),
    meck:new(elib_oss, [passthrough, no_passthrough_cover]),
    meck:expect(elib_oss, head_object, fun(_B, _K) -> {error, not_found} end),
    try
        ?assertMatch(
            {error, {<<"invalid_request">>, object_not_found}},
            enterprise_asset_logic:confirm_tx(C, ctx_a(State), #{object_key => Key})
        )
    after
        meck:unload(elib_oss)
    end.

confirm_too_large(C, State) ->
    Key = presigned_key(C, State),
    meck:new(elib_oss, [passthrough, no_passthrough_cover]),
    TooBig = elib_oss:max_file_size() + 1,
    meck:expect(
        elib_oss,
        head_object,
        fun(_B, _K) -> {ok, #{size => TooBig, content_type => <<"text/plain">>}} end
    ),
    meck:expect(elib_oss, delete_object, fun(_B, _K) -> ok end),
    try
        ?assertMatch(
            {error, {<<"invalid_request">>, file_too_large}},
            enterprise_asset_logic:confirm_tx(C, ctx_a(State), #{object_key => Key})
        ),
        ?assertEqual(1, meck:num_calls(elib_oss, delete_object, 2))
    after
        meck:unload(elib_oss)
    end.

%%%===================================================================
%%% ③ INT-09 direct
%%%===================================================================

direct_human_success(C, State) ->
    {ok, Result} = enterprise_message_logic:direct_tx(C, ctx_a(State), direct_input_human()),
    MsgId = maps:get(<<"msg_id">>, Result),
    ?assertMatch(true, is_binary(MsgId) andalso byte_size(MsgId) > 0),
    ?assertEqual(<<"human">>, maps:get(<<"sender_kind">>, Result)),
    ?assertEqual(?H_A1, maps:get(<<"sender_user_id">>, Result)),
    ?assertEqual(<<"enterprise_application">>, maps:get(<<"origin_kind">>, Result)),
    Row = one(
        C,
        <<"SELECT from_id, to_id, e2ee, payload::text AS payload FROM msg_c2c WHERE msg_id = $1">>,
        [MsgId]
    ),
    ?assertEqual(?H_A1, maps:get(<<"from_id">>, Row)),
    ?assertEqual(?H_A2, maps:get(<<"to_id">>, Row)),
    ?assertEqual(null, maps:get(<<"e2ee">>, Row)),
    Payload = jsone:decode(maps:get(<<"payload">>, Row)),
    ?assertEqual(<<"hello from oa"/utf8>>, maps:get(<<"content">>, Payload)),
    ?assertMatch(false, maps:is_key(<<"e2ee">>, Payload)),
    Origin = maps:get(<<"origin">>, Payload),
    ?assertEqual(<<"human">>, maps:get(<<"sender_kind">>, Origin)),
    ?assertEqual(?H_A1, maps:get(<<"sender_user_id">>, Origin)),
    ?assertEqual(maps:get(app_a, State), maps:get(<<"application_id">>, Origin)).

direct_application_success(C, State) ->
    {ok, Result} = enterprise_message_logic:direct_tx(C, ctx_a(State), direct_input_app()),
    ?assertEqual(<<"application">>, maps:get(<<"sender_kind">>, Result)),
    Row = one(
        C,
        <<"SELECT from_id, e2ee FROM msg_c2c WHERE msg_id = $1">>,
        [maps:get(<<"msg_id">>, Result)]
    ),
    ?assertEqual(?PRIN_A, maps:get(<<"from_id">>, Row)),
    ?assertEqual(null, maps:get(<<"e2ee">>, Row)).

direct_cross_org(C, State) ->
    Input = (direct_input_human())#{recipient_user_id => ?EXT_B1},
    ?assertMatch(
        {error, {<<"identity_not_mapped">>, _}},
        enterprise_message_logic:direct_tx(C, ctx_a(State), Input)
    ).

direct_unmapped(C, State) ->
    Input = (direct_input_human())#{recipient_user_id => <<"ext-ghost">>},
    ?assertMatch(
        {error, {<<"identity_not_mapped">>, _}},
        enterprise_message_logic:direct_tx(C, ctx_a(State), Input)
    ).

direct_removed(C, State) ->
    %% 运行时移除：mapping 存在（绑定时 active），成员随后 removed ->
    %% 发消息实时校验拒绝（identity_not_mapped）
    exec(
        C,
        <<"UPDATE organization_member SET status = 'removed' WHERE organization_id = ",
            (integer_to_binary(?ORG_A))/binary, " AND user_id = ",
            (integer_to_binary(?REMOVED_A))/binary>>
    ),
    Input = (direct_input_human())#{recipient_user_id => ?EXT_A3},
    ?assertMatch(
        {error, {<<"identity_not_mapped">>, _}},
        enterprise_message_logic:direct_tx(C, ctx_a(State), Input)
    ).

direct_disabled(C, State) ->
    %% 运行时实时校验：mapping 存在但 user 已停用 -> identity_not_mapped
    exec(C, <<"UPDATE \"user\" SET status = 0 WHERE id = ", (integer_to_binary(?H_A2))/binary>>),
    ?assertMatch(
        {error, {<<"identity_not_mapped">>, _}},
        enterprise_message_logic:direct_tx(C, ctx_a(State), direct_input_human())
    ).

direct_alias_rejected(C, State) ->
    ?assertMatch(
        {error, {<<"invalid_request">>, {forbidden_field, _}}},
        enterprise_message_logic:direct_tx(
            C,
            ctx_a(State),
            (direct_input_human())#{as_user_id => ?EXT_A1}
        )
    ),
    ?assertMatch(
        {error, {<<"invalid_request">>, {forbidden_field, _}}},
        enterprise_message_logic:direct_tx(
            C,
            ctx_a(State),
            (direct_input_human())#{actor_user_id => ?EXT_A1}
        )
    ),
    %% group 路径同样拒绝
    ?assertMatch(
        {error, {<<"invalid_request">>, {forbidden_field, _}}},
        enterprise_message_logic:group_tx(
            C,
            ctx_a(State),
            ?GRP_A1,
            (direct_input_human())#{actor_user_id => ?EXT_A1}
        )
    ).

direct_missing_sender(C, State) ->
    Input = maps:remove(sender_user_id, direct_input_human()),
    ?assertMatch(
        {error, {<<"invalid_request">>, sender_user_id_required}},
        enterprise_message_logic:direct_tx(C, ctx_a(State), Input)
    ).

direct_scope_negative(C, State) ->
    %% human 模式但只有 messages:send -> insufficient_scope（无隐含包含）
    ?assertMatch(
        {error, {<<"insufficient_scope">>, _}},
        enterprise_message_logic:direct_tx(C, ctx_a_send_only(State), direct_input_human())
    ),
    %% application 模式但只有 messages:send_as_human -> insufficient_scope
    ?assertMatch(
        {error, {<<"insufficient_scope">>, _}},
        enterprise_message_logic:direct_tx(C, ctx_a_human_only(State), direct_input_app())
    ).

direct_no_principal(C, State) ->
    ?assertMatch(
        {error, {<<"invalid_request">>, application_principal_required}},
        enterprise_message_logic:direct_tx(C, ctx_a_nopin(State), direct_input_app())
    ).

direct_invalid_params(C, State) ->
    ?assertMatch(
        {error, {<<"invalid_request">>, invalid_sender_mode}},
        enterprise_message_logic:direct_tx(
            C,
            ctx_a(State),
            (direct_input_human())#{sender_mode => <<"robot">>}
        )
    ),
    ?assertMatch(
        {error, {<<"invalid_request">>, invalid_msg_type}},
        enterprise_message_logic:direct_tx(
            C,
            ctx_a(State),
            (direct_input_human())#{msg_type => <<"sticker">>}
        )
    ),
    ?assertMatch(
        {error, {<<"invalid_request">>, invalid_content}},
        enterprise_message_logic:direct_tx(
            C,
            ctx_a(State),
            (direct_input_human())#{content => <<>>}
        )
    ),
    ?assertMatch(
        {error, {<<"invalid_request">>, invalid_external_user_id}},
        enterprise_message_logic:direct_tx(
            C,
            ctx_a(State),
            (direct_input_human())#{recipient_user_id => 42}
        )
    ),
    ?assertMatch(
        {error, {<<"identity_not_mapped">>, _}},
        enterprise_message_logic:direct_tx(
            C,
            ctx_a(State),
            (direct_input_human())#{sender_user_id => <<"ext-ghost">>}
        )
    ),
    ?assertMatch(
        {error, {<<"invalid_request">>, input_not_map}},
        enterprise_message_logic:direct_tx(C, ctx_a(State), <<"not-a-map">>)
    ).

direct_file_unconfirmed(C, State) ->
    Key =
        <<"eoa/", (integer_to_binary(?ORG_A))/binary, "/",
            (integer_to_binary(maps:get(app_a, State)))/binary, "/20260921/file_1_ab/never.txt">>,
    Input = ((direct_input_human())#{msg_type => <<"file">>, object_key => Key}),
    ?assertMatch(
        {error, {<<"resource_not_found">>, file_not_confirmed}},
        enterprise_message_logic:direct_tx(C, ctx_a(State), Input)
    ).

direct_file_confirmed(C, State) ->
    Key = presigned_key(C, State),
    meck:new(elib_oss, [passthrough, no_passthrough_cover]),
    meck:expect(
        elib_oss,
        head_object,
        fun(_B, _K) -> {ok, #{size => 64, content_type => <<"text/plain">>}} end
    ),
    try
        {ok, _} = enterprise_asset_logic:confirm_tx(C, ctx_a(State), #{object_key => Key}),
        Input = (direct_input_human())#{msg_type => <<"file">>, object_key => Key},
        {ok, Result} = enterprise_message_logic:direct_tx(C, ctx_a(State), Input),
        Row = one(
            C,
            <<"SELECT msg_type, payload::text AS payload FROM msg_c2c WHERE msg_id = $1">>,
            [maps:get(<<"msg_id">>, Result)]
        ),
        ?assertEqual(<<"file">>, maps:get(<<"msg_type">>, Row)),
        Payload = jsone:decode(maps:get(<<"payload">>, Row)),
        File = maps:get(<<"file">>, Payload),
        ?assertEqual(Key, maps:get(<<"object_key">>, File)),
        ?assertEqual(64, maps:get(<<"size">>, File))
    after
        meck:unload(elib_oss)
    end.

%%%===================================================================
%%% ④ INT-10 group
%%%===================================================================

group_success(C, State) ->
    {ok, Result} = enterprise_message_logic:group_tx(
        C,
        ctx_a(State),
        ?GRP_A1,
        direct_input_human()
    ),
    Row = one(
        C,
        <<"SELECT from_id, to_id, e2ee, payload::text AS payload FROM msg_c2g WHERE msg_id = $1">>,
        [maps:get(<<"msg_id">>, Result)]
    ),
    ?assertEqual(?H_A1, maps:get(<<"from_id">>, Row)),
    ?assertEqual(?GRP_A1, maps:get(<<"to_id">>, Row)),
    ?assertEqual(null, maps:get(<<"e2ee">>, Row)),
    Payload = jsone:decode(maps:get(<<"payload">>, Row)),
    ?assertMatch(true, is_map(maps:get(<<"origin">>, Payload))),
    ?assertMatch(false, maps:is_key(<<"e2ee">>, Payload)).

group_cross_org(C, State) ->
    ?assertMatch(
        {error, {<<"resource_not_found">>, group_not_found}},
        enterprise_message_logic:group_tx(C, ctx_a(State), ?GRP_B1, direct_input_human())
    ).

group_personal(C, State) ->
    ?assertMatch(
        {error, {<<"resource_not_found">>, group_not_found}},
        enterprise_message_logic:group_tx(C, ctx_a(State), ?GRP_PERSONAL, direct_input_human())
    ).

group_archived_ws(C, State) ->
    %% 群在 archived Workspace -> resource_not_found（不泄露存在性，
    %% workspace_not_active 与 group_not_found 同属 404 族）
    ?assertMatch(
        {error, {<<"resource_not_found">>, _}},
        enterprise_message_logic:group_tx(C, ctx_a(State), ?GRP_ARCH, direct_input_human())
    ).

group_sender_not_member(C, State) ->
    %% H_A2 在 WS_A1 但不在 GRP_A1
    Input = (direct_input_human())#{sender_user_id => ?EXT_A2, recipient_user_id => ?EXT_A1},
    ?assertMatch(
        {error, {<<"organization_boundary_violation">>, sender_not_group_member}},
        enterprise_message_logic:group_tx(C, ctx_a(State), ?GRP_A1, Input)
    ).

group_sender_not_ws(C, State) ->
    %% H_A3 是 org 成员+已映射但不在 WS_A1
    Input = (direct_input_human())#{sender_user_id => ?EXT_A3, recipient_user_id => ?EXT_A1},
    ?assertMatch(
        {error, {<<"organization_boundary_violation">>, _}},
        enterprise_message_logic:group_tx(C, ctx_a(State), ?GRP_A1, Input)
    ).

%%%===================================================================
%%% ⑤ 幂等三态
%%%===================================================================

idem_replay(C, State) ->
    Ctx = ctx_a(State),
    Digest = hash_of(<<"digest-a">>),
    Key = <<"idem-key-1">>,
    ?assertMatch(
        {ok, inserted},
        enterprise_internal_idempotency:begin_tx(C, Ctx, <<"enterprise_message">>, Key, Digest)
    ),
    {ok, Result} = enterprise_message_logic:direct_tx(C, Ctx, direct_input_human()),
    RowId = msg_row_id(C, maps:get(<<"msg_id">>, Result)),
    %% V2.1 §11 v2：complete_tx/7 需带 response 快照（status + JSON body）
    Body = jsone:encode(Result),
    ok = enterprise_internal_idempotency:complete_tx(
        C, Ctx, <<"enterprise_message">>, Key, RowId, 200, Body
    ),
    ?assertMatch(
        {ok, replay, #{resource_id := RowId, response_code := 200, response_body := Body}},
        enterprise_internal_idempotency:begin_tx(C, Ctx, <<"enterprise_message">>, Key, Digest)
    ).

idem_conflict(C, State) ->
    Ctx = ctx_a(State),
    ?assertMatch(
        {ok, inserted},
        enterprise_internal_idempotency:begin_tx(
            C, Ctx, <<"enterprise_message">>, <<"k2">>, hash_of(<<"d1">>)
        )
    ),
    ?assertMatch(
        {error, digest_conflict},
        enterprise_internal_idempotency:begin_tx(
            C, Ctx, <<"enterprise_message">>, <<"k2">>, hash_of(<<"d2">>)
        )
    ).

idem_pending(C, State) ->
    Ctx = ctx_a(State),
    %% begin 后不 complete（模拟在途/中断）-> pending
    ?assertMatch(
        {ok, inserted},
        enterprise_internal_idempotency:begin_tx(
            C, Ctx, <<"enterprise_message">>, <<"k3">>, hash_of(<<"d1">>)
        )
    ),
    ?assertMatch(
        {ok, pending},
        enterprise_internal_idempotency:begin_tx(
            C, Ctx, <<"enterprise_message">>, <<"k3">>, hash_of(<<"d1">>)
        )
    ).

msg_row_id(C, MsgId) ->
    case enterprise_message_repo:find_direct_tx(C, MsgId) of
        {ok, #{<<"id">> := Id}} -> Id;
        _ -> null
    end.

%%%===================================================================
%%% ⑥ 非 E2EE 断言 + Human E2EE 回归
%%%===================================================================

non_e2ee(C, State) ->
    meck:new(msg_store_ds, [passthrough, no_passthrough_cover]),
    meck:expect(msg_store_ds, stage, fun(_, _, _, _, _, _, _, _, _, _, _) ->
        erlang:error({unexpected_stage_call, e2ee_path_touched})
    end),
    try
        %% direct + group 双路径零触 E2EE staging
        {ok, R1} = enterprise_message_logic:direct_tx(C, ctx_a(State), direct_input_human()),
        {ok, R2} = enterprise_message_logic:group_tx(
            C,
            ctx_a(State),
            ?GRP_A1,
            direct_input_human()
        ),
        ?assertMatch(true, is_binary(maps:get(<<"msg_id">>, R1))),
        ?assertMatch(true, is_binary(maps:get(<<"msg_id">>, R2))),
        %% E2EE 离线真源零行（msg_store_staging 若由运行时建表则必须无行）
        Staging = one(C, <<"SELECT to_regclass('public.msg_store_staging') AS t">>, []),
        case maps:get(<<"t">>, Staging, null) of
            null ->
                ok;
            _ ->
                N = one(C, <<"SELECT count(*) AS n FROM msg_store_staging">>, []),
                ?assertEqual(0, maps:get(<<"n">>, N))
        end,
        lists:foreach(
            fun(Mid) ->
                Row = one(C, <<"SELECT e2ee FROM msg_c2c WHERE msg_id = $1">>, [Mid]),
                ?assertEqual(null, maps:get(<<"e2ee">>, Row))
            end,
            [maps:get(<<"msg_id">>, R1)]
        ),
        Row2 = one(
            C,
            <<"SELECT e2ee FROM msg_c2g WHERE msg_id = $1">>,
            [maps:get(<<"msg_id">>, R2)]
        ),
        ?assertEqual(null, maps:get(<<"e2ee">>, Row2)),
        ?assertEqual(0, meck:num_calls(msg_store_ds, stage, 11))
    after
        meck:unload(msg_store_ds)
    end.

%% Human E2EE 回归：required 模式明文 C2C/C2G 仍拒（policy_violation）。
%% 与 msg_c2c_logic_tests / msg_c2g_logic_tests 的同名既有用例同判定源
%% （imboy_policy:validate_message_write/5 是 Human 链的真实门）——EPGZ-04
%% 零改动 msg_c2c_logic / msg_c2g_logic / imboy_policy（diff 证明），此处
%% 重钉一次证明门行为未被放宽。
human_e2ee_regression_test_() ->
    ?WITH_MECKS(
        [
            {friend_ds, [
                {'check_relationship', 2, fun(456, 123) -> {true, false} end}
            ]},
            {ai_agent_ds, [
                {'is_agent', 1, fun(456) -> false end}
            ]},
            {bot_ds, [
                {'is_bot', 1, fun(_) -> false end}
            ]},
            {group_member_logic, [
                {'check_mute', 2, fun(100, 1001) -> false end}
            ]},
            {group_ds, [
                {'e2ee_mode', 1, fun(_) -> {ok, 0} end},
                {'is_member', 2, fun(1001, 100) -> true end},
                {'member_uids', 1, fun(100) -> [1001, 1002, 1003] end},
                {'member_uids_strict', 1, fun(100) -> {ok, [1001, 1002, 1003]} end}
            ]},
            {imboy_policy, [
                {'validate_message_write', 5, fun(_, _, _, _, _) ->
                    {error, <<"encrypted_message_required">>}
                end}
            ]},
            {msg_store_ds, [
                {'stage', 11, fun(_, _, _, _, _, _, _, _, _, _, _) -> {ok, new} end},
                {'stage', 12, fun(_, _, _, _, _, _, FromId, _, _, _, _, 1) ->
                    {ok, new, 11, [FromId]}
                end},
                {'enqueue', 3, fun(_, _, _) -> ok end}
            ]}
        ],
        fun() ->
            DataC2C = #{
                <<"to">> => <<"456">>,
                <<"payload">> => #{<<"content">> => <<"hello">>},
                <<"created_at">> => 1708768700000,
                <<"msg_type">> => <<"text">>,
                <<"action">> => <<>>,
                <<"e2ee">> => null
            },
            {reply, Reply} = msg_c2c_logic:c2c(<<"epgz04-e2ee-reg-1">>, 123, DataC2C),
            ?assertEqual(<<"policy_violation">>, maps:get(<<"action">>, Reply)),
            DataC2G = #{
                <<"to">> => <<"100">>,
                <<"payload">> => #{<<"content">> => <<"hello group">>, <<"mentions">> => []},
                <<"created_at">> => 1708768700000,
                <<"msg_type">> => <<"text">>,
                <<"action">> => <<>>,
                <<"e2ee">> => null
            },
            {reply, ReplyG} = msg_c2g_logic:c2g(<<"epgz04-e2ee-reg-2">>, 1001, DataC2G),
            ?assertEqual(<<"policy_violation">>, maps:get(<<"action">>, ReplyG)),
            ?assertEqual(0, meck:num_calls(msg_store_ds, stage, 11)),
            ?assertEqual(0, meck:num_calls(msg_store_ds, stage, 12))
        end
    ).

%%%===================================================================
%%% ⑦ origin_application_id 持久化
%%%===================================================================

origin_persisted(C, State) ->
    {ok, Result} = enterprise_message_logic:direct_tx(C, ctx_a(State), direct_input_human()),
    RowId = msg_row_id(C, maps:get(<<"msg_id">>, Result)),
    {ok, Audits} = enterprise_message_repo:find_audit_tx(C, ?ORG_A, RowId),
    ?assertMatch(true, length(Audits) >= 1),
    Audit = lists:last(Audits),
    ?assertEqual(<<"message.enterprise.accepted">>, maps:get(<<"action">>, Audit)),
    ?assertEqual(<<"enterprise_application">>, maps:get(<<"actor_role">>, Audit)),
    Detail = jsone:decode(maps:get(<<"detail">>, Audit)),
    ?assertEqual(maps:get(app_a, State), maps:get(<<"origin_application_id">>, Detail)),
    ?assertEqual(<<"enterprise_application">>, maps:get(<<"origin_kind">>, Detail)),
    ?assertEqual(?H_A1, maps:get(<<"sender_user_id">>, Detail)),
    ?assertEqual(<<"human">>, maps:get(<<"sender_kind">>, Detail)).

%%%===================================================================
%%% ⑧ INT-12 webhook 配置
%%%===================================================================

wh_configure(C, State) ->
    with_public_dns(fun() ->
        {ok, Result} = enterprise_webhook_logic:configure_tx(C, ctx_a(State), #{
            url => <<"https://oa.example.com/hook">>,
            events => [<<"message.enterprise.accepted">>, <<"file.confirmed">>]
        }),
        ?assertEqual(<<"https://oa.example.com/hook">>, maps:get(<<"url">>, Result)),
        Secret = maps:get(<<"secret">>, Result),
        ?assertMatch(true, is_binary(Secret) andalso byte_size(Secret) > 40),
        Bot = one(
            C,
            <<"SELECT username, webhook_url, status, events::text AS events FROM bot WHERE user_id = $1">>,
            [?PRIN_A]
        ),
        ?assertEqual(<<"eapp_epgz04-oa-a">>, maps:get(<<"username">>, Bot)),
        ?assertEqual(<<"https://oa.example.com/hook">>, maps:get(<<"webhook_url">>, Bot)),
        ?assertEqual(1, maps:get(<<"status">>, Bot)),
        Events = jsone:decode(maps:get(<<"events">>, Bot)),
        ?assertEqual(true, lists:member(<<"file.confirmed">>, Events))
    end).

wh_https_required(C, State) ->
    %% 6c08eca6 起 scheme 校验移交 SSRF guard：HTTP 非 loopback 由 guard 以
    %% invalid_scheme 拒（与 wh_ssrf_test_ 的 forbidden_host 同族错误结构）。
    ?assertMatch(
        {error, {<<"invalid_request">>, {ssrf_or_invalid_url, invalid_scheme}}},
        enterprise_webhook_logic:configure_tx(C, ctx_a(State), #{
            url => <<"http://oa.example.com/hook">>, events => [<<"file.confirmed">>]
        })
    ).

%% SSRF：私网地址拒（guard DNS 解析 meck；不触真网；guard 在触库前拒绝）。
wh_ssrf_test_() ->
    ?_test(begin
        ok = meck:new(inet, [unstick, passthrough, no_passthrough_cover]),
        meck:expect(inet, getaddrs, fun("internal.corp", inet) -> {ok, [{10, 0, 0, 5}]} end),
        try
            ?assertMatch(
                {error, {<<"invalid_request">>, {ssrf_or_invalid_url, forbidden_host}}},
                enterprise_webhook_logic:configure_tx(undefined, #{}, #{
                    url => <<"https://internal.corp/hook">>, events => [<<"file.confirmed">>]
                })
            )
        after
            meck:unload(inet)
        end
    end).

wh_events_whitelist(C, State) ->
    with_public_dns(fun() ->
        ?assertMatch(
            {error, {<<"invalid_request">>, invalid_webhook_config}},
            enterprise_webhook_logic:configure_tx(C, ctx_a(State), #{
                url => <<"https://oa.example.com/hook">>,
                events => [<<"message.personal.received">>]
            })
        ),
        ?assertMatch(
            {ok, _},
            enterprise_webhook_logic:configure_tx(C, ctx_a(State), #{
                url => <<"https://oa.example.com/hook">>,
                events => [<<"group.member.changed">>]
            })
        )
    end).

wh_no_principal(C, State) ->
    with_public_dns(fun() ->
        ?assertMatch(
            {error, {<<"invalid_request">>, application_principal_required}},
            enterprise_webhook_logic:configure_tx(C, ctx_a_nopin(State), #{
                url => <<"https://oa.example.com/hook">>, events => [<<"file.confirmed">>]
            })
        )
    end).

wh_rotate(C, State) ->
    with_public_dns(fun() ->
        {ok, R1} = enterprise_webhook_logic:configure_tx(C, ctx_a(State), #{
            url => <<"https://oa.example.com/hook">>, events => [<<"file.confirmed">>]
        }),
        {ok, R2} = enterprise_webhook_logic:configure_tx(C, ctx_a(State), #{
            url => <<"https://oa.example.com/hook">>,
            events => [<<"file.confirmed">>],
            rotate => true
        }),
        S1 = maps:get(<<"secret">>, R1),
        S2 = maps:get(<<"secret">>, R2),
        ?assertMatch(true, is_binary(S1) andalso is_binary(S2) andalso S1 =/= S2)
    end).

wh_disable(C, State) ->
    with_public_dns(fun() ->
        {ok, _} = enterprise_webhook_logic:configure_tx(C, ctx_a(State), #{
            url => <<"https://oa.example.com/hook">>, events => [<<"file.confirmed">>]
        }),
        {ok, _} = enterprise_webhook_logic:configure_tx(C, ctx_a(State), #{
            url => <<"https://oa.example.com/hook">>,
            events => [<<"file.confirmed">>],
            status => disabled
        }),
        Bot = one(C, <<"SELECT status FROM bot WHERE user_id = $1">>, [?PRIN_A]),
        ?assertEqual(0, maps:get(<<"status">>, Bot)),
        %% 停用后事件入箱跳过
        ?assertMatch(
            {ok, skipped},
            enterprise_webhook_logic:emit_event_tx(
                C,
                ctx_a(State),
                <<"file.confirmed">>,
                #{resource_type => <<"attachment">>, resource_id => 1}
            )
        )
    end).

%%%===================================================================
%%% ⑨ 事件入箱
%%%===================================================================

emit_envelope(C, State) ->
    with_public_dns(fun() ->
        {ok, _} = enterprise_webhook_logic:configure_tx(C, ctx_a(State), #{
            url => <<"https://oa.example.com/hook">>,
            events => [<<"message.enterprise.accepted">>]
        }),
        {ok, Result} = enterprise_message_logic:direct_tx(C, ctx_a(State), direct_input_human()),
        RowId = msg_row_id(C, maps:get(<<"msg_id">>, Result)),
        Delivery = one(
            C,
            <<
                "SELECT bot_id, event_type, payload::text AS payload, idempotency_key,"
                " webhook_url, pinned_ip FROM bot_delivery WHERE idempotency_key LIKE 'evt-%'"
                " ORDER BY created_at DESC LIMIT 1"
            >>,
            []
        ),
        ?assertEqual(<<"message.enterprise.accepted">>, maps:get(<<"event_type">>, Delivery)),
        ?assertEqual(
            <<"eapp:", (integer_to_binary(?PRIN_A))/binary>>,
            maps:get(<<"bot_id">>, Delivery)
        ),
        Env = jsone:decode(maps:get(<<"payload">>, Delivery)),
        lists:foreach(
            fun(K) ->
                ?assertMatch(true, maps:is_key(K, Env), {missing_key, K})
            end,
            [
                <<"event_id">>,
                <<"delivery_id">>,
                <<"event_type">>,
                <<"version">>,
                <<"occurred_at">>,
                <<"organization_id">>,
                <<"application_id">>,
                <<"resource">>
            ]
        ),
        ?assertMatch(false, maps:is_key(<<"secret">>, Env)),
        ?assertMatch(false, maps:is_key(<<"body">>, Env)),
        ?assertMatch(false, maps:is_key(<<"put_url">>, Env)),
        ?assertEqual(?ORG_A, maps:get(<<"organization_id">>, Env)),
        ?assertEqual(maps:get(app_a, State), maps:get(<<"application_id">>, Env)),
        ?assertEqual(<<"msg_c2c">>, maps:get(<<"type">>, maps:get(<<"resource">>, Env))),
        ?assertEqual(RowId, maps:get(<<"id">>, maps:get(<<"resource">>, Env)))
    end).

emit_unsubscribed(C, State) ->
    with_public_dns(fun() ->
        {ok, _} = enterprise_webhook_logic:configure_tx(C, ctx_a(State), #{
            url => <<"https://oa.example.com/hook">>, events => [<<"file.confirmed">>]
        }),
        ?assertMatch(
            {ok, skipped},
            enterprise_webhook_logic:emit_event_tx(
                C,
                ctx_a(State),
                <<"message.enterprise.accepted">>,
                #{resource_type => <<"msg_c2c">>, resource_id => 99}
            )
        ),
        ?assertMatch(
            {ok, skipped},
            enterprise_webhook_logic:emit_event_tx(
                C,
                ctx_a(State),
                <<"message.personal.x">>,
                #{resource_type => <<"msg_c2c">>, resource_id => 99}
            )
        )
    end).

emit_file_confirmed(C, State) ->
    with_public_dns(fun() ->
        {ok, _} = enterprise_webhook_logic:configure_tx(C, ctx_a(State), #{
            url => <<"https://oa.example.com/hook">>, events => [<<"file.confirmed">>]
        }),
        Key = presigned_key(C, State),
        meck:new(elib_oss, [passthrough, no_passthrough_cover]),
        meck:expect(
            elib_oss,
            head_object,
            fun(_B, _K) -> {ok, #{size => 32, content_type => <<"text/plain">>}} end
        ),
        try
            {ok, Confirm} = enterprise_asset_logic:confirm_tx(C, ctx_a(State), #{object_key => Key}),
            Delivery = one(
                C,
                <<"SELECT event_type FROM bot_delivery WHERE payload::text LIKE $1 LIMIT 1">>,
                [<<"%", (integer_to_binary(maps:get(<<"file_id">>, Confirm)))/binary, "%">>]
            ),
            ?assertEqual(<<"file.confirmed">>, maps:get(<<"event_type">>, Delivery))
        after
            meck:unload(elib_oss)
        end
    end).

%%%===================================================================
%%% ⑩ INT-13 replay
%%%===================================================================

seed_dead_delivery(C, State) ->
    with_public_dns(fun() ->
        {ok, _} = enterprise_webhook_logic:configure_tx(C, ctx_a(State), #{
            url => <<"https://oa.example.com/hook">>,
            events => [<<"message.enterprise.accepted">>]
        }),
        {ok, Result} = enterprise_message_logic:direct_tx(C, ctx_a(State), direct_input_human()),
        _ = msg_row_id(C, maps:get(<<"msg_id">>, Result)),
        Did = one(
            C,
            <<
                "SELECT delivery_id FROM bot_delivery WHERE idempotency_key LIKE 'evt-%'"
                " ORDER BY created_at DESC LIMIT 1"
            >>,
            []
        ),
        maps:get(<<"delivery_id">>, Did)
    end).

replay_ok(C, State) ->
    with_public_dns(fun() ->
        OldId = seed_dead_delivery(C, State),
        exec(
            C,
            <<"UPDATE bot_delivery SET status = 'dead' WHERE delivery_id = '", OldId/binary, "'">>
        ),
        {ok, Replay} = enterprise_webhook_logic:replay_tx(C, ctx_a(State), OldId),
        %% 冻结合同（2ff626f8 / INT-BE-02 conformance）：响应体恰为
        %% {"replayed": true}——新投递行细节是服务端内部状态，不外泄；
        %% 新行 ID 从 DB 以 replay_of 指针反查后继续做代际断言。
        ?assertMatch(#{<<"replayed">> := true}, Replay),
        NewId = maps:get(
            <<"delivery_id">>,
            one(
                C,
                <<"SELECT delivery_id FROM bot_delivery WHERE ewh_replay_of = $1">>,
                [OldId]
            )
        ),
        ?assertMatch(true, is_binary(NewId) andalso NewId =/= OldId),
        Old = one(
            C,
            <<"SELECT payload::text AS payload FROM bot_delivery WHERE delivery_id = $1">>,
            [OldId]
        ),
        New = one(
            C,
            <<"SELECT payload::text AS payload, status FROM bot_delivery WHERE delivery_id = $1">>,
            [NewId]
        ),
        OldEnv = jsone:decode(maps:get(<<"payload">>, Old)),
        NewEnv = jsone:decode(maps:get(<<"payload">>, New)),
        ?assertEqual(maps:get(<<"event_id">>, OldEnv), maps:get(<<"event_id">>, NewEnv)),
        ?assertEqual(NewId, maps:get(<<"delivery_id">>, NewEnv)),
        ?assertEqual(<<"pending">>, maps:get(<<"status">>, New))
    end).

replay_cross_org(C, State) ->
    with_public_dns(fun() ->
        OldId = seed_dead_delivery(C, State),
        exec(
            C,
            <<"UPDATE bot_delivery SET status = 'dead' WHERE delivery_id = '", OldId/binary, "'">>
        ),
        ?assertMatch(
            {error, {<<"resource_not_found">>, delivery_not_found}},
            enterprise_webhook_logic:replay_tx(C, ctx_b(State), OldId)
        ),
        %% 个人 bot 的纯数字 bot_id 行同理（非 eapp: 命名空间）
        exec(C, <<
            "INSERT INTO bot_delivery (delivery_id, bot_id, event_type, payload,"
            " correlation_id, idempotency_key, webhook_url, webhook_host, pinned_ip)"
            " VALUES ('bot-d-1', '123456', 'message', '{}', 'corr1234567890abcdef',"
            " 'bot-idem-1', 'https://x.example/', 'x.example', '1.2.3.4')"
        >>),
        ?assertMatch(
            {error, {<<"resource_not_found">>, delivery_not_found}},
            enterprise_webhook_logic:replay_tx(C, ctx_a(State), <<"bot-d-1">>)
        )
    end).

replay_in_flight(C, State) ->
    with_public_dns(fun() ->
        OldId = seed_dead_delivery(C, State),
        ?assertMatch(
            {error, {<<"invalid_request">>, delivery_in_flight}},
            enterprise_webhook_logic:replay_tx(C, ctx_a(State), OldId)
        )
    end).

%%%===================================================================
%%% ⑪ 投递执行（池化路径全 meck；DB 断言在 ROLLBACK 事务外不可行，验证调用）
%%%===================================================================

%% 从 marker 库直连查询构造 bot_webhook_delivery_worker claim_due 同构行
%% （payload 保持 binary——与 claim_due 的 payload::text 同构）。
find_delivery_row(C, DeliveryId) ->
    Row = one(
        C,
        <<
            "SELECT delivery_id, bot_id, event_type, payload::text AS payload,"
            " attempt_count, webhook_url, webhook_host, pinned_ip"
            " FROM bot_delivery WHERE delivery_id = $1"
        >>,
        [DeliveryId]
    ),
    Row#{
        <<"payload">> => maps:get(<<"payload">>, Row, <<"{}">>),
        <<"attempt_count">> => maps:get(<<"attempt_count">>, Row, 0)
    }.

exec_2xx(C, State) ->
    with_public_dns(fun() ->
        OldId = seed_dead_delivery(C, State),
        Delivery = find_delivery_row(C, OldId),
        Captured = [],
        with_delivery_stubs(Captured, {ok, 200}, fun() ->
            ok = enterprise_webhook_logic:execute_delivery(Delivery),
            ?assertEqual(1, meck:num_calls(bot_webhook_delivery_repo, mark_success, 2)),
            ?assertEqual(1, meck:num_calls(bot_webhook_delivery_repo, insert_attempt, 2))
        end)
    end).

exec_4xx(C, State) ->
    with_public_dns(fun() ->
        OldId = seed_dead_delivery(C, State),
        Delivery = find_delivery_row(C, OldId),
        with_delivery_stubs([], {ok, 404}, fun() ->
            ok = enterprise_webhook_logic:execute_delivery(Delivery),
            ?assertEqual(1, meck:num_calls(bot_webhook_delivery_repo, mark_dead, 2))
        end)
    end).

exec_5xx(C, State) ->
    with_public_dns(fun() ->
        OldId = seed_dead_delivery(C, State),
        Delivery = find_delivery_row(C, OldId),
        %% sender 桩捕获签名头；断言在 with_secret_stub 内部完成
        %% （meck 卸载后 num_calls 会 {not_mocked}）。
        put(epgz04_exec5_headers, undefined),
        with_sender_stub(
            fun(Headers) ->
                put(epgz04_exec5_headers, Headers),
                {ok, 503}
            end,
            fun() ->
                with_secret_stub(fun() ->
                    ok = enterprise_webhook_logic:execute_delivery(Delivery),
                    ?assertEqual(1, meck:num_calls(bot_webhook_delivery_repo, mark_retry, 4)),
                    Headers = get(epgz04_exec5_headers),
                    ?assertMatch(true, is_list(Headers)),
                    Ts = proplists:get_value(<<"x-imboy-timestamp">>, Headers),
                    Sig = proplists:get_value(<<"x-imboy-signature">>, Headers),
                    ?assertMatch(true, is_binary(Ts) andalso is_binary(Sig)),
                    Body = maps:get(<<"payload">>, Delivery),
                    Secret =
                        case get(epgz04_secret) of
                            S when is_binary(S) -> S;
                            _ -> <<"stub-secret">>
                        end,
                    Expected = enterprise_webhook_logic:sign(
                        Secret, enterprise_webhook_logic:signature_base(Ts, Body)
                    ),
                    ?assertEqual(Expected, Sig)
                end)
            end
        )
    end).

%% 投递执行所需的池化 meck 集（secret 解密 + 状态落账 + sender）。
with_delivery_stubs(_Captured, SenderReply, Fun) ->
    ok = meck:new(enterprise_webhook_repo, [passthrough, no_passthrough_cover]),
    Secret =
        case get(epgz04_secret) of
            S when is_binary(S) -> S;
            _ -> <<"stub-secret">>
        end,
    meck:expect(enterprise_webhook_repo, get_secret, fun(_Principal) -> {ok, Secret} end),
    ok = meck:new(bot_webhook_delivery_repo, [passthrough, no_passthrough_cover]),
    meck:expect(bot_webhook_delivery_repo, mark_success, fun(_D, _N) -> {ok, 1} end),
    meck:expect(bot_webhook_delivery_repo, mark_retry, fun(_D, _S, _N, _T) -> {ok, 1} end),
    meck:expect(bot_webhook_delivery_repo, mark_dead, fun(_D, _N) -> {ok, 1} end),
    meck:expect(bot_webhook_delivery_repo, insert_attempt, fun(_D, _A) -> ok end),
    ok = meck:new(bot_webhook_delivery_sender, [passthrough, no_passthrough_cover]),
    meck:expect(
        bot_webhook_delivery_sender,
        post,
        fun(_IP, _Port, _Tls, _Path, _Host, _Headers, _Body) -> SenderReply end
    ),
    try
        Fun()
    after
        meck:unload(enterprise_webhook_repo),
        meck:unload(bot_webhook_delivery_repo),
        meck:unload(bot_webhook_delivery_sender)
    end.

with_secret_stub(Fun) ->
    ok = meck:new(enterprise_webhook_repo, [passthrough, no_passthrough_cover]),
    Secret =
        case get(epgz04_secret) of
            S when is_binary(S) -> S;
            _ -> <<"stub-secret">>
        end,
    meck:expect(enterprise_webhook_repo, get_secret, fun(_P) -> {ok, Secret} end),
    ok = meck:new(bot_webhook_delivery_repo, [passthrough, no_passthrough_cover]),
    meck:expect(bot_webhook_delivery_repo, mark_success, fun(_D, _N) -> {ok, 1} end),
    meck:expect(bot_webhook_delivery_repo, mark_retry, fun(_D, _S, _N, _T) -> {ok, 1} end),
    meck:expect(bot_webhook_delivery_repo, mark_dead, fun(_D, _N) -> {ok, 1} end),
    meck:expect(bot_webhook_delivery_repo, insert_attempt, fun(_D, _A) -> ok end),
    try
        Fun()
    after
        meck:unload(enterprise_webhook_repo),
        meck:unload(bot_webhook_delivery_repo)
    end.

%% sender 桩（单独提供，可与 secret 桩组合）。
with_sender_stub(ReplyFun, Fun) ->
    ok = meck:new(bot_webhook_delivery_sender, [passthrough, no_passthrough_cover]),
    meck:expect(
        bot_webhook_delivery_sender,
        post,
        fun(_IP, _Port, _Tls, _Path, _Host, Headers, _Body) -> ReplyFun(Headers) end
    ),
    try
        Fun()
    after
        meck:unload(bot_webhook_delivery_sender)
    end.

%% worker 分派 hook：eapp: 前缀行委托 enterprise_webhook_logic，纯数字行不委托。
worker_hook_test_() ->
    ?_test(begin
        ok = meck:new(enterprise_webhook_logic, [passthrough, no_passthrough_cover]),
        meck:expect(enterprise_webhook_logic, execute_delivery, fun(_D) -> ok end),
        try
            EnterpriseRow = #{<<"delivery_id">> => <<"ewd-1">>, <<"bot_id">> => <<"eapp:991014">>},
            bot_webhook_delivery_worker:execute(EnterpriseRow),
            ?assertEqual(1, meck:num_calls(enterprise_webhook_logic, execute_delivery, 1))
        after
            meck:unload(enterprise_webhook_logic)
        end
    end).

worker_bot_path_test_() ->
    ?_test(begin
        ok = meck:new(enterprise_webhook_logic, [passthrough, no_passthrough_cover]),
        meck:expect(enterprise_webhook_logic, execute_delivery, fun(_D) -> ok end),
        ok = meck:new(bot_repo, [passthrough, no_passthrough_cover]),
        meck:expect(bot_repo, get_verify_token, fun(_Id) -> {error, no_key} end),
        ok = meck:new(bot_webhook_delivery_repo, [passthrough, no_passthrough_cover]),
        meck:expect(bot_webhook_delivery_repo, mark_retry, fun(_D, _S, _N, _T) -> {ok, 1} end),
        meck:expect(bot_webhook_delivery_repo, mark_dead, fun(_D, _N) -> {ok, 1} end),
        meck:expect(bot_webhook_delivery_repo, insert_attempt, fun(_D, _A) -> ok end),
        ok = meck:new(bot_webhook_guard, [passthrough, no_passthrough_cover]),
        Pin = #{host => <<"x">>, port => 443, ip => {1, 2, 3, 4}, path => <<"/">>, tls => true},
        meck:expect(bot_webhook_guard, validate_pinned, fun(_U, _P) -> {ok, Pin} end),
        try
            BotRow = #{
                <<"delivery_id">> => <<"bd-1">>,
                <<"bot_id">> => <<"123456">>,
                <<"webhook_url">> => <<"https://x/">>,
                <<"pinned_ip">> => <<"1.2.3.4">>
            },
            bot_webhook_delivery_worker:execute(BotRow),
            ?assertEqual(0, meck:num_calls(enterprise_webhook_logic, execute_delivery, 1)),
            %% 原路径继续工作（credential_error -> retry）
            ?assertMatch(true, meck:num_calls(bot_repo, get_verify_token, 1) >= 1)
        after
            meck:unload(enterprise_webhook_logic),
            meck:unload(bot_repo),
            meck:unload(bot_webhook_delivery_repo),
            meck:unload(bot_webhook_guard)
        end
    end).

%%%===================================================================
%%% ⑫ 导出面
%%%===================================================================

exported_surface_test_() ->
    ?_test(begin
        MsgExports = enterprise_message_logic:module_info(exports),
        ?assertEqual(true, lists:member({direct_tx, 3}, MsgExports)),
        ?assertEqual(true, lists:member({group_tx, 4}, MsgExports)),
        lists:foreach(
            fun({F, _A}) ->
                ?assertMatch(false, is_list_all(F), {forbidden_export, F})
            end,
            MsgExports
        ),
        WhExports = enterprise_webhook_logic:module_info(exports),
        lists:foreach(
            fun({F, _A}) ->
                ?assertMatch(false, is_list_all(F), {forbidden_export, F})
            end,
            WhExports
        )
    end).

is_list_all(F) ->
    Name = atom_to_list(F),
    lists:prefix("list_", Name) orelse lists:suffix("_all", Name) orelse
        lists:prefix("export_", Name).

%%%===================================================================
%%% Helpers
%%%===================================================================

hash_of(Bin) ->
    binary:encode_hex(crypto:hash(sha256, Bin), lowercase).

%% SSRF/DNS：meck inet 解析为公网 IP（guard 放行；无真网调用）。
%% 嵌套安全：已 mock 时（外层包装过的 helper 复用）直接执行，不重复 new。
with_public_dns(Fun) ->
    try
        ok = meck:new(inet, [unstick, passthrough, no_passthrough_cover]),
        meck:expect(inet, getaddrs, fun("oa.example.com", inet) -> {ok, [?PUBLIC_IP]} end),
        try
            Fun()
        after
            meck:unload(inet)
        end
    catch
        %% 嵌套复用：外层已 mock（seed_dead_delivery 等被包装 helper）——
        %% 内层直接执行，由外层负责 unload。
        _:{already_started, _} ->
            Fun()
    end.

%%%===================================================================
%%% repair-f2-high：handler 级真实分派回归（F2 B4 HIGH bug）
%%%===================================================================
%%% F2 发现：enterprise_message_handler:with_principal/2（INT-09/10）与
%%% enterprise_webhook_handler:delivery_ctx/2、with_principal/2（INT-12/23）
%%% 以 arity-2 调 enterprise_application_repo:find_tx/3（repo 只导出 /3）
%%% → 运行时 undef → 端点真实 500。既有门禁全是 logic 层直调，覆盖不到
%%% handler 壳——这里补壳面用例：marker 连接 shim elib_pg 池化入口
%%% （push 套件同款）、meck cowboy_req 传输层（adm handler 套件同款），
%%% 直调 handler:init/2 走真实壳代码（幂等 begin/complete、boundary、
%%% principal 预取、logic、回包形状），断言非 5xx 且形状正确。

repair_seed_grant(C, State) ->
    {ok, _} =
        enterprise_internal_ops:issue_grant_tx(C, ?ORG_A, maps:get(app_a, State), #{
            scopes => [<<"messages:send">>, <<"webhooks:manage">>],
            idempotency_key => <<"repair-f2-high-grant">>,
            expires_at => <<"2099-01-01T00:00:00Z">>
        }),
    ok.

%% 与认证链同形状的 ctx（context_tx 真链路求值；full api 套件 ctx_managed 同款）。
repair_ctx(C, State) ->
    AppA = maps:get(app_a, State),
    {ok, #{grant_governed := Governed, effective_scopes := Effective}} =
        enterprise_application_grant_logic:context_tx(C, ?ORG_A, AppA, ?SCOPES_FULL),
    #{
        organization_id => ?ORG_A,
        application_id => AppA,
        granted_scopes => Effective,
        grant_governed => Governed,
        principal_user_id => ?PRIN_A
    }.

%% handler 壳跑在 elib_pg:with_tx 里——池化入口 shim 到 marker 连接；
%% 事务由外层用例 BEGIN/ROLLBACK 包住（幂等行等随用例回滚）。
with_pool_shim(C, Fun) ->
    ok = meck:new(elib_pg, [passthrough, no_link]),
    ok = meck:expect(elib_pg, with_tx, 1, fun(F) -> F(C) end),
    try
        Fun()
    after
        meck:unload(elib_pg)
    end.

mock_cowboy(Method, Opts) ->
    ok = meck:new(cowboy_req, [no_link]),
    ok = meck:expect(cowboy_req, method, 1, fun(_R) -> Method end),
    %% CP-CON-02：INT-23 versioned 400 路径的 WARN_LOG 读 path（进事务前）
    ok = meck:expect(
        cowboy_req, path, 1, fun(_R) -> <<"/api/internal/v1/webhook/deliveries">> end
    ),
    ok =
        meck:expect(cowboy_req, header, 2, fun(_H, _R) ->
            maps:get(idem_key, Opts, undefined)
        end),
    ok = meck:expect(cowboy_req, parse_qs, 1, fun(_R) -> maps:get(qs, Opts, []) end),
    ok =
        meck:expect(cowboy_req, parse_header, 2, fun(_H, _R) ->
            {<<"application">>, <<"json">>, #{}}
        end),
    ok =
        meck:expect(cowboy_req, read_body, 2, fun(_R, _O) ->
            {ok, maps:get(body, Opts, <<"{}">>), req}
        end),
    ok =
        meck:expect(cowboy_req, reply, 4, fun(Status, _H, Body, _R) ->
            put(repair_f2_high_reply, {Status, Body}),
            req
        end),
    erase(repair_f2_high_reply),
    %% elib_param:post 有进程字典缓存——同进程多用例时各自先清。
    erase({elib_param, post_vals}),
    ok.

%% 断言回包非 5xx（成功面：2xx）并返回解码后的 JSON 体。
reply_captured() ->
    {Status, Body} = get(repair_f2_high_reply),
    ?assert(is_integer(Status) andalso Status >= 200 andalso Status < 300),
    {Status, jsone:decode(Body)}.

%% 原样取回包（CP-CON-02：INT-23 versioned 400 断言不能用 2xx 版本）。
reply_captured_raw() ->
    {Status, Body} = get(repair_f2_high_reply),
    ?assert(is_integer(Status)),
    {Status, jsone:decode(Body)}.

%% INT-09 POST /api/internal/v1/messages/direct（application 代发）：
%% 修前在 with_principal/2 → find_tx(Conn, AppId) 处 undef → 500。
repair_int09_handler(C, State) ->
    repair_seed_grant(C, State),
    Ctx = repair_ctx(C, State),
    Body =
        jsone:encode(#{
            <<"sender_mode">> => <<"application">>,
            <<"recipient_user_id">> => ?EXT_A1,
            <<"msg_type">> => <<"text">>,
            <<"content">> => <<"repair-f2-high INT-09 via handler"/utf8>>
        }),
    with_pool_shim(C, fun() ->
        mock_cowboy(<<"POST">>, #{idem_key => <<"repair-f2-high-int09">>, body => Body}),
        try
            {ok, _Req, _State1} =
                enterprise_message_handler:init(req, #{
                    action => direct, enterprise_internal => Ctx
                }),
            {200, Decoded} = reply_captured(),
            MsgId = maps:get(<<"msg_id">>, Decoded),
            ?assert(is_binary(MsgId) andalso byte_size(MsgId) > 0)
        after
            meck:unload(cowboy_req)
        end
    end).

%% INT-12 PUT /api/internal/v1/webhook（configure）：
%% 修前在 with_principal/2 → find_tx(Conn, AppId) 处 undef → 500。
repair_int12_handler(C, State) ->
    repair_seed_grant(C, State),
    Ctx = repair_ctx(C, State),
    Body =
        jsone:encode(#{
            <<"url">> => <<"https://oa.example.com/hook">>,
            <<"events">> => [<<"file.confirmed">>]
        }),
    with_public_dns(fun() ->
        with_pool_shim(C, fun() ->
            mock_cowboy(<<"PUT">>, #{idem_key => <<"repair-f2-high-int12">>, body => Body}),
            try
                {ok, _Req, _State1} =
                    enterprise_webhook_handler:init(req, #{
                        action => configure, enterprise_internal => Ctx
                    }),
                {200, Decoded} = reply_captured(),
                ?assertEqual(<<"https://oa.example.com/hook">>, maps:get(<<"url">>, Decoded)),
                Secret = maps:get(<<"secret">>, Decoded),
                ?assert(is_binary(Secret) andalso byte_size(Secret) > 40)
            after
                meck:unload(cowboy_req)
            end
        end)
    end).

%% INT-23 GET /api/internal/v1/webhook/deliveries（投递列表 + 健康度摘要）：
%% 修前在 delivery_ctx/2 → find_tx(Conn, AppId) 处 undef → 500。
%% CP-CON-02：CURSOR-V2 keyset 读面（DEC-INT23-COMPAT）——形状为
%% {items, page_size, has_more, next_cursor, summary}；旧 page/size query
%% 出现即 versioned 400 cursor_required_v1（进事务前拒绝）。
repair_int23_handler(C, State) ->
    repair_seed_grant(C, State),
    Ctx = repair_ctx(C, State),
    with_pool_shim(C, fun() ->
        mock_cowboy(<<"GET">>, #{qs => []}),
        try
            {ok, _Req, _State1} =
                enterprise_webhook_handler:init(req, #{
                    action => deliveries, enterprise_internal => Ctx
                }),
            {200, Decoded} = reply_captured(),
            %% 形状：CURSOR-V2 页（items/page_size/has_more/next_cursor）+
            %% summary（健康度摘要），见 enterprise_webhook_repo:
            %% page_deliveries_tx/6（CP-CON-02 keyset）。
            ?assert(is_list(maps:get(<<"items">>, Decoded))),
            ?assert(is_map(maps:get(<<"summary">>, Decoded))),
            ?assertEqual(false, maps:get(<<"has_more">>, Decoded)),
            ?assertEqual(null, maps:get(<<"next_cursor">>, Decoded)),
            ?assertEqual(20, maps:get(<<"page_size">>, Decoded))
        after
            meck:unload(cowboy_req)
        end,
        %% 旧 offset query（page/size 任一）→ 400 cursor_required_v1
        mock_cowboy(<<"GET">>, #{qs => [{<<"page">>, <<"2">>}]}),
        try
            {ok, _Req2, _State2} =
                enterprise_webhook_handler:init(req, #{
                    action => deliveries, enterprise_internal => Ctx
                }),
            {400, Decoded400} = reply_captured_raw(),
            ?assertEqual(
                <<"cursor_required_v1">>,
                maps:get(<<"code">>, maps:get(<<"error">>, Decoded400))
            )
        after
            meck:unload(cowboy_req)
        end
    end).
