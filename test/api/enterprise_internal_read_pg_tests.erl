%% enterprise_internal_read_pg_tests
%% V2.1 A2 — INT-24..31（Internal 资源只读面：workspaces/groups/group_members/
%% projects/channels 五个 family）真库集成测试。
%%
%% 一次性 marker 库（inttest_marker_db 配方，env 前缀 EOV21A2_INTTEST）。
%% setup 阶段 COMMIT 提交双 Org × 多 Workspace × 项目/频道/企业群基线夹具；
%% 业务用例每条 BEGIN ... ROLLBACK，不留数据。
%%
%% 覆盖（plan §6.2 行为矩阵 / §10.1 CURSOR-V2 / §10.2 排序 / §5.2 deny
%% precedence / §15.1 封闭投影冻结）：
%%   ① INT-24：org 全域 Grant 全覆盖列表 + 显式 Grant 收窄 + keyset 翻页
%%      oracle（无重复无丢失）+ 越界 limit 拒绝 + archived/personal/跨 Org
%%      不可见 + 零 Grant deny
%%   ② INT-25：详情 + IDOR 404 同不存在同体 + 未覆盖 W → boundary violation
%%   ③ INT-26：origin app 过滤（本 app 建群可见，人类群/他 app 群不可见）
%%      + active 过滤 + 排序 + tampered/foreign cursor 拒绝
%%   ④ INT-27：active 成员升序分页 + 非活跃成员不可见 + 跨群游标拒绝
%%      + 跨 Org 群 404
%%   ⑤ INT-28：workspace_id 必填（缺失/非整数 400）+ status 三值过滤 + 排序
%%      + filter 绑定（换 status 的旧游标拒绝）
%%   ⑥ INT-29：详情投影 + 跨 Org 404 同体 + 未覆盖 W boundary violation
%%   ⑦ INT-30：scope=workspace AND status=1 强制（personal/停用/删除不可见）
%%      + workspace_id 必填
%%   ⑧ INT-31：详情 + personal/停用/跨 Org 一律 404 同体
%%   ⑨ 投影冻结 oracle：五 family 行键集恰等于冻结清单（封闭投影）
%%   ⑩ A-R usage：workspace.read/group.read/project.read/channel.read 计数
-module(enterprise_internal_read_pg_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

%% ---- 夹具（993 段独立 ID，与 987/988/991/992 段互不冲突） ----

-define(ORG_A, 993101).
-define(ORG_B, 993102).

-define(OWNER_A, 993001).
-define(OWNER_B, 993002).
-define(H_A1, 993011).
-define(H_A2, 993012).
-define(H_B1, 993021).

-define(WS_A1, 993201).
-define(WS_A2, 993202).
-define(WS_A_ARCH, 993203).
-define(WS_A_PERSONAL, 993204).
-define(WS_B1, 993211).

-define(GRANT_KEY, <<"eov21a2-org-full">>).
-define(GRANT_KEY_C, <<"eov21a2-ws-narrow">>).

-define(SCOPES_READ, [
    <<"workspaces:read">>,
    <<"groups:read">>,
    <<"projects:read">>,
    <<"channels:read">>
]).
-define(SCOPES_GROUP_WRITE, [<<"groups:read">>, <<"groups:write">>]).

-define(FAR_FUTURE, <<"2099-01-01T00:00:00Z">>).
-define(CURSOR_KEY_CFG, enterprise_internal_cursor_signing_key).
-define(CURSOR_SECRET, <<"eov21a2_cursor_signing_key_0123456789abcdef">>).

%%%===================================================================
%%% Fixture
%%%===================================================================

%% 收敛 logic 层 {error, {Code, _}} 返回形态（非 error 形态即断言失败）。
-spec catch_err(fun(() -> {ok, term()} | {error, {binary(), term()}})) ->
    {error, {binary(), term()}}.
catch_err(Fun) when is_function(Fun, 0) ->
    {error, {_Code, _Detail}} = Fun(),
    {error, {_Code, _Detail}}.

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
        [group_info, group_member, enterprise_application, enterprise_external_identity]
    ),
    {ok, _} = application:ensure_all_started(throttle),
    ok = application:set_env(imboy, ?CURSOR_KEY_CFG, ?CURSOR_SECRET),
    State =
        inttest_marker_db:provision(
            #{
                env_prefix => <<"EOV21A2_INTTEST">>,
                connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
            }
        ),
    C = maps:get(conn, State),
    ok = exec(C, <<"BEGIN">>),
    try
        seed_matrix(C),
        ok = exec(C, <<"COMMIT">>)
    catch
        Class:Reason:Stack ->
            _ = exec_quiet(C, <<"ROLLBACK">>),
            application:unset_env(imboy, ?CURSOR_KEY_CFG),
            inttest_marker_db:release(State),
            erlang:raise(Class, {fixture_seed_failed, Reason}, Stack)
    end,
    {AppA, AppC} = app_ids(C),
    State#{conn => C, app_a => AppA, app_c => AppC}.

close_conn(State) ->
    application:unset_env(imboy, ?CURSOR_KEY_CFG),
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

exec_params(C, Sql, Params) ->
    case elib_pg:query(C, Sql, Params) of
        {ok, _} -> ok;
        {error, Reason} -> erlang:error({sql_error, Reason, Sql})
    end.

one(C, Sql, Params) ->
    case elib_pg:query(C, Sql, Params) of
        {ok, [Row | _]} -> Row;
        {ok, []} -> #{};
        {error, Reason} -> erlang:error({sql_error, Reason, Sql})
    end.

scalar(C, Sql, Params) ->
    case elib_pg:query(C, Sql, Params) of
        {ok, [Row | _]} -> hd(maps:values(Row));
        {ok, []} -> undefined;
        {error, Reason} -> erlang:error({sql_error, Reason, Sql})
    end.

exec_quiet(C, IoData) ->
    _ = elib_pg:query(C, iolist_to_binary(IoData), []),
    ok.

%% ---- 夹具矩阵 ----

seed_matrix(C) ->
    seed_user(C, ?OWNER_A),
    seed_user(C, ?OWNER_B),
    seed_user(C, ?H_A1),
    seed_user(C, ?H_A2),
    seed_user(C, ?H_B1),
    seed_org(C, ?ORG_A, ?OWNER_A, <<"eov21a2-org-a">>),
    seed_org(C, ?ORG_B, ?OWNER_B, <<"eov21a2-org-b">>),
    seed_workspace(C, ?WS_A1, ?ORG_A, ?OWNER_A, <<"active">>, 1000),
    seed_workspace(C, ?WS_A2, ?ORG_A, ?OWNER_A, <<"active">>, 2000),
    seed_workspace(C, ?WS_A_ARCH, ?ORG_A, ?OWNER_A, <<"archived">>, 3000),
    %% personal workspace：organization_id IS NULL（历史个人区，Internal 面不可见）
    seed_workspace(C, ?WS_A_PERSONAL, undefined, ?OWNER_A, <<"active">>, 4000),
    seed_workspace(C, ?WS_B1, ?ORG_B, ?OWNER_B, <<"active">>, 5000),
    seed_ws_member(C, ?WS_A1, ?H_A1),
    seed_ws_member(C, ?WS_A2, ?H_A2),
    seed_ws_member(C, ?WS_B1, ?H_B1),
    %% 群 993501 的 active 成员必须是 WS_A1 active 成员
    %% （trg_group_member_ws_subset，00000077）
    seed_ws_member(C, ?WS_A1, ?H_A2),
    seed_ws_member(C, ?WS_A1, ?OWNER_A),
    seed_ws_member(C, ?WS_A1, ?OWNER_B),
    %% app A：org 全域 Grant（四 read scope 全覆盖）
    {ok, AppA} = enterprise_internal_ops:create_application_tx(
        C, ?ORG_A, <<"eov21a2-oa-a">>, <<"eov21a2 org A oa"/utf8>>, ?SCOPES_READ
    ),
    ok = issue_grant(C, ?ORG_A, maps:get(<<"id">>, AppA), ?SCOPES_READ, ?GRANT_KEY, none, []),
    %% app C：显式 Workspace Grant（仅 WS_A1）——收窄正例/未覆盖负例
    {ok, AppC} = enterprise_internal_ops:create_application_tx(
        C, ?ORG_A, <<"eov21a2-oa-c">>, <<"eov21a2 org A oa c"/utf8>>, ?SCOPES_READ
    ),
    ok = issue_grant(
        C, ?ORG_A, maps:get(<<"id">>, AppC), ?SCOPES_READ, ?GRANT_KEY_C, explicit, [?WS_A1]
    ),
    %% app D：零 Grant（allowed_scopes 有 workspaces:read 但无 Grant 行）
    {ok, _AppD} = enterprise_internal_ops:create_application_tx(
        C, ?ORG_A, <<"eov21a2-oa-d">>, <<"eov21a2 org A oa d"/utf8>>, ?SCOPES_READ
    ),
    %% 项目：WS_A1 三行（active×2 + done×1，created_at 递增）、WS_A2 一行
    seed_project(C, 993301, ?WS_A1, <<"p-1">>, <<"active">>, 100),
    seed_project(C, 993302, ?WS_A1, <<"p-2">>, <<"done">>, 200),
    seed_project(C, 993303, ?WS_A1, <<"p-3">>, <<"active">>, 300),
    seed_project(C, 993304, ?WS_A2, <<"p-4">>, <<"active">>, 400),
    seed_project(C, 993305, ?WS_B1, <<"p-b">>, <<"active">>, 500),
    %% 频道：WS_A1 workspace 频道 3 条（status 1/0/-1）+ personal 频道 1 条
    seed_channel(C, 993401, ?WS_A1, <<"ch-1">>, workspace, 1, 100),
    seed_channel(C, 993402, ?WS_A1, <<"ch-2">>, workspace, 1, 200),
    seed_channel(C, 993403, ?WS_A1, <<"ch-0-disabled">>, workspace, 0, 300),
    seed_channel(C, 993404, ?WS_A1, <<"ch-1-deleted">>, workspace, -1, 350),
    seed_channel(C, 993405, ?WS_A2, <<"ch-ws2">>, workspace, 1, 400),
    seed_channel(C, 993406, undefined, <<"ch-personal">>, personal, 1, 500),
    %% 企业群：app A origin（WS_A1 两行 + WS_A2 一行）；人类群；app B origin
    seed_group(C, 993501, ?WS_A1, <<"g-1">>, 1, 100),
    seed_group(C, 993502, ?WS_A1, <<"g-2">>, 1, 200),
    seed_group(C, 993503, ?WS_A2, <<"g-3">>, 1, 300),
    seed_group(C, 993504, ?WS_A1, <<"g-archived">>, 0, 400),
    ok = seed_group_origin(C, 993501, ?ORG_A, app_a_id(C), ?WS_A1, <<"active">>),
    ok = seed_group_origin(C, 993502, ?ORG_A, app_a_id(C), ?WS_A1, <<"active">>),
    ok = seed_group_origin(C, 993503, ?ORG_A, app_a_id(C), ?WS_A2, <<"active">>),
    ok = seed_group_origin(C, 993504, ?ORG_A, app_a_id(C), ?WS_A1, <<"archived">>),
    %% app C origin 群：WS_A1 一行（Grant 覆盖，可见）+ WS_A2 一行（未覆盖，
    %% 行级收窄排除）
    seed_group(C, 993506, ?WS_A1, <<"g-c1">>, 1, 150),
    seed_group(C, 993507, ?WS_A2, <<"g-c2">>, 1, 250),
    ok = seed_group_origin(C, 993506, ?ORG_A, app_c_id(C), ?WS_A1, <<"active">>),
    ok = seed_group_origin(C, 993507, ?ORG_A, app_c_id(C), ?WS_A2, <<"active">>),
    %% 993505：人类群（无 origin 行）
    %% 群成员：993501 六行（4 active + 2 非活跃，created_at 递增）
    seed_group_member(C, 993601, 993501, 993011, 4, 1, 100),
    seed_group_member(C, 993602, 993501, 993012, 1, 1, 200),
    seed_group_member(C, 993603, 993501, 993001, 1, 1, 300),
    seed_group_member(C, 993604, 993501, 993002, 1, 1, 400),
    seed_group_member(C, 993605, 993501, 993021, 1, 0, 500),
    seed_group_member(C, 993606, 993501, 993099, 1, 2, 600),
    ok.

app_c_id(C) ->
    maps:get(
        <<"id">>,
        one(C, <<"SELECT id FROM enterprise_application WHERE application_key = $1">>, [
            <<"eov21a2-oa-c">>
        ])
    ).

app_a_id(C) ->
    maps:get(
        <<"id">>,
        one(C, <<"SELECT id FROM enterprise_application WHERE application_key = $1">>, [
            <<"eov21a2-oa-a">>
        ])
    ).

app_ids(C) ->
    A = app_a_id(C),
    CC = maps:get(
        <<"id">>,
        one(C, <<"SELECT id FROM enterprise_application WHERE application_key = $1">>, [
            <<"eov21a2-oa-c">>
        ])
    ),
    {A, CC}.

issue_grant(C, OrgId, AppId, Scopes, Key, Kind, WsIds) ->
    case
        enterprise_internal_ops:issue_grant_tx(C, OrgId, AppId, #{
            scopes => Scopes,
            workspace_scope_kind => Kind,
            workspace_ids => WsIds,
            idempotency_key => Key,
            expires_at => ?FAR_FUTURE
        })
    of
        {ok, _} -> ok;
        {error, Reason} -> erlang:error({grant_seed_failed, Key, Reason})
    end.

seed_user(C, Uid) ->
    exec(C, [
        <<"INSERT INTO \"user\" (id, password, account, reg_ip, reg_cosv) VALUES (">>,
        integer_to_binary(Uid),
        ", 'x', 't993_u",
        integer_to_binary(Uid),
        <<"', '127.0.0.1', 'x')">>
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

seed_workspace(C, WsId, OrgId, OwnerUid, Status, CreatedOffsetSec) ->
    OrgClause =
        case OrgId of
            undefined -> <<"NULL">>;
            _ -> integer_to_binary(OrgId)
        end,
    exec(C, [
        <<"INSERT INTO workspace (id, name, owner_id, status, organization_id, created_at, updated_at) VALUES (">>,
        integer_to_binary(WsId),
        ", 't993_ws_",
        integer_to_binary(WsId),
        "', ",
        integer_to_binary(OwnerUid),
        ", '",
        Status,
        "', ",
        OrgClause,
        ", NOW() - ('",
        integer_to_binary(CreatedOffsetSec),
        <<" seconds'::interval), CURRENT_TIMESTAMP)">>
    ]).

seed_ws_member(C, WsId, Uid) ->
    exec(C, [
        <<"INSERT INTO workspace_member (workspace_id, user_id, role, status, joined_at, created_at, updated_at) VALUES (">>,
        integer_to_binary(WsId),
        ", ",
        integer_to_binary(Uid),
        <<", 'member', 'active', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)">>
    ]).

seed_project(C, PId, WsId, Name, Status, CreatedOffsetSec) ->
    %% owner 必须是该 workspace 的 active 成员（fk_project_owner_membership
    %% + trg_project_owner_membership_active 复合兜底）
    Owner =
        case WsId of
            ?WS_A1 -> ?H_A1;
            ?WS_A2 -> ?H_A2;
            _ -> ?H_B1
        end,
    exec(C, [
        <<"INSERT INTO project (id, workspace_id, name, description, owner_id, status, created_at, updated_at) VALUES (">>,
        integer_to_binary(PId),
        ", ",
        integer_to_binary(WsId),
        ", '",
        Name,
        <<"', 'd', ">>,
        integer_to_binary(Owner),
        ", '",
        Status,
        <<"', NOW() - ('">>,
        integer_to_binary(CreatedOffsetSec),
        <<" seconds'::interval), CURRENT_TIMESTAMP)">>
    ]).

seed_channel(C, ChId, WsId, Name, Scope, Status, CreatedOffsetSec) ->
    WsClause =
        case WsId of
            undefined -> <<"NULL">>;
            _ -> integer_to_binary(WsId)
        end,
    exec(C, [
        <<"INSERT INTO channel (id, name, creator_uid, status, scope, workspace_id, created_at, updated_at) VALUES (">>,
        integer_to_binary(ChId),
        ", '",
        Name,
        <<"', ">>,
        integer_to_binary(?H_A1),
        ", ",
        integer_to_binary(Status),
        ", '",
        atom_to_binary(Scope, utf8),
        "', ",
        WsClause,
        <<" , NOW() - ('">>,
        integer_to_binary(CreatedOffsetSec),
        <<" seconds'::interval), CURRENT_TIMESTAMP)">>
    ]).

seed_group(C, Gid, WsId, Title, Status, CreatedOffsetSec) ->
    exec(C, [
        <<"INSERT INTO \"group\" (id, owner_uid, creator_uid, title, status, scope, workspace_id, member_count, created_at, updated_at) VALUES (">>,
        integer_to_binary(Gid),
        ", ",
        integer_to_binary(?H_A1),
        ", ",
        integer_to_binary(?H_A1),
        ", '",
        Title,
        "', ",
        integer_to_binary(Status),
        <<", 'workspace', ">>,
        integer_to_binary(WsId),
        <<", 0, NOW() - ('">>,
        integer_to_binary(CreatedOffsetSec),
        <<" seconds'::interval), CURRENT_TIMESTAMP)">>
    ]).

seed_group_origin(C, Gid, OrgId, AppId, WsId, Status) ->
    %% archived 归属行必须带 archived_at（ck_ego_archived_consistency）
    case Status of
        <<"archived">> ->
            exec(C, [
                <<
                    "INSERT INTO enterprise_group_origin"
                    " (group_id, organization_id, application_id, workspace_id, status,"
                    "  created_at, archived_at)"
                    " VALUES ("
                >>,
                integer_to_binary(Gid),
                ", ",
                integer_to_binary(OrgId),
                ", ",
                integer_to_binary(AppId),
                ", ",
                integer_to_binary(WsId),
                <<", 'archived', NOW(), NOW())">>
            ]);
        <<"active">> ->
            exec(C, [
                <<
                    "INSERT INTO enterprise_group_origin"
                    " (group_id, organization_id, application_id, workspace_id, status,"
                    "  created_at)"
                    " VALUES ("
                >>,
                integer_to_binary(Gid),
                ", ",
                integer_to_binary(OrgId),
                ", ",
                integer_to_binary(AppId),
                ", ",
                integer_to_binary(WsId),
                <<", 'active', NOW())">>
            ])
    end.

seed_group_member(C, MId, Gid, Uid, Role, Status, CreatedOffsetSec) ->
    exec(C, [
        <<"INSERT INTO group_member (id, group_id, user_id, role, status, created_at, updated_at) VALUES (">>,
        integer_to_binary(MId),
        ", ",
        integer_to_binary(Gid),
        ", ",
        integer_to_binary(Uid),
        ", ",
        integer_to_binary(Role),
        ", ",
        integer_to_binary(Status),
        <<" , NOW() - ('">>,
        integer_to_binary(CreatedOffsetSec),
        <<" seconds'::interval), CURRENT_TIMESTAMP)">>
    ]).

%% ---- ctx 构造（真链路 context_tx 求值；与 handler 从认证链拿到的同形状） ----

ctx_of(C, State, AppKey) ->
    AppId = maps:get(AppKey, State),
    {ok, #{grant_governed := Governed, effective_scopes := Effective}} =
        enterprise_application_grant_logic:context_tx(C, ?ORG_A, AppId, ?SCOPES_READ),
    #{
        organization_id => ?ORG_A,
        application_id => AppId,
        granted_scopes => Effective,
        grant_governed => Governed
    }.

%%%===================================================================
%%% Tests
%%%===================================================================

read_page_test_() ->
    {timeout, 900,
        {setup, fun setup_conn/0, fun close_conn/1, fun(State) ->
            C = maps:get(conn, State),
            tests(C, State)
        end}}.

tests(C, State) ->
    [
        %% ① INT-24
        {"int24_org_wide_grant_lists_all_active_workspaces",
            with_tx(C, fun(C1) -> int24_full(C1, State) end)},
        {"int24_explicit_grant_narrows_rows", with_tx(C, fun(C1) -> int24_narrow(C1, State) end)},
        {"int24_paging_oracle_no_dup_no_loss", with_tx(C, fun(C1) -> int24_paging(C1, State) end)},
        {"int24_zero_grant_denied", with_tx(C, fun(C1) -> int24_zero_grant(C1, State) end)},
        {"int24_cursor_negatives",
            with_tx(C, fun(C1) -> cursor_negatives(C1, State, <<"workspaces">>, workspaces) end)},
        %% ② INT-25
        {"int25_detail_and_idor_same_body_and_boundary",
            with_tx(C, fun(C1) -> int25(C1, State) end)},
        %% ③ INT-26
        {"int26_origin_app_filter_and_ordering", with_tx(C, fun(C1) -> int26(C1, State) end)},
        {"int26_cursor_negatives",
            with_tx(C, fun(C1) -> cursor_negatives(C1, State, <<"groups">>, groups) end)},
        %% ④ INT-27
        {"int27_members_asc_and_filters", with_tx(C, fun(C1) -> int27(C1, State) end)},
        %% ⑤ INT-28
        {"int28_workspace_required_and_status_filter", with_tx(C, fun(C1) -> int28(C1, State) end)},
        {"int28_cursor_filter_binding", with_tx(C, fun(C1) -> int28_cursor_binding(C1, State) end)},
        %% ⑥ INT-29
        {"int29_detail_idor_boundary", with_tx(C, fun(C1) -> int29(C1, State) end)},
        %% ⑦ INT-30
        {"int30_scope_status_forced_and_ws_required", with_tx(C, fun(C1) -> int30(C1, State) end)},
        %% ⑧ INT-31
        {"int31_detail_invisible_variants_404", with_tx(C, fun(C1) -> int31(C1, State) end)},
        %% ⑨ 投影冻结 + ⑩ usage
        {"projection_frozen_all_families", with_tx(C, fun(C1) -> projection_frozen(C1, State) end)},
        %% read_page 纯函数：limit 解析（越界拒绝不截断）
        {"read_page_limit_parsing", limit_parsing_test()}
    ].

%%%===================================================================
%%% INT-24
%%%===================================================================

int24_full(C, State) ->
    Ctx = ctx_of(C, State, app_a),
    {ok, Page} = enterprise_workspace_logic:list_workspaces_tx(C, Ctx, #{limit => 50}),
    Ids = [maps:get(<<"workspace_id">>, I) || I <- maps:get(<<"items">>, Page)],
    %% org 全域 Grant：A 的全部 active workspace（A1/A2），archived/personal/B 不可见
    ?assertEqual([?WS_A1, ?WS_A2], Ids),
    ?assertEqual(false, maps:get(<<"has_more">>, Page)),
    ?assertEqual(null, maps:get(<<"next_cursor">>, Page)),
    ?assertEqual(50, maps:get(<<"limit">>, Page)).

int24_narrow(C, State) ->
    Ctx = ctx_of(C, State, app_c),
    {ok, Page} = enterprise_workspace_logic:list_workspaces_tx(C, Ctx, #{limit => 50}),
    Ids = [maps:get(<<"workspace_id">>, I) || I <- maps:get(<<"items">>, Page)],
    %% 显式 Grant 只覆盖 WS_A1
    ?assertEqual([?WS_A1], Ids).

int24_paging(C, State) ->
    Ctx = ctx_of(C, State, app_a),
    %% 额外插入 5 个 workspace（使总数 7）后 limit=3 翻页
    lists:foreach(
        fun(N) ->
            exec(C, [
                <<"INSERT INTO workspace (id, name, owner_id, status, organization_id, created_at, updated_at) VALUES (">>,
                integer_to_binary(993250 + N),
                ", 't993_ws_page', ",
                integer_to_binary(?OWNER_A),
                <<", 'active', ">>,
                integer_to_binary(?ORG_A),
                <<" , NOW() - ('">>,
                integer_to_binary(600 + N),
                <<" seconds'::interval), CURRENT_TIMESTAMP)">>
            ])
        end,
        lists:seq(1, 5)
    ),
    {All, Pages} = walk(C, Ctx, workspaces, 3, undefined, [], 0),
    %% 7 行（A1/A2 + 5 新增）恰好各出现一次；created_at DESC, id DESC
    Expected =
        scalar(
            C,
            <<"SELECT ARRAY_AGG(id ORDER BY created_at DESC, id DESC) FROM workspace WHERE organization_id = $1 AND status = 'active'">>,
            [
                ?ORG_A
            ]
        ),
    ?assertEqual({array, Expected}, {array, All}),
    ?assertEqual(7, length(All)),
    ?assertEqual(7, length(lists:usort(All))),
    ?assertEqual(3, Pages).

%% 通用翻页 walker：翻到 next_cursor=null 为止，返回 {全部行主键, 页数}。
walk(C, Ctx, Family, Limit, Cursor, Acc, Pages) ->
    {ok, Page} = list_call(C, Ctx, Family, #{limit => Limit, cursor => Cursor}),
    Ids = [item_id(Family, I) || I <- maps:get(<<"items">>, Page)],
    case maps:get(<<"next_cursor">>, Page) of
        null ->
            {Acc ++ Ids, Pages + 1};
        Next ->
            walk(C, Ctx, Family, Limit, Next, Acc ++ Ids, Pages + 1)
    end.

item_id(workspaces, I) -> maps:get(<<"workspace_id">>, I);
item_id(groups, I) -> maps:get(<<"group_id">>, I);
item_id(group_members, I) -> maps:get(<<"user_id">>, I);
item_id(projects, I) -> maps:get(<<"project_id">>, I);
item_id(channels, I) -> maps:get(<<"channel_id">>, I).

list_call(C, Ctx, workspaces, Opts) ->
    enterprise_workspace_logic:list_workspaces_tx(C, Ctx, Opts);
list_call(C, Ctx, groups, Opts) ->
    enterprise_group_logic:list_groups_tx(C, Ctx, Opts);
list_call(C, Ctx, group_members, Opts) ->
    #{cursor := Cursor, limit := Limit} = maps:merge(#{cursor => undefined, limit => 50}, Opts),
    enterprise_group_logic:list_members_tx(C, Ctx, 993501, #{
        limit => Limit, cursor => Cursor
    });
list_call(C, Ctx, projects, Opts) ->
    enterprise_project_logic:list_projects_tx(
        C, Ctx, maps:merge(#{workspace_id => ?WS_A1, status => all}, Opts)
    );
list_call(C, Ctx, channels, Opts) ->
    enterprise_channel_logic:list_channels_tx(
        C, Ctx, maps:merge(#{workspace_id => ?WS_A1}, Opts)
    ).

int24_zero_grant(C, _State) ->
    %% app D：零 Grant → enforce（kind=list）insufficient_scope（零 Grant 恒 403）
    AppD = maps:get(
        <<"id">>,
        one(C, <<"SELECT id FROM enterprise_application WHERE application_key = $1">>, [
            <<"eov21a2-oa-d">>
        ])
    ),
    {ok, #{effective_scopes := Effective}} =
        enterprise_application_grant_logic:context_tx(C, ?ORG_A, AppD, ?SCOPES_READ),
    ?assertEqual([], Effective),
    Ctx = #{
        organization_id => ?ORG_A,
        application_id => AppD,
        granted_scopes => Effective
    },
    ?assertEqual(
        {error, insufficient_scope},
        enterprise_internal_boundary:enforce(C, Ctx, <<"INT-24">>, undefined)
    ),
    ?assertEqual(
        {error, insufficient_scope},
        enterprise_internal_boundary:enforce(C, Ctx, <<"INT-25">>, ?WS_A1)
    ).

%%%===================================================================
%%% 通用 cursor 负例（tampered / foreign family / foreign O/App / filter 漂移）
%%%===================================================================

cursor_negatives(C, State, FamilyBin, FamilyAtom) ->
    Ctx = ctx_of(C, State, app_a),
    {ok, Page} = list_call(C, Ctx, FamilyAtom, #{limit => 1}),
    ?assertEqual(true, maps:get(<<"has_more">>, Page)),
    Cursor = maps:get(<<"next_cursor">>, Page),
    %% tampered：翻转首字符
    Tampered = tamper_first(Cursor),
    ?assertMatch(
        {error, {<<"invalid_request">>, cursor_invalid}},
        list_call(C, Ctx, FamilyAtom, #{limit => 1, cursor => Tampered})
    ),
    %% malformed：非游标形态
    ?assertMatch(
        {error, {<<"invalid_request">>, cursor_invalid}},
        list_call(C, Ctx, FamilyAtom, #{limit => 1, cursor => <<"not-a-cursor">>})
    ),
    %% foreign family：同签名 key 下签 projects family 游标用于本 family
    Foreign = signed_foreign_family(Ctx, FamilyBin),
    ?assertMatch(
        {error, {<<"invalid_request">>, cursor_invalid}},
        list_call(C, Ctx, FamilyAtom, #{limit => 1, cursor => Foreign})
    ),
    %% foreign App：同 family 不同 application_id
    ForeignApp = signed_foreign_app(State, FamilyBin),
    ?assertMatch(
        {error, {<<"invalid_request">>, cursor_invalid}},
        list_call(C, Ctx, FamilyAtom, #{limit => 1, cursor => ForeignApp})
    ),
    %% 过期游标（issued_at = now - 25h）
    Expired = signed_expired(Ctx, FamilyBin),
    ?assertMatch(
        {error, {<<"invalid_request">>, cursor_invalid}},
        list_call(C, Ctx, FamilyAtom, #{limit => 1, cursor => Expired})
    ).

tamper_first(<<First, Rest/binary>>) when First =:= $A ->
    <<$B, Rest/binary>>;
tamper_first(<<First, Rest/binary>>) ->
    Next = First + 1,
    <<Next, Rest/binary>>.

signed_foreign_family(Ctx, MyFamily) ->
    Other =
        case MyFamily of
            <<"workspaces">> -> <<"groups">>;
            <<"groups">> -> <<"workspaces">>
        end,
    {ok, Cursor} = sign_cursor(Ctx, Other, #{}, [<<"2026-01-01T00:00:00Z">>, 1]),
    Cursor.

signed_foreign_app(State, Family) ->
    ForeignCtx = #{organization_id => ?ORG_A, application_id => maps:get(app_c, State)},
    {ok, Cursor} = sign_cursor(ForeignCtx, Family, #{}, [<<"2026-01-01T00:00:00Z">>, 1]),
    Cursor.

signed_expired(Ctx, Family) ->
    {ok, Cursor} = sign_cursor_at(
        Ctx, Family, #{}, [<<"2026-01-01T00:00:00Z">>, 1], os:system_time(second) - 90000
    ),
    Cursor.

sign_cursor(Ctx, Family, Filter, Tuple) ->
    sign_cursor_at(Ctx, Family, Filter, Tuple, os:system_time(second)).

sign_cursor_at(Ctx, Family, Filter, Tuple, IssuedAt) ->
    Payload = enterprise_cursor_v2:build_payload(
        Family,
        maps:get(organization_id, Ctx),
        maps:get(application_id, Ctx),
        Filter,
        Tuple,
        IssuedAt
    ),
    enterprise_cursor_v2:sign(Payload, ?CURSOR_SECRET).

%%%===================================================================
%%% INT-25
%%%===================================================================

int25(C, State) ->
    Ctx = ctx_of(C, State, app_a),
    %% 详情正例（投影冻结：恰 4 键）
    {ok, Ws1} = enterprise_workspace_logic:locate_active_tx(C, Ctx, ?WS_A1),
    ?assertEqual(ok, enterprise_internal_boundary:enforce(C, Ctx, <<"INT-25">>, ?WS_A1)),
    {ok, Detail} = enterprise_workspace_logic:detail_tx(C, Ctx, Ws1),
    ?assertEqual(
        [<<"created_at">>, <<"name">>, <<"owner_id">>, <<"workspace_id">>],
        lists:sort(maps:keys(Detail))
    ),
    ?assertEqual(?WS_A1, maps:get(<<"workspace_id">>, Detail)),
    %% archived → resource_not_found
    ?assertMatch(
        {error, {<<"resource_not_found">>, _}},
        enterprise_workspace_logic:locate_active_tx(C, Ctx, ?WS_A_ARCH)
    ),
    %% 跨 Org / 不存在 / personal：IDOR 404 同不存在同体
    {error, {CodeCross, _}} = catch_err(fun() ->
        enterprise_workspace_logic:locate_active_tx(C, Ctx, ?WS_B1)
    end),
    {error, {CodeMissing, _}} = catch_err(fun() ->
        enterprise_workspace_logic:locate_active_tx(C, Ctx, 999999999)
    end),
    {error, {CodePersonal, _}} = catch_err(fun() ->
        enterprise_workspace_logic:locate_active_tx(C, Ctx, ?WS_A_PERSONAL)
    end),
    ?assertEqual(<<"resource_not_found">>, CodeCross),
    ?assertEqual(CodeMissing, CodeCross),
    ?assertEqual(CodePersonal, CodeCross),
    %% 同 Org 未覆盖 W（app C 仅覆盖 WS_A1）：404 不触发（行在 Org 内），
    %% enforce 403 organization_boundary_violation（deny precedence 第 6 步）
    CtxC = ctx_of(C, State, app_c),
    {ok, _Ws2} = enterprise_workspace_logic:locate_active_tx(C, CtxC, ?WS_A2),
    ?assertEqual(
        {error, organization_boundary_violation},
        enterprise_internal_boundary:enforce(C, CtxC, <<"INT-25">>, ?WS_A2)
    ).

%%%===================================================================
%%% INT-26
%%%===================================================================

int26(C, State) ->
    Ctx = ctx_of(C, State, app_a),
    {ok, Page} = enterprise_group_logic:list_groups_tx(C, Ctx, #{limit => 50}),
    Ids = [maps:get(<<"group_id">>, I) || I <- maps:get(<<"items">>, Page)],
    %% app A origin 的 active 群（created_at DESC：100/200/300 秒前 → id 升序）：
    %% 人类群（993505 无 origin）、archived 群（993504，origin archived）、
    %% app C 的 origin 群（993506/993507，origin app 过滤排除）、他域不可见
    ?assertEqual([993501, 993502, 993503], Ids),
    %% app C（显式 Grant 仅 WS_A1）：自己的 origin 群中 993507（WS_A2）被
    %% 行级「Grant 覆盖 W」收窄排除，只剩 993506
    CtxC = ctx_of(C, State, app_c),
    {ok, PageC} = enterprise_group_logic:list_groups_tx(C, CtxC, #{limit => 50}),
    ?assertEqual(
        [993506],
        [maps:get(<<"group_id">>, I) || I <- maps:get(<<"items">>, PageC)]
    ).

%%%===================================================================
%%% INT-27
%%%===================================================================

int27(C, State) ->
    Ctx = ctx_of(C, State, app_a),
    %% active 成员 4 人（993601-993604），created_at ASC：993011(owner) 先入
    {ok, Page} = enterprise_group_logic:list_members_tx(C, Ctx, 993501, #{limit => 50}),
    Uids = [maps:get(<<"user_id">>, I) || I <- maps:get(<<"items">>, Page)],
    ?assertEqual([993002, 993001, 993012, 993011], Uids),
    %% 投影恰 3 键
    lists:foreach(
        fun(I) ->
            ?assertEqual([<<"created_at">>, <<"role">>, <<"user_id">>], lists:sort(maps:keys(I)))
        end,
        maps:get(<<"items">>, Page)
    ),
    %% 翻页（limit=2，升序无重复无丢失）
    {All, _Pages} = walk(C, Ctx, group_members, 2, undefined, [], 0),
    ?assertEqual([993002, 993001, 993012, 993011], All),
    %% 跨群游标（filter group_id 绑定）：993501 的游标用于 993503 → 拒绝
    {ok, P1} = enterprise_group_logic:list_members_tx(C, Ctx, 993501, #{limit => 2}),
    Cursor = maps:get(<<"next_cursor">>, P1),
    ?assertMatch(
        {error, {<<"invalid_request">>, cursor_invalid}},
        enterprise_group_logic:list_members_tx(C, Ctx, 993503, #{limit => 2, cursor => Cursor})
    ),
    %% 跨 Org 群：boundary_workspace_tx 404（IDOR 同体）
    %% 993505 是本 Org 人类群（无 origin）；构造跨 Org 群不可行（ws 归属 B），
    %% 断言 locate 对不存在群同体
    ?assertMatch(
        {error, {<<"resource_not_found">>, _}},
        enterprise_group_logic:boundary_workspace_tx(C, Ctx, 999999999)
    ).

%%%===================================================================
%%% INT-28
%%%===================================================================

int28(C, State) ->
    Ctx = ctx_of(C, State, app_a),
    %% workspace_id 缺失 / 非整数 → invalid_request
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_project_logic:list_projects_tx(C, Ctx, #{limit => 10})
    ),
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_project_logic:list_projects_tx(C, Ctx, #{
            workspace_id => <<"nope">>, limit => 10
        })
    ),
    %% 非法 status → invalid_request
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_project_logic:list_projects_tx(C, Ctx, #{
            workspace_id => ?WS_A1, status => <<"archived">>, limit => 10
        })
    ),
    %% all（缺省）：WS_A1 三行 created_at DESC, id DESC
    {ok, All} = enterprise_project_logic:list_projects_tx(
        C, Ctx, #{workspace_id => ?WS_A1, status => all, limit => 50}
    ),
    ?assertEqual(
        [993301, 993302, 993303],
        [maps:get(<<"project_id">>, I) || I <- maps:get(<<"items">>, All)]
    ),
    %% active：两行
    {ok, Active} = enterprise_project_logic:list_projects_tx(
        C, Ctx, #{workspace_id => ?WS_A1, status => <<"active">>, limit => 50}
    ),
    ?assertEqual(
        [993301, 993303],
        [maps:get(<<"project_id">>, I) || I <- maps:get(<<"items">>, Active)]
    ),
    %% done：一行
    {ok, Done} = enterprise_project_logic:list_projects_tx(
        C, Ctx, #{workspace_id => ?WS_A1, status => <<"done">>, limit => 50}
    ),
    ?assertEqual(
        [993302],
        [maps:get(<<"project_id">>, I) || I <- maps:get(<<"items">>, Done)]
    ),
    %% 跨 Org workspace：行级收窄不在 logic（handler 已 enforce 边界）；
    %% boundary 层对跨 Org W 判 organization_boundary_violation（fail-closed）
    ?assertEqual(
        {error, organization_boundary_violation},
        enterprise_internal_boundary:enforce(C, Ctx, <<"INT-28">>, ?WS_B1)
    ),
    %% 翻页 oracle
    {Ids, Pages} = walk(C, Ctx, projects, 2, undefined, [], 0),
    ?assertEqual([993301, 993302, 993303], Ids),
    ?assertEqual(2, Pages).

int28_cursor_binding(C, State) ->
    Ctx = ctx_of(C, State, app_a),
    {ok, P1} = enterprise_project_logic:list_projects_tx(
        C, Ctx, #{workspace_id => ?WS_A1, status => <<"active">>, limit => 1}
    ),
    Cursor = maps:get(<<"next_cursor">>, P1),
    %% 换 status 的旧游标（filter 漂移）→ 拒绝
    ?assertMatch(
        {error, {<<"invalid_request">>, cursor_invalid}},
        enterprise_project_logic:list_projects_tx(C, Ctx, #{
            workspace_id => ?WS_A1, status => <<"done">>, limit => 1, cursor => Cursor
        })
    ),
    %% 换 workspace_id 的旧游标 → 拒绝
    ?assertMatch(
        {error, {<<"invalid_request">>, cursor_invalid}},
        enterprise_project_logic:list_projects_tx(C, Ctx, #{
            workspace_id => ?WS_A2, status => <<"active">>, limit => 1, cursor => Cursor
        })
    ).

%%%===================================================================
%%% INT-29
%%%===================================================================

int29(C, State) ->
    Ctx = ctx_of(C, State, app_a),
    {ok, Row} = enterprise_project_logic:locate_tx(C, Ctx, 993301),
    ?assertEqual(ok, enterprise_internal_boundary:enforce(C, Ctx, <<"INT-29">>, ?WS_A1)),
    {ok, Detail} = enterprise_project_logic:detail_tx(C, Ctx, Row),
    ?assertEqual(
        [
            <<"created_at">>,
            <<"description">>,
            <<"name">>,
            <<"owner_id">>,
            <<"project_id">>,
            <<"status">>,
            <<"workspace_id">>
        ],
        lists:sort(maps:keys(Detail))
    ),
    ?assertEqual(?WS_A1, maps:get(<<"workspace_id">>, Detail)),
    %% 跨 Org / 不存在：404 同体
    {error, {E1, _}} = catch_err(fun() ->
        enterprise_project_logic:locate_tx(C, Ctx, 993305)
    end),
    {error, {E2, _}} = catch_err(fun() ->
        enterprise_project_logic:locate_tx(C, Ctx, 999999999)
    end),
    ?assertEqual(<<"resource_not_found">>, E1),
    ?assertEqual(E1, E2),
    %% app C 未覆盖 WS_A2 → project 993304 定位成功（同 Org）但 enforce 403
    CtxC = ctx_of(C, State, app_c),
    {ok, _} = enterprise_project_logic:locate_tx(C, CtxC, 993304),
    ?assertEqual(
        {error, organization_boundary_violation},
        enterprise_internal_boundary:enforce(C, CtxC, <<"INT-29">>, ?WS_A2)
    ).

%%%===================================================================
%%% INT-30
%%%===================================================================

int30(C, State) ->
    Ctx = ctx_of(C, State, app_a),
    %% workspace_id 必填
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_channel_logic:list_channels_tx(C, Ctx, #{limit => 10})
    ),
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_channel_logic:list_channels_tx(C, Ctx, #{workspace_id => <<"x">>, limit => 10})
    ),
    %% scope=workspace AND status=1 强制：WS_A1 只见 ch-2/ch-1（DESC）
    {ok, Page} = enterprise_channel_logic:list_channels_tx(
        C, Ctx, #{workspace_id => ?WS_A1, limit => 50}
    ),
    ?assertEqual(
        [993401, 993402],
        [maps:get(<<"channel_id">>, I) || I <- maps:get(<<"items">>, Page)]
    ),
    %% personal 频道（993406）不在任何 W 列表；跨 W 查询不见 WS_A2 行以外的域
    {ok, Ws2} = enterprise_channel_logic:list_channels_tx(
        C, Ctx, #{workspace_id => ?WS_A2, limit => 50}
    ),
    ?assertEqual(
        [993405],
        [maps:get(<<"channel_id">>, I) || I <- maps:get(<<"items">>, Ws2)]
    ),
    %% 翻页 oracle（limit=1）
    {Ids, Pages} = walk(C, Ctx, channels, 1, undefined, [], 0),
    ?assertEqual([993401, 993402], Ids),
    ?assertEqual(2, Pages).

%%%===================================================================
%%% INT-31
%%%===================================================================

int31(C, State) ->
    Ctx = ctx_of(C, State, app_a),
    {ok, Row} = enterprise_channel_logic:locate_tx(C, Ctx, 993401),
    ?assertEqual(ok, enterprise_internal_boundary:enforce(C, Ctx, <<"INT-31">>, ?WS_A1)),
    {ok, Detail} = enterprise_channel_logic:detail_tx(C, Ctx, Row),
    ?assertEqual(
        [
            <<"channel_id">>,
            <<"created_at">>,
            <<"description">>,
            <<"name">>,
            <<"subscriber_count">>,
            <<"workspace_id">>
        ],
        lists:sort(maps:keys(Detail))
    ),
    %% personal / 停用 / 删除 / 跨 Org / 不存在 → 一律 404 同体
    lists:foreach(
        fun(Id) ->
            ?assertMatch(
                {error, {<<"resource_not_found">>, _}},
                enterprise_channel_logic:locate_tx(C, Ctx, Id)
            )
        end,
        [993406, 993403, 993404, 999999999]
    ).

%%%===================================================================
%%% 投影冻结 + usage
%%%===================================================================

projection_frozen(C, State) ->
    Ctx = ctx_of(C, State, app_a),
    %% 投影冻结 oracle：五 family 行键集恰等于冻结清单（封闭投影）。
    %% A-R usage 计数面：ck_eau_metric DB CHECK 封闭枚举未含 read metric，
    %% 扩展需 A0 分配新迁移号（A2 RESULT deviations 登记）——本期 A-R 事件面
    %% 由 handler 层结构化访问日志承担。
    {ok, WP} = enterprise_workspace_logic:list_workspaces_tx(C, Ctx, #{}),
    lists:foreach(
        fun(I) ->
            ?assertEqual(
                [<<"created_at">>, <<"name">>, <<"owner_id">>, <<"workspace_id">>],
                lists:sort(maps:keys(I))
            )
        end,
        maps:get(<<"items">>, WP)
    ),
    {ok, GP} = enterprise_group_logic:list_groups_tx(C, Ctx, #{}),
    lists:foreach(
        fun(I) ->
            ?assertEqual(
                [
                    <<"created_at">>,
                    <<"group_id">>,
                    <<"member_count">>,
                    <<"title">>,
                    <<"workspace_id">>
                ],
                lists:sort(maps:keys(I))
            )
        end,
        maps:get(<<"items">>, GP)
    ),
    {ok, _} = enterprise_group_logic:list_members_tx(C, Ctx, 993501, #{}),
    {ok, PP} = enterprise_project_logic:list_projects_tx(
        C, Ctx, #{workspace_id => ?WS_A1}
    ),
    lists:foreach(
        fun(I) ->
            ?assertEqual(
                [<<"created_at">>, <<"name">>, <<"owner_id">>, <<"project_id">>, <<"status">>],
                lists:sort(maps:keys(I))
            )
        end,
        maps:get(<<"items">>, PP)
    ),
    {ok, CP} = enterprise_channel_logic:list_channels_tx(C, Ctx, #{workspace_id => ?WS_A1}),
    lists:foreach(
        fun(I) ->
            ?assertEqual(
                [
                    <<"channel_id">>,
                    <<"created_at">>,
                    <<"name">>,
                    <<"subscriber_count">>,
                    <<"workspace_id">>
                ],
                lists:sort(maps:keys(I))
            )
        end,
        maps:get(<<"items">>, CP)
    ),
    %% 成员行投影恰 3 键
    {ok, MP} = enterprise_group_logic:list_members_tx(C, Ctx, 993501, #{}),
    lists:foreach(
        fun(I) ->
            ?assertEqual([<<"created_at">>, <<"role">>, <<"user_id">>], lists:sort(maps:keys(I)))
        end,
        maps:get(<<"items">>, MP)
    ).

%%%===================================================================
%%% read_page 纯函数
%%%===================================================================

limit_parsing_test() ->
    ?_test(begin
        %% 缺省 50；1..100 合法
        ?assertEqual({ok, 50}, enterprise_internal_read_page:parse_limit(#{})),
        ?assertEqual({ok, 1}, enterprise_internal_read_page:parse_limit(#{<<"limit">> => <<"1">>})),
        ?assertEqual(
            {ok, 100}, enterprise_internal_read_page:parse_limit(#{<<"limit">> => <<"100">>})
        ),
        %% 越界/非整数/负数/浮点/空串：拒绝不截断（§10.1）
        lists:foreach(
            fun(Bad) ->
                ?assertEqual(
                    {error, invalid_request},
                    enterprise_internal_read_page:parse_limit(#{<<"limit">> => Bad})
                )
            end,
            [<<"0">>, <<"101">>, <<"-1">>, <<"abc">>, <<"1.5">>, <<>>, true, <<" 5">>]
        ),
        %% cursor 提取
        ?assertEqual(undefined, enterprise_internal_read_page:parse_cursor(#{})),
        ?assertEqual(
            <<"c">>, enterprise_internal_read_page:parse_cursor(#{<<"cursor">> => <<"c">>})
        ),
        ?assertEqual(undefined, enterprise_internal_read_page:parse_cursor(#{<<"cursor">> => true}))
    end).
