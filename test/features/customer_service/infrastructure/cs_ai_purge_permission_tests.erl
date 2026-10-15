%%% @doc P0-7（v4 03-F）：AI trace purge 三层防御真库套件。
%%%
%%% 覆盖 F-1..F-6（04 §P0-7.8）：
%%%   * F-1  普通角色裸 DELETE ⇒ 42501（L1 权限层）；
%%%   * F-2  SET LOCAL GUC 后 DELETE ⇒ 仍 42501（F5 对治：GUC 非权限边界）；
%%%   * F-3a 非 principal 角色调 cs_ai_purge_trace ⇒ 42501；
%%%   * F-3b SET ROLE cs_ai_purge 后调用 ⇒ 仍 42501（session_user 未变，核心）；
%%%   * F-4  合法 principal ⇒ 删除生效且审计行 +1（ai.trace_purged）；
%%%   * F-5  有 UPDATE 权限时触发器 ⇒ 23514（L2）；
%%%   * F-6  超管绕过路径显式可见（K1：记录，不"修"）。
%%%
%%% 进入条件（K1b，硬）：purge 连接使用独立非超级凭据 `cs_ai_purge`
%%% （迁移 180 建角色，口令仅本地探针库由 setup 注入）。
-module(cs_ai_purge_permission_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FIX, cs_pg_test_fixture).
-define(PURGE_PASS, <<"csai_purge_probe">>).
-define(PROBE_PASS, <<"probe_ops_probe">>).

cs_ai_purge_permission_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun cases/1}.

setup() ->
    case eunit_runner:eunit_setup_with_db() of
        {ok, Conn} ->
            Scope = seed_scope(),
            ok = setup_roles(),
            Baseline = audit_count(maps:get(org, Scope)),
            {ok, Conn, State} = {ok, Conn, #{scope => Scope, baseline => Baseline}},
            {ok, Conn, State};
        {error, Reason} ->
            {error, Reason}
    end.

cleanup({ok, Conn, #{scope := Scope}}) ->
    cleanup_scope(Scope),
    eunit_runner:eunit_cleanup_db(Conn);
cleanup(Other) ->
    Other.

cases({ok, _Conn, #{scope := Scope, baseline := Baseline}}) ->
    [
        {timeout, 60, fun() -> f1_l1_blocks_delete(Scope) end},
        {timeout, 60, fun() -> f2_guc_not_a_boundary(Scope) end},
        {timeout, 60, fun() -> f3a_call_without_principal(Scope) end},
        {timeout, 60, fun() -> f3b_set_role_not_forgery(Scope) end},
        {timeout, 60, fun() -> f4_authorized_purge_audited(Scope, Baseline) end},
        {timeout, 60, fun() -> f5_update_always_rejected(Scope) end},
        {timeout, 60, fun() -> f6_superuser_bypass_visible(Scope) end}
    ];
cases({error, Reason}) ->
    erlang:error({csai_f_group_suite_db_unavailable, Reason}).

%% ===================================================================
%% F-1：L1 权限层——cs_ai_purge 对 turn 无 DELETE ⇒ 42501
%% ===================================================================

f1_l1_blocks_delete(Scope) ->
    #{org := Org, session_id := SessionId} = Scope,
    seed_turn(Org, SessionId, 1),
    {ok, C} = purge_conn(),
    R = epgsql:squery(C, delete_sql(Org)),
    epgsql:close(C),
    ?assertMatch({error, {error, _, <<"42501">>, _, _, _}}, R),
    1 = turn_count_for(Org, 1).

%% ===================================================================
%% F-2：GUC 不是权限边界（F5 对治）——SET LOCAL 后仍 42501
%% ===================================================================

f2_guc_not_a_boundary(Scope) ->
    #{org := Org, session_id := SessionId} = Scope,
    seed_turn(Org, SessionId, 2),
    {ok, C} = purge_conn(),
    {ok, _, _} = epgsql:squery(C, <<"BEGIN">>),
    {ok, [], []} = epgsql:squery(C, <<"SET LOCAL imboy.ai_trace_purge = 'on'">>),
    R = epgsql:squery(C, delete_sql(Org)),
    {ok, _, _} = epgsql:squery(C, <<"ROLLBACK">>),
    epgsql:close(C),
    ?assertMatch({error, {error, _, <<"42501">>, _, _, _}}, R),
    1 = turn_count_for(Org, 2).

%% ===================================================================
%% F-3a：非 principal 角色调函数 ⇒ 42501（purge_not_authorized）
%% ===================================================================

f3a_call_without_principal(Scope) ->
    #{org := Org} = Scope,
    {ok, C} = probe_conn(),
    R = epgsql:equery(C, call_sql(), [
        Org, now_ts(), <<"probe">>, <<"req-f3a">>, elib_tsid:generate()
    ]),
    epgsql:close(C),
    ?assertMatch({error, {error, _, <<"42501">>, _, <<"purge_not_authorized">>, _}}, R).

%% ===================================================================
%% F-3b（核心）：SET ROLE cs_ai_purge 后调用 ⇒ 仍 42501
%% （SET ROLE 只改 current_user，session_user 不可伪造）
%% ===================================================================

f3b_set_role_not_forgery(Scope) ->
    #{org := Org} = Scope,
    {ok, C} = probe_conn(),
    {ok, [], []} = epgsql:squery(C, <<"SET ROLE cs_ai_purge">>),
    R = epgsql:equery(C, call_sql(), [
        Org, now_ts(), <<"probe">>, <<"req-f3b">>, elib_tsid:generate()
    ]),
    {ok, [], []} = epgsql:squery(C, <<"RESET ROLE">>),
    epgsql:close(C),
    ?assertMatch({error, {error, _, <<"42501">>, _, <<"purge_not_authorized">>, _}}, R).

%% ===================================================================
%% F-4：合法 principal ⇒ 删除生效且审计行 +1（worker 同路径）
%% ===================================================================

f4_authorized_purge_audited(Scope, Baseline) ->
    #{org := Org, session_id := SessionId} = Scope,
    seed_turn(Org, SessionId, 4),
    %% purge 是 org 级（before_ts 之前的全部 turn）：前置行数含 f1/f2 留存行。
    Expected = turn_count(Org),
    OldConfig = application:get_env(imboy, cs_ai_purge),
    {ok, PgConf} = application:get_env(imboy, pg_conf),
    {_M, _F, [Opts]} = maps:get(start_mfa, PgConf),
    application:set_env(
        imboy,
        cs_ai_purge,
        Opts#{username => <<"cs_ai_purge">>, password => ?PURGE_PASS}
    ),
    try
        {ok, Expected} = cs_ai_purge_worker:purge(
            Org, now_ts(), <<"retention-sweep">>, <<"req-f4">>
        )
    after
        case OldConfig of
            {ok, Value} -> application:set_env(imboy, cs_ai_purge, Value);
            undefined -> application:unset_env(imboy, cs_ai_purge)
        end
    end,
    0 = turn_count(Org),
    ?assertEqual(Baseline + 1, audit_count(Org)).

%% ===================================================================
%% F-5：L2 触发器层——有 UPDATE 权限时仍 23514
%% ===================================================================

f5_update_always_rejected(Scope) ->
    #{org := Org, session_id := SessionId} = Scope,
    seed_turn(Org, SessionId, 5),
    %% ACL 管理走专用超管直连（池连接的 GRANT 生效性不可靠——真红实证），
    %% 并显式断言授权可见后再做负例。
    {ok, S} = super_conn(),
    %% F-5 前提（03-F F3b）：有 UPDATE 权限时 UPDATE 应达 L2 触发器 ⇒ 23514。
    %% 注意 UPDATE 的 WHERE 读列还要求 SELECT（aclchk 先于触发器执行）。
    {ok, [], []} = epgsql:squery(
        S, <<"GRANT SELECT, UPDATE ON customer_service_ai_turn TO cs_ai_purge">>
    ),
    1 =
        ?FIX:scalar(
            <<
                "SELECT count(*) FROM information_schema.role_table_grants"
                " WHERE table_name='customer_service_ai_turn'"
                "   AND grantee='cs_ai_purge' AND privilege_type='UPDATE'"
            >>,
            []
        ),
    {ok, C} = purge_conn(),
    %% 同连接自证：身份与表级 UPDATE 权限（排嫁接库/search_path 分歧）。
    {ok, _, [{Who, HasUpd}]} =
        epgsql:equery(
            C,
            <<
                "SELECT current_user::text,"
                "       has_table_privilege('cs_ai_purge',"
                " 'customer_service_ai_turn', 'UPDATE')"
            >>,
            []
        ),
    ?assertEqual(<<"cs_ai_purge">>, Who),
    ?assertEqual(true, HasUpd),
    R =
        epgsql:squery(
            C,
            <<
                "UPDATE customer_service_ai_turn SET intent='x'"
                " WHERE organization_id=",
                (integer_to_binary(Org))/binary,
                " AND seq=5"
            >>
        ),
    epgsql:close(C),
    {ok, [], []} = epgsql:squery(
        S, <<"REVOKE SELECT, UPDATE ON customer_service_ai_turn FROM cs_ai_purge">>
    ),
    epgsql:close(S),
    ?assertMatch({error, {error, _, <<"23514">>, _, _, _}}, R),
    1 = turn_count_for(Org, 5).

%% ===================================================================
%% F-6：K1 超管绕过路径显式可见（记录，不"修"）
%% ===================================================================

f6_superuser_bypass_visible(Scope) ->
    #{org := Org, session_id := SessionId} = Scope,
    seed_turn(Org, SessionId, 6),
    ok = elib_pg:with_tx(fun(Conn) ->
        {ok, [], _} =
            epgsql:squery(Conn, <<"SET LOCAL imboy.ai_trace_purge = 'on'">>),
        {ok, _} = elib_pg:execute(Conn, delete_sql(Org), []),
        ok
    end),
    0 = turn_count(Org).

%% ===================================================================
%% 辅助
%% ===================================================================

seed_scope() ->
    Scope = ?FIX:new_scope(),
    SessionId = ?FIX:id(),
    Identity = maps:get(service_identity_id, Scope),
    ok = ?FIX:exec(
        <<
            "INSERT INTO customer_service_session"
            " (id, organization_id, workspace_id, contact_id, conversation_id,"
            "  business_identity_id, status)"
            " VALUES ($1,$2,$3,$4,$5,$6,'active')"
        >>,
        [
            SessionId,
            maps:get(org_id, Scope),
            maps:get(workspace_id, Scope),
            maps:get(contact_id, Scope),
            maps:get(conversation_id, Scope),
            Identity
        ]
    ),
    #{
        scope => Scope,
        org => maps:get(org_id, Scope),
        session_id => SessionId
    }.

%% turn 有 FK ⇒ 会话行先行；seq 由各用例自配（PK org+session+seq）。
seed_turn(Org, SessionId, Seq) ->
    ok = ?FIX:exec(
        <<
            "INSERT INTO customer_service_ai_turn"
            " (organization_id, session_id, seq, direction)"
            " VALUES ($1,$2,$3,'inbound')"
        >>,
        [Org, SessionId, Seq]
    ),
    1 = turn_count_for(Org, Seq).

setup_roles() ->
    %% cs_ai_purge 由迁移 180 创建（LOGIN NOSUPERUSER）；本地探针口令在此
    %% 注入（生产凭据走 IMBOY_* / 环境管理）。probe_ops 用于 F-3b 的
    %% SET ROLE 伪造尝试（对 cs_ai_purge 有 MEMBERSHIP 才能 SET ROLE）。
    ok = ?FIX:exec(
        <<
            "DO $$ BEGIN"
            "  IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname='cs_ai_probe_ops') THEN"
            "    CREATE ROLE cs_ai_probe_ops LOGIN PASSWORD 'probe_ops_probe';"
            "  END IF;"
            " END $$;"
        >>,
        []
    ),
    ok = ?FIX:exec(<<"ALTER ROLE cs_ai_purge WITH PASSWORD 'csai_purge_probe'">>, []),
    ok = ?FIX:exec(<<"GRANT cs_ai_purge TO cs_ai_probe_ops">>, []),
    ok.

%% 连接参数取运行时 config（eunit 下已被 relay sed 指向 15432），
%% 仅覆盖用户/口令——保证 P0-11 验收库（cs_ai_v4_accept）可复用。
purge_conn() ->
    probe_conn(<<"cs_ai_purge">>, ?PURGE_PASS).

probe_conn() ->
    probe_conn(<<"cs_ai_probe_ops">>, ?PROBE_PASS).

probe_conn(User, Pass) ->
    {ok, PgConf} = application:get_env(imboy, pg_conf),
    {_M, _F, [Opts]} = maps:get(start_mfa, PgConf),
    epgsql:connect(Opts#{username => User, password => Pass}).

%% ACL/角色管理专用：运行时配置的超级应用凭据直连（绕过池）。
super_conn() ->
    {ok, PgConf} = application:get_env(imboy, pg_conf),
    {_M, _F, [Opts]} = maps:get(start_mfa, PgConf),
    epgsql:connect(Opts).

delete_sql(Org) ->
    <<"DELETE FROM customer_service_ai_turn WHERE organization_id=",
        (integer_to_binary(Org))/binary>>.

call_sql() ->
    <<"SELECT cs_ai_purge_trace($1, $2, $3, $4, $5)">>.

now_ts() ->
    elib_dt:to_rfc3339(os:system_time(millisecond)).

turn_count(Org) ->
    ?FIX:scalar(
        <<"SELECT count(*) FROM customer_service_ai_turn WHERE organization_id=$1">>,
        [Org]
    ).

turn_count_for(Org, Seq) ->
    ?FIX:scalar(
        <<
            "SELECT count(*) FROM customer_service_ai_turn"
            " WHERE organization_id=$1 AND seq=$2"
        >>,
        [Org, Seq]
    ).

audit_count(Org) ->
    ?FIX:scalar(
        <<
            "SELECT count(*) FROM customer_service_event"
            " WHERE organization_id=$1 AND action='ai.trace_purged'"
        >>,
        [Org]
    ).

%% cleanup 需删 turn（触发器要求 GUC）+ 会话，再走 fixture 清理。
cleanup_scope(#{scope := Scope, org := Org, session_id := SessionId}) ->
    ok = elib_pg:with_tx(fun(Conn) ->
        {ok, [], _} = epgsql:squery(Conn, <<"SET LOCAL imboy.ai_trace_purge = 'on'">>),
        {ok, _} = elib_pg:execute(Conn, delete_sql(Org), []),
        ok
    end),
    _ = ?FIX:exec(
        <<"DELETE FROM customer_service_session WHERE organization_id=$1 AND id=$2">>,
        [Org, SessionId]
    ),
    ?FIX:cleanup(Scope),
    ok.
