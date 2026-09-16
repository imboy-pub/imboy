%%% @doc EB-08 并发套件：S2/S3 的并发裁决与 DB guard 竞态闭合（真库 + 真并发）。
%%%
%%% 依计划 v4.1 EB-08 的 S2「并发仅一方成功」、S3「最终 DB guard 同点复核」与
%%% `control/required-acceptance.tsv` 的 `EB-08-A03` / `EB-08-A07`。
%%%
%%% A03（并发 execute/finalize 幂等且审计一次）：
%%%   * `execute`：N 个进程持同一 `expected_version` 同时执行 ⇒ **恰一个**推进成功，
%%%     其余拿到 `stale_version` / `case_conflict`；审计恰一次；经办只被换一次。
%%%   * `finalize`：N 个进程同时 finalize 一个已 verify 的 case ⇒ 成员**恰被移除一次**、
%%%     审计恰一次、case 恰推进一次；其余调用的结果只可能是「幂等成功」或
%%%     「可区分的错误」，绝不产生第二次移除或第二条审计。
%%%
%%% A07（DB guard 竞态闭合）：用**两个真实连接**复现「检查与使用之间」的窗口——
%%%   会话 A 读到「无 active 经办」（这是 verify 时刻的视图），会话 B 在该读之后提交
%%%   一条 active 经办，会话 A 再执行无条件 removed ⇒ 必须被数据库守卫在同一语句内
%%%   拒绝；并用「同一事务内禁用触发器后同一条 UPDATE 成功」作为负例，证明拒绝**来自
%%%   该守卫**而不是别的约束（回滚后触发器与数据都复原）。
-module(eb_offboarding_concurrency_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FIX, eb_pg_test_fixture).
-define(APP, eb_offboarding_app).
-define(GUARD, <<"trg_organization_member_offboarding_guard">>).

concurrency_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun cases/1}.

setup() ->
    case eunit_runner:eunit_setup_with_db() of
        {ok, Conn} -> {ok, Conn};
        {error, Reason} -> {error, Reason}
    end.

cleanup({ok, Conn}) ->
    eunit_runner:eunit_cleanup_db(Conn);
cleanup(Other) ->
    Other.

cases({ok, _Conn}) ->
    [
        {timeout, 120, fun a03_concurrent_execute_only_one_wins/0},
        {timeout, 120, fun a03_concurrent_finalize_removes_once_and_audits_once/0},
        {timeout, 120, fun a07_check_then_use_window_is_closed_by_db_guard/0},
        {timeout, 120, fun a07_guard_is_load_bearing_negative_control/0}
    ];
cases(Other) ->
    erlang:error({eb08_concurrency_suite_db_unavailable, Other}).

%% ===================================================================
%% A03：并发 execute
%% ===================================================================

a03_concurrent_execute_only_one_wins() ->
    Scope = ?FIX:new_scope(),
    {Org, Ws} = tenant(Scope),
    Leaver = maps:get(actor_user_id, Scope),
    Successor = maps:get(peer_user_id, Scope),
    Identity = maps:get(sales_identity_id, Scope),
    try
        ok = insert_member(Org, Successor, <<"member">>),
        {ok, Opened} = ?APP:open_offboarding(Org, open_params(Ws, Leaver, Successor, Scope)),
        CaseId = maps:get(case_id, Opened),
        Params = #{
            workspace_id => Ws,
            case_id => CaseId,
            expected_version => maps:get(version, Opened),
            actor_user_id => maps:get(owner_user_id, Scope)
        },
        Results = parallel(4, fun() -> ?APP:execute_offboarding(Org, Params) end),

        Winners = [R || {ok, _} = R <- Results],
        Losers = [R || {error, _} = R <- Results],
        ?assertEqual(1, length(Winners)),
        ?assertEqual(3, length(Losers)),
        [{ok, Executed}] = Winners,
        ?assertEqual(transferring, maps:get(status, Executed)),
        %% 落败方必须是可区分的并发裁决结论（不是笼统的内部错误）
        ?assert(lists:all(fun({error, R}) -> is_concurrency_verdict(R) end, Losers)),

        %% 审计恰一次；经办恰被换一次（旧行 ended + 新行 active，无重复 active）
        ?assertEqual(1, audit_count(Org, <<"offboarding.execute">>)),
        ?assertEqual([Successor], [maps:get(user_id, R) || R <- active_rows(Scope, Identity)]),
        ?assertEqual(1, length(active_rows(Scope, Identity))),
        ?assertEqual(2, length(identity_rows(Scope, Identity))),
        %% case 版本恰推进一次
        {ok, Case} = eb_pg_store:fetch_offboarding_case(Org, Ws, CaseId),
        ?assertEqual(maps:get(version, Opened) + 1, maps:get(version, Case))
    after
        scrub(Scope)
    end.

%% 落败方的三种可区分结论（取决于与胜者的交错位置，三者都表示「这一方没有推进」）：
%%   * stale_version：期望版本已过期（读到了推进后的版本）；
%%   * case_conflict：CAS 在数据库行锁上落败（读到旧版本但 CAS 已被抢走）；
%%   * case_in_transfer：状态门先看到别人已推进（读到的是 transferring）。
is_concurrency_verdict({stale_version, _, _}) -> true;
is_concurrency_verdict({case_conflict, _}) -> true;
is_concurrency_verdict({case_in_transfer, _}) -> true;
is_concurrency_verdict(_Other) -> false.

%% ===================================================================
%% A03：并发 finalize
%% ===================================================================

a03_concurrent_finalize_removes_once_and_audits_once() ->
    Scope = ?FIX:new_scope(),
    {Org, Ws} = tenant(Scope),
    Leaver = maps:get(actor_user_id, Scope),
    Successor = maps:get(peer_user_id, Scope),
    Owner = maps:get(owner_user_id, Scope),
    try
        ok = insert_member(Org, Successor, <<"member">>),
        {ok, CaseId, VerifiedVersion} = open_verify(Scope, Leaver, Successor),
        Params = #{workspace_id => Ws, case_id => CaseId, actor_user_id => Owner},
        Results = parallel(4, fun() -> ?APP:finalize_offboarding(Org, Params) end),

        %% 恰一个「非幂等」的成功（= 真正执行了移除 + 完成 + 审计的那一次）
        Real = [R || {ok, #{idempotent := false}} = R <- Results],
        ?assertEqual(1, length(Real)),
        %% 其余调用只可能是幂等成功或可区分的错误 —— 不得出现第二次「真移除」
        Others = [R || R <- Results, R =/= hd(Real)],
        ?assert(lists:all(fun finalize_loser_outcome/1, Others)),
        %% 实质不变量：成员被移除、case 完成、审计恰一次、版本恰推进一次
        ?assertEqual({ok, removed}, member_status(Org, Leaver)),
        {ok, Done} = eb_pg_store:fetch_offboarding_case(Org, Ws, CaseId),
        ?assertEqual(completed, maps:get(status, Done)),
        ?assertEqual(VerifiedVersion + 1, maps:get(version, Done)),
        ?assertEqual(1, audit_count(Org, <<"offboarding.finalize">>)),
        %% 顺序重放：幂等且不再审计（并发与重放的结论一致）
        {ok, Replay} = ?APP:finalize_offboarding(Org, Params),
        ?assertEqual(true, maps:get(idempotent, Replay)),
        ?assertEqual(1, audit_count(Org, <<"offboarding.finalize">>))
    after
        scrub(Scope)
    end.

finalize_loser_outcome({ok, #{idempotent := true}}) -> true;
finalize_loser_outcome({error, _}) -> true;
finalize_loser_outcome(_Other) -> false.

%% ===================================================================
%% A07：两会话竞态（检查与使用之间的窗口）
%% ===================================================================

a07_check_then_use_window_is_closed_by_db_guard() ->
    Scope = ?FIX:new_scope(),
    {Org, _Ws} = tenant(Scope),
    Leaver = maps:get(actor_user_id, Scope),
    Successor = maps:get(peer_user_id, Scope),
    Service = maps:get(service_identity_id, Scope),
    try
        ok = insert_member(Org, Successor, <<"member">>),
        %% 走到 verify 之后：leaver 已无任何 active 经办（此刻 DB 守卫本来会放行）
        {ok, _CaseId, _Version} = open_verify(Scope, Leaver, Successor),
        ?assertEqual([], leaver_active_rows(Scope, Leaver)),

        Probe = ?FIX:tx(fun(Conn) ->
            %% ① 「检查」（verify 时刻的视图）：本 Org 内 leaver 的 active 经办数
            Count =
                case
                    elib_pg:query(
                        Conn,
                        <<
                            "SELECT count(*) AS n FROM organization_business_identity_assignment"
                            " WHERE organization_id = $1 AND user_id = $2 AND status = 'active'"
                        >>,
                        [Org, Leaver]
                    )
                of
                    {ok, [#{<<"n">> := N}]} -> N;
                    Other -> Other
                end,
            put(t_check_count, Count),
            %% ② 并发会话 B（连接池里的另一条连接）在「检查」与「使用」之间提交一条
            %%    active 经办 —— 这就是窗口内的新事实
            ok = bind_identity_via_pool(Scope, Service, <<"customer_service">>, Leaver),
            %% ③ 「使用」：同一事务内的无条件 removed ⇒ 必须被 DB 守卫在同一语句内拒绝
            throw(
                {probe,
                    elib_pg:execute(
                        Conn,
                        <<
                            "UPDATE organization_member SET status = 'removed'"
                            " WHERE organization_id = $1 AND user_id = $2"
                        >>,
                        [Org, Leaver]
                    )}
            )
        end),
        %% 前置：窗口内的「检查」确实看到 0（否则本用例证明不了竞态）
        ?assertEqual(0, get(t_check_count)),
        %% 窗口被关闭：语句级拒绝（23514），而不是「检查通过就无条件写」
        ?assertMatch({rollback, {probe, {error, _}}}, Probe),
        {rollback, {probe, {error, Err}}} = Probe,
        ?assertEqual(<<"23514">>, error_code(Err)),
        ?assertEqual(?GUARD, error_constraint(Err)),
        %% 且没有任何行被改动
        ?assertEqual({ok, suspended}, member_status(Org, Leaver))
    after
        scrub(Scope)
    end.

bind_identity_via_pool(Scope, Identity, FunctionKey, To) ->
    {Org, Ws} = tenant(Scope),
    ok = end_active_assignment(Scope, Identity),
    {ok, _} = eb_pg_store:insert_assignment(Org, Ws, #{
        id => ?FIX:id(),
        business_identity_id => Identity,
        function_key => FunctionKey,
        user_id => To,
        assigned_by => To
    }),
    ok.

%% ===================================================================
%% A07：负例（有牙齿）—— 拒绝确实来自该触发器
%% ===================================================================

a07_guard_is_load_bearing_negative_control() ->
    Scope = ?FIX:new_scope(),
    {Org, _Ws} = tenant(Scope),
    Leaver = maps:get(actor_user_id, Scope),
    Successor = maps:get(peer_user_id, Scope),
    Service = maps:get(service_identity_id, Scope),
    try
        ok = insert_member(Org, Successor, <<"member">>),
        {ok, _CaseId, _Version} = open_verify(Scope, Leaver, Successor),
        ok = bind_identity_via_pool(Scope, Service, <<"customer_service">>, Leaver),
        %% ① 触发器在线：无条件 removed 被拒
        ?assertMatch(
            {error, _},
            ?FIX:exec(
                <<
                    "UPDATE organization_member SET status = 'removed'"
                    " WHERE organization_id = $1 AND user_id = $2"
                >>,
                [Org, Leaver]
            )
        ),
        %% ② 同一事务内**禁用**该触发器：同一条 UPDATE 成功（1 行）
        %%    ⇒ 「拒绝来自该守卫」这一归因成立；事务回滚后一切复原
        Probe = ?FIX:tx(fun(Conn) ->
            Ddl = <<"ALTER TABLE organization_member DISABLE TRIGGER ", ?GUARD/binary>>,
            1 = ddl_ok(elib_pg:execute(Conn, Ddl, [])),
            throw(
                {probe,
                    elib_pg:execute(
                        Conn,
                        <<
                            "UPDATE organization_member SET status = 'removed'"
                            " WHERE organization_id = $1 AND user_id = $2"
                        >>,
                        [Org, Leaver]
                    )}
            )
        end),
        ?assertEqual({rollback, {probe, {ok, 1}}}, Probe),
        %% ③ 复原：触发器在线、成员仍是 suspended
        ?assertEqual(true, trigger_enabled()),
        ?assertEqual({ok, suspended}, member_status(Org, Leaver))
    after
        scrub(Scope)
    end.

ddl_ok({ok, _}) -> 1;
ddl_ok({ok, _, _}) -> 1;
ddl_ok(Other) -> Other.

trigger_enabled() ->
    1 =:=
        ?FIX:scalar(
            <<"SELECT count(*) FROM pg_trigger WHERE tgname = $1 AND tgenabled = 'O'">>,
            [?GUARD],
            -1
        ).

%% epgsql 的错误项是 `#error{}` 的记录形状（避免在测试里再引一个头文件依赖）。
error_code({error, _Severity, Code, _Codename, _Message, _Extra}) -> Code;
error_code(_Other) -> undefined.

error_constraint({error, _Severity, _Code, _Codename, _Message, Extra}) when is_list(Extra) ->
    case lists:keyfind(constraint_name, 1, Extra) of
        {constraint_name, Name} -> Name;
        _ -> undefined
    end;
error_constraint(_Other) ->
    undefined.

%% ===================================================================
%% 夹具辅助
%% ===================================================================

tenant(Scope) ->
    {maps:get(org_id, Scope), maps:get(workspace_id, Scope)}.

open_params(Ws, Leaver, Successor, Scope) ->
    #{
        workspace_id => Ws,
        leaver_user_id => Leaver,
        successor_user_id => Successor,
        reason => <<"eb08-concurrency-synthetic">>,
        actor_user_id => maps:get(owner_user_id, Scope)
    }.

open_verify(Scope, Leaver, Successor) ->
    {Org, Ws} = tenant(Scope),
    {ok, Opened} = ?APP:open_offboarding(Org, open_params(Ws, Leaver, Successor, Scope)),
    CaseId = maps:get(case_id, Opened),
    {ok, _Executed} = ?APP:execute_offboarding(Org, #{
        workspace_id => Ws,
        case_id => CaseId,
        expected_version => maps:get(version, Opened),
        actor_user_id => maps:get(owner_user_id, Scope)
    }),
    {ok, Verified} = ?APP:verify_offboarding(Org, #{
        workspace_id => Ws,
        case_id => CaseId,
        expected_snapshot_hash => maps:get(snapshot_hash, Opened),
        actor_user_id => maps:get(owner_user_id, Scope)
    }),
    ?assertEqual(verifying, maps:get(status, Verified)),
    {ok, CaseId, maps:get(version, Verified)}.

%% 真并发：全部进程就绪后同时放行（同一时刻持同一 expected_version 冲 CAS）。
parallel(N, Fun) ->
    Parent = self(),
    Go = make_ref(),
    Pids = [
        spawn(fun() ->
            receive
                Go -> ok
            after 30000 -> ok
            end,
            Parent ! {self(), run(Fun)}
        end)
     || _ <- lists:seq(1, N)
    ],
    [Pid ! Go || Pid <- Pids],
    [
        receive
            {Pid, Result} -> Result
        after 60000 -> timeout
        end
     || Pid <- Pids
    ].

run(Fun) ->
    try
        Fun()
    catch
        Class:Reason -> {crashed, Class, Reason}
    end.

insert_member(Org, Uid, Role) ->
    ?FIX:exec(
        <<
            "INSERT INTO organization_member(organization_id,user_id,role,status)"
            " VALUES ($1,$2,$3,'active')"
            " ON CONFLICT (organization_id,user_id) DO UPDATE SET role=EXCLUDED.role,"
            " status='active'"
        >>,
        [Org, Uid, Role]
    ).

end_active_assignment(Scope, Identity) ->
    {Org, Ws} = tenant(Scope),
    case active_rows(Scope, Identity) of
        [] -> ok;
        _ -> eb_pg_store:advance_assignment(Org, Ws, Identity, active, ended)
    end.

identity_rows(Scope, Identity) ->
    {Org, Ws} = tenant(Scope),
    {ok, Rows} = eb_pg_store:list_assignments(Org, Ws),
    [R || R <- Rows, maps:get(business_identity_id, R, undefined) =:= Identity].

active_rows(Scope, Identity) ->
    [R || R <- identity_rows(Scope, Identity), maps:get(status, R, undefined) =:= active].

leaver_active_rows(Scope, UserId) ->
    {Org, Ws} = tenant(Scope),
    {ok, Rows} = eb_pg_store:list_assignments(Org, Ws),
    [
        R
     || R <- Rows,
        maps:get(user_id, R, undefined) =:= UserId,
        maps:get(status, R, undefined) =:= active
    ].

member_status(Org, UserId) ->
    {ok, MemberFact} = eb_infra_ports:resolve(member_fact),
    MemberFact:member_status(Org, UserId).

audit_count(Org, Action) ->
    ?FIX:scalar(
        <<
            "SELECT count(*) FROM enterprise_audit_event"
            " WHERE organization_id=$1 AND action=$2"
        >>,
        [Org, Action],
        -1
    ).

scrub(Scope) ->
    Org = maps:get(org_id, Scope, undefined),
    case is_integer(Org) of
        true ->
            _ = ?FIX:exec(<<"DELETE FROM enterprise_offboarding_item WHERE organization_id=$1">>, [
                Org
            ]),
            _ = ?FIX:exec(<<"DELETE FROM enterprise_offboarding_case WHERE organization_id=$1">>, [
                Org
            ]);
        false ->
            ok
    end,
    _ = ?FIX:cleanup(Scope),
    case is_integer(Org) of
        true ->
            _ = ?FIX:exec(<<"DELETE FROM organization_member WHERE organization_id=$1">>, [Org]);
        false ->
            ok
    end,
    ok.
