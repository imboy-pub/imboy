%% teaching_roster_repo_integration_tests
%% MN-ROSTER-01 — 只读班级学员名单仓库层真库集成测试。
%%
%% 直连 scratch 库 moya_zcode_181902@127.0.0.1:4323（迁移 1→103 +
%% review_asset + group_task 幂等列全量态 162 表），每用例 BEGIN ... ROLLBACK，
%% 不留数据。测试直接驱动 teaching_roster_ds:list 与 teaching_roster_repo
%% 的查询函数（与生产 elib_pg 同一代码路径），验证：
%%   ① active enrollment 过滤：removed enrollment 不出现
%%   ② 跨班过滤：他班 learner 不出现
%%   ③ 跨机构过滤：learner.organization_id != 班机构 的 learner 不出现
%%   ④ guardian 计数 SQL：0/1/2+ active can_submit 组合的 submit_guardians
%%      数值正确（can_submit=false 不计、removed guardian 不计）
%%   ⑤ display_name 回读
%%   ⑥ 空班（无 active enrollment）→ 空列表
%% DB 不可达时自动 skip。

-module(teaching_roster_repo_integration_tests).

-include_lib("eunit/include/eunit.hrl").

%% ---- 夹具（96 段独立 ID，与 97/98/99 段互不冲突） ----
-define(PG_HOST, "127.0.0.1").
-define(PG_PORT, 4323).
-define(PG_USER, <<"imboy_user">>).
-define(PG_PASS, <<"abc54321">>).
-define(PG_DB, <<"moya_zcode_181902">>).

-define(TEACHER_A1, 960001).
-define(TEACHER_B2, 960002).
-define(ASSIST_A1, 960003).
-define(MANAGER_A1, 960005).
-define(PARENT_1, 960011).
-define(PARENT_2, 960012).
-define(PARENT_3, 960013).
-define(PARENT_4, 960014).
-define(PARENT_5, 960015).
-define(OUTSIDER, 960099).

-define(ORG_A, 963101).
-define(ORG_X, 963199).
-define(WS_A, 963111).
-define(WS_B, 963112).
-define(WS_X, 963191).
-define(GROUP_A1, 963201).
-define(GROUP_B2, 963202).

-define(LEARNER_OK, 964001).
-define(LEARNER_MULTI, 964002).
-define(LEARNER_NOG, 964003).
-define(LEARNER_REMOVED, 964004).
-define(LEARNER_B2, 964005).
-define(LEARNER_XORG, 964099).

%%%===================================================================
%%% Fixture
%%%===================================================================

setup_conn() ->
    try
        {ok, _} = application:ensure_all_started(epgsql),
        try
            elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3})
        catch
            _:_ -> ok
        end,
        {ok, C} = epgsql:connect(#{
            host => ?PG_HOST,
            port => ?PG_PORT,
            username => ?PG_USER,
            password => ?PG_PASS,
            database => ?PG_DB,
            timeout => 5000,
            %% 与生产 pg_conf 同款 timestamptz codec（RFC3339 binary）
            codecs => [{epgsql_codec_rfc3339_bin, []}]
        }),
        C
    catch
        _:_ -> skip
    end.

close_conn(skip) ->
    ok;
close_conn(C) ->
    try
        epgsql:close(C)
    catch
        _:_ -> ok
    end,
    ok.

with_tx(TestFun) ->
    {setup, fun setup_conn/0, fun close_conn/1, fun
        (skip) ->
            [];
        (C) ->
            ?_test(begin
                ok = exec(C, <<"BEGIN">>),
                try
                    TestFun(C),
                    ok
                after
                    exec(C, <<"ROLLBACK">>)
                end
            end)
    end}.

exec(C, Sql) ->
    exec(C, Sql, []).

exec(C, Sql, Params) ->
    case elib_pg:query(C, Sql, Params) of
        {ok, _} -> ok;
        {error, Reason} -> erlang:error({sql_error, Reason})
    end.

q(C, Sql, Params) ->
    case elib_pg:query(C, Sql, Params) of
        {ok, Rows} -> Rows;
        {error, Reason} -> erlang:error({sql_error, Reason})
    end.

%%%===================================================================
%%% Seed（与 teaching_task_repo_integration_tests 同构，96 段独立夹具）
%%%===================================================================

seed(C) ->
    %% 用户（teacher/assistant/manager/parents/outsider）
    Uids = [
        {?TEACHER_A1, <<"t96_teacher_a1">>},
        {?TEACHER_B2, <<"t96_teacher_b2">>},
        {?ASSIST_A1, <<"t96_assist_a1">>},
        {?MANAGER_A1, <<"t96_manager_a1">>},
        {?PARENT_1, <<"t96_parent_1">>},
        {?PARENT_2, <<"t96_parent_2">>},
        {?PARENT_3, <<"t96_parent_3">>},
        {?PARENT_4, <<"t96_parent_4">>},
        {?PARENT_5, <<"t96_parent_5">>},
        {?OUTSIDER, <<"t96_outsider">>}
    ],
    lists:foreach(
        fun({Uid, Account}) ->
            exec(
                C,
                <<
                    "INSERT INTO \"user\" (id, password, account, reg_ip, reg_cosv) "
                    "VALUES ($1, 'x', $2, '127.0.0.1', 'x')"
                >>,
                [Uid, Account]
            )
        end,
        Uids
    ),
    %% 机构 / 工作区（X 机构用于跨机构 learner）
    exec(C, <<"INSERT INTO organization (id, name, owner_id) VALUES ($1, $2, $3)">>, [
        ?ORG_A, <<"ORG-96-A"/utf8>>, ?MANAGER_A1
    ]),
    exec(C, <<"INSERT INTO organization (id, name, owner_id) VALUES ($1, $2, $3)">>, [
        ?ORG_X, <<"ORG-96-X"/utf8>>, ?MANAGER_A1
    ]),
    exec(
        C,
        <<
            "INSERT INTO workspace (id, name, owner_id, organization_id) VALUES "
            "($1, $2, $3, $4), ($5, $6, $3, $4), ($7, $8, $3, $9)"
        >>,
        [
            ?WS_A,
            <<"WS-A"/utf8>>,
            ?MANAGER_A1,
            ?ORG_A,
            ?WS_B,
            <<"WS-B"/utf8>>,
            ?WS_X,
            <<"WS-X"/utf8>>,
            ?ORG_X
        ]
    ),
    lists:foreach(
        fun(WsId) ->
            exec(
                C,
                <<
                    "INSERT INTO workspace_member (workspace_id, user_id, role, invited_by, status) "
                    "VALUES ($1, $2, 'owner', $2, 'active')"
                >>,
                [WsId, ?MANAGER_A1]
            )
        end,
        [?WS_A, ?WS_B, ?WS_X]
    ),
    %% 班级群
    exec(
        C,
        <<
            "INSERT INTO \"group\" (id, owner_uid, creator_uid, scope, workspace_id, title) VALUES "
            "($1, $2, $2, 'workspace', $3, $4), ($5, $2, $2, 'workspace', $6, $7)"
        >>,
        [
            ?GROUP_A1,
            ?MANAGER_A1,
            ?WS_A,
            <<"A1-96"/utf8>>,
            ?GROUP_B2,
            ?WS_B,
            <<"B2-96"/utf8>>
        ]
    ),
    %% staff：A1 teacher/assistant/manager；B2 teacher
    exec(
        C,
        <<
            "INSERT INTO class_staff (group_id, user_id, role) VALUES "
            "($1, $2, 'teacher'), ($1, $3, 'assistant'), ($1, $4, 'manager'), ($5, $6, 'teacher')"
        >>,
        [?GROUP_A1, ?TEACHER_A1, ?ASSIST_A1, ?MANAGER_A1, ?GROUP_B2, ?TEACHER_B2]
    ),
    %% learners（964099 属 X 机构；display_name 各不相同供回读断言）
    exec(
        C,
        <<
            "INSERT INTO learner (id, organization_id, display_name) VALUES "
            "($1, $2, $3), ($4, $2, $5), ($6, $2, $7), ($8, $2, $9), ($10, $2, $11), ($12, $13, $14)"
        >>,
        [
            ?LEARNER_OK,
            ?ORG_A,
            <<"DN-96-OK"/utf8>>,
            ?LEARNER_MULTI,
            <<"DN-96-MULTI"/utf8>>,
            ?LEARNER_NOG,
            <<"DN-96-NOG"/utf8>>,
            ?LEARNER_REMOVED,
            <<"DN-96-REMOVED"/utf8>>,
            ?LEARNER_B2,
            <<"DN-96-B2"/utf8>>,
            ?LEARNER_XORG,
            ?ORG_X,
            <<"DN-96-XORG"/utf8>>
        ]
    ),
    %% enrollment：A1 in {OK, MULTI, NOG, REMOVED(removed)}；B2 in {B2学员}
    exec(
        C,
        <<
            "INSERT INTO class_enrollment (group_id, learner_id, status) VALUES "
            "($1, $2, 'active'), ($1, $3, 'active'), ($1, $4, 'active'), "
            "($1, $5, 'removed'), ($6, $7, 'active')"
        >>,
        [
            ?GROUP_A1,
            ?LEARNER_OK,
            ?LEARNER_MULTI,
            ?LEARNER_NOG,
            ?LEARNER_REMOVED,
            ?GROUP_B2,
            ?LEARNER_B2
        ]
    ),
    %% guardian_learner（计数矩阵）：
    %%   OK       = P1(active, cs=true) + P5(removed, cs=true 不计)      → 1
    %%   MULTI    = P2(active, cs=true) + P3(active, cs=true)            → 2
    %%   NOG      = P4(active, cs=false 不计)                            → 0
    exec(
        C,
        <<
            "INSERT INTO guardian_learner (guardian_uid, learner_id, can_submit, can_view_review, status) VALUES "
            "($1, $2, true, true, 'active'), "
            "($3, $4, true, true, 'removed'), "
            "($5, $6, true, true, 'active'), ($7, $6, true, true, 'active'), "
            "($8, $9, false, true, 'active')"
        >>,
        [
            ?PARENT_1,
            ?LEARNER_OK,
            ?PARENT_5,
            ?LEARNER_OK,
            ?PARENT_2,
            ?LEARNER_MULTI,
            ?PARENT_3,
            ?PARENT_4,
            ?LEARNER_NOG
        ]
    ).

%%%===================================================================
%%% ①②③⑤ 名单过滤 + display_name 回读（走 ds 生产入口）
%%%===================================================================

roster_filters_and_names_test_() ->
    with_tx(fun(C) ->
        seed(C),
        {ok, Rows} = teaching_roster_ds:list_tx(C, ?GROUP_A1, ?ORG_A),
        %% 仅 active enrollment 且同机构 learner：OK/MULTI/NOG
        Ids = [maps:get(<<"learner_id">>, R) || R <- Rows],
        ?assertEqual([?LEARNER_OK, ?LEARNER_MULTI, ?LEARNER_NOG], Ids),
        %% removed enrollment 不出现（①）
        ?assertNot(lists:member(?LEARNER_REMOVED, Ids)),
        %% 跨班 learner 不出现（②）
        ?assertNot(lists:member(?LEARNER_B2, Ids)),
        %% 跨机构 learner 不出现（③）
        ?assertNot(lists:member(?LEARNER_XORG, Ids)),
        %% display_name 回读（⑤）
        ById = maps:from_list([{maps:get(<<"learner_id">>, R), R} || R <- Rows]),
        ?assertEqual(
            <<"DN-96-OK"/utf8>>,
            maps:get(<<"display_name">>, maps:get(?LEARNER_OK, ById))
        ),
        ?assertEqual(
            <<"DN-96-NOG"/utf8>>,
            maps:get(<<"display_name">>, maps:get(?LEARNER_NOG, ById))
        )
    end).

%%%===================================================================
%%% ④ guardian 计数 SQL（0/1/2 + removed 不计 + can_submit=false 不计）
%%%===================================================================

guardian_counts_test_() ->
    with_tx(fun(C) ->
        seed(C),
        {ok, Rows} = teaching_roster_ds:list_tx(C, ?GROUP_A1, ?ORG_A),
        ById = maps:from_list([{maps:get(<<"learner_id">>, R), R} || R <- Rows]),
        %% OK：1 个 active can_submit + 1 个 removed（不计）→ 1
        ?assertEqual(1, maps:get(<<"submit_guardians">>, maps:get(?LEARNER_OK, ById))),
        %% MULTI：2 个 active can_submit → 2
        ?assertEqual(2, maps:get(<<"submit_guardians">>, maps:get(?LEARNER_MULTI, ById))),
        %% NOG：仅 1 个 can_submit=false（不计）→ 0
        ?assertEqual(0, maps:get(<<"submit_guardians">>, maps:get(?LEARNER_NOG, ById)))
    end).

%%%===================================================================
%%% ⑥ 空班 → 空列表
%%%===================================================================

empty_class_test_() ->
    with_tx(fun(C) ->
        seed(C),
        %% GROUP_B2 有 learner；换一个无任何 enrollment 的班不存在于夹具，
        %% 用 B2 班 + ORG_X（B2 班学员均属 ORG_A）模拟空结果：
        {ok, []} = teaching_roster_ds:list_tx(C, ?GROUP_B2, ?ORG_X),
        %% B2 班真实机构下有其学员（对照：不是 SQL 恒空）
        {ok, Rows} = teaching_roster_ds:list_tx(C, ?GROUP_B2, ?ORG_A),
        ?assertEqual([?LEARNER_B2], [maps:get(<<"learner_id">>, R) || R <- Rows])
    end).

%%%===================================================================
%%% 直接驱动 repo（同 SQL 代码路径的另一入口）
%%%===================================================================

repo_class_learners_direct_test_() ->
    with_tx(fun(C) ->
        seed(C),
        {ok, Rows} = teaching_roster_repo:class_learners_tx(C, ?GROUP_A1, ?ORG_A),
        ?assertEqual(3, length(Rows)),
        %% 行契约：learner_id/display_name/submit_guardians，无其他键
        lists:foreach(
            fun(R) ->
                ?assertEqual(
                    [<<"display_name">>, <<"learner_id">>, <<"submit_guardians">>],
                    lists:sort(maps:keys(R))
                )
            end,
            Rows
        )
    end).
