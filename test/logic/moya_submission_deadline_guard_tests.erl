%% moya_submission_deadline_guard_tests
%% A1-D01：create_submission 的 deadline 守卫。
%% assignment_scope 注释与 group_task 先例（check_deadline/1：Now > Deadline
%% 严格大于，恰等于未过期）声明「开放 = task.status =/= 3 且 deadline 未过」，
%% 但 moya 路径此前只查 task_status=1——截止后（老师未手动关单）仍可提交。
%% 本文件锁定：
%%   截止后（status=1）        → assignment_closed（5442）
%%   未截止 / 恰等于 / 无截止  → 守卫放行（不误伤既有正常提交）

-module(moya_submission_deadline_guard_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(UID, 987001).
-define(LEARNER, 987101).
-define(ASSIGNMENT, 987201).
-define(ATT_VIDEO, 987301).
-define(GROUP, 987401).

%%%===================================================================
%%% 夹具
%%%===================================================================

scope(TaskStatus, Deadline) ->
    #{
        <<"assignment_id">> => ?ASSIGNMENT,
        <<"learner_id">> => ?LEARNER,
        <<"task_status">> => TaskStatus,
        <<"task_deadline">> => Deadline,
        <<"group_id">> => ?GROUP
    }.

body() ->
    #{
        <<"learner_id">> => integer_to_binary(?LEARNER),
        <<"assets">> => [
            #{
                <<"attachment_id">> => integer_to_binary(?ATT_VIDEO),
                <<"kind">> => <<"practice_video">>,
                <<"sort_order">> => 0
            }
        ]
    }.

%% 守卫链全部放行（归属/监护/附件），并在 with_tx 处短路——
%% 到达事务即证明「deadline 守卫放行」（事务体非本测试对象）。
guards_mocks(TaskStatus, Deadline) ->
    [
        {moya_context_repo, [
            {'assignment_scope', 1, fun(?ASSIGNMENT) ->
                {ok, scope(TaskStatus, Deadline)}
            end}
        ]},
        {moya_acl, [
            {'resolve_guardian', 3, fun(?UID, ?LEARNER, submit) -> {ok, #{}} end}
        ]},
        {moya_submission_repo, [
            {'validate_assets', 2, fun(?UID, _Assets) ->
                {ok, [#{<<"id">> => ?ATT_VIDEO}]}
            end}
        ]},
        {elib_pg, [
            {'with_tx', 2, fun(_Tx, _Opts) -> {rollback, not_found} end}
        ]}
    ].

past_deadline() ->
    elib_dt:to_rfc3339(elib_dt:millisecond() - 3600_000).

future_deadline() ->
    elib_dt:to_rfc3339(elib_dt:millisecond() + 3600_000).

%%%===================================================================
%%% A1-D01 主断言：截止后创建提交必须被拒（assignment_closed → 5442）
%%%===================================================================

deadline_passed_submission_rejected_test_() ->
    ?WITH_MECKS(
        guards_mocks(1, past_deadline()),
        fun() ->
            ?assertEqual(
                {error, assignment_closed},
                moya_assignment_logic:create_submission(
                    ?UID, ?ASSIGNMENT, <<"idem-key-01">>, body()
                )
            )
        end
    ).

%% 已关闭（status/=1）+ 已截止：仍拒绝（关门状态不因 deadline 语义变化）
task_closed_submission_rejected_test_() ->
    ?WITH_MECKS(
        guards_mocks(3, future_deadline()),
        fun() ->
            ?assertEqual(
                {error, assignment_closed},
                moya_assignment_logic:create_submission(
                    ?UID, ?ASSIGNMENT, <<"idem-key-05">>, body()
                )
            )
        end
    ).

%%%===================================================================
%%% 回归：不误伤
%%%===================================================================

%% 未截止照常放行（守卫不得把正常提交拦死）
deadline_future_submission_passes_guards_test_() ->
    ?WITH_MECKS(
        guards_mocks(1, future_deadline()),
        fun() ->
            ?assertEqual(
                {error, not_found},
                moya_assignment_logic:create_submission(
                    ?UID, ?ASSIGNMENT, <<"idem-key-02">>, body()
                )
            )
        end
    ).

%% 边界：恰等于 deadline 时刻未过期——跟随 group_task check_deadline 严格大于先例
%%（用 rfc3339 往返值钉死「now == deadline」，不受字符串精度截断影响）
deadline_exact_now_still_open_test_() ->
    Deadline = elib_dt:to_rfc3339(elib_dt:millisecond()),
    DeadlineMs = elib_dt:rfc3339_to(Deadline),
    ?WITH_MECKS(
        [
            {elib_dt, [
                {'millisecond', 0, fun() -> DeadlineMs end}
            ]}
            | guards_mocks(1, Deadline)
        ],
        fun() ->
            ?assertEqual(
                {error, not_found},
                moya_assignment_logic:create_submission(
                    ?UID, ?ASSIGNMENT, <<"idem-key-03">>, body()
                )
            )
        end
    ).

%% deadline 缺失（null）不视为过期——group_task check_deadline(undefined) 同语义
deadline_null_submission_passes_guards_test_() ->
    ?WITH_MECKS(
        guards_mocks(1, null),
        fun() ->
            ?assertEqual(
                {error, not_found},
                moya_assignment_logic:create_submission(
                    ?UID, ?ASSIGNMENT, <<"idem-key-04">>, body()
                )
            )
        end
    ).
