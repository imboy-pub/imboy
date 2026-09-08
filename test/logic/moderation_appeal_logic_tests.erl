-module(moderation_appeal_logic_tests).

%% R-04 申诉链测试。覆盖计划 Tests 六条：
%% eligible/ineligible、deadline、reviewer conflict、reversal、
%% notification（decision notice 可达性=用户可自助查询）、
%% reporter identity privacy（出参无 case_id/reporter/reviewer_id）。

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(ACTION_ID, 501).
-define(APPELLANT, 77).
-define(ACTOR, 1001).
-define(REVIEWER, 2002).

%% ── fixtures ──────────────────────────────────────────────────────

action_row(Status) ->
    #{
        <<"id">> => ?ACTION_ID,
        <<"case_id">> => 9,
        <<"action">> => <<"group_mute">>,
        <<"target_uid">> => ?APPELLANT,
        <<"actor_id">> => ?ACTOR,
        <<"status">> => Status,
        <<"reason">> => <<"群内违规行为"/utf8>>,
        <<"reversed_at">> => null,
        <<"created_at">> => elib_dt:to_rfc3339(elib_dt:millisecond(), millisecond)
    }.

appeal_row(Status) ->
    #{
        <<"id">> => 801,
        <<"action_id">> => ?ACTION_ID,
        <<"case_id">> => 9,
        <<"appellant_uid">> => ?APPELLANT,
        <<"reason">> => <<"处罚过重"/utf8>>,
        <<"status">> => Status,
        <<"reviewer_id">> => ?REVIEWER,
        <<"review_reason">> => <<>>,
        <<"reviewed_at">> => null,
        <<"created_at">> => elib_dt:to_rfc3339(elib_dt:millisecond(), millisecond)
    }.

%% 门开 + 动作仓库桩（find_by_id 返回 Status 态的 executed 行）
open_gate_and_action(Status) ->
    [
        {imboy_feature, [
            {'enabled', 1, fun
                (appeal) -> true;
                (_) -> false
            end}
        ]},
        {moderation_action_repo, [
            {'find_by_id', 1, fun(?ACTION_ID) -> {ok, action_row(Status)} end}
        ]},
        {config_ds, [
            {'get', 2, fun(_K, Default) -> Default end}
        ]}
    ].

appeal_repo_open() ->
    {moderation_appeal_repo, [
        {'find_by_action_appellant', 2, fun(_A, _U) -> {error, not_found} end},
        {'insert', 1, fun(#{action_id := A, appellant_uid := U}) ->
            {ok, (appeal_row(<<"pending">>))#{<<"action_id">> => A, <<"appellant_uid">> => U}}
        end},
        {'list_by_appellant', 1, fun(_U) -> {ok, [appeal_row(<<"pending">>)]} end},
        {'find_by_id', 1, fun(801) -> {ok, appeal_row(<<"pending">>)} end},
        {'mark_reviewed', 5, fun(_Id, Verdict, _R, _Reason, _Ts) -> {ok, Verdict} end}
    ]}.

%% ── eligible / ineligible ────────────────────────────────────────

submit_happy_path_test_() ->
    ?WITH_MECKS(
        open_gate_and_action(<<"executed">>) ++ [appeal_repo_open()],
        fun() ->
            {ok, View} = moderation_appeal_logic:submit(
                ?APPELLANT, ?ACTION_ID, <<"处罚过重"/utf8>>
            ),
            ?assertEqual(<<"pending">>, maps:get(<<"status">>, View)),
            ?assertEqual(1, meck:num_calls(moderation_appeal_repo, insert, 1))
        end
    ).

submit_rejects_non_target_user_test_() ->
    ?WITH_MECKS(
        open_gate_and_action(<<"executed">>) ++ [appeal_repo_open()],
        fun() ->
            {error, Msg} = moderation_appeal_logic:submit(
                999, ?ACTION_ID, <<"不是我的处罚"/utf8>>
            ),
            ?assertNotEqual(nomatch, binary:match(Msg, <<"针对自己"/utf8>>)),
            ?assertEqual(0, meck:num_calls(moderation_appeal_repo, insert, 1))
        end
    ).

submit_rejects_non_executed_action_test_() ->
    ?WITH_MECKS(
        open_gate_and_action(<<"failed">>) ++ [appeal_repo_open()],
        fun() ->
            {error, Msg} = moderation_appeal_logic:submit(
                ?APPELLANT, ?ACTION_ID, <<"理由"/utf8>>
            ),
            ?assertNotEqual(nomatch, binary:match(Msg, <<"不可申诉"/utf8>>))
        end
    ).

submit_rejects_duplicate_appeal_test_() ->
    ?WITH_MECKS(
        [
            {imboy_feature, [
                {'enabled', 1, fun
                    (appeal) -> true;
                    (_) -> false
                end}
            ]},
            {moderation_action_repo, [
                {'find_by_id', 1, fun(?ACTION_ID) -> {ok, action_row(<<"executed">>)} end}
            ]},
            {moderation_appeal_repo, [
                {'find_by_action_appellant', 2, fun(_A, _U) ->
                    {ok, appeal_row(<<"pending">>)}
                end}
            ]},
            {config_ds, [
                {'get', 2, fun(_K, Default) -> Default end}
            ]}
        ],
        fun() ->
            {error, Msg} = moderation_appeal_logic:submit(
                ?APPELLANT, ?ACTION_ID, <<"重复申诉"/utf8>>
            ),
            ?assertNotEqual(nomatch, binary:match(Msg, <<"已对该处置"/utf8>>)),
            ?assertEqual(0, meck:num_calls(moderation_appeal_repo, insert, 1))
        end
    ).

%% ── deadline ─────────────────────────────────────────────────────

submit_rejects_expired_action_test_() ->
    ExpiredAction = (action_row(<<"executed">>))#{
        <<"created_at">> => elib_dt:to_rfc3339(
            elib_dt:millisecond() - 40 * 24 * 3600 * 1000, millisecond
        )
    },
    ?WITH_MECKS(
        [
            {imboy_feature, [
                {'enabled', 1, fun
                    (appeal) -> true;
                    (_) -> false
                end}
            ]},
            {moderation_action_repo, [
                {'find_by_id', 1, fun(?ACTION_ID) -> {ok, ExpiredAction} end}
            ]},
            {config_ds, [
                {'get', 2, fun(_K, Default) -> Default end}
            ]},
            {moderation_appeal_repo, [
                {'find_by_action_appellant', 2, fun(_A, _U) -> {error, not_found} end}
            ]}
        ],
        fun() ->
            {error, Msg} = moderation_appeal_logic:submit(
                ?APPELLANT, ?ACTION_ID, <<"超期申诉"/utf8>>
            ),
            ?assertNotEqual(nomatch, binary:match(Msg, <<"超过申诉期限"/utf8>>)),
            ?assertEqual(0, meck:num_calls(moderation_appeal_repo, insert, 1))
        end
    ).

%% ── feature 门（availability by policy profile）─────────────────

submit_blocked_when_appeal_feature_disabled_test_() ->
    ?WITH_MECKS(
        [
            {imboy_feature, [
                {'enabled', 1, fun
                    (appeal) -> false;
                    (_) -> true
                end}
            ]},
            {moderation_action_repo, [
                {'find_by_id', 1, fun(_) -> erlang:error(should_not_lookup) end}
            ]}
        ],
        fun() ->
            {error, Msg} = moderation_appeal_logic:submit(
                ?APPELLANT, ?ACTION_ID, <<"理由"/utf8>>
            ),
            ?assertNotEqual(nomatch, binary:match(Msg, <<"未开放"/utf8>>))
        end
    ).

%% ── reviewer conflict（independent reviewer）────────────────────

review_rejects_original_actor_test_() ->
    ?WITH_MECKS(
        [
            {moderation_appeal_repo, [
                {'find_by_id', 1, fun(801) -> {ok, appeal_row(<<"pending">>)} end},
                {'mark_reviewed', 5, fun(_I, _V, _R, _RS, _T) -> {ok, 1} end}
            ]},
            {moderation_action_repo, [
                {'find_by_id', 1, fun(?ACTION_ID) -> {ok, action_row(<<"executed">>)} end}
            ]}
        ],
        fun() ->
            %% 原执行者 ACTOR 试图复审自己执行的处置
            {error, Msg} = moderation_appeal_logic:review(
                ?ACTOR, 801, <<"accept">>, <<"通过"/utf8>>
            ),
            ?assertNotEqual(nomatch, binary:match(Msg, <<"原处置执行者"/utf8>>)),
            ?assertEqual(0, meck:num_calls(moderation_appeal_repo, mark_reviewed, 5))
        end
    ).

%% ── reversal（accept 联动撤销；reject 不动）────────────────────

review_accept_reverses_action_test_() ->
    ?WITH_MECKS(
        [
            {moderation_appeal_repo, [
                {'find_by_id', 1, fun(801) -> {ok, appeal_row(<<"pending">>)} end},
                {'mark_reviewed', 5, fun(_I, _V, _R, _RS, _T) -> {ok, 1} end}
            ]},
            {moderation_action_repo, [
                {'find_by_id', 1, fun(?ACTION_ID) -> {ok, action_row(<<"executed">>)} end}
            ]},
            {moderation_action_logic, [
                {'reverse', 3, fun(?REVIEWER, ?ACTION_ID, Reason) ->
                    ?assertNotEqual(nomatch, binary:match(Reason, <<"申诉翻案"/utf8>>)),
                    {ok, #{<<"id">> => ?ACTION_ID}}
                end}
            ]}
        ],
        fun() ->
            {ok, _} = moderation_appeal_logic:review(
                ?REVIEWER, 801, <<"accept">>, <<"处罚确属过重"/utf8>>
            ),
            ?assertEqual(1, meck:num_calls(moderation_appeal_repo, mark_reviewed, 5)),
            ?assertEqual(1, meck:num_calls(moderation_action_logic, reverse, 3))
        end
    ).

review_reject_keeps_action_test_() ->
    ?WITH_MECKS(
        [
            {moderation_appeal_repo, [
                {'find_by_id', 1, fun(801) -> {ok, appeal_row(<<"pending">>)} end},
                {'mark_reviewed', 5, fun(_I, _V, _R, _RS, _T) -> {ok, 1} end}
            ]},
            {moderation_action_repo, [
                {'find_by_id', 1, fun(?ACTION_ID) -> {ok, action_row(<<"executed">>)} end}
            ]},
            {moderation_action_logic, [
                {'reverse', 3, fun(_R, _A, _RS) -> erlang:error(should_not_reverse) end}
            ]}
        ],
        fun() ->
            {ok, _} = moderation_appeal_logic:review(
                ?REVIEWER, 801, <<"reject">>, <<"维持原处置"/utf8>>
            ),
            ?assertEqual(0, meck:num_calls(moderation_action_logic, reverse, 3))
        end
    ).

%% ── notification（decision notice 可达性）+ reporter privacy ────

my_list_view_reaches_user_and_hides_reporter_test_() ->
    ?WITH_MECKS(
        [
            {moderation_appeal_repo, [
                {'list_by_appellant', 1, fun(?APPELLANT) ->
                    {ok, [appeal_row(<<"accepted">>)]}
                end}
            ]},
            {moderation_action_repo, [
                {'find_by_id', 1, fun(?ACTION_ID) -> {ok, action_row(<<"executed">>)} end}
            ]}
        ],
        fun() ->
            {ok, [View]} = moderation_appeal_logic:my_list(?APPELLANT),
            %% 终审结果对用户可达（decision notice 可达性）
            ?assertEqual(<<"accepted">>, maps:get(<<"status">>, View)),
            %% reporter identity privacy：用户面无 case_id/举报人/复审人
            ?assertEqual(false, maps:is_key(<<"case_id">>, View)),
            ?assertEqual(false, maps:is_key(<<"reporter_uid">>, View)),
            ?assertEqual(false, maps:is_key(<<"reviewer_id">>, View)),
            ActionView = maps:get(<<"action">>, View),
            ?assertEqual(false, maps:is_key(<<"case_id">>, ActionView)),
            ?assertEqual(false, maps:is_key(<<"reporter_uid">>, ActionView))
        end
    ).
