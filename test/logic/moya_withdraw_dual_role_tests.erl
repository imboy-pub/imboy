%% 撤回权限必须识别同一用户兼具 staff + guardian 的场景。
-module(moya_withdraw_dual_role_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(UID, 986001).
-define(SUBMISSION, 986101).
-define(LEARNER, 986201).
-define(ORG, 986301).

scope() ->
    #{
        <<"learner_id">> => ?LEARNER,
        <<"org_id">> => ?ORG
    }.

dual_role_guardian_can_withdraw_test_() ->
    ?WITH_MECKS(
        [
            {moya_acl, [
                {'submission_access', 2, fun(?UID, ?SUBMISSION) -> {ok, staff, scope()} end},
                {'resolve_guardian', 3, fun(?UID, ?LEARNER, submit) -> {ok, #{}} end}
            ]},
            {moya_context_repo, [
                {'learner_org', 1, fun(?LEARNER) -> {ok, ?ORG} end}
            ]},
            {elib_pg, [
                {'with_tx', 2, fun(Tx, _Opts) -> Tx(test_conn) end}
            ]},
            {moya_submission_repo, [
                {'withdraw_tx', 3, fun(test_conn, ?SUBMISSION, ?UID) -> {ok, withdrawn} end}
            ]}
        ],
        fun() ->
            ?assertEqual({ok, withdrawn}, moya_review_logic:withdraw(?UID, ?SUBMISSION))
        end
    ).

staff_without_guardian_relation_is_denied_test_() ->
    ?WITH_MECKS(
        [
            {moya_acl, [
                {'submission_access', 2, fun(?UID, ?SUBMISSION) -> {ok, staff, scope()} end},
                {'resolve_guardian', 3, fun(?UID, ?LEARNER, submit) ->
                    {error, not_guardian}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual({error, not_guardian}, moya_review_logic:withdraw(?UID, ?SUBMISSION))
        end
    ).

cross_org_dual_role_is_denied_test_() ->
    ?WITH_MECKS(
        [
            {moya_acl, [
                {'submission_access', 2, fun(?UID, ?SUBMISSION) -> {ok, staff, scope()} end},
                {'resolve_guardian', 3, fun(?UID, ?LEARNER, submit) -> {ok, #{}} end}
            ]},
            {moya_context_repo, [
                {'learner_org', 1, fun(?LEARNER) -> {ok, ?ORG + 1} end}
            ]}
        ],
        fun() ->
            ?assertEqual({error, not_guardian}, moya_review_logic:withdraw(?UID, ?SUBMISSION))
        end
    ).
