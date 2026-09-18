%% moya_context_logic_tests
%% MFS2-F05（A1-D01）— context/switch 二次查库错误强匹配 → function_clause 崩溃。
%%
%% 覆盖：
%%   错误传播 —— switch 四分支（guardian/teacher/organization/org_owner）的
%%               find_*_context 二次调用 contexts/1,2 时，任一底层 repo 查询
%%               返回 {error, _} → switch 必须返回 {error, db_error}（结构化
%%               业务错误，handler 已有映射分支），**绝不抛 function_clause
%%               崩溃成 cowboy 500**（A1-D13 bundle_aux 同型残留收口）。
%%   快照回显 —— guardian 正常路径回归守卫：resolve_guardian 过 + contexts
%%               命中 → {ok, Ctx}，TSID 字符串契约不变。
%%
%% 全部 meck，零真实库（SQL 正确性由 repo 集成测试验证）。

-module(moya_context_logic_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(UID, 97001).
-define(UID_ORG_OWNER, 97002).
-define(LEARNER_ID, 974001).
-define(LEARNER_TSID, <<"974001">>).
-define(GROUP_ID, 973001).
-define(GROUP_TSID, <<"973001">>).
-define(ORG_ID, 972001).
-define(ORG_TSID, <<"972001">>).

%%%===================================================================
%%% RED：四分支二次查库失败 → {error, db_error}（不崩溃）
%%%===================================================================

%% guardian_contexts 二次查库失败：ACL 首查（guardian_relation）已过，
%% find_guardian_context → contexts/1 重建全量上下文时遇到 DB 错误。
switch_guardian_second_query_db_error_test_() ->
    ?WITH_MECKS(
        repo_mocks(#{
            guardian_relation => {ok, active_guardian_row()},
            guardian_contexts => {error, db_down},
            staff_contexts => {ok, []},
            owner_contexts => {ok, []}
        }),
        fun() ->
            ?assertEqual(
                {error, db_error},
                moya_context_logic:switch(?UID, #{
                    <<"context_type">> => <<"guardian">>,
                    <<"learner_id">> => ?LEARNER_TSID
                })
            )
        end
    ).

%% staff_contexts 二次查库失败（resolve_staff 首查 staff_relation 已过）。
switch_teacher_second_query_db_error_test_() ->
    ?WITH_MECKS(
        repo_mocks(#{
            staff_relation => {ok, active_staff_row()},
            guardian_contexts => {ok, []},
            staff_contexts => {error, db_down},
            owner_contexts => {ok, []}
        }),
        fun() ->
            ?assertEqual(
                {error, db_error},
                moya_context_logic:switch(?UID, #{
                    <<"context_type">> => <<"teacher">>,
                    <<"group_id">> => ?GROUP_TSID
                })
            )
        end
    ).

%% organization_contexts 二次查库失败（contexts/2 organization 形态）。
switch_organization_second_query_db_error_test_() ->
    ?WITH_MECKS(
        repo_mocks(#{
            org_manager => ok,
            guardian_contexts => {ok, []},
            staff_contexts => {ok, []},
            organization_contexts => {error, db_down}
        }),
        fun() ->
            ?assertEqual(
                {error, db_error},
                moya_context_logic:switch(?UID, #{
                    <<"context_type">> => <<"organization">>,
                    <<"organization_id">> => ?ORG_TSID
                })
            )
        end
    ).

%% owner_contexts 二次查库失败（contexts/2 legacy 形态，滚动兼容 org_owner）。
switch_legacy_owner_second_query_db_error_test_() ->
    ?WITH_MECKS(
        repo_mocks(#{
            org_owner_uid => {ok, ?UID_ORG_OWNER},
            guardian_contexts => {ok, []},
            staff_contexts => {ok, []},
            owner_contexts => {error, db_down}
        }),
        fun() ->
            ?assertEqual(
                {error, db_error},
                moya_context_logic:switch(?UID_ORG_OWNER, #{
                    <<"context_type">> => <<"org_owner">>,
                    <<"organization_id">> => ?ORG_TSID
                })
            )
        end
    ).

%%%===================================================================
%%% 回归守卫：guardian 正常路径（重构不改变快照回显契约）
%%%===================================================================

switch_guardian_ok_test_() ->
    ?WITH_MECKS(
        repo_mocks(#{
            guardian_relation => {ok, active_guardian_row()},
            guardian_contexts => {ok, [guardian_repo_row()]},
            staff_contexts => {ok, []},
            owner_contexts => {ok, []}
        }),
        fun() ->
            ?assertMatch(
                {ok, #{
                    <<"context_type">> := <<"guardian">>,
                    <<"learner_id">> := ?LEARNER_TSID
                }},
                moya_context_logic:switch(?UID, #{
                    <<"context_type">> => <<"guardian">>,
                    <<"learner_id">> => ?LEARNER_TSID
                })
            )
        end
    ).

%%%===================================================================
%%% Helpers
%%%===================================================================

%% repo 层 mock 统一组装：键缺省 {ok, []} / {ok, undefined}（deny 但不炸）；
%% org_manager => ok 时额外放行 resolve_org_manager 的 organization_member_repo 查询。
repo_mocks(Config) ->
    RepoFuns = [
        {'guardian_relation', 2, fun(_Uid, _LearnerId) ->
            maps:get(guardian_relation, Config, {ok, undefined})
        end},
        {'guardian_contexts', 1, fun(_Uid) ->
            maps:get(guardian_contexts, Config, {ok, []})
        end},
        {'staff_relation', 2, fun(_Uid, _GroupId) ->
            maps:get(staff_relation, Config, {ok, undefined})
        end},
        {'staff_contexts', 1, fun(_Uid) ->
            maps:get(staff_contexts, Config, {ok, []})
        end},
        {'organization_contexts', 1, fun(_Uid) ->
            maps:get(organization_contexts, Config, {ok, []})
        end},
        {'owner_contexts', 1, fun(_Uid) ->
            maps:get(owner_contexts, Config, {ok, []})
        end},
        {'org_owner_uid', 1, fun(_OrgId) ->
            maps:get(org_owner_uid, Config, {ok, undefined})
        end}
    ],
    Mocks = [{moya_context_repo, RepoFuns}],
    case maps:get(org_manager, Config, undefined) of
        undefined ->
            Mocks;
        ok ->
            [
                {organization_member_repo, [
                    {'find_active', 3, fun(_OrgId, _Uid, _Col) ->
                        {ok, #{<<"role">> => <<"owner">>}}
                    end}
                ]}
                | Mocks
            ]
    end.

active_guardian_row() ->
    #{
        <<"guardian_uid">> => ?UID,
        <<"learner_id">> => ?LEARNER_ID,
        <<"relation">> => <<"mother">>,
        <<"can_submit">> => true,
        <<"can_view_review">> => true,
        <<"status">> => <<"active">>,
        <<"learner_status">> => <<"active">>
    }.

active_staff_row() ->
    #{
        <<"group_id">> => ?GROUP_ID,
        <<"user_id">> => ?UID,
        <<"role">> => <<"teacher">>,
        <<"status">> => <<"active">>
    }.

%% guardian_context/1 必填键：learner_id（其余键缺省 null/<<>>）
guardian_repo_row() ->
    #{
        <<"learner_id">> => ?LEARNER_ID,
        <<"can_submit">> => true,
        <<"can_view_review">> => true,
        <<"display_name">> => <<"小墨"/utf8>>
    }.
