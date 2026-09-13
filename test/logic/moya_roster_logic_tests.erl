%% moya_roster_logic_tests
%% MN-ROSTER-01 — 只读班级学员名单业务逻辑测试（角色矩阵 + 机构守卫 + 三分支组装）。
%%
%% 覆盖：
%%   角色矩阵 —— manager✓ teacher✓ assistant✓（读路径 Need=undefined，
%%               不带 write 白名单）；非 staff✗ removed staff✗（均折叠
%%               class_not_visible，不泄漏班级存在性）
%%   机构守卫 —— group 无机构（workspace 未挂 org）→ cross_org fail closed
%%   传播     —— ds/repo db_error → db_error
%%   三分支   —— submit_guardians 计数 1→ready+null / 0→no_submit_guardian /
%%               ≥2→multiple_submit_guardians；ready 时 setup_reason 必须 null
%%   DTO     —— group_id/learner_id TSID string；display_name 透传；
%%               排序稳定（按 learner_id）
%%   隐私     —— 响应深度遍历断言无 guardian_uid/openid/relation/birth_year 等键
%%
%% 全部 meck，零真实库（SQL 计数正确性由 repo 集成测试验证）。

-module(moya_roster_logic_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(UID_MANAGER, 960005).
-define(UID_TEACHER, 960001).
-define(UID_ASSIST, 960003).
-define(UID_OUTSIDER, 960099).
-define(GROUP_A1, 963201).
-define(ORG_A, 963101).
-define(LEARNER_OK, 964001).
-define(LEARNER_MULTI, 964002).
-define(LEARNER_NOG, 964003).

%%%===================================================================
%%% Mock 基建
%%%===================================================================

%% Role 模拟 moya_acl:resolve_staff/3：读路径必须以 Need=undefined 调用
%% （manager/teacher/assistant 三角色均可读；不带 write 白名单）
acl_mocks(Role) ->
    StaffResult =
        case Role of
            manager -> {ok, #{<<"role">> => <<"manager">>}};
            teacher -> {ok, #{<<"role">> => <<"teacher">>}};
            assistant -> {ok, #{<<"role">> => <<"assistant">>}};
            not_staff -> {error, not_staff};
            inactive -> {error, inactive}
        end,
    [
        {moya_acl, [
            {'resolve_staff', 3, fun(_Uid, _GroupId, Need) ->
                self() ! {acl_need, Need},
                StaffResult
            end}
        ]},
        {moya_context_repo, [
            {'group_org', 1, fun(_GroupId) -> {ok, ?ORG_A} end}
        ]}
    ].

%% ds 行形态：#{<<"learner_id">> => int, <<"display_name">> => bin,
%%             <<"submit_guardians">> => int}
ds_mock(Rows) ->
    {moya_roster_ds, [
        {'list', 2, fun(GroupId, OrgId) ->
            self() ! {ds_list, GroupId, OrgId},
            {ok, Rows}
        end}
    ]}.

ds_error_mock(Reason) ->
    {moya_roster_ds, [
        {'list', 2, fun(_GroupId, _OrgId) -> {error, Reason} end}
    ]}.

three_branch_rows() ->
    [
        #{
            <<"learner_id">> => ?LEARNER_OK,
            <<"display_name">> => <<"L-OK">>,
            <<"submit_guardians">> => 1
        },
        #{
            <<"learner_id">> => ?LEARNER_NOG,
            <<"display_name">> => <<"L-NOG">>,
            <<"submit_guardians">> => 0
        },
        #{
            <<"learner_id">> => ?LEARNER_MULTI,
            <<"display_name">> => <<"L-MULTI">>,
            <<"submit_guardians">> => 2
        }
    ].

%%%===================================================================
%%% 深度遍历守卫：响应永不携带监护人/身份字段
%%%===================================================================

-define(FORBIDDEN_KEYS, [
    <<"guardian_uid">>,
    <<"guardian_id">>,
    <<"guardian">>,
    <<"guardians">>,
    <<"guardian_learner">>,
    <<"openid">>,
    <<"open_id">>,
    <<"relation">>,
    <<"birth_year">>,
    <<"contact">>,
    <<"phone">>,
    <<"wechat">>
]).

assert_no_guardian_keys(Term) ->
    walk(Term).

walk(Map) when is_map(Map) ->
    lists:foreach(
        fun({K, V}) ->
            ?assertNot(lists:member(K, ?FORBIDDEN_KEYS), {forbidden_key, K}),
            walk(V)
        end,
        maps:to_list(Map)
    );
walk(L) when is_list(L) ->
    lists:foreach(fun walk/1, L);
walk(_) ->
    ok.

%%%===================================================================
%%% 角色矩阵（deny-by-default）
%%%===================================================================

list_manager_allowed_test_() ->
    ?WITH_MECKS(acl_mocks(manager) ++ [ds_mock(three_branch_rows())], fun() ->
        {ok, Payload} = moya_roster_logic:list(?UID_MANAGER, ?GROUP_A1),
        ?assertEqual(3, length(maps:get(<<"learners">>, Payload))),
        %% 读路径不带 write 白名单（三角色均可读）
        receive
            {acl_need, undefined} -> ok
        after 0 -> ?assert(false, "resolve_staff must be called with Need=undefined")
        end
    end).

list_teacher_allowed_test_() ->
    ?WITH_MECKS(acl_mocks(teacher) ++ [ds_mock(three_branch_rows())], fun() ->
        {ok, _} = moya_roster_logic:list(?UID_TEACHER, ?GROUP_A1)
    end).

list_assistant_allowed_test_() ->
    ?WITH_MECKS(acl_mocks(assistant) ++ [ds_mock(three_branch_rows())], fun() ->
        %% assistant 只读可读：不带 write 语义下 resolve_staff 返回 ok
        {ok, Payload} = moya_roster_logic:list(?UID_ASSIST, ?GROUP_A1),
        ?assertEqual(integer_to_binary(?GROUP_A1), maps:get(<<"group_id">>, Payload))
    end).

list_not_staff_denied_test_() ->
    Ds = ds_mock(three_branch_rows()),
    ?WITH_MECKS(acl_mocks(not_staff) ++ [Ds], fun() ->
        %% 非 staff：折叠 class_not_visible（不泄漏班级存在性）
        ?assertEqual(
            {error, class_not_visible},
            moya_roster_logic:list(?UID_OUTSIDER, ?GROUP_A1)
        ),
        receive
            {ds_list, _, _} -> ?assert(false, "non-staff must not reach ds")
        after 0 -> ok
        end
    end).

list_removed_staff_denied_test_() ->
    Ds = ds_mock(three_branch_rows()),
    ?WITH_MECKS(acl_mocks(inactive) ++ [Ds], fun() ->
        %% removed staff（status != active）：同样 class_not_visible
        ?assertEqual(
            {error, class_not_visible},
            moya_roster_logic:list(?UID_TEACHER, ?GROUP_A1)
        ),
        receive
            {ds_list, _, _} -> ?assert(false, "removed staff must not reach ds")
        after 0 -> ok
        end
    end).

%%%===================================================================
%%% 机构守卫 + 错误传播
%%%===================================================================

list_group_without_org_rejected_test_() ->
    StaffMock =
        {moya_acl, [
            {'resolve_staff', 3, fun(_Uid, _GroupId, _Need) ->
                {ok, #{<<"role">> => <<"teacher">>}}
            end}
        ]},
    CtxMock =
        {moya_context_repo, [
            {'group_org', 1, fun(_GroupId) -> {ok, undefined} end}
        ]},
    Ds = ds_mock(three_branch_rows()),
    ?WITH_MECKS([StaffMock, CtxMock, Ds], fun() ->
        %% 班级机构解析 NULL → fail closed（T1/T2 跨机构防御）
        ?assertEqual(
            {error, cross_org},
            moya_roster_logic:list(?UID_TEACHER, ?GROUP_A1)
        ),
        receive
            {ds_list, _, _} -> ?assert(false, "org-less group must not reach ds")
        after 0 -> ok
        end
    end).

list_group_org_db_error_test_() ->
    StaffMock =
        {moya_acl, [
            {'resolve_staff', 3, fun(_Uid, _GroupId, _Need) ->
                {ok, #{<<"role">> => <<"teacher">>}}
            end}
        ]},
    CtxMock =
        {moya_context_repo, [
            {'group_org', 1, fun(_GroupId) -> {error, boom} end}
        ]},
    ?WITH_MECKS([StaffMock, CtxMock], fun() ->
        ?assertEqual(
            {error, db_error},
            moya_roster_logic:list(?UID_TEACHER, ?GROUP_A1)
        )
    end).

list_staff_db_error_test_() ->
    StaffMock =
        {moya_acl, [
            {'resolve_staff', 3, fun(_Uid, _GroupId, _Need) ->
                {error, db_error}
            end}
        ]},
    ?WITH_MECKS([StaffMock], fun() ->
        ?assertEqual(
            {error, db_error},
            moya_roster_logic:list(?UID_TEACHER, ?GROUP_A1)
        )
    end).

list_ds_db_error_propagates_test_() ->
    ?WITH_MECKS(
        acl_mocks(manager) ++ [ds_error_mock({error, boom})],
        fun() ->
            ?assertEqual(
                {error, db_error},
                moya_roster_logic:list(?UID_MANAGER, ?GROUP_A1)
            )
        end
    ).

%%%===================================================================
%%% 三分支组装（0/1/多 active can_submit 监护人）+ DTO 契约
%%%===================================================================

list_three_branch_reasons_test_() ->
    ?WITH_MECKS(acl_mocks(manager) ++ [ds_mock(three_branch_rows())], fun() ->
        {ok, Payload} = moya_roster_logic:list(?UID_MANAGER, ?GROUP_A1),
        ?assertEqual(integer_to_binary(?GROUP_A1), maps:get(<<"group_id">>, Payload)),
        Learners = maps:get(<<"learners">>, Payload),
        ?assertEqual(3, length(Learners)),
        %% ds 收到 {GroupId, OrgId}（机构过滤参数）
        receive
            {ds_list, ?GROUP_A1, ?ORG_A} -> ok
        after 0 -> ?assert(false, "ds args wrong")
        end,
        %% 三分支映射（按 learner_id 断言）
        ById = maps:from_list([{maps:get(<<"learner_id">>, L), L} || L <- Learners]),
        Ok = maps:get(<<"964001">>, ById),
        ?assertEqual(true, maps:get(<<"assignment_ready">>, Ok)),
        ?assertEqual(null, maps:get(<<"setup_reason">>, Ok)),
        Nog = maps:get(<<"964003">>, ById),
        ?assertEqual(false, maps:get(<<"assignment_ready">>, Nog)),
        ?assertEqual(<<"no_submit_guardian">>, maps:get(<<"setup_reason">>, Nog)),
        Multi = maps:get(<<"964002">>, ById),
        ?assertEqual(false, maps:get(<<"assignment_ready">>, Multi)),
        ?assertEqual(
            <<"multiple_submit_guardians">>, maps:get(<<"setup_reason">>, Multi)
        ),
        %% P0-2：响应永不携带 guardian UID/openid/关系/出生年份/联系方式
        assert_no_guardian_keys(Payload)
    end).

list_many_guardians_same_reason_test_() ->
    Rows = [
        #{
            <<"learner_id">> => 964009,
            <<"display_name">> => <<"L-5G">>,
            <<"submit_guardians">> => 5
        }
    ],
    ?WITH_MECKS(acl_mocks(teacher) ++ [ds_mock(Rows)], fun() ->
        {ok, Payload} = moya_roster_logic:list(?UID_TEACHER, ?GROUP_A1),
        [L] = maps:get(<<"learners">>, Payload),
        ?assertEqual(false, maps:get(<<"assignment_ready">>, L)),
        ?assertEqual(<<"multiple_submit_guardians">>, maps:get(<<"setup_reason">>, L))
    end).

list_display_name_passthrough_test_() ->
    Rows = [
        #{
            <<"learner_id">> => ?LEARNER_OK,
            <<"display_name">> => <<"小明同学"/utf8>>,
            <<"submit_guardians">> => 1
        }
    ],
    ?WITH_MECKS(acl_mocks(teacher) ++ [ds_mock(Rows)], fun() ->
        {ok, Payload} = moya_roster_logic:list(?UID_TEACHER, ?GROUP_A1),
        [L] = maps:get(<<"learners">>, Payload),
        ?assertEqual(<<"小明同学"/utf8>>, maps:get(<<"display_name">>, L)),
        ?assertEqual(integer_to_binary(?LEARNER_OK), maps:get(<<"learner_id">>, L))
    end).

list_empty_roster_test_() ->
    ?WITH_MECKS(acl_mocks(teacher) ++ [ds_mock([])], fun() ->
        {ok, Payload} = moya_roster_logic:list(?UID_TEACHER, ?GROUP_A1),
        ?assertEqual([], maps:get(<<"learners">>, Payload)),
        assert_no_guardian_keys(Payload)
    end).

list_rows_sorted_by_learner_id_test_() ->
    %% ds 行乱序返回 → DTO 按 learner_id 升序稳定输出
    Rows = lists:reverse(three_branch_rows()),
    ?WITH_MECKS(acl_mocks(manager) ++ [ds_mock(Rows)], fun() ->
        {ok, Payload} = moya_roster_logic:list(?UID_MANAGER, ?GROUP_A1),
        Ids = [maps:get(<<"learner_id">>, L) || L <- maps:get(<<"learners">>, Payload)],
        ?assertEqual([<<"964001">>, <<"964002">>, <<"964003">>], Ids)
    end).
