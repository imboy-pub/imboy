-module(adm_acl_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% adm_acl 共享 RBAC 权限模块的 EUnit 测试
%%%
%%% 覆盖：有权限 / 无权限 / 角色为空 / role_id 异常 / 任意权限 /
%%%       find 返回非 map（DB miss / 异常）/ AdmUserId 非法 / 多角色合并
%%%===================================================================

%% mock elib_response:error/3 返回标记元组，便于断言透传的 Req
-define(MOCK_RESP,
    {elib_response, [
        {'error', 3, fun(Req, _Msg, _Code) -> {replied, Req} end}
    ]}
).

%% mock 角色权限表：角色 1 -> [user:view,user:edit]；角色 2 -> [user:edit,stats:view]；其余空
-define(MOCK_ROLE_ACL,
    {adm_index_handler, [
        {'role_acl', 1, fun
            (1) -> {<<"admin">>, [<<"user:view">>, <<"user:edit">>], [<<"/u">>]};
            (2) -> {<<"ops">>, [<<"user:edit">>, <<"stats:view">>], [<<"/s">>]};
            (_) -> {<<"none">>, [], []}
        end}
    ]}
).

%% 构造 find mock：给定 role_id 字段值
mock_find(RoleIdValue) ->
    {adm_user_logic, [
        {'find', 3, fun(_Uid, _Col, _Key) -> #{<<"role_id">> => RoleIdValue} end}
    ]}.

%% ===================================================================
%% ensure_permission/3
%% ===================================================================

ensure_permission_granted_test_() ->
    ?WITH_MECKS([mock_find(1), ?MOCK_ROLE_ACL, ?MOCK_RESP], fun() ->
        State = #{adm_user_id => 100},
        ?assertEqual(ok, adm_acl:ensure_permission(State, <<"user:view">>, req0))
    end).

ensure_permission_denied_test_() ->
    ?WITH_MECKS([mock_find(1), ?MOCK_ROLE_ACL, ?MOCK_RESP], fun() ->
        State = #{adm_user_id => 100},
        ?assertEqual(
            {error, {replied, req0}},
            adm_acl:ensure_permission(State, <<"stats:view">>, req0)
        )
    end).

%% 角色为空（role_id = 0）-> 无任何权限 -> 拒绝
ensure_permission_empty_role_denied_test_() ->
    ?WITH_MECKS([mock_find(0), ?MOCK_ROLE_ACL, ?MOCK_RESP], fun() ->
        State = #{adm_user_id => 100},
        ?assertEqual(
            {error, {replied, req0}},
            adm_acl:ensure_permission(State, <<"user:view">>, req0)
        )
    end).

%% State 缺 adm_user_id（默认 0）-> 守卫 false -> 拒绝
ensure_permission_no_user_denied_test_() ->
    ?WITH_MECKS([?MOCK_RESP], fun() ->
        ?assertEqual(
            {error, {replied, req0}},
            adm_acl:ensure_permission(#{}, <<"user:view">>, req0)
        )
    end).

%% find 抛异常（catch 分支）-> [] -> 拒绝
ensure_permission_find_crash_denied_test_() ->
    ?WITH_MECKS(
        [
            {adm_user_logic, [{'find', 3, fun(_, _, _) -> erlang:error(db_down) end}]},
            ?MOCK_ROLE_ACL,
            ?MOCK_RESP
        ],
        fun() ->
            State = #{adm_user_id => 100},
            ?assertEqual(
                {error, {replied, req0}},
                adm_acl:ensure_permission(State, <<"user:view">>, req0)
            )
        end
    ).

%% find 返回非 map（DB miss）-> [] -> 拒绝
ensure_permission_find_miss_denied_test_() ->
    ?WITH_MECKS(
        [
            {adm_user_logic, [{'find', 3, fun(_, _, _) -> undefined end}]},
            ?MOCK_ROLE_ACL,
            ?MOCK_RESP
        ],
        fun() ->
            State = #{adm_user_id => 100},
            ?assertEqual(
                {error, {replied, req0}},
                adm_acl:ensure_permission(State, <<"user:view">>, req0)
            )
        end
    ).

%% ===================================================================
%% ensure_any_permission/3
%% ===================================================================

ensure_any_permission_hit_test_() ->
    ?WITH_MECKS([mock_find(2), ?MOCK_ROLE_ACL, ?MOCK_RESP], fun() ->
        State = #{adm_user_id => 100},
        ?assertEqual(
            ok,
            adm_acl:ensure_any_permission(State, [<<"nope:x">>, <<"stats:view">>], req0)
        )
    end).

ensure_any_permission_none_test_() ->
    ?WITH_MECKS([mock_find(2), ?MOCK_ROLE_ACL, ?MOCK_RESP], fun() ->
        State = #{adm_user_id => 100},
        ?assertEqual(
            {error, {replied, req0}},
            adm_acl:ensure_any_permission(State, [<<"nope:x">>, <<"nope:y">>], req0)
        )
    end).

%% ===================================================================
%% permissions/1 —— 多角色合并去重
%% ===================================================================

permissions_multi_role_merge_test_() ->
    ?WITH_MECKS([mock_find([1, 2]), ?MOCK_ROLE_ACL], fun() ->
        ?assertEqual(
            [<<"stats:view">>, <<"user:edit">>, <<"user:view">>],
            adm_acl:permissions(100)
        )
    end).

permissions_unknown_role_empty_test_() ->
    ?WITH_MECKS([mock_find(999), ?MOCK_ROLE_ACL], fun() ->
        ?assertEqual([], adm_acl:permissions(100))
    end).

%% ===================================================================
%% normalize_role_ids/1 —— 纯函数，无需 mock
%% ===================================================================

normalize_role_ids_integer_test() ->
    ?assertEqual([5], adm_acl:normalize_role_ids(5)).

normalize_role_ids_zero_test() ->
    ?assertEqual([], adm_acl:normalize_role_ids(0)).

normalize_role_ids_list_mixed_test() ->
    ?assertEqual([1, 2], adm_acl:normalize_role_ids([2, 1, 0, <<"abc">>, 2])).

normalize_role_ids_binary_number_test() ->
    ?assertEqual([3], adm_acl:normalize_role_ids(<<"3">>)).

normalize_role_ids_binary_invalid_test() ->
    ?assertEqual([], adm_acl:normalize_role_ids(<<"abc">>)).

normalize_role_ids_other_test() ->
    ?assertEqual([], adm_acl:normalize_role_ids(undefined)).
