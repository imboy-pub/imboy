-module(role_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% role_logic 模块的 EUnit 测试
%%%
%%% 目标：验证角色业务逻辑功能
%%% 覆盖：CRUD 操作、权限分配、缓存管理
%%%===================================================================

%% ===================================================================
%% list/0 测试
%% ===================================================================

list_returns_all_roles_test_() ->
    ?WITH_MECK(role_repo, [
        {'all', 0, fun() ->
            {ok, [
                #{<<"id">> => 1, <<"role_key">> => <<"super_admin">>},
                #{<<"id">> => 2, <<"role_key">> => <<"admin">>}
            ]}
        end}
    ], fun() ->
        Result = role_logic:list(),
        ?ASSERT_OK(Result),
        {ok, List} = Result,
        ?assert(length(List) >= 2)
    end).

%% ===================================================================
%% list/2 测试
%% ===================================================================

list_with_pagination_test_() ->
    ?WITH_MECK(role_repo, [
        {'page', 2, fun(_Page, _Size) ->
            {ok, #{
                <<"list">> => [
                    #{<<"id">> => 1, <<"role_key">> => <<"super_admin">>},
                    #{<<"id">> => 2, <<"role_key">> => <<"admin">>}
                ],
                <<"total">> => 5
            }}
        end}
    ], fun() ->
        Page = 1,
        Size = 10,
        Result = role_logic:list(Page, Size),
        ?ASSERT_OK(Result),
        {ok, Data} = Result,
        ?assert(maps:is_key(<<"list">>, Data)),
        ?assert(maps:is_key(<<"total">>, Data)),
        ?assertEqual(5, maps:get(<<"total">>, Data))
    end).

%% ===================================================================
%% get/1 测试
%% ===================================================================

get_existing_role_test_() ->
    ?WITH_MECK(role_repo, [
        {'find_by_id', 1, fun(_Id) ->
            #{<<"id">> => 2, <<"role_key">> => <<"admin">>, <<"role_name">> => <<"系统管理员"/utf8>>}
        end}
    ], fun() ->
        Id = 2,
        Result = role_logic:get(Id),
        ?ASSERT_OK(Result),
        {ok, Role} = Result,
        ?assertEqual(2, maps:get(<<"id">>, Role))
    end).

get_not_existing_role_test_() ->
    ?WITH_MECK(role_repo, [
        {'find_by_id', 1, fun(_Id) ->
            #{}
        end}
    ], fun() ->
        Id = 999999,
        Result = role_logic:get(Id),
        ?assertEqual({error, not_found}, Result)
    end).

%% ===================================================================
%% save/1 测试
%% ===================================================================

save_insert_new_role_test_() ->
    ?WITH_MECK(role_repo, [
        {'insert', 1, fun(_Data) ->
            {ok, 10}
        end}
    ], fun() ->
        Data = #{
            <<"id">> => 0,
            <<"role_name">> => <<"测试角色"/utf8>>,
            <<"role_key">> => <<"test_role">>,
            <<"description">> => <<"这是一个测试角色"/utf8>>,
            <<"status">> => 1
        },
        Result = role_logic:save(Data),
        ?ASSERT_OK(Result),
        {ok, SavedData} = Result,
        ?assert(maps:is_key(<<"id">>, SavedData)),
        ?assert(maps:get(<<"id">>, SavedData) > 0)
    end).

save_update_existing_role_test_() ->
    ?WITH_MECK(role_repo, [
        {'update', 2, fun(_Id, _Data) ->
            {ok, 1}
        end}
    ], fun() ->
        Data = #{
            <<"id">> => 2,
            <<"role_name">> => <<"更新后的角色"/utf8>>,
            <<"role_key">> => <<"admin">>,
            <<"status">> => 1
        },
        Result = role_logic:save(Data),
        ?ASSERT_OK(Result),
        {ok, SavedData} = Result,
        ?assertEqual(2, maps:get(<<"id">>, SavedData))
    end).

%% ===================================================================
%% delete/1 测试
%% ===================================================================

delete_existing_role_test_() ->
    ?WITH_MECK(role_repo, [
        {'delete', 1, fun(_Id) ->
            {ok, 1}
        end}
    ], fun() ->
        Id = 5,
        Result = role_logic:delete(Id),
        ?assertEqual({ok, 1}, Result)
    end).

delete_role_in_use_test_() ->
    % Mock check_role_in_use 返回 true
    ?WITH_MECK(role_repo, [
        {'delete', 1, fun(_Id) ->
            {error, role_in_use}
        end}
    ], fun() ->
        Id = 2,
        Result = role_logic:delete(Id),
        ?assertEqual({error, role_in_use}, Result)
    end).

%% ===================================================================
%% get_permissions/1 测试
%% ===================================================================

get_permissions_returns_list_test_() ->
    ?WITH_MECK(role_repo, [
        {'get_permissions', 1, fun(_Id) ->
            {ok, [
                #{<<"id">> => 1, <<"permission_key">> => <<"user:view">>},
                #{<<"id">> => 2, <<"permission_key">> => <<"user:create">>}
            ]}
        end}
    ], fun() ->
        RoleId = 2,
        Result = role_logic:get_permissions(RoleId),
        ?ASSERT_OK(Result),
        {ok, Permissions} = Result,
        ?assert(is_list(Permissions)),
        ?assert(length(Permissions) >= 2)
    end).

%% ===================================================================
%% assign_permissions/2 测试
%% ===================================================================

assign_permissions_success_test_() ->
    ?WITH_MECK(role_repo, [
        {'assign_permissions', 2, fun(_RoleId, _PermissionIds) ->
            ok
        end}
    ], fun() ->
        RoleId = 2,
        PermissionIds = [1, 2, 3, 4, 5],
        Result = role_logic:assign_permissions(RoleId, PermissionIds),
        ?assertEqual(ok, Result)
    end).

assign_permissions_empty_list_test_() ->
    ?WITH_MECK(role_repo, [
        {'assign_permissions', 2, fun(_RoleId, _PermissionIds) ->
            ok
        end}
    ], fun() ->
        RoleId = 2,
        PermissionIds = [],
        Result = role_logic:assign_permissions(RoleId, PermissionIds),
        ?assertEqual(ok, Result)
    end).
