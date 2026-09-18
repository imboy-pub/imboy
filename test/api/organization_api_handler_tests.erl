-module(organization_api_handler_tests).

%% ORG-BACKEND-GAP 缺口 2：department 应用层错误 → HTTP 状态的机械映射
%% （map_dept_error/1，经 department_item PATCH 路径 + respond_dept/2 驱动）。
%%
%% 卡片契约锚点：「并发冲突（expected-version）→ 稳定 409」。organization_department_pg
%% 的乐观锁拒绝（CAS UPDATE 影响行数 0）经应用层透传为 {error, conflict}，
%% 此前落入 else 兜底被映射成 400；本套件冻结其必须为 409，并锚定相邻映射
%% （not_found → 404、形状非法 → 400）不回归。
%%
%% ORG-BACKEND-GAP2（ORG-15 G08 旅程实跑抓出）：handler→app 参数形状接线
%% （二进制键 → 原子键归一、list 的 status 缺键语义）与 accept 的
%% membership_hook 注入。见下方 GAP2 测试节。

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(ORG_ID, 301).
-define(DEPT_ID, 302).
-define(UID, 303).

%% CAS 版本冲突必须稳定 409（而非 400 兜底）。
dept_cas_conflict_maps_to_stable_409_test_() ->
    ?WITH_MECKS(
        dept_error_mocks(),
        fun() ->
            put(t_dept_error, conflict),
            Result = organization_api_handler:handle_action(
                department_item, patch_req, #{current_uid => ?UID}
            ),
            ?assertEqual(409, maps:get(response_status, Result)),
            erase(t_dept_error)
        end
    ).

%% 相邻映射不回归：not_found 仍 404。
dept_not_found_still_maps_to_404_test_() ->
    ?WITH_MECKS(
        dept_error_mocks(),
        fun() ->
            put(t_dept_error, not_found),
            Result = organization_api_handler:handle_action(
                department_item, patch_req, #{current_uid => ?UID}
            ),
            ?assertEqual(404, maps:get(response_status, Result)),
            erase(t_dept_error)
        end
    ).

%% 形状非法（invalid_expected_version 属参数形状错误）仍走 400 兜底——
%% 与 conflict 的 409 形成边界：并发写碰撞 ≠ 客户端参数错误。
dept_shape_error_still_maps_to_400_test_() ->
    ?WITH_MECKS(
        dept_error_mocks(),
        fun() ->
            put(t_dept_error, {invalid_expected_version, 3}),
            Result = organization_api_handler:handle_action(
                department_item, patch_req, #{current_uid => ?UID}
            ),
            ?assertEqual(400, maps:get(response_status, Result)),
            erase(t_dept_error)
        end
    ).

%% GAP3：读路径授权门错误（actor_not_member 等）稳定映射 403——
%% 非成员探测部门一律 403，不得伪装成 400 参数错误或借 404 泄露存在性。
dept_gate_error_maps_to_403_test_() ->
    ?WITH_MECKS(
        dept_error_mocks(),
        fun() ->
            put(t_dept_error, {actor_not_member, 9}),
            Result = organization_api_handler:handle_action(
                department_item, patch_req, #{current_uid => ?UID}
            ),
            ?assertEqual(403, maps:get(response_status, Result)),
            erase(t_dept_error)
        end
    ).

%% 非 PATCH/GET 方法仍是 405（映射测试不放松 method 门）。
dept_update_rejects_non_patch_with_405_test_() ->
    ?WITH_MECKS(
        dept_error_mocks(),
        fun() ->
            Result = organization_api_handler:handle_action(
                department_item, put_req, #{current_uid => ?UID}
            ),
            ?assertEqual(405, maps:get(response_status, Result)),
            ?assertEqual(<<"GET, PATCH">>, maps:get(allow, Result))
        end
    ).

%% ===================================================================
%% ORG-BACKEND-GAP2：handler→app 参数形状接线（G08 旅程实跑抓出的两个缺陷）
%%
%% DEFECT-2：elib_param:post 产物是二进制键，部门 handler 曾把 Body 原样
%% 透传给原子键 app 层 → create/rename/move/add 全部落 400；
%% list 曾显式传 status => undefined → invalid_status → 400。
%% DEFECT-1：invitation_accept 曾传空 Opts，membership_hook 永不注入 →
%% accept 消费成功但不建成员行（C11 链路断）。
%% 本节冻结修复后的接线形状。
%% ===================================================================

%% create：二进制键 body 必须被归一为原子键，parent_id 二进制 → 整数。
dept_create_converts_binary_keys_to_atom_keys_test_() ->
    ?WITH_MECKS(
        dept_create_mocks(
            #{<<"name">> => <<"研发部"/utf8>>, <<"parent_id">> => <<"205">>}
        ),
        fun() ->
            organization_api_handler:handle_action(
                department_collection, post_req, #{current_uid => ?UID}
            ),
            receive
                {create_department, ?ORG_ID, Params} ->
                    ?assertEqual(<<"研发部"/utf8>>, maps:get(name, Params)),
                    ?assertEqual(205, maps:get(parent_id, Params)),
                    ?assertEqual(?UID, maps:get(actor_user_id, Params)),
                    ?assertNot(maps:is_key(<<"name">>, Params))
            after 1000 -> erlang:error(no_create_department_call)
            end
        end
    ).

%% create：parent_id 显式 JSON null / 缺省都归一为 null（建根部门语义）。
dept_create_parent_null_passthrough_test_() ->
    ?WITH_MECKS(
        dept_create_mocks(#{<<"name">> => <<"d">>, <<"parent_id">> => null}),
        fun() ->
            organization_api_handler:handle_action(
                department_collection, post_req, #{current_uid => ?UID}
            ),
            receive
                {create_department, ?ORG_ID, Params} ->
                    ?assertEqual(null, maps:get(parent_id, Params))
            after 1000 -> erlang:error(no_create_department_call)
            end
        end
    ).

%% move：parent_id 与 expected_version 都要从二进制键归一为整数（CAS 关键）。
dept_move_converts_parent_and_version_test_() ->
    ?WITH_MECKS(
        dept_move_mocks(#{<<"parent_id">> => <<"206">>, <<"expected_version">> => <<"7">>}),
        fun() ->
            organization_api_handler:handle_action(
                department_move, post_req, #{current_uid => ?UID}
            ),
            receive
                {move_department, ?ORG_ID, Params} ->
                    ?assertEqual(206, maps:get(parent_id, Params)),
                    ?assertEqual(7, maps:get(expected_version, Params)),
                    ?assertEqual(?DEPT_ID, maps:get(department_id, Params))
            after 1000 -> erlang:error(no_move_department_call)
            end
        end
    ).

%% move：parent_id=null（提根）必须原样是原子 null，不是二进制。
dept_move_promote_to_root_keeps_null_atom_test_() ->
    ?WITH_MECKS(
        dept_move_mocks(#{<<"parent_id">> => null, <<"expected_version">> => 7}),
        fun() ->
            organization_api_handler:handle_action(
                department_move, post_req, #{current_uid => ?UID}
            ),
            receive
                {move_department, _OrgId, Params} ->
                    ?assertEqual(null, maps:get(parent_id, Params))
            after 1000 -> erlang:error(no_move_department_call)
            end
        end
    ).

%% list：查询串未带 status 时必须**缺键**（app 层缺省才落 all；
%% 显式 undefined 曾被判 invalid_status → 400）。
dept_list_omits_status_key_when_absent_test_() ->
    ?WITH_MECKS(
        dept_list_mocks(fun(<<"status">>, _Req, _Default) -> <<>> end),
        fun() ->
            organization_api_handler:handle_action(
                department_collection, get_req, #{current_uid => ?UID}
            ),
            receive
                {list_departments, _OrgId, Params} ->
                    ?assertNot(maps:is_key(status, Params))
            after 1000 -> erlang:error(no_list_departments_call)
            end
        end
    ).

%% list：显式 ?status=active 仍透传原子（合法值路径不回归）。
dept_list_passes_status_atom_when_present_test_() ->
    ?WITH_MECKS(
        dept_list_mocks(fun(<<"status">>, _Req, _Default) -> <<"active">> end),
        fun() ->
            organization_api_handler:handle_action(
                department_collection, get_req, #{current_uid => ?UID}
            ),
            receive
                {list_departments, _OrgId, Params} ->
                    ?assertEqual(active, maps:get(status, Params))
            after 1000 -> erlang:error(no_list_departments_call)
            end
        end
    ).

%% DEFECT-1：accept 必须注入 membership_hook；hook 以同事务语义调用
%% upsert_active_tx（role=member），并把 repo 结果归一为 hook 契约。
invitation_accept_wires_membership_hook_test_() ->
    ?WITH_MECKS(
        invitation_accept_mocks(),
        fun() ->
            organization_api_handler:handle_action(
                invitation_accept, post_req, #{current_uid => 305}
            ),
            receive
                {accept_invitation, 305, ?ORG_ID, <<"tok">>, Opts} ->
                    ?assert(maps:is_key(membership_hook, Opts)),
                    Hook = maps:get(membership_hook, Opts),
                    Row = #{
                        <<"organization_id">> => ?ORG_ID,
                        <<"target_user_id">> => 305,
                        <<"invited_by">> => 304
                    },
                    ?assertEqual(ok, Hook(fake_conn, Row))
            after 1000 -> erlang:error(no_accept_call)
            end
        end
    ).

%% hook 失败必须把错误透传（app 层据此整体回滚事务）。
invitation_membership_hook_propagates_repo_error_test_() ->
    ?WITH_MECKS(
        invitation_accept_mocks(#{upsert_result => {error, {unexpected_write_result, x}}}),
        fun() ->
            organization_api_handler:handle_action(
                invitation_accept, post_req, #{current_uid => 305}
            ),
            receive
                {accept_invitation, _Uid, _OrgId, _Token, Opts} ->
                    Hook = maps:get(membership_hook, Opts),
                    Row = #{
                        <<"organization_id">> => ?ORG_ID,
                        <<"target_user_id">> => 305,
                        <<"invited_by">> => null
                    },
                    ?assertMatch({error, _}, Hook(fake_conn, Row))
            after 1000 -> erlang:error(no_accept_call)
            end
        end
    ).

%% ------------------------------------------------------------------
%% Internal
%% ------------------------------------------------------------------

dept_error_mocks() ->
    [
        {cowboy_req, [
            {'method', 1, fun
                (patch_req) -> <<"PATCH">>;
                (put_req) -> <<"PUT">>
            end},
            {'binding', 2, fun
                (organization_id, _) -> integer_to_binary(?ORG_ID);
                (department_id, _) -> integer_to_binary(?DEPT_ID)
            end},
            {'reply', 4, fun(StatusCode, Headers, _Body, _Req) ->
                #{response_status => StatusCode, allow => maps:get(<<"allow">>, Headers)}
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) -> #{} end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) ->
                #{response_status => 200, payload => Payload}
            end},
            {'error', 3, fun(_Req, _Msg, Code) -> #{response_status => Code} end}
        ]},
        {organization_department_app, [
            {'update_department', 2, fun(OrgId, Params) ->
                self() ! {update_department, OrgId, Params},
                {error, dept_error()}
            end}
        ]}
    ].

%% department_collection POST（create）：二进制键 body → app 层收到的形状。
dept_create_mocks(Body) ->
    [
        {cowboy_req, [
            {'method', 1, fun(post_req) -> <<"POST">> end},
            {'binding', 2, fun
                (organization_id, _) -> integer_to_binary(?ORG_ID);
                (_, _) -> undefined
            end},
            {'reply', 4, fun(StatusCode, _Headers, _Body, _Req) ->
                #{response_status => StatusCode}
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) -> Body end},
            {'get', 3, fun(_Key, _Req, Default) -> Default end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) ->
                #{response_status => 200, payload => Payload}
            end},
            {'error', 3, fun(_Req, _Msg, Code) -> #{response_status => Code} end}
        ]},
        {organization_department_app, [
            {'create_department', 2, fun(OrgId, Params) ->
                self() ! {create_department, OrgId, Params},
                {ok, #{id => 999, name => maps:get(name, Params, undefined)}}
            end}
        ]}
    ].

%% department_move POST：parent_id / expected_version 归一锁。
dept_move_mocks(Body) ->
    [
        {cowboy_req, [
            {'method', 1, fun(post_req) -> <<"POST">> end},
            {'binding', 2, fun
                (organization_id, _) -> integer_to_binary(?ORG_ID);
                (department_id, _) -> integer_to_binary(?DEPT_ID);
                (_, _) -> undefined
            end},
            {'reply', 4, fun(StatusCode, _Headers, _Body, _Req) ->
                #{response_status => StatusCode}
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) -> Body end},
            {'get', 3, fun(_Key, _Req, Default) -> Default end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) ->
                #{response_status => 200, payload => Payload}
            end},
            {'error', 3, fun(_Req, _Msg, Code) -> #{response_status => Code} end}
        ]},
        {organization_department_app, [
            {'move_department', 2, fun(OrgId, Params) ->
                self() ! {move_department, OrgId, Params},
                {ok, #{id => ?DEPT_ID}}
            end}
        ]}
    ].

%% department_collection GET（list）：status 查询键缺省/显式两态。
dept_list_mocks(GetFun) ->
    [
        {cowboy_req, [
            {'method', 1, fun(get_req) -> <<"GET">> end},
            {'binding', 2, fun
                (organization_id, _) -> integer_to_binary(?ORG_ID);
                (_, _) -> undefined
            end},
            {'reply', 4, fun(StatusCode, _Headers, _Body, _Req) ->
                #{response_status => StatusCode}
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) -> #{} end},
            {'get', 3, GetFun}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) ->
                #{response_status => 200, payload => Payload}
            end},
            {'error', 3, fun(_Req, _Msg, Code) -> #{response_status => Code} end}
        ]},
        {organization_department_app, [
            {'list_departments', 2, fun(OrgId, Params) ->
                self() ! {list_departments, OrgId, Params},
                {ok, []}
            end}
        ]}
    ].

%% invitation_accept POST：hook 注入 + hook→repo 契约锁。
invitation_accept_mocks() ->
    invitation_accept_mocks(#{upsert_result => {ok, changed, #{}}}).

invitation_accept_mocks(#{upsert_result := UpsertResult}) ->
    [
        {cowboy_req, [
            {'method', 1, fun(post_req) -> <<"POST">> end},
            {'binding', 2, fun
                (organization_id, _) -> integer_to_binary(?ORG_ID);
                (_, _) -> undefined
            end},
            {'reply', 4, fun(StatusCode, _Headers, _Body, _Req) ->
                #{response_status => StatusCode}
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) -> #{<<"token">> => <<"tok">>} end},
            {'get', 3, fun(_Key, _Req, Default) -> Default end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) ->
                #{response_status => 200, payload => Payload}
            end},
            {'error', 3, fun(_Req, _Msg, Code) -> #{response_status => Code} end}
        ]},
        {organization_invitation_app, [
            {'accept', 4, fun(Uid, OrgId, Token, Opts) ->
                self() ! {accept_invitation, Uid, OrgId, Token, Opts},
                {ok, #{status => accepted, already_accepted => false}}
            end}
        ]},
        {organization_member_repo, [
            {'upsert_active_tx', 5, fun(Conn, OrgId, Uid, Role, InvitedBy) ->
                self() ! {upsert_active_tx, Conn, OrgId, Uid, Role, InvitedBy},
                UpsertResult
            end}
        ]}
    ].

dept_error() ->
    case get(t_dept_error) of
        undefined -> conflict;
        Reason -> Reason
    end.
