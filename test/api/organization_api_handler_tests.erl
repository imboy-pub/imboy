-module(organization_api_handler_tests).

%% ORG-BACKEND-GAP 缺口 2：department 应用层错误 → HTTP 状态的机械映射
%% （map_dept_error/1，经 department_item PATCH 路径 + respond_dept/2 驱动）。
%%
%% 卡片契约锚点：「并发冲突（expected-version）→ 稳定 409」。organization_department_pg
%% 的乐观锁拒绝（CAS UPDATE 影响行数 0）经应用层透传为 {error, conflict}，
%% 此前落入 else 兜底被映射成 400；本套件冻结其必须为 409，并锚定相邻映射
%% （not_found → 404、形状非法 → 400）不回归。

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

dept_error() ->
    case get(t_dept_error) of
        undefined -> conflict;
        Reason -> Reason
    end.
