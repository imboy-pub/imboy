%%% @doc Human Organization Directory handler 协议面测试。
%%%
%%% 冻结口径：
%%%   * 成功：elib_response:success/2（imboy envelope，payload 含 list 键）
%%%   * 失败：真实 HTTP 状态 + {"error":{"code","message"}} 信封
%%%     （复用 enterprise_internal_error 的 stable 码表）
%%%   * 非 GET → 405；organization_id 非法 → 400 invalid_request
%%%   * query 参数（parent_id/department_id/q/cursor/limit）原样透传 app 层
-module(organization_directory_handler_tests).

-include_lib("eunit/include/eunit.hrl").

-define(HANDLER, organization_directory_handler).
-define(ORG_ID, 9001).
-define(UID, 9002).

%% ------------------------------------------------------------------
%% 方法门 / binding 门
%% ------------------------------------------------------------------

non_get_methods_are_405_test_() ->
    with_handler_mocks(
        fun() ->
            lists:foreach(
                fun(Action) ->
                    Result = ?HANDLER:handle_action(Action, post_req, #{current_uid => ?UID}),
                    ?assertEqual(405, maps:get(response_status, Result), {action, Action})
                end,
                [directory_departments, directory_members, directory_me, directory_search]
            )
        end
    ).

bad_org_binding_is_400_invalid_request_test_() ->
    with_handler_mocks(
        fun() ->
            Result = ?HANDLER:handle_action(directory_departments, bad_req, #{current_uid => ?UID}),
            ?assertEqual(400, maps:get(response_status, Result)),
            ?assertEqual(
                <<"{\"error\":{\"code\":\"invalid_request\",\"message\":\"invalid request\"}}">>,
                maps:get(response_body, Result)
            )
        end
    ).

%% ------------------------------------------------------------------
%% 错误映射：stable 码 → 真实 HTTP 状态 + error 信封
%% ------------------------------------------------------------------

error_code_maps_to_real_status_test_() ->
    with_handler_mocks(
        fun() ->
            Cases = [
                {<<"insufficient_scope">>, 403},
                {<<"organization_disabled">>, 403},
                {<<"resource_not_found">>, 404},
                {<<"invalid_request">>, 400},
                {<<"security_gate_closed">>, 503},
                {<<"internal_error">>, 500}
            ],
            lists:foreach(
                fun({Code, Status}) ->
                    put(t_dir_err, Code),
                    Result = ?HANDLER:handle_action(
                        directory_departments, get_req, #{current_uid => ?UID}
                    ),
                    ?assertEqual(Status, maps:get(response_status, Result), {code, Code}),
                    ?assertMatch(
                        #{<<"error">> := #{<<"code">> := Code, <<"message">> := _}},
                        maps:get(response_json, Result),
                        {envelope, Code}
                    )
                end,
                Cases
            )
        end
    ).

%% ------------------------------------------------------------------
%% 成功路径：payload 原样进 imboy envelope；参数透传
%% ------------------------------------------------------------------

success_uses_imboy_envelope_with_list_test_() ->
    with_handler_mocks(
        fun() ->
            Result = ?HANDLER:handle_action(directory_departments, get_req, #{
                current_uid => ?UID
            }),
            ?assertEqual(200, maps:get(response_status, Result)),
            ?assertMatch(#{list := _, cursor := null, has_more := false}, maps:get(payload, Result))
        end
    ).

me_passes_uid_and_org_test_() ->
    with_handler_mocks(
        fun() ->
            Result = ?HANDLER:handle_action(directory_me, get_req, #{current_uid => ?UID}),
            ?assertEqual(200, maps:get(response_status, Result)),
            ?assertEqual(?ORG_ID, maps:get(org_called, maps:get(payload, Result))),
            ?assertEqual(?UID, maps:get(uid_called, maps:get(payload, Result)))
        end
    ).

search_passes_query_params_test_() ->
    with_handler_mocks(
        fun() ->
            Result = ?HANDLER:handle_action(directory_search, get_req, #{current_uid => ?UID}),
            ?assertEqual(200, maps:get(response_status, Result)),
            ?assertEqual(
                #{
                    q => <<"张三"/utf8>>,
                    cursor => <<"cur-1">>,
                    limit => <<"7">>
                },
                maps:get(search_params, maps:get(payload, Result))
            )
        end
    ).

members_passes_query_params_test_() ->
    with_handler_mocks(
        fun() ->
            Result = ?HANDLER:handle_action(directory_members, get_req, #{current_uid => ?UID}),
            ?assertEqual(200, maps:get(response_status, Result)),
            ?assertEqual(
                #{
                    department_id => <<"12">>,
                    cursor => <<"cur-1">>,
                    limit => <<"7">>
                },
                maps:get(members_params, maps:get(payload, Result))
            )
        end
    ).

%% ------------------------------------------------------------------
%% mocks
%% ------------------------------------------------------------------

with_handler_mocks(Body) ->
    {setup,
        fun() ->
            ok = meck:new(cowboy_req, [no_passthrough_cover]),
            ok = meck:new(elib_param, [no_passthrough_cover]),
            ok = meck:new(elib_response, [no_passthrough_cover]),
            ok = meck:new(organization_directory_app, [no_passthrough_cover]),

            meck:expect(cowboy_req, method, fun
                (get_req) -> <<"GET">>;
                (bad_req) -> <<"GET">>;
                (post_req) -> <<"POST">>
            end),
            meck:expect(cowboy_req, binding, fun
                (organization_id, get_req) -> integer_to_binary(?ORG_ID);
                (organization_id, post_req) -> integer_to_binary(?ORG_ID);
                (organization_id, bad_req) -> <<"abc">>
            end),
            meck:expect(cowboy_req, path, fun(_Req) -> <<"/api/v1/test">> end),
            meck:expect(cowboy_req, reply, fun
                (Status, _Headers, <<"{", _/binary>> = Body, _Req) ->
                    #{
                        response_status => Status,
                        response_json => jsone:decode(Body),
                        response_body => Body
                    };
                (Status, Headers, Body, _Req) when is_binary(Body) ->
                    #{
                        response_status => Status,
                        allow => maps:get(<<"allow">>, Headers, undefined),
                        plain => Body
                    }
            end),
            meck:expect(elib_param, get, 3, fun
                (<<"q">>, _Req, _D) -> <<"张三"/utf8>>;
                (<<"cursor">>, _Req, _D) -> <<"cur-1">>;
                (<<"department_id">>, _Req, _D) -> <<"12">>;
                (<<"limit">>, _Req, _D) -> <<"7">>;
                (_Key, _Req, Default) -> Default
            end),
            meck:expect(elib_response, success, fun(_Req, Payload) ->
                #{response_status => 200, payload => Payload}
            end),

            %% app 层：默认成功形态；错误由 t_dir_err 控制。
            meck:expect(organization_directory_app, list_departments, fun(OrgId, Uid, _Params) ->
                app_result(#{org_called => OrgId, uid_called => Uid})
            end),
            meck:expect(organization_directory_app, list_members, fun(_OrgId, _Uid, Params) ->
                app_result(#{members_params => Params})
            end),
            meck:expect(organization_directory_app, my_departments, fun(OrgId, Uid) ->
                app_result(#{org_called => OrgId, uid_called => Uid})
            end),
            meck:expect(organization_directory_app, search, fun(_OrgId, _Uid, Params) ->
                app_result(#{search_params => Params})
            end),
            ok
        end,
        fun(_) ->
            catch meck:unload(cowboy_req),
            catch meck:unload(elib_param),
            catch meck:unload(elib_response),
            catch meck:unload(organization_directory_app),
            erase(t_dir_err),
            ok
        end,
        Body}.

app_result(Extra) ->
    case get(t_dir_err) of
        undefined ->
            {ok, maps:merge(#{list => [], cursor => null, has_more => false}, Extra)};
        Code ->
            {error, Code}
    end.
