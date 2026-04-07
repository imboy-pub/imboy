-module(adm_admin_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% adm_admin_handler 模块 EUnit 测试
%%%
%%% 目标：覆盖管理员列表、创建、分配角色的核心路径
%%%===================================================================

init_list_success_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"GET">> end}
        ]},
        {adm_user_logic, [
            {'find', 3, fun(1001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 1001, <<"role_id">> => [1]}
            end}
        ]},
        {elib_param, [
            {'page', 1, fun(_Req) -> {1, 10} end},
            {'int', 3, fun(Key, _Req, Default) ->
                case Key of
                    status -> {ok, -1};
                    role_id -> {ok, -1};
                    _ -> {ok, Default}
                end
            end},
            {'binary', 3, fun(keyword, _Req, _Default) -> {ok, <<>>} end}
        ]},
        {adm_user_repo, [
            {'tablename', 0, fun() -> <<"public.adm_user">> end}
        ]},
        {elib_pg, [
            {'one', 2, fun(_Sql, []) -> {ok, #{<<"count">> => 1}} end},
            {'query', 2, fun(_Sql, [10, 0]) ->
                {ok, [
                    #{
                        <<"id">> => 9,
                        <<"account">> => <<"ops_9">>,
                        <<"nickname">> => <<"ops">>,
                        <<"role_id">> => [2],
                        <<"status">> => 1,
                        <<"login_count">> => 12
                    }
                ]}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = #{},
        {ok, RespReq, _State} = adm_admin_handler:init(Req, #{action => list, adm_user_id => 1001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        Payload = maps:get(payload, RespReq),
        ?assertEqual(1, maps:get(total, Payload)),
        [Row | _] = maps:get(list, Payload),
        ?assertEqual(2, maps:get(<<"role_id">>, Row)),
        ?assertEqual([2], maps:get(<<"role_ids">>, Row))
    end).

init_create_forbidden_without_permission_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"POST">> end}
        ]},
        {adm_user_logic, [
            {'find', 3, fun(2001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 2001, <<"role_id">> => [3]}
            end},
            {'save', 1, fun(_Data) -> erlang:error(should_not_save) end}
        ]},
        {elib_response, [
            {'error', 3, fun(Req, Msg, Code) ->
                Req#{response_status => 403, error_msg => Msg, error_code => Code}
            end}
        ]}
    ], fun() ->
        Req = #{},
        {ok, RespReq, _State} = adm_admin_handler:init(Req, #{action => create, adm_user_id => 2001}),
        ?assertEqual(403, maps:get(response_status, RespReq)),
        ?assertEqual(<<"无权限操作"/utf8>>, maps:get(error_msg, RespReq)),
        ?assertEqual(0, meck:num_calls(adm_user_logic, save, 1))
    end).

init_create_success_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"POST">> end}
        ]},
        {adm_user_logic, [
            {'find', 3, fun(1001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 1001, <<"role_id">> => [1]}
            end},
            {'save', 1, fun(Data) ->
                ?assertEqual(<<"new_ops">>, maps:get(<<"account">>, Data)),
                ?assertEqual([2], maps:get(<<"role_id">>, Data)),
                ?assertEqual(1, maps:get(<<"status">>, Data)),
                {ok, 1}
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"account">> => <<"new_ops">>,
                    <<"pwd">> => <<"123456">>,
                    <<"nickname">> => <<"运营新号"/utf8>>,
                    <<"role_id">> => 2,
                    <<"status">> => 1
                }
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, _Payload) ->
                Req#{response_status => 200}
            end}
        ]}
    ], fun() ->
        Req = #{},
        {ok, RespReq, _State} = adm_admin_handler:init(Req, #{action => create, adm_user_id => 1001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual(1, meck:num_calls(adm_user_logic, save, 1))
    end).

init_assign_role_success_and_flush_cache_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"PUT">> end}
        ]},
        {adm_user_logic, [
            {'find', 3, fun(1001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 1001, <<"role_id">> => [1]}
            end},
            {'assign_roles', 2, fun(9, [3]) -> ok end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) -> #{<<"admin_id">> => 9, <<"role_id">> => 3} end}
        ]},
        {imboy_cache, [
            {'flush', 1, fun(_Key) -> ok end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, _Payload) ->
                Req#{response_status => 200}
            end}
        ]}
    ], fun() ->
        Req = #{},
        {ok, RespReq, _State} = adm_admin_handler:init(Req, #{action => assign_role, adm_user_id => 1001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual(1, meck:num_calls(adm_user_logic, assign_roles, 2)),
        ?assertEqual(8, meck:num_calls(imboy_cache, flush, 1))
    end).
