-module(adm_role_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% adm_role_handler 模块 EUnit 测试
%%%
%%% 目标：覆盖角色列表、角色创建、权限保存核心路径
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
            {'page', 1, fun(_Req) -> {1, 20} end},
            {'int', 3, fun(status, _Req, _Default) -> {ok, -1} end},
            {'binary', 3, fun(keyword, _Req, _Default) -> {ok, <<>>} end}
        ]},
        {elib_pg_sql, [
            {'public_tablename', 1, fun(<<"adm_role">>) -> <<"public.adm_role">> end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, []) ->
                {ok, [
                    #{<<"id">> => 9, <<"role_name">> => <<"custom_role">>, <<"status">> => 1, <<"created_at">> => <<"2026-01-01">>}
                ]}
            end}
        ]},
        {config_ds, [
            {'get', 2, fun(_Key, Default) -> Default end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = #{},
        {ok, RespReq, _State} = adm_role_handler:init(Req, #{action => list, adm_user_id => 1001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        Payload = maps:get(payload, RespReq),
        Items = maps:get(list, Payload),
        ?assert(length(Items) >= 4),
        RoleIds = lists:sort([maps:get(<<"id">>, Item) || Item <- Items]),
        ?assert(lists:member(1, RoleIds)),
        ?assert(lists:member(2, RoleIds)),
        ?assert(lists:member(3, RoleIds)),
        ?assert(lists:member(9, RoleIds))
    end).

init_create_role_success_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"POST">> end}
        ]},
        {adm_user_logic, [
            {'find', 3, fun(1001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 1001, <<"role_id">> => [1]}
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"name">> => <<"content_ops">>,
                    <<"description">> => <<"内容运营"/utf8>>,
                    <<"permissions">> => [<<"reports:read">>, <<"reports:handle">>],
                    <<"status">> => 1
                }
            end}
        ]},
        {elib_pg_sql, [
            {'public_tablename', 1, fun(<<"adm_role">>) -> <<"public.adm_role">> end}
        ]},
        {elib_pg, [
            {'query', 2, fun(Sql, Params) ->
                case binary:match(Sql, <<"SELECT id FROM">>) of
                    nomatch ->
                        erlang:error({unexpected_sql, Sql, Params});
                    _ ->
                        ?assertEqual([<<"content_ops">>], Params),
                        {ok, []}
                end
            end},
            {'one', 2, fun(Sql, Params) ->
                case binary:match(Sql, <<"SELECT COALESCE(MAX(sort), 99)">>) of
                    nomatch ->
                        ?assertNotEqual(nomatch, binary:match(Sql, <<"INSERT INTO">>)),
                        ?assertEqual([0, 100, <<"content_ops">>, 1], Params),
                        {ok, #{<<"id">> => 12}};
                    _ ->
                        {ok, #{<<"max_sort">> => 99}}
                end
            end}
        ]},
        {config_ds, [
            {'get', 2, fun(_Key, Default) -> Default end},
            {'set', 2, fun(_Key, _Value) -> ok end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = #{},
        {ok, RespReq, _State} = adm_role_handler:init(Req, #{action => create, adm_user_id => 1001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        Payload = maps:get(payload, RespReq),
        ?assertEqual(12, maps:get(<<"id">>, Payload)),
        ?assertEqual(12, maps:get(<<"role_id">>, Payload)),
        ?assertEqual(3, meck:num_calls(config_ds, set, 2))
    end).

init_permissions_save_success_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"PUT">> end}
        ]},
        {adm_user_logic, [
            {'find', 3, fun(1001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 1001, <<"role_id">> => [1]}
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"role_id">> => 2,
                    <<"permissions">> => [<<"dashboard:view">>, <<"reports:read">>, <<"reports:read">>]
                }
            end}
        ]},
        {config_ds, [
            {'get', 2, fun(_Key, Default) -> Default end},
            {'set', 2, fun(_Key, _Value) -> ok end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, _Payload) ->
                Req#{response_status => 200}
            end}
        ]}
    ], fun() ->
        Req = #{},
        {ok, RespReq, _State} = adm_role_handler:init(Req, #{action => permissions_save, adm_user_id => 1001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual(1, meck:num_calls(config_ds, set, 2))
    end).

init_permissions_save_forbidden_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"PUT">> end}
        ]},
        {adm_user_logic, [
            {'find', 3, fun(3001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 3001, <<"role_id">> => [3]}
            end}
        ]},
        {config_ds, [
            {'set', 2, fun(_Key, _Value) -> erlang:error(should_not_write) end}
        ]},
        {elib_response, [
            {'error', 3, fun(Req, Msg, Code) ->
                Req#{response_status => 403, error_msg => Msg, error_code => Code}
            end}
        ]}
    ], fun() ->
        Req = #{},
        {ok, RespReq, _State} = adm_role_handler:init(Req, #{action => permissions_save, adm_user_id => 3001}),
        ?assertEqual(403, maps:get(response_status, RespReq)),
        ?assertEqual(<<"无权限操作"/utf8>>, maps:get(error_msg, RespReq)),
        ?assertEqual(0, meck:num_calls(config_ds, set, 2))
    end).
