-module(adm_moment_handler_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

moment_feature_enabled_mocks() ->
    [
        {imboy_feature, [
            {'ensure_enabled', 2, fun(_Req, moment) -> ok end}
        ]}
    ].

init_list_reads_filters_and_pagination_test_() ->
    ?WITH_MECKS(moment_feature_enabled_mocks() ++ [
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"GET">> end},
            {'parse_qs', 1, fun(_Req) ->
                [
                    {<<"keyword">>, <<"spam">>},
                    {<<"uid">>, <<"6q58gm">>},
                    {<<"status">>, <<"1">>}
                ]
            end}
        ]},
        {elib_param, [
            {'page', 1, fun(_Req) -> {1, 20} end}
        ]},
        {adm_user_logic, [
            {'find', 3, fun(9001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9001, <<"role_id">> => [1]}
            end}
        ]},
        {adm_index_handler, [
            {'role_acl', 1, fun(1) ->
                {<<"super_admin">>, [<<"moments:read">>], []}
            end}
        ]},
        {moment_logic, [
            {'admin_list_posts', 5, fun(<<"spam">>, <<"6q58gm">>, 1, 1, 20) ->
                {ok, #{list => [], total => 0, page => 1, size => 20}}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        {ok, RespReq, _State} = adm_moment_handler:init(req_mock(), #{action => list, adm_user_id => 9001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual(0, maps:get(total, maps:get(payload, RespReq)))
    end).

init_delete_uses_admin_user_id_test_() ->
    ?WITH_MECKS(moment_feature_enabled_mocks() ++ [
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"POST">> end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"moment_id">> => <<"9xz3qp">>, <<"reason">> => <<"违规"/utf8>>}
            end}
        ]},
        {adm_user_logic, [
            {'find', 3, fun(9001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9001, <<"role_id">> => [1]}
            end}
        ]},
        {adm_index_handler, [
            {'role_acl', 1, fun(1) ->
                {<<"super_admin">>, [<<"moments:delete">>], []}
            end}
        ]},
        {moment_logic, [
            {'admin_delete_post', 3, fun(9001, <<"9xz3qp">>, <<"违规"/utf8>>) ->
                ok
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, Payload, Msg) ->
                Req#{response_status => 200, payload => Payload, msg => Msg}
            end}
        ]}
    ], fun() ->
        {ok, RespReq, _State} = adm_moment_handler:init(req_mock(), #{action => delete, adm_user_id => 9001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual(<<"删除成功"/utf8>>, maps:get(msg, RespReq))
    end).

init_report_resolve_passes_params_to_logic_test_() ->
    ?WITH_MECKS(moment_feature_enabled_mocks() ++ [
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"POST">> end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"report_id">> => <<"rp123">>, <<"result">> => <<"2">>, <<"note">> => <<"确认违规"/utf8>>}
            end}
        ]},
        {adm_user_logic, [
            {'find', 3, fun(9001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9001, <<"role_id">> => [1]}
            end}
        ]},
        {adm_index_handler, [
            {'role_acl', 1, fun(1) ->
                {<<"super_admin">>, [<<"moments:report:handle">>], []}
            end}
        ]},
        {moment_logic, [
            {'admin_resolve_report', 4, fun(9001, <<"rp123">>, 2, <<"确认违规"/utf8>>) ->
                ok
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        {ok, RespReq, _State} = adm_moment_handler:init(req_mock(), #{action => report_resolve, adm_user_id => 9001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual(#{}, maps:get(payload, RespReq))
    end).

req_mock() ->
    #{}.
