-module(group_category_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

mock_request() ->
    #{method => <<"POST">>, path => <<"/api/v1/group/category">>}.

init_false_action_passthrough_test_() ->
    ?TEST_SIMPLE(fun() ->
        Req = mock_request(),
        {ok, RespReq, State} = group_category_handler:init(Req, #{
            action => false, current_uid => 12345
        }),
        ?assertEqual(Req, RespReq),
        ?assertEqual(#{current_uid => 12345}, State)
    end).

init_create_success_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{<<"category_name">> => <<"分类A"/utf8>>}
                end}
            ]},
            {group_category_logic, [
                {'create', 2, fun(12345, <<"分类A"/utf8>>) ->
                    {ok, 77}
                end}
            ]},
            {elib_response, [
                {'success', 3, fun(Req, Payload, <<"创建分类成功"/utf8>>) ->
                    Req#{response_status => 200, payload => Payload}
                end}
            ]}
        ],
        fun() ->
            Req = mock_request(),
            {ok, RespReq, _State} = group_category_handler:init(Req, #{
                action => create, current_uid => 12345
            }),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            ?assertEqual(77, maps:get(<<"id">>, maps:get(payload, RespReq)))
        end
    ).

init_list_success_test_() ->
    ?WITH_MECKS(
        [
            {group_category_logic, [
                {'list', 1, fun(12345) ->
                    {ok, [#{<<"id">> => 1, <<"category_name">> => <<"默认">>}]}
                end}
            ]},
            {elib_response, [
                {'success', 3, fun(Req, Payload, <<"success."/utf8>>) ->
                    Req#{response_status => 200, payload => Payload}
                end}
            ]}
        ],
        fun() ->
            Req = mock_request(),
            {ok, RespReq, _State} = group_category_handler:init(Req, #{
                action => list, current_uid => 12345
            }),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            ?assertEqual(1, length(maps:get(<<"categories">>, maps:get(payload, RespReq))))
        end
    ).

init_rename_success_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{<<"id">> => 8, <<"category_name">> => <<"重命名后"/utf8>>}
                end}
            ]},
            {group_category_logic, [
                {'rename', 3, fun(12345, 8, <<"重命名后"/utf8>>) ->
                    ok
                end}
            ]},
            {elib_response, [
                {'success', 3, fun(Req, Payload, <<"重命名成功"/utf8>>) ->
                    Req#{response_status => 200, payload => Payload}
                end}
            ]}
        ],
        fun() ->
            Req = mock_request(),
            {ok, RespReq, _State} = group_category_handler:init(Req, #{
                action => rename, current_uid => 12345
            }),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            ?assertEqual(#{}, maps:get(payload, RespReq))
        end
    ).
