-module(group_tag_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

mock_request() ->
    #{method => <<"POST">>, path => <<"/api/v1/group/tag">>}.

init_false_action_passthrough_test_() ->
    ?TEST_SIMPLE(fun() ->
        Req = mock_request(),
        {ok, RespReq, State} = group_tag_handler:init(Req, #{action => false, current_uid => 12345}),
        ?assertEqual(Req, RespReq),
        ?assertEqual(#{current_uid => 12345}, State)
    end).

init_add_success_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{<<"gid">> => <<"321">>, <<"tag_name">> => <<"tag-a">>}
                end}
            ]},
            {group_tag_logic, [
                {'add', 3, fun(321, 12345, <<"tag-a">>) ->
                    {ok, 1}
                end}
            ]},
            {elib_response, [
                {'success', 1, fun(_Req) -> req_ok end},
                {'success', 2, fun(_Req, Payload) -> req_ok end},
                {'success', 3, fun(Req, Payload, <<"标签添加成功"/utf8>>) ->
                    Req#{response_status => 200, payload => Payload}
                end},
                {'success', 4, fun(_Req, _Payload, _Msg, _Opts) -> req_ok end},
                {'error', 1, fun(_Req) -> req_error end},
                {'error', 2, fun(_Req, _Msg) -> req_error end},
                {'error', 3, fun(_Req, _Msg, _Code) -> req_error end},
                {'error', 4, fun(_Req, _Msg, _Code, _Opts) -> req_error end}
            ]}
        ],
        fun() ->
            Req = mock_request(),
            {ok, RespReq, State} = group_tag_handler:init(Req, #{
                action => add, current_uid => 12345
            }),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            ?assertEqual(1, maps:get(<<"tag_id">>, maps:get(payload, RespReq))),
            ?assertEqual(#{current_uid => 12345}, State)
        end
    ).

init_add_missing_gid_error_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{<<"tag_name">> => <<"tag-a">>}
                end}
            ]},
            {elib_response, [
                {'success', 1, fun(_Req) -> req_ok end},
                {'success', 2, fun(_Req, _Payload) -> req_ok end},
                {'success', 3, fun(_Req, _Payload, _Msg) -> req_ok end},
                {'success', 4, fun(_Req, _Payload, _Msg, _Opts) -> req_ok end},
                {'error', 1, fun(_Req) -> req_error end},
                {'error', 2, fun(Req, <<"群组ID不能为空"/utf8>>) ->
                    Req#{response_status => 400}
                end},
                {'error', 3, fun(_Req, _Msg, _Code) -> req_error end},
                {'error', 4, fun(_Req, _Msg, _Code, _Opts) -> req_error end}
            ]}
        ],
        fun() ->
            Req = mock_request(),
            {ok, RespReq, _State} = group_tag_handler:init(Req, #{
                action => add, current_uid => 12345
            }),
            ?assertEqual(400, maps:get(response_status, RespReq))
        end
    ).

init_remove_success_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{<<"gid">> => <<"321">>, <<"tag_name">> => <<"tag-a">>}
                end}
            ]},
            {group_tag_logic, [
                {'remove', 3, fun(321, 12345, <<"tag-a">>) ->
                    ok
                end}
            ]},
            {elib_response, [
                {'success', 1, fun(_Req) -> req_ok end},
                {'success', 2, fun(_Req, Payload) -> req_ok end},
                {'success', 3, fun(Req, Payload, <<"标签删除成功"/utf8>>) ->
                    Req#{response_status => 200, payload => Payload}
                end},
                {'success', 4, fun(_Req, _Payload, _Msg, _Opts) -> req_ok end},
                {'error', 1, fun(_Req) -> req_error end},
                {'error', 2, fun(_Req, _Msg) -> req_error end},
                {'error', 3, fun(_Req, _Msg, _Code) -> req_error end},
                {'error', 4, fun(_Req, _Msg, _Code, _Opts) -> req_error end}
            ]}
        ],
        fun() ->
            Req = mock_request(),
            {ok, RespReq, _State} = group_tag_handler:init(Req, #{
                action => remove, current_uid => 12345
            }),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            ?assertEqual(#{}, maps:get(payload, RespReq))
        end
    ).

init_list_success_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'parse_qs', 1, fun(_Req) ->
                    [{<<"gid">>, <<"321">>}]
                end}
            ]},
            {group_tag_logic, [
                {'list', 2, fun(321, 12345) ->
                    {ok, [#{<<"id">> => 7, <<"tag_name">> => <<"tag-a">>}]}
                end}
            ]},
            {elib_response, [
                {'success', 1, fun(_Req) -> req_ok end},
                {'success', 2, fun(_Req, Payload) -> req_ok end},
                {'success', 3, fun(Req, Payload, <<"success."/utf8>>) ->
                    Req#{response_status => 200, payload => Payload}
                end},
                {'success', 4, fun(_Req, _Payload, _Msg, _Opts) -> req_ok end},
                {'error', 1, fun(_Req) -> req_error end},
                {'error', 2, fun(_Req, _Msg) -> req_error end},
                {'error', 3, fun(_Req, _Msg, _Code) -> req_error end},
                {'error', 4, fun(_Req, _Msg, _Code, _Opts) -> req_error end}
            ]}
        ],
        fun() ->
            Req = mock_request(),
            {ok, RespReq, _State} = group_tag_handler:init(Req, #{
                action => list, current_uid => 12345
            }),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            Payload = maps:get(payload, RespReq),
            [Tag] = maps:get(<<"list">>, Payload),
            ?assertEqual(7, maps:get(<<"id">>, Tag))
        end
    ).

init_search_success_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'parse_qs', 1, fun(_Req) ->
                    [{<<"tag_name">>, <<"tag-a">>}]
                end}
            ]},
            {group_tag_logic, [
                {'search', 1, fun(<<"tag-a">>) ->
                    {ok, [#{<<"group_id">> => 99, <<"tag_name">> => <<"tag-a">>}]}
                end}
            ]},
            {elib_response, [
                {'success', 1, fun(_Req) -> req_ok end},
                {'success', 2, fun(_Req, Payload) -> req_ok end},
                {'success', 3, fun(Req, Payload, <<"success."/utf8>>) ->
                    Req#{response_status => 200, payload => Payload}
                end},
                {'success', 4, fun(_Req, _Payload, _Msg, _Opts) -> req_ok end},
                {'error', 1, fun(_Req) -> req_error end},
                {'error', 2, fun(_Req, _Msg) -> req_error end},
                {'error', 3, fun(_Req, _Msg, _Code) -> req_error end},
                {'error', 4, fun(_Req, _Msg, _Code, _Opts) -> req_error end}
            ]}
        ],
        fun() ->
            Req = mock_request(),
            {ok, RespReq, _State} = group_tag_handler:init(Req, #{
                action => search, current_uid => 12345
            }),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            Payload = maps:get(payload, RespReq),
            [Group] = maps:get(<<"list">>, Payload),
            ?assertEqual(99, maps:get(<<"group_id">>, Group))
        end
    ).

init_hot_default_limit_success_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'parse_qs', 1, fun(_Req) ->
                    []
                end}
            ]},
            {elib_param, [
                {'int', 3, fun(limit, _Req, 20) ->
                    {error, invalid}
                end}
            ]},
            {group_tag_logic, [
                {'hot_tags', 1, fun(20) ->
                    {ok, [#{<<"tag_name">> => <<"tag-a">>, <<"count">> => 3}]}
                end}
            ]},
            {elib_response, [
                {'success', 1, fun(_Req) -> req_ok end},
                {'success', 2, fun(_Req, Payload) -> req_ok end},
                {'success', 3, fun(Req, Payload, <<"success."/utf8>>) ->
                    Req#{response_status => 200, payload => Payload}
                end},
                {'success', 4, fun(_Req, _Payload, _Msg, _Opts) -> req_ok end},
                {'error', 1, fun(_Req) -> req_error end},
                {'error', 2, fun(_Req, _Msg) -> req_error end},
                {'error', 3, fun(_Req, _Msg, _Code) -> req_error end},
                {'error', 4, fun(_Req, _Msg, _Code, _Opts) -> req_error end}
            ]}
        ],
        fun() ->
            Req = mock_request(),
            {ok, RespReq, _State} = group_tag_handler:init(Req, #{
                action => hot, current_uid => 12345
            }),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            Payload = maps:get(payload, RespReq),
            ?assertEqual(1, length(maps:get(<<"list">>, Payload)))
        end
    ).
