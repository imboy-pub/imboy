-module(feedback_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

module_loaded_test_() ->
    ?TEST_SIMPLE(fun() ->
        code:ensure_loaded(feedback_handler),
        ?assertMatch({file, _}, code:is_loaded(feedback_handler))
    end).

page_returns_current_user_feedbacks_test_() ->
    ?WITH_MECKS(
        [
            {auth_ds, [
                {'current_uid', 1, fun(_State) -> 12345 end}
            ]},
            {elib_param, [
                {'page', 1, fun(_Req) -> {1, 20} end}
            ]},
            {feedback_repo, [
                {'tablename', 0, fun() -> <<"feedback">> end}
            ]},
            {elib_pg, [
                {'page_with_total', 6, fun(_Tb, _Column, Where, _Order, 1, 20) ->
                    ?assertEqual(12345, maps:get(user_id, Where)),
                    {ok, #{total => 1, page => 1, size => 20, list => [#{<<"feedback_id">> => 1}]}}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(Req, Payload) ->
                    Req#{response_status => 200, payload => Payload}
                end}
            ]}
        ],
        fun() ->
            {ok, Req, _State} = feedback_handler:init(#{}, #{action => page, current_uid => 12345}),
            Payload = maps:get(payload, Req),
            ?assertEqual(200, maps:get(response_status, Req)),
            ?assertEqual(1, maps:get(total, Payload))
        end
    ).

add_supports_description_alias_and_screenshots_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'header', 2, fun(Key, _Req) ->
                    case Key of
                        <<"cos">> -> <<"android">>;
                        <<"vsn">> -> <<"1.0.1">>;
                        <<"did">> -> <<"device_123">>
                    end
                end}
            ]},
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{
                        <<"type">> => <<"2">>,
                        <<"rating">> => <<"4">>,
                        <<"contact_detail">> => <<"user@example.com">>,
                        <<"description">> => <<"Bug report">>,
                        <<"screenshot">> => [<<"shot1.png">>, <<"shot2.png">>],
                        <<"sys_version">> => <<"Android 15">>
                    }
                end}
            ]},
            {feedback_ds, [
                {'add', 10, fun(Uid, Did, COS, COSV, AppVsn, Type, Rating, Contact, Body, Attach) ->
                    ?assertEqual(12345, Uid),
                    ?assertEqual(<<"device_123">>, Did),
                    ?assertEqual(<<"android">>, COS),
                    ?assertEqual(<<"Android 15">>, COSV),
                    ?assertEqual(<<"1.0.1">>, AppVsn),
                    ?assertEqual(<<"2">>, Type),
                    ?assertEqual(<<"4">>, Rating),
                    ?assertEqual(<<"user@example.com">>, Contact),
                    ?assertEqual(<<"Bug report">>, Body),
                    ?assertNotEqual(nomatch, binary:match(Attach, <<"shot1.png">>)),
                    ?assertNotEqual(nomatch, binary:match(Attach, <<"shot2.png">>)),
                    ok
                end}
            ]},
            {elib_response, [
                {'success', 1, fun(Req) -> Req#{response_status => 200} end}
            ]}
        ],
        fun() ->
            {ok, Req, _State} = feedback_handler:init(#{}, #{action => add, current_uid => 12345}),
            ?assertEqual(200, maps:get(response_status, Req))
        end
    ).

add_rejects_empty_description_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'header', 2, fun(_Key, _Req) -> <<>> end}
            ]},
            {elib_param, [
                {'post', 1, fun(_Req) -> #{<<"description">> => <<>>} end}
            ]},
            {elib_response, [
                {'error', 2, fun(Req, _Msg) -> Req#{response_status => 400} end}
            ]}
        ],
        fun() ->
            {ok, Req, _State} = feedback_handler:init(#{}, #{action => add, current_uid => 12345}),
            ?assertEqual(400, maps:get(response_status, Req))
        end
    ).

remove_calls_ds_for_valid_feedback_id_test_() ->
    ?WITH_MECKS(
        [
            {auth_ds, [
                {'current_uid', 1, fun(_State) -> 12345 end}
            ]},
            {elib_param, [
                {'int', 3, fun(feedback_id, _Req, _Default) -> {ok, 1001} end}
            ]},
            {feedback_ds, [
                {'remove', 2, fun(12345, 1001) -> ok end}
            ]},
            {elib_response, [
                {'success', 1, fun(Req) -> Req#{response_status => 200} end}
            ]}
        ],
        fun() ->
            {ok, Req, _State} = feedback_handler:init(#{}, #{action => remove, current_uid => 12345}),
            ?assertEqual(200, maps:get(response_status, Req))
        end
    ).

page_reply_normalizes_reply_ids_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'int', 3, fun(feedback_id, _Req, _Default) -> {ok, 1001} end},
                {'page', 1, fun(_Req) -> {1, 20} end}
            ]},
            {feedback_reply_repo, [
                {'tablename', 0, fun() -> <<"feedback_reply">> end}
            ]},
            {elib_pg, [
                {'one', 2, fun(_Sql, [1001]) -> {ok, #{<<"user_id">> => 12345}} end},
                {'page_with_total', 6, fun(_Tb, _Column, Where, _Order, 1, 20) ->
                    ?assertEqual(1001, maps:get(feedback_id, Where)),
                    {ok, #{
                        total => 1,
                        page => 1,
                        size => 20,
                        list => [
                            #{
                                <<"feedback_reply_id">> => 9,
                                <<"feedback_id">> => 1001,
                                <<"replier_user_id">> => 7
                            }
                        ]
                    }}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(Req, Payload) ->
                    Req#{response_status => 200, payload => Payload}
                end}
            ]}
        ],
        fun() ->
            {ok, Req, _State} = feedback_handler:init(#{}, #{
                action => page_reply, current_uid => 12345
            }),
            Payload = maps:get(payload, Req),
            [Item] = maps:get(list, Payload),
            ?assertEqual(200, maps:get(response_status, Req)),
            ?assertEqual(9, maps:get(<<"feedback_reply_id">>, Item)),
            ?assertEqual(false, maps:is_key(items, Payload))
        end
    ).

page_reply_invalid_feedback_id_returns_error_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'int', 3, fun(feedback_id, _Req, _Default) -> {ok, 0} end}
            ]},
            {elib_response, [
                {'error', 2, fun(Req, _Msg) -> Req#{response_status => 400} end}
            ]}
        ],
        fun() ->
            {ok, Req, _State} = feedback_handler:init(#{}, #{
                action => page_reply, current_uid => 12345
            }),
            ?assertEqual(400, maps:get(response_status, Req))
        end
    ).

%% 安全回归：非反馈提交者本人不能查看该反馈的回复（曾经零归属校验）
page_reply_permission_denied_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'int', 3, fun(feedback_id, _Req, _Default) -> {ok, 1001} end},
                {'page', 1, fun(_Req) -> {1, 20} end}
            ]},
            {elib_pg, [
                {'one', 2, fun(_Sql, [1001]) -> {ok, #{<<"user_id">> => 99999}} end}
            ]},
            {elib_response, [
                {'error', 3, fun(Req, _Msg, Code) -> Req#{response_status => Code} end}
            ]}
        ],
        fun() ->
            {ok, Req, _State} = feedback_handler:init(#{}, #{
                action => page_reply, current_uid => 12345
            }),
            ?assertEqual(?ERR_FORBIDDEN, maps:get(response_status, Req))
        end
    ).
