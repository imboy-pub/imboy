-module(group_album_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

%%%===================================================================
%%% @doc
%%% group_album_handler 模块的 EUnit 测试
%%%
%%% 目标：验证处理层的错误码映射与上传分支路由
%%%===================================================================

delete_album_not_found_maps_error_code_test_() ->
    ?WITH_MECKS(
        [
            {elib_req, [
                {'body', 2, fun(_Req, _Opts) ->
                    {ok, #{<<"album_id">> => <<"album_1">>}, req_body}
                end}
            ]},
            {group_album_logic, [
                {'delete_album', 2, fun(<<"album_1">>, 100) ->
                    {error, <<"相册不存在"/utf8>>}
                end}
            ]},
            {elib_response, [
                {'error', 3, fun(_Req, _Msg, Code) ->
                    self() ! {resp_code, Code},
                    req_error
                end}
            ]}
        ],
        fun() ->
            Result = group_album_handler:handle_action(delete_album, req_mock(), #{
                current_uid => 100
            }),
            ?assertEqual(req_error, Result),
            ?assertEqual(?ERR_ALBUM_NOT_FOUND, receive_resp_code())
        end
    ).

delete_album_permission_denied_maps_error_code_test_() ->
    ?WITH_MECKS(
        [
            {elib_req, [
                {'body', 2, fun(_Req, _Opts) ->
                    {ok, #{<<"album_id">> => <<"album_2">>}, req_body}
                end}
            ]},
            {group_album_logic, [
                {'delete_album', 2, fun(<<"album_2">>, 101) ->
                    {error, <<"相册权限不足"/utf8>>}
                end}
            ]},
            {elib_response, [
                {'error', 3, fun(_Req, _Msg, Code) ->
                    self() ! {resp_code, Code},
                    req_error
                end}
            ]}
        ],
        fun() ->
            Result = group_album_handler:handle_action(delete_album, req_mock(), #{
                current_uid => 101
            }),
            ?assertEqual(req_error, Result),
            ?assertEqual(?ERR_ALBUM_PERMISSION_DENIED, receive_resp_code())
        end
    ).

delete_album_missing_album_id_returns_bad_request_test_() ->
    ?WITH_MECKS(
        [
            {elib_req, [
                {'body', 2, fun(_Req, _Opts) ->
                    {ok, #{}, req_body}
                end}
            ]},
            {elib_response, [
                {'error', 3, fun(_Req, _Msg, Code) ->
                    self() ! {resp_code, Code},
                    req_error
                end}
            ]}
        ],
        fun() ->
            Result = group_album_handler:handle_action(delete_album, req_mock(), #{
                current_uid => 100
            }),
            ?assertEqual(req_error, Result),
            ?assertEqual(?ERR_BAD_REQUEST, receive_resp_code())
        end
    ).

delete_album_success_returns_success_response_test_() ->
    ?WITH_MECKS(
        [
            {elib_req, [
                {'body', 2, fun(_Req, _Opts) ->
                    {ok, #{<<"album_id">> => <<"album_ok">>}, req_body}
                end}
            ]},
            {group_album_logic, [
                {'delete_album', 2, fun(<<"album_ok">>, 100) ->
                    ok
                end}
            ]},
            {elib_response, [
                {'success', 3, fun(_Req, _Data, _Msg) ->
                    req_ok
                end}
            ]}
        ],
        fun() ->
            Result = group_album_handler:handle_action(delete_album, req_mock(), #{
                current_uid => 100
            }),
            ?assertEqual(req_ok, Result)
        end
    ).

upload_photo_with_json_content_type_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'header', 3, fun(<<"content-type">>, _Req, <<>>) ->
                    <<"application/json">>
                end}
            ]},
            {elib_req, [
                {'body', 2, fun(_Req, _Opts) ->
                    {ok,
                        #{
                            <<"gid">> => <<"group_1">>,
                            <<"album_id">> => <<"album_3">>,
                            <<"photo">> => <<"aGVsbG8=">>,
                            <<"photo_name">> => <<"a.jpg">>
                        },
                        req_body}
                end}
            ]},
            {group_album_logic, [
                {'upload_photo', 5, fun(
                    <<"group_1">>, 102, <<"album_3">>, <<"hello">>, <<"a.jpg">>
                ) ->
                    {ok, #{<<"photo_id">> => 1}}
                end}
            ]},
            {elib_response, [
                {'success', 3, fun(_Req, _Data, _Msg) ->
                    req_ok
                end}
            ]}
        ],
        fun() ->
            Result = group_album_handler:handle_action(upload_photo, req_mock(), #{
                current_uid => 102
            }),
            ?assertEqual(req_ok, Result)
        end
    ).

upload_photo_with_missing_album_id_returns_bad_request_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'header', 3, fun(<<"content-type">>, _Req, <<>>) ->
                    <<"application/json">>
                end}
            ]},
            {elib_req, [
                {'body', 2, fun(_Req, _Opts) ->
                    {ok,
                        #{
                            <<"gid">> => <<"group_1">>,
                            <<"photo">> => <<"aGVsbG8=">>,
                            <<"photo_name">> => <<"a.jpg">>
                        },
                        req_body}
                end}
            ]},
            {elib_response, [
                {'error', 3, fun(_Req, _Msg, Code) ->
                    self() ! {resp_code, Code},
                    req_error
                end}
            ]}
        ],
        fun() ->
            Result = group_album_handler:handle_action(upload_photo, req_mock(), #{
                current_uid => 102
            }),
            ?assertEqual(req_error, Result),
            ?assertEqual(?ERR_BAD_REQUEST, receive_resp_code())
        end
    ).

upload_photo_with_invalid_base64_returns_bad_request_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'header', 3, fun(<<"content-type">>, _Req, <<>>) ->
                    <<"application/json">>
                end}
            ]},
            {elib_req, [
                {'body', 2, fun(_Req, _Opts) ->
                    {ok,
                        #{
                            <<"gid">> => <<"group_1">>,
                            <<"album_id">> => <<"album_3">>,
                            <<"photo">> => <<"%%%INVALID_BASE64%%%">>,
                            <<"photo_name">> => <<"a.jpg">>
                        },
                        req_body}
                end}
            ]},
            {elib_response, [
                {'error', 3, fun(_Req, _Msg, Code) ->
                    self() ! {resp_code, Code},
                    req_error
                end}
            ]}
        ],
        fun() ->
            Result = group_album_handler:handle_action(upload_photo, req_mock(), #{
                current_uid => 102
            }),
            ?assertEqual(req_error, Result),
            ?assertEqual(?ERR_BAD_REQUEST, receive_resp_code())
        end
    ).

upload_photo_with_missing_gid_returns_bad_request_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'header', 3, fun(<<"content-type">>, _Req, <<>>) ->
                    <<"application/json">>
                end}
            ]},
            {elib_req, [
                {'body', 2, fun(_Req, _Opts) ->
                    {ok,
                        #{
                            <<"album_id">> => <<"album_3">>,
                            <<"photo">> => <<"aGVsbG8=">>,
                            <<"photo_name">> => <<"a.jpg">>
                        },
                        req_body}
                end}
            ]},
            {elib_response, [
                {'error', 3, fun(_Req, _Msg, Code) ->
                    self() ! {resp_code, Code},
                    req_error
                end}
            ]}
        ],
        fun() ->
            Result = group_album_handler:handle_action(upload_photo, req_mock(), #{
                current_uid => 102
            }),
            ?assertEqual(req_error, Result),
            ?assertEqual(?ERR_BAD_REQUEST, receive_resp_code())
        end
    ).

upload_photo_json_album_not_found_maps_error_code_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'header', 3, fun(<<"content-type">>, _Req, <<>>) ->
                    <<"application/json">>
                end}
            ]},
            {elib_req, [
                {'body', 2, fun(_Req, _Opts) ->
                    {ok,
                        #{
                            <<"gid">> => <<"group_1">>,
                            <<"album_id">> => <<"album_404">>,
                            <<"photo">> => <<"aGVsbG8=">>,
                            <<"photo_name">> => <<"a.jpg">>
                        },
                        req_body}
                end}
            ]},
            {group_album_logic, [
                {'upload_photo', 5, fun(
                    <<"group_1">>, 102, <<"album_404">>, <<"hello">>, <<"a.jpg">>
                ) ->
                    {error, <<"相册不存在"/utf8>>}
                end}
            ]},
            {elib_response, [
                {'error', 3, fun(_Req, _Msg, Code) ->
                    self() ! {resp_code, Code},
                    req_error
                end}
            ]}
        ],
        fun() ->
            Result = group_album_handler:handle_action(upload_photo, req_mock(), #{
                current_uid => 102
            }),
            ?assertEqual(req_error, Result),
            ?assertEqual(?ERR_ALBUM_NOT_FOUND, receive_resp_code())
        end
    ).

upload_photo_json_permission_denied_maps_error_code_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'header', 3, fun(<<"content-type">>, _Req, <<>>) ->
                    <<"application/json">>
                end}
            ]},
            {elib_req, [
                {'body', 2, fun(_Req, _Opts) ->
                    {ok,
                        #{
                            <<"gid">> => <<"group_1">>,
                            <<"album_id">> => <<"album_perm">>,
                            <<"photo">> => <<"aGVsbG8=">>,
                            <<"photo_name">> => <<"a.jpg">>
                        },
                        req_body}
                end}
            ]},
            {group_album_logic, [
                {'upload_photo', 5, fun(
                    <<"group_1">>, 102, <<"album_perm">>, <<"hello">>, <<"a.jpg">>
                ) ->
                    {error, <<"相册权限不足"/utf8>>}
                end}
            ]},
            {elib_response, [
                {'error', 3, fun(_Req, _Msg, Code) ->
                    self() ! {resp_code, Code},
                    req_error
                end}
            ]}
        ],
        fun() ->
            Result = group_album_handler:handle_action(upload_photo, req_mock(), #{
                current_uid => 102
            }),
            ?assertEqual(req_error, Result),
            ?assertEqual(?ERR_ALBUM_PERMISSION_DENIED, receive_resp_code())
        end
    ).

upload_photo_with_malformed_multipart_returns_bad_request_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'header', 3, fun(<<"content-type">>, _Req, <<>>) ->
                    <<"multipart/form-data; boundary=abc">>
                end},
                {'read_part', 1, fun(_Req) ->
                    {ok, #{mock_header => true}, req_part}
                end},
                {'read_part_body', 2, fun(_Req, _Opts) ->
                    {ok, <<"binary">>, req_part_body}
                end}
            ]},
            {cow_multipart, [
                {'form_data', 1, fun(_Headers) ->
                    erlang:error(malformed_headers)
                end}
            ]},
            {elib_response, [
                {'error', 3, fun(_Req, _Msg, Code) ->
                    self() ! {resp_code, Code},
                    req_error
                end}
            ]}
        ],
        fun() ->
            Result = group_album_handler:handle_action(upload_photo, req_mock(), #{
                current_uid => 103
            }),
            ?assertEqual(req_error, Result),
            ?assertEqual(?ERR_BAD_REQUEST, receive_resp_code())
        end
    ).

create_album_missing_gid_returns_bad_request_test_() ->
    ?WITH_MECKS(
        [
            {elib_req, [
                {'body', 2, fun(_Req, _Opts) ->
                    {ok, #{<<"album_name">> => <<"活动相册"/utf8>>}, req_body}
                end}
            ]},
            {elib_response, [
                {'error', 3, fun(_Req, _Msg, Code) ->
                    self() ! {resp_code, Code},
                    req_error
                end}
            ]}
        ],
        fun() ->
            Result = group_album_handler:handle_action(create_album, req_mock(), #{
                current_uid => 105
            }),
            ?assertEqual(req_error, Result),
            ?assertEqual(?ERR_BAD_REQUEST, receive_resp_code())
        end
    ).

list_albums_missing_gid_returns_bad_request_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'method', 1, fun(_Req) -> <<"GET">> end},
                {'parse_qs', 1, fun(_Req) -> [] end}
            ]},
            {elib_response, [
                {'error', 3, fun(_Req, _Msg, Code) ->
                    self() ! {resp_code, Code},
                    req_error
                end}
            ]}
        ],
        fun() ->
            Result = group_album_handler:handle_action(list_albums, req_mock(), #{
                current_uid => 105
            }),
            ?assertEqual(req_error, Result),
            ?assertEqual(?ERR_BAD_REQUEST, receive_resp_code())
        end
    ).

%% ===================================================================
%% Helpers
%% ===================================================================

req_mock() ->
    #{mock_req => true}.

receive_resp_code() ->
    receive
        {resp_code, Code} -> Code
    after 1000 ->
        timeout
    end.
