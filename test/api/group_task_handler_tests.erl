-module(group_task_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

%%%===================================================================
%%% @doc
%%% group_task_handler 参数校验与错误码回归测试
%%%===================================================================

create_missing_group_id_returns_bad_request_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"title">> => <<"章节作业"/utf8>>
                }
            end}
        ]},
        {elib_response, [
            {'error', 3, fun(_Req, _Msg, Code) ->
                self() ! {resp_code, Code},
                req_error
            end}
        ]}
    ], fun() ->
        Result = group_task_handler:handle_action(create, req_mock(), #{current_uid => 200}),
        ?assertEqual(req_error, Result),
        ?assertEqual(?ERR_BAD_REQUEST, receive_resp_code())
    end).

create_missing_title_returns_title_required_code_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"group_id">> => <<"g_valid">>,
                    <<"title">> => <<>>
                }
            end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(_Any) -> 12 end}
        ]},
        {elib_response, [
            {'error', 3, fun(_Req, _Msg, Code) ->
                self() ! {resp_code, Code},
                req_error
            end}
        ]}
    ], fun() ->
        Result = group_task_handler:handle_action(create, req_mock(), #{current_uid => 200}),
        ?assertEqual(req_error, Result),
        ?assertEqual(?ERR_TASK_TITLE_REQUIRED, receive_resp_code())
    end).

create_logic_error_propagates_code_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"group_id">> => <<"g_valid">>,
                    <<"title">> => <<"第 1 章作业"/utf8>>
                }
            end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(_Any) -> 12 end}
        ]},
        {group_task_logic, [
            {'create', 4, fun(_Gid, _Uid, _Title, _Data) ->
                {error, <<"无权限"/utf8>>, ?ERR_FORBIDDEN}
            end}
        ]},
        {elib_response, [
            {'error', 3, fun(_Req, _Msg, Code) ->
                self() ! {resp_code, Code},
                req_error
            end}
        ]}
    ], fun() ->
        Result = group_task_handler:handle_action(create, req_mock(), #{current_uid => 200}),
        ?assertEqual(req_error, Result),
        ?assertEqual(?ERR_FORBIDDEN, receive_resp_code())
    end).

create_with_user_ids_assigns_members_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"group_id">> => <<"g_valid">>,
                    <<"title">> => <<"第 1 章作业"/utf8>>,
                    <<"user_ids">> => [101, 102]
                }
            end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(_Any) -> 12 end},
            {'encode', 1, fun(42) -> <<"task_h42">> end}
        ]},
        {group_task_logic, [
            {'create', 4, fun(12, 200, _Title, _Data) ->
                {ok, 42}
            end},
            {'assign', 2, fun(42, [101, 102]) ->
                ok
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, _Payload, _Msg) ->
                req_ok
            end}
        ]}
    ], fun() ->
        Result = group_task_handler:handle_action(create, req_mock(), #{current_uid => 200}),
        ?assertEqual(req_ok, Result)
    end).

update_numeric_task_id_compatible_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"task_id">> => 42,
                    <<"title">> => <<"更新标题"/utf8>>
                }
            end}
        ]},
        {group_task_logic, [
            {'update', 3, fun(42, 200, Data) ->
                ?assertEqual(<<"更新标题"/utf8>>, maps:get(title, Data)),
                ok
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, _Payload, _Msg) ->
                req_ok
            end}
        ]}
    ], fun() ->
        Result = group_task_handler:handle_action(update, req_mock(), #{current_uid => 200}),
        ?assertEqual(req_ok, Result)
    end).

assign_invalid_task_id_returns_bad_request_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"task_id">> => <<"invalid_task">>,
                    <<"user_ids">> => [<<"u1">>]
                }
            end}
        ]},
        {elib_response, [
            {'error', 3, fun(_Req, _Msg, Code) ->
                self() ! {resp_code, Code},
                req_error
            end}
        ]}
    ], fun() ->
        Result = group_task_handler:handle_action(assign, req_mock(), #{current_uid => 200}),
        ?assertEqual(req_error, Result),
        ?assertEqual(?ERR_BAD_REQUEST, receive_resp_code())
    end).

assign_hashid_task_id_and_user_ids_compatible_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"task_id">> => <<"task_hash_42">>,
                    <<"user_ids">> => [<<"user_hash_1">>, <<"user_hash_2">>]
                }
            end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun
                (<<"task_hash_42">>) -> 42;
                (<<"user_hash_1">>) -> 101;
                (<<"user_hash_2">>) -> 102;
                (_Any) -> 0
            end}
        ]},
        {group_task_logic, [
            {'assign', 2, fun(42, [101, 102]) ->
                ok
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, _Payload, _Msg) ->
                req_ok
            end}
        ]}
    ], fun() ->
        Result = group_task_handler:handle_action(assign, req_mock(), #{current_uid => 200}),
        ?assertEqual(req_ok, Result)
    end).

submit_missing_task_id_returns_bad_request_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"content">> => <<"作业内容"/utf8>>}
            end}
        ]},
        {elib_response, [
            {'error', 3, fun(_Req, _Msg, Code) ->
                self() ! {resp_code, Code},
                req_error
            end}
        ]}
    ], fun() ->
        Result = group_task_handler:handle_action(submit, req_mock(), #{current_uid => 201}),
        ?assertEqual(req_error, Result),
        ?assertEqual(?ERR_BAD_REQUEST, receive_resp_code())
    end).

submit_numeric_task_id_compatible_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"task_id">> => 123,
                    <<"content">> => <<"作业内容"/utf8>>
                }
            end}
        ]},
        {group_task_repo, [
            {'find_by_id', 1, fun(123) ->
                {ok, #{<<"task_id">> => <<"task_uid_123">>}}
            end}
        ]},
        {group_task_logic, [
            {'submit', 3, fun(<<"task_uid_123">>, 201, _Data) ->
                ok
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, _Payload, _Msg) ->
                req_ok
            end}
        ]}
    ], fun() ->
        Result = group_task_handler:handle_action(submit, req_mock(), #{current_uid => 201}),
        ?assertEqual(req_ok, Result)
    end).

submit_attachments_array_compatible_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"task_id">> => 123,
                    <<"content">> => <<"作业内容"/utf8>>,
                    <<"attachments">> => [<<"a">>, <<"b">>]
                }
            end}
        ]},
        {group_task_repo, [
            {'find_by_id', 1, fun(123) ->
                {ok, #{<<"task_id">> => <<"task_uid_123">>}}
            end}
        ]},
        {group_task_logic, [
            {'submit', 3, fun(<<"task_uid_123">>, 201, Data) ->
                Attachment = maps:get(attachment, Data, <<>>),
                ?assert(is_binary(Attachment)),
                ok
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, _Payload, _Msg) ->
                req_ok
            end}
        ]}
    ], fun() ->
        Result = group_task_handler:handle_action(submit, req_mock(), #{current_uid => 201}),
        ?assertEqual(req_ok, Result)
    end).

review_hashid_assignment_id_compatible_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"assignment_id">> => <<"assignment_hash_456">>,
                    <<"score">> => 95,
                    <<"comment">> => <<"很好"/utf8>>
                }
            end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"assignment_hash_456">>) -> 456 end}
        ]},
        {group_task_logic, [
            {'review', 3, fun(456, 200, Data) ->
                ?assertEqual(95, maps:get(score, Data)),
                ?assertEqual(<<"很好"/utf8>>, maps:get(comment, Data)),
                ok
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, _Payload, _Msg) ->
                req_ok
            end}
        ]}
    ], fun() ->
        Result = group_task_handler:handle_action(review, req_mock(), #{current_uid => 200}),
        ?assertEqual(req_ok, Result)
    end).

review_invalid_assignment_id_returns_bad_request_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"assignment_id">> => <<"bad_assignment">>,
                    <<"score">> => 90
                }
            end}
        ]},
        {elib_response, [
            {'error', 3, fun(_Req, _Msg, Code) ->
                self() ! {resp_code, Code},
                req_error
            end}
        ]}
    ], fun() ->
        Result = group_task_handler:handle_action(review, req_mock(), #{current_uid => 200}),
        ?assertEqual(req_error, Result),
        ?assertEqual(?ERR_BAD_REQUEST, receive_resp_code())
    end).

detail_hashid_task_id_compatible_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'parse_qs', 1, fun(_Req) -> [{<<"task_id">>, <<"task_hash_88">>}] end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"task_hash_88">>) -> 88 end}
        ]},
        {group_task_logic, [
            {'detail', 1, fun(88) ->
                {ok, #{<<"task_id">> => <<"task_uid_88">>}}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, _Payload) ->
                req_ok
            end}
        ]}
    ], fun() ->
        Result = group_task_handler:handle_action(detail, req_mock(), #{current_uid => 200}),
        ?assertEqual(req_ok, Result)
    end).

pending_review_missing_task_id_returns_bad_request_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'parse_qs', 1, fun(_Req) -> [] end}
        ]},
        {elib_param, [
            {'page', 1, fun(_Req) -> {1, 20} end}
        ]},
        {elib_response, [
            {'error', 3, fun(_Req, _Msg, Code) ->
                self() ! {resp_code, Code},
                req_error
            end}
        ]}
    ], fun() ->
        Result = group_task_handler:handle_action(pending_review, req_mock(), #{current_uid => 200}),
        ?assertEqual(req_error, Result),
        ?assertEqual(?ERR_BAD_REQUEST, receive_resp_code())
    end).

pending_review_numeric_task_id_compatible_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'parse_qs', 1, fun(_Req) -> [{<<"task_id">>, <<"123">>}] end}
        ]},
        {elib_param, [
            {'page', 1, fun(_Req) -> {1, 20} end}
        ]},
        {group_task_repo, [
            {'find_by_id', 1, fun(123) ->
                {ok, #{<<"task_id">> => <<"task_uid_123">>}}
            end}
        ]},
        {group_task_logic, [
            {'pending_review', 3, fun(<<"task_uid_123">>, 1, 20) ->
                {ok, []}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, _Payload) ->
                req_ok
            end}
        ]}
    ], fun() ->
        Result = group_task_handler:handle_action(pending_review, req_mock(), #{current_uid => 200}),
        ?assertEqual(req_ok, Result)
    end).

list_defaults_to_current_uid_assignee_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'parse_qs', 1, fun(_Req) -> [{<<"group_id">>, <<"gid_12">>}] end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"gid_12">>) -> 12 end}
        ]},
        {elib_param, [
            {'page', 1, fun(_Req) -> {1, 20} end}
        ]},
        {group_task_logic, [
            {'list', 5, fun(12, undefined, 200, 1, 20) ->
                {ok, []}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, _Payload) ->
                req_ok
            end}
        ]}
    ], fun() ->
        Result = group_task_handler:handle_action(list, req_mock(), #{current_uid => 200}),
        ?assertEqual(req_ok, Result)
    end).

list_status_filter_uses_current_uid_assignee_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'parse_qs', 1, fun(_Req) ->
                [{<<"group_id">>, <<"gid_12">>}, {<<"status">>, <<"1">>}]
            end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"gid_12">>) -> 12 end}
        ]},
        {elib_param, [
            {'page', 1, fun(_Req) -> {1, 20} end}
        ]},
        {group_task_logic, [
            {'list', 5, fun(12, 1, 200, 1, 20) ->
                {ok, []}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, _Payload) ->
                req_ok
            end}
        ]}
    ], fun() ->
        Result = group_task_handler:handle_action(list, req_mock(), #{current_uid => 200}),
        ?assertEqual(req_ok, Result)
    end).

list_hashid_assignee_compatible_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'parse_qs', 1, fun(_Req) ->
                [{<<"group_id">>, <<"gid_12">>}, {<<"assignee_id">>, <<"user_hash_321">>}]
            end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun
                (<<"gid_12">>) -> 12;
                (<<"user_hash_321">>) -> 321;
                (_Any) -> 0
            end}
        ]},
        {elib_param, [
            {'page', 1, fun(_Req) -> {1, 20} end}
        ]},
        {group_task_logic, [
            {'list', 5, fun(12, undefined, 321, 1, 20) ->
                {ok, []}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, _Payload) ->
                req_ok
            end}
        ]}
    ], fun() ->
        Result = group_task_handler:handle_action(list, req_mock(), #{current_uid => 200}),
        ?assertEqual(req_ok, Result)
    end).

list_assignee_all_uses_group_view_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'parse_qs', 1, fun(_Req) ->
                [{<<"group_id">>, <<"gid_12">>}, {<<"assignee_id">>, <<"all">>}]
            end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"gid_12">>) -> 12 end}
        ]},
        {elib_param, [
            {'page', 1, fun(_Req) -> {1, 20} end}
        ]},
        {group_task_logic, [
            {'list', 5, fun(12, undefined, undefined, 1, 20) ->
                {ok, []}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, _Payload) ->
                req_ok
            end}
        ]}
    ], fun() ->
        Result = group_task_handler:handle_action(list, req_mock(), #{current_uid => 200}),
        ?assertEqual(req_ok, Result)
    end).

list_invalid_assignee_returns_bad_request_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'parse_qs', 1, fun(_Req) ->
                [{<<"group_id">>, <<"gid_12">>}, {<<"assignee_id">>, <<"bad_uid">>}]
            end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun
                (<<"gid_12">>) -> 12;
                (<<"bad_uid">>) -> 0
            end}
        ]},
        {elib_param, [
            {'page', 1, fun(_Req) -> {1, 20} end}
        ]},
        {elib_response, [
            {'error', 3, fun(_Req, _Msg, Code) ->
                self() ! {resp_code, Code},
                req_error
            end}
        ]}
    ], fun() ->
        Result = group_task_handler:handle_action(list, req_mock(), #{current_uid => 200}),
        ?assertEqual(req_error, Result),
        ?assertEqual(?ERR_BAD_REQUEST, receive_resp_code())
    end).

req_mock() ->
    #{mock_req => true}.

receive_resp_code() ->
    receive
        {resp_code, Code} -> Code
    after 1000 ->
        timeout
    end.
