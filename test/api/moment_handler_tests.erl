-module(moment_handler_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

create_uses_current_uid_and_post_values_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"content">> => <<"hello">>}
            end}
        ]},
        {moment_logic, [
            {'create_post', 2, fun(1001, #{<<"content">> := <<"hello">>}) ->
                {ok, #{<<"id">> => <<"abc123">>}}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) ->
                {ok_resp, Payload}
            end}
        ]}
    ], fun() ->
        Result = moment_handler:handle_action(create, req_mock(), #{current_uid => 1001}),
        ?assertEqual({ok_resp, #{<<"id">> => <<"abc123">>}}, Result)
    end).

show_prefers_path_moment_id_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'binding', 2, fun(moment_id, _Req) -> <<"m_hash_path">> end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) -> #{<<"moment_id">> => <<"m_hash_body">>} end}
        ]},
        {moment_logic, [
            {'get_post', 2, fun(1001, <<"m_hash_path">>) ->
                {ok, #{<<"id">> => <<"m_hash_path">>}}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
        ]}
    ], fun() ->
        Result = moment_handler:handle_action(show, req_mock(), #{current_uid => 1001}),
        ?assertEqual({ok_resp, #{<<"id">> => <<"m_hash_path">>}}, Result)
    end).

delete_prefers_path_moment_id_and_returns_success_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'binding', 2, fun(moment_id, _Req) -> <<"m_hash_path">> end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) -> #{<<"moment_id">> => <<"m_hash_body">>} end}
        ]},
        {moment_logic, [
            {'delete_post', 2, fun(1001, <<"m_hash_path">>) -> ok end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
        ]}
    ], fun() ->
        Result = moment_handler:handle_action(delete, req_mock(), #{current_uid => 1001}),
        ?assertEqual({ok_resp, #{}}, Result)
    end).

feed_decodes_cursor_and_passes_limit_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'parse_qs', 1, fun(_Req) ->
                [{<<"cursor">>, <<"6q58gm">>}, {<<"limit">>, <<"30">>}]
            end}
        ]},
        {moment_logic, [
            {'feed', 3, fun(1001, 11, 30) ->
                {ok, #{list => [], cursor => <<>>, limit => 30}}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
        ]}
    ], fun() ->
        Result = moment_handler:handle_action(feed, req_mock(), #{current_uid => 1001}),
        ?assertMatch({ok_resp, #{limit := 30}}, Result)
    end).

user_posts_prefers_path_uid_over_query_and_decodes_cursor_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'parse_qs', 1, fun(_Req) ->
                [
                    {<<"uid">>, <<"uid_from_query">>},
                    {<<"cursor">>, <<"6q58gm">>},
                    {<<"limit">>, <<"30">>}
                ]
            end},
            {'binding', 2, fun(uid, _Req) -> <<"uid_from_path">> end}
        ]},
        {moment_logic, [
            {'user_posts', 4, fun(1001, <<"uid_from_path">>, 11, 30) ->
                {ok, #{list => [], cursor => <<>>, limit => 30}}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
        ]}
    ], fun() ->
        Result = moment_handler:handle_action(user_posts, req_mock(), #{current_uid => 1001}),
        ?assertMatch({ok_resp, #{limit := 30}}, Result)
    end).

like_prefers_path_moment_id_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) -> #{<<"moment_id">> => <<"m_hash_body">>} end}
        ]},
        {cowboy_req, [
            {'binding', 2, fun(moment_id, _Req) -> <<"m_hash_path">> end}
        ]},
        {moment_logic, [
            {'like_post', 2, fun(1001, <<"m_hash_path">>) -> ok end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
        ]}
    ], fun() ->
        Result = moment_handler:handle_action(like, req_mock(), #{current_uid => 1001}),
        ?assertEqual({ok_resp, #{}}, Result)
    end).

unlike_prefers_path_moment_id_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) -> #{<<"moment_id">> => <<"m_hash_body">>} end}
        ]},
        {cowboy_req, [
            {'binding', 2, fun(moment_id, _Req) -> <<"m_hash_path">> end}
        ]},
        {moment_logic, [
            {'unlike_post', 2, fun(1001, <<"m_hash_path">>) -> ok end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
        ]}
    ], fun() ->
        Result = moment_handler:handle_action(unlike, req_mock(), #{current_uid => 1001}),
        ?assertEqual({ok_resp, #{}}, Result)
    end).

add_comment_passes_content_and_reply_to_uid_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"content">> => <<"hello comment">>,
                    <<"reply_to_uid">> => <<"u_reply">>
                }
            end}
        ]},
        {cowboy_req, [
            {'binding', 2, fun(moment_id, _Req) -> <<"m_hash_path">> end}
        ]},
        {moment_logic, [
            {'add_comment', 4, fun(1001, <<"m_hash_path">>, <<"hello comment">>, <<"u_reply">>) ->
                {ok, #{<<"id">> => <<"c_hash_1">>}}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
        ]}
    ], fun() ->
        Result = moment_handler:handle_action(add_comment, req_mock(), #{current_uid => 1001}),
        ?assertEqual({ok_resp, #{<<"id">> => <<"c_hash_1">>}}, Result)
    end).

comments_decodes_cursor_and_clamps_limit_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) -> #{} end}
        ]},
        {cowboy_req, [
            {'binding', 2, fun(moment_id, _Req) -> <<"m_hash_path">> end},
            {'parse_qs', 1, fun(_Req) ->
                [{<<"cursor">>, <<"6q58gm">>}, {<<"limit">>, <<"500">>}]
            end}
        ]},
        {moment_logic, [
            {'list_comments', 4, fun(1001, <<"m_hash_path">>, 11, 100) ->
                {ok, #{list => [], cursor => <<>>, limit => 100}}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
        ]}
    ], fun() ->
        Result = moment_handler:handle_action(comments, req_mock(), #{current_uid => 1001}),
        ?assertMatch({ok_resp, #{limit := 100}}, Result)
    end).

report_uses_path_moment_id_and_reason_desc_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"moment_id">> => <<"m_hash_body">>,
                    <<"reason">> => <<"spam">>,
                    <<"description">> => <<"abuse details">>
                }
            end}
        ]},
        {cowboy_req, [
            {'binding', 2, fun(moment_id, _Req) -> <<"m_hash_path">> end}
        ]},
        {moment_logic, [
            {'report_post', 4, fun(1001, <<"m_hash_path">>, <<"spam">>, <<"abuse details">>) ->
                {ok, #{<<"report_id">> => <<"r_hash_1">>}}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
        ]}
    ], fun() ->
        Result = moment_handler:handle_action(report, req_mock(), #{current_uid => 1001}),
        ?assertEqual({ok_resp, #{<<"report_id">> => <<"r_hash_1">>}}, Result)
    end).

delete_comment_prefers_path_ids_and_returns_success_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) -> #{} end}
        ]},
        {cowboy_req, [
            {'binding', 2, fun(Key, _Req) ->
                case Key of
                    moment_id -> <<"m_hash_path">>;
                    comment_id -> <<"c_hash_path">>;
                    _ -> undefined
                end
            end}
        ]},
        {moment_logic, [
            {'delete_comment', 3, fun(1001, <<"m_hash_path">>, <<"c_hash_path">>) -> ok end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
        ]}
    ], fun() ->
        Result = moment_handler:handle_action(delete_comment, req_mock(), #{current_uid => 1001}),
        ?assertEqual({ok_resp, #{}}, Result)
    end).

delete_comment_returns_error_when_comment_id_missing_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) -> #{} end}
        ]},
        {cowboy_req, [
            {'binding', 2, fun(Key, _Req) ->
                case Key of
                    moment_id -> <<"m_hash_path">>;
                    comment_id -> undefined;
                    _ -> undefined
                end
            end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Result = moment_handler:handle_action(delete_comment, req_mock(), #{current_uid => 1001}),
        ?assertEqual({error_resp, <<"评论ID不能为空"/utf8>>}, Result)
    end).

req_mock() ->
    #{}.
