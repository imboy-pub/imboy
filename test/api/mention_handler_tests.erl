-module(mention_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

%%%===================================================================
%%% @doc
%%% mention_handler 参数校验与错误码回归测试
%%%===================================================================

list_mentions_with_is_read_true_passes_filter_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'parse_qs', 1, fun(_Req) -> [{<<"is_read">>, <<"true">>}] end}
        ]},
        {elib_param, [
            {'page', 1, fun(_Req) -> {2, 15} end}
        ]},
        {mention_logic, [
            {'list_mentions', 3, fun(_Uid, _IsRead, _Options) ->
                {ok, [
                    #{
                        <<"msg_id">> => <<"m_1">>,
                        <<"group_id">> => 8,
                        <<"from_uid">> => 9,
                        <<"mentioned_uid">> => 100
                    }
                ]}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Data) ->
                self() ! {resp_data, Data},
                req_ok
            end}
        ]}
    ], fun() ->
        Result = mention_handler:list_mentions(req_mock(), #{current_uid => 100}),
        ?assertEqual(req_ok, Result),
        Data = receive_resp_data(),
        ?assertEqual(2, maps:get(page, Data)),
        ?assertEqual(1, maps:get(total, Data))
    end).

list_mentions_logic_error_returns_generic_error_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'parse_qs', 1, fun(_Req) -> [] end}
        ]},
        {elib_param, [
            {'page', 1, fun(_Req) -> {1, 20} end}
        ]},
        {mention_logic, [
            {'list_mentions', 3, fun(_Uid, _IsRead, _Opt) -> {error, db_down} end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) ->
                self() ! {resp_msg, Msg},
                req_error
            end}
        ]}
    ], fun() ->
        Result = mention_handler:list_mentions(req_mock(), #{current_uid => 100}),
        ?assertEqual(req_error, Result),
        ?assertEqual(<<"获取@消息列表失败"/utf8>>, msg_to_binary(receive_resp_msg()))
    end).

mark_read_missing_msg_id_returns_validation_error_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) -> #{} end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) ->
                self() ! {resp_msg, Msg},
                req_error
            end}
        ]}
    ], fun() ->
        Result = mention_handler:mark_read(req_mock(), #{current_uid => 100}),
        ?assertEqual(req_error, Result),
        ?assertEqual(<<"消息ID必须提供"/utf8>>, msg_to_binary(receive_resp_msg()))
    end).

suggest_missing_gid_returns_bad_request_code_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'parse_qs', 1, fun(_Req) -> [] end}
        ]},
        {elib_response, [
            {'error', 3, fun(_Req, _Msg, Code) ->
                self() ! {resp_code, Code},
                req_error
            end}
        ]}
    ], fun() ->
        Result = mention_handler:suggest(req_mock(), #{current_uid => 100}),
        ?assertEqual(req_error, Result),
        ?assertEqual(?ERR_BAD_REQUEST, receive_resp_code())
    end).

suggest_not_group_member_maps_permission_denied_code_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'parse_qs', 1, fun(_Req) ->
                [{<<"gid">>, <<"g_12">>}, {<<"keyword">>, <<"张"/utf8>>}]
            end}
        ]},
        {mention_logic, [
            {'get_member_suggestions', 3, fun(_Gid, _Uid, _Keyword) ->
                {error, not_group_member}
            end}
        ]},
        {elib_response, [
            {'error', 3, fun(_Req, _Msg, Code) ->
                self() ! {resp_code, Code},
                req_error
            end}
        ]}
    ], fun() ->
        Result = mention_handler:suggest(req_mock(), #{current_uid => 100}),
        ?assertEqual(req_error, Result),
        ?assertEqual(?ERR_GROUP_PERMISSION_DENIED, receive_resp_code())
    end).

list_mentions_accepts_group_id_in_post_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) -> #{<<"group_id">> => <<"g_12">>, <<"page">> => 1, <<"size">> => 20} end},
            {'page', 1, fun(_Req) -> {1, 20} end}
        ]},
        {cowboy_req, [
            {'parse_qs', 1, fun(_Req) -> [] end}
        ]},
        {mention_logic, [
            {'list_group_mentions', 4, fun(_Gid, _Uid, _IsRead, _Options) ->
                {ok, [#{
                    <<"id">> => 99,
                    <<"msg_id">> => <<"m_99">>,
                    <<"group_id">> => 12,
                    <<"from_uid">> => 9,
                    <<"mentioned_uid">> => 100,
                    <<"is_read">> => false,
                    <<"created_at">> => 1700000000
                }]}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Data) ->
                self() ! {resp_data, Data},
                req_ok
            end}
        ]}
    ], fun() ->
        Result = mention_handler:list_mentions(req_mock(), #{current_uid => 100}),
        ?assertEqual(req_ok, Result),
        Data = receive_resp_data(),
        ?assertEqual(1, length(maps:get(items, Data))),
        ?assertEqual(1, length(maps:get(list, Data)))
    end).

mark_read_with_mention_id_fallback_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) -> #{<<"mention_id">> => 99} end}
        ]},
        {mention_logic, [
            {'mark_as_read_by_mention_id', 2, fun(_MentionId, _Uid) ->
                {ok, <<"m_99">>}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Data) ->
                self() ! {resp_data, Data},
                req_ok
            end}
        ]}
    ], fun() ->
        Result = mention_handler:mark_read(req_mock(), #{current_uid => 100}),
        ?assertEqual(req_ok, Result),
        Data = receive_resp_data(),
        ?assertEqual(99, maps:get(<<"mention_id">>, Data)),
        ?assertEqual(<<"m_99">>, maps:get(<<"msg_id">>, Data))
    end).

suggest_accepts_group_id_in_post_and_returns_items_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) -> #{
                <<"group_id">> => <<"g_12">>,
                <<"keyword">> => <<"ab">>
            } end}
        ]},
        {cowboy_req, [
            {'parse_qs', 1, fun(_Req) -> [] end}
        ]},
        {mention_logic, [
            {'get_member_suggestions', 3, fun(_Gid, _Uid, _Keyword) ->
                {ok, [#{<<"id">> => <<"u_1">>}]}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Data) ->
                self() ! {resp_data, Data},
                req_ok
            end}
        ]}
    ], fun() ->
        Result = mention_handler:suggest(req_mock(), #{current_uid => 100}),
        ?assertEqual(req_ok, Result),
        Data = receive_resp_data(),
        ?assertEqual(maps:get(<<"members">>, Data), maps:get(<<"items">>, Data))
    end).

unread_supports_group_filter_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) -> #{<<"group_id">> => <<"g_12">>} end}
        ]},
        {cowboy_req, [
            {'parse_qs', 1, fun(_Req) -> [] end}
        ]},
        {mention_logic, [
            {'count_group_unread', 2, fun(_Uid, _Gid) -> 3 end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Data) ->
                self() ! {resp_data, Data},
                req_ok
            end}
        ]}
    ], fun() ->
        Result = mention_handler:unread(req_mock(), #{current_uid => 100}),
        ?assertEqual(req_ok, Result),
        Data = receive_resp_data(),
        ?assertEqual(3, maps:get(<<"count">>, Data))
    end).

req_mock() ->
    #{mock_req => true}.

receive_resp_msg() ->
    receive
        {resp_msg, Msg} -> Msg
    after 1000 ->
        timeout
    end.

receive_resp_code() ->
    receive
        {resp_code, Code} -> Code
    after 1000 ->
        timeout
    end.

receive_resp_data() ->
    receive
        {resp_data, Data} -> Data
    after 1000 ->
        timeout
    end.

msg_to_binary(Msg) when is_binary(Msg) ->
    Msg;
msg_to_binary(Msg) when is_list(Msg) ->
    unicode:characters_to_binary(Msg);
msg_to_binary(Other) ->
    unicode:characters_to_binary(io_lib:format("~p", [Other])).
