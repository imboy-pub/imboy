-module(group_vote_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% group_vote_handler 参数校验与错误映射回归测试
%%%===================================================================

create_missing_gid_returns_validation_error_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"title">> => <<"午餐投票"/utf8>>,
                    <<"options">> => [#{<<"option_text">> => <<"火锅"/utf8>>}]
                }
            end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<>>) -> 0 end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) ->
                self() ! {resp_msg, Msg},
                req_error
            end}
        ]}
    ], fun() ->
        Result = group_vote_handler:handle_action(create, req_mock(), #{current_uid => 101}),
        ?assertEqual(req_error, Result),
        ?assertEqual(<<"gid 必须"/utf8>>, msg_to_binary(receive_resp_msg()))
    end).

create_invalid_vote_type_returns_validation_error_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"gid">> => <<"g1">>,
                    <<"title">> => <<"午餐投票"/utf8>>,
                    <<"options">> => [#{<<"option_text">> => <<"火锅"/utf8>>}],
                    <<"vote_type">> => 9
                }
            end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(_Any) -> 1 end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) ->
                self() ! {resp_msg, Msg},
                req_error
            end}
        ]}
    ], fun() ->
        Result = group_vote_handler:handle_action(create, req_mock(), #{current_uid => 101}),
        ?assertEqual(req_error, Result),
        ?assertEqual(<<"vote_type 必须是 1 或 2"/utf8>>, msg_to_binary(receive_resp_msg()))
    end).

cast_missing_option_ids_returns_validation_error_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"vote_id">> => <<"vote_1">>}
            end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) ->
                self() ! {resp_msg, Msg},
                req_error
            end}
        ]}
    ], fun() ->
        Result = group_vote_handler:handle_action(cast, req_mock(), #{current_uid => 101}),
        ?assertEqual(req_error, Result),
        ?assertEqual(<<"option_ids 必须"/utf8>>, msg_to_binary(receive_resp_msg()))
    end).

cast_vote_closed_maps_error_message_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"vote_id">> => <<"vote_1">>,
                    <<"option_ids">> => [<<"opt_1">>]
                }
            end}
        ]},
        {group_vote_logic, [
            {'cast_vote', 3, fun(_VoteId, _Uid, _OptionIds) ->
                {error, vote_is_closed}
            end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) ->
                self() ! {resp_msg, Msg},
                req_error
            end}
        ]}
    ], fun() ->
        Result = group_vote_handler:handle_action(cast, req_mock(), #{current_uid => 101}),
        ?assertEqual(req_error, Result),
        ?assertEqual(<<"投票已结束"/utf8>>, msg_to_binary(receive_resp_msg()))
    end).

close_vote_not_found_maps_error_message_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"vote_id">> => <<"vote_404">>}
            end}
        ]},
        {group_vote_logic, [
            {'close_vote', 1, fun(_VoteId) ->
                {error, vote_not_found}
            end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) ->
                self() ! {resp_msg, Msg},
                req_error
            end}
        ]}
    ], fun() ->
        Result = group_vote_handler:handle_action(close, req_mock(), #{current_uid => 101}),
        ?assertEqual(req_error, Result),
        ?assertEqual(<<"投票不存在"/utf8>>, msg_to_binary(receive_resp_msg()))
    end).

req_mock() ->
    #{mock_req => true}.

receive_resp_msg() ->
    receive
        {resp_msg, Msg} -> Msg
    after 1000 ->
        timeout
    end.

msg_to_binary(Msg) when is_binary(Msg) ->
    Msg;
msg_to_binary(Msg) when is_list(Msg) ->
    unicode:characters_to_binary(Msg);
msg_to_binary(Other) ->
    unicode:characters_to_binary(io_lib:format("~p", [Other])).
