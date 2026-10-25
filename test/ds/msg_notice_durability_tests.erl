-module(msg_notice_durability_tests).
-include_lib("eunit/include/eunit.hrl").

read_notification_failure_test_() ->
    [
        read_case(Sync, Notice)
     || {Sync, Notice} <- [
            {ok, ok}, {{error, unavailable}, ok}, {ok, {error, timeout}}
        ]
    ].
read_case(Sync, Notice) ->
    {timeout, 45, fun() ->
        pure_mock_fixture:run(fun() ->
            pure_mock_fixture:with_mocks(
                [
                    {friend_ds, [{check_relationship, 2, fun(_, _) -> {true, false} end}]},
                    {msg_read_ds, [{save_read, 5, fun(_, _, _, _, _) -> ok end}]},
                    {msg_s2c_ds, [{send, 7, fun(_, _, _, _, _, _, _) -> Sync end}]},
                    {user_logic, [{is_online, 1, fun(_) -> true end}]},
                    {msg_operation_ds, [{register_read_notice, 4, fun(_, _, _, _) -> Notice end}]},
                    {imboy_message_helper, [{encode_and_send, 4, fun(_, _, _, _) -> ok end}]},
                    {message_ds, [
                        {assemble_s2c, 3, fun(Id, Action, _) ->
                            #{<<"id">> => Id, <<"action">> => Action}
                        end}
                    ]}
                ],
                fun() ->
                    Data = #{
                        <<"from">> => <<"9">>,
                        <<"to">> => <<"8">>,
                        <<"payload">> => #{<<"msg_ids">> => [<<"business">>]}
                    },
                    {reply, R} = msg_c2c_logic:c2c_read(<<"ReadId">>, 9, Data),
                    ?assertEqual(1, meck:num_calls(msg_read_ds, save_read, 5)),
                    case {Sync, Notice} of
                        {ok, ok} ->
                            ?assertEqual(<<"message_read_ack">>, maps:get(<<"action">>, R)),
                            ?assertEqual(
                                1, meck:num_calls(imboy_message_helper, encode_and_send, 4)
                            );
                        _ ->
                            ?assertEqual(<<"internal_error">>, maps:get(<<"action">>, R)),
                            ?assertEqual(
                                0, meck:num_calls(imboy_message_helper, encode_and_send, 4)
                            )
                    end
                end
            )
        end)
    end}.

nosave_real_publish_result_continues_all_recipients_test_() ->
    {timeout, 45, fun() ->
        pure_mock_fixture:run(fun() ->
            pure_mock_fixture:with_mocks(
                [
                    {elib_id, [{gen, 1, fun(_) -> <<"notice-id">> end}]},
                    {imboy_syn, [{publish, 2, fun(_, _) -> {ok, 1} end}]}
                ],
                fun() ->
                    ?assertEqual(
                        ok, msg_s2c_ds:send(0, [8, 9], <<"pin">>, <<>>, null, #{}, no_save)
                    ),
                    ?assertEqual(1, meck:num_calls(imboy_syn, publish, [8, '_'])),
                    ?assertEqual(1, meck:num_calls(imboy_syn, publish, [9, '_']))
                end
            )
        end)
    end}.
