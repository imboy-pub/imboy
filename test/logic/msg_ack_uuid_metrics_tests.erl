-module(msg_ack_uuid_metrics_tests).
-include_lib("eunit/include/eunit.hrl").

ack_metrics_require_trusted_acceptance_test_() ->
    {timeout, 30, fun() ->
        pure_mock_fixture:run(fun() ->
            Now = 1800000000000,
            UUID = <<"01a3185c-3c78-7000-8000-000000000000">>,
            pure_mock_fixture:with_mocks(
                [
                    {elib_pg, [{with_tx, 1, fun(_) -> {error, no_pg_in_isolated_peer} end}]},
                    {msg_operation_ds, [
                        {ack_c2c_msg, 3, fun
                            (<<"duplicate">>, _, _) -> {ok, 0};
                            (<<"failed">>, _, _) -> {error, <<"persistence_failed">>};
                            (_, _, _) -> {ok, 1}
                        end}
                    ]},
                    {msg_store_repo, [
                        {find_by_msg_id, 2, fun
                            (_, <<"staged">>) ->
                                put(be11_ack_clock_ms, Now + 5000),
                                row(7, Now - 1000);
                            (_, <<"duplicate">>) ->
                                row(7, Now - 1000);
                            (_, <<"failed">>) ->
                                row(7, Now - 1000);
                            (_, <<"foreign">>) ->
                                row(8, Now - 1000);
                            (_, <<"read-error">>) ->
                                erlang:error(read_unavailable);
                            (_, _) ->
                                {error, not_found}
                        end},
                        {resolve_accepted_ms, 1, fun
                            (#{<<"accepted_at">> := null, <<"server_ts">> := Ts}) ->
                                {ok, Ts, low_trust};
                            (#{<<"accepted_at">> := Ts}) ->
                                {ok, Ts, trusted}
                        end}
                    ]},
                    {msg_c2c_repo, [
                        {read_msg, 4, fun
                            (_, _, _, [<<"persisted">>, 7]) ->
                                formal_row(7, Now - 2000);
                            (_, _, _, [<<"future">>, 7]) ->
                                formal_row(7, Now + 1);
                            (_, _, _, [<<"too-old">>, 7]) ->
                                formal_row(7, Now - 3600001);
                            (_, _, _, [<<"legacy">>, 7]) ->
                                {ok, [
                                    #{
                                        <<"to_id">> => 7,
                                        <<"accepted_at">> => null,
                                        <<"server_ts">> => Now - 3000
                                    }
                                ]};
                            (_, _, _, _) ->
                                {ok, []}
                        end}
                    ]},
                    {elib_dt, [
                        {millisecond, 0, fun() ->
                            case get(be11_ack_clock_ms) of
                                undefined -> Now;
                                Changed -> Changed
                            end
                        end},
                        {to_rfc3339, 2, fun(_, _) -> <<"2026-11-05T00:00:00Z">> end}
                    ]},
                    {elib_metric, [
                        {increment, 1, fun(_) -> ok end},
                        {increment, 3, fun(_, _, _) -> ok end},
                        {record, 2, fun(_, _) -> ok end}
                    ]}
                ],
                fun() ->
                    Ids = [
                        <<"staged">>,
                        <<"persisted">>,
                        <<"duplicate">>,
                        <<"foreign">>,
                        <<"read-error">>,
                        <<"future">>,
                        <<"too-old">>,
                        <<"legacy">>,
                        UUID
                    ],
                    lists:foreach(
                        fun(Id) ->
                            ok = ack(Id),
                            ?assert(
                                meck:called(msg_operation_ds, ack_c2c_msg, [Id, 7, <<"device">>])
                            )
                        end,
                        Ids
                    ),
                    ?assertEqual({error, <<"persistence_failed">>}, ack(<<"failed">>)),
                    Metric = imboy_msg_deliver_duration_seconds,
                    ?assertEqual(2, meck:num_calls(elib_metric, record, 2)),
                    ?assert(meck:called(elib_metric, record, [Metric, 1.0])),
                    ?assert(meck:called(elib_metric, record, [Metric, 2.0]))
                end
            )
        end)
    end}.

ack(Id) ->
    erase(be11_ack_clock_ms),
    msg_ack_logic:client_ack(<<"c2c">>, Id, 7, <<"device">>).

row(Uid, AcceptedAt) ->
    {ok, #{<<"to_id">> => Uid, <<"accepted_at">> => AcceptedAt}}.

formal_row(Uid, AcceptedAt) ->
    {ok, [#{<<"to_id">> => Uid, <<"accepted_at">> => AcceptedAt}]}.
