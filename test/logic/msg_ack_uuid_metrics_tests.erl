-module(msg_ack_uuid_metrics_tests).
-include_lib("eunit/include/eunit.hrl").

%% No application or database: exercise the public ACK boundary in an isolated peer.
ack_duration_accepts_uuid_v7_and_legacy_tsid_test_() ->
    {timeout, 30, fun() ->
        pure_mock_fixture:run(fun() ->
            Now = 1800000000000,
            Legacy = 123456789,
            pure_mock_fixture:with_mocks(
                [
                    {msg_operation_ds, [{ack_c2c_msg, 3, fun(_, _, _) -> {ok, 1} end}]},
                    {elib_dt, [{millisecond, 0, fun() -> Now end}]},
                    {elib_tsid, [{timestamp, 1, fun(Id) when Id =:= Legacy -> Now - 2000 end}]},
                    {elib_metric, [
                        {increment, 1, fun(_) -> ok end},
                        {record, 2, fun(_, _) -> ok end}
                    ]},
                    %% V3-24 G-4：ack 成功后 emit_enterprise_delivered 走
                    %% enterprise_webhook_logic:side_effect_tx 的 elib_pg:with_tx
                    %% 独立小事务。显式声明并 stub（返回 ok，不执行 DB Fun）——
                    %% 与产品 fail-safe（异常恒 ok）语义一致；否则 Guards 空 mock
                    %% 下该调用即 undeclared（cleanup fail-closed）。
                    {elib_pg, [{with_tx, 1, fun(_Fun) -> ok end}]}
                ],
                fun() ->
                    UUID = uuid_at(Now - 1000, 7, 2),
                    Ids = [
                        UUID,
                        list_to_binary(string:uppercase(binary_to_list(UUID))),
                        integer_to_binary(Legacy),
                        Legacy
                    ],
                    lists:foreach(
                        fun(Id) ->
                            ok = msg_ack_logic:client_ack(<<"c2c">>, Id, 7, <<"device">>),
                            ?assert(
                                meck:called(msg_operation_ds, ack_c2c_msg, [Id, 7, <<"device">>])
                            )
                        end,
                        Ids
                    ),
                    ?assertEqual(
                        2,
                        meck:num_calls(
                            elib_metric,
                            record,
                            [imboy_msg_deliver_duration_seconds, 1.0]
                        )
                    ),
                    ?assertEqual(
                        2,
                        meck:num_calls(
                            elib_metric,
                            record,
                            [imboy_msg_deliver_duration_seconds, 2.0]
                        )
                    )
                end
            )
        end)
    end}.

ack_duration_rejects_invalid_uuid_and_out_of_window_test_() ->
    {timeout, 30, fun() ->
        pure_mock_fixture:run(fun() ->
            Now = 1800000000000,
            pure_mock_fixture:with_mocks(
                [
                    {msg_operation_ds, [{ack_c2c_msg, 3, fun(_, _, _) -> {ok, 1} end}]},
                    {elib_dt, [{millisecond, 0, fun() -> Now end}]},
                    {elib_metric, [
                        {increment, 1, fun(_) -> ok end},
                        {record, 2, fun(_, _) -> ok end}
                    ]},
                    %% V3-24 G-4 同上：stub elib_pg:with_tx（fail-safe 语义保持）。
                    {elib_pg, [{with_tx, 1, fun(_Fun) -> ok end}]}
                ],
                fun() ->
                    Valid = uuid_at(Now - 1000, 7, 2),
                    <<Prefix:35/binary, _>> = Valid,
                    Ids = [
                        uuid_at(Now - 1000, 4, 2),
                        uuid_at(Now - 1000, 7, 0),
                        uuid_at(Now - 1000, 7, 3),
                        <<Prefix/binary, "z">>,
                        uuid_at(Now + 1, 7, 2),
                        uuid_at(Now - 3600001, 7, 2),
                        <<"d2q9i6rvp1dc73i5hhvg">>,
                        <<"invalid">>
                    ],
                    lists:foreach(
                        fun(Id) ->
                            ok = msg_ack_logic:client_ack(<<"c2c">>, Id, 7, <<"device">>),
                            ?assert(
                                meck:called(msg_operation_ds, ack_c2c_msg, [Id, 7, <<"device">>])
                            )
                        end,
                        Ids
                    ),
                    ?assertEqual(0, meck:num_calls(elib_metric, record, 2)),
                    ?assertEqual(length(Ids), meck:num_calls(elib_metric, increment, 1))
                end
            )
        end)
    end}.

uuid_at(Ts, Version, Variant) ->
    Hex = binary:encode_hex(<<Ts:48, Version:4, 0:12, Variant:2, 0:62>>),
    <<A:8/binary, B:4/binary, C:4/binary, D:4/binary, E:12/binary>> = Hex,
    list_to_binary(
        string:lowercase(
            binary_to_list(
                <<A/binary, "-", B/binary, "-", C/binary, "-", D/binary, "-", E/binary>>
            )
        )
    ).
