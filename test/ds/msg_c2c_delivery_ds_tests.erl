-module(msg_c2c_delivery_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

only_authenticated_sender_other_online_devices_test_() ->
    ?WITH_MECKS(
        [
            {imboy_syn, [
                {list_by_uid, 1, fun(100) ->
                    [
                        {self(), {<<"android">>, <<"original">>}},
                        {self(), {<<"macos">>, <<"other">>}},
                        {self(), {<<"macos">>, <<"other">>}},
                        {self(), {<<"android">>, <<"no-envelope">>}}
                    ]
                end}
            ]},
            {elib_retry_config, [{intervals, 1, fun(<<"c2c">>) -> [0, 10] end}]},
            {message_ds, [
                {send_next, 6, fun(100, <<"mid">>, Bytes, [0, 10], [<<"other">>], true) ->
                    Decoded = jsone:decode(Bytes),
                    ?assertEqual(200, maps:get(<<"to">>, Decoded)),
                    ?assertEqual(<<"original">>, maps:get(<<"sender_did">>, Decoded)),
                    ?assertEqual(maps:get(<<"e2ee">>, message()), maps:get(<<"e2ee">>, Decoded)),
                    ok
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                ok,
                msg_c2c_delivery_ds:send_other_devices(100, <<"original">>, <<"mid">>, message())
            ),
            ?assertEqual(1, meck:num_calls(message_ds, send_next, 6))
        end
    ).

production_retry_dispatch_uses_nonempty_whitelist_test_() ->
    ?WITH_MECKS(
        retry_mocks(),
        fun() ->
            erase(delivered_bytes),
            ?assertEqual(
                ok,
                msg_c2c_delivery_ds:send_other_devices(
                    100, <<"original">>, <<"mid">>, message()
                )
            ),
            Bytes = get(delivered_bytes),
            ?assert(is_binary(Bytes)),
            ?assertEqual(message(), jsone:decode(Bytes)),
            ?assertEqual(1, meck:num_calls(imboy_syn, start_delivery_timer, 3))
        end
    ).

retry_mocks() ->
    [
        {imboy_syn, [
            {list_by_uid, 1, fun(100) ->
                [
                    {self(), {<<"android">>, <<"original">>}},
                    {self(), {<<"macos">>, <<"other">>}},
                    {self(), {<<"android">>, <<"no-envelope">>}}
                ]
            end},
            {start_delivery_timer, 3, fun(0, _Pid, Bytes) ->
                put(delivered_bytes, Bytes),
                make_ref()
            end}
        ]},
        {elib_retry_config, [{intervals, 1, fun(<<"c2c">>) -> [0] end}]},
        {ack_retry_cache, [
            {get, 1, fun({ack_received, 100, <<"other">>, <<"mid">>}) ->
                not_found
            end}
        ]},
        {elib_metric, [{increment, 1, fun(_) -> ok end}]},
        {elib_log, [
            {internal_log, 4, fun(_, _, _, _) -> ok end},
            {internal_log, 5, fun(_, _, _, _, _) -> ok end}
        ]}
    ].

no_eligible_online_device_does_not_broadcast_test_() ->
    ?WITH_MECKS(
        [
            {imboy_syn, [
                {list_by_uid, 1, fun(100) ->
                    [
                        {self(), {<<"android">>, <<"original">>}},
                        {self(), {<<"android">>, <<"no-envelope">>}}
                    ]
                end}
            ]},
            {message_ds, [
                {send_next, 6, fun(_, _, _, _, _, _) ->
                    error(empty_whitelist_broadcast)
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                ok,
                msg_c2c_delivery_ds:send_other_devices(
                    100, <<"original">>, <<"mid">>, message()
                )
            ),
            ?assertEqual(0, meck:num_calls(message_ds, send_next, 6))
        end
    ).

unsupported_or_unbound_sender_never_fans_out_test_() ->
    [
        skip_case(Msg)
     || Msg <- [
            maps:remove(<<"e2ee">>, message()),
            (message())#{<<"e2ee">> => #{}},
            (message())#{<<"e2ee">> => #{<<"version">> => 2}},
            (message())#{<<"e2ee">> => <<"unknown-legacy">>},
            (plain_message())#{<<"from">> => 999},
            (plain_message())#{<<"sender_did">> => <<"spoofed">>},
            (plain_message())#{<<"sender_did">> => <<>>},
            maps:remove(<<"sender_did">>, plain_message()),
            (message())#{<<"from">> => 999},
            (message())#{<<"sender_did">> => <<"spoofed">>},
            (message())#{<<"type">> => <<"C2G">>}
        ]
    ].

skip_case(Msg) ->
    ?WITH_MECK(
        imboy_syn,
        [{list_by_uid, 1, fun(_) -> error(unexpected_owner_lookup) end}],
        fun() ->
            ?assertEqual(
                ok, msg_c2c_delivery_ds:send_other_devices(100, <<"original">>, <<"mid">>, Msg)
            ),
            ?assertEqual(
                ok, msg_c2c_delivery_ds:send_other_devices(100, <<"original">>, <<"mid">>, Msg, true)
            ),
            ?assertEqual(0, meck:num_calls(imboy_syn, list_by_uid, 1))
        end
    ).

message() ->
    #{
        <<"type">> => <<"C2C">>,
        <<"from">> => 100,
        <<"to">> => 200,
        <<"id">> => <<"mid">>,
        <<"sender_did">> => <<"original">>,
        <<"payload">> => <<>>,
        <<"e2ee">> => #{
            <<"meta_version">> => 3,
            <<"fan_out">> => <<"per_device">>,
            <<"devices">> => #{
                <<"original">> => #{<<"ciphertext">> => <<"self">>},
                <<"other">> => #{<<"ciphertext">> => <<"own-other">>},
                <<"foreign">> => #{<<"ciphertext">> => <<"peer">>}
            }
        }
    }.

plaintext_only_explicit_null_preserves_recipient_and_excludes_sender_device_test_() ->
    ?WITH_MECKS(
        [
            {elib_retry_config, [{intervals, 1, fun(<<"c2c">>) -> [0] end}]},
            {message_ds, [{send_next, 6,
                fun(100, <<"mid">>, Bytes, [0], [<<"original">>], false) ->
                    ?assertEqual(plain_message(), jsone:decode(Bytes)),
                    ok
                end
            }]}
        ],
        fun() ->
            ?assertEqual(ok, msg_c2c_delivery_ds:send_other_devices(
                100, <<"original">>, <<"mid">>, plain_message(), true
            )),
            ?assertEqual(1, meck:num_calls(message_ds, send_next, 6))
        end
    ).

plaintext_production_retry_excludes_original_device_test_() ->
    ?WITH_MECKS(
        retry_mocks(),
        fun() ->
            erase(delivered_bytes),
            meck:expect(imboy_syn, list_by_uid, fun(100) -> [
                {self(), {<<"android">>, <<"original">>}},
                {self(), {<<"macos">>, <<"other">>}}
            ] end),
            ?assertEqual(ok, msg_c2c_delivery_ds:send_other_devices(
                100, <<"original">>, <<"mid">>, plain_message(), true
            )),
            Bytes = get(delivered_bytes),
            ?assert(is_binary(Bytes)),
            ?assertEqual(plain_message(), jsone:decode(Bytes)),
            ?assertEqual(1, meck:num_calls(imboy_syn, start_delivery_timer, 3))
        end
    ).

plain_message() ->
    (message())#{<<"e2ee">> => null, <<"payload">> => #{<<"text">> => <<"owned-test">>}}.

plaintext_without_trusted_source_never_sends_test_() ->
    ?WITH_MECK(message_ds,
        [{send_next, 6, fun(_, _, _, _, _, _) -> error(untrusted_plain_echo) end}],
        fun() ->
            ?assertEqual(ok, msg_c2c_delivery_ds:send_other_devices(
                100, <<"original">>, <<"mid">>, plain_message())),
            ?assertEqual(ok, msg_c2c_delivery_ds:send_other_devices(
                100, <<"original">>, <<"mid">>, plain_message(), false)),
            ?assertEqual(ok, msg_c2c_delivery_ds:send_other_devices(
                100, <<"original">>, <<"mid">>, plain_message(), <<"true">>)),
            ?assertEqual(0, meck:num_calls(message_ds, send_next, 6))
        end).
