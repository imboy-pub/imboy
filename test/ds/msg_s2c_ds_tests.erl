-module(msg_s2c_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

encoded_uid(Uid) ->
    list_to_binary(["uid:", integer_to_list(Uid)]).

module_loaded_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual({module, msg_s2c_ds}, code:ensure_loaded(msg_s2c_ds)),
        ?assertMatch({file, _}, code:is_loaded(msg_s2c_ds))
    end).

write_msg_extracts_action_and_msg_type_from_payload_test_() ->
    CreatedAt = <<"2026-03-16T00:00:00Z">>,
    MsgId = <<"msg_s2c_123">>,
    FromId = 0,
    ToId = 1,
    ServerTS = <<"2026-03-16T00:00:01Z">>,
    Payload = #{
        <<"action">> => <<"notification">>,
        <<"msg_type">> => <<"system">>,
        <<"content">> => <<"System notification">>
    },
    PayloadJson = <<"payload-json">>,
    ?WITH_MECKS(
        [
            {jsone, [
                {'encode', 2, fun(Payload, [native_utf8]) -> PayloadJson end},
                {'decode', 1, fun(PayloadJson) -> Payload end}
            ]},
            {msg_s2c_repo, [
                {'count_by_to_id', 1, fun(ToId) -> 0 end},
                {'write_msg', 9, fun(
                    CreatedAt,
                    MsgId,
                    PayloadJson,
                    FromId,
                    ToId,
                    ServerTS,
                    <<"notification">>,
                    <<"system">>,
                    null
                ) ->
                    {ok, saved}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {ok, saved},
                msg_s2c_ds:write_msg(CreatedAt, MsgId, Payload, FromId, ToId, ServerTS)
            )
        end
    ).

read_msg_returns_processed_rows_test_() ->
    ToUid = 1,
    Limit = 20,
    Row = #{
        <<"id">> => 100,
        <<"payload">> => <<"payload-json">>,
        <<"from_id">> => 0,
        <<"to_id">> => ToUid,
        <<"created_at">> => <<"2026-03-16T00:00:00Z">>,
        <<"server_ts">> => <<"2026-03-16T00:00:01Z">>,
        <<"msg_id">> => <<"msg_s2c_123">>,
        <<"msg_type">> => <<"system">>,
        <<"action">> => <<"notification">>,
        <<"e2ee">> => null
    },
    ?WITH_MECKS(
        [
            {msg_s2c_repo, [
                {'read_msg', 4, fun(_Where, [ToUid], _Column, Limit) ->
                    {ok, [Row]}
                end}
            ]},
            {elib_response, [
                {'json_decode_field', 2, fun
                    (Row, <<"payload">>) ->
                        Row#{
                            <<"payload">> => #{
                                <<"payload">> => #{<<"content">> => <<"System notification">>}
                            }
                        };
                    (Row, <<"e2ee">>) ->
                        Row
                end}
            ]}
        ],
        fun() ->
            [Msg] = msg_s2c_ds:read_msg(ToUid, Limit),
            ?assertEqual(<<"msg_s2c_123">>, maps:get(<<"id">>, Msg)),
            ?assertEqual(false, maps:is_key(<<"msg_id">>, Msg)),
            ?assertEqual(
                #{<<"content">> => <<"System notification">>},
                maps:get(<<"payload">>, Msg)
            )
        end
    ).

delete_msg_returns_repo_result_test_() ->
    MsgId = <<"msg_to_delete_123">>,
    ?WITH_MECK(
        msg_s2c_repo,
        [
            {'delete_msg', 1, fun(MsgId) -> {ok, 1} end}
        ],
        fun() ->
            ?assertEqual({ok, 1}, msg_s2c_ds:delete_msg(MsgId))
        end
    ).

send_nosave_uses_current_send_7_contract_test_() ->
    FromId = 0,
    ToUid = 1,
    Action = <<"notification">>,
    MsgType = <<"system">>,
    Payload = #{<<"content">> => <<"Test">>},
    MsgId = <<"s2c_msg_1">>,
    EncodedMsg = <<"encoded-msg">>,
    Msg = #{<<"id">> => MsgId},
    EncodedFrom = encoded_uid(FromId),
    EncodedTo = encoded_uid(ToUid),
    ?WITH_MECKS(
        [
            {elib_id, [
                {'gen', 1, fun("s2c") -> MsgId end}
            ]},
            {message_ds, [
                {'assemble_msg', 8, fun(
                    <<"S2C">>,
                    EncodedFrom,
                    EncodedTo,
                    Payload,
                    MsgId,
                    MsgType,
                    Action,
                    null
                ) ->
                    Msg
                end}
            ]},
            {jsone, [
                {'encode', 2, fun(Msg, [native_utf8]) -> EncodedMsg end}
            ]},
            {imboy_syn, [
                {'publish', 2, fun(ToUid, EncodedMsg) -> ok end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                ok, msg_s2c_ds:send(FromId, [ToUid], Action, MsgType, null, Payload, nosave)
            ),
            ?assert(meck:called(imboy_syn, publish, [ToUid, EncodedMsg]))
        end
    ).

send_save_persists_payload_and_schedules_retry_test_() ->
    FromId = 0,
    ToUid = 1,
    Action = <<"notification">>,
    MsgType = <<"system">>,
    Payload = #{<<"content">> => <<"Persist me">>},
    MsgId = <<"s2c_msg_2">>,
    EncodedMsg = <<"encoded-msg">>,
    PayloadJson = <<"payload-json">>,
    CreatedAt = <<"2026-03-16T00:00:00Z">>,
    Msg = #{<<"id">> => MsgId},
    EncodedFrom = encoded_uid(FromId),
    EncodedTo = encoded_uid(ToUid),
    ?WITH_MECKS(
        [
            {elib_id, [
                {'gen', 1, fun("s2c") -> MsgId end}
            ]},
            {message_ds, [
                {'assemble_msg', 8, fun(
                    <<"S2C">>,
                    EncodedFrom,
                    EncodedTo,
                    Payload,
                    MsgId,
                    MsgType,
                    Action,
                    null
                ) ->
                    Msg
                end},
                {'send_next', 4, fun(ToUid, MsgId, EncodedMsg, [0, 1000000, 1000000]) -> ok end}
            ]},
            {jsone, [
                {'encode', 2, fun(Value, [native_utf8]) ->
                    case Value of
                        #{<<"id">> := MsgId} -> EncodedMsg;
                        Payload -> PayloadJson
                    end
                end}
            ]},
            {elib_dt, [
                {'now', 0, fun() -> CreatedAt end}
            ]},
            {msg_s2c_repo, [
                {'count_by_to_id', 1, fun(ToUid) -> 0 end},
                {'write_msg', 9, fun(
                    CreatedAt,
                    MsgId,
                    PayloadJson,
                    FromId,
                    ToUid,
                    CreatedAt,
                    Action,
                    MsgType,
                    null
                ) ->
                    {ok, saved}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                ok, msg_s2c_ds:send(FromId, [ToUid], Action, MsgType, null, Payload, save)
            ),
            ?assert(
                meck:called(message_ds, send_next, [ToUid, MsgId, EncodedMsg, [0, 1000000, 1000000]])
            )
        end
    ).
