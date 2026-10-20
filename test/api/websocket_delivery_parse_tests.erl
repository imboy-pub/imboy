-module(websocket_delivery_parse_tests).
-include_lib("eunit/include/eunit.hrl").
-include("micro_community_test.hrl").
-include("imboy_frame.hrl").

binary_protocol_bytes_test_() ->
    [{atom_to_list(Protocol) ++ "/" ++ atom_to_list(Framing), fun() ->
        State = #{current_uid => 7, protocol => Protocol, framing => Framing},
        lists:foreach(fun(Bin) ->
            {reply, Actual, State, hibernate} = websocket_handler:websocket_info({reply, Bin}, State),
            Map = jsone:decode(Bin, [{object_format, map}]),
            Expected = case {Protocol, Framing} of
                {_, v2} -> {binary, imboy_codec:wrap_v2_frame(frame_type(Map), 0, Bin)};
                {json, none} -> {text, Bin};
                {protobuf, none} -> try imboy_codec:encode(protobuf, Map) of
                    <<>> -> {text, Bin};
                    Encoded -> {binary, Encoded}
                catch _:_ -> {text, Bin} end
            end,
            ?assertEqual(Expected, Actual)
        end, messages())
    end} || {Protocol, Framing} <- [{json,none},{json,v2},{protobuf,none},{protobuf,v2}]].

one_parse_per_object_callback_test_() ->
    {setup, fun() -> meck:new(jsone, [passthrough, no_link]) end,
     fun(_) -> meck:unload(jsone) end, fun(_) -> ?_test(begin
        Bin = hd(messages()),
        lists:foreach(fun(State) ->
            meck:reset(jsone),
            websocket_handler:websocket_info({reply, Bin}, State#{current_uid => 7}),
            ?assertEqual(1, meck:num_calls(jsone, decode, '_'))
        end, [#{protocol => json}, #{framing => v2}, #{protocol => protobuf}])
     end) end}.

retry_rechecks_access_and_preserves_binary_test_() ->
    ?MC_WITH_MECKS([
        {micro_community_access_repo, [{delivery_allowed, 2, fun(20,7) -> get(allow) end}]},
        {websocket_logic, [{send_next, 6, fun(7, <<"id">>, Bin, [30], [<<"did">>], true) ->
            put(rescheduled, Bin), ok
        end}]}
    ], fun() ->
        Bin = <<"{\"type\":\"C2G\",\"to\":20,\"id\":\"id\",\"conv_seq\":42,\"sender_did\":\"sender\",\"payload\":{\"ciphertext\":\"unchanged.opaque\"}}">>,
        State = #{current_uid => 7, framing => v2}, Key = {7, <<"did">>, <<"id">>},
        put(allow, true), Ref = make_ref(), ack_retry_cache:set(Key, Ref, 10000),
        ?assertMatch({reply, {binary, _}, State, hibernate},
            websocket_handler:websocket_info({timeout, Ref, {[30], Key, Bin}}, State)),
        ?assertEqual(Bin, erase(rescheduled)),
        put(allow, false), Ref2 = make_ref(), ack_retry_cache:set(Key, Ref2, 10000),
        ?assertEqual({ok, State, hibernate},
            websocket_handler:websocket_info({timeout, Ref2, {[30], Key, Bin}}, State)),
        ?assertEqual(undefined, ack_retry_cache:get(Key)),
        ?assertEqual(2, meck:num_calls(micro_community_access_repo, delivery_allowed, [20,7])),
        ?assertEqual(1, meck:num_calls(websocket_logic, send_next, '_')),
        erase(allow)
    end).

acknowledged_and_stale_retry_do_not_send_test() ->
    State = #{current_uid => 7, framing => v2}, Bin = hd(messages()),
    Key = {7, <<"ackdid">>, <<"ackid">>}, Ref = make_ref(),
    AckKey = {ack_received,7,<<"ackdid">>,<<"ackid">>},
    ack_retry_cache:set(Key, Ref, 10000),
    ack_retry_cache:set(AckKey, true, 10000),
    try
        ?assertEqual({ok, State, hibernate}, websocket_handler:websocket_info({timeout,Ref,{[],Key,Bin}},State)),
        ?assertEqual({ok, State, hibernate}, websocket_handler:websocket_info({timeout,Ref,{[],Key,Bin}},State))
    after
        ack_retry_cache:delete_if_value(AckKey, true),
        ack_retry_cache:delete_if_value(Key, Ref)
    end.

malformed_and_non_object_json_test() ->
    State = #{current_uid => 7, framing => v2},
    lists:foreach(fun(Bin) ->
        ?assertEqual({ok, State, hibernate}, websocket_handler:websocket_info({reply,Bin},State))
    end, [<<"invalid">>, <<"null">>, <<"42">>, <<"[true]">>]).

old_and_reused_guard_are_equivalent_test_() ->
    ?MC_WITH_MECKS([
        {micro_community_access_repo, [{delivery_allowed, 2, fun(20,7) -> get(allow) end}]}
    ], fun() ->
        State = #{current_uid => 7},
        Values = messages() ++ [<<"bad">>, <<"null">>, <<"12">>, <<"true">>,
            <<"[{},{}]">>, <<"{\"type\":true}">>, <<"{\"type\":42}">>,
            <<"{\"type\":[]}">>, <<"{\"type\":{}}">>,
            <<"{\"type\":\"C2C\",\"type\":\"C2G\",\"to\":20}">>,
            <<"{\"type\":\"C2G\",\"to\":\"bad-id\"}">>,
            <<"{\"type\":\"S2C\",\"payload\":{\"gid\":20}}">>,
            <<"{\"type\":\"S2C\",\"payload\":[]} ">>,
            <<"{\"type\":\"S2C\",\"payload\":null}">>,
            <<"{\"type\":\"S2C\",\"payload\":\"bad\"}">>,
            #{<<"type">> => <<"C2C">>}, [{<<"type">>, <<"C2C">>}], invalid],
        Continue = fun(I,S) -> {continued,I,S} end,
        ContinueBinary = fun(I,S,_Decoded) -> Continue(I,S) end,
        lists:foreach(fun(Allowed) ->
            put(allow, Allowed),
            lists:foreach(fun(Value) ->
                lists:foreach(fun(Info) ->
                    ?assertEqual(websocket_delivery_guard:run(Info,State,Continue),
                        websocket_delivery_guard:run(Info,State,Continue,ContinueBinary))
                end, [{reply,Value}, {timeout, make_ref(), Value},
                      {timeout,make_ref(),{[],{7,<<"did">>,<<"mid">>},Value}}])
            end, Values)
        end, [true,false]),
        erase(allow),
        ?assertError({badmap,invalid}, websocket_delivery_guard:run({reply,hd(messages())},invalid,Continue)),
        ?assertError({badmap,invalid}, websocket_delivery_guard:run({reply,hd(messages())},invalid,Continue,ContinueBinary))
    end).

messages() ->
    [<<" { \"type\" : \"C2C\", \"id\":\"id\", \"from\":\"8\", \"to\":\"7\", \"sender_did\":\"sender\", \"conv_seq\":42, \"payload\":{\"ciphertext\":\"one.two.three\"}, \"e2ee\":{\"protected_header\":\"opaque\"} } ">>,
     <<"{\"type\":\"S2C\",\"payload\":{}}">>, <<"{\"type\":\"UNKNOWN\"}">>,
     <<"{\"type\":\"C2S\"}">>, <<"{}">>, <<"[]">>, <<"[{}]">>, <<"\"{}\"">>].
frame_type(#{<<"type">> := <<"C2C">>}) -> ?FRAME_TYPE_MSG_C2C;
frame_type(#{<<"type">> := <<"C2G">>}) -> ?FRAME_TYPE_MSG_C2G;
frame_type(#{<<"type">> := <<"C2S">>}) -> ?FRAME_TYPE_MSG_C2S;
frame_type(_) -> ?FRAME_TYPE_MSG_S2C.

encoder_exception_fallback_bytes_test_() ->
    [{atom_to_list(Protocol) ++ "/" ++ atom_to_list(Class), fun() ->
        Bin = hd(messages()),
        State = #{current_uid => 7, protocol => Protocol,
            framing => case Protocol of json -> v2; protobuf -> none end},
        Expected = case Protocol of
            json -> {binary, imboy_codec:wrap_v2_frame(?FRAME_TYPE_MSG_S2C, 0, Bin)};
            protobuf -> {text, Bin}
        end,
        ok = meck:new(imboy_codec, [passthrough, no_link]),
        try
            case Protocol of
                protobuf -> meck:expect(imboy_codec, encode, fun(_, _) ->
                    meck:exception(Class, synthetic_codec_failure) end);
                json -> meck:expect(imboy_codec, wrap_v2_frame, fun
                    (?FRAME_TYPE_MSG_S2C, Flags, Payload) ->
                        meck:passthrough([?FRAME_TYPE_MSG_S2C, Flags, Payload]);
                    (_, _, _) -> meck:exception(Class, synthetic_codec_failure)
                end)
            end,
            ?assertEqual({reply, Expected, State, hibernate},
                websocket_handler:websocket_info({reply, Bin}, State)),
            ?assert(meck:validate(imboy_codec))
        after
            meck:unload(imboy_codec)
        end
    end} || Protocol <- [json, protobuf], Class <- [error, throw, exit]].
