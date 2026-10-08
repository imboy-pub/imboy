-module(micro_community_websocket_tests).
-include_lib("eunit/include/eunit.hrl").
-include("micro_community_test.hrl").

denied_retry_consumes_only_its_own_reference_test_() ->
    denied_fixture(
        fun({7, <<"did">>, <<"mid">>}, old_ref) ->
            put(cleaned_ref, old_ref),
            true
        end,
        fun() ->
            Info = {timeout, old_ref, {[10], {7, <<"did">>, <<"mid">>}, group_message()}},
            ?assertEqual({ok, state(), hibernate}, websocket_handler:websocket_info(Info, state())),
            ?assertEqual(old_ref, get(cleaned_ref)),
            ?assertEqual(0, meck:num_calls(websocket_logic, send_next, '_'))
        end
    ).

denied_stale_retry_cannot_clear_replacement_timer_test_() ->
    denied_fixture(fun({7, <<"did">>, <<"mid">>}, old_ref) -> false end, fun() ->
        Info = {timeout, old_ref, {[10], {7, <<"did">>, <<"mid">>}, group_message()}},
        ?assertEqual({ok, state(), hibernate}, websocket_handler:websocket_info(Info, state())),
        ?assertEqual(undefined, get(cleaned_ref)),
        ?assertEqual(
            1,
            meck:num_calls(
                ack_retry_cache,
                delete_if_value,
                [{7, <<"did">>, <<"mid">>}, old_ref]
            )
        ),
        ?assertEqual(0, meck:num_calls(websocket_logic, send_next, '_'))
    end).

denied_generic_timeout_has_no_timer_key_to_clear_test_() ->
    denied_fixture(fun(_, _) -> erlang:error(unexpected_cache_delete) end, fun() ->
        ?assertEqual(
            {ok, state(), hibernate},
            websocket_handler:websocket_info({timeout, generic_ref, group_message()}, state())
        ),
        ?assertEqual(0, meck:num_calls(ack_retry_cache, delete_if_value, '_'))
    end).

ordinary_proplist_reply_preserves_json_and_group_array_is_denied_test_() ->
    ?MC_WITH_MECKS(
        [{micro_community_access_repo, [{delivery_allowed, 2, fun(20, 7) -> false end}]}], fun() ->
            Ordinary = [{<<"type">>, <<"C2C">>}, {<<"to">>, 8}],
            {reply, {text, Bin}, _, hibernate} =
                websocket_handler:websocket_info({reply, Ordinary}, state()),
            ?assertEqual(
                <<"C2C">>, maps:get(<<"type">>, jsone:decode(Bin, [{object_format, map}]))
            ),
            ?assertEqual(
                {ok, state(), hibernate},
                websocket_handler:websocket_info(
                    {reply, [#{<<"type">> => <<"C2C">>}, group_message()]}, state()
                )
            ),
            ?assertEqual(
                {ok, state(), hibernate},
                websocket_handler:websocket_info({reply, <<"invalid-json">>}, state())
            )
        end
    ).

denied_fixture(Delete, Test) ->
    ?MC_WITH_MECKS(
        [
            {micro_community_access_repo, [{delivery_allowed, 2, fun(20, 7) -> false end}]},
            {ack_retry_cache, [{delete_if_value, 2, Delete}]},
            {websocket_logic, [
                {send_next, 6, fun(_, _, _, _, _, _) -> erlang:error(unexpected_reschedule) end}
            ]}
        ],
        fun() ->
            erase(cleaned_ref),
            try
                Test()
            after
                erase(cleaned_ref)
            end
        end
    ).

state() -> #{current_uid => 7}.
group_message() -> #{<<"type">> => <<"C2G">>, <<"to">> => 20}.
