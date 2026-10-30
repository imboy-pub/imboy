-module(attachment_lifecycle_v5_w601_w2_confirm_sequence_tests).

-include_lib("eunit/include/eunit.hrl").

-define(S, attachment_lifecycle_v5_w601_w2_support).
-define(F, attachment_w403_http_fixture).
-define(C, attachment_lifecycle_v5_w601_w2_confirm_tests).
-define(TAB, w601_confirm_sequence_test).
-define(BODY, <<"w601 confirmed immutable content">>).
-define(OTHER, <<"w601 overwritten different bytes">>).

old_put_sequence_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            fun first_write_precedes_barrier/0,
            fun backend_failure_is_not_a_conflict/0,
            fun unexpected_provider_write_is_released_and_rejected/0,
            fun changed_pin_is_rejected/0
        ]
    end}.

setup() ->
    ets:new(?TAB, [named_table, public, set]),
    meck:new([?S, ?F], [passthrough, no_link]),
    meck:expect(?F, register, fun(S, Ep, Driver, _) ->
        #{
            fixture => S,
            surface => Ep,
            driver => Driver,
            key => <<"test-key">>,
            client_upload_id => <<"test-cid">>,
            registration_request_id => <<"register">>
        }
    end),
    meck:expect(?F, put, fun(_, Bytes) ->
        case Bytes of
            ?BODY ->
                [] = ets:lookup(?TAB, barrier),
                #{status => 200, request_id => <<"put-first">>};
            ?OTHER ->
                [{barrier, true}] = ets:lookup(?TAB, barrier),
                [{old_status, Status}] = ets:lookup(?TAB, old_status),
                case Status of
                    provider ->
                        [{barrier_ref, Ref}, {parent, Parent}] =
                            [hd(ets:lookup(?TAB, K)) || K <- [barrier_ref, parent]],
                        Parent ! {w601_barrier, Ref, self()},
                        receive
                            {w601_continue, Ref} -> ok
                        after 1000 -> error(not_released)
                        end,
                        #{status => 200, request_id => <<"put-old">>};
                    pin_mutated ->
                        true = ets:insert(?TAB, {pin_mutated, true}),
                        #{status => 409, request_id => <<"put-old">>};
                    _ ->
                        #{status => Status, request_id => <<"put-old">>}
                end
        end
    end),
    meck:expect(?F, confirm, fun(_) ->
        [] = ets:lookup(?TAB, barrier),
        #{status => 200, request_id => <<"confirm">>}
    end),
    meck:expect(?F, bind_request, fun(_, _, _, _) -> ok end),
    meck:expect(?F, success, fun(#{status := Status}) -> Status =:= 200 end),
    meck:expect(?F, read, fun(_) -> {ok, ?BODY} end),
    meck:expect(?S, content_pin_any, fun(_, _) ->
        case ets:lookup(?TAB, pin_mutated) of
            [] -> {ok, #{<<"sha256">> => ?S:sha(?BODY)}};
            _ -> {ok, #{<<"sha256">> => <<"changed">>}}
        end
    end),
    meck:expect(?S, pending_row, fun(_) ->
        {ok, #{
            <<"session_state">> => <<"confirmed">>,
            <<"object_key">> => <<"test-key">>,
            <<"storage_driver">> => <<"disk">>,
            <<"storage_namespace">> => <<"test">>,
            <<"storage_container">> => <<"test">>,
            <<"storage_locator_sha256">> => <<"hash">>
        }}
    end),
    meck:expect(?S, attach_count, fun(_) -> 1 end),
    meck:expect(?S, put_calls, fun(_) -> 0 end),
    meck:expect(?S, with_put_barrier, fun(_, Fun) ->
        Ref = make_ref(),
        true = ets:insert(?TAB, [{barrier, true}, {barrier_ref, Ref}, {parent, self()}]),
        Fun(Ref)
    end),
    ok.

cleanup(_) ->
    meck:unload([?S, ?F]),
    ets:delete(?TAB),
    ok.

first_write_precedes_barrier() ->
    ets:delete(?TAB, barrier),
    ets:insert(?TAB, {old_status, 409}),
    Result = ?C:imm_scenario(
        #{leaf => <<"test">>},
        ordinary,
        disk,
        <<"put_A_confirm_old_capability_put_B">>
    ),
    ?assertEqual(409, maps:get(old_capability_put_status, Result)),
    ?assertEqual(<<"confirm">>, maps:get(confirm_request_id, Result)).

backend_failure_is_not_a_conflict() ->
    ets:delete(?TAB, barrier),
    ets:insert(?TAB, {old_status, 500}),
    ?assertError(
        {assertEqual, _},
        ?C:imm_scenario(
            #{leaf => <<"test">>},
            ordinary,
            disk,
            <<"put_A_confirm_old_capability_put_B">>
        )
    ).

unexpected_provider_write_is_released_and_rejected() ->
    ets:delete(?TAB, barrier),
    ets:insert(?TAB, {old_status, provider}),
    ?assertError(
        old_capability_reached_provider,
        ?C:imm_scenario(
            #{leaf => <<"test">>},
            ordinary,
            disk,
            <<"put_A_confirm_old_capability_put_B">>
        )
    ).

changed_pin_is_rejected() ->
    ets:delete(?TAB, barrier),
    ets:delete(?TAB, pin_mutated),
    ets:insert(?TAB, {old_status, pin_mutated}),
    ?assertError(
        {assertEqual, _},
        ?C:imm_scenario(
            #{leaf => <<"test">>},
            ordinary,
            disk,
            <<"put_A_confirm_old_capability_put_B">>
        )
    ).
