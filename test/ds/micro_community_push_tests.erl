-module(micro_community_push_tests).
-include_lib("eunit/include/eunit.hrl").
-include("micro_community_test.hrl").

initial_denial_never_calls_provider_test_() ->
    fixture(fun(20, 7) -> false end, fun() ->
        schedule_group(),
        ?assertEqual(ok, fire_retry()),
        ?assertEqual(0, provider_calls())
    end).

retry_rechecks_revocation_after_first_provider_attempt_test_() ->
    fixture(fun(20, 7) -> get(eligible) =:= true end, fun() ->
        put(eligible, true),
        meck:expect(
            push_provider_jpush,
            send,
            fun(<<"isolated-token">>, <<"title">>, <<"body">>, #{}) -> {error, rate_limited} end
        ),
        schedule_group(),
        ?assertError({push_failed, rate_limited}, fire_retry()),
        ?assertEqual(1, provider_calls()),
        put(eligible, false),
        ?assertEqual(ok, fire_retry()),
        ?assertEqual(1, provider_calls()),
        ?assertEqual(2, meck:num_calls(micro_community_access_repo, delivery_allowed, [20, 7]))
    end).

database_exception_never_calls_provider_test_() ->
    fixture(fun(20, 7) -> erlang:error(db_unavailable) end, fun() ->
        schedule_group(),
        ?assertEqual(ok, fire_retry()),
        ?assertEqual(0, provider_calls())
    end).

eligible_group_send_keeps_group_identity_out_of_provider_data_test_() ->
    fixture(fun(20, 7) -> true end, fun() ->
        schedule_group(),
        ?assertEqual(ok, fire_retry()),
        ?assertEqual(
            1,
            meck:num_calls(
                push_provider_jpush,
                send,
                [<<"isolated-token">>, <<"title">>, <<"body">>, #{}]
            )
        )
    end).

ordinary_send_preserves_unguarded_compatibility_test_() ->
    fixture(fun(_, _) -> erlang:error(unexpected_acl) end, fun() ->
        ?assertEqual(ok, push_notification_ds:send_to_users([7], <<"title">>, <<"body">>)),
        ?assertEqual(ok, fire_retry()),
        ?assertEqual(1, provider_calls()),
        ?assertEqual(0, meck:num_calls(micro_community_access_repo, delivery_allowed, '_'))
    end).

fixture(Guard, Test) ->
    ?MC_WITH_MECKS(
        [
            {push_token_repo, [
                {list_by_uids, 1, fun([7]) ->
                    {ok, [
                        #{
                            <<"user_id">> => 7,
                            <<"platform">> => <<"jpush">>,
                            <<"token">> => <<"isolated-token">>
                        }
                    ]}
                end}
            ]},
            {micro_community_access_repo, [{delivery_allowed, 2, Guard}]},
            {elib_async, [
                {async_retry, 3, fun(Callback, 2, 3000) ->
                    put(push_retry_callback, Callback),
                    ok
                end}
            ]},
            {push_provider_jpush, [
                {send, 4, fun(<<"isolated-token">>, <<"title">>, <<"body">>, Data) ->
                    ?assertEqual(#{}, Data),
                    ok
                end}
            ]}
        ],
        fun() ->
            erase(push_retry_callback),
            erase(eligible),
            try
                Test()
            after
                erase(push_retry_callback),
                erase(eligible)
            end
        end
    ).

schedule_group() ->
    ?assertEqual(ok, push_notification_ds:send_to_group_users(20, [7], <<"title">>, <<"body">>)),
    ?assertEqual(0, provider_calls()).

fire_retry() -> (get(push_retry_callback))().
provider_calls() -> meck:num_calls(push_provider_jpush, send, '_').
