-module(channel_ds_idempotency_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

publish_duplicate_request_does_not_increment_unread_test_() ->
    ?WITH_MECKS(
        [
            {user_repo, [
                {'find_by_id', 2, fun(1001, <<"nickname,avatar">>) ->
                    #{
                        <<"nickname">> => <<"admin">>,
                        <<"avatar">> => <<"avatar">>
                    }
                end}
            ]},
            {jsone_encode, [
                {'encode', 2, fun(#{}, [native_utf8]) -> {ok, <<"{}">>} end}
            ]},
            {elib_dt, [
                {'now', 0, fun() -> <<"2026-08-09T00:00:00Z">> end}
            ]},
            {channel_message_repo, [
                {'add_with_request_id', 2, fun(Data, <<"req-1">>) ->
                    ?assertEqual(11, maps:get(channel_id, Data)),
                    {ok, 99, duplicate}
                end}
            ]},
            {channel_subscription_repo, [
                {'tablename', 0, fun() -> erlang:error(unread_must_not_change) end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {ok, 99, duplicate},
                channel_ds:publish_message(
                    11,
                    1001,
                    <<"hello">>,
                    <<"text">>,
                    #{},
                    <<"req-1">>
                )
            )
        end
    ).
