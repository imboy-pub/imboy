-module(bot_webhook_guard_tests).

-include_lib("eunit/include/eunit.hrl").

private_and_reserved_ipv4_test_() ->
    Addresses = [
        {0, 1, 2, 3},
        {10, 1, 2, 3},
        {100, 64, 0, 1},
        {100, 127, 255, 254},
        {127, 0, 0, 1},
        {169, 254, 1, 1},
        {172, 16, 0, 1},
        {172, 31, 255, 254},
        {192, 0, 0, 1},
        {192, 0, 2, 1},
        {192, 168, 1, 1},
        {198, 18, 0, 1},
        {198, 19, 255, 254},
        {198, 51, 100, 1},
        {203, 0, 113, 1},
        {224, 0, 0, 1},
        {239, 255, 255, 255},
        {240, 0, 0, 1},
        {255, 255, 255, 255}
    ],
    [?_assert(bot_webhook_guard:is_private_ip(Address)) || Address <- Addresses].

public_ipv4_test_() ->
    Addresses = [
        {1, 1, 1, 1},
        {8, 8, 8, 8},
        {93, 184, 216, 34},
        {100, 63, 255, 255},
        {100, 128, 0, 1},
        {172, 15, 255, 255},
        {172, 32, 0, 1},
        {198, 20, 0, 1},
        {223, 255, 255, 255}
    ],
    [?_assertNot(bot_webhook_guard:is_private_ip(Address)) || Address <- Addresses].

ipv6_is_fail_closed_test_() ->
    Addresses = [
        {0, 0, 0, 0, 0, 0, 0, 1},
        {16#fc00, 0, 0, 0, 0, 0, 0, 1},
        {16#fe80, 0, 0, 0, 0, 0, 0, 1},
        {16#2001, 16#4860, 16#4860, 0, 0, 0, 0, 16#8888}
    ],
    [?_assert(bot_webhook_guard:is_private_ip(Address)) || Address <- Addresses].

pinned_target_validation_test() ->
    ?assertMatch(
        {ok, #{host := <<"example.com">>, ip := {93, 184, 216, 34}}},
        bot_webhook_guard:validate_pinned(
            <<"https://example.com/hook">>,
            <<"93.184.216.34">>
        )
    ),
    ?assertEqual(
        {error, forbidden_host},
        bot_webhook_guard:validate_pinned(<<"https://example.com/hook">>, <<"239.1.2.3">>)
    ),
    ?assertEqual(
        {error, invalid_pin},
        bot_webhook_guard:validate_pinned(<<"https://example.com/hook">>, <<"not-an-ip">>)
    ).
