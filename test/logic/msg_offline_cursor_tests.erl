-module(msg_offline_cursor_tests).
-include_lib("eunit/include/eunit.hrl").

roundtrip_preserves_microseconds_test() ->
    Cursor = {<<"2026-10-08T00:00:00.123456Z">>, <<"116849000000000002">>},
    ?assertEqual({ok, Cursor}, msg_offline_cursor:decode(msg_offline_cursor:encode(Cursor))).

empty_cursor_is_legacy_test() ->
    ?assertEqual({ok, undefined}, msg_offline_cursor:decode(<<>>)),
    ?assertEqual(<<>>, msg_offline_cursor:encode(undefined)).

malformed_and_unsupported_cursor_test() ->
    Inputs = [
        <<"!">>,
        binary:copy(<<"x">>, 513),
        base64:encode(
            jsone:encode(#{
                <<"v">> => 2, <<"at">> => <<"2026-10-08T00:00:00Z">>, <<"id">> => <<"m1">>
            })
        ),
        base64:encode(
            jsone:encode(#{<<"v">> => 1, <<"at">> => <<"bad-time">>, <<"id">> => <<"m1">>})
        ),
        base64:encode(
            jsone:encode(#{<<"v">> => 1, <<"at">> => <<"2026-10-08T00:00:00Z">>, <<"id">> => <<>>})
        )
    ],
    [
        ?assertEqual({error, invalid_offline_cursor}, msg_offline_cursor:decode(Token))
     || Token <- Inputs
    ].
