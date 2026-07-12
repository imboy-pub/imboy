-module(rtc_room_logic_tests).

%%%
% rtc_room_logic 单元测试
% Unit tests for rtc_room_logic (LiveKit token issuing)
%%%

-include_lib("eunit/include/eunit.hrl").

-define(TEST_SECRET, <<"testsecret_at_least_32_chars_long_x">>).

setup() ->
    application:set_env(imboy, livekit, #{
        ws_url => <<"wss://test.example.com/livekit">>,
        api_key => <<"testkey">>,
        api_secret => ?TEST_SECRET
    }),
    meck:new(group_member_ds, [passthrough]),
    meck:new(friend_ds, [passthrough]),
    ok.

cleanup(_) ->
    meck:unload(group_member_ds),
    meck:unload(friend_ds),
    ok.

rtc_room_logic_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        fun test_group_member_ok/0,
        fun test_group_non_member_rejected/0,
        fun test_c2c_friend_ok/0,
        fun test_c2c_stranger_rejected/0,
        fun test_c2c_room_name_symmetric/0,
        fun test_unknown_kind_rejected/0,
        fun test_token_claims/0
    ]}.

test_group_member_ok() ->
    meck:expect(group_member_ds, is_member, fun(100, 1) -> true end),
    {ok, Grant} = rtc_room_logic:join(1, <<"did1">>, <<"group">>, 100),
    ?assertEqual(<<"rtc_group_100">>, maps:get(<<"room_name">>, Grant)),
    ?assertEqual(<<"wss://test.example.com/livekit">>, maps:get(<<"ws_url">>, Grant)),
    ?assert(is_binary(maps:get(<<"token">>, Grant))).

test_group_non_member_rejected() ->
    meck:expect(group_member_ds, is_member, fun(100, 2) -> false end),
    ?assertMatch({error, _}, rtc_room_logic:join(2, <<"did1">>, <<"group">>, 100)).

test_c2c_friend_ok() ->
    meck:expect(friend_ds, is_friend, fun(1, 2) -> true end),
    {ok, Grant} = rtc_room_logic:join(1, <<"did1">>, <<"c2c">>, 2),
    ?assertEqual(<<"rtc_c2c_1_2">>, maps:get(<<"room_name">>, Grant)).

test_c2c_stranger_rejected() ->
    meck:expect(friend_ds, is_friend, fun(1, 999) -> false end),
    ?assertMatch({error, _}, rtc_room_logic:join(1, <<"did1">>, <<"c2c">>, 999)).

test_c2c_room_name_symmetric() ->
    %% 两端各自发起，必须落到同一个房间
    ?assertEqual(
        rtc_room_logic:room_name(<<"c2c">>, {7, 3}),
        rtc_room_logic:room_name(<<"c2c">>, {3, 7})
    ).

test_unknown_kind_rejected() ->
    ?assertMatch({error, _}, rtc_room_logic:join(1, <<"did1">>, <<"channel">>, 1)).

test_token_claims() ->
    meck:expect(group_member_ds, is_member, fun(200, 5) -> true end),
    {ok, Grant} = rtc_room_logic:join(5, <<"devA">>, <<"group">>, 200),
    Token = maps:get(<<"token">>, Grant),
    %% jwerl 往返：验签并断言 LiveKit 必需 claims
    {ok, Claims} = jwerl:verify(Token, hs256, ?TEST_SECRET),
    ?assertEqual(<<"testkey">>, maps:get(iss, Claims)),
    ?assertEqual(<<"5_devA">>, maps:get(sub, Claims)),
    Video = maps:get(video, Claims),
    ?assertEqual(<<"rtc_group_200">>, maps:get(room, Video)),
    ?assertEqual(true, maps:get(roomJoin, Video)),
    ?assertEqual(true, maps:get(canPublish, Video)),
    ?assertEqual(true, maps:get(canSubscribe, Video)),
    Now = erlang:system_time(second),
    Exp = maps:get(exp, Claims),
    ?assert(Exp > Now),
    ?assert(Exp =< Now + 601).
