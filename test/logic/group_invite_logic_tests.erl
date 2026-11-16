-module(group_invite_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(KEY, <<"synthetic-server-key-32-bytes-long-only-tests">>).

mocks() -> [
    {config_ds, [{env, 2, fun(jwt_key, <<>>) -> ?KEY end}]},
    {group_member_ds, [{is_member, 2, fun(101, 7) -> true; (_, _) -> false end}]},
    {group_member_logic, [
        {get_group_capacity, 1, fun(101) ->
            #{<<"member_max">> => 100, <<"member_count">> => 2}
        end},
        {join_group, 4, fun(<<"scan_qr_code">>, 8, 101, Options) ->
            ?assertEqual(7, maps:get(invite_issuer, Options)),
            ok
        end}
    ]}
].

invite_key() -> crypto:mac(hmac, sha256, ?KEY, <<"imboy/group-invite/v1">>).
claims() ->
    Now = erlang:system_time(second),
    #{<<"aud">> => <<"group_invite_v1">>, <<"sub">> => <<"group_invite_v1">>,
      <<"gid">> => <<"101">>, <<"inviter_uid">> => <<"7">>,
      <<"iat">> => Now, <<"exp">> => Now + 3600}.

authorized_issue_and_redeem_test_() ->
    ?WITH_MECKS(mocks(), fun() ->
        {ok, #{<<"token">> := Token, <<"gid">> := <<"101">>, <<"exp">> := Exp}} =
            group_invite_logic:issue(7, 101),
        {ok, Claims} = imboy_jwt:verify(Token, invite_key()),
        ?assertEqual(<<"101">>, maps:get(<<"gid">>, Claims)),
        ?assertEqual(<<"7">>, maps:get(<<"inviter_uid">>, Claims)),
        ?assertEqual(Exp, maps:get(<<"exp">>, Claims) * 1000),
        ?assertMatch({error, _}, imboy_jwt:verify(Token, ?KEY)),
        ?assertEqual(ok, group_invite_logic:redeem(8, 101, Token))
    end).

outsider_cannot_mint_test_() ->
    ?WITH_MECKS(mocks(), fun() ->
        ?assertMatch({error, {403, _}}, group_invite_logic:issue(8, 101))
    end).

other_group_and_legacy_checksum_rejected_test_() ->
    ?WITH_MECKS(mocks(), fun() ->
        {ok, #{<<"token">> := Token}} = group_invite_logic:issue(7, 101),
        ?assertMatch({error, {403, _}}, group_invite_logic:redeem(8, 102, Token)),
        ?assertMatch({error, {403, _}},
            group_invite_logic:redeem(8, 101, <<"0123456789abcdef0123456789abcdef">>)),
        ?assertNot(meck:called(group_member_logic, join_group, ['_', '_', '_', '_']))
    end).

invalid_claims_and_signatures_test_() ->
    ?WITH_MECKS(mocks(), fun() ->
        Claims = claims(),
        Now = erlang:system_time(second),
        Invalid = [
            imboy_jwt:sign(Claims#{<<"exp">> => Now - 1}, invite_key()),
            imboy_jwt:sign(Claims#{<<"aud">> => <<"tk">>}, invite_key()),
            imboy_jwt:sign(Claims#{<<"inviter_uid">> => <<"0">>}, invite_key()),
            imboy_jwt:sign(Claims#{<<"exp">> => Now + 604801}, invite_key()),
            imboy_jwt:sign(Claims, ?KEY),
            imboy_jwt:sign(Claims, <<"synthetic-app-shipped-key-is-not-server-key">>)
        ],
        lists:foreach(fun(Token) ->
            ?assertMatch({error, {403, _}}, group_invite_logic:redeem(8, 101, Token))
        end, Invalid),
        ?assertNot(meck:called(group_member_logic, join_group, ['_', '_', '_', '_']))
    end).

revoked_inviter_rejected_at_transaction_test_() ->
    ?WITH_MECKS(mocks(), fun() ->
        {ok, #{<<"token">> := Token}} = group_invite_logic:issue(7, 101),
        meck:expect(group_member_logic, join_group, fun(_, _, _, Options) ->
            ?assertEqual(7, maps:get(invite_issuer, Options)),
            {error, invalid_group_invitation}
        end),
        ?assertMatch({error, {403, _}}, group_invite_logic:redeem(8, 101, Token))
    end).

missing_server_key_fails_closed_test_() ->
    ?WITH_MECKS(mocks(), fun() ->
        meck:expect(config_ds, env, fun(jwt_key, <<>>) -> <<>> end),
        ?assertMatch({error, {503, _}}, group_invite_logic:issue(7, 101)),
        ?assertMatch({error, {503, _}}, group_invite_logic:redeem(8, 101, <<"token">>))
    end).

public_self_join_requires_explicit_server_policy_test_() ->
    ?WITH_MECKS([{group_ds, [{find_by_id, 2, fun
        (1, _) -> #{<<"status">> => 1, <<"type">> => 1, <<"join_limit">> => 1};
        (2, _) -> #{<<"status">> => 1, <<"type">> => 2, <<"join_limit">> => 1};
        (3, _) -> #{<<"status">> => 1, <<"type">> => 1, <<"join_limit">> => 2};
        (4, _) -> #{<<"status">> => 0, <<"type">> => 1, <<"join_limit">> => 1};
        (_, _) -> {error, unavailable}
    end}]}], fun() ->
        ?assert(group_invite_logic:allows_public_join(1)),
        lists:foreach(fun(Gid) ->
            ?assertNot(group_invite_logic:allows_public_join(Gid))
        end, [2, 3, 4, 5])
    end).
