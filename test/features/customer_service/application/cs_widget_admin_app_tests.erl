%%% @doc Widget installation 管理用例：公开标识、真实分页与撤销；不制造 shop_key。
-module(cs_widget_admin_app_tests).

-include_lib("eunit/include/eunit.hrl").

-define(ORG, 7001001).
-define(WS, 90001).
-define(T0, 1700000000).

widget_admin_app_test_() ->
    {setup,
        fun() ->
            ok = cs_fake_store:init(),
            ok = cs_fake_id:reset()
        end,
        fun(_) ->
            ok = cs_fake_store:destroy(),
            ok = cs_fake_id:reset()
        end,
        [
            fun create_list_revoke_uses_public_id_only/0,
            fun invalid_origin_is_rejected_before_store/0
        ]}.

create_list_revoke_uses_public_id_only() ->
    Params = params(),
    {ok, #{installation := Created}} = cs_widget_app:create_installation(?ORG, Params),
    Id = maps:get(id, Created),
    ?assertEqual(<<"wgt_pub_test">>, maps:get(public_widget_id, Created)),
    ?assertNot(maps:is_key(workspace_id, Created)),
    ?assertNot(maps:is_key(created_by_user_id, Created)),
    ?assertNot(maps:is_key(one_time_secret, Created)),
    ?assertNot(maps:is_key(shop_key, Created)),

    {ok, #{installations := [Listed]}} = cs_widget_app:list_installations(?ORG, Params),
    ?assertEqual(Id, maps:get(id, Listed)),
    ?assertEqual(active, maps:get(status, Listed)),

    {ok, #{installation := Revoked}} =
        cs_widget_app:revoke_installation(?ORG, Params#{id => Id}),
    ?assertEqual(revoked, maps:get(status, Revoked)),
    ?assertEqual(?T0, maps:get(revoked_at, Revoked)),
    ?assertEqual(1, length(cs_fake_store:events_with_action(<<"widget.installation.created">>))),
    ?assertEqual(1, length(cs_fake_store:events_with_action(<<"widget.installation.revoked">>))).

invalid_origin_is_rejected_before_store() ->
    {ok, Before} = cs_widget_app:list_installations(?ORG, params()),
    ?assertMatch(
        {error, {invalid_origin, _}},
        cs_widget_app:create_installation(
            ?ORG, (params())#{allowed_origins => [<<"https://shop.example.com/path">>]}
        )
    ),
    ?assertEqual({ok, Before}, cs_widget_app:list_installations(?ORG, params())).

params() ->
    #{
        workspace_id => ?WS,
        display_name => <<"Store support">>,
        allowed_origins => [<<"HTTPS://SHOP.EXAMPLE.COM:443">>],
        branding => #{<<"display_name">> => <<"Store">>, <<"internal">> => <<"drop">>},
        consent_version => <<"v1">>,
        at => ?T0,
        store => cs_fake_store,
        id => cs_fake_id,
        new_public_widget_id => fun() -> <<"wgt_pub_test">> end
    }.
