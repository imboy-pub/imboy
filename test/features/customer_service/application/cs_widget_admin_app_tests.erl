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
            fun update_installation_roundtrip/0,
            fun update_rejected_for_revoked_and_bad_origins/0,
            fun invalid_origin_is_rejected_before_store/0,
            fun generated_public_widget_id_is_decimal_tsid/0
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

%% PUT 语义：四键全量提交；不可编辑键（public_widget_id/status）不受影响；
%% 更新落审计事件 widget.installation.updated，version 前进。
update_installation_roundtrip() ->
    CreateParams = (params())#{new_public_widget_id => fun() -> <<"wgt_pub_upd">> end},
    {ok, #{installation := Created}} = cs_widget_app:create_installation(?ORG, CreateParams),
    Id = maps:get(id, Created),
    Updates = CreateParams#{
        id => Id,
        display_name => <<"Renamed support">>,
        allowed_origins => [<<"HTTPS://DOCS.EXAMPLE.COM">>, <<"https://shop.example.com">>],
        branding => #{<<"display_name">> => <<"Docs">>, <<"internal">> => <<"drop">>},
        consent_version => <<"v2">>,
        at => ?T0 + 10
    },
    {ok, #{installation := Updated}} = cs_widget_app:update_installation(?ORG, Updates),
    ?assertEqual(Id, maps:get(id, Updated)),
    ?assertEqual(<<"wgt_pub_upd">>, maps:get(public_widget_id, Updated)),
    ?assertEqual(active, maps:get(status, Updated)),
    ?assertEqual(<<"Renamed support">>, maps:get(display_name, Updated)),
    ?assertEqual(
        [<<"https://docs.example.com">>, <<"https://shop.example.com">>],
        lists:sort(maps:get(allowed_origins, Updated))
    ),
    ?assertEqual(#{<<"display_name">> => <<"Docs">>}, maps:get(branding, Updated)),
    ?assertEqual(<<"v2">>, maps:get(consent_version, Updated)),
    ?assertEqual(?T0 + 10, maps:get(updated_at, Updated)),
    ?assert(maps:get(version, Updated) > maps:get(version, Created)),
    %% 更新结果落库（同 Org fetch 可见），并追加 updated 审计事件。
    {ok, Stored} = cs_fake_store:fetch_widget_installation(?ORG, Id),
    ?assertEqual(<<"v2">>, maps:get(consent_version, Stored)),
    ?assertEqual(1, length(cs_fake_store:events_with_action(<<"widget.installation.updated">>))).

update_rejected_for_revoked_and_bad_origins() ->
    CreateParams = (params())#{new_public_widget_id => fun() -> <<"wgt_pub_rej">> end},
    {ok, #{installation := Created}} = cs_widget_app:create_installation(?ORG, CreateParams),
    Id = maps:get(id, Created),
    %% 非法 origin 在触达 store 前拒绝；空 origin 列表同样拒绝。
    ?assertMatch(
        {error, {invalid_origin, _}},
        cs_widget_app:update_installation(
            ?ORG, CreateParams#{id => Id, allowed_origins => [<<"ftp://shop.example.com">>]}
        )
    ),
    ?assertMatch(
        {error, {invalid_argument, allowed_origins}},
        cs_widget_app:update_installation(
            ?ORG, CreateParams#{id => Id, allowed_origins => []}
        )
    ),
    ?assertMatch(
        {error, {invalid_argument, display_name}},
        cs_widget_app:update_installation(?ORG, CreateParams#{id => Id, display_name => <<>>})
    ),
    %% 已吊销行拒绝编辑（installation_revoked，HTTP 403 面）。
    {ok, _} = cs_widget_app:revoke_installation(?ORG, CreateParams#{id => Id, at => ?T0 + 20}),
    ?assertMatch(
        {error, installation_revoked},
        cs_widget_app:update_installation(
            ?ORG, CreateParams#{id => Id, display_name => <<"after revoke">>}
        )
    ).

%% CSD-BE-01R（R4，hosted-widget-contract S1）：public_widget_id 生成口径
%% = TSID 十进制 string（FE loader `isValidPublicWidgetId` 只接受 1..26 位
%% 十进制；旧 `wgt_pub_<hex>` 生成口径废止——真实发放的 ID 曾被前端
%% fail-closed 拒绝）。未注入 `new_public_widget_id` 时走 id 端口生成。
generated_public_widget_id_is_decimal_tsid() ->
    {ok, #{installation := Created}} =
        cs_widget_app:create_installation(?ORG, maps:without([new_public_widget_id], params())),
    PublicId = maps:get(public_widget_id, Created),
    ?assert(byte_size(PublicId) > 0),
    ?assert(byte_size(PublicId) =< 26),
    ?assert(lists:all(fun(C) -> C >= $0 andalso C =< $9 end, binary_to_list(PublicId))),
    %% 生成的 ID 经全局反查 round-trip 可服务（bootstrap / /w/ 面）。
    ?assertMatch(
        {ok, _}, cs_fake_store:fetch_widget_installation_by_public_id_global(PublicId)
    ),
    %% 存量形状（wgt_pub_*）不做迁移：形状门保持 [A-Za-z0-9_-] 宽口径，
    %% 旧行照常命中（零 DDL 的存量兼容）。
    LegacyInst = cs_fake_id:new_id(cs_session),
    {ok, _} =
        cs_fake_store:insert_widget_installation(?ORG, #{
            id => LegacyInst,
            public_widget_id => <<"wgt_pub_legacy_shape">>,
            display_name => <<"legacy">>,
            allowed_origins => [<<"https://shop.example.com">>],
            branding => #{},
            consent_version => <<"v1">>
        }),
    ?assertMatch(
        {ok, _},
        cs_fake_store:fetch_widget_installation_by_public_id_global(
            <<"wgt_pub_legacy_shape">>
        )
    ).

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
