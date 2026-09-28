-module(organization_logic_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(ORG_ID, 101).
-define(UID, 201).

create_relies_on_owner_trigger_and_returns_owner_role_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'with_tx', 1, fun(Tx) -> Tx(fake_conn) end}
            ]},
            {organization_repo, [
                {'create_tx', 4, fun(fake_conn, ?UID, <<"Acme">>, <<"pending">>) ->
                    {ok, #{
                        <<"id">> => ?ORG_ID,
                        <<"name">> => <<"Acme">>,
                        <<"branding">> => <<"{}">>,
                        <<"settings">> => <<"{\"locale\":\"zh-CN\"}">>
                    }}
                end}
            ]},
            {organization_member_repo, [
                {'find_active_tx', 4, fun(fake_conn, ?ORG_ID, ?UID, <<"role">>) ->
                    {ok, #{<<"role">> => <<"owner">>}}
                end}
            ]},
            %% APP 建企必须同事务创建默认 Workspace 模板（workspace 行 / 默认关系 /
            %% owner 成员 / 全员群 / 公告频道）——否则 owner 名下无工作区、加入者
            %% 拿到的 group_id/channel_id 全是 none。
            {workspace_ds, [
                {'create_default_template_tx', 4, fun(fake_conn, ?UID, ?ORG_ID, WsName) ->
                    ?assertEqual(<<"默认工作区"/utf8>>, WsName),
                    {ok, #{workspace_id => 9001, group_id => 9002, channel_id => 9003}}
                end}
            ]}
        ],
        fun() ->
            ?assertMatch(
                {ok, #{
                    <<"id">> := ?ORG_ID,
                    <<"member_role">> := <<"owner">>,
                    <<"branding">> := #{},
                    <<"settings">> := #{<<"locale">> := <<"zh-CN">>}
                }},
                organization_logic:create(?UID, <<"  Acme  ">>)
            ),
            ?assertEqual(
                1, meck:num_calls(workspace_ds, create_default_template_tx, 4)
            )
        end
    ).

create_rejects_blank_and_invalid_utf8_names_test() ->
    ?assertMatch({error, {400, _}}, organization_logic:create(?UID, <<"   ">>)),
    ?assertMatch({error, {400, _}}, organization_logic:create(?UID, <<255>>)).

detail_requires_active_organization_membership_test_() ->
    ?WITH_MECKS(
        [
            {organization_repo, [
                {'find_by_id', 1, fun(?ORG_ID) ->
                    {ok, #{<<"id">> => ?ORG_ID, <<"status">> => <<"archived">>}}
                end}
            ]},
            {organization_member_repo, [
                {'find_active', 3, fun(?ORG_ID, ?UID, <<"role">>) ->
                    {ok, #{<<"role">> => <<"member">>}}
                end}
            ]}
        ],
        fun() ->
            ?assertMatch(
                {ok, #{<<"status">> := <<"archived">>, <<"member_role">> := <<"member">>}},
                organization_logic:detail(?UID, ?ORG_ID)
            )
        end
    ).

detail_distinguishes_missing_from_non_member_test_() ->
    ?WITH_MECKS(
        [
            {organization_repo, [
                {'find_by_id', 1, fun
                    (?ORG_ID) -> {ok, #{<<"id">> => ?ORG_ID}};
                    (404) -> {error, not_found}
                end}
            ]},
            {organization_member_repo, [
                {'find_active', 3, fun(?ORG_ID, ?UID, <<"role">>) -> {error, not_found} end}
            ]}
        ],
        fun() ->
            ?assertMatch({error, {403, _}}, organization_logic:detail(?UID, ?ORG_ID)),
            ?assertMatch({error, {404, _}}, organization_logic:detail(?UID, 404))
        end
    ).

mine_clamps_pagination_test_() ->
    ?WITH_MECKS(
        [
            {organization_repo, [
                {'page_by_member', 3, fun(?UID, 1, 100) ->
                    {ok, #{
                        list => [#{<<"id">> => ?ORG_ID, <<"branding">> => <<"{}">>}],
                        page => 1,
                        size => 100,
                        total => 1,
                        total_page => 1
                    }}
                end}
            ]}
        ],
        fun() ->
            ?assertMatch(
                {ok, #{page := 1, size := 100, list := [#{<<"branding">> := #{}}]}},
                organization_logic:mine(?UID, 0, 999)
            )
        end
    ).

admin_can_update_generic_fields_test_() ->
    ?WITH_MECKS(
        update_mocks(<<"admin">>, <<"active">>),
        fun() ->
            ?assertMatch(
                {ok, #{
                    <<"name">> := <<"Renamed">>,
                    <<"branding">> := #{<<"logo">> := <<"x">>},
                    <<"settings">> := #{<<"client_a">> := #{<<"enabled">> := true}},
                    <<"member_role">> := <<"admin">>
                }},
                organization_logic:update(
                    ?UID,
                    ?ORG_ID,
                    <<"  Renamed  ">>,
                    #{<<"logo">> => <<"x">>},
                    #{<<"client_a">> => #{<<"enabled">> => true}}
                )
            )
        end
    ).

member_cannot_update_test_() ->
    ?WITH_MECKS(
        update_mocks(<<"member">>, <<"active">>),
        fun() ->
            ?assertMatch(
                {error, {403, _}},
                organization_logic:update(?UID, ?ORG_ID, <<"Renamed">>, undefined, undefined)
            )
        end
    ).

archived_organization_cannot_update_test_() ->
    ?WITH_MECKS(
        update_mocks(<<"owner">>, <<"archived">>),
        fun() ->
            ?assertMatch(
                {error, {409, _}},
                organization_logic:update(?UID, ?ORG_ID, <<"Renamed">>, undefined, undefined)
            )
        end
    ).

update_validates_patch_shape_test() ->
    ?assertMatch(
        {error, {400, _}},
        organization_logic:update(?UID, ?ORG_ID, undefined, undefined, undefined)
    ),
    ?assertMatch(
        {error, {400, _}},
        organization_logic:update(?UID, ?ORG_ID, undefined, [], undefined)
    ).

update_mocks(Role, Status) ->
    [
        {elib_pg, [
            {'with_tx', 1, fun(Tx) ->
                try Tx(fake_conn) of
                    Result -> Result
                catch
                    throw:{abort_tx, Reason} -> {error, Reason}
                end
            end}
        ]},
        {organization_repo, [
            {'find_for_update_tx', 2, fun(fake_conn, ?ORG_ID) ->
                {ok, #{<<"id">> => ?ORG_ID, <<"status">> => Status}}
            end},
            {'update_tx', 5, fun(fake_conn, ?ORG_ID, Name, Branding, Settings) ->
                ?assertEqual(<<"Renamed">>, Name),
                {ok, #{
                    <<"id">> => ?ORG_ID,
                    <<"name">> => Name,
                    <<"branding">> => Branding,
                    <<"settings">> => Settings
                }}
            end}
        ]},
        {organization_member_repo, [
            {'find_active_for_share_tx', 4, fun(fake_conn, ?ORG_ID, ?UID, <<"role">>) ->
                {ok, #{<<"role">> => Role}}
            end}
        ]}
    ].
