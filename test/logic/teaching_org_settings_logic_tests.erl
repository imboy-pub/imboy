-module(teaching_org_settings_logic_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(ORG_ID, 101).
-define(UID, 202).

owner_can_read_settings_test_() ->
    can_read_settings_as(<<"owner">>).

admin_can_read_settings_test_() ->
    can_read_settings_as(<<"admin">>).

admin_write_uses_locked_membership_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'with_tx', 2, fun(Tx, _Opts) -> Tx(fake_conn) end}
            ]},
            {organization_member_repo, [
                {'find_active_for_share_tx', 4, fun(_, ?ORG_ID, ?UID, <<"role">>) ->
                    {ok, #{<<"role">> => <<"admin">>}}
                end}
            ]},
            {teaching_org_settings_repo, repo_mocks()}
        ],
        fun() ->
            ?assertMatch(
                {ok, #{<<"ai_assist_enabled">> := true}},
                teaching_org_settings_logic:set_ai_assist(?UID, ?ORG_ID, true)
            )
        end
    ).

ordinary_member_is_denied_test_() ->
    with_role(<<"member">>, fun() ->
        ?assertEqual(
            {error, not_authorized},
            teaching_org_settings_logic:get_settings(?UID, ?ORG_ID)
        )
    end).

membership_lookup_error_fails_closed_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'with_tx', 2, fun(Tx, _Opts) -> Tx(fake_conn) end}
            ]},
            {organization_member_repo, [
                {'find_active_tx', 4, fun(_, _, _, _) -> {error, unavailable} end}
            ]},
            {teaching_org_settings_repo, repo_mocks()}
        ],
        fun() ->
            ?assertEqual(
                {error, db_error},
                teaching_org_settings_logic:get_settings(?UID, ?ORG_ID)
            )
        end
    ).

can_read_settings_as(Role) ->
    with_role(Role, fun() ->
        ?assertMatch(
            {ok, #{<<"ai_assist_enabled">> := true}},
            teaching_org_settings_logic:get_settings(?UID, ?ORG_ID)
        )
    end).

with_role(Role, Assert) ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'with_tx', 2, fun(Tx, _Opts) -> Tx(fake_conn) end}
            ]},
            {organization_member_repo, [
                {'find_active_tx', 4, fun(_, ?ORG_ID, ?UID, <<"role">>) ->
                    {ok, #{<<"role">> => Role}}
                end}
            ]},
            {teaching_org_settings_repo, repo_mocks()}
        ],
        Assert
    ).

repo_mocks() ->
    [
        {'org_row_tx', 2, fun(_, ?ORG_ID) ->
            {ok, #{<<"name">> => <<"Acme">>, <<"settings">> => #{}}}
        end},
        {'ai_assist_enabled_of', 1, fun(_) -> true end},
        {'set_ai_assist_enabled_tx', 3, fun(_, _, _) -> ok end}
    ].
