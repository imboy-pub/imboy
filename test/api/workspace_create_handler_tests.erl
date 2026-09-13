-module(workspace_create_handler_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(UID, 900001).
-define(ORG_ID, 700001).

create_preserves_organization_scope_input_test_() ->
    ?WITH_MECKS(
        [
            {auth_ds, [
                {'current_uid', 1, fun(_) -> ?UID end}
            ]},
            {throttle, [
                {'check', 2, fun(three_second_once, {workspace_create, ?UID}) -> ok end}
            ]},
            {elib_param, [
                {'post', 1, fun
                    (personal_req) ->
                        #{<<"name">> => <<"Personal">>};
                    (organization_req) ->
                        #{
                            <<"name">> => <<"Organization Workspace">>,
                            <<"organization_id">> => integer_to_binary(?ORG_ID),
                            <<"request_id">> => <<"request-1">>
                        };
                    (invalid_req) ->
                        #{<<"name">> => <<"Invalid">>, <<"organization_id">> => <<"bad">>}
                end}
            ]},
            {workspace_logic, [
                {'create', 4, fun(Uid, OrganizationId, Name, RequestId) ->
                    put(last_workspace_create, {Uid, OrganizationId, Name, RequestId}),
                    case OrganizationId of
                        0 -> {error, {400, <<"organization_id invalid">>}};
                        _ -> {ok, #{workspace_id => 800001}, created}
                    end
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) -> #{status => 200, payload => Payload} end},
                {'error', 3, fun(_Req, _Msg, Code) -> #{status => Code} end}
            ]}
        ],
        fun() ->
            ?assertMatch(
                #{status := 200},
                workspace_handler:handle_action(create, personal_req, #{current_uid => ?UID})
            ),
            ?assertEqual(
                {?UID, undefined, <<"Personal">>, undefined},
                erase(last_workspace_create)
            ),
            ?assertMatch(
                #{status := 200},
                workspace_handler:handle_action(create, organization_req, #{current_uid => ?UID})
            ),
            ?assertEqual(
                {?UID, ?ORG_ID, <<"Organization Workspace">>, <<"request-1">>},
                erase(last_workspace_create)
            ),
            ?assertMatch(
                #{status := 400},
                workspace_handler:handle_action(create, invalid_req, #{current_uid => ?UID})
            ),
            ?assertEqual({?UID, 0, <<"Invalid">>, undefined}, erase(last_workspace_create)),
            ok
        end
    ).
