-module(organization_member_handler_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(ORG_ID, 101).
-define(UID, 201).
-define(TARGET, 202).

rest_actions_parse_and_delegate_test_() ->
    ?WITH_MECKS(
        handler_mocks(),
        fun() ->
            ListRes = organization_member_handler:handle_action(
                collection, get_req, #{current_uid => ?UID}
            ),
            ?assertEqual(200, maps:get(response_status, ListRes)),
            receive
                {list, ?UID, ?ORG_ID, 1, 10} -> ok
            after 0 -> ?assert(false)
            end,

            InviteRes = organization_member_handler:handle_action(
                collection, post_req, #{current_uid => ?UID}
            ),
            ?assertEqual(200, maps:get(response_status, InviteRes)),
            receive
                {invite, ?UID, ?ORG_ID, ?TARGET, <<"member">>} -> ok
            after 0 -> ?assert(false)
            end,

            RoleRes = organization_member_handler:handle_action(
                role, put_req, #{current_uid => ?UID}
            ),
            ?assertEqual(200, maps:get(response_status, RoleRes)),
            receive
                {role, ?UID, ?ORG_ID, ?TARGET, <<"admin">>} -> ok
            after 0 -> ?assert(false)
            end,

            RemoveRes = organization_member_handler:handle_action(
                member, delete_req, #{current_uid => ?UID}
            ),
            ?assertEqual(200, maps:get(response_status, RemoveRes)),
            receive
                {remove, ?UID, ?ORG_ID, ?TARGET} -> ok
            after 0 -> ?assert(false)
            end,

            TransferRes = organization_member_handler:handle_action(
                owner_transfer, transfer_req, #{current_uid => ?UID}
            ),
            ?assertEqual(200, maps:get(response_status, TransferRes)),
            receive
                {transfer_owner, ?UID, ?ORG_ID, ?TARGET} -> ok
            after 0 -> ?assert(false)
            end
        end
    ).

unsupported_method_returns_real_405_test_() ->
    ?WITH_MECKS(
        handler_mocks(),
        fun() ->
            Result = organization_member_handler:handle_action(
                collection, put_req, #{current_uid => ?UID}
            ),
            ?assertEqual(405, maps:get(response_status, Result)),
            ?assertEqual(<<"GET, POST">>, maps:get(allow, Result))
        end
    ).

invalid_path_id_returns_400_test_() ->
    ?WITH_MECKS(
        handler_mocks(),
        fun() ->
            Result = organization_member_handler:handle_action(
                collection, invalid_req, #{current_uid => ?UID}
            ),
            ?assertEqual(400, maps:get(response_status, Result))
        end
    ).

router_registers_specific_role_before_member_route_test() ->
    {ok, Router} = file:read_file("src/imboy_router.erl"),
    Collection = binary:match(Router, <<"organizations/:organization_id/members\"">>),
    Transfer = binary:match(Router, <<"organizations/:organization_id/members/transfer_owner">>),
    Role = binary:match(Router, <<"organizations/:organization_id/members/:user_id/role">>),
    Member = binary:match(Router, <<"organizations/:organization_id/members/:user_id\"">>),
    ?assertNotEqual(nomatch, Collection),
    ?assertNotEqual(nomatch, Transfer),
    ?assertNotEqual(nomatch, Role),
    ?assertNotEqual(nomatch, Member),
    {RoleOffset, _} = Role,
    {TransferOffset, _} = Transfer,
    {MemberOffset, _} = Member,
    ?assert(TransferOffset < RoleOffset),
    ?assert(RoleOffset < MemberOffset).

handler_mocks() ->
    [
        {cowboy_req, [
            {'method', 1, fun
                (get_req) -> <<"GET">>;
                (post_req) -> <<"POST">>;
                (put_req) -> <<"PUT">>;
                (delete_req) -> <<"DELETE">>;
                (transfer_req) -> <<"POST">>;
                (invalid_req) -> <<"GET">>
            end},
            {'binding', 2, fun
                (organization_id, invalid_req) -> <<"bad">>;
                (organization_id, _) -> integer_to_binary(?ORG_ID);
                (user_id, _) -> integer_to_binary(?TARGET)
            end},
            {'reply', 4, fun(405, Headers, _Body, _Req) ->
                #{response_status => 405, allow => maps:get(<<"allow">>, Headers)}
            end}
        ]},
        {elib_param, [
            {'page', 1, fun(_) -> {1, 10} end},
            {'post', 1, fun
                (post_req) -> #{<<"user_id">> => ?TARGET};
                (transfer_req) -> #{<<"user_id">> => ?TARGET};
                (put_req) -> #{<<"role">> => <<"admin">>};
                (_) -> #{}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) ->
                #{response_status => 200, payload => Payload}
            end},
            {'error', 3, fun(_Req, _Msg, Code) -> #{response_status => Code} end}
        ]},
        {organization_member_logic, [
            {'list', 4, fun(Uid, OrgId, Page, Size) ->
                self() ! {list, Uid, OrgId, Page, Size},
                {ok, #{list => []}}
            end},
            {'invite', 4, fun(Uid, OrgId, TargetUid, Role) ->
                self() ! {invite, Uid, OrgId, TargetUid, Role},
                {ok, changed, #{user_id => TargetUid}}
            end},
            {'change_role', 4, fun(Uid, OrgId, TargetUid, Role) ->
                self() ! {role, Uid, OrgId, TargetUid, Role},
                {ok, changed, #{user_id => TargetUid, role => Role}}
            end},
            {'remove', 3, fun(Uid, OrgId, TargetUid) ->
                self() ! {remove, Uid, OrgId, TargetUid},
                {ok, #{user_id => TargetUid, status => <<"removed">>}}
            end},
            {'transfer_owner', 3, fun(Uid, OrgId, TargetUid) ->
                self() ! {transfer_owner, Uid, OrgId, TargetUid},
                {ok, #{organization_id => OrgId, owner_id => TargetUid}}
            end}
        ]}
    ].
