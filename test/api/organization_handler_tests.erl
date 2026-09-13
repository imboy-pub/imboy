-module(organization_handler_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(ORG_ID, 101).
-define(UID, 201).

actions_parse_and_delegate_test_() ->
    ?WITH_MECKS(
        handler_mocks(),
        fun() ->
            Create = organization_handler:handle_action(collection, post_req, #{current_uid => ?UID}),
            ?assertEqual(200, maps:get(response_status, Create)),
            Mine = organization_handler:handle_action(mine, get_req, #{current_uid => ?UID}),
            ?assertEqual(200, maps:get(response_status, Mine)),
            Detail = organization_handler:handle_action(detail, get_req, #{current_uid => ?UID}),
            ?assertEqual(200, maps:get(response_status, Detail)),
            Update = organization_handler:handle_action(detail, patch_req, #{current_uid => ?UID}),
            ?assertEqual(200, maps:get(response_status, Update)),
            ?assertEqual(
                [
                    {create, ?UID, <<"Acme">>},
                    {mine, ?UID, 1, 10},
                    {detail, ?UID, ?ORG_ID},
                    {update, ?UID, ?ORG_ID, <<"Renamed">>, #{<<"logo">> => <<"x">>}, undefined}
                ],
                lists:reverse(get(calls))
            )
        end
    ).

unsupported_method_and_invalid_id_are_rejected_test_() ->
    ?WITH_MECKS(
        handler_mocks(),
        fun() ->
            Method = organization_handler:handle_action(collection, get_req, #{current_uid => ?UID}),
            ?assertEqual(405, maps:get(response_status, Method)),
            ?assertEqual(<<"POST">>, maps:get(allow, Method)),
            Invalid = organization_handler:handle_action(detail, invalid_req, #{current_uid => ?UID}),
            ?assertEqual(400, maps:get(response_status, Invalid))
        end
    ).

router_registers_mine_before_dynamic_detail_test() ->
    {ok, Router} = file:read_file("src/imboy_router.erl"),
    Mine = binary:match(Router, <<"organizations/mine\"">>),
    Detail = binary:match(Router, <<"organizations/:organization_id\"">>),
    ?assertNotEqual(nomatch, Mine),
    ?assertNotEqual(nomatch, Detail),
    {MineOffset, _} = Mine,
    {DetailOffset, _} = Detail,
    ?assert(MineOffset < DetailOffset).

handler_mocks() ->
    [
        {cowboy_req, [
            {'method', 1, fun
                (post_req) -> <<"POST">>;
                (patch_req) -> <<"PATCH">>;
                (_) -> <<"GET">>
            end},
            {'binding', 2, fun
                (organization_id, invalid_req) -> <<"bad">>;
                (organization_id, _) -> integer_to_binary(?ORG_ID)
            end},
            {'reply', 4, fun(405, Headers, _Body, _Req) ->
                #{response_status => 405, allow => maps:get(<<"allow">>, Headers)}
            end}
        ]},
        {elib_param, [
            {'post', 1, fun
                (patch_req) ->
                    #{<<"name">> => <<"Renamed">>, <<"branding">> => #{<<"logo">> => <<"x">>}};
                (_) ->
                    #{<<"name">> => <<"Acme">>}
            end},
            {'page', 1, fun(_) -> {1, 10} end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> #{response_status => 200, payload => Payload} end},
            {'error', 3, fun(_Req, _Msg, Code) -> #{response_status => Code} end}
        ]},
        {organization_logic, [
            {'create', 2, fun(Uid, Name) ->
                record_call({create, Uid, Name}),
                {ok, #{<<"id">> => ?ORG_ID}}
            end},
            {'mine', 3, fun(Uid, Page, Size) ->
                record_call({mine, Uid, Page, Size}),
                {ok, #{list => []}}
            end},
            {'detail', 2, fun(Uid, OrgId) ->
                record_call({detail, Uid, OrgId}),
                {ok, #{<<"id">> => OrgId}}
            end},
            {'update', 5, fun(Uid, OrgId, Name, Branding, Settings) ->
                record_call({update, Uid, OrgId, Name, Branding, Settings}),
                {ok, #{<<"id">> => OrgId, <<"name">> => Name}}
            end}
        ]}
    ].

record_call(Call) ->
    put(calls, [
        Call
        | case get(calls) of
            undefined -> [];
            Calls -> Calls
        end
    ]),
    ok.
