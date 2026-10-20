-module(enterprise_directory_source_read_http_checks).
-export([run/1]).
-include_lib("eunit/include/eunit.hrl").

%% Uses the same migrated marker DB and real authenticated Cowboy as all 47 operations.
run(#{conn := C, app_a := App} = S) ->
    {ok, [#{<<"allowed_scopes">> := Original}]} = elib_pg:query(
        C,
        <<"SELECT allowed_scopes FROM enterprise_application WHERE id=$1">>,
        [App]
    ),
    {ok, _} = elib_pg:query(
        C,
        <<"UPDATE enterprise_application SET allowed_scopes='[\"identities:read\"]' WHERE id=$1">>,
        [App]
    ),
    try
        read_only_success(S),
        negatives(S)
    after
        {ok, _} = elib_pg:query(
            C,
            <<"UPDATE enterprise_application SET allowed_scopes=$2::jsonb WHERE id=$1">>,
            [App, Original]
        )
    end.

read_only_success(#{port := Port, cred_a := Credential}) ->
    Auth = intbe02_http_support:auth(Credential),
    lists:foreach(
        fun({Id, Kind}) ->
            Path = <<"/api/internal/v1/directory/", Kind/binary>>,
            Query =
                case Kind of
                    <<"organizations">> -> <<>>;
                    _ -> <<"?external_organization_id=audit-directory-org">>
                end,
            Reply = intbe02_http_support:http(
                Port, <<"GET">>, <<Path/binary, Query/binary>>, <<>>, Auth
            ),
            ?assertEqual(200, maps:get(status, Reply)),
            Page = jsone:decode(maps:get(body, Reply)),
            ?assert(is_list(maps:get(<<"items">>, Page))),
            ?assertEqual(false, maps:get(<<"has_more">>, Page)),
            intbe02_http_support:cover(Id, ok)
        end,
        [
            {<<"INT-45">>, <<"organizations">>},
            {<<"INT-46">>, <<"departments">>},
            {<<"INT-47">>, <<"persons">>}
        ]
    ).

negatives(#{port := Port, cred_a := Cred, cred_ro := ReadOnly}) ->
    Auth = intbe02_http_support:auth(Cred),
    Base = <<"/api/internal/v1/directory/">>,
    Bad = [
        {<<Base/binary, "persons">>, Auth, <<"invalid_request">>},
        {<<Base/binary, "persons?external_organization_id=%">>, Auth, <<"invalid_request">>},
        {
            <<Base/binary, "persons?external_organization_id=a&external_organization_id=b">>,
            Auth,
            <<"invalid_request">>
        },
        {
            <<Base/binary, "persons?external_organization_id=foreign">>,
            Auth,
            <<"resource_not_found">>
        },
        {<<Base/binary, "organizations?unknown=1">>, Auth, <<"invalid_request">>},
        {
            <<Base/binary, "organizations">>,
            intbe02_http_support:auth(ReadOnly),
            <<"insufficient_scope">>
        }
    ],
    lists:foreach(
        fun({Path, Headers, Code}) ->
            assert_error(intbe02_http_support:http(Port, <<"GET">>, Path, <<>>, Headers), Code)
        end,
        Bad
    ),
    lists:foreach(
        fun(Kind) ->
            assert_error(
                intbe02_http_support:http(
                    Port,
                    <<"GET">>,
                    <<Base/binary, Kind/binary>>,
                    <<>>,
                    #{}
                ),
                <<"invalid_credential">>
            )
        end,
        [<<"organizations">>, <<"departments">>, <<"persons">>]
    ).

assert_error(Reply, Code) ->
    ?assertEqual(enterprise_internal_error:http_status(Code), maps:get(status, Reply)),
    ?assertMatch(#{<<"error">> := #{<<"code">> := Code}}, jsone:decode(maps:get(body, Reply))).
