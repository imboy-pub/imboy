-module(attachment_storage_garage_tests).
-include_lib("eunit/include/eunit.hrl").

delete_identity_test_() ->
    [
        {timeout, 35, ?_test(pure_mock_fixture:run(fun() -> delete_case(Kind) end))}
     || Kind <- [matching, endpoint_changed, container_changed, unknown]
    ].

delete_case(Kind) ->
    Endpoint = <<"http://owned-storage.invalid">>,
    Root = #{driver => <<"garage">>, container => <<"bucket">>, endpoint => Endpoint},
    pure_mock_fixture:with_mocks(
        [
            {attachment_storage, [
                {resolve, 1, fun(<<"ns">>) ->
                    case Kind of
                        unknown -> {error, unknown};
                        _ -> {ok, Root}
                    end
                end}
            ]},
            {elib_s3_sign, [
                {presign_url_at, 8, fun(<<"DELETE">>, EP, <<"bucket">>, <<"key">>, _, _, _, Cfg) ->
                    ?assertEqual(Endpoint, EP),
                    ?assertEqual(Endpoint, maps:get(endpoint, Cfg)),
                    <<"http://owned-storage.invalid/delete">>
                end}
            ]},
            {httpc, [
                {request, 4, fun(delete, {"http://owned-storage.invalid/delete", []}, _, []) ->
                    ?assertEqual(matching, Kind),
                    put(deleted, true),
                    {ok, {{"HTTP/1.1", 204, "No Content"}, [], <<>>}}
                end}
            ]}
        ],
        fun() ->
            EP =
                case Kind of
                    endpoint_changed -> <<"http://other.invalid">>;
                    _ -> Endpoint
                end,
            application:set_env(imboy, garage, #{endpoint => EP}),
            Container =
                case Kind of
                    container_changed -> <<"other">>;
                    _ -> <<"bucket">>
                end,
            Expected =
                case Kind of
                    matching -> ok;
                    endpoint_changed -> {error, location_changed};
                    _ -> {error, location_unverified}
                end,
            ?assertEqual(
                Expected, attachment_storage_garage:delete(<<"ns">>, Container, <<"key">>)
            ),
            ?assertEqual(Kind =:= matching, get(deleted) =:= true)
        end
    ).
