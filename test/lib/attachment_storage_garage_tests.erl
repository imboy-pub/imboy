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
            {elib_oss, [
                {endpoint, 0, fun() ->
                    case Kind of
                        endpoint_changed -> <<"http://other.invalid">>;
                        _ -> Endpoint
                    end
                end},
                {delete_object, 2, fun(<<"bucket">>, <<"key">>) ->
                    ?assertEqual(matching, Kind),
                    put(deleted, true),
                    ok
                end}
            ]}
        ],
        fun() ->
            Container =
                case Kind of
                    container_changed -> <<"other">>;
                    _ -> <<"bucket">>
                end,
            Expected =
                case Kind of
                    matching -> ok;
                    _ -> {error, storage_identity_changed}
                end,
            ?assertEqual(
                Expected, attachment_storage_garage:delete(<<"ns">>, Container, <<"key">>)
            ),
            ?assertEqual(Kind =:= matching, get(deleted) =:= true)
        end
    ).
