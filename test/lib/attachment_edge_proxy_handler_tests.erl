-module(attachment_edge_proxy_handler_tests).
-include_lib("eunit/include/eunit.hrl").

unsatisfied_range_test_() ->
    [
        {timeout, 35, ?_test(pure_mock_fixture:run(fun() -> unsatisfied_range(Method) end))}
     || Method <- [<<"GET">>, <<"HEAD">>]
    ].

unsatisfied_range(Method) ->
    Request = #{method => Method},
    Context = {"http://authorize.invalid", "owned-test-token"},
    Origin = <<"http://storage.invalid/private-key">>,
    RequestId = make_ref(),
    pure_mock_fixture:with_mocks(
        [
            {cowboy_req, [
                {method, 1, fun(_) -> Method end},
                {parse_qs, 1, fun(_) -> [{<<"ticket">>, <<"owned-ticket">>}] end},
                {header, 2, fun(<<"range">>, _) -> <<"bytes=99-">> end},
                {reply, 4, fun(416, Headers, <<>>, ActualRequest) ->
                    ?assertEqual(Request, ActualRequest),
                    ?assertEqual(<<"bytes */10">>, maps:get(<<"content-range">>, Headers)),
                    ?assertEqual(<<"private, no-store">>, maps:get(<<"cache-control">>, Headers)),
                    ?assertNot(maps:is_key(<<"content-length">>, Headers)),
                    ?assertNot(maps:is_key(<<"location">>, Headers)),
                    ?assertNot(maps:is_key(<<"set-cookie">>, Headers)),
                    Request#{replied => true}
                end}
            ]},
            {httpc, [
                {request, 4, fun
                    (post, {"http://authorize.invalid", _, "application/json", Body}, _, _) ->
                        ?assertEqual(
                            Method, maps:get(<<"method">>, jsone:decode(list_to_binary(Body)))
                        ),
                        {ok, {{"HTTP/1.1", 200, "OK"}, [], jsone:encode(#{<<"url">> => Origin})}};
                    (get, {Url, [{"range", "bytes=99-"}]}, Options, _) ->
                        ?assertEqual(binary_to_list(Origin), Url),
                        ?assertEqual(false, proplists:get_value(autoredirect, Options)),
                        self() !
                            {http,
                                {RequestId, {
                                    {"HTTP/1.1", 416, "Range Not Satisfiable"},
                                    [
                                        {"content-range", "bytes */10"},
                                        {"content-length", "27"},
                                        {"location", binary_to_list(Origin)},
                                        {"set-cookie", "origin-secret=1"}
                                    ],
                                    <<"private upstream error body">>
                                }}},
                        {ok, RequestId}
                end},
                {cancel_request, 1, fun(ActualId) ->
                    ?assertEqual(RequestId, ActualId),
                    ok
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {ok, Request#{replied => true}, Context},
                attachment_edge_proxy_handler:init(Request, Context)
            )
        end
    ).
