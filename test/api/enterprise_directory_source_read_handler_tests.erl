-module(enterprise_directory_source_read_handler_tests).
-include_lib("eunit/include/eunit.hrl").

query_transport_test() ->
    Modules = [cowboy_req, enterprise_directory_source_read_logic, enterprise_internal_error],
    lists:foreach(fun(M) -> meck:new(M, [non_strict, no_link]) end, Modules),
    try
        meck:expect(cowboy_req, method, fun(_) -> <<"GET">> end),
        meck:expect(cowboy_req, reply, fun(Code, Headers, Body, Req) ->
            Req#{code => Code, headers => Headers, body => Body}
        end),
        meck:expect(enterprise_internal_error, reply, fun(Req, Code) -> Req#{error => Code} end),
        meck:expect(cowboy_req, parse_qs, fun(_) -> exit({request_error, qs, malformed}) end),
        ?assertMatch(#{error := <<"invalid_request">>}, invoke()),
        meck:expect(cowboy_req, parse_qs, fun(_) ->
            [
                {<<"page_size">>, <<"1">>},
                {<<"page_size">>, <<"2">>}
            ]
        end),
        ?assertMatch(#{error := <<"invalid_request">>}, invoke()),
        ?assertEqual(0, meck:num_calls(enterprise_directory_source_read_logic, page, '_')),
        meck:expect(cowboy_req, parse_qs, fun(_) -> [] end),
        meck:expect(
            enterprise_directory_source_read_logic,
            page,
            fun(_, organizations, #{}) -> {ok, #{<<"items">> => []}} end
        ),
        ?assertMatch(#{code := 200, headers := #{<<"cache-control">> := <<"no-store">>}}, invoke())
    after
        lists:foreach(fun meck:unload/1, Modules)
    end.

invoke() ->
    {ok, Req, _} = enterprise_directory_handler:init(
        #{},
        #{action => organizations, enterprise_internal => #{}}
    ),
    Req.
