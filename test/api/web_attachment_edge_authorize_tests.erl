-module(web_attachment_edge_authorize_tests).
-include_lib("eunit/include/eunit.hrl").
web_ticket_dispatch_test() ->
    Modules = [cowboy_req, attach_logic],
    lists:foreach(fun(M) -> meck:new(M, [passthrough, no_link]) end, Modules),
    Previous = application:get_env(imboy, edge_internal_token),
    try
        application:set_env(imboy, edge_internal_token, <<"synthetic-internal">>),
        meck:expect(cowboy_req, method, fun(_) -> <<"POST">> end),
        meck:expect(cowboy_req, header, fun(_, _, _) -> <<"synthetic-internal">> end),
        meck:expect(cowboy_req, read_body, fun(R, _) -> {ok, maps:get(body, R), R} end),
        meck:expect(cowboy_req, reply, fun(S, _, B, R) -> R#{status => S, body => B} end),
        meck:expect(attach_logic, web_download, fun
            (<<"valid">>) -> {ok, <<"https://internal.test/signed">>};
            (_) -> {error, forbidden}
        end),
        Body = jsone:encode(#{<<"ticket">> => <<"web.valid">>, <<"method">> => <<"GET">>}),
        {ok, Good, _} = attachment_edge_authorize_handler:init(#{body => Body}, #{}),
        ?assertEqual(200, maps:get(status, Good)),
        ?assertEqual(1, meck:num_calls(attach_logic, web_download, [<<"valid">>])),
        ?assertEqual(0, meck:num_calls(attach_logic, download, '_')),
        BadBody = jsone:encode(#{<<"ticket">> => <<"web.bad">>, <<"method">> => <<"GET">>}),
        {ok, Bad, _} = attachment_edge_authorize_handler:init(#{body => BadBody}, #{}),
        ?assertEqual(404, maps:get(status, Bad)),
        ?assertEqual(0, meck:num_calls(attach_logic, download, '_'))
    after
        case Previous of
            {ok, V} -> application:set_env(imboy, edge_internal_token, V);
            undefined -> application:unset_env(imboy, edge_internal_token)
        end,
        lists:foreach(fun meck:unload/1, lists:reverse(Modules))
    end.
