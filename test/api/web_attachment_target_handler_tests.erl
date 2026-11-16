-module(web_attachment_target_handler_tests).
-include_lib("eunit/include/eunit.hrl").

native_web_acl_boundary_test() ->
    Modules = [cowboy_req, auth_ds, attach_logic, config_ds, elib_response],
    lists:foreach(fun(M) -> meck:new(M, [passthrough, no_link]) end, Modules),
    Saved = application:get_env(imboy, garage),
    try
        application:set_env(imboy, garage, #{public_endpoint => <<"https://s3.test">>}),
        meck:expect(cowboy_req, method, fun(_) -> <<"GET">> end),
        meck:expect(cowboy_req, parse_qs, fun(_) -> [{<<"object_key">>, <<"u1/x">>}] end),
        meck:expect(auth_ds, current_uid, fun(_) -> 1 end),
        meck:expect(config_ds, env, fun(base_url, _) -> <<"https://api.test">>;
                                      (edge_base_url, _) -> <<"https://edge.test/edge">>;
                                      (_, Default) -> Default end),
        meck:expect(elib_response, success, fun(R, P, _) -> R#{payload => P} end),
        meck:expect(elib_response, error, fun(R, _, _) -> R#{denied => true} end),
        Url = <<"https://api.test/api/v1/attachment/content?ticket=a.b-c">>,
        meck:expect(attach_logic, view_url, fun(1, <<"u1/x">>) -> {ok, Url} end),
        meck:expect(attach_logic, web_view_url, fun(1, <<"u1/x">>) -> web_attachment_targets:resolve_ticket(Url) end),
        {ok, Native, _} = attach_handler:init(#{}, #{action => view_url}),
        ?assertEqual(Url, maps:get(<<"url">>, maps:get(payload, Native))),
        {ok, Web, _} = attach_handler:init(#{}, #{action => view_url, surface => web_human}),
        ?assertEqual(<<"https://edge.test/edge?ticket=a.b-c">>, maps:get(<<"url">>, maps:get(payload, Web))),
        meck:expect(attach_logic, web_view_url, fun(_, _) -> {error, forbidden} end),
        {ok, Denied, _} = attach_handler:init(#{}, #{action => view_url, surface => web_human}),
        ?assertEqual(true, maps:get(denied, Denied)),
        meck:expect(attach_logic, web_view_url, fun(_, _) -> web_attachment_targets:resolve_ticket(<<"https://evil.test/x">>) end),
        {ok, Bad, _} = attach_handler:init(#{}, #{action => view_url, surface => web_human}),
        ?assertEqual(true, maps:get(denied, Bad))
    after
        case Saved of {ok, V} -> application:set_env(imboy, garage, V); undefined -> application:unset_env(imboy, garage) end,
        lists:foreach(fun meck:unload/1, lists:reverse(Modules))
    end.
