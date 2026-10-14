-module(adm_enterprise_directory_handler_tests).
-include_lib("eunit/include/eunit.hrl").

configuration_boundary_test() ->
    Modules = [
        cowboy_req, adm_acl, elib_param, elib_response, enterprise_directory_source_config_logic
    ],
    lists:foreach(fun(Module) -> meck:new(Module, [non_strict, no_link]) end, Modules),
    try
        setup(),
        Body = #{
            <<"external_organization_id">> => <<"oa-org">>,
            <<"confirmed_owner_user_id">> => 71,
            <<"enabled">> => true,
            <<"authorization_expires_at">> => <<"2030-01-01T00:00:00Z">>,
            <<"expected_version">> => 0,
            <<"adm_user_id">> => 999
        },
        Req = #{method => <<"POST">>, app_id => <<"17">>, body => Body},
        State = #{action => configure_source, adm_user_id => 31},
        meck:expect(
            enterprise_directory_source_config_logic,
            configure_source,
            fun(#{adm_user_id := 31}, 17, <<"oa-org">>, #{confirmed_owner_user_id := 71}) ->
                {ok, #{
                    <<"enabled">> => true,
                    <<"configuration_version">> => 1,
                    <<"secret">> => <<"hidden">>
                }}
            end
        ),
        {ok, Response, _} = adm_enterprise_directory_handler:init(Req, State),
        ?assertMatch(#{result := #{<<"enabled">> := true}}, Response),
        ?assertNot(maps:is_key(<<"secret">>, maps:get(result, Response))),
        ?assertMatch(
            {ok, #{code := 400}, _},
            adm_enterprise_directory_handler:init(Req#{app_id => <<"bad">>}, State)
        ),
        ?assertMatch(
            {ok, #{code := 401}, _},
            adm_enterprise_directory_handler:init(Req, maps:remove(adm_user_id, State))
        ),
        meck:expect(adm_acl, ensure_permission, fun(_, _, Request) ->
            {error, Request#{code => 403}}
        end),
        ?assertMatch({ok, #{code := 403}, _}, adm_enterprise_directory_handler:init(Req, State)),
        ?assertEqual(
            1, meck:num_calls(enterprise_directory_source_config_logic, configure_source, '_')
        ),
        ?assertMatch(
            {ok, #{code := 405}, _},
            adm_enterprise_directory_handler:init(Req#{method => <<"GET">>}, State)
        )
    after
        lists:foreach(fun(Module) -> meck:unload(Module) end, Modules)
    end.

setup() ->
    meck:expect(cowboy_req, method, fun(Req) -> maps:get(method, Req) end),
    meck:expect(cowboy_req, binding, fun(application_id, Req) -> maps:get(app_id, Req) end),
    meck:expect(cowboy_req, reply, fun(Code, _, _, Req) -> Req#{code => Code} end),
    meck:expect(adm_acl, ensure_permission, fun(_, <<"enterprise_business:write">>, _) -> ok end),
    meck:expect(elib_param, post, fun(Req) -> maps:get(body, Req) end),
    meck:expect(elib_response, success, fun(Req, Result) -> Req#{result => Result} end),
    meck:expect(elib_response, error, fun(Req, _, Code) -> Req#{code => Code} end).
