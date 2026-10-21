-module(enterprise_directory_sync_handler_tests).
-include_lib("eunit/include/eunit.hrl").

sync_handler_test() ->
    Modules = [cowboy_req, elib_param, enterprise_directory_sync_logic, enterprise_internal_error],
    lists:foreach(fun(Module) -> meck:new(Module, [non_strict, no_link]) end, Modules),
    try
        meck:expect(cowboy_req, method, fun(Req) -> maps:get(method, Req) end),
        meck:expect(cowboy_req, header, fun(<<"idempotency-key">>, _, <<>>) -> <<"request-key">> end),
        meck:expect(cowboy_req, reply, fun(Code, _, Body, Req) ->
            Req#{code => Code, response => Body}
        end),
        meck:expect(elib_param, post, fun(Req) -> maps:get(body, Req) end),
        meck:expect(enterprise_internal_error, reply, fun(Req, Code) -> Req#{error => Code} end),
        Snapshot = #{response_code => 200, response_body => <<"saved snapshot">>},
        ?assertMatch(
            #{code := 200, response := <<"saved snapshot">>}, invoke({ok, applied, Snapshot})
        ),
        ?assertMatch(
            #{code := 200, response := <<"saved snapshot">>}, invoke({ok, replay, Snapshot})
        ),
        lists:foreach(
            fun({Reason, Code}) ->
                ?assertMatch(#{error := Code}, invoke({error, Reason}))
            end,
            [
                {directory_source_not_configured, <<"organization_boundary_violation">>},
                {directory_bootstrap_not_authorized, <<"organization_boundary_violation">>},
                {organization_not_active, <<"organization_boundary_violation">>},
                {organization_boundary_violation, <<"organization_boundary_violation">>},
                {security_gate_closed, <<"security_gate_closed">>},
                {local_membership_conflict, <<"resource_conflict">>},
                {{membership_conflict, #{owned_projects => [1]}}, <<"resource_conflict">>},
                {digest_conflict, <<"idempotency_conflict">>},
                {stale_source_version, <<"version_conflict">>},
                {identity_conflict, <<"resource_conflict">>},
                {cycle, <<"invalid_request">>},
                {invalid_source_membership_status, <<"invalid_request">>},
                {department_archived, <<"resource_conflict">>},
                {department_not_empty, <<"resource_conflict">>},
                {name_conflict, <<"resource_conflict">>},
                {conflict, <<"resource_conflict">>},
                {local_department_admin_conflict, <<"resource_conflict">>},
                {department_not_found, <<"resource_not_found">>},
                {parent_not_found, <<"resource_not_found">>},
                {active_source_memberships, <<"resource_conflict">>},
                {directory_only_application_required, <<"organization_boundary_violation">>},
                {{403, <<"owner guard">>}, <<"organization_boundary_violation">>},
                {{409, <<"role guard">>}, <<"resource_conflict">>},
                {invalid_request, <<"invalid_request">>},
                {unexpected_database_failure, <<"internal_error">>}
            ]
        ),
        {ok, Response, _} = enterprise_directory_handler:init(#{method => <<"GET">>}, state()),
        ?assertMatch(#{code := 405}, Response)
    after
        lists:foreach(fun(Module) -> meck:unload(Module) end, Modules)
    end.

invoke(Result) ->
    meck:expect(
        enterprise_directory_sync_logic,
        sync,
        fun(#{application_id := 17}, <<"request-key">>, #{<<"fixture">> := true}) -> Result end
    ),
    Req = #{method => <<"POST">>, body => #{<<"fixture">> => true}},
    {ok, Response, _} = enterprise_directory_handler:init(Req, state()),
    Response.

state() -> #{action => sync, enterprise_internal => #{application_id => 17}}.
