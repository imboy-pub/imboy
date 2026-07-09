-module(mcp_governance_logic_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc MCP 治理 logic 审批状态机 + grants 测试（Phase 3 T3.5）
%%% repo/registry/审计 用 meck mock，无需 DB。
%%%===================================================================

logic_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun t_approve_ok/1,
        fun t_reject_revoked/1,
        fun t_approve_notfound/1,
        fun t_grants_shape/1
    ]}.

setup() ->
    meck:new(mcp_client_repo, [no_link, passthrough]),
    meck:new(mcp_client_grant_repo, [no_link, passthrough]),
    meck:new(mcp_audit_repo, [no_link, passthrough]),
    meck:new(barrel_mcp_registry, [no_link, passthrough]),
    meck:new(adm_operation_log_ds, [no_link, passthrough]),
    meck:expect(mcp_audit_repo, insert, fun(_, _, _, _, _, _) -> {ok, [#{}]} end),
    meck:expect(adm_operation_log_ds, insert, fun(_, _, _, _, _, _) -> {ok, 1} end),
    meck:expect(barrel_mcp_registry, all, fun(tool) -> [{<<"get_contacts">>, #{}}] end),
    meck:expect(mcp_client_grant_repo, upsert, fun(_, _, _) -> {ok, [#{}]} end),
    ok.

cleanup(_) -> meck:unload().

t_approve_ok(_) ->
    fun() ->
        meck:expect(mcp_client_repo, find, fun(900) -> {ok, #{<<"owner_uid">> => 42}} end),
        meck:expect(mcp_client_repo, set_status, fun(900, <<"approved">>, _, _) -> {ok, 1} end),
        R = mcp_governance_logic:approve(900, 7, <<"1.2.3.4">>),
        ?assertMatch({ok, #{<<"status">> := <<"approved">>}}, R),
        %% approve 授予当前全部 tool
        ?assert(meck:called(mcp_client_grant_repo, upsert, [900, <<"get_contacts">>, true]))
    end.

t_reject_revoked(_) ->
    fun() ->
        meck:expect(mcp_client_repo, find, fun(900) -> {ok, #{<<"owner_uid">> => 42}} end),
        meck:expect(mcp_client_repo, set_status, fun(900, <<"revoked">>, <<"spam">>, _) ->
            {ok, 1}
        end),
        R = mcp_governance_logic:reject(900, <<"spam">>, 7, <<"ip">>),
        ?assertMatch({ok, #{<<"status">> := <<"revoked">>}}, R)
    end.

t_approve_notfound(_) ->
    fun() ->
        meck:expect(mcp_client_repo, find, fun(999) -> {error, notfound} end),
        ?assertMatch({error, _}, mcp_governance_logic:approve(999, 7, <<"ip">>))
    end.

t_grants_shape(_) ->
    fun() ->
        meck:expect(mcp_client_grant_repo, list_by_client, fun(900) ->
            {ok, [#{<<"tool_name">> => <<"get_contacts">>, <<"enabled">> => true}]}
        end),
        {ok, G} = mcp_governance_logic:grants(900),
        ?assertMatch(#{<<"tools">> := [#{<<"name">> := <<"get_contacts">>}], <<"scopes">> := []}, G)
    end.
