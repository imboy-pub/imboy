-module(mcp_authz_gate_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc MCP 授权闸门 + 治理 logic 测试（Phase 3 T3.5）
%%% 覆盖安全核心：enforce on/off、approved/pending/revoked、tool grant、未认证。
%%% repo 用 meck mock，无需真实 DB。
%%%===================================================================

gate_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun t_enforce_off_allows_pending/1,
        fun t_enforce_on_approved_granted_allows/1,
        fun t_enforce_on_approved_not_granted_denies/1,
        fun t_enforce_on_pending_denies/1,
        fun t_enforce_on_revoked_denies/1,
        fun t_unauth_off_allows_on_denies/1
    ]}.

setup() ->
    meck:new(mcp_client_repo, [no_link, passthrough]),
    meck:new(mcp_client_grant_repo, [no_link, passthrough]),
    meck:new(mcp_audit_repo, [no_link, passthrough]),
    %% 登记恒成功、审计恒成功；client_id 固定 900
    meck:expect(mcp_client_repo, ensure, fun(_Uid, _Name) -> {ok, 900} end),
    meck:expect(mcp_audit_repo, insert, fun(_, _, _, _, _, _) -> {ok, [#{}]} end),
    application:set_env(imboy, mcp_governance_enforce, false),
    ok.

cleanup(_) ->
    application:set_env(imboy, mcp_governance_enforce, false),
    meck:unload().

ctx(Uid) -> #{auth_info => Uid}.

t_enforce_off_allows_pending(_) ->
    fun() ->
        application:set_env(imboy, mcp_governance_enforce, false),
        meck:expect(mcp_client_repo, find, fun(900) -> {ok, #{<<"status">> => <<"pending">>}} end),
        ?assertEqual(ok, mcp_authz_gate:check(<<"get_contacts">>, ctx(42)))
    end.

t_enforce_on_approved_granted_allows(_) ->
    fun() ->
        application:set_env(imboy, mcp_governance_enforce, true),
        meck:expect(mcp_client_repo, find, fun(900) -> {ok, #{<<"status">> => <<"approved">>}} end),
        meck:expect(mcp_client_grant_repo, is_enabled, fun(900, <<"get_contacts">>) -> true end),
        ?assertEqual(ok, mcp_authz_gate:check(<<"get_contacts">>, ctx(42)))
    end.

t_enforce_on_approved_not_granted_denies(_) ->
    fun() ->
        application:set_env(imboy, mcp_governance_enforce, true),
        meck:expect(mcp_client_repo, find, fun(900) -> {ok, #{<<"status">> => <<"approved">>}} end),
        meck:expect(mcp_client_grant_repo, is_enabled, fun(900, _) -> false end),
        ?assertMatch({deny, _}, mcp_authz_gate:check(<<"send_message">>, ctx(42)))
    end.

t_enforce_on_pending_denies(_) ->
    fun() ->
        application:set_env(imboy, mcp_governance_enforce, true),
        meck:expect(mcp_client_repo, find, fun(900) -> {ok, #{<<"status">> => <<"pending">>}} end),
        ?assertMatch({deny, _}, mcp_authz_gate:check(<<"get_contacts">>, ctx(42)))
    end.

t_enforce_on_revoked_denies(_) ->
    fun() ->
        application:set_env(imboy, mcp_governance_enforce, true),
        meck:expect(mcp_client_repo, find, fun(900) -> {ok, #{<<"status">> => <<"revoked">>}} end),
        ?assertMatch({deny, _}, mcp_authz_gate:check(<<"get_contacts">>, ctx(42)))
    end.

t_unauth_off_allows_on_denies(_) ->
    fun() ->
        application:set_env(imboy, mcp_governance_enforce, false),
        ?assertEqual(ok, mcp_authz_gate:check(<<"get_contacts">>, ctx(0))),
        application:set_env(imboy, mcp_governance_enforce, true),
        ?assertMatch({deny, _}, mcp_authz_gate:check(<<"get_contacts">>, ctx(0)))
    end.
