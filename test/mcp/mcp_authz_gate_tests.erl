-module(mcp_authz_gate_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc MCP 授权闸门测试（Phase 3 T3.5 + MCP-01 Principal 改造）
%%% 覆盖安全核心：principal map 注入、enforce on/off、approved+grant/
%%% 未授权默认拒、pending/revoked、未认证（含 legacy 整数形态）、限流降级。
%%% 治理 logic 按用例 meck；gate 是薄适配层。

principal() ->
    #{owner_uid => 42, client_id => 900, client_key => <<"mck-test">>}.

ctx(Principal) -> #{auth_info => Principal}.

setup_gate_mocks(Extra) ->
    Base =
        {mcp_governance_logic, [
            {'check_rate', 1, fun(_Key) -> allow end},
            {'authorize_client', 3, fun
                (900, 42, <<"get_contacts">>) -> allow;
                (900, 42, <<"send_message">>) -> {deny, <<"该 tool 未授权"/utf8>>};
                (_Cid, _Uid, _T) -> {deny, <<"denied">>}
            end}
        ]},
    setup_mecks([Base | Extra]).

gate_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun t_enforce_off_allows_pending/1,
        fun t_enforce_on_approved_granted_allows/1,
        fun t_enforce_on_approved_not_granted_denies/1,
        fun t_enforce_on_pending_denies/1,
        fun t_enforce_on_revoked_denies/1,
        fun t_unauth_off_allows_on_denies/1,
        fun t_rate_limited_denies/1
    ]}.

setup() ->
    application:set_env(imboy, mcp_governance_enforce, false),
    ok.

cleanup(_) ->
    application:set_env(imboy, mcp_governance_enforce, false),
    try
        meck_helper:cleanup_mock(mcp_governance_logic)
    catch
        _:_ -> ok
    end,
    ok.

t_enforce_off_allows_pending(_) ->
    fun() ->
        %% pending client：enforce off → 放行（治理 logic 的 allow 分支）
        setup_gate_mocks([]),
        ?assertEqual(ok, mcp_authz_gate:check(<<"get_contacts">>, ctx(principal())))
    end.

t_enforce_on_approved_granted_allows(_) ->
    fun() ->
        application:set_env(imboy, mcp_governance_enforce, true),
        setup_gate_mocks([]),
        ?assertEqual(ok, mcp_authz_gate:check(<<"get_contacts">>, ctx(principal())))
    end.

t_enforce_on_approved_not_granted_denies(_) ->
    fun() ->
        application:set_env(imboy, mcp_governance_enforce, true),
        setup_gate_mocks([]),
        ?assertMatch(
            {deny, _},
            mcp_authz_gate:check(<<"send_message">>, ctx(principal()))
        )
    end.

t_enforce_on_pending_denies(_) ->
    fun() ->
        application:set_env(imboy, mcp_governance_enforce, true),
        %% pending：authorize_client 对任意 tool 都 deny
        setup_mecks([
            {mcp_governance_logic, [
                {'check_rate', 1, fun(_Key) -> allow end},
                {'authorize_client', 3, fun(_Cid, _Uid, _T) -> {deny, <<"客户端待审批"/utf8>>} end}
            ]}
        ]),
        ?assertMatch({deny, _}, mcp_authz_gate:check(<<"get_contacts">>, ctx(principal())))
    end.

t_enforce_on_revoked_denies(_) ->
    fun() ->
        application:set_env(imboy, mcp_governance_enforce, true),
        setup_mecks([
            {mcp_governance_logic, [
                {'check_rate', 1, fun(_Key) -> allow end},
                {'authorize_client', 3, fun(_Cid, _Uid, _T) -> {deny, <<"客户端授权已撤销"/utf8>>} end}
            ]}
        ]),
        ?assertMatch({deny, _}, mcp_authz_gate:check(<<"get_contacts">>, ctx(principal())))
    end.

t_unauth_off_allows_on_denies(_) ->
    fun() ->
        %% 未认证（legacy 整数形态/无 auth_info）：gate 直接拒绝，不经治理 logic
        setup_gate_mocks([]),
        ?assertMatch({deny, _}, mcp_authz_gate:check(<<"get_contacts">>, ctx(0))),
        ?assertMatch({deny, _}, mcp_authz_gate:check(<<"get_contacts">>, #{}))
    end.

t_rate_limited_denies(_) ->
    fun() ->
        setup_mecks([
            {mcp_governance_logic, [
                {'check_rate', 1, fun(_Key) -> {deny, rate_limited} end}
            ]}
        ]),
        ?assertMatch(
            {deny, _},
            mcp_authz_gate:check(<<"get_contacts">>, ctx(principal()))
        )
    end.

%% ===================================================================
setup_mecks(Mecks) ->
    lists:foreach(
        fun({Module, Expectations}) ->
            case meck_helper:setup_mock(Module, Expectations) of
                {ok, _} -> ok;
                {error, Reason} -> erlang:error({meck_setup, Module, Reason})
            end
        end,
        Mecks
    ),
    ok.
