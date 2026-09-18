%% @doc AG31-03：agent_grant_handler API 契约套件（接口层形状：请求形状 →
%% 命令调用 → 响应形状；router 未接——本套件直接驱动 handler，.contract 零变更）。
%%
%% meck agent_grant_command（application 语义真源已有 agent_grant_tests 覆盖），
%% 本套件只冻结**接口面**：
%%   * 请求归一：workspace_scope_kind 二进制 → 原子；缺失必填键 → 400 且
%%     不触达 command（fail fast）；
%%   * 响应形状：成功 issue=201 / get,list,revoke=200，body 键集稳定、
%%     effective_status 等原子转 binary（JSON 安全）；
%%   * 错误映射表（router 接入时沿用）：400 请求不可受理 / 403 身份边界 /
%%     404 缺失 / 409 冲突 / 503 fail-closed 不可用。
-module(agent_grant_api_contract_tests).

-include_lib("eunit/include/eunit.hrl").

-define(CMD, agent_grant_command).

-define(ORG, 22).
-define(GRANT_ID, 4242).
-define(NOW, {{2026, 9, 17}, {12, 0, 0}}).

%% ===================================================================
%% 套件夹具
%% ===================================================================

handler_contract_test_() ->
    {foreach,
        fun() ->
            meck:new(?CMD, [no_link]),
            ok
        end,
        fun(_) ->
            meck:unload(?CMD),
            ok
        end,
        [
            fun issue_shape_tests/1,
            fun get_list_shape_tests/1,
            fun revoke_shape_tests/1,
            fun error_mapping_tests/1
        ]}.

%% ------------------------------------------------------------------
%% issue 形状
%% ------------------------------------------------------------------

issue_shape_tests(_) ->
    [
        {"issue: 201 body shape (grant_id/version/effective_status binary/replay)", fun() ->
            meck:expect(?CMD, issue, fun(_C, Ctx) ->
                %% 请求归一：scope 二进制 → 原子；now 透传
                ?assertEqual(none, maps:get(workspace_scope_kind, Ctx)),
                ?assertEqual(?NOW, maps:get(now, Ctx)),
                {ok, #{
                    grant_id => ?GRANT_ID, version => 1, effective_status => active, replay => false
                }}
            end),
            R = agent_grant_handler:handle_issue(conn(), issue_req(#{})),
            ?assertEqual(201, maps:get(status, R)),
            Body = maps:get(body, R),
            ?assertEqual(?GRANT_ID, maps:get(grant_id, Body)),
            ?assertEqual(1, maps:get(version, Body)),
            ?assertEqual(<<"active">>, maps:get(effective_status, Body)),
            ?assertEqual(false, maps:get(replay, Body))
        end},
        {"issue: scope binary normalized before command call", fun() ->
            meck:expect(?CMD, issue, fun(_C, Ctx) ->
                ?assertEqual(explicit, maps:get(workspace_scope_kind, Ctx)),
                {ok, #{grant_id => 1, version => 1, effective_status => active, replay => false}}
            end),
            R = agent_grant_handler:handle_issue(
                conn(), issue_req(#{workspace_scope_kind => <<"explicit">>})
            ),
            ?assertEqual(201, maps:get(status, R))
        end},
        {"issue: command error mapped via error table", fun() ->
            meck:expect(?CMD, issue, fun(_C, _Ctx) ->
                {error, {unknown_capability, {<<"a">>, <<"b">>, <<"c">>}}}
            end),
            R = agent_grant_handler:handle_issue(conn(), issue_req(#{})),
            ?assertEqual(400, maps:get(status, R)),
            ?assertEqual(true, maps:get(error, maps:get(body, R)))
        end}
    ].

%% ------------------------------------------------------------------
%% get/list 形状
%% ------------------------------------------------------------------

get_list_shape_tests(_) ->
    [
        {"get: 200 full view shape (JSON-safe)", fun() ->
            meck:expect(?CMD, get, fun(_C, ?ORG, ?GRANT_ID, ?NOW) ->
                {ok, grant_view()}
            end),
            R = agent_grant_handler:handle_get(conn(), #{
                organization_id => ?ORG, grant_id => ?GRANT_ID, now => ?NOW
            }),
            ?assertEqual(200, maps:get(status, R)),
            Body = maps:get(body, R),
            ?assertEqual(<<"explicit">>, maps:get(workspace_scope_kind, Body)),
            ?assertEqual(<<"active">>, maps:get(effective_status, Body)),
            ?assertEqual(<<"active">>, maps:get(stored_status, Body)),
            ?assertEqual([33], maps:get(workspace_ids, Body)),
            Caps = maps:get(capabilities, Body),
            ?assertEqual(1, length(Caps)),
            ?assertEqual(#{}, maps:get(constraint, hd(Caps)))
        end},
        {"get: missing keys -> 400 without touching command", fun() ->
            meck:reset(?CMD),
            R = agent_grant_handler:handle_get(conn(), #{organization_id => ?ORG}),
            ?assertEqual(400, maps:get(status, R)),
            ?assertEqual(0, meck:num_calls(?CMD, get, '_'))
        end},
        {"get: not_found -> 404", fun() ->
            meck:expect(?CMD, get, fun(_C, _O, _G, _N) -> {error, not_found} end),
            R = agent_grant_handler:handle_get(conn(), #{
                organization_id => ?ORG, grant_id => ?GRANT_ID, now => ?NOW
            }),
            ?assertEqual(404, maps:get(status, R))
        end},
        {"list: 200 grants array + missing now -> 400", fun() ->
            meck:expect(?CMD, list, fun(_C, Filter) ->
                ?assertEqual(?NOW, maps:get(now, Filter)),
                ?assertEqual(?ORG, maps:get(organization_id, Filter)),
                {ok, [grant_view()]}
            end),
            R = agent_grant_handler:handle_list(conn(), #{organization_id => ?ORG, now => ?NOW}),
            ?assertEqual(200, maps:get(status, R)),
            ?assertEqual(1, length(maps:get(grants, maps:get(body, R)))),
            %% now 缺失 → fail fast
            R2 = agent_grant_handler:handle_list(conn(), #{organization_id => ?ORG}),
            ?assertEqual(400, maps:get(status, R2))
        end}
    ].

%% ------------------------------------------------------------------
%% revoke 形状
%% ------------------------------------------------------------------

revoke_shape_tests(_) ->
    [
        {"revoke: 200 body shape", fun() ->
            meck:expect(?CMD, revoke, fun(_C, Ctx) ->
                ?assertEqual(3, maps:get(expected_version, Ctx)),
                ?assertEqual(?NOW, maps:get(now, Ctx)),
                {ok, #{grant_id => ?GRANT_ID, version => 4, effective_status => revoked}}
            end),
            Req = #{
                organization_id => ?ORG,
                grant_id => ?GRANT_ID,
                revoker_user_id => 7,
                expected_version => 3,
                now => ?NOW
            },
            R = agent_grant_handler:handle_revoke(conn(), Req),
            ?assertEqual(200, maps:get(status, R)),
            Body = maps:get(body, R),
            ?assertEqual(<<"revoked">>, maps:get(effective_status, Body)),
            ?assertEqual(4, maps:get(version, Body))
        end},
        {"revoke: version_conflict -> 409; already_revoked -> 409", fun() ->
            meck:expect(?CMD, revoke, fun(_C, _Ctx) -> {error, version_conflict} end),
            R = agent_grant_handler:handle_revoke(conn(), revoke_req()),
            ?assertEqual(409, maps:get(status, R)),
            meck:expect(?CMD, revoke, fun(_C, _Ctx) -> {error, already_revoked} end),
            R2 = agent_grant_handler:handle_revoke(conn(), revoke_req()),
            ?assertEqual(409, maps:get(status, R2))
        end}
    ].

%% ------------------------------------------------------------------
%% 错误映射表（router 接入时沿用的冻结面）
%% ------------------------------------------------------------------

error_mapping_tests(_) ->
    Cases = [
        {validation_failed, 400},
        {invalid_workspace_scope, 400},
        {invalid_validity, 400},
        {delegator_not_human, 403},
        {agent_not_agent, 403},
        {agent_membership_denied, 403},
        {cross_org_workspace, 403},
        {delegator_not_found, 404},
        {agent_not_found, 404},
        {not_found, 404},
        {revoker_not_found, 404},
        {idempotency_conflict, 409},
        {already_revoked, 409},
        {version_conflict, 409},
        {membership_unavailable, 503},
        {catalog_unavailable, 503},
        {{unknown_capability, {<<"a">>, <<"b">>, <<"c">>}}, 400},
        {{invalid_constraint, <<"k">>}, 400},
        {{db_error, anything}, 503}
    ],
    [
        {reason_label(Reason) ++ " -> " ++ integer_to_list(Status), fun() ->
            meck:expect(?CMD, issue, fun(_C, _Ctx) -> {error, Reason} end),
            R = agent_grant_handler:handle_issue(conn(), issue_req(#{})),
            ?assertEqual(Status, maps:get(status, R)),
            Body = maps:get(body, R),
            ?assertEqual(true, maps:get(error, Body)),
            ?assert(is_binary(maps:get(code, Body)))
        end}
     || {Reason, Status} <- Cases
    ].

reason_label(R) when is_atom(R) -> atom_to_list(R);
reason_label({Tag, _}) when is_atom(Tag) -> atom_to_list(Tag);
reason_label(_) -> "unknown".

%% ===================================================================
%% fixtures
%% ===================================================================

issue_req(Over) ->
    maps:merge(
        #{
            organization_id => ?ORG,
            agent_id => 11,
            delegator_user_id => 77,
            workspace_scope_kind => <<"none">>,
            workspace_ids => [],
            capabilities => [
                #{capability => <<"echo">>, action => <<"invoke">>, resource_type => <<"message">>}
            ],
            valid_from => {{2026, 9, 1}, {0, 0, 0}},
            expires_at => {{2027, 9, 1}, {0, 0, 0}},
            idempotency_key => <<"k1">>,
            now => ?NOW
        },
        Over
    ).

revoke_req() ->
    #{
        organization_id => ?ORG,
        grant_id => ?GRANT_ID,
        revoker_user_id => 7,
        expected_version => 1,
        now => ?NOW
    }.

grant_view() ->
    #{
        id => ?GRANT_ID,
        agent_id => 11,
        organization_id => ?ORG,
        delegator_user_id => 77,
        workspace_scope_kind => explicit,
        status => active,
        valid_from => {{2026, 9, 1}, {0, 0, 0}},
        expires_at => {{2027, 9, 1}, {0, 0, 0}},
        version => 1,
        workspace_ids => [33],
        capabilities => [
            #{
                capability => <<"echo">>,
                action => <<"invoke">>,
                resource_type => <<"message">>,
                constraint => #{}
            }
        ],
        effective_status => active
    }.

conn() ->
    self().
