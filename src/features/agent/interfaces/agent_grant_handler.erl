%%% @doc Agent Grant 命令面**薄 Handler**（AG31-03；接口层：请求形状 → 命令调用
%%% → 响应形状）。
%%%
%%% **不接 router**（AG31-03 铁律：router 由 integration owner 接管；本模块
%%% 不被 imboy_router.erl 引用，.contract 零变更）。本模块只做形状转换：
%%%
%%%   * 输入：`Req` 为 plain map（未来 HTTP 层从 JSON body/param 解出的形状；
%%%     DB 连接句柄 `Conn` 显式作首参注入——接入 Cowboy 时由 route state 提供）。
%%%   * 输出：`#{status := StatusCode, body := Body}`——`status` 是预留的
%%%     HTTP 语义码（201/200/400/403/404/409/503），`body` 是 JSON 安全形状
%%%     （binary/integer/boolean/list/map，原子错误码转为 binary `code`）。
%%%   * **本模块不做**：不读库、不写 SQL、不做业务判定、不缓存事实、不取
%%%     系统时间（`now` 由请求方注入；接入 HTTP 层时由该层提供服务器时钟）。
%%%
%%% 状态码映射（AG31-03 API 契约草案，integration owner 接 router 时沿用）：
%%%
%%%   * issue 成功 → 201；get/list 成功 → 200；revoke 成功 → 200；
%%%   * validation_failed / invalid_workspace_scope / invalid_validity /
%%%     unknown_capability / invalid_constraint → 400（请求不可受理）；
%%%   * delegator_not_human / agent_not_agent / agent_membership_denied /
%%%     cross_org_workspace → 403（身份/边界拒绝）；
%%%   * delegator_not_found / agent_not_found / not_found → 404；
%%%   * idempotency_conflict / already_revoked / version_conflict → 409；
%%%   * membership_unavailable / catalog_unavailable / db_error / 其它 → 503
%%%     （fail closed 对外呈现）。
-module(agent_grant_handler).

-export([handle_issue/2, handle_get/2, handle_list/2, handle_revoke/2]).

%% ===================================================================
%% issue：POST 语义
%% ===================================================================

%% @doc 请求形状：organization_id/agent_id/delegator_user_id、
%% workspace_scope_kind（<<"none">>|<<"explicit">>|none|explicit）、
%% workspace_ids、capabilities（[#{capability,action,resource_type,constraint}]）、
%% valid_from/expires_at、idempotency_key、now。
handle_issue(Conn, Req) ->
    case agent_grant_command:issue(Conn, normalize_issue_req(Req)) of
        {ok, Result} ->
            #{
                status => 201,
                body => #{
                    grant_id => maps:get(grant_id, Result),
                    version => maps:get(version, Result),
                    effective_status => atom_b(maps:get(effective_status, Result)),
                    replay => maps:get(replay, Result)
                }
            };
        {error, Reason} ->
            reply_error(Reason)
    end.

normalize_issue_req(Req) ->
    Req#{
        workspace_scope_kind => scope_in(maps:get(workspace_scope_kind, Req, undefined))
    }.

%% ===================================================================
%% get / list：GET 语义
%% ===================================================================

%% @doc 请求形状：organization_id/grant_id/now。
handle_get(Conn, Req) ->
    Now = maps:get(now, Req, undefined),
    case
        validate_present([
            {organization_id, maps:get(organization_id, Req, undefined)},
            {grant_id, maps:get(grant_id, Req, undefined)},
            {now, Now}
        ])
    of
        ok ->
            case
                agent_grant_command:get(
                    Conn,
                    maps:get(organization_id, Req),
                    maps:get(grant_id, Req),
                    Now
                )
            of
                {ok, View} ->
                    #{status => 200, body => view_out(View)};
                {error, Reason} ->
                    reply_error(Reason)
            end;
        {error, _} ->
            reply_error(validation_failed)
    end.

%% @doc 请求形状：organization_id/now；可选 agent_id/limit/offset。
handle_list(Conn, Req) ->
    Now = maps:get(now, Req, undefined),
    case
        validate_present([{organization_id, maps:get(organization_id, Req, undefined)}, {now, Now}])
    of
        ok ->
            Filter0 = maps:with([organization_id, agent_id, limit, offset], Req),
            %% list/2 恒返回 {ok, Views}（org 域 bounded 空列表亦为 ok）
            {ok, Views} = agent_grant_command:list(Conn, Filter0#{now => Now}),
            #{status => 200, body => #{grants => [view_out(V) || V <- Views]}};
        {error, _} ->
            reply_error(validation_failed)
    end.

%% ===================================================================
%% revoke：POST 语义
%% ===================================================================

%% @doc 请求形状：organization_id/grant_id/revoker_user_id/expected_version/now。
handle_revoke(Conn, Req) ->
    case agent_grant_command:revoke(Conn, Req) of
        {ok, Result} ->
            #{
                status => 200,
                body => #{
                    grant_id => maps:get(grant_id, Result),
                    version => maps:get(version, Result),
                    effective_status => atom_b(maps:get(effective_status, Result))
                }
            };
        {error, Reason} ->
            reply_error(Reason)
    end.

%% ===================================================================
%% 响应形状 / 错误映射
%% ===================================================================

view_out(View) ->
    #{
        grant_id => maps:get(id, View),
        agent_id => maps:get(agent_id, View),
        organization_id => maps:get(organization_id, View),
        delegator_user_id => maps:get(delegator_user_id, View),
        workspace_scope_kind => atom_b(maps:get(workspace_scope_kind, View)),
        workspace_ids => maps:get(workspace_ids, View, []),
        capabilities => caps_out(maps:get(capabilities, View, [])),
        stored_status => atom_b(maps:get(status, View)),
        effective_status => atom_b(maps:get(effective_status, View)),
        version => maps:get(version, View)
    }.

caps_out(Caps) ->
    [
        #{
            capability => maps:get(capability, C),
            action => maps:get(action, C),
            resource_type => maps:get(resource_type, C),
            constraint => maps:get(constraint, C, #{})
        }
     || C <- Caps
    ].

%% 错误码 → 预留 HTTP 语义码（见模块 doc 映射表）
reply_error(Reason) ->
    #{
        status => status_for(Reason),
        body => #{code => reason_b(Reason), error => true}
    }.

status_for(Reason) when is_atom(Reason) ->
    status_atom(Reason);
status_for({unknown_capability, _}) ->
    400;
status_for({invalid_constraint, _}) ->
    400;
status_for({_Other, _}) ->
    503.

status_atom(validation_failed) -> 400;
status_atom(invalid_workspace_scope) -> 400;
status_atom(invalid_validity) -> 400;
status_atom(delegator_not_human) -> 403;
status_atom(agent_not_agent) -> 403;
status_atom(agent_membership_denied) -> 403;
status_atom(cross_org_workspace) -> 403;
status_atom(delegator_not_found) -> 404;
status_atom(agent_not_found) -> 404;
status_atom(not_found) -> 404;
status_atom(idempotency_conflict) -> 409;
status_atom(already_revoked) -> 409;
status_atom(version_conflict) -> 409;
status_atom(membership_unavailable) -> 503;
status_atom(catalog_unavailable) -> 503;
status_atom(revoker_not_found) -> 404;
status_atom(_Other) -> 503.

reason_b({unknown_capability, {C, A, R}}) ->
    <<"unknown_capability: ", C/binary, ":", A/binary, ":", R/binary>>;
reason_b({invalid_constraint, Key}) ->
    <<"invalid_constraint: ", Key/binary>>;
reason_b({Tag, _Raw}) when is_atom(Tag) ->
    atom_to_binary(Tag, utf8);
reason_b(Reason) when is_atom(Reason) ->
    atom_to_binary(Reason, utf8);
reason_b(_Other) ->
    <<"internal_error">>.

%% ===================================================================
%% 形状归一
%% ===================================================================

scope_in(<<"none">>) -> none;
scope_in(<<"explicit">>) -> explicit;
scope_in(none) -> none;
scope_in(explicit) -> explicit;
scope_in(Other) -> Other.

atom_b(A) when is_atom(A) -> atom_to_binary(A, utf8);
atom_b(B) when is_binary(B) -> B;
atom_b(Other) -> Other.

validate_present([]) ->
    ok;
validate_present([{_K, undefined} | _Rest]) ->
    {error, missing};
validate_present([_ | Rest]) ->
    validate_present(Rest).
