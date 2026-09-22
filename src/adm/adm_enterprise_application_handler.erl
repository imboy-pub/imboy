-module(adm_enterprise_application_handler).
-compile([nowarn_deprecated_catch]).

%%%
% adm_enterprise_application_handler 是 **Admin（Human/Admin 会话）** 企业应用
% 治理面（FULL-08 / plan-full §3.1、§3.2、§7）。
%
% 端点族 A-01..A-14 与 imboyadmin 分支 run/full-candidate-admin-20260921T101806Z
% 的 src/modules/enterprise_apps/api/contracts.ts:ENDPOINTS 逐字对应（前端字符串
% 被单测钉死，后端不得漂移）。
%
% 硬边界（plan 产品硬边界 §2、§6，本模块逐条落实）：
%   * 鉴权**只**认 Admin Cookie 会话（adm_auth_middleware）+ adm_acl 权限门
%     （读 enterprise_business:read / 写 enterprise_business:write）。
%     **绝不接受** Application Credential —— OA 凭据走 /api/internal/v1/* 的
%     独立鉴权链，两者不可互换。本模块不读、不解析、不签发任何 credential 请求头。
%   * 只做平台鉴权、参数转换、错误分类；业务一律下沉
%     enterprise_admin_governance_logic（handler → logic 单向依赖）。
%   * 写面一律 CAS（expected_version）+ 审计留痕（logic 内完成）。
%   * 读面**永不**回显 secret / digest / payload（logic 侧已投影，本模块不再加键）。
%
% TSID 传输：路径/查询里的 64-bit ID 按 string 接收；出站 ID 一律 string
% （logic 已投影为 binary 数字串），防 JS 精度丢失。
%%%

-behavior(cowboy_rest).

-export([init/2]).

-include("log.hrl").
-include("common.hrl").
-include("error_code.hrl").

-define(ACL_READ, <<"enterprise_business:read">>).
-define(ACL_WRITE, <<"enterprise_business:write">>).

%% ===================================================================
%% API
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 = dispatch(Action, Method, Req0, State),
    {ok, Req1, State}.

%% ===================================================================
%% 分发
%% ===================================================================

-spec dispatch(atom(), binary(), cowboy_req:req(), map()) -> cowboy_req:req().
dispatch(applications, Method, Req0, State) ->
    applications_action(Method, Req0, State);
dispatch(application_detail, Method, Req0, State) ->
    application_detail_action(Method, Req0, State);
dispatch(application_status, Method, Req0, State) ->
    application_status_action(Method, Req0, State);
dispatch(application_scopes, Method, Req0, State) ->
    application_scopes_action(Method, Req0, State);
dispatch(credentials, Method, Req0, State) ->
    credentials_action(Method, Req0, State);
dispatch(credential_rotate, Method, Req0, State) ->
    credential_rotate_action(Method, Req0, State);
dispatch(credential_revoke, Method, Req0, State) ->
    credential_revoke_action(Method, Req0, State);
dispatch(grants, Method, Req0, State) ->
    grants_action(Method, Req0, State);
dispatch(grant, Method, Req0, State) ->
    grant_action(Method, Req0, State);
dispatch(delivery_stats, Method, Req0, State) ->
    delivery_stats_action(Method, Req0, State);
dispatch(deliveries, Method, Req0, State) ->
    deliveries_action(Method, Req0, State);
dispatch(audit_logs, Method, Req0, State) ->
    audit_logs_action(Method, Req0, State);
dispatch(_Unknown, _Method, Req0, _State) ->
    elib_response:error(Req0, <<"未知的企业治理动作"/utf8>>, ?ERR_NOT_FOUND).

%% ===================================================================
%% A-01 列表 / A-02 详情
%% ===================================================================

-spec applications_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
applications_action(<<"GET">>, Req0, State) ->
    with_read(Req0, State, fun(Req1) ->
        case parse_id(Req1, org_id) of
            error ->
                bad_id(Req1);
            {ok, OrgId} ->
                {Page, Size} = elib_param:page(Req1),
                {ok, Status} = elib_param:binary(status, Req1, <<>>),
                {ok, Keyword} = elib_param:binary(q, Req1, <<>>),
                Opts = compact_opts([{status, Status}, {q, Keyword}]),
                case enterprise_admin_governance_logic:list_applications(OrgId, Page, Size, Opts) of
                    {ok, PageMap} -> elib_response:success(Req1, PageMap);
                    {error, Reason} -> reply_error(Req1, Reason)
                end
        end
    end);
applications_action(_, Req0, _State) ->
    method_not_allowed(Req0).

-spec application_detail_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
application_detail_action(<<"GET">>, Req0, State) ->
    with_read(Req0, State, fun(Req1) ->
        case both_ids(Req1) of
            error ->
                bad_id(Req1);
            {ok, OrgId, AppId} ->
                case enterprise_admin_governance_logic:application_detail(OrgId, AppId) of
                    {ok, Detail} -> elib_response:success(Req1, Detail);
                    {error, Reason} -> reply_error(Req1, Reason)
                end
        end
    end);
application_detail_action(_, Req0, _State) ->
    method_not_allowed(Req0).

%% ===================================================================
%% A-03 生命周期 / A-04 scopes（写，CAS）
%% ===================================================================

-spec application_status_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
application_status_action(<<"POST">>, Req0, State) ->
    with_write(Req0, State, fun(Req1) ->
        case both_ids(Req1) of
            error ->
                bad_id(Req1);
            {ok, OrgId, AppId} ->
                case read_body_map(Req1) of
                    {error, Msg} ->
                        elib_response:error(Req1, Msg, ?ERR_BAD_REQUEST);
                    {ok, Body} ->
                        case
                            {bin_field(Body, <<"status">>), int_field(Body, <<"expected_version">>)}
                        of
                            {<<>>, _} ->
                                elib_response:error(Req1, <<"缺少 status"/utf8>>, ?ERR_BAD_REQUEST);
                            {_, undefined} ->
                                elib_response:error(
                                    Req1, <<"缺少 expected_version"/utf8>>, ?ERR_BAD_REQUEST
                                );
                            {Status, Version} ->
                                case
                                    enterprise_admin_governance_logic:set_status(
                                        OrgId, AppId, Version, Status, actor(State)
                                    )
                                of
                                    ok -> elib_response:success(Req1, #{<<"ok">> => true});
                                    {error, Reason} -> reply_error(Req1, Reason)
                                end
                        end
                end
        end
    end);
application_status_action(_, Req0, _State) ->
    method_not_allowed(Req0).

-spec application_scopes_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
application_scopes_action(<<"PUT">>, Req0, State) ->
    with_write(Req0, State, fun(Req1) ->
        case both_ids(Req1) of
            error ->
                bad_id(Req1);
            {ok, OrgId, AppId} ->
                case read_body_map(Req1) of
                    {error, Msg} ->
                        elib_response:error(Req1, Msg, ?ERR_BAD_REQUEST);
                    {ok, Body} ->
                        Scopes = bin_list(Body, <<"scopes">>),
                        case int_field(Body, <<"expected_version">>) of
                            undefined ->
                                elib_response:error(
                                    Req1, <<"缺少 expected_version"/utf8>>, ?ERR_BAD_REQUEST
                                );
                            Version ->
                                case
                                    enterprise_admin_governance_logic:set_scopes(
                                        OrgId, AppId, Version, Scopes, actor(State)
                                    )
                                of
                                    {ok, Applied} ->
                                        elib_response:success(Req1, #{<<"scopes">> => Applied});
                                    {error, Reason} ->
                                        reply_error(Req1, Reason)
                                end
                        end
                end
        end
    end);
application_scopes_action(_, Req0, _State) ->
    method_not_allowed(Req0).

%% ===================================================================
%% A-05 列表 / A-06 签发（同路径，按 method 分）
%% ===================================================================

-spec credentials_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
credentials_action(<<"GET">>, Req0, State) ->
    with_read(Req0, State, fun(Req1) ->
        case both_ids(Req1) of
            error ->
                bad_id(Req1);
            {ok, OrgId, AppId} ->
                {ok, Metas} = enterprise_admin_governance_logic:list_credentials(OrgId, AppId),
                elib_response:success(Req1, Metas)
        end
    end);
credentials_action(<<"POST">>, Req0, State) ->
    with_write(Req0, State, fun(Req1) ->
        case both_ids(Req1) of
            error ->
                bad_id(Req1);
            {ok, OrgId, AppId} ->
                case read_body_map(Req1) of
                    {error, Msg} ->
                        elib_response:error(Req1, Msg, ?ERR_BAD_REQUEST);
                    {ok, Body} ->
                        ExpiresAt =
                            case bin_field(Body, <<"expires_at">>) of
                                <<>> -> undefined;
                                E -> E
                            end,
                        case
                            enterprise_admin_governance_logic:issue_credential(
                                OrgId, AppId, ExpiresAt, actor(State)
                            )
                        of
                            {ok, Once} -> elib_response:success(Req1, Once);
                            {error, Reason} -> reply_error(Req1, Reason)
                        end
                end
        end
    end);
credentials_action(_, Req0, _State) ->
    method_not_allowed(Req0).

%% ===================================================================
%% A-07 轮换 / A-08 撤销
%% ===================================================================

-spec credential_rotate_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
credential_rotate_action(<<"POST">>, Req0, State) ->
    with_write(Req0, State, fun(Req1) ->
        case credential_ids(Req1) of
            error ->
                bad_id(Req1);
            {ok, OrgId, AppId, CredId} ->
                case
                    enterprise_admin_governance_logic:rotate_credential(
                        OrgId, AppId, CredId, actor(State)
                    )
                of
                    {ok, Once} -> elib_response:success(Req1, Once);
                    {error, Reason} -> reply_error(Req1, Reason)
                end
        end
    end);
credential_rotate_action(_, Req0, _State) ->
    method_not_allowed(Req0).

-spec credential_revoke_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
credential_revoke_action(<<"DELETE">>, Req0, State) ->
    with_write(Req0, State, fun(Req1) ->
        case credential_ids(Req1) of
            error ->
                bad_id(Req1);
            {ok, OrgId, AppId, CredId} ->
                case
                    enterprise_admin_governance_logic:revoke_credential(
                        OrgId, AppId, CredId, actor(State)
                    )
                of
                    ok -> elib_response:success(Req1, #{<<"ok">> => true});
                    {error, Reason} -> reply_error(Req1, Reason)
                end
        end
    end);
credential_revoke_action(_, Req0, _State) ->
    method_not_allowed(Req0).

%% ===================================================================
%% A-09 列表 / A-10 签发 / A-11 CAS 增删
%% ===================================================================

-spec grants_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
grants_action(<<"GET">>, Req0, State) ->
    with_read(Req0, State, fun(Req1) ->
        case both_ids(Req1) of
            error ->
                bad_id(Req1);
            {ok, OrgId, AppId} ->
                case enterprise_admin_governance_logic:list_grants(OrgId, AppId) of
                    {ok, Rows} -> elib_response:success(Req1, Rows);
                    {error, Reason} -> reply_error(Req1, Reason)
                end
        end
    end);
grants_action(<<"POST">>, Req0, State) ->
    with_write(Req0, State, fun(Req1) ->
        case both_ids(Req1) of
            error ->
                bad_id(Req1);
            {ok, OrgId, AppId} ->
                case read_body_map(Req1) of
                    {error, Msg} ->
                        elib_response:error(Req1, Msg, ?ERR_BAD_REQUEST);
                    {ok, Body} ->
                        case int_field(Body, <<"expected_version">>) of
                            undefined ->
                                elib_response:error(
                                    Req1, <<"缺少 expected_version"/utf8>>, ?ERR_BAD_REQUEST
                                );
                            Version ->
                                Input = grant_input(Body),
                                case
                                    enterprise_admin_governance_logic:issue_grant(
                                        OrgId, AppId, Input, Version, actor(State)
                                    )
                                of
                                    {ok, Grant} -> elib_response:success(Req1, Grant);
                                    {error, Reason} -> reply_error(Req1, Reason)
                                end
                        end
                end
        end
    end);
grants_action(_, Req0, _State) ->
    method_not_allowed(Req0).

-spec grant_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
grant_action(<<"PATCH">>, Req0, State) ->
    with_write(Req0, State, fun(Req1) ->
        case grant_ids(Req1) of
            error ->
                bad_id(Req1);
            {ok, OrgId, AppId, GrantId} ->
                case read_body_map(Req1) of
                    {error, Msg} ->
                        elib_response:error(Req1, Msg, ?ERR_BAD_REQUEST);
                    {ok, Body} ->
                        case int_field(Body, <<"expected_version">>) of
                            undefined ->
                                elib_response:error(
                                    Req1, <<"缺少 expected_version"/utf8>>, ?ERR_BAD_REQUEST
                                );
                            Version ->
                                Patch = grant_patch(Body),
                                case
                                    enterprise_admin_governance_logic:patch_grant(
                                        OrgId, AppId, GrantId, Version, Patch, actor(State)
                                    )
                                of
                                    ok -> elib_response:success(Req1, #{<<"ok">> => true});
                                    {error, Reason} -> reply_error(Req1, Reason)
                                end
                        end
                end
        end
    end);
grant_action(_, Req0, _State) ->
    method_not_allowed(Req0).

%% ===================================================================
%% A-12 统计 / A-13 投递列表 / A-14 审计
%% ===================================================================

-spec delivery_stats_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
delivery_stats_action(<<"GET">>, Req0, State) ->
    with_read(Req0, State, fun(Req1) ->
        case both_ids(Req1) of
            error ->
                bad_id(Req1);
            {ok, OrgId, AppId} ->
                case enterprise_admin_governance_logic:delivery_stats(OrgId, AppId) of
                    {ok, Stats} -> elib_response:success(Req1, Stats);
                    {error, Reason} -> reply_error(Req1, Reason)
                end
        end
    end);
delivery_stats_action(_, Req0, _State) ->
    method_not_allowed(Req0).

-spec deliveries_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
deliveries_action(<<"GET">>, Req0, State) ->
    with_read(Req0, State, fun(Req1) ->
        case both_ids(Req1) of
            error ->
                bad_id(Req1);
            {ok, OrgId, AppId} ->
                {Page, Size} = elib_param:page(Req1),
                {ok, Status} = elib_param:binary(status, Req1, <<>>),
                StatusOpt =
                    case Status of
                        <<>> -> undefined;
                        S -> S
                    end,
                case
                    enterprise_admin_governance_logic:list_deliveries(
                        OrgId, AppId, Page, Size, StatusOpt
                    )
                of
                    {ok, Rows} -> elib_response:success(Req1, Rows);
                    {error, Reason} -> reply_error(Req1, Reason)
                end
        end
    end);
deliveries_action(_, Req0, _State) ->
    method_not_allowed(Req0).

-spec audit_logs_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
audit_logs_action(<<"GET">>, Req0, State) ->
    with_read(Req0, State, fun(Req1) ->
        case both_ids(Req1) of
            error ->
                bad_id(Req1);
            {ok, OrgId, AppId} ->
                {Page, Size} = elib_param:page(Req1),
                case enterprise_admin_governance_logic:list_audit(OrgId, AppId, Page, Size) of
                    {ok, Rows} -> elib_response:success(Req1, Rows);
                    {error, Reason} -> reply_error(Req1, Reason)
                end
        end
    end);
audit_logs_action(_, Req0, _State) ->
    method_not_allowed(Req0).

%% ===================================================================
%% 鉴权包裹（读 / 写）
%% ===================================================================

-spec with_read(cowboy_req:req(), map(), fun((cowboy_req:req()) -> cowboy_req:req())) ->
    cowboy_req:req().
with_read(Req0, State, Fun) ->
    case adm_acl:ensure_permission(State, ?ACL_READ, Req0) of
        {error, RespReq} -> RespReq;
        ok -> Fun(Req0)
    end.

-spec with_write(cowboy_req:req(), map(), fun((cowboy_req:req()) -> cowboy_req:req())) ->
    cowboy_req:req().
with_write(Req0, State, Fun) ->
    case adm_acl:ensure_permission(State, ?ACL_WRITE, Req0) of
        {error, RespReq} -> RespReq;
        ok -> Fun(Req0)
    end.

%% ===================================================================
%% 内部：ID / 参数 / body
%% ===================================================================

-spec parse_id(cowboy_req:req(), atom()) -> {ok, integer()} | error.
parse_id(Req, Key) ->
    case cowboy_req:binding(Key, Req) of
        V when is_integer(V), V > 0 -> {ok, V};
        V when is_binary(V) ->
            try binary_to_integer(string:trim(V)) of
                Id when Id > 0 -> {ok, Id};
                _ -> error
            catch
                _:_ -> error
            end;
        _ ->
            error
    end.

%% 路径绑定名与 imboy_router:enterprise_application_governance_routes/0 的
%% `:org_id` / `:application_id` 逐字一致（改一处必须同时改另一处）。
-spec both_ids(cowboy_req:req()) -> {ok, integer(), integer()} | error.
both_ids(Req) ->
    case {parse_id(Req, org_id), parse_id(Req, application_id)} of
        {{ok, OrgId}, {ok, AppId}} -> {ok, OrgId, AppId};
        _ -> error
    end.

-spec credential_ids(cowboy_req:req()) -> {ok, integer(), integer(), integer()} | error.
credential_ids(Req) ->
    case {both_ids(Req), parse_id(Req, credential_id)} of
        {{ok, OrgId, AppId}, {ok, CredId}} -> {ok, OrgId, AppId, CredId};
        _ -> error
    end.

-spec grant_ids(cowboy_req:req()) -> {ok, integer(), integer(), integer()} | error.
grant_ids(Req) ->
    case {both_ids(Req), parse_id(Req, grant_id)} of
        {{ok, OrgId, AppId}, {ok, GrantId}} -> {ok, OrgId, AppId, GrantId};
        _ -> error
    end.

%% @doc 读 JSON body（空 body = #{}）；非 JSON 对象 → 400。
-spec read_body_map(cowboy_req:req()) -> {ok, map()} | {error, binary()}.
read_body_map(Req0) ->
    {ok, Body, _Req} = cowboy_req:read_body(Req0),
    case byte_size(Body) of
        0 ->
            {ok, #{}};
        _ ->
            try jsone:decode(Body, [{object_format, map}]) of
                Data when is_map(Data) -> {ok, Data};
                _ -> {error, <<"请求体必须是 JSON 对象"/utf8>>}
            catch
                _:_ -> {error, <<"请求体必须是合法 JSON"/utf8>>}
            end
    end.

-spec bin_field(map(), binary()) -> binary().
bin_field(Body, Key) ->
    case maps:get(Key, Body, <<>>) of
        V when is_binary(V) -> V;
        _ -> <<>>
    end.

-spec bin_list(map(), binary()) -> [binary()].
bin_list(Body, Key) ->
    case maps:get(Key, Body, undefined) of
        L when is_list(L) -> [S || S <- L, is_binary(S)];
        _ -> []
    end.

-spec int_field(map(), binary()) -> integer() | undefined.
int_field(Body, Key) ->
    case maps:get(Key, Body, undefined) of
        V when is_integer(V), V > 0 -> V;
        V when is_binary(V) ->
            try binary_to_integer(string:trim(V)) of
                I when I > 0 -> I;
                _ -> undefined
            catch
                _:_ -> undefined
            end;
        _ ->
            undefined
    end.

-spec compact_opts([{atom(), binary()}]) -> map().
compact_opts(Pairs) ->
    maps:from_list([{K, V} || {K, V} <- Pairs, is_binary(V), V =/= <<>>]).

-spec grant_input(map()) -> map().
grant_input(Body) ->
    Input0 = #{
        scopes => bin_list(Body, <<"scopes">>),
        workspace_ids => [I || I <- [to_int(W) || W <- bin_list(Body, <<"workspace_ids">>)], I > 0]
    },
    Input1 =
        case bin_field(Body, <<"workspace_scope_kind">>) of
            <<"explicit">> -> Input0#{workspace_scope_kind => explicit};
            <<"none">> -> Input0#{workspace_scope_kind => none};
            _ -> Input0#{workspace_scope_kind => none}
        end,
    Input2 =
        case bin_field(Body, <<"valid_from">>) of
            <<>> -> Input1;
            VF -> Input1#{valid_from => VF}
        end,
    case bin_field(Body, <<"valid_to">>) of
        <<>> -> Input2;
        VT -> Input2#{valid_to => VT}
    end.

-spec grant_patch(map()) -> map().
grant_patch(Body) ->
    P0 = #{},
    P1 =
        case maps:get(<<"revoke">>, Body, undefined) of
            true -> P0#{revoke => true};
            _ -> P0
        end,
    P2 =
        case maps:get(<<"scopes">>, Body, undefined) of
            L when is_list(L) -> P1#{scopes => [S || S <- L, is_binary(S)]};
            _ -> P1
        end,
    P3 =
        case bin_field(Body, <<"workspace_scope_kind">>) of
            <<"none">> -> P2#{workspace_scope_kind => none};
            <<"explicit">> -> P2#{workspace_scope_kind => explicit};
            _ -> P2
        end,
    case maps:get(<<"workspace_ids">>, Body, undefined) of
        L2 when is_list(L2) ->
            P3#{workspace_ids => [I || I <- [to_int(W) || W <- L2, is_binary(W)], I > 0]};
        _ ->
            P3
    end.

-spec to_int(binary()) -> integer().
to_int(V) ->
    case catch binary_to_integer(string:trim(V)) of
        I when is_integer(I) -> I;
        _ -> 0
    end.

%% ===================================================================
%% 内部：执行者 / 错误
%% ===================================================================

%% @doc 审计执行者：平台管理员（**不是**租户 user）。
-spec actor(map()) -> map().
actor(State) ->
    AdmUserId = maps:get(adm_user_id, State, 0),
    #{adm_user_id => AdmUserId, account => adm_account(AdmUserId)}.

-spec adm_account(integer()) -> binary().
adm_account(AdmUserId) when is_integer(AdmUserId), AdmUserId > 0 ->
    Key = {adm_user_account, AdmUserId},
    case catch adm_user_logic:find(AdmUserId, <<"id,account">>, Key) of
        #{<<"account">> := Account} when is_binary(Account) -> Account;
        _ -> <<>>
    end;
adm_account(_) ->
    <<>>.

-spec bad_id(cowboy_req:req()) -> cowboy_req:req().
bad_id(Req0) ->
    elib_response:error(Req0, <<"ID 格式错误"/utf8>>, ?ERR_BAD_REQUEST).

-spec method_not_allowed(cowboy_req:req()) -> cowboy_req:req().
method_not_allowed(Req0) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% @doc 业务错误 → 稳定信封码。失败原因对治理 UI 可见（不塌成 500）。
-spec reply_error(cowboy_req:req(), term()) -> cowboy_req:req().
reply_error(Req0, Reason) ->
    {Code, Msg} = error_map(Reason),
    case Code of
        500 ->
            ok = ?ERROR_LOG([adm_enterprise_governance_error, Reason]);
        _ ->
            ok
    end,
    elib_response:error(Req0, Msg, Code).

-spec error_map(term()) -> {integer(), binary()}.
error_map(not_found) -> {?ERR_NOT_FOUND, <<"资源不存在或不属该组织"/utf8>>};
error_map(application_not_found) -> {?ERR_NOT_FOUND, <<"应用不存在或不属该组织"/utf8>>};
error_map(version_conflict) -> {?ERR_CONFLICT, <<"版本冲突：资源已被他人更新，请刷新后重试"/utf8>>};
error_map(already_revoked) -> {?ERR_CONFLICT, <<"该授权已被撤销"/utf8>>};
error_map(not_active) -> {?ERR_CONFLICT, <<"该凭证当前不是有效状态"/utf8>>};
error_map(key_conflict) -> {?ERR_CONFLICT, <<"同内容的授权已存在（幂等键冲突）"/utf8>>};
error_map(invalid_status) -> {?ERR_BAD_REQUEST, <<"非法生命周期状态"/utf8>>};
error_map(invalid_scope) -> {?ERR_BAD_REQUEST, <<"未登记的 scope"/utf8>>};
error_map(empty_scopes) -> {?ERR_BAD_REQUEST, <<"scope 不能为空"/utf8>>};
error_map(invalid_workspaces) -> {?ERR_BAD_REQUEST, <<"非法的 workspace 授权组合"/utf8>>};
error_map(workspace_not_found) -> {?ERR_BAD_REQUEST, <<"workspace 不存在或不属该组织"/utf8>>};
error_map(missing_idempotency_key) -> {?ERR_BAD_REQUEST, <<"缺少幂等键"/utf8>>};
error_map(invalid_spec) -> {?ERR_BAD_REQUEST, <<"非法的请求参数"/utf8>>};
error_map(invalid_request) -> {?ERR_BAD_REQUEST, <<"请求缺少可执行的变更"/utf8>>};
error_map(Reason) -> {500, reason_msg(Reason)}.

-spec reason_msg(term()) -> binary().
reason_msg(_) ->
    <<"服务内部错误"/utf8>>.
