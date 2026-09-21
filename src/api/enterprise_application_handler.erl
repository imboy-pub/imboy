-module(enterprise_application_handler).

%%%
% EPGZ-08 W4 INT-01 企业 Application 自检端点壳。
%
% 路由（A0 W4 经 Router lease 登记）：
%   GET /api/internal/v1/application -> {Path, enterprise_application_handler,
%       #{action => self_info}}
% —— 必须经 enterprise_internal_middleware（A2 认证链：credential →
%    active/expiry → application active → organization active →
%    scope application:read → rate internal_read fail-closed）。
%
% 语义：返回本凭证所属 application 的 org/app/status/granted_scopes 与
% 凭证清单（prefix 级，**不含 digest/secret**——A2 list_credentials 只选
% id/credential_prefix/status/expires_at/last_used_at/revoked_at）。只读，
% 无幂等键要求（manifest INT-01 idempotency=not_required）。
%%%

-behavior(cowboy_rest).

-export([init/2]).

-include("log.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 =
        case Action of
            self_info -> self_info(Method, Req0, State);
            _ -> Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal
%% ===================================================================

-spec self_info(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
self_info(<<"GET">>, Req0, State) ->
    Ctx = maps:get(enterprise_internal, State, #{}),
    OrgId = maps:get(organization_id, Ctx, undefined),
    AppId = maps:get(application_id, Ctx, undefined),
    case {is_integer(OrgId), is_integer(AppId)} of
        {true, true} ->
            case enterprise_internal_ops:application_status(OrgId, AppId) of
                {ok, Status} ->
                    reply_json(Req0, 200, self_payload(Status, Ctx));
                {error, Reason} ->
                    ?ERROR_LOG("enterprise_application_handler status error: ~p~n", [Reason]),
                    enterprise_internal_error:reply(Req0, <<"internal_error">>)
            end;
        _ ->
            %% 缺认证产物 = 中间件未注入（fail-closed）
            enterprise_internal_error:reply(Req0, <<"internal_error">>)
    end;
self_info(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% @doc 自检载荷：application 元数据 + 本次 credential 的授权 scope + 凭证清单。
-spec self_payload(map(), map()) -> map().
self_payload(Status, Ctx) ->
    App = maps:get(application, Status, #{}),
    #{
        <<"organization_id">> => maps:get(organization_id, Ctx),
        <<"application_id">> => maps:get(application_id, Ctx),
        <<"credential_id">> => maps:get(credential_id, Ctx, null),
        <<"application_key">> => maps:get(application_key, Ctx, null),
        <<"application">> => atom_map_to_bin(App),
        <<"granted_scopes">> => maps:get(granted_scopes, Ctx, maps:get(granted_scopes, Status, [])),
        <<"credentials">> => [atom_map_to_bin(C) || C <- maps:get(credentials, Status, [])]
    }.

%% @doc ops 返回 atom 键 map（repo 行），internal 面响应统一 binary 键 JSON。
-spec atom_map_to_bin(map()) -> map().
atom_map_to_bin(Map) ->
    maps:fold(
        fun
            (K, V, Acc) when is_atom(K) -> Acc#{atom_to_binary(K, utf8) => V};
            (K, V, Acc) -> Acc#{K => V}
        end,
        #{},
        Map
    ).

-spec reply_json(cowboy_req:req(), non_neg_integer(), map()) -> cowboy_req:req().
reply_json(Req0, Status, Map) ->
    Body = jsone:encode(Map),
    cowboy_req:reply(Status, #{<<"content-type">> => <<"application/json">>}, Body, Req0).
