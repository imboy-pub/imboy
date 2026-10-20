-module(enterprise_application_handler).

-moduledoc "企业 Application 自检端点壳（EPGZ-08 W4 INT-01）—— 路由经 Router lease 登记。".

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
    case {is_integer(OrgId) orelse OrgId =:= null, is_integer(AppId)} of
        {true, true} ->
            case
                enterprise_internal_ops:application_status(
                    maps:get(application_organization_id, Ctx, OrgId), AppId
                )
            of
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
        <<"authentication">> => <<"hmac">>,
        <<"application">> => atom_map_to_bin(App),
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
