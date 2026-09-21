-module(enterprise_asset_handler).

%%%
% EPGZ-04 INT-07/08 企业附件端点壳（不接线 Router——A0 W4 做）。
%
% 路由（A0 W4 经 Router lease 串行登记）：
%   POST /api/internal/v1/files/presign -> {Path, enterprise_asset_handler,
%       #{action => presign}}
%   POST /api/internal/v1/files/confirm -> {Path, enterprise_asset_handler,
%       #{action => confirm}}
% —— 必须经 enterprise_internal_middleware（A2 认证链：credential →
%    active/expiry → application active → organization active →
%    scope files:write → rate internal_write fail-closed）。ctx 注入
%    handler_opts.enterprise_internal（atom 键 map）。
%
% 幂等（A2 模式，INV-7）：Idempotency-Key 已由 decide 强制存在；单事务
% begin_tx -> logic -> complete_tx；业务 {error} 经 throw({rollback,...})
% 整体回滚（幂等行一并消失，同 key 重试可过），回滚后补发
% message.enterprise.failed 旁路事件（独立小事务）。
% 资源：presign -> enterprise_asset_presign；confirm -> attachment。
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
            presign -> presign(Method, Req0, State);
            confirm -> confirm(Method, Req0, State);
            _ -> Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal
%% ===================================================================

-spec presign(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
presign(<<"POST">>, Req0, State) ->
    Ctx = maps:get(enterprise_internal, State, #{}),
    Params = elib_param:post(Req0),
    IdemKey = idempotency_key(Req0),
    Digest = enterprise_internal_idempotency:request_digest(
        <<"POST">>, <<"/api/internal/v1/files/presign">>, Params
    ),
    with_idempotency(Req0, Ctx, <<"enterprise_asset_presign">>, IdemKey, Digest, fun(Conn) ->
        enterprise_asset_logic:presign_tx(Conn, Ctx, params_to_input(Params))
    end);
presign(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

-spec confirm(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
confirm(<<"POST">>, Req0, State) ->
    Ctx = maps:get(enterprise_internal, State, #{}),
    Params = elib_param:post(Req0),
    IdemKey = idempotency_key(Req0),
    Digest = enterprise_internal_idempotency:request_digest(
        <<"POST">>, <<"/api/internal/v1/files/confirm">>, Params
    ),
    with_idempotency(Req0, Ctx, <<"attachment">>, IdemKey, Digest, fun(Conn) ->
        enterprise_asset_logic:confirm_tx(Conn, Ctx, params_to_input(Params))
    end);
confirm(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% @doc A2 幂等模式（单事务）。返回值契约见 enterprise_internal_idempotency。
-spec with_idempotency(
    cowboy_req:req(), map(), binary(), binary() | undefined, binary(), fun()
) ->
    cowboy_req:req().
with_idempotency(Req0, Ctx, ResourceType, IdemKey, Digest, LogicFun) ->
    TxResult =
        elib_pg:with_tx(fun(Conn) ->
            case
                enterprise_internal_idempotency:begin_tx(Conn, Ctx, ResourceType, IdemKey, Digest)
            of
                {ok, inserted} ->
                    case LogicFun(Conn) of
                        {ok, Result} ->
                            _ = enterprise_internal_idempotency:complete_tx(
                                Conn, Ctx, ResourceType, IdemKey, resource_id_of(Result), 200
                            ),
                            {tx_ok, Result};
                        {error, {Code, _Detail}} ->
                            throw({rollback, {business_error, Code}})
                    end;
                Other ->
                    Other
            end
        end),
    case TxResult of
        {tx_ok, Result} ->
            reply_json(Req0, 200, Result);
        {rollback, {business_error, Code}} ->
            enterprise_webhook_logic:emit_event_failed(
                Ctx,
                <<"message.enterprise.failed">>,
                #{resource_type => ResourceType},
                Code
            ),
            enterprise_internal_error:reply(Req0, Code);
        {rollback, Reason} ->
            ?ERROR_LOG("enterprise_asset_handler tx rollback: ~p~n", [Reason]),
            enterprise_internal_error:reply(Req0, <<"internal_error">>);
        {ok, replay, #{resource_id := RId, response_code := Code}} ->
            reply_json(Req0, ok_code(Code), #{<<"replayed">> => true, <<"file_id">> => RId});
        {ok, pending} ->
            enterprise_internal_error:reply(Req0, <<"idempotency_conflict">>);
        {error, digest_conflict} ->
            enterprise_internal_error:reply(Req0, enterprise_internal_idempotency:conflict_code());
        {error, Reason} ->
            ?ERROR_LOG("enterprise_asset_handler idempotency error: ~p~n", [Reason]),
            enterprise_internal_error:reply(Req0, <<"internal_error">>)
    end.

resource_id_of(Result) when is_map(Result) ->
    case maps:find(<<"file_id">>, Result) of
        {ok, Id} when is_integer(Id) -> Id;
        _ -> null
    end;
resource_id_of(_) ->
    null.

ok_code(Code) when is_integer(Code), Code >= 200, Code < 300 -> Code;
ok_code(_) -> 200.

-spec reply_json(cowboy_req:req(), non_neg_integer(), map()) -> cowboy_req:req().
reply_json(Req0, Status, Map) ->
    Body = jsone:encode(Map),
    cowboy_req:reply(Status, #{<<"content-type">> => <<"application/json">>}, Body, Req0).

idempotency_key(Req0) ->
    case cowboy_req:header(<<"idempotency-key">>, Req0) of
        K when is_binary(K), K =/= <<>> -> K;
        _ -> undefined
    end.

params_to_input(Params) when is_map(Params) ->
    maps:fold(
        fun(K, V, Acc) ->
            Acc#{binary_to_atom(K, utf8) => V}
        end,
        #{},
        Params
    );
params_to_input(_) ->
    #{}.
