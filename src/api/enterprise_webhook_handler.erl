-module(enterprise_webhook_handler).

%%%
% EPGZ-04 INT-12/13 企业 Webhook 端点壳（不接线 Router——A0 W4 做）。
% FULL-03 追加只读 action `deliveries`（投递列表 + 健康度摘要；**未接线**，
% 路由/manifest 登记交 A0——见 FULL-03 checkpoint「需要 A0 接线」）。
%
% 路由（A0 经 Router lease 串行登记）：
%   PUT  /api/internal/v1/webhook ->
%       {Path, enterprise_webhook_handler, #{action => configure}}
%   POST /api/internal/v1/webhook/deliveries/{delivery_id}/replay ->
%       {Path, enterprise_webhook_handler, #{action => replay,
%         delivery_id => binary()}}（path 段注入 handler_opts）
%   GET  /api/internal/v1/webhook/deliveries ->   % FULL-03 提议 INT-23
%       {Path, enterprise_webhook_handler, #{action => deliveries}}
% —— 必须经 enterprise_internal_middleware（A2 认证链：scope webhooks:manage
%    → rate internal_write）。ctx 注入 handler_opts.enterprise_internal。
%
% 幂等：INT-12 configure upsert 天然幂等（同 key 同 body 重放原结果）；
% INT-13 replay 的幂等资源 = 新 delivery 行（idempotency_key=replay-<新id>
% 天然唯一，重复 replay 请求产生多个新 delivery——由 Idempotency-Key
% 在 begin_tx 层兜底重放原结果；并发相同原行的重放由 DB 唯一索引
% uq_ewh_delivery_replay_inflight 仲裁成 idempotency_conflict）。
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
            configure -> configure(Method, Req0, State);
            replay -> replay(Method, Req0, State);
            deliveries -> deliveries(Method, Req0, State);
            _ -> Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal
%% ===================================================================

-spec configure(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
configure(<<"PUT">>, Req0, State) ->
    Ctx0 = maps:get(enterprise_internal, State, #{}),
    Params = elib_param:post(Req0),
    IdemKey = idempotency_key(Req0),
    Digest = enterprise_internal_idempotency:request_digest(
        <<"PUT">>, <<"/api/internal/v1/webhook">>, Params
    ),
    TxResult =
        elib_pg:with_tx(fun(Conn) ->
            case
                enterprise_internal_idempotency:begin_tx(
                    Conn, Ctx0, <<"enterprise_webhook_config">>, IdemKey, Digest
                )
            of
                {ok, inserted} ->
                    Ctx = with_principal(Conn, Ctx0),
                    case
                        enterprise_webhook_logic:configure_tx(Conn, Ctx, params_to_input(Params))
                    of
                        {ok, Result} ->
                            _ = enterprise_internal_idempotency:complete_tx(
                                Conn, Ctx0, <<"enterprise_webhook_config">>, IdemKey, null, 200
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
            enterprise_internal_error:reply(Req0, Code);
        {rollback, Reason} ->
            ?ERROR_LOG("enterprise_webhook_handler configure rollback: ~p~n", [Reason]),
            enterprise_internal_error:reply(Req0, <<"internal_error">>);
        {ok, replay, #{response_code := Code}} ->
            reply_json(Req0, ok_code(Code), #{<<"replayed">> => true});
        {ok, pending} ->
            enterprise_internal_error:reply(Req0, <<"idempotency_conflict">>);
        {error, digest_conflict} ->
            enterprise_internal_error:reply(Req0, enterprise_internal_idempotency:conflict_code());
        {error, Reason} ->
            ?ERROR_LOG("enterprise_webhook_handler configure error: ~p~n", [Reason]),
            enterprise_internal_error:reply(Req0, <<"internal_error">>)
    end;
configure(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

-spec replay(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
replay(<<"POST">>, Req0, State) ->
    Ctx0 = maps:get(enterprise_internal, State, #{}),
    DeliveryId = maps:get(delivery_id, State, undefined),
    IdemKey = idempotency_key(Req0),
    Digest = enterprise_internal_idempotency:request_digest(
        <<"POST">>,
        <<"/api/internal/v1/webhook/deliveries/", (to_bin(DeliveryId))/binary, "/replay">>,
        #{}
    ),
    TxResult =
        elib_pg:with_tx(fun(Conn) ->
            case
                enterprise_internal_idempotency:begin_tx(
                    Conn, Ctx0, <<"enterprise_webhook_replay">>, IdemKey, Digest
                )
            of
                {ok, inserted} ->
                    case enterprise_webhook_logic:replay_tx(Conn, Ctx0, to_bin(DeliveryId)) of
                        {ok, Result} ->
                            _ = enterprise_internal_idempotency:complete_tx(
                                Conn, Ctx0, <<"enterprise_webhook_replay">>, IdemKey, null, 200
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
            enterprise_internal_error:reply(Req0, Code);
        {rollback, Reason} ->
            ?ERROR_LOG("enterprise_webhook_handler replay rollback: ~p~n", [Reason]),
            enterprise_internal_error:reply(Req0, <<"internal_error">>);
        {ok, replay, #{response_code := Code}} ->
            reply_json(Req0, ok_code(Code), #{<<"replayed">> => true});
        {ok, pending} ->
            enterprise_internal_error:reply(Req0, <<"idempotency_conflict">>);
        {error, digest_conflict} ->
            enterprise_internal_error:reply(Req0, enterprise_internal_idempotency:conflict_code());
        {error, Reason} ->
            ?ERROR_LOG("enterprise_webhook_handler replay error: ~p~n", [Reason]),
            enterprise_internal_error:reply(Req0, <<"internal_error">>)
    end;
replay(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% @doc GET /api/internal/v1/webhook/deliveries（FULL-03 提议 INT-23，待 A0 接线）：
%% 本 Application 的投递元数据列表 + 健康度摘要（成功率/重试/死信）。
%% 只读、无 Idempotency-Key 要求、页大小夹紧；响应不含 payload/secret。
-spec deliveries(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
deliveries(<<"GET">>, Req0, State) ->
    Ctx0 = maps:get(enterprise_internal, State, #{}),
    Qs = maps:from_list(cowboy_req:parse_qs(Req0)),
    Params = #{
        page => maps:get(<<"page">>, Qs, <<"1">>),
        size => maps:get(<<"size">>, Qs, <<"20">>),
        status => maps:get(<<"status">>, Qs, undefined)
    },
    TxResult =
        elib_pg:with_tx(fun(Conn) ->
            case delivery_ctx(Conn, Ctx0) of
                {ok, Ctx} ->
                    case enterprise_webhook_logic:deliveries_tx(Conn, Ctx, Params, 20) of
                        {ok, Result} -> {tx_ok, Result};
                        {error, {Code, _Detail}} -> throw({rollback, {business_error, Code}})
                    end;
                {error, Code} ->
                    throw({rollback, {business_error, Code}})
            end
        end),
    case TxResult of
        {tx_ok, Result} ->
            reply_json(Req0, 200, Result);
        {rollback, {business_error, Code}} ->
            enterprise_internal_error:reply(Req0, Code);
        {rollback, Reason} ->
            ?ERROR_LOG("enterprise_webhook_handler deliveries rollback: ~p~n", [Reason]),
            enterprise_internal_error:reply(Req0, <<"internal_error">>);
        {error, Reason} ->
            ?ERROR_LOG("enterprise_webhook_handler deliveries error: ~p~n", [Reason]),
            enterprise_internal_error:reply(Req0, <<"internal_error">>)
    end;
deliveries(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% 读面 ctx：与 configure 同源的 principal 链路解析（无 principal → 明确错误，
%% 不给存在性 oracle）。
delivery_ctx(Conn, Ctx0) ->
    AppId = maps:get(application_id, Ctx0, undefined),
    case is_integer(AppId) of
        true ->
            case enterprise_application_repo:find_tx(Conn, AppId) of
                {ok, App} ->
                    case maps:get(<<"principal_user_id">>, App, null) of
                        P when is_integer(P), P > 0 -> {ok, Ctx0#{principal_user_id => P}};
                        _ -> {error, <<"invalid_request">>}
                    end;
                _ ->
                    {error, <<"resource_not_found">>}
            end;
        false ->
            {error, <<"invalid_request">>}
    end.

%% principal 预取（webhook 配置归属判定在 logic，此处只补 ctx）。
with_principal(Conn, Ctx) ->
    AppId = maps:get(application_id, Ctx),
    case enterprise_application_repo:find_tx(Conn, AppId) of
        {ok, App} ->
            case maps:get(<<"principal_user_id">>, App, null) of
                P when is_integer(P), P > 0 -> Ctx#{principal_user_id => P};
                _ -> Ctx
            end;
        _ ->
            Ctx
    end.

ok_code(Code) when is_integer(Code), Code >= 200, Code < 300 -> Code;
ok_code(_) -> 200.

to_bin(B) when is_binary(B) -> B;
to_bin(I) when is_integer(I) -> integer_to_binary(I);
to_bin(_) -> <<>>.

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
