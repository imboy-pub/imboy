-module(enterprise_identity_handler).

%%%
% EPGZ-08 W4 INT-02/03 企业身份映射端点壳（handler 壳需求见 A3 EPGZ-03
% 冻结合同「给 A0 W4 的 handler 壳需求清单」，本模块严格照此编排）。
%
% 路由（A0 W4 经 Router lease 登记）：
%   PUT  /api/internal/v1/identity-mappings         -> #{action => bind}
%   POST /api/internal/v1/identity-mappings/resolve -> #{action => resolve}
% —— 必须经 enterprise_internal_middleware（A2 认证链：credential →
%    active/expiry → application active → organization active → scope
%    identities:write|read → rate internal_read|internal_write fail-closed）。
%    ctx 注入 handler_opts.enterprise_internal（atom 键 map）。
%
% INT-02 幂等（A2 模式，INV-7）：Idempotency-Key 已由 decide 强制存在；
% 单事务 begin_tx -> logic -> complete_tx；业务 {error} 经
% throw({rollback, ...}) 整体回滚（幂等行一并消失，同 key 重试可过）。
% 资源：identity_mapping。
%
% INT-03 只读（manifest idempotency=not_required）：无幂等键、无事务写入，
% 直接池化读；**无 list-all/全量导出形态**（只按入参 external_user_id 批量
% 解析，缺席=未映射不报错）。
%%%

-behavior(cowboy_rest).

-export([init/2]).

-include("log.hrl").

-define(BIND_PATH, <<"/api/internal/v1/identity-mappings">>).
-define(RESOLVE_PATH, <<"/api/internal/v1/identity-mappings/resolve">>).

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
            bind -> bind(Method, Req0, State);
            resolve -> resolve(Method, Req0, State);
            _ -> Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal
%% ===================================================================

-spec bind(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
bind(<<"PUT">>, Req0, State) ->
    Ctx = maps:get(enterprise_internal, State, #{}),
    Params = elib_param:post(Req0),
    IdemKey = idempotency_key(Req0),
    Digest = enterprise_internal_idempotency:request_digest(<<"PUT">>, ?BIND_PATH, Params),
    TxResult =
        elib_pg:with_tx(fun(Conn) ->
            case
                enterprise_internal_idempotency:begin_tx(
                    Conn, Ctx, <<"identity_mapping">>, IdemKey, Digest
                )
            of
                {ok, inserted} ->
                    ExternalUserId = to_bin(maps:get(<<"external_user_id">>, Params, undefined)),
                    UserId = maps:get(<<"user_id">>, Params, undefined),
                    case
                        enterprise_identity_logic:bind_mapping_tx(
                            Conn, Ctx, ExternalUserId, UserId
                        )
                    of
                        {ok, Result} ->
                            _ = enterprise_internal_idempotency:complete_tx(
                                Conn,
                                Ctx,
                                <<"identity_mapping">>,
                                IdemKey,
                                maps:get(<<"user_id">>, Result, null),
                                200
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
            ?ERROR_LOG("enterprise_identity_handler bind rollback: ~p~n", [Reason]),
            enterprise_internal_error:reply(Req0, <<"internal_error">>);
        {ok, replay, #{resource_id := RId, response_code := Code}} ->
            reply_json(Req0, ok_code(Code), #{
                <<"replayed">> => true, <<"user_id">> => RId, <<"status">> => <<"active">>
            });
        {ok, pending} ->
            enterprise_internal_error:reply(Req0, <<"idempotency_conflict">>);
        {error, digest_conflict} ->
            enterprise_internal_error:reply(Req0, enterprise_internal_idempotency:conflict_code());
        {error, Reason} ->
            ?ERROR_LOG("enterprise_identity_handler bind idempotency error: ~p~n", [Reason]),
            enterprise_internal_error:reply(Req0, <<"internal_error">>)
    end;
bind(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

-spec resolve(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
resolve(<<"POST">>, Req0, State) ->
    Ctx = maps:get(enterprise_internal, State, #{}),
    Params = elib_param:post(Req0),
    ExternalIds = maps:get(<<"external_user_ids">>, Params, undefined),
    Resolved =
        elib_pg:with_tx(fun(Conn) ->
            enterprise_identity_logic:resolve_mappings_tx(Conn, Ctx, ExternalIds)
        end),
    case Resolved of
        {ok, Mappings} ->
            reply_json(Req0, 200, #{<<"mappings">> => Mappings});
        {error, {Code, _Detail}} ->
            enterprise_internal_error:reply(Req0, Code);
        {rollback, Reason} ->
            ?ERROR_LOG("enterprise_identity_handler resolve rollback: ~p~n", [Reason]),
            enterprise_internal_error:reply(Req0, <<"internal_error">>);
        {error, Reason} ->
            ?ERROR_LOG("enterprise_identity_handler resolve error: ~p~n", [Reason]),
            enterprise_internal_error:reply(Req0, <<"internal_error">>)
    end;
resolve(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% ===================================================================
%% Helpers
%% ===================================================================

ok_code(Code) when is_integer(Code), Code >= 200, Code < 300 -> Code;
ok_code(_) -> 200.

to_bin(B) when is_binary(B) -> B;
to_bin(_) -> undefined.

-spec reply_json(cowboy_req:req(), non_neg_integer(), map()) -> cowboy_req:req().
reply_json(Req0, Status, Map) ->
    Body = jsone:encode(Map),
    cowboy_req:reply(Status, #{<<"content-type">> => <<"application/json">>}, Body, Req0).

idempotency_key(Req0) ->
    case cowboy_req:header(<<"idempotency-key">>, Req0) of
        K when is_binary(K), K =/= <<>> -> K;
        _ -> undefined
    end.
