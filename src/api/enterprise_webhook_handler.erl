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
    case
        enterprise_internal_idempotency:request_digest(
            <<"PUT">>, <<"/api/internal/v1/webhook">>, Params
        )
    of
        {ok, Digest} -> configure_tx(Req0, Ctx0, IdemKey, Digest, Params);
        {error, non_canonical} -> enterprise_internal_error:reply(Req0, <<"invalid_request">>)
    end;
configure(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

-spec configure_tx(cowboy_req:req(), map(), binary(), binary(), map()) -> cowboy_req:req().
configure_tx(Req0, Ctx0, IdemKey, Digest, Params) ->
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
                            Body = jsone:encode(Result),
                            _ = enterprise_internal_idempotency:complete_tx(
                                Conn,
                                Ctx0,
                                <<"enterprise_webhook_config">>,
                                IdemKey,
                                null,
                                200,
                                Body
                            ),
                            {tx_ok, Body};
                        {error, {Code, _Detail}} ->
                            throw({rollback, {business_error, Code}})
                    end;
                Other ->
                    Other
            end
        end),
    case TxResult of
        {tx_ok, Body} ->
            reply_json_body(Req0, 200, Body);
        {rollback, {business_error, Code}} ->
            enterprise_internal_error:reply(Req0, Code);
        {rollback, Reason} ->
            ?ERROR_LOG("enterprise_webhook_handler configure rollback: ~p~n", [Reason]),
            enterprise_internal_error:reply(Req0, <<"internal_error">>);
        {ok, replay, #{response_code := Code, response_body := Body}} ->
            replay_json_body(Req0, Code, Body);
        {ok, pending} ->
            enterprise_internal_error:reply(Req0, <<"idempotency_conflict">>);
        {error, digest_conflict} ->
            enterprise_internal_error:reply(Req0, enterprise_internal_idempotency:conflict_code());
        {error, Reason} ->
            ?ERROR_LOG("enterprise_webhook_handler configure error: ~p~n", [Reason]),
            enterprise_internal_error:reply(Req0, <<"internal_error">>)
    end.

-spec replay(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
replay(<<"POST">>, Req0, State) ->
    Ctx0 = maps:get(enterprise_internal, State, #{}),
    DeliveryId = maps:get(delivery_id, State, undefined),
    IdemKey = idempotency_key(Req0),
    case
        enterprise_internal_idempotency:request_digest(
            <<"POST">>,
            <<"/api/internal/v1/webhook/deliveries/", (to_bin(DeliveryId))/binary, "/replay">>,
            #{}
        )
    of
        {ok, Digest} -> replay_tx(Req0, Ctx0, DeliveryId, IdemKey, Digest);
        {error, non_canonical} -> enterprise_internal_error:reply(Req0, <<"invalid_request">>)
    end;
replay(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

-spec replay_tx(cowboy_req:req(), map(), term(), binary(), binary()) -> cowboy_req:req().
replay_tx(Req0, Ctx0, DeliveryId, IdemKey, Digest) ->
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
                            Body = jsone:encode(Result),
                            _ = enterprise_internal_idempotency:complete_tx(
                                Conn,
                                Ctx0,
                                <<"enterprise_webhook_replay">>,
                                IdemKey,
                                null,
                                200,
                                Body
                            ),
                            {tx_ok, Body};
                        {error, {Code, _Detail}} ->
                            throw({rollback, {business_error, Code}})
                    end;
                Other ->
                    Other
            end
        end),
    case TxResult of
        {tx_ok, Body} ->
            reply_json_body(Req0, 200, Body);
        {rollback, {business_error, Code}} ->
            enterprise_internal_error:reply(Req0, Code);
        {rollback, Reason} ->
            ?ERROR_LOG("enterprise_webhook_handler replay rollback: ~p~n", [Reason]),
            enterprise_internal_error:reply(Req0, <<"internal_error">>);
        {ok, replay, #{response_code := Code, response_body := Body}} ->
            replay_json_body(Req0, Code, Body);
        {ok, pending} ->
            enterprise_internal_error:reply(Req0, <<"idempotency_conflict">>);
        {error, digest_conflict} ->
            enterprise_internal_error:reply(Req0, enterprise_internal_idempotency:conflict_code());
        {error, Reason} ->
            ?ERROR_LOG("enterprise_webhook_handler replay error: ~p~n", [Reason]),
            enterprise_internal_error:reply(Req0, <<"internal_error">>)
    end.

%% @doc GET /api/internal/v1/webhook/deliveries（INT-23；CP-CON-02 切 CURSOR-V2
%% keyset——DEC-INT23-COMPAT）：本 Application 的投递元数据分页 + 健康度摘要
%% （成功率/重试/死信）。只读、无 Idempotency-Key 要求；响应不含 payload/secret。
%% query：cursor（上一页 next_cursor）/ page_size（缺省 20，上限 50，越界 400）/
%% status（可选过滤）；**旧 offset 参数 page/size 任一出现即 400
%% cursor_required_v1（versioned 迁移错误，进事务前拒绝）**。
-spec deliveries(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
deliveries(<<"GET">>, Req0, State) ->
    Ctx0 = maps:get(enterprise_internal, State, #{}),
    Qs = maps:from_list(cowboy_req:parse_qs(Req0)),
    case legacy_offset_qs(Qs) of
        true ->
            reply_versioned_400(Req0);
        false ->
            Params = #{
                cursor => maps:get(<<"cursor">>, Qs, undefined),
                page_size => page_size_of(Qs, 20),
                status => maps:get(<<"status">>, Qs, undefined)
            },
            TxResult =
                elib_pg:with_tx(fun(Conn) ->
                    case delivery_ctx(Conn, Ctx0) of
                        {ok, Ctx} ->
                            case enterprise_webhook_logic:deliveries_tx(Conn, Ctx, Params, 20) of
                                {ok, Result} ->
                                    {tx_ok, Result};
                                {error, {Code, _Detail}} ->
                                    throw({rollback, {business_error, Code}})
                            end;
                        {error, Code} ->
                            throw({rollback, {business_error, Code}})
                    end
                end),
            case TxResult of
                {tx_ok, Result} ->
                    reply_json(Req0, 200, Result);
                {rollback, {business_error, Code}} ->
                    reply_error(Req0, Code);
                {rollback, Reason} ->
                    ?ERROR_LOG("enterprise_webhook_handler deliveries rollback: ~p~n", [Reason]),
                    enterprise_internal_error:reply(Req0, <<"internal_error">>);
                {error, Reason} ->
                    ?ERROR_LOG("enterprise_webhook_handler deliveries error: ~p~n", [Reason]),
                    enterprise_internal_error:reply(Req0, <<"internal_error">>)
            end
    end;
deliveries(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% 旧 offset 参数门（DEC-INT23-COMPAT）：page/size 任一出现即 versioned 400。
-spec legacy_offset_qs(map()) -> boolean().
legacy_offset_qs(Qs) ->
    maps:is_key(<<"page">>, Qs) orelse maps:is_key(<<"size">>, Qs).

%% page_size 解析：二进制整数原样转 integer；非数值保留原值交 logic 拒绝
%% （400 invalid_request，不静默回落）。
-spec page_size_of(map(), pos_integer()) -> pos_integer() | binary().
page_size_of(Qs, Default) ->
    case maps:get(<<"page_size">>, Qs, undefined) of
        undefined ->
            Default;
        Bin when is_binary(Bin) ->
            try
                binary_to_integer(Bin)
            catch
                _:_ -> Bin
            end;
        Other ->
            Other
    end.

%% deliveries 错误映射：versioned 迁移码走专属 400 信封，其余 stable 码走
%% enterprise_internal_error（13 码冻结合同不动）。
-spec reply_error(cowboy_req:req(), binary()) -> cowboy_req:req().
reply_error(Req0, <<"cursor_required_v1">>) ->
    reply_versioned_400(Req0);
reply_error(Req0, Code) ->
    enterprise_internal_error:reply(Req0, Code).

%% versioned 400（DEC-INT23-COMPAT）：与 internal 错误信封同形态
%% （{"error":{"code":...,"message":...}}，message 固定通用文案不回显请求
%% 细节），code 为本读面专属迁移码 cursor_required_v1——不进
%% enterprise_internal_error 的 13 个 stable 码（那是 EPGZ-02 冻结面，
%% versioned 迁移错误不新造 stable 码）。
-spec reply_versioned_400(cowboy_req:req()) -> cowboy_req:req().
reply_versioned_400(Req0) ->
    ?WARN_LOG([
        enterprise_webhook_pagination_migrated,
        #{
            code => <<"cursor_required_v1">>,
            method => cowboy_req:method(Req0),
            path => cowboy_req:path(Req0)
        }
    ]),
    Body = jsone:encode(#{
        <<"error">> => #{
            <<"code">> => <<"cursor_required_v1">>,
            <<"message">> =>
                <<"pagination migrated to signed cursor; use page_size and next_cursor">>
        }
    }),
    cowboy_req:reply(
        400, #{<<"content-type">> => <<"application/json">>}, Body, Req0
    ).

%% 读面 ctx：与 configure 同源的 principal 链路解析（无 principal → 明确错误，
%% 不给存在性 oracle）。
delivery_ctx(Conn, Ctx0) ->
    AppId = maps:get(application_id, Ctx0, undefined),
    case is_integer(AppId) of
        true ->
            %% find_tx/3 以 (organization_id, id) 定位（Org 边界在 SQL 内强制，
            %% F2 修复：原 arity-2 调用运行时 undef → INT-23 真实 500）。
            case
                enterprise_application_repo:find_tx(
                    Conn, maps:get(organization_id, Ctx0), AppId
                )
            of
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
%% find_tx/3 以 (organization_id, id) 定位（Org 边界在 SQL 内强制，F2 修复：
%% 原 arity-2 调用运行时 undef → INT-12 真实 500）。
with_principal(Conn, Ctx) ->
    AppId = maps:get(application_id, Ctx),
    OrgId = maps:get(organization_id, Ctx),
    case enterprise_application_repo:find_tx(Conn, OrgId, AppId) of
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

-spec reply_json_body(cowboy_req:req(), non_neg_integer(), binary()) -> cowboy_req:req().
reply_json_body(Req0, Status, Body) ->
    cowboy_req:reply(
        Status, #{<<"content-type">> => <<"application/json">>}, Body, Req0
    ).

-spec replay_json_body(cowboy_req:req(), non_neg_integer(), binary() | null) ->
    cowboy_req:req().
replay_json_body(Req0, Code, Body) ->
    BodyBin =
        case Body of
            B when is_binary(B), B =/= <<>> -> B;
            _ -> <<"{}">>
        end,
    Headers = maps:from_list([enterprise_internal_idempotency:replay_header()]),
    cowboy_req:reply(
        ok_code(Code),
        Headers#{<<"content-type">> => <<"application/json">>},
        BodyBin,
        Req0
    ).

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
