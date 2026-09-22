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
            %% INT-02(PUT)/INT-15(DELETE) 共用 /identity-mappings 这一条 cowboy
            %% path（cowboy 不允许同 path 重复登记），方法分派在此收敛；
            %% enterprise_internal_routes:match/2 仍按 method+path 冻结判定，
            %% 两条 INT 的 scope/幂等/rate 各自独立生效。
            mappings -> mappings(Method, Req0, State);
            bind -> bind(Method, Req0, State);
            resolve -> resolve(Method, Req0, State);
            revoke -> revoke(Method, Req0, State);
            directory -> directory(Method, Req0, State);
            _ -> Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal
%% ===================================================================

-spec mappings(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
mappings(<<"PUT">>, Req0, State) ->
    bind(<<"PUT">>, Req0, State);
mappings(<<"DELETE">>, Req0, State) ->
    revoke(<<"DELETE">>, Req0, State);
mappings(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

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
                        with_boundary(Conn, Ctx, <<"INT-02">>, undefined, fun() ->
                            enterprise_identity_logic:bind_mapping_tx(
                                Conn, Ctx, ExternalUserId, UserId
                            )
                        end)
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
            with_boundary(Conn, Ctx, <<"INT-03">>, undefined, fun() ->
                enterprise_identity_logic:resolve_mappings_tx(Conn, Ctx, ExternalIds)
            end)
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

%% @doc INT-15（FULL-02 新增，待 A0 接线）：撤销 external identity 映射（软删）。
%% 路由：DELETE /api/internal/v1/identity-mappings -> #{action => revoke}
%% scope identities:write（internal_write，幂等 required）。
-spec revoke(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
revoke(<<"DELETE">>, Req0, State) ->
    Ctx = maps:get(enterprise_internal, State, #{}),
    Params = elib_param:post(Req0),
    IdemKey = idempotency_key(Req0),
    Digest = enterprise_internal_idempotency:request_digest(
        <<"DELETE">>, ?BIND_PATH, Params
    ),
    TxResult =
        elib_pg:with_tx(fun(Conn) ->
            case
                enterprise_internal_idempotency:begin_tx(
                    Conn, Ctx, <<"identity_mapping_revoke">>, IdemKey, Digest
                )
            of
                {ok, inserted} ->
                    ExternalUserId = to_bin(maps:get(<<"external_user_id">>, Params, undefined)),
                    case
                        with_boundary(Conn, Ctx, <<"INT-15">>, undefined, fun() ->
                            enterprise_identity_logic:revoke_mapping_tx(
                                Conn, Ctx, ExternalUserId
                            )
                        end)
                    of
                        {ok, Result} ->
                            _ = enterprise_internal_idempotency:complete_tx(
                                Conn,
                                Ctx,
                                <<"identity_mapping_revoke">>,
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
            ?ERROR_LOG("enterprise_identity_handler revoke rollback: ~p~n", [Reason]),
            enterprise_internal_error:reply(Req0, <<"internal_error">>);
        {ok, replay, #{resource_id := RId, response_code := Code}} ->
            reply_json(Req0, ok_code(Code), #{
                <<"replayed">> => true, <<"user_id">> => RId, <<"status">> => <<"removed">>
            });
        {ok, pending} ->
            enterprise_internal_error:reply(Req0, <<"idempotency_conflict">>);
        {error, digest_conflict} ->
            enterprise_internal_error:reply(Req0, enterprise_internal_idempotency:conflict_code());
        {error, Reason} ->
            ?ERROR_LOG("enterprise_identity_handler revoke error: ~p~n", [Reason]),
            enterprise_internal_error:reply(Req0, <<"internal_error">>)
    end;
revoke(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% @doc INT-16（FULL-02 新增，待 A0 接线）：映射 cursor directory（受限分页）。
%% 路由：POST /api/internal/v1/identity-mappings/directory -> #{action => directory}
%% scope identities:read（internal_read，幂等 not_required——纯查询）。
-spec directory(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
directory(<<"POST">>, Req0, State) ->
    Ctx = maps:get(enterprise_internal, State, #{}),
    Params = elib_param:post(Req0),
    Result =
        elib_pg:with_tx(fun(Conn) ->
            with_boundary(Conn, Ctx, <<"INT-16">>, undefined, fun() ->
                enterprise_directory_logic:page_mappings_tx(Conn, Ctx, page_opts(Params))
            end)
        end),
    reply_page_result(Req0, Result);
directory(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% ===================================================================
%% Helpers
%% ===================================================================

%% @doc 分页入参（只取契约内已知键；未知键丢弃，不进 atom 表）。
-spec page_opts(map()) -> map().
page_opts(Params) when is_map(Params) ->
    Opts0 = #{
        cursor => maps:get(<<"cursor">>, Params, undefined),
        page_size => maps:get(<<"page_size">>, Params, undefined)
    },
    case maps:get(<<"workspace_id">>, Params, undefined) of
        undefined -> Opts0;
        WsId -> Opts0#{workspace_id => WsId}
    end;
page_opts(_Params) ->
    #{}.

%% @doc 分页应答（只读路径；无幂等键）。
-spec reply_page_result(cowboy_req:req(), term()) -> cowboy_req:req().
reply_page_result(Req0, Result) ->
    case Result of
        {ok, Page} ->
            reply_json(Req0, 200, Page);
        {error, {Code, _Detail}} ->
            enterprise_internal_error:reply(Req0, Code);
        {rollback, Reason} ->
            ?ERROR_LOG("enterprise_identity_handler directory rollback: ~p~n", [Reason]),
            enterprise_internal_error:reply(Req0, <<"internal_error">>);
        {error, Reason} ->
            ?ERROR_LOG("enterprise_identity_handler directory error: ~p~n", [Reason]),
            enterprise_internal_error:reply(Req0, <<"internal_error">>)
    end.

%% @doc FULL-02 授权边界接线（唯一接线点
%% enterprise_internal_boundary:enforce/4）：org 级资源要求覆盖 Org 全域的
%% 同一生效 Grant 同时覆盖该 scope；未受管应用 no-op。
-spec with_boundary(any(), map(), binary(), undefined | integer(), fun()) ->
    {ok, map()} | {error, {binary(), term()}}.
with_boundary(Conn, Ctx, RouteId, WorkspaceId, Fun) ->
    case enterprise_internal_boundary:enforce(Conn, Ctx, RouteId, WorkspaceId) of
        ok ->
            Fun();
        {error, Code} ->
            {error, {enterprise_internal_boundary:error_code(Code), grant_boundary}}
    end.

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
