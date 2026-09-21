-module(enterprise_message_handler).

%%%
% EPGZ-04 INT-09/10 OA 代发消息端点壳（不接线 Router——A0 W4 做）。
%
% 路由（A0 W4 经 Router lease 串行登记）：
%   POST /api/internal/v1/messages/direct ->
%       {Path, enterprise_message_handler, #{action => direct}}
%   POST /api/internal/v1/groups/{group_id}/messages ->
%       {Path, enterprise_message_handler, #{action => group,
%         group_id_binding => path}}（group_id 从 path 提取后以 atom 键
%         group_id 进入 Input——本壳从 handler_opts.group_id 读）
% —— 必须经 enterprise_internal_middleware（A2 认证链；INT-09/10 动态
%    scope：中间件放行并标 dynamic_scope=messages_send，本壳不重复判
%    scope——sender_mode 的 scope 选择在 logic 内完成（messages:send /
%    messages:send_as_human 精确判定，无隐含包含））。
%
% ctx 增强：本壳在调 logic 前把 principal_user_id 预取进 ctx（application
% 发送主体；查询失败不阻断 human 模式——缺省键即 invalid_request）。
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
            direct -> direct(Method, Req0, State);
            group -> group(Method, Req0, State);
            _ -> Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal
%% ===================================================================

-spec direct(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
direct(<<"POST">>, Req0, State) ->
    Ctx0 = maps:get(enterprise_internal, State, #{}),
    Params = elib_param:post(Req0),
    IdemKey = idempotency_key(Req0),
    Digest = enterprise_internal_idempotency:request_digest(
        <<"POST">>, <<"/api/internal/v1/messages/direct">>, Params
    ),
    with_idempotency(
        Req0,
        Ctx0,
        <<"enterprise_message">>,
        IdemKey,
        Digest,
        <<"msg_c2c">>,
        fun(Ctx, Conn) ->
            enterprise_message_logic:direct_tx(Conn, Ctx, params_to_input(Params))
        end
    );
direct(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

-spec group(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
group(<<"POST">>, Req0, State) ->
    Ctx0 = maps:get(enterprise_internal, State, #{}),
    Params0 = elib_param:post(Req0),
    IdemKey = idempotency_key(Req0),
    Digest = enterprise_internal_idempotency:request_digest(
        <<"POST">>, group_path(maps:get(group_id, State, 0)), Params0
    ),
    with_idempotency(
        Req0,
        Ctx0,
        <<"enterprise_message">>,
        IdemKey,
        Digest,
        <<"msg_c2g">>,
        fun(Ctx, Conn) ->
            enterprise_message_logic:group_tx(
                Conn, Ctx, maps:get(group_id, State, 0), params_to_input(Params0)
            )
        end
    );
group(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

group_path(GroupId) when is_integer(GroupId) ->
    <<"/api/internal/v1/groups/", (integer_to_binary(GroupId))/binary, "/messages">>;
group_path(_) ->
    <<"/api/internal/v1/groups/{group_id}/messages">>.

%% @doc A2 幂等模式（单事务）+ ctx principal 预取。
-spec with_idempotency(
    cowboy_req:req(), map(), binary(), binary() | undefined, binary(), binary(), fun()
) ->
    cowboy_req:req().
with_idempotency(Req0, Ctx0, ResourceType, IdemKey, Digest, MsgTable, LogicFun) ->
    TxResult =
        elib_pg:with_tx(fun(Conn) ->
            case
                enterprise_internal_idempotency:begin_tx(Conn, Ctx0, ResourceType, IdemKey, Digest)
            of
                {ok, inserted} ->
                    Ctx = with_principal(Conn, Ctx0),
                    case LogicFun(Ctx, Conn) of
                        {ok, #{<<"msg_id">> := MsgId} = Result} ->
                            RowId = fetch_row_id(Conn, MsgTable, MsgId),
                            _ = enterprise_internal_idempotency:complete_tx(
                                Conn, Ctx0, ResourceType, IdemKey, RowId, 200
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
                Ctx0,
                <<"message.enterprise.failed">>,
                #{resource_type => MsgTable},
                Code
            ),
            enterprise_internal_error:reply(Req0, Code);
        {rollback, Reason} ->
            ?ERROR_LOG("enterprise_message_handler tx rollback: ~p~n", [Reason]),
            enterprise_internal_error:reply(Req0, <<"internal_error">>);
        {ok, replay, #{resource_id := _RId, response_code := Code}} ->
            reply_json(Req0, ok_code(Code), #{<<"replayed">> => true});
        {ok, pending} ->
            enterprise_internal_error:reply(Req0, <<"idempotency_conflict">>);
        {error, digest_conflict} ->
            enterprise_internal_error:reply(Req0, enterprise_internal_idempotency:conflict_code());
        {error, Reason} ->
            ?ERROR_LOG("enterprise_message_handler idempotency error: ~p~n", [Reason]),
            enterprise_internal_error:reply(Req0, <<"internal_error">>)
    end.

%% principal 预取（application 发送主体；human 模式不依赖，缺省即拒）。
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

fetch_row_id(Conn, <<"msg_c2c">>, MsgId) ->
    case enterprise_message_repo:find_direct_tx(Conn, MsgId) of
        {ok, #{<<"id">> := Id}} -> Id;
        _ -> null
    end;
fetch_row_id(Conn, <<"msg_c2g">>, MsgId) ->
    case enterprise_message_repo:find_group_tx(Conn, MsgId) of
        {ok, #{<<"id">> := Id}} -> Id;
        _ -> null
    end;
fetch_row_id(_Conn, _Table, _MsgId) ->
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
