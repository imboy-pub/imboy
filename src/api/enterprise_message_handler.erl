-module(enterprise_message_handler).

-behaviour(cowboy_rest).

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
    Params = enterprise_internal_request:params(Req0),
    IdemKey = idempotency_key(Req0),
    case
        enterprise_internal_idempotency:request_digest(
            <<"POST">>, <<"/api/internal/v1/messages/direct">>, Params
        )
    of
        {ok, Digest} ->
            with_idempotency(
                Req0,
                Ctx0,
                <<"enterprise_message">>,
                IdemKey,
                Digest,
                <<"msg_c2c">>,
                fun(Ctx, Conn) ->
                    enforce_dynamic(Conn, Ctx, <<"INT-09">>, undefined, Params)
                end,
                fun(Ctx, Conn) ->
                    enterprise_message_logic:direct_tx(Conn, Ctx, params_to_input(Params))
                end
            );
        {error, non_canonical} ->
            enterprise_internal_error:reply(Req0, <<"invalid_request">>)
    end;
direct(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

-spec group(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
group(<<"POST">>, Req0, State) ->
    Ctx0 = maps:get(enterprise_internal, State, #{}),
    Params0 = enterprise_internal_request:params(Req0),
    GroupId = maps:get(group_id, State, 0),
    IdemKey = idempotency_key(Req0),
    case enterprise_internal_idempotency:request_digest(<<"POST">>, group_path(GroupId), Params0) of
        {ok, Digest} ->
            with_idempotency(
                Req0,
                Ctx0,
                <<"enterprise_message">>,
                IdemKey,
                Digest,
                <<"msg_c2g">>,
                fun(Ctx, Conn) ->
                    %% INT-10 的边界资源是该群所属 workspace（workspace scoped +
                    %% group in workspace）：先按 Org 边界定位群（不泄露存在性），
                    %% 再用其 workspace_id 做 Grant workspace 边界判定。
                    case enterprise_group_logic:boundary_workspace_tx(Conn, Ctx, GroupId) of
                        {ok, WsId} ->
                            enforce_dynamic(Conn, Ctx, <<"INT-10">>, WsId, Params0);
                        {error, _} = Err ->
                            Err
                    end
                end,
                fun(Ctx, Conn) ->
                    enterprise_message_logic:group_tx(Conn, Ctx, GroupId, params_to_input(Params0))
                end
            );
        {error, non_canonical} ->
            enterprise_internal_error:reply(Req0, <<"invalid_request">>)
    end;
group(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% The message logic validates sender_mode and resolves its current identity.
enforce_dynamic(Conn, Ctx, RouteId, WorkspaceId, _Params) ->
    case enterprise_internal_boundary:enforce(Conn, Ctx, RouteId, WorkspaceId) of
        ok ->
            ok;
        {error, Code} ->
            {error, {enterprise_internal_boundary:error_code(Code), resource_attribution}}
    end.

group_path(GroupId) when is_integer(GroupId) ->
    <<"/api/internal/v1/groups/", (integer_to_binary(GroupId))/binary, "/messages">>;
group_path(_) ->
    <<"/api/internal/v1/groups/{group_id}/messages">>.

-spec with_idempotency(
    cowboy_req:req(), map(), binary(), binary() | undefined, binary(), binary(), fun(), fun()
) ->
    cowboy_req:req().
with_idempotency(Req0, Ctx0, ResourceType, IdemKey, Digest, MsgTable, BoundaryFun, LogicFun) ->
    TxResult =
        elib_pg:with_tx(fun(Conn) ->
            case
                enterprise_internal_idempotency:begin_tx(Conn, Ctx0, ResourceType, IdemKey, Digest)
            of
                {ok, inserted} ->
                    Ctx = enterprise_message_logic:with_principal_tx(Conn, Ctx0),
                    case with_boundary(BoundaryFun, Ctx, Conn) of
                        ok ->
                            run_logic(Conn, Ctx0, Ctx, ResourceType, IdemKey, MsgTable, LogicFun);
                        {error, {Code, _Detail}} ->
                            throw({rollback, {business_error, Code}})
                    end;
                Other ->
                    Other
            end
        end),
    case TxResult of
        {tx_ok, Result, Body} ->
            %% COMMIT 后离线推送（FULL-07）。幂等 replay 分支不走这里——同一条
            %% 消息不会因客户端重放而二次推送；失败只记日志，不改消息结果。
            ok = push_after_commit(MsgTable, Result),
            reply_json_body(Req0, 200, Body);
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
        {ok, replay, #{response_code := Code, response_body := Body}} ->
            replay_json_body(Req0, Code, Body);
        {ok, pending} ->
            enterprise_internal_error:reply(Req0, <<"idempotency_conflict">>);
        {error, digest_conflict} ->
            enterprise_internal_error:reply(Req0, enterprise_internal_idempotency:conflict_code());
        {error, Reason} ->
            ?ERROR_LOG("enterprise_message_handler idempotency error: ~p~n", [Reason]),
            enterprise_internal_error:reply(Req0, <<"internal_error">>)
    end.

%% @doc 提交后离线推送（FULL-07）：企业托管消息发给离线收件人时必须推送。
%% 目标收件人真源在 logic 内由**已提交的消息行**导出（direct: msg_c2c.to_id；
%% group: 群 active 成员），本壳只负责「在 COMMIT 之后、且仅在首次插入成功
%% 的分支」触发。Result 无 msg_id 时 no-op（不掩盖任何错误）。
-spec push_after_commit(binary(), map()) -> ok.
push_after_commit(Table, #{<<"msg_id">> := MsgId}) when is_binary(MsgId) ->
    enterprise_message_logic:push_after_commit(Table, MsgId);
push_after_commit(_Table, _Result) ->
    ok.

-spec run_logic(any(), map(), map(), binary(), binary(), binary(), fun()) -> term().
run_logic(Conn, Ctx0, Ctx, ResourceType, IdemKey, MsgTable, LogicFun) ->
    case LogicFun(Ctx, Conn) of
        {ok, #{<<"msg_id">> := MsgId} = Result} ->
            RowId = enterprise_message_logic:fetch_row_id_tx(Conn, MsgTable, MsgId),
            Body = jsone:encode(Result),
            ok = enterprise_internal_idempotency:must_complete_tx(
                Conn, Ctx0, ResourceType, IdemKey, RowId, 200, Body
            ),
            {tx_ok, Result, Body};
        {error, {Code, _Detail}} ->
            throw({rollback, {business_error, Code}})
    end.

%% @doc BoundaryFun 调用（形状：fun(Ctx, Conn) -> ok | {error, {Code, Detail}}）。
-spec with_boundary(fun(), map(), any()) -> ok | {error, {binary(), term()}}.
with_boundary(BoundaryFun, Ctx, Conn) ->
    BoundaryFun(Ctx, Conn).

ok_code(Code) when is_integer(Code), Code >= 200, Code < 300 -> Code;
ok_code(_) -> 200.

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
    ).
