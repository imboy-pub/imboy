-module(enterprise_friend_request_handler).

%%%
% EPGZ-08 W4 INT-11 好友申请（只发起）端点壳（handler 壳需求见 A3 EPGZ-03
% 冻结合同「给 A0 W4 的 handler 壳需求清单」，本模块严格照此编排）。
%
% 路由（A0 W4 经 Router lease 登记）：
%   POST /api/internal/v1/friend-requests -> #{action => create}
% —— 必须经 enterprise_internal_middleware（A2 认证链：credential →
%    active/expiry → application active → organization active → scope
%    friend_requests:create → rate internal_write fail-closed）。scope 必须
%    显式授予，无隐含包含（INV-4）。
%
% 幂等（A2 模式，INV-7）：Idempotency-Key 已由 decide 强制存在；单事务
% begin_tx -> logic -> complete_tx -> COMMIT；业务 {error} 整体回滚。
%
% **通知在 COMMIT 之后**（A3 冻结口径，best-effort）：失败不影响申请结果，
% 也绝不在事务内发送——避免回滚后仍向目标推了通知。
%
% 硬边界（plan §3）：只创建 pending 态申请（user_friend status=0），复用现有
% 人工审批流（friend_logic:confirm_friend / reject_friend），**无自动接受/确认
% /删除/批量外部路径**（A3 导出面负例已断言）。
%%%

-behavior(cowboy_rest).

-export([init/2]).

-include("log.hrl").

-define(CREATE_PATH, <<"/api/internal/v1/friend-requests">>).

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
            create -> create(Method, Req0, State);
            _ -> Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal
%% ===================================================================

-spec create(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
create(<<"POST">>, Req0, State) ->
    Ctx = maps:get(enterprise_internal, State, #{}),
    Params = elib_param:post(Req0),
    IdemKey = idempotency_key(Req0),
    case enterprise_internal_idempotency:request_digest(<<"POST">>, ?CREATE_PATH, Params) of
        {ok, Digest} -> create_tx(Req0, Ctx, IdemKey, Digest, Params);
        {error, non_canonical} -> enterprise_internal_error:reply(Req0, <<"invalid_request">>)
    end;
create(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

-spec create_tx(cowboy_req:req(), map(), binary(), binary(), map()) -> cowboy_req:req().
create_tx(Req0, Ctx, IdemKey, Digest, Params) ->
    TxResult =
        elib_pg:with_tx(fun(Conn) ->
            case
                enterprise_internal_idempotency:begin_tx(
                    Conn, Ctx, <<"friend_request">>, IdemKey, Digest
                )
            of
                {ok, inserted} ->
                    case
                        enterprise_friend_request_logic:create_request_tx(
                            Conn, Ctx, params_to_input(Params)
                        )
                    of
                        {ok, Result} ->
                            Body = jsone:encode(Result),
                            _ = enterprise_internal_idempotency:complete_tx(
                                Conn, Ctx, <<"friend_request">>, IdemKey, null, 200, Body
                            ),
                            {tx_ok, Result, Body};
                        {error, {Code, _Detail}} ->
                            throw({rollback, {business_error, Code}})
                    end;
                Other ->
                    Other
            end
        end),
    case TxResult of
        {tx_ok, Result, Body} ->
            %% COMMIT 后通知（独立池化路径；失败只记日志，不影响申请结果；
            %% replay 分支不走这里——同一申请不因客户端重放而二次通知，§11）
            notify_after_commit(Ctx, Result, Params),
            reply_json_body(Req0, 200, Body);
        {rollback, {business_error, Code}} ->
            enterprise_internal_error:reply(Req0, Code);
        {rollback, Reason} ->
            ?ERROR_LOG("enterprise_friend_request_handler rollback: ~p~n", [Reason]),
            enterprise_internal_error:reply(Req0, <<"internal_error">>);
        {ok, replay, #{response_code := Code, response_body := Body}} ->
            replay_json_body(Req0, Code, Body);
        {ok, pending} ->
            enterprise_internal_error:reply(Req0, <<"idempotency_conflict">>);
        {error, digest_conflict} ->
            enterprise_internal_error:reply(Req0, enterprise_internal_idempotency:conflict_code());
        {error, Reason} ->
            ?ERROR_LOG("enterprise_friend_request_handler idempotency error: ~p~n", [Reason]),
            enterprise_internal_error:reply(Req0, <<"internal_error">>)
    end.

%% @doc 提交后通知：外部 id 已在 logic 内校验为双侧 active 映射，这里独立
%% 事务回读 uid（logic 结果只带 external id，不带 uid）。任一步失败只记日志。
-spec notify_after_commit(map(), map(), map()) -> ok.
notify_after_commit(Ctx, Result, Params) ->
    OrgId = maps:get(organization_id, Ctx, undefined),
    AppId = maps:get(application_id, Ctx, undefined),
    SenderExt = maps:get(<<"sender_user_id">>, Result, undefined),
    TargetExt = maps:get(<<"target_user_id">>, Result, undefined),
    Greeting = maps:get(<<"greeting">>, Params, <<>>),
    case {is_integer(OrgId), is_integer(AppId), SenderExt, TargetExt} of
        {true, true, S, T} when is_binary(S), is_binary(T) ->
            Uids = elib_pg:with_tx(fun(Conn) ->
                enterprise_external_identity_repo:resolve_tx(Conn, OrgId, AppId, [S, T])
            end),
            case Uids of
                {ok, Rows} when is_list(Rows) ->
                    case {uid_of(S, Rows), uid_of(T, Rows)} of
                        {SU, TU} when is_integer(SU), is_integer(TU) ->
                            _ = enterprise_friend_request_logic:notify_request(#{
                                sender_uid => SU, target_uid => TU, greeting => Greeting
                            }),
                            ok;
                        _ ->
                            ok
                    end;
                _ ->
                    ok
            end;
        _ ->
            ok
    end.

uid_of(ExternalUserId, Rows) ->
    case
        lists:search(
            fun(R) -> maps:get(<<"external_user_id">>, R, undefined) =:= ExternalUserId end, Rows
        )
    of
        {value, Row} ->
            case maps:get(<<"user_id">>, Row, undefined) of
                U when is_integer(U) -> U;
                _ -> undefined
            end;
        false ->
            undefined
    end.

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

%% @doc 冻结合同只读 sender_user_id / target_user_id / greeting 三键；
%% 未知键丢弃（不做 blanket binary_to_atom）。
params_to_input(Params) when is_map(Params) ->
    maps:fold(
        fun(K, V, Acc) ->
            case known_key(K) of
                {ok, Atom} -> Acc#{Atom => V};
                error -> Acc
            end
        end,
        #{},
        Params
    );
params_to_input(_) ->
    #{}.

known_key(<<"sender_user_id">>) -> {ok, sender_user_id};
known_key(<<"target_user_id">>) -> {ok, target_user_id};
known_key(<<"greeting">>) -> {ok, greeting};
known_key(_) -> error.
