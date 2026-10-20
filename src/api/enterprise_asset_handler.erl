-module(enterprise_asset_handler).

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
            governance -> governance(Method, Req0, State);
            _ -> Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal
%% ===================================================================

-spec presign(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
presign(<<"POST">>, Req0, State) ->
    Ctx = maps:get(enterprise_internal, State, #{}),
    Params = enterprise_internal_request:params(Req0),
    IdemKey = idempotency_key(Req0),
    case
        enterprise_internal_idempotency:request_digest(
            <<"POST">>, <<"/api/internal/v1/files/presign">>, Params
        )
    of
        {ok, Digest} ->
            %% F19（02 §137）：幂等事务只保存稳定登记结果（无能力）；
            %% COMMIT（含幂等重放）后由 post_commit_presign 签发受控网关
            %% 能力。DB 不可用/回滚一律无能力泄漏（503 语义在错误分支）。
            with_idempotency(
                Req0,
                Ctx,
                <<"enterprise_asset_presign">>,
                IdemKey,
                Digest,
                fun(Conn) ->
                    with_boundary(Conn, Ctx, <<"INT-07">>, undefined, fun() ->
                        enterprise_asset_logic:presign_tx(Conn, Ctx, params_to_input(Params))
                    end)
                end,
                fun(Req, Body, Kind) -> post_commit_presign(Req, Ctx, Body, Kind) end
            );
        {error, non_canonical} ->
            enterprise_internal_error:reply(Req0, <<"invalid_request">>)
    end;
presign(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

-spec confirm(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
confirm(<<"POST">>, Req0, State) ->
    Ctx = maps:get(enterprise_internal, State, #{}),
    Params = enterprise_internal_request:params(Req0),
    IdemKey = idempotency_key(Req0),
    case
        enterprise_internal_idempotency:request_digest(
            <<"POST">>, <<"/api/internal/v1/files/confirm">>, Params
        )
    of
        {ok, Digest} ->
            with_idempotency(Req0, Ctx, <<"attachment">>, IdemKey, Digest, fun(Conn) ->
                with_boundary(Conn, Ctx, <<"INT-08">>, undefined, fun() ->
                    enterprise_asset_logic:confirm_tx(Conn, Ctx, params_to_input(Params))
                end)
            end);
        {error, non_canonical} ->
            enterprise_internal_error:reply(Req0, <<"invalid_request">>)
    end;
confirm(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

-spec governance(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
governance(<<"POST">>, Req0, State) ->
    Ctx = maps:get(enterprise_internal, State, #{}),
    Params = enterprise_internal_request:params(Req0),
    IdemKey = idempotency_key(Req0),
    case
        enterprise_internal_idempotency:request_digest(
            <<"POST">>, <<"/api/internal/v1/files/governance">>, Params
        )
    of
        {ok, Digest} ->
            with_idempotency(
                Req0, Ctx, <<"enterprise_asset_governance">>, IdemKey, Digest, fun(Conn) ->
                    with_boundary(Conn, Ctx, <<"INT-22">>, undefined, fun() ->
                        enterprise_asset_retention_logic:governance_tx(
                            Conn, Ctx, params_to_input(Params)
                        )
                    end)
                end
            );
        {error, non_canonical} ->
            enterprise_internal_error:reply(Req0, <<"invalid_request">>)
    end;
governance(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

-spec with_boundary(any(), map(), binary(), undefined | integer(), fun()) ->
    {ok, map()} | {error, {binary(), term()}}.
with_boundary(Conn, Ctx, RouteId, WorkspaceId, Fun) ->
    case enterprise_internal_boundary:enforce(Conn, Ctx, RouteId, WorkspaceId) of
        ok ->
            Fun();
        {error, Code} ->
            {error, {enterprise_internal_boundary:error_code(Code), resource_attribution}}
    end.

%% @doc A2 幂等模式（单事务）。返回值契约见 enterprise_internal_idempotency。
-spec with_idempotency(
    cowboy_req:req(), map(), binary(), binary() | undefined, binary(), fun()
) ->
    cowboy_req:req().
with_idempotency(Req0, Ctx, ResourceType, IdemKey, Digest, LogicFun) ->
    with_idempotency(Req0, Ctx, ResourceType, IdemKey, Digest, LogicFun, undefined).

%% F19（02 §137）：幂等事务只保存稳定登记结果；PostCommitFun 在事务
%% 成功提交（含幂等重放）后执行，签名类能力只在这里产生。DB 异常/回滚
%% 永远到不了签名分支（503 无能力）。
with_idempotency(Req0, Ctx, ResourceType, IdemKey, Digest, LogicFun, PostCommitFun) ->
    TxResult =
        elib_pg:with_tx(fun(Conn) ->
            case
                enterprise_internal_idempotency:begin_tx(Conn, Ctx, ResourceType, IdemKey, Digest)
            of
                {ok, inserted} ->
                    case LogicFun(Conn) of
                        {ok, Result} ->
                            Body = jsone:encode(Result),
                            ok = enterprise_internal_idempotency:must_complete_tx(
                                Conn,
                                Ctx,
                                ResourceType,
                                IdemKey,
                                resource_id_of(Result),
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
            post_commit_reply(Req0, PostCommitFun, Body, plain);
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
        {ok, replay, #{response_code := _Code, response_body := Body}} ->
            post_commit_reply(Req0, PostCommitFun, Body, replay);
        {ok, pending} ->
            enterprise_internal_error:reply(Req0, <<"idempotency_conflict">>);
        {error, digest_conflict} ->
            enterprise_internal_error:reply(Req0, enterprise_internal_idempotency:conflict_code());
        {error, Reason} ->
            ?ERROR_LOG("enterprise_asset_handler idempotency error: ~p~n", [Reason]),
            enterprise_internal_error:reply(Req0, <<"internal_error">>)
    end.

post_commit_reply(Req0, undefined, Body, Kind) ->
    case Kind of
        replay -> replay_json_body(Req0, 200, Body);
        _ -> reply_json_body(Req0, 200, Body)
    end;
post_commit_reply(Req0, PostCommitFun, Body, Kind) ->
    BodyBin =
        case Body of
            B when is_binary(B), B =/= <<>> -> B;
            _ -> <<"{}">>
        end,
    PostCommitFun(Req0, BodyBin, Kind).

%% 签名后响应统一出口：重放携带幂等重放头；V5 会话/续签视图一律 no-store。
presign_signed_reply(Req0, Status, Body, Kind, NoStore) ->
    Base = #{<<"content-type">> => <<"application/json">>},
    H0 =
        case Kind of
            replay -> maps:from_list([enterprise_internal_idempotency:replay_header()]);
            _ -> #{}
        end,
    Headers =
        case NoStore of
            true -> H0#{<<"cache-control">> => <<"no-store">>};
            false -> H0
        end,
    cowboy_req:reply(Status, maps:merge(Base, Headers), Body, Req0).

%% Registration has already committed. Reauthorize and read the current session
%% under its normal I/path/session lock, retaining that lock through signing.
%% 原登记已提交；重验授权并持既有身份锁签发，回读事务失败时不释放任何能力。
recover_presign_reply(Req0, Ctx, Reg, Kind) ->
    Result =
        try
            elib_pg:with_tx(fun(Conn) ->
                with_boundary(Conn, Ctx, <<"INT-07">>, undefined, fun() ->
                    case enterprise_asset_logic:presign_recovery_tx(Conn, Ctx, Reg) of
                        {ok, #{<<"session_state">> := <<"registered">>} = Fresh} ->
                            signed_session_data(Fresh);
                        Other ->
                            Other
                    end
                end)
            end)
        catch
            _:_ -> {error, {<<"security_gate_closed">>, recovery_unavailable}}
        end,
    case Result of
        {ok, View} when is_map(View) ->
            presign_signed_reply(Req0, 200, jsone:encode(View), Kind, true);
        {error, {Code, _}} when is_binary(Code) ->
            presign_recovery_error(Req0, Code, Kind);
        _ ->
            presign_recovery_error(Req0, <<"security_gate_closed">>, Kind)
    end.

signed_session_data(Reg) ->
    case maps:get(<<"expires_at">>, Reg) > erlang:system_time(second) of
        true -> signed_unexpired_session_data(Reg);
        false -> {error, {<<"gone">>, session_expired}}
    end.

signed_unexpired_session_data(Reg) ->
    Base = application:get_env(imboy, base_url, <<>>),
    case uri_string:parse(Base) of
        #{scheme := Scheme, host := Host} when
            (Scheme =:= <<"http">> orelse Scheme =:= <<"https">>), Host =/= <<>>
        ->
            Key = maps:get(<<"object_key">>, Reg),
            case
                imboy_jwt:issue_upload_capability(
                    Key,
                    maps:get(<<"mime_type">>, Reg),
                    maps:get(<<"expires_at">>, Reg)
                )
            of
                {ok, Capability} ->
                    Cid = maps:get(<<"client_upload_id">>, Reg),
                    Prefix = list_to_binary(string:trim(binary_to_list(Base), trailing, "/")),
                    Url =
                        <<Prefix/binary, "/api/v1/attachment/gateway/", Cid/binary, "?capability=",
                            Capability/binary>>,
                    {ok, Reg#{
                        <<"put_url">> => Url,
                        <<"upload_capability">> => Capability,
                        <<"gateway">> => true
                    }};
                _ ->
                    {error, {<<"security_gate_closed">>, signing_unavailable}}
            end;
        _ ->
            {error, {<<"security_gate_closed">>, signing_unavailable}}
    end.

%% Keep the internal API's stable error envelope, with real lifecycle HTTP status
%% and the recovery retry hint. No cached registration or capability is echoed.
%% 保留内部接口稳定错误信封，以真实状态码和恢复提示失败关闭，不回显能力。
presign_recovery_error(Req, Code, Kind) ->
    {Status, StableCode, Retry} =
        case Code of
            <<"gone">> -> {410, <<"resource_conflict">>, <<"reupload_required">>};
            <<"resource_conflict">> -> {409, Code, <<"reupload_required">>};
            <<"security_gate_closed">> -> {503, Code, <<"retry_same_id">>};
            _ -> {enterprise_internal_error:http_status(Code), Code, <<"none">>}
        end,
    Envelope = jsone:decode(enterprise_internal_error:error_body(StableCode)),
    Body = Envelope#{<<"can_resume">> => false, <<"retry_action">> => Retry},
    presign_signed_reply(Req, Status, jsone:encode(Body), Kind, true).

resource_id_of(Result) when is_map(Result) ->
    case maps:find(<<"file_id">>, Result) of
        {ok, Id} when is_integer(Id) -> Id;
        _ -> null
    end.

%% COMMIT（含重放）后的能力签发（02 §137：内部应用 V5 会话发受控网关
%% 能力；重放同样走这里，以冻结 expiry 重签新 token）。status 只读视图
%% 不产生能力；旧缓存裸 PUT URL 拒绝重放。
post_commit_presign(Req0, Ctx, Body, Kind) ->
    Decoded =
        try
            jsone:decode(Body)
        catch
            _:_ -> Body
        end,
    case Decoded of
        #{<<"session_state">> := _, <<"client_upload_id">> := Cid} = Reg when
            is_binary(Cid), byte_size(Cid) > 0, is_map_key(<<"bucket">>, Reg)
        ->
            recover_presign_reply(Req0, Ctx, Reg, Kind);
        #{<<"can_resume">> := _} = StatusView when is_map(StatusView) ->
            presign_signed_reply(Req0, 200, jsone:encode(StatusView), Kind, true);
        #{<<"put_url">> := _} ->
            presign_recovery_error(Req0, <<"resource_conflict">>, Kind);
        _ when Kind =:= replay ->
            replay_json_body(Req0, 200, Body);
        _ ->
            reply_json_body(Req0, 200, Body)
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

params_to_input(Params) when is_map(Params) ->
    maps:fold(
        fun(K, V, Acc) ->
            Acc#{binary_to_atom(K, utf8) => V}
        end,
        #{},
        Params
    ).
