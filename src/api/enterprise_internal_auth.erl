-module(enterprise_internal_auth).

-export([
    authenticate_signed/5,
    authenticate_signed_tx/6,
    decide/4,
    take_retry_after_seconds/0
]).

-include("log.hrl").

%% rate_gate → middleware 的 429 Retry-After 通道（进程字典键）。
%% decide/4 的错误返回形态（{error, atom()}，A2 冻结测试按 atom 断言）保持
%% 不变，秒数经此键在同请求进程内传递，take 后即清（见 take_retry_after_seconds/0）。
-define(PD_RETRY_AFTER_SECONDS, {enterprise_internal_auth, retry_after_seconds}).

%%%===================================================================
%%% API
%%%===================================================================

authenticate_signed(Method, Path, Query, Body, Headers) ->
    case enterprise_internal_signature:parse(Headers, erlang:system_time(second)) of
        {ok, Signed} ->
            case
                elib_pg:with_tx(fun(Conn) ->
                    authenticate_signed_tx(Conn, Method, Path, Query, Body, Signed#{
                        headers => Headers
                    })
                end)
            of
                {rollback, _} -> {error, security_gate_closed};
                Result -> Result
            end;
        Error ->
            Error
    end.

authenticate_signed_tx(Conn, Method, Path, Query, Body, Signed) ->
    case enterprise_application_credential_repo:find_by_prefix_tx(Conn, maps:get(prefix, Signed)) of
        {ok, Row} ->
            case enterprise_application_credential_repo:signing_secret(Row) of
                {ok, Secret} ->
                    case
                        enterprise_internal_signature:verify(
                            Secret, Method, Path, Query, Body, Signed
                        )
                    of
                        true -> signed_status_tx(Conn, Row, Signed);
                        false -> {error, invalid_credential}
                    end;
                {error, _} ->
                    {error, invalid_credential}
            end;
        {error, not_found} ->
            {error, invalid_credential};
        {error, _} ->
            {error, security_gate_closed}
    end.

signed_status_tx(Conn, Row, Signed) ->
    case check_credential_status(Conn, Row) of
        {ok, Ctx} ->
            Target = maps:get(<<"x-imboy-organization">>, maps:get(headers, Signed), undefined),
            case enterprise_internal_access_repo:resolve_organization_tx(Conn, Ctx, Target) of
                {ok, Bound} -> reserve_signed_nonce_tx(Conn, Bound, Signed);
                Error -> Error
            end;
        Error ->
            Error
    end.

reserve_signed_nonce_tx(Conn, Ctx, Signed) ->
    case
        enterprise_internal_signature_repo:reserve_tx(
            Conn,
            maps:get(credential_id, Ctx),
            maps:get(nonce, Signed),
            maps:get(seconds, Signed)
        )
    of
        ok ->
            %% Resolve and lock application attribution before touching its credential,
            %% matching the application -> credential order used by rotation/revocation.
            _ = enterprise_application_credential_repo:touch_last_used_tx(
                Conn, maps:get(credential_id, Ctx)
            ),
            {ok, Ctx};
        Error ->
            Error
    end.

%% @doc 中间件编排决策（认证链之上的路由/限流/幂等键编排）。
%% AuthFun/0 返回认证链结果（{ok, Ctx} | {error, StableCode}）——运行时
%% 由 enterprise_internal_middleware 注入池化认证；测试可注入直连事务闭包。
%% 返回 {ok, Ctx}（含路由元数据）或 {error, Code}，Code 为本模块内部
%% 内部错误码使用 atom，由 HTTP 适配器统一转换。manifest 的 snake_case **二进制**码由 HTTP 适配器
%% enterprise_internal_middleware:normalize_code/1 归一（见该函数注释）。
-spec decide(binary(), binary(), map(), fun()) -> {ok, map()} | {error, atom()}.
decide(Method, Path, Headers, AuthFun) when is_map(Headers) ->
    case enterprise_internal_routes:match(Method, Path) of
        {error, not_found} ->
            {error, resource_not_found};
        {ok, Route} ->
            case enterprise_internal_signature:present(Headers) of
                true ->
                    case
                        {
                            maps:is_key(<<"authorization">>, Headers),
                            enterprise_internal_signature:parse(Headers, erlang:system_time(second))
                        }
                    of
                        {false, {ok, _}} -> chain(Route, Headers, AuthFun);
                        _ -> {error, invalid_credential}
                    end;
                false ->
                    {error, invalid_credential}
            end
    end.

%% @doc 取出本请求限流拒绝的 Retry-After 秒数（一次性：取后即清）。
%% rate_gate 在 {limited, _} 分支以进程字典暂存换算后的整秒；cowboy 中间件
%% 与认证链在同一请求进程内顺序执行，enterprise_internal_middleware 在回复
%% 429 前调用本函数取值生成 Retry-After 头。取后清除保证 worker 进程复用
%% 不会把上一请求的秒数泄漏给后续无关响应；未限流/已消费时返回 undefined。
-spec take_retry_after_seconds() -> pos_integer() | undefined.
take_retry_after_seconds() ->
    erlang:erase(?PD_RETRY_AFTER_SECONDS).

%%%===================================================================
%%% 认证链
%%%===================================================================

-spec chain(map(), map(), fun()) -> {ok, map()} | {error, atom()}.
chain(Route, Headers, AuthFun) ->
    case AuthFun() of
        {error, Code} ->
            {error, Code};
        {ok, Ctx} ->
            rate_gate(Route, Headers, Ctx)
    end.

-spec rate_gate(map(), map(), map()) -> {ok, map()} | {error, atom()}.
rate_gate(#{rate_bucket := Bucket} = Route, Headers, Ctx) ->
    AppId = maps:get(application_id, Ctx, 0),
    case enterprise_internal_rate:check(Bucket, AppId) of
        {ok, _Remaining} ->
            idempotency_gate(Route, Headers, Ctx);
        {limited, RetryAfterMs} ->
            log_reject(rate_limit, rate_limited),
            %% 429 Retry-After 头数据源（纯追加，不改变拒绝语义）：
            %% throttle 返回毫秒，HTTP delta-seconds 需整秒——向上取整且
            %% 至少 1s，避免窗口尾沿发出 Retry-After: 0。
            _ = erlang:put(?PD_RETRY_AFTER_SECONDS, ceil_seconds(RetryAfterMs)),
            {error, rate_limited};
        {error, rate_not_configured} ->
            %% INV-9：缺配置 fail-closed，绝不放行
            log_reject(rate_config, security_gate_closed),
            {error, security_gate_closed}
    end.

%% INV-7 + §11：mutation 必带 Idempotency-Key（INT-14 single_use_code 豁免）。
%% Key 形态：1..128 个可打印 ASCII——空/过长/控制字符 400 invalid_request
%% （enterprise_internal_idempotency:valid_key/1）。
-spec idempotency_gate(map(), map(), map()) -> {ok, map()} | {error, atom()}.
idempotency_gate(#{idempotency := required} = Route, Headers, Ctx) ->
    case maps:get(<<"idempotency-key">>, Headers, undefined) of
        Key when is_binary(Key) ->
            case enterprise_internal_idempotency:valid_key(Key) of
                true ->
                    {ok, finalize_ctx(Route, Ctx)};
                false ->
                    log_reject(idempotency_key_invalid, invalid_request),
                    {error, invalid_request}
            end;
        _ ->
            log_reject(idempotency_key_missing, invalid_request),
            {error, invalid_request}
    end;
idempotency_gate(Route, _Headers, Ctx) ->
    {ok, finalize_ctx(Route, Ctx)}.

-spec finalize_ctx(map(), map()) -> map().
finalize_ctx(Route, Ctx) ->
    Ctx#{
        route_id => maps:get(id, Route),
        rate_bucket => maps:get(rate_bucket, Route),
        idempotency => maps:get(idempotency, Route)
    }.

%%%===================================================================
%%% 认证链各步
%%%===================================================================

-spec check_credential_status(any(), map()) ->
    {ok, map()} | {error, atom()}.
check_credential_status(Conn, Row) ->
    case maps:get(<<"status">>, Row) of
        <<"active">> ->
            check_credential_expiry(Conn, Row);
        _Revoked ->
            %% revoked 与未知状态一律 invalid_credential（无 oracle）
            log_reject(credential_revoked, invalid_credential),
            {error, invalid_credential}
    end.

-spec check_credential_expiry(any(), map()) ->
    {ok, map()} | {error, atom()}.
check_credential_expiry(Conn, Row) ->
    case maps:get(<<"expires_at">>, Row) of
        ExpiresAt when is_binary(ExpiresAt), ExpiresAt =/= <<>> ->
            Expired =
                try
                    elib_dt:compare_rfc3339(ExpiresAt, elib_dt:now(), lt) =:= true
                catch
                    _:_ ->
                        %% 无法解析的过期时间：fail-closed 视同过期
                        true
                end,
            case Expired of
                true ->
                    log_reject(credential_expired, credential_expired),
                    {error, credential_expired};
                false ->
                    check_application(Conn, Row)
            end;
        _NoExpiry ->
            check_application(Conn, Row)
    end.

-spec check_application(any(), map()) -> {ok, map()} | {error, atom()}.
check_application(Conn, Row) ->
    AppId = maps:get(<<"application_id">>, Row),
    case enterprise_application_repo:find_by_id_tx(Conn, AppId) of
        {ok, #{<<"status">> := <<"active">>} = App} ->
            finalize_auth(Conn, Row, App);
        {ok, _DisabledApp} ->
            log_reject(application_disabled, application_disabled),
            {error, application_disabled};
        {error, not_found} ->
            %% AppId FK 保证存在；缺失即异常态：fail-closed，不给 oracle
            log_reject(application_missing, invalid_credential),
            {error, invalid_credential};
        {error, Reason} ->
            ?ERROR_LOG([
                enterprise_internal_auth_db_error, #{stage => application_lookup, reason => Reason}
            ]),
            {error, invalid_credential}
    end.

-spec finalize_auth(any(), map(), map()) -> {ok, map()} | {error, atom()}.
finalize_auth(_Conn, Row, App) ->
    CredId = maps:get(<<"id">>, Row),
    OrgId = maps:get(<<"organization_id">>, App),
    {ok, #{
        authentication => hmac,
        organization_id => OrgId,
        application_organization_id => OrgId,
        application_id => maps:get(<<"application_id">>, Row),
        credential_id => CredId,
        application_key => maps:get(<<"application_key">>, App, undefined),
        principal_user_id => maps:get(<<"principal_user_id">>, App, null)
    }}.

-spec log_reject(atom(), atom()) -> ok.
log_reject(Stage, Code) ->
    ?WARN_LOG([
        enterprise_internal_auth_rejected,
        #{stage => Stage, code => Code}
    ]).

%% 毫秒 → HTTP delta-seconds：向上取整，下限 1（Retry-After: 0 无意义，
%% 且窗口尾沿 0ms 会误导客户端立即重试再撞限流）。
-spec ceil_seconds(non_neg_integer()) -> pos_integer().
ceil_seconds(Ms) when is_integer(Ms), Ms >= 0 ->
    max(1, (Ms + 999) div 1000).
