-module(enterprise_internal_auth).

%%%
% enterprise_internal_auth 是 /api/internal/v1/* 的 Application Credential
% 认证链与中间件编排（EPGZ-02，plan-gz §4.1 / manifest auth_contexts）。
%
% 凭证形态：Authorization: Bearer ib_int_<credential_id>.<secret>。
% 认证顺序（固定，任一步失败立即终止并返回对应 stable 码）：
%   TLS（部署层终结；本地测试豁免——本模块不重复判定，见 moduledoc 尾注）
%   -> credential 格式/digest（prefix 定位 + SHA-256 常数时间比对）
%   -> credential active/expiry（revoked→invalid_credential；过期→credential_expired）
%   -> application active（disabled→application_disabled）
%   -> organization active（archived/缺失→organization_disabled）
%   -> Grant 求值（FULL-01：受管应用的生效 scope = allowed_scopes ∩ 生效 Grant
%      scopes，逐请求真库读、无缓存；读取失败 fail-closed→security_gate_closed）
%   -> required scope（静态路由在此判定；INT-09/10 动态 scope 由 handler
%      按 sender_mode 裁决，ctx 标注 dynamic_scope）
%   -> rate limit（internal_read/write/sso；缺配置 fail-closed）
%   -> Idempotency-Key 存在性（mutation 必带；INT-14 豁免）
%   （organization/workspace/resource boundary 与 operation 由 handler 用
%    认证产物 ctx 继续裁决；audit 在业务侧。Grant 的 workspace 边界求值入口：
%    enterprise_application_grant_logic:require_workspace_tx/4）
%
% 认证产物 context（atom 键，供 A3/A4/A5 handler 使用）：
%   #{organization_id, application_id, credential_id, application_key,
%     granted_scopes :: [binary()], grant_governed :: boolean(),
%     route_id, rate_bucket, idempotency,
%     dynamic_scope（仅动态 scope 路由）}
%   granted_scopes 是**生效** scope = allowed_scopes ∩ 生效 Grant scopes
%   （零 Grant ⇒ 空集 ⇒ 任一 required scope 均 403 insufficient_scope，
%   V2.1 §5.2/F-09：不存在「零 Grant 回退 allowed_scopes」旁路）；
%   grant_governed 仅为诊断标记（是否存在任何 Grant 行），不参与判定。
%
% redaction 红线：secret/Authorization 值/prefix 不落日志（失败日志只含
% stage 与 stable 码）；错误响应只含 stable 码。
%
% TLS 注：生产公网必须 HTTPS，由部署层（Caddy/gateway）终结并在应用前
% 强制；本地/CI 直连 HTTP 测试豁免。应用层不读 X-Forwarded-Proto 猜测，
% 避免可伪造头引入旁路。
%%%

-export([
    parse_bearer/1,
    authenticate_tx/3,
    authenticate/2,
    decide/4
]).

-include("log.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc 解析 Authorization 头（Bearer ib_int_<id>.<secret>）。
%% 缺头 → credential_missing；格式非法 → invalid_credential。
-spec parse_bearer(undefined | binary()) ->
    {ok, Prefix :: binary(), Secret :: binary()} | {error, atom()}.
parse_bearer(undefined) ->
    {error, credential_missing};
parse_bearer(<<>>) ->
    {error, credential_missing};
parse_bearer(<<"Bearer ", Rest/binary>>) when Rest =/= <<>> ->
    parse_credential(Rest);
parse_bearer(_) ->
    {error, invalid_credential}.

%% @doc 事务内执行完整认证链（prefix 定位 → digest 常数时间比对 →
%% credential active/expiry → application active → organization active →
%% touch last_used）。成功返回认证产物 context。
-spec authenticate_tx(any(), binary(), binary()) ->
    {ok, map()}
    | {error,
        invalid_credential
        | credential_expired
        | application_disabled
        | organization_disabled
        | security_gate_closed
        | internal_error}.
authenticate_tx(Conn, Prefix, Secret) when
    is_binary(Prefix), is_binary(Secret), Secret =/= <<>>
->
    case enterprise_application_credential_repo:find_by_prefix_tx(Conn, Prefix) of
        {error, not_found} ->
            log_reject(credential_locate, invalid_credential),
            {error, invalid_credential};
        {ok, Row} ->
            StoredDigest = maps:get(<<"secret_digest">>, Row),
            Computed = enterprise_application_credential_repo:digest_hex(Secret),
            case constant_time_eq(Computed, StoredDigest) of
                false ->
                    log_reject(credential_digest, invalid_credential),
                    {error, invalid_credential};
                true ->
                    check_credential_status(Conn, Row)
            end;
        {error, Reason} ->
            ?ERROR_LOG([
                enterprise_internal_auth_db_error, #{stage => credential_locate, reason => Reason}
            ]),
            {error, invalid_credential}
    end;
authenticate_tx(_Conn, _Prefix, _Secret) ->
    log_reject(credential_format, invalid_credential),
    {error, invalid_credential}.

%% @doc 池化运行时入口（中间件用）：单事务执行认证链。
-spec authenticate(binary(), binary()) ->
    {ok, map()} | {error, atom()}.
authenticate(Prefix, Secret) ->
    case elib_pg:with_tx(fun(Conn) -> authenticate_tx(Conn, Prefix, Secret) end) of
        {rollback, Reason} ->
            ?ERROR_LOG([
                enterprise_internal_auth_db_error, #{stage => tx, reason => Reason}
            ]),
            {error, invalid_credential};
        Result ->
            Result
    end.

%% @doc 中间件编排决策（认证链之上的路由/scope/限流/幂等键编排）。
%% AuthFun/0 返回认证链结果（{ok, Ctx} | {error, StableCode}）——运行时
%% 由 enterprise_internal_middleware 注入池化认证；测试可注入直连事务闭包。
%% 返回 {ok, Ctx}（含路由元数据）或 {error, Code}，Code 为本模块内部
%% **atom** 约定（与 parse_bearer/1、authenticate_tx/* 一致：invalid_credential /
%% resource_not_found / insufficient_scope / rate_limited / security_gate_closed /
%% invalid_request 等）。manifest 的 snake_case **二进制**码由 HTTP 适配器
%% enterprise_internal_middleware:normalize_code/1 归一（见该函数注释）。
-spec decide(binary(), binary(), map(), fun()) -> {ok, map()} | {error, atom()}.
decide(Method, Path, Headers, AuthFun) when is_map(Headers) ->
    case enterprise_internal_routes:match(Method, Path) of
        {error, not_found} ->
            {error, resource_not_found};
        {ok, Route} ->
            case parse_bearer(maps:get(<<"authorization">>, Headers, undefined)) of
                {error, Code} when Code =:= credential_missing; Code =:= invalid_credential ->
                    %% 缺头/畸形头统一 invalid_credential（401，无 oracle）。
                    %% 码形态保持本模块内部的 atom 约定（与 parse_bearer/1、
                    %% authenticate_tx/* 一致；A2 冻结测试按 atom 断言）。
                    %% HTTP 面需要的 manifest **二进制**码由适配器
                    %% enterprise_internal_middleware:normalize_code/1 统一归一
                    %% ——atom 越过该边界会落 error 信封的"未知码 → 500
                    %% internal_error"兜底，把 401 变 500（W4 真 cowboy 请求实测）。
                    log_reject(credential_header, invalid_credential),
                    {error, invalid_credential};
                {ok, _Prefix, _Secret} ->
                    chain(Route, Headers, AuthFun)
            end
    end.

%%%===================================================================
%%% 认证链
%%%===================================================================

-spec chain(map(), map(), fun()) -> {ok, map()} | {error, atom()}.
chain(Route, Headers, AuthFun) ->
    case AuthFun() of
        {error, Code} ->
            {error, Code};
        {ok, Ctx} ->
            case scope_gate(Route, Ctx) of
                {ok, Ctx1} ->
                    rate_gate(Route, Headers, Ctx1);
                {error, Code} ->
                    {error, Code}
            end
    end.

%% 静态 scope 在中间件判定；动态 scope（INT-09/10）由 handler 裁决
%% （ctx 标注 dynamic_scope 后原样放行）。
-spec scope_gate(map(), map()) -> {ok, map()} | {error, atom()}.
scope_gate(#{scope := {dynamic, Kind}}, Ctx) ->
    {ok, Ctx#{dynamic_scope => Kind}};
scope_gate(#{scope := Required}, Ctx) ->
    case enterprise_internal_scope:authorize(Required, maps:get(granted_scopes, Ctx, [])) of
        ok ->
            {ok, Ctx};
        {error, insufficient_scope} ->
            log_reject(scope, insufficient_scope),
            {error, insufficient_scope};
        {error, invalid_scope} ->
            %% 注册表 scope 不在固定枚举内属程序错误：fail-closed 拒绝
            log_reject(scope, insufficient_scope),
            {error, insufficient_scope}
    end.

-spec rate_gate(map(), map(), map()) -> {ok, map()} | {error, atom()}.
rate_gate(#{rate_bucket := Bucket} = Route, Headers, Ctx) ->
    AppId = maps:get(application_id, Ctx, 0),
    case enterprise_internal_rate:check(Bucket, AppId) of
        {ok, _Remaining} ->
            idempotency_gate(Route, Headers, Ctx);
        {limited, _RetryAfter} ->
            log_reject(rate_limit, rate_limited),
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
    OrgId = maps:get(<<"organization_id">>, Row),
    AppId = maps:get(<<"application_id">>, Row),
    case enterprise_application_repo:find_tx(Conn, OrgId, AppId) of
        {ok, #{<<"status">> := <<"active">>} = App} ->
            check_organization(Conn, Row, App);
        {ok, _DisabledApp} ->
            log_reject(application_disabled, application_disabled),
            {error, application_disabled};
        {error, not_found} ->
            %% 复合 FK 保证存在；缺失即异常态：fail-closed，不给 oracle
            log_reject(application_missing, invalid_credential),
            {error, invalid_credential};
        {error, Reason} ->
            ?ERROR_LOG([
                enterprise_internal_auth_db_error, #{stage => application_lookup, reason => Reason}
            ]),
            {error, invalid_credential}
    end.

-spec check_organization(any(), map(), map()) -> {ok, map()} | {error, atom()}.
check_organization(Conn, Row, App) ->
    OrgId = maps:get(<<"organization_id">>, Row),
    case organization_status(Conn, OrgId) of
        {ok, <<"active">>} ->
            finalize_auth(Conn, Row, App);
        {ok, _Archived} ->
            log_reject(organization_disabled, organization_disabled),
            {error, organization_disabled};
        {error, not_found} ->
            %% org 缺失同样落 organization_disabled（fail-closed）
            log_reject(organization_missing, organization_disabled),
            {error, organization_disabled};
        {error, Reason} ->
            ?ERROR_LOG([
                enterprise_internal_auth_db_error, #{stage => organization_lookup, reason => Reason}
            ]),
            {error, invalid_credential}
    end.

%% organization_repo 未导出普通事务内 find（find_for_update_tx 会行锁
%% organization 行，认证高频路径不可接受）——此处在认证模块内做无锁
%% 只读 status 查询（repo 缺口已记录 EPGZ-02 checkpoint 交 A0 裁决）。
-spec organization_status(any(), integer()) ->
    {ok, binary()} | {error, not_found | term()}.
organization_status(Conn, OrgId) ->
    Sql =
        <<"SELECT status FROM organization WHERE id = $1 LIMIT 1">>,
    case elib_pg:query(Conn, Sql, [OrgId]) of
        {ok, [#{<<"status">> := Status} | _]} ->
            {ok, Status};
        {ok, []} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

-spec finalize_auth(any(), map(), map()) -> {ok, map()} | {error, atom()}.
finalize_auth(Conn, Row, App) ->
    CredId = maps:get(<<"id">>, Row),
    %% last_used 记录失败不影响认证结果
    _ = enterprise_application_credential_repo:touch_last_used_tx(Conn, CredId),
    Scopes = decode_scopes(App),
    OrgId = maps:get(<<"organization_id">>, Row),
    AppId = maps:get(<<"application_id">>, Row),
    %% FULL-01 Grant 求值（V2.1 §5.2 收紧）：同一事务、逐请求真库读（无缓存）。
    %% 生效 scope 恒为 allowed_scopes ∩ 生效 Grant scopes——零 Grant（从未授予/
    %% 全部撤销/全部过期）即空集，本请求的 scope gate 必然 403 insufficient_scope；
    %% grant_governed 只是诊断标记，不再触发「回退 allowed_scopes」旁路。
    case enterprise_application_grant_logic:context_tx(Conn, OrgId, AppId, Scopes) of
        {ok, #{grant_governed := Governed, effective_scopes := Effective}} ->
            {ok, #{
                organization_id => OrgId,
                application_id => AppId,
                credential_id => CredId,
                application_key => maps:get(<<"application_key">>, App, undefined),
                %% INT-BE-03：principal 随认证产物下传（Application 的锚定
                %% 用户，可空），供审计行 actor_user_id 统一口径——原先只有
                %% message/webhook 壳在 handler 层自行预取，其余 mutation 的
                %% 审计 actor 只能落 null。
                principal_user_id => maps:get(<<"principal_user_id">>, App, null),
                granted_scopes => Effective,
                grant_governed => Governed
            }};
        {error, security_gate_closed} ->
            %% 授权读取失败：fail-closed，绝不放行（也不给 oracle）
            log_reject(grant_read_failed, security_gate_closed),
            {error, security_gate_closed}
    end.

%% allowed_scopes（jsonb 数组）→ 固定 scope 二进制列表；形态异常一律空集
%% （空集 fail-closed：任何 required scope 都不会被满足）。
-spec decode_scopes(map()) -> [binary()].
decode_scopes(App) ->
    case jsone:decode(maps:get(<<"allowed_scopes">>, App, <<"[]">>)) of
        L when is_list(L) -> [S || S <- L, is_binary(S)];
        _ -> []
    end.

%%%===================================================================
%%% 内部
%%%===================================================================

-spec parse_credential(binary()) -> {ok, binary(), binary()} | {error, invalid_credential}.
parse_credential(Rest) ->
    case binary:split(Rest, <<".">>) of
        [Prefix, Secret] when Secret =/= <<>> ->
            case is_ib_int_prefix(Prefix) of
                true -> {ok, Prefix, Secret};
                false -> {error, invalid_credential}
            end;
        _ ->
            {error, invalid_credential}
    end.

-spec is_ib_int_prefix(binary()) -> boolean().
is_ib_int_prefix(<<"ib_int_", Digits/binary>>) when Digits =/= <<>> ->
    is_digits(Digits);
is_ib_int_prefix(_) ->
    false.

-spec is_digits(binary()) -> boolean().
is_digits(<<>>) ->
    true;
is_digits(<<C, Rest/binary>>) when C >= $0, C =< $9 ->
    is_digits(Rest);
is_digits(_) ->
    false.

%% 等长常数时间摘要比较（镜像 eb_managed_crypto:constant_time_eq/2 先例）。
-spec constant_time_eq(binary(), binary()) -> boolean().
constant_time_eq(A, B) when byte_size(A) =:= byte_size(B) ->
    crypto:hash_equals(A, B);
constant_time_eq(_A, _B) ->
    false.

%% 认证失败日志：只含 stage 与 stable 码——不含 prefix/secret/Authorization
%% 值/请求体（redaction 红线，测试断言日志行零 secret）。
-spec log_reject(atom(), binary()) -> ok.
log_reject(Stage, Code) ->
    ?WARN_LOG([
        enterprise_internal_auth_rejected,
        #{stage => Stage, code => Code}
    ]).
