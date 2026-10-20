-module(enterprise_oa_sso_logic).

-export([
    issue_code/2,
    issue_code_tx/3,
    entries/1,
    entries_tx/2,
    login_start_fields/2,
    exchange/2,
    exchange_tx/3,
    validate_issue_params/1,
    validate_exchange_params/1
]).

-include("log.hrl").
-include("error_code.hrl").

%% code 形态（合同 §2，冻结）：前缀 + ≥256bit CSPRNG base64url（无 padding）。
-define(CODE_PREFIX, <<"oa_sso_">>).
-define(CODE_RANDOM_BYTES, 32).
%% TTL 固定 60 秒，不可配置放大（合同 §3.3 冻结）。
-define(CODE_TTL_SECONDS, 60).

%%%===================================================================
%%% API
%%%===================================================================

%% 配置发现返回实例内可用 OA 应用（定位修订：org/成员关系不参与发现）。
%% 空列表在标准部署属故障态（M0 seed 保证默认应用存在）；客户端据以静默
%% 隐藏 OA 入口，不向用户报错。
entries(Uid) when is_integer(Uid), Uid > 0 ->
    case elib_pg:with_tx(fun(Conn) -> entries_tx(Conn, Uid) end) of
        {ok, _} = Result -> Result;
        _ -> {error, {?ERR_INTERNAL_SERVER_ERROR, <<"配置读取失败"/utf8>>}}
    end;
entries(_) ->
    {error, {?ERR_UNAUTHORIZED, <<"未登录，请先登录"/utf8>>}}.

%% Uid 仅作已认证断言（JWT 已解析）；发现查询本身不依赖用户身份。
entries_tx(Conn, Uid) when is_integer(Uid), Uid > 0 ->
    case enterprise_application_repo:workbench_entries_tx(Conn) of
        {ok, Rows} ->
            Entries = lists:filtermap(fun entry/1, Rows),
            {ok, #{<<"entries">> => Entries}};
        {error, _} ->
            {error, {?ERR_INTERNAL_SERVER_ERROR, <<"配置读取失败"/utf8>>}}
    end.

entry(#{<<"allowed_redirect_uris">> := [Redirect | _], <<"application_key">> := Key} = Row) ->
    case valid_redirect_uri(Redirect) andalso valid_application_key(Key) of
        true ->
            Name = unicode:characters_to_list(maps:get(<<"name">>, Row, <<>>)),
            {true,
                maps:merge(login_start_fields(Key, Redirect), #{
                    <<"kind">> => <<"oa">>,
                    <<"organization_id">> => maps:get(<<"organization_id">>, Row),
                    <<"application_id">> => maps:get(<<"application_id">>, Row),
                    <<"application_key">> => Key,
                    <<"label">> => unicode:characters_to_binary(lists:sublist(Name, 64)),
                    <<"redirect_uri">> => Redirect
                })};
        false ->
            false
    end;
entry(_) ->
    false.

%% Optional deployment-owned URL per application; never derive an OA endpoint.
login_start_fields(Key, Redirect) ->
    Config = application:get_env(imboy, oa_login_start_uris, #{}),
    Uri =
        case Config of
            M when is_map(M) -> maps:get(Key, M, undefined);
            _ -> undefined
        end,
    case {https_origin(Uri), https_origin(Redirect)} of
        {{ok, Origin}, {ok, Origin}} -> #{<<"login_start_uri">> => Uri};
        _ -> #{}
    end.

https_origin(Uri) when is_binary(Uri), byte_size(Uri) > 0, byte_size(Uri) =< 2048 ->
    try uri_string:parse(Uri) of
        #{scheme := <<"https">>, host := Host} = Parts when Host =/= <<>> ->
            case maps:is_key(userinfo, Parts) orelse maps:is_key(fragment, Parts) of
                true -> error;
                false -> {ok, {string:lowercase(Host), maps:get(port, Parts, 443)}}
            end;
        _ ->
            error
    catch
        _:_ -> error
    end;
https_origin(_) ->
    error.

%% @doc HUMAN-SSO-01 池化签发入口（handler 用）。
%% Uid 为 Human JWT 身份（auth_ds:current_uid/1）；非正整数按未认证拒绝。
-spec issue_code(integer(), map()) ->
    {ok, #{binary() := binary() | integer()}} | {error, {integer(), binary()}}.
issue_code(Uid, Params) ->
    case elib_pg:with_tx(fun(Conn) -> issue_code_tx(Conn, Uid, Params) end) of
        {rollback, Reason} ->
            ?ERROR_LOG([enterprise_oa_sso_issue_db_error, #{reason => Reason}]),
            {error, {?ERR_INTERNAL_SERVER_ERROR, <<"签发失败"/utf8>>}};
        Result ->
            Result
    end.

%% @doc HUMAN-SSO-01 事务内签发（测试可直连连接复用）。
-spec issue_code_tx(any(), integer(), map()) ->
    {ok, #{binary() := binary() | integer()}} | {error, {integer(), binary()}}.
issue_code_tx(Conn, Uid, Params) when is_integer(Uid), Uid > 0 ->
    case validate_issue_params(Params) of
        {ok, ApplicationKey, RedirectUri, Nonce} ->
            issue_validated(Conn, Uid, ApplicationKey, RedirectUri, Nonce);
        {error, invalid_param} ->
            {error, {?ERR_INVALID_PARAM, <<"参数不合法"/utf8>>}}
    end;
issue_code_tx(_Conn, _Uid, _Params) ->
    %% 未认证（无 Human JWT 上下文）：401 真实状态码（合同 §3.5）
    {error, {?ERR_UNAUTHORIZED, <<"未登录，请先登录"/utf8>>}}.

%% @doc INT-14 池化交换入口（handler 用）。Ctx 为 A2 认证链产物
%% （#{organization_id, application_id, ...}，atom 键）。
-spec exchange(map(), map()) ->
    {ok, map()} | {error, binary()}.
exchange(Ctx, Params) ->
    Fun = fun(Conn) ->
        case exchange_authorized_tx(Conn, Ctx, Params) of
            %% 返回错误值不会自动回滚，包含 REQUIRED_AUDIT 软失败。
            {error, Code} ->
                ?WARN_LOG([
                    enterprise_oa_sso_exchange_rollback, #{code => Code}
                ]),
                erlang:throw({rollback, Code});
            Result ->
                Result
        end
    end,
    case elib_pg:with_tx(Fun) of
        {rollback, Code} ->
            {error, Code};
        Result ->
            Result
    end.

%% 中间件认证事务已结束，在业务事务内复核并锁定权限事实。
exchange_authorized_tx(Conn, Ctx, Params) ->
    case lock_authority_tx(Conn, Ctx) of
        ok ->
            case exchange_tx(Conn, Ctx, Params) of
                {ok, Payload} ->
                    %% code/identity 等锁可以跨凭证截止时间，成功前再次复核。
                    case credential_current_tx(Conn, maps:get(credential_id, Ctx)) of
                        ok ->
                            {ok, Payload};
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

lock_authority_tx(
    Conn,
    #{
        organization_id := OrgId, application_id := AppId, credential_id := CredId
    } = Ctx
) ->
    case
        enterprise_application_credential_repo:authority_for_share_tx(
            Conn, maps:get(application_organization_id, Ctx, OrgId), AppId, CredId
        )
    of
        {ok, #{<<"credential_status">> := Status}} when Status =/= <<"active">> ->
            {error, <<"invalid_credential">>};
        {ok, Row} ->
            case credential_current_tx(Conn, CredId) of
                ok ->
                    case authority_status(Row) of
                        ok ->
                            ok;
                        StatusError ->
                            StatusError
                    end;
                Error ->
                    Error
            end;
        {error, not_found} ->
            {error, <<"invalid_credential">>};
        {error, _} ->
            {error, <<"internal_error">>}
    end;
lock_authority_tx(_, _) ->
    {error, <<"internal_error">>}.

authority_status(#{<<"application_status">> := Status}) when Status =/= <<"active">> ->
    {error, <<"application_disabled">>};
authority_status(_) ->
    %% organization_status 不再是拒绝条件（定位修订：org 是业务对象，
    %% 非授权边界）；authority_for_share_tx 的 org JOIN 仅作寻址。
    ok.

credential_current_tx(Conn, CredId) ->
    case enterprise_application_credential_repo:expired_tx(Conn, CredId) of
        {ok, false} -> ok;
        {ok, true} -> {error, <<"credential_expired">>};
        {error, _} -> {error, <<"internal_error">>}
    end.

%% @doc INT-14 事务内交换（测试直连用；任何错误时调用方须回滚）。
-spec exchange_tx(any(), map(), map()) -> {ok, map()} | {error, binary()}.
exchange_tx(Conn, Ctx, Params) ->
    OrgId = maps:get(organization_id, Ctx, undefined),
    AppId = maps:get(application_id, Ctx, undefined),
    case
        ((is_integer(OrgId) andalso OrgId > 0) orelse OrgId =:= null) andalso
            is_integer(AppId) andalso AppId > 0
    of
        false ->
            %% 认证产物缺失属程序错误（中间件未注入 ctx）：fail-closed
            ?ERROR_LOG([enterprise_oa_sso_exchange_ctx_invalid, #{}]),
            {error, <<"internal_error">>};
        true ->
            %% INT-BE-03 冻结政策 INT-14=REQUIRED_AUDIT：交换成功审计与 code
            %% CAS 消费同事务（审计失败 → internal_error → 调用方回滚 → code
            %% 停留 issued）；重放已消费 code 走统一 404 拒绝、无业务变更 →
            %% 无审计行，幂等由 CAS 保证（政策冻结条款）。
            case exchange_authenticated(Conn, AppId, Params) of
                {ok, Payload} ->
                    case audit_exchange(Conn, Ctx, Payload) of
                        ok -> {ok, Payload};
                        {error, _Reason} -> {error, <<"internal_error">>}
                    end;
                {error, _} = Err ->
                    Err
            end
    end.

%% @doc INT-BE-03 冻结政策 REQUIRED_AUDIT 审计接线（本模块私有）：调用
%% enterprise_audit_event_repo:append_tx/3（append-only 真源的唯一 repo 入口）
%% 在调用方事务内落审计行；actor_role 恒为 enterprise_application；
%% detail 只放结构化摘要（application/correlation/user_id），无 code 原文、
%% 无 nonce 原文、无 digest、无 Authorization。
-spec audit_exchange(any(), map(), map()) -> ok | {error, term()}.
audit_exchange(Conn, Ctx, Payload) ->
    Detail = #{
        <<"user_id">> => maps:get(<<"user_id">>, Payload, null),
        <<"origin_application_id">> => maps:get(application_id, Ctx, null),
        <<"correlation_id">> => maps:get(correlation_id, Ctx, null)
    },
    case
        enterprise_audit_event_repo:append_tx(Conn, maps:get(organization_id, Ctx), #{
            resource_type => <<"enterprise_oa_sso">>,
            resource_id => null,
            action => <<"oa.sso.exchanged">>,
            actor_user_id => maps:get(<<"user_id">>, Payload, null),
            actor_role => <<"enterprise_application">>,
            detail => Detail
        })
    of
        {ok, _AuditId} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc HUMAN-SSO-01 请求字段语法校验（合同 §3.2）。
%% 通过返回 {ok, ApplicationKey, RedirectUri, Nonce}；任一字段非法返回
%% {error, invalid_param}（handler 面映射 400 ?ERR_INVALID_PARAM）。
-spec validate_issue_params(map()) ->
    {ok, binary(), binary(), binary()} | {error, invalid_param}.
validate_issue_params(Params) when is_map(Params) ->
    ApplicationKey = maps:get(<<"application_key">>, Params, undefined),
    RedirectUri = maps:get(<<"redirect_uri">>, Params, undefined),
    Nonce = maps:get(<<"nonce">>, Params, undefined),
    %% organization_id 容忍并忽略（定位修订 §4.1：旧客户端兼容）——
    %% 不校验、不参与判定、不回显。
    case
        valid_application_key(ApplicationKey) andalso
            valid_redirect_uri(RedirectUri) andalso
            valid_nonce(Nonce)
    of
        true ->
            {ok, ApplicationKey, RedirectUri, Nonce};
        false ->
            {error, invalid_param}
    end;
validate_issue_params(_) ->
    {error, invalid_param}.

%% @doc INT-14 请求字段语法校验（合同 §4.2）。
%% 语法与语义分离：语法非法 invalid_request；格式合法但绑定不匹配归
%% resource_not_found（exchange 主流程）。
-spec validate_exchange_params(map()) ->
    {ok, binary(), binary(), binary()} | {error, invalid_request}.
validate_exchange_params(Params) when is_map(Params) ->
    Code = maps:get(<<"code">>, Params, undefined),
    RedirectUri = maps:get(<<"redirect_uri">>, Params, undefined),
    Nonce = maps:get(<<"nonce">>, Params, undefined),
    case valid_code(Code) andalso valid_redirect_uri(RedirectUri) andalso valid_nonce(Nonce) of
        true ->
            {ok, Code, RedirectUri, Nonce};
        false ->
            {error, invalid_request}
    end;
validate_exchange_params(_) ->
    {error, invalid_request}.

%%%===================================================================
%%% HUMAN-SSO-01 签发主流程
%%%===================================================================

-spec issue_validated(any(), integer(), binary(), binary(), binary()) ->
    {ok, map()} | {error, {integer(), binary()}}.
issue_validated(Conn, Uid, ApplicationKey, RedirectUri, Nonce) ->
    case resolve_application_tx(Conn, ApplicationKey) of
        {ok, App} ->
            issue_with_allowlist(Conn, Uid, App, RedirectUri, Nonce);
        {error, Reason} ->
            issue_error(Reason)
    end.

-spec issue_with_allowlist(any(), integer(), map(), binary(), binary()) ->
    {ok, map()} | {error, {integer(), binary()}}.
issue_with_allowlist(Conn, Uid, App, RedirectUri, Nonce) ->
    Allowlist = maps:get(<<"allowed_redirect_uris">>, App, []),
    case lists:member(RedirectUri, Allowlist) of
        true ->
            %% organization_id 只是 application 的业务归属（code 行冗余落库，
            %% 供审计/兑换回显），不参与判定。
            issue_persist(
                Conn,
                Uid,
                maps:get(<<"organization_id">>, App),
                maps:get(<<"id">>, App),
                RedirectUri,
                Nonce
            );
        false ->
            %% 空 allowlist / 未注册 / 变体（尾斜杠/query/端口）一律拒绝
            {error, {?ERR_INVALID_PARAM, <<"redirect_uri 未注册"/utf8>>}}
    end.

-spec issue_persist(any(), integer(), integer() | null, integer(), binary(), binary()) ->
    {ok, map()} | {error, {integer(), binary()}}.
issue_persist(Conn, Uid, OrgId, AppId, RedirectUri, Nonce) ->
    %% 与兑换复用同一 active Human 行锁，持有到签发事务提交；不恢复 org 门。
    case active_human_tx(Conn, Uid) of
        {ok, [_ | _]} ->
            issue_active_persist(Conn, Uid, OrgId, AppId, RedirectUri, Nonce);
        {ok, _} ->
            {error, {?ERR_FORBIDDEN, <<"账号不可用"/utf8>>}};
        {error, _} ->
            {error, {?ERR_INTERNAL_SERVER_ERROR, <<"签发失败"/utf8>>}}
    end.

issue_active_persist(Conn, Uid, OrgId, AppId, RedirectUri, Nonce) ->
    Code = generate_code(),
    CodeDigest = enterprise_oa_sso_code_repo:digest_hex(Code),
    NonceDigest = enterprise_oa_sso_code_repo:digest_hex(Nonce),
    ExpiresAt = elib_dt:add(elib_dt:now(), {?CODE_TTL_SECONDS, second}),
    case
        enterprise_oa_sso_code_repo:issue_tx(
            Conn, OrgId, AppId, Uid, CodeDigest, RedirectUri, NonceDigest, ExpiresAt
        )
    of
        {ok, _Row} ->
            %% 明文 code 仅此一次出现（合同 §2）；redirect_uri 原样回显
            {ok, #{
                <<"code">> => Code,
                <<"expires_in">> => ?CODE_TTL_SECONDS,
                <<"redirect_uri">> => RedirectUri
            }};
        {error, Reason} ->
            ?ERROR_LOG([
                enterprise_oa_sso_issue_db_error, #{reason => Reason}
            ]),
            {error, {?ERR_INTERNAL_SERVER_ERROR, <<"签发失败"/utf8>>}}
    end.

%% application_key 解析（定位修订 §4.1）：key 实例内全局唯一
%% （迁移 00000171 uq_ea_key_global），按 key 直取。org active /
%% org 成员关系不再是判定条件；org 归属仅随行携带（业务归属）。
-spec resolve_application_tx(any(), binary()) ->
    {ok, map()} | {error, not_found | term()}.
resolve_application_tx(Conn, ApplicationKey) ->
    case applications_by_key_tx(Conn, ApplicationKey) of
        {ok, [App]} ->
            case maps:get(<<"status">>, App) =:= <<"active">> of
                true -> {ok, App};
                false -> {error, not_found}
            end;
        {ok, []} ->
            {error, not_found};
        {ok, _Multiple} ->
            %% 前置迁移校验保证全局唯一；多行属数据异常，fail-closed
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

%% 跨 Org 按 key 取 application 行（只读内联；列集与 A1 repo ?COLUMNS 对齐，
%% text[] 由 epgsql 解码为 [binary()]）。
-spec applications_by_key_tx(any(), binary()) -> {ok, [map()]} | {error, term()}.
applications_by_key_tx(Conn, ApplicationKey) ->
    Sql = <<
        "SELECT id, organization_id, principal_user_id, application_key, name, status,"
        " allowed_redirect_uris, created_at, updated_at"
        " FROM enterprise_application WHERE application_key = $1"
    >>,
    case elib_pg:query(Conn, Sql, [ApplicationKey]) of
        {ok, Rows} when is_list(Rows) ->
            {ok, Rows};
        {ok, _Other} ->
            {ok, []};
        {error, Reason} ->
            {error, Reason}
    end.

%% organization 活性检查已随定位修订移除（org 是业务对象，非授权边界）。
%% business attribution 仍保留在 application 行上，供审计/兑换回显。

%%%===================================================================
%%% INT-14 交换主流程
%%%===================================================================

-spec exchange_authenticated(any(), integer(), map()) ->
    {ok, map()} | {error, binary()}.
exchange_authenticated(Conn, AppId, Params) ->
    case validate_exchange_params(Params) of
        {error, invalid_request} ->
            {error, <<"invalid_request">>};
        {ok, Code, RedirectUri, Nonce} ->
            CodeDigest = enterprise_oa_sso_code_repo:digest_hex(Code),
            exchange_with_digest(Conn, AppId, CodeDigest, RedirectUri, Nonce)
    end.

-spec exchange_with_digest(any(), integer(), binary(), binary(), binary()) ->
    {ok, map()} | {error, binary()}.
exchange_with_digest(Conn, AppId, CodeDigest, RedirectUri, Nonce) ->
    case enterprise_oa_sso_code_repo:find_by_digest_tx(Conn, CodeDigest) of
        {error, not_found} ->
            %% 统一不透明拒绝（NEG-01）：未知 code 无存在性 oracle
            {error, <<"resource_not_found">>};
        {ok, Row} ->
            exchange_with_row(Conn, AppId, Row, RedirectUri, Nonce);
        {error, Reason} ->
            ?ERROR_LOG([
                enterprise_oa_sso_exchange_db_error, #{reason => Reason}
            ]),
            {error, <<"internal_error">>}
    end.

%% 绑定校验 -> CAS -> identity 解析（定位修订 §4.3：绑定四元组
%% app/user/redirect_uri/nonce；org 不参与——任一绑定失败一律
%% resource_not_found，不区分具体原因）。user 绑定经 CAS 后的
%% consume_and_resolve 流程隐式成立（code 行 user_id 即绑定值）。
-spec exchange_with_row(any(), integer(), map(), binary(), binary()) ->
    {ok, map()} | {error, binary()}.
exchange_with_row(Conn, AppId, Row, RedirectUri, Nonce) ->
    NonceDigest = enterprise_oa_sso_code_repo:digest_hex(Nonce),
    BindingOk =
        maps:get(<<"expired">>, Row, false) =:= false andalso
            maps:get(<<"consumed_at">>, Row) =:= null andalso
            maps:get(<<"application_id">>, Row) =:= AppId andalso
            maps:get(<<"redirect_uri">>, Row) =:= RedirectUri andalso
            constant_time_eq(NonceDigest, maps:get(<<"nonce_digest">>, Row)),
    case BindingOk of
        false ->
            {error, <<"resource_not_found">>};
        true ->
            consume_and_resolve(Conn, Row)
    end.

%% CAS 单次消费 + 同事务 identity 解析（合同 §5：解析失败整体回滚）。
-spec consume_and_resolve(any(), map()) -> {ok, map()} | {error, binary()}.
consume_and_resolve(Conn, Row) ->
    case enterprise_oa_sso_code_repo:consume_tx(Conn, maps:get(<<"code_digest">>, Row)) of
        {ok, ConsumedRow} ->
            resolve_identity(Conn, ConsumedRow);
        {error, already_consumed} ->
            %% 重放（NEG-03）/ 并发输家（NEG-04）：拒绝而非回放
            {error, <<"resource_not_found">>};
        {error, expired} ->
            {error, <<"resource_not_found">>};
        {error, not_found} ->
            {error, <<"resource_not_found">>};
        {error, Reason} ->
            ?ERROR_LOG([
                enterprise_oa_sso_consume_db_error, #{reason => Reason}
            ]),
            {error, <<"internal_error">>}
    end.

%% identity 解析（同事务，定位修订 §4.4 mapping 可选化）：
%%   ① 登记 mapping 命中（active + 目标仍是 active Human 账号——
%%      account_type=0 + user.status=1，账号级事实；org 成员关系不再
%%      联查，org 是业务对象非授权边界）→ 外部员工号原样返回；
%%   ② mapping 未登记（可选别名缺省）→ 账号级活性复核通过后回退
%%      external_user_id = user_id 十进制字符串（ADR §4.4）；
%%   ③ 账号非 active Human（停用/非人类）→ identity_not_mapped 拒绝，
%%      调用方回滚（code 停留 issued）——账号级 kill-switch：撤销先取
%%      锁时等待在途交换并重检最新行，拒绝旧快照（INT-BE-02 语义保留）。
%% 共享锁持有到提交。
-spec resolve_identity(any(), map()) -> {ok, map()} | {error, binary()}.
resolve_identity(Conn, ConsumedRow) ->
    AppId = maps:get(<<"application_id">>, ConsumedRow),
    UserId = maps:get(<<"user_id">>, ConsumedRow),
    MappingSql = <<
        "SELECT eei.external_user_id"
        " FROM enterprise_external_identity eei"
        " JOIN \"user\" u ON u.id = eei.user_id"
        "  AND u.account_type = 0 AND u.status = 1"
        " WHERE eei.application_id = $1"
        "   AND eei.user_id = $2 AND eei.status = 'active'"
        " LIMIT 1 FOR SHARE OF u, eei"
    >>,
    case elib_pg:query(Conn, MappingSql, [AppId, UserId]) of
        {ok, [#{<<"external_user_id">> := ExternalUserId} | _]} ->
            {ok, exchange_payload(ConsumedRow, ExternalUserId)};
        {ok, _} ->
            fallback_identity(Conn, ConsumedRow, UserId);
        {error, Reason} ->
            ?ERROR_LOG([
                enterprise_oa_sso_identity_db_error, #{reason => Reason}
            ]),
            {error, <<"internal_error">>}
    end.

%% mapping 缺省回退（ADR §4.4）：账号仍是 active Human 才放行——user 行
%% FOR SHARE 使账号撤销等待在途交换（kill-switch 行锁语义）；回退形态为
%% TSID int64 的十进制字符串（与 external_user_id text 列同型，OA 侧按
%% 字符串处理即可与登记形态统一）。
-spec fallback_identity(any(), map(), integer()) -> {ok, map()} | {error, binary()}.
fallback_identity(Conn, ConsumedRow, UserId) ->
    case active_human_tx(Conn, UserId) of
        {ok, [_ | _]} ->
            {ok, exchange_payload(ConsumedRow, integer_to_binary(UserId))};
        {ok, _} ->
            {error, <<"identity_not_mapped">>};
        {error, Reason} ->
            ?ERROR_LOG([
                enterprise_oa_sso_identity_db_error, #{reason => Reason}
            ]),
            {error, <<"internal_error">>}
    end.

%% 仅复用原 fallback 身份查询；FOR SHARE 与停用 UPDATE 互斥到同事务提交。
-spec active_human_tx(any(), integer()) -> {ok, [map()]} | {error, term()}.
active_human_tx(Conn, UserId) ->
    AccountSql = <<
        "SELECT 1 FROM \"user\" u WHERE u.id = $1"
        " AND u.account_type = 0 AND u.status = 1 LIMIT 1 FOR SHARE OF u"
    >>,
    elib_pg:query(Conn, AccountSql, [UserId]).

%% 成功 payload（合同 §4.3）：五个字段、永不含 IMBoy 凭证（R-6/NEG-14）；
%% TSID int64 以 JSON integer 输出（INV-8）；consumed_at 归一 ISO-8601
%% UTC 毫秒（CONVENTIONS §2）。
-spec exchange_payload(map(), binary()) -> map().
exchange_payload(ConsumedRow, ExternalUserId) ->
    #{
        <<"organization_id">> => maps:get(<<"organization_id">>, ConsumedRow),
        <<"application_id">> => maps:get(<<"application_id">>, ConsumedRow),
        <<"user_id">> => maps:get(<<"user_id">>, ConsumedRow),
        <<"external_user_id">> => ExternalUserId,
        <<"consumed_at">> => normalize_ms_utc(maps:get(<<"consumed_at">>, ConsumedRow))
    }.

-spec normalize_ms_utc(binary() | null) -> binary() | null.
normalize_ms_utc(null) ->
    null;
normalize_ms_utc(Bin) when is_binary(Bin) ->
    case elib_dt:rfc3339_to(Bin, millisecond) of
        Ms when is_integer(Ms) ->
            elib_dt:to_rfc3339(Ms, millisecond, "Z");
        _ ->
            Bin
    end.

%%%===================================================================
%%% 校验与工具
%%%===================================================================

%% application_key：8..128 可打印 ASCII（合同 §3.2）。
-spec valid_application_key(binary() | term()) -> boolean().
valid_application_key(Key) when is_binary(Key) ->
    Size = byte_size(Key),
    Size >= 8 andalso Size =< 128 andalso printable_ascii(Key);
valid_application_key(_) ->
    false.

%% redirect_uri：HTTPS、非空 host、禁 fragment、长度 ≤2048（合同 §3.2/§4.2）。
%% 只做形态校验；是否已注册由 allowlist exact-match 判定。
-spec valid_redirect_uri(binary() | term()) -> boolean().
valid_redirect_uri(Uri) when is_binary(Uri) ->
    Size = byte_size(Uri),
    Size > 8 andalso Size =< 2048 andalso
        binary:part(Uri, 0, 8) =:= <<"https://">> andalso
        binary:match(Uri, <<"#">>) =:= nomatch andalso
        valid_host_head(Uri);
valid_redirect_uri(_) ->
    false.

%% https:// 后第一个字节必须是 host 首字符（非 / ? # ——空 host 拒绝）。
-spec valid_host_head(binary()) -> boolean().
valid_host_head(Uri) ->
    case binary:at(Uri, 8) of
        C when C =/= $/, C =/= $?, C =/= $# ->
            true;
        _ ->
            false
    end.

%% nonce：16..128 字符 [A-Za-z0-9_-]（合同 §3.2，即 state 参数原值）。
-spec valid_nonce(binary() | term()) -> boolean().
valid_nonce(Nonce) when is_binary(Nonce) ->
    Size = byte_size(Nonce),
    Size >= 16 andalso Size =< 128 andalso urlsafe_token(Nonce);
valid_nonce(_) ->
    false.

%% code：前缀 oa_sso_、整体 8..128、字符集 [A-Za-z0-9_-]（合同 §2/§4.2）。
-spec valid_code(binary() | term()) -> boolean().
%% 注意：?CODE_PREFIX 是 7 字节（oa_sso_），模式中的字面前缀须与宏同步。
valid_code(<<"oa_sso_", Rest/binary>> = Code) when Rest =/= <<>> ->
    Size = byte_size(Code),
    Size >= 8 andalso Size =< 128 andalso urlsafe_token(Code);
valid_code(_) ->
    false.

%% oa_sso_ + 32 字节 CSPRNG 的 base64url（无 padding，43 字符）——熵 ≥256bit。
-spec generate_code() -> binary().
generate_code() ->
    Raw = crypto:strong_rand_bytes(?CODE_RANDOM_BYTES),
    B64 = base64:encode(Raw),
    %% 只剥 padding（32B → base64 44 字符含 1 个 '='；剥 2 会丢数据字符降熵）
    Url = <<<<(urlsafe(C))/binary>> || <<C>> <= B64, C =/= $=>>,
    <<?CODE_PREFIX/binary, Url/binary>>.

urlsafe($+) -> <<"-">>;
urlsafe($/) -> <<"_">>;
urlsafe(C) -> <<C>>.

%% 常时比较（长度不等直接 false；同长走 crypto:hash_equals）。
-spec constant_time_eq(binary(), binary()) -> boolean().
constant_time_eq(A, B) when byte_size(A) =:= byte_size(B) ->
    crypto:hash_equals(A, B);
constant_time_eq(_A, _B) ->
    false.

%% 全串可打印 ASCII（0x20..0x7E）。
-spec printable_ascii(binary()) -> boolean().
printable_ascii(<<>>) ->
    true;
printable_ascii(<<C, Rest/binary>>) when C >= 16#20, C =< 16#7E ->
    printable_ascii(Rest);
printable_ascii(_) ->
    false.

%% 全串 [A-Za-z0-9_-]。
-spec urlsafe_token(binary()) -> boolean().
urlsafe_token(<<>>) ->
    true;
urlsafe_token(<<C, Rest/binary>>) when
    (C >= $a andalso C =< $z);
    (C >= $A andalso C =< $Z);
    (C >= $0 andalso C =< $9);
    C =:= $-;
    C =:= $_
->
    urlsafe_token(Rest);
urlsafe_token(_) ->
    false.

%% issue 内部错误原子 -> human 面整数信封。
%% forbidden 分支已随成员收敛移除（定位修订：org 成员关系不参与判定）。
-spec issue_error(not_found | term()) ->
    {error, {integer(), binary()}}.
issue_error(not_found) ->
    {error, {?ERR_NOT_FOUND, <<"企业应用不存在或已停用"/utf8>>}};
issue_error(Reason) ->
    ?ERROR_LOG([enterprise_oa_sso_issue_db_error, #{reason => Reason}]),
    {error, {?ERR_INTERNAL_SERVER_ERROR, <<"签发失败"/utf8>>}}.
