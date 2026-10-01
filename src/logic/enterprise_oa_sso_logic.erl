-module(enterprise_oa_sso_logic).

%%%
% enterprise_oa_sso_logic 是 OA 一次性 SSO code 的签发（HUMAN-SSO-01）与
% 原子交换（INT-14）logic 层（EPGZ-05，plan-gz §7.2 / §6 INT-14）。
%
% 权威合同：docs/architecture/2026-09-21-epgz05-oa-sso-contract.md（W1 冻结）。
%
% HUMAN-SSO-01 POST /api/v1/oa/sso/code（Human JWT，handler=enterprise_oa_sso_handler）
%   判定顺序（合同 §3.4，fail-closed）：
%     JWT 身份 -> 请求字段语法（application_key/redirect_uri/nonce）
%     -> application_key 可解析且 application active（key 仅 Org 内唯一，
%        跨 Org 候选按「请求者是该 Org active member」收敛；收敛后仍多义
%        或零候选按 fail-closed 归 not_found）
%     -> organization active（非 active 与 app 非 active 同族归 not_found）
%     -> 请求者是该 org 的 active Human member（organization_member_repo）
%     -> 已存在 (org, app, user) active identity mapping（fail-early）
%     -> redirect_uri 对 allowed_redirect_uris 逐字节 exact match
%       （lists:member/2，EPGZ-01R；空 allowlist 一律拒绝）
%     -> 签发：oa_sso_ + 32B CSPRNG base64url（≥256bit），60s 固定 TTL
%   重复签发不幂等不互斥：每个 code 独立 TTL、独立单次消费（NEG-H07）。
%
% INT-14 POST /api/internal/v1/oa/sso/exchange（Application Credential，
% A2 认证链前置，handler=enterprise_oa_sso_exchange_handler）
%   判定顺序（合同 §4.4，fail-closed）：
%     A2 认证链（credential/app/org/scope=sso:exchange/rate=internal_sso）
%     -> 请求字段语法（code 前缀/长度/字符集；redirect_uri https/禁 fragment；
%        nonce 长度/字符集）                      => invalid_request
%     -> code_digest 等值查找                      => 未命中 resource_not_found
%     -> expires_at > now（find_by_digest_tx 的 expired 派生列）
%     -> consumed_at IS NULL                       => 重放 resource_not_found
%     -> 绑定校验（org/app/redirect exact/nonce digest 常时比较）
%                                                  => 任一不符 resource_not_found
%       （统一不透明拒绝：不给猜测 code 的对端提供存在性/生命周期/绑定 oracle）
%     -> CAS 单次消费（enterprise_oa_sso_code_repo:consume_tx/2）
%     -> 同事务解析 identity mapping（active + 目标仍为 active Human member）
%                                                  => 缺失 identity_not_mapped
%   错误形态：stable 二进制码（enterprise_internal_error 信封承载）；
%   human 面错误形态：{error, {Code :: integer(), Msg :: binary()}}（整数信封）。
%
% 交换失败回滚契约（合同 §5 / REQUIRED_AUDIT）：CAS、identity 与审计同事务；
% 任一步失败时调用方必须使事务回滚（code 停留 issued，TTL 内修复后可重试）。
% 池化入口 exchange/2 以 throw({rollback, ...}) 强制回滚；直接使用
% exchange_tx/3 的调用方（测试）须以 SAVEPOINT 回滚到消费前。
%
% redaction 红线（合同 §0/§2）：code/nonce 明文零落库、零日志——日志只含
% stage 与错误码，不含 code/nonce/Authorization 值。
%%%

-export([
    issue_code/2,
    issue_code_tx/3,
    entries/2,
    entries_tx/3,
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

%% 配置发现按显式企业限定；无权或无配置统一空列表，不枚举其他企业。
entries(Uid, _OrgId) when not is_integer(Uid); Uid =< 0 ->
    {error, {?ERR_UNAUTHORIZED, <<"未登录，请先登录"/utf8>>}};
entries(Uid, OrgId) when is_integer(OrgId), OrgId > 0, OrgId =< 9223372036854775807 ->
    case elib_pg:with_tx(fun(Conn) -> entries_tx(Conn, Uid, OrgId) end) of
        {ok, _} = Result -> Result;
        _ -> {error, {?ERR_INTERNAL_SERVER_ERROR, <<"配置读取失败"/utf8>>}}
    end;
entries(_, _) ->
    {error, {?ERR_INVALID_PARAM, <<"organization_id 必须是正整数"/utf8>>}}.

entries_tx(Conn, Uid, OrgId) ->
    case enterprise_application_repo:workbench_entries_tx(Conn, OrgId, Uid) of
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
            {true, #{
                <<"kind">> => <<"oa">>,
                <<"organization_id">> => maps:get(<<"organization_id">>, Row),
                <<"application_id">> => maps:get(<<"application_id">>, Row),
                <<"application_key">> => Key,
                <<"label">> => unicode:characters_to_binary(lists:sublist(Name, 64)),
                <<"redirect_uri">> => Redirect
            }};
        false ->
            false
    end;
entry(_) ->
    false.

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
            issue_validated(
                Conn,
                Uid,
                ApplicationKey,
                RedirectUri,
                Nonce,
                maps:get(<<"organization_id">>, Params, undefined)
            );
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
        case exchange_tx(Conn, Ctx, Params) of
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

%% @doc INT-14 事务内交换（测试直连用；任何错误时调用方须回滚）。
-spec exchange_tx(any(), map(), map()) -> {ok, map()} | {error, binary()}.
exchange_tx(Conn, Ctx, Params) ->
    OrgId = maps:get(organization_id, Ctx, undefined),
    AppId = maps:get(application_id, Ctx, undefined),
    case is_integer(OrgId) andalso OrgId > 0 andalso is_integer(AppId) andalso AppId > 0 of
        false ->
            %% 认证产物缺失属程序错误（中间件未注入 ctx）：fail-closed
            ?ERROR_LOG([enterprise_oa_sso_exchange_ctx_invalid, #{}]),
            {error, <<"internal_error">>};
        true ->
            %% INT-BE-03 冻结政策 INT-14=REQUIRED_AUDIT：交换成功审计与 code
            %% CAS 消费同事务（审计失败 → internal_error → 调用方回滚 → code
            %% 停留 issued）；重放已消费 code 走统一 404 拒绝、无业务变更 →
            %% 无审计行，幂等由 CAS 保证（政策冻结条款）。
            case exchange_authenticated(Conn, OrgId, AppId, Params) of
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
    case
        valid_application_key(ApplicationKey) andalso
            valid_redirect_uri(RedirectUri) andalso
            valid_nonce(Nonce) andalso valid_issue_organization(Params)
    of
        true ->
            {ok, ApplicationKey, RedirectUri, Nonce};
        false ->
            {error, invalid_param}
    end;
validate_issue_params(_) ->
    {error, invalid_param}.

valid_issue_organization(Params) ->
    case maps:find(<<"organization_id">>, Params) of
        error ->
            true;
        {ok, OrgId} ->
            is_integer(OrgId) andalso OrgId > 0 andalso
                OrgId =< 9223372036854775807
    end.

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

-spec issue_validated(any(), integer(), binary(), binary(), binary(), integer() | undefined) ->
    {ok, map()} | {error, {integer(), binary()}}.
issue_validated(Conn, Uid, ApplicationKey, RedirectUri, Nonce, ExpectedOrgId) ->
    case resolve_application_tx(Conn, Uid, ApplicationKey, ExpectedOrgId) of
        {ok, App} ->
            AppId = maps:get(<<"id">>, App),
            OrgId = maps:get(<<"organization_id">>, App),
            case has_active_identity_tx(Conn, OrgId, AppId, Uid) of
                true ->
                    issue_with_allowlist(
                        Conn, Uid, App, OrgId, AppId, RedirectUri, Nonce
                    );
                false ->
                    %% fail-early（NEG-H04）：签发即拦截无 active mapping
                    {error, {?ERR_FORBIDDEN, <<"未绑定企业应用身份"/utf8>>}}
            end;
        {error, Reason} ->
            issue_error(Reason)
    end.

-spec issue_with_allowlist(
    any(), integer(), map(), integer(), integer(), binary(), binary()
) ->
    {ok, map()} | {error, {integer(), binary()}}.
issue_with_allowlist(Conn, Uid, App, OrgId, AppId, RedirectUri, Nonce) ->
    Allowlist = maps:get(<<"allowed_redirect_uris">>, App, []),
    case lists:member(RedirectUri, Allowlist) of
        true ->
            issue_persist(Conn, Uid, OrgId, AppId, RedirectUri, Nonce);
        false ->
            %% 空 allowlist / 未注册 / 变体（尾斜杠/query/端口）一律拒绝
            {error, {?ERR_INVALID_PARAM, <<"redirect_uri 未注册"/utf8>>}}
    end.

-spec issue_persist(any(), integer(), integer(), integer(), binary(), binary()) ->
    {ok, map()} | {error, {integer(), binary()}}.
issue_persist(Conn, Uid, OrgId, AppId, RedirectUri, Nonce) ->
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

%% application_key 解析（合同 §3.4 前两步 + 归一）：
%%   1. 跨 Org 取同 key 的 active application 候选（key 仅 Org 内唯一，
%%      enterprise_application_repo 无跨 Org find——repo 缺口，镜像
%%      enterprise_internal_auth 内联只读先例）；
%%   2. 候选按「请求者是该 Org active member」收敛：
%%      零候选 = 非成员（forbidden，NEG-H03 区分于 404）；
%%      恰一 = 命中；多义 = fail-closed not_found（不提供多 Org oracle）；
%%   3. 命中后校验 organization active（非 active 与 app 不可用同族 404）。
-spec resolve_application_tx(any(), integer(), binary(), integer() | undefined) ->
    {ok, map()} | {error, not_found | forbidden | term()}.
resolve_application_tx(Conn, Uid, ApplicationKey, ExpectedOrgId) ->
    case applications_by_key_tx(Conn, ApplicationKey) of
        {ok, []} ->
            {error, not_found};
        {ok, Rows} ->
            Active = [
                R
             || R <- Rows,
                maps:get(<<"status">>, R) =:= <<"active">>,
                ExpectedOrgId =:= undefined orelse
                    maps:get(<<"organization_id">>, R) =:= ExpectedOrgId
            ],
            case Active of
                [] ->
                    {error, not_found};
                _ ->
                    narrow_by_membership(Conn, Uid, Active)
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec narrow_by_membership(any(), integer(), [map()]) ->
    {ok, map()} | {error, not_found | forbidden}.
narrow_by_membership(Conn, Uid, ActiveApps) ->
    MemberApps = lists:filtermap(
        fun(App) ->
            OrgId = maps:get(<<"organization_id">>, App),
            case organization_member_repo:find_active_tx(Conn, OrgId, Uid, <<"user_id">>) of
                {ok, _} -> {true, App};
                _ -> false
            end
        end,
        ActiveApps
    ),
    case MemberApps of
        [] ->
            {error, forbidden};
        [One] ->
            case organization_active_tx(Conn, maps:get(<<"organization_id">>, One)) of
                true -> {ok, One};
                false -> {error, not_found}
            end;
        _Multiple ->
            %% 同 key 多 Org 且请求者均为 active member：fail-closed 拒绝
            {error, not_found}
    end.

%% 跨 Org 按 key 取 application 行（只读内联；列集与 A1 repo ?COLUMNS 对齐，
%% text[] 由 epgsql 解码为 [binary()]）。
-spec applications_by_key_tx(any(), binary()) -> {ok, [map()]} | {error, term()}.
applications_by_key_tx(Conn, ApplicationKey) ->
    Sql = <<
        "SELECT id, organization_id, principal_user_id, application_key, name, status,"
        " allowed_scopes, allowed_redirect_uris, created_at, updated_at"
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

%% organization 活性（只读内联；organization_repo 未导出普通事务内 find，
%% find_for_update_tx 行锁会拖垮签发路径——同 EPGZ-02 repo 缺口口径）。
-spec organization_active_tx(any(), integer()) -> boolean().
organization_active_tx(Conn, OrgId) ->
    case
        elib_pg:query(
            Conn, <<"SELECT status FROM organization WHERE id = $1 LIMIT 1">>, [OrgId]
        )
    of
        {ok, [#{<<"status">> := <<"active">>} | _]} ->
            true;
        _ ->
            false
    end.

%% 签发侧 fail-early 检查（合同 §3.4 第 4 步，NEG-H04）：(org, app, user)
%% 是否存在 active identity mapping。exchange 侧运行时复核在
%% resolve_identity/2（含 active Human member 联查）。
-spec has_active_identity_tx(any(), integer(), integer(), integer()) -> boolean().
has_active_identity_tx(Conn, OrgId, AppId, Uid) ->
    Sql = <<
        "SELECT 1 FROM enterprise_external_identity"
        " WHERE organization_id = $1 AND application_id = $2 AND user_id = $3"
        "   AND status = 'active' LIMIT 1"
    >>,
    case elib_pg:query(Conn, Sql, [OrgId, AppId, Uid]) of
        {ok, [_ | _]} ->
            true;
        _ ->
            false
    end.

%%%===================================================================
%%% INT-14 交换主流程
%%%===================================================================

-spec exchange_authenticated(any(), integer(), integer(), map()) ->
    {ok, map()} | {error, binary()}.
exchange_authenticated(Conn, OrgId, AppId, Params) ->
    case validate_exchange_params(Params) of
        {error, invalid_request} ->
            {error, <<"invalid_request">>};
        {ok, Code, RedirectUri, Nonce} ->
            CodeDigest = enterprise_oa_sso_code_repo:digest_hex(Code),
            exchange_with_digest(Conn, OrgId, AppId, CodeDigest, RedirectUri, Nonce)
    end.

-spec exchange_with_digest(any(), integer(), integer(), binary(), binary(), binary()) ->
    {ok, map()} | {error, binary()}.
exchange_with_digest(Conn, OrgId, AppId, CodeDigest, RedirectUri, Nonce) ->
    case enterprise_oa_sso_code_repo:find_by_digest_tx(Conn, CodeDigest) of
        {error, not_found} ->
            %% 统一不透明拒绝（NEG-01）：未知 code 无存在性 oracle
            {error, <<"resource_not_found">>};
        {ok, Row} ->
            exchange_with_row(Conn, OrgId, AppId, Row, RedirectUri, Nonce);
        {error, Reason} ->
            ?ERROR_LOG([
                enterprise_oa_sso_exchange_db_error, #{reason => Reason}
            ]),
            {error, <<"internal_error">>}
    end.

%% 绑定校验 -> CAS -> identity 解析（合同 §4.4 顺序；任一绑定失败一律
%% resource_not_found，不区分具体原因）。
-spec exchange_with_row(any(), integer(), integer(), map(), binary(), binary()) ->
    {ok, map()} | {error, binary()}.
exchange_with_row(Conn, OrgId, AppId, Row, RedirectUri, Nonce) ->
    NonceDigest = enterprise_oa_sso_code_repo:digest_hex(Nonce),
    BindingOk =
        maps:get(<<"expired">>, Row, false) =:= false andalso
            maps:get(<<"consumed_at">>, Row) =:= null andalso
            maps:get(<<"organization_id">>, Row) =:= OrgId andalso
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

%% identity 解析（同事务）：mapping 行 active 且目标仍是同 Org active
%% Human member（organization_member active + account_type=0 + user.status=1，
%% 与 trg_..._member_guard 的 DB 侧写入守卫同语义，运行时复核）。
%% 缺失 => {error, <<"identity_not_mapped">>}——调用方必须回滚事务
%% （池化 exchange/2 已强制；code 停留 issued，TTL 内修复后可重试）。
-spec resolve_identity(any(), map()) -> {ok, map()} | {error, binary()}.
resolve_identity(Conn, ConsumedRow) ->
    OrgId = maps:get(<<"organization_id">>, ConsumedRow),
    AppId = maps:get(<<"application_id">>, ConsumedRow),
    UserId = maps:get(<<"user_id">>, ConsumedRow),
    Sql = <<
        "SELECT eei.external_user_id"
        " FROM enterprise_external_identity eei"
        " JOIN organization_member om"
        "   ON om.organization_id = eei.organization_id AND om.user_id = eei.user_id"
        "  AND om.status = 'active'"
        " JOIN \"user\" u ON u.id = eei.user_id"
        "  AND u.account_type = 0 AND u.status = 1"
        " WHERE eei.organization_id = $1 AND eei.application_id = $2"
        "   AND eei.user_id = $3 AND eei.status = 'active'"
        " LIMIT 1"
    >>,
    case elib_pg:query(Conn, Sql, [OrgId, AppId, UserId]) of
        {ok, [#{<<"external_user_id">> := ExternalUserId} | _]} ->
            {ok, exchange_payload(ConsumedRow, ExternalUserId)};
        {ok, _} ->
            {error, <<"identity_not_mapped">>};
        {error, Reason} ->
            ?ERROR_LOG([
                enterprise_oa_sso_identity_db_error, #{reason => Reason}
            ]),
            {error, <<"internal_error">>}
    end.

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
-spec issue_error(not_found | forbidden | term()) ->
    {error, {integer(), binary()}}.
issue_error(not_found) ->
    {error, {?ERR_NOT_FOUND, <<"企业应用不存在或已停用"/utf8>>}};
issue_error(forbidden) ->
    {error, {?ERR_FORBIDDEN, <<"非该企业活跃成员"/utf8>>}};
issue_error(Reason) ->
    ?ERROR_LOG([enterprise_oa_sso_issue_db_error, #{reason => Reason}]),
    {error, {?ERR_INTERNAL_SERVER_ERROR, <<"签发失败"/utf8>>}}.
