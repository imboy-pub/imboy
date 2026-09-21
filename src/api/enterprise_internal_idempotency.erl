-module(enterprise_internal_idempotency).

%%%
% enterprise_internal_idempotency 是 internal API 幂等中间件（EPGZ-02，
% plan-gz §6 / manifest INV-7）。
%
% 语义（INV-7，INT-14 single_use_code 豁免由路由表 idempotency 字段表达）：
%   * mutation 必带非空 Idempotency-Key（存在性检查在
%     enterprise_internal_auth:idempotency_gate/3，缺失 → invalid_request）；
%   * begin_tx：同 key 同 body（request_digest 一致）
%       - 首次 → {ok, inserted}（执行业务，成功后 complete_tx 回填）；
%       - 已有结果（response_code 已回填）→ {ok, replay, #{resource_id,
%         response_code}}——上层以存储的 response_code + resource_id 重放
%         原结果；
%       - 已登记但尚无结果（并发在途/上次执行中断）→ {ok, pending}
%         （上层按 409/稍后重试处理，不重复执行业务）；
%   * 同 key 异 body → {error, digest_conflict}（上层 409
%     idempotency_conflict，conflict_code/0 提供映射）。
%
% request_digest = SHA-256(method + " " + path + "\n" + body)——同一资源
% 路径与请求体的规范化指纹；跨路由同 key 天然 digest_conflict。
%
% 已知 repo 缺口（记录于 EPGZ-02 checkpoint）：enterprise_internal_idempotency
% 表未存响应体，replay 只能回读 {resource_id, response_code}，完整响应体
% 由 handler 依据 resource_id 重建；是否补 response_body 列由 A0 裁决。
%%%

-export([
    request_digest/3,
    begin_tx/5,
    complete_tx/6,
    required/1,
    ttl_seconds/0,
    conflict_code/0
]).

-define(TTL_ENV_KEY, enterprise_internal_idempotency_ttl_seconds).
-define(DEFAULT_TTL_SECONDS, 86400).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc 请求体规范化指纹（64 hex）。Body 为 binary 原样；map/list 先 JSON 编码。
-spec request_digest(binary(), binary(), binary() | map() | list()) -> binary().
request_digest(Method, Path, Body) when
    is_binary(Method), is_binary(Path)
->
    BodyBin =
        case Body of
            B when is_binary(B) -> B;
            T -> jsone:encode(T)
        end,
    binary:encode_hex(
        crypto:hash(sha256, <<Method/binary, " ", Path/binary, "\n", BodyBin/binary>>),
        lowercase
    ).

%% @doc 幂等前置：登记/重放判定（语义见 moduledoc）。
%% Ctx 需含 organization_id 与 application_id（认证产物）。
-spec begin_tx(any(), map(), binary(), binary(), binary()) ->
    {ok, inserted | pending}
    | {ok, replay, #{resource_id => integer() | null, response_code => integer() | null}}
    | {error, digest_conflict | term()}.
begin_tx(Conn, Ctx, ResourceType, Key, Digest) when
    is_binary(ResourceType), is_binary(Key), Key =/= <<>>, is_binary(Digest)
->
    OrgId = maps:get(organization_id, Ctx),
    AppId = maps:get(application_id, Ctx),
    ExpiresAt = elib_dt:add(elib_dt:now(), {ttl_seconds(), second}),
    case
        enterprise_internal_idempotency_repo:record_tx(
            Conn, OrgId, AppId, Key, ResourceType, Digest, ExpiresAt
        )
    of
        {ok, inserted, _Row} ->
            {ok, inserted};
        {ok, existing, Row} ->
            ResponseCode = maps:get(<<"response_code">>, Row, null),
            ResourceId = maps:get(<<"resource_id">>, Row, null),
            case ResponseCode of
                null ->
                    {ok, pending};
                _ ->
                    {ok, replay, #{resource_id => ResourceId, response_code => ResponseCode}}
            end;
        {error, _} = Err ->
            Err
    end.

%% @doc 业务执行成功后回填结果（仅允许一次；并发第二执行者得 already_claimed）。
-spec complete_tx(any(), map(), binary(), binary(), integer(), integer()) ->
    ok | {error, already_claimed | not_found | term()}.
complete_tx(Conn, Ctx, ResourceType, Key, ResourceId, ResponseCode) ->
    OrgId = maps:get(organization_id, Ctx),
    AppId = maps:get(application_id, Ctx),
    _ = ResourceType,
    enterprise_internal_idempotency_repo:claim_tx(
        Conn, OrgId, AppId, Key, ResourceId, ResponseCode
    ).

%% @doc 路由是否要求 Idempotency-Key（required 才要求；INT-14 豁免）。
-spec required(map()) -> boolean().
required(Route) when is_map(Route) ->
    maps:get(idempotency, Route, not_required) =:= required.

%% @doc 幂等窗口时长（秒；config 可覆写，缺省 24h）。
-spec ttl_seconds() -> pos_integer().
ttl_seconds() ->
    case config_ds:env(imboy, ?TTL_ENV_KEY, ?DEFAULT_TTL_SECONDS) of
        N when is_integer(N), N > 0 -> N;
        _ -> ?DEFAULT_TTL_SECONDS
    end.

%% @doc 同 key 异 body 的 stable 错误码映射（409）。
-spec conflict_code() -> binary().
conflict_code() ->
    <<"idempotency_conflict">>.
