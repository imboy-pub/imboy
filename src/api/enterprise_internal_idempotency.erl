-module(enterprise_internal_idempotency).

%%%
% enterprise_internal_idempotency 是 internal API 幂等中间件（EPGZ-02 起；
% V2.1 §11 合同重做——完整 response snapshot 精确重放）。
%
% 语义（§11 冻结；INT-14 single_use_code 豁免由路由表 idempotency 字段表达）：
%   * Key：Idempotency-Key 必填，1..128 个可打印 ASCII；空/过长/控制字符
%     在认证链 gate 即 400 invalid_request（valid_key/1，auth 接线）；
%   * Digest：SHA-256(method ++ concrete_path ++ "\n" ++ canonical_json_body)，
%     canonical_json_body 严格复用 enterprise_cursor_v2:canonical_json/1
%     （对象 key 按 UTF-8 字节序递归排序——JSON key 顺序不得改变 digest；
%     含 float/不可规范化形态 → {error, non_canonical} → 400 invalid_request）；
%   * Scope：PK (organization_id, application_id, key)；跨 app 同 key 不冲突；
%   * TTL：24h（config enterprise_internal_idempotency_ttl_seconds 可覆写）。
%     过期后下一请求在**同一事务内原子重置** digest/result/expiry 并作为
%     新请求执行（repo record_tx 的 FOR UPDATE 分支），不依赖清理任务；
%   * begin_tx：
%       - 首次/过期重置 → {ok, inserted}（执行业务，complete_tx/7 回填）；
%       - 已完成（response_code 非空）→ {ok, replay, #{resource_id,
%         response_code, response_body}}——上层按存储快照**字节精确**重放
%         原 status + JSON body，并附 replay_header/0（Idempotent-Replayed:
%         true）；不得重执行业务/audit；
%       - 已登记未完成（并发在途且未提交完成）→ {ok, pending}（并发语义：
%         repo 的 INSERT DO NOTHING + FOR UPDATE 使后来者在提交边界排队，
%         正常路径会在锁释放后读到完成行回放；pending 只在「事务提交了
%         insert 却没回填结果」的异常态出现，上层按稍后重试处理）；
%   * 同 key 异 digest（TTL 内）→ {error, digest_conflict}（上层 409
%     idempotency_conflict，conflict_code/0 提供映射）；
%   * complete_tx/7：业务写/audit/response_code/response_body 同一事务回填，
%     仅允许一次（response_code IS NULL 才更新；并发第二执行者
%     already_claimed）。
%%%

-export([
    request_digest/3,
    begin_tx/5,
    complete_tx/7,
    must_complete_tx/7,
    required/1,
    ttl_seconds/0,
    conflict_code/0,
    valid_key/1,
    replay_header/0
]).

-define(TTL_ENV_KEY, enterprise_internal_idempotency_ttl_seconds).
-define(DEFAULT_TTL_SECONDS, 86400).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc 请求规范化指纹（64 hex，§11 冻结公式）：
%% SHA-256(method ++ concrete_path ++ "\n" ++ canonical_json_body)。
%% Body：binary（HTTP 原文 JSON，空 body 视为 {}）/ map / list——统一先走
%% canonical_json（key 字节序排序），再进 digest；不可规范化（非法 JSON /
%% float / 非法形态）→ {error, non_canonical}（上层 400 invalid_request）。
-spec request_digest(binary(), binary(), binary() | map() | list()) ->
    {ok, binary()} | {error, non_canonical}.
request_digest(Method, Path, Body) when
    is_binary(Method), is_binary(Path)
->
    case canonical_body(Body) of
        {ok, Canonical} ->
            {ok,
                binary:encode_hex(
                    crypto:hash(
                        sha256, <<Method/binary, Path/binary, "\n", Canonical/binary>>
                    ),
                    lowercase
                )};
        {error, non_canonical} ->
            {error, non_canonical}
    end.

%% @doc 幂等前置：登记/原子重置/重放判定（语义见 moduledoc）。
%% Ctx 需含 organization_id 与 application_id（认证产物）。
%% replay map 恰含三个键：resource_id（integer() | null）/ response_code /
%% response_body（JSON binary——上层按其原样重放响应体）。
-spec begin_tx(any(), map(), binary(), binary(), binary()) ->
    {ok, inserted | pending}
    | {ok, replay, #{
        resource_id => integer() | null,
        response_code => integer(),
        response_body => binary() | null
    }}
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
            case maps:get(<<"response_code">>, Row, null) of
                null ->
                    {ok, pending};
                Code when is_integer(Code) ->
                    {ok, replay, #{
                        resource_id => maps:get(<<"resource_id">>, Row, null),
                        response_code => Code,
                        response_body => maps:get(<<"response_body">>, Row, null)
                    }}
            end;
        {error, _} = Err ->
            Err
    end.

%% @doc 业务执行成功后回填结果快照（仅允许一次；并发第二执行者得
%% already_claimed）。ResourceId 可为 null（无新资源的 mutation，如配置
%% 替换）；ResponseCode/ResponseBody 是首次响应的 status 与完整 JSON 体
%% ——与业务写、audit 同一事务提交（§11 Completion）。
-spec complete_tx(
    any(), map(), binary(), binary(), integer() | null, integer(), binary() | null
) -> ok | {error, already_claimed | not_found | term()}.
complete_tx(Conn, Ctx, ResourceType, Key, ResourceId, ResponseCode, ResponseBody) ->
    OrgId = maps:get(organization_id, Ctx),
    AppId = maps:get(application_id, Ctx),
    _ = ResourceType,
    enterprise_internal_idempotency_repo:claim_tx(
        Conn, OrgId, AppId, Key, ResourceId, ResponseCode, ResponseBody
    ).

%% Mandatory completion for HTTP callers: never acknowledge a missing replay snapshot.
-spec must_complete_tx(
    any(), map(), binary(), binary(), integer() | null, integer(), binary() | null
) -> ok.
must_complete_tx(Conn, Ctx, Type, Key, Id, Status, Body) ->
    case complete_tx(Conn, Ctx, Type, Key, Id, Status, Body) of
        ok -> ok;
        _ -> throw({rollback, {business_error, <<"internal_error">>}})
    end.

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

%% @doc Idempotency-Key 形态校验（§11）：1..128 个可打印 ASCII
%% （0x20..0x7E，无控制字符）。认证链 gate 在 mutation 上调用，违例
%% 400 invalid_request。
-spec valid_key(undefined | binary()) -> boolean().
valid_key(Key) when is_binary(Key) ->
    byte_size(Key) >= 1 andalso byte_size(Key) =< 128 andalso is_printable_ascii(Key);
valid_key(_) ->
    false.

%% @doc 重放响应头（§11 Replay）：{Idempotent-Replayed, true}——仅重放路径
%% 附加，首次响应不带。
-spec replay_header() -> {binary(), binary()}.
replay_header() ->
    {<<"idempotent-replayed">>, <<"true">>}.

%%%===================================================================
%%% Internal
%%%===================================================================

-spec canonical_body(binary() | map() | list()) ->
    {ok, binary()} | {error, non_canonical}.
canonical_body(<<>>) ->
    %% 无 body 的 mutation：以空对象参与 digest（同 method+path 稳定可比）
    enterprise_cursor_v2:canonical_json(#{});
canonical_body(B) when is_binary(B) ->
    try jsone:decode(B) of
        Decoded -> enterprise_cursor_v2:canonical_json(Decoded)
    catch
        _:_ -> {error, non_canonical}
    end;
canonical_body(T) ->
    enterprise_cursor_v2:canonical_json(T).

-spec is_printable_ascii(binary()) -> boolean().
is_printable_ascii(<<C, Rest/binary>>) when C >= 16#20, C =< 16#7E ->
    is_printable_ascii(Rest);
is_printable_ascii(<<>>) ->
    true;
is_printable_ascii(_) ->
    false.
