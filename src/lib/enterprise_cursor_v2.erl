-module(enterprise_cursor_v2).

%%%
% enterprise_cursor_v2 是 CURSOR-V2 签名游标（plan §10.1 冻结合同）的唯一
% 实现，供 8 个 internal list family（INT-16/17/23/24/26/27/28/30）与
% A4 的 Human Directory（family=human_directory，绑定 Human uid/O/filter）
% 复用。A2/A4 只依赖本模块的导出面，不重复实现编码/验签。
%
% 冻结值（§10.1）：
%   * encoding：base64url(canonical_json) ++ "." ++
%     base64url(HMAC-SHA256(key, canonical_json))，base64url 无 padding；
%   * expiry：issued_at + 24h（86400s，合同冻结值，不做配置覆写）；
%   * malformed/tampered/foreign → {error, invalid}——**不回显原因/解码内容**
%     （调用方统一映射 400 invalid_request，不得把 error term 写进响应）；
%   * 过期单独 {error, expired}（调用方同样 400 invalid_request）；
%   * secret：config {imboy, enterprise_internal_cursor_signing_key}，
%     至少 32 bytes；缺失/形态非法由 signing_key/0 返回
%     {error, key_unavailable}，调用方映射 503 security_gate_closed。
%
% canonical_json（§10.1，与 Idempotency digest 共用同一规范）：
%   * 对象 key 按 UTF-8 字节序递归排序（Erlang binary 比较即字节序）；
%   * array 保持输入顺序；string 用 JSON 标准转义；boolean/null 固定
%     小写 JSON token；integer 无前导零十进制；
%   * 本合同请求字段**禁止浮点/NaN/Infinity**——输入树中出现 float、
%     非法 atom、非 binary key 等不可规范化形态一律
%     {error, non_canonical}，不得退回普通 JSON encoder 的不稳定输出。
%
% payload 结构（§10.1 冻结）：v=2, family, organization_id, application_id,
% workspace_id/filter, status, sort_tuple, issued_at。本模块只强校验
% v/family/issued_at 的结构形态（verify/2），O/App/W/filter 的**逐字段绑定
% 比对是 handler 义务**（foreign family/O/App/W/filter → 400 invalid_request，
% 不得自动改写为当前请求）。build_payload/6 提供规范形态的构造器。
%%%

-export([
    canonical_json/1,
    sign/2,
    verify/2,
    signing_key/0,
    build_payload/6,
    families/0,
    ttl_seconds/0
]).

%% 8 个 internal list family（§10.2 冻结；INT-16/17/23/24/26/27/28/30）
-define(INTERNAL_FAMILIES, [
    <<"identity_mappings">>,
    <<"directory_users">>,
    <<"webhook_deliveries">>,
    <<"workspaces">>,
    <<"groups">>,
    <<"group_members">>,
    <<"projects">>,
    <<"channels">>
]).

%% A4 Human Directory 复用同一算法，但 payload 域不同（绑定 Human uid，
%% 不绑定 application_id）——验签层与 internal family 同一张白名单。
-define(HUMAN_DIRECTORY_FAMILY, <<"human_directory">>).

%% issued_at + 24h 过期（§10.1 冻结值）。
-define(TTL_SECONDS, 86400).

%% config 键：{imboy, enterprise_internal_cursor_signing_key}（>= 32 bytes）。
-define(SIGNING_KEY_ENV_KEY, enterprise_internal_cursor_signing_key).
-define(MIN_KEY_BYTES, 32).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc 规范 JSON 编码：对象 key 按 UTF-8 字节序递归排序；array 保序；
%% 只接受 binary key 与 binary/integer/boolean/null/map/list 值。
%% 出现 float / NaN / Infinity / 非法 atom / 非 binary key / 无效 UTF-8
%% 字符串 → {error, non_canonical}（fail-closed，不退回不稳定编码器）。
-spec canonical_json(term()) -> {ok, binary()} | {error, non_canonical}.
canonical_json(Term) ->
    case canonicalize(Term) of
        {ok, Parts} -> {ok, iolist_to_binary(Parts)};
        {error, non_canonical} -> {error, non_canonical}
    end.

%% @doc 签发游标：base64url(canonical_json) ++ "." ++
%% base64url(HMAC-SHA256(Secret, canonical_json))。
%% PayloadMap 不可规范化（含 float 等）→ {error, non_canonical}。
%% Secret 由调用方经 signing_key/0 取得（形态已由该函数保证）。
-spec sign(map(), binary()) -> {ok, binary()} | {error, non_canonical}.
sign(PayloadMap, Secret) when is_map(PayloadMap), is_binary(Secret) ->
    case canonical_json(PayloadMap) of
        {ok, Canonical} ->
            Mac = crypto:mac(hmac, sha256, Secret, Canonical),
            {ok, <<(b64url_encode(Canonical))/binary, ".", (b64url_encode(Mac))/binary>>};
        {error, non_canonical} ->
            {error, non_canonical}
    end;
sign(_PayloadMap, _Secret) ->
    {error, non_canonical}.

%% @doc 验证游标：校验分段形态、HMAC（常数时间）、payload 结构
%% （JSON object + v=2 + 已知 family + issued_at 为整数秒）与
%% issued_at + 24h 过期窗口。
%% 返回 {ok, Payload}（解码后的 map）| {error, invalid} | {error, expired}。
%% **不回显原因**：malformed/tampered/foreign-结构统一 invalid（仅过期单独
%% expired），调用方一律映射 400 invalid_request，错误 detail 不得进入响应体。
%% O/App/W/filter 与当前请求的绑定比对是 handler 义务，不在本函数内。
-spec verify(binary(), binary()) -> {ok, map()} | {error, invalid | expired}.
verify(Cursor, Secret) when is_binary(Cursor), is_binary(Secret) ->
    case binary:split(Cursor, <<".">>, [global]) of
        [EncPayload, EncMac] when EncPayload =/= <<>>, EncMac =/= <<>> ->
            case {b64url_decode(EncPayload), b64url_decode(EncMac)} of
                {{ok, PayloadBin}, {ok, MacBin}} when byte_size(MacBin) =:= 32 ->
                    verify_payload(PayloadBin, MacBin, Secret);
                _ ->
                    {error, invalid}
            end;
        _ ->
            {error, invalid}
    end;
verify(_Cursor, _Secret) ->
    {error, invalid}.

%% @doc 游标签名密钥：config {imboy, enterprise_internal_cursor_signing_key}。
%% 缺失 / 非 binary / 短于 32 bytes → {error, key_unavailable}（调用方映射
%% 503 security_gate_closed，绝不以弱/缺密钥继续签发或验签）。
-spec signing_key() -> {ok, binary()} | {error, key_unavailable}.
signing_key() ->
    case config_ds:env(imboy, ?SIGNING_KEY_ENV_KEY, undefined) of
        Key when is_binary(Key), byte_size(Key) >= ?MIN_KEY_BYTES ->
            {ok, Key};
        _ ->
            {error, key_unavailable}
    end.

%% @doc 规范 payload 构造器（§10.1 冻结形态；A2/A4 统一经此构造，避免
%% key 漂移）。Filter 是任意 map（可含 workspace_id/status 等家族过滤键，
%% 值必须可规范化——integer/binary）；SortTuple 是 keyset 排序值列表；
%% IssuedAt 是 epoch 秒（调用方取 os:system_time(second)）。
%% Human Directory（A4）不用本构造器（payload 域不同：绑定 uid 而非
%% application_id），自行构造 map 后同样走 sign/2。
-spec build_payload(
    binary(), integer(), integer(), map(), [term()], integer()
) -> map().
build_payload(Family, OrgId, AppId, Filter, SortTuple, IssuedAt) when
    is_binary(Family),
    is_integer(OrgId),
    is_integer(AppId),
    is_map(Filter),
    is_list(SortTuple),
    is_integer(IssuedAt)
->
    #{
        <<"v">> => 2,
        <<"family">> => Family,
        <<"organization_id">> => OrgId,
        <<"application_id">> => AppId,
        <<"filter">> => Filter,
        <<"sort_tuple">> => SortTuple,
        <<"issued_at">> => IssuedAt
    }.

%% @doc 8 个 internal list family 全集（§10.2 冻结）。验签白名单另含
%% human_directory（A4 Human Directory 复用本算法，payload 域不同）。
-spec families() -> [binary(), ...].
families() ->
    ?INTERNAL_FAMILIES.

%% @doc 过期窗口（秒；§10.1 冻结 24h，不开放配置覆写）。
-spec ttl_seconds() -> pos_integer().
ttl_seconds() ->
    ?TTL_SECONDS.

%%%===================================================================
%%% Internal
%%%===================================================================

-spec verify_payload(binary(), binary(), binary()) ->
    {ok, map()} | {error, invalid | expired}.
verify_payload(PayloadBin, MacBin, Secret) ->
    Expected = crypto:mac(hmac, sha256, Secret, PayloadBin),
    case crypto:hash_equals(Expected, MacBin) of
        false ->
            {error, invalid};
        true ->
            case jsone:decode(PayloadBin) of
                Payload when is_map(Payload) ->
                    check_structure(Payload);
                _ ->
                    {error, invalid}
            end
    end.

%% 结构校验：v=2、family 在白名单（8 internal + human_directory）、
%% issued_at 为整数且在 24h 窗口内。其余键（organization_id/application_id/
%% workspace_id/filter/sort_tuple...）的形态与绑定由 handler 比对
%% （foreign → invalid_request）。
-spec check_structure(map()) -> {ok, map()} | {error, invalid | expired}.
check_structure(Payload) ->
    case
        {
            maps:get(<<"v">>, Payload, undefined),
            maps:get(<<"family">>, Payload, undefined),
            maps:get(<<"issued_at">>, Payload, undefined)
        }
    of
        {2, Family, IssuedAt} when is_binary(Family), is_integer(IssuedAt) ->
            case known_family(Family) of
                false ->
                    {error, invalid};
                true ->
                    case expired(IssuedAt) of
                        true -> {error, expired};
                        false -> {ok, Payload}
                    end
            end;
        _ ->
            {error, invalid}
    end.

-spec known_family(binary()) -> boolean().
known_family(Family) ->
    lists:member(Family, [?HUMAN_DIRECTORY_FAMILY | ?INTERNAL_FAMILIES]).

-spec expired(integer()) -> boolean().
expired(IssuedAt) ->
    IssuedAt + ?TTL_SECONDS < os:system_time(second).

%% ------------------------------------------------------------------
%% canonical JSON 编码（返回 iolist；调用方 iolist_to_binary）
%% ------------------------------------------------------------------

-spec canonicalize(term()) -> {ok, iolist()} | {error, non_canonical}.
canonicalize(Map) when is_map(Map) ->
    Keys = maps:keys(Map),
    case lists:all(fun(K) -> is_binary(K) end, Keys) of
        true ->
            %% Erlang binary 比较即 UTF-8 字节序：sort 后递归编码
            encode_map(Map, lists:sort(Keys));
        false ->
            {error, non_canonical}
    end;
canonicalize(L) when is_list(L) ->
    encode_list(L);
canonicalize(B) when is_binary(B) ->
    case is_valid_utf8(B) of
        true -> {ok, [json_string(B)]};
        false -> {error, non_canonical}
    end;
canonicalize(I) when is_integer(I) ->
    {ok, [integer_to_binary(I)]};
canonicalize(true) ->
    {ok, <<"true">>};
canonicalize(false) ->
    {ok, <<"false">>};
canonicalize(null) ->
    {ok, <<"null">>};
%% 浮点（含 NaN/Infinity，Erlang 中均为 float）与任何其他形态一律拒绝：
%% 本合同请求字段禁止浮点，不得静默舍入后编码。
canonicalize(_) ->
    {error, non_canonical}.

-spec encode_map(map(), [binary()]) -> {ok, iolist()} | {error, non_canonical}.
encode_map(_Map, []) ->
    {ok, "{}"};
encode_map(Map, [K | Rest]) ->
    case canonicalize(maps:get(K, Map)) of
        {ok, V} -> encode_map_tail(Map, Rest, ["{", json_string(K), ":", V]);
        {error, non_canonical} = Err -> Err
    end.

-spec encode_map_tail(map(), [binary()], iolist()) ->
    {ok, iolist()} | {error, non_canonical}.
encode_map_tail(_Map, [], Acc) ->
    {ok, [Acc, "}"]};
encode_map_tail(Map, [K | Rest], Acc) ->
    case canonicalize(maps:get(K, Map)) of
        {ok, V} -> encode_map_tail(Map, Rest, [Acc, ",", json_string(K), ":", V]);
        {error, non_canonical} = Err -> Err
    end.

-spec encode_list([term()]) -> {ok, iolist()} | {error, non_canonical}.
encode_list([]) ->
    {ok, "[]"};
encode_list([V | Rest]) ->
    case canonicalize(V) of
        {ok, VParts} -> encode_list_tail(Rest, ["[", VParts]);
        {error, non_canonical} = Err -> Err
    end.

-spec encode_list_tail([term()], iolist()) -> {ok, iolist()} | {error, non_canonical}.
encode_list_tail([], Acc) ->
    {ok, [Acc, "]"]};
encode_list_tail([V | Rest], Acc) ->
    case canonicalize(V) of
        {ok, VParts} -> encode_list_tail(Rest, [Acc, ",", VParts]);
        {error, non_canonical} = Err -> Err
    end.

%% JSON string：标准转义复用 jsone 对裸 binary 的编码（输出自带引号）。
-spec json_string(binary()) -> binary().
json_string(B) ->
    jsone:encode(B).

-spec is_valid_utf8(binary()) -> boolean().
is_valid_utf8(B) ->
    case unicode:characters_to_binary(B, utf8, utf8) of
        Re when is_binary(Re) -> true;
        _ -> false
    end.

%%%===================================================================
%%% base64url（RFC 4648 §5，无 padding）
%%%===================================================================

-spec b64url_encode(binary()) -> binary().
b64url_encode(Data) ->
    base64:encode(Data, #{padding => false, mode => url}).

-spec b64url_decode(binary()) -> {ok, binary()} | error.
b64url_decode(Data) ->
    try
        {ok, base64:decode(Data, #{padding => false, mode => url})}
    catch
        _:_ -> error
    end.
