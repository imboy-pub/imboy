-module(list_cursor).

%%%
% W07.b（channel-group-ux-v2.2 §6.5 C2）：个人/工作区清单签名游标。
%
% 复用 enterprise_cursor_v2 的防篡改实现模式（canonical_json 排序键
% 规范化 + HMAC-SHA256 常数时间验签 + 24h 过期 + fail-closed 不回显
% 原因），但**授权域不同**：payload 绑定
%   uid / scope / workspace_id / list_kind / 规范 q / 排序版本 / 上界，
% 与 enterprise 的 organization/application 域无交集（不走其 family
% 白名单，不经其 build_payload）。
%
% 合同要点（C2/J20/J27.c）：
%   * 上界是资源 id（全家族统一 id keyset：id 唯一且不可变，全扫描
%     无漏/重；展示排序仍按各家族 C2 定义）；
%   * 关键词/范围变化 → 绑定比对失败 → {error, foreign}——调用方必须
%     400 invalid_request，**绝不回落第 1 页或猜 offset**（J27.c）；
%   * malformed/tampered/跨账号 → {error, invalid}（同样 400，不回显
%     解码内容）；过期单独 {error, expired}（400）；
%   * secret：config {imboy, list_cursor_signing_key}，>= 32 bytes；
%     缺失/形态非法 → {error, key_unavailable}（调用方映射 503）。
%%%

-export([
    canonical_q/1,
    issue/4,
    resolve/4,
    kinds/0,
    sort_version/0
]).

-define(SIGNING_KEY_ENV_KEY, list_cursor_signing_key).
-define(MIN_KEY_BYTES, 32).
-define(TTL_SECONDS, 86400).
-define(SORT_VERSION, 1).
-define(PAYLOAD_VERSION, 1).

-define(KINDS, [
    <<"group_owned">>,
    <<"group_joined">>,
    <<"group_managed">>,
    <<"channel_subscribed">>,
    <<"channel_managed">>,
    <<"ws_group_directory">>,
    <<"ws_group_member">>,
    <<"ws_channel_directory">>
]).

%%%===================================================================
%%% API
%%%===================================================================

%% 八类清单游标域。
-spec kinds() -> [binary(), ...].
kinds() -> ?KINDS.

%% 排序版本（查询形状/排序定义变更时 +1，旧游标随即 foreign 失效）。
-spec sort_version() -> pos_integer().
sort_version() -> ?SORT_VERSION.

%% @doc 规范 q：trim + 64 字符截断。签发绑定与 repo 的 ILIKE 子句必须
%% 使用同一规范形式（repo 的 title/name 子句经本函数取规范词）。
-spec canonical_q(term()) -> binary().
canonical_q(Q) when is_binary(Q) ->
    elib_str:trunc(string:trim(Q), 64);
canonical_q(_) ->
    <<>>.

%% @doc 签发清单游标。
%% Ctx = #{scope => personal|workspace, workspace_id => integer(),
%%         q => binary(), upper => integer()（keyset 上界资源 id）}。
-spec issue(integer(), binary(), map(), integer()) ->
    {ok, binary()} | {error, key_unavailable | non_canonical}.
issue(Uid, Kind, Ctx, NowSec) when
    is_integer(Uid),
    is_binary(Kind),
    is_map(Ctx),
    is_integer(NowSec)
->
    case signing_key() of
        {ok, Secret} ->
            Payload = #{
                <<"v">> => ?PAYLOAD_VERSION,
                <<"kind">> => <<"imboy_list_cursor">>,
                <<"uid">> => Uid,
                <<"list_kind">> => Kind,
                <<"scope">> => maps:get(scope, Ctx, <<"personal">>),
                <<"workspace_id">> => maps:get(workspace_id, Ctx, 0),
                <<"q">> => canonical_q(maps:get(q, Ctx, <<>>)),
                <<"sort_version">> => ?SORT_VERSION,
                <<"upper">> => maps:get(upper, Ctx, 0),
                <<"issued_at">> => NowSec
            },
            sign_payload(Payload, Secret);
        {error, _} = Err ->
            Err
    end.

%% @doc 验证并解析清单游标，返回上界资源 id。
%% Ctx 同 issue（uid/scope/workspace_id/list_kind 逐字段绑定；q 经同一
%% canonical_q 规范后比对）。绑定不符 → {error, foreign}（J27.c：错
%% cursor 不回落）；结构/HMAC/跨账号问题 → invalid；超 24h → expired。
-spec resolve(binary(), integer(), binary(), map()) ->
    {ok, integer()}
    | {error, invalid | expired | key_unavailable}.
resolve(Token, Uid, Kind, Ctx) when
    is_binary(Token),
    is_integer(Uid),
    is_binary(Kind),
    is_map(Ctx)
->
    case signing_key() of
        {ok, Secret} ->
            case verify_payload(Token, Secret) of
                {ok, Payload} -> bind(Payload, Uid, Kind, Ctx);
                {error, _} = Err -> Err
            end;
        {error, _} = Err ->
            Err
    end.

%%%===================================================================
%%% Internal
%%%===================================================================

%% 签发：base64url(canonical_json) ++ "." ++ base64url(HMAC-SHA256)。与
%% enterprise_cursor_v2 同一防篡改构造，但结构校验独立（下方），避免
%% 授权域互串（C2：复用模式而非授权域）。
-spec sign_payload(map(), binary()) -> {ok, binary()} | {error, non_canonical}.
sign_payload(Payload, Secret) ->
    case enterprise_cursor_v2:canonical_json(Payload) of
        {ok, Canonical} ->
            Mac = crypto:mac(hmac, sha256, Secret, Canonical),
            {ok, <<(b64url_encode(Canonical))/binary, ".", (b64url_encode(Mac))/binary>>};
        {error, _} = Err ->
            Err
    end.

%% 验签：分段形态 → HMAC（常数时间）→ JSON → 本域结构校验（v/kind/
%% uid/list_kind/scope/upper/issued_at 形态）→ 24h 过期。malformed/
%% tampered 一律 invalid，不回显原因；过期单独 expired。
-spec verify_payload(binary(), binary()) ->
    {ok, map()} | {error, invalid | expired}.
verify_payload(Token, Secret) ->
    case binary:split(Token, <<".">>, [global]) of
        [EncPayload, EncMac] when EncPayload =/= <<>>, EncMac =/= <<>> ->
            case {b64url_decode(EncPayload), b64url_decode(EncMac)} of
                {{ok, PayloadBin}, {ok, MacBin}} when byte_size(MacBin) =:= 32 ->
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
                    end;
                _ ->
                    {error, invalid}
            end;
        _ ->
            {error, invalid}
    end.

-spec check_structure(map()) -> {ok, map()} | {error, invalid | expired}.
check_structure(Payload) ->
    Now = os:system_time(second),
    IssuedAt = maps:get(<<"issued_at">>, Payload, undefined),
    StructuralOk =
        maps:get(<<"v">>, Payload, undefined) =:= ?PAYLOAD_VERSION andalso
            maps:get(<<"kind">>, Payload, undefined) =:= <<"imboy_list_cursor">> andalso
            is_integer(maps:get(<<"uid">>, Payload, undefined)) andalso
            is_binary(maps:get(<<"list_kind">>, Payload, undefined)) andalso
            lists:member(maps:get(<<"list_kind">>, Payload, undefined), ?KINDS) andalso
            maps:get(<<"scope">>, Payload, undefined) =:= <<"personal">> orelse
            maps:get(<<"scope">>, Payload, undefined) =:= <<"workspace">> andalso
                is_integer(maps:get(<<"workspace_id">>, Payload, undefined)) andalso
                is_binary(maps:get(<<"q">>, Payload, undefined)) andalso
                maps:get(<<"sort_version">>, Payload, undefined) =:= ?SORT_VERSION andalso
                is_integer(maps:get(<<"upper">>, Payload, undefined)) andalso
                is_integer(IssuedAt),
    case StructuralOk of
        true when is_integer(IssuedAt) ->
            case Now =< IssuedAt + ?TTL_SECONDS of
                true -> {ok, Payload};
                false -> {error, expired}
            end;
        _ ->
            {error, invalid}
    end.

b64url_encode(Bin) when is_binary(Bin) ->
    Base64 = base64:encode(Bin),
    Strip = <<
        <<
            case C of
                $+ -> $-;
                $/ -> $_;
                _ -> C
            end
        >>
     || <<C>> <= Base64
    >>,
    %% 去掉 '=' padding（enterprise_cursor_v2 同款无 padding 约定）。
    binary:replace(Strip, <<"=">>, <<>>, [global]).

%% 返回 {ok, Bin} | {error, invalid}（供 verify_payload 的形态匹配）。
-spec b64url_decode(binary()) -> {ok, binary()} | {error, invalid}.
b64url_decode(Bin) when is_binary(Bin) ->
    Restored = <<
        <<
            case C of
                $- -> $+;
                $_ -> $/;
                _ -> C
            end
        >>
     || <<C>> <= Bin
    >>,
    try
        {ok, base64:decode(append_pad(Restored))}
    catch
        _:_ -> {error, invalid}
    end.

append_pad(Restored) ->
    case byte_size(Restored) rem 4 of
        2 -> <<Restored/binary, "==">>;
        3 -> <<Restored/binary, "=">>;
        _ -> Restored
    end.

-spec bind(map(), integer(), binary(), map()) ->
    {ok, integer()} | {error, invalid}.
bind(Payload, Uid, Kind, Ctx) ->
    Scope = maps:get(scope, Ctx, <<"personal">>),
    WorkspaceId = maps:get(workspace_id, Ctx, 0),
    Q = canonical_q(maps:get(q, Ctx, <<>>)),
    ExpectedScope =
        case Scope of
            <<"workspace">> -> <<"workspace">>;
            _ -> <<"personal">>
        end,
    Bindings = #{
        <<"kind">> => <<"imboy_list_cursor">>,
        <<"uid">> => Uid,
        <<"list_kind">> => Kind,
        <<"scope">> => ExpectedScope,
        <<"workspace_id">> => WorkspaceId,
        <<"q">> => Q,
        <<"sort_version">> => ?SORT_VERSION
    },
    Matches =
        maps:fold(
            fun(K, V, Acc) ->
                Acc andalso maps:get(K, Payload, undefined) =:= V
            end,
            true,
            Bindings
        ),
    Upper = maps:get(<<"upper">>, Payload, undefined),
    %% 绑定不符与形态非法统一 invalid（对齐 enterprise 模式：不回显是哪个
    %% 字段不匹配，避免字典/探测面）。调用方一律 400 invalid_request，
    %% 绝不回落第 1 页（J27.c）。
    case Matches andalso is_integer(Upper) andalso Upper >= 0 of
        true -> {ok, Upper};
        false -> {error, invalid}
    end.

-spec signing_key() -> {ok, binary()} | {error, key_unavailable}.
signing_key() ->
    case config_ds:env(imboy, ?SIGNING_KEY_ENV_KEY, undefined) of
        Key when is_binary(Key), byte_size(Key) >= ?MIN_KEY_BYTES ->
            {ok, Key};
        _ ->
            {error, key_unavailable}
    end.
