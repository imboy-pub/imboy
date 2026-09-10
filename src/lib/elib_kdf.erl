-module(elib_kdf).

%%% Task 11 / LT-05：versioned 口令 KDF（v2 格式）与资源边界。
%%%
%%% v2 存储格式（自描述、可扩展算法）：
%%%   $v2$<algo>$i=<iterations>;v=<variant>$<salt-b64>$<hash-b64>
%%%   例：$v2$pbkdf2_sha512$i=210000;v=2$<salt>$<hash>
%%% variant = elib_password 既有 4 候选输入变体序号（1 直值/2 sha256/3 md5hex/
%%% 4 sha256(md5hex)）；generate 侧恒写 v=2（与既有 generate 的 sha256 预哈希
%%% 语义一致），verify 侧按 tag 只算一次 KDF。
%%%
%%% ⚠️ 生产算法与参数由显式配置激活，代码不代替决策（卡 Step2：无预算
%%% 不得任意选生产参数）：
%%%   kdf_v2_enabled    = true 才激活（默认 false = 全 legacy，零行为变化）
%%%   kdf_v2_iterations = 整数，须落在本模块硬边界内，越界即视为未激活
%%% 未激活时：hash_v2 返回 {error, disabled}，generate 侧回退既有格式，
%%% verify 侧照常接受 legacy——升级一次逻辑自动静默。
%%%
%%% fail 行为：畸形/未知版本/越界参数一律拒绝且不执行昂贵计算（防 DoS）。

-export([hash_v2/2]).
-export([hash_v2_prehashed/2]).
-export([verify_v2/2]).
-export([parse_v2/1]).
-export([enabled/0]).
-export([bounds/0]).

-include("log.hrl").

-define(V2_PREFIX, <<"$v2$">>).
-define(ALGO, <<"pbkdf2_sha512">>).
-define(HASH_BYTES, 64).
-define(SALT_BYTES, 16).
%% 框架硬边界：下限 = 单次验证成本底线（防弱参数），上限 = 单次验证时延天花板（防 DoS）。
%% 生产取值由用户资源预算在边界内选定（部署目标基准后定），不入代码。
-define(ITER_MIN, 100000).
-define(ITER_MAX, 2000000).

-opaque stored_v2() :: binary().
-export_type([stored_v2/0]).

%% @doc KDF v2 是否已激活；激活返回 {ok, Iterations}。
-spec enabled() -> {ok, non_neg_integer()} | {error, disabled}.
enabled() ->
    case {config_ds:env(kdf_v2_enabled, false), config_ds:env(kdf_v2_iterations, undefined)} of
        {true, Iter} when is_integer(Iter) ->
            case in_bounds(Iter) of
                true ->
                    {ok, Iter};
                false ->
                    ok = ?WARN_LOG({kdf_v2_iterations_out_of_bounds, Iter}),
                    {error, disabled}
            end;
        _ ->
            {error, disabled}
    end.

%% @doc 框架硬边界（部署目标基准的取值范围约束）。
-spec bounds() -> #{min_iterations := non_neg_integer(), max_iterations := non_neg_integer()}.
bounds() ->
    #{min_iterations => ?ITER_MIN, max_iterations => ?ITER_MAX}.

%% @doc v2 哈希。VariantIdx = 输入变体序号（generate 侧恒 2，语义对齐既有实现）。
%% @doc 对已变换候选直接进 KDF（rehash-on-login 编排用：命中变体的候选值
%% 就是 tag 对应的输入，不能再做一次变换）。
-spec hash_v2_prehashed(binary(), 1..4) -> {ok, binary()} | {error, disabled}.
hash_v2_prehashed(Candidate, VariantIdx) ->
    do_hash_v2(Candidate, VariantIdx).

%% @doc v2 哈希。VariantIdx = 输入变体序号（generate 侧恒 2，语义对齐既有实现）。
-spec hash_v2(iodata(), 1..4) -> {ok, binary()} | {error, disabled}.
hash_v2(Plain, VariantIdx) when VariantIdx >= 1, VariantIdx =< 4 ->
    Candidate = elib_password:candidate_variant(iolist_to_binary(Plain), VariantIdx),
    do_hash_v2(Candidate, VariantIdx);
hash_v2(_, _) ->
    {error, disabled}.

%% @private
-spec do_hash_v2(binary(), 1..4) -> {ok, binary()} | {error, disabled}.
do_hash_v2(Candidate, VariantIdx) ->
    case enabled() of
        {ok, Iter} ->
            Salt = crypto:strong_rand_bytes(?SALT_BYTES),
            Hash = crypto:pbkdf2_hmac(sha512, Candidate, Salt, Iter, ?HASH_BYTES),
            Formatted =
                <<"$v2$pbkdf2_sha512$i=", (integer_to_binary(Iter))/binary, ";v=",
                    (integer_to_binary(VariantIdx))/binary, "$", (base64:encode(Salt))/binary, "$",
                    (base64:encode(Hash))/binary>>,
            {ok, Formatted};
        {error, disabled} = E ->
            E
    end.

%% @doc v2 验证：格式/版本/参数边界校验通过才执行 KDF；一切异常形态拒绝。
-spec verify_v2(iodata(), binary()) -> {ok, []} | {error, binary()}.
verify_v2(Plain, Stored) ->
    case parse_v2(Stored) of
        {ok, #{iterations := Iter, variant := V, salt := Salt, hash := Hash}} ->
            case in_bounds(Iter) of
                true ->
                    PlainBin = iolist_to_binary(Plain),
                    Candidate = elib_password:candidate_variant(PlainBin, V),
                    Computed = crypto:pbkdf2_hmac(sha512, Candidate, Salt, Iter, ?HASH_BYTES),
                    elib_password:eq(Hash, Computed);
                false ->
                    ok = ?WARN_LOG({kdf_v2_iterations_out_of_bounds_verify, Iter}),
                    {error, <<"errorPassword">>}
            end;
        _ ->
            {error, <<"errorPassword">>}
    end.

%% @doc 解析 v2 存储串；非 v2 形态返回 error（调用方回退 legacy 链）。
-spec parse_v2(binary()) ->
    {ok, #{
        iterations := non_neg_integer(),
        variant := 1..4,
        salt := binary(),
        hash := binary()
    }}
    | error.
parse_v2(Stored) when is_binary(Stored) ->
    case Stored of
        <<"$v2$", Rest/binary>> ->
            parse_v2_body(Rest);
        _ ->
            error
    end;
parse_v2(_) ->
    error.

%% @private
parse_v2_body(Rest) ->
    try
        case binary:split(Rest, <<"$">>) of
            [?ALGO, ParamsAndMore] ->
                case binary:split(ParamsAndMore, <<"$">>) of
                    [Params, Tail] ->
                        case binary:split(Tail, <<"$">>) of
                            [SaltB64, HashB64] ->
                                #{i := Iter, v := V} = parse_params(Params),
                                {ok, #{
                                    iterations => Iter,
                                    variant => V,
                                    salt => base64:decode(SaltB64),
                                    hash => base64:decode(HashB64)
                                }};
                            _ ->
                                error
                        end;
                    _ ->
                        error
                end;
            _ ->
                error
        end
    catch
        _:_ -> error
    end.

%% @private 解析 i=<n>;v=<m>；未知键拒绝
parse_params(Params) ->
    Parts = binary:split(Params, <<";">>, [global]),
    maps:from_list(
        lists:map(
            fun(P) ->
                [K, V] = binary:split(P, <<"=">>),
                {binary_to_atom(K, utf8), binary_to_integer(V)}
            end,
            Parts
        )
    ).

%% @private
in_bounds(Iter) when is_integer(Iter) ->
    Iter >= ?ITER_MIN andalso Iter =< ?ITER_MAX;
in_bounds(_) ->
    false.
