-module(imboy_secret_policy).

%%%
%%% Task 12 / LT-06：三类核心 secret 的唯一校验源。
%%%
%%% jwt_key（终端 JWT）/ postgre_aes_key（PG 列加密）/ adm_cookie_secret
%%% （管理后台 cookie HMAC）三者在交付（strict）profile 下必须：
%%%   ① 存在（空串占位视同缺失）；
%%%   ② 达到最小长度（>= 32 字节， resisting brute force 的下界）；
%%%   ③ 两两互异（一类泄露不得波及另外两类——密钥拆分的本意）。
%%% imboy_app:validate_runtime_config/0 的 strict 分支与 deploy/preflight.sh
%%% 共同消费本模块的同一组规则常量，禁止各自实现。
%%%
%%% 开发（dev/local/test）profile：缺失时由 ensure_dev_defaults/0 派生
%%% node-local 稳定值（重启不变，节点间不同，且三值互异），保证开发可用
%%% 但不存在公开常量密钥。strict profile 下 ensure 不会被调用——
%%% validate_runtime_config 先行 fail-fast。
%%%
%%% 配套环境变量（imboy_env 覆盖层）：IMBOY_JWT_KEY / IMBOY_POSTGRE_AES_KEY /
%%% IMBOY_ADM_COOKIE_SECRET。
%%%

-export([required_keys/0]).
-export([min_length/0]).
-export([validate_strict/0]).
-export([ensure_dev_defaults/0]).

-include("log.hrl").

-define(MIN_LENGTH, 32).

%% @doc 受本策略管辖的 secret 键（application env 键名）。
-spec required_keys() -> [jwt_key | postgre_aes_key | adm_cookie_secret].
required_keys() ->
    [jwt_key, postgre_aes_key, adm_cookie_secret].

%% @doc 最小长度（字节）。deploy/preflight.sh 的同名规则引用此值。
-spec min_length() -> 32.
min_length() ->
    ?MIN_LENGTH.

%% @doc strict（交付/生产）profile 校验：缺失/过短/相同都以 {error, Reason} 返回，
%% 由调用方 fail-fast（imboy_app: erlang:error；preflight: 报错退出）。
-spec validate_strict() -> ok | {error, term()}.
validate_strict() ->
    case missing_key(required_keys()) of
        {missing, Key} ->
            {error, {missing_required_config, Key}};
        false ->
            case short_key(required_keys()) of
                {short, Key, Len} ->
                    {error, {secret_too_short, Key, Len}};
                false ->
                    ensure_pairwise_distinct(required_keys())
            end
    end.

%% @doc 开发 profile 兜底：为缺失的键派生 node-local 稳定 dev 值。
%% 派生盐互异 ⇒ 三值互异；与既有 ensure_jwt_key/ensure_postgre_aes_key 的
%% node 哈希派生同模式（重启不变，开发会话/加密数据不因重启失效）。
%% strict profile 不会走到这里（validate_runtime_config 先行 fail-fast）。
-spec ensure_dev_defaults() -> ok.
ensure_dev_defaults() ->
    Seed = erlang:phash2(node()),
    ok = ensure_derived(jwt_key, <<"jwt:", (integer_to_binary(Seed))/binary>>, fun base64:encode/1),
    ok =
        ensure_derived(
            postgre_aes_key,
            <<"pgaes:", (integer_to_binary(Seed))/binary>>,
            fun(B) -> B end
        ),
    ok =
        ensure_derived(
            adm_cookie_secret,
            <<"admcookie:", (integer_to_binary(Seed))/binary>>,
            fun base64:encode/1
        ),
    ok.

%% ===================================================================
%% Internal
%% ===================================================================

-spec missing_key([atom()]) -> {missing, atom()} | false.
missing_key([Key | Rest]) ->
    case normalize(config_ds:env(Key, <<>>)) of
        <<>> -> {missing, Key};
        _ -> missing_key(Rest)
    end;
missing_key([]) ->
    false.

-spec short_key([atom()]) -> {short, atom(), non_neg_integer()} | false.
short_key([Key | Rest]) ->
    Value = normalize(config_ds:env(Key, <<>>)),
    Len = byte_size(Value),
    case Len < ?MIN_LENGTH of
        true -> {short, Key, Len};
        false -> short_key(Rest)
    end;
short_key([]) ->
    false.

%% @doc 两两互异校验；返回第一对相同键，全部互异则 ok。
-spec ensure_pairwise_distinct([atom()]) -> ok | {error, term()}.
ensure_pairwise_distinct(Keys) ->
    Values = [{Key, normalize(config_ds:env(Key, <<>>))} || Key <- Keys],
    case first_duplicate(Values) of
        {K1, K2} -> {error, {secrets_must_differ, K1, K2}};
        false -> ok
    end.

-spec first_duplicate([{atom(), binary()}]) -> {atom(), atom()} | false.
first_duplicate([{K1, V} | Rest]) ->
    case [K2 || {K2, V2} <- Rest, V2 =:= V] of
        [K2 | _] ->
            {K1, K2};
        [] ->
            first_duplicate(Rest)
    end;
first_duplicate([]) ->
    false.

-spec ensure_derived(atom(), binary(), fun((binary()) -> binary())) -> ok.
ensure_derived(Key, Seed, Wrap) ->
    case normalize(config_ds:env(Key, <<>>)) of
        <<>> ->
            DevKey = Wrap(crypto:hash(sha256, Seed)),
            ok = application:set_env(imboy, Key, DevKey),
            ok =
                ?WARN_LOG(
                    {dev_secret_derived, Key,
                        <<"set a real value via the matching IMBOY_* env in production">>}
                ),
            ok;
        _ ->
            ok
    end.

-spec normalize(term()) -> binary().
normalize(undefined) ->
    <<>>;
normalize(false) ->
    <<>>;
normalize(Value) when is_binary(Value) ->
    Value;
normalize(Value) when is_list(Value) ->
    unicode:characters_to_binary(Value);
normalize(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
normalize(Value) ->
    ec_cnv:to_binary(Value).
