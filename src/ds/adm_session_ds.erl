-module(adm_session_ds).

%%%
%%% Task 12 / LT-06：管理后台 cookie 会话生命周期（ds 层唯一真源）。
%%%
%%% 模型：每管理员单调递增 epoch（public.adm_auth_epoch，DB 权威 = 重启后生效）。
%%% 登录签发的 adm_user_sig 内嵌 (epoch, exp) 并整体 HMAC：
%%%   sig = base64(hmac_sha256(uid:epoch:exp, adm_cookie_secret))
%%%   adm_user_sig = "v1:" epoch ":" exp ":" sig
%%% 中间件校验：签名一致 → exp 未过 → sig.epoch >= 当势 epoch；
%%% 任一不满足即拒绝；epoch 现势不可确认（DB 不可用）fail-closed 拒绝。
%%% bump 触发点：登出（当前管理员自身）；未来管理端强制下线复用同管道。
%%% legacy 裸 HMAC 格式（无 v1: 前缀）一律拒绝——升级后存量 cookie 失效一次，
%%% 管理员重新登录即可（fail-closed，不做无过期豁免）。
%%%
%%% 密钥：独立 adm_cookie_secret（不复用 jwt_key）；缺失时启动期由
%%% imboy_secret_policy:ensure_dev_defaults/0 派生 node-local 值（开发），
%%% strict profile 由 validate_runtime_config fail-fast；本模块遇到"仍为空"
%%% 一律 fail-loud（防御纵深，正常不可达）。
%%%

-export([current_epoch/1]).
-export([bump/1]).
-export([issue/1]).
-export([issue_with/3]).
-export([verify/2]).
-export([signing_key/0]).
-export([session_ttl_sec/0]).
-export([cookie_opts/1]).

-include("log.hrl").

-define(EPOCH_MEMO_SEC, 60).
-define(DEFAULT_TTL_SEC, 28800).
-define(UPSERT_SQL, <<
    "INSERT INTO public.adm_auth_epoch (admin_id, epoch, updated_at) "
    "VALUES ($1, 2, now()) "
    "ON CONFLICT (admin_id) DO UPDATE "
    "SET epoch = public.adm_auth_epoch.epoch + 1, updated_at = now() "
    "RETURNING epoch"
>>).

%% @doc 当前 epoch；缺行 = 1；DB 不可用 = {error, unavailable}。
%% 正向 memo 60s；bump 时主动失效并跨节点广播（与 auth_session_ds 同款时序）。
-spec current_epoch(integer()) -> {ok, non_neg_integer()} | {error, unavailable}.
current_epoch(AdmId) when is_integer(AdmId), AdmId > 0 ->
    Key = {adm_session_epoch, AdmId},
    Cached = safe_cache_get(Key),
    case Cached of
        {ok, {ok, N}} when is_integer(N), N >= 1 ->
            {ok, N};
        _ ->
            case safe_query_epoch(AdmId) of
                {ok, E} when is_integer(E), E >= 1 ->
                    safe_cache_memo(Key, E),
                    {ok, E};
                _ ->
                    ok = ?WARN_LOG({adm_session_epoch_unavailable, AdmId, unavailable}),
                    {error, unavailable}
            end
    end;
current_epoch(_) ->
    {error, unavailable}.

%% @doc 独立事务 bump（登出/强制下线），成功后失效本节点缓存并跨节点广播。
-spec bump(integer()) -> ok | {error, term()}.
bump(AdmId) ->
    case
        elib_pg:with_tx(fun(Conn) ->
            {ok, _, _, [{_E}]} = epgsql:equery(Conn, ?UPSERT_SQL, [AdmId]),
            ok
        end)
    of
        ok ->
            evict(AdmId),
            ok;
        {rollback, Reason} ->
            {error, Reason};
        {error, Reason} ->
            {error, Reason};
        Other ->
            {error, Other}
    end.

%% @doc 签发当前有效的 cookie sig（当势 epoch + TTL）；epoch 不可确认时拒绝签发。
-spec issue(binary()) -> {ok, binary()} | {error, unavailable}.
issue(UidBin) ->
    case current_epoch(uid_int(UidBin)) of
        {ok, Epoch} ->
            {ok, issue_with(UidBin, Epoch, erlang:system_time(second) + session_ttl_sec())};
        {error, _} ->
            {error, unavailable}
    end.

%% @doc 以给定 claims 签发（登录走 issue/1；测试与管理端操作用）。
-spec issue_with(binary(), non_neg_integer(), integer()) -> binary().
issue_with(UidBin, Epoch, ExpTs) when
    is_binary(UidBin), is_integer(Epoch), Epoch >= 1, is_integer(ExpTs)
->
    Claims = claims(UidBin, Epoch, ExpTs),
    Sig = base64:encode(elib_hasher:hmac_sha256(Claims, signing_key())),
    <<"v1:", (ec_cnv:to_binary(Epoch))/binary, ":", (ec_cnv:to_binary(ExpTs))/binary, ":",
        Sig/binary>>.

%% @doc 校验 cookie sig；返回 {ok, AdminId} 或 {error, malformed | bad_signature |
%% expired | revoked | store_unavailable}。
-spec verify(binary(), binary() | false | undefined) ->
    {ok, integer()} | {error, malformed | bad_signature | expired | revoked | store_unavailable}.
verify(UidBin, Value) when is_binary(UidBin), is_binary(Value) ->
    case binary:split(Value, <<":">>, [global]) of
        [<<"v1">>, EpochBin, ExpBin, SigB64] ->
            case {safe_int(EpochBin), safe_int(ExpBin), safe_uid(UidBin), safe_b64(SigB64)} of
                {Epoch, ExpTs, Uid, SigBin} when
                    is_integer(Epoch),
                    Epoch >= 1,
                    is_integer(ExpTs),
                    is_integer(Uid),
                    Uid > 0,
                    is_binary(SigBin)
                ->
                    Claims = claims(UidBin, Epoch, ExpTs),
                    %% 常数时间比较：原始 hmac 字节 vs 解码后的 sig 字节（同域比较）
                    HmacBin = elib_hasher:hmac_sha256(Claims, signing_key()),
                    case
                        crypto:hash_equals(
                            crypto:hash(sha256, HmacBin), crypto:hash(sha256, SigBin)
                        )
                    of
                        false ->
                            {error, bad_signature};
                        true ->
                            verify_claims(Uid, Epoch, ExpTs)
                    end;
                _ ->
                    {error, malformed}
            end;
        _ ->
            %% legacy 裸 HMAC / 任意其他形态：fail-closed
            {error, malformed}
    end;
verify(_, _) ->
    {error, malformed}.

%% @doc 管理后台 cookie 签名密钥（独立 adm_cookie_secret，不复用 jwt_key）。
%% 空 = 未配置：生产由 validate_runtime_config fail-fast + 启动期 ensure_dev_defaults
%% 兜底，此处仅可能发生在未走启动链的测试/降级环境——派生 node-local 稳定值并落
%% env（同进程内稳定）。严禁回落公开常量（旧实现回落 <<"imboy-adm-cookie">>
%% 可被任意伪造）；派生值节点间不同、外部不可知，伪造面与常量相比为零。
-spec signing_key() -> binary().
signing_key() ->
    case normalize(config_ds:env(adm_cookie_secret, <<>>)) of
        <<>> ->
            Key = base64:encode(
                crypto:hash(
                    sha256,
                    integer_to_binary(erlang:phash2({adm_cookie_secret_derived, node()}))
                )
            ),
            application:set_env(imboy, adm_cookie_secret, Key),
            Key;
        Key ->
            Key
    end.

%% @doc 会话 TTL（秒），config adm_session_ttl_sec，默认 8 小时。
-spec session_ttl_sec() -> pos_integer().
session_ttl_sec() ->
    case config_ds:env(adm_session_ttl_sec, ?DEFAULT_TTL_SEC) of
        N when is_integer(N), N >= 60 ->
            N;
        _ ->
            ?DEFAULT_TTL_SEC
    end.

%% @doc 认证 cookie 属性（安全属性唯一真源，签发/清除共用，可断言）：
%% HttpOnly + SameSite=Lax + 显式 Max-Age；Secure 随 start_mode（tls/http_tls）。
-spec cookie_opts(non_neg_integer()) -> map().
cookie_opts(MaxAge) ->
    #{
        path => <<"/">>,
        http_only => true,
        same_site => lax,
        secure => cookie_secure(),
        max_age => MaxAge
    }.

%% ===================================================================
%% Internal
%% ===================================================================

-spec verify_claims(integer(), non_neg_integer(), integer()) ->
    {ok, integer()} | {error, expired | revoked | store_unavailable}.
verify_claims(Uid, Epoch, ExpTs) ->
    Now = erlang:system_time(second),
    case ExpTs =< Now of
        true ->
            {error, expired};
        false ->
            case current_epoch(Uid) of
                {ok, Current} when Epoch >= Current ->
                    {ok, Uid};
                {ok, _} ->
                    {error, revoked};
                {error, _} ->
                    {error, store_unavailable}
            end
    end.

-spec claims(binary(), non_neg_integer(), integer()) -> binary().
claims(UidBin, Epoch, ExpTs) ->
    <<UidBin/binary, ":", (ec_cnv:to_binary(Epoch))/binary, ":", (ec_cnv:to_binary(ExpTs))/binary>>.

-spec safe_cache_get(term()) -> term().
safe_cache_get(Key) ->
    try imboy_cache:get(Key) of
        Other -> Other
    catch
        _:_ -> undefined
    end.

-spec safe_cache_memo(term(), non_neg_integer()) -> ok.
safe_cache_memo(Key, E) ->
    try imboy_cache:memo(fun() -> {ok, E} end, Key, ?EPOCH_MEMO_SEC) of
        _ -> ok
    catch
        _:_ -> ok
    end.

-spec safe_query_epoch(integer()) -> {ok, non_neg_integer()} | {error, unavailable}.
safe_query_epoch(AdmId) ->
    try
        case
            elib_pg:query(
                <<"SELECT epoch FROM public.adm_auth_epoch WHERE admin_id = $1">>,
                [AdmId]
            )
        of
            {ok, []} ->
                {ok, 1};
            {ok, [#{<<"epoch">> := E}]} when is_integer(E), E >= 1 ->
                {ok, E};
            {ok, _} ->
                {error, unavailable};
            {error, _} ->
                {error, unavailable}
        end
    catch
        _:_ ->
            {error, unavailable}
    end.

%% @private bump 后失效本节点缓存并跨节点广播
-spec evict(integer()) -> ok.
evict(AdmId) ->
    Key = {adm_session_epoch, AdmId},
    imboy_cache:delete(Key),
    _ = imboy_cache:broadcast({flush, Key}),
    ok.

-spec uid_int(binary()) -> integer().
uid_int(UidBin) ->
    safe_uid(UidBin).

-spec safe_uid(binary()) -> integer() | malformed.
safe_uid(UidBin) ->
    safe_int(UidBin).

-spec safe_int(binary()) -> integer() | malformed.
safe_int(Bin) ->
    try binary_to_integer(Bin) of
        N when is_integer(N) -> N
    catch
        _:_ -> malformed
    end.

-spec safe_b64(binary()) -> binary() | malformed.
safe_b64(Bin) ->
    try base64:decode(Bin) of
        B when is_binary(B) -> B
    catch
        _:_ -> malformed
    end.

-spec cookie_secure() -> boolean().
cookie_secure() ->
    StartMode = config_ds:env(start_mode, http),
    StartMode =:= tls orelse StartMode =:= http_tls.

-spec normalize(term()) -> binary().
normalize(undefined) ->
    <<>>;
normalize(false) ->
    <<>>;
normalize(Value) when is_binary(Value) ->
    Value;
normalize(Value) when is_list(Value) ->
    unicode:characters_to_binary(Value);
normalize(Value) ->
    ec_cnv:to_binary(Value).
