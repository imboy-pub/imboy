-module(auth_oidc_logic).

%%%
% OIDC 登录流业务逻辑（P0-C）
% OIDC login flow logic: Authorization Code + PKCE + state/nonce
%
% 流程：
%   authorize/1  读 oauth2 provider 配置 -> 生成 state/nonce/PKCE -> 302 到 IdP
%   callback/2   IP 限流 -> state 一次性消费 -> httpc 换 token -> id_token claims
%                校验(iss/aud/exp/nonce) -> userinfo -> 账号映射/自动建号 ->
%                复用 passport_logic:login_resp/2 签发 imboy JWT
%   exchange/1   App 深链一次性码（OTC）换登录 payload
%
% 安全边界：
%   - 所有失败路径对外统一返回 <<"SSO 登录失败"/utf8>>，细节只进服务端日志
%     （License 配额拒绝除外，402 语义对外可见）。
%   - token_url 必须 https（127.0.0.1/localhost 白名单供本地 E2E）。
%   - MVP 不做 JWKS 验签：id_token 经 TLS 直连 token endpoint 取得，
%     依赖信道 + iss/aud/exp/nonce claims 校验（OIDC Core §3.1.3.7）。
%   - state/otc 存本模块自有 ETS 表（?ONETIME_TAB，单节点内存），消费走
%     ets:take/2（查+删单个原子操作），同一 state/otc 并发下只有一个进程能取到。
%     ponytail: 单节点原子性已解决；仍是节点本地存储 —— 各节点 ETS 独立，
%     多节点部署时 authorize/callback/exchange 必须落在同一节点（现单 upstream
%     成立）。真要多节点无粘性，换 DB/Redis 做跨节点一次性消费。
%%%

-export([authorize/1, callback/2, exchange/1]).
%% 供 imboy_app 启动期建表（长驻属主）与定时清扫
-export([init_table/0, sweep_expired/0]).
%% 多节点一次性状态自检（供测试与 preflight 同口径判断）
-export([state_sharing_status/2, warn_if_state_not_shared/0]).

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").
-include("common.hrl").
-include("error_code.hrl").

-define(PROVIDER, <<"oauth2">>).
%% state 有效期（秒）
-define(STATE_TTL, 600).
%% App 一次性码有效期（秒）
-define(OTC_TTL, 60).
-define(HTTP_TIMEOUT, 5000).
%% state/otc 一次性凭据表（自有 ETS，消费用 ets:take/2 保证原子）
-define(ONETIME_TAB, imboy_oidc_onetime).
%% 过期项清扫周期（毫秒）：只回收无人消费的残留，正确性由 take 时的过期判定保证
-define(SWEEP_INTERVAL, 60000).
-define(DEFAULT_SCOPES, <<"openid profile email">>).
%% 统一对外失败消息（不泄 IdP/配置细节）
-define(FAIL_MSG, <<"SSO 登录失败"/utf8>>).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 构造 IdP authorize 跳转 URL；Client ∈ app | test（存进 state，callback 分流）
-spec authorize(binary()) -> {redirect, binary()} | {error, binary()}.
authorize(Client) ->
    case enabled_provider() of
        {ok, Cfg} ->
            ok = warn_if_state_not_shared(),
            State = rand_token(),
            Nonce = rand_token(),
            Verifier = rand_token(),
            Challenge = b64url(crypto:hash(sha256, Verifier)),
            ok = onetime_put(
                {oidc_state, State},
                #{nonce => Nonce, verifier => Verifier, client => Client},
                ?STATE_TTL
            ),
            AuthUrl = maps:get(<<"auth_url">>, Cfg, <<>>),
            Query = compose_query([
                {<<"response_type">>, <<"code">>},
                {<<"client_id">>, maps:get(<<"client_id">>, Cfg, <<>>)},
                {<<"redirect_uri">>, redirect_uri(Cfg)},
                {<<"scope">>, scopes(Cfg)},
                {<<"state">>, State},
                {<<"nonce">>, Nonce},
                {<<"code_challenge">>, Challenge},
                {<<"code_challenge_method">>, <<"S256">>}
            ]),
            Sep =
                case binary:match(AuthUrl, <<"?">>) of
                    nomatch -> <<"?">>;
                    _ -> <<"&">>
                end,
            {redirect, <<AuthUrl/binary, Sep/binary, Query/binary>>};
        {error, Reason} ->
            ?LOG_WARNING("auth_oidc authorize rejected: ~p", [Reason]),
            {error, ?FAIL_MSG}
    end.

%% @doc IdP 回调：换 token、校验 claims、映射/建号、签发
%% 返回 {ok, LoginPayload}（client=test）| {redirect, DeepLink}（client=app）
-spec callback(map(), binary()) ->
    {ok, map()} | {redirect, binary()} | {error, binary()} | {error, binary(), integer()}.
callback(Params, Ip) ->
    case login_attempt_ds:check_ip_rate_limit(Ip) of
        {error, rate_limited} ->
            ?LOG_WARNING("auth_oidc callback rate limited ip=~ts", [Ip]),
            {error, ?FAIL_MSG};
        _ ->
            do_callback(Params, Ip)
    end.

%% @doc App 深链一次性码换登录 payload（一次性消费）
-spec exchange(binary()) -> {ok, map()} | {error, binary()}.
exchange(Otc) when is_binary(Otc), byte_size(Otc) > 0 ->
    case onetime_take({oidc_otc, Otc}) of
        {ok, Payload} -> {ok, Payload};
        undefined -> {error, ?FAIL_MSG}
    end;
exchange(_) ->
    {error, ?FAIL_MSG}.

%% ===================================================================
%% Internal: callback 流水线
%% ===================================================================

do_callback(#{<<"error">> := IdpErr}, Ip) ->
    %% IdP 返回 error 参数（如用户取消授权）——友好失败，不外泄 IdP 细节
    fail(Ip, {idp_error, IdpErr});
do_callback(Params, Ip) ->
    Code = maps:get(<<"code">>, Params, <<>>),
    State = maps:get(<<"state">>, Params, <<>>),
    case consume_state(State) of
        {ok, StData} when Code =/= <<>> ->
            case enabled_provider() of
                {ok, Cfg} ->
                    callback_with_config(Code, StData, Cfg, Ip);
                {error, Reason} ->
                    fail(Ip, {provider_unavailable, Reason})
            end;
        {ok, _} ->
            fail(Ip, missing_code);
        error ->
            fail(Ip, invalid_or_replayed_state)
    end.

callback_with_config(Code, StData, Cfg, Ip) ->
    case fetch_token(Code, StData, Cfg) of
        {ok, TokenResp} ->
            IdToken = maps:get(<<"id_token">>, TokenResp, <<>>),
            AccessToken = maps:get(<<"access_token">>, TokenResp, <<>>),
            case verify_claims(IdToken, Cfg, maps:get(nonce, StData)) of
                {ok, Claims} ->
                    case fetch_userinfo(Cfg, AccessToken, Claims) of
                        {ok, Sub, Email, Name, Verified} ->
                            case map_or_provision(Sub, Email, Name, Verified) of
                                {ok, Uid} ->
                                    finish_login(Uid, maps:get(client, StData, <<"app">>), Ip);
                                {error, {quota, Msg, ErrCode}} ->
                                    %% License 满额：402 语义对外可见（非敏感信息）
                                    {error, Msg, ErrCode};
                                {error, Reason} ->
                                    fail(Ip, {mapping_failed, Reason})
                            end;
                        {error, Reason} ->
                            fail(Ip, {userinfo_failed, Reason})
                    end;
                {error, Reason} ->
                    fail(Ip, {claims_rejected, Reason})
            end;
        {error, Reason} ->
            fail(Ip, {token_exchange_failed, sanitize_http_error(Reason)})
    end.

%% @doc 脱敏 IdP 出站错误：剥离原始响应体（可能含 token 类字段），只留状态码与长度
-spec sanitize_http_error(term()) -> term().
sanitize_http_error({http_status, Code, Body}) when is_binary(Body) ->
    {http_status, Code, {body_bytes, byte_size(Body)}};
sanitize_http_error(Other) ->
    Other.

%% @doc state 一次性消费：命中即 delete，重放/伪造/过期均失败
-spec consume_state(binary()) -> {ok, map()} | error.
consume_state(<<>>) ->
    error;
consume_state(State) ->
    case onetime_take({oidc_state, State}) of
        {ok, StData} when is_map(StData) -> {ok, StData};
        _ -> error
    end.

%% ===================================================================
%% Internal: 一次性凭据存储（自有 ETS + ets:take/2 原子消费）
%% ===================================================================
%% 安全：state/otc 必须"查到即失效"。depcache 的 get 是调用进程内的裸 ETS 读、
%%       delete 是 gen_server call，两步之间存在并发窗口 —— 实测 32 个并发进程
%%       可用同一个 otc 各换到一份登录 payload。ets:take/2 是单个原子操作
%%       （查+删），并发下有且只有一个调用方拿到值，其余得 []。

%% @doc 多节点一次性状态自检（纯函数，与 deploy/preflight.sh 的 4b 段同口径）。
%%
%% state/otc 存节点本地 ETS：多节点部署时 authorize 落在 A 节点、callback 被
%% 负载均衡打到 B 节点就取不到，登录以「state 无效」失败，表现得像遭攻击。
%% 粘性会话下同一浏览器会话固定打到同一后端，节点本地状态才是安全的。
-spec state_sharing_status(non_neg_integer(), boolean()) -> ok | {error, atom()}.
state_sharing_status(PeerNodeCount, _Sticky) when PeerNodeCount =< 0 ->
    ok;
state_sharing_status(_PeerNodeCount, true) ->
    ok;
state_sharing_status(_PeerNodeCount, false) ->
    {error, oidc_state_not_shared}.

%% @doc authorize 时自检并告警。只记日志不阻断登录：
%% 单节点承载回调时功能其实是好的，硬失败会误伤；真正的硬闸门在 preflight。
-spec warn_if_state_not_shared() -> ok.
warn_if_state_not_shared() ->
    Sticky = application:get_env(imboy, lb_sticky_session, false) =:= true,
    case state_sharing_status(length(nodes()), Sticky) of
        ok ->
            ok;
        {error, oidc_state_not_shared} ->
            ?WARN_LOG(
                "[oidc] 集群 ~p 个对端节点且未声明粘性会话；state/otc 仅存本节点 ETS，"
                "回调若落到其他节点将以 state 无效失败。"
                "请开启负载均衡粘性会话并设置 lb_sticky_session=true，或单节点承载 OIDC 回调。",
                [length(nodes())]
            ),
            ok
    end.

%% @doc 启动期由长驻进程建表（imboy_app 调用）。
%% 必须在此显式建表：否则表由首个惰性建表的 Cowboy 请求进程持有，
%% 请求结束进程退出即销毁全表 -> 所有在途 state/otc 消失 -> 登录中断。
-spec init_table() -> ok.
init_table() ->
    ensure_table().

-spec ensure_table() -> ok.
ensure_table() ->
    case ets:whereis(?ONETIME_TAB) of
        undefined ->
            try
                _ = ets:new(?ONETIME_TAB, [
                    set,
                    public,
                    named_table,
                    {read_concurrency, true},
                    {write_concurrency, true}
                ]),
                %% 只有建表成功者挂清扫定时器（并发建表的失败方在此之前已抛 badarg）
                _ = timer:apply_interval(?SWEEP_INTERVAL, ?MODULE, sweep_expired, []),
                ok
            catch
                error:badarg -> ok
            end;
        _ ->
            ok
    end.

-spec onetime_put(term(), term(), pos_integer()) -> ok.
onetime_put(Key, Value, TtlSec) ->
    ok = ensure_table(),
    true = ets:insert(?ONETIME_TAB, {Key, Value, erlang:system_time(second) + TtlSec}),
    ok.

%% @doc 原子消费：命中且未过期返回 {ok, Value}；未命中/已被他人消费/已过期返回 undefined
-spec onetime_take(term()) -> {ok, term()} | undefined.
onetime_take(Key) ->
    ok = ensure_table(),
    Now = erlang:system_time(second),
    case ets:take(?ONETIME_TAB, Key) of
        [{_, Value, ExpireAt}] when ExpireAt > Now ->
            {ok, Value};
        _ ->
            %% 未命中 / 已被他人消费 / 已过期（过期项已被 take 顺带删除）
            undefined
    end.

%% @doc 回收无人消费的过期残留（未走完流程的 state 会一直占内存；
%% authorize 是未认证端点，不清扫等于给出一条内存增长路径）
-spec sweep_expired() -> ok.
sweep_expired() ->
    case ets:whereis(?ONETIME_TAB) of
        undefined ->
            ok;
        _ ->
            Now = erlang:system_time(second),
            _ = ets:select_delete(?ONETIME_TAB, [
                {{'_', '_', '$1'}, [{'=<', '$1', Now}], [true]}
            ]),
            ok
    end.

%% ===================================================================
%% Internal: IdP 外呼（httpc，同 sso_logic 先例）
%% ===================================================================

fetch_token(Code, StData, Cfg) ->
    TokenUrl = maps:get(<<"token_url">>, Cfg, <<>>),
    Body = compose_query([
        {<<"grant_type">>, <<"authorization_code">>},
        {<<"code">>, Code},
        {<<"redirect_uri">>, redirect_uri(Cfg)},
        {<<"client_id">>, maps:get(<<"client_id">>, Cfg, <<>>)},
        {<<"client_secret">>, maps:get(<<"client_secret">>, Cfg, <<>>)},
        {<<"code_verifier">>, maps:get(verifier, StData, <<>>)}
    ]),
    http_post_form(TokenUrl, Body).

http_post_form(Url, Body) ->
    UrlL = binary_to_list(Url),
    Opts = https_request_opts(),
    Req = {UrlL, [], "application/x-www-form-urlencoded", Body},
    case httpc:request(post, Req, Opts, [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, RespBody}} ->
            decode_json(RespBody);
        {ok, {{_, HttpCode, _}, _, RespBody}} ->
            {error, {http_status, HttpCode, RespBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 出站 HTTPS 请求选项：强制校验证书链 + 主机名（防 MITM 伪造 IdP 响应）
%% 安全：id_token 的信任依赖 TLS 信道完整性（本 MVP 未做 JWKS 验签），
%%       故到 IdP 的出站请求必须 verify_peer，否则中间人可伪造 token/userinfo。
https_request_opts() ->
    SslOpts = [
        {verify, verify_peer},
        {cacerts, public_key:cacerts_get()},
        {depth, 9},
        {customize_hostname_check, [
            {match_fun, public_key:pkix_verify_hostname_match_fun(https)}
        ]}
    ],
    [
        {timeout, ?HTTP_TIMEOUT},
        {connect_timeout, ?HTTP_TIMEOUT},
        {autoredirect, false},
        {ssl, SslOpts}
    ].

%% @doc userinfo_url 已配置则外呼；未配置回退 id_token claims
fetch_userinfo(Cfg, AccessToken, Claims) ->
    case maps:get(<<"userinfo_url">>, Cfg, <<>>) of
        <<>> ->
            userinfo_fields(Claims);
        Url ->
            Headers = [{"authorization", "Bearer " ++ binary_to_list(AccessToken)}],
            Opts = https_request_opts(),
            case
                httpc:request(get, {binary_to_list(Url), Headers}, Opts, [{body_format, binary}])
            of
                {ok, {{_, 200, _}, _, RespBody}} ->
                    case decode_json(RespBody) of
                        {ok, Info} -> userinfo_fields(Info);
                        {error, Reason} -> {error, Reason}
                    end;
                {ok, {{_, HttpCode, _}, _, _}} ->
                    {error, {userinfo_http_status, HttpCode}};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

userinfo_fields(Info) ->
    case maps:get(<<"sub">>, Info, <<>>) of
        <<>> ->
            {error, missing_sub};
        Sub ->
            Email = to_bin(maps:get(<<"email">>, Info, <<>>)),
            Name = to_bin(maps:get(<<"name">>, Info, <<>>)),
            Verified = email_verified(Info),
            {ok, Sub, Email, Name, Verified}
    end.

%% @doc email_verified claim（兼容 boolean 与字符串 "true"）；缺省视为未验证
%% 安全：只有 IdP 明确断言 email 已验证，才允许按 email 关联到现有 imboy 账号，
%%       否则攻击者可在允许自填未验证邮箱的 IdP 上冒用他人 email 接管账号。
email_verified(Info) ->
    case maps:get(<<"email_verified">>, Info, false) of
        true -> true;
        <<"true">> -> true;
        _ -> false
    end.

%% ===================================================================
%% Internal: id_token claims 校验（TLS 信道 + iss/aud/exp/nonce）
%% ===================================================================

verify_claims(IdToken, Cfg, Nonce) ->
    case id_token_payload(IdToken) of
        {ok, Claims} ->
            CfgIss = maps:get(<<"issuer">>, Cfg, <<>>),
            ClientId = maps:get(<<"client_id">>, Cfg, <<>>),
            Iss = maps:get(<<"iss">>, Claims, <<>>),
            Aud = maps:get(<<"aud">>, Claims, <<>>),
            Exp = maps:get(<<"exp">>, Claims, 0),
            TokNonce = maps:get(<<"nonce">>, Claims, <<>>),
            Now = erlang:system_time(second),
            %% 安全：issuer 必须已配置且匹配（留空不再放行，否则 iss 校验形同虚设）
            IssOk = CfgIss =/= <<>> andalso Iss =:= CfgIss,
            AudOk = aud_ok(Aud, ClientId),
            ExpOk = is_integer(Exp) andalso Exp > Now,
            NonceOk = TokNonce =/= <<>> andalso TokNonce =:= Nonce,
            case {IssOk, AudOk, ExpOk, NonceOk} of
                {true, true, true, true} ->
                    {ok, Claims};
                Flags ->
                    {error, {claims_mismatch, Flags}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% aud 可能是字符串或数组（OIDC Core §2）
aud_ok(Aud, ClientId) when is_binary(Aud) -> Aud =:= ClientId;
aud_ok(Aud, ClientId) when is_list(Aud) -> lists:member(ClientId, Aud);
aud_ok(_, _) -> false.

id_token_payload(IdToken) when is_binary(IdToken), IdToken =/= <<>> ->
    case binary:split(IdToken, <<".">>, [global]) of
        [_Header, Payload, _Sig] ->
            try jsone:decode(b64url_decode(Payload), [{object_format, map}]) of
                Claims when is_map(Claims) -> {ok, Claims};
                _ -> {error, bad_id_token}
            catch
                _:_ -> {error, bad_id_token}
            end;
        _ ->
            {error, bad_id_token}
    end;
id_token_payload(_) ->
    {error, bad_id_token}.

%% ===================================================================
%% Internal: 账号映射 / 自动建号（License quota gate）
%% ===================================================================

map_or_provision(Sub, Email, Name, Verified) ->
    case sso_identity_ds:find_uid(?PROVIDER, Sub) of
        {ok, Uid} ->
            {ok, Uid};
        not_found ->
            link_or_provision(Sub, Email, Name, Verified);
        {error, Reason} ->
            {error, Reason}
    end.

%% 已验证 email 匹配现有用户则建映射关联；否则自动建号
%% 安全：email_verified =/= true 时禁止按 email 关联（防冒用他人邮箱接管），退化为独立建号
link_or_provision(Sub, Email, Name, Verified) ->
    ExistingId =
        case {Verified, Email} of
            {true, E} when E =/= <<>> -> user_ds:find_id_by_email(E);
            _ -> 0
        end,
    case ExistingId of
        Id when is_integer(Id), Id > 0 ->
            bind_identity(Sub, Id, Email);
        _ ->
            provision(Sub, Email, Name)
    end.

provision(Sub, Email, Name) ->
    %% License 规模 gate：复用 passport_logic:quota_guard/0（勿绕过）
    case passport_logic:quota_guard() of
        {error, Msg, ErrCode} ->
            {error, {quota, Msg, ErrCode}};
        ok ->
            %% SSO 用户不走密码登录：置随机密码
            RandPwd = b64url(crypto:strong_rand_bytes(32)),
            Base0 = #{<<"password">> => elib_password:generate(RandPwd)},
            Base =
                case Email of
                    <<>> -> Base0;
                    _ -> Base0#{<<"email">> => Email}
                end,
            Data = passport_logic:pick_data_for_insert(Base, #{
                <<"source">> => <<"oidc">>,
                <<"nickname">> => nickname(Name, Sub)
            }),
            case user_ds:insert_and_get_id(Data) of
                {ok, Uid} ->
                    bind_identity(Sub, Uid, Email);
                {error, {error, error, <<"23505">>, unique_violation, _Msg, _Details}} ->
                    %% 并发注册竞态：回读 email 关联
                    case Email =/= <<>> andalso user_ds:find_id_by_email(Email) of
                        Id2 when is_integer(Id2), Id2 > 0 ->
                            bind_identity(Sub, Id2, Email);
                        _ ->
                            {error, provision_conflict}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

bind_identity(Sub, Uid, Email) ->
    case sso_identity_ds:bind(?PROVIDER, Sub, Uid, Email) of
        ok -> {ok, Uid};
        {error, Reason} -> {error, Reason}
    end.

nickname(<<>>, Sub) ->
    %% sub 通常为 ASCII 标识；截断兜底
    Len = min(byte_size(Sub), 30),
    <<Head:Len/binary, _/binary>> = Sub,
    <<"sso_", Head/binary>>;
nickname(Name, _Sub) ->
    Name.

%% ===================================================================
%% Internal: 签发（复用 passport_logic:login_resp/2 唯一收口）
%%
%% E2EE-013 已知缺口：这里签发的是 did-less token（设备级吊销对它无效）。
%% OIDC 流三个入口都拿不到设备 DID：
%%   - authorize：浏览器 GET，只带 client 参数（open 白名单，无 did 头）
%%   - callback ：IdP 重定向，只带 code/state
%%   - exchange ：App POST，只带 otc（且 Payload 已在此处签好）
%% 且本流程全程不写 user_device 行，凭空造一个 did 只会产生幽灵设备。
%% 补齐需要客户端配合：App 在 authorize 或 exchange 上报自己的 did，
%% 随 state/otc 透传到此处，并同步落 user_device 行后再签发。
%% ===================================================================

finish_login(Uid, Client, Ip) ->
    User = user_ds:find_by_id(Uid, ?LOGIN_COLUMN),
    Status = maps:get(<<"status">>, User, -2),
    case map_size(User) > 0 andalso (Status =:= 1 orelse Status =:= 2) of
        true ->
            Payload = passport_logic:login_resp(User, #{}),
            case Client of
                <<"test">> ->
                    {ok, Payload};
                _ ->
                    Otc = rand_token(),
                    ok = onetime_put({oidc_otc, Otc}, Payload, ?OTC_TTL),
                    {redirect, <<"imboy://oidc/callback?otc=", Otc/binary>>}
            end;
        false ->
            fail(Ip, {account_unavailable, Uid, Status})
    end.

%% ===================================================================
%% Internal: provider 配置
%% ===================================================================

enabled_provider() ->
    case sso_config_ds:get_provider(?PROVIDER) of
        {ok, Cfg} ->
            case is_enabled(Cfg) of
                true -> check_token_url(Cfg);
                false -> {error, provider_disabled}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

is_enabled(Cfg) ->
    case maps:get(<<"enabled">>, Cfg, false) of
        true -> true;
        <<"true">> -> true;
        _ -> false
    end.

%% token_url 必须 https；127.0.0.1/localhost 白名单供本地/E2E（防跳过 JWKS 验签被误用于明文信道）
check_token_url(Cfg) ->
    TokenUrl = maps:get(<<"token_url">>, Cfg, <<>>),
    case allowed_url(TokenUrl) of
        true -> {ok, Cfg};
        false -> {error, insecure_token_url}
    end.

allowed_url(<<"https://", _/binary>>) ->
    true;
allowed_url(<<"http://", _/binary>> = Url) ->
    case uri_string:parse(Url) of
        #{host := H} when H =:= <<"127.0.0.1">>; H =:= <<"localhost">> -> true;
        #{host := H} when H =:= "127.0.0.1"; H =:= "localhost" -> true;
        _ -> false
    end;
allowed_url(_) ->
    false.

redirect_uri(Cfg) ->
    %% 优先 provider 配置显式 redirect_uri（本地 E2E / 多域名场景），缺省用 base_url
    case maps:get(<<"redirect_uri">>, Cfg, <<>>) of
        <<>> ->
            Base = ec_cnv:to_binary(config_ds:env(base_url, <<>>)),
            <<Base/binary, "/api/v1/auth/oidc/callback">>;
        Uri ->
            Uri
    end.

scopes(Cfg) ->
    case maps:get(<<"scopes">>, Cfg, <<>>) of
        <<>> -> ?DEFAULT_SCOPES;
        S -> S
    end.

%% ===================================================================
%% Internal: 小工具
%% ===================================================================

%% @doc 统一失败出口：细节只进日志 + 失败计数，对外统一消息
fail(Ip, Detail) ->
    ?LOG_WARNING("auth_oidc callback failed: ~p ip=~ts", [Detail, Ip]),
    _ = login_attempt_ds:record_failure(<<"oidc">>, Ip),
    {error, ?FAIL_MSG}.

rand_token() ->
    b64url(crypto:strong_rand_bytes(32)).

b64url(Bin) ->
    base64:encode(Bin, #{mode => urlsafe, padding => false}).

b64url_decode(Bin) ->
    base64:decode(Bin, #{mode => urlsafe, padding => false}).

compose_query(Pairs) ->
    iolist_to_binary(uri_string:compose_query(Pairs)).

decode_json(Bin) ->
    try jsone:decode(Bin, [{object_format, map}]) of
        Map when is_map(Map) -> {ok, Map};
        _ -> {error, bad_json}
    catch
        _:_ -> {error, bad_json}
    end.

to_bin(V) when is_binary(V) -> V;
to_bin(_) -> <<>>.
