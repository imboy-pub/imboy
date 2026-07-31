-module(adm_auth_middleware).
-dialyzer({nowarn_function, [ip_in_allowlist/2]}).
-behaviour(cowboy_middleware).

-include("log.hrl").

-export([execute/2]).
-export([condition/4]).
-export([remove_last_forward_slash/1]).
-export([sign_admin_cookie/1]).

%% 这个是回调函数
%% @doc 执行认证中间件
-spec execute(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()} | {stop, cowboy_req:req()}.
execute(Req, Env) ->
    % elib_log:info([is_binary(cowboy_req:path(Req)), cowboy_req:path(Req)]),
    Path = remove_last_forward_slash(cowboy_req:path(Req)),
    % elib_log:info(Path),
    case Path of
        <<"/static">> ->
            {ok, Req, Env};
        <<"/static/", _Tail/binary>> ->
            {ok, Req, Env};
        <<"/adm/passport/", _Tail/binary>> ->
            % elib_log:info("passport xxxxxxxxxxxxxx\n"),
            {ok, Req, Env};
        <<"/api/adm/passport/", _Tail/binary>> ->
            % /api 前缀的登录前 passport 接口（captcha/meta/login）同样开放，
            % 与 /adm/passport/ 行为一致；其余 /api/adm/* 仍落 _ 分支校验 admin cookie
            {ok, Req, Env};
        _ ->
            % GAP-12: 在认证前先校验 IP 白名单（配置 adm_ip_allowlist）
            case check_ip_allowlist(Req) of
                deny ->
                    reply_ip_forbidden(Req);
                allow ->
                    % {ok, Req, Env} | {stop, Req}
                    Method = cowboy_req:method(Req),
                    Uid = elib_req:cookie(<<"adm_user_id">>, Req),
                    UidSig = elib_req:cookie(<<"adm_user_sig">>, Req),
                    % elib_log:info([Method, Uid]),
                    condition(Method, Uid, UidSig, Req, Env)
            end
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 条件判断处理
-spec condition(binary(), binary() | undefined, cowboy_req:req(), map()) ->
    {ok, cowboy_req:req(), map()} | {stop, cowboy_req:req()}.
condition(Method, Uid, Req, Env) when is_binary(Uid) ->
    % 向下兼容 condition/4 的调用场景（测试和旧代码），自动补签名
    condition(Method, Uid, sign_admin_cookie(Uid), Req, Env);
condition(Method, Uid, Req, Env) ->
    condition(Method, Uid, false, Req, Env).

-spec condition(
    binary(), binary() | false | undefined, binary() | false | undefined, cowboy_req:req(), map()
) ->
    {ok, cowboy_req:req(), map()} | {stop, cowboy_req:req()}.
condition(Method, Uid, UidSig, Req, Env) ->
    case authorize_admin_cookie(Uid, UidSig) of
        {ok, DecodedUid} ->
            grant_access(DecodedUid, Req, Env);
        error ->
            handle_unauthorized(Method, Req)
    end.

-spec grant_access(integer(), cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
grant_access(DecodedUid, Req, Env) ->
    #{handler_opts := HandlerOpts} = Env,
    Env1 = maps:remove(has_sent_resp, Env),
    Env2 = Env1#{handler_opts := HandlerOpts#{adm_user_id => DecodedUid}},
    {ok, Req, Env2}.

-spec handle_unauthorized(binary(), cowboy_req:req()) -> {stop, cowboy_req:req()}.
handle_unauthorized(<<"GET">>, Req) ->
    Path = remove_last_forward_slash(cowboy_req:path(Req)),
    case should_redirect_to_login(Path) of
        true ->
            Req0 = clear_auth_cookies(Req),
            Uri = cowboy_req:uri(Req),
            Req1 = cowboy_req:set_resp_cookie(
                <<"back_uri">>,
                Uri,
                Req0,
                #{
                    path => <<"/">>,
                    http_only => true,
                    same_site => lax,
                    secure => cookie_secure()
                }
            ),
            Location = "/adm/passport/login",
            Req2 = cowboy_req:reply(
                302,
                #{<<"Location">> => Location},
                Req1
            ),
            {stop, Req2};
        false ->
            unauthorized_api_response(Req)
    end;
handle_unauthorized(<<"POST">>, Req) ->
    unauthorized_api_response(Req);
handle_unauthorized(_, Req) ->
    unauthorized_api_response(Req).

-spec clear_auth_cookies(cowboy_req:req()) -> cowboy_req:req().
clear_auth_cookies(Req) ->
    Req1 = clear_cookie(<<"adm_user_id">>, Req),
    clear_cookie(<<"adm_user_sig">>, Req1).

-spec clear_cookie(binary(), cowboy_req:req()) -> cowboy_req:req().
clear_cookie(Name, Req) ->
    cowboy_req:set_resp_cookie(
        Name,
        <<>>,
        Req,
        #{
            path => <<"/">>,
            max_age => 0,
            http_only => true,
            same_site => lax,
            secure => cookie_secure()
        }
    ).

%% @doc 管理后台 Cookie 授权校验
-spec authorize_admin_cookie(binary() | false | undefined, binary() | false | undefined) ->
    {ok, integer()} | error.
authorize_admin_cookie(Uid, UidSig) when is_binary(Uid), is_binary(UidSig) ->
    case verify_admin_cookie(Uid, UidSig) of
        true ->
            decode_uid(Uid);
        false ->
            maybe_authorize_with_legacy_cookie(Uid)
    end;
authorize_admin_cookie(Uid, _) when is_binary(Uid) ->
    maybe_authorize_with_legacy_cookie(Uid);
authorize_admin_cookie(_, _) ->
    error.

%% @doc 可选的旧 Cookie 兼容逻辑（仅限 dev/local 环境，生产环境强制禁用）
-spec maybe_authorize_with_legacy_cookie(binary()) -> {ok, integer()} | error.
maybe_authorize_with_legacy_cookie(Uid) ->
    case is_prod_env() of
        true ->
            % 生产环境强制禁用 legacy cookie，防止绕过签名验证
            error;
        false ->
            case config_ds:env(adm_auth_legacy_cookie_enabled, false) of
                true ->
                    decode_uid(Uid);
                _ ->
                    error
            end
    end.

%% @doc 判断当前是否为生产环境
-spec is_prod_env() -> boolean().
is_prod_env() ->
    Env = ec_cnv:to_binary(config_ds:env(env, <<"local">>)),
    Env =:= <<"pro">> orelse Env =:= <<"prod">> orelse Env =:= <<"production">>.

-spec decode_uid(binary()) -> {ok, integer()} | error.
decode_uid(Uid) ->
    try ec_cnv:to_integer(Uid) of
        DecodedUid when is_integer(DecodedUid), DecodedUid > 0 ->
            {ok, DecodedUid};
        _ ->
            error
    catch
        _:_ ->
            error
    end.

%% @doc 生成管理后台 Cookie 签名（HMAC-SHA256）
-spec sign_admin_cookie(binary()) -> binary().
sign_admin_cookie(Uid) when is_binary(Uid) ->
    elib_hasher:hmac_sha256(Uid, signing_key()).

%% @doc 验证管理后台 Cookie 签名
-spec verify_admin_cookie(binary() | false | undefined, binary() | false | undefined) -> boolean().
verify_admin_cookie(Uid, UidSig) when is_binary(Uid), is_binary(UidSig) ->
    Expected = sign_admin_cookie(Uid),
    UidSig =/= <<>> andalso Expected =:= UidSig;
verify_admin_cookie(_, _) ->
    false.

%% Remove the last forward slash
%% 删除最后一个正斜杠
%% auth_middleware:remove_last_forward_slash(<<"/abc/">>).
%%  will be echo <<"/abc">>
-spec remove_last_forward_slash(binary()) -> binary().
remove_last_forward_slash(<<"">>) ->
    <<"/">>;
remove_last_forward_slash(<<"/">>) ->
    <<"/">>;
remove_last_forward_slash(Path) ->
    case binary:last(Path) of
        47 ->
            binary:part(Path, 0, byte_size(Path) - 1);
        _ ->
            Path
    end.

%% @doc 判断是否需要为 Cookie 设置 secure 属性
-spec cookie_secure() -> boolean().
cookie_secure() ->
    StartMode = config_ds:env(start_mode, http),
    StartMode =:= tls orelse StartMode =:= http_tls.

%% @doc 管理后台 Cookie 签名密钥
%% 始终使用独立的 adm_cookie_secret，不复用 jwt_key，确保密钥隔离
-spec signing_key() -> binary().
signing_key() ->
    normalize_binary(config_ds:env(adm_cookie_secret, <<"imboy-adm-cookie">>)).

-spec normalize_binary(term()) -> binary().
normalize_binary(undefined) ->
    <<>>;
normalize_binary(false) ->
    <<>>;
normalize_binary(Value) when is_binary(Value) ->
    Value;
normalize_binary(Value) when is_list(Value) ->
    unicode:characters_to_binary(Value);
normalize_binary(Value) ->
    ec_cnv:to_binary(Value).

%% @doc GAP-12: 检查请求 IP 是否在管理后台访问白名单中
%% 从 config_ds 读取 adm_ip_allowlist（字符串 CIDR 列表），空列表表示不启用 IP 限制。
%% CIDR 支持：精确 IP（"192.168.1.1"）和前缀匹配（"192.168.1."）。
%% 如需完整 CIDR 解析，可替换为 inet_cidr 库。
-spec check_ip_allowlist(cowboy_req:req()) -> allow | deny.
check_ip_allowlist(Req) ->
    Allowlist = config_ds:env(adm_ip_allowlist, []),
    case Allowlist of
        [] ->
            %% 未配置白名单，不启用 IP 限制（向下兼容）
            allow;
        _ ->
            ClientIp = elib_req:get_client_ip(Req),
            case ip_in_allowlist(ClientIp, Allowlist) of
                true ->
                    allow;
                false ->
                    ok = ?WARN_LOG([adm_ip_blocked, #{ip => ClientIp, path => cowboy_req:path(Req)}]),
                    deny
            end
    end.

%% @doc 判断 IP 是否在白名单内（精确匹配或前缀匹配）。
%% 实现已提升到 elib_req 作为唯一真源（支付回调白名单 A-04 复用同一语义），
%% 本函数保留为薄委托：与原实现的唯一差别是空串条目不再命中所有 IP。
-spec ip_in_allowlist(binary() | undefined, [binary() | list()]) -> boolean().
ip_in_allowlist(Ip, Allowlist) ->
    elib_req:ip_in_allowlist(Ip, Allowlist).

%% @doc 返回 403 Forbidden（IP 不在白名单）
-spec reply_ip_forbidden(cowboy_req:req()) -> {stop, cowboy_req:req()}.
reply_ip_forbidden(Req) ->
    Body = jsone:encode(
        #{
            <<"code">> => 403,
            <<"msg">> => <<"Access denied: IP not in allowlist">>
        },
        [native_utf8]
    ),
    Req1 = cowboy_req:reply(
        403,
        #{<<"content-type">> => <<"application/json; charset=utf-8">>},
        Body,
        Req
    ),
    {stop, Req1}.

%% @doc 仅页面入口使用 302 跳转，API 统一返回 401 JSON
-spec should_redirect_to_login(binary()) -> boolean().
should_redirect_to_login(<<"/adm">>) ->
    true;
should_redirect_to_login(<<"/adm/index">>) ->
    true;
should_redirect_to_login(_) ->
    false.

%% @doc 未授权 API 响应（401 + JSON envelope）
-spec unauthorized_api_response(cowboy_req:req()) -> {stop, cowboy_req:req()}.
unauthorized_api_response(Req) ->
    Req0 = clear_auth_cookies(Req),
    Body = jsone:encode(
        #{
            <<"code">> => 706,
            <<"msg">> => <<"Need to log in again">>,
            <<"sv_ts">> => elib_dt:millisecond(),
            <<"payload">> => #{}
        },
        [native_utf8]
    ),
    Req1 = cowboy_req:reply(
        401,
        #{
            <<"content-type">> => <<"application/json; charset=utf-8">>,
            <<"Referrer-Policy">> => <<"strict-origin-when-cross-origin">>
        },
        Body,
        Req0
    ),
    {stop, Req1}.
