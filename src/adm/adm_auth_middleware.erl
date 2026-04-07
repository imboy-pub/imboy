-module(adm_auth_middleware).
-behaviour(cowboy_middleware).

-include("log.hrl").

-export([execute/2]).
-export([condition/4]).
-export([remove_last_forward_slash/1]).
-export([sign_admin_cookie/1, verify_admin_cookie/2]).


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
        _ ->
            % {ok, Req, Env} | {stop, Req}
            Method = cowboy_req:method(Req),
            Uid = elib_req:cookie(<<"adm_user_id">>, Req),
            UidSig = elib_req:cookie(<<"adm_user_sig">>, Req),
            % elib_log:info([Method, Uid]),
            condition(Method, Uid, UidSig, Req, Env)
    end.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 条件判断处理
-spec condition(binary(), binary() | undefined, cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()} | {stop, cowboy_req:req()}.
condition(Method, Uid, Req, Env) when is_binary(Uid) ->
    % 向下兼容 condition/4 的调用场景（测试和旧代码），自动补签名
    condition(Method, Uid, sign_admin_cookie(Uid), Req, Env);
condition(Method, Uid, Req, Env) ->
    condition(Method, Uid, false, Req, Env).

-spec condition(binary(), binary() | false | undefined, binary() | false | undefined, cowboy_req:req(), map()) ->
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
                    path => <<"/adm">>,
                    http_only => true,
                    same_site => lax,
                    secure => cookie_secure()
                }
            ),
            Location = "/adm/passport/login",
            Req2 = cowboy_req:reply(302
                , #{<<"Location">> => Location}
                , Req1),
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
            path => <<"/adm">>,
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

%% @doc 可选的旧 Cookie 兼容逻辑（默认关闭）
-spec maybe_authorize_with_legacy_cookie(binary()) -> {ok, integer()} | error.
maybe_authorize_with_legacy_cookie(Uid) ->
    case config_ds:env(adm_auth_legacy_cookie_enabled, false) of
        true ->
            decode_uid(Uid);
        _ ->
            error
    end.

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
-spec signing_key() -> binary().
signing_key() ->
    JwtKey = normalize_binary(config_ds:env(jwt_key, <<>>)),
    case JwtKey of
        <<>> ->
            normalize_binary(config_ds:env(hashids_salt, <<"imboy-adm-cookie">>));
        _ ->
            JwtKey
    end.

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
    Body = jsone:encode(#{
        <<"code">> => 706,
        <<"msg">> => <<"Need to log in again">>,
        <<"sv_ts">> => elib_dt:millisecond(),
        <<"payload">> => #{}
    }, [native_utf8]),
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
