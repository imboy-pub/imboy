-module(adm_passport_handler).
%%%
% adm_passport 控制器模块
% adm_passport controller module
%%%
-behavior(cowboy_rest).

-export([init/2]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include("common.hrl").

-define(ADM_TEST_CAPTCHA, <<"1234">>).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 初始化认证通行证处理器
%% 根据请求中的 action 参数分发到不同的处理函数
%% @param Req0 Cowboy 请求对象
%% @param State0 状态映射，包含 action 等信息
%% @return {ok, Req, State} 更新后的请求和状态
-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 = case Action of
               captcha ->
                   captcha(Req0, State);
               meta ->
                   meta(Method, Req0, State);
               login ->
                   login(Method, Req0, State);
               do_login ->
                   login(Method, Req0, State);
               logout ->
                   logout(Method, Req0, State);
               false ->
                   Req0
           end,
    {ok, Req1, State}.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 生成并返回验证码图片
%% 创建验证码图片并设置相关的 Cookie 信息
%% @param Req Cowboy 请求对象
%% @param State 状态映射
%% @return cowboy_req:req() 更新后的请求对象
%%
%% 容错：simple_captcha:create/0 依赖 ImageMagick 的 `convert` CLI；
%% 未安装时 os:cmd 静默返回空串，导致 file:read_file 拿到 {error,enoent}
%% 然后 badmatch 崩溃整个 cowboy stream。
%% 这里包裹 try/catch，缺依赖时返回 503 + JSON，不阻塞 adm 进程。
-spec captcha(cowboy_req:req(), map()) -> cowboy_req:req().
captcha(Req, _State) ->
    try simple_captcha:create() of
        {CryptKey, BinPng} ->
            Req2 = cowboy_req:set_resp_cookie(
                <<"captcha_key">>,
                CryptKey,
                Req,
                #{
                    path => <<"/adm/passport">>,
                    http_only => true,
                    same_site => lax,
                    secure => cookie_secure()
                }
            ),
            cowboy_req:reply(200, #{
                <<"content-type">> => <<"image/png; charset=utf-8">>
            }, BinPng, Req2)
    catch
        Class:Reason:Stack ->
            ?ERROR_LOG("simple_captcha:create failed ~p:~p ~p",
                [Class, Reason, Stack]),
            cowboy_req:reply(503, #{
                <<"content-type">> => <<"application/json; charset=utf-8">>
            }, jsx:encode(#{
                code => 503,
                msg => <<"captcha_unavailable">>,
                hint => <<"ImageMagick `convert` missing; run: brew install imagemagick"/utf8>>
            }), Req)
    end.

%% @doc 处理登录页面请求
%% 返回包含 CSRF 令牌和 RSA 公钥的登录页面
-spec login(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
login(<<"GET">>, Req0, _State) ->
    Meta = build_login_meta(),
    Data = [
        {system_name, binary_to_list(maps:get(<<"system_name">>, Meta, <<"IMBoy Admin System">>))},
        {csrf_token, maps:get(<<"csrf_token">>, Meta, <<>>)},
        {public_key, binary_to_list(maps:get(<<"public_key">>, Meta, <<>>))}
    ],
    {ok, Body} = imboy_dtl:template(login_dtl, Data, imboy),
    cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/html; charset=utf-8">>
    }, Body, Req0);

%% @doc 处理登录表单提交
%% 验证用户名密码、验证码和 CSRF 令牌，完成用户认证
login(<<"POST">>, Req0, _State) ->
    CryptKey = elib_req:cookie(<<"captcha_key">>, Req0),
    PostVals = elib_param:post(Req0),
    Captcha = maps:get(<<"captcha">>, PostVals, ""),
    Csrf = maps:get(<<"csrf_token">>, PostVals, ""),
    case imboy_cache:get(Csrf) of
        {ok, 1} ->
            case safe_captcha_check(CryptKey, Captcha) of
                true ->
                    Account = maps:get(<<"account">>, PostVals, undefined),
                    Pwd = maps:get(<<"pwd">>, PostVals, undefined),
                    case decrypt_password(Pwd) of
                        <<>> ->
                            elib_response:error(Req0, <<"密码有误"/utf8>>);
                        Password ->
                            case adm_passport_logic:do_login(Account, Password) of
                                {ok, AdmUser} ->
                                    imboy_cache:flush(Csrf),
                                    #{<<"id">> := AdmUserId} = AdmUser,
                                    Req1 = cowboy_req:set_resp_cookie(
                                        <<"adm_user_id">>,
                                        AdmUserId,
                                        Req0,
                                        #{
                                            path => <<"/adm">>,
                                            http_only => true,
                                            same_site => lax,
                                            secure => cookie_secure()
                                        }
                                    ),
                                    AdmUserSig = adm_auth_middleware:sign_admin_cookie(AdmUserId),
                                    Req2 = cowboy_req:set_resp_cookie(
                                        <<"adm_user_sig">>,
                                        AdmUserSig,
                                        Req1,
                                        #{
                                            path => <<"/adm">>,
                                            http_only => true,
                                            same_site => lax,
                                            secure => cookie_secure()
                                        }
                                    ),
                                    Next = case elib_req:cookie(<<"back_uri">>, Req0) of
                                        BackUri when is_binary(BackUri) ->
                                            BackUri;
                                        _ ->
                                            <<"/adm/">>
                                    end,
                                    Req3 = clear_cookie(<<"back_uri">>, Req2, <<"/adm">>),
                                    RespData = maps:put(<<"next">>, Next, AdmUser),
                                    elib_response:success(Req3, RespData, "操作成功.");
                                {error, Msg} ->
                                    elib_response:error(Req0, Msg)
                            end
                    end;
                false ->
                    elib_response:error(Req0, "验证码有误")
            end;
        _ ->
            elib_response:error(Req0, "Csrf token error.")
    end;
login(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% @doc 解密登录密码，失败时返回空二进制，避免请求进程崩溃
-spec decrypt_password(term()) -> binary().
decrypt_password(Pwd) ->
    try elib_cipher:rsa_decrypt(Pwd) of
        Password when is_binary(Password) -> Password;
        _ -> <<>>
    catch
        _:_ -> <<>>
    end.

-spec safe_captcha_check(term(), term()) -> boolean().
safe_captcha_check(CryptKey, ?ADM_TEST_CAPTCHA) ->
    case admin_test_captcha_enabled() of
        true -> true;
        false -> safe_captcha_check_real(CryptKey, ?ADM_TEST_CAPTCHA)
    end;
safe_captcha_check(CryptKey, Captcha) ->
    safe_captcha_check_real(CryptKey, Captcha).

-spec safe_captcha_check_real(term(), term()) -> boolean().
safe_captcha_check_real(CryptKey, Captcha) ->
    try simple_captcha:check(CryptKey, Captcha) of
        true -> true;
        _ -> false
    catch
        _:_ -> false
    end.

-spec admin_test_captcha_enabled() -> boolean().
admin_test_captcha_enabled() ->
    %% imboy_env:current/0 已统一 OS env (IMBOYENV) > application env (imboy.env) 优先级
    lists:member(imboy_env:current(), [<<"local">>, <<"dev">>, <<"test">>]).

%% @doc 管理后台登录元数据
-spec meta(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
meta(<<"GET">>, Req0, _State) ->
    elib_response:success(Req0, build_login_meta());
meta(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% @doc 处理退出登录
%% 清理管理后台认证相关 Cookie，并返回成功响应
-spec logout(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
logout(<<"POST">>, Req0, _State) ->
    Req1 = clear_cookie(<<"adm_user_id">>, Req0, <<"/adm">>),
    Req2 = clear_cookie(<<"adm_user_sig">>, Req1, <<"/adm">>),
    Req3 = clear_cookie(<<"back_uri">>, Req2, <<"/adm">>),
    elib_response:success(Req3, #{}, <<"退出成功"/utf8>>);
logout(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

-spec clear_cookie(binary(), cowboy_req:req(), binary()) -> cowboy_req:req().
clear_cookie(Name, Req, Path) ->
    cowboy_req:set_resp_cookie(
        Name,
        <<>>,
        Req,
        #{
            path => Path,
            max_age => 0,
            http_only => true,
            same_site => lax,
            secure => cookie_secure()
        }
    ).

%% @doc 判断是否需要为 Cookie 设置 secure 属性
-spec cookie_secure() -> boolean().
cookie_secure() ->
    StartMode = config_ds:env(start_mode, http),
    StartMode =:= tls orelse StartMode =:= http_tls.

-spec build_login_meta() -> map().
build_login_meta() ->
    Csrf = elib_id:gen("csrf"),
    imboy_cache:set(Csrf, 1),
    PublicKey = re:replace(
        config_ds:env(login_rsa_pub_key),
        "\\n",
        "",
        [global, {return, binary}]
    ),
    #{
        <<"system_name">> => <<"IMBoy Admin System">>,
        <<"csrf_token">> => Csrf,
        <<"public_key">> => PublicKey
    }.

%% ===================================================================
%% EUnit tests.
%% ===================================================================

safe_captcha_check_allows_fixed_code_in_local_env_test_() ->
    {setup,
        fun snapshot_runtime_env/0,
        fun restore_runtime_env/1,
        fun() ->
            os:putenv("IMBOYENV", "local"),
            application:set_env(imboy, env, prod),
            ?assertEqual(true, safe_captcha_check(undefined, ?ADM_TEST_CAPTCHA)),
            ?assertEqual(false, safe_captcha_check(undefined, <<"wrong">>))
        end}.

safe_captcha_check_respects_prod_env_without_bypass_test_() ->
    {setup,
        fun snapshot_runtime_env/0,
        fun restore_runtime_env/1,
        fun() ->
            os:putenv("IMBOYENV", "prod"),
            application:set_env(imboy, env, prod),
            ?assertEqual(false, safe_captcha_check(undefined, ?ADM_TEST_CAPTCHA))
        end}.

snapshot_runtime_env() ->
    {os:getenv("IMBOYENV"), application:get_env(imboy, env)}.

restore_runtime_env({PrevImboyEnv, PrevAppEnv}) ->
    case PrevImboyEnv of
        false -> os:unsetenv("IMBOYENV");
        PrevOsValue -> os:putenv("IMBOYENV", PrevOsValue)
    end,
    case PrevAppEnv of
        {ok, PrevEnvValue} -> application:set_env(imboy, env, PrevEnvValue);
        undefined -> application:unset_env(imboy, env)
    end.
