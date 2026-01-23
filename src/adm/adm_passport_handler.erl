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
    % ?DEBUG_LOG(State),
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 = case Action of
               captcha ->
                   captcha(Req0, State);
               login ->
                   login(Method, Req0, State);
               % demo_action ->
               %     demo_action(Req0, State);
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
-spec captcha(cowboy_req:req(), map()) -> cowboy_req:req().
captcha(Req, _State) ->
    %CryptKey用于验证的时候用，需本地保存，CapCode为用户提交的数据
    %simple_captcha:check(CryptKey, CapCode)
    {CryptKey, BinPng} =  simple_captcha:create(),

    Req2 = cowboy_req:set_resp_cookie(<<"captcha_key">>, CryptKey, Req),
    cowboy_req:reply(200, #{
        <<"content-type">> => <<"image/png; charset=utf-8">>
        , <<"Access-Control-Allow-Origin">> => <<"*">>
    }, BinPng, Req2).


%% @doc 处理登录页面请求
%% 返回包含 CSRF 令牌和 RSA 公钥的登录页面
%% @param Method HTTP 方法（GET）
%% @param Req0 Cowboy 请求对象
%% @param State 状态映射
%% @return cowboy_req:req() 更新后的请求对象
-spec login(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
login(<<"GET">>, Req0, _State) ->
    Csrf = elib_id:gen("csrf"),
    imboy_cache:set(Csrf, 1),
    % cowboy_req:set_resp_cookie("csrf_token", Csrf, Req0),
    {ok, Body} = imboy_dtl:template(login_dtl, [
         {system_name, "IMBoy Admin System"}
        , {csrf_token, Csrf}
        , {public_key, re:replace(config_ds:get(<<"login_rsa_pub_key">>), "\\n", "", [global, {return, list}])}
    ], imboy),

    cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/html; charset=utf-8">>
        , <<"Access-Control-Allow-Origin">> => <<"*">>
    }, Body, Req0);

%% @doc 处理登录表单提交
%% 验证用户名密码、验证码和 CSRF 令牌，完成用户认证
%% @param Method HTTP 方法（POST）
%% @param Req0 Cowboy 请求对象
%% @param State 状态映射
%% @return cowboy_req:req() 更新后的请求对象
login(<<"POST">>, Req0, _State) ->
    % CurrentUid = auth_ds:current_uid(State),
    % Uid = elib_hashids:encode(CurrentUid),
    CryptKey = elib_req:cookie(<<"captcha_key">>, Req0),
    % ?DEBUG_LOG(['CryptKey ', CryptKey]),
    PostVals = elib_param:post(Req0),
    Captcha = maps:get(<<"captcha">>, PostVals, ""),
    Csrf = maps:get(<<"csrf_token">>, PostVals, ""),
    CsrfVal = imboy_cache:get(Csrf),
    % CryptKeyFromEts = simple_captcha_ets:find(Code),
    case {CsrfVal, simple_captcha:check(CryptKey, Captcha)} of
        {{ok, 1}, true} ->
            Account = maps:get(<<"account">>, PostVals, undefined),
            Pwd = maps:get(<<"pwd">>, PostVals, undefined),
            Password = elib_cipher:rsa_decrypt(Pwd),
            % ?DEBUG_LOG([Account, 'pwd ', Password]),
            case adm_passport_logic:do_login(Account, Password) of
                {ok, AdmUser} ->
                    imboy_cache:flush(Csrf),
                    #{<<"id">> := AdmUserId} = AdmUser,
                    % ?DEBUG_LOG(['AdmUserId ', AdmUserId]),

                    Req1 = cowboy_req:set_resp_cookie(<<"adm_user_id">>
                        , AdmUserId
                        , Req0
                        , #{path => <<"/">>}),
                    Next = case elib_req:cookie(<<"back_uri">>, Req0) of
                        BackUri when is_binary(BackUri) ->
                            BackUri;
                        _ ->
                            % 必须是binnary
                            <<"/adm/">>
                    end,
                    % ?DEBUG_LOG(["NextNextNextNextNextNext", Next]),
                    elib_response:success(Req1, AdmUser#{next => Next}, "操作成功.");
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end;
        {{ok, 1}, _} ->
            elib_response:error(Req0, "验证码有误");
        {_, _} ->
            elib_response:error(Req0, "Csrf token error.")
    end.

%% ===================================================================
%% EUnit tests.
%% ===================================================================

