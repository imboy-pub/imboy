-module(adm_passport_handler).
%%%
% adm_passport 控制器模块
% adm_passport controller module
%%%
-behavior(cowboy_rest).

-export([init/2]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("imlib/include/log.hrl").
-include_lib("imlib/include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================


init(Req0, State0) ->
    % ?LOG(State),
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

captcha(Req, _State) ->
    %CryptKey用于验证的时候用，需本地保存，CapCode为用户提交的数据
    %simple_captcha:check(CryptKey, CapCode)
    {CryptKey, BinPng} =  simple_captcha:create(),

    Req2 = cowboy_req:set_resp_cookie(<<"captcha_key">>, CryptKey, Req),
    cowboy_req:reply(200, #{
        <<"content-type">> => <<"image/png; charset=utf-8">>
        , <<"Access-Control-Allow-Origin">> => <<"*">>
    }, BinPng, Req2).


login(<<"GET">>, Req0, _State) ->
    Csrf = imboy_func:uid("csrf"),
    imboy_cache:set(Csrf, 1),
    % cowboy_req:set_resp_cookie("csrf_token", Csrf, Req0),
    {ok, Body} = imboy_dtl:template(login_dtl, [
         {name, "IMBoy Admin System"}
        , {csrf_token, Csrf}
        , {public_key, re:replace(config_ds:get("login_rsa_pub_key"), "\\n", "", [global, {return, list}])}
    ], imadm),

    cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/html; charset=utf-8">>
        , <<"Access-Control-Allow-Origin">> => <<"*">>
    }, Body, Req0);
login(<<"POST">>, Req0, _State) ->
    % CurrentUid = maps:get(current_uid, State),
    % Uid = imboy_hashids:uid_encode(CurrentUid),
    CryptKey = imboy_req:cookie(<<"captcha_key">>, Req0),
    % ?LOG(['CryptKey ', CryptKey]),
    PostVals = imboy_req:post_params(Req0),
    Captcha = proplists:get_value(<<"captcha">>, PostVals, ""),
    Csrf = proplists:get_value(<<"csrf_token">>, PostVals, ""),
    CsrfVal = imboy_cache:get(Csrf),
    % CryptKeyFromEts = simple_captcha_ets:find(Code),
    case {CsrfVal, simple_captcha:check(CryptKey, Captcha)} of
        {{ok, 1}, true} ->
            Account = proplists:get_value(<<"account">>, PostVals),
            Pwd = proplists:get_value(<<"pwd">>, PostVals),
            Password = imboy_cipher:rsa_decrypt(Pwd),
            % ?LOG([Account, 'pwd ', Password]),
            case adm_passport_logic:do_login(Account, Password) of
                {ok, AdmUser} ->
                    imboy_cache:flush(Csrf),
                    #{<<"id">> := AdmUserId} = AdmUser,
                    % ?LOG(['AdmUserId ', AdmUserId]),

                    Req1 = cowboy_req:set_resp_cookie(<<"adm_user_id">>
                        , AdmUserId
                        , Req0
                        , #{path => <<"/">>}),
                    Next = case imboy_req:cookie(<<"back_uri">>, Req0) of
                        BackUri when is_binary(BackUri) ->
                            BackUri;
                        _ ->
                            % 必须是binnary
                            <<"/adm/">>
                    end,
                    % ?LOG(["NextNextNextNextNextNext", Next]),
                    imboy_response:success(Req1, AdmUser#{next => Next}, "操作成功.");
                {error, Msg} ->
                    imboy_response:error(Req0, Msg);
                {error, Msg, Code} ->
                    imboy_response:error(Req0, Msg, Code)
            end;
        {{ok, 1}, _} ->
            imboy_response:error(Req0, "验证码有误");
        {_, _} ->
            imboy_response:error(Req0, "Csrf token error.")
    end.

%% ===================================================================
%% EUnit tests.
%% ===================================================================

-ifdef(EUNIT).
%addr_test_() ->
%    [?_assert(is_public_addr(?PUBLIC_IPV4ADDR)),
%     ?_assert(is_public_addr(?PUBLIC_IPV6ADDR)),
%     ?_test(my_if_addr(inet)),
%     ?_test(my_if_addr(inet6))].
-endif.
