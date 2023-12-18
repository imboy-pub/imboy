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
    Req1 = case Action of
               captcha ->
                   captcha(Req0, State);
               login ->
                   login(Req0, State);
               demo_action ->
                   demo_action(Req0, State);
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


login(Req0, _State) ->
    % {ok, Body} = file:read_file(iolist_to_binary([code:priv_dir(imadm), "/template/login.html"])),
    {ok, Body} = imboy_dtl:template(login_dtl, [
         {name, "IMBoy Admin System"}
         , {public_key, "xxxxxx"}
    ], imadm),

    imboy_log:info(Body),
    cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/html; charset=utf-8">>
        , <<"Access-Control-Allow-Origin">> => <<"*">>
    }, Body, Req0).


demo_action(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    % Uid = imboy_hashids:uid_encode(CurrentUid),

    PostVals = imboy_req:post_params(Req0),
    Val1 = proplists:get_value(<<"val1">>, PostVals, ""),
    Val2 = proplists:get_value(<<"val2">>, PostVals, ""),
    adm_passport_logic:demo(CurrentUid, Val1, Val2),
    imboy_response:success(Req0, PostVals, "success.").


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
