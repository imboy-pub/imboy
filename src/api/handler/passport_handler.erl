-module(passport_handler).
-behavior(cowboy_handler).

-export([init/2]).

-include("common.hrl").

init(Req0, State) ->
    Req1 = case lists:keyfind(action, 1, State) of
        {action, do_login} ->
            do_login(Req0);
        false ->
            Req0
    end,
    {ok, Req1, State}.

do_login(Req0) ->
    %%%
    %%% 在POST请求中取出内容
    %%% 用户名ＮＡＭＥ
    %%% 密码 ＰＡＳＳＷＤ
    {ok, PostVals, _Req} = cowboy_req:read_urlencoded_body(Req0),
    % ?LOG(PostVals),
    RsaEncrypt = proplists:get_value(<<"rsa_encrypt">>, PostVals, <<"1">>),
    Account = proplists:get_value(<<"account">>, PostVals),
    Password = proplists:get_value(<<"pwd">>, PostVals),
    ?LOG(['Password', Password]),
    Pwd = if
        RsaEncrypt == <<"1">> ->
            imboy_cipher:rsa_decrypt(Password);
        true ->
            Password
    end,
    case user_as:do_login(Account, Pwd) of
        {ok, Data} ->
            resp_json_dto:success(Req0, Data, "操作成功.");
        {error, Msg} ->
            resp_json_dto:error(Req0, Msg);
        {error, Msg, Code} ->
            resp_json_dto:error(Req0, Msg, Code)
    end.

