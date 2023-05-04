-module(passport_handler).
-behavior(cowboy_handler).

-export([init/2]).

-include_lib("kernel/include/inet.hrl").
-include_lib("imboy/include/log.hrl").

%% ===================================================================
%% API
%% ===================================================================

init(Req0, State0) ->
    % ?LOG(State),
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 = case Action of
        refreshtoken ->
            refreshtoken(Req0);
        do_login ->
            do_login(Req0);
        do_signup ->
            do_signup(Req0);
        send_code ->
            send_code(Req0);
        find_password ->
            find_password(Req0);
        false ->
            Req0
    end,
    {ok, Req1, State}.


do_login(Req0) ->
    % ?LOG(["peer", cowboy_req:peer(Req0)]),
    % {Ip,_Port} = cowboy_req:peer(Req0),
    % {ok, ClientSocket} = gen_tcp:accept(State#client_state.socket_fd),
    % {ok,{IP_Address,Port}} = inet:peername(ClientSocket),
    % io:format("[~p][~p] ~n", [IP_Address, Port]),
    %%%
    %%% 在POST请求中取出内容
    %%% 用户名account
    %%% 密码 pwd
    PostVals = imboy_req:post_params(Req0),
    % ?LOG(PostVals),
    Type = proplists:get_value(<<"type">>, PostVals, <<"email">>),
    RsaEncrypt = proplists:get_value(<<"rsa_encrypt">>,
                                     PostVals,
                                     <<"1">>),
    Account = proplists:get_value(<<"account">>, PostVals),
    Password = proplists:get_value(<<"pwd">>, PostVals),
    % ?LOG(['Type', Type,'Password', Password]),
    Pwd = case RsaEncrypt == <<"1">> of
        true ->
            imboy_cipher:rsa_decrypt(Password);
        _ ->
            Password
    end,
    Ip = cowboy_req:header(<<"x-forwarded-for">>, Req0),
    % ?LOG(["Ip", Ip]),
    Post2 = [{<<"ip">>, Ip} | PostVals],
    case passport_logic:do_login(Type, Account, Pwd) of
        {ok, Data} ->
            % 检查消息 用异步队列实现
            Uid = proplists:get_value(<<"uid">>, Data),
            % gen_server:call是同步的，gen_server:cast是异步的
            gen_server:cast(user_server, {login_success, Uid, Post2}),
            Setting = user_setting_ds:find_by_uid(Uid),
            imboy_response:success(Req0, [{<<"setting">>, Setting} | Data], "success.");
        {error, Msg} ->
            imboy_response:error(Req0, Msg);
        {error, Msg, Code} ->
            imboy_response:error(Req0, Msg, Code)
    end.


refreshtoken(Req0) ->
    % Token = cowboy_req:header(<<"authorization">>, Req0),
    Refreshtoken = cowboy_req:header(<<"imboy-refreshtoken">>, Req0),
    % ?LOG(["refreshtoken ", Refreshtoken]),
    case throttle:check(refreshtoken, Refreshtoken) of
        {limit_exceeded, _, _} ->
            lager:warning("Auth ~p exceeded api limit~n", [Refreshtoken]),
            cowboy_req:reply(429, Req0);
    _ ->
        case token_ds:decrypt_token(Refreshtoken) of
            {ok, Id, _ExpireAt, <<"rtk">>} ->
                Data = [{<<"token">>, token_ds:encrypt_token(Id)}],
                imboy_response:success(Req0, Data, "success.");
            {error, Code, Msg, _Map} ->
                imboy_response:error(Req0, Msg, Code)
        end
  end.


send_code(Req0) ->
    %%
    %% 在POST请求中取出内容
    %% type 验证码类型 email sms
    %% account 账号 Email 或者 手机号码
    PostVals = imboy_req:post_params(Req0),
    % ?LOG(PostVals),
    Type = proplists:get_value(<<"type">>, PostVals, <<"email">>),
    Account = proplists:get_value(<<"account">>, PostVals),
    case Type of
        <<"email">> ->
            case passport_logic:send_email_code(Account) of
                {ok, _} ->
                    imboy_response:success(Req0, #{}, "success.");
                {error, Msg} ->
                    imboy_response:error(Req0, [], Msg)
            end;
        _ ->
            imboy_response:success(Req0, [], "暂未实现功能.")
    end.


do_signup(Req0) ->
    %%
    %% 在POST请求中取出内容
    %% 用户名account
    %% 密码 pwd
    PostVals = imboy_req:post_params(Req0),
    % ?LOG(PostVals),
    Type = proplists:get_value(<<"type">>, PostVals, <<"email">>),
    Account = proplists:get_value(<<"account">>, PostVals),
    Password = proplists:get_value(<<"pwd">>, PostVals),
    Code = proplists:get_value(<<"code">>, PostVals),
    % 邀请人ID
    % RefUid = proplists:get_value(<<"ref_uid">>, PostVals),
    % RegIp = proplists:get_value(<<"reg_ip">>, PostVals),
    % 注册客服端操作系统
    % RegCos = proplists:get_value(<<"reg_cos">>, PostVals),

    Cosv = cowboy_req:header(<<"cosv">>, Req0),
    Ip = cowboy_req:header(<<"x-forwarded-for">>, Req0, <<"{}">>),
    % ?LOG(["Ip", Ip]),
    Post2 = [{<<"cosv">>, Cosv} | [{<<"ip">>, Ip} | PostVals]],
    case passport_logic:do_signup(Type,
                                  Account,
                                  Password,
                                  Code,
                                  Post2) of
        {ok, Data} ->
            imboy_response:success(Req0, Data, "success.");
        {error, Msg} ->
            imboy_response:error(Req0, Msg);
        {error, Msg, Code} ->
            imboy_response:error(Req0, Msg, Code)
    end.


find_password(Req0) ->
    %%
    %% 在POST请求中取出内容
    %% 用户名account
    %% 密码 pwd
    PostVals = imboy_req:post_params(Req0),
    % ?LOG(PostVals),
    Type = proplists:get_value(<<"type">>, PostVals, <<"email">>),
    Account = proplists:get_value(<<"account">>, PostVals),
    Password = proplists:get_value(<<"pwd">>, PostVals),
    Code = proplists:get_value(<<"code">>, PostVals),
    % 邀请人ID
    % RefUid = proplists:get_value(<<"ref_uid">>, PostVals),
    % RegIp = proplists:get_value(<<"reg_ip">>, PostVals),
    % 注册客服端操作系统
    % RegCos = proplists:get_value(<<"reg_cos">>, PostVals),

    Cosv = cowboy_req:header(<<"cosv">>, Req0),
    Ip = cowboy_req:header(<<"x-forwarded-for">>, Req0),
    % ?LOG(["Ip", Ip]),
    Post2 = [{<<"cosv">>, Cosv} | [{<<"ip">>, Ip} | PostVals]],
    case passport_logic:find_password(Type,
                                      Account,
                                      Password,
                                      Code,
                                      Post2) of
        {ok, Data} ->
            imboy_response:success(Req0, Data, "success.");
        {error, Msg} ->
            imboy_response:error(Req0, Msg);
        {error, Msg, Code} ->
            imboy_response:error(Req0, Msg, Code)
    end.
