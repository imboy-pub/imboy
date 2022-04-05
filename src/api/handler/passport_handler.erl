-module(passport_handler).
-behavior(cowboy_handler).

-export([init/2]).

-include_lib("kernel/include/inet.hrl").
-include("common.hrl").

init(Req0, State) ->
    Req1 = case lists:keyfind(action, 1, State) of
        {action, refreshtoken} ->
            refreshtoken(Req0);
        {action, do_login} ->
            do_login(Req0);
        {action, do_signup} ->
            do_signup(Req0);
        {action, send_code} ->
            send_code(Req0);
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
    {ok, PostVals, _Req} = cowboy_req:read_urlencoded_body(Req0),
    % ?LOG(PostVals),
    Type = proplists:get_value(<<"type">>, PostVals, <<"email">>),
    RsaEncrypt = proplists:get_value(<<"rsa_encrypt">>, PostVals, <<"1">>),
    Account = proplists:get_value(<<"account">>, PostVals),
    Password = proplists:get_value(<<"pwd">>, PostVals),
    % ?LOG(['Type', Type,'Password', Password]),
    Pwd = if
        RsaEncrypt == <<"1">> ->
            imboy_cipher:rsa_decrypt(Password);
        true ->
            Password
    end,
    {Ip, _Port} = cowboy_req:peer(Req0),
    Ip2 = list_to_binary(lists:flatten(io_lib:format("~w", [Ip]))),
    % ?LOG(["Ip", Ip, "port", Port]),
    Cosv = cowboy_req:header(<<"cosv">>, Req0),
    Post2 = [{<<"cosv">>, Cosv} | [{<<"ip">>, Ip2} | PostVals]],
    case passport_logic:do_login(Type, Account, Pwd) of
        {ok, Data} ->
            % 检查消息 用异步队列实现
            Uid = proplists:get_value(<<"uid">>, Data),
            gen_server:cast(user_server, {login_success, Uid, Post2}),
            resp_json_dto:success(Req0, Data, "操作成功.");
        {error, Msg} ->
            resp_json_dto:error(Req0, Msg);
        {error, Msg, Code} ->
            resp_json_dto:error(Req0, Msg, Code)
    end.

refreshtoken(Req0) ->
    % Token = cowboy_req:header(<<"authorization">>, Req0),
    Refreshtoken = cowboy_req:header(<<"imboy-refreshtoken">>, Req0),
    ?LOG(["refreshtoken ", Refreshtoken]),
    case token_ds:decrypt_token(Refreshtoken) of
        {ok, Id, _ExpireAt, <<"rtk">>} ->
            Data = [
                {<<"token">>, token_ds:encrypt_token(Id)}
            ],
            resp_json_dto:success(Req0, Data, "操作成功.");
        {error, Code, Msg, _Li} ->
            resp_json_dto:error(Req0, Msg, Code)
    end.

send_code(Req0) ->
    %%
    %% 在POST请求中取出内容
    %% type 验证码类型 email sms
    %% account 账号 Email 或者 手机号码
    {ok, PostVals, _Req} = cowboy_req:read_urlencoded_body(Req0),
    % ?LOG(PostVals),
    Type = proplists:get_value(<<"type">>, PostVals, <<"email">>),
    Account = proplists:get_value(<<"account">>, PostVals),
    case Type of
        <<"email">> ->
            case passport_logic:send_email_code(Account) of
                {ok, _} ->
                    resp_json_dto:success(Req0, [], "操作成功.");
                {error, Msg} ->
                    resp_json_dto:error(Req0, [], Msg)
            end;
        _ ->
            resp_json_dto:success(Req0, [], "暂未实现功能.")
    end.

do_signup(Req0) ->
    %%
    %% 在POST请求中取出内容
    %% 用户名account
    %% 密码 pwd
    {ok, PostVals, _Req} = cowboy_req:read_urlencoded_body(Req0),
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

    {Ip, _Port} = cowboy_req:peer(Req0),
    Ip2 = list_to_binary(lists:flatten(io_lib:format("~w", [Ip]))),
    % ?LOG(["Ip", Ip, "port", Port]),
    Cosv = cowboy_req:header(<<"cosv">>, Req0),
    Post2 = [{<<"cosv">>, Cosv} | [{<<"ip">>, Ip2} | PostVals]],
    case passport_logic:do_signup(Type, Account, Password, Code, Post2) of
        {ok, Data} ->
            resp_json_dto:success(Req0, Data, "操作成功.");
        {error, Msg} ->
            resp_json_dto:error(Req0, Msg);
        {error, Msg, Code} ->
            resp_json_dto:error(Req0, Msg, Code)
    end.
