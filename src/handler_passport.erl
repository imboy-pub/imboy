-module(handler_passport).
-behavior(cowboy_handler).

-export([init/2]).

-include_lib("kernel/include/inet.hrl").
-include("common.hrl").


init(Req0, State) ->
    Req1 =
        case lists:keyfind(action, 1, State) of
            {action, refreshtoken} ->
                refreshtoken(Req0);
            {action, do_login} ->
                do_login(Req0);
            {action, do_signup} ->
                do_signup(Req0);
            {action, send_code} ->
                send_code(Req0);
            {action, find_password} ->
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
    {ok, PostVals, _Req} = cowboy_req:read_urlencoded_body(Req0),
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
    case logic_passport:do_login(Type, Account, Pwd) of
        {ok, Data} ->
            % 检查消息 用异步队列实现
            Uid = proplists:get_value(<<"uid">>, Data),
            gen_server:cast(server_user, {login_success, Uid, Post2}),
            dto_resp_json:success(Req0, Data, "操作成功.");
        {error, Msg} ->
            dto_resp_json:error(Req0, Msg);
        {error, Msg, Code} ->
            dto_resp_json:error(Req0, Msg, Code)
    end.


refreshtoken(Req0) ->
    % Token = cowboy_req:header(<<"authorization">>, Req0),
    Refreshtoken = cowboy_req:header(<<"imboy-refreshtoken">>, Req0),
    ?LOG(["refreshtoken ", Refreshtoken]),
    case ds_token:decrypt_token(Refreshtoken) of
        {ok, Id, _ExpireAt, <<"rtk">>} ->
            Data = [{<<"token">>, ds_token:encrypt_token(Id)}],
            dto_resp_json:success(Req0, Data, "操作成功.");
        {error, Code, Msg, _Li} ->
            dto_resp_json:error(Req0, Msg, Code)
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
            case logic_passport:send_email_code(Account) of
                {ok, _} ->
                    dto_resp_json:success(Req0, [], "操作成功.");
                {error, Msg} ->
                    dto_resp_json:error(Req0, [], Msg)
            end;
        _ ->
            dto_resp_json:success(Req0, [], "暂未实现功能.")
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

    Cosv = cowboy_req:header(<<"cosv">>, Req0),
    Ip = cowboy_req:header(<<"x-forwarded-for">>, Req0),
    % ?LOG(["Ip", Ip]),
    Post2 = [{<<"cosv">>, Cosv} | [{<<"ip">>, Ip} | PostVals]],
    case logic_passport:do_signup(Type,
                                  Account,
                                  Password,
                                  Code,
                                  Post2) of
        {ok, Data} ->
            dto_resp_json:success(Req0, Data, "操作成功.");
        {error, Msg} ->
            dto_resp_json:error(Req0, Msg);
        {error, Msg, Code} ->
            dto_resp_json:error(Req0, Msg, Code)
    end.


find_password(Req0) ->
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

    Cosv = cowboy_req:header(<<"cosv">>, Req0),
    Ip = cowboy_req:header(<<"x-forwarded-for">>, Req0),
    % ?LOG(["Ip", Ip]),
    Post2 = [{<<"cosv">>, Cosv} | [{<<"ip">>, Ip} | PostVals]],
    case logic_passport:find_password(Type,
                                      Account,
                                      Password,
                                      Code,
                                      Post2) of
        {ok, Data} ->
            dto_resp_json:success(Req0, Data, "操作成功.");
        {error, Msg} ->
            dto_resp_json:error(Req0, Msg);
        {error, Msg, Code} ->
            dto_resp_json:error(Req0, Msg, Code)
    end.