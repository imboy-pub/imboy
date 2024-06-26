-module(passport_handler).
-behavior(cowboy_handler).

-export([init/2]).

-include_lib("kernel/include/inet.hrl").
-include_lib("imlib/include/log.hrl").

%% ===================================================================
%% API
%% ===================================================================


init(Req0, State0) ->
    % ?LOG(State),
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 =
        case Action of
            refreshtoken ->
                refreshtoken(Req0);
            login ->
                login(Req0);
            signup ->
                signup(Req0);
            getcode ->
                getcode(Req0);
            find_password ->
                find_password(Req0);
            false ->
                Req0
        end,
    {ok, Req1, State}.


login(Req0) ->
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
    RsaEncrypt = proplists:get_value(<<"rsa_encrypt">>, PostVals, <<"1">>),
    Account = proplists:get_value(<<"account">>, PostVals),
    Password = proplists:get_value(<<"pwd">>, PostVals),
    % ?LOG(['Type', Type,'Password', Password]),
    Pwd = case RsaEncrypt == <<"1">> of
        true ->
            try imboy_cipher:rsa_decrypt(Password) of
                Pwd0 ->
                    Pwd0
            catch
                _Type:_Reason ->
                    <<>>
            end;
        _ ->
            Password
    end,
    Ip = cowboy_req:header(<<"x-forwarded-for">>, Req0),
    % ?LOG(["Ip", Ip]),
    Post2 = [{<<"ip">>, Ip} | PostVals],
    case passport_logic:do_login(Type, Account, Pwd) of
        {ok, Data} ->
            % 检查消息 用异步队列实现
            Uid = maps:get(<<"uid">>, Data),
            % gen_server:call是同步的，gen_server:cast是异步的
            gen_server:cast(user_server, {login_success, Uid, Post2}),
            Setting = user_setting_ds:find_by_uid(Uid),
            Data2 = Data#{<<"setting">> => Setting},
            imboy_response:success(Req0, Data2, "success.");
        {error, Msg} ->
            imboy_response:error(Req0, Msg)
    end.


refreshtoken(Req0) ->
    % Token = cowboy_req:header(<<"authorization">>, Req0),
    Refreshtoken = cowboy_req:header(<<"imboy-refreshtoken">>, Req0),
    % ?LOG(["refreshtoken ", Refreshtoken]),
    case throttle:check(refreshtoken, Refreshtoken) of
        {limit_exceeded, _, _} ->
            % imboy_log:warning("Auth ~p exceeded api limit~n", [Refreshtoken]),
            cowboy_req:reply(429, Req0);
        _ ->
            case token_ds:decrypt_token(Refreshtoken) of
                {ok, Id, _ExpireDAt, <<"rtk">>} ->
                    % 状态: -1 删除  0 禁用  1 启用
                    Status = imboy_db:pluck(user_repo:tablename()
                        , <<"id=", (ec_cnv:to_binary(Id))/binary>>
                        , <<"status">>
                        , -2),
                    case Status of
                        _ when Status > -1 ->
                            imboy_response:success(Req0, #{
                                <<"token">> => token_ds:encrypt_token(Id)
                            });
                        _ ->
                            imboy_response:error(Req0, "用户被禁用或已删除")
                    end;
                {error, Code, Msg, _Map} ->
                    imboy_response:error(Req0, Msg, Code)
            end
    end.


getcode(Req0) ->
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


signup(Req0) ->
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
    case passport_logic:do_signup(Type, Account, Password, Code, Post2) of
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
    case passport_logic:find_password(Type, Account, Password, Code, Post2) of
        {ok, Data} ->
            imboy_response:success(Req0, Data, "success.");
        {error, Msg} ->
            imboy_response:error(Req0, Msg);
        {error, Msg, Code} ->
            imboy_response:error(Req0, Msg, Code)
    end.
