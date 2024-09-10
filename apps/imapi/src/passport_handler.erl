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
    Req1 = case Action of
        bind_mail ->
            bind_mail(Req0);
        refreshtoken ->
            refreshtoken(Req0);
        login ->
            login(Req0);
        quick_login ->
            quick_login(Req0);
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

%% 根据 user_logic:send_bind_email/2 方法生成的规则校验绑定EMail
bind_mail(Req0) ->
    #{ts := Ts} = cowboy_req:match_qs([{ts, [], <<>>}], Req0),
    #{tk := Tk} = cowboy_req:match_qs([{tk, [], <<>>}], Req0),
    #{uin := Uid} = cowboy_req:match_qs([{uin, [], <<>>}], Req0),
    #{mail := Mail} = cowboy_req:match_qs([{mail, [], <<>>}], Req0),

    CacheKey = {bind_mail, Mail, Ts},
    % CacheVal =  imboy_cache:get(CacheKey),
    CacheVal = undefined,
    ?LOG(["CacheVal ", CacheVal]),
    SolKey = config_ds:get(solidified_key), % {ok, 1}
    Args = #{
        ts => Ts,
        uin => Uid,
        mail => Mail
    },
    Tk2 = imboy_str:replace(Tk, " ", "+"),
    % ?LOG(["tk ", Tk]),
    % ?LOG(["tk ", Tk2]),
    % ?LOG(["Ts ", Ts]),
    Now = imboy_dt:second(),
    Ts2 = binary_to_integer(Ts),
    NewTk = imboy_hasher:hmac_sha512(
        imboy_cnv:map_to_query(Args), SolKey),

    % User = user_repo:find_by_email(Mail, <<"id">>),
    % CheckUser = maps:size(User),
    Err1 = case CacheVal of
        undefined ->
                Id = imboy_db:pluck(user_repo:tablename()
                    , <<"email='", Mail/binary, "'">>
                    , <<"id">>
                    , 0),
                if Id > 0 -> true; true -> false end;
        _ ->
            true
    end,
    if
        Err1 ->
            imboy_response:error(Req0, "抱歉，该邮箱地址验证已失效\n造成此情况可能是您更改了邮箱，也可能是您已确认过该邮箱不是您的。");
        Now > Ts2 ->
            imboy_response:error(Req0, "签名已过期");
        NewTk == Tk2 ->
            Uid2 = imboy_hashids:decode(Uid),
            Where = <<"id=", (ec_cnv:to_binary(Uid2))/binary>>,
            imboy_db:update(user_repo:tablename(), Where, #{
                <<"email">> => Mail
            }),
            imboy_cache:set(CacheKey, 1, 86400),
            imboy_response:success(Req0, #{});
        true ->
            imboy_response:error(Req0, "签名有误")
    end.

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
    % ?LOG(['Type', Type,'Password', Password, "PostVals ", PostVals]),
    Pwd = case RsaEncrypt == <<"1">> of
        true ->
            try imboy_cipher:rsa_decrypt(Password) of
                Pwd0 ->
                    Pwd0
            catch
                Class:Reason:Stacktrace ->
                    ?LOG(["websocket_handle try catch: Class:", Class,
                          "Reason:", Reason,
                          "Stacktrace:", Stacktrace,
                          erlang:trace(all, true, [call])]),
                    <<>>
            end;
        _ ->
            Password
    end,
    Ip = cowboy_req:header(<<"x-forwarded-for">>, Req0, <<"{}">>),
    Post2 = [{<<"ip">>, Ip} | PostVals],
    % ?LOG(["Ip", Ip, "Pwd " , Post2]),
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


quick_login(Req0) ->
    PostVals = imboy_req:post_params(Req0),
    % ?LOG(PostVals),
    % jverify | huawei
    Service = proplists:get_value(<<"service">>, PostVals, <<>>),
    % 成功时为对应运营商，CM代表中国移动，CU代表中国联通，CT代表中国电信。失败时可能为 null
    Operator = proplists:get_value(<<"operator">>, PostVals, <<>>),
    %
    Token = proplists:get_value(<<"token">>, PostVals),
    Cosv = proplists:get_value(<<"sys_version">>, PostVals, <<>>),
    Ip = cowboy_req:header(<<"x-forwarded-for">>, Req0, <<"{}">>),
    % ?LOG(["Ip", Ip]),
    Post2 = [{<<"cosv">>, Cosv} | [{<<"ip">>, Ip} | PostVals]],
    % ?LOG(["PostVals", PostVals, Post2]),
    case passport_logic:quick_login(Service, Operator, Token, Post2) of
        {ok, Data} ->
            % ?LOG(["Data", Data]),
            % 检查消息 用异步队列实现
            Uid = maps:get(<<"uid">>, Data),
            % gen_server:call是同步的，gen_server:cast是异步的
            gen_server:cast(user_server, {login_success, Uid, Post2}),
            Setting = user_setting_ds:find_by_uid(Uid),
            Data2 = Data#{<<"setting">> => Setting},
            % ?LOG(["Data2", Data2]),
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
    % type sms | email
    Type = proplists:get_value(<<"type">>, PostVals, <<"email">>),
    % scene = forgot_pwd | signup
    Scene = proplists:get_value(<<"scene">>, PostVals),
    Account = proplists:get_value(<<"account">>, PostVals),
    % ?LOG([Type, Account]),
    Id = if
        Type == <<"sms">>, Scene == <<"signup">> ->
            imboy_db:pluck(user_repo:tablename()
                , <<"mobile='", Account/binary, "'">>
                , <<"id">>
                , 0);
            % imboy_response:error(Req0, "Msg1");
        true ->
            0
            % imboy_response:error(Req0, "Msg2")
    end,
    % ?LOG([Type, Account, "id ", Id, Type == <<"sms">>, Scene == <<"signup">>]),
    if
        Id > 0 ->
            imboy_response:error(Req0, "param_already_exist");
        true ->
            % imboy_response:success(Req0, #{}, "success.")
            case passport_logic:send_code(Account, Type) of
                {ok, _} ->
                    imboy_response:success(Req0, #{}, "success.");
                {error, Msg} ->
                    imboy_response:error(Req0, Msg)
            end
    end.


signup(Req0) ->
    %%
    %% 在POST请求中取出内容
    %% 用户名account
    %% 密码 pwd
    PostVals = imboy_req:post_params(Req0),
    % ?LOG(PostVals),
    % type = email | mobile
    Type = proplists:get_value(<<"type">>, PostVals, <<"email">>),
    Account = proplists:get_value(<<"account">>, PostVals),
    Pwd = proplists:get_value(<<"pwd">>, PostVals),
    Code = proplists:get_value(<<"code">>, PostVals),
    % 邀请人ID
    % RefUid = proplists:get_value(<<"ref_uid">>, PostVals),
    % RegIp = proplists:get_value(<<"reg_ip">>, PostVals),
    % 注册客服端操作系统
    % RegCos = proplists:get_value(<<"reg_cos">>, PostVals),

    % Cosv = cowboy_req:header(<<"cosv">>, Req0),
    Cosv = proplists:get_value(<<"sys_version">>, PostVals, <<>>),
    Ip = cowboy_req:header(<<"x-forwarded-for">>, Req0, <<"{}">>),
    % ?LOG(["Ip", Ip]),
    Post2 = [{<<"cosv">>, Cosv} | [{<<"ip">>, Ip} | PostVals]],
    case passport_logic:do_signup(Type, Account, Pwd, Code, Post2) of
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
    Pwd = proplists:get_value(<<"pwd">>, PostVals),
    Code = proplists:get_value(<<"code">>, PostVals),
    % 邀请人ID
    % RefUid = proplists:get_value(<<"ref_uid">>, PostVals),
    % RegIp = proplists:get_value(<<"reg_ip">>, PostVals),
    % 注册客服端操作系统
    % RegCos = proplists:get_value(<<"reg_cos">>, PostVals),

    % Cosv = cowboy_req:header(<<"cosv">>, Req0),
    Cosv = proplists:get_value(<<"sys_version">>, PostVals, <<>>),
    Ip = cowboy_req:header(<<"x-forwarded-for">>, Req0, <<"{}">>),
    % ?LOG(["Ip", Ip]),
    Post2 = [{<<"cosv">>, Cosv} | [{<<"ip">>, Ip} | PostVals]],
    case passport_logic:find_password(Type, Account, Pwd, Code, Post2) of
        {ok, Data} ->
            imboy_response:success(Req0, Data, "success.");
        {error, Msg} ->
            imboy_response:error(Req0, Msg);
        {error, Msg, Code} ->
            imboy_response:error(Req0, Msg, Code)
    end.
