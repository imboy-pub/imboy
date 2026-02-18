-module(passport_handler).

-dialyzer({nowarn_function, [validate_bind_mail_params/4, validate_bind_mail_cache/2, bind_mail/1]}).

-behavior(cowboy_handler).

-export([init/2]).

-include_lib("kernel/include/inet.hrl").

-include("log.hrl").
-include("imboy_const.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 初始化认证处理器
%% 根据请求中的 action 参数调用相应的处理函数
%%
%% @param Req0 Cowboy请求对象
%% @param State0 状态映射，包含 action 参数
%% @return {ok, Req1, State} 处理后的请求对象和状态
%% @end
-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    % ?DEBUG_LOG(State),
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 =
        case Action of
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

%% @doc 绑定邮箱
%% 根据 user_logic:send_bind_email/2 方法生成的规则校验绑定Email
%%
%% @param Req0 Cowboy请求对象，包含验证参数
%% @return 返回成功或错误响应
%% @end
-spec bind_mail(cowboy_req:req()) -> cowboy_req:req().
bind_mail(Req0) ->
    Qs = cowboy_req:parse_qs(Req0),
    Ts = proplists:get_value(<<"ts">>, Qs, <<>>),
    Tk = proplists:get_value(<<"tk">>, Qs, <<>>),
    Uid = proplists:get_value(<<"uin">>, Qs, <<>>),
    Mail = proplists:get_value(<<"mail">>, Qs, <<>>),

    % 验证参数
    case validate_bind_mail_params(Ts, Tk, Uid, Mail) of
        {error, Msg} ->
            elib_response:error(Req0, Msg);
        {ok, Params} ->
            process_bind_mail(Req0, Params)
    end.

%% @doc 验证绑定邮箱参数
-spec validate_bind_mail_params(binary(), binary(), binary(), binary()) ->
    {ok, map()} | {error, binary()}.
validate_bind_mail_params(Ts, Tk, Uid, Mail) ->
    CacheKey = {bind_mail, Mail, Ts},
    CacheVal = imboy_cache:get(CacheKey),
    SolKey = config_ds:get(<<"solidified_key">>),
    Args = #{ts => Ts, uin => Uid, mail => Mail},
    Tk2 = elib_str:replace(Tk, " ", "+"),
    Now = elib_dt:second(),
    Ts2 = binary_to_integer(Ts),
    ExpectedTk = elib_hasher:hmac_sha512(elib_cnv:map_to_query(Args), SolKey),

    % 检查缓存
    case validate_bind_mail_cache(CacheVal, Mail) of
        {error, _} = Error ->
            Error;
        ok ->
            % 检查签名过期和签名匹配
            if Now > Ts2 ->
                    {error, "签名已过期"};
               ExpectedTk == Tk2 ->
                    {ok, #{uid => Uid, mail => Mail, cache_key => CacheKey}};
               true ->
                    {error, "签名有误"}
            end
    end.

%% @doc 验证绑定邮箱缓存状态
-spec validate_bind_mail_cache(term(), binary()) -> ok | {error, binary()}.
validate_bind_mail_cache(undefined, Mail) ->
    Id = elib_pg:pluck_value(user_repo:tablename(), <<"id">>, #{email => Mail}, #{}, 0),
    case Id > 0 of
        true -> {error, "抱歉，该邮箱地址验证已失效\n造成此情况可能是您更改了邮箱，也可能是您已确认过该邮箱不是您的。"};
        false -> ok
    end;
validate_bind_mail_cache(_CacheVal, _Mail) ->
    {error, "抱歉，该邮箱地址验证已失效\n造成此情况可能是您更改了邮箱，也可能是您已确认过该邮箱不是您的。"}.

%% @doc 处理绑定邮箱
-spec process_bind_mail(cowboy_req:req(), map()) -> cowboy_req:req().
process_bind_mail(Req0, #{uid := Uid, mail := Mail, cache_key := CacheKey}) ->
    Uid2 = elib_hashids:decode(Uid),
    _ = elib_pg:update(user_repo:tablename(), #{<<"email">> => Mail}, <<"id = $1">>, [Uid2]),
    imboy_cache:set(CacheKey, 1, 86400),
    elib_response:success(Req0, #{}).

%% @doc 用户登录
%% 使用账号密码进行登录
%%
%% @param Req0 Cowboy请求对象，包含登录信息
%% @return 返回包含用户信息和token的响应
%% @end
-spec login(cowboy_req:req()) -> cowboy_req:req().
login(Req0) ->
    PostVals = elib_param:post(Req0),
    Type = maps:get(<<"type">>, PostVals, ?TYPE_EMAIL),
    RsaEncrypt = maps:get(<<"rsa_encrypt">>, PostVals, ?RSA_ENCRYPT_YES),
    Account = maps:get(<<"account">>, PostVals, <<>>),
    Password = maps:get(<<"pwd">>, PostVals, <<>>),
    % 使用安全解密函数
    Pwd = elib_cipher:safe_rsa_decrypt(Password, RsaEncrypt),
    Ip = cowboy_req:header(<<"x-forwarded-for">>, Req0, <<"{}">>),

    % 提取设备信息
    DType = cowboy_req:header(<<"cos">>, Req0, <<>>),
    Did = cowboy_req:header(<<"did">>, Req0, <<>>),
    DName = cowboy_req:header(<<"dname">>, Req0, <<>>),

    Post2 = PostVals#{<<"ip">> => Ip, <<"dtype">> => DType, <<"did">> => Did, <<"dname">> => DName},

    case passport_logic:do_login(Type, Account, Pwd, DType, Did) of
        {ok, Data} ->
            Uid = maps:get(<<"uid">>, Data),
            gen_server:cast(user_server, {login_success, Uid, Post2}),
            Setting = user_setting_ds:find_by_uid(Uid),
            Data2 = Data#{<<"setting">> => Setting},
            elib_response:success(Req0, Data2, "success.");
        {{error, conflict}, ConflictInfo} ->
            % 返回设备冲突信息
            elib_response:error(Req0, ConflictInfo, 5100);
        {error, Msg} ->
            elib_response:error(Req0, Msg)
    end.

%% @doc 快速登录
%% 使用运营商一键登录功能
%%
%% @param Req0 Cowboy请求对象，包含运营商认证信息
%% @return 返回包含用户信息的响应
%% @end
-spec quick_login(cowboy_req:req()) -> cowboy_req:req().
quick_login(Req0) ->
    PostVals = elib_param:post(Req0),
    % ?DEBUG_LOG(PostVals),
    % jverify | huawei
    Service = maps:get(<<"service">>, PostVals, <<>>),
    % 成功时为对应运营商，CM代表中国移动，CU代表中国联通，CT代表中国电信。失败时可能为 null
    Operator = maps:get(<<"operator">>, PostVals, <<>>),
    %
    Token = maps:get(<<"token">>, PostVals, <<>>),
    Cosv = maps:get(<<"sys_version">>, PostVals, <<>>),
    Ip = cowboy_req:header(<<"x-forwarded-for">>, Req0, <<"{}">>),
    % ?DEBUG_LOG(["Ip", Ip]),
    Post2 = PostVals#{<<"cosv">> => Cosv, <<"ip">> => Ip},
    % ?DEBUG_LOG(["PostVals", PostVals, Post2]),
    case passport_logic:quick_login(Service, Operator, Token, Post2) of
        {ok, Data} ->
            % ?DEBUG_LOG(["Data", Data]),
            % 检查消息 用异步队列实现
            Uid = maps:get(<<"uid">>, Data),
            % gen_server:call是同步的，gen_server:cast是异步的
            gen_server:cast(user_server, {login_success, Uid, Post2}),
            Setting = user_setting_ds:find_by_uid(Uid),
            Data2 = Data#{<<"setting">> => Setting},
            % ?DEBUG_LOG(["Data2", Data2]),
            elib_response:success(Req0, Data2, "success.");
        {error, Msg} ->
            elib_response:error(Req0, Msg)
    end.

%% @doc 刷新Token
%% 使用refresh token换取新的access token
%%
%% @param Req0 Cowboy请求对象，包含refresh token
%% @return 返回包含新token的响应或错误
%% @end
-spec refreshtoken(cowboy_req:req()) -> cowboy_req:req().
refreshtoken(Req0) ->
    % Token = cowboy_req:header(<<"authorization">>, Req0),
    Refreshtoken = cowboy_req:header(<<"imboy-refreshtoken">>, Req0),
    % ?DEBUG_LOG(["refreshtoken ", Refreshtoken]),
    case throttle:check(refreshtoken, Refreshtoken) of
        {limit_exceeded, _, _} ->
            % elib_log:warning("Auth ~p exceeded api limit~n", [Refreshtoken]),
            cowboy_req:reply(429, Req0);
        _ ->
            case token_ds:decrypt_token(Refreshtoken) of
                {ok, Id, _ExpireDAt, <<"rtk">>} ->
                    % 状态: -1 删除  0 禁用  1 启用
                    % 使用安全的参数化查询，避免SQL注入
                    Status =
                        elib_pg:pluck_value(
                            user_repo:tablename(), <<"status">>, #{id => Id}, #{}, -2),
                    case Status of
                        _Other when Status > -1 ->
                            elib_response:success(Req0,
                                                   #{<<"token">> => token_ds:encrypt_token(Id)});
                        _ ->
                            elib_response:error(Req0, "用户被禁用或已删除")
                    end;
                {error, ErrCode, Msg, _Map} ->
                    elib_response:error(Req0, Msg, ErrCode)
            end
    end.

%% @doc 获取验证码
%% 发送邮箱或短信验证码
%%
%% @param Req0 Cowboy请求对象，包含账号和验证码类型
%% @return 返回成功或错误响应
%% @end
-spec getcode(cowboy_req:req()) -> cowboy_req:req().
getcode(Req0) ->
    %%
    %% 在POST请求中取出内容
    %% type 验证码类型 email sms
    %% account 账号 Email 或者 手机号码
    PostVals = elib_param:post(Req0),
    % ?DEBUG_LOG(PostVals),
    % type sms | email
    Type = maps:get(<<"type">>, PostVals, <<"email">>),
    % scene = forgot_pwd | signup
    Scene = maps:get(<<"scene">>, PostVals, <<>>),
    Account = maps:get(<<"account">>, PostVals, <<>>),
    % ?DEBUG_LOG([Type, Account]),
    Id = if Type == <<"sms">>, Scene == <<"signup">> ->
                % 使用安全的参数化查询，避免SQL注入
                elib_pg:pluck_value(
                    user_repo:tablename(), <<"id">>, #{mobile => Account}, #{}, 0);
            % elib_response:error(Req0, "Msg1");
            true ->
                0
         end,
    % elib_response:error(Req0, "Msg2")
    % ?DEBUG_LOG([Type, Account, "id ", Id, Type == <<"sms">>, Scene == <<"signup">>]),
    if Id > 0 ->
           elib_response:error(Req0, "paramAlreadyExist");
       true ->
           % elib_response:success(Req0, #{}, "success.")
           case passport_logic:send_code(Account, Type) of
               {ok, _} ->
                   elib_response:success(Req0, #{}, "success.");
               {error, Msg} ->
                   elib_response:error(Req0, Msg)
           end
    end.

%% @doc 用户注册
%% 创建新用户账号
%%
%% @param Req0 Cowboy请求对象，包含注册信息
%% @return 返回包含用户信息的响应
%% @end
-spec signup(cowboy_req:req()) -> cowboy_req:req().
signup(Req0) ->
    PostVals = elib_param:post(Req0),
    Type = maps:get(<<"type">>, PostVals, <<"email">>),
    Account = maps:get(<<"account">>, PostVals, <<>>),
    Password = maps:get(<<"pwd">>, PostVals, <<>>),
    Code = maps:get(<<"code">>, PostVals, <<>>),
    RsaEncrypt = maps:get(<<"rsa_encrypt">>, PostVals, <<"1">>),
    Cosv = maps:get(<<"sys_version">>, PostVals, <<>>),
    Ip = cowboy_req:header(<<"x-forwarded-for">>, Req0, <<"{}">>),
    % 使用安全解密函数
    Pwd = elib_cipher:safe_rsa_decrypt(Password, RsaEncrypt),
    Post2 = PostVals#{<<"cosv">> => Cosv, <<"ip">> => Ip},
    % 使用统一的结果处理函数
    elib_response:handle_logic_result(
        Req0,
        passport_logic:do_signup(Type, Account, Pwd, Code, Post2)).

%% @doc 找回密码
%% 通过验证码重置用户密码
%%
%% @param Req0 Cowboy请求对象，包含账号和新密码
%% @return 返回成功或错误响应
%% @end
-spec find_password(cowboy_req:req()) -> cowboy_req:req().
find_password(Req0) ->
    PostVals = elib_param:post(Req0),
    Type = maps:get(<<"type">>, PostVals, <<"email">>),
    Account = maps:get(<<"account">>, PostVals, <<>>),
    Password = maps:get(<<"pwd">>, PostVals, <<>>),
    Code = maps:get(<<"code">>, PostVals, <<>>),
    RsaEncrypt = maps:get(<<"rsa_encrypt">>, PostVals, <<"1">>),
    Cosv = maps:get(<<"sys_version">>, PostVals, <<>>),
    Ip = cowboy_req:header(<<"x-forwarded-for">>, Req0, <<"{}">>),
    % 使用安全解密函数
    Pwd = elib_cipher:safe_rsa_decrypt(Password, RsaEncrypt),
    Post2 = PostVals#{<<"cosv">> => Cosv, <<"ip">> => Ip},
    % 使用统一的结果处理函数
    elib_response:handle_logic_result(
        Req0,
        passport_logic:find_password(Type, Account, Pwd, Code, Post2)).
