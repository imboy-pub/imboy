-module(passport_handler).
%% Thin HTTP adapter for the identity passport boundary.
%% Keep request parsing here and delegate auth/account flows to passport_logic.

-dialyzer(
    {nowarn_function, [validate_bind_mail_params/4, validate_bind_mail_cache/2, bind_mail/1]}
).

-behavior(cowboy_handler).

-export([init/2]).

-include_lib("kernel/include/inet.hrl").

-include("log.hrl").
-include("imboy_const.hrl").
-include("error_code.hrl").

%% @doc 获取可信任的真实客户端 IP。
%%
%% 实现已提升到 elib_req:get_client_ip/1 作为全站唯一真源：此前本模块与
%% elib_req 各有一份，本模块这份是对的（带受信代理白名单），而
%% throttle_middleware 用的那份无条件采信 XFF —— 于是登录接口自己的
%% 锁定计数按真实 IP 记，限流中间件却按可伪造的 IP 记，攻击者只要每次
%% 换一个 XFF 就能绕过限流。两份实现分叉本身就是这个洞的成因。
%%
%% 保留本函数是为了不改动 4 个调用点的写法；语义完全不变。
-spec get_real_ip(cowboy_req:req()) -> binary().
get_real_ip(Req) ->
    elib_req:get_client_ip(Req).

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
            alipay_login ->
                alipay_login(Req0);
            alipay_authinfo ->
                alipay_authinfo(Req0);
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
    case parse_ts(Ts) of
        error ->
            {error, <<"签名有误"/utf8>>};
        {ok, Ts2} ->
            CacheKey = {bind_mail, Mail, Ts},
            CacheVal = imboy_cache:get(CacheKey),
            SolKey = config_ds:env(solidified_key),
            Args = #{ts => Ts, uin => Uid, mail => Mail},
            Tk2 = elib_str:replace(Tk, " ", "+"),
            Now = elib_dt:second(),
            ExpectedTk = elib_hasher:hmac_sha512(elib_cnv:map_to_query(Args), SolKey),

            % 检查缓存
            case validate_bind_mail_cache(CacheVal, Mail) of
                {error, _} = Error ->
                    Error;
                ok ->
                    % 检查签名过期和签名匹配
                    if
                        Now > Ts2 ->
                            {error, "签名已过期"};
                        ExpectedTk == Tk2 ->
                            {ok, #{uid => Uid, mail => Mail, cache_key => CacheKey}};
                        true ->
                            {error, "签名有误"}
                    end
            end
    end.

%% @doc 安全解析时间戳，非法二进制返回 error 而非崩溃
-spec parse_ts(binary()) -> {ok, integer()} | error.
parse_ts(Ts) ->
    try
        {ok, binary_to_integer(Ts)}
    catch
        error:badarg ->
            error
    end.

%% @doc 验证绑定邮箱缓存状态
-spec validate_bind_mail_cache(term(), binary()) -> ok | {error, binary()}.
validate_bind_mail_cache(undefined, Mail) ->
    case passport_logic:email_in_use(Mail) of
        true -> {error, "抱歉，该邮箱地址验证已失效\n造成此情况可能是您更改了邮箱，也可能是您已确认过该邮箱不是您的。"};
        false -> ok
    end;
validate_bind_mail_cache(_CacheVal, _Mail) ->
    {error, "抱歉，该邮箱地址验证已失效\n造成此情况可能是您更改了邮箱，也可能是您已确认过该邮箱不是您的。"}.

%% @doc 处理绑定邮箱
-spec process_bind_mail(cowboy_req:req(), map()) -> cowboy_req:req().
process_bind_mail(Req0, #{uid := Uid, mail := Mail, cache_key := CacheKey}) ->
    Uid2 = ec_cnv:to_integer(Uid),
    case passport_logic:bind_email(Uid2, Mail) of
        {ok, _} ->
            imboy_cache:set(CacheKey, 1, 86400),
            elib_response:success(Req0, #{});
        {error, Reason} ->
            ?ERROR_LOG({bind_mail_update_failed, Uid2, Reason}),
            elib_response:error(Req0, <<"邮箱绑定失败"/utf8>>)
    end.

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
    Ip = get_real_ip(Req0),

    % 提取设备信息
    DType = cowboy_req:header(<<"cos">>, Req0, <<>>),
    Did = cowboy_req:header(<<"did">>, Req0, <<>>),
    DName = cowboy_req:header(<<"dname">>, Req0, <<>>),

    Post2 = PostVals#{<<"ip">> => Ip, <<"dtype">> => DType, <<"did">> => Did, <<"dname">> => DName},

    %% 验证码登录：客户端发送 code 字段（无 pwd）
    Code = maps:get(<<"code">>, PostVals, <<>>),
    LoginResult =
        case Code of
            <<>> ->
                passport_logic:do_login(Type, Account, Pwd, DType, Did);
            _ ->
                passport_logic:do_login_by_code(Type, Account, Code, DType, Did)
        end,
    case LoginResult of
        {ok, Data} ->
            Uid = maps:get(<<"uid">>, Data),
            gen_server:cast(user_server, {login_success, Uid, Post2}),
            Setting = passport_logic:find_user_setting(Uid),
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
    Ip = get_real_ip(Req0),
    % ?DEBUG_LOG(["Ip", Ip]),
    %% did 头必须提上来：passport_logic:quick_login/4 用它绑 token 的 did claim，
    %% user_server 的 login_success 也从同一个 map 取 did 写 user_device 行。
    %% 缺这一行时两者同为空，该设备既无法被按设备吊销，设备记录也是断的。
    Did = cowboy_req:header(<<"did">>, Req0, <<>>),
    Post2 = PostVals#{<<"cosv">> => Cosv, <<"ip">> => Ip, <<"did">> => Did},
    % ?DEBUG_LOG(["PostVals", PostVals, Post2]),
    case passport_logic:quick_login(Service, Operator, Token, Post2) of
        {ok, Data} ->
            % ?DEBUG_LOG(["Data", Data]),
            % 检查消息 用异步队列实现
            Uid = maps:get(<<"uid">>, Data),
            % gen_server:call是同步的，gen_server:cast是异步的
            gen_server:cast(user_server, {login_success, Uid, Post2}),
            Setting = passport_logic:find_user_setting(Uid),
            Data2 = Data#{<<"setting">> => Setting},
            % ?DEBUG_LOG(["Data2", Data2]),
            elib_response:success(Req0, Data2, "success.");
        %% quota_guard 返回三元组（402 用户数达授权上限），必须显式接住：
        %% 只匹配 {error, Msg} 会在配额满时 case_clause 崩溃。
        {error, Msg, Code} ->
            elib_response:error(Req0, Msg, Code);
        {error, Msg} ->
            elib_response:error(Req0, Msg)
    end.

%% @doc 支付宝登录（APP 授权码换登录态）
%% 客户端支付宝 SDK 授权得 auth_code，POST 本端点换取 imboy token。
%% did 头绑定 token 设备 claim（与 quick_login 同款），并通知 user_server 落设备行。
%%
%% @param Req0 Cowboy请求对象，body 含 auth_code
%% @return 返回包含用户信息的响应
%% @end
-spec alipay_login(cowboy_req:req()) -> cowboy_req:req().
alipay_login(Req0) ->
    PostVals = elib_param:post(Req0),
    AuthCode = maps:get(<<"auth_code">>, PostVals, <<>>),
    case AuthCode of
        <<>> ->
            %% 空授权码直接拒绝：外呼网关必失败且浪费一次签名/HTTP 往返
            elib_response:error(Req0, <<"授权码不能为空"/utf8>>);
        _ ->
            Cosv = maps:get(<<"sys_version">>, PostVals, <<>>),
            Ip = get_real_ip(Req0),
            Did = cowboy_req:header(<<"did">>, Req0, <<>>),
            Post2 = PostVals#{<<"cosv">> => Cosv, <<"ip">> => Ip, <<"did">> => Did},
            case passport_logic:alipay_login(AuthCode, Post2) of
                {ok, Data} ->
                    Uid = maps:get(<<"uid">>, Data),
                    gen_server:cast(user_server, {login_success, Uid, Post2}),
                    Setting = passport_logic:find_user_setting(Uid),
                    Data2 = Data#{<<"setting">> => Setting},
                    elib_response:success(Req0, Data2, "success.");
                %% quota_guard 三元组（402 用户数达授权上限）
                {error, Msg, Code} ->
                    elib_response:error(Req0, Msg, Code);
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% @doc 获取支付宝授权签名串（客户端唤起 SDK 用，私钥不出服务端）
-spec alipay_authinfo(cowboy_req:req()) -> cowboy_req:req().
alipay_authinfo(Req0) ->
    case passport_logic:alipay_authinfo() of
        {ok, Data} ->
            elib_response:success(Req0, Data, "success.");
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
            % P2-8f: 429 响应必须返回完整 envelope（code/msg/sv_ts/payload），
            % 避免客户端拿不到业务错误信息
            elib_response:error_with_status(
                Req0,
                429,
                <<"刷新过于频繁，请稍后再试"/utf8>>,
                429
            );
        _ ->
            case token_ds:decrypt_token(Refreshtoken) of
                {ok, Id, _ExpireDAt, <<"rtk">>, Did} ->
                    % 状态: -1 删除  0 禁用  1 启用
                    Status = user_logic:get_status(Id),
                    % 设备被移除 → 拒绝刷新。这是真正切断 refresh token 356 天窗口
                    % 的地方；did 为空的 legacy refresh token 原样放行（零全端登出）。
                    DeviceActive = Did =:= <<>> orelse user_device_logic:is_active(Id, Did),
                    case {Status > -1, DeviceActive} of
                        {true, true} ->
                            % E2EE-013：刷新保留原 refresh token 绑定的设备 DID。
                            elib_response:success(
                                Req0,
                                #{<<"token">> => token_ds:encrypt_token(Id, Did)}
                            );
                        {false, _} ->
                            elib_response:error(Req0, "用户被禁用或已删除");
                        {_, false} ->
                            elib_response:error(
                                Req0, <<"设备已被移除，请重新登录"/utf8>>, ?ERR_TOKEN_INVALID
                            )
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
    MobileExists =
        if
            Type == <<"sms">>, Scene == <<"signup">> ->
                passport_logic:mobile_registered(Account);
            true ->
                false
        end,
    % ?DEBUG_LOG([Type, Account, "MobileExists ", MobileExists, Type == <<"sms">>, Scene == <<"signup">>]),
    if
        MobileExists ->
            elib_response:error(Req0, <<"该手机号已注册"/utf8>>);
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
    Ip = get_real_ip(Req0),
    % 使用安全解密函数
    Pwd = elib_cipher:safe_rsa_decrypt(Password, RsaEncrypt),
    Post2 = PostVals#{<<"cosv">> => Cosv, <<"ip">> => Ip},
    % 使用统一的结果处理函数
    elib_response:handle_logic_result(
        Req0,
        passport_logic:do_signup(Type, Account, Pwd, Code, Post2)
    ).

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
    Ip = get_real_ip(Req0),
    % 使用安全解密函数
    Pwd = elib_cipher:safe_rsa_decrypt(Password, RsaEncrypt),
    Post2 = PostVals#{<<"cosv">> => Cosv, <<"ip">> => Ip},
    % 使用统一的结果处理函数
    elib_response:handle_logic_result(
        Req0,
        passport_logic:find_password(Type, Account, Pwd, Code, Post2)
    ).
