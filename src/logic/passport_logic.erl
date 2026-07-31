-module(passport_logic).
%% Stable identity passport domain boundary.
%% API adapters should call this module instead of reaching auth/user internals directly.
-dialyzer({nowarn_function, [{send_email_code, 1}, {send_sms_code, 1}, {do_login_verify, 4}]}).
%%%
% passport_logic 是 passport application logic 缩写
%%%

-export([send_code/2]).
-export([signup/4]).
-export([login/3]).
-export([do_login/3]).
-export([do_login/5]).
-export([do_login_by_code/5]).
-export([do_signup/5]).
-export([find_password/5]).
-export([verify_user/3]).
-export([quick_login/4]).
-export([find_user_setting/1]).
-export([email_in_use/1]).
-export([bind_email/2]).
-export([mobile_registered/1]).
%% P0-C: OIDC 登录流复用（auth_oidc_logic）——签发收口 + License 配额闸门 + 建号数据组装
-export([login_resp/2]).
-export([quota_guard/0]).
-export([pick_data_for_insert/2]).

-include("log.hrl").
-include("common.hrl").
-include("error_code.hrl").

%% @doc 快速登录（支持第三方服务）
%% @param Service 登录服务类型（如 jverify）
%% @param Operator 操作员
%% @param Token 认证令牌
%% @param PostVals 请求参数
%% @returns {ok, Data} | {error, Msg}
-spec quick_login(binary(), binary() | undefined, binary(), map()) ->
    {ok, map()} | {error, binary() | map()}.

quick_login(<<>>, _, _, _) ->
    {error, <<"未指定登录服务"/utf8>>};
quick_login(<<"jverify">>, _Operator, Token, PostVals) ->
    %% E2EE-013：绑定登录设备 DID 进 token。
    %% did 与 user_server 的 {login_success, Uid, PostVals} 写 user_device 行取的是
    %% 同一个 PostVals key，两者天然一致；调用方没给就退化为 legacy 空 did。
    Did = maps:get(<<"did">>, PostVals, <<>>),
    case imboy_sms:jverification(Token) of
        {error, Msg} ->
            {error, Msg};
        {ok, Mobile} ->
            Mobile2 = <<"+86", Mobile/binary>>,
            User = user_ds:find_by_mobile(Mobile2, ?LOGIN_COLUMN),
            Uid = maps:get(<<"id">>, User, 0),
            case Uid of
                0 ->
                    Data = pick_data_for_insert(
                        #{
                            <<"source">> => <<"jverify">>,
                            <<"mobile">> => Mobile2,
                            <<"password">> => <<>>
                        },
                        PostVals
                    ),
                    case user_ds:insert_and_get_id(Data) of
                        {ok, Uid2} ->
                            User2 = user_ds:find_by_id(Uid2, ?LOGIN_COLUMN),
                            {ok,
                                login_resp(User2, Did, #{
                                    <<"action">> => <<"need_set_password">>
                                })};
                        {error, Reason} ->
                            {error, Reason}
                    end;
                _ ->
                    {ok, login_resp(User, Did, #{})}
            end
    end;
quick_login(_, _, _, _) ->
    {error, <<"不支持的登录服务"/utf8>>}.

% passport_logic:send_code(<<>>, <<"sms">>).
% passport_logic:send_code(<<>>, <<"email">>).
send_code(Mobile, <<"sms">>) ->
    % Res = throttle:check(per_minute_once, Mobile),
    % ?DEBUG_LOG([per_minute_once, Res]),
    % case Res of
    case throttle:check(per_minute_once, {send_code, Mobile}) of
        {limit_exceeded, _, _} ->
            {error, <<"per_minute_once">>};
        _ ->
            send_sms_code(Mobile)
    end;
send_code(EMail, <<"email">>) ->
    send_email_code(EMail);
send_code(_, _) ->
    {error, <<"验证码类型不支持，仅支持 sms 或 email"/utf8>>}.

%% @doc 兼容旧测试入口：明文手机号注册，并直接返回登录态数据
-spec signup(binary(), binary(), binary(), map()) -> {ok, map()} | {error, binary(), integer()}.
signup(Mobile, Pwd, Email, PostVals) when
    is_binary(Mobile), is_binary(Pwd), is_binary(Email), is_map(PostVals)
->
    case validate_compat_password(Pwd) of
        ok ->
            case prepare_compat_mobile(Mobile) of
                ok ->
                    case quota_guard() of
                        {error, _, _} = QErr ->
                            QErr;
                        ok ->
                            Nickname = compat_signup_nickname(Mobile, PostVals),
                            Email2 = compat_signup_email(Email),
                            PostVals2 = PostVals#{
                                <<"nickname">> => Nickname
                            },
                            BaseData0 = #{
                                <<"password">> => elib_password:generate(Pwd),
                                <<"mobile">> => Mobile,
                                <<"account">> => Mobile
                            },
                            BaseData =
                                case Email2 of
                                    undefined ->
                                        BaseData0;
                                    _ ->
                                        BaseData0#{<<"email">> => Email2}
                                end,
                            Data = pick_data_for_insert(BaseData, PostVals2),
                            case user_ds:insert_and_get_id(Data) of
                                {ok, Uid} ->
                                    User = user_ds:find_by_id(Uid, ?LOGIN_COLUMN),
                                    {ok, login_resp(User, #{})};
                                {error,
                                    {error, error, <<"23505">>, unique_violation, _Msg, _Details}} ->
                                    {error, <<"账号已被占用"/utf8>>, 400};
                                {error, Reason} ->
                                    {error, compat_error_message(Reason), 400}
                            end
                    end;
                {error, Msg, Code} ->
                    {error, Msg, Code}
            end;
        {error, Msg} ->
            {error, Msg, 400}
    end.

%% @doc 兼容旧测试入口：使用明文账号密码登录
-spec login(binary(), binary(), map()) -> {ok, map()} | {error, binary(), integer()}.
login(Account, Pwd, PostVals) when is_binary(Account), is_binary(Pwd), is_map(PostVals) ->
    Type = compat_login_type(Account),
    Did = maps:get(<<"did">>, PostVals, <<>>),
    DType = compat_device_type(PostVals, Did),
    DName = maps:get(<<"dname">>, PostVals, <<>>),
    Ip = maps:get(<<"ip">>, PostVals, <<"{}">>),
    PostVals2 = PostVals#{
        <<"did">> => Did,
        <<"dtype">> => DType,
        <<"cos">> => DType,
        <<"dname">> => DName,
        <<"ip">> => Ip
    },
    case do_login(Type, Account, Pwd, DType, Did) of
        {ok, Data} ->
            _ = record_compat_login_success(Data, PostVals2),
            {ok, Data};
        {{error, conflict}, ConflictInfo} ->
            {error, compat_error_message(ConflictInfo), 5100};
        {error, Msg} ->
            {error, compat_error_message(Msg), 400}
    end.

-spec do_login(binary(), binary(), binary()) -> {ok, map()} | {error, binary() | map()}.
do_login(Type, Account, Pwd) ->
    do_login(Type, Account, Pwd, <<>>, <<>>).

%% @doc 账号密码登录（带设备信息）
%% 检查设备冲突，如果同类型设备已登录则返回冲突信息
-spec do_login(binary(), binary(), binary(), binary(), binary()) ->
    {ok, map()} | {{error, conflict}, map()} | {error, binary() | map()}.
do_login(_Type, _Email, <<>>, _DType, _Did) ->
    {error, <<"密码有误"/utf8>>};
do_login(Type, Mobile, Pwd, DType, Did) when Type == <<"mobile">> ->
    User = user_ds:find_by_mobile(Mobile, ?LOGIN_COLUMN),
    do_login_verify(Pwd, User, DType, Did);
do_login(Type, Email, Pwd, DType, Did) when Type == <<"email">> ->
    case elib_type:is_email(Email) of
        true ->
            User = user_ds:find_by_email(Email, ?LOGIN_COLUMN),
            do_login_verify(Pwd, User, DType, Did);
        false ->
            {error, <<"Email格式有误."/utf8>>}
    end;
do_login(Type, Account, Pwd, DType, Did) when Type == <<"account">> ->
    User =
        case elib_type:is_email(Account) of
            true ->
                user_ds:find_by_email(Account, ?LOGIN_COLUMN);
            false ->
                user_ds:find_by_account(Account, ?LOGIN_COLUMN)
        end,
    do_login_verify(Pwd, User, DType, Did).

%% @doc 验证码登录（mobile/email）
%% 客户端发送 code 字段代替 pwd
-spec do_login_by_code(binary(), binary(), binary(), binary(), binary()) ->
    {ok, map()} | {{error, conflict}, map()} | {error, binary()}.
%% 与密码登录路径（do_login_verify/4，:286 起）对齐三件事，缺一不可：
%%   1) 前置 check_login_allowed —— 此前验证码路径完全没有锁定门；
%%   2) 失败时 record_login_failure —— 此前失败不计数，等于给 6 位码
%%      开了无限次尝试；
%%   3) 用 consume/2 而不是 verify_code/2 —— 验证成功后立即失效，
%%      杜绝同一个码被反复使用。
%% 三者叠加才关得住爆破：只做 (3) 的话攻击者仍可无限猜；只做 (1)(2)
%% 的话正确的码被撞出来后仍可复用。
%%
%% Ip 用 <<>> 与密码路径保持一致（见 do_login_by_code_verify/3 注释）。
do_login_by_code(Type, Account, Code, DType, Did) ->
    Ip = <<>>,
    case login_security_logic:check_login_allowed(Account, Ip) of
        {error, LockMsg} ->
            {error, LockMsg};
        _ ->
            do_login_by_code_checked(Type, Account, Code, DType, Did, Ip)
    end.

%% @private 已过锁定门，开始消费验证码
-spec do_login_by_code_checked(binary(), binary(), binary(), binary(), binary(), binary()) ->
    {ok, map()} | {{error, conflict}, map()} | {error, binary()}.
do_login_by_code_checked(Type, Account, Code, DType, Did, Ip) ->
    case consume_code(Account, Code) of
        {ok, _} ->
            User =
                case Type of
                    <<"mobile">> -> user_ds:find_by_mobile(Account, ?LOGIN_COLUMN);
                    <<"email">> -> user_ds:find_by_email(Account, ?LOGIN_COLUMN);
                    <<"account">> -> user_ds:find_by_account(Account, ?LOGIN_COLUMN);
                    _ -> #{}
                end,
            do_login_by_code_verify(User, DType, Did);
        {error, Msg} ->
            %% 关键：验证码错误必须计数，否则 6 位码等于可无限尝试
            _ = login_security_logic:record_login_failure(Account, Ip),
            {error, Msg}
    end.

%% @doc 验证码登录后的用户验证
-spec do_login_by_code_verify(map(), binary(), binary()) ->
    {ok, map()} | {{error, conflict}, map()} | {error, binary()}.
do_login_by_code_verify(User, _DType, _Did) when map_size(User) =:= 0 ->
    {error, <<"账号不存在"/utf8>>};
do_login_by_code_verify(User, DType, Did) ->
    Account = maps:get(
        <<"account">>, User, maps:get(<<"mobile">>, User, maps:get(<<"email">>, User, <<>>))
    ),
    %% Ip 用 <<>>，与密码登录路径（do_login_verify/4）一致。
    %% 此前这里是 Ip = Did —— Did 来自客户端自报的请求头，攻击者每次换一个
    %% did 就换一个锁定计数 key，account 级锁定形同虚设；而且同一账号在
    %% 密码路径（Ip=<<>>）与验证码路径（Ip=Did）落在两个不同的键空间里，
    %% 两条路径的失败次数互不累加。
    Ip = <<>>,
    % 验证码登录：验证通过后即视为成功，重置失败计数
    _ = login_security_logic:record_login_success(Account, Ip),
    Status = maps:get(<<"status">>, User, -2),
    case Status of
        -2 ->
            {error, <<"账号不存在"/utf8>>};
        -1 ->
            {error, <<"账号不存在或者已删除"/utf8>>};
        0 ->
            {error, <<"账号被禁用"/utf8>>};
        1 ->
            Data = login_resp(User, Did, #{}),
            UidHashed = maps:get(<<"uid">>, Data),
            Uid = ec_cnv:to_integer(UidHashed),
            % GAP-01: 异步写验证码登录成功审计日志（type=100）
            _ = elib_async:async(fun() ->
                {ok, LogBody} = jsone_encode:encode(
                    #{
                        <<"account">> => Account,
                        <<"dtype">> => DType,
                        <<"login_method">> => <<"code">>
                    },
                    [native_utf8]
                ),
                _ = user_log_ds:add_internal(undefined, 100, Uid, LogBody, elib_dt:now())
            end),
            case user_device_logic:check_login_conflict(Uid, DType) of
                {ok, no_conflict} -> {ok, Data};
                {ok, conflict, ConflictInfo} -> {{error, conflict}, ConflictInfo};
                {error, _} -> {ok, Data}
            end;
        2 ->
            {ok, login_resp(User, Did, #{})}
    end.

%% @doc 登录验证（带设备冲突检查）
%% 先认证用户，成功后检查设备冲突
-spec do_login_verify(binary(), map(), binary(), binary()) ->
    {ok, map()} | {{error, conflict}, map()} | {error, term()}.
do_login_verify(Pwd, User, DType, Did) ->
    % 提取账户标识符用于暴力破解保护
    Account = maps:get(
        <<"account">>,
        User,
        maps:get(
            <<"mobile">>,
            User,
            maps:get(<<"email">>, User, <<>>)
        )
    ),
    % IP 在此层不可用，使用空字符串（账户维度保护仍有效）
    Ip = <<>>,
    % 登录前检查是否被锁定
    case login_security_logic:check_login_allowed(Account, Ip) of
        {error, locked, LockInfo} ->
            {error, LockInfo};
        {ok, true} ->
            case verify_user(Pwd, User, Did) of
                {ok, Data} ->
                    % 登录成功，重置失败计数
                    _ = login_security_logic:record_login_success(Account, Ip),
                    % 从 uid (字符串格式) 转换为整数 ID
                    UidHashed = maps:get(<<"uid">>, Data),
                    Uid = ec_cnv:to_integer(UidHashed),
                    % GAP-01: 异步写登录成功审计日志（type=100），fire-and-forget 不阻塞登录
                    _ = elib_async:async(fun() ->
                        {ok, LogBody} = jsone_encode:encode(
                            #{
                                <<"account">> => Account,
                                <<"dtype">> => DType,
                                <<"ip">> => Ip
                            },
                            [native_utf8]
                        ),
                        _ = user_log_ds:add_internal(undefined, 100, Uid, LogBody, elib_dt:now())
                    end),
                    % 检查设备类型是否有效
                    case user_device_logic:validate_device_type(DType) of
                        false when DType =/= <<>> ->
                            % 无效的设备类型，但允许登录（兼容旧客户端）
                            {ok, Data};
                        _ ->
                            % 检查登录冲突
                            case user_device_logic:check_login_conflict(Uid, DType) of
                                {ok, no_conflict} ->
                                    % 无冲突，正常登录
                                    {ok, Data};
                                {ok, conflict, ConflictInfo} ->
                                    % 有冲突，返回冲突信息
                                    {{error, conflict}, ConflictInfo};
                                {error, _Reason} ->
                                    % 检查失败，允许登录
                                    {ok, Data}
                            end
                    end;
                {error, Reason} ->
                    % 登录失败，记录失败次数
                    _ = login_security_logic:record_login_failure(Account, Ip),
                    {error, Reason}
            end
    end.

-spec do_signup(
    Type :: binary(),
    EmailOrMobile :: binary(),
    Pwd :: binary(),
    Code :: binary(),
    PostVals :: map()
) ->
    {ok, map()} | {error, binary()}.
do_signup(<<"email">>, Email, Pwd, Code, PostVals) ->
    case elib_type:is_email(Email) of
        true ->
            % 校验验证码
            case consume_code(Email, Code) of
                {ok, _} ->
                    do_signup_by_email(Email, Pwd, PostVals);
                {error, Msg} ->
                    {error, Msg}
            end;
        false ->
            {error, <<"Email格式有误.."/utf8>>}
    end;
do_signup(<<"mobile">>, Mobile, Pwd, Code, PostVals) ->
    % 校验验证码
    case consume_code(Mobile, Code) of
        {ok, _} ->
            do_signup_by_mobile(Mobile, Pwd, PostVals);
        {error, Msg} ->
            {error, Msg}
    end;
do_signup(_Type, _Account, _Pwd, _Code, _PostVals) ->
    {error, <<"不支持的注册类型"/utf8>>}.

-spec find_password(
    Type :: binary(),
    EmailOrMobile :: binary(),
    Pwd :: binary(),
    Code :: binary(),
    PostVals :: map()
) ->
    {ok, map()} | {error, binary()}.
find_password(Type, Email, Pwd, Code, PostVals) when Type == <<"email">> ->
    ok = ?DEBUG_LOG(["Email ", Email, elib_type:is_email(Email)]),
    case elib_type:is_email(Email) of
        true ->
            % 校验验证码
            case consume_code(Email, Code) of
                {ok, _} ->
                    find_password_by_email(Email, Pwd, PostVals);
                {error, Msg} ->
                    {error, Msg}
            end;
        false ->
            {error, <<"Email格式有误..."/utf8>>}
    end;
find_password(_Type, _Account, _Pwd, _Code, _PostVals) ->
    {error, <<"不支持的注册类型"/utf8>>}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

-spec send_email_code(binary() | undefined) -> {error, binary()} | {ok, binary()}.
send_email_code(undefined) ->
    {error, <<"Email必须"/utf8>>};
send_email_code(ToEmail) ->
    Now = elib_dt:now(),
    NowP1M = elib_dt:minus(Now, {1, minute}),
    case verification_code_ds:find_by_id(ToEmail) of
        % 60000 = 60 * 1000 = 1分钟
        % #{<<"created_at">> := CreatedAt} when (Now - CreatedAt) < 60000 ->
        %  Now - 1m < CreatedAt
        #{<<"created_at">> := CreatedAt} when NowP1M < CreatedAt ->
            {ok, <<"一分钟内重复请求不发送Email"/utf8>>};
        #{<<"code">> := Code, <<"validity_at">> := ValidityAt} when Now < ValidityAt ->
            Msg = <<"Code is ", Code/binary, " will expire in 10 minutes.">>,
            _ = elib_email:send(ToEmail, Msg),
            {ok, <<"验证码已发送"/utf8>>};
        _ ->
            VerifyCode = elib_cipher:num_random(6),
            VerifyCodeBinary = integer_to_binary(VerifyCode),
            % 600000 = 600 * 1000 = 10分钟
            try elib_dt:add(Now, {10, minute}) of
                ValidityAt when is_binary(ValidityAt) ->
                    _ = verification_code_ds:save(ToEmail, VerifyCodeBinary, ValidityAt, Now),
                    Msg = <<"Code is ", VerifyCodeBinary/binary, " will expire in 10 minutes.">>,
                    _ = elib_email:send(ToEmail, Msg),
                    {ok, <<"验证码已发送"/utf8>>};
                _ ->
                    {error, <<"invalid_datetime"/utf8>>}
            catch
                _:_ ->
                    {error, <<"invalid_datetime"/utf8>>}
            end
    end.

-spec send_sms_code(binary()) -> {ok, binary()}.
send_sms_code(Mobile) ->
    Now = elib_dt:now(),
    NowP2M = elib_dt:minus(Now, {2, minute}),

    case verification_code_ds:find_by_id(Mobile) of
        % 120000 = 120 * 1000 = 2分钟
        #{<<"created_at">> := CreatedAt} when NowP2M < CreatedAt ->
            {ok, <<"两分钟内重复请求不会重复发送"/utf8>>};
        #{<<"code">> := Code, <<"validity_at">> := ValidityAt} when Now < ValidityAt ->
            Content =
                <<"【IMBoy】您的验证码： "/utf8, (ec_cnv:to_binary(Code))/binary,
                    " ，10分钟内有效。如非本人操作，请忽略！"/utf8>>,
            case imboy_sms:send(Mobile, Content, <<"yjsms">>) of
                ok -> ok;
                {error, SmsErr} -> ?WARN_LOG({sms_send_failed, Mobile, SmsErr})
            end,
            {ok, <<"验证码已发送"/utf8>>};
        _ ->
            Code = elib_cipher:num_random(6),
            CodeBinary = ec_cnv:to_binary(Code),
            % 600000 = 600 * 1000 = 10分钟
            try elib_dt:add(Now, {10, minute}) of
                ValidityAt when is_binary(ValidityAt) ->
                    _ = verification_code_ds:save(Mobile, CodeBinary, ValidityAt, Now),
                    Content =
                        <<"【IMBoy】您的验证码： "/utf8, CodeBinary/binary, " ，10分钟内有效。如非本人操作，请忽略！"/utf8>>,
                    case imboy_sms:send(Mobile, Content, <<"yjsms">>) of
                        ok -> ok;
                        {error, SmsErr2} -> ?WARN_LOG({sms_send_failed, Mobile, SmsErr2})
                    end,
                    {ok, <<"验证码已发送"/utf8>>};
                _ ->
                    {error, <<"invalid_datetime"/utf8>>}
            catch
                _:_ ->
                    {error, <<"invalid_datetime"/utf8>>}
            end
    end.

%% 校验验证码，委托给 verification_code_ds:verify_code/2
%% 万能码通过 {imboy, verification_master_code} 配置，不再硬编码
%% @doc 校验并**消费**验证码（一次性）。
%%
%% 原名 verify_code/2，只读不写：验证成功后码仍有效直到自然过期，
%% 同一个码可被反复使用。注册、找回密码、验证码登录都是"验证通过即产生
%% 权限变更"的入口，必须一次性消费，否则一次撞对可以反复利用。
-spec consume_code(binary(), binary()) -> {error, binary()} | {ok, binary()}.
consume_code(Id, Code) ->
    verification_code_ds:consume(Id, Code).

-spec do_signup_by_email(binary(), binary(), map()) ->
    {ok, map()} | {error, binary()} | {error, binary(), Code :: integer()}.
do_signup_by_email(Email, Pwd, PostVals) ->
    % 验证 nickname 不为空
    Nickname = maps:get(<<"nickname">>, PostVals, <<>>),
    case byte_size(Nickname) of
        0 ->
            {error, <<"昵称不能为空"/utf8>>};
        _ ->
            %% License 规模 gate：用户数达授权上限时拒绝新注册（402）
            case quota_guard() of
                {error, _, _} = QErr ->
                    QErr;
                ok ->
                    Id = user_ds:find_id_by_email(Email),
                    case Id of
                        0 ->
                            Data = pick_data_for_insert(
                                #{
                                    <<"password">> => elib_password:generate(Pwd),
                                    <<"email">> => Email
                                },
                                PostVals
                            ),
                            case user_ds:insert_and_get_id(Data) of
                                {ok, NewUid} ->
                                    %% 新手引导（AI 冷启动）：fire-and-forget，
                                    %% onboarding 任何失败不阻断注册
                                    elib_async:async(fun() ->
                                        user_onboarding_logic:after_signup(NewUid, Nickname)
                                    end),
                                    % 注册成功
                                    {ok, #{}};
                                {error,
                                    {error, error, <<"23505">>, unique_violation, _Msg, _Details}} ->
                                    % 唯一约束冲突，返回友好的错误信息
                                    {error, <<"账号已被占用"/utf8>>};
                                {error, Reason} ->
                                    {error, Reason}
                            end;
                        _ ->
                            {error, <<"Email已经被占用了"/utf8>>}
                    end
            end
    end.

-spec do_signup_by_mobile(binary(), binary(), map()) ->
    {ok, map()} | {error, binary() | any()} | {error, binary(), integer()}.
do_signup_by_mobile(Mobile, Pwd, PostVals) ->
    % 验证 nickname 不为空
    Nickname = maps:get(<<"nickname">>, PostVals, <<>>),
    case byte_size(Nickname) of
        0 ->
            {error, <<"昵称不能为空"/utf8>>};
        _ ->
            %% License 规模 gate：用户数达授权上限时拒绝新注册（402）
            case quota_guard() of
                {error, _, _} = QErr ->
                    QErr;
                ok ->
                    Id = user_ds:find_id_by_mobile(Mobile),
                    case Id of
                        0 ->
                            Data = pick_data_for_insert(
                                #{
                                    <<"password">> => elib_password:generate(Pwd),
                                    <<"mobile">> => Mobile
                                },
                                PostVals
                            ),
                            case user_ds:insert_and_get_id(Data) of
                                {ok, NewUid} ->
                                    %% 新手引导（AI 冷启动）：fire-and-forget，
                                    %% onboarding 任何失败不阻断注册
                                    elib_async:async(fun() ->
                                        user_onboarding_logic:after_signup(NewUid, Nickname)
                                    end),
                                    % 注册成功
                                    {ok, #{}};
                                {error, Reason} ->
                                    {error, Reason}
                            end;
                        _ ->
                            {error, <<"手机号已经被占用了"/utf8>>}
                    end
            end
    end.

%% @doc License 规模 gate：当前用户数达到授权 max_users 时拒绝新注册。
%% 容错：计数异常一律放行（fail-open），避免 License 计数 bug 阻断全站注册，
%% 与 imboy_license 的"无效降级不阻断"理念一致。
-spec quota_guard() -> ok | {error, binary(), integer()}.
quota_guard() ->
    try user_ds:count() of
        Count ->
            case imboy_license:check_user_quota(Count) of
                ok ->
                    ok;
                {error, quota_exceeded} ->
                    {error, <<"用户数已达授权上限，请联系管理员升级 License 后再注册"/utf8>>, ?ERR_PAYMENT_REQUIRED}
            end
    catch
        _:_ -> ok
    end.

pick_data_for_insert(Data, PostVals) ->
    Source = maps:get(<<"source">>, PostVals, <<>>),
    Ip = maps:get(<<"ip">>, PostVals, <<"{}">>),
    Nickname = maps:get(<<"nickname">>, PostVals, <<>>),
    Avatar = maps:get(<<"avatar">>, PostVals, <<>>),
    Cosv = maps:get(<<"cosv">>, PostVals, <<>>),
    RefUid = maps:get(<<"ref_uid">>, PostVals, <<>>),

    [RefUid2, ParentRefUid2] =
        case is_binary(RefUid) andalso bit_size(RefUid) > 5 of
            true ->
                RefUid2_in = ec_cnv:to_integer(RefUid),
                P = user_ds:find_by_id(RefUid2_in, <<"ref_user_id">>),
                [RefUid2_in, maps:get(<<"ref_user_id">>, P, 0)];
            _ ->
                [0, 0]
        end,
    % ?DEBUG_LOG(["RefUid2", RefUid2]),
    % ?DEBUG_LOG(["Email", Email]),
    % ?DEBUG_LOG(["Pwd2", Pwd2]),
    % ?DEBUG_LOG(["PostVals", PostVals]),
    % ?DEBUG_LOG(["Ip", Ip]),
    % ?DEBUG_LOG(["Cosv", Cosv]),
    maps:merge(
        #{
            <<"account">> => account_ds:allocate(),
            <<"nickname">> => Nickname,
            <<"avatar">> => Avatar,
            <<"ref_user_id">> => RefUid2,
            <<"ref_parent_user_id">> => ParentRefUid2,
            <<"reg_ip">> => Ip,
            <<"reg_cosv">> => Cosv,
            <<"source">> => Source,
            <<"status">> => 1,
            <<"created_at">> => elib_dt:now()
        },
        Data
    ).

-spec find_password_by_email(Email :: binary(), Pwd :: binary(), PostVals :: map()) ->
    {ok, map()} | {error, Msg :: list()}.
find_password_by_email(Email, Pwd, _PostVals) ->
    Id = user_ds:find_id_by_email(Email),
    case Id of
        0 ->
            {error, <<"Email不存在或已被删除"/utf8>>};
        Id when is_integer(Id), Id > 0 ->
            % 密码已经在 handler 层面处理了解密，这里直接使用
            Pwd2 = elib_password:generate(Pwd),
            case user_ds:update_password(Id, Pwd2) of
                {ok, _} ->
                    {ok, #{}};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

-spec verify_user(binary(), map(), binary()) -> {ok, map()} | {error, any()}.
verify_user(<<>>, _, _Did) ->
    {error, <<"账号不存在"/utf8>>};
verify_user(_Pwd, User, _Did) when map_size(User) =:= 0 ->
    {error, <<"账号不存在"/utf8>>};
verify_user(Pwd, User, Did) ->
    Pwd2 = maps:get(<<"password">>, User, <<>>),
    % 状态: -1 删除  0 禁用  1 启用  2 申请注销中
    Status = maps:get(<<"status">>, User, -2),
    case elib_password:verify(Pwd, Pwd2) of
        {ok, _} when Status == -2 ->
            {error, <<"账号不存在"/utf8>>};
        {ok, _} when Status == -1 ->
            {error, <<"账号不存在或者已删除"/utf8>>};
        {ok, _} when Status == 0 ->
            {error, <<"账号被禁用"/utf8>>};
        {ok, _} when Status == 1; Status == 2 ->
            % E2EE-013：绑定登录设备 DID 进 token。
            {ok, login_resp(User, Did, #{})};
        {error, Msg} ->
            {error, Msg}
    end.

login_resp(User, Resp) ->
    %% 兼容旧调用（无设备 DID）→ legacy token，crypto 写端点将 fail-closed。
    login_resp(User, <<>>, Resp).

%% @doc E2EE-013：签发绑定设备 DID 的 token/refreshtoken。
login_resp(User, Did, Resp) ->
    Id = maps:get(<<"id">>, User),
    % 从 User 数据中读取 role 字段；user 表暂未设置 role 列，默认为 1（普通用户）
    % 待 user 表添加 role 列后，LOGIN_COLUMN 中需加入 role，届时可自动生效
    Role = maps:get(<<"role">>, User, 1),
    maps:merge(
        #{
            <<"uid">> => Id,
            <<"user_id">> => Id,
            <<"token">> => token_ds:encrypt_token(Id, Did),
            <<"refreshtoken">> => token_ds:encrypt_refreshtoken(Id, Did),
            <<"email">> => maps:get(<<"email">>, User),
            <<"nickname">> => maps:get(<<"nickname">>, User),
            <<"avatar">> => maps:get(<<"avatar">>, User),
            <<"account">> => maps:get(<<"account">>, User),
            <<"gender">> => maps:get(<<"gender">>, User),
            <<"region">> => maps:get(<<"region">>, User),
            <<"sign">> => maps:get(<<"sign">>, User),
            <<"status">> => maps:get(<<"status">>, User),
            <<"role">> => Role
        },
        Resp
    ).

validate_compat_password(<<>>) ->
    {error, <<"密码不能为空"/utf8>>};
validate_compat_password(Pwd) when is_binary(Pwd) ->
    Len = byte_size(Pwd),
    HasDigit = lists:any(fun(C) -> C >= $0 andalso C =< $9 end, binary_to_list(Pwd)),
    HasAlpha = lists:any(
        fun(C) ->
            (C >= $a andalso C =< $z) orelse (C >= $A andalso C =< $Z)
        end,
        binary_to_list(Pwd)
    ),
    case Len >= 8 andalso HasDigit andalso HasAlpha of
        true ->
            ok;
        false ->
            {error, <<"密码格式不正确"/utf8>>}
    end.

compat_signup_nickname(Mobile, PostVals) ->
    case maps:get(<<"nickname">>, PostVals, <<>>) of
        <<>> ->
            Mobile;
        Nickname ->
            Nickname
    end.

prepare_compat_mobile(Mobile) ->
    case user_ds:find_by_mobile(Mobile, <<"id,status">>) of
        #{<<"id">> := Id, <<"status">> := Status} when Status < 0 ->
            ok = user_ds:delete_all_related_data(Id),
            ok;
        #{<<"id">> := _Id} ->
            {error, <<"手机号已经被占用了"/utf8>>, 400};
        _ ->
            ok
    end.

compat_signup_email(Email) ->
    case elib_type:is_email(Email) of
        false ->
            undefined;
        true ->
            case user_ds:find_by_email(Email, <<"id,status">>) of
                #{<<"id">> := Id, <<"status">> := Status} when Status < 0 ->
                    ok = user_ds:delete_all_related_data(Id),
                    Email;
                #{<<"id">> := _Id} ->
                    undefined;
                _ ->
                    Email
            end
    end.

compat_login_type(Account) ->
    case elib_type:is_email(Account) of
        true ->
            <<"email">>;
        false ->
            <<"mobile">>
    end.

compat_device_type(PostVals, Did) ->
    case maps:get(<<"dtype">>, PostVals, maps:get(<<"cos">>, PostVals, <<>>)) of
        DType when
            DType == <<"ios">>;
            DType == <<"android">>;
            DType == <<"macos">>;
            DType == <<"windows">>;
            DType == <<"linux">>;
            DType == <<"web">>
        ->
            DType;
        _ ->
            infer_device_type(Did)
    end.

infer_device_type(Did) when is_binary(Did) ->
    LowerDid = cowboy_bstr:to_lower(Did),
    case binary:match(LowerDid, <<"android">>) of
        {_, _} ->
            <<"android">>;
        nomatch ->
            case binary:match(LowerDid, <<"web">>) of
                {_, _} ->
                    <<"web">>;
                nomatch ->
                    case binary:match(LowerDid, <<"mac">>) of
                        {_, _} ->
                            <<"macos">>;
                        nomatch ->
                            case binary:match(LowerDid, <<"win">>) of
                                {_, _} ->
                                    <<"windows">>;
                                nomatch ->
                                    case binary:match(LowerDid, <<"linux">>) of
                                        {_, _} ->
                                            <<"linux">>;
                                        nomatch ->
                                            <<"ios">>
                                    end
                            end
                    end
            end
    end.

record_compat_login_success(Data, PostVals) ->
    UidHash = maps:get(<<"uid">>, Data, <<>>),
    Uid = ec_cnv:to_integer(UidHash),
    Did = maps:get(<<"did">>, PostVals, <<>>),
    Now = elib_dt:now(),
    _ = user_device_ds:save(Now, Uid, Did, PostVals),
    _ = user_ds:update_friends_last_seen_at(Uid, Now),
    _ = message_ds:check_and_notify_offline_msgs(Uid, Did),
    ok.

%% @doc 获取用户登录后的设置信息
%% Handler 层通过此接口获取设置，不直接访问 user_setting_ds
%% @param Uid 用户ID
%% @return map() 用户设置
-spec find_user_setting(integer()) -> map().
find_user_setting(Uid) ->
    user_setting_ds:find_by_uid(Uid).

%% @doc 检查邮箱是否已被注册
%% @param Email 邮箱地址
%% @return boolean()
-spec email_in_use(binary()) -> boolean().
email_in_use(Email) ->
    user_ds:find_id_by_email(Email) > 0.

%% @doc 绑定邮箱到指定用户
%% @param Uid 用户ID
%% @param Email 邮箱地址
%% @return {ok, integer()} | {error, any()}
-spec bind_email(integer(), binary()) -> {ok, integer()} | {error, any()}.
bind_email(Uid, Email) ->
    user_ds:bind_email(Uid, Email).

%% @doc 检查手机号是否已注册
%% @param Mobile 手机号
%% @return boolean()
-spec mobile_registered(binary()) -> boolean().
mobile_registered(Mobile) ->
    user_ds:find_id_by_mobile(Mobile) > 0.

compat_error_message(#{<<"reason">> := Reason}) when is_binary(Reason) ->
    Reason;
compat_error_message(Map) when is_map(Map) ->
    jsx:encode(Map);
compat_error_message(Reason) when is_binary(Reason) ->
    Reason;
compat_error_message(Reason) when is_list(Reason) ->
    unicode:characters_to_binary(Reason);
compat_error_message(Reason) when is_atom(Reason) ->
    atom_to_binary(Reason, utf8);
compat_error_message(Reason) ->
    ec_cnv:to_binary(Reason).
