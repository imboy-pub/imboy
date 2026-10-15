-module(passport_logic).
%% Stable identity passport domain boundary.
%% API adapters should call this module instead of reaching auth/user internals directly.
-compile([nowarn_deprecated_catch]).
-dialyzer({nowarn_function, [{send_email_code, 1}, {send_sms_code, 1}, {do_login_verify, 6}]}).
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
-export([alipay_login/2]).
-export([alipay_authinfo/0]).
-export([find_user_setting/1]).
-export([record_login_device/2]).
-export([email_in_use/1]).
-export([bind_email/2]).
-export([mobile_registered/1]).
-export([consume_code/2]).
-export([refresh_session_epoch_valid/3]).
%% P0-C: OIDC 登录流复用（auth_oidc_logic）——签发收口 + License 配额闸门 + 建号数据组装
-export([login_resp/2]).
-export([quota_guard/0]).
-export([pick_data_for_insert/2]).

-include("log.hrl").
-include("common.hrl").
-include("error_code.hrl").

-spec refresh_session_epoch_valid(integer(), binary(), term()) -> boolean().
refresh_session_epoch_valid(_Uid, <<>>, _Epoch) ->
    true;
refresh_session_epoch_valid(Uid, _Did, Epoch) ->
    not auth_session_ds:revoked(Uid, Epoch).

%% @doc 快速登录（支持第三方服务）
%% @param Service 登录服务类型（如 jverify）
%% @param Operator 操作员
%% @param Token 认证令牌
%% @param PostVals 请求参数
%% @returns {ok, Data} | {error, Msg}
-spec quick_login(binary(), binary() | undefined, binary(), map()) ->
    {ok, map()} | {error, binary() | map()} | {error, binary(), integer()}.

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
                    %% License 规模 gate：用户数达授权上限时拒绝新注册（402）。
                    %% 只挡"号码首次出现即注册"这一条分支——已存在用户走下面的
                    %% login_resp，配额满时不能把老用户一起锁在门外。
                    case quota_guard() of
                        {error, _, _} = QErr ->
                            QErr;
                        ok ->
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
                                    case user_ds:find_by_id(Uid2, ?LOGIN_COLUMN) of
                                        User2 when map_size(User2) =:= 0 ->
                                            {error, <<"注册异常，请稍后重试"/utf8>>};
                                        User2 ->
                                            {ok,
                                                login_resp(User2, Did, #{
                                                    <<"action">> => <<"need_set_password">>
                                                })}
                                    end;
                                {error, Reason} ->
                                    {error, Reason}
                            end
                    end;
                _ ->
                    {ok, login_resp(User, Did, #{})}
            end
    end;
quick_login(_, _, _, _) ->
    {error, <<"不支持的登录服务"/utf8>>}.

%%%===================================================================
%%% APP 支付宝登录：auth_code → oauth.token → user.info.share → 映射/建号 → 签发
%%% 身份映射复用 sso_identity（provider=alipay, subject=支付宝 user_id），
%%% 与 OIDC 登录流同一 find-or-create 范式；支付宝侧不暴露 email/手机号，
%%% 故无"按 email 关联存量账号"分支，一律 (provider, subject) 直查。
%%%===================================================================

-define(ALIPAY_PROVIDER, <<"alipay">>).

%% @doc 支付宝登录。AuthCode 为客户端 SDK 授权码；PostVals 带 did/ip/cosv。
%% 返回 {ok, 登录态 map} | {error, Msg} | {error, Msg, Code}（402=配额满）。
-spec alipay_login(binary(), map()) ->
    {ok, map()} | {error, binary()} | {error, binary(), integer()}.
alipay_login(AuthCode, PostVals) ->
    Did = maps:get(<<"did">>, PostVals, <<>>),
    case alipay_cfg() of
        {ok, Cfg} ->
            case alipay_openapi:oauth_token(Cfg, AuthCode) of
                %% AlipayUid 必须非空：空 uid 若继续 bind 会创建 (alipay, <<>>)
                %% 身份映射，所有拿不到 user_id 的登录都会撞进同一账号（混淆）
                {ok, #{access_token := AccessToken, user_id := AlipayUid}} when
                    AlipayUid =/= <<>>
                ->
                    case alipay_openapi:user_info_share(Cfg, AccessToken) of
                        {ok, Info} ->
                            alipay_map_or_provision(AlipayUid, Info, Did, PostVals);
                        {error, Msg} ->
                            {error, Msg}
                    end;
                {ok, _} ->
                    {error, <<"支付宝授权失败：未获取到用户标识"/utf8>>};
                {error, Msg} ->
                    {error, Msg}
            end;
        {error, Msg} ->
            {error, Msg}
    end.

%% @doc 生成客户端唤起支付宝 SDK 的 authInfo（私钥不出服务端）。
%% target_id 每次新生成（16 hex），客户端拿到 authinfo 后直接喂 SDK。
-spec alipay_authinfo() -> {ok, map()} | {error, binary()}.
alipay_authinfo() ->
    case alipay_cfg() of
        {ok, Cfg} ->
            TargetId = epay_crypto:lower_hex(crypto:strong_rand_bytes(8)),
            case alipay_openapi:authinfo(Cfg, TargetId) of
                {ok, Info} ->
                    {ok, #{<<"authinfo">> => Info}};
                {error, Msg} ->
                    {error, Msg}
            end;
        {error, Msg} ->
            {error, Msg}
    end.

%% 凭据读取（IMBOY_* 注入）：app_id/private_key 必填；
%% app_cert_sn/alipay_root_cert_sn 仅证书模式需要（部署脚本由证书 PEM 算好注入）。
-spec alipay_cfg() -> {ok, alipay_openapi:cfg()} | {error, binary()}.
alipay_cfg() ->
    AppId = application:get_env(imboy, alipay_app_id, <<>>),
    PriKey = application:get_env(imboy, alipay_private_key, <<>>),
    case AppId =/= <<>> andalso PriKey =/= <<>> of
        true ->
            {ok, #{
                app_id => AppId,
                private_key => PriKey,
                pid => application:get_env(imboy, alipay_pid, <<>>),
                aes_key => application:get_env(imboy, alipay_aes_key, <<>>),
                app_cert_sn => application:get_env(imboy, alipay_app_cert_sn, <<>>),
                alipay_root_cert_sn => application:get_env(imboy, alipay_root_cert_sn, <<>>)
            }};
        false ->
            {error, <<"支付宝登录未配置凭据"/utf8>>}
    end.

-spec alipay_map_or_provision(binary(), map(), binary(), map()) ->
    {ok, map()} | {error, binary()} | {error, binary(), integer()}.
alipay_map_or_provision(AlipayUid, Info, Did, PostVals) ->
    case sso_identity_ds:find_uid(?ALIPAY_PROVIDER, AlipayUid) of
        {ok, Uid} ->
            auto_bind_alipay(Uid, Info, AlipayUid),
            alipay_finish(Uid, Did);
        not_found ->
            alipay_provision(AlipayUid, Info, Did, PostVals);
        {error, Reason} ->
            ?ERROR_LOG("alipay_login find_uid error ~p", [Reason]),
            {error, <<"支付宝登录失败，请稍后再试"/utf8>>}
    end.

-spec alipay_provision(binary(), map(), binary(), map()) ->
    {ok, map()} | {error, binary()} | {error, binary(), integer()}.
alipay_provision(AlipayUid, Info, Did, PostVals) ->
    case quota_guard() of
        {error, _, _} = QErr ->
            QErr;
        ok ->
            %% SSO 用户不走密码登录：随机密码占位（与 OIDC provision 同款）
            RandPwd = base64:encode(crypto:strong_rand_bytes(32)),
            Data = pick_data_for_insert(
                #{
                    <<"password">> => elib_password:generate(RandPwd),
                    <<"gender">> => alipay_gender(maps:get(<<"gender">>, Info, <<>>))
                },
                PostVals#{
                    <<"source">> => ?ALIPAY_PROVIDER,
                    <<"nickname">> => alipay_nickname(
                        maps:get(<<"nick_name">>, Info, <<>>), AlipayUid
                    ),
                    <<"avatar">> => maps:get(<<"avatar">>, Info, <<>>)
                }
            ),
            case user_ds:insert_and_get_id(Data) of
                {ok, Uid2} ->
                    ok = sso_identity_ds:bind(?ALIPAY_PROVIDER, AlipayUid, Uid2, <<>>),
                    auto_bind_alipay(Uid2, Info, AlipayUid),
                    alipay_finish(Uid2, Did);
                {error, {error, error, <<"23505">>, unique_violation, _Msg, _Details}} ->
                    %% 并发建号竞态：回读映射，命中即直登（与 OIDC 同款兜底）
                    case sso_identity_ds:find_uid(?ALIPAY_PROVIDER, AlipayUid) of
                        {ok, Uid3} ->
                            alipay_finish(Uid3, Did);
                        _ ->
                            {error, <<"支付宝登录失败，请稍后再试"/utf8>>}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% 签发收口：status 1 启用 / 2 申请注销中 放行，其余拒绝（与 OIDC finish_login 同严）
-spec alipay_finish(integer(), binary()) -> {ok, map()} | {error, binary()}.
alipay_finish(Uid, Did) ->
    User = user_ds:find_by_id(Uid, ?LOGIN_COLUMN),
    case maps:get(<<"status">>, User, -2) of
        1 ->
            {ok, login_resp(User, Did, #{})};
        2 ->
            {ok, login_resp(User, Did, #{})};
        0 ->
            {error, <<"账号被禁用"/utf8>>};
        _ ->
            {error, <<"账号不存在或者已删除"/utf8>>}
    end.

%% 支付宝 gender：m 男 f 女；user 表 smallint 1 男 2 女 0 未知（chk_user_gender）
-spec alipay_gender(term()) -> 0 | 1 | 2.
alipay_gender(<<"m">>) -> 1;
alipay_gender(<<"f">>) -> 2;
alipay_gender(_) -> 0.

%% 昵称兜底：用户未公开昵称时用 alipay_ + 支付宝 user_id 尾 6 位
-spec alipay_nickname(binary(), binary()) -> binary().
alipay_nickname(<<>>, AlipayUid) ->
    Tail = binary:part(AlipayUid, byte_size(AlipayUid), -6),
    <<"alipay_", Tail/binary>>;
alipay_nickname(Nick, _AlipayUid) ->
    Nick.

%% 支付宝登录成功时，自动填充 setting.alipay（仅首次或为空时）。
%% 不覆盖已有值（手动填过的结算账号）。
-spec auto_bind_alipay(integer(), map(), binary()) -> ok.
auto_bind_alipay(Uid, Info, AlipayUid) ->
    Setting = user_setting_ds:find_by_uid(Uid),
    case maps:get(<<"alipay">>, Setting, <<>>) of
        <<>> ->
            NickName = alipay_nickname(
                maps:get(<<"nick_name">>, Info, <<>>), AlipayUid
            ),
            user_setting_ds:save(Uid, <<"alipay">>, NickName);
        _ ->
            ok
    end.

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
    %% email 路径此前完全没有 throttle 守卫（只有 sms 有），补齐。
    %% 两条路径都按目标（手机号/邮箱）计数而非按调用方自报的值，
    %% 攻击者换 key 就等于换受害目标，无法用来放大对单一目标的轰炸。
    case throttle:check(per_minute_once, {send_code, EMail}) of
        {limit_exceeded, _, _} ->
            {error, <<"per_minute_once">>};
        _ ->
            send_email_code(EMail)
    end;
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
                                    case user_ds:find_by_id(Uid, ?LOGIN_COLUMN) of
                                        User when map_size(User) =:= 0 ->
                                            {error, <<"注册异常，请稍后重试"/utf8>>, 500};
                                        User ->
                                            {ok, login_resp(User, #{})}
                                    end;
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
        {error, service_unavailable} ->
            %% 服务不可用不冒充普通口令错误（C.6：503 映射）
            {error, <<"服务暂不可用，请稍后重试"/utf8>>, 503};
        {error, Msg} ->
            {error, compat_error_message(Msg), 400}
    end.

-spec do_login(binary(), binary(), binary()) -> {ok, map()} | {error, binary() | map()}.
do_login(Type, Account, Pwd) ->
    do_login(Type, Account, Pwd, <<>>, <<>>).

%% @doc 账号密码登录（带设备信息）
%% 检查设备冲突，如果同类型设备已登录则返回冲突信息
-spec do_login(binary(), binary(), binary(), binary(), binary()) ->
    {ok, map()} | {{error, conflict}, map()} | {error, binary() | map() | service_unavailable}.
do_login(_Type, _Email, <<>>, _DType, _Did) ->
    %% 空密码属格式非法提前返回（C.2）：不进入校验、不计数
    {error, <<"密码有误"/utf8>>};
do_login(Type, Mobile, Pwd, DType, Did) when Type == <<"mobile">> ->
    User = user_ds:find_by_mobile(Mobile, ?LOGIN_COLUMN),
    do_login_verify(Type, Mobile, User, Pwd, DType, Did);
do_login(Type, Email, Pwd, DType, Did) when Type == <<"email">> ->
    case elib_type:is_email(Email) of
        true ->
            User = user_ds:find_by_email(Email, ?LOGIN_COLUMN),
            do_login_verify(Type, Email, User, Pwd, DType, Did);
        false ->
            %% Email 格式非法提前返回（C.2）：不产生身份、不计数
            {error, <<"Email格式有误."/utf8>>}
    end;
do_login(Type, Account, Pwd, DType, Did) when Type == <<"account">> ->
    User =
        case elib_type:is_email(Account) of
            true ->
                user_ds:find_by_email(Account, ?LOGIN_COLUMN);
            false ->
                %% 批次123（2026-09-07）：account 查无时回退 mobile 查询。
                %% UI 登录恒发 type=account（login_page 按 @ 判定邮箱，其余
                %% 全走 account），而手机号注册用户的 account 字段是系统
                %% 分配号（≠手机号）——不回退则手机号+密码登录恒报
                %% 「账号不存在」，新注册用户无法用密码登录（GF9 实证）。
                case user_ds:find_by_account(Account, ?LOGIN_COLUMN) of
                    #{} = U when map_size(U) > 0 ->
                        U;
                    _ ->
                        case user_ds:find_by_mobile(Account, ?LOGIN_COLUMN) of
                            #{} = U2 when map_size(U2) > 0 -> U2;
                            _ -> #{}
                        end
                end
        end,
    do_login_verify(Type, Account, User, Pwd, DType, Did).

%% @doc 验证码登录（mobile/email）
%% 客户端发送 code 字段代替 pwd
%%
%% Limiter V1 L3 编排（计划附录 C.1/C.2/C.6）：
%%   begin_attempt(Identity) → check_login_allowed → consume_code → status
%%   → reset 确认成功才 login_resp。三条 F 修复均在此链闭合：
%%   - F2：check 的 {error, locked, _} 三元组与 {error, unavailable} 显式
%%     分支，不再落入默认放行；锁定时验证码**不被消费**；
%%   - F3：失败计数与成功 reset 用同一 identity_of 键（已知用户
%%     {user, Uid}，与输入别名无关），不再按输入/User.account 分叉；
%%   - AC-17 成功点：reset 在 status 检查**之后**、签发之前，且必须确认
%%     ok 才 login_resp；reset 失败 → 丢弃凭据，返回服务不可用。
-spec do_login_by_code(binary(), binary(), binary(), binary(), binary()) ->
    {ok, map()}
    | {{error, conflict}, map()}
    | {error, binary() | map() | service_unavailable}.
do_login_by_code(Type, Account, Code, DType, Did) ->
    %% 身份先于锁定门解析：已知用户用稳定 {user, Uid}，未知输入用
    %% 有界 {unknown, Kind, Norm}（C.1），consume 失败按同一键记账。
    User = find_user_by_type(Type, Account),
    Identity = identity_of(Type, Account, User),
    case login_attempt_ds:begin_attempt(Identity, self(), default) of
        {error, _Unavailable} ->
            {error, service_unavailable};
        {ok, Ref} ->
            try do_code_auth(Ref, Identity, Account, Code, User, DType, Did) of
                Result ->
                    Result
            catch
                %% 认证主体任何未预期异常：终态必达（no_count 兜底），
                %% 否则 DOWN 会把账户域打成 degraded（保守但过重）。
                Class:Reason:Stack ->
                    login_attempt_ds:end_attempt(Ref, no_count),
                    erlang:raise(Class, Reason, Stack)
            end
    end.

%% @private 已登记生命周期，过账户锁定门。
-spec do_code_auth(
    Ref :: term(),
    Identity :: login_attempt_state:identity(),
    Account :: binary(),
    Code :: binary(),
    User :: map(),
    DType :: binary(),
    Did :: binary()
) -> {ok, map()} | {{error, conflict}, map()} | {error, binary() | service_unavailable}.
do_code_auth(Ref, Identity, Account, Code, User, DType, Did) ->
    case login_security_logic:check_login_allowed(Identity) of
        {ok, true} ->
            do_code_consume(Ref, Identity, Account, Code, User, DType, Did);
        {error, locked, LockInfo} ->
            %% 锁定拒绝：不触碰验证码存储（consume 是观测点，AC-02）
            login_attempt_ds:end_attempt(Ref, no_count),
            {error, lock_error_msg(LockInfo)};
        {error, _Unavailable} ->
            %% 不得默认放行（F2 的另一半）：转服务不可用
            login_attempt_ds:end_attempt(Ref, no_count),
            {error, service_unavailable}
    end.

%% @private 过锁后消费验证码；consume 失败 = 凭据失败（C.2，含未知账户）。
-spec do_code_consume(
    Ref :: term(),
    Identity :: login_attempt_state:identity(),
    Account :: binary(),
    Code :: binary(),
    User :: map(),
    DType :: binary(),
    Did :: binary()
) -> {ok, map()} | {{error, conflict}, map()} | {error, binary() | service_unavailable}.
do_code_consume(Ref, Identity, Account, Code, User, DType, Did) ->
    case consume_code(Account, Code) of
        {ok, _} ->
            do_code_finish(Ref, Identity, User, DType, Did);
        {error, Msg} ->
            %% 关键：验证码错误必须计数，否则 6 位码等于可无限尝试。
            %% failure 先提交（可观测 locked），no_count 仅释放生命周期。
            _ = login_security_logic:record_login_failure(Identity),
            login_attempt_ds:end_attempt(Ref, no_count),
            {error, Msg}
    end.

%% @private 验证码已消费（不可回滚），按账户状态收尾。
%% consume 成功但用户查无/禁用 → no_count（C.2：非凭据失败）。
-spec do_code_finish(
    Ref :: term(),
    Identity :: login_attempt_state:identity(),
    User :: map(),
    DType :: binary(),
    Did :: binary()
) -> {ok, map()} | {{error, conflict}, map()} | {error, binary() | service_unavailable}.
do_code_finish(Ref, Identity, User, DType, Did) ->
    Status = maps:get(<<"status">>, User, -2),
    case Status of
        -2 ->
            login_attempt_ds:end_attempt(Ref, no_count),
            {error, <<"账号不存在"/utf8>>};
        -1 ->
            login_attempt_ds:end_attempt(Ref, no_count),
            {error, <<"账号不存在或者已删除"/utf8>>};
        0 ->
            login_attempt_ds:end_attempt(Ref, no_count),
            {error, <<"账号被禁用"/utf8>>};
        _ when Status == 1; Status == 2 ->
            %% 成功点：reset 确认成功才签发（AC-17）；此前 reset 早于
            %% status 检查，禁用账户也会被清掉失败历史（F3b）。
            case login_security_logic:record_login_success(Identity) of
                ok ->
                    login_attempt_ds:end_attempt(Ref, no_count),
                    code_login_success(User, DType, Did);
                {error, _Unavailable} ->
                    %% code 已消费不可回滚 → 本次不给凭据，需重新取得有效码
                    login_attempt_ds:end_attempt(Ref, no_count),
                    {error, service_unavailable}
            end;
        _ ->
            login_attempt_ds:end_attempt(Ref, no_count),
            {error, <<"账号不存在"/utf8>>}
    end.

%% @doc 验证码登录成功链：签发 + 审计 + 设备冲突检查。
-spec code_login_success(map(), binary(), binary()) ->
    {ok, map()} | {{error, conflict}, map()}.
code_login_success(User, DType, Did) ->
    Account = maps:get(
        <<"account">>, User, maps:get(<<"mobile">>, User, maps:get(<<"email">>, User, <<>>))
    ),
    case maps:get(<<"status">>, User, -2) of
        2 ->
            {ok, login_resp(User, Did, #{})};
        _ ->
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
            login_conflict_result(Uid, DType, Data)
    end.

%% @doc 登录验证（带设备冲突检查）
%% Limiter V1 L3 编排（C.2 事件表 / AC-17 成功点）：
%%   begin_attempt → 锁定门（F 后拒绝且不继续认证）→
%%   verify_credential 三态分类 →
%%   - 凭据失败（密码错/查无用户）：failure 记账 + 终态收尾；
%%   - 密码正确但 status 拒绝：no_count（不 reset 不计失败）；
%%   - 凭据正确且 status 允许：reset 确认成功才 login_resp 签发；
%%     reset 失败 → 丢弃响应 Data、不返回凭据，服务不可用。
%% Ip 在此层不可用也无需伪造：账户域跨 IP（C.1）。
-spec do_login_verify(binary(), binary(), map(), binary(), binary(), binary()) ->
    {ok, map()}
    | {{error, conflict}, map()}
    | {error, binary() | map() | service_unavailable}.
do_login_verify(Type, Account, User, Pwd, DType, Did) ->
    Identity = identity_of(Type, Account, User),
    case login_attempt_ds:begin_attempt(Identity, self(), default) of
        {error, _Unavailable} ->
            {error, service_unavailable};
        {ok, Ref} ->
            try do_login_auth(Ref, Identity, User, Pwd, DType, Did) of
                Result ->
                    Result
            catch
                Class:Reason:Stack ->
                    login_attempt_ds:end_attempt(Ref, no_count),
                    erlang:raise(Class, Reason, Stack)
            end
    end.

%% @private 密码认证主体：锁定门 → verify_credential 三态 → 记账/签发。
-spec do_login_auth(
    Ref :: term(),
    Identity :: login_attempt_state:identity(),
    User :: map(),
    Pwd :: binary(),
    DType :: binary(),
    Did :: binary()
) ->
    {ok, map()}
    | {{error, conflict}, map()}
    | {error, binary() | service_unavailable}.
do_login_auth(Ref, Identity, User, Pwd, DType, Did) ->
    %% 锁定门（AC-01：F 后拒绝且不继续认证——锁定时不再触碰凭据校验）
    case login_security_logic:check_login_allowed(Identity) of
        {ok, true} ->
            do_login_credential(Ref, Identity, User, Pwd, DType, Did);
        {error, locked, LockInfo} ->
            login_attempt_ds:end_attempt(Ref, no_count),
            {error, lock_error_msg(LockInfo)};
        {error, _Unavailable} ->
            %% 不得默认放行（F2 同款）：转服务不可用
            login_attempt_ds:end_attempt(Ref, no_count),
            {error, service_unavailable}
    end.

%% @private 已过锁定门，凭据校验与分类收尾。
-spec do_login_credential(
    Ref :: term(),
    Identity :: login_attempt_state:identity(),
    User :: map(),
    Pwd :: binary(),
    DType :: binary(),
    Did :: binary()
) ->
    {ok, map()}
    | {{error, conflict}, map()}
    | {error, binary() | service_unavailable}.
do_login_credential(Ref, Identity, User, Pwd, DType, Did) ->
    case verify_credential(Pwd, User) of
        {ok, User2} ->
            %% 成功点：reset 确认成功才签发（AC-17）。签发（login_resp/
            %% token）此前发生在 reset 之前，reset 失败时凭据已生成——
            %% 现在丢弃的是"尚未生成"的状态。
            case login_security_logic:record_login_success(Identity) of
                ok ->
                    login_attempt_ds:end_attempt(Ref, no_count),
                    password_login_success(User2, DType, Did);
                {error, _Unavailable} ->
                    login_attempt_ds:end_attempt(Ref, no_count),
                    {error, service_unavailable}
            end;
        {reject, Msg} ->
            %% 密码正确但 status 禁用/删除：不 reset、不计失败（C.2）
            login_attempt_ds:end_attempt(Ref, no_count),
            {error, Msg};
        {fail, Msg} ->
            %% 凭据失败（密码错/查无用户，含未知账户）：failure 记账一次，
            %% end_attempt(no_count) 仅释放生命周期（若用 end_attempt(failure)
            %% 会再记一次——apply_outcome(failure) 自带记账，单次提交语义）。
            _ = login_security_logic:record_login_failure(Identity),
            login_attempt_ds:end_attempt(Ref, no_count),
            {error, Msg}
    end.

%% @private 密码登录成功链：签发 + 审计 + 设备类型/冲突检查。
-spec password_login_success(map(), binary(), binary()) ->
    {ok, map()} | {{error, conflict}, map()}.
password_login_success(User, DType, Did) ->
    Account = maps:get(
        <<"account">>, User, maps:get(<<"mobile">>, User, maps:get(<<"email">>, User, <<>>))
    ),
    Data = login_resp(User, Did, #{}),
    UidHashed = maps:get(<<"uid">>, Data),
    Uid = ec_cnv:to_integer(UidHashed),
    % GAP-01: 异步写登录成功审计日志（type=100），fire-and-forget 不阻塞登录
    _ = elib_async:async(fun() ->
        {ok, LogBody} = jsone_encode:encode(
            #{
                <<"account">> => Account,
                <<"dtype">> => DType,
                <<"ip">> => <<>>
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
            login_conflict_result(Uid, DType, Data)
    end.

%% @private 设备冲突检查收口（成功链共用）。
-spec login_conflict_result(integer(), binary(), map()) ->
    {ok, map()} | {{error, conflict}, map()}.
login_conflict_result(Uid, DType, Data) ->
    case user_device_logic:check_login_conflict(Uid, DType) of
        {ok, no_conflict} ->
            {ok, Data};
        {ok, conflict, ConflictInfo} ->
            ConflictDID = maps:get(<<"device_id">>, ConflictInfo),
            _ = user_device_logic:kick_device(Uid, DType, ConflictDID),
            {ok, Data};
        {error, _Reason} ->
            {ok, Data}
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
%% 批次124（2026-09-07）：UI 忘记密码页手机号链恒发 type=mobile，此前的
%% 兜底子句把它当「不支持的注册类型」整链拒绝——手机号用户无法通过验证码
%% 找回密码（getcode 照常发码落库，走到 findpassword 才爆，两头不一致）。
%% 号码格式不做 is_mobile 严格校验：UI 传的是带国家码前缀的完整号码
%% （如 <<"+86199...">>），is_mobile 的 ^1[0-9]{10}$ 不匹配，加了反而挡住
%% 合法链路；查无此号由 find_password_by_mobile 的 Id=0 分支兜底。
find_password(Type, Mobile, Pwd, Code, PostVals) when Type == <<"mobile">>; Type == <<"sms">> ->
    case consume_code(Mobile, Code) of
        {ok, _} ->
            find_password_by_mobile(Mobile, Pwd, PostVals);
        {error, Msg} ->
            {error, Msg}
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
        %% 【已移除】原先这里有一条"有效期内重发同一个码"的分支，且重发时
        %% 不调用 save/4 刷新 created_at。后果有两个：
        %%   1) 同一个 6 位码稳定存活 10 分钟，给穷举 000000-999999 留出窗口；
        %%   2) created_at 永远停在首次发送时刻，上面那条"一分钟内不重发"
        %%      的守卫过了 1 分钟就永久失效 —— 可无限次触发真实发信。
        %% 现在一律落到下面的分支重新生成并 save，两个问题一起消失：
        %% 新码使攻击者已猜过的空间作废，created_at 随之刷新使守卫真正生效。
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
        %% 【已移除】同 send_email_code/1：原"有效期内重发同一个码"分支不刷新
        %% created_at，使码存活 10 分钟且两分钟守卫过期后可无限触发真实短信
        %% （直接烧运营商账单）。改为一律重新生成。
        _ ->
            Code = elib_cipher:num_random(6),
            CodeBinary = ec_cnv:to_binary(Code),
            % 600000 = 600 * 1000 = 10分钟
            try elib_dt:add(Now, {10, minute}) of
                ValidityAt when is_binary(ValidityAt) ->
                    _ = verification_code_ds:save(Mobile, CodeBinary, ValidityAt, Now),
                    Content =
                        <<"【IMBoy】您的验证码： "/utf8, CodeBinary/binary, " ，10分钟内有效。如非本人操作，请忽略！"/utf8>>,
                    SmsPlatform = config_ds:env([sms, platform], <<"yjsms">>),
                    SmsPayload =
                        case SmsPlatform of
                            <<"jsms">> -> CodeBinary;
                            _ -> Content
                        end,
                    %% sms.switch=off（local/dev/test）：验证码照常落库供
                    %% 万能码/查库验证，但不外发——此前该开关无人消费，
                    %% 平台子句缺失（如 aliyun）时 function_clause 炸穿
                    %% handler，getcode 返回 500 空响应，注册/找回密码不可用。
                    case config_ds:env([sms, switch], <<"on">>) of
                        <<"off">> ->
                            ok;
                        _ ->
                            case catch imboy_sms:send(Mobile, SmsPayload, SmsPlatform) of
                                ok -> ok;
                                {ok, _} -> ok;
                                {error, SmsErr2} -> ?WARN_LOG({sms_send_failed, Mobile, SmsErr2});
                                SmsErr3 -> ?WARN_LOG({sms_send_failed, Mobile, SmsErr3})
                            end
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
    {ok, map()} | {error, Msg :: binary()}.
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
                    %% Task 10 / LT-04：忘记密码 = 全端会话吊销（旧 tk/rtk 全失效）
                    ok = auth_session_ds:bump(Id),
                    ok = auth_session_ds:kick_all_sessions(Id),
                    {ok, #{}};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

find_password_by_mobile(Mobile, Pwd, _PostVals) ->
    Id = user_ds:find_id_by_mobile(Mobile),
    case Id of
        0 ->
            {error, <<"手机号不存在或已被删除"/utf8>>};
        Id2 when is_integer(Id2), Id2 > 0 ->
            Pwd2 = elib_password:generate(Pwd),
            case user_ds:update_password(Id2, Pwd2) of
                {ok, _} ->
                    %% Task 10 / LT-04：同 email 路径，全端会话吊销
                    ok = auth_session_ds:bump(Id2),
                    ok = auth_session_ds:kick_all_sessions(Id2),
                    {ok, #{}};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @doc 验证凭据并分类（C.2 事件表的分类点：不靠事后 status 覆盖）。
%% 三态：
%%   {ok, User}      凭据正确且 status 允许（1/2），已顺手完成 KDF 升级；
%%   {reject, Msg}   密码正确但 status 禁用/删除 → 不计失败（非凭据失败）；
%%   {fail, Msg}     密码错误或查无用户（含未知账户）→ 计账户失败。
%% 导出的 verify_user/3 与 do_login_auth/6 共用本分类，避免两条链
%% 分叉（AC-19：verify_user 直调不计数，计数只发生在 do_login_verify 层）。
-spec verify_credential(binary(), map()) ->
    {ok, map()} | {reject, binary()} | {fail, binary()}.
verify_credential(<<>>, _User) ->
    {fail, <<"账号不存在"/utf8>>};
verify_credential(_Pwd, User) when map_size(User) =:= 0 ->
    {fail, <<"账号不存在"/utf8>>};
verify_credential(Pwd, User) ->
    Pwd2 = maps:get(<<"password">>, User, <<>>),
    % 状态: -1 删除  0 禁用  1 启用  2 申请注销中
    Status = maps:get(<<"status">>, User, -2),
    case elib_password:verify(Pwd, Pwd2) of
        {ok, _} when Status == -2 ->
            {reject, <<"账号不存在"/utf8>>};
        {ok, _} when Status == -1 ->
            {reject, <<"账号不存在或者已删除"/utf8>>};
        {ok, _} when Status == 0 ->
            {reject, <<"账号被禁用"/utf8>>};
        {ok, _} when Status == 1; Status == 2 ->
            % Task 11 / LT-05：登录成功后按计划升级 legacy 口令到 v2 KDF
            % （激活配置存在才生效；失败不阻断登录、不锁死账号）
            maybe_upgrade_password_kdf(User, Pwd),
            {ok, User};
        {error, Msg} ->
            {fail, Msg}
    end.

%% @doc 兼容导出：凭据校验（改密流程 user_logic:change_password 复用）。
%% 返回形状保持 {ok, Data} | {error, Msg}；本函数**不计数**（AC-19：
%% 不给低层另加重复计数，计数只发生在 do_login_verify 层）。
-spec verify_user(binary(), map(), binary()) -> {ok, map()} | {error, any()}.
verify_user(Pwd, User, Did) ->
    case verify_credential(Pwd, User) of
        {ok, User2} ->
            % E2EE-013：绑定登录设备 DID 进 token。
            {ok, login_resp(User2, Did, #{})};
        {reject, Msg} ->
            {error, Msg};
        {fail, Msg} ->
            {error, Msg}
    end.

%%%===================================================================
%%% Limiter V1：账户域身份解析（C.1 冻结）与锁定消息
%%%===================================================================

%% @doc 登录身份解析：任何 find_by_* 查到用户 → {user, 主键 Uid}
%% （与输入别名无关，email/account/mobile 失败历史合并到同一稳定键，
%% F3 修复）；查不到 → {unknown, Kind, Norm}（Kind 按登录 Type 分支，
%% Norm 为 trim 后原输入截断 320 字节，不把未知身份折成空键）。
-spec identity_of(binary(), binary(), map()) -> login_attempt_state:identity().
identity_of(_Type, _Account, User) when map_size(User) > 0 ->
    {user, maps:get(<<"id">>, User)};
identity_of(Type, Account, _) ->
    {unknown, compat_kind(Type), trim_norm(Account)}.

%% @private Type → 身份 Kind（account 及未知类型一律归 account 域）。
-spec compat_kind(binary()) -> email | mobile | account.
compat_kind(<<"email">>) ->
    email;
compat_kind(<<"mobile">>) ->
    mobile;
compat_kind(_) ->
    account.

%% @private 未知身份规范化：trim 后截断 320 字节（C.1 有界键）。
-spec trim_norm(binary()) -> binary().
trim_norm(Account) when is_binary(Account) ->
    Trimmed = string:trim(Account),
    case byte_size(Trimmed) > 320 of
        true ->
            binary:part(Trimmed, 0, 320);
        false ->
            Trimmed
    end;
trim_norm(_) ->
    <<>>.

%% @private 验证码路径的用户查询（先于锁定门，仅只读；供 identity_of 用）。
-spec find_user_by_type(binary(), binary()) -> map().
find_user_by_type(<<"mobile">>, Account) ->
    user_ds:find_by_mobile(Account, ?LOGIN_COLUMN);
find_user_by_type(<<"email">>, Account) ->
    user_ds:find_by_email(Account, ?LOGIN_COLUMN);
find_user_by_type(<<"account">>, Account) ->
    user_ds:find_by_account(Account, ?LOGIN_COLUMN);
find_user_by_type(_, _) ->
    #{}.

%% @doc 锁定错误对外消息：保持现有 HTTP200 + code1 envelope，锁定详情
%% 不以内部 map 结构外泄（C.6：LockInfo 字符串化为 msg）。
-spec lock_error_msg(map()) -> binary().
lock_error_msg(_LockInfo) ->
    <<"失败次数过多，账户已临时锁定，请稍后再试"/utf8>>.

%% @doc Task 11 / LT-05：rehash-on-login。
%% legacy 验证成功 ⇒ 按命中变体的候选值计算 v2（需 v2 已激活），CAS 原子写
%% （升级一次，并发双登录竞态仅一方生效）。任何失败仅记日志不阻断登录。
maybe_upgrade_password_kdf(User, Pwd) ->
    Uid = maps:get(<<"id">>, User, 0),
    Stored = maps:get(<<"password">>, User, <<>>),
    case elib_password:verify_and_plan(Pwd, Stored) of
        {ok, #{version := legacy, variant := V, candidate := Cand}} ->
            case elib_kdf:hash_v2_prehashed(Cand, V) of
                {ok, V2} ->
                    case user_ds:upgrade_password_kdf_cas(Uid, V2) of
                        {ok, 1} ->
                            ?INFO_LOG([kdf_upgraded, Uid]);
                        {ok, 0} ->
                            ok;
                        {error, Reason} ->
                            _ = ?ERROR_LOG([kdf_upgrade_write_failed, Uid, Reason]),
                            ok
                    end;
                _ ->
                    %% v2 未激活（无用户预算决策）或参数异常：保持 legacy，不阻断
                    ok
            end;
        _ ->
            ok
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
            %% 兼容窄列 User map（测试桩/旧调用方）：无 mobile 键时回退空串
            <<"mobile">> => maps:get(<<"mobile">>, User, <<>>),
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
    jsone:encode(Map, [native_utf8]);
compat_error_message(Reason) when is_binary(Reason) ->
    Reason;
compat_error_message(Reason) when is_list(Reason) ->
    unicode:characters_to_binary(Reason);
compat_error_message(Reason) when is_atom(Reason) ->
    atom_to_binary(Reason, utf8);
compat_error_message(Reason) ->
    ec_cnv:to_binary(Reason).

%% Persist the device whitelist before exposing its bound token to the client.
-spec record_login_device(integer(), map()) -> ok | {error, term()}.
record_login_device(Uid, PostVals) ->
    Did = maps:get(<<"did">>, PostVals, <<>>),
    case user_device_ds:save(elib_dt:now(), Uid, Did, PostVals) of
        {ok, _} -> ok;
        {error, _} = Error -> Error
    end.
