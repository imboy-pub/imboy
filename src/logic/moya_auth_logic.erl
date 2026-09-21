-module(moya_auth_logic).
%%%
% 墨芽微信小程序登录业务逻辑
% WeChat mini-program login logic
%
% 流程（AUTH-01 / 计划 Step 8）：
%   js_code --(服务端持 appsecret)--> jscode2session --> openid
%   openid --(sso_identity: provider=wechat_mini)--> IMBoy uid --> token
%
% 安全约束：
%   - AppSecret 只在服务端 config（wechat_mini_appid/wechat_mini_secret），
%     未配置 → {error, provider_unconfigured}（5403），响应不泄漏内部细节
%   - openid/session_key 不出现在任何 API 响应（只进身份映射层）
%   - code 错误/重放（微信 errcode 任意）→ {error, code_invalid}（5402）
%
% 首登自动开户（2026-09-20 试点方案 B，改动见下）：
%   原实现未命中 sso_identity 时返回 {error, identity_none}（5404），要求
%   「机构侧建立绑定」。但 openid 只在服务端 jscode2session 那一次可见、
%   不落库不落日志（log_redact 把 openid 列为脱敏键），机构侧**没有任何
%   途径**拿到它 —— 该分支在实现上不可执行，新家长永远进不了门。
%   现改为：未命中即在本次请求内自动开户（user 行 + sso_identity 映射，
%   同一事务，见 moya_identity_ds），家长扫码即可登录，落到 no-identity 页
%   「等待老师开通」，再由老师用既有的 learners/:id/bind 建立家长关系。
%   ⚠ 开户是**建用户行**的动作 ⇒ 任何人扫码都能造行，故必须先过
%   passport_logic:quota_guard/0（License 用户数上限），不可绕过。
%   ⚠ 5404 分支保留（服务端将来若关闭自动开户仍会产生该码），客户端文案不动。
%%%

-export([wechat_mini_login/1]).

-include_lib("kernel/include/logger.hrl").
-include("common.hrl").
-include("log.hrl").

-define(WECHAT_PROVIDER, <<"wechat_mini">>).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc 微信小程序登录
%% 入参 #{code => binary(), device_id => binary() | undefined, ip => binary() | undefined,
%%        reg_cosv => binary() | undefined}
%% 出参 {ok, #{token, expires_in, refresh_token, has_teaching_identity, uid}}
%%     | {error, missing_code | invalid_code | provider_unconfigured |
%%               login_failed | identity_none | account_quota_exceeded}
-spec wechat_mini_login(map()) ->
    {ok, map()}
    | {error,
        missing_code
        | invalid_code
        | code_invalid
        | provider_unconfigured
        | login_failed
        | identity_none
        | account_quota_exceeded}.
wechat_mini_login(#{code := Code0} = Params) when is_binary(Code0) ->
    Code = trim_binary(Code0),
    DeviceId = maps:get(device_id, Params, <<>>),
    Ip = maps:get(ip, Params, <<>>),
    RegCosv = maps:get(reg_cosv, Params, <<>>),
    case valid_code(Code) of
        false ->
            {error, invalid_code};
        true ->
            case provider_config() of
                {error, provider_unconfigured} = E ->
                    E;
                {ok, AppId, Secret} ->
                    do_login(AppId, Secret, Code, #{
                        device_id => DeviceId, ip => Ip, reg_cosv => RegCosv
                    })
            end
    end;
wechat_mini_login(_) ->
    {error, missing_code}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% string:trim 返回 chardata、characters_to_binary 带 error/incomplete 分支；
%% 输入恒为 binary 时实际恒成功——包装把类型收敛回 binary（异常输入归空串，
%% 后续 valid_code 判 false → invalid_code，语义不变）
-spec trim_binary(binary()) -> binary().
trim_binary(B) ->
    case unicode:characters_to_binary(string:trim(B)) of
        T when is_binary(T) -> T;
        _ -> <<>>
    end.

-spec valid_code(binary()) -> boolean().
valid_code(Code) ->
    Len = byte_size(Code),
    Len >= 5 andalso Len =< 128.

-spec provider_config() -> {ok, binary(), binary()} | {error, provider_unconfigured}.
provider_config() ->
    AppId = elib_cnv:safe_to_binary(config_ds:env(wechat_mini_appid, <<>>)),
    Secret = elib_cnv:safe_to_binary(config_ds:env(wechat_mini_secret, <<>>)),
    case {AppId, Secret} of
        {<<>>, _} -> {error, provider_unconfigured};
        {_, <<>>} -> {error, provider_unconfigured};
        _ -> {ok, AppId, Secret}
    end.

-spec do_login(binary(), binary(), binary(), map()) ->
    {ok, map()} | {error, code_invalid | login_failed | account_quota_exceeded}.
do_login(AppId, Secret, Code, Opts) ->
    %% 首版 token 不绑定设备（legacy did）；设备绑定随 Step 13 客户端会话设计再启用
    case moya_wechat_client:jscode2session(AppId, Secret, Code) of
        {ok, Openid} ->
            resolve_uid_and_issue(Openid, Opts);
        {error, invalid_code} ->
            {error, code_invalid};
        {error, network} ->
            {error, login_failed}
    end.

-spec resolve_uid_and_issue(binary(), map()) ->
    {ok, map()} | {error, identity_none | login_failed | account_quota_exceeded}.
resolve_uid_and_issue(Openid, Opts) ->
    case sso_identity_ds:find_uid(?WECHAT_PROVIDER, Openid) of
        {ok, Uid} when is_integer(Uid) ->
            {ok, issue_token(Uid)};
        not_found ->
            %% 老用户走过上面；这里是「首次见到这个 openid」⇒ 自动开户
            provision_and_issue(Openid, Opts);
        {error, Reason} ->
            ?LOG_ERROR("moya_auth_logic sso lookup error ~p", [Reason]),
            {error, login_failed}
    end.

%% @doc 首登自动开户并签发（试点方案 B）。
%% License 规模 gate 放在这里而不是 DS 里：quota_guard/0 属 logic 层，
%% 且「是否允许再开户」是业务裁决，不该下沉到数据服务。
%% ⚠ 失败一律折叠为 login_failed；只有配额耗尽单独成码 —— 它是**永久**条件，
%% 若也折叠为 login_failed，家长会看到可重试的文案并无限重试（5404 同款坑）。
-spec provision_and_issue(binary(), map()) ->
    {ok, map()} | {error, login_failed | account_quota_exceeded}.
provision_and_issue(Openid, Opts) ->
    case passport_logic:quota_guard() of
        ok ->
            case moya_identity_ds:provision_and_bind(?WECHAT_PROVIDER, Openid, Opts) of
                {ok, Uid} when is_integer(Uid) ->
                    {ok, issue_token(Uid)};
                {error, Reason} ->
                    ?LOG_ERROR("moya_auth_logic provision failed ~p", [Reason]),
                    {error, login_failed}
            end;
        {error, _Msg, _ErrCode} ->
            %% quota_guard 的 msg 含 License 细节，不进响应（由 handler 出固定文案）
            ?LOG_ERROR("moya_auth_logic provision rejected: user quota exceeded"),
            {error, account_quota_exceeded}
    end.

-spec issue_token(integer()) -> map().
issue_token(Uid) ->
    Token = token_ds:encrypt_token(Uid),
    RefreshToken = token_ds:encrypt_refreshtoken(Uid, <<>>),
    #{
        token => Token,
        expires_in => ?TOKEN_VALID,
        refresh_token => RefreshToken,
        has_teaching_identity => has_teaching_identity(Uid),
        %% 本人 uid，**字符串形态**（契约硬规则1 / STEP-04：64-bit ID 的 JSON
        %% 表示一律 string）。真实 uid 已到 19 位（如 9000000000000000001），
        %% 以 number 下发会被 JS 的 JSON.parse 折成 ...000 —— 家长看到的号
        %% 和老师要绑的号就不一致，且**任何门禁都发现不了**（能解析、能用、
        %% 只是错了）。此前客户端拿不到自己的 uid，闭环最后一段（老师据
        %% uid 调 learners/:id/bind）无从下手，故在此随登录一并下发。
        uid => integer_to_binary(Uid)
    }.

-spec has_teaching_identity(integer()) -> boolean().
has_teaching_identity(Uid) ->
    try moya_context_logic:contexts(Uid, organization) of
        {ok, #{contexts := [_ | _]}} -> true;
        _ -> false
    catch
        _:_ -> false
    end.
