-module(teaching_auth_logic).
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
%   - 无 sso_identity 映射 → {error, identity_none}（5404）：首版家长账号由
%     机构侧建立绑定（试点流程），不做微信侧自动注册
%   - code 错误/重放（微信 errcode 任意）→ {error, code_invalid}（5402）
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
%% 入参 #{code => binary(), device_id => binary() | undefined}
%% 出参 {ok, #{token, expires_in, refresh_token, has_teaching_identity}}
%%     | {error, missing_code | invalid_code | provider_unconfigured |
%%               login_failed | identity_none}
-spec wechat_mini_login(map()) ->
    {ok, map()}
    | {error, missing_code | invalid_code | provider_unconfigured | login_failed | identity_none}.
wechat_mini_login(#{code := Code0} = Params) when is_binary(Code0) ->
    Code = string:trim(Code0),
    DeviceId = maps:get(device_id, Params, <<>>),
    case valid_code(Code) of
        false ->
            {error, invalid_code};
        true ->
            case provider_config() of
                {error, provider_unconfigured} = E ->
                    E;
                {ok, AppId, Secret} ->
                    do_login(AppId, Secret, Code, DeviceId)
            end
    end;
wechat_mini_login(_) ->
    {error, missing_code}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

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

-spec do_login(binary(), binary(), binary(), binary()) ->
    {ok, map()} | {error, code_invalid | login_failed | identity_none}.
do_login(AppId, Secret, Code, _DeviceId) ->
    %% 首版 token 不绑定设备（legacy did）；设备绑定随 Step 13 客户端会话设计再启用
    case teaching_wechat_client:jscode2session(AppId, Secret, Code) of
        {ok, Openid} ->
            resolve_uid_and_issue(Openid);
        {error, invalid_code} ->
            {error, code_invalid};
        {error, network} ->
            {error, login_failed}
    end.

-spec resolve_uid_and_issue(binary()) -> {ok, map()} | {error, identity_none | login_failed}.
resolve_uid_and_issue(Openid) ->
    case sso_identity_ds:find_uid(?WECHAT_PROVIDER, Openid) of
        {ok, Uid} when is_integer(Uid) ->
            {ok, issue_token(Uid)};
        not_found ->
            %% openid 在身份映射层无映射：拒绝；不泄漏"该微信是否存在"的细节
            {error, identity_none};
        {error, Reason} ->
            ?LOG_ERROR("teaching_auth_logic sso lookup error ~p", [Reason]),
            {error, login_failed}
    end.

-spec issue_token(integer()) -> map().
issue_token(Uid) ->
    Token = token_ds:encrypt_token(Uid),
    RefreshToken = token_ds:encrypt_refreshtoken(Uid, <<>>),
    #{
        token => Token,
        expires_in => ?TOKEN_VALID,
        refresh_token => RefreshToken,
        has_teaching_identity => has_teaching_identity(Uid)
    }.

-spec has_teaching_identity(integer()) -> boolean().
has_teaching_identity(Uid) ->
    try teaching_context_logic:contexts(Uid) of
        {ok, #{contexts := [_ | _]}} -> true;
        _ -> false
    catch
        _:_ -> false
    end.
