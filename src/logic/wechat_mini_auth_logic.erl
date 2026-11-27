-module(wechat_mini_auth_logic).

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
%% 出参 {ok, #{token, expires_in, refresh_token, uid}}
%%     | {error, missing_code | invalid_code | provider_unconfigured |
%%               login_failed}
-spec wechat_mini_login(map()) ->
    {ok, map()}
    | {error,
        missing_code
        | invalid_code
        | code_invalid
        | provider_unconfigured
        | login_failed}.
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
    {ok, map()} | {error, code_invalid | login_failed}.
do_login(AppId, Secret, Code, Opts) ->
    %% 首版 token 不绑定设备（legacy did）；设备绑定随 Step 13 客户端会话设计再启用
    case wechat_mini_client:jscode2session(AppId, Secret, Code) of
        {ok, Openid} ->
            resolve_uid_and_issue(Openid, Opts);
        {error, invalid_code} ->
            {error, code_invalid};
        {error, network} ->
            {error, login_failed}
    end.

-spec resolve_uid_and_issue(binary(), map()) ->
    {ok, map()} | {error, identity_none | login_failed}.
resolve_uid_and_issue(Openid, Opts) ->
    case sso_identity_ds:find_uid(?WECHAT_PROVIDER, Openid) of
        {ok, Uid} when is_integer(Uid) ->
            {ok, issue_token(Uid)};
        not_found ->
            %% 老用户走过上面；这里是「首次见到这个 openid」⇒ 自动开户
            provision_and_issue(Openid, Opts);
        {error, Reason} ->
            ?LOG_ERROR("wechat_mini_auth_logic sso lookup error ~p", [Reason]),
            {error, login_failed}
    end.

%% @doc 首登自动开户并签发（试点方案 B）。
-spec provision_and_issue(binary(), map()) -> {ok, map()} | {error, login_failed}.
provision_and_issue(Openid, Opts) ->
    case wechat_mini_identity_ds:provision_and_bind(?WECHAT_PROVIDER, Openid, Opts) of
        {ok, Uid} when is_integer(Uid) ->
            {ok, issue_token(Uid)};
        {error, Reason} ->
            ?LOG_ERROR("wechat_mini_auth_logic provision failed ~p", [Reason]),
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
        %% 本人 uid，**字符串形态**（契约硬规则1 / STEP-04：64-bit ID 的 JSON
        %% 表示一律 string）。真实 uid 已到 19 位（如 9000000000000000001），
        %% 以 number 下发会被 JS 的 JSON.parse 折成 ...000 —— 家长看到的号
        %% 和老师要绑的号就不一致，且**任何门禁都发现不了**（能解析、能用、
        %% 只是错了）。此前客户端拿不到自己的 uid，闭环最后一段（老师据
        %% uid 调 learners/:id/bind）无从下手，故在此随登录一并下发。
        uid => integer_to_binary(Uid)
    }.
