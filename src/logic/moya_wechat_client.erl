-module(moya_wechat_client).
-moduledoc "微信小程序服务端 HTTP 客户端（薄封装，可 meck）。".
%%%
% 微信小程序服务端 HTTP 客户端（薄封装，可 meck）
% WeChat mini-program server-side client
%
% 设计（AUTH-01）：
%   - 独立模块的唯一原因是让 moya_*_logic 的测试可以 meck 掉外呼；
%     业务代码不得绕过本模块直连微信端点
%   - 端点可通过 config 覆盖（本地 mock 服务用）：
%       wechat_mini_jscode_url        jscode2session（AUTH-01 既有）
%       wechat_mini_token_url         cgi-bin/token（订阅消息下发）
%       wechat_mini_subscribe_send_url subscribeMessage.send
%   - 任何 errcode（含 40029 无效 / 40163 已使用=重放）一律折叠为 invalid_code，
%     不向调用方区分细节（威胁模型 T11：防探测）
%   - 网络层失败折叠为 network；永不抛出
%
% access_token 缓存（SUB-01）：
%   - persistent_term 存 {token, expire_at_millisecond}；提前 300s 视为过期
%     （微信有效期 7200s，提前量防「拿到即临期」）
%   - 40001/42001（token 失效/过期）→ 清缓存强刷重试一次
%   - 并发取 token 竞争可接受（多取一次而已）；本产品生产为单节点
%%%

-export([jscode2session/3]).
-export([access_token/0, invalidate_token/0]).
-export([subscribe_send/4]).

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

-define(DEFAULT_JSCODE_URL, <<"https://api.weixin.qq.com/sns/jscode2session">>).
-define(DEFAULT_TOKEN_URL, <<"https://api.weixin.qq.com/cgi-bin/token">>).
-define(DEFAULT_SUBSCRIBE_SEND_URL,
    <<"https://api.weixin.qq.com/cgi-bin/message/subscribe/send">>
).
-define(HTTP_TIMEOUT_MS, 5000).
-define(TOKEN_EARLY_EXPIRE_MS, 300_000).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc 用一次性 js_code 换 openid
%% {ok, Openid} | {error, invalid_code | network}
-spec jscode2session(binary(), binary(), binary()) ->
    {ok, binary()} | {error, invalid_code | network}.
jscode2session(AppId, Secret, Code) ->
    Url = config_ds:env(wechat_mini_jscode_url, ?DEFAULT_JSCODE_URL),
    Qs = cow_qs:qs([
        {<<"appid">>, AppId},
        {<<"secret">>, Secret},
        {<<"js_code">>, Code},
        {<<"grant_type">>, <<"authorization_code">>}
    ]),
    Full = <<Url/binary, (url_sep(Url))/binary, Qs/binary>>,
    HttpOpts = [{timeout, ?HTTP_TIMEOUT_MS}, {autoredirect, true}],
    try httpc:request(get, {binary_to_list(Full), []}, HttpOpts, [{body_format, binary}]) of
        {ok, {{_Line, 200, _}, _Headers, Body}} ->
            parse_body(body_to_binary(Body));
        {ok, {{_Line, Status, _}, _Headers, _Body}} ->
            ?LOG_WARNING("moya_wechat_client http ~p from jscode2session", [Status]),
            {error, invalid_code};
        {error, Reason} ->
            ?LOG_WARNING("moya_wechat_client network error ~p", [Reason]),
            {error, network}
    catch
        _:_ ->
            {error, network}
    end.

%% @doc 取（并缓存）接口调用凭据 access_token。
%% {ok, Token} | {error, provider_unconfigured | network | token_error}
%% 密钥未配置 → provider_unconfigured（fail-closed，不静默降级）。
-spec access_token() ->
    {ok, binary()}
    | {error, provider_unconfigured | network | token_error | invalid_code}.
access_token() ->
    case cached_token() of
        {ok, Token} ->
            {ok, Token};
        error ->
            fetch_access_token()
    end.

%% @doc 清除缓存 token（40001/42001 强刷用；也可运维手动清）。
-spec invalidate_token() -> ok.
invalidate_token() ->
    persistent_term:erase({?MODULE, access_token}),
    ok.

%% @doc 下发一次性订阅消息（subscribeMessage.send）。
%% 调用前提：该 openid 对该模板存在已授权额度（moya_subscribe_grant），
%% 额度消费由 logic 层负责——本函数只做无状态的 HTTP 发送。
%% Data 形如 #{<<"thing1">> => #{<<"value">> => <<"…">>}}（键与模板申请一致）。
%% Page 为小程序内跳转路径（含 query）。
%% {ok, sent} | {error, token_invalid | provider_unconfigured | network | bad_template | send_failed}
%%   - token_invalid：40001/42001 以外的凭据问题或重试后仍失败
%%   - bad_template：40037（模板 ID 无效）/ 200014（模板与 appid 不匹配）
%%   - send_failed：其余 errcode（如 43101 用户拒收；调用方一律静默）
-spec subscribe_send(binary(), binary(), map(), binary()) ->
    {ok, sent}
    | {error, token_invalid | provider_unconfigured | network | bad_template | send_failed}.
subscribe_send(Openid, TemplateId, Data, Page) ->
    case provider_credentials() of
        {error, provider_unconfigured} = E ->
            E;
        {ok, AppId, Secret} ->
            subscribe_send_with_token(AppId, Secret, Openid, TemplateId, Data, Page, fresh)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec provider_credentials() -> {ok, binary(), binary()} | {error, provider_unconfigured}.
provider_credentials() ->
    AppId = config_ds:env(wechat_mini_appid, <<>>),
    Secret = config_ds:env(wechat_mini_secret, <<>>),
    case
        is_binary(AppId) andalso is_binary(Secret) andalso
            byte_size(AppId) > 0 andalso byte_size(Secret) > 0
    of
        true -> {ok, AppId, Secret};
        false -> {error, provider_unconfigured}
    end.

%%%-------------------------------------------------------------------
%%% access_token 缓存与获取
%%%-------------------------------------------------------------------

-spec cached_token() -> {ok, binary()} | error.
cached_token() ->
    try persistent_term:get({?MODULE, access_token}) of
        {Token, ExpireAt} when is_binary(Token), is_integer(ExpireAt) ->
            case erlang:system_time(millisecond) < ExpireAt of
                true -> {ok, Token};
                false -> error
            end;
        _ ->
            error
    catch
        _:_ ->
            error
    end.

-spec fetch_access_token() ->
    {ok, binary()}
    | {error, provider_unconfigured | network | token_error | invalid_code}.
fetch_access_token() ->
    case provider_credentials() of
        {error, provider_unconfigured} = E ->
            E;
        {ok, AppId, Secret} ->
            Url = config_ds:env(wechat_mini_token_url, ?DEFAULT_TOKEN_URL),
            Qs = cow_qs:qs([
                {<<"grant_type">>, <<"client_credential">>},
                {<<"appid">>, AppId},
                {<<"secret">>, Secret}
            ]),
            Full = <<Url/binary, (url_sep(Url))/binary, Qs/binary>>,
            HttpOpts = [{timeout, ?HTTP_TIMEOUT_MS}, {autoredirect, true}],
            try httpc:request(get, {binary_to_list(Full), []}, HttpOpts, [{body_format, binary}]) of
                {ok, {{_Line, 200, _}, _Headers, Body}} ->
                    cache_token(body_to_binary(Body));
                {ok, {{_Line, Status, _}, _Headers, _Body}} ->
                    ?LOG_WARNING("moya_wechat_client http ~p from token", [Status]),
                    {error, token_error};
                {error, Reason} ->
                    ?LOG_WARNING("moya_wechat_client network error ~p from token", [Reason]),
                    {error, network}
            catch
                _:_ ->
                    {error, network}
            end
    end.

-spec cache_token(binary()) ->
    {ok, binary()} | {error, token_error | invalid_code | network}.
cache_token(Body) ->
    try jsone:decode(Body, [{object_format, map}]) of
        #{<<"access_token">> := Token, <<"expires_in">> := ExpiresIn} when
            is_binary(Token), byte_size(Token) > 0, is_integer(ExpiresIn), ExpiresIn > 0
        ->
            %% 提前 300s 过期，防「拿到即临期」；下限保护防负数
            Lifetime = max(ExpiresIn * 1000 - ?TOKEN_EARLY_EXPIRE_MS, 60_000),
            ExpireAt = erlang:system_time(millisecond) + Lifetime,
            persistent_term:put({?MODULE, access_token}, {Token, ExpireAt}),
            {ok, Token};
        #{<<"errcode">> := _ErrCode} ->
            %% 40164（IP 不在白名单）等一律折叠（防探测；具体原因服务端日志可查）
            ?LOG_WARNING("moya_wechat_client token errcode ~p", [_ErrCode]),
            {error, invalid_code};
        _ ->
            {error, token_error}
    catch
        _:_ ->
            {error, network}
    end.

%%%-------------------------------------------------------------------
%%% subscribeMessage.send
%%%-------------------------------------------------------------------

%% Retry = fresh（首次）| retried（40001/42001 强刷后重试，只重试一次）
-spec subscribe_send_with_token(
    binary(), binary(), binary(), binary(), map(), binary(), fresh | retried
) ->
    {ok, sent}
    | {error, token_invalid | provider_unconfigured | network | bad_template | send_failed}.
subscribe_send_with_token(AppId, Secret, Openid, TemplateId, Data, Page, Retry) ->
    case access_token() of
        {ok, Token} ->
            do_subscribe_send(AppId, Secret, Openid, TemplateId, Data, Page, Token, Retry);
        {error, provider_unconfigured} = E ->
            E;
        {error, _Other} ->
            {error, token_invalid}
    end.

-spec do_subscribe_send(
    binary(), binary(), binary(), binary(), map(), binary(), binary(), fresh | retried
) ->
    {ok, sent}
    | {error, token_invalid | provider_unconfigured | network | bad_template | send_failed}.
do_subscribe_send(AppId, Secret, Openid, TemplateId, Data, Page, Token, Retry) ->
    Url0 = config_ds:env(wechat_mini_subscribe_send_url, ?DEFAULT_SUBSCRIBE_SEND_URL),
    Qs = cow_qs:qs([{<<"access_token">>, Token}]),
    Full = <<Url0/binary, (url_sep(Url0))/binary, Qs/binary>>,
    %% miniprogram_state: formal=正式版（开发/体验版收不到 formal 消息）。
    %% 体验阶段应配 wechat_mini_subscribe_state = developer/trial；缺省 formal。
    State = config_ds:env(wechat_mini_subscribe_state, <<"formal">>),
    Payload = #{
        <<"touser">> => Openid,
        <<"template_id">> => TemplateId,
        <<"page">> => Page,
        <<"data">> => Data,
        <<"miniprogram_state">> => State
    },
    Body = jsone:encode(Payload),
    HttpOpts = [{timeout, ?HTTP_TIMEOUT_MS}, {autoredirect, true}],
    Headers = [{"content-type", "application/json"}],
    try
        httpc:request(
            post,
            {binary_to_list(Full), Headers, "application/json", binary_to_list(Body)},
            HttpOpts,
            [{body_format, binary}]
        )
    of
        {ok, {{_Line, 200, _}, _Headers, RespBody}} ->
            parse_subscribe_send(
                body_to_binary(RespBody), AppId, Secret, Openid, TemplateId, Data, Page, Retry
            );
        {ok, {{_Line, Status, _}, _Headers, _RespBody}} ->
            ?LOG_WARNING("moya_wechat_client http ~p from subscribe_send", [Status]),
            {error, network};
        {error, Reason} ->
            ?LOG_WARNING("moya_wechat_client network error ~p from subscribe_send", [Reason]),
            {error, network}
    catch
        _:_ ->
            {error, network}
    end.

-spec parse_subscribe_send(
    binary(), binary(), binary(), binary(), binary(), map(), binary(), fresh | retried
) ->
    {ok, sent}
    | {error, token_invalid | provider_unconfigured | network | bad_template | send_failed}.
parse_subscribe_send(RespBody, AppId, Secret, Openid, TemplateId, Data, Page, Retry) ->
    try jsone:decode(RespBody, [{object_format, map}]) of
        #{<<"errcode">> := 0} ->
            {ok, sent};
        #{<<"errcode">> := Code} = Err when Code =:= 40001; Code =:= 42001 ->
            %% access_token 失效/过期：清缓存强刷重试一次（只一次，防循环）
            case Retry of
                fresh ->
                    ?LOG_WARNING("moya_wechat_client token stale (~p), retry once", [Code]),
                    _ = invalidate_token(),
                    subscribe_send_with_token(
                        AppId, Secret, Openid, TemplateId, Data, Page, retried
                    );
                retried ->
                    ?LOG_WARNING("moya_wechat_client token still stale after retry (~p)", [
                        Code, maps:get(<<"errmsg">>, Err, <<>>)
                    ]),
                    {error, token_invalid}
            end;
        #{<<"errcode">> := Code} = Err when Code =:= 40037; Code =:= 200014 ->
            %% 模板 ID 无效 / 模板与 appid 不匹配（常见于把测试号模板用到正式号）
            ?LOG_WARNING("moya_wechat_client bad template (~p)", [
                Code, maps:get(<<"errmsg">>, Err, <<>>)
            ]),
            {error, bad_template};
        #{<<"errcode">> := Code} = Err ->
            %% 43101（用户拒收）等：订阅额度的消费语义见 repo 注释——
            %% 已授权额度被拒收即不可复用，此处只折叠为 bad_template 以外
            %% 的通用失败（network 语义不符，用 token_invalid 也不对——
            %% 调用方对 send 失败一律静默，细分仅入日志）
            ?LOG_WARNING("moya_wechat_client subscribe_send errcode ~p ~p", [
                Code, maps:get(<<"errmsg">>, Err, <<>>)
            ]),
            {error, send_failed};
        _ ->
            {error, network}
    catch
        _:_ ->
            {error, network}
    end.

%% body_format=binary 下 httpc 实际恒返回 binary；此包装把 httpc spec 的
%% string()|binary() 联合类型收敛为 binary（异常输入归空串 → parse_body 判
%% invalid_code，语义不变）
-spec body_to_binary(term()) -> binary().
body_to_binary(B) when is_binary(B) ->
    B;
body_to_binary(CD) ->
    case unicode:characters_to_binary(CD) of
        T when is_binary(T) -> T;
        _ -> <<>>
    end.

-spec parse_body(binary()) -> {ok, binary()} | {error, invalid_code | network}.
parse_body(Body) ->
    try jsone:decode(Body, [{object_format, map}]) of
        #{<<"openid">> := Openid} when is_binary(Openid), byte_size(Openid) > 0 ->
            {ok, Openid};
        #{<<"errcode">> := _ErrCode} ->
            %% 40029 无效 / 40163 已使用等一律折叠（T11）
            {error, invalid_code};
        _ ->
            {error, invalid_code}
    catch
        _:_ ->
            {error, network}
    end.

-spec url_sep(binary()) -> binary().
url_sep(Url) ->
    case binary:match(Url, <<"?">>) of
        nomatch -> <<"?">>;
        _ -> <<"&">>
    end.
