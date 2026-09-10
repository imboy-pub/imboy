-module(teaching_wechat_client).
%%%
% 微信小程序 jscode2session HTTP 客户端（薄封装，可 meck）
% WeChat mini-program jscode2session client
%
% 设计（AUTH-01）：
%   - 独立模块的唯一原因是让 teaching_auth_logic 的测试可以 meck 掉外呼；
%     业务代码不得绕过本模块直连微信端点
%   - 端点可通过 wechat_mini_jscode_url 覆盖（本地 mock 服务用）；
%     默认官方地址 https://api.weixin.qq.com/sns/jscode2session
%   - 任何 errcode（含 40029 无效 / 40163 已使用=重放）一律折叠为 invalid_code，
%     不向调用方区分细节（威胁模型 T11：防探测）
%   - 网络层失败折叠为 network；永不抛出
%%%

-export([jscode2session/3]).

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

-define(DEFAULT_JSCODE_URL, <<"https://api.weixin.qq.com/sns/jscode2session">>).
-define(HTTP_TIMEOUT_MS, 5000).

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
            ?LOG_WARNING("teaching_wechat_client http ~p from jscode2session", [Status]),
            {error, invalid_code};
        {error, Reason} ->
            ?LOG_WARNING("teaching_wechat_client network error ~p", [Reason]),
            {error, network}
    catch
        _:_ ->
            {error, network}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

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
