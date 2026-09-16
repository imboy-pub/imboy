-module(moya_wechat_msg_handler).

%%%===================================================================
%%% @doc 墨芽小程序「消息推送」HTTP 适配层
%%%
%%%   GET  /api/v1/wechat/mini/events
%%%     微信保存「消息推送」配置时的校验请求。验签通过必须**原样返回
%%%     `echostr` 字符串**（不能包成 JSON 信封），否则后台报 Token 验证失败。
%%%   POST /api/v1/wechat/mini/events
%%%     用户与小程序客服交互时推送的事件（兼容/安全模式 + JSON 数据格式）。
%%%
%%% 免 Bearer（进 `imboy_router:open()` 白名单）：微信侧没有 IMBoy 凭证，
%%% 唯一的凭证就是 URL 上的签名，因此**不得**要求 Authorization。
%%%
%%% 响应码口径：
%%%   - GET 验签失败 → 403（要让人在后台看得见「接入失败」，这是配置期的反馈）
%%%   - POST 处理失败 → **200 + 空串**。微信对非 200 / 超时会重试 3 次，而验签
%%%     或解密失败重试一万次也不会成功，只会把同一份噪声放大三倍。真正的问题
%%%     由 `moya_wechat_msg_logic` 的错误日志给出。
%%% @end
%%%===================================================================

-export([init/2]).

-include("log.hrl").
-include_lib("kernel/include/logger.hrl").

%% 微信推送体量很小（一条客服消息），1MB 已远超需要；给上限是为了不让
%% 未鉴权端点成为一个无限读取的入口。
-define(MAX_BODY_BYTES, 1024 * 1024).
-define(TEXT_CT, <<"text/plain; charset=utf-8">>).
-define(JSON_CT, <<"application/json; charset=utf-8">>).

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State) ->
    Query = query_map(Req0),
    Req =
        case cowboy_req:method(Req0) of
            <<"GET">> ->
                handle_get(Req0, Query);
            <<"POST">> ->
                handle_post(Req0, Query);
            _Other ->
                cowboy_req:reply(
                    405,
                    #{<<"allow">> => <<"GET, POST">>},
                    <<>>,
                    Req0
                )
        end,
    {ok, Req, State}.

%%%===================================================================
%%% Internal
%%%===================================================================

-spec handle_get(cowboy_req:req(), map()) -> cowboy_req:req().
handle_get(Req, Query) ->
    case moya_wechat_msg_logic:verify_url(Query) of
        {ok, EchoStr} ->
            cowboy_req:reply(200, #{<<"content-type">> => ?TEXT_CT}, EchoStr, Req);
        {error, Reason} ->
            ?LOG_ERROR("moya_wechat_msg verify_url failed ~p", [Reason]),
            cowboy_req:reply(403, #{<<"content-type">> => ?TEXT_CT}, <<"forbidden">>, Req)
    end.

-spec handle_post(cowboy_req:req(), map()) -> cowboy_req:req().
handle_post(Req0, Query) ->
    case read_body(Req0) of
        {ok, Body} ->
            case moya_wechat_msg_logic:handle_push(Query, Body) of
                {ok, Reply} ->
                    reply(Reply, Req0);
                {error, Reason} ->
                    ?LOG_ERROR("moya_wechat_msg handle_push failed ~p", [Reason]),
                    reply(<<>>, Req0)
            end;
        {error, Reason} ->
            ?LOG_ERROR("moya_wechat_msg read_body failed ~p", [Reason]),
            reply(<<>>, Req0)
    end.

%% @doc 回包：有被动回复用 application/json（数据格式选的是 JSON），
%% 否则回空串。空串是微信认可的「已收到，无需回复」信号。
-spec reply(binary(), cowboy_req:req()) -> cowboy_req:req().
reply(<<>>, Req) ->
    cowboy_req:reply(200, #{<<"content-type">> => ?TEXT_CT}, <<>>, Req);
reply(Reply, Req) ->
    cowboy_req:reply(200, #{<<"content-type">> => ?JSON_CT}, Reply, Req).

-spec read_body(cowboy_req:req()) -> {ok, map()} | {error, atom()}.
read_body(Req) ->
    case cowboy_req:has_body(Req) of
        false ->
            {ok, #{}};
        true ->
            case
                cowboy_req:read_body(Req, #{
                    length => ?MAX_BODY_BYTES, period => 5000
                })
            of
                {ok, Raw, _Meta} ->
                    decode(Raw);
                {more, _Raw, _Meta} ->
                    {error, body_too_large}
            end
    end.

-spec decode(binary()) -> {ok, map()} | {error, atom()}.
decode(<<>>) ->
    {ok, #{}};
decode(Raw) ->
    try jsx:decode(Raw, [return_maps]) of
        Map when is_map(Map) -> {ok, Map};
        _NotObject -> {error, body_not_object}
    catch
        _:_ -> {error, malformed_json}
    end.

-spec query_map(cowboy_req:req()) -> map().
query_map(Req) ->
    maps:from_list(cowboy_req:parse_qs(Req)).
