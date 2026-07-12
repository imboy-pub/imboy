-module(channel_webhook_handler).
%%%
% 频道 incoming webhook 处理器 / Channel incoming webhook handler
%
% 管理端点（JWT 认证，State 携带 current_uid）：
%   POST /api/v1/channel/:channel_id/webhook/create
%   POST /api/v1/channel/:channel_id/webhook/:webhook_id/disable
%   GET  /api/v1/channel/:channel_id/webhook/list
%
% 入站端点（免 JWT/免 902 签名，token 即凭证，放行见 auth_middleware_api_v1
% 的 IsChannelWebhook 前缀，复刻支付回调范式）：
%   POST /api/v1/webhook/channel/:token   body: {"text": "..."}（1MB 上限）
%   应答：200 {"ok":true} | 400 | 404（无效/停用统一 404）| 429 限流
%%%

-behavior(cowboy_rest).

-export([init/2]).

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 =
        case Action of
            create -> create(Req0, State);
            disable -> disable(Req0, State);
            list -> list(Req0, State);
            incoming -> incoming(Req0)
        end,
    {ok, Req1, State}.

%% ===================================================================
%% 管理端点
%% ===================================================================

-spec create(cowboy_req:req(), map()) -> cowboy_req:req().
create(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    ChannelIdBin = binding_or_empty(channel_id, Req0),
    Name = maps:get(<<"name">>, PostVals, <<>>),
    case channel_webhook_logic:create(Uid, ChannelIdBin, Name) of
        {ok, Webhook} ->
            elib_response:success(Req0, Webhook);
        {error, Msg} ->
            elib_response:error(Req0, Msg)
    end.

-spec disable(cowboy_req:req(), map()) -> cowboy_req:req().
disable(Req0, State) ->
    Uid = maps:get(current_uid, State),
    ChannelIdBin = binding_or_empty(channel_id, Req0),
    WebhookIdBin = binding_or_empty(webhook_id, Req0),
    case channel_webhook_logic:disable(Uid, ChannelIdBin, WebhookIdBin) of
        ok ->
            elib_response:success(Req0, #{});
        {error, Msg} ->
            elib_response:error(Req0, Msg)
    end.

-spec list(cowboy_req:req(), map()) -> cowboy_req:req().
list(Req0, State) ->
    Uid = maps:get(current_uid, State),
    ChannelIdBin = binding_or_empty(channel_id, Req0),
    case channel_webhook_logic:list(Uid, ChannelIdBin) of
        {ok, Webhooks} ->
            elib_response:success(Req0, #{list => Webhooks});
        {error, Msg} ->
            elib_response:error(Req0, Msg)
    end.

%% ===================================================================
%% 入站端点（免认证，token 即凭证）
%% ===================================================================

-spec incoming(cowboy_req:req()) -> cowboy_req:req().
incoming(Req0) ->
    Token = binding_or_empty(token, Req0),
    {RawBody, Req1} = read_raw_body(Req0),
    Text = extract_text(RawBody),
    case channel_webhook_logic:incoming(Token, Text) of
        ok ->
            reply_json(200, <<"{\"ok\":true}">>, Req1);
        {error, rate_limited} ->
            reply_json(429, <<"{\"ok\":false,\"error\":\"rate_limited\"}">>, Req1);
        {error, not_found} ->
            reply_json(404, <<"{\"ok\":false,\"error\":\"not_found\"}">>, Req1);
        {error, _Msg} ->
            %% text 为空/发布失败等：不细分泄露内部原因
            reply_json(400, <<"{\"ok\":false,\"error\":\"bad_request\"}">>, Req1)
    end.

%% ===================================================================
%% Internal
%% ===================================================================

%% @doc 读取完整原始报文（1MB/5s 上限，复刻 payment_callback_handler 范式）
-spec read_raw_body(cowboy_req:req()) -> {binary(), cowboy_req:req()}.
read_raw_body(Req0) ->
    read_raw_body(Req0, <<>>).

-spec read_raw_body(cowboy_req:req(), binary()) -> {binary(), cowboy_req:req()}.
read_raw_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0, #{length => 1048576, period => 5000}) of
        {ok, Data, Req1} ->
            {<<Acc/binary, Data/binary>>, Req1};
        {more, Data, Req1} ->
            read_raw_body(Req1, <<Acc/binary, Data/binary>>)
    end.

%% @doc 解析 body JSON 提取 text 字段；非法 JSON / 非 binary text 一律空
-spec extract_text(binary()) -> binary().
extract_text(<<>>) ->
    <<>>;
extract_text(RawBody) ->
    try jsone:decode(RawBody, [{object_format, map}]) of
        #{<<"text">> := Text} when is_binary(Text) -> Text;
        _ -> <<>>
    catch
        _:_ -> <<>>
    end.

-spec reply_json(non_neg_integer(), binary(), cowboy_req:req()) -> cowboy_req:req().
reply_json(Status, Body, Req) ->
    cowboy_req:reply(
        Status,
        #{<<"content-type">> => <<"application/json; charset=utf-8">>},
        Body,
        Req
    ).

-spec binding_or_empty(atom(), cowboy_req:req()) -> binary().
binding_or_empty(Key, Req0) ->
    case cowboy_req:binding(Key, Req0) of
        undefined -> <<>>;
        Val -> Val
    end.
