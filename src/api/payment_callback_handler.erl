-module(payment_callback_handler).
%%%
% 统一支付回调处理器 / Unified payment webhook handler
%
% 路由：POST /v1/payment/callback/:gateway （免 JWT 认证，见 imboy_router:open/0）
%
% 职责（纯 HTTP 入口，不含业务）：
%   1) 读取路径 :gateway 与回调原始报文 RawBody（验签必须用原始字节）。
%   2) 按网关把回调归一化为统一 Notify map（gateway_payment_no/biz_type/...）。
%   3) 委托 payment_callback_logic:handle/3 完成验签→幂等→入账→落流水。
%   4) 返回各网关要求的成功应答：
%        - alipay : 文本 "success"
%        - wechat : JSON {"code":"SUCCESS","message":"成功"}
%        - stripe : 200（空体即可）
%        - 其他   : 标准 JSON 信封（success）
%
% 注意：回调来自第三方服务器，State 中没有 current_uid；user_id 由回调报文
%       自身携带（充值场景）或经 biz_order_no 反查（频道订单场景）。
%%%

-behavior(cowboy_rest).

-export([init/2]).

-include("log.hrl").

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0, notify),
    State = maps:remove(action, State0),
    Req1 =
        case Action of
            notify -> notify(Req0);
            _ -> Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal
%% ===================================================================

%% @doc 处理回调通知
-spec notify(cowboy_req:req()) -> cowboy_req:req().
notify(Req0) ->
    Gateway = gateway_binding(Req0),
    {RawBody, Req1} = read_raw_body(Req0),
    Headers = cowboy_req:headers(Req1),
    Notify = normalize(Gateway, RawBody, Req1),
    Ctx = #{raw => RawBody, headers => Headers},
    Result =
        try
            payment_callback_logic:handle(Gateway, Notify, Ctx)
        catch
            Class:Reason:Stack ->
                ?ERROR_LOG([payment_callback, crash, Gateway, Class, Reason, Stack]),
                {error, <<"回调处理异常"/utf8>>}
        end,
    reply(Gateway, Result, Req1).

%% @doc 读取路径中的 :gateway
-spec gateway_binding(cowboy_req:req()) -> binary().
gateway_binding(Req) ->
    case cowboy_req:binding(gateway, Req) of
        undefined -> <<>>;
        Gw when is_binary(Gw) -> Gw;
        Gw -> elib_cnv:safe_to_binary(Gw)
    end.

%% @doc 读取完整原始报文（验签依赖原始字节，不能用解析后的 map）
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

%% @doc 按网关把回调报文归一化为统一 Notify map。
%%   sandbox/联调阶段：约定回调体为 JSON，字段已对齐统一格式，直接解析即可。
%%   live 阶段：在各网关分支内把网关私有字段映射到统一字段（TODO[live] 注明）。
-spec normalize(binary(), binary(), cowboy_req:req()) -> map().
normalize(<<"alipay">>, RawBody, Req) ->
    %% TODO[live]: 支付宝异步通知为 application/x-www-form-urlencoded，
    %%   字段 out_trade_no/trade_no/trade_status/total_amount 等需映射：
    %%     gateway_payment_no <- trade_no
    %%     biz_order_no       <- out_trade_no
    %%   并按 passback_params 还原 biz_type/user_id。
    decode_notify(RawBody, Req);
normalize(<<"wechat">>, RawBody, Req) ->
    %% TODO[live]: 微信 v3 回调体经 AES-256-GCM 解密后才得明文 JSON，
    %%   字段 transaction_id/out_trade_no/amount.total 映射到统一字段。
    decode_notify(RawBody, Req);
normalize(<<"stripe">>, RawBody, Req) ->
    %% TODO[live]: Stripe event.data.object（checkout.session/payment_intent）
    %%   的 id/metadata 映射到统一字段。
    decode_notify(RawBody, Req);
normalize(_Gateway, RawBody, Req) ->
    decode_notify(RawBody, Req).

%% @doc 默认解析：优先 JSON，其次 urlencoded form
-spec decode_notify(binary(), cowboy_req:req()) -> map().
decode_notify(RawBody, Req) ->
    case try_json(RawBody) of
        {ok, Map} ->
            Map;
        error ->
            case cowboy_req:parse_header(<<"content-type">>, Req) of
                {<<"application">>, <<"x-www-form-urlencoded">>, _} ->
                    form_to_map(RawBody);
                _ ->
                    #{}
            end
    end.

-spec try_json(binary()) -> {ok, map()} | error.
try_json(<<>>) ->
    error;
try_json(Bin) ->
    try jsone:decode(Bin, [{object_format, map}]) of
        Map when is_map(Map) -> {ok, Map};
        _ -> error
    catch
        _:_ -> error
    end.

%% @doc 极简 urlencoded -> map（仅 sandbox 兜底；live 真实 form 解析建议用 cowboy 标准能力）
-spec form_to_map(binary()) -> map().
form_to_map(<<>>) ->
    #{};
form_to_map(Bin) ->
    Pairs = binary:split(Bin, <<"&">>, [global]),
    lists:foldl(
        fun(Pair, Acc) ->
            case binary:split(Pair, <<"=">>) of
                [K, V] -> Acc#{uri_unquote(K) => uri_unquote(V)};
                [K] -> Acc#{uri_unquote(K) => <<>>};
                _ -> Acc
            end
        end,
        #{},
        Pairs
    ).

-spec uri_unquote(binary()) -> binary().
uri_unquote(Bin) ->
    try
        cow_qs:urldecode(Bin)
    catch
        _:_ -> Bin
    end.

%% @doc 各网关要求的成功/失败应答
-spec reply(binary(), {ok, term()} | {error, binary()}, cowboy_req:req()) ->
    cowboy_req:req().
reply(<<"alipay">>, {ok, _}, Req) ->
    cowboy_req:reply(200, text_headers(), <<"success">>, Req);
reply(<<"alipay">>, {error, _Msg}, Req) ->
    %% 支付宝：非 "success" 即视为失败，会按策略重推
    cowboy_req:reply(200, text_headers(), <<"failure">>, Req);
reply(<<"wechat">>, {ok, _}, Req) ->
    Body = jsone:encode(
        #{<<"code">> => <<"SUCCESS">>, <<"message">> => <<"成功"/utf8>>},
        [native_utf8]
    ),
    cowboy_req:reply(200, json_headers(), Body, Req);
reply(<<"wechat">>, {error, Msg}, Req) ->
    Body = jsone:encode(#{<<"code">> => <<"FAIL">>, <<"message">> => Msg}, [native_utf8]),
    cowboy_req:reply(200, json_headers(), Body, Req);
reply(<<"stripe">>, {ok, _}, Req) ->
    cowboy_req:reply(200, json_headers(), <<"{\"received\":true}">>, Req);
reply(<<"stripe">>, {error, _Msg}, Req) ->
    %% Stripe 对 4xx/5xx 会重推；验签失败应回 400
    cowboy_req:reply(400, json_headers(), <<"{\"received\":false}">>, Req);
reply(_Gateway, {ok, _}, Req) ->
    elib_response:success(Req, #{}, "success.");
reply(_Gateway, {error, Msg}, Req) ->
    elib_response:error(Req, Msg).

-spec text_headers() -> map().
text_headers() ->
    #{<<"content-type">> => <<"text/plain; charset=utf-8">>}.

-spec json_headers() -> map().
json_headers() ->
    #{<<"content-type">> => <<"application/json; charset=utf-8">>}.
