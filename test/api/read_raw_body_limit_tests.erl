-module(read_raw_body_limit_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% NEW-D：read_raw_body 递归累加必须有总量守卫。
%%%
%%% 免认证端点（payment_callback / channel_webhook）的 read_raw_body 复刻同一
%%% 范式：cowboy 的 length 是单次读取窗口非总量上限，{more} 分支递归累加 Acc
%%% 无守卫 → 匿名攻击者 POST 超大 body 即 BEAM OOM。守卫子句在 Acc 达 8MB 时
%%% 停止累加，让验签/解析自然失败拒掉超大报文。
%%%
%%% 反转断言（Rule 7）：缺陷行为是「Acc 无界增长」，这里断言「被 ?MAX 封顶」。
%%%===================================================================

%% 须与两个 handler 的 ?MAX_RAW_BODY_BYTES 保持一致。
-define(MAX_RAW_BODY_BYTES, 8 * 1048576).

setup() ->
    meck:new(cowboy_req, [no_link, passthrough]),
    ok.
cleanup(_) ->
    meck:unload(cowboy_req),
    ok.

read_raw_body_limit_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun payment_unbounded_stream_capped/0,
        fun channel_unbounded_stream_capped/0,
        fun small_body_passes_through/0
    ]}.

%% 无限 1MB {more} 流 → payment 守卫在 8MB 封顶，不无限累加
payment_unbounded_stream_capped() ->
    meck:expect(cowboy_req, read_body, fun(Req, _Opts) -> {more, <<0:(1048576 * 8)>>, Req} end),
    {Body, _} = payment_callback_handler:read_raw_body(#{}, <<>>),
    ?assert(byte_size(Body) =< ?MAX_RAW_BODY_BYTES),
    ?assert(byte_size(Body) > 0).

%% 无限 1MB {more} 流 → channel 守卫在 8MB 封顶
channel_unbounded_stream_capped() ->
    meck:expect(cowboy_req, read_body, fun(Req, _Opts) -> {more, <<0:(1048576 * 8)>>, Req} end),
    {Body, _} = channel_webhook_handler:read_raw_body(#{}, <<>>),
    ?assert(byte_size(Body) =< ?MAX_RAW_BODY_BYTES).

%% 正常小回调一次性 {ok} → 不受守卫影响，原样返回
small_body_passes_through() ->
    Small = <<"{\"text\":\"hi\"}">>,
    meck:expect(cowboy_req, read_body, fun(Req, _Opts) -> {ok, Small, Req} end),
    {Body1, _} = payment_callback_handler:read_raw_body(#{}, <<>>),
    {Body2, _} = channel_webhook_handler:read_raw_body(#{}, <<>>),
    ?assertEqual(Small, Body1),
    ?assertEqual(Small, Body2).
