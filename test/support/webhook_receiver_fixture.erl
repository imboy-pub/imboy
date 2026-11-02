-module(webhook_receiver_fixture).

%%%
% V3-24 G-6 合成 Webhook 接收器（真实 loopback HTTP；SPEC §4.1）。
%
% cowboy 一次性 listener（127.0.0.1 随机高位端口）；per-delivery 响应策略可
% 编程（200/4xx/5xx/sleep 后回），捕获 {headers, raw_body, received_at}
% 队列供验签/乱序/重复断言。
%
% 前置：IMBOYENV ∈ test|local|dev（bot_webhook_guard loopback HTTP 白名单，
% bot_webhook_guard.erl loopback_allowed/0）。生产 HTTPS 策略不受影响。
%
% 验签断言复用生产原语：enterprise_webhook_logic:verify/4 与 verify_within/5
% （接收器侧用 configure 返回的明文 secret 校验四头）。
%%%

-export([start/0, stop/1, set_policy/2, take_all/1, clear/1, count/1, init/2]).

-type ref() :: ets:tab().

%% @doc 启动一次性 loopback listener。返回 {ok, Url, Ref}
%% （Url 形如 <<"http://127.0.0.1:<port>/hook">>，缺省策略 200）。
-spec start() -> {ok, binary(), ref()}.
start() ->
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(ranch),
    Ref = ets:new(webhook_receiver, [public, ordered_set]),
    ets:insert(Ref, {policy, 200, 0}),
    Dispatch = cowboy_router:compile([{'_', [{"/hook", ?MODULE, Ref}]}]),
    {ok, _} = cowboy:start_clear(
        Ref,
        #{socket_opts => [{ip, {127, 0, 0, 1}}, {port, 0}], num_acceptors => 1},
        #{env => #{dispatch => Dispatch}}
    ),
    Port = ranch:get_port(Ref),
    true = is_integer(Port) andalso Port > 0,
    Url = <<"http://127.0.0.1:", (integer_to_binary(Port))/binary, "/hook">>,
    {ok, Url, Ref}.

-spec stop(ref()) -> ok.
stop(Ref) ->
    try
        cowboy:stop_listener(Ref)
    catch
        _:_ -> ok
    end,
    try
        ets:delete(Ref)
    catch
        _:_ -> ok
    end,
    ok.

%% @doc 设置响应策略：StatusCode（200/404/503/...）| {sleep, Ms, StatusCode}。
-spec set_policy(ref(), non_neg_integer() | {sleep, pos_integer(), non_neg_integer()}) -> ok.
set_policy(Ref, {sleep, Ms, Code}) when is_integer(Ms), Ms > 0, is_integer(Code) ->
    ets:insert(Ref, {policy, Code, Ms}),
    ok;
set_policy(Ref, Code) when is_integer(Code) ->
    ets:insert(Ref, {policy, Code, 0}),
    ok.

%% @doc 取全部捕获（按接收序；headers 为 binary 键 map；received_at 为 ms）。
-spec take_all(ref()) -> [map()].
take_all(Ref) ->
    Rows = ets:match(Ref, {{capture, '$1'}, '$2', '$3', '$4'}),
    [
        #{
            index => I,
            headers => H,
            body => B,
            received_at => T
        }
     || [I, H, B, T] <- lists:sort(Rows)
    ].

%% @doc 捕获条数。
-spec count(ref()) -> non_neg_integer().
count(Ref) ->
    length(ets:match(Ref, {{capture, '$1'}, '_', '_', '_'})).

%% @doc 清空捕获（乱序/多段用例分段断言用）。
-spec clear(ref()) -> ok.
clear(Ref) ->
    [ets:delete(Ref, K) || [K] <- ets:match(Ref, '{capture, $1}')],
    ok.

%%%===================================================================
%%% cowboy handler（behaviour: cowboy_loop 最小形态 —— init 即完成回复）
%%%===================================================================

init(Req0, Ref) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    Headers = cowboy_req:headers(Req1),
    {Code, SleepMs} =
        case ets:lookup(Ref, policy) of
            [{policy, C, S}] -> {C, S};
            _ -> {200, 0}
        end,
    case SleepMs of
        0 -> ok;
        _ -> timer:sleep(SleepMs)
    end,
    Idx =
        case ets:match(Ref, {{capture, '$1'}, '_', '_', '_'}) of
            [] -> 1;
            Keys -> lists:max([K || [K] <- Keys]) + 1
        end,
    ets:insert(
        Ref,
        {
            {capture, Idx},
            Headers,
            Body,
            erlang:system_time(millisecond)
        }
    ),
    Req2 =
        case Code of
            200 ->
                cowboy_req:reply(
                    200,
                    #{
                        <<"content-type">> => <<"application/json">>,
                        %% 每投递独立连接（普通 handler 不消费同连接后续请求，
                        %% keep-alive 复用会静默丢失捕获）
                        <<"connection">> => <<"close">>
                    },
                    <<"{}">>,
                    Req1
                );
            _ ->
                cowboy_req:reply(
                    Code, #{<<"connection">> => <<"close">>}, <<>>, Req1
                )
        end,
    {ok, Req2, Ref}.
