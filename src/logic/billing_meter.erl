-module(billing_meter).

%%%===================================================================
%%% @doc billing_meter — 用量软计量埋点（金钱相邻，消息热路径 fire-and-forget）
%%%
%%% meter/2 在消息发送成功路径被旁路调用：解析单租户 (tenant_id=0) 活跃订阅
%%% id（imboy_cache 缓存 5 分钟，避免每条消息查库），再裸累加用量。
%%%
%%% ⚠️ 金钱红线：绝不调用 billing_logic:report_usage —— 它超配额会返回
%%%   {error, quota_exceeded}，在消息热路径用它会「拒发消息」＝生产事故。
%%%   本模块只做「软计量」纯累加（billing_usage_ds:incr），超配额也不拦截消息；
%%%   配额硬限由计费/运营侧离线核对，不进消息热路径。
%%%
%%% 失败只 WARN，绝不抛错影响主投递（外层 meter/2 已 fire-and-forget）。
%%%
%%% ponytail: 单租户硬取 tenant_id=0。真多租户须换 uid→tenant→sub 映射
%%%   （从发送者 uid 解析所属租户，再取该租户活跃订阅），并把缓存键带上 tenant。
%%% @end
%%%===================================================================

-export([meter/2]).
%% 供测试：同步计量核心（绕过 meter/2 的 async spawn）
-export([do_meter/2]).

-include("log.hrl").

%% 单租户逻辑租户 id（billing 多租户实体表未落地，硬取 0）
-define(TENANT_ID, 0).
%% 活跃订阅 id 缓存键与 TTL（秒）：避免每条消息查库
-define(SUB_CACHE_KEY, {billing_meter, active_sub, 0}).
-define(SUB_CACHE_TTL, 300).

%% @doc 计量一次用量（fire-and-forget）：异步累加，绝不阻塞主返回。
-spec meter(binary(), non_neg_integer()) -> ok.
meter(Metric, Delta) ->
    _ = elib_async:async(fun() -> do_meter(Metric, Delta) end),
    ok.

%%%===================================================================
%%% Internal
%%%===================================================================

%% @doc 同步计量核心：解析活跃订阅 → 裸累加。无订阅 no-op；任何异常只 WARN。
-spec do_meter(binary(), non_neg_integer()) -> ok.
do_meter(Metric, Delta) ->
    try
        case active_sub_id() of
            undefined ->
                %% 无生效订阅（社区版/未订阅）：no-op
                ok;
            SubId ->
                Period = current_period(),
                case billing_usage_ds:incr(SubId, Metric, Period, Delta) of
                    {ok, _Used} ->
                        ok;
                    {error, Reason} ->
                        ok = ?WARN_LOG(
                            "[billing_meter] incr failed metric=~ts reason=~p", [Metric, Reason]
                        ),
                        ok
                end
        end
    catch
        Class:Rsn ->
            ok = ?WARN_LOG("[billing_meter] guard ~p:~p", [Class, Rsn]),
            ok
    end.

%% @doc 解析单租户活跃订阅 id，缓存 5 分钟。无生效订阅返回 undefined（也缓存，
%%      避免无订阅时每条消息查库）。
-spec active_sub_id() -> integer() | undefined.
active_sub_id() ->
    case imboy_cache:get(?SUB_CACHE_KEY) of
        {ok, Cached} ->
            Cached;
        undefined ->
            SubId = lookup_active_sub_id(),
            ok = imboy_cache:set(?SUB_CACHE_KEY, SubId, ?SUB_CACHE_TTL),
            SubId
    end.

-spec lookup_active_sub_id() -> integer() | undefined.
lookup_active_sub_id() ->
    Sub = billing_subscription_ds:find_active_by_tenant(?TENANT_ID),
    case maps:get(<<"id">>, Sub, undefined) of
        Id when is_integer(Id) -> Id;
        _ -> undefined
    end.

%% @doc 当前计费周期 <<"YYYY-MM">>（UTC），与 billing_logic:resolve_period 同格式。
-spec current_period() -> binary().
current_period() ->
    {{Y, M, _D}, _} = calendar:universal_time(),
    YB = integer_to_binary(Y),
    MB =
        case M < 10 of
            true -> <<"0", (integer_to_binary(M))/binary>>;
            false -> integer_to_binary(M)
        end,
    <<YB/binary, "-", MB/binary>>.
