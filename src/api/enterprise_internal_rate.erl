-module(enterprise_internal_rate).

-moduledoc "internal API 专用限流（EPGZ-02）—— 对应 manifest rate_buckets / plan-gz §4.1 INV-9。".
%%%
% enterprise_internal_rate 是 internal API 专用限流（EPGZ-02，
% manifest rate_buckets / plan-gz §4.1 INV-9）。
%
% 冻结桶键：internal_read / internal_write / internal_sso（与 manifest 一致，
% 不新增、不改名）。数值从 {imboy, enterprise_internal_rate_limits} 配置
% （map：Bucket => 每分钟正整数），语义为**每 application 每分钟**，
% 以认证产物的 application_id 为 key。
%
% fail-closed（INV-9）：配置缺失 / 部分缺失 / 非正数值 / throttle scope
% 未注册 —— 一律 {error, rate_not_configured}，上层映射
% security_gate_closed（503）拒绝请求；绝不 fail-open。
%
% 实现：throttle 库 scope 与桶同名；throttle:setup/3 每次调用会向
% throttle_sup 追加子进程，故仅在配置数值变化时 setup（persistent_term
% 记录已 setup 的数值）。配置读取每次请求都做——运行期删除配置立即
% 回到 fail-closed。
%%%

-export([buckets/0, limit/1, check/2, ensure_configured/0]).

-define(RATE_ENV_KEY, enterprise_internal_rate_limits).
-define(PT_KEY(Bucket), {enterprise_internal_rate, Bucket}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc 冻结桶键（manifest rate_buckets 逐字）。
-spec buckets() -> [internal_read | internal_write | internal_sso, ...].
buckets() ->
    [internal_read, internal_write, internal_sso].

%% @doc 读取桶的每分钟限额。配置缺失/非正整数 → {error, rate_not_configured}。
-spec limit(internal_read | internal_write | internal_sso) ->
    {ok, pos_integer()} | {error, rate_not_configured}.
limit(Bucket) when is_atom(Bucket) ->
    case config_ds:env(imboy, ?RATE_ENV_KEY, undefined) of
        Rates when is_map(Rates) ->
            case maps:get(Bucket, Rates, undefined) of
                N when is_integer(N), N > 0 ->
                    {ok, N};
                _ ->
                    {error, rate_not_configured}
            end;
        _ ->
            {error, rate_not_configured}
    end.

%% @doc 消费一次配额（key = application_id）。返回：
%%   {ok, Remaining}           —— 放行；
%%   {limited, RetryAfterMs}   —— 超限（上层 rate_limited 429）；
%%   {error, rate_not_configured} —— fail-closed（上层 security_gate_closed 503）。
-spec check(internal_read | internal_write | internal_sso, term()) ->
    {ok, non_neg_integer()} | {limited, non_neg_integer()} | {error, rate_not_configured}.
check(Bucket, Key) ->
    case limit(Bucket) of
        {error, rate_not_configured} = Err ->
            Err;
        {ok, N} ->
            ok = ensure_setup(Bucket, N),
            case throttle:check(Bucket, Key) of
                {ok, Remaining, _RetryAfter} ->
                    {ok, Remaining};
                {limit_exceeded, _Remaining, RetryAfter} ->
                    {limited, RetryAfter};
                rate_not_set ->
                    %% 备忘与实际脱节自愈（rv8 全量实证）：throttle 应用中途
                    %% 死亡后被 ensure_all_started 空重启时，persistent_term
                    %% 备忘（跨应用重启存活）仍声称已 setup → ensure_setup
                    %% 跳过 → scope 子进程缺失 → rate_not_set。强制重建
                    %% scope 并重试一次；仍失败照旧 fail-closed，绝不放行。
                    case rebuild_scope(Bucket, N) of
                        ok ->
                            case throttle:check(Bucket, Key) of
                                {ok, Remaining2, _} ->
                                    {ok, Remaining2};
                                {limit_exceeded, _, RetryAfter2} ->
                                    {limited, RetryAfter2};
                                _ ->
                                    {error, rate_not_configured}
                            end;
                        _ ->
                            {error, rate_not_configured}
                    end
            end
    end.

%% @doc 启动期配置校验（W4 接线可在 boot 时调用以"拒绝启动"）：
%% 三桶配置齐全且为正整数才 ok。
-spec ensure_configured() -> ok | {error, {bucket_missing, [atom()]}}.
ensure_configured() ->
    Missing = [B || B <- buckets(), limit(B) =:= {error, rate_not_configured}],
    case Missing of
        [] -> ok;
        _ -> {error, {bucket_missing, Missing}}
    end.

%%%===================================================================
%%% Internal
%%%===================================================================

%% 仅在配置数值变化时 setup（throttle:setup 每次调用都会追加子进程）。
-spec ensure_setup(atom(), pos_integer()) -> ok.
ensure_setup(Bucket, N) ->
    case persistent_term:get(?PT_KEY(Bucket), undefined) of
        %% memo 比较必须走 guard：裸 N 模式是变量模式恒匹配（连 undefined
        %% 都命中），setup 将永不被执行——首流量会一直依赖 rebuild_scope
        %% 的 rate_not_set 自愈兜底才能建 scope。
        V when V =:= N ->
            ok;
        _ ->
            try throttle:setup(Bucket, N, per_minute) of
                ok ->
                    persistent_term:put(?PT_KEY(Bucket), N),
                    ok
            catch
                %% simple_one_for_one 同参子进程已在：setup 以 badmatch 崩，
                %% 语义等同成功（进程在），与 rebuild_scope 同构。
                error:{badmatch, {error, {already_started, _Pid}}} ->
                    persistent_term:put(?PT_KEY(Bucket), N),
                    ok
            end
    end.

%% 备忘脱节自愈（rv8 全量实证）：throttle 应用重启后 scope 子进程缺失而
%% persistent_term 备忘声称已配置。throttle:setup/3 的真实契约
%% （deps/throttle）：{ok,_Pid} = supervisor:start_child(...) 后返回 ok；
%% throttle_sup 为 simple_one_for_one，同参子进程已在时 start_child 返回
%% {error,{already_started,Pid}}，setup 以 badmatch 崩溃而非返回 error 元组
%% （already_present 仅标准 supervisor 产生，此形态不存在）。自愈语义 =
%% 「进程在即等同成功」，故捕获该 badmatch 视作自愈成功并记 WARN 留痕；
%% 其余异常照旧传播，返回 error，调用方 fail-closed。
-spec rebuild_scope(internal_read | internal_write | internal_sso, pos_integer()) ->
    ok | error.
rebuild_scope(Bucket, N) ->
    logger:warning(
        "[enterprise_internal_rate] rate scope ~p missing despite memo (stale "
        "persistent_term across throttle restart); rebuilding",
        [Bucket]
    ),
    try throttle:setup(Bucket, N, per_minute) of
        ok ->
            persistent_term:put(?PT_KEY(Bucket), N),
            ok
    catch
        error:{badmatch, {error, {already_started, _Pid}}} ->
            persistent_term:put(?PT_KEY(Bucket), N),
            ok;
        %% 其余任何失败一律 error：调用方按「自愈失败照旧 fail-closed，
        %% 绝不放行」收敛，异常不得穿透本函数。
        _:_ ->
            error
    end.
