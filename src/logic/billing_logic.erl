-module(billing_logic).
-dialyzer({nowarn_function, [do_pay_invoice/3]}).
-dialyzer({nowarn_function, [period_field_ms/2]}).

%%%===================================================================
%%% @doc SaaS 计费业务逻辑 / SaaS billing business logic
%%%
%%% 覆盖：套餐 CRUD、订阅创建/续费/取消、用量上报与配额校验、
%%%       账单生成（按周期）、账单支付（复用统一支付通道 biz_type=3）。
%%%
%%% 金额单位全程「分」(bigint)。多租户：imboy 当前无多租户实体表，
%%% tenant_id 作逻辑字段（单租户=0 或站点 id），由调用方传入。
%%%
%%% 配额校验 check_quota/3：读取套餐 quota_config[Metric] 上限 vs
%%%   当前周期 billing_usage 已用量，判断是否超额。无上限配置=不限。
%%%
%%% 账单生成 generate_invoice/1：按订阅当前周期 + 套餐价格生成账单，
%%%   依赖 (subscription_id, period_start, period_end) 唯一约束幂等。
%%%
%%% 账单支付 pay_invoice/2：调 payment_gateway:pay 拿网关单号，
%%%   成功后 mark_paid（条件更新幂等）。
%%% @end
%%%===================================================================

%% 套餐
-export([create_plan/1]).
-export([update_plan/2]).
-export([get_plan/1]).
-export([list_plans/0]).
-export([list_plans_page/3]).
%% 订阅
-export([subscribe/3]).
-export([renew/1]).
-export([cancel/1]).
-export([get_subscription/1]).
-export([current_subscription/1]).
-export([list_subscriptions_page/3]).
%% 用量与配额
-export([report_usage/4]).
-export([check_quota/3]).
%% 账单
-export([generate_invoice/1]).
-export([pay_invoice/2]).
-export([list_invoices/1]).
-export([list_invoices_page/3]).

-ifdef(TEST).
-export([
    validate_plan/4,
    period_ms/1,
    resolve_period/1,
    pad2/1,
    encode_jsonb/1,
    decode_jsonb/1,
    ts_ms_to_sql/1
]).
-endif.

-include("log.hrl").

%% 订阅状态
-define(SUB_TRIAL, 0).
-define(SUB_ACTIVE, 1).
-define(SUB_EXPIRED, 2).
-define(SUB_CANCELLED, 3).

%% 账单状态
-define(INV_UNPAID, 0).

%% 统一支付 biz_type：3 = SaaS 账单
-define(BIZ_TYPE_BILLING, 3).

%% 一天毫秒数
-define(MS_PER_DAY, 86400000).

%%===================================================================
%%% 套餐 CRUD
%%===================================================================

%% @doc 创建套餐
%% @param Params map，须含 code, name, price(分)；可选 billing_period(month/year)、
%%        quota_config(map)、description、status
%% @returns {ok, PlanId} | {error, binary()}
-spec create_plan(map()) -> {ok, integer()} | {error, binary()}.
create_plan(Params) ->
    Code = maps:get(<<"code">>, Params, <<>>),
    Name = maps:get(<<"name">>, Params, <<>>),
    Price = maps:get(<<"price">>, Params, 0),
    Period = maps:get(<<"billing_period">>, Params, <<"month">>),
    Quota = maps:get(<<"quota_config">>, Params, #{}),
    Desc = maps:get(<<"description">>, Params, null),
    case validate_plan(Code, Name, Price, Period) of
        {error, _} = Err ->
            Err;
        ok ->
            Data = #{
                <<"code">> => Code,
                <<"name">> => Name,
                <<"price">> => Price,
                <<"billing_period">> => Period,
                <<"quota_config">> => encode_jsonb(Quota),
                <<"description">> => Desc,
                <<"status">> => maps:get(<<"status">>, Params, 1)
            },
            case billing_plan_ds:create(Data) of
                {ok, Id} -> {ok, Id};
                {error, duplicate} -> {error, <<"套餐编码已存在"/utf8>>};
                {error, Reason} -> {error, elib_cnv:safe_to_binary(Reason)}
            end
    end.

%% @doc 更新套餐（部分字段）；quota_config 若存在则编码为 jsonb
-spec update_plan(integer(), map()) -> {ok, non_neg_integer()} | {error, binary()}.
update_plan(PlanId, Params0) ->
    Params =
        case maps:is_key(<<"quota_config">>, Params0) of
            true ->
                Q = maps:get(<<"quota_config">>, Params0),
                Params0#{<<"quota_config">> => encode_jsonb(Q)};
            false ->
                Params0
        end,
    case billing_plan_ds:update(PlanId, Params) of
        {ok, Count} -> {ok, Count};
        {error, Reason} -> {error, elib_cnv:safe_to_binary(Reason)}
    end.

%% @doc 获取套餐（quota_config 解码为 map）
-spec get_plan(integer()) -> map().
get_plan(PlanId) ->
    plan_transfer(billing_plan_ds:find_by_id(PlanId)).

%% @doc 列出上架套餐
-spec list_plans() -> [map()].
list_plans() ->
    [plan_transfer(P) || P <- billing_plan_ds:list_active()].

%% @doc 套餐分页查询（运营后台用）。WhereMap 由调用方组装（如 #{status => 1}）。
%%      list 中各条 quota_config 解码为 map 输出。
-spec list_plans_page(map(), pos_integer(), pos_integer()) ->
    {ok, map()} | {error, term()}.
list_plans_page(WhereMap, Page, Size) ->
    case billing_plan_ds:page(WhereMap, <<"id desc">>, Page, Size) of
        {ok, Payload} ->
            List = maps:get(list, Payload, []),
            {ok, Payload#{list => [plan_transfer(P) || P <- List]}};
        {error, Reason} ->
            {error, Reason}
    end.

%%===================================================================
%%% 订阅
%%===================================================================

%% @doc 订阅套餐（创建订阅）
%% @param TenantId 租户 id（逻辑字段，单租户=0）
%% @param PlanId 套餐 id
%% @param Opts map，可选 trial(boolean，是否试用)
%% @returns {ok, SubId} | {error, binary()}
-spec subscribe(integer(), integer(), map()) -> {ok, integer()} | {error, binary()}.
subscribe(TenantId, PlanId, Opts) ->
    Plan = billing_plan_ds:find_by_id(PlanId),
    case map_size(Plan) =:= 0 of
        true ->
            {error, <<"套餐不存在"/utf8>>};
        false ->
            Period = maps:get(<<"billing_period">>, Plan, <<"month">>),
            NowMs = erlang:system_time(millisecond),
            EndMs = NowMs + period_ms(Period),
            IsTrial = maps:get(trial, Opts, false),
            Status =
                case IsTrial of
                    true -> ?SUB_TRIAL;
                    false -> ?SUB_ACTIVE
                end,
            Data = #{
                <<"tenant_id">> => TenantId,
                <<"plan_id">> => PlanId,
                <<"status">> => Status,
                <<"current_period_start">> => {raw, <<"NOW()">>},
                <<"current_period_end">> =>
                    {raw, ts_ms_to_sql(EndMs)},
                <<"auto_renew">> => maps:get(auto_renew, Opts, true)
            },
            case billing_subscription_ds:create(Data) of
                {ok, Id} -> {ok, Id};
                {error, duplicate} -> {error, <<"该租户已有生效订阅"/utf8>>};
                {error, Reason} -> {error, elib_cnv:safe_to_binary(Reason)}
            end
    end.

%% @doc 续费：按套餐周期推进 current_period_end，状态置生效
%% @returns {ok, NewEndMs} | {error, binary()}
-spec renew(integer()) -> {ok, integer()} | {error, binary()}.
renew(SubId) ->
    Sub = billing_subscription_ds:find_by_id(SubId),
    case map_size(Sub) =:= 0 of
        true ->
            {error, <<"订阅不存在"/utf8>>};
        false ->
            Status = maps:get(<<"status">>, Sub, 0),
            case Status =:= ?SUB_CANCELLED of
                true ->
                    {error, <<"订阅已取消，不可续费"/utf8>>};
                false ->
                    PlanId = maps:get(<<"plan_id">>, Sub),
                    Plan = billing_plan_ds:find_by_id(PlanId),
                    Period = maps:get(<<"billing_period">>, Plan, <<"month">>),
                    %% 从当前周期末顺延一个周期；若已过期则从当下顺延
                    BaseMs = renew_base_ms(Sub),
                    NewStartMs = BaseMs,
                    NewEndMs = BaseMs + period_ms(Period),
                    case billing_subscription_ds:renew(SubId, NewStartMs, NewEndMs) of
                        {ok, _} -> {ok, NewEndMs};
                        {error, Reason} -> {error, elib_cnv:safe_to_binary(Reason)}
                    end
            end
    end.

%% @doc 取消订阅（置取消状态，关闭自动续费）
-spec cancel(integer()) -> ok | {error, binary()}.
cancel(SubId) ->
    case
        billing_subscription_ds:update(
            SubId,
            #{<<"status">> => ?SUB_CANCELLED, <<"auto_renew">> => false}
        )
    of
        {ok, _} -> ok;
        {error, Reason} -> {error, elib_cnv:safe_to_binary(Reason)}
    end.

%% @doc 获取订阅
-spec get_subscription(integer()) -> map().
get_subscription(SubId) ->
    billing_subscription_ds:find_by_id(SubId).

%% @doc 获取租户当前生效/试用订阅
-spec current_subscription(integer()) -> map().
current_subscription(TenantId) ->
    billing_subscription_ds:find_active_by_tenant(TenantId).

%% @doc 订阅分页查询（运营后台用）。WhereMap 由调用方组装（如 #{status => 1, plan_id => Id}）。
-spec list_subscriptions_page(map(), pos_integer(), pos_integer()) ->
    {ok, map()} | {error, term()}.
list_subscriptions_page(WhereMap, Page, Size) ->
    billing_subscription_ds:page(WhereMap, <<"id desc">>, Page, Size).

%%===================================================================
%%% 用量与配额
%%===================================================================

%% @doc 上报用量（累加到当前周期）。先校验配额，超额则拒绝。
%% @param SubId 订阅 id
%% @param Metric 指标键（message/storage/dau...）
%% @param Delta 增量（>=0）
%% @param Period 计费周期标识 binary（如 <<"2026-06">>），传 undefined 用当前月
%% @returns {ok, Used} | {error, quota_exceeded | binary()}
-spec report_usage(integer(), binary(), non_neg_integer(), binary() | undefined) ->
    {ok, non_neg_integer()} | {error, quota_exceeded | binary()}.
report_usage(SubId, Metric, Delta, Period0) when is_integer(Delta), Delta >= 0 ->
    Period = resolve_period(Period0),
    case check_quota_with_delta(SubId, Metric, Period, Delta) of
        {error, _} = Err ->
            Err;
        ok ->
            case billing_usage_ds:incr(SubId, Metric, Period, Delta) of
                {ok, Used} -> {ok, Used};
                {error, Reason} -> {error, elib_cnv:safe_to_binary(Reason)}
            end
    end;
report_usage(_SubId, _Metric, _Delta, _Period) ->
    {error, <<"用量增量不合法"/utf8>>}.

%% @doc 配额校验：判断订阅在某指标当前周期是否仍在配额内（未超额）
%% @returns {ok, #{limit, used, remaining, unlimited}} | {error, binary()}
%%   limit < 0 或无配置 = 不限量（unlimited=true）
-spec check_quota(integer(), binary(), binary() | undefined) ->
    {ok, map()} | {error, binary()}.
check_quota(SubId, Metric, Period0) ->
    Sub = billing_subscription_ds:find_by_id(SubId),
    case map_size(Sub) =:= 0 of
        true ->
            {error, <<"订阅不存在"/utf8>>};
        false ->
            Period = resolve_period(Period0),
            Limit = quota_limit(Sub, Metric),
            Used = billing_usage_ds:get_used(SubId, Metric, Period),
            Unlimited = Limit < 0,
            Remaining =
                case Unlimited of
                    true -> -1;
                    false -> max(0, Limit - Used)
                end,
            {ok, #{
                <<"metric">> => Metric,
                <<"limit">> => Limit,
                <<"used">> => Used,
                <<"remaining">> => Remaining,
                <<"unlimited">> => Unlimited,
                <<"exceeded">> => (not Unlimited) andalso Used >= Limit
            }}
    end.

%%===================================================================
%%% 账单
%%===================================================================

%% @doc 按订阅当前周期生成账单（金额=套餐价格，单位分）
%% 幂等：依赖 (subscription_id, period_start, period_end) 唯一约束；
%%       已存在则返回 {ok, already_generated}。
%% @returns {ok, InvoiceId} | {ok, already_generated} | {error, binary()}
-spec generate_invoice(integer()) ->
    {ok, integer()} | {ok, already_generated} | {error, binary()}.
generate_invoice(SubId) ->
    Sub = billing_subscription_ds:find_by_id(SubId),
    case map_size(Sub) =:= 0 of
        true ->
            {error, <<"订阅不存在"/utf8>>};
        false ->
            PlanId = maps:get(<<"plan_id">>, Sub),
            Plan = billing_plan_ds:find_by_id(PlanId),
            case map_size(Plan) =:= 0 of
                true ->
                    {error, <<"套餐不存在"/utf8>>};
                false ->
                    do_generate_invoice(SubId, Sub, Plan)
            end
    end.

%% @doc 账单支付：调网关支付，成功后标记账单已付（幂等）
%% @param InvoiceNo 账单号
%% @param Method 支付方式（alipay/wechat/stripe/mock...）
%% @returns {ok, #{payment_no, ...}} | {error, binary()}
-spec pay_invoice(binary(), binary()) -> {ok, map()} | {error, binary()}.
pay_invoice(InvoiceNo, Method) ->
    Inv = billing_invoice_ds:find_by_invoice_no(InvoiceNo),
    case map_size(Inv) =:= 0 of
        true ->
            {error, <<"账单不存在"/utf8>>};
        false ->
            Status = maps:get(<<"status">>, Inv, 0),
            case Status of
                ?INV_UNPAID ->
                    do_pay_invoice(Inv, InvoiceNo, Method);
                1 ->
                    {error, <<"账单已支付"/utf8>>};
                _ ->
                    do_pay_invoice(Inv, InvoiceNo, Method)
            end
    end.

%% @doc 列出订阅账单
-spec list_invoices(integer()) -> [map()].
list_invoices(SubId) ->
    billing_invoice_ds:list_by_subscription(SubId).

%% @doc 账单分页查询（运营后台用）。WhereMap 由调用方组装（如 #{status => 0, subscription_id => Id}）。
-spec list_invoices_page(map(), pos_integer(), pos_integer()) ->
    {ok, map()} | {error, term()}.
list_invoices_page(WhereMap, Page, Size) ->
    billing_invoice_ds:page(WhereMap, <<"id desc">>, Page, Size).

%%===================================================================
%%% Internal
%%===================================================================

%% @doc 生成账单实现
-spec do_generate_invoice(integer(), map(), map()) ->
    {ok, integer()} | {ok, already_generated} | {error, binary()}.
do_generate_invoice(SubId, Sub, Plan) ->
    Amount = maps:get(<<"price">>, Plan, 0),
    %% 周期边界：用订阅 current_period_start/end（毫秒时间戳）
    StartMs = period_field_ms(Sub, <<"current_period_start">>),
    EndMs = period_field_ms(Sub, <<"current_period_end">>),
    InvoiceNo = gen_invoice_no(),
    Data = #{
        invoice_no => InvoiceNo,
        subscription_id => SubId,
        amount => Amount,
        currency => <<"CNY">>,
        period_start => StartMs,
        period_end => EndMs
    },
    case billing_invoice_ds:create(Data) of
        {ok, Id} -> {ok, Id};
        {error, duplicate} -> {ok, already_generated};
        {error, Reason} -> {error, elib_cnv:safe_to_binary(Reason)}
    end.

%% @doc 账单支付实现
-spec do_pay_invoice(map(), binary(), binary()) -> {ok, map()} | {error, binary()}.
do_pay_invoice(Inv, InvoiceNo, Method) ->
    Amount = maps:get(<<"amount">>, Inv, 0),
    PayOpts = #{amount => Amount, biz_type => ?BIZ_TYPE_BILLING},
    case payment_gateway:pay(Method, InvoiceNo, PayOpts) of
        {ok, PaymentNo} ->
            case billing_invoice_ds:mark_paid(InvoiceNo, PaymentNo) of
                ok ->
                    {ok, #{
                        <<"invoice_no">> => InvoiceNo,
                        <<"payment_no">> => PaymentNo,
                        <<"payment_method">> => Method,
                        <<"amount">> => Amount,
                        <<"status">> => 1
                    }};
                {error, not_found_or_paid} ->
                    %% 已被其它路径标记已付，幂等返回成功
                    {ok, #{
                        <<"invoice_no">> => InvoiceNo,
                        <<"payment_no">> => PaymentNo,
                        <<"status">> => 1,
                        <<"already_paid">> => true
                    }};
                {error, Reason} ->
                    {error, elib_cnv:safe_to_binary(Reason)}
            end;
        {error, Msg} when is_binary(Msg) ->
            {error, Msg};
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)}
    end.

%% @doc 含增量的配额预校验：used + Delta 是否超过 limit
-spec check_quota_with_delta(integer(), binary(), binary(), non_neg_integer()) ->
    ok | {error, quota_exceeded | binary()}.
check_quota_with_delta(SubId, Metric, Period, Delta) ->
    Sub = billing_subscription_ds:find_by_id(SubId),
    case map_size(Sub) =:= 0 of
        true ->
            {error, <<"订阅不存在"/utf8>>};
        false ->
            Limit = quota_limit(Sub, Metric),
            case Limit < 0 of
                true ->
                    %% 不限量
                    ok;
                false ->
                    Used = billing_usage_ds:get_used(SubId, Metric, Period),
                    case Used + Delta > Limit of
                        true -> {error, quota_exceeded};
                        false -> ok
                    end
            end
    end.

%% @doc 从订阅取套餐配额上限：读套餐 quota_config[Metric]，无则 -1（不限）
-spec quota_limit(map(), binary()) -> integer().
quota_limit(Sub, Metric) ->
    PlanId = maps:get(<<"plan_id">>, Sub, 0),
    Plan = billing_plan_ds:find_by_id(PlanId),
    Quota = decode_jsonb(maps:get(<<"quota_config">>, Plan, #{})),
    case maps:get(Metric, Quota, undefined) of
        undefined ->
            -1;
        V when is_integer(V) -> V;
        V when is_binary(V) ->
            try
                binary_to_integer(V)
            catch
                _:_ -> -1
            end;
        _ ->
            -1
    end.

%% @doc 套餐周期 -> 毫秒数（month 近似 30 天，year 近似 365 天）
-spec period_ms(binary()) -> pos_integer().
period_ms(<<"year">>) -> 365 * ?MS_PER_DAY;
period_ms(_) -> 30 * ?MS_PER_DAY.

%% @doc 续费基准时间（毫秒）：未过期从周期末顺延，已过期从当下顺延
-spec renew_base_ms(map()) -> integer().
renew_base_ms(Sub) ->
    NowMs = erlang:system_time(millisecond),
    EndMs = period_field_ms(Sub, <<"current_period_end">>),
    case EndMs of
        0 -> NowMs;
        _ when EndMs > NowMs -> EndMs;
        _ -> NowMs
    end.

%% @doc 从订阅 map 取时间字段并转毫秒时间戳；缺失返回 0
-spec period_field_ms(map(), binary()) -> integer().
period_field_ms(Sub, Key) ->
    case maps:get(Key, Sub, undefined) of
        undefined ->
            0;
        null ->
            0;
        V when is_binary(V) ->
            case elib_dt:rfc3339_to(V, millisecond) of
                N when is_integer(N) -> N;
                _ -> 0
            end;
        V when is_integer(V) -> V;
        _ ->
            0
    end.

%% @doc 生成 SQL 片段把毫秒时间戳转为 timestamptz
-spec ts_ms_to_sql(integer()) -> binary().
ts_ms_to_sql(Ms) ->
    MsBin = integer_to_binary(Ms),
    <<"to_timestamp(", MsBin/binary, "::bigint/1000)">>.

%% @doc 解析计费周期标识：undefined -> 当前年月 <<"YYYY-MM">>
-spec resolve_period(binary() | undefined) -> binary().
resolve_period(P) when is_binary(P), byte_size(P) > 0 -> P;
resolve_period(_) ->
    {{Y, M, _D}, _} = calendar:universal_time(),
    YB = integer_to_binary(Y),
    MB = pad2(M),
    <<YB/binary, "-", MB/binary>>.

-spec pad2(integer()) -> binary().
pad2(N) when N < 10 -> <<"0", (integer_to_binary(N))/binary>>;
pad2(N) -> integer_to_binary(N).

%% @doc 生成账单号 BILL<毫秒><随机8字节hex>
-spec gen_invoice_no() -> binary().
gen_invoice_no() ->
    Ts = integer_to_binary(erlang:system_time(millisecond)),
    Rand = binary:encode_hex(crypto:strong_rand_bytes(8)),
    <<"BILL", Ts/binary, "_", Rand/binary>>.

%% @doc 套餐字段校验
-spec validate_plan(binary(), binary(), integer(), binary()) -> ok | {error, binary()}.
validate_plan(Code, Name, Price, Period) ->
    if
        not is_binary(Code) orelse byte_size(Code) =:= 0 ->
            {error, <<"套餐编码不能为空"/utf8>>};
        not is_binary(Name) orelse byte_size(Name) =:= 0 ->
            {error, <<"套餐名称不能为空"/utf8>>};
        not is_integer(Price) orelse Price < 0 ->
            {error, <<"套餐价格不合法（单位：分）"/utf8>>};
        Period =/= <<"month">> andalso Period =/= <<"year">> ->
            {error, <<"计费周期仅支持 month/year"/utf8>>};
        true ->
            ok
    end.

%% @doc 套餐 transfer：quota_config 解码为 map 输出
-spec plan_transfer(map()) -> map().
plan_transfer(Plan) when map_size(Plan) =:= 0 -> #{};
plan_transfer(Plan) ->
    Quota = decode_jsonb(maps:get(<<"quota_config">>, Plan, #{})),
    Plan#{<<"quota_config">> => Quota}.

%% @doc map -> jsonb binary（写入 jsonb 列）
-spec encode_jsonb(map() | binary()) -> binary().
encode_jsonb(V) when is_binary(V) -> V;
encode_jsonb(V) when is_map(V) -> jsone:encode(V);
encode_jsonb(_) -> <<"{}">>.

%% @doc jsonb（binary 或已解码 map）-> map
-spec decode_jsonb(map() | binary() | undefined | null) -> map().
decode_jsonb(V) when is_map(V) -> V;
decode_jsonb(V) when is_binary(V) ->
    try jsone:decode(V) of
        M when is_map(M) -> M;
        _ -> #{}
    catch
        _:_ -> #{}
    end;
decode_jsonb(_) ->
    #{}.
