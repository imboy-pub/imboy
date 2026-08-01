-module(channel_order_repo).
%%%===================================================================
%%% @doc 频道订单 Repo 层
%%%
%%% 管理付费频道订单的数据库操作
%%% @end
%%%===================================================================

-include("log.hrl").

%%===================================================================
%%% API Functions
%%===================================================================

-export([tablename/0]).
-export([create_order/1]).
-export([find_by_order_no/1]).
-export([pay/2, pay/3]).
-export([refund/3]).
-export([cancel/1]).
-export([list_by_user/2]).
-export([has_purchased/2]).
-export([get_active_subscription/2]).

%%===================================================================
%%% Constants
%%===================================================================

-define(STATUS_PENDING, 0).
-define(STATUS_PAID, 1).
-define(STATUS_REFUNDED, 2).
-define(STATUS_CANCELLED, 3).
-define(STATUS_EXPIRED, 4).

-define(ORDER_EXPIRE_MINUTES, 30).

%%===================================================================
%%% API Implementation
%%===================================================================

%% @doc 返回表名
-spec tablename() -> binary().
tablename() ->
    <<"channel_order">>.

%% @doc 创建订单
%% @param Data 订单数据 map
%% @returns {ok, OrderNo} | {error, Reason}
-spec create_order(map()) -> {ok, binary()} | {error, term()}.
create_order(Data) ->
    ChannelId = maps:get(channel_id, Data),
    UserId = maps:get(user_id, Data),
    Amount = maps:get(amount, Data),
    Currency = maps:get(currency, Data, <<"CNY">>),
    PaymentMethod = maps:get(payment_method, Data, <<"mock">>),
    ExpiresAt = maps:get(expires_at, Data, default_expire_time()),
    ExtraData =
        case maps:get(extra_data, Data, null) of
            null -> null;
            % ponytail: encode map/list→jsonb, guard against future callers
            % 上限：binary 分支原样落库、不校验是否合法 JSON，非法 JSON binary 会在
            %   PG 侧报 22P02（与 msg_store 密文那次真 bug 同形）；非 map/list/binary/null
            %   的类型直接 function_clause 崩在这里。
            % 升级触发：extra_data 开始接受外部/客户端直传的 binary 时，比照
            %   msg_store_repo:msg_store_payload_to_jsonb 补 try-decode 校验。
            M when is_map(M) -> jsone:encode(M, [native_utf8]);
            L when is_list(L) -> jsone:encode(L, [native_utf8]);
            B when is_binary(B) -> B
        end,
    CreatedAt = maps:get(created_at, Data, elib_dt:millisecond()),

    OrderNo = generate_order_no(),
    GenId = elib_tsid:generate(channel_order),

    Sql =
        <<"INSERT INTO channel_order ",
            "(id, channel_id, user_id, order_no, amount, currency, status, payment_method, ",
            "expires_at, extra_data, created_at) ",
            "VALUES ($1, $2, $3, $4, $5, $6, $7, $8, to_timestamp($9::bigint / 1000), $10, to_timestamp($11::bigint / 1000)) ",
            "RETURNING order_no">>,
    case
        elib_pg:execute(Sql, [
            GenId,
            ChannelId,
            UserId,
            OrderNo,
            Amount,
            Currency,
            ?STATUS_PENDING,
            PaymentMethod,
            ExpiresAt,
            ExtraData,
            CreatedAt
        ])
    of
        {ok, 1, [{OrderNo}]} ->
            {ok, OrderNo};
        {error, {pgsql_error, #{code := <<"23505">>}}} ->
            % 订单号重复（极少情况），重试
            create_order(Data#{order_no => generate_order_no()});
        {error, Reason} ->
            ?ERROR_LOG([channel_order_create, failed, Reason]),
            {error, Reason}
    end.

%% @doc 根据订单号查找订单
-spec find_by_order_no(binary()) -> {ok, map()} | {error, not_found}.
find_by_order_no(OrderNo) ->
    Sql =
        <<"SELECT id, channel_id, user_id, order_no, amount, currency, status, ",
            "payment_method, payment_no, payment_at, subscription_start_at, subscription_end_at, ",
            "expires_at, refund_reason, refund_at, extra_data, created_at, updated_at ",
            "FROM channel_order WHERE order_no = $1">>,
    case elib_pg:query(Sql, [OrderNo]) of
        {ok, []} -> {error, not_found};
        {ok, [Row | _]} -> {ok, Row};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 支付订单
%% @param OrderNo 订单号
%% @param PaymentData 支付数据 #{payment_no, payment_method, ...}
-spec pay(binary(), map()) -> ok | {error, term()}.
pay(OrderNo, PaymentData) ->
    pay(OrderNo, PaymentData, 0).

%% @doc 支付订单，允许 GraceMinutes 分钟的过期宽限（B-08）。
%%
%% 用户主动支付走 pay/2（宽限 0）：订单过期就该重新下单。
%% **第三方回调走 pay/3 带宽限**：钱是真收了的，第 31 分钟到达的回调若被
%% `expires_at > NOW()` 挡掉，等于我们收了钱不发货 —— 比订单超时严重得多。
-spec pay(binary(), map(), non_neg_integer()) -> ok | {error, term()}.
pay(OrderNo, PaymentData, GraceMinutes) ->
    PaymentNo = maps:get(payment_no, PaymentData, <<>>),
    PaymentMethod = maps:get(payment_method, PaymentData, <<"mock">>),
    SubscriptionStart = maps:get(subscription_start_at, PaymentData, elib_dt:millisecond()),
    SubscriptionEnd = maps:get(subscription_end_at, PaymentData, null),

    % 构建SQL
    Sql =
        <<"UPDATE channel_order ",
            "SET status = $1, payment_no = $2, payment_method = $3, payment_at = NOW(), ",
            "subscription_start_at = to_timestamp($4::bigint / 1000), ",
            "subscription_end_at = CASE WHEN $5 IS NULL THEN NULL ELSE to_timestamp($5::bigint / 1000) END, ",
            "updated_at = NOW() ", "WHERE order_no = $6 AND status = 0 ",
            "AND expires_at > NOW() - make_interval(mins => $7::int)">>,
    case
        elib_pg:execute(Sql, [
            ?STATUS_PAID,
            PaymentNo,
            PaymentMethod,
            SubscriptionStart,
            SubscriptionEnd,
            OrderNo,
            max(0, GraceMinutes)
        ])
    of
        {ok, 0} -> {error, not_found_or_expired};
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 取消订单
-spec cancel(binary()) -> ok | {error, term()}.
cancel(OrderNo) ->
    Sql =
        <<"UPDATE channel_order ", "SET status = $1, updated_at = NOW() ",
            "WHERE order_no = $2 AND status = 0">>,
    case elib_pg:execute(Sql, [?STATUS_CANCELLED, OrderNo]) of
        {ok, 0} -> {error, not_found_or_not_pending};
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 退款
-spec refund(binary(), integer(), binary()) -> ok | {error, term()}.
refund(OrderNo, _AdminUid, Reason) ->
    Sql =
        <<"UPDATE channel_order ",
            "SET status = $1, refund_reason = $2, refund_at = NOW(), updated_at = NOW() ",
            "WHERE order_no = $3 AND status = 1">>,
    case elib_pg:execute(Sql, [?STATUS_REFUNDED, Reason, OrderNo]) of
        {ok, 0} -> {error, not_found_or_not_paid};
        {ok, _} -> ok;
        {error, Err} -> {error, Err}
    end.

%% @doc 获取用户的订单列表
-spec list_by_user(integer(), integer()) -> {ok, [map()]} | {error, term()}.
list_by_user(UserId, Limit) ->
    Sql =
        <<"SELECT o.id, o.channel_id, o.order_no, o.amount, o.currency, o.status, ",
            "o.payment_method, o.payment_at, o.expires_at, o.created_at, ",
            "c.name as channel_name ", "FROM channel_order o ",
            "JOIN channel c ON o.channel_id = c.id ", "WHERE o.user_id = $1 ",
            "ORDER BY o.created_at DESC LIMIT $2">>,
    case elib_pg:query(Sql, [UserId, Limit]) of
        {ok, Rows} -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 检查用户是否已购买频道
-spec has_purchased(integer(), integer()) -> boolean().
has_purchased(ChannelId, UserId) ->
    Sql =
        <<"SELECT COUNT(*) as cnt FROM channel_order ",
            "WHERE channel_id = $1 AND user_id = $2 AND status = 1">>,
    case elib_pg:query(Sql, [ChannelId, UserId]) of
        {ok, [#{<<"cnt">> := Count}]} when Count > 0 -> true;
        _ -> false
    end.

%% @doc 获取用户的有效订阅
-spec get_active_subscription(integer(), integer()) -> {ok, map()} | {error, not_found}.
get_active_subscription(ChannelId, UserId) ->
    Sql =
        <<"SELECT id, channel_id, user_id, order_no, amount, currency, ",
            "subscription_start_at, subscription_end_at, created_at ", "FROM channel_order ",
            "WHERE channel_id = $1 AND user_id = $2 AND status = 1 ",
            "AND (subscription_end_at IS NULL OR subscription_end_at > NOW()) ",
            "ORDER BY created_at DESC LIMIT 1">>,
    case elib_pg:query(Sql, [ChannelId, UserId]) of
        {ok, []} -> {error, not_found};
        {ok, [Row | _]} -> {ok, Row};
        {error, Reason} -> {error, Reason}
    end.

%%===================================================================
%%% Internal Functions
%%===================================================================

%% @doc 生成订单号
%% 格式: CH + 时间戳(13位) + 随机数(6位)
-spec generate_order_no() -> binary().
generate_order_no() ->
    Timestamp = erlang:system_time(millisecond),
    Random = rand:uniform(1000000) - 1,
    RandomStr = lists:flatten(io_lib:format("~6..0B", [Random])),
    iolist_to_binary(["CH", integer_to_binary(Timestamp), RandomStr]).

%% @doc 默认过期时间（30分钟后）
-spec default_expire_time() -> integer().
default_expire_time() ->
    elib_dt:millisecond() + (?ORDER_EXPIRE_MINUTES * 60 * 1000).
