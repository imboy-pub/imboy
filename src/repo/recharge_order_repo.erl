-module(recharge_order_repo).
%%%===================================================================
%%% @doc 充值订单 Repo 层 / Recharge order repository
%%%
%%% 钱包充值走「下单 -> 第三方支付 -> 回调入账」模式。
%%% 金额单位「分」与钱包统一；订单号 RCH 前缀；默认 30 分钟过期。
%%% status: 0=待支付 / 1=已支付 / 2=取消 / 3=退款 / 4=过期
%%%
%%% 所有 SQL 通过 elib_pg 参数化访问；TSID 由 elib_tsid:generate(recharge_order) 生成。
%%% @end
%%%===================================================================

-include("log.hrl").

%%===================================================================
%%% API Functions
%%===================================================================

-export([tablename/0]).
-export([create/1]).
-export([find_by_order_no/1]).
-export([mark_paid/2]).
-export([update_status/2]).
-export([page_by_user/3]).

%%===================================================================
%%% Constants
%%===================================================================

-define(STATUS_PENDING, 0).
-define(STATUS_PAID, 1).
-define(STATUS_CANCELLED, 2).
-define(STATUS_REFUNDED, 3).
-define(STATUS_EXPIRED, 4).

-define(ORDER_EXPIRE_MINUTES, 30).

%%===================================================================
%%% API Implementation
%%===================================================================

%% @doc 返回表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"recharge_order">>).

%% @doc 创建充值订单（status=0 待支付）
%% @param Data 包含 user_id, amount(分), currency, payment_method
%% @returns {ok, OrderNo} | {error, Reason}
-spec create(map()) -> {ok, binary()} | {error, term()}.
create(Data) ->
    Tb = tablename(),
    UserId = maps:get(user_id, Data),
    Amount = maps:get(amount, Data),
    Currency = maps:get(currency, Data, <<"CNY">>),
    PaymentMethod = maps:get(payment_method, Data),
    ExpiresAt = maps:get(expires_at, Data, default_expire_time()),
    CreatedAt = maps:get(created_at, Data, elib_dt:now()),

    OrderNo = generate_order_no(),
    GenId = elib_tsid:generate(recharge_order),

    Sql =
        <<"INSERT INTO ", Tb/binary,
            " (id, order_no, user_id, amount, currency, payment_method, status,"
            " expires_at, created_at)"
            " VALUES ($1, $2, $3, $4, $5, $6, $7,"
            " to_timestamp($8/1000), to_timestamp($9/1000))"
            " RETURNING order_no">>,
    case
        elib_pg:execute(Sql, [
            GenId,
            OrderNo,
            UserId,
            Amount,
            Currency,
            PaymentMethod,
            ?STATUS_PENDING,
            ExpiresAt,
            CreatedAt
        ])
    of
        {ok, 1, [{OrderNo}]} ->
            {ok, OrderNo};
        {error, {pgsql_error, #{code := <<"23505">>}}} ->
            %% 订单号重复（极少情况），重试一次（生成新订单号）
            create(maps:remove(created_at, Data));
        {error, Reason} ->
            ?ERROR_LOG([recharge_order_create, failed, Reason]),
            {error, Reason}
    end.

%% @doc 根据订单号查找充值订单
-spec find_by_order_no(binary()) -> {ok, map()} | {error, not_found | term()}.
find_by_order_no(OrderNo) ->
    Tb = tablename(),
    Sql = <<
        "SELECT id, order_no, user_id, amount, currency, payment_method,"
        " payment_no, status, paid_at, expires_at, extra_data,"
        " created_at, updated_at"
        " FROM ",
        Tb/binary,
        " WHERE order_no = $1"
    >>,
    case elib_pg:query(Sql, [OrderNo]) of
        {ok, []} -> {error, not_found};
        {ok, [Row | _]} -> {ok, Row};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 标记订单已支付（条件更新，幂等保护）
%% 仅当 status=0（待支付）且未过期时才会更新成功，回填 payment_no 与 paid_at。
%% @param OrderNo 订单号
%% @param GatewayPaymentNo 第三方支付单号
%% @returns ok 更新成功 | {error, not_found_or_paid} 已支付/不存在/已过期 | {error, Reason}
-spec mark_paid(binary(), binary()) -> ok | {error, not_found_or_paid | term()}.
mark_paid(OrderNo, GatewayPaymentNo) ->
    Tb = tablename(),
    Sql =
        <<"UPDATE ", Tb/binary,
            " SET status = $1, payment_no = $2, paid_at = NOW(), updated_at = NOW()"
            " WHERE order_no = $3 AND status = 0 AND expires_at > NOW()">>,
    case elib_pg:execute(Sql, [?STATUS_PAID, GatewayPaymentNo, OrderNo]) of
        {ok, 0} -> {error, not_found_or_paid};
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 更新订单状态（用于取消/退款/过期等流转）
-spec update_status(binary(), integer()) -> ok | {error, not_found | term()}.
update_status(OrderNo, Status) when is_integer(Status) ->
    Tb = tablename(),
    Sql = <<"UPDATE ", Tb/binary, " SET status = $1, updated_at = NOW() WHERE order_no = $2">>,
    case elib_pg:execute(Sql, [Status, OrderNo]) of
        {ok, 0} -> {error, not_found};
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 分页查询用户充值订单
-spec page_by_user(integer(), pos_integer(), pos_integer()) -> {ok, map()} | {error, term()}.
page_by_user(Uid, Page, Size) ->
    Tb = tablename(),
    Column = <<
        "id, order_no, user_id, amount, currency, payment_method,"
        " payment_no, status, paid_at, expires_at, created_at"
    >>,
    WhereMap = #{user_id => Uid},
    Order = <<"id desc">>,
    elib_pg:page_with_total(Tb, Column, WhereMap, Order, Page, Size).

%%===================================================================
%%% Internal Functions
%%===================================================================

%% @doc 生成充值订单号
%% 格式: RCH + 时间戳(13位) + 随机数(6位)
-spec generate_order_no() -> binary().
generate_order_no() ->
    Timestamp = erlang:system_time(millisecond),
    Random = rand:uniform(1000000) - 1,
    RandomStr = lists:flatten(io_lib:format("~6..0B", [Random])),
    iolist_to_binary(["RCH", integer_to_binary(Timestamp), RandomStr]).

%% @doc 默认过期时间（30 分钟后，毫秒时间戳）
-spec default_expire_time() -> integer().
default_expire_time() ->
    elib_dt:now() + (?ORDER_EXPIRE_MINUTES * 60 * 1000).
