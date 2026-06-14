-module(finance_adm_logic).
%%%===================================================================
%%% @doc 运营财务查询业务逻辑 / Admin finance query logic
%%%
%%% 供管理后台 /adm/finance/* 端点使用，提供跨用户的运营查询：
%%%   - 钱包分页列表 / 某用户钱包流水
%%%   - 充值订单分页列表
%%%   - 支付流水分页列表（对账）
%%%
%%% 与 /v1/* 用户态查询的区别：用户态是单用户视角，本模块是跨用户运营视角。
%%% billing 套餐/订阅/账单的运营查询复用 billing_logic（不在本模块）。
%%%
%%% 金额单位全程「分」(bigint) 透传不改，前端负责分→元显示。
%%% TSID 字段（id/user_id 等）经 elib_id:tsid_keys_to_bin/2 转字符串，避免 JS 精度丢失。
%%% @end
%%%===================================================================

-export([list_wallets/3]).
-export([list_wallet_transactions/3]).
-export([list_recharge_orders/3]).
-export([list_payment_transactions/3]).

-include("log.hrl").

%% 支付流水查询列（与 payment_transaction_repo 对齐）
-define(PAYMENT_TX_COLUMNS,
    <<"id, trade_no, biz_type, biz_order_no, user_id, gateway, ",
        "gateway_payment_no, amount, currency, status, paid_at, created_at, updated_at">>
).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 钱包分页列表（运营）
%% @param Filter map，可选 user_id / status（等值筛选）
%% @return {ok, Payload} list 中 id/user_id 已转字符串 | {error, term()}
-spec list_wallets(map(), pos_integer(), pos_integer()) -> {ok, map()} | {error, term()}.
list_wallets(Filter, Page, Size) ->
    WhereMap = build_where(Filter, [user_id, status]),
    case wallet_ds:page(WhereMap, Page, Size) of
        {ok, Payload} ->
            {ok, normalize_payload(Payload, [<<"id">>, <<"user_id">>])};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 某用户钱包流水分页（运营）
%% @param Uid 用户 id（整数，>0）
%% @return {ok, Payload} list 中 id/wallet_id/user_id 已转字符串 | {error, term()}
-spec list_wallet_transactions(integer(), pos_integer(), pos_integer()) ->
    {ok, map()} | {error, term()}.
list_wallet_transactions(Uid, Page, Size) ->
    %% wallet_ds:page_transactions/3 成功类型恒为 {ok, Payload}（dialyzer 推断 ok-only），
    %% 故此处直接解包返回。
    {ok, Payload} = wallet_ds:page_transactions(Page, Size, Uid),
    {ok, normalize_payload(Payload, [<<"id">>, <<"wallet_id">>, <<"user_id">>])}.

%% @doc 充值订单分页列表（运营）
%% @param Filter map，可选 status / payment_method / user_id / order_no（等值筛选）
%% @return {ok, Payload} list 中 id/user_id 已转字符串 | {error, term()}
-spec list_recharge_orders(map(), pos_integer(), pos_integer()) -> {ok, map()} | {error, term()}.
list_recharge_orders(Filter, Page, Size) ->
    WhereMap = build_where(Filter, [status, payment_method, user_id, order_no]),
    case recharge_order_ds:page(WhereMap, Page, Size) of
        {ok, Payload} ->
            {ok, normalize_payload(Payload, [<<"id">>, <<"user_id">>])};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 支付流水分页列表（对账，运营）
%% @param Filter map，可选 gateway / biz_type / status / user_id（等值筛选）
%% @return {ok, Payload} list 中 id/user_id 已转字符串 | {error, term()}
-spec list_payment_transactions(map(), pos_integer(), pos_integer()) ->
    {ok, map()} | {error, term()}.
list_payment_transactions(Filter, Page, Size) ->
    WhereMap = build_where(Filter, [gateway, biz_type, status, user_id, trade_no]),
    case payment_transaction_ds:page(?PAYMENT_TX_COLUMNS, WhereMap, <<"id desc">>, Page, Size) of
        {ok, Payload} ->
            {ok, normalize_payload(Payload, [<<"id">>, <<"user_id">>])};
        {error, Reason} ->
            {error, Reason}
    end.

%% ===================================================================
%% Internal
%% ===================================================================

%% @doc 从 Filter 中按白名单提取非 undefined 字段，组装等值 WhereMap。
%% 仅保留白名单内且有值的键，避免任意筛选注入与无意义条件。
-spec build_where(map(), [atom()]) -> map().
build_where(Filter, AllowedKeys) ->
    lists:foldl(
        fun(Key, Acc) ->
            case maps:get(Key, Filter, undefined) of
                undefined -> Acc;
                Value -> Acc#{Key => Value}
            end
        end,
        #{},
        AllowedKeys
    ).

%% @doc 规范化分页 payload：list 中各条目的指定 TSID 键转字符串。
-spec normalize_payload(map(), [binary()]) -> map().
normalize_payload(Payload, TsidKeys) ->
    List = maps:get(list, Payload, []),
    List2 = [elib_id:tsid_keys_to_bin(Item, TsidKeys) || Item <- List],
    Payload#{list => List2}.
