-module(channel_order_ds).
%%%
% channel_order_ds — G3 架构治理：channel_logic_order 不应直调 channel_order_repo
% G3: thin DS wrapper for channel orders
%%%

-include("log.hrl").

%% ==================== API ====================
-export([find_by_order_no/1]).
-export([list_by_user/2]).
-export([page/5]).

-spec find_by_order_no(binary()) -> map() | {error, any()}.
find_by_order_no(OrderNo) -> channel_order_repo:find_by_order_no(OrderNo).

-spec list_by_user(integer(), integer()) -> {ok, list(map())} | {error, any()}.
list_by_user(UserId, Limit) -> channel_order_repo:list_by_user(UserId, Limit).

-spec page(binary(), map(), binary(), pos_integer(), pos_integer()) ->
    {ok, map()} | {error, term()}.
page(Column, Where, Order, Page, Size) ->
    Tb = channel_order_repo:tablename(),
    elib_pg:page_with_total(Tb, Column, Where, Order, Page, Size).
