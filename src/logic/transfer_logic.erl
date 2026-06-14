-module(transfer_logic).
%%%===================================================================
%%% @doc 单聊转账业务逻辑层 / P2P Transfer business logic
%%%===================================================================

-include("log.hrl").

-export([send/4, accept/2, refund/1, detail/1]).

%% @doc 发起转账
-spec send(integer(), integer(), integer(), binary()) ->
    {ok, integer()} | {error, term()}.
send(SenderUid, ReceiverUid, Amount, Remark) ->
    case is_integer(Amount) andalso Amount >= 100 andalso SenderUid =/= ReceiverUid of
        false ->
            {error, <<"转账参数不合法"/utf8>>};
        true ->
            case transfer_repo:create(SenderUid, ReceiverUid, Amount, Remark) of
                {ok, TransferId} ->
                    {ok, TransferId};
                {rollback, insufficient_balance} ->
                    {error, <<"钱包余额不足"/utf8>>};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @doc 收取转账
-spec accept(integer(), integer()) -> {ok, integer()} | {error, term()}.
accept(TransferId, ReceiverUid) ->
    case transfer_repo:accept(TransferId, ReceiverUid) of
        {ok, Amount} ->
            {ok, Amount};
        {rollback, invalid_status} ->
            {error, <<"状态不合法，无法收取"/utf8>>};
        {rollback, not_found} ->
            {error, <<"转账订单不存在"/utf8>>};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 拒收/逾期退回转账
-spec refund(integer()) -> {ok, integer()} | {error, term()}.
refund(TransferId) ->
    case transfer_repo:refund(TransferId) of
        {ok, Amount} ->
            {ok, Amount};
        {rollback, invalid_status} ->
            {error, <<"状态不合法，无法退回"/utf8>>};
        {rollback, not_found} ->
            {error, <<"转账订单不存在"/utf8>>};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 查询转账单详情
-spec detail(integer()) -> {ok, map()} | {error, term()}.
detail(TransferId) ->
    Order = transfer_repo:find_by_id(TransferId),
    case map_size(Order) =:= 0 of
        true ->
            {error, <<"转账单不存在"/utf8>>};
        false ->
            {ok, Order}
    end;
detail(_) ->
    {error, <<"参数不合法"/utf8>>}.
