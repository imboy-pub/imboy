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
    case is_integer(Amount) andalso Amount >= 1 andalso SenderUid =/= ReceiverUid of
        false ->
            {error, <<"转账参数不合法"/utf8>>};
        true ->
            case transfer_repo:create(SenderUid, ReceiverUid, Amount, Remark) of
                {ok, TransferId} ->
                    {ok, TransferId};
                {rollback, insufficient_balance} ->
                    {error, <<"钱包余额不足"/utf8>>};
                %% with_tx 对任意事务内 throw({rollback, _}) 均原样返回
                %% {rollback, Reason}；缺此分支会 case_clause 崩溃（500）。
                %% 事务已回滚、无资金变动，归一为可重试错误。
                {rollback, _Reason} ->
                    {error, <<"转账失败，请稍后再试"/utf8>>};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @doc 收取转账
-spec accept(integer(), integer()) -> {ok, integer()} | {error, term()}.
accept(TransferId, ReceiverUid) ->
    %% 先确保接收方钱包存在：transfer_repo:accept 在事务内用
    %% "UPDATE wallet ... WHERE user_id RETURNING balance, id" 给接收方入账，
    %% 若接收方从未开通钱包则命中 0 行 → 硬匹配 {ok, 1, _} badmatch → 事务崩溃回滚，
    %% 转账永久卡在 pending、款项无法到账。ensure_wallet 幂等，与充值/提现/发红包一致。
    _ = wallet_ds:ensure_wallet(ReceiverUid),
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
detail(TransferId) when is_integer(TransferId) ->
    Order = transfer_repo:find_by_id(TransferId),
    case map_size(Order) =:= 0 of
        true ->
            {error, <<"转账单不存在"/utf8>>};
        false ->
            {ok, Order}
    end;
detail(_) ->
    {error, <<"参数不合法"/utf8>>}.
