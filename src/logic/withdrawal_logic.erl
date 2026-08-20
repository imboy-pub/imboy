-module(withdrawal_logic).
%%%===================================================================
%%% @doc 提现业务逻辑层 / Withdrawal business logic
%%%===================================================================

-include("log.hrl").

-export([withdraw/4]).

%% @doc 提现：原子扣除钱包余额 + 记录提现交易
-spec withdraw(integer(), integer(), binary(), binary()) ->
    {ok, integer()} | {error, term()}.
withdraw(Uid, Amount, Method, Account) ->
    case
        is_integer(Amount) andalso Amount >= 1 andalso byte_size(Method) > 0 andalso
            byte_size(Account) > 0
    of
        false ->
            {error, <<"提现参数不合法"/utf8>>};
        true ->
            Wallet = wallet_ds:ensure_wallet(Uid),
            case map_size(Wallet) =:= 0 of
                true ->
                    {error, <<"钱包不可用"/utf8>>};
                false ->
                    WalletId = maps:get(<<"id">>, Wallet),
                    Remark = <<"提现至"/utf8, Method/binary, "[", Account/binary, "]">>,
                    TxData = #{
                        <<"wallet_id">> => WalletId,
                        <<"user_id">> => Uid,
                        <<"amount">> => -Amount,
                        % 提现
                        <<"tx_type">> => 10,
                        <<"remark">> => Remark,
                        <<"status">> => 0
                    },
                    RefNo = gen_ref_no(),
                    case wallet_ds:atomic_balance_change(-Amount, Uid, TxData, RefNo) of
                        {ok, NewBalance} ->
                            {ok, #{
                                <<"balance">> => NewBalance,
                                <<"balance_yuan">> => NewBalance / 100.0,
                                <<"reference_no">> => RefNo
                            }};
                        %% atomic_balance_change 余额不足时抛 {rollback, insufficient_balance}
                        %% （wallet_repo.erl 内 throw），不是 {error, insufficient_balance}；
                        %% 旧写法这里恒不匹配 → 提现余额不足时必现 case_clause 崩溃。
                        {rollback, insufficient_balance} ->
                            {error, <<"钱包余额不足"/utf8>>};
                        %% with_tx 对任意事务内 throw({rollback, _}) 均原样返回
                        %% {rollback, Reason}（如 reference_no 唯一冲突 / 事务内 DB 故障）；
                        %% 缺此分支会落入 case_clause 崩溃（500）。事务已回滚、无资金变动，
                        %% 归一为可重试错误，与 payment_wallet_gateway 的完整处理一致。
                        {rollback, _Reason} ->
                            {error, <<"提现失败，请稍后再试"/utf8>>};
                        {error, Reason} ->
                            {error, Reason}
                    end
            end
    end.

%% ===================================================================
%% Internal Functions
%% ===================================================================

gen_ref_no() ->
    Id = elib_tsid:generate(wallet_transaction),
    <<"WDO_", (integer_to_binary(Id))/binary>>.
