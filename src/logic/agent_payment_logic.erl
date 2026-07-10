-module(agent_payment_logic).

%%%
% Agent 受控支付业务逻辑 / Agent controlled-payment business logic（Phase 4 T4.3 地基切片）
%
% pay_with_mandate/4 串联三道闸门，任一不过绝不扣款：
%   ① mandate 有效：agent 有未过期、status=1 的授权（find_active 已在 SQL 侧过滤）
%   ② 额度门控：单笔 <= max_amount_fen（本层纯判定）；周期累计 + 本笔 <= max_total_fen
%              （try_reserve 原子预留，窗口过期自动重置）
%   ③ 从 owner_uid 扣款：wallet_ds:atomic_balance_change 单侧原子扣负数，RefNo 幂等
%              （前置 find_transaction_by_ref 挡重放）
%
% 金融安全不变量：
%   - 付款人恒为 mandate.owner_uid，绝不是 agent 自己。
%   - 扣款失败 → release 释放已预留额度，周期额度不被空耗、无资金变动。
%   - 幂等：同 RefNo 重放不重复预留、不重复扣款。
%
% ponytail: 本切片只做「门控扣款」（原子借记付款人 + 幂等 ledger）。给 ToUid 的
%   结算入账（贷记收款方钱包）留到下一切片：当前无暴露的两腿原子结算原语，拆成
%   两次非原子 atomic_balance_change 会有「借记成功/贷记失败」的资金丢失窗口，
%   故 ToUid 仅记入 ledger remark，结算走后续 transfer/escrow 集成。
%%%

-export([pay_with_mandate/4]).

-include("log.hrl").

%% wallet_transaction.tx_type：agent 受控支付（借记）
-define(TX_TYPE_AGENT_PAYMENT, 20).

%% @doc Agent 凭 mandate 受控扣款。付款人=mandate.owner_uid，收款方=ToUid。
%% @returns {ok, map()} | {error, Reason}
%%   Reason: invalid_params | mandate_invalid | invalid_payee | exceeds_single_limit
%%         | exceeds_total_limit | insufficient_balance | wallet_unavailable | payment_failed
-spec pay_with_mandate(integer(), integer(), integer(), binary()) ->
    {ok, map()} | {error, atom()}.
pay_with_mandate(AgentUid, ToUid, AmountFen, RefNo) ->
    case validate(AgentUid, ToUid, AmountFen, RefNo) of
        ok ->
            case agent_payment_mandate_ds:find_active(AgentUid) of
                {ok, Mandate} ->
                    gate_limits(Mandate, AgentUid, ToUid, AmountFen, RefNo);
                {error, _} ->
                    {error, mandate_invalid}
            end;
        Err ->
            Err
    end.

%% ===================================================================
%% Internal
%% ===================================================================

-spec validate(integer(), integer(), integer(), binary()) -> ok | {error, invalid_params}.
validate(AgentUid, ToUid, AmountFen, RefNo) when
    is_integer(AgentUid),
    is_integer(ToUid),
    is_integer(AmountFen),
    AmountFen > 0,
    is_binary(RefNo),
    RefNo =/= <<>>,
    ToUid =/= AgentUid
->
    ok;
validate(_, _, _, _) ->
    {error, invalid_params}.

%% 闸门②-单笔 + 付款人自付校验
-spec gate_limits(map(), integer(), integer(), integer(), binary()) ->
    {ok, map()} | {error, atom()}.
gate_limits(Mandate, AgentUid, ToUid, AmountFen, RefNo) ->
    OwnerUid = to_int(maps:get(<<"owner_uid">>, Mandate)),
    MaxAmount = to_int(maps:get(<<"max_amount_fen">>, Mandate)),
    MandateId = maps:get(<<"id">>, Mandate),
    case ToUid =:= OwnerUid of
        true ->
            {error, invalid_payee};
        false ->
            case AmountFen =< MaxAmount of
                false ->
                    {error, exceeds_single_limit};
                true ->
                    maybe_settle(MandateId, OwnerUid, AgentUid, ToUid, AmountFen, RefNo)
            end
    end.

%% 幂等前置 → 闸门②-周期累计预留
-spec maybe_settle(integer(), integer(), integer(), integer(), integer(), binary()) ->
    {ok, map()} | {error, atom()}.
maybe_settle(MandateId, OwnerUid, AgentUid, ToUid, AmountFen, RefNo) ->
    case is_duplicate(RefNo) of
        true ->
            %% 该 RefNo 已入账，直接返回幂等成功（不重复预留/扣款）
            {ok, #{ref_no => RefNo, idempotent => true}};
        false ->
            case agent_payment_mandate_ds:try_reserve(MandateId, AmountFen) of
                {ok, _NewSpent} ->
                    settle(MandateId, OwnerUid, AgentUid, ToUid, AmountFen, RefNo);
                {error, exceeds_total_limit} ->
                    {error, exceeds_total_limit};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% 闸门③：从 owner_uid 原子扣款；失败则释放预留额度
-spec settle(integer(), integer(), integer(), integer(), integer(), binary()) ->
    {ok, map()} | {error, atom()}.
settle(MandateId, OwnerUid, AgentUid, ToUid, AmountFen, RefNo) ->
    case debit_owner(OwnerUid, AgentUid, ToUid, AmountFen, RefNo) of
        {ok, Balance} ->
            {ok, #{
                ref_no => RefNo,
                owner_uid => OwnerUid,
                amount_fen => AmountFen,
                balance => Balance
            }};
        {error, Reason} ->
            _ = agent_payment_mandate_ds:release(MandateId, AmountFen),
            {error, Reason}
    end.

%% 从 owner_uid 借记 AmountFen（负数），RefNo 作 reference_no 幂等键
-spec debit_owner(integer(), integer(), integer(), integer(), binary()) ->
    {ok, integer()} | {error, atom()}.
debit_owner(OwnerUid, AgentUid, ToUid, AmountFen, RefNo) ->
    Wallet = wallet_ds:ensure_wallet(OwnerUid),
    case map_size(Wallet) =:= 0 of
        true ->
            {error, wallet_unavailable};
        false ->
            WalletId = maps:get(<<"id">>, Wallet),
            TxData = #{
                <<"wallet_id">> => WalletId,
                <<"user_id">> => OwnerUid,
                <<"amount">> => -AmountFen,
                <<"tx_type">> => ?TX_TYPE_AGENT_PAYMENT,
                <<"remark">> => remark(AgentUid, ToUid),
                <<"status">> => 1
            },
            case wallet_ds:atomic_balance_change(-AmountFen, OwnerUid, TxData, RefNo) of
                {ok, Balance} -> {ok, Balance};
                {rollback, insufficient_balance} -> {error, insufficient_balance};
                {rollback, _} -> {error, payment_failed};
                {error, _} -> {error, payment_failed}
            end
    end.

%% 幂等守卫：该 RefNo 是否已有成功流水
-spec is_duplicate(binary()) -> boolean().
is_duplicate(RefNo) ->
    map_size(wallet_ds:find_transaction_by_ref(RefNo)) > 0.

-spec remark(integer(), integer()) -> binary().
remark(AgentUid, ToUid) ->
    iolist_to_binary(io_lib:format("agent=~p to=~p", [AgentUid, ToUid])).

-spec to_int(term()) -> integer().
to_int(V) when is_integer(V) -> V;
to_int(V) when is_binary(V) -> binary_to_integer(V);
to_int(_) -> 0.
