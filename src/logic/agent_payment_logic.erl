-module(agent_payment_logic).

%%%
% Agent 受控支付业务逻辑 / Agent controlled-payment business logic（Phase 4 T4.3 地基切片）
%
% pay_with_mandate/4 串联三道闸门，任一不过绝不扣款：
%   ① mandate 有效：agent 有未过期、status=1 的授权（find_active 已在 SQL 侧过滤）
%   ② 额度门控：单笔 <= max_amount_fen（本层纯判定）；周期累计 + 本笔 <= max_total_fen
%              （try_reserve_with_compensation 原子预留 + 补偿 outbox，窗口过期自动重置）
%   ③ 原子结算：wallet_ds:atomic_transfer 单事务内「借记 owner_uid + 贷记 ToUid」，
%              各写一条 wallet_transaction 流水（借记用 RefNo、贷记用 RefNo<>"_CR"）。
%              前置 find_transaction_by_ref(RefNo) 挡重放；两腿不同 RefNo 撞唯一索引兜底。
%
% 金融安全不变量：
%   - 付款人恒为 mandate.owner_uid，绝不是 agent 自己；收款方=ToUid。
%   - 结算是**单事务两腿原子**：借记成功 + 贷记失败 无法发生（任一步失败整笔 rollback），
%     故不存在「扣了付款人却没到账」的资金丢失窗口。
%   - 结算失败 → compensation outbox 在同一事务中释放已预留额度，周期额度不被空耗、无资金变动。
%   - 幂等：同 RefNo 重放不重复预留、不重复结算。
%%%

-export([pay_with_mandate/4]).

-include("log.hrl").

%% wallet_transaction.tx_type：agent 受控支付借记(付款人) / 贷记(收款方)
%% ⚠️ 值域受 chk_wallet_tx_type 约束，须由迁移 00000030 放行 20/21
-define(TX_TYPE_AGENT_PAYMENT, 20).
-define(TX_TYPE_AGENT_PAYMENT_CREDIT, 21).
-define(RELEASE_RETRY_COUNT, 3).
-define(RELEASE_RETRY_DELAY_MS, 100).

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
    %% 借记 RefNo 不得以贷记派生后缀结尾：否则攻击者可提交 <<"<他人未来RefNo>_CR">> 作借记键，
    %% 占用他人合法支付贷记腿的 reference_no，令其贷记 INSERT 撞唯一索引→整笔 rollback（拒付型 DoS）。
    case is_reserved_ref(RefNo) of
        true -> {error, invalid_params};
        false -> ok
    end;
validate(_, _, _, _) ->
    {error, invalid_params}.

%% RefNo 是否以贷记派生后缀 "_CR" 结尾（credit_ref/1 的产物，属保留命名空间）
-spec is_reserved_ref(binary()) -> boolean().
is_reserved_ref(RefNo) ->
    Suffix = <<"_CR">>,
    Sz = byte_size(Suffix),
    byte_size(RefNo) >= Sz andalso
        binary:part(RefNo, byte_size(RefNo) - Sz, Sz) =:= Suffix.

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
            case
                agent_payment_mandate_ds:try_reserve_with_compensation(
                    MandateId, AmountFen, RefNo
                )
            of
                {ok, _NewSpent, CompensationId} ->
                    settle(
                        OwnerUid,
                        AgentUid,
                        ToUid,
                        AmountFen,
                        RefNo,
                        CompensationId
                    );
                {error, exceeds_total_limit} ->
                    {error, exceeds_total_limit};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% 闸门③：单事务两腿原子结算（借记 owner + 贷记 ToUid）。
%% compensation outbox 在此事务内标记 settled；失败时由同一 outbox 事务释放额度。
-spec settle(integer(), integer(), integer(), integer(), binary(), integer()) ->
    {ok, map()} | {error, atom()}.
settle(OwnerUid, AgentUid, ToUid, AmountFen, RefNo, CompensationId) ->
    case settle_transfer(OwnerUid, AgentUid, ToUid, AmountFen, RefNo, CompensationId) of
        {ok, #{debit_balance := OwnerBal, credit_balance := PayeeBal}} ->
            {ok, #{
                ref_no => RefNo,
                owner_uid => OwnerUid,
                to_uid => ToUid,
                amount_fen => AmountFen,
                owner_balance => OwnerBal,
                payee_balance => PayeeBal
            }};
        {error, Reason} ->
            _ = release_reservation(CompensationId),
            {error, Reason}
    end.

-spec release_reservation(integer()) -> ok | {error, term()}.
release_reservation(CompensationId) ->
    Retry = elib_retry:with_retry(
        fun() ->
            case agent_payment_compensation_ds:release(CompensationId) of
                ok -> ok;
                {error, ReleaseReason} -> erlang:error({release_failed, ReleaseReason});
                Other -> erlang:error({release_unexpected, Other})
            end
        end,
        ?RELEASE_RETRY_COUNT,
        ?RELEASE_RETRY_DELAY_MS
    ),
    case Retry of
        {ok, ok} ->
            ok;
        {error, RetryReason} ->
            ok = ?ERROR_LOG(
                {agent_payment_reservation_release_failed, CompensationId, RetryReason}
            ),
            {error, RetryReason}
    end.

%% 组装借/贷两腿并原子结算。付款人=OwnerUid、收款方=ToUid，绝不互换。
%% 借记腿 RefNo，贷记腿 credit_ref(RefNo)（不同 RefNo 避免撞唯一索引）。
-spec settle_transfer(integer(), integer(), integer(), integer(), binary(), integer()) ->
    {ok, map()} | {error, atom()}.
settle_transfer(OwnerUid, AgentUid, ToUid, AmountFen, RefNo, CompensationId) ->
    OwnerWallet = wallet_ds:ensure_wallet(OwnerUid),
    ToWallet = wallet_ds:ensure_wallet(ToUid),
    case (map_size(OwnerWallet) =:= 0) orelse (map_size(ToWallet) =:= 0) of
        true ->
            {error, wallet_unavailable};
        false ->
            Remark = remark(AgentUid, ToUid),
            Debit = #{
                user_id => OwnerUid,
                wallet_id => maps:get(<<"id">>, OwnerWallet),
                amount => AmountFen,
                tx_type => ?TX_TYPE_AGENT_PAYMENT,
                remark => Remark,
                reference_no => RefNo
            },
            Credit = #{
                user_id => ToUid,
                wallet_id => maps:get(<<"id">>, ToWallet),
                amount => AmountFen,
                tx_type => ?TX_TYPE_AGENT_PAYMENT_CREDIT,
                remark => Remark,
                reference_no => credit_ref(RefNo),
                after_transfer => fun(Conn) ->
                    agent_payment_compensation_ds:mark_settled(CompensationId, Conn)
                end
            },
            case wallet_ds:atomic_transfer(Debit, Credit) of
                {ok, _} = Ok -> Ok;
                {rollback, insufficient_balance} -> {error, insufficient_balance};
                {rollback, _} -> {error, payment_failed};
                {error, _} -> {error, payment_failed};
                %% 兜底：elib_pg:with_tx 对非 rollback 异常重试耗尽会返回 {error,Class,Reason}
                %% 等其它形态，若不归一会 case_clause 逃逸出 settle_transfer→settle 的 release
                %% 补偿永不执行，导致预留额度永久泄漏 + 未捕获崩溃。归一为 payment_failed。
                _Other -> {error, payment_failed}
            end
    end.

%% 贷记腿 RefNo：借记腿 RefNo 派生，避免撞 wallet_transaction.reference_no 唯一索引
-spec credit_ref(binary()) -> binary().
credit_ref(RefNo) -> <<RefNo/binary, "_CR">>.

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
