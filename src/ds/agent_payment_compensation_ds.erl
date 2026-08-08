-module(agent_payment_compensation_ds).

%% Agent 支付补偿 outbox 的数据服务层。

-export([mark_settled/2]).
-export([release/1]).
-export([claim_pending/1]).
-export([mark_retry/3]).

-spec mark_settled(integer(), epgsql:connection()) -> ok | no_return().
mark_settled(CompensationId, Conn) ->
    agent_payment_compensation_repo:mark_settled(CompensationId, Conn).

-spec release(integer()) -> ok | {error, term()}.
release(CompensationId) ->
    agent_payment_compensation_repo:release(CompensationId).

-spec claim_pending(pos_integer()) -> {ok, [map()]} | {error, term()}.
claim_pending(Limit) ->
    agent_payment_compensation_repo:claim_pending(Limit).

-spec mark_retry(integer(), pos_integer(), term()) -> ok | {error, term()}.
mark_retry(CompensationId, DelaySecs, Reason) ->
    agent_payment_compensation_repo:mark_retry(CompensationId, DelaySecs, Reason).
