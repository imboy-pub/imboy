-module(bot_webhook_delivery_logic).

%%%
%%% WH-01：死信重放 logic（管理员）。
%%%

-export([replay_dead/1, dead_page/2]).

%% @doc 单次重放 dead delivery：delivery_id 不变，attempt 续号。
-spec replay_dead(binary()) ->
    {ok, reused_delivery} | {error, not_dead | notfound | term()}.
replay_dead(DeliveryId) when is_binary(DeliveryId), DeliveryId =/= <<>> ->
    bot_webhook_delivery_repo:replay(DeliveryId);
replay_dead(_) ->
    {error, notfound}.

%% @doc 死信分页（审计）。
dead_page(Page, Size) ->
    bot_webhook_delivery_repo:list_dead(Page, Size).
