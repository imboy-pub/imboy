-module(bot_webhook_delivery_logic).

-moduledoc "Bot webhook 投递死信重放业务逻辑（WH-01，管理员侧）。".
%%%
%%% WH-01：死信重放 logic（管理员）。
%%%

-export([replay_dead/1, dead_page/2, reencrypt_all/0]).

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

%% @doc 在双密钥窗内验证所有 Bot 密文；旧 key 命中时由 repo 原子重加密为当前 key。
%% 返回值只含计数和失败 Bot ID，绝不包含 secret 或密文。
reencrypt_all() ->
    case bot_repo:list_encrypted_bot_ids() of
        {ok, BotIds} -> reencrypt_all(BotIds, 0, []);
        {error, Reason} -> {error, {list_failed, Reason}}
    end.

reencrypt_all([], Processed, []) ->
    {ok, #{processed => Processed, failed_bot_ids => []}};
reencrypt_all([], Processed, Failed) ->
    {error, #{processed => Processed, failed_bot_ids => lists:reverse(Failed)}};
reencrypt_all([BotId | Rest], Processed, Failed) ->
    case bot_repo:get_verify_token(BotId) of
        {ok, _Secret} -> reencrypt_all(Rest, Processed + 1, Failed);
        {error, _Reason} -> reencrypt_all(Rest, Processed + 1, [BotId | Failed])
    end.
