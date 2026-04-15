-module(push_token_ds).
%%%
% push_token_ds — G3 架构治理：push_notification_logic 不应直调 push_token_repo
% G3: thin DS wrapper for push tokens
%%%

-include("log.hrl").

%% ==================== API ====================
-export([upsert/5]).
-export([deactivate/2]).
-export([list_page/2]).

-spec upsert(integer(), binary(), binary(), binary(), binary()) -> {ok, term()} | {error, term()}.
upsert(Uid, DeviceId, DeviceType, Platform, Token) ->
    push_token_repo:upsert(Uid, DeviceId, DeviceType, Platform, Token).

-spec deactivate(integer(), binary()) -> {ok, integer()} | {error, term()}.
deactivate(Uid, DeviceId) ->
    push_token_repo:deactivate(Uid, DeviceId).

-spec list_page(pos_integer(), pos_integer()) -> {ok, map()} | {error, term()}.
list_page(Page, Size) -> push_token_repo:list_page(Page, Size).
