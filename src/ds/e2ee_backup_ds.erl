-module(e2ee_backup_ds).
%%%===================================================================
%%% @doc E2EE 加密密钥备份 DS 层
%%%
%%% 低频端点（换机/备份时才调用），无缓存必要，
%%% 仅做分层透传（Logic → DS → Repo）。
%%%===================================================================

%%===================================================================
%%% API Functions Export
%%===================================================================
-export([save/1]).
-export([latest/1]).
-export([delete_by_uid/1]).

%%===================================================================
%%% API Functions
%%===================================================================

-spec save(map()) -> {ok, integer()} | {error, term()}.
save(Params) ->
    e2ee_backup_repo:save(Params).

-spec latest(integer()) -> {ok, map()} | {error, not_found} | {error, term()}.
latest(Uid) ->
    e2ee_backup_repo:latest(Uid).

-spec delete_by_uid(integer()) -> {ok, non_neg_integer()} | {error, term()}.
delete_by_uid(Uid) ->
    e2ee_backup_repo:delete_by_uid(Uid).
