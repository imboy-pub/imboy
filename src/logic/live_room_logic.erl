-module(live_room_logic).
%% 直播间业务逻辑层
%% Live room business logic layer — thin delegation to live_room_ds.

-export([page_active/2]).
-export([page_by_uid/3]).
-export([create/1]).
-export([find_by_id/1]).
-export([update/2]).

-include("log.hrl").
-include("common.hrl").

-spec page_active(integer(), integer()) -> {ok, map()} | {error, term()}.
page_active(Page, Size) ->
    live_room_ds:page_active(Page, Size).

-spec page_by_uid(integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
page_by_uid(Uid, Page, Size) ->
    live_room_ds:page_by_uid(Uid, Page, Size).

-spec create(map()) -> {ok, integer()} | {error, term()}.
create(Data) ->
    live_room_ds:create(Data).

-spec find_by_id(binary() | integer()) -> map() | {error, term()}.
find_by_id(Id) ->
    live_room_ds:find_by_id(Id).

-spec update(integer() | binary(), map()) -> {ok, non_neg_integer()} | {error, term()}.
update(Id, Data) ->
    live_room_ds:update(Id, Data).
