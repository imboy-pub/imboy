-module(live_room_ds).
%%%
%% live_room_ds — 直播间数据服务层（thin pass-through to live_room_repo）
%% Live room data service layer (thin pass-through to live_room_repo).
%%
%% 创建动机 / Motivation:
%%   G2-a 治理：`live_room_handler` 原本直调 `live_room_repo` 8 处，违反
%%   Handler → Logic/DS → Repo 的 4 层架构。此模块作为 DS 层薄封装，
%%   不承担额外业务逻辑，仅路由到 repo；后续如需缓存 / 事务 / 事件广播，
%%   再在此处集中加工。
%%   (G2-a remediation — handler no longer hits repo directly; future cache /
%%    transaction / broadcast hooks land here.)
%%%

-export([find_by_id/1]).
-export([create/1]).
-export([update/2]).
-export([page_active/2]).
-export([page_by_uid/3]).

-spec find_by_id(binary() | integer()) -> map() | {error, term()}.
find_by_id(Id) ->
    live_room_repo:find_by_id(Id).

-spec create(map()) -> {ok, integer()} | {error, term()}.
create(Data) ->
    live_room_repo:create(Data).

-spec update(integer() | binary(), map()) -> {ok, non_neg_integer()} | {error, term()}.
update(Id, Data) ->
    live_room_repo:update(Id, Data).

-spec page_active(integer(), integer()) -> {ok, map()} | {error, term()}.
page_active(Page, Size) ->
    live_room_repo:page_active(Page, Size).

-spec page_by_uid(integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
page_by_uid(Uid, Page, Size) ->
    live_room_repo:page_by_uid(Uid, Page, Size).
