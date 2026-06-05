-module(feedback_logic).

%%%
% feedback 业务逻辑模块
% feedback business logic module
%%%

-export([add/10]).
-export([page/5]).
-export([page_reply/5]).
-export([remove/2]).

-include("log.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 新增用户反馈
-spec add(
    integer(),
    binary(),
    binary(),
    binary(),
    binary(),
    binary(),
    binary(),
    binary(),
    binary(),
    binary()
) ->
    ok | {ok, non_neg_integer()}.
add(Uid, Did, COS, COSV, AppVsn, Type, Rating, ContactDetail, Body, Attach) ->
    feedback_ds:add(Uid, Did, COS, COSV, AppVsn, Type, Rating, ContactDetail, Body, Attach).

%% @doc 删除用户反馈
-spec remove(integer(), integer()) -> ok.
remove(Uid, FeedbackId) ->
    feedback_ds:remove(Uid, FeedbackId).

%% @doc 分页查询反馈列表
-spec page(binary(), map(), binary(), pos_integer(), pos_integer()) ->
    {ok, map()} | {error, term()}.
page(Column, Where, Order, Page, Size) ->
    feedback_ds:page(Column, Where, Order, Page, Size).

%% @doc 分页查询反馈回复列表
-spec page_reply(binary(), map(), binary(), pos_integer(), pos_integer()) ->
    {ok, map()} | {error, term()}.
page_reply(Column, Where, Order, Page, Size) ->
    feedback_ds:page_reply(Column, Where, Order, Page, Size).

%% ===================================================================
%% EUnit tests.
%% ===================================================================
