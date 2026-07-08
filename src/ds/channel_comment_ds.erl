-module(channel_comment_ds).
%%%
% channel_comment_ds — 频道评论数据源层
% 镜像 channel_message_ds 模式，封装 repo 并提供查询编排。
%%%

-export([add/1]).
-export([find_by_id/1]).
-export([list_by_message/3]).
-export([list_by_channel/3]).
-export([delete/1]).
-export([count_by_message/1]).
-export([like/1]).
-export([unlike/1]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ===================================================================
%% API functions
%% ===================================================================

-spec add(map()) -> {ok, integer()} | {error, term()}.
add(Data) ->
    channel_comment_repo:add(Data).

-spec find_by_id(integer()) -> map() | {error, any()}.
find_by_id(CommentId) ->
    channel_comment_repo:find_by_id(CommentId).

-spec list_by_message(integer(), integer(), integer()) -> {ok, list(map())} | {error, any()}.
list_by_message(MessageId, Cursor, Limit) ->
    channel_comment_repo:list_by_message(MessageId, Cursor, Limit).

-spec list_by_channel(integer(), integer(), integer()) -> {ok, list(map())} | {error, any()}.
list_by_channel(ChannelId, Cursor, Limit) ->
    channel_comment_repo:list_by_channel(ChannelId, Cursor, Limit).

-spec delete(integer()) -> {ok, non_neg_integer()} | {error, any()}.
delete(CommentId) ->
    channel_comment_repo:delete(CommentId).

-spec count_by_message(integer()) -> {ok, non_neg_integer()} | {error, any()}.
count_by_message(MessageId) ->
    channel_comment_repo:count_by_message(MessageId).

-spec like(integer()) -> {ok, non_neg_integer()} | {error, any()}.
like(CommentId) ->
    channel_comment_repo:increment_like(CommentId).

-spec unlike(integer()) -> {ok, non_neg_integer()} | {error, any()}.
unlike(CommentId) ->
    channel_comment_repo:decrement_like(CommentId).
