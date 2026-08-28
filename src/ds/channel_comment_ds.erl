-module(channel_comment_ds).
%%%
% channel_comment_ds — 频道评论数据源层
% 镜像 channel_message_ds 模式，封装 repo 并提供查询编排。
%
% T7 归档写守卫（R3 #9 收口）：全部写路径（add/delete/like/unlike）经
% workspace_guard:write_tx 与守卫同事务提交（channel→workspace 行锁），
% 消除原 logic 层前置检查的"检查-写窗口"；personal 频道由 resolver 直通。
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
    ChannelId = maps:get(<<"channel_id">>, Data, maps:get(channel_id, Data, 0)),
    workspace_guard:write_tx({channel, ChannelId}, fun(Conn) ->
        channel_comment_repo:add_tx(Conn, Data)
    end).

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
    workspace_guard:write_tx({channel_comment, CommentId}, fun(Conn) ->
        channel_comment_repo:delete_tx(Conn, CommentId)
    end).

-spec count_by_message(integer()) -> {ok, non_neg_integer()} | {error, any()}.
count_by_message(MessageId) ->
    channel_comment_repo:count_by_message(MessageId).

-spec like(integer()) -> {ok, non_neg_integer()} | {error, any()}.
like(CommentId) ->
    workspace_guard:write_tx({channel_comment, CommentId}, fun(Conn) ->
        channel_comment_repo:increment_like_tx(Conn, CommentId)
    end).

-spec unlike(integer()) -> {ok, non_neg_integer()} | {error, any()}.
unlike(CommentId) ->
    workspace_guard:write_tx({channel_comment, CommentId}, fun(Conn) ->
        channel_comment_repo:decrement_like_tx(Conn, CommentId)
    end).
