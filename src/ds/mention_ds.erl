-module(mention_ds).
%%%
% mention_ds 是 @提及数据服务层
% 提供 @提及记录的数据服务功能
%%%
-include("log.hrl").

-export([save_mentions/4]).
-export([find_by_msg_id/1]).
-export([list_by_uid/2]).
-export([list_by_uid/3]).
-export([list_by_group_and_uid/3]).
-export([list_by_group_and_uid/4]).
-export([mark_as_read/2]).
-export([mark_as_read_by_mention_id/2]).
-export([mark_all_as_read/1]).
-export([mark_group_as_read/2]).
-export([count_unread/1]).
-export([count_unread_in_group/2]).
-export([delete_by_msg_id/1]).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 保存@提及记录
%% @param MsgId 消息ID
%% @param Gid 群组ID
%% @param Mentions 被@的用户ID列表，可以是 <<"all">> 表示@所有人
%% @param FromUid 发送者ID
%% @return ok | {error, Reason}
-spec save_mentions(binary(), integer(), list(binary()), integer()) -> ok | {error, term()}.
save_mentions(MsgId, Gid, Mentions, FromUid) when is_list(Mentions) ->
    case lists:member(<<"all">>, Mentions) of
        true ->
            % @所有人，获取群组所有成员
            MemberUids = group_ds:member_uids(Gid),
            save_mentions_to_members(MsgId, Gid, MemberUids, FromUid);
        false ->
            % @特定用户，解析ID列表
            MemberUids = decode_mention_uids(Mentions, []),
            save_mentions_to_members(MsgId, Gid, MemberUids, FromUid)
    end.

%% @private
%% @doc 解析@用户ID列表
-spec decode_mention_uids(list(binary()), list(integer())) -> list(integer()).
decode_mention_uids([], Acc) ->
    lists:reverse(Acc);
decode_mention_uids([IdBin | Rest], Acc) ->
    Uid = ec_cnv:to_integer(IdBin),
    case Uid > 0 of
        true -> decode_mention_uids(Rest, [Uid | Acc]);
        false -> decode_mention_uids(Rest, Acc)
    end.

%% @private
%% @doc 保存@提及记录到数据库
-spec save_mentions_to_members(binary(), integer(), list(integer()), integer()) -> ok.
save_mentions_to_members(_MsgId, _Gid, [], _FromUid) ->
    ok;
save_mentions_to_members(MsgId, Gid, [Uid | Rest], FromUid) ->
    % 异步插入，不阻塞消息发送
    elib_async:async(fun() ->
        mention_repo:insert(MsgId, Gid, Uid, FromUid)
    end),
    save_mentions_to_members(MsgId, Gid, Rest, FromUid).

%% @doc 查询用户的所有@提及（支持已读/未读过滤）
%% @param Uid 用户ID
%% @param IsRead 已读状态（true/false/undefined 表示全部）
%% @return {ok, list(map())} | {error, Reason}
-spec list_by_uid(integer(), boolean() | undefined) -> {ok, list(map())} | {error, term()}.
list_by_uid(Uid, IsRead) ->
    list_by_uid(Uid, IsRead, #{}).

%% @doc 查询用户的所有@提及（支持分页）
%% @param Uid 用户ID
%% @param IsRead 已读状态（true/false/undefined 表示全部）
%% @param Options 选项，可包含 #{page => integer(), size => integer()}
%% @return {ok, list(map())} | {error, Reason}
-spec list_by_uid(integer(), boolean() | undefined, map()) -> {ok, list(map())} | {error, term()}.
list_by_uid(Uid, IsRead, Options) ->
    case mention_repo:find_by_uid(Uid, IsRead) of
        {ok, Results} ->
            % 处理分页
            case maps:get(page, Options, undefined) of
                undefined ->
                    {ok, Results};
                Page ->
                    Size = maps:get(size, Options, 20),
                    Start = (Page - 1) * Size,
                    Limited = lists:sublist(Results, Start + 1, Size),
                    {ok, Limited}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 查询用户在指定群组的@提及
%% @param Gid 群组ID
%% @param Uid 用户ID
%% @param IsRead 已读状态（true/false/undefined 表示全部）
%% @return {ok, list(map())} | {error, Reason}
-spec list_by_group_and_uid(integer(), integer(), boolean() | undefined) -> {ok, list(map())} | {error, term()}.
list_by_group_and_uid(Gid, Uid, IsRead) ->
    list_by_group_and_uid(Gid, Uid, IsRead, #{}).

%% @doc 查询用户在指定群组的@提及（支持分页）
%% @param Gid 群组ID
%% @param Uid 用户ID
%% @param IsRead 已读状态（true/false/undefined 表示全部）
%% @param Options 选项，可包含 #{page => integer(), size => integer()}
%% @return {ok, list(map())} | {error, Reason}
-spec list_by_group_and_uid(integer(), integer(), boolean() | undefined, map()) -> {ok, list(map())} | {error, term()}.
list_by_group_and_uid(Gid, Uid, IsRead, Options) ->
    case mention_repo:find_by_group_and_uid(Gid, Uid, IsRead) of
        {ok, Results} ->
            % 处理分页
            case maps:get(page, Options, undefined) of
                undefined ->
                    {ok, Results};
                Page ->
                    Size = maps:get(size, Options, 20),
                    Start = (Page - 1) * Size,
                    Limited = lists:sublist(Results, Start + 1, Size),
                    {ok, Limited}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 标记@消息为已读
%% @param MsgId 消息ID
%% @param Uid 被@的用户ID
%% @return ok | {error, Reason}
-spec mark_as_read(binary(), integer()) -> ok | {error, term()}.
mark_as_read(MsgId, Uid) ->
    mention_repo:mark_as_read(MsgId, Uid).

%% @doc 根据 mention 记录ID标记@消息为已读
%% @param MentionId mention记录ID
%% @param Uid 被@的用户ID
%% @return {ok, MsgId} | {error, Reason}
-spec mark_as_read_by_mention_id(integer(), integer()) -> {ok, binary()} | {error, term()}.
mark_as_read_by_mention_id(MentionId, Uid) ->
    case mention_repo:find_msg_id_by_mention_id(MentionId, Uid) of
        {ok, MsgId} ->
            case mention_repo:mark_as_read(MsgId, Uid) of
                ok -> {ok, MsgId};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 标记用户所有@消息为已读
%% @param Uid 用户ID
%% @return ok | {error, term()}
-spec mark_all_as_read(integer()) -> ok | {error, term()}.
mark_all_as_read(Uid) ->
    mention_repo:mark_all_as_read(Uid).

%% @doc 标记用户在指定群中的@消息为已读
%% @param Gid 群组ID
%% @param Uid 用户ID
%% @return ok | {error, term()}
-spec mark_group_as_read(integer(), integer()) -> ok | {error, term()}.
mark_group_as_read(Gid, Uid) ->
    mention_repo:mark_group_as_read(Gid, Uid).

%% @doc 统计用户未读的@消息数量
%% @param Uid 用户ID
%% @return 未读数量
-spec count_unread(integer()) -> non_neg_integer().
count_unread(Uid) ->
    mention_repo:count_unread(Uid).

%% @doc 统计用户在指定群组中的未读@消息数量
%% @param Uid 用户ID
%% @param Gid 群组ID
%% @return 未读数量
-spec count_unread_in_group(integer(), integer()) -> non_neg_integer().
count_unread_in_group(Uid, Gid) ->
    mention_repo:count_unread_in_group(Uid, Gid).

%% @doc 根据消息ID删除所有@提及记录
%% @param MsgId 消息ID
%% @return ok | {error, Reason}
-spec delete_by_msg_id(binary()) -> ok | {error, term()}.
delete_by_msg_id(MsgId) ->
    case mention_repo:delete_by_msg_id(MsgId) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% G3: mention_handler 不应直调 mention_repo
-spec find_by_msg_id(binary()) -> {ok, list(map())} | {error, term()}.
find_by_msg_id(MsgId) -> mention_repo:find_by_msg_id(MsgId).
