-module(mention_logic).
%%%
% mention_logic 是 @提及业务逻辑层
% 提供 @提及功能的业务逻辑
%%%
-include("log.hrl").
-include("error_code.hrl").

-export([create_mentions/4]).
-export([list_mentions/3]).
-export([list_group_mentions/4]).
-export([mark_as_read/2]).
-export([count_unread/1]).
-export([get_member_suggestions/3]).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 创建@提及记录
%% @param MsgId 消息ID
%% @param Gid 群组ID
%% @param Mentions 被@的用户列表（hashids编码），可以是 <<"all">> 表示@所有人
%% @param FromUid 发送者ID
%% @return ok | {error, Reason}
-spec create_mentions(binary(), integer(), list(binary) | binary(), integer()) -> ok | {error, term()}.
create_mentions(_MsgId, _Gid, Mentions, _FromUid) when not is_list(Mentions) ->
    {error, invalid_mentions_format};
create_mentions(_MsgId, _Gid, [], _FromUid) ->
    ok;
create_mentions(MsgId, Gid, Mentions, FromUid) ->
    % 检查是否@所有人
    case lists:member(<<"all">>, Mentions) of
        true ->
            % @所有人需要管理员权限
            case group_member_ds:check_admin(FromUid, Gid) of
                true ->
                    mention_ds:save_mentions(MsgId, Gid, Mentions, FromUid);
                false ->
                    _ = ?WARN_LOG("用户 ~p 尝试 @所有人但没有管理员权限", [FromUid]),
                    {error, permission_denied}
            end;
        false ->
            % @特定用户，不需要特殊权限
            mention_ds:save_mentions(MsgId, Gid, Mentions, FromUid)
    end.

%% @doc 查询用户的所有@提及（支持已读/未读过滤和分页）
%% @param Uid 用户ID
%% @param IsRead 已读状态（true/false/undefined 表示全部）
%% @param Options 选项，可包含 #{page => integer(), size => integer()}
%% @return {ok, list(map())} | {error, Reason}
-spec list_mentions(integer(), boolean() | undefined, map()) -> {ok, list(map())} | {error, term()}.
list_mentions(Uid, IsRead, Options) ->
    mention_ds:list_by_uid(Uid, IsRead, Options).

%% @doc 查询用户在指定群组的@提及（支持已读/未读过滤和分页）
%% @param Gid 群组ID
%% @param Uid 用户ID
%% @param IsRead 已读状态（true/false/undefined 表示全部）
%% @param Options 选项，可包含 #{page => integer(), size => integer()}
%% @return {ok, list(map())} | {error, Reason}
-spec list_group_mentions(integer(), integer(), boolean() | undefined, map()) -> {ok, list(map())} | {error, term()}.
list_group_mentions(Gid, Uid, IsRead, Options) ->
    mention_ds:list_by_group_and_uid(Gid, Uid, IsRead, Options).

%% @doc 标记@消息为已读
%% @param MsgId 消息ID
%% @param Uid 被@的用户ID
%% @return ok | {error, Reason}
-spec mark_as_read(binary(), integer()) -> ok | {error, term()}.
mark_as_read(MsgId, Uid) ->
    mention_ds:mark_as_read(MsgId, Uid).

%% @doc 统计用户未读的@消息数量
%% @param Uid 用户ID
%% @return 未读数量
-spec count_unread(integer()) -> non_neg_integer().
count_unread(Uid) ->
    mention_ds:count_unread(Uid).

%% @doc 获取群组成员建议列表（用于@输入）
%% @param Gid 群组ID
%% @param Uid 当前用户ID
%% @param Keyword 搜索关键字（昵称或账号）
%% @return {ok, list(map())} | {error, Reason}
-spec get_member_suggestions(integer(), integer(), binary()) -> {ok, list(map())} | {error, term()}.
get_member_suggestions(Gid, Uid, Keyword) ->
    % 验证用户是否是群成员
    case group_ds:is_member(Uid, Gid) of
        false ->
            {error, not_group_member};
        true ->
            % 获取群组所有成员
            MemberUids = group_ds:member_uids(Gid),
            % 查询用户信息
            Column = <<"id, account, nickname, avatar">>,
            case user_repo:list_by_ids(MemberUids, Column) of
                {ok, Users} ->
                    % 根据关键字过滤
                    FilteredUsers = filter_users_by_keyword(Users, Keyword),
                    % 编码ID并返回
                    {ok, encode_user_ids(FilteredUsers)};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

%% @private
%% @doc 根据关键字过滤用户列表
-spec filter_users_by_keyword(list(map()), binary()) -> list(map()).
filter_users_by_keyword(Users, <<>>) ->
    Users;
filter_users_by_keyword(Users, Keyword) ->
    KeywordLower = elib_string:to_lower(Keyword),
    lists:filter(fun(User) ->
        Nickname = maps:get(<<"nickname">>, User, <<>>),
        Account = maps:get(<<"account">>, User, <<>>),
        NicknameLower = elib_string:to_lower(Nickname),
        AccountLower = elib_string:to_lower(Account),
        % 检查昵称或账号是否包含关键字
        binary:match(NicknameLower, KeywordLower) =/= nomatch orelse
        binary:match(AccountLower, KeywordLower) =/= nomatch
    end, Users).

%% @private
%% @doc 编码用户ID
-spec encode_user_ids(list(map())) -> list(map()).
encode_user_ids(Users) ->
    lists:map(fun(User) ->
        UserId = maps:get(<<"id">>, User),
        UserIdEncoded = elib_hashids:encode(UserId),
        User#{<<"id">> => UserIdEncoded}
    end, Users).
