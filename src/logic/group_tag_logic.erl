-module(group_tag_logic).

%%%
% group_tag_logic 是群组标签业务逻辑层
% 提供群组标签的业务逻辑处理，包括权限验证
%%%

-include("log.hrl").

%% 导出函数
-export([add/3]).
-export([remove/3]).
-export([list/2]).
-export([search/1]).
-export([hot_tags/1]).

%% 标签名最大长度
-define(MAX_TAG_NAME_LENGTH, 50).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 添加群组标签
%% @param GroupId 群组ID
%% @param Uid 操作者用户ID
%% @param TagName 标签名称
%% @return {ok, TagId} | {error, Reason}
-spec add(integer(), integer(), binary()) -> {ok, integer()} | {error, binary()}.
add(GroupId, _Uid, _TagName) when GroupId =< 0 ->
    {error, <<"无效的群组ID"/utf8>>};
add(_GroupId, Uid, _TagName) when Uid =< 0 ->
    {error, <<"无效的用户ID"/utf8>>};
add(_GroupId, _Uid, TagName) when TagName =:= <<>> ->
    {error, <<"标签名不能为空"/utf8>>};
add(_GroupId, _Uid, TagName) when byte_size(TagName) > ?MAX_TAG_NAME_LENGTH ->
    {error, <<"标签名过长"/utf8>>};
add(GroupId, Uid, TagName) ->
    % 验证用户是否是群成员
    MemberUids = group_ds:member_uids(GroupId),
    case lists:member(Uid, MemberUids) of
        true ->
            group_tag_ds:add(GroupId, Uid, TagName);
        false ->
            {error, <<"只有群成员可以添加标签"/utf8>>}
    end.

%% @doc 删除群组标签
%% @param GroupId 群组ID
%% @param Uid 操作者用户ID
%% @param TagName 标签名称
%% @return ok | {error, Reason}
-spec remove(integer(), integer(), binary()) -> ok | {error, binary()}.
remove(GroupId, _Uid, _TagName) when GroupId =< 0 ->
    {error, <<"无效的群组ID"/utf8>>};
remove(_GroupId, Uid, _TagName) when Uid =< 0 ->
    {error, <<"无效的用户ID"/utf8>>};
remove(_GroupId, _Uid, TagName) when TagName =:= <<>> ->
    {error, <<"标签名不能为空"/utf8>>};
remove(GroupId, Uid, TagName) ->
    % 验证用户是否是群成员
    MemberUids = group_ds:member_uids(GroupId),
    case lists:member(Uid, MemberUids) of
        true ->
            case group_tag_ds:remove(GroupId, Uid, TagName) of
                ok -> ok;
                {error, Reason} -> {error, Reason}
            end;
        false ->
            {error, <<"只有群成员可以删除标签"/utf8>>}
    end.

%% @doc 查询群组的标签列表
%% @param GroupId 群组ID
%% @param Uid 查询者用户ID
%% @return {ok, [TagMap]} | {error, Reason}
-spec list(integer(), integer()) -> {ok, list(map())} | {error, binary()}.
list(GroupId, _Uid) when GroupId =< 0 ->
    {error, <<"无效的群组ID"/utf8>>};
list(_GroupId, Uid) when Uid =< 0 ->
    {error, <<"无效的用户ID"/utf8>>};
list(GroupId, Uid) ->
    % 验证用户是否是群成员
    MemberUids = group_ds:member_uids(GroupId),
    case lists:member(Uid, MemberUids) of
        true ->
            group_tag_ds:list(GroupId);
        false ->
            {error, <<"只有群成员可以查看标签"/utf8>>}
    end.

%% @doc 按标签名搜索使用该标签的群组
%% @param TagName 标签名称
%% @return {ok, [GroupTagMap]} | {error, Reason}
-spec search(binary()) -> {ok, list(map())} | {error, binary()}.
search(TagName) when TagName =:= <<>> ->
    {error, <<"标签名不能为空"/utf8>>};
search(TagName) ->
    group_tag_ds:search(TagName).

%% @doc 获取热门标签（按使用频率排序）
%% @param Limit 返回数量限制
%% @return {ok, [{tag_name, count}]} | {error, Reason}
-spec hot_tags(integer()) -> {ok, list(map())} | {error, binary()}.
hot_tags(Limit) when Limit > 0 ->
    group_tag_ds:hot_tags(Limit);
hot_tags(_) ->
    {ok, []}.
