-module(group_notice_ds).
%%%
% group_notice 数据服务模块
% group_notice data service module
% 提供群公告相关的数据服务，封装 Repo 层操作
%%%

%% API
-export ([insert/1]).
-export ([update/2]).
-export ([find_by_id/1]).
-export ([list_by_group_id/3]).
-export ([count_by_group_id/1]).
-export ([soft_delete/1]).
-export ([pin/1]).
-export ([unpin/1]).
-export ([mark_as_read/1]).
-export ([get_pinned_notices/1]).
-export([page/5]).
-export([latest_published/2]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 插入群公告
%% @param Data 公告数据映射
%% @return {ok, NoticeId} | {error, Reason}
-spec insert(map()) -> {ok, integer()} | {error, term()}.
insert(Data) ->
    % 参数验证
    case validate_notice_data(Data) of
        {error, Reason} ->
            {error, Reason};
        ok ->
            % 检查标题和内容长度
            Title = maps:get(title, Data, <<>>),
            Body = maps:get(body, Data, <<>>),
            case validate_length(Title, 200) andalso validate_length(Body, 2000) of
                false ->
                    {error, {string_too_long, field}};
                true ->
                    case group_notice_repo:insert(Data) of
                        {ok, NoticeId} ->
                            {ok, NoticeId};
                        {error, Reason} ->
                            {error, Reason}
                    end
            end
    end.

%% @doc 更新群公告
%% @param NoticeId 公告ID
%% @param Data 要更新的数据
%% @return {ok, NoticeId} | {error, Reason}
-spec update(integer(), map()) -> {ok, integer()} | {error, term()}.
update(NoticeId, Data) when is_integer(NoticeId), NoticeId > 0 ->
    % 验证数据
    case maps:is_key(title, Data) orelse maps:is_key(body, Data) of
        true ->
            Title = maps:get(title, Data, <<>>),
            Body = maps:get(body, Data, <<>>),
            case validate_length(Title, 200) andalso validate_length(Body, 2000) of
                false ->
                    {error, {string_too_long, field}};
                true ->
                    case group_notice_repo:update(NoticeId, Data) of
                        {ok, 1} ->
                            {ok, NoticeId};
                        {ok, 0} ->
                            {error, not_found};
                        {error, Reason} ->
                            {error, Reason}
                    end
            end;
        false ->
            group_notice_repo:update(NoticeId, Data)
    end;
update(_NoticeId, _Data) ->
    {error, invalid_notice_id}.

%% @doc 根据ID查询群公告
%% @param NoticeId 公告ID
%% @return {ok, Notice} | {error, not_found}
-spec find_by_id(integer()) -> {ok, map()} | {error, not_found}.
find_by_id(NoticeId) ->
    group_notice_repo:find_by_id(NoticeId).

%% @doc 分页查询群公告列表
%% @param GroupId 群组ID
%% @param Page 页码（从1开始）
%% @param Size 每页数量
%% @return {ok, {Total, List}} | {error, Reason}
-spec list_by_group_id(integer(), integer(), integer()) ->
    {ok, {integer(), [map()]}} | {error, term()}.
list_by_group_id(GroupId, Page, Size) ->
    case group_notice_repo:list_by_group_id(GroupId, Page, Size) of
        {ok, List} ->
            case group_notice_repo:count_by_group_id(GroupId) of
                {ok, Total} ->
                    {ok, {Total, List}};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 统计群组的公告数量
%% @param GroupId 群组ID
%% @return {ok, Count} | {error, Reason}
-spec count_by_group_id(integer()) -> {ok, integer()} | {error, term()}.
count_by_group_id(GroupId) ->
    group_notice_repo:count_by_group_id(GroupId).

%% @doc 软删除群公告
%% @param NoticeId 公告ID
%% @return ok | {error, Reason}
-spec soft_delete(integer()) -> ok | {error, term()}.
soft_delete(NoticeId) ->
    case group_notice_repo:soft_delete(NoticeId) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 置顶公告
%% @param NoticeId 公告ID
%% @return ok | {error, Reason}
-spec pin(integer()) -> ok | {error, term()}.
pin(NoticeId) ->
    case group_notice_repo:pin(NoticeId) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 取消置顶公告
%% @param NoticeId 公告ID
%% @return ok | {error, Reason}
-spec unpin(integer()) -> ok | {error, term()}.
unpin(NoticeId) ->
    case group_notice_repo:unpin(NoticeId) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 标记公告为已读
%% @param NoticeId 公告ID
%% @return {ok, Notice} | {error, Reason}
-spec mark_as_read(integer()) -> {ok, map()} | {error, term()}.
mark_as_read(NoticeId) ->
    group_notice_repo:increment_read_count(NoticeId).

%% @doc 获取群组的置顶公告列表
%% @param GroupId 群组ID
%% @return {ok, [Notice]} | {error, Reason}
-spec get_pinned_notices(integer()) -> {ok, [map()]} | {error, term()}.
get_pinned_notices(GroupId) ->
    group_notice_repo:get_pinned_notices(GroupId).

%% @doc 分页查询群公告列表
%% @param Gid 群组ID
%% @param Column 查询列
%% @param Order 排序
%% @param Page 页码
%% @param Size 每页大小
%% @return {ok, map()} | {error, term()}
-spec page(integer(), binary(), binary(), pos_integer(), pos_integer()) ->
    {ok, map()} | {error, term()}.
page(Gid, Column, Order, Page, Size) ->
    Tb = group_notice_repo:tablename(),
    elib_pg:page_with_total(Tb, Column, #{group_id => Gid}, Order, Page, Size).

%% @doc 查询最新已发布公告
%% @param Gid 群组ID
%% @param Column 查询列
%% @return {ok, [map()]} | {error, term()}
-spec latest_published(integer(), binary()) -> {ok, [map()]} | {error, term()}.
latest_published(Gid, Column) ->
    Tb = group_notice_repo:tablename(),
    Sql = <<"SELECT ", Column/binary,
            " FROM ", Tb/binary,
            " WHERE status = 1 AND group_id = $1"
            " ORDER BY id desc">>,
    elib_pg:query(Sql, [Gid]).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 验证公告数据
%% @param Data 公告数据
%% @return ok | {error, Reason}
-spec validate_notice_data(map()) -> ok | {error, term()}.
validate_notice_data(Data) ->
    GroupId = maps:get(group_id, Data, undefined),
    UserId = maps:get(user_id, Data, undefined),
    case {GroupId, UserId} of
        {undefined, _} ->
            {error, {missing_field, group_id}};
        {_, undefined} ->
            {error, {missing_field, user_id}};
        {G, U} when is_integer(G), G > 0, is_integer(U), U > 0 ->
            ok;
        _ ->
            {error, invalid_param}
    end.

%% @doc 验证字符串长度
%% @param String 字符串
%% @param MaxLen 最大长度
%% @return true | false
-spec validate_length(binary(), integer()) -> boolean().
validate_length(String, MaxLen) when is_binary(String) ->
    byte_size(String) =< MaxLen;
validate_length(_, _) ->
    true.

%% ===================================================================
%% EUnit tests.
%% ===================================================================
