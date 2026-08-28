-module(group_notice_ds).
%%%
% group_notice 数据服务模块
% group_notice data service module
% 提供群公告相关的数据服务，封装 Repo 层操作
%
% T7 归档写守卫（R3 #17 收口）：全部内容写路径（insert/update/soft_delete/
% pin/unpin）经 workspace_guard:write_tx 与守卫同事务提交（group/group_notice
% → workspace 行锁），消除原 logic 层前置检查的"检查-写窗口"；
% mark_as_read（派生已读计数，R3 #18）保持 logic 层 skip 语义不在此收口。
%%%

%% API
-export([insert/1]).
-export([update/2]).
-export([find_by_id/1]).
-export([list_by_group_id/3]).
-export([count_by_group_id/1]).
-export([soft_delete/1]).
-export([pin/1]).
-export([unpin/1]).
-export([mark_as_read/1]).
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
                    %% T7 归档写守卫（R3 #17 收口）：{group, Gid} 行锁与写入同事务
                    Gid = maps:get(group_id, Data),
                    workspace_guard:write_tx({group, Gid}, fun(Conn) ->
                        group_notice_repo:insert_tx(Conn, Data)
                    end)
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
                    %% T7 归档写守卫（R3 #17 收口）：{group_notice, Id} → workspace
                    %% 行锁（FOR UPDATE）与写入同事务；{ok,1}/{ok,0} 语义保持。
                    case
                        workspace_guard:write_tx({group_notice, NoticeId}, fun(Conn) ->
                            group_notice_repo:update_tx(Conn, NoticeId, Data)
                        end)
                    of
                        {ok, 1} ->
                            {ok, NoticeId};
                        {ok, 0} ->
                            {error, not_found};
                        {error, Reason} ->
                            {error, Reason}
                    end
            end;
        false ->
            %% 无 title/body 字段的更新（publish 置状态等）：守卫同事务，结果原样
            workspace_guard:write_tx({group_notice, NoticeId}, fun(Conn) ->
                group_notice_repo:update_tx(Conn, NoticeId, Data)
            end)
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
    %% T7 归档写守卫（R3 #17 收口）：守卫与写入同事务
    notice_write_tx(NoticeId, fun(Conn) ->
        group_notice_repo:soft_delete_tx(Conn, NoticeId)
    end).

%% @doc 置顶公告
%% @param NoticeId 公告ID
%% @return ok | {error, Reason}
-spec pin(integer()) -> ok | {error, term()}.
pin(NoticeId) ->
    %% T7 归档写守卫（R3 #17 收口）：守卫与写入同事务
    notice_write_tx(NoticeId, fun(Conn) ->
        group_notice_repo:pin_tx(Conn, NoticeId)
    end).

%% @doc 取消置顶公告
%% @param NoticeId 公告ID
%% @return ok | {error, Reason}
-spec unpin(integer()) -> ok | {error, term()}.
unpin(NoticeId) ->
    %% T7 归档写守卫（R3 #17 收口）：守卫与写入同事务
    notice_write_tx(NoticeId, fun(Conn) ->
        group_notice_repo:unpin_tx(Conn, NoticeId)
    end).

%% @doc 标记公告为已读
%% @param NoticeId 公告ID
%% @return {ok, Notice} | {error, Reason}
-spec mark_as_read(integer()) -> {ok, map()} | {error, term()}.
mark_as_read(NoticeId) ->
    group_notice_repo:increment_read_count(NoticeId).

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
    Sql =
        <<"SELECT ", Column/binary, " FROM ", Tb/binary,
            " WHERE status = 1 AND group_id = $1"
            " ORDER BY id desc">>,
    elib_pg:query(Sql, [Gid]).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 按公告 ID 的守卫事务写（pin/unpin/soft_delete 共用）：
%% {group_notice, NoticeId} 解析 → workspace 行锁（FOR UPDATE）→ WriteFun(Conn)，
%% 与归档事务线性化；{ok,_}/{ok,0} 语义与原自动提交版一致，
%% 稳定错误码（980）原样透传。
-spec notice_write_tx(integer(), fun((any()) -> {ok, integer()} | {error, term()})) ->
    ok | {error, term()}.
notice_write_tx(NoticeId, WriteFun) ->
    case workspace_guard:write_tx({group_notice, NoticeId}, WriteFun) of
        {ok, 0} -> {error, not_found};
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

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
