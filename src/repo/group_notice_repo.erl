-module(group_notice_repo).
%%%
% group_notice 相关操作都放到该模块，存储库模块
% group_notice related operations are put in this module, repository module
% 群公告数据仓库层，提供群公告信息的基础数据库操作
%%%

-export ([tablename/0]).
-export ([insert/1]).
-export ([update/2]).
-export ([find_by_id/1]).
-export ([list_by_group_id/3]).
-export ([count_by_group_id/1]).
-export ([soft_delete/1]).
-export ([pin/1]).
-export ([unpin/1]).
-export ([increment_read_count/1]).
-export ([get_pinned_notices/1]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 获取群公告表的表名
%% @return 返回群公告表的完整表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"group_notice">>).

%% @doc 插入群公告
%% @param Data 公告数据映射
%% @return {ok, NoticeId} | {error, Reason}
-spec insert(map()) -> {ok, integer()} | {error, term()}.
insert(Data) ->
    Tb = tablename(),
    % 验证必填字段
    case {maps:get(group_id, Data, undefined), maps:get(user_id, Data, undefined)} of
        {undefined, _} ->
            {error, {missing_field, group_id}};
        {_, undefined} ->
            {error, {missing_field, user_id}};
        {Gid, Uid} when is_integer(Gid), is_integer(Uid) ->
            % 设置默认值
            Now = elib_dt:now(),
            Data2 = Data#{
                created_at => maps:get(created_at, Data, Now),
                status => maps:get(status, Data, 0),
                pinned => maps:get(pinned, Data, false),
                read_count => maps:get(read_count, Data, 0),
                title => maps:get(title, Data, <<>>)
            },
            Id = elib_tsid:generate(group_notice),
            Data3 = Data2#{id => Id},
            {Sql, Params} = elib_pg_sql:insert(Tb, Data3),
            case elib_pg:query(Sql, Params) of
                {ok, _Count} -> {ok, Id};
                {error, _} = Err -> Err
            end;
        _ ->
            {error, invalid_param}
    end.

%% @doc 更新群公告
%% @param NoticeId 公告ID
%% @param Data 要更新的数据
%% @return {ok, Count} | {error, Reason}
-spec update(integer(), map()) -> {ok, integer()} | {error, term()}.
update(NoticeId, Data) when is_integer(NoticeId), NoticeId > 0 ->
    Tb = tablename(),
    Where = <<"id = $1 AND deleted_at IS NULL">>,
    elib_pg:update(Tb, Data, Where, [NoticeId]);
update(_NoticeId, _Data) ->
    {error, invalid_notice_id}.

%% @doc 根据ID查询群公告
%% @param NoticeId 公告ID
%% @return {ok, Notice} | {error, not_found}
-spec find_by_id(integer()) -> {ok, map()} | {error, not_found}.
find_by_id(NoticeId) when is_integer(NoticeId), NoticeId > 0 ->
    Tb = tablename(),
    Column = <<"*">>,
    Where = <<"id = $1 AND deleted_at IS NULL">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE ", Where/binary>>,
    case elib_pg:query(Sql, [NoticeId]) of
        {ok, []} ->
            {error, not_found};
        {ok, [Notice]} ->
            {ok, Notice};
        {error, Reason} ->
            {error, Reason}
    end;
find_by_id(_NoticeId) ->
    {error, invalid_notice_id}.

%% @doc 分页查询群公告列表
%% 置顶的公告排在前面，然后按创建时间倒序
%% @param GroupId 群组ID
%% @param Page 页码（从1开始）
%% @param Size 每页数量
%% @return {ok, [Notice]} | {error, Reason}
-spec list_by_group_id(integer(), integer(), integer()) -> {ok, [map()]} | {error, term()}.
list_by_group_id(GroupId, Page, Size)
    when is_integer(GroupId), GroupId > 0,
         is_integer(Page), Page > 0,
         is_integer(Size), Size > 0, Size =< 100 ->
    Tb = tablename(),
    Column = <<"id, group_id, user_id, edit_user_id, title, body, status, ",
               "pinned, read_count, expired_at, updated_at, created_at">>,
    Where = <<"group_id = $1 AND deleted_at IS NULL">>,
    OrderBy = <<"pinned DESC, id DESC">>,  % 置顶排在前面
    Offset = (Page - 1) * Size,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary,
            " WHERE ", Where/binary,
            " ORDER BY ", OrderBy/binary,
            " LIMIT ", (integer_to_binary(Size))/binary,
            " OFFSET ", (integer_to_binary(Offset))/binary>>,
    elib_pg:query(Sql, [GroupId]);
list_by_group_id(_GroupId, _Page, _Size) ->
    {error, invalid_param}.

%% @doc 统计群组的公告数量
%% @param GroupId 群组ID
%% @return {ok, Count} | {error, Reason}
-spec count_by_group_id(integer()) -> {ok, integer()} | {error, term()}.
count_by_group_id(GroupId) when is_integer(GroupId), GroupId > 0 ->
    Tb = tablename(),
    Where = <<"group_id = $1 AND deleted_at IS NULL">>,
    Sql = <<"SELECT COUNT(*) AS count FROM ", Tb/binary, " WHERE ", Where/binary>>,
    case elib_pg:query(Sql, [GroupId]) of
        {ok, [[{<<"count">>, Count}]]} ->
            {ok, Count};
        {ok, [{#{<<"count">> := Count}}]} ->
            {ok, Count};
        {ok, [#{<<"count">> := Count}]} ->
            {ok, Count};
        {error, Reason} ->
            {error, Reason}
    end;
count_by_group_id(_GroupId) ->
    {error, invalid_param}.

%% @doc 软删除群公告
%% @param NoticeId 公告ID
%% @return {ok, Count} | {error, Reason}
-spec soft_delete(integer()) -> {ok, integer()} | {error, term()}.
soft_delete(NoticeId) when is_integer(NoticeId), NoticeId > 0 ->
    Tb = tablename(),
    Now = elib_dt:now(),
    Data = #{deleted_at => Now},
    Where = <<"id = $1 AND deleted_at IS NULL">>,
    elib_pg:update(Tb, Data, Where, [NoticeId]);
soft_delete(_NoticeId) ->
    {error, invalid_notice_id}.

%% @doc 置顶公告
%% @param NoticeId 公告ID
%% @return {ok, Count} | {error, Reason}
-spec pin(integer()) -> {ok, integer()} | {error, term()}.
pin(NoticeId) when is_integer(NoticeId), NoticeId > 0 ->
    Tb = tablename(),
    Data = #{pinned => true},
    Where = <<"id = $1 AND deleted_at IS NULL">>,
    elib_pg:update(Tb, Data, Where, [NoticeId]);
pin(_NoticeId) ->
    {error, invalid_notice_id}.

%% @doc 取消置顶公告
%% @param NoticeId 公告ID
%% @return {ok, Count} | {error, Reason}
-spec unpin(integer()) -> {ok, integer()} | {error, term()}.
unpin(NoticeId) when is_integer(NoticeId), NoticeId > 0 ->
    Tb = tablename(),
    Data = #{pinned => false},
    Where = <<"id = $1 AND deleted_at IS NULL">>,
    elib_pg:update(Tb, Data, Where, [NoticeId]);
unpin(_NoticeId) ->
    {error, invalid_notice_id}.

%% @doc 增加公告已读数
%% @param NoticeId 公告ID
%% @return {ok, Notice} | {error, Reason}
-spec increment_read_count(integer()) -> {ok, map()} | {error, term()}.
increment_read_count(NoticeId) when is_integer(NoticeId), NoticeId > 0 ->
    Tb = tablename(),
    Sql = <<"UPDATE ", Tb/binary,
            " SET read_count = read_count + 1 ",
            " WHERE id = $1 AND deleted_at IS NULL ",
            " RETURNING *">>,
    case elib_pg:query(Sql, [NoticeId]) of
        {ok, []} ->
            {error, not_found};
        {ok, [Notice]} ->
            {ok, Notice};
        {error, Reason} ->
            {error, Reason}
    end;
increment_read_count(_NoticeId) ->
    {error, invalid_notice_id}.

%% @doc 获取群组的置顶公告列表
%% @param GroupId 群组ID
%% @return {ok, [Notice]} | {error, Reason}
-spec get_pinned_notices(integer()) -> {ok, [map()]} | {error, term()}.
get_pinned_notices(GroupId) when is_integer(GroupId), GroupId > 0 ->
    Tb = tablename(),
    Column = <<"id, group_id, user_id, edit_user_id, title, body, status, ",
               "pinned, read_count, expired_at, updated_at, created_at">>,
    Where = <<"group_id = $1 AND pinned = true AND deleted_at IS NULL">>,
    OrderBy = <<"id DESC">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary,
            " WHERE ", Where/binary,
            " ORDER BY ", OrderBy/binary>>,
    elib_pg:query(Sql, [GroupId]);
get_pinned_notices(_GroupId) ->
    {error, invalid_param}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% ===================================================================
%% EUnit tests.
%% ===================================================================
