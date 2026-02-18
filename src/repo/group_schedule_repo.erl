-module(group_schedule_repo).
%%%
% group_schedule_repo 是群组日程数据仓库层
% 提供群组日程、参与人、提醒的数据访问操作
%%%

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%% 日程表操作
-export([tablename/0]).
-export([insert/1]).
-export([update/2]).
-export([find_by_id/1]).
-export([find_by_id/2]).
-export([find_by_schedule_id/1]).
-export([find_by_schedule_id/2]).
-export([list_by_group_id/3]).
-export([list_by_group_id/4]).
-export([list_by_user_id/3]).
-export([list_by_user_id/4]).
-export([update_status/2]).
-export([count_by_group_id/1]).

%% 参与人表操作
-export([participant_tablename/0]).
-export([insert_participant/1]).
-export([update_participant_status/3]).
-export([list_participants/1]).
-export([list_participants/2]).
-export([count_participants/1]).
-export([delete_participant/2]).

%% 提醒表操作
-export([remind_tablename/0]).
-export([insert_remind/1]).
-export([list_pending_reminds/0]).
-export([list_pending_reminds/1]).
-export([update_remind_sent/1]).
-export([delete_remind/1]).

%% ===================================================================
%% API functions - 日程表
%% ===================================================================

%% @doc 获取日程表的表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"group_schedule">>).

%% @doc 插入新日程
-spec insert(map()) -> {ok, integer(), map()} | {error, term()}.
insert(Data) ->
    Tb = tablename(),
    % 验证必填字段
    case validate_schedule_data(Data) of
        ok ->
            elib_pg:parse_result(elib_pg:insert(Tb, Data, <<"RETURNING id">>));
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 更新日程
-spec update(integer(), map()) -> {ok, non_neg_integer()} | {error, term()}.
update(Id, Data) ->
    Tb = tablename(),
    UpdateData = maps:without([<<"id">>], Data),
    UpdateData2 = UpdateData#{<<"updated_at">> => elib_dt:now()},
    elib_pg:update(Tb, UpdateData2, <<"id = $1">>, [Id]).

%% @doc 根据ID查询日程（默认查询所有字段）
-spec find_by_id(integer()) -> map() | {error, term()}.
find_by_id(Id) ->
    find_by_id(Id, <<"*">>).

%% @doc 根据ID查询日程
-spec find_by_id(integer(), binary()) -> map() | {error, term()}.
find_by_id(Id, Column) when is_list(Id); is_binary(Id) ->
    find_by_id(ec_cnv:to_integer(Id), Column);
find_by_id(Id, Column) when Id > 0 ->
    Tb = tablename(),
    {Sql, Params} = elib_pg_sql:build_select(Tb, Column, #{id => Id}, #{limit => 1}),
    case elib_pg:one(Sql, Params) of
        {ok, Row} -> Row;
        {error, Reason} -> {error, Reason}
    end;
find_by_id(_, _) ->
    {error, invalid_id}.

%% @doc 根据schedule_id查询日程（默认查询所有字段）
-spec find_by_schedule_id(binary()) -> map() | {error, term()}.
find_by_schedule_id(ScheduleId) ->
    find_by_schedule_id(ScheduleId, <<"*">>).

%% @doc 根据schedule_id查询日程
-spec find_by_schedule_id(binary(), binary()) -> map() | {error, term()}.
find_by_schedule_id(ScheduleId, Column) when is_binary(ScheduleId) ->
    Tb = tablename(),
    {Sql, Params} = elib_pg_sql:build_select(Tb, Column, #{schedule_id => ScheduleId}, #{limit => 1}),
    case elib_pg:one(Sql, Params) of
        {ok, Row} -> Row;
        {error, Reason} -> {error, Reason}
    end;
find_by_schedule_id(_, _) ->
    {error, invalid_schedule_id}.

%% @doc 查询群组的日程列表（默认字段）
-spec list_by_group_id(integer(), integer(), integer()) -> {ok, list(map())} | {error, term()}.
list_by_group_id(GroupId, Page, Size) ->
    list_by_group_id(GroupId, <<"id,schedule_id,group_id,title,description,location,creator_id,start_at,end_at,remind_before,status,created_at">>, Page, Size).

%% @doc 查询群组的日程列表
-spec list_by_group_id(integer(), binary(), integer(), integer()) -> {ok, list(map())} | {error, term()}.
list_by_group_id(GroupId, Column, Page, Size) when GroupId > 0 ->
    Tb = tablename(),
    Where = #{group_id => GroupId},
    OrderBy = <<"start_at ASC, id DESC">>,
    {Sql, Params} = elib_pg_sql:build_select(Tb, Column, Where, #{order_by => OrderBy, limit => Size, offset => (Page - 1) * Size}),
    elib_pg:query(Sql, Params);
list_by_group_id(_, _, _, _) ->
    {ok, []}.

%% @doc 查询用户参与的日程列表（默认字段）
-spec list_by_user_id(integer(), integer(), integer()) -> {ok, list(map())} | {error, term()}.
list_by_user_id(UserId, Page, Size) ->
    list_by_user_id(UserId, <<"gs.id,gs.schedule_id,gs.group_id,gs.title,gs.start_at,gs.end_at,gs.status">>, Page, Size).

%% @doc 查询用户参与的日程列表
-spec list_by_user_id(integer(), binary(), integer(), integer()) -> {ok, list(map())} | {error, term()}.
list_by_user_id(UserId, Column, Page, Size) when UserId > 0 ->
    Tb = <<(tablename())/binary, " gs">>,
    PTb = participant_tablename(),
    JoinClause = <<" INNER JOIN ", (PTb)/binary, " p ON gs.schedule_id = p.schedule_id">>,
    Where = #{<<"p.user_id">> => UserId},
    OrderBy = <<"gs.start_at ASC">>,
    {Sql, Params} = elib_pg_sql:build_select(Tb, Column, Where, #{order_by => OrderBy, limit => Size, offset => (Page - 1) * Size}),
    FinalSql = <<Sql/binary, JoinClause/binary>>,
    elib_pg:query(FinalSql, Params);
list_by_user_id(_, _, _, _) ->
    {ok, []}.

%% @doc 更新日程状态
-spec update_status(integer(), integer()) -> {ok, non_neg_integer()} | {error, term()}.
update_status(Id, Status) when is_integer(Status), Status >= 1, Status =< 4 ->
    Tb = tablename(),
    Data = #{status => Status, updated_at => elib_dt:now()},
    elib_pg:update(Tb, Data, <<"id = $1">>, [Id]);
update_status(_, _) ->
    {error, invalid_status}.

%% @doc 统计群组的日程数量
-spec count_by_group_id(integer()) -> {ok, non_neg_integer()} | {error, term()}.
count_by_group_id(GroupId) when GroupId > 0 ->
    Tb = tablename(),
    Sql = <<"SELECT COUNT(*) as count FROM ", Tb/binary, " WHERE group_id = $1 AND status != 4">>,
    case elib_pg:one(Sql, [GroupId]) of
        {ok, #{<<"count">> := Count}} -> {ok, Count};
        {error, Reason} -> {error, Reason}
    end;
count_by_group_id(_) ->
    {ok, 0}.

%% ===================================================================
%% API functions - 参与人表
%% ===================================================================

%% @doc 获取参与人表的表名
-spec participant_tablename() -> binary().
participant_tablename() ->
    elib_pg_sql:public_tablename(<<"group_schedule_participant">>).

%% @doc 插入参与人
-spec insert_participant(map()) -> {ok, integer(), map()} | {error, term()}.
insert_participant(Data) ->
    Tb = participant_tablename(),
    elib_pg:parse_result(elib_pg:insert(Tb, Data, <<"RETURNING id">>)).

%% @doc 更新参与人状态
-spec update_participant_status(binary(), integer(), integer()) -> {ok, non_neg_integer()} | {error, term()}.
update_participant_status(ScheduleId, UserId, Status) when is_integer(Status), Status >= 0, Status =< 2 ->
    Tb = participant_tablename(),
    Data = #{status => Status, updated_at => elib_dt:now()},
    elib_pg:update(Tb, Data, <<"schedule_id = $1 AND user_id = $2">>, [ScheduleId, UserId]);
update_participant_status(_, _, _) ->
    {error, invalid_status}.

%% @doc 查询日程的参与人列表（默认字段）
-spec list_participants(binary()) -> {ok, list(map())} | {error, term()}.
list_participants(ScheduleId) ->
    list_participants(ScheduleId, <<"id,schedule_id,user_id,status,created_at">>).

%% @doc 查询日程的参与人列表
-spec list_participants(binary(), binary()) -> {ok, list(map())} | {error, term()}.
list_participants(ScheduleId, Column) when is_binary(ScheduleId) ->
    Tb = participant_tablename(),
    {Sql, Params} = elib_pg_sql:build_select(Tb, Column, #{schedule_id => ScheduleId}, #{order_by => <<"id ASC">>}),
    elib_pg:query(Sql, Params);
list_participants(_, _) ->
    {ok, []}.

%% @doc 统计日程参与人数量
-spec count_participants(binary()) -> {ok, non_neg_integer()} | {error, term()}.
count_participants(ScheduleId) when is_binary(ScheduleId) ->
    Tb = participant_tablename(),
    Sql = <<"SELECT COUNT(*) as count FROM ", Tb/binary, " WHERE schedule_id = $1">>,
    case elib_pg:one(Sql, [ScheduleId]) of
        {ok, #{<<"count">> := Count}} -> {ok, Count};
        {error, Reason} -> {error, Reason}
    end;
count_participants(_) ->
    {ok, 0}.

%% @doc 删除参与人
-spec delete_participant(binary(), integer()) -> {ok, non_neg_integer()} | {error, term()}.
delete_participant(ScheduleId, UserId) ->
    Tb = participant_tablename(),
    elib_pg:delete(Tb, <<"schedule_id = $1 AND user_id = $2">>, [ScheduleId, UserId]).

%% ===================================================================
%% API functions - 提醒表
%% ===================================================================

%% @doc 获取提醒表的表名
-spec remind_tablename() -> binary().
remind_tablename() ->
    elib_pg_sql:public_tablename(<<"group_schedule_remind">>).

%% @doc 插入提醒记录
-spec insert_remind(map()) -> {ok, integer(), map()} | {error, term()}.
insert_remind(Data) ->
    Tb = remind_tablename(),
    elib_pg:parse_result(elib_pg:insert(Tb, Data, <<"RETURNING id">>)).

%% @doc 查询待发送的提醒列表（默认字段）
-spec list_pending_reminds() -> {ok, list(map())} | {error, term()}.
list_pending_reminds() ->
    list_pending_reminds(<<"id,schedule_id,user_id,remind_at,created_at">>).

%% @doc 查询待发送的提醒列表
-spec list_pending_reminds(binary()) -> {ok, list(map())} | {error, term()}.
list_pending_reminds(Column) ->
    Tb = remind_tablename(),
    Now = elib_dt:now(),
    Where = #{is_sent => false, remind_at => {op, <<"<=">>, Now}},
    {Sql, Params} = elib_pg_sql:build_select(Tb, Column, Where, #{order_by => <<"remind_at ASC">>, limit => 100}),
    elib_pg:query(Sql, Params).

%% @doc 更新提醒为已发送
-spec update_remind_sent(integer()) -> {ok, non_neg_integer()} | {error, term()}.
update_remind_sent(Id) when Id > 0 ->
    Tb = remind_tablename(),
    Data = #{is_sent => true},
    elib_pg:update(Tb, Data, <<"id = $1">>, [Id]);
update_remind_sent(_) ->
    {error, invalid_id}.

%% @doc 删除提醒记录
-spec delete_remind(integer()) -> {ok, non_neg_integer()} | {error, term()}.
delete_remind(Id) ->
    Tb = remind_tablename(),
    elib_pg:delete(Tb, <<"id = $1">>, [Id]).

%% ===================================================================
%% Internal functions
%% ===================================================================

%% @doc 验证日程数据
-spec validate_schedule_data(map()) -> ok | {error, term()}.
validate_schedule_data(Data) ->
    case maps:get(<<"title">>, Data, <<>>) of
        <<>> -> {error, {missing_field, title}};
        Title when byte_size(Title) > 200 -> {error, {field_too_long, title}};
        _ ->
            validate_time_range(Data)
    end.

%% @doc 验证时间范围
-spec validate_time_range(map()) -> ok | {error, term()}.
validate_time_range(Data) ->
    StartAt = maps:get(<<"start_at">>, Data),
    EndAt = maps:get(<<"end_at">>, Data),
    case {StartAt, EndAt} of
        {undefined, _} -> {error, {missing_field, start_at}};
        {_, undefined} -> {error, {missing_field, end_at}};
        {_, _} ->
            try
                StartTs = elib_dt:rfc3339_to(StartAt),
                EndTs = elib_dt:rfc3339_to(EndAt),
                case StartTs < EndTs of
                    true -> ok;
                    false -> {error, {invalid_time_range, start_at, end_at}}
                end
            catch
                _:_ -> {error, invalid_datetime_format}
            end
    end.

%% ===================================================================
%% EUnit tests
%% ===================================================================
