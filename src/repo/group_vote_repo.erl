-module(group_vote_repo).
%%%
% group_vote 相关操作都放到该模块，存储库模块
% group_vote related operations are put in this module, repository module
% 群投票数据仓库层，提供群投票信息的基础数据库操作
%%%

-export([tablename/0]).

-export([insert_vote/1]).
-export([find_by_vote_id/1]).
-export([list_votes_by_group_id/3]).
-export([count_votes_by_group_id/1]).
-export([update_vote_status/2]).
-export([update_vote/2]).

-export([insert_option/1]).
-export([insert_options_batch/1]).
-export([list_options_by_vote_id/1]).
-export([delete_vote_option_by_option_id/1]).

-export([insert_record/1]).
-export([find_record_by_vote_and_user/2]).
-export([update_record/2]).
-export([delete_record/1]).

-export([count_votes_by_option_id/1]).
-export([count_total_votes_by_vote_id/1]).
-export([count_votes_grouped_by_vote_id/1]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 获取群投票表的表名
%% @return 返回群投票表的完整表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"group_vote">>).

%% @doc 获取群投票选项表的表名
-spec tablename_option() -> binary().
tablename_option() ->
    elib_pg_sql:public_tablename(<<"group_vote_option">>).

%% @doc 获取群投票记录表的表名
-spec tablename_record() -> binary().
tablename_record() ->
    elib_pg_sql:public_tablename(<<"group_vote_record">>).

%% ===================================================================
%% 群投票主表操作
%% ===================================================================

%% @doc 插入群投票
%% @param Data 投票数据映射
%% @return {ok, VoteId, Data3}（3-tuple，Data3 含完整插入数据）| {error, Reason}
%% 注意：刻意返回 3-tuple，与其他 repo 的 {ok, Id} 2-tuple 约定不同，调用方不可改为 2-tuple 匹配。
-spec insert_vote(map()) -> {ok, integer(), map()} | {error, term()}.
insert_vote(Data) ->
    Tb = tablename(),
    % 验证必填字段
    case
        {
            maps:get(group_id, Data, undefined),
            maps:get(vote_id, Data, undefined),
            maps:get(title, Data, undefined),
            maps:get(creator_id, Data, undefined)
        }
    of
        {undefined, _, _, _} ->
            {error, {missing_field, group_id}};
        {_, undefined, _, _} ->
            {error, {missing_field, vote_id}};
        {_, _, undefined, _} ->
            {error, {missing_field, title}};
        {_, _, _, undefined} ->
            {error, {missing_field, creator_id}};
        {Gid, VoteId, Title, CreatorId} when
            is_integer(Gid),
            is_binary(VoteId),
            is_binary(Title),
            is_integer(CreatorId)
        ->
            % 设置默认值
            Now = elib_dt:now(),
            Data2 = Data#{
                created_at => maps:get(created_at, Data, Now),
                updated_at => maps:get(updated_at, Data, Now),
                description => maps:get(description, Data, <<>>),
                vote_type => maps:get(vote_type, Data, 1),
                is_anonymous => maps:get(is_anonymous, Data, false),
                status => maps:get(status, Data, 1)
            },
            Id = elib_tsid:generate(group_vote),
            Data3 = Data2#{id => Id},
            {Sql, Params} = elib_pg_sql:insert(Tb, Data3),
            case elib_pg:query(Sql, Params) of
                {ok, _Count} -> {ok, Id, Data3};
                {error, _} = Err -> Err
            end;
        _ ->
            {error, invalid_param}
    end.

%% @doc 根据vote_id查询投票
%% @param VoteId 投票ID (字符串)
%% @return {ok, Vote} | {error, not_found}
-spec find_by_vote_id(binary()) -> {ok, map()} | {error, not_found | term()}.
find_by_vote_id(VoteId) when is_binary(VoteId), byte_size(VoteId) > 0 ->
    Tb = tablename(),
    Column = <<"*">>,
    Where = <<"vote_id = $1">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE ", Where/binary>>,
    case elib_pg:query(Sql, [VoteId]) of
        {ok, []} ->
            {error, not_found};
        {ok, [Vote]} ->
            {ok, Vote};
        {error, Reason} ->
            {error, Reason}
    end;
find_by_vote_id(_VoteId) ->
    {error, invalid_vote_id}.

%% @doc 分页查询群投票列表
%% @param GroupId 群组ID
%% @param Page 页码（从1开始）
%% @param Size 每页数量
%% @return {ok, [Vote]} | {error, Reason}
-spec list_votes_by_group_id(integer(), integer(), integer()) ->
    {ok, [map()]} | {error, term()}.
list_votes_by_group_id(GroupId, Page, Size) when
    is_integer(GroupId),
    GroupId > 0,
    is_integer(Page),
    Page > 0,
    is_integer(Size),
    Size > 0
->
    Tb = tablename(),
    RecordTb = tablename_record(),
    Offset = (Page - 1) * Size,
    % participant_count: 子查询一次聚合去重参与人数（distinct user_id，
    % 多选投票下 ≠ 总票数），避免逐项 count 的 N+1。
    Column =
        <<
            "t.id, t.group_id, t.vote_id, t.title, t.description, t.creator_id, t.vote_type, t.is_anonymous, t.status, t.end_at, t.created_at, "
            "(SELECT COUNT(DISTINCT r.user_id) FROM ",
            RecordTb/binary,
            " r WHERE r.vote_id = t.vote_id) AS participant_count"
        >>,
    Where = <<"t.group_id = $1">>,
    Order = <<"t.created_at DESC">>,
    Sql =
        <<"SELECT ", Column/binary, " FROM ", Tb/binary, " t WHERE ", Where/binary, " ORDER BY ",
            Order/binary, " LIMIT $2 OFFSET $3">>,
    elib_pg:query(Sql, [GroupId, Size, Offset]);
list_votes_by_group_id(_GroupId, _Page, _Size) ->
    {error, invalid_param}.

%% @doc 统计群投票数量
%% @param GroupId 群组ID
%% @return {ok, Count} | {error, Reason}
-spec count_votes_by_group_id(integer()) -> {ok, integer()} | {error, term()}.
count_votes_by_group_id(GroupId) when is_integer(GroupId), GroupId > 0 ->
    Tb = tablename(),
    Sql = <<"SELECT COUNT(*) AS count FROM ", Tb/binary, " WHERE group_id = $1">>,
    case elib_pg:query(Sql, [GroupId]) of
        {ok, [#{<<"count">> := Count}]} ->
            {ok, Count};
        {error, Reason} ->
            {error, Reason}
    end;
count_votes_by_group_id(_GroupId) ->
    {error, invalid_param}.

%% @doc 更新投票状态
%% @param VoteId 投票ID (字符串)
%% @param Status 新状态 (1=进行中, 2=已结束, 3=已取消)
%% @return {ok, Count} | {error, Reason}
-spec update_vote_status(binary(), integer()) -> {ok, integer()} | {error, term()}.
update_vote_status(VoteId, Status) when
    is_binary(VoteId), byte_size(VoteId) > 0, is_integer(Status), Status > 0
->
    Tb = tablename(),
    Where = <<"vote_id = $1">>,
    Data = #{status => Status, updated_at => elib_dt:now()},
    elib_pg:update(Tb, Data, Where, [VoteId]);
update_vote_status(_VoteId, _Status) ->
    {error, invalid_param}.

%% @doc 更新投票信息
%% @param VoteId 投票ID (字符串)
%% @param Data 要更新的数据
%% @return {ok, Count} | {error, Reason}
-spec update_vote(binary(), map()) -> {ok, integer()} | {error, term()}.
update_vote(VoteId, Data) when is_binary(VoteId), byte_size(VoteId) > 0, is_map(Data) ->
    Tb = tablename(),
    Where = <<"vote_id = $1">>,
    Data2 = Data#{updated_at => elib_dt:now()},
    elib_pg:update(Tb, Data2, Where, [VoteId]);
update_vote(_VoteId, _Data) ->
    {error, invalid_param}.

%% ===================================================================
%% 群投票选项表操作
%% ===================================================================

%% @doc 插入投票选项
%% @param Data 选项数据映射
%% @return {ok, OptionId, InsertResult} | {error, Reason}
-spec insert_option(map()) -> {ok, integer(), map()} | {error, term()}.
insert_option(Data) ->
    Tb = tablename_option(),
    % 验证必填字段
    case
        {
            maps:get(vote_id, Data, undefined),
            maps:get(option_id, Data, undefined),
            maps:get(option_text, Data, undefined)
        }
    of
        {undefined, _, _} ->
            {error, {missing_field, vote_id}};
        {_, undefined, _} ->
            {error, {missing_field, option_id}};
        {_, _, undefined} ->
            {error, {missing_field, option_text}};
        {VoteId, OptionId, OptionText} when
            is_binary(VoteId),
            is_binary(OptionId),
            is_binary(OptionText)
        ->
            Now = elib_dt:now(),
            Data2 = Data#{
                created_at => maps:get(created_at, Data, Now),
                sort_order => maps:get(sort_order, Data, 0)
            },
            Id = elib_tsid:generate(group_vote_option),
            Data3 = Data2#{id => Id},
            {Sql, Params} = elib_pg_sql:insert(Tb, Data3),
            case elib_pg:query(Sql, Params) of
                {ok, _Count} -> {ok, Id, Data3};
                {error, _} = Err -> Err
            end;
        _ ->
            {error, invalid_param}
    end.

%% @doc 批量插入投票选项
%% @param Options 选项列表
%% @return {ok, Count} | {error, Reason}
-spec insert_options_batch([map()]) -> {ok, integer()} | {error, term()}.
insert_options_batch(Options) when is_list(Options), length(Options) > 0 ->
    Tb = tablename_option(),
    Now = elib_dt:now(),

    % 构建批量插入SQL
    % id 列 NOT NULL 且无数据库默认值，必须逐行生成 TSID（与单条
    % insert_option/1 一致）；此前遗漏 id 导致批量插入必崩 23502。
    {ValuesList, Params} = lists:foldl(
        fun(Option, {ValsAcc, ParamsAcc}) ->
            Id = elib_tsid:generate(group_vote_option),
            VoteId = maps:get(vote_id, Option),
            OptionId = maps:get(option_id, Option),
            OptionText = maps:get(option_text, Option),
            SortOrder = maps:get(sort_order, Option, 0),
            Placeholder = io_lib:format(
                "($~p, $~p, $~p, $~p, $~p, $~p)",
                [
                    length(ParamsAcc) + 1,
                    length(ParamsAcc) + 2,
                    length(ParamsAcc) + 3,
                    length(ParamsAcc) + 4,
                    length(ParamsAcc) + 5,
                    length(ParamsAcc) + 6
                ]
            ),
            {
                [Placeholder | ValsAcc],
                ParamsAcc ++ [Id, VoteId, OptionId, OptionText, SortOrder, Now]
            }
        end,
        {[], []},
        Options
    ),

    ValuesStr = lists:join(<<",">>, lists:reverse(ValuesList)),
    Sql =
        <<"INSERT INTO ", Tb/binary,
            " (id, vote_id, option_id, option_text, sort_order, created_at) VALUES ",
            (iolist_to_binary(ValuesStr))/binary>>,

    case elib_pg:query(Sql, Params) of
        {ok, Count} -> {ok, Count};
        {error, Reason} -> {error, Reason}
    end;
insert_options_batch(_Options) ->
    {error, invalid_param}.

%% @doc 查询投票选项列表
%% @param VoteId 投票ID (字符串)
%% @return {ok, [Option]} | {error, Reason}
-spec list_options_by_vote_id(binary()) -> {ok, [map()]} | {error, term()}.
list_options_by_vote_id(VoteId) when is_binary(VoteId), byte_size(VoteId) > 0 ->
    Tb = tablename_option(),
    Column = <<"id, vote_id, option_id, option_text, sort_order, created_at">>,
    Where = <<"vote_id = $1">>,
    Order = <<"sort_order ASC">>,
    Sql =
        <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE ", Where/binary, " ORDER BY ",
            Order/binary>>,
    case elib_pg:query(Sql, [VoteId]) of
        {ok, Options} ->
            {ok, Options};
        {error, Reason} ->
            {error, Reason}
    end;
list_options_by_vote_id(_VoteId) ->
    {error, invalid_param}.

%% @doc 删除投票选项
%% @param OptionId 选项ID (字符串)
%% @return {ok, Count} | {error, Reason}
-spec delete_vote_option_by_option_id(binary()) -> {ok, integer()} | {error, term()}.
delete_vote_option_by_option_id(OptionId) when is_binary(OptionId), byte_size(OptionId) > 0 ->
    Tb = tablename_option(),
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE option_id = $1">>,
    case elib_pg:query(Sql, [OptionId]) of
        {ok, Count} -> {ok, Count};
        {error, Reason} -> {error, Reason}
    end;
delete_vote_option_by_option_id(_OptionId) ->
    {error, invalid_param}.

%% ===================================================================
%% 群投票记录表操作
%% ===================================================================

%% @doc 插入投票记录
%% @param Data 记录数据映射
%% @return {ok, RecordId, Data3}（3-tuple，Data3 含完整插入数据）| {error, Reason}
%% 注意：刻意返回 3-tuple，与其他 repo 的 {ok, Id} 2-tuple 约定不同，调用方不可改为 2-tuple 匹配。
-spec insert_record(map()) -> {ok, integer(), map()} | {error, term()}.
insert_record(Data) ->
    Tb = tablename_record(),
    % 验证必填字段
    case
        {
            maps:get(vote_id, Data, undefined),
            maps:get(user_id, Data, undefined),
            maps:get(option_ids, Data, undefined)
        }
    of
        {undefined, _, _} ->
            {error, {missing_field, vote_id}};
        {_, undefined, _} ->
            {error, {missing_field, user_id}};
        {_, _, undefined} ->
            {error, {missing_field, option_ids}};
        {VoteId, UserId, _OptionIds} when is_binary(VoteId), is_integer(UserId) ->
            Now = elib_dt:now(),
            Data2 = Data#{created_at => maps:get(created_at, Data, Now)},
            Id = elib_tsid:generate(group_vote),
            Data3 = Data2#{id => Id},
            {Sql, Params} = elib_pg_sql:insert(Tb, Data3),
            case elib_pg:query(Sql, Params) of
                {ok, _Count} -> {ok, Id, Data3};
                {error, _} = Err -> Err
            end;
        _ ->
            {error, invalid_param}
    end.

%% @doc 查询用户在指定投票的记录
%% @param VoteId 投票ID (字符串)
%% @param UserId 用户ID (整数)
%% @return {ok, Record} | {error, not_found}
-spec find_record_by_vote_and_user(binary(), integer()) ->
    {ok, map()} | {error, not_found | term()}.
find_record_by_vote_and_user(VoteId, UserId) when
    is_binary(VoteId), byte_size(VoteId) > 0, is_integer(UserId), UserId > 0
->
    Tb = tablename_record(),
    Column = <<"*">>,
    Where = <<"vote_id = $1 AND user_id = $2">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE ", Where/binary>>,
    case elib_pg:query(Sql, [VoteId, UserId]) of
        {ok, []} ->
            {error, not_found};
        {ok, [Record]} ->
            {ok, Record};
        {error, Reason} ->
            {error, Reason}
    end;
find_record_by_vote_and_user(_VoteId, _UserId) ->
    {error, invalid_param}.

%% @doc 更新投票记录
%% @param RecordId 记录ID (整数)
%% @param Data 要更新的数据
%% @return {ok, Count} | {error, Reason}
-spec update_record(integer(), map()) -> {ok, integer()} | {error, term()}.
update_record(RecordId, Data) when is_integer(RecordId), RecordId > 0, is_map(Data) ->
    Tb = tablename_record(),
    Where = <<"id = $1">>,
    elib_pg:update(Tb, Data, Where, [RecordId]);
update_record(_RecordId, _Data) ->
    {error, invalid_param}.

%% @doc 删除投票记录
%% @param RecordId 记录ID (整数)
%% @return {ok, Count} | {error, Reason}
-spec delete_record(integer()) -> {ok, integer()} | {error, term()}.
delete_record(RecordId) when is_integer(RecordId), RecordId > 0 ->
    Tb = tablename_record(),
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE id = $1">>,
    case elib_pg:query(Sql, [RecordId]) of
        {ok, Count} -> {ok, Count};
        {error, Reason} -> {error, Reason}
    end;
delete_record(_RecordId) ->
    {error, invalid_param}.

%% ===================================================================
%% 统计相关操作
%% ===================================================================

%% @doc 统计选项得票数
%% @param OptionId 选项ID (字符串)
%% @return {ok, Count} | {error, Reason}
-spec count_votes_by_option_id(binary()) -> {ok, integer()} | {error, term()}.
count_votes_by_option_id(OptionId) when is_binary(OptionId), byte_size(OptionId) > 0 ->
    Tb = tablename_record(),
    % option_ids 是 JSON 数组，使用 PostgreSQL 的 JSON 操作符
    Sql = <<"SELECT COUNT(*) AS count FROM ", Tb/binary, " WHERE option_ids::jsonb ? $1">>,
    case elib_pg:query(Sql, [OptionId]) of
        {ok, [#{<<"count">> := Count}]} ->
            {ok, Count};
        {error, Reason} ->
            {error, Reason}
    end;
count_votes_by_option_id(_OptionId) ->
    {error, invalid_param}.

%% @doc 统计投票总人数
%% @param VoteId 投票ID (字符串)
%% @return {ok, Count} | {error, Reason}
-spec count_total_votes_by_vote_id(binary()) -> {ok, integer()} | {error, term()}.
count_total_votes_by_vote_id(VoteId) when is_binary(VoteId), byte_size(VoteId) > 0 ->
    Tb = tablename_record(),
    Sql = <<"SELECT COUNT(*) AS count FROM ", Tb/binary, " WHERE vote_id = $1">>,
    case elib_pg:query(Sql, [VoteId]) of
        {ok, [#{<<"count">> := Count}]} ->
            {ok, Count};
        {error, Reason} ->
            {error, Reason}
    end;
count_total_votes_by_vote_id(_VoteId) ->
    {error, invalid_param}.

%% @doc 一次性统计某投票下所有选项的得票数（避免逐选项 N+1 查询）
%% option_ids 是 jsonb 数组（允许多选），用子查询 unnest 后按 option_id 聚合，
%% 用 LEFT JOIN 保证零票选项也会出现在结果中
%% @param VoteId 投票ID (字符串)
%% @return {ok, [#{<<"option_id">> => binary(), <<"vote_count">> => integer()}]} | {error, Reason}
-spec count_votes_grouped_by_vote_id(binary()) -> {ok, [map()]} | {error, term()}.
count_votes_grouped_by_vote_id(VoteId) when is_binary(VoteId), byte_size(VoteId) > 0 ->
    OptTb = tablename_option(),
    RecTb = tablename_record(),
    Sql = <<
        "SELECT o.option_id AS option_id, COUNT(v.chosen_option_id) AS vote_count",
        " FROM ",
        OptTb/binary,
        " o",
        " LEFT JOIN (",
        "   SELECT jsonb_array_elements_text(option_ids) AS chosen_option_id",
        "   FROM ",
        RecTb/binary,
        "   WHERE vote_id = $1",
        " ) v ON v.chosen_option_id = o.option_id",
        " WHERE o.vote_id = $1",
        " GROUP BY o.option_id"
    >>,
    case elib_pg:query(Sql, [VoteId]) of
        {ok, Rows} ->
            {ok, Rows};
        {error, Reason} ->
            {error, Reason}
    end;
count_votes_grouped_by_vote_id(_VoteId) ->
    {error, invalid_param}.
