-module(msg_read_repo).
%%%
% msg_read_repo 是消息已读回执数据仓库层
% 提供消息已读状态的基础数据库操作
%%%

-include("log.hrl").

-export([tablename/0]).
-export([save_read/5]).
-export([get_read_status/2]).
-export([get_unread_count/2]).
-export([mark_messages_read/3]).
-export([delete_read_records/2]).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 获取消息已读表的表名
%% @return 返回消息已读表的完整表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"msg_read">>).


%% @doc 保存消息已读记录
%% 使用 INSERT ... ON CONFLICT DO NOTHING 确保幂等性
%% @param MsgId 消息ID
%% @param FromUid 发送者用户ID（整数）
%% @param ToUid 接收者用户ID（整数）
%% @param ToDid 接收者设备ID（binary）
%% @param ReadAt 已读时间（RFC3339 binary）
%% @return ok | {error, Reason}
-spec save_read(binary(), integer(), integer(), binary(), binary()) -> ok | {error, term()}.
save_read(MsgId, FromUid, ToUid, ToDid, ReadAt) ->
    Tb = tablename(),
    GenId = elib_tsid:generate(msg_read),
    Sql = [
        <<"INSERT INTO ">>, Tb,
        <<" (id, msg_id, from_uid, to_uid, to_did, read_at, created_at)">>,
        <<" VALUES ($1, $2, $3, $4, $5, $6, $6)">>,
        <<" ON CONFLICT (msg_id, to_uid, to_did) DO NOTHING">>
    ],
    case elib_pg:query(Sql, [GenId, MsgId, FromUid, ToUid, ToDid, ReadAt]) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.


%% @doc 获取消息的已读状态
%% 返回指定消息的已读记录列表
%% @param MsgId 消息ID
%% @param FromUid 发送者用户ID（整数）
%% @return {ok, list(map())} | {error, Reason}
-spec get_read_status(binary(), integer()) -> {ok, list(map())} | {error, term()}.
get_read_status(MsgId, FromUid) ->
    Tb = tablename(),
    Sql = <<"SELECT to_uid, to_did, read_at FROM ", Tb/binary,
            " WHERE msg_id = $1 AND from_uid = $2 ORDER BY read_at ASC">>,
    case elib_pg:query(Sql, [MsgId, FromUid]) of
        {ok, Rows} -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.


%% @doc 获取未读消息数量
%% 统计从指定发送者发给当前用户的所有未读消息数量
%% 注意：这里需要与 msg_c2c 表联合查询
%% @param FromUid 发送者用户ID（整数）
%% @param ToUid 接收者用户ID（整数）
%% @return {ok, non_neg_integer()} | {error, Reason}
-spec get_unread_count(integer(), integer()) -> {ok, non_neg_integer()} | {error, term()}.
get_unread_count(FromUid, ToUid) ->
    % 使用左连接找出未读消息
    % 未读消息 = 在 msg_c2c 中存在但在 msg_read 中不存在的消息
    TbRead = tablename(),
    TbC2C = msg_c2c_repo:tablename(),
    Sql = <<
        "SELECT COUNT(*) as count FROM ", TbC2C/binary, " c",
        " LEFT JOIN ", TbRead/binary, " r",
        " ON c.msg_id = r.msg_id AND r.to_uid = $2",
        " WHERE c.from_id = $1 AND c.to_id = $2 AND r.id IS NULL"
    >>,
    case elib_pg:query(Sql, [FromUid, ToUid]) of
        {ok, [{#{<<"count">> := Count}}]} -> {ok, Count};
        {ok, [#{<<"count">> := Count}]} -> {ok, Count};
        {ok, [[{<<"count">>, Count}]]} -> {ok, Count};
        {ok, []} -> {ok, 0};
        {error, Reason} -> {error, Reason}
    end.


%% @doc 批量标记消息为已读
%% 将指定消息列表标记为已读（用于多设备同步）
%% @param MsgIds 消息ID列表
%% @param ToUid 接收者用户ID（整数）
%% @param ToDid 接收者设备ID（binary）
%% @return {ok, non_neg_integer()} 插入的记录数 | {error, Reason}
-spec mark_messages_read(list(binary()), integer(), binary()) -> {ok, non_neg_integer()} | {error, term()}.
mark_messages_read(MsgIds, ToUid, ToDid) when is_list(MsgIds), length(MsgIds) > 0 ->
    Tb = tablename(),
    ReadAt = elib_dt:now(),

    % 为每条记录预生成 TSID，构建批量 INSERT VALUES
    N = length(MsgIds),
    {ValuesList, AllParams} = lists:foldl(fun(MsgId, {ValsAcc, ParamsAcc}) ->
        GenId = elib_tsid:generate(msg_read),
        Idx = length(ParamsAcc),
        Placeholder = io_lib:format("($~p, $~p, 0, $~p, $~p, $~p, $~p)",
            [Idx + 1, Idx + 2, Idx + 3, Idx + 4, Idx + 5, Idx + 5]),
        {[Placeholder | ValsAcc],
         ParamsAcc ++ [GenId, MsgId, ToUid, ToDid, ReadAt]}
    end, {[], []}, MsgIds),

    ValuesStr = lists:join(<<",">>, lists:reverse(ValuesList)),
    Sql = [
        <<"INSERT INTO ">>, Tb,
        <<" (id, msg_id, from_uid, to_uid, to_did, read_at, created_at)">>,
        <<" VALUES ">>, iolist_to_binary(ValuesStr),
        <<" ON CONFLICT (msg_id, to_uid, to_did) DO NOTHING">>
    ],

    case elib_pg:query(Sql, AllParams) of
        {ok, _} -> {ok, N};
        {error, Reason} -> {error, Reason}
    end;
mark_messages_read([], _ToUid, _ToDid) ->
    {ok, 0}.


%% @doc 删除已读记录（用于清理或隐私保护）
%% @param MsgId 消息ID
%% @param ToUid 接收者用户ID（整数）
%% @return {ok, Count} | {error, Reason}
-spec delete_read_records(binary(), integer()) -> {ok, non_neg_integer()} | {error, term()}.
delete_read_records(MsgId, ToUid) ->
    Tb = tablename(),
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE msg_id = $1 AND to_uid = $2">>,
    case elib_pg:execute(Sql, [MsgId, ToUid]) of
        {ok, Count} -> {ok, Count};
        {ok, Count, _} -> {ok, Count};
        {error, Reason} -> {error, Reason}
    end.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
