-module(msg_read_repo).
%%%
% msg_read_repo 是消息已读回执数据仓库层
% 提供消息已读状态的基础数据库操作
%%%

-include("log.hrl").

-export([tablename/0]).
-export([save_read/5]).
-export([get_read_status/2]).
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
    %% 注意：msg_read 表没有 id 列；主键约束实际是
    %% uk_msg_read_msg_to_did_created (msg_id, to_uid, to_did, created_at)。
    %% 旧 SQL 同时写 id 列与 ON CONFLICT (msg_id, to_uid, to_did)，导致：
    %%   - 写 id 触发 SQLSTATE 42703 column "id" of relation "msg_read" does not exist
    %%   - 三列冲突子句也对不上唯一约束
    %% 这里去掉 id 列、对齐唯一约束的四列子句。
    Sql = [
        <<"INSERT INTO ">>,
        Tb,
        <<" (msg_id, from_uid, to_uid, to_did, read_at, created_at)">>,
        <<" VALUES ($1, $2, $3, $4, $5, $5)">>,
        <<" ON CONFLICT (msg_id, to_uid, to_did, created_at) DO NOTHING">>
    ],
    case elib_pg:query(Sql, [MsgId, FromUid, ToUid, ToDid, ReadAt]) of
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
    Sql =
        <<"SELECT to_uid, to_did, read_at FROM ", Tb/binary,
            " WHERE msg_id = $1 AND from_uid = $2 ORDER BY read_at ASC">>,
    case elib_pg:query(Sql, [MsgId, FromUid]) of
        {ok, Rows} -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.

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
