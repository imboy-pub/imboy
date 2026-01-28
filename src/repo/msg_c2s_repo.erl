-module(msg_c2s_repo).
%%%
% msg_c2s_repo 是 msg_c2s repository 缩写
% 机器人到群组消息数据仓库层，提供C2S消息的基础数据库操作
%%%

-include("chat.hrl").
-include("log.hrl").

-export([tablename/0]).
-export([write_msg/8]).
-export([list_by_ids/2]).
-export([delete_msg/1]).
-export([delete_msg/2]).

%% ===================================================================
%% API
%% ===================================================================


%% @doc 获取C2S消息表的表名
%% @return 返回C2S消息表的完整表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"msg_c2s">>).


%% @doc 写入机器人离线消息及时间线表
%% 批量插入机器人消息表及时间线表
%% @param CreatedAtRaw 创建时间（binary | integer毫秒时间戳）
%% @param MsgId 消息ID
%% @param Payload 消息载荷（binary | list）
%% @param FromId 发送者用户ID
%% @param ToUids 接收者用户ID列表
%% @param Gid 群组ID
%% @param MsgType 消息类型（text, image, audio, video, file 等）
%% @param E2EE 端到端加密信息（JSON binary，可选）
%% @return ok
%% @details 注意：from_id 和 to_groupid 是 bigint 类型，必须传入 integer
-spec write_msg(binary() | integer(), binary(), binary() | list(), integer(), list(), integer(), binary(), map() | null) -> ok.
write_msg(CreatedAtRaw, MsgId, Payload, FromId, ToUids, Gid, MsgType, E2EE) ->
    CreatedAt = elib_dt:to_rfc3339(CreatedAtRaw),
    Tb = tablename(),
    % ?DEBUG_LOG([CreatedAt, Payload, FromId, ToUids, Gid]),
    elib_pg:with_tx(fun(Conn) ->
        %% ---------- 插入机器人离线消息 ----------
        %% 使用 elib_pg:insert/4 在事务中插入，与其他 repo 保持一致的安全方式
        _ = elib_pg:insert(Conn, Tb, #{
            payload => Payload,
            to_id => Gid,
            from_id => FromId,
            created_at => CreatedAt,
            msg_id => MsgId,
            msg_type => MsgType,
            e2ee => case E2EE of
                <<>> -> null;
                null -> null;
                Map when is_map(Map) -> jsone:encode(Map, [native_utf8]);  % map 需要 encode
                Bin when is_binary(Bin) -> Bin;  % 已经是 JSON binary（避免双重编码）
                _ -> null
            end
        }, <<>>),

        %% ---------- 批量插入时间线表 ----------
        %% 注意：时间线表的 to_uid 和 to_gid 是 bigint 类型，需要传入 integer
        Vals = [ [MsgId, ToId, Gid, CreatedAt] || ToId <- ToUids ],
        {SqlTimeline, ParamsTimeline} =
            elib_pg_sql:insert_batch(msg_c2g_timeline_repo:tablename(),
                                      [msg_id, to_uid, to_gid, created_at], Vals),
        {ok, _} = elib_pg:execute(Conn, SqlTimeline, ParamsTimeline),
        ok
    end).


%% @doc 根据消息ID列表查询机器人消息
%% @param Ids 消息ID列表
%% @param Column 要查询的列名
%% @return {ok, Rows} 查询成功返回map列表 | {error, Reason} 查询失败
%% @example msg_c2s_repo:list_by_ids([<<"msg1">>, <<"msg2">>], <<"payload">>).
-spec list_by_ids(list(binary()), binary()) -> {ok, list(map())} | {error, term()}.
list_by_ids([], _Column) ->
    {ok, []};
list_by_ids(Ids, Column) ->
    Tb = tablename(),
    % 使用安全的参数化查询，避免SQL注入
    {InClause, Params} = elib_pg_sql:in(<<"msg_id">>, Ids),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE ", InClause/binary, " ORDER BY created_at ASC">>,
    elib_pg:query(Sql, Params).


%% @doc 删除机器人消息（根据消息ID）
%% @param Id 消息ID
%% @return ok | {ok, Count} 删除成功 | {error, Reason} 删除失败
%% @example msg_c2s_repo:delete_msg(<<"msg_id">>).
-spec delete_msg(binary() | integer()) -> ok | {ok, non_neg_integer()} | {error, term()}.
delete_msg(Id) ->
    Where = <<"WHERE msg_id = $1">>,
    delete_msg(Where, Id).


%% @doc 根据WHERE条件删除机器人消息
%% @param Where SQL WHERE子句
%% @param Val 参数（支持list或binary）
%% @return {ok, Count} 删除成功 | {error, Reason} 删除失败
-spec delete_msg(binary(), list()) -> {ok, non_neg_integer()} | {error, term()};
             (binary(), binary()) -> {ok, non_neg_integer()} | {error, term()}.
delete_msg(Where, Val) when is_list(Val) ->
    % 支持参数列表的安全版本（修复 SQL 注入风险）
    Tb = tablename(),
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE ", Where/binary>>,
    elib_pg:execute(Sql, Val);

delete_msg(Where, Val) ->
    % 兼容旧版本（单值）
    Tb = tablename(),
    Sql = <<"DELETE FROM ", Tb/binary, " ", Where/binary>>,
    elib_pg:execute(Sql, [Val]).
