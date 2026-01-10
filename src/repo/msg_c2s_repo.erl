-module(msg_c2s_repo).
%%%
% msg_c2s_repo 是 msg_c2s repository 缩写
%%%

-include("chat.hrl").
-include("log.hrl").

-export([tablename/0]).
-export([write_msg/6]).
-export([list_by_ids/2]).
-export([delete_msg/1]).
-export([delete_msg/2]).

%% ===================================================================
%% API
%% ===================================================================


-spec tablename() -> binary().
tablename() ->
    imboy_pg_sql:public_tablename(<<"msg_c2s">>).


-spec write_msg(binary() | integer(), binary(), binary() | list(), integer(), list(), integer()) -> ok.
% 批量插入机器人消息表 及 时间线表
% 注意：from_id 和 to_groupid 是 bigint 类型，必须传入 integer
write_msg(CreatedAtRaw, MsgId, Payload, FromId, ToUids, Gid) ->
    CreatedAt = imboy_dt:to_rfc3339(CreatedAtRaw),
    Tb = tablename(),
    % ?DEBUG_LOG([CreatedAt, Payload, FromId, ToUids, Gid]),
    imboy_pg:with_tx(fun(Conn) ->
        %% ---------- 插入机器人离线消息 ----------
        %% 使用 imboy_pg:insert/4 在事务中插入，与其他 repo 保持一致的安全方式
        _ = imboy_pg:insert(Conn, Tb, #{
            payload => {raw, imboy_hasher:encoded_val(Payload)},
            to_groupid => Gid,
            from_id => FromId,
            created_at => CreatedAt,
            msg_id => MsgId
        }, <<>>),

        %% ---------- 批量插入时间线表 ----------
        %% 注意：时间线表的 to_uid 和 to_gid 是 bigint 类型，需要传入 integer
        Vals = [ [MsgId, ToId, Gid, CreatedAt] || ToId <- ToUids ],
        {SqlTimeline, ParamsTimeline} =
            imboy_pg_sql:insert_batch(msg_c2g_timeline_repo:tablename(),
                                      [msg_id, to_uid, to_gid, created_at], Vals),
        {ok, _} = imboy_pg:execute(Conn, SqlTimeline, ParamsTimeline),
        ok
    end).


% msg_c2s_repo:list_by_ids(MsgIds, <<"payload">>).
-spec list_by_ids(list(binary()), binary()) -> {ok, list(map())} | {error, term()}.
list_by_ids([], _Column) ->
    {ok, []};
list_by_ids(Ids, Column) ->
    Tb = tablename(),
    % 使用安全的参数化查询，避免SQL注入
    {InClause, Params} = imboy_pg_sql:in(<<"msg_id">>, Ids),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE ", InClause/binary, " ORDER BY created_at ASC">>,
    imboy_pg:query(Sql, Params).


% msg_c2s_repo:delete_msg(6).
delete_msg(Id) ->
    Where = <<"WHERE msg_id = $1">>,
    delete_msg(Where, Id).


delete_msg(Where, Val) when is_list(Val) ->
    % 支持参数列表的安全版本（修复 SQL 注入风险）
    Tb = tablename(),
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE ", Where/binary>>,
    imboy_pg:execute(Sql, Val);

delete_msg(Where, Val) ->
    % 兼容旧版本（单值）
    Tb = tablename(),
    Sql = <<"DELETE FROM ", Tb/binary, " ", Where/binary>>,
    imboy_pg:execute(Sql, [Val]).
