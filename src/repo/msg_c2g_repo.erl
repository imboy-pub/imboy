-module(msg_c2g_repo).
%%%
% msg_c2g_repo 是 msg_c2g repository 缩写
% 用户到群组离线消息数据仓库层，提供C2G消息的基础数据库操作
%%%

-include("chat.hrl").
-include("log.hrl").

-export([tablename/0]).
-export([write_msg/6]).
-export([list_by_ids/2]).
-export([delete_msg/1]).
-export([delete_msg/2]).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 获取C2G消息表的表名
%% @return 返回C2G消息表的完整表名
-spec tablename() -> binary().
tablename() ->
    imboy_pg_sql:public_tablename(<<"msg_c2g">>).

% msg_c2g_repo:write_msg(1707686743435, <<"msg_id_1">>,  <<"{\"a\":1}">>,  1, [2,3,107], 7).
% msg_c2g_repo:write_msg(<<"2026-01-01 05:50:28.465444+00:00">>, <<"msg_id_1">>, <<"{\"a\":1}">>, 1, [2,3,107], 7).
%%====================================================================
%% @doc 写入群离线消息及时间线表
%% 支持 CreatedAt: binary() | integer() (毫秒时间戳)
%%====================================================================
-spec write_msg(
        binary() | integer(), %% CreatedAt
        binary(),             %% MsgId
        binary(),             %% Payload
        integer(),            %% FromId
        [integer()],          %% ToUids
        integer()             %% Gid
      ) -> ok.
write_msg(CreatedAtRaw, MsgId, Payload, FromId, ToUids, Gid) ->
    %% ---------- 统一转换 CreatedAt ----------
    CreatedAt = imboy_dt:to_rfc3339(CreatedAtRaw),

    TbMsg = tablename(),           %% 群离线消息表
    TbTimeline = msg_c2g_timeline_repo:tablename(), %% 群消息时间线表

    % ?DEBUG_LOG([CreatedAt, Payload, FromId, ToUids, Gid]),

    imboy_pg:with_tx(fun(Conn) ->
        %% ---------- 插入群离线消息 ----------
        %% 使用 imboy_pg:insert/4 在事务中插入，与其他 repo 保持一致的安全方式
        %% 注意：msg_c2g 表中 to_id 是群组 ID (Gid)，不是用户 ID
        _ = imboy_pg:insert(Conn, TbMsg, #{
            payload => {raw, imboy_hasher:encoded_val(Payload)},
            to_id => Gid,
            from_id => FromId,
            created_at => CreatedAt,
            msg_id => MsgId
        }, <<>>),

        %% ---------- 批量插入时间线表 ----------
        %% 注意：to_uid 和 to_gid 是 bigint 类型，需要传入 integer，不能转换成 binary
        Vals = [ [MsgId, ToId, Gid, CreatedAt] || ToId <- ToUids ],
        {SqlTimeline, ParamsTimeline} =
            imboy_pg_sql:insert_batch(TbTimeline, [msg_id, to_uid, to_gid, created_at], Vals),
        {ok, _} = imboy_pg:execute(Conn, SqlTimeline, ParamsTimeline),
        ok
    end).


% msg_c2g_repo:list_by_ids(MsgIds, <<"payload">>).
list_by_ids([], _Column) ->
    {ok, []};
list_by_ids(Ids, Column) ->
    Tb = tablename(),
    % 使用安全的参数化查询，避免SQL注入
    Placeholders = iolist_to_binary(lists:join(<<",">>,
        [<<"$", (integer_to_binary(I))/binary>> || I <- lists:seq(1, length(Ids))])),
    Where = <<" WHERE msg_id IN (", Placeholders/binary, ")">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, Where/binary, " ORDER BY created_at ASC">>,
    % 将Ids转换为参数列表
    Params = [Id || Id <- Ids],
    % ?DEBUG_LOG(Sql),
    imboy_pg:query(Sql, Params).


% msg_c2g_repo:delete_msg(6).
delete_msg(Id) ->
    Where = <<"WHERE msg_id = $1">>,
    delete_msg(Where, [Id]).


delete_msg(Where, Params) when is_list(Params) ->
    Tb = tablename(),
    Sql = <<"DELETE FROM ", Tb/binary, " ", Where/binary>>,
    imboy_pg:execute(Sql, Params).
