-module(msg_c2g_ds).
%%%
% msg_c2g_ds 是 msg_c2g domain service 缩写
%%%
-export([write_msg/6]).
-export([revoke_offline_msg/6]).
-export([edit_offline_msg/6]).
-export([read_msg/1]).
-export([read_msg/3]).
-export([delete_msg/1]).

-include("log.hrl").

%% @doc 存储群组消息
%%
%% 将消息存储到群组消息表中，先检查消息是否已存在，避免重复存储
%%
%% @param CreatedAt 消息创建时间戳（integer 毫秒或 binary RFC3339）
%% @param Id 消息ID
%% @param Payload 消息内容（二进制格式）
%% @param FromId 发送方用户ID
%% @param ToUids 接收消息的用户ID列表
%% @param Gid 群组ID
%% @returns any() 数据库操作结果
-spec write_msg(integer() | binary(), binary(), binary(), integer(), list(), integer()) -> ok.
% msg_c2g_ds:write_msg(1707686743435, <<"msg_id_1">>,  <<"{}">>,  1, [2,3,4], 1).
% msg_c2g_ds:write_msg(1707686743435, <<"msg_id_1">>,  <<"{\"a\":1}">>,  1, [2,3,107], 7).
write_msg(CreatedAtRaw, Id, Payload, FromId, ToUids, Gid) ->
    CreatedAt = imboy_dt:to_rfc3339(CreatedAtRaw),
    % 使用安全的参数化查询，避免SQL注入
    case imboy_pg:pluck_value(
        msg_c2g_repo:tablename()
        , <<"count(*)">>
        , #{<<"msg_id">> => Id}
        , #{}, 0) of
        0 ->
            msg_c2g_repo:write_msg(CreatedAt, Id, Payload, FromId, ToUids, Gid);
        _ ->
            ok
    end.


%% @doc 撤回离线群组消息
%%
%% 处理群组消息的撤回操作，更新消息内容，重置用户确认状态
%%
%% @param Msg2 撤回消息的新内容
%% @param NowTs 当前时间戳
%% @param MsgId 原消息ID
%% @param FromId 发送方用户ID
%% @param MemberUids 群组成员用户ID列表
%% @param Gid 群组ID
%% @returns ok 表示操作成功
-spec revoke_offline_msg(binary(), binary() | integer(), binary(), integer(), list(), integer()) -> ok.
revoke_offline_msg(Msg2, NowTs, MsgId, FromId, MemberUids, Gid) ->
    % 存储消息
    write_msg(NowTs, MsgId, Msg2, FromId, MemberUids, Gid),
    % 使用 imboy_pg:update/4 + {raw, ...} 安全地更新 payload
    _ = imboy_pg:update(
        msg_c2g_repo:tablename(),
        #{payload => {raw, imboy_hasher:encoded_val(Msg2)}},
        <<"msg_id = $1">>,
        [MsgId]
    ),
    % 已确认的消息需要重新确认
    % 使用安全的参数化查询，避免SQL注入
    _ = imboy_pg:update(
        msg_c2g_timeline_repo:tablename(),
        #{client_ack => 0},
        <<"msg_id = $1">>,
        [MsgId]
    ),
    ok.

%% @doc 编辑离线消息
%% @param Payload 消息内容
%% @param NowTs 时间戳
%% @param MsgId 消息ID
%% @param FromId 发送者ID
%% @param MemberUids 成员ID列表
%% @param Gid 群组ID
%% @returns ok 表示操作成功
-spec edit_offline_msg(binary(), binary() | integer(), binary(), integer(), list(), integer()) -> ok.
edit_offline_msg(Payload, _NowTs, MsgId, FromId, _MemberUids, _Gid) ->
    % 使用 imboy_pg:update/4 + {raw, ...} 安全地更新 payload
    _ = imboy_pg:update(
        msg_c2g_repo:tablename(),
        #{payload => {raw, imboy_hasher:encoded_val(Payload)}},
        <<"msg_id = $1 AND from_id = $2">>,
        [MsgId, FromId]
    ),
    % 已确认的消息需要重新确认
    % 使用安全的参数化查询，避免SQL注入
    _ = imboy_pg:update(
        msg_c2g_timeline_repo:tablename(),
        #{client_ack => 0},
        <<"msg_id = $1">>,
        [MsgId]
    ),
    ok.

%% @doc 读取离线消息
%%
%% 读取指定用户的离线群组消息，使用默认限制
%%
%% @param ToUid 接收消息的用户ID
%% @returns list() 离线消息列表
-spec read_msg(integer()) -> [map()].
% msg_c2g_ds:read_msg(3).
read_msg(ToUid) ->
    read_msg(ToUid, 1000, undefined).

%% @doc 读取离线消息 - 支持 last_msg_at 分页
%%
%% 读取指定用户的离线群组消息，支持数量限制和时间戳过滤
%%
%% @param ToUid 接收消息的用户ID
%% @param Limit 读取消息数量限制
%% @param LastMsgAt 最后消息时间戳，undefined表示读取所有未确认消息
%% @returns list() 离线消息列表，按时间顺序排列
-spec read_msg(integer(), integer(), undefined | integer() | binary()) -> [map()].
% msg_c2g_ds:read_msg(3, 1000, 1707686743435).
read_msg(ToUid, Limit, undefined) ->
    % 获取用户未确认的消息
    Column = <<"msg_id, created_at">>,
    {ok, Rows} = msg_c2g_timeline_repo:list_by_uid(ToUid, Column, Limit),
    MsgIds = [MsgId || #{<<"msg_id">> := MsgId} <- Rows],
    % 按创建时间排序获取消息内容（包含 from_id 和 to_id）
    P = imboy_hasher:decoded_payload(),
    Column2 = <<"id, ", P/binary, ", from_id, to_id, created_at, server_ts, msg_id">>,
    case msg_c2g_repo:list_by_ids(MsgIds, Column2) of
        {ok, []} ->
            [];
        {ok, Rows2} ->
            % 与 msg_c2c_ds:read_msg 保持一致，返回包含 from_id 和 to_id 的数据
            [imboy_response:json_decode_field(Row, <<"payload">>) || Row <- Rows2]
    end;
read_msg(ToUid, Limit, LastMsgAt) ->
    % 使用 imboy_dt:to_rfc3339/1 统一转换时间戳为 RFC3339 格式
    FixedLastMsgAt = imboy_dt:to_rfc3339(LastMsgAt),
    % 获取指定时间之后的用户未确认消息
    Tb = msg_c2g_timeline_repo:tablename(),
    Column = <<"msg_id, created_at">>,
    Where = <<" WHERE to_uid = $1 AND client_ack = 0 AND created_at >= $2 ORDER BY created_at ASC LIMIT $3">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, Where/binary>>,
    case imboy_pg:query(Sql, [ToUid, FixedLastMsgAt, Limit]) of
        {ok, []} ->
            [];
        {ok, Rows} ->
            MsgIds = [MsgId || #{<<"msg_id">> := MsgId} <- Rows],
            % 按创建时间排序获取消息内容（包含 from_id 和 to_id）
            P = imboy_hasher:decoded_payload(),
            Column2 = <<"id, ", P/binary, ", from_id, to_id, created_at, server_ts, msg_id">>,
            case msg_c2g_repo:list_by_ids(MsgIds, Column2) of
                {ok, []} ->
                    [];
                {ok, Rows2} ->
                    % 与 msg_c2c_ds:read_msg 保持一致，返回包含 from_id 和 to_id 的数据
                    [imboy_response:json_decode_field(Row, <<"payload">>) || Row <- Rows2]
            end
    end.

%% @doc 删除群组消息
%%
%% 根据消息ID从群组消息表中删除消息
%%
%% @param Id 消息ID
%% @returns any() 数据库删除操作结果
-spec delete_msg(any()) -> any().
delete_msg(Id) ->
    msg_c2g_repo:delete_msg(Id).
