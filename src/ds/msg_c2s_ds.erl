-module(msg_c2s_ds).
%%%
% msg_c2s_ds 是 msg_c2s domain service 缩写
%%%

-include("chat.hrl").
-include("log.hrl").

-export([write_topic/6]).
-export([write_msg/2]).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 写入消息主题
%%
%% 创建或更新客户端到服务端的消息主题，用于分类和管理消息
%%
%% @param Type 消息类型，通常为 <<"C2S">>
%% @param TopicId 主题ID
%% @param Uid 用户ID
%% @param To 接收方标识
%% @param Title 主题标题
%% @param CreatedAt 创建时间戳（integer 毫秒或 binary RFC3339）
%% @returns ok 表示操作成功
% msg_c2s_ds:write_topic(1, #{}).
-spec write_topic(binary(), binary(), integer(), binary(), binary(), integer() | binary()) -> ok.
write_topic(<<"C2S">>, _, _, _, <<>>, _) ->
    ok;
write_topic(<<"C2S">>, TopicId, Uid, To, Title, CreatedAtRaw) ->
    CreatedAt = elib_dt:to_rfc3339(CreatedAtRaw),
    % index type, user_id, title
    Tb = <<"msg_topic">>,
    % 使用安全的参数化查询，避免SQL注入
    Query = <<"SELECT id FROM ", Tb/binary, " WHERE type = $1 AND user_id = $2 AND title = $3 ORDER BY id DESC LIMIT 1">>,
    Id = case elib_pg:query(Query, [<<"C2S">>, Uid, Title]) of
        {ok, [#{<<"id">> := Id2}]} when is_integer(Id2) -> Id2;
        _ -> undefined
    end,
    % Id.
    Data = #{
    topic_id => TopicId,
    user_id => Uid,
    to_id => To,
    type => <<"C2S">>,
    title => Title,
    created_at => CreatedAt
    },
    if
        Id > 0 ->
            ok;
        true ->
            {Sql, Params} = elib_pg_sql:insert(Tb, Data, <<>>),
            {ok, _} = elib_pg:execute(Sql, Params),
            ok
    end.

%% @doc 存储客户端到服务端的消息
%%
%% 将消息存储到数据库中，避免重复存储相同的消息ID
%%
%% @param MsgId 消息ID
%% @param Data 消息数据映射
%% @returns any() 数据库操作结果
-spec write_msg(binary(), map()) -> ok | {error, any()}.
write_msg(MsgId, Data) ->
    Tb = <<"msg_c2s">>,
    % 使用安全的参数化查询，避免SQL注入
    case elib_pg:query(<<"SELECT count(*) AS count FROM ", Tb/binary, " WHERE msg_id = $1 ORDER BY created_at DESC LIMIT 1">>, [MsgId]) of
        {ok, [#{<<"count">> := 0}]} ->
            {Sql, Params} = elib_pg_sql:insert(Tb, Data, <<>>),
            case elib_pg:execute(Sql, Params) of
                {ok, _} -> ok;
                {error, Reason} -> {error, Reason}
            end;
        _ ->
            ok
    end.


%% 读取消息
% read_msg(ToUid, Limit) ->
%     read_msg(ToUid, Limit, undefined).


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
