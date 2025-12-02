-module(msg_c2s_ds).
%%%
% msg_c2s_ds 是 msg_c2s domain service 缩写
%%%

-include_lib("imlib/include/chat.hrl").
-include_lib("imlib/include/log.hrl").

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
%% @param CreatedAt 创建时间戳
%% @returns ok 表示操作成功
% msg_c2s_ds:write_topic(1, #{}).
-spec write_topic(binary(), binary(), integer(), binary(), binary(), integer()) -> ok.
write_topic(<<"C2S">>, _, _, _, <<>>, _) ->
    ok;
write_topic(<<"C2S">>, TopicId, Uid, To, Title, CreatedAt) ->
    % index type, user_id, title
    Tb = <<"msg_topic">>,
    Where = <<"type = 'C2S' AND user_id = '", (ec_cnv:to_binary(Uid))/binary, "' AND title= '", Title/binary, "'">>,
    Query = <<"SELECT id From  ", Tb/binary, " WHERE ", Where/binary, " ORDER BY id desc limit 1;">>,
    Id = imboy_db:pluck(Query, 0),
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
            {ok, _, [{_}]} = imboy_db:insert_into(Tb, Data),
            ok
    end.

%% @doc 存储客户端到服务端的消息
%%
%% 将消息存储到数据库中，避免重复存储相同的消息ID
%%
%% @param MsgId 消息ID
%% @param Data 消息数据映射
%% @returns any() 数据库操作结果
-spec write_msg(binary(), map()) -> any().
write_msg(MsgId, Data) ->
    Tb = <<"msg_c2s">>,
    Where = <<"msg_id = '", MsgId/binary, "'">>,
    Query = <<"SELECT count(*) count From  ", Tb/binary, " WHERE ", Where/binary, " ORDER BY created_at desc limit 1;">>,
    Count = imboy_db:pluck(Query, 0),
    if
        Count > 0 ->
            ok;
        true ->
            imboy_db:insert_into(Tb, Data, <<>>),
            ok
    end.


%% 读取消息
% read_msg(ToUid, Limit) ->
%     read_msg(ToUid, Limit, undefined).


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
