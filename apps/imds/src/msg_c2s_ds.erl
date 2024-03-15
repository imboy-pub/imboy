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

% msg_c2s_ds:write_topic(1, #{}).
-spec write_topic(binary(), binary(), integer(), binary(), Title::binary(), integer()) -> ok.
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


-spec write_msg(binary(), map()) -> any().
%% 存储消息
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
