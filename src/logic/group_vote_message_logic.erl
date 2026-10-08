-module(group_vote_message_logic).
%% 创建后复用 C2G 的持久化、成员权限、禁言与加密策略，禁止直接广播绕过消息闸门。
-export([publish/1]).

publish(#{
    <<"vote_id">> := VoteId,
    <<"group_id">> := Gid,
    <<"creator_id">> := Creator,
    <<"title">> := Title
}) ->
    MsgId = <<"vote_", VoteId/binary>>,
    Message = #{
        <<"id">> => MsgId,
        <<"type">> => <<"C2G">>,
        <<"from">> => integer_to_binary(Creator),
        <<"to">> => integer_to_binary(Gid),
        <<"msg_type">> => <<"groupVote">>,
        <<"created_at">> => elib_dt:millisecond(),
        <<"payload">> => #{
            <<"vote_id">> => VoteId,
            <<"group_id">> => integer_to_binary(Gid),
            <<"title">> => Title,
            <<"content">> => Title
        }
    },
    try msg_c2g_logic:c2g(MsgId, Creator, Message) of
        ok -> accepted(MsgId, Message);
        _ -> {error, message_rejected}
    catch
        Class:_ ->
            logger:warning("Group vote message delivery failed: ~p", [Class]),
            {error, message_not_accepted}
    end.

accepted(MsgId, Message) ->
    receive
        {reply, #{<<"id">> := MsgId, <<"type">> := <<"C2G_SERVER_ACK">>}} ->
            {ok, Message};
        {reply, #{<<"id">> := MsgId}} ->
            {error, message_rejected}
    after 0 -> {error, message_not_accepted}
    end.
