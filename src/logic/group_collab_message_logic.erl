-module(group_collab_message_logic).
-export([publish/5]).

%% 复用群消息持久化、去重、禁言及加密策略；HTTP 调用也必须收到持久化 ACK。
publish(Gid, Creator, Type, ResourceId, Payload) ->
    MsgId = message_id(Type, ResourceId),
    Message = #{
        <<"id">> => MsgId,
        <<"type">> => <<"C2G">>,
        <<"from">> => integer_to_binary(Creator),
        <<"to">> => integer_to_binary(Gid),
        <<"msg_type">> => Type,
        <<"created_at">> => elib_dt:millisecond(),
        <<"payload">> => Payload
    },
    try msg_c2g_logic:c2g(MsgId, Creator, Message) of
        ok -> accepted(MsgId, Message)
    catch
        Class:_ ->
            logger:warning("Group collaboration message delivery failed: ~p", [Class]),
            {error, message_not_accepted}
    end.

accepted(MsgId, Message) ->
    receive
        {reply, #{<<"id">> := MsgId, <<"type">> := <<"C2G_SERVER_ACK">>}} -> {ok, Message};
        {reply, #{<<"id">> := MsgId}} -> {error, message_rejected}
    after 0 -> {error, message_not_accepted}
    end.

%% message id 列最多 40 字符；日程外部 ID 已有 38 字符，不能再拼长类型前缀。
message_id(<<"groupVote">>, ResourceId) when byte_size(ResourceId) =< 35 ->
    <<"vote_", ResourceId/binary>>;
message_id(Type, ResourceId) ->
    <<Prefix:16/binary, _/binary>> = crypto:hash(sha256, <<Type/binary, 0, ResourceId/binary>>),
    <<"gc_", (binary:encode_hex(Prefix))/binary>>.
