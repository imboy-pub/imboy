-module(channel_message_store).
-moduledoc "Keep legacy and idempotent channel publish result conventions at the DS boundary.".
-export([publish/6]).

publish(legacy, ChannelId, Uid, Content, MsgType, Payload) ->
    case channel_ds:publish_message(ChannelId, Uid, Content, MsgType, Payload) of
        {ok, MessageId} -> {ok, MessageId, inserted};
        Error -> Error
    end;
publish(
    {request_id, RequestId}, ChannelId, Uid, Content, MsgType, Payload
) ->
    channel_ds:publish_message(ChannelId, Uid, Content, MsgType, Payload, RequestId).
