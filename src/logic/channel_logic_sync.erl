-module(channel_logic_sync).

-export([sync_channels/2]).

-spec sync_channels(integer(), integer()) -> {ok, map()} | {error, binary()}.
sync_channels(Uid, Since) ->
    Now = erlang:system_time(millisecond),
    case channel_subscription_ds:list_by_uid(Uid) of
        {ok, Subscriptions} when is_list(Subscriptions) ->
            ChannelIds = extract_channel_ids(Subscriptions),
            case ChannelIds of
                [] ->
                    {ok, #{channels => [], server_time => Now}};
                _ ->
                    case channel_ds:list_by_ids_since(ChannelIds, Since) of
                        {ok, Channels} when is_list(Channels) ->
                            Transferred =
                                [
                                    channel_logic_common:channel_transfer(C)
                                 || C <- Channels, is_map(C)
                                ],
                            {ok, #{channels => Transferred, server_time => Now}};
                        {ok, UnexpectedChannels} ->
                            {error, elib_cnv:safe_to_binary(UnexpectedChannels)};
                        {error, Reason} ->
                            {error, elib_cnv:safe_to_binary(Reason)};
                        Unexpected ->
                            {error, elib_cnv:safe_to_binary(Unexpected)}
                    end
            end;
        {ok, UnexpectedSubscriptions} ->
            {error, elib_cnv:safe_to_binary(UnexpectedSubscriptions)};
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)};
        Unexpected ->
            {error, elib_cnv:safe_to_binary(Unexpected)}
    end.

-spec extract_channel_ids(list()) -> list(integer()).
extract_channel_ids(Subscriptions) ->
    lists:usort(
        [
            ChannelId
         || S <- Subscriptions,
            is_map(S),
            ChannelId <- [maps:get(<<"channel_id">>, S, 0)],
            is_integer(ChannelId),
            ChannelId > 0
        ]
    ).
