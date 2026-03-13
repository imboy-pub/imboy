-module(channel_logic_common).

-export([channel_transfer/1]).
-export([decode_positive_id/1]).
-export([resolve_channel_id/1]).
-export([get_user_role/2]).
-export([ensure_channel_content_access/2]).
-export([channel_revoke_window_seconds/0]).
-export([log_channel_action/6]).

-include("log.hrl").

-spec channel_transfer(map()) -> map().
channel_transfer(Channel) when is_map(Channel) ->
    Channel2 = elib_hashids:replace_id(Channel),
    Channel3 = elib_hashids:replace_id(Channel2, <<"creator_uid">>),
    Channel3.

-spec resolve_channel_id(binary()) -> integer().
resolve_channel_id(ChannelIdBin) ->
    case decode_positive_id(ChannelIdBin) of
        ChannelId when ChannelId > 0 ->
            ChannelId;
        _ ->
            resolve_custom_channel_id(ChannelIdBin)
    end.

-spec decode_positive_id(term()) -> integer().
decode_positive_id(Value) ->
    case catch elib_hashids:decode(Value) of
        Id when is_integer(Id), Id > 0 ->
            Id;
        _ ->
            0
    end.

-spec resolve_custom_channel_id(binary()) -> integer().
resolve_custom_channel_id(ChannelIdBin) ->
    case catch channel_repo:find_by_custom_id(ChannelIdBin) of
        Channel when is_map(Channel) ->
            case maps:get(<<"id">>, Channel, 0) of
                ChannelId when is_integer(ChannelId), ChannelId > 0 ->
                    ChannelId;
                _ ->
                    0
            end;
        _ ->
            0
    end.

-spec get_user_role(integer(), integer()) -> integer().
get_user_role(ChannelId, Uid) ->
    case channel_admin_repo:get_role(ChannelId, Uid) of
        Role when is_integer(Role), Role > 0 ->
            Role;
        _ ->
            case channel_repo:find_by_id(ChannelId, <<"*">>) of
                #{<<"creator_uid">> := CreatorUid} when CreatorUid =:= Uid ->
                    3;
                #{<<"owner_id">> := OwnerUid} when OwnerUid =:= Uid ->
                    3;
                _ ->
                    0
            end
    end.

-spec ensure_channel_content_access(integer(), integer()) -> ok | {error, binary()}.
ensure_channel_content_access(Uid, ChannelId) ->
    case channel_repo:find_by_id(ChannelId, <<"id,type,status">>) of
        {error, _} ->
            {error, <<"频道不存在"/utf8>>};
        Channel when is_map(Channel), map_size(Channel) =:= 0 ->
            {error, <<"频道不存在"/utf8>>};
        Channel when is_map(Channel) ->
            Status = maps:get(<<"status">>, Channel, 1),
            case Status =/= 1 of
                true ->
                    {error, <<"频道已禁用或删除"/utf8>>};
                false ->
                    Role = get_user_role(ChannelId, Uid),
                    case Role > 0 of
                        true ->
                            ok;
                        false ->
                            ensure_channel_content_access_by_type(Uid, ChannelId, maps:get(<<"type">>, Channel, 0))
                    end
            end;
        _ ->
            {error, <<"频道不存在"/utf8>>}
    end.

-spec ensure_channel_content_access_by_type(integer(), integer(), integer()) -> ok | {error, binary()}.
ensure_channel_content_access_by_type(Uid, ChannelId, Type) ->
    case Type of
        0 ->
            ok;
        1 ->
            case channel_subscription_repo:is_subscribed(ChannelId, Uid) of
                true -> ok;
                _ -> {error, <<"私有频道仅限订阅用户访问"/utf8>>}
            end;
        2 ->
            IsSubscribed = channel_subscription_repo:is_subscribed(ChannelId, Uid),
            HasPurchased = channel_subscribe_ds:has_purchased(ChannelId, Uid),
            case IsSubscribed =:= true orelse HasPurchased =:= true of
                true -> ok;
                false -> {error, <<"付费频道需要先购买"/utf8>>}
            end;
        _ ->
            {error, <<"频道类型无效"/utf8>>}
    end.

-spec channel_revoke_window_seconds() -> non_neg_integer().
channel_revoke_window_seconds() ->
    case application:get_env(imboy, channel_revoke_window_seconds) of
        {ok, Value} when is_integer(Value), Value >= 0 ->
            Value;
        _ ->
            120
    end.

-spec log_channel_action(integer(), integer(), integer() | undefined, binary(), term(), integer()) -> ok.
log_channel_action(Uid, ChannelId, MessageId, Action, Result, StartMs) ->
    CostMs = erlang:max(0, elib_dt:millisecond() - StartMs),
    ?INFO_LOG([
        channel_action,
        {uid, Uid},
        {channel_id, ChannelId},
        {message_id, MessageId},
        {action, Action},
        {result, result_tag(Result)},
        {cost_ms, CostMs}
    ]),
    ok.

-spec result_tag(term()) -> binary().
result_tag(ok) ->
    <<"ok">>;
result_tag(changed) ->
    <<"changed">>;
result_tag(noop) ->
    <<"noop">>;
result_tag(<<"changed">>) ->
    <<"changed">>;
result_tag(<<"noop">>) ->
    <<"noop">>;
result_tag(<<"error">>) ->
    <<"error">>;
result_tag(<<"denied">>) ->
    <<"denied">>;
result_tag({error, _}) ->
    <<"error">>;
result_tag(_) ->
    <<"unknown">>.
