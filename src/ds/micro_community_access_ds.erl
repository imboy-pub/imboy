-module(micro_community_access_ds).
-export([
    eligible/2,
    eligible_tx/3,
    delivery_allowed/2,
    revoke_channel_tx/3,
    join/2,
    group_detail_allowed/2
]).

join(Uid, Cid) ->
    try
        elib_pg:with_tx(fun(Conn) ->
            ok = workspace_guard:abort_on_error(
                workspace_guard:ensure_writable_tx(Conn, {channel, Cid})
            ),
            case micro_community_access_repo:lock_channel_tx(Conn, Cid) of
                Gid when is_integer(Gid), Gid > 0 ->
                    {ok, _} = micro_community_binding_repo:read_tx(Conn, Uid, Cid),
                    case
                        group_member_ds:join_group(Conn, <<"channel_discussion">>, Uid, Gid, #{
                            role => 1
                        })
                    of
                        {ok, _} -> {ok, #{discussion_group_id => Gid}}
                    end;
                _ ->
                    throw({abort_tx, discussion_access_denied})
            end
        end)
    of
        {ok, _} = Success -> Success;
        _ -> {error, {403, <<"讨论区不可加入"/utf8>>}}
    catch
        _:_ -> {error, {503, <<"讨论服务暂不可用"/utf8>>}}
    end.

eligible(Gid, Uid) ->
    try
        micro_community_access_repo:eligible(Gid, Uid)
    catch
        _:_ -> false
    end.

eligible_tx(Conn, Gid, Uid) ->
    micro_community_access_repo:eligible_tx(Conn, Gid, Uid).

%% Called at the actual websocket delivery boundary, including fired retries.
delivery_allowed(Uid, Data) when is_binary(Data) ->
    try
        delivery_allowed(Uid, jsone:decode(Data, [{object_format, map}]))
    catch
        _:_ -> false
    end;
delivery_allowed(Uid, #{<<"type">> := <<"C2G">>} = Msg) ->
    Gid = maps:get(<<"to">>, Msg, maps:get(<<"to_id">>, Msg, 0)),
    try
        micro_community_access_repo:delivery_allowed(ec_cnv:to_integer(Gid), ec_cnv:to_integer(Uid))
    catch
        _:_ -> false
    end;
delivery_allowed(Uid, #{<<"type">> := <<"S2C">>} = Msg) ->
    case normalize_delivery_object(maps:get(<<"payload">>, Msg, undefined)) of
        {ok, Payload} ->
            case maps:get(<<"gid">>, Payload, maps:get(<<"group_id">>, Payload, undefined)) of
                undefined ->
                    true;
                Gid ->
                    try
                        micro_community_access_repo:delivery_allowed(
                            ec_cnv:to_integer(Gid), ec_cnv:to_integer(Uid)
                        )
                    catch
                        _:_ -> false
                    end
            end;
        error ->
            false
    end;
delivery_allowed(Uid, Messages) when is_list(Messages) ->
    case normalize_delivery_object(Messages) of
        {ok, Map} when Messages =/= [] -> delivery_allowed(Uid, Map);
        _ -> lists:all(fun(Msg) -> delivery_allowed(Uid, Msg) end, Messages)
    end;
delivery_allowed(_Uid, Msg) when is_map(Msg) -> true;
delivery_allowed(_, _) ->
    false.

%% Binary-key objects only; duplicate keys cannot override a group id/type.
normalize_delivery_object(Map) when is_map(Map) ->
    case lists:all(fun is_binary/1, maps:keys(Map)) of
        true -> {ok, Map};
        false -> error
    end;
normalize_delivery_object(Pairs) when is_list(Pairs) ->
    case
        lists:all(
            fun
                ({Key, _}) when is_binary(Key) -> true;
                (_) -> false
            end,
            Pairs
        )
    of
        true ->
            Map = maps:from_list(Pairs),
            case map_size(Map) =:= length(Pairs) of
                true -> {ok, Map};
                false -> error
            end;
        false ->
            error
    end;
normalize_delivery_object(_) ->
    error.

%% Caller must execute before changing the subscription, and retain the same
%% transaction through closing the history generation and member deactivation.
revoke_channel_tx(Conn, Cid, Uid) ->
    case micro_community_access_repo:lock_channel_tx(Conn, Cid) of
        null ->
            ok;
        Gid ->
            case micro_community_access_repo:eligible_tx(Conn, Gid, Uid) of
                true ->
                    ok;
                false ->
                    ok = group_member_ds:close_history_generation(
                        Conn, Gid, Uid, <<"channel_entitlement_revoked">>
                    ),
                    micro_community_access_repo:deactivate_tx(Conn, Gid, Uid)
            end
    end.

%% Repo preserves unbound behavior; bound groups require entitlement and membership.
group_detail_allowed(Gid, Uid) ->
    try micro_community_access_repo:delivery_allowed(Gid, Uid) of
        true -> true;
        _ -> false
    catch
        _:_ -> false
    end.
