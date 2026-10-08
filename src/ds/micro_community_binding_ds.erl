-module(micro_community_binding_ds).
-export([get/2, set/3, candidates/4, guard_group_tx/2, guard_channel_tx/2]).

get(Uid, Cid) ->
    tx(fun(Conn) -> micro_community_binding_repo:read_tx(Conn, Uid, Cid) end).

set(Uid, Cid, New) ->
    tx(fun(Conn) ->
        ok = micro_community_binding_repo:lock_tables_tx(Conn),
        C = channel(Conn, Uid, Cid),
        Old = maps:get(<<"discussion_group_id">>, C),
        Owner = maps:get(<<"creator_uid">>, C),
        validate_groups(Conn, Uid, Owner, Old, New),
        case Old =:= New of
            true -> ok;
            false -> micro_community_binding_repo:write_tx(Conn, Uid, Cid, {Old, New})
        end,
        {ok, #{discussion_group_id => New}}
    end).

validate_groups(Conn, Uid, Owner, Old, New) ->
    %% Deterministic group order also covers a simultaneous swap attempt.
    Ids = lists:usort([G || G <- [Old, New], is_integer(G)]),
    [micro_community_binding_repo:group_tx(Conn, Uid, G, Owner) || G <- Ids],
    case Old =:= New of
        true ->
            ok;
        false ->
            [micro_community_binding_repo:pristine_tx(Conn, G, Owner) || G <- Ids],
            ok
    end.

candidates(Uid, Cid, Page, Size) ->
    tx(fun(Conn) ->
        C = channel(Conn, Uid, Cid),
        Result = micro_community_binding_repo:candidates_tx(
            Conn,
            Uid,
            maps:get(<<"creator_uid">>, C),
            Page,
            Size
        ),
        {ok, Result#{page => Page, size => Size}}
    end).

channel(Conn, Uid, Cid) ->
    case micro_community_binding_repo:channel_tx(Conn, Uid, Cid) of
        #{<<"status">> := 1, <<"scope">> := <<"personal">>, <<"manages">> := true} = C -> C;
        _ -> throw({rollback, forbidden})
    end.

guard_group_tx(Conn, Gid) -> micro_community_binding_repo:guard_group_tx(Conn, Gid).
guard_channel_tx(Conn, Cid) -> micro_community_binding_repo:guard_channel_tx(Conn, Cid).

tx(Fun) ->
    try micro_community_binding_repo:transaction(Fun) of
        {ok, _} = Success -> Success;
        {rollback, Reason} -> {error, Reason};
        {error, Reason} -> {error, Reason};
        _ -> {error, binding_query_failed}
    catch
        throw:{rollback, Reason} -> {error, Reason};
        Class:Reason -> {error, {binding_transaction_failed, Class, Reason}}
    end.
