-module(adm_micro_community_ds).
-export([get/2, set/3, candidates/4, essence/4]).

get(AdminId, Cid) ->
    tx(fun(Conn) ->
        authorize(Conn, AdminId, discussion),
        C = micro_community_binding_repo:platform_channel_tx(Conn, Cid),
        {ok, #{discussion_group_id => maps:get(<<"discussion_group_id">>, C)}}
    end).

set(AdminId, Cid, New) ->
    tx(fun(Conn) ->
        ok = micro_community_binding_repo:lock_tables_tx(Conn),
        authorize(Conn, AdminId, discussion),
        C = micro_community_binding_repo:platform_channel_tx(Conn, Cid),
        Old = maps:get(<<"discussion_group_id">>, C),
        Owner = maps:get(<<"creator_uid">>, C),
        Ids = lists:usort([G || G <- [Old, New], is_integer(G)]),
        _ = [micro_community_binding_repo:platform_group_tx(Conn, G, Owner) || G <- Ids],
        case Old =:= New of
            true ->
                ok;
            false ->
                [micro_community_binding_repo:pristine_tx(Conn, G, Owner) || G <- Ids],
                micro_community_binding_repo:write_platform_tx(Conn, AdminId, Cid, {Old, New})
        end,
        {ok, #{discussion_group_id => New}}
    end).

candidates(AdminId, Cid, Page, Size) ->
    tx(fun(Conn) ->
        authorize(Conn, AdminId, discussion),
        C = micro_community_binding_repo:platform_channel_tx(Conn, Cid),
        {ok,
            micro_community_binding_repo:platform_candidates_tx(
                Conn,
                maps:get(<<"creator_uid">>, C),
                Page,
                Size
            )}
    end).

essence(AdminId, Cid, Mid, Value) ->
    tx(fun(Conn) ->
        authorize(Conn, AdminId, essence),
        ok = workspace_guard:abort_on_error(
            workspace_guard:ensure_writable_tx(Conn, {channel, Cid})
        ),
        _ = adm_micro_community_repo:channel_tx(Conn, Cid),
        adm_micro_community_repo:essence_tx(Conn, AdminId, Cid, Mid, Value)
    end).

authorize(Conn, AdminId, Action) ->
    Roles = adm_acl:normalize_role_ids(adm_micro_community_repo:authority_tx(Conn, AdminId)),
    Permissions = lists:usort(lists:append([adm_acl:role_permissions(Role) || Role <- Roles])),
    Required =
        case Action of
            discussion -> [<<"channels:update">>, <<"groups:update">>];
            essence -> [<<"channels:update">>]
        end,
    case lists:all(fun(P) -> lists:member(P, Permissions) end, Required) of
        true -> ok;
        false -> throw({rollback, forbidden})
    end.

tx(Fun) ->
    try adm_micro_community_repo:transaction(Fun) of
        {ok, _} = Ok -> Ok;
        {rollback, {error, Reason}} -> {error, Reason};
        {rollback, Reason} -> {error, Reason};
        {error, _} = Error -> Error;
        _ -> {error, transaction_failed}
    catch
        throw:{rollback, {error, Reason}} -> {error, Reason};
        throw:{rollback, Reason} -> {error, Reason};
        Class:Reason -> {error, {transaction_failed, Class, Reason}}
    end.
