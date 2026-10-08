-module(adm_micro_community_logic).
-compile({no_auto_import, [error/1]}).
-export([get/2, set/3, candidates/4, essence/4]).

get(AdminId, Cid) -> dispatch(AdminId, Cid, fun(Id) -> adm_micro_community_ds:get(AdminId, Id) end).
set(AdminId, Cid, Gid0) ->
    case id(Gid0, true) of
        {ok, Gid} ->
            dispatch(AdminId, Cid, fun(Id) -> adm_micro_community_ds:set(AdminId, Id, Gid) end);
        error ->
            error(400)
    end.
candidates(AdminId, Cid, Page, Size) when
    is_integer(Page),
    Page > 0,
    is_integer(Size),
    Size > 0,
    Size =< 50
->
    dispatch(AdminId, Cid, fun(Id) -> adm_micro_community_ds:candidates(AdminId, Id, Page, Size) end);
candidates(_, _, _, _) ->
    error(400).
essence(AdminId, Cid, Mid0, Value) when is_boolean(Value) ->
    case id(Mid0, false) of
        {ok, Mid} ->
            dispatch(AdminId, Cid, fun(Id) ->
                adm_micro_community_ds:essence(AdminId, Id, Mid, Value)
            end);
        error ->
            error(400)
    end;
essence(_, _, _, _) ->
    error(400).

dispatch(AdminId, Cid, Fun) ->
    case
        {
            is_integer(AdminId) andalso AdminId > 0,
            id(Cid, false),
            imboy_policy:current_profile(),
            imboy_feature:enabled(channel)
        }
    of
        {true, {ok, Id}, Profile, true} when Profile =/= phase1_delivery -> result(Fun(Id));
        {_, error, _, _} -> error(400);
        _ -> error(403)
    end.

id(null, true) ->
    {ok, null};
id(V, _) when is_integer(V), V > 0, V =< 9223372036854775807 -> {ok, V};
id(V, Nullable) when is_binary(V), byte_size(V) > 0 ->
    try
        id(binary_to_integer(V), Nullable)
    catch
        _:_ -> error
    end;
id(_, _) ->
    error.

result({ok, _} = Ok) -> Ok;
result({error, forbidden}) -> error(403);
result({error, invalid_group}) -> error(403);
result({error, not_found}) -> error(404);
result({error, group_not_pristine}) -> error(409);
result({error, binding_conflict}) -> error(409);
result(_) -> error(500).

error(Code) -> {error, {Code, <<"Micro-community admin operation rejected">>}}.
