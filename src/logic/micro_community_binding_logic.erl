-module(micro_community_binding_logic).
-export([get/2, set/3, candidates/4]).

get(Uid, Cid) -> dispatch(Uid, Cid, fun(Id) -> micro_community_binding_ds:get(Uid, Id) end).
set(Uid, Cid, GroupId) ->
    case id(GroupId, true) of
        {ok, Gid} ->
            dispatch(Uid, Cid, fun(Id) -> micro_community_binding_ds:set(Uid, Id, Gid) end);
        error ->
            error_result(400)
    end.
candidates(Uid, Cid, Page, Size) when
    is_integer(Page),
    Page > 0,
    is_integer(Size),
    Size > 0,
    Size =< 50
->
    dispatch(Uid, Cid, fun(Id) -> micro_community_binding_ds:candidates(Uid, Id, Page, Size) end);
candidates(_, _, _, _) ->
    error_result(400).

dispatch(Uid, Cid, Fun) ->
    case
        {
            is_integer(Uid) andalso Uid > 0,
            id(Cid, false),
            imboy_policy:current_profile(),
            imboy_feature:enabled(channel)
        }
    of
        {true, {ok, Id}, Profile, true} when Profile =/= phase1_delivery -> result(Fun(Id));
        {_, error, _, _} -> error_result(400);
        _ -> error_result(403)
    end.

id(null, true) ->
    {ok, null};
id(Value, _) when is_integer(Value), Value > 0, Value =< 9223372036854775807 -> {ok, Value};
id(Value, Nullable) when is_binary(Value), byte_size(Value) > 0 ->
    try
        id(binary_to_integer(Value), Nullable)
    catch
        _:_ -> error
    end;
id(_, _) ->
    error.

result({ok, _} = Ok) -> Ok;
result({error, not_found}) -> error_result(404);
result({error, forbidden}) -> error_result(403);
result({error, invalid_group}) -> error_result(403);
result({error, group_not_pristine}) -> error_result(409);
result({error, binding_conflict}) -> error_result(409);
result(_) -> error_result(500).

error_result(400) -> {error, {400, <<"Invalid discussion binding input">>}};
error_result(403) -> {error, {403, <<"Discussion binding unavailable or forbidden">>}};
error_result(404) -> {error, {404, <<"Channel or group not found">>}};
error_result(409) -> {error, {409, <<"Discussion group is in use">>}};
error_result(500) -> {error, {500, <<"Discussion binding failed">>}}.
