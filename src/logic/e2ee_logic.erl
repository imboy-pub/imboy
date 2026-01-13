-module(e2ee_logic).

-export([user_keys/2]).
-export([group_member_keys/2]).

-include("log.hrl").

-spec user_keys(integer(), integer()) -> {ok, map()} | {error, binary(), integer()}.
user_keys(CurrentUid, TargetUid) when is_integer(CurrentUid), is_integer(TargetUid) ->
    case CurrentUid =:= TargetUid of
        true ->
            user_keys_payload(TargetUid);
        false ->
            {IsFriend, InDenylist} = friend_ds:check_relationship(TargetUid, CurrentUid),
            case {IsFriend, InDenylist} of
                {true, 0} ->
                    user_keys_payload(TargetUid);
                _ ->
                    {error, <<"forbidden">>, 403}
            end
    end.

-spec group_member_keys(integer(), integer()) -> {ok, map()} | {error, binary(), integer()}.
group_member_keys(CurrentUid, Gid) when is_integer(CurrentUid), is_integer(Gid) ->
    case group_ds:is_member(CurrentUid, Gid) of
        true ->
            MemberUids = group_ds:member_uids(Gid),
            case user_device_ds:list_public_keys_by_uids(MemberUids) of
                {ok, Rows} ->
                    {ok, #{
                        <<"gid">> => elib_hashids:encode(Gid),
                        <<"members">> => group_by_uid(Rows)
                    }};
                {error, Reason} ->
                    ok = ?ERROR_LOG({e2ee_group_member_keys_db_error, Reason}),
                    {error, <<"internal_error">>, 500}
            end;
        false ->
            {error, <<"forbidden">>, 403}
    end.

-spec user_keys_payload(integer()) -> {ok, map()} | {error, binary(), integer()}.
user_keys_payload(TargetUid) ->
    case user_device_ds:list_public_keys(TargetUid) of
        {ok, Devices} ->
            {ok, #{
                <<"uid">> => elib_hashids:encode(TargetUid),
                <<"devices">> => Devices
            }};
        {error, Reason} ->
            ok = ?ERROR_LOG({e2ee_user_keys_db_error, Reason}),
            {error, <<"internal_error">>, 500}
    end.

-spec group_by_uid([map()]) -> [map()].
group_by_uid(Rows) ->
    Map0 =
        lists:foldl(
            fun(Row, Acc) ->
                Uid = maps:get(<<"user_id">>, Row),
                Existing = maps:get(Uid, Acc, []),
                Row2 = maps:put(<<"uid">>, elib_hashids:encode(Uid), maps:remove(<<"user_id">>, Row)),
                maps:put(Uid, [Row2 | Existing], Acc)
            end,
            #{},
            Rows
        ),
    lists:map(
        fun({Uid, DevsRev}) ->
            #{<<"uid">> => elib_hashids:encode(Uid), <<"devices">> => lists:reverse(DevsRev)}
        end,
        lists:sort(maps:to_list(Map0))
    ).
