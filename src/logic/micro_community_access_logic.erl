-module(micro_community_access_logic).
-export([delivery_allowed/2, join/2, guard_group_detail/2]).

delivery_allowed(Uid, Msg) -> micro_community_access_ds:delivery_allowed(Uid, Msg).

join(Uid, Cid0) ->
    try {imboy_policy:current_profile(), imboy_feature:enabled(channel), ec_cnv:to_integer(Cid0)} of
        {Profile, true, Cid} when
            Profile =/= phase1_delivery,
            is_integer(Uid),
            Uid > 0,
            Cid > 0,
            Cid =< 9223372036854775807
        ->
            micro_community_access_ds:join(Uid, Cid);
        _ ->
            {error, {403, <<"讨论区未开放"/utf8>>}}
    catch
        _:_ -> {error, {400, <<"无效讨论区参数"/utf8>>}}
    end.

%% Detail shares the same current bound-discussion eligibility as delivery.
guard_group_detail(Uid, Gid) ->
    case micro_community_access_ds:group_detail_allowed(Gid, Uid) of
        true -> ok;
        false -> {error, {403, <<"无权限访问讨论区"/utf8>>}}
    end.
