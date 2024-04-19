-module(group_member_handler).
-behavior(cowboy_rest).

-export([init/2]).

-include_lib("imlib/include/log.hrl").

%% ===================================================================
%% API
%% ===================================================================


init(Req0, State0) ->
    % ?LOG(State),
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 =
        case Action of
            join ->
                join(Req0, State);
            leave ->
                leave(Req0, State);
            alias ->
                alias(Req0, State);
            page ->
                page(Req0, State);
            false ->
                Req0
        end,
    {ok, Req1, State}.

join(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    PostVals = imboy_req:post_params(Req0),
    Gid = proplists:get_value(<<"gid">>, PostVals, 0),
    Gid2 = imboy_hashids:decode(Gid),
    case throttle:check(three_second_once, {group_member, CurrentUid}) of
        {limit_exceeded, _, _} ->
            imboy_response:error(Req0, "在处理中，请稍后重试");
        _ when Gid2 == 0 ->
            imboy_response:error(Req0, "group id 格式有误");
        _ ->
            GM = group_member_repo:find(Gid2, CurrentUid, <<"id">>),
            GMSize = maps:size(GM),
            if
                GMSize > 0 ->
                    imboy_response:success(Req0, [
                        {<<"gid">>, Gid}
                    ], "success.");
                true ->
                    G = group_repo:find_by_id(Gid2, <<"member_max,member_count">>),
                    Max = maps:get(<<"member_max">>, G, 0),
                    Count = maps:get(<<"member_count">>, G, 0),
                    % ?LOG([Max, Count, G]),
                    case group_member_logic:join(CurrentUid, Gid2, Max, Count) of
                        ok ->
                            imboy_response:success(Req0, [
                                {<<"gid">>, Gid}
                            ], "success.");
                        {error, Msg} ->
                            imboy_response:error(Req0, Msg)
                    end
            end
    end.

leave(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    PostVals = imboy_req:post_params(Req0),
    Gid = proplists:get_value(<<"gid">>, PostVals, 0),
    Gid2 = imboy_hashids:decode(Gid),
    case throttle:check(three_second_once, {group_member, CurrentUid}) of
        {limit_exceeded, _, _} ->
            imboy_response:error(Req0, "在处理中，请稍后重试");
        _ when Gid2 == 0 ->
            imboy_response:error(Req0, "group id 格式有误");
        _ ->
            GM = group_member_repo:find(Gid2, CurrentUid, <<"*">>),
            GMSize = maps:size(GM),
            case group_member_logic:leave(CurrentUid, Gid2, GMSize, GM) of
                ok ->
                    imboy_response:success(Req0, [
                        {<<"gid">>, Gid}
                    ], "success.");
                {error, Msg} ->
                    imboy_response:error(Req0, Msg)
            end
    end.

alias(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    PostVals = imboy_req:post_params(Req0),
    Gid = proplists:get_value(<<"gid">>, PostVals, 0),
    Gid2 = imboy_hashids:decode(Gid),
    case Gid2 of
        0 ->
            imboy_response:error(Req0, "group id 必须");
        _ ->
            Alias = proplists:get_value(<<"alias">>, PostVals, <<>>),
            Description = proplists:get_value(<<"description">>, PostVals, <<>>),
            group_member_logic:alias(CurrentUid, Gid2, Alias, Description),
            imboy_response:success(Req0, [{<<"gid">>, Gid}], "success.")
    end.

page(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    #{gid := Gid} = cowboy_req:match_qs([{gid, [], undefined}], Req0),
    Gid2 = imboy_hashids:decode(Gid),
    GM = group_member_repo:find(Gid2, CurrentUid, <<"id">>),
    GMSize = maps:size(GM),
    case Gid2 of
        0 ->
            imboy_response:error(Req0, "group id 必须");
        _ when GMSize == 0 ->
            imboy_response:error(Req0, "你不是群成员");
        _ ->
            {Page, Size} = imboy_req:page_size(Req0),

            Column = <<"u.avatar, u.account, u.nickname, u.sign, m.*">>,
            Where = <<"m.group_id =", (ec_cnv:to_binary(Gid2))/binary>>,
            OrderBy = <<"m.role desc, m.created_at desc">>,
            UTb = user_repo:tablename(),
            MTb = group_member_repo:tablename(),
            Tb = <<UTb/binary, " u LEFT JOIN ", MTb/binary, " m ON u.id = m.user_id">>,
            Payload = imboy_db:page(Page, Size, Tb, Where, OrderBy, Column),
            imboy_response:success(Req0, page_transfer(Payload))
    end.


page_transfer(Payload) ->
    K = <<"list">>,
    Li = proplists:get_value(K, Payload),
    % Li2 = [imboy_hashids:replace_id(imboy_hashids:replace_id(M, <<"group_id">>), <<"user_id">>) || M <- Li],
    Li2 = group_member_transfer:member_list(Li),
    proplists:delete(K, Payload),
    Payload ++ [{K, Li2}].

