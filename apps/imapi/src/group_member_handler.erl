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
            same_group ->
                same_group(Req0, State);
            % alias -> % 设置群内昵称
            %     alias(Req0, State);
            false ->
                Req0
        end,
    {ok, Req1, State}.

same_group(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    PostVals = imboy_req:post_params(Req0),
    A = proplists:get_value(<<"uid1">>, PostVals, <<>>),
    B = proplists:get_value(<<"uid2">>, PostVals, <<>>),
    A1 = imboy_hashids:decode(A),
    B1 = imboy_hashids:decode(B),

    {Count, Li4} = if
        CurrentUid == A1; CurrentUid == B1 ->
            Li = group_member_repo:list_same_group(A1, B1),
            Column = <<"id as gid, type, join_limit, content_limit, owner_uid, creator_uid, member_max, member_count, introduction, avatar, title, updated_at, created_at">>,
            Li2 = group_repo:list_by_ids(Li, Column),
            Li3 = [group_logic:group_transfer(M) || M <- Li2],
            {length(Li), Li3};
        true ->
            {0, []}
    end,
    imboy_response:success(Req0, #{
        <<"count">> => Count,
        <<"list">> => Li4
    }, "success.").


join(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    PostVals = imboy_req:post_params(Req0),
    MemberUids = proplists:get_value(<<"member_uids">>, PostVals, []),
    JoinMode = proplists:get_value(<<"join_mode">>, PostVals, <<>>),
    Gid = proplists:get_value(<<"gid">>, PostVals, 0),
    Gid2 = imboy_hashids:decode(Gid),
    JoinMode2 = case JoinMode of
        <<>> ->
            UserTitle = user_ds:title(CurrentUid),
            <<"invite_",  (ec_cnv:to_binary(CurrentUid))/binary, "_", UserTitle/binary>>;
        _ ->
            JoinMode
    end,
    case throttle:check(three_second_once, {group_member, CurrentUid}) of
        {limit_exceeded, _, _} ->
            imboy_response:error(Req0, "在处理中，请稍后重试");
        _ when Gid2 == 0 ->
            imboy_response:error(Req0, "group id 格式有误");
        _ when is_list(MemberUids) == false  ->
            imboy_response:error(Req0, "member_uids 必须是list");
        _ when MemberUids == []  ->
            imboy_response:error(Req0, "member_uids 不能为空");
        _ ->
            G = group_repo:find_by_id(Gid2, <<"member_max,member_count">>),
            Max = maps:get(<<"member_max">>, G, 0),
            Count = maps:get(<<"member_count">>, G, 0),
            Len = length(MemberUids),
            Diff = Max - Count,
            if
                Diff == 0 ->
                    imboy_response:error(Req0, "群成员已满。");
                Len > Diff ->
                    imboy_response:error(Req0, "还可以加入" + integer_to_list(Diff)+ "名群成员");
                true ->
                    MemberUids2 = [imboy_hashids:decode(Id) || Id <- MemberUids],
                    MemberListRes = group_member_logic:list_member(Gid2, MemberUids2),
                    % ?LOG([MemberListRes]),
                    case MemberListRes of
                        {ok, _, []} ->
                            imboy_db:with_transaction(fun(Conn) ->
                                [group_member_logic:join(Conn, JoinMode2, Uid2, Gid2) || Uid2 <- MemberUids2]
                            end),
                            MemberListRes2 = group_member_logic:list_member(Gid2, MemberUids2),
                            Sum = imboy_db:pluck(group_repo:tablename(),
                                <<"id = ",  (ec_cnv:to_binary(Gid2))/binary>>,
                                <<"user_id_sum">>,
                                0
                            ),
                            imboy_response:success(Req0, #{
                                <<"gid">> => Gid,
                                <<"user_id_sum">> => Sum,
                                <<"member_list">> => group_member_transfer:member_list(imboy_cnv:zipwith_equery(MemberListRes2))
                            }, "success.");
                        {ok, _, _} ->
                            Sum = imboy_db:pluck(group_repo:tablename(),
                                <<"id = ",  (ec_cnv:to_binary(Gid2))/binary>>,
                                <<"user_id_sum">>,
                                0
                            ),
                            imboy_response:success(Req0, #{
                                <<"gid">> => Gid,
                                <<"user_id_sum">> => Sum,
                                <<"member_list">> => group_member_transfer:member_list(imboy_cnv:zipwith_equery(MemberListRes))
                            }, "success.")
                    end
            end
    end.

leave(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    PostVals = imboy_req:post_params(Req0),
    Gid = proplists:get_value(<<"gid">>, PostVals, 0),
    MemberUids = proplists:get_value(<<"member_uids">>, PostVals, []),
    Gid2 = imboy_hashids:decode(Gid),
    case throttle:check(three_second_once, {group_member, CurrentUid}) of
        {limit_exceeded, _, _} ->
            imboy_response:error(Req0, "在处理中，请稍后重试");
        _ when Gid2 == 0 ->
            imboy_response:error(Req0, "group id 格式有误");
        _ ->
            [group_member_logic:leave(imboy_hashids:decode(Uid), Gid2, CurrentUid) || Uid <- MemberUids],
            imboy_response:success(Req0, [
                {<<"gid">>, Gid}
            ], "success.")
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
