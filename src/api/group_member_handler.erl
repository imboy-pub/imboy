-module(group_member_handler).

-behavior(cowboy_rest).

-export([init/2]).

-include("log.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    % ?DEBUG_LOG(State),
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
    #{uid1 := A} = cowboy_req:match_qs([{uid1, [], <<>>}], Req0),
    #{uid2 := B} = cowboy_req:match_qs([{uid2, [], <<>>}], Req0),

    A1 = imboy_hashids:decode(A),
    B1 = imboy_hashids:decode(B),

    {Count, Li4} =
        if CurrentUid == A1; CurrentUid == B1 ->
               Li = group_member_repo:list_same_group(A1, B1),
               Column =
                   <<"id as gid, type, join_limit, content_limit, owner_uid, creator_uid, "
                     "member_max, member_count, introduction, avatar, title, updated_at, "
                     "created_at">>,
               Li2 = case group_repo:list_by_ids(Li, Column) of
                         {ok, Rows} ->
                             Rows;
                         _ ->
                             []
                     end,
               Li3 = [group_logic:group_transfer(M) || M <- Li2],
               {length(Li), Li3};
           true ->
               {0, []}
        end,
    imboy_response:success(Req0, #{<<"count">> => Count, <<"list">> => Li4}, "success.").

join(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    PostVals = imboy_param:post(Req0),
    MemberUids = maps:get(<<"member_uids">>, PostVals, []),
    JoinMode = maps:get(<<"join_mode">>, PostVals, <<>>),
    Gid = maps:get(<<"gid">>, PostVals, 0),
    Gid2 = imboy_hashids:decode(Gid),
    JoinMode2 =
        case JoinMode of
            <<>> ->
                UserTitle = user_ds:title(CurrentUid),
                <<"invite_", (ec_cnv:to_binary(CurrentUid))/binary, "_", UserTitle/binary>>;
            _ ->
                JoinMode
        end,
    case throttle:check(three_second_once, {group_member, CurrentUid}) of
        {limit_exceeded, _, _} ->
            imboy_response:error(Req0, "在处理中，请稍后重试");
        _ when Gid2 == 0 ->
            imboy_response:error(Req0, "group id 格式有误");
        _ when is_list(MemberUids) == false ->
            imboy_response:error(Req0, "member_uids 必须是list");
        _ when MemberUids == [] ->
            imboy_response:error(Req0, "member_uids 不能为空");
        _ ->
            case group_repo:find_by_id(Gid2, <<"member_max,member_count">>) of
                {error, _Reason} ->
                    imboy_response:error(Req0, "群组不存在");
                G ->
                    Max = maps:get(<<"member_max">>, G, 0),
                    Count = maps:get(<<"member_count">>, G, 0),
                    Len = length(MemberUids),
                    Diff = Max - Count,
                    if Diff == 0 ->
                           imboy_response:error(Req0, "群成员已满。");
                       Len > Diff ->
                           imboy_response:error(Req0,
                                                imboy_cnv:implode("",
                                                                  ["还可以加入",
                                                                   integer_to_list(Diff),
                                                                   "名群成员"]));
                       true ->
                           MemberUids2 = [imboy_hashids:decode(Id) || Id <- MemberUids],
                           MemberListRes = group_member_logic:list_member(Gid2, MemberUids2),
                           % ?DEBUG_LOG([MemberListRes]),
                           case MemberListRes of
                               {ok, []} ->
                                   imboy_pg:with_tx(fun(Conn) ->
                                                       [group_member_logic:join_group(Conn,
                                                                                      JoinMode2,
                                                                                      Uid2,
                                                                                      Gid2,
                                                                                      #{})
                                                        || Uid2 <- MemberUids2]
                                                    end),
                                   {ok, MemberListRes2} =
                                       group_member_logic:list_member(Gid2, MemberUids2),
                                   Sum = imboy_pg:pluck_value(
                                             group_repo:tablename(),
                                             <<"user_id_sum">>,
                                             #{id => Gid2},
                                             #{},
                                             0),
                                   imboy_response:success(Req0,
                                                            #{<<"gid">> => Gid,
                                                            <<"user_id_sum">> => Sum,
                                                            <<"member_list">> =>
                                                                group_member_transfer:member_list(MemberListRes2)},
                                                          "success.");
                               {ok, MemberList} ->
                                   % 已经是成员，直接使用查询结果
                                   Sum = imboy_pg:pluck_value(
                                             group_repo:tablename(),
                                             <<"user_id_sum">>,
                                             #{id => Gid2},
                                             #{},
                                             0),
                                   imboy_response:success(Req0,
                                                          #{<<"gid">> => Gid,
                                                            <<"user_id_sum">> => Sum,
                                                            <<"member_list">> =>
                                                                group_member_transfer:member_list(MemberList)},
                                                          "success.")
                           end
                    end
            end
    end.

leave(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    PostVals = imboy_param:post(Req0),
    Gid = maps:get(<<"gid">>, PostVals, 0),
    MemberUids = maps:get(<<"member_uids">>, PostVals, []),
    Gid2 = imboy_hashids:decode(Gid),
    case throttle:check(three_second_once, {group_member, CurrentUid}) of
        {limit_exceeded, _, _} ->
            imboy_response:error(Req0, "在处理中，请稍后重试");
        _ when Gid2 == 0 ->
            imboy_response:error(Req0, "group id 格式有误");
        _ ->
            [group_member_logic:leave(
                 imboy_hashids:decode(Uid), Gid2, CurrentUid)
             || Uid <- MemberUids],
            imboy_response:success(Req0, #{<<"gid">> => Gid}, "success.")
    end.

alias(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    PostVals = imboy_param:post(Req0),
    Gid = maps:get(<<"gid">>, PostVals, 0),
    Gid2 = imboy_hashids:decode(Gid),
    case Gid2 of
        0 ->
            imboy_response:error(Req0, "group id 必须");
        _ ->
            Alias = maps:get(<<"alias">>, PostVals, <<>>),
            Description = maps:get(<<"description">>, PostVals, <<>>),
            group_member_logic:alias(CurrentUid, Gid2, Alias, Description),
            imboy_response:success(Req0, #{<<"gid">> => Gid}, "success.")
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
            {Page, Size} = imboy_param:page(Req0),

            WhereSql = <<"m.group_id =", (ec_cnv:to_binary(Gid2))/binary>>,
            Where = #{<<"__raw">> => WhereSql},
            UTb = user_repo:tablename(),
            MTb = group_member_repo:tablename(),
            Tb = <<UTb/binary, " u LEFT JOIN ", MTb/binary, " m ON u.id = m.user_id">>,
            Fields =
                <<"u.nickname, u.avatar, u.account, u.sign, m.user_id, m.group_id, "
                  "m.alias, m.invite_code, m.description, m.role, m.is_join, m.join_mod"
                  "e, m.status, m.updated_at, m.created_at">>,
            Payload =
                case imboy_pg:page_with_total(Tb, Fields, Where, <<"m.id desc">>, Page, Size) of
                    {ok, #{total := Total, list := Rows}} ->
                        Rows2 = group_member_transfer:member_list(Rows),
                        #{total => Total,
                          page => Page,
                          size => Size,
                          list => Rows2};
                    _ ->
                        #{total => 0,
                          page => Page,
                          size => Size,
                          list => []}
                end,
            imboy_response:success(Req0, page_transfer(Payload))
    end.

page_transfer(Payload) ->
    K = <<"list">>,
    Li = maps:get(K, Payload, []),
    Li2 = group_member_transfer:member_list(Li),
    Payload#{K => Li2}.
