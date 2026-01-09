-module(group_handler).

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
            face2face ->
                face2face(Req0, State);
            face2face_save ->
                face2face_save(Req0, State);
            add ->
                add(Req0, State);
            edit ->
                edit(Req0, State);
            dissolve ->
                dissolve(Req0, State);
            detail ->
                detail(Req0, State);
            page ->
                #{attr := Attr} = cowboy_req:match_qs([{attr, [], undefined}], Req0),
                page(Req0, State, Attr);
            msg_page ->
                msg_page(Req0, State);
            qrcode ->
                qrcode(Req0, State);
            false ->
                Req0
        end,
    {ok, Req1, State}.

detail(Req0, _State) ->
    #{gid := Gid} = cowboy_req:match_qs([{gid, [], <<>>}], Req0),
    Gid2 = imboy_hashids:decode(Gid),
    case Gid2 of
        0 ->
            imboy_response:error(Req0, "group id 必须");
        Gid2 when Gid2 > 0 ->
            % Uid = maps:get(current_uid, State),
            % GM = group_member_repo:find(Gid2, Uid, <<"id">>),
            % GMSize = maps:size(GM),
            % Column
            case group_repo:find_by_id(Gid2, <<"*">>) of
                {error, _Reason} ->
                    imboy_response:error(Req0, "群组不存在");
                G ->
                    imboy_response:success(Req0, group_logic:group_transfer(G), "success.")
            end
    end.

face2face(Req0, State) ->
    #{longitude := Lng} = cowboy_req:match_qs([{longitude, [], undefined}], Req0),
    #{latitude := Lat} = cowboy_req:match_qs([{latitude, [], undefined}], Req0),
    #{code := Code} = cowboy_req:match_qs([{code, [], <<>>}], Req0),
    Uid = maps:get(current_uid, State),
    case throttle:check(three_second_once, Uid) of
        {limit_exceeded, _, _} ->
            imboy_response:error(Req0, "在处理中，请稍后重试");
        _ ->
            case group_logic:face2face(Uid, Code, Lng, Lat) of
                {ok, Gid} ->
                    Gid2 = imboy_hashids:encode(Gid),
                    ToUidLi = group_ds:member_uids(Gid),
                    User = user_repo:find_by_id(Uid, <<"account,avatar,nickname">>),
                    Payload =
                        #{<<"gid">> => Gid2,
                          <<"user_id_sum">> => lists:sum(ToUidLi),
                          <<"nickname">> => maps:get(<<"nickname">>, User),
                          <<"avatar">> => maps:get(<<"avatar">>, User),
                          <<"account">> => maps:get(<<"account">>, User),
                          <<"msg_type">> => <<"group_member_join">>},
                    msg_s2c_ds:send(Uid, Payload, ToUidLi, no_save),

                    MemberListRes =
                        user_repo:list_by_ids(ToUidLi, <<"id as user_id,account,avatar,nickname">>),
                    MemberList =
                        case MemberListRes of
                            {ok, L} ->
                                L;
                            _ ->
                                []
                        end,
                    imboy_response:success(Req0,
                                           #{gid => Gid2,
                                             member_list =>
                                                 group_member_transfer:member_list(MemberList)},
                                           "success.");
                {error, Msg} ->
                    imboy_response:error(Req0, Msg)
            end
    end.

face2face_save(Req0, State) ->
    PostVals = imboy_param:post(Req0),
    Code = maps:get(<<"code">>, PostVals, []),
    Gid = maps:get(<<"gid">>, PostVals, []),
    Uid = maps:get(current_uid, State),
    Gid2 = imboy_hashids:decode(Gid),
    case group_logic:face2face_save(Code, Gid2, Uid) of
        {ok, _} ->
            case group_member_logic:list_member(Gid2) of
                {error, Reason} ->
                    imboy_response:error(Req0, Reason);
                {ok, MemberList} ->
                    case group_repo:find_by_id(Gid2, <<"*">>) of
                        {error, Reason2} ->
                            imboy_response:error(Req0, Reason2);
                        G2 ->
                            imboy_response:success(Req0,
                                                   #{group => group_logic:group_transfer(G2),
                                                     member_list =>
                                                         group_member_transfer:member_list(MemberList)},
                                                   "success.")
                    end
            end
    end.

add(Req0, State) ->
    Uid = maps:get(current_uid, State),
    Type = 2, % 类型: 1 公开群组  2 私有群组
    case throttle:check(three_second_once, Uid) of
        {limit_exceeded, _, _} ->
            imboy_response:error(Req0, "在处理中，请稍后重试");
        _ ->
            Count =
                imboy_pg:pluck_value(
                    group_repo:tablename(), <<"count(*)">>, #{status => 1, owner_uid => Uid}, 0),
            PostVals = imboy_param:post(Req0),
            MemberUids = maps:get(<<"member_uids">>, PostVals, []),
            % 确保 MemberUids 是一个列表
            MemberUids2 = case MemberUids of
                List when is_list(List) -> List;
                _ -> []
            end,
            case group_logic:add(Count, Uid, Type, MemberUids2) of
                {ok, Gid} ->
                    case group_repo:find_by_id(Gid, <<"*">>) of
                        {error, Reason} ->
                            imboy_response:error(Req0, Reason);
                        GData ->
                            GData1 = imboy_hashids:replace_id(GData),
                            GData2 = imboy_hashids:replace_id(GData1, <<"owner_uid">>),
                            GData3 = imboy_hashids:replace_id(GData2, <<"creator_uid">>),
                            case group_member_logic:list_member(Gid) of
                                {error, Reason2} ->
                                    imboy_response:error(Req0, Reason2);
                                {ok, MemberList} ->
                                    imboy_response:success(Req0,
                                                           #{group => GData3,
                                                             member_list =>
                                                                 group_member_transfer:member_list(MemberList)},
                                                           "success.")
                            end
                    end;
                {error, Msg} ->
                    imboy_response:error(Req0, Msg)
            end
    end.

edit(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = imboy_param:post(Req0),
    Gid = maps:get(<<"gid">>, PostVals, 0),
    Gid2 = imboy_hashids:decode(Gid),

    Title = maps:get(<<"title">>, PostVals, undefined),
    Avatar = maps:get(<<"avatar">>, PostVals, undefined),
    Introduction = maps:get(<<"introduction">>, PostVals, undefined),
    Data = #{},
    % title => Title,
    % avatar => Avatar,
    % introduction => Introduction
    Data1 =
        if Title /= undefined ->
               Data#{title => Title};
           true ->
               Data
        end,
    Data2 =
        if Avatar /= undefined ->
               Data1#{avatar => Avatar};
           true ->
               Data1
        end,
    Data3 =
        if Introduction /= undefined ->
               Data2#{introduction => Introduction};
           true ->
               Data2
        end,
    case Gid2 of
        0 ->
            imboy_response:error(Req0, "group id 必须");
        Gid2 when Gid2 > 0 ->
            % 类型: 1 公开群组  2 私有群组
            Now = imboy_dt:now(),
            Tb = group_repo:tablename(),
            Count = imboy_pg:pluck_value(Tb, <<"count(*)">>, #{id => Gid2}, 0),
            % ?DEBUG_LOG([Tb, Gid2, Count]),
            _ = case Count > 0 of
                    true ->
                        {ok, _} =
                            imboy_pg:update(Tb, Data3#{updated_at => Now}, <<"id = $1">>, [Gid2]),
                        ToUidLi = group_ds:member_uids(Gid2),
                        Payload = Data3#{<<"gid">> => Gid, <<"msg_type">> => <<"group_eidt">>},
                        _ = msg_s2c_ds:send(Uid, Payload, ToUidLi, save);
                    false ->
                        M3 = group_random_code_repo:find_by_gid(Gid2, <<"user_id, created_at">>),
                        Data4 =
                            Data3#{owner_uid => maps:get(<<"user_id">>, M3, Uid),
                                   creator_uid => maps:get(<<"user_id">>, M3, Uid),
                                   created_at => maps:get(<<"created_at">>, M3, Now),
                                   id => Gid2},
                        imboy_pg:insert(Tb, Data4)
                end,
            imboy_response:success(Req0, #{<<"gid">> => Gid}, "success.");
        _ ->
            imboy_response:error(Req0, "group id 格式有误")
    end.

%% 解散群
dissolve(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    PostVals = imboy_param:post(Req0),
    Gid = maps:get(<<"gid">>, PostVals, 0),
    Gid2 = imboy_hashids:decode(Gid),
    case throttle:check(per_hour_once, {group, Gid2}) of
        {limit_exceeded, _, _} ->
            imboy_response:error(Req0, "在处理中，请稍后重试");
        _ when Gid2 == 0 ->
            imboy_response:error(Req0, "group id 必须");
        _ when Gid2 > 0 ->
            case group_repo:find_by_id(Gid2, <<"*">>) of
                {error, _Reason} ->
                    imboy_response:error(Req0, "群组不存在");
                G ->
                    OwnerUid = maps:get(<<"owner_uid">>, G, 0),
                    % ?DEBUG_LOG(["OwnerUid", OwnerUid, "uid", CurrentUid, G]),
                    case group_logic:dissolve(CurrentUid, Gid2, OwnerUid, G) of
                        ok ->
                            imboy_response:success(Req0, #{<<"gid">> => Gid}, "success.");
                        {error, Msg} ->
                            imboy_response:error(Req0, Msg)
                    end
            end;
        _ ->
            imboy_response:error(Req0, "group id 格式有误")
    end.

%% 我拥有的群
page(Req0, State, <<"owner">>) ->
    CurrentUid = maps:get(current_uid, State),
    {Page, Size} = imboy_param:page(Req0),

    Where = #{status => 1, owner_uid => CurrentUid},

    Tb = group_repo:tablename(),
    Payload =
        case imboy_pg:page_with_total(Tb, Where, Page, Size) of
            {ok, #{total := Total, list := Rows}} ->
                #{total => Total,
                  page => Page,
                  size => Size,
                  list => Rows};
            _ ->
                #{total => 0,
                  page => Page,
                  size => Size,
                  list => []}
        end,
    imboy_response:success(Req0, page_transfer(Payload));
%% 我加入的群
page(Req0, State, <<"join">>) ->
    CurrentUid = maps:get(current_uid, State),
    {Page, Size} = imboy_param:page(Req0),

    Where =
        #{<<"g.status">> => 1,
          <<"m.is_join">> => 1,
          <<"m.user_id">> => CurrentUid},
    GTb = group_repo:tablename(),
    MTb = group_member_repo:tablename(),
    Tb = <<GTb/binary, " g LEFT JOIN ", MTb/binary, " m ON g.id = m.group_id">>,
    Payload =
        case imboy_pg:page_with_total(Tb, Where, Page, Size) of
            {ok, #{total := Total, list := Rows}} ->
                #{total => Total,
                  page => Page,
                  size => Size,
                  list => Rows};
            _ ->
                #{total => 0,
                  page => Page,
                  size => Size,
                  list => []}
        end,
    imboy_response:success(Req0, page_transfer(Payload)).

msg_page(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    #{gid := Gid} = cowboy_req:match_qs([{gid, [], undefined}], Req0),
    Gid2 = imboy_hashids:decode(Gid),
    GM = group_member_repo:find(Gid2, CurrentUid, <<"id">>),
    GMSize = maps:size(GM),
    WhereSql =
        case imboy_param:int(last_time, Req0, 0) of
            {ok, Last} when Last > 0 ->
                <<"to_groupid=",
                  (ec_cnv:to_binary(Gid2))/binary,
                  " AND created_at >= ",
                  (ec_cnv:to_binary(Last))/binary>>;
            _ ->
                <<"to_groupid=", (ec_cnv:to_binary(Gid2))/binary>>
        end,
    Where = #{<<"__raw">> => WhereSql},
    case Gid2 of
        0 ->
            imboy_response:error(Req0, "group id 必须");
        _ when GMSize == 0 ->
            imboy_response:error(Req0, "你不是群成员");
        _ ->
            {Page, Size} = imboy_param:page(Req0),
            Tb = msg_c2g_repo:tablename(),

            Payload =
                case imboy_pg:page_with_total(Tb, Where, Page, Size) of
                    {ok, #{total := Total, list := Rows}} ->
                        #{total => Total,
                          page => Page,
                          size => Size,
                          list => Rows};
                    _ ->
                        #{total => 0,
                          page => Page,
                          size => Size,
                          list => []}
                end,
            imboy_response:success(Req0, msg_page_transfer(Payload))
    end.

%% 扫描"群二维码"
qrcode(Req0, State) ->
    #{id := Gid} = cowboy_req:match_qs([{id, [], undefined}], Req0),
    #{exp := ExpiredAt} = cowboy_req:match_qs([{exp, [], undefined}], Req0),
    #{tk := Tk} = cowboy_req:match_qs([{tk, [], undefined}], Req0),

    Key = config_ds:get(<<"solidified_key">>),
    ExpiredAt2 = ec_cnv:to_binary(ExpiredAt),
    ExpiredAtInt = binary_to_integer(ExpiredAt2),
    Verified =
        imboy_hasher:md5(<<ExpiredAt2/binary, "_", (ec_cnv:to_binary(Key))/binary>>) == Tk,
    Now = imboy_dt:now(),
    NowInt = imboy_dt:rfc3339_to(Now),
    CurrentUid = maps:get(current_uid, State),
    % ?DEBUG_LOG([" Verified", Verified, "ExpiredAt2 ", ExpiredAt2, "Key ", Key, " Tk ", Tk, Now > ExpiredAt]),
    case {CurrentUid, Verified} of
        {undefined, _} ->
            Req = cowboy_req:reply(302, #{<<"Location">> => <<"http://www.imboy.pub">>}, Req0),
            {ok, Req, State};
        {_, false} ->
            Req = cowboy_req:reply(302, #{<<"Location">> => <<"http://www.imboy.pub">>}, Req0),
            {ok, Req, State};
        {_, true} when NowInt > ExpiredAtInt ->
            imboy_response:error(Req0, "验证码已过期");
        _ ->
            Gid2 = imboy_hashids:decode(Gid),
            % ?DEBUG_LOG(["Gid2", Gid2, "CurrentUid ", CurrentUid]),
            Column = <<"id,title,avatar,member_count, member_max">>,
            case group_repo:find_by_id(Gid2, Column) of
                {error, Reason} ->
                    imboy_response:error(Req0, Reason);
                G ->
                    Res = group_member_logic:join_group(<<"scan_qr_code">>,
                                                        CurrentUid,
                                                        Gid2,
                                                        #{max_members =>
                                                              maps:get(<<"member_max">>, G, 10),
                                                          current_count =>
                                                              maps:get(<<"member_count">>, G, 0)}),
                    % ?DEBUG_LOG(["Gid2", Gid2, "CurrentUid ", CurrentUid, " Res ", Res]),
                    case Res of
                        ok ->
                            case group_repo:find_by_id(Gid2, Column) of
                                {error, Reason2} ->
                                    imboy_response:error(Req0, Reason2);
                                G2 ->
                                    Gm = group_member_repo:find(Gid2, CurrentUid, <<"*">>),
                                    [Gm2] = group_member_transfer:member_list([Gm]),
                                    G3 = G#{<<"member_count">> := maps:get(<<"member_count">>, G2),
                                            <<"type">> => <<"group">>,
                                            <<"group_member">> => Gm2},
                                    % ?DEBUG_LOG(["Gid2", Gid2, "CurrentUid ", CurrentUid, " Res ", Res, " G3 ", group_logic:group_transfer(G3)]),
                                    imboy_response:success(Req0, group_logic:group_transfer(G3))
                            end
                    end
            end
    end.

%% ===================================================================
%% EUnit tests.
%% ===================================================================

page_transfer(Payload) ->
    K = <<"list">>,
    Li = maps:get(K, Payload, []),
    Li2 = [group_logic:group_transfer(M) || M <- Li],
    Payload#{K => Li2}.

msg_page_transfer(Payload) ->
    K = <<"list">>,
    Li = maps:get(K, Payload, []),
    Li2 = [group_logic:group_transfer(M) || M <- Li],
    Payload#{K => Li2}.
