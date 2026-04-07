-module(group_handler).
%% Thin HTTP adapter for the core group_collab boundary.
%% Keep transport parsing here and route domain work through group_logic.

-dialyzer({nowarn_function, [validate_gid/1, process_group_edit/5, edit/2, handle_action/3]}).

-behavior(cowboy_rest).

-export([init/2]).
-export([handle_action/3]).

-include("log.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 初始化群组处理器
%% 根据请求中的 action 参数调用相应的处理函数
%%
%% @param Req0 Cowboy请求对象
%% @param State0 状态映射，包含 action 和 current_uid 等信息
%% @return {ok, Req1, State} 处理后的请求对象和状态
%% @end
-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 = handle_action(Action, Req0, State),
    {ok, Req1, State}.

%% @doc Action 分发处理
%% 将不同的 action 分发到对应的处理函数
-spec handle_action(atom() | false, cowboy_req:req(), map()) -> cowboy_req:req().
handle_action(face2face, Req, State) -> face2face(Req, State);
handle_action(face2face_save, Req, State) -> face2face_save(Req, State);
handle_action(add, Req, State) -> add(Req, State);
handle_action(edit, Req, State) -> edit(Req, State);
handle_action(dissolve, Req, State) -> dissolve(Req, State);
handle_action(detail, Req, State) -> detail(Req, State);
handle_action(transfer, Req, State) -> transfer(Req, State);
handle_action(page, Req, State) ->
    Qs0 = cowboy_req:parse_qs(Req),
    Attr = proplists:get_value(<<"attr">>, Qs0, undefined),
    page(Req, State, Attr);
handle_action(msg_page, Req, State) -> msg_page(Req, State);
handle_action(qrcode, Req, State) -> qrcode(Req, State);
handle_action(remark, Req, State) -> remark(Req, State);
handle_action(false, Req, _State) -> Req.

%% @doc 获取群组详情
%% 查询指定群组的详细信息
%%
%% @param Req0 Cowboy请求对象，包含群组ID
%% @param _State 状态映射
%% @return 返回包含群组详情的响应
%% @end
-spec detail(cowboy_req:req(), map()) -> cowboy_req:req().
detail(Req0, _State) ->
    Qs1 = cowboy_req:parse_qs(Req0),
    Gid = proplists:get_value(<<"gid">>, Qs1, <<>>),
    % 【优化】使用统一的 ID 验证函数
    case imboy_error:validate_id(Req0, Gid) of
        {error, Req} -> Req;
        {ok, Gid2} ->
            case group_repo:find_by_id(Gid2, <<"*">>) of
                {error, _Reason} ->
                    elib_response:error(Req0, "群组不存在");
                G ->
                    elib_response:success(Req0, group_logic:group_transfer(G), "success.")
            end
    end.

%% @doc 面对面建群
%% 通过面对面建群功能创建群组
%%
%% @param Req0 Cowboy请求对象，包含位置信息和验证码
%% @param State 状态映射，包含 current_uid
%% @return 返回包含群组信息的响应
%% @end
-spec face2face(cowboy_req:req(), map()) -> cowboy_req:req().
face2face(Req0, State) ->
    Qs2 = cowboy_req:parse_qs(Req0),
    Lng = proplists:get_value(<<"longitude">>, Qs2, undefined),
    Lat = proplists:get_value(<<"latitude">>, Qs2, undefined),
    Code = proplists:get_value(<<"code">>, Qs2, <<>>),
    Uid = maps:get(current_uid, State),
    case throttle:check(three_second_once, Uid) of
        {limit_exceeded, _, _} ->
            elib_response:error(Req0, "在处理中，请稍后重试");
        _ ->
            case group_logic:face2face(Uid, Code, Lng, Lat) of
                {ok, Gid} ->
                    Gid2 = Gid,
                    ToUidLi = group_ds:member_uids(Gid),
                    % 修复：如果成员列表为空，至少包含创建者，确保通知能发送
                    ToUidLi2 = case ToUidLi of
                        [] -> [Uid];
                        _ -> ToUidLi
                    end,
                    User = user_repo:find_by_id(Uid, <<"account,avatar,nickname">>),
                    %% v2.0: 使用 send/7 API
                    Action = <<"group_member_join">>,
                    Payload =
                        #{<<"gid">> => Gid2,
                          <<"user_id_sum">> => lists:sum(ToUidLi2),
                          <<"nickname">> => maps:get(<<"nickname">>, User),
                          <<"avatar">> => maps:get(<<"avatar">>, User),
                          <<"account">> => maps:get(<<"account">>, User)},
                    msg_s2c_ds:send(Uid, ToUidLi2, Action, <<>>, null, Payload, save),

                    MemberListRes =
                        user_repo:list_by_ids(ToUidLi, <<"id as user_id,account,avatar,nickname">>),
                    MemberList =
                        case MemberListRes of
                            {ok, L} ->
                                L;
                            _ ->
                                []
                        end,
                    elib_response:success(Req0,
                                           #{gid => Gid2,
                                             member_list =>
                                                 group_member_transfer:member_list(MemberList)},
                                           "success.");
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% @doc 面对面建群保存
%% 保存面对面建群的验证码和群组信息
%%
%% @param Req0 Cowboy请求对象，包含验证码和群组ID
%% @param State 状态映射，包含 current_uid
%% @return 返回包含群组信息和成员列表的响应
%% @end
-spec face2face_save(cowboy_req:req(), map()) -> cowboy_req:req().
face2face_save(Req0, State) ->
    PostVals = elib_param:post(Req0),
    Code = maps:get(<<"code">>, PostVals, []),
    Gid = maps:get(<<"gid">>, PostVals, []),
    Uid = maps:get(current_uid, State),
    Gid2 = ec_cnv:to_integer(Gid),
    case group_logic:face2face_save(Code, Gid2, Uid) of
        {ok, _} ->
            case group_member_logic:list_member(Gid2) of
                {error, Reason} ->
                    elib_response:error(Req0, Reason);
                {ok, MemberList} ->
                    case group_repo:find_by_id(Gid2, <<"*">>) of
                        {error, Reason2} ->
                            elib_response:error(Req0, Reason2);
                        G2 ->
                            elib_response:success(Req0,
                                                   #{group => group_logic:group_transfer(G2),
                                                     member_list =>
                                                         group_member_transfer:member_list(MemberList)},
                                                   "success.")
                    end
            end
    end.

%% @doc 创建群组
%% 创建一个新的群组
%%
%% @param Req0 Cowboy请求对象，包含群组信息和成员列表
%% @param State 状态映射，包含 current_uid
%% @return 返回包含群组信息的响应
%% @end
-spec add(cowboy_req:req(), map()) -> cowboy_req:req().
add(Req0, State) ->
    Uid = maps:get(current_uid, State),
    Type = 2, % 类型: 1 公开群组  2 私有群组
    case throttle:check(three_second_once, Uid) of
        {limit_exceeded, _, _} ->
            elib_response:error(Req0, "在处理中，请稍后重试");
        _ ->
            Count =
                elib_pg:pluck_value(
                    group_repo:tablename(), <<"count(*)">>, #{status => 1, owner_uid => Uid}, 0),
            PostVals = elib_param:post(Req0),
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
                            elib_response:error(Req0, Reason);
                        GData ->
                            GData3 = GData,
                            case group_member_logic:list_member(Gid) of
                                {error, Reason2} ->
                                    elib_response:error(Req0, Reason2);
                                {ok, MemberList} ->
                                    elib_response:success(Req0,
                                                           #{group => GData3,
                                                             member_list =>
                                                                 group_member_transfer:member_list(MemberList)},
                                                           "success.")
                            end
                    end;
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% @doc 编辑群组信息
%% 修改群组的标题、头像、简介等信息
%%
%% @param Req0 Cowboy请求对象，包含群组ID和要修改的信息
%% @param State 状态映射，包含 current_uid
%% @return 返回成功或错误响应
%% @end
-spec edit(cowboy_req:req(), map()) -> cowboy_req:req().
edit(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    Gid = maps:get(<<"gid">>, PostVals, 0),
    Gid2 = ec_cnv:to_integer(Gid),

    % 构建更新数据
    Data = build_group_update_data(PostVals),

    case validate_gid(Gid2) of
        {error, Msg} ->
            elib_response:error(Req0, Msg);
        ok ->
            process_group_edit(Req0, Uid, Gid, Gid2, Data)
    end.

%% @doc 构建群组更新数据
-spec build_group_update_data(map()) -> map().
build_group_update_data(PostVals) ->
    Fields = [<<"title">>, <<"avatar">>, <<"introduction">>],
    lists:foldl(fun(Field, Acc) ->
        case maps:get(Field, PostVals, undefined) of
            undefined -> Acc;
            Val -> Acc#{Field => Val}
        end
    end, #{}, Fields).

%% @doc 验证群组 ID
-spec validate_gid(integer()) -> ok | {error, binary()}.
validate_gid(0) -> {error, "group id 必须"};
validate_gid(Gid) when Gid > 0 -> ok;
validate_gid(_) -> {error, "group id 格式有误"}.

%% @doc 处理群组编辑
-spec process_group_edit(cowboy_req:req(), integer(), binary(), integer(), map()) -> cowboy_req:req().
process_group_edit(Req0, Uid, Gid, Gid2, Data) ->
    Now = elib_dt:now(),
    Tb = group_repo:tablename(),
    Count = elib_pg:pluck_value(Tb, <<"count(*)">>, #{id => Gid2}, 0),
    case Count > 0 of
        true ->
            % 更新现有群组
            case elib_pg:update(Tb, Data#{updated_at => Now}, <<"id = $1">>, [Gid2]) of
                {ok, _} ->
                    ToUidLi = group_ds:member_uids(Gid2),
                    Action = <<"group_edit">>,
                    Payload = Data#{<<"gid">> => Gid},
                    msg_s2c_ds:send(Uid, ToUidLi, Action, <<>>, null, Payload, save);
                {error, UpdateReason} ->
                    ?ERROR_LOG({group_edit_update_failed, Gid2, UpdateReason})
            end;
        false ->
            % 创建新群组
            M3 = group_random_code_repo:find_by_gid(Gid2, <<"user_id, created_at">>),
            Data4 = Data#{owner_uid => maps:get(<<"user_id">>, M3, Uid),
                          creator_uid => maps:get(<<"user_id">>, M3, Uid),
                          created_at => maps:get(<<"created_at">>, M3, Now),
                          id => Gid2},
            case elib_pg:insert(Tb, Data4) of
                {ok, _} -> ok;
                {error, InsertReason} ->
                    ?ERROR_LOG({group_create_insert_failed, Gid2, InsertReason})
            end
    end,
    elib_response:success(Req0, #{<<"gid">> => Gid}, "success.").

%% @doc 解散群组
%% 解散指定的群组（仅群主可操作）
%%
%% @param Req0 Cowboy请求对象，包含群组ID
%% @param State 状态映射，包含 current_uid
%% @return 返回成功或错误响应
%% @end
-spec dissolve(cowboy_req:req(), map()) -> cowboy_req:req().
dissolve(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    Gid = maps:get(<<"gid">>, PostVals, 0),
    Gid2 = ec_cnv:to_integer(Gid),
    case throttle:check(per_hour_once, {group, Gid2}) of
        {limit_exceeded, _, _} ->
            elib_response:error(Req0, "在处理中，请稍后重试");
        _ when Gid2 == 0 ->
            elib_response:error(Req0, "group id 必须");
        _ when Gid2 > 0 ->
            case group_repo:find_by_id(Gid2, <<"*">>) of
                {error, _Reason} ->
                    elib_response:error(Req0, "群组不存在");
                G ->
                    OwnerUid = maps:get(<<"owner_uid">>, G, 0),
                    % ?DEBUG_LOG(["OwnerUid", OwnerUid, "uid", CurrentUid, G]),
                    case group_logic:dissolve(CurrentUid, Gid2, OwnerUid, G) of
                        ok ->
                            elib_response:success(Req0, #{<<"gid">> => Gid}, "success.");
                        {error, Msg} ->
                            elib_response:error(Req0, Msg)
                    end
            end;
        _ ->
            elib_response:error(Req0, "group id 格式有误")
    end.

%% @doc 我拥有的群
%% 获取当前用户创建的群组列表（分页）
%%
%% @param Req0 Cowboy请求对象，包含分页参数
%% @param State 状态映射，包含 current_uid
%% @return 返回包含群组列表的响应
%% @end
-spec page(cowboy_req:req(), map(), term()) -> cowboy_req:req().
page(Req0, State, <<"owner">>) ->
    CurrentUid = auth_ds:current_uid(State),
    {Page, Size} = elib_param:page(Req0),

    Where = #{status => 1, owner_uid => CurrentUid},

    Tb = group_repo:tablename(),
    Payload =
        case elib_pg:page_with_total(Tb, Where, Page, Size) of
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
    elib_response:success(Req0, page_transfer(Payload));
%% 我加入的群
page(Req0, State, <<"join">>) ->
    CurrentUid = auth_ds:current_uid(State),
    {Page, Size} = elib_param:page(Req0),

    Where =
        #{<<"g.status">> => 1,
          <<"m.status">> => 1,
          <<"m.user_id">> => CurrentUid,
          <<"g.owner_uid">> => {op, <<"!=">>, CurrentUid}},
    GTb = group_repo:tablename(),
    MTb = group_member_repo:tablename(),
    Tb = <<GTb/binary, " g LEFT JOIN ", MTb/binary, " m ON g.id = m.group_id">>,
    Payload =
        case elib_pg:page_with_total(Tb,
                                     <<"g.*">>,
                                     Where,
                                     <<"g.id desc">>,
                                     Page,
                                     Size)
        of
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
    elib_response:success(Req0, page_transfer(Payload));
%% 我管理的群（群主/副群主/管理员）
page(Req0, State, <<"manager">>) ->
    CurrentUid = auth_ds:current_uid(State),
    {Page, Size} = elib_param:page(Req0),

    Where =
        #{<<"g.status">> => 1,
          <<"__or">> =>
              [#{<<"g.owner_uid">> => CurrentUid},
               #{<<"m.status">> => 1,
                 <<"m.user_id">> => CurrentUid,
                 <<"m.role">> => {op, <<">=">>, 3}}]},
    GTb = group_repo:tablename(),
    MTb = group_member_repo:tablename(),
    Tb = <<GTb/binary, " g LEFT JOIN ", MTb/binary, " m ON g.id = m.group_id">>,
    Payload =
        case elib_pg:page_with_total(Tb,
                                     <<"g.*">>,
                                     Where,
                                     <<"g.id desc">>,
                                     Page,
                                     Size)
        of
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
    elib_response:success(Req0, page_transfer(Payload));
%% 兜底：未传 attr 或传入未知 attr 时，默认返回 owner 视图
page(Req0, State, _Attr) ->
    page(Req0, State, <<"owner">>).

%% @doc 群组消息分页
%% 获取群组的消息列表（分页）
%%
%% @param Req0 Cowboy请求对象，包含群组ID和分页参数
%% @param State 状态映射，包含 current_uid
%% @return 返回包含消息列表的响应
%% @end
-spec msg_page(cowboy_req:req(), map()) -> cowboy_req:req().
msg_page(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    Qs3 = cowboy_req:parse_qs(Req0),
    Gid = proplists:get_value(<<"gid">>, Qs3, undefined),
    Gid2 = ec_cnv:to_integer(Gid),
    GM = group_member_repo:find(Gid2, CurrentUid, <<"id">>),
    GMSize = maps:size(GM),
    Where0 = #{to_groupid => Gid2},
    Where =
        case elib_param:int(last_time, Req0, 0) of
            {ok, Last} when Last > 0 ->
                Where0#{created_at => {op, <<">=">>, elib_dt:to_rfc3339(Last)}};
            _ ->
                Where0
        end,
    case Gid2 of
        0 ->
            elib_response:error(Req0, "group id 必须");
        _ when GMSize == 0 ->
            elib_response:error(Req0, "你不是群成员");
        _ ->
            {Page, Size} = elib_param:page(Req0),
            Tb = msg_c2g_repo:tablename(),

            Payload =
                case elib_pg:page_with_total(Tb, Where, Page, Size) of
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
            elib_response:success(Req0, msg_page_transfer(Payload))
    end.

%% @doc 群转让
%% 将群组转让给指定的新群主
%%
%% @param Req0 Cowboy请求对象，包含群组ID和新群主ID
%% @param State 状态映射，包含 current_uid
%% @return 返回成功或错误响应
%% @end
-spec transfer(cowboy_req:req(), map()) -> cowboy_req:req().
transfer(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    Gid = maps:get(<<"gid">>, PostVals, 0),
    Gid2 = ec_cnv:to_integer(Gid),
    NewOwnerUid = maps:get(<<"new_owner_uid">>, PostVals, 0),
    NewOwnerUid2 = ec_cnv:to_integer(NewOwnerUid),

    case throttle:check(per_hour_once, {group_transfer, Gid2}) of
        {limit_exceeded, _, _} ->
            elib_response:error(Req0, "在处理中，请稍后重试");
        _ when Gid2 == 0 ->
            elib_response:error(Req0, "group id 必须");
        _ when NewOwnerUid2 == 0 ->
            elib_response:error(Req0, "新群主 id 必须");
        _ when Gid2 > 0, NewOwnerUid2 > 0 ->
            case group_logic:transfer(CurrentUid, Gid2, NewOwnerUid2) of
                ok ->
                    elib_response:success(Req0, #{<<"gid">> => Gid}, "success.");
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% @doc 扫描群二维码
%% 通过扫描群二维码加入群组
%%
%% @param Req0 Cowboy请求对象，包含群组ID和验证信息
%% @param State 状态映射，包含 current_uid
%% @return 返回包含群组信息的响应
%% @end
-spec qrcode(cowboy_req:req(), map()) -> cowboy_req:req().
qrcode(Req0, State) ->
    Qs4 = cowboy_req:parse_qs(Req0),
    Gid = proplists:get_value(<<"id">>, Qs4, undefined),
    ExpiredAt = proplists:get_value(<<"exp">>, Qs4, undefined),
    Tk = proplists:get_value(<<"tk">>, Qs4, undefined),

    Key = config_ds:get(<<"solidified_key">>),
    ExpiredAt2 = ec_cnv:to_binary(ExpiredAt),
    ExpiredAtInt = binary_to_integer(ExpiredAt2),
    Verified =
        elib_hasher:md5(<<ExpiredAt2/binary, "_", (ec_cnv:to_binary(Key))/binary>>) == Tk,
    Now = elib_dt:now(),
    NowInt = elib_dt:rfc3339_to(Now),
    CurrentUid = auth_ds:current_uid(State),
    % ?DEBUG_LOG([" Verified", Verified, "ExpiredAt2 ", ExpiredAt2, "Key ", Key, " Tk ", Tk, Now > ExpiredAt]),
    case {CurrentUid, Verified} of
        {0, _} ->
            Req = cowboy_req:reply(302, #{<<"Location">> => config_ds:env(redirect_url, <<"http://www.imboy.pub">>)}, Req0),
            {ok, Req, State};
        {_, false} ->
            Req = cowboy_req:reply(302, #{<<"Location">> => config_ds:env(redirect_url, <<"http://www.imboy.pub">>)}, Req0),
            {ok, Req, State};
        {_, true} when NowInt > ExpiredAtInt ->
            elib_response:error(Req0, "验证码已过期");
        _ ->
            Gid2 = ec_cnv:to_integer(Gid),
            % ?DEBUG_LOG(["Gid2", Gid2, "CurrentUid ", CurrentUid]),
            Column = <<"id,title,avatar,member_count, member_max">>,
            case group_repo:find_by_id(Gid2, Column) of
                {error, Reason} ->
                    elib_response:error(Req0, Reason);
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
                                    elib_response:error(Req0, Reason2);
                                G2 ->
                                    Gm = group_member_repo:find(Gid2, CurrentUid, <<"*">>),
                                    [Gm2] = group_member_transfer:member_list([Gm]),
                                    G3 = G#{<<"member_count">> := maps:get(<<"member_count">>, G2),
                                            <<"type">> => <<"group">>,
                                            <<"group_member">> => Gm2},
                                    % ?DEBUG_LOG(["Gid2", Gid2, "CurrentUid ", CurrentUid, " Res ", Res, " G3 ", group_logic:group_transfer(G3)]),
                                    elib_response:success(Req0, group_logic:group_transfer(G3))
                            end
                    end
            end
    end.

%% @doc 获取或更新群备注（仅自己可见）
%% GET  /v1/group/remark?gid=<gid>       → 返回当前用户对该群的备注
%% POST /v1/group/remark {gid, remark}   → 更新备注
%%
%% @param Req0 Cowboy请求对象
%% @param State 状态映射，包含 current_uid
%% @return 返回备注信息或成功响应
%% @end
-spec remark(cowboy_req:req(), map()) -> cowboy_req:req().
remark(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    Method = cowboy_req:method(Req0),
    remark(Method, Req0, State, CurrentUid).

remark(<<"GET">>, Req0, _State, CurrentUid) ->
    Qs = cowboy_req:parse_qs(Req0),
    Gid = proplists:get_value(<<"gid">>, Qs, <<>>),
    Gid2 = ec_cnv:to_integer(Gid),
    case Gid2 of
        0 ->
            elib_response:error(Req0, <<"group id 必须"/utf8>>);
        _ ->
            Member = group_member_repo:find(Gid2, CurrentUid, <<"remark">>),
            Remark = maps:get(<<"remark">>, Member, <<"">>),
            elib_response:success(Req0, #{<<"remark">> => Remark, <<"gid">> => Gid}, "success.")
    end;
remark(<<"POST">>, Req0, _State, CurrentUid) ->
    PostVals = elib_param:post(Req0),
    Gid = maps:get(<<"gid">>, PostVals, 0),
    Gid2 = ec_cnv:to_integer(Gid),
    case Gid2 of
        0 ->
            elib_response:error(Req0, <<"group id 必须"/utf8>>);
        _ ->
            Remark = maps:get(<<"remark">>, PostVals, <<"">>),
            % 验证是否为群成员
            Member = group_member_repo:find(Gid2, CurrentUid, <<"id">>),
            case maps:size(Member) of
                0 ->
                    elib_response:error(Req0, <<"你不是该群成员"/utf8>>);
                _ ->
                    Tb = group_member_repo:tablename(),
                    WhereSql = <<" WHERE group_id = $1 AND user_id = $2">>,
                    case elib_pg:update(Tb, #{<<"remark">> => Remark}, WhereSql, [Gid2, CurrentUid]) of
                        {ok, _} ->
                            elib_response:success(Req0, #{<<"gid">> => Gid}, "success.");
                        {error, Reason} ->
                            ?ERROR_LOG(["group remark update error: ", Reason]),
                            elib_response:error(Req0, <<"更新失败"/utf8>>)
                    end
            end
    end;
remark(_, Req0, _State, _CurrentUid) ->
    Req0.

%% ===================================================================
%% EUnit tests.
%% ===================================================================

%% @doc 转换群组分页数据
%% 将群组数据进行编码转换
%%
%% @param Payload 原始分页数据
%% @return 转换后的分页数据
%% @end
-spec page_transfer(map()) -> map().
page_transfer(Payload) ->
    Key = payload_list_key(Payload),
    Li = maps:get(Key, Payload, []),
    Li2 = [group_logic:group_transfer(M) || M <- Li],
    Payload#{Key => Li2}.

%% @doc 转换群组消息分页数据
%% 将群组消息数据进行编码转换
%%
%% @param Payload 原始分页数据
%% @return 转换后的分页数据
%% @end
-spec msg_page_transfer(map()) -> map().
msg_page_transfer(Payload) ->
    Key = payload_list_key(Payload),
    Li = maps:get(Key, Payload, []),
    Li2 = [group_logic:group_transfer(M) || M <- Li],
    Payload#{Key => Li2}.

-spec payload_list_key(map()) -> list | binary().
payload_list_key(Payload) ->
    case maps:is_key(list, Payload) of
        true ->
            list;
        false ->
            case maps:is_key(<<"list">>, Payload) of
                true -> <<"list">>;
                false -> list
            end
    end.
