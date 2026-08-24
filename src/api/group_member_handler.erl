-module(group_member_handler).
-dialyzer({nowarn_function, [payload_list_key/1]}).

-behavior(cowboy_rest).

-export([init/2]).

-include("log.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 初始化群组成员处理器
%% 根据请求中的 action 参数调用相应的处理函数
%%
%% @param Req0 Cowboy请求对象
%% @param State0 状态映射，包含 action 和 current_uid 等信息
%% @return {ok, Req1, State} 处理后的请求对象和状态
%% @end
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
            mute ->
                mute(Req0, State);
            unmute ->
                unmute(Req0, State);
            role ->
                role(Req0, State);
            % alias -> % 设置群内昵称
            %     alias(Req0, State);
            false ->
                Req0
        end,
    {ok, Req1, State}.

%% @doc 查询共同群组
%% 查询两个用户共同加入的群组列表
%%
%% @param Req0 Cowboy请求对象，包含两个用户ID
%% @param State 状态映射，包含 current_uid
%% @return 返回包含群组列表的响应
%% @end
-spec same_group(cowboy_req:req(), map()) -> cowboy_req:req().
same_group(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    Qs0 = cowboy_req:parse_qs(Req0),
    A = proplists:get_value(<<"uid1">>, Qs0, <<>>),
    B = proplists:get_value(<<"uid2">>, Qs0, <<>>),

    A1 = elib_cnv:safe_to_integer(A),
    B1 = elib_cnv:safe_to_integer(B),

    {Count, Li4} =
        if
            CurrentUid == A1; CurrentUid == B1 ->
                Li = group_member_logic:list_same_group(A1, B1),
                Column =
                    <<
                        "id as gid, type, join_limit, content_limit, owner_uid, creator_uid, "
                        "member_max, member_count, introduction, avatar, title, updated_at, "
                        "created_at"
                    >>,
                Li2 =
                    case group_member_logic:list_same_group_detail(Li, Column) of
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
    elib_response:success(Req0, #{<<"count">> => Count, <<"list">> => Li4}, "success.").

%% @doc 加入群组
%% 将指定用户加入到群组中
%%
%% @param Req0 Cowboy请求对象，包含群组ID和成员列表
%% @param State 状态映射，包含 current_uid
%% @return 返回包含成员列表的响应
%% @end
-spec join(cowboy_req:req(), map()) -> cowboy_req:req().
join(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    MemberUids = maps:get(<<"member_uids">>, PostVals, []),
    JoinMode = maps:get(<<"join_mode">>, PostVals, <<>>),
    Gid = maps:get(<<"gid">>, PostVals, 0),
    Gid2 = elib_cnv:safe_to_integer(Gid),
    JoinMode2 =
        case JoinMode of
            <<>> ->
                group_member_logic:build_invite_join_mode(CurrentUid);
            _ ->
                JoinMode
        end,
    %% 邀请他人入群必须自己先是群成员；自己加入自己（扫码 / 面对面入群）除外。
    %% 缺这道门则任何持 token 的用户都能把任意 uid 塞进任意有容量的群。
    IsSelfJoin =
        is_list(MemberUids) andalso
            [ec_cnv:to_binary(U) || U <- MemberUids] ==
                [ec_cnv:to_binary(CurrentUid)],
    case throttle:check(three_second_once, {group_member, CurrentUid}) of
        {limit_exceeded, _, _} ->
            elib_response:error(Req0, <<"在处理中，请稍后重试"/utf8>>);
        _ when Gid2 == 0 ->
            elib_response:error(Req0, <<"group id 格式有误"/utf8>>);
        _ when is_list(MemberUids) == false ->
            elib_response:error(Req0, <<"member_uids 必须是list"/utf8>>);
        _ when MemberUids == [] ->
            elib_response:error(Req0, <<"member_uids 不能为空"/utf8>>);
        %% 邀请他人入群必须自己先是群成员；自己加入自己（扫码 / 面对面入群）除外。
        %% 成员身份查询刻意放在**限流与参数校验之后**惰性求值：早先版本把它
        %% 提到 case 之前，导致被限流拒绝的请求也照打一次 DB 查询，
        %% 等于限流挡不住数据库放大。
        _ when not IsSelfJoin ->
            case is_group_member(Gid2, CurrentUid) of
                false ->
                    elib_response:error(Req0, <<"你不是群成员"/utf8>>);
                true ->
                    join_with_capacity(Req0, Gid, Gid2, MemberUids, JoinMode2)
            end;
        _ ->
            join_with_capacity(Req0, Gid, Gid2, MemberUids, JoinMode2)
    end.

%% @private 自己是否已在群内
is_group_member(Gid, Uid) ->
    Gid /= 0 andalso
        maps:size(group_member_logic:find_by_gid_and_uid(Gid, Uid, <<"id">>)) > 0.

%% @private 通过成员身份门之后的容量校验与入群
join_with_capacity(Req0, Gid, Gid2, MemberUids, JoinMode2) ->
    case group_member_logic:get_group_capacity(Gid2) of
        {error, _Reason} ->
            elib_response:error(Req0, <<"群组不存在"/utf8>>);
        G ->
            Max = maps:get(<<"member_max">>, G, 0),
            Count = maps:get(<<"member_count">>, G, 0),
            Len = length(MemberUids),
            Diff = Max - Count,
            if
                Diff == 0 ->
                    elib_response:error(Req0, <<"群成员已满。"/utf8>>);
                Len > Diff ->
                    elib_response:error(
                        Req0,
                        elib_cnv:implode(
                            <<>>,
                            [
                                <<"还可以加入"/utf8>>,
                                ec_cnv:to_binary(Diff),
                                <<"名群成员"/utf8>>
                            ]
                        )
                    );
                true ->
                    MemberUids2 = [elib_cnv:safe_to_integer(Id) || Id <- MemberUids],
                    MemberListRes = group_member_logic:list_member(Gid2, MemberUids2),
                    % ?DEBUG_LOG([MemberListRes]),
                    case MemberListRes of
                        {ok, []} ->
                            elib_pg:with_tx(fun(Conn) ->
                                [
                                    group_member_logic:join_group(
                                        Conn,
                                        JoinMode2,
                                        Uid2,
                                        Gid2,
                                        #{}
                                    )
                                 || Uid2 <- MemberUids2
                                ]
                            end),
                            {ok, MemberListRes2} =
                                group_member_logic:list_member(Gid2, MemberUids2),
                            Sum = group_member_logic:get_user_id_sum(Gid2),
                            elib_response:success(
                                Req0,
                                #{
                                    <<"gid">> => Gid,
                                    <<"user_id_sum">> => Sum,
                                    <<"member_list">> =>
                                        group_member_transfer:member_list(MemberListRes2)
                                },
                                "success."
                            );
                        {ok, MemberList} ->
                            % 已经是成员，直接使用查询结果
                            Sum = group_member_logic:get_user_id_sum(Gid2),
                            elib_response:success(
                                Req0,
                                #{
                                    <<"gid">> => Gid,
                                    <<"user_id_sum">> => Sum,
                                    <<"member_list">> =>
                                        group_member_transfer:member_list(MemberList)
                                },
                                "success."
                            )
                    end
            end
    end.

%% @doc 离开群组
%% 将指定用户从群组中移除
%%
%% @param Req0 Cowboy请求对象，包含群组ID和成员列表
%% @param State 状态映射，包含 current_uid
%% @return 返回成功响应
%% @end
-spec leave(cowboy_req:req(), map()) -> cowboy_req:req().
leave(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    Gid = maps:get(<<"gid">>, PostVals, 0),
    MemberUids = maps:get(<<"member_uids">>, PostVals, []),
    Gid2 = elib_cnv:safe_to_integer(Gid),
    case throttle:check(three_second_once, {group_member, CurrentUid}) of
        {limit_exceeded, _, _} ->
            elib_response:error(Req0, <<"在处理中，请稍后重试"/utf8>>);
        _ when Gid2 == 0 ->
            elib_response:error(Req0, <<"group id 格式有误"/utf8>>);
        _ ->
            [
                group_member_logic:leave(
                    elib_cnv:safe_to_integer(Uid), Gid2, CurrentUid
                )
             || Uid <- MemberUids
            ],
            elib_response:success(Req0, #{<<"gid">> => Gid}, "success.")
    end.

%% @doc 设置群内昵称
%% 设置用户在群组中的昵称和描述
%%
%% @param Req0 Cowboy请求对象，包含群组ID、昵称和描述
%% @param State 状态映射，包含 current_uid
%% @return 返回成功或错误响应
%% @end
-spec alias(cowboy_req:req(), map()) -> cowboy_req:req().
alias(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    Gid = maps:get(<<"gid">>, PostVals, 0),
    Gid2 = elib_cnv:safe_to_integer(Gid),
    case Gid2 of
        0 ->
            elib_response:error(Req0, <<"group id 必须"/utf8>>);
        _ ->
            Alias = maps:get(<<"alias">>, PostVals, <<>>),
            Description = maps:get(<<"description">>, PostVals, <<>>),
            case group_member_logic:alias(CurrentUid, Gid2, Alias, Description) of
                ok ->
                    elib_response:success(Req0, #{<<"gid">> => Gid}, "success.");
                {error, Reason} ->
                    elib_response:error(Req0, fmt_reason(Reason))
            end
    end.

%% @doc 群组成员分页
%% 获取群组成员列表（分页）
%%
%% @param Req0 Cowboy请求对象，包含群组ID和分页参数
%% @param State 状态映射，包含 current_uid
%% @return 返回包含成员列表的响应
%% @end
-spec page(cowboy_req:req(), map()) -> cowboy_req:req().
page(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    Qs1 = cowboy_req:parse_qs(Req0),
    Gid = proplists:get_value(<<"gid">>, Qs1, undefined),
    Gid2 = elib_cnv:safe_to_integer(Gid),
    GM = group_member_logic:find_by_gid_and_uid(Gid2, CurrentUid, <<"id">>),
    GMSize = maps:size(GM),
    case Gid2 of
        0 ->
            elib_response:error(Req0, <<"group id 必须"/utf8>>);
        _ when GMSize == 0 ->
            elib_response:error(Req0, <<"你不是群成员"/utf8>>);
        _ ->
            {Page, Size} = elib_param:page(Req0),
            Payload =
                case group_member_logic:page_with_user_info(Gid2, Page, Size) of
                    {ok, #{total := Total, list := Rows}} ->
                        Rows2 = group_member_transfer:member_list(Rows),
                        #{
                            total => Total,
                            page => Page,
                            size => Size,
                            list => Rows2
                        };
                    _ ->
                        #{
                            total => 0,
                            page => Page,
                            size => Size,
                            list => []
                        }
                end,
            elib_response:success(Req0, page_transfer(Payload))
    end.

%% @doc 群成员禁言
%% 禁言指定群成员
%%
%% @param Req0 Cowboy请求对象，包含群组ID、用户ID和禁言时长
%% @param State 状态映射，包含 current_uid
%% @return 返回成功或错误响应
%% @end
-spec mute(cowboy_req:req(), map()) -> cowboy_req:req().
mute(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    Gid = maps:get(<<"gid">>, PostVals, 0),
    UserId = maps:get(<<"user_id">>, PostVals, 0),
    Duration = elib_cnv:safe_to_integer(maps:get(<<"duration">>, PostVals, 0)),

    Gid2 = elib_cnv:safe_to_integer(Gid),
    UserId2 = elib_cnv:safe_to_integer(UserId),

    case throttle:check(three_second_once, {group_member_mute, CurrentUid}) of
        {limit_exceeded, _, _} ->
            elib_response:error(Req0, <<"在处理中，请稍后重试"/utf8>>);
        _ when Gid2 == 0 ->
            elib_response:error(Req0, <<"群组ID格式有误"/utf8>>);
        _ when UserId2 == 0 ->
            elib_response:error(Req0, <<"用户ID格式有误"/utf8>>);
        _ when Duration =< 0 ->
            elib_response:error(Req0, <<"禁言时长必须大于0"/utf8>>);
        _ ->
            case group_member_logic:mute(CurrentUid, Gid2, UserId2, Duration) of
                ok ->
                    elib_response:success(
                        Req0, #{<<"gid">> => Gid, <<"user_id">> => UserId}, "success."
                    );
                {error, Reason} ->
                    elib_response:error(Req0, fmt_reason(Reason))
            end
    end.

%% @doc 解除群成员禁言（slice-9b）
%% 对应 `POST /v1/group_member/unmute`，将目标成员的 `mute_until`
%% 置为 NULL 并广播 `group_member_mute` 通知（payload `mute_until == 0`
%% 作为解禁信号）。
%%
%% @param Req0 Cowboy请求对象，包含 gid / user_id
%% @param State 状态映射，包含 current_uid
%% @end
-spec unmute(cowboy_req:req(), map()) -> cowboy_req:req().
unmute(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    Gid = maps:get(<<"gid">>, PostVals, 0),
    UserId = maps:get(<<"user_id">>, PostVals, 0),

    Gid2 = elib_cnv:safe_to_integer(Gid),
    UserId2 = elib_cnv:safe_to_integer(UserId),

    case throttle:check(three_second_once, {group_member_unmute, CurrentUid}) of
        {limit_exceeded, _, _} ->
            elib_response:error(Req0, <<"在处理中，请稍后重试"/utf8>>);
        _ when Gid2 == 0 ->
            elib_response:error(Req0, <<"群组ID格式有误"/utf8>>);
        _ when UserId2 == 0 ->
            elib_response:error(Req0, <<"用户ID格式有误"/utf8>>);
        _ ->
            case group_member_logic:unmute(CurrentUid, Gid2, UserId2) of
                ok ->
                    elib_response:success(
                        Req0, #{<<"gid">> => Gid, <<"user_id">> => UserId}, "success."
                    );
                {error, Reason} ->
                    elib_response:error(Req0, fmt_reason(Reason))
            end
    end.

%% @doc 更新群成员角色
%% 更新指定群成员的角色
%%
%% @param Req0 Cowboy请求对象，包含群组ID、用户ID和角色
%% @param State 状态映射，包含 current_uid
%% @return 返回成功或错误响应
%% @end
-spec role(cowboy_req:req(), map()) -> cowboy_req:req().
role(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    Gid = maps:get(<<"gid">>, PostVals, 0),
    UserId = maps:get(<<"user_id">>, PostVals, 0),
    Role = elib_cnv:safe_to_integer(maps:get(<<"role">>, PostVals, 0)),

    Gid2 = elib_cnv:safe_to_integer(Gid),
    UserId2 = elib_cnv:safe_to_integer(UserId),

    case throttle:check(three_second_once, {group_member_role, CurrentUid}) of
        {limit_exceeded, _, _} ->
            elib_response:error(Req0, <<"在处理中，请稍后重试"/utf8>>);
        _ when Gid2 == 0 ->
            elib_response:error(Req0, <<"群组ID格式有误"/utf8>>);
        _ when UserId2 == 0 ->
            elib_response:error(Req0, <<"用户ID格式有误"/utf8>>);
        _ when Role < 1 orelse Role > 3 ->
            elib_response:error(Req0, <<"角色值必须在1-3之间"/utf8>>);
        _ ->
            case group_member_logic:update_role(CurrentUid, Gid2, UserId2, Role) of
                ok ->
                    elib_response:success(
                        Req0, #{<<"gid">> => Gid, <<"user_id">> => UserId}, "success."
                    );
                {error, Reason} ->
                    elib_response:error(Req0, fmt_reason(Reason))
            end
    end.

%% @doc 转换群组成员分页数据
%% 将群组成员数据进行编码转换
%%
%% @param Payload 原始分页数据
%% @return 转换后的分页数据
%% @end
-spec page_transfer(map()) -> map().
page_transfer(Payload) ->
    Key = payload_list_key(Payload),
    Li = maps:get(Key, Payload, []),
    Li2 = group_member_transfer:member_list(Li),
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

%% @doc 错误原因安全转文案：binary（logic 层已中文化的业务消息，
%% 如「你没有权限禁言群成员」）直接透传；atom / 复杂 term
%% （如 epgsql 错误元组）记录日志后兜底为通用中文，
%% 避免英文码和 term dump 直达用户。
%% 与 group_vote_handler:fmt_reason/1 同一约定。
-spec fmt_reason(term()) -> binary().
fmt_reason(Reason) when is_binary(Reason) ->
    Reason;
fmt_reason(Reason) ->
    ?ERROR_LOG([<<"group_member_handler op failed">>, Reason]),
    <<"操作失败，请稍后重试"/utf8>>.
