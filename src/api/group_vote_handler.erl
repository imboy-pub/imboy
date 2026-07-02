-module(group_vote_handler).
-dialyzer({nowarn_function, [create/2, cancel/2, my_vote/2]}).
%% Thin HTTP adapter for the group_collab vote boundary.
%% Plugin-gated vote actions should stay behind this handler and group_vote_logic.

-behavior(cowboy_rest).

-export([init/2]).
-export([handle_action/3]).

-include("log.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 初始化群投票处理器
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
    Req1 =
        case imboy_plugin_registry:required_feature(api, group_vote_handler, Action) of
            undefined ->
                handle_action(Action, Req0, State);
            Feature ->
                case imboy_feature:ensure_enabled(Req0, Feature) of
                    ok ->
                        handle_action(Action, Req0, State);
                    {error, RespReq} ->
                        RespReq
                end
        end,
    {ok, Req1, State}.

%% @doc Action 分发处理
%% 将不同的 action 分发到对应的处理函数
-spec handle_action(atom() | false, cowboy_req:req(), map()) -> cowboy_req:req().
handle_action(create, Req, State) -> create(Req, State);
handle_action(list, Req, State) -> list(Req, State);
handle_action(detail, Req, State) -> detail(Req, State);
handle_action(cast, Req, State) -> cast(Req, State);
handle_action(update, Req, State) -> update(Req, State);
handle_action(cancel, Req, State) -> cancel(Req, State);
handle_action(close, Req, State) -> close(Req, State);
handle_action(my_vote, Req, State) -> my_vote(Req, State);
handle_action(false, Req, _State) -> Req.

%% @doc 创建投票
%% 创建一个新的群投票
%%
%% @param Req0 Cowboy请求对象，包含投票信息
%% @param State 状态映射，包含 current_uid
%% @return 返回包含投票信息的响应
%% @end
-spec create(cowboy_req:req(), map()) -> cowboy_req:req().
create(Req0, State) ->
    PostVals = elib_param:post(Req0),
    Gid = maps:get(<<"gid">>, PostVals, <<>>),
    Gid2 = elib_cnv:safe_to_integer(Gid),
    Uid = maps:get(current_uid, State),

    Title = maps:get(<<"title">>, PostVals, <<>>),
    Description = maps:get(<<"description">>, PostVals, <<>>),
    Options = maps:get(<<"options">>, PostVals, []),
    VoteType = elib_cnv:safe_to_integer(maps:get(<<"vote_type">>, PostVals, 1)),
    IsAnonymous = to_boolean(maps:get(<<"is_anonymous">>, PostVals, false), false),
    EndAt = maps:get(<<"end_at">>, PostVals, undefined),

    % 验证必填参数
    case {Gid2, Title} of
        {0, _} ->
            elib_response:error(Req0, "gid 必须");
        {_, <<>>} ->
            elib_response:error(Req0, "title 必须");
        {_, _} when VoteType < 1; VoteType > 2 ->
            elib_response:error(Req0, "vote_type 必须是 1 或 2");
        {_, _} when Options =:= [] ->
            elib_response:error(Req0, "options 必须");
        {_, _} ->
            Extra = #{
                vote_type => VoteType,
                is_anonymous => IsAnonymous,
                description => Description,
                end_at => EndAt
            },
            case group_vote_logic:create_vote(Gid2, Uid, Title, Options, Extra, #{}) of
                {ok, Vote} ->
                    elib_response:success(Req0, Vote, <<"创建投票成功"/utf8>>);
                {error, {missing_param, Field}} ->
                    elib_response:error(Req0, <<"缺少必填参数: ", (ec_cnv:to_binary(Field))/binary>>);
                {error, {invalid_param, Reason}} ->
                    elib_response:error(Req0, <<"参数错误: ", (ec_cnv:to_binary(Reason))/binary>>);
                {error, not_group_member} ->
                    elib_response:error(Req0, "您不是该群成员");
                {error, Reason} ->
                    elib_response:error(Req0, ec_cnv:to_binary(Reason))
            end
    end.

%% @doc 查询群投票列表
%% 获取指定群的投票列表（分页）
%%
%% @param Req0 Cowboy请求对象，包含群组ID和分页参数
%% @param State 状态映射，包含 current_uid
%% @return 返回包含投票列表的响应
%% @end
-spec list(cowboy_req:req(), map()) -> cowboy_req:req().
list(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    Qs = cowboy_req:parse_qs(Req0),
    Gid = proplists:get_value(<<"gid">>, Qs, <<>>),
    Gid2 = elib_cnv:safe_to_integer(Gid),
    {Page, Size} = elib_param:page(Req0),

    case Gid2 of
        0 ->
            elib_response:error(Req0, "gid 必须");
        _ ->
            case group_vote_logic:list_votes(Gid2, CurrentUid, Page, Size) of
                {ok, Votes} ->
                    elib_response:success(Req0, Votes, "success.");
                {error, permission_denied} ->
                    elib_response:error(Req0, "无权限查看该群投票");
                {error, Reason} ->
                    elib_response:error(Req0, ec_cnv:to_binary(Reason))
            end
    end.

%% @doc 查询投票详情
%% 查询指定投票的详细信息
%%
%% @param Req0 Cowboy请求对象，包含投票ID
%% @param _State 状态映射
%% @return 返回包含投票详情的响应
%% @end
-spec detail(cowboy_req:req(), map()) -> cowboy_req:req().
detail(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    Qs = cowboy_req:parse_qs(Req0),
    VoteId = proplists:get_value(<<"vote_id">>, Qs, <<>>),

    case VoteId of
        <<>> ->
            elib_response:error(Req0, "vote_id 必须");
        _ ->
            case group_vote_logic:get_vote_detail(VoteId, CurrentUid) of
                {ok, VoteDetail} ->
                    elib_response:success(Req0, VoteDetail, "success.");
                {error, vote_not_found} ->
                    elib_response:error(Req0, "投票不存在");
                {error, permission_denied} ->
                    elib_response:error(Req0, "无权限查看该投票");
                {error, Reason} ->
                    elib_response:error(Req0, ec_cnv:to_binary(Reason))
            end
    end.

%% @doc 投票
%% 对指定投票进行投票
%%
%% @param Req0 Cowboy请求对象，包含投票ID和选项ID
%% @param State 状态映射，包含 current_uid
%% @return 返回成功或错误响应
%% @end
-spec cast(cowboy_req:req(), map()) -> cowboy_req:req().
cast(Req0, State) ->
    PostVals = elib_param:post(Req0),
    VoteId = maps:get(<<"vote_id">>, PostVals, <<>>),
    OptionIds = maps:get(<<"option_ids">>, PostVals, []),
    Uid = maps:get(current_uid, State),

    case {VoteId, OptionIds} of
        {<<>>, _} ->
            elib_response:error(Req0, "vote_id 必须");
        {_, []} ->
            elib_response:error(Req0, "option_ids 必须");
        {_, _} ->
            case group_vote_logic:cast_vote(VoteId, Uid, OptionIds) of
                {ok, Vote} ->
                    elib_response:success(Req0, Vote, <<"投票成功"/utf8>>);
                {error, vote_not_found} ->
                    elib_response:error(Req0, "投票不存在");
                {error, vote_is_closed} ->
                    elib_response:error(Req0, "投票已结束");
                {error, vote_is_expired} ->
                    elib_response:error(Req0, "投票已过期");
                {error, already_voted} ->
                    elib_response:error(Req0, "您已投过票");
                {error, invalid_option} ->
                    elib_response:error(Req0, "无效的选项");
                {error, single_choice_requires_one_option} ->
                    elib_response:error(Req0, "单选投票只能选择一个选项");
                {error, multiple_choice_requires_at_least_two_options} ->
                    elib_response:error(Req0, "多选投票至少选择两个选项");
                {error, not_group_member} ->
                    elib_response:error(Req0, "您不是该群成员");
                {error, Reason} ->
                    elib_response:error(Req0, ec_cnv:to_binary(Reason))
            end
    end.

%% @doc 修改投票
%% 修改已投的选项
%%
%% @param Req0 Cowboy请求对象，包含投票ID和新选项ID
%% @param State 状态映射，包含 current_uid
%% @return 返回成功或错误响应
%% @end
-spec update(cowboy_req:req(), map()) -> cowboy_req:req().
update(Req0, State) ->
    PostVals = elib_param:post(Req0),
    VoteId = maps:get(<<"vote_id">>, PostVals, <<>>),
    OptionIds = maps:get(<<"option_ids">>, PostVals, []),
    Uid = maps:get(current_uid, State),

    case {VoteId, OptionIds} of
        {<<>>, _} ->
            elib_response:error(Req0, "vote_id 必须");
        {_, []} ->
            elib_response:error(Req0, "option_ids 必须");
        {_, _} ->
            case group_vote_logic:update_vote(VoteId, Uid, OptionIds) of
                {ok, Vote} ->
                    elib_response:success(Req0, Vote, <<"修改投票成功"/utf8>>);
                {error, vote_not_found} ->
                    elib_response:error(Req0, "投票不存在");
                {error, not_voted_yet} ->
                    elib_response:error(Req0, "您还未投票");
                {error, vote_is_closed} ->
                    elib_response:error(Req0, "投票已结束");
                {error, invalid_option} ->
                    elib_response:error(Req0, "无效的选项");
                {error, not_group_member} ->
                    elib_response:error(Req0, "您不是该群成员");
                {error, Reason} ->
                    elib_response:error(Req0, ec_cnv:to_binary(Reason))
            end
    end.

%% @doc 取消投票
%% 取消已投的投票
%%
%% @param Req0 Cowboy请求对象，包含投票ID
%% @param State 状态映射，包含 current_uid
%% @return 返回成功或错误响应
%% @end
-spec cancel(cowboy_req:req(), map()) -> cowboy_req:req().
cancel(Req0, State) ->
    PostVals = elib_param:post(Req0),
    VoteId = maps:get(<<"vote_id">>, PostVals, <<>>),
    Uid = maps:get(current_uid, State),

    case VoteId of
        <<>> ->
            elib_response:error(Req0, "vote_id 必须");
        _ ->
            case group_vote_logic:cancel_vote(VoteId, Uid) of
                ok ->
                    elib_response:success(Req0, #{<<"vote_id">> => VoteId}, <<"取消投票成功"/utf8>>);
                {error, not_voted_yet} ->
                    elib_response:error(Req0, "您还未投票");
                {error, Reason} ->
                    elib_response:error(Req0, ec_cnv:to_binary(Reason))
            end
    end.

%% @doc 结束投票
%% 结束指定的投票（创建者操作）
%%
%% @param Req0 Cowboy请求对象，包含投票ID
%% @param State 状态映射，包含 current_uid
%% @return 返回成功或错误响应
%% @end
-spec close(cowboy_req:req(), map()) -> cowboy_req:req().
close(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    VoteId = maps:get(<<"vote_id">>, PostVals, <<>>),

    case VoteId of
        <<>> ->
            elib_response:error(Req0, "vote_id 必须");
        _ ->
            case group_vote_logic:close_vote(VoteId, CurrentUid) of
                ok ->
                    elib_response:success(Req0, #{<<"vote_id">> => VoteId}, <<"结束投票成功"/utf8>>);
                {error, vote_not_found} ->
                    elib_response:error(Req0, "投票不存在");
                {error, vote_already_closed} ->
                    elib_response:error(Req0, "投票已结束");
                {error, permission_denied} ->
                    elib_response:error(Req0, "无权限结束该投票");
                {error, Reason} ->
                    elib_response:error(Req0, ec_cnv:to_binary(Reason))
            end
    end.

%% @doc 查询我的投票记录
%% 查询用户在指定投票的记录
%%
%% @param Req0 Cowboy请求对象，包含投票ID
%% @param State 状态映射，包含 current_uid
%% @return 返回包含投票记录的响应
%% @end
-spec my_vote(cowboy_req:req(), map()) -> cowboy_req:req().
my_vote(Req0, State) ->
    Qs = cowboy_req:parse_qs(Req0),
    VoteId = proplists:get_value(<<"vote_id">>, Qs, <<>>),
    Uid = maps:get(current_uid, State),

    case VoteId of
        <<>> ->
            elib_response:error(Req0, "vote_id 必须");
        _ ->
            case group_vote_logic:get_my_vote(VoteId, Uid) of
                {ok, MyVote} ->
                    elib_response:success(Req0, MyVote, "success.");
                {error, not_voted_yet} ->
                    elib_response:error(Req0, "您还未投票");
                {error, Reason} ->
                    elib_response:error(Req0, ec_cnv:to_binary(Reason))
            end
    end.

%% ===================================================================
%% EUnit tests.
%% ===================================================================

%% @doc 将请求参数中的布尔值统一转换为 boolean()
to_boolean(true, _Default) -> true;
to_boolean(false, _Default) -> false;
to_boolean(1, _Default) -> true;
to_boolean(0, _Default) -> false;
to_boolean(<<"1">>, _Default) -> true;
to_boolean(<<"0">>, _Default) -> false;
to_boolean(<<"true">>, _Default) -> true;
to_boolean(<<"false">>, _Default) -> false;
to_boolean(<<"TRUE">>, _Default) -> true;
to_boolean(<<"FALSE">>, _Default) -> false;
to_boolean(_, Default) -> Default.
