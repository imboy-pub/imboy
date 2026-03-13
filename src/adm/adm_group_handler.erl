-module(adm_group_handler).
%%%
% adm_group 控制器模块
% 群组管理 API
%%%
-behavior(cowboy_rest).

-export([init/2]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 =
        case required_feature(Action) of
            undefined ->
                case Action of
                    list -> list(Method, Req0, State);
                    detail -> detail(Method, Req0, State);
                    dissolve -> dissolve(Method, Req0, State);
                    search -> search(Method, Req0, State);
                    members -> members(Method, Req0, State);
                    _ -> Req0
                end;
            Feature ->
                case imboy_feature:ensure_enabled(Req0, Feature) of
                    ok ->
                        case Action of
                            list -> list(Method, Req0, State);
                            detail -> detail(Method, Req0, State);
                            dissolve -> dissolve(Method, Req0, State);
                            search -> search(Method, Req0, State);
                            members -> members(Method, Req0, State);
                            _ -> Req0
                        end;
                    {error, RespReq} ->
                        RespReq
                end
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

-spec required_feature(atom()) -> atom() | undefined.
required_feature(vote_list) -> group_vote;
required_feature(vote_detail) -> group_vote;
required_feature(vote_close) -> group_vote;
required_feature(schedule_list) -> group_schedule;
required_feature(schedule_detail) -> group_schedule;
required_feature(schedule_cancel) -> group_schedule;
required_feature(schedule_restore) -> group_schedule;
required_feature(task_list) -> group_task;
required_feature(task_detail) -> group_task;
required_feature(task_pending_review) -> group_task;
required_feature(task_review) -> group_task;
required_feature(task_restore) -> group_task;
required_feature(task_close) -> group_task;
required_feature(task_delete) -> group_task;
required_feature(_) -> undefined.

%% @doc 群组列表
-spec list(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
list(<<"GET">>, Req0, _State) ->
    {Page, Size} = elib_param:page(Req0),
    {ok, Status} = elib_param:int(status, Req0, -1),
    {ok, Type} = elib_param:int(type, Req0, -1),

    Where = build_where(Status, Type),
    {ok, P} = group_repo:page(Page, Size, Where, <<"created_at DESC">>),
    elib_response:success(Req0, P).

%% @doc 群组详情
-spec detail(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
detail(<<"GET">>, Req0, _State) ->
    {ok, Gid} = elib_param:int(gid, Req0, 0),
    case Gid > 0 of
        true ->
            Column = <<"id,title,avatar,introduction,owner_uid,creator_uid,member_count,member_max,type,join_limit,status,created_at">>,
            Group = group_repo:find_by_id(Gid, Column),
            case map_size(Group) > 0 of
                true ->
                    % 获取群主信息
                    OwnerUid = maps:get(<<"owner_uid">>, Group),
                    Owner = user_repo:find_by_id(OwnerUid, <<"id,nickname,avatar">>),
                    Result = Group#{owner => Owner},
                    elib_response:success(Req0, Result);
                false ->
                    elib_response:error(Req0, "群组不存在")
            end;
        false ->
            elib_response:error(Req0, "参数错误")
    end.

%% @doc 解散群组
-spec dissolve(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
dissolve(<<"POST">>, Req0, _State) ->
    {ok, Gid} = elib_param:int(gid, Req0, 0),
    case Gid > 0 of
        true ->
            case group_repo:update(#{id => Gid, status => -1}) of
                {ok, _} ->
                    elib_response:success(Req0, #{}, "操作成功");
                {error, Reason} ->
                    ?ERROR_LOG(["dissolve group error: ", Reason]),
                    elib_response:error(Req0, "操作失败")
            end;
        false ->
            elib_response:error(Req0, "参数错误")
    end.

%% @doc 搜索群组
-spec search(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
search(<<"GET">>, Req0, _State) ->
    {ok, Keyword} = elib_param:binary(keyword, Req0, <<>>),
    {Page, Size} = elib_param:page(Req0),
    
    case byte_size(Keyword) > 0 of
        true ->
            Where = #{
                'or' => [
                    #{title => {like, <<"%", Keyword/binary, "%">>}},
                    #{introduction => {like, <<"%", Keyword/binary, "%">>}}
                ]
            },
            {ok, P} = group_repo:page(Page, Size, Where, <<"created_at DESC">>),
            elib_response:success(Req0, P);
        false ->
            elib_response:error(Req0, "请输入搜索关键词")
    end.

%% @doc 群组成员列表
-spec members(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
members(<<"GET">>, Req0, _State) ->
    {ok, Gid} = elib_param:int(gid, Req0, 0),
    {Page, Size} = elib_param:page(Req0),
    
    case Gid > 0 of
        true ->
            Column = <<"user_id,nickname,avatar,role,joined_at">>,
            {ok, P} = group_member_repo:page_by_gid(Gid, Page, Size, Column),
            elib_response:success(Req0, P);
        false ->
            elib_response:error(Req0, "参数错误")
    end.

%% @doc 构建查询条件
-spec build_where(integer(), integer()) -> map().
build_where(Status, Type) when Status >= 0, Type >= 0 ->
    #{status => Status, type => Type};
build_where(Status, _Type) when Status >= 0 ->
    #{status => Status};
build_where(_Status, Type) when Type >= 0 ->
    #{type => Type};
build_where(_Status, _Type) ->
    #{}.
