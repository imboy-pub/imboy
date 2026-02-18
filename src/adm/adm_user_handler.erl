-module(adm_user_handler).
%%%
% adm_user 控制器模块
% 用户管理 API
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
    Req1 = case Action of
        list -> list(Method, Req0, State);
        detail -> detail(Method, Req0, State);
        ban -> ban(Method, Req0, State);
        unban -> unban(Method, Req0, State);
        search -> search(Method, Req0, State);
        _ -> Req0
    end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 用户列表
-spec list(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
list(<<"GET">>, Req0, _State) ->
    {Page, Size} = elib_param:page(Req0),
    {ok, Status} = elib_param:int(status, Req0, -1),
    {ok, Keyword} = elib_param:binary(keyword, Req0, <<>>),

    Where = build_where(Status, Keyword),
    {ok, P} = user_repo:page(Page, Size, Where, <<"created_at DESC">>),
    elib_response:success(Req0, P).

%% @doc 用户详情
-spec detail(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
detail(<<"GET">>, Req0, _State) ->
    {ok, Uid} = elib_param:int(uid, Req0, 0),
    case Uid > 0 of
        true ->
            Column = <<"id,account,nickname,mobile,email,avatar,gender,region,sign,status,experience,created_at">>,
            User = user_repo:find_by_id(Uid, Column),
            case map_size(User) > 0 of
                true ->
                    % 获取用户设备数
                    DeviceCount = user_device_repo:count_by_uid(Uid),
                    % 获取用户好友数
                    FriendCount = friend_repo:count_by_uid(Uid),
                    % 获取用户群组数
                    GroupCount = group_member_repo:count_by_uid(Uid),
                    Result = User#{
                        device_count => DeviceCount,
                        friend_count => FriendCount,
                        group_count => GroupCount
                    },
                    elib_response:success(Req0, Result);
                false ->
                    elib_response:error(Req0, "用户不存在")
            end;
        false ->
            elib_response:error(Req0, "参数错误")
    end.

%% @doc 封禁用户
-spec ban(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
ban(<<"POST">>, Req0, _State) ->
    {ok, Uid} = elib_param:int(uid, Req0, 0),
    case Uid > 0 of
        true ->
            case user_repo:update(Uid, #{status => 0}) of
                {ok, _} ->
                    elib_response:success(Req0, #{}, "操作成功");
                {error, Reason} ->
                    ?ERROR_LOG(["ban user error: ", Reason]),
                    elib_response:error(Req0, "操作失败")
            end;
        false ->
            elib_response:error(Req0, "参数错误")
    end.

%% @doc 解封用户
-spec unban(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
unban(<<"POST">>, Req0, _State) ->
    {ok, Uid} = elib_param:int(uid, Req0, 0),
    case Uid > 0 of
        true ->
            case user_repo:update(Uid, #{status => 1}) of
                {ok, _} ->
                    elib_response:success(Req0, #{}, "操作成功");
                {error, Reason} ->
                    ?ERROR_LOG(["unban user error: ", Reason]),
                    elib_response:error(Req0, "操作失败")
            end;
        false ->
            elib_response:error(Req0, "参数错误")
    end.

%% @doc 搜索用户
-spec search(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
search(<<"GET">>, Req0, _State) ->
    {ok, Keyword} = elib_param:binary(keyword, Req0, <<>>),
    {Page, Size} = elib_param:page(Req0),
    
    case byte_size(Keyword) > 0 of
        true ->
            Where = #{
                'or' => [
                    #{account => {like, <<"%", Keyword/binary, "%">>}},
                    #{nickname => {like, <<"%", Keyword/binary, "%">>}},
                    #{email => {like, <<"%", Keyword/binary, "%">>}},
                    #{mobile => {like, <<"%", Keyword/binary, "%">>}}
                ]
            },
            {ok, P} = user_repo:page(Page, Size, Where, <<"created_at DESC">>),
            elib_response:success(Req0, P);
        false ->
            elib_response:error(Req0, "请输入搜索关键词")
    end.

%% @doc 构建查询条件
-spec build_where(integer(), binary()) -> map().
build_where(Status, Keyword) when byte_size(Keyword) > 0 ->
    KeywordWhere = #{
        'or' => [
            #{account => {like, <<"%", Keyword/binary, "%">>}},
            #{nickname => {like, <<"%", Keyword/binary, "%">>}},
            #{email => {like, <<"%", Keyword/binary, "%">>}}
        ]
    },
    case Status >= 0 of
        true -> #{status => Status, 'and' => KeywordWhere};
        false -> KeywordWhere
    end;
build_where(Status, _Keyword) ->
    case Status >= 0 of
        true -> #{status => Status};
        false -> #{}
    end.
