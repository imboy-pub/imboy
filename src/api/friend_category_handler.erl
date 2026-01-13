-module(friend_category_handler).

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
            add ->
                add(Req0, State);
            delete ->
                delete(Req0, State);
            rename ->
                rename(Req0, State);
            false ->
                Req0
        end,
    {ok, Req1, State}.

%% @doc 添加好友分组
%% 为当前用户创建一个新的好友分组
%%
%% @param Req0 Cowboy请求对象，包含分组名称
%% @param State 状态映射，包含 current_uid
%% @return 返回包含新分组ID的响应或错误响应
%% @end
-spec add(cowboy_req:req(), map()) -> cowboy_req:req().
add(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    Name = maps:get(<<"name">>, PostVals, <<"Unnamed">>),
    case friend_category_ds:add(CurrentUid, Name) of
        {error, ErrorMsg} ->
            elib_response:error(Req0, ErrorMsg);
        {ok, LastInsertId} ->
            Data = #{<<"id">> => LastInsertId, <<"name">> => Name},
            elib_response:success(Req0, Data, "success.")
    end.

%% @doc 删除好友分组
%% 删除当前用户的指定好友分组
%%
%% @param Req0 Cowboy请求对象，包含分组ID
%% @param State 状态映射，包含 current_uid
%% @return 返回成功或错误响应
%% @end
-spec delete(cowboy_req:req(), map()) -> cowboy_req:req().
delete(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    Id = maps:get(<<"id">>, PostVals, undefined),
    case friend_category_logic:delete(CurrentUid, Id) of
        {error, ErrorMsg} ->
            elib_response:error(Req0, ErrorMsg);
        {ok, 1} ->
            elib_response:success(Req0, #{}, "success.")
    end.

%% @doc 重命名好友分组
%% 修改当前用户的指定好友分组名称
%%
%% @param Req0 Cowboy请求对象，包含分组ID和新名称
%% @param State 状态映射，包含 current_uid
%% @return 返回成功或错误响应
%% @end
-spec rename(cowboy_req:req(), map()) -> cowboy_req:req().
rename(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    % ?DEBUG_LOG([CurrentUid, PostVals]),
    Id = maps:get(<<"id">>, PostVals, undefined),
    Name = maps:get(<<"name">>, PostVals, undefined),
    case friend_category_ds:rename(CurrentUid, Id, Name) of
        {error, ErrorMsg} ->
            elib_response:error(Req0, ErrorMsg);
        {ok, 1} ->
            elib_response:success(Req0, #{}, "success.")
    end.
