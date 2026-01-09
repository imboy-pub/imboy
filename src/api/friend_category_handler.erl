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

add(Req0, State) ->
    %%
    CurrentUid = maps:get(current_uid, State),
    PostVals = imboy_param:post(Req0),
    Name = maps:get(<<"name">>, PostVals, <<"Unnamed">>),
    case friend_category_ds:add(CurrentUid, Name) of
        {error, ErrorMsg} ->
            imboy_response:error(Req0, ErrorMsg);
        {ok, LastInsertId} ->
            Data = #{<<"id">> => LastInsertId, <<"name">> => Name},
            imboy_response:success(Req0, Data, "success.")
    end.

%% 删除好友分组
delete(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    PostVals = imboy_param:post(Req0),
    Id = maps:get(<<"id">>, PostVals, undefined),
    case friend_category_logic:delete(CurrentUid, Id) of
        {error, ErrorMsg} ->
            imboy_response:error(Req0, ErrorMsg);
        {ok, 1} ->
            imboy_response:success(Req0, #{}, "success.")
    end.

%% 重命名好友分组
rename(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    PostVals = imboy_param:post(Req0),
    % ?DEBUG_LOG([CurrentUid, PostVals]),
    Id = maps:get(<<"id">>, PostVals, undefined),
    Name = maps:get(<<"name">>, PostVals, undefined),
    case friend_category_ds:rename(CurrentUid, Id, Name) of
        {error, ErrorMsg} ->
            imboy_response:error(Req0, ErrorMsg);
        {ok, 1} ->
            imboy_response:success(Req0, #{}, "success.")
    end.
