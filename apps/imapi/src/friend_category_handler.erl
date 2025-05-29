-module(friend_category_handler).
-behavior(cowboy_rest).

-export([init/2]).

-include_lib("imlib/include/log.hrl").

%% ===================================================================
%% API
%% ===================================================================


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
    PostVals = imboy_req:post_params(Req0),
    Name = proplists:get_value(<<"name">>, PostVals, <<"Unnamed">>),
    case friend_category_logic:add(CurrentUid, Name) of
        {error, ErrorMsg} ->
            imboy_response:error(Req0, ErrorMsg);
        {ok, LastInsertId} ->
            Data = [{<<"id">>, LastInsertId}, {<<"name">>, Name}],
            imboy_response:success(Req0, Data, "success.")
    end.


%% 删除好友分组
delete(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    PostVals = imboy_req:post_params(Req0),
    Id = proplists:get_value(<<"id">>, PostVals),
    case friend_category_logic:delete(CurrentUid, Id) of
        {error, ErrorMsg} ->
            imboy_response:error(Req0, ErrorMsg);
        {ok, 1} ->
            imboy_response:success(Req0, #{}, "success.")
    end.


%% 重命名好友分组
rename(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    PostVals = imboy_req:post_params(Req0),
    Id = proplists:get_value(<<"id">>, PostVals),
    Name = proplists:get_value(<<"name">>, PostVals),
    % ?DEBUG_LOG([CurrentUid, Id, Name, PostVals]),
    case friend_category_ds:rename(CurrentUid, Id, Name) of
        {error, {_, _, ErrorMsg}} ->
            imboy_response:error(Req0, ErrorMsg);
        {ok, 1} ->
            imboy_response:success(Req0, #{}, "success.")
    end.
