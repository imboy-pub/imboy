-module(friend_category_handler).
-behavior(cowboy_rest).

-export([init/2]).

-include("common.hrl").


init(Req0, State) ->
    Req1 =
        case lists:keyfind(action, 1, State) of
            {action, add} ->
                add(Req0, State);
            {action, delete} ->
                delete(Req0, State);
            {action, rename} ->
                rename(Req0, State);
            false ->
                Req0
        end,
    {ok, Req1, State}.


add(Req0, State) ->
    %%
    CurrentUid = proplists:get_value(current_uid, State),
    {ok, PostVals, _Req} = cowboy_req:read_urlencoded_body(Req0),
    Name = proplists:get_value(<<"name">>, PostVals, <<"Unnamed">>),
    case friend_category_logic:add(CurrentUid, Name) of
        {error, ErrorMsg} ->
            response:error(Req0, ErrorMsg);
        {ok, LastInsertId} ->
            Data = [{<<"id">>, LastInsertId}, {<<"name">>, Name}],
            response:success(Req0, Data, "操作成功.")
    end.


%% 删除好友分组
delete(Req0, State) ->
    CurrentUid = proplists:get_value(current_uid, State),
    {ok, PostVals, _Req} = cowboy_req:read_urlencoded_body(Req0),
    Id = proplists:get_value(<<"id">>, PostVals),
    case friend_category_logic:delete(CurrentUid, Id) of
        {error, ErrorMsg} ->
            response:error(Req0, ErrorMsg);
        ok ->
            response:success(Req0, [], "操作成功.")
    end.


%% 重命名好友分组
rename(Req0, State) ->
    CurrentUid = proplists:get_value(current_uid, State),
    {ok, PostVals, _Req} = cowboy_req:read_urlencoded_body(Req0),
    Id = proplists:get_value(<<"id">>, PostVals),
    Name = proplists:get_value(<<"name">>, PostVals),
    % ?LOG([CurrentUid, Id, Name, PostVals]),
    case friend_category_ds:rename(CurrentUid, Id, Name) of
        {error, {_, _, ErrorMsg}} ->
            response:error(Req0, ErrorMsg);
        ok ->
            response:success(Req0, [], "操作成功.")
    end.
