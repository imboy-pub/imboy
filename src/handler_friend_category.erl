-module(handler_friend_category).
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
    case logic_friend_category:add(CurrentUid, Name) of
        {error, ErrorMsg} ->
            dto_resp_json:error(Req0, ErrorMsg);
        {ok, LastInsertId} ->
            Data = [{<<"id">>, LastInsertId}, {<<"name">>, Name}],
            dto_resp_json:success(Req0, Data, "操作成功.")
    end.


%% 删除好友分组
delete(Req0, State) ->
    CurrentUid = proplists:get_value(current_uid, State),
    {ok, PostVals, _Req} = cowboy_req:read_urlencoded_body(Req0),
    Id = proplists:get_value(<<"id">>, PostVals),
    case logic_friend_category:delete(CurrentUid, Id) of
        {error, ErrorMsg} ->
            dto_resp_json:error(Req0, ErrorMsg);
        ok ->
            dto_resp_json:success(Req0, [], "操作成功.")
    end.


%% 重命名好友分组
rename(Req0, State) ->
    CurrentUid = proplists:get_value(current_uid, State),
    {ok, PostVals, _Req} = cowboy_req:read_urlencoded_body(Req0),
    Id = proplists:get_value(<<"id">>, PostVals),
    Name = proplists:get_value(<<"name">>, PostVals),
    % ?LOG([CurrentUid, Id, Name, PostVals]),
    case ds_friend_category:rename(CurrentUid, Id, Name) of
        {error, {_, _, ErrorMsg}} ->
            dto_resp_json:error(Req0, ErrorMsg);
        ok ->
            dto_resp_json:success(Req0, [], "操作成功.")
    end.
