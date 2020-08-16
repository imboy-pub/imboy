-module(friend_handler).
-behavior(cowboy_rest).

-export([init/2]).

-include("common.hrl").

init(Req0, State) ->
    Req1 = case lists:keyfind(action, 1, State) of
        {action, friend_list} ->
            friend_list(Req0, State);
        {action, myfriend} ->
            myfriend(Req0, State);
        {action, move} ->
            move(Req0, State);
        {action, information} ->
            information(Req0, State);
        {action, find} ->
            find(Req0, State);
        {action, change_remark} ->
            change_remark(Req0, State);
        false ->
            Req0
    end,
    {ok, Req1, State}.

friend_list(Req0, State) ->
    CurrentUid = proplists:get_value(current_uid, State),
    Mine = user_ds:find_by_id(CurrentUid),
    MineState = user_ds:mine_state(CurrentUid),
    Friend = friend_as:friend_list(CurrentUid),
    Data = friend_list_aas:data([MineState | Mine], Friend),
    % ?LOG(Data),
    resp_json_dto:success(Req0, Data, "操作成功.").

myfriend(Req0, State) ->
    CurrentUid = proplists:get_value(current_uid, State),
    Mine = user_ds:find_by_id(CurrentUid),
    MineState = user_ds:mine_state(CurrentUid),
    Friend = friend_as:category_friend(CurrentUid),
    Group = group_as:user_group(CurrentUid),
    Data = friend_myfriend_aas:data([MineState | Mine], Friend, Group),
    % ?LOG(Data),
    resp_json_dto:success(Req0, Data, "操作成功.").

%% 移动好友分组
move(Req0, State) ->
    CurrentUid = proplists:get_value(current_uid, State),

    {ok, PostVals, _Req} = cowboy_req:read_urlencoded_body(Req0),
    Uid = proplists:get_value(<<"uid">>, PostVals),
    CategoryId = proplists:get_value(<<"category_id">>, PostVals, 0),

    friend_as:move_to_category(CurrentUid, Uid, CategoryId),
    resp_json_dto:success(Req0, [], "操作成功.").


%% 好友群资料
information(Req0, State) ->
    CurrentUid = proplists:get_value(current_uid, State),
    #{id := Uid} = cowboy_req:match_qs([{id, [], undefined}], Req0),
    case cowboy_req:match_qs([{type, [], undefined}], Req0) of
        #{type := <<"friend">>} ->
            Column = <<"`id`, `nickname`, `account`, `mobile`, `email`, `gender`, `experience`, `avatar`, `sign`">>,
            User = user_ds:find_by_id(Uid, Column),
            ?LOG(User),
            UserSetting = user_setting_ds:find_by_uid(Uid),
            Friend = [],
            Data = friend_infomaction_aas:data(CurrentUid, <<"friend">>, User, UserSetting, Friend),
            resp_json_dto:success(Req0, Data, "操作成功.");
        #{type := <<"group">>} ->
            resp_json_dto:success(Req0, [], "操作成功.");
        _ ->
            resp_json_dto:success(Req0, [], "操作成功.")
    end.

find(Req0, State) ->
    %%
    CurrentUid = proplists:get_value(current_uid, State),
    Mine = user_ds:find_by_id(CurrentUid),
    Friends = friend_as:get_by_uid(CurrentUid),
    Data = myfriend_aas:data(Mine, Friends),
    resp_json_dto:success(Req0, Data, "操作成功.").

change_remark(Req0, State) ->
    %%
    CurrentUid = proplists:get_value(current_uid, State),
    {ok, PostVals, _Req} = cowboy_req:read_urlencoded_body(Req0),
    Uid = proplists:get_value(<<"uid">>, PostVals),
    Remark = proplists:get_value(<<"remark">>, PostVals, ""),
    case friend_ds:change_remark(CurrentUid, Uid, Remark) of
        {error, {_, _, ErrorMsg}} ->
            resp_json_dto:error(Req0, ErrorMsg);
        ok ->
            resp_json_dto:success(Req0, Remark, "操作成功.")
    end.
