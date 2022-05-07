-module(handler_friend).
-behavior(cowboy_rest).

-export([init/2]).

-include("common.hrl").


init(Req0, State) ->
    Req1 =
        case lists:keyfind(action, 1, State) of
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


%%% 查找非好友
find(Req0, State) ->
    CurrentUid = proplists:get_value(current_uid, State),
    Mine = ds_user:find_by_id(CurrentUid),
    Friends = logic_friend:search(CurrentUid),
    Data = aas_friend_myfriend:data(Mine, Friends),
    dto_resp_json:success(Req0, Data, "操作成功.").


%%% 我的好友，无好友分组的
friend_list(Req0, State) ->
    CurrentUid = proplists:get_value(current_uid, State),
    Mine = ds_user:find_by_id(CurrentUid),
    MineState = ds_user:mine_state(CurrentUid),
    Friend = logic_friend:friend_list(CurrentUid),
    Data = aas_friend_list:data([MineState | Mine], Friend),
    % ?LOG(Data),
    dto_resp_json:success(Req0, Data, "操作成功.").


%%% 我的好友，带分组的
myfriend(Req0, State) ->
    CurrentUid = proplists:get_value(current_uid, State),
    Mine = ds_user:find_by_id(CurrentUid),
    MineState = ds_user:mine_state(CurrentUid),
    Friend = logic_friend:category_friend(CurrentUid),
    Group = logic_group:user_group(CurrentUid),
    Data = aas_friend_myfriend:data([MineState | Mine], Friend, Group),
    % ?LOG(Data),
    dto_resp_json:success(Req0, Data, "操作成功.").


%%% 移动好友分组
move(Req0, State) ->
    CurrentUid = proplists:get_value(current_uid, State),

    {ok, PostVals, _Req} = cowboy_req:read_urlencoded_body(Req0),
    Uid = proplists:get_value(<<"uid">>, PostVals),
    CategoryId = proplists:get_value(<<"category_id">>, PostVals, 0),

    logic_friend:move_to_category(CurrentUid, Uid, CategoryId),
    dto_resp_json:success(Req0, [], "操作成功.").


%%% 好友群资料
information(Req0, State) ->
    CurrentUid = proplists:get_value(current_uid, State),
    #{id := Uid} = cowboy_req:match_qs([{id, [], undefined}], Req0),
    case cowboy_req:match_qs([{type, [], undefined}], Req0) of
        #{type := <<"friend">>} ->
            Column = <<"`id`, `nickname`, `account`, `mobile`, `email`,
                `gender`, `experience`, `avatar`, `sign`">>,
            User = ds_user:find_by_id(Uid, Column),
            ?LOG(User),
            UserSetting = ds_user_setting:find_by_uid(Uid),
            Friend = [],
            Data = aas_friend_infomaction:data(CurrentUid,
                                               <<"friend">>,
                                               User,
                                               UserSetting,
                                               Friend),
            dto_resp_json:success(Req0, Data, "操作成功.");
        #{type := <<"group">>} ->
            dto_resp_json:success(Req0, [], "操作成功.");
        _ ->
            dto_resp_json:success(Req0, [], "操作成功.")
    end.


%%% 修改好友备注
change_remark(Req0, State) ->
    CurrentUid = proplists:get_value(current_uid, State),
    {ok, PostVals, _Req} = cowboy_req:read_urlencoded_body(Req0),
    Uid = proplists:get_value(<<"uid">>, PostVals),
    Remark = proplists:get_value(<<"remark">>, PostVals, ""),
    case ds_friend:change_remark(CurrentUid, Uid, Remark) of
        {error, {_, _, ErrorMsg}} ->
            dto_resp_json:error(Req0, ErrorMsg);
        ok ->
            dto_resp_json:success(Req0, Remark, "操作成功.")
    end.
