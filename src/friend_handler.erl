-module(friend_handler).
-behavior(cowboy_rest).

-export([init/2]).

-include_lib("imboy/include/log.hrl").

%% ------------------------------------------------------------------
%% api
%% ------------------------------------------------------------------

init(Req0, State0) ->
    % ?LOG(State),
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 = case Action of
        friend_list ->
            friend_list(Req0, State);
        add_friend ->
            add_friend(Req0, State);
        confirm_friend ->
            confirm_friend(Req0, State);
        delete_friend ->
            delete_friend(Req0, State);
        myfriend ->
            myfriend(Req0, State);
        move ->
            move(Req0, State);
        information ->
            information(Req0, State);
        find ->
            find(Req0, State);
        change_remark ->
            change_remark(Req0, State);
        false ->
            Req0
    end,
    {ok, Req1, State}.


%%% 申请添加好友
add_friend(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    {ok, PostVals, _Req} = cowboy_req:read_urlencoded_body(Req0),
    To = proplists:get_value(<<"to">>, PostVals),
    Payload = proplists:get_value(<<"payload">>, PostVals),
    CreatedAt = proplists:get_value(<<"created_at">>, PostVals),
    case friend_logic:add_friend(CurrentUid, To, Payload, CreatedAt) of
        ok ->
            imboy_response:success(Req0, #{}, "操作成功.");
        {error, Msg, Param} ->
            imboy_response:error(Req0, Msg, 1, [{<<"field">>, Param}])
    end.

%%% 申请添加好友确认
confirm_friend(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    {ok, PostVals, _Req} = cowboy_req:read_urlencoded_body(Req0),
    From = proplists:get_value(<<"from">>, PostVals),
    To = proplists:get_value(<<"to">>, PostVals),
    Payload = proplists:get_value(<<"payload">>, PostVals),
    case friend_logic:confirm_friend(CurrentUid, From, To, Payload) of
        {ok, FromID, Remark, Source} ->
            % From 的个人信息
            % Remark 为 to 对 from 定义的 remark
            Resp = friend_logic:confirm_friend_resp(FromID, Remark),
            imboy_response:success(Req0, [{<<"source">>, Source} | Resp], "操作成功.");
        {error, Msg, Param} ->
            imboy_response:error(Req0, Msg, 1, [{<<"field">>, Param}])
    end.

%%% 删除好友关系
delete_friend(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    {ok, PostVals, _Req} = cowboy_req:read_urlencoded_body(Req0),
    Uid = proplists:get_value(<<"uid">>, PostVals),
    friend_logic:delete_friend(CurrentUid, Uid),
    imboy_response:success(Req0, #{}, "操作成功.").

%%% 查找非好友
find(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    Mine = user_logic:find_by_id(CurrentUid),
    Friends = friend_logic:search(CurrentUid),
    Data = find_transfer(Mine, Friends),
    imboy_response:success(Req0, Data, "操作成功.").

find_transfer(User, Friend) ->
    [{<<"mine">>, imboy_hashids:replace_id(User)},
     {<<"friend">>,
      [[{<<"id">>,
         imboy_hashids:uid_encode(proplists:get_value(<<"id">>,
                                                           GF))},
        {<<"groupname">>, proplists:get_value(<<"groupname">>, GF)},
        {<<"list">>,
         [imboy_hashids:replace_id(U) ||
             U <- proplists:get_value(<<"list">>, GF)]}] ||
          GF <- Friend]}].


%%% 我的好友，无好友分组的
friend_list(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    ?LOG(["CurrentUid", CurrentUid, "; State ", State]),
    Mine = user_logic:find_by_id(CurrentUid),
    MineState = user_logic:mine_state(CurrentUid),
    Friend = friend_logic:friend_list(CurrentUid),
    Data = friend_list_transfer([MineState | Mine], Friend),
    % ?LOG(Data),
    imboy_response:success(Req0, Data, "操作成功.").

friend_list_transfer(User, Friends) ->
    [{<<"mine">>, imboy_hashids:replace_id(User)},
     {<<"friend">>, [imboy_hashids:replace_id(F) || F <- Friends]}
    % {<<"mine">>, User}
    % , {<<"friend">>, Friends}
    ].

%%% 我的好友，带分组的
myfriend(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    ?LOG(["CurrentUid", CurrentUid, "; State ", State]),
    Mine = user_logic:find_by_id(CurrentUid),
    MineState = user_logic:mine_state(CurrentUid),
    Friend = friend_logic:category_friend(CurrentUid),
    Group = group_logic:user_group(CurrentUid),
    Data = myfriend_transfer([MineState | Mine], Friend, Group),
    % ?LOG(Data),
    imboy_response:success(Req0, Data, "操作成功.").

myfriend_transfer(User, Friend, Group) ->
    [{<<"mine">>, imboy_hashids:replace_id(User)},
     {<<"group">>, [imboy_hashids:replace_id(F) || F <- Group]},
     {<<"friend">>,
      [[{<<"id">>,
         imboy_hashids:uid_encode(proplists:get_value(<<"id">>,
                                                           GF))},
        {<<"groupname">>, proplists:get_value(<<"groupname">>, GF)},
        {<<"list">>,
         [imboy_hashids:replace_id(U) ||
             U <- proplists:get_value(<<"list">>, GF)]}] ||
          GF <- Friend]}].

%%% 移动好友分组
move(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),

    {ok, PostVals, _Req} = cowboy_req:read_urlencoded_body(Req0),
    Uid = proplists:get_value(<<"uid">>, PostVals),
    CategoryId = proplists:get_value(<<"category_id">>, PostVals, 0),

    friend_logic:move_to_category(CurrentUid, Uid, CategoryId),
    imboy_response:success(Req0, [], "操作成功.").


%%% 好友群资料
information(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    #{id := Uid} = cowboy_req:match_qs([{id, [], undefined}], Req0),
    case cowboy_req:match_qs([{type, [], undefined}], Req0) of
        #{type := <<"friend">>} ->
            Column = <<"`id`, `nickname`, `account`, `mobile`, `email`,
                `gender`, `experience`, `avatar`, `sign`">>,
            User = user_logic:find_by_id(Uid, Column),
            ?LOG(User),
            UserSetting = user_setting_ds:find_by_uid(Uid),
            Friend = [],
            Data = information_transfer(CurrentUid,
                                               <<"friend">>,
                                               User,
                                               UserSetting,
                                               Friend),
            imboy_response:success(Req0, Data, "操作成功.");
        #{type := <<"group">>} ->
            imboy_response:success(Req0, [], "操作成功.");
        _ ->
            imboy_response:success(Req0, [], "操作成功.")
    end.

information_transfer(CurrentUid, Type, User, UserSetting, Friend) ->
    lists:append([[{<<"mine_uid">>,
                    imboy_hashids:uid_encode(CurrentUid)},
                   {<<"type">>, Type}],
                  imboy_hashids:replace_id(User),
                  UserSetting,
                  Friend]).


%%% 修改好友备注
change_remark(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    {ok, PostVals, _Req} = cowboy_req:read_urlencoded_body(Req0),
    Uid = proplists:get_value(<<"uid">>, PostVals),
    Remark = proplists:get_value(<<"remark">>, PostVals, ""),
    case friend_ds:change_remark(CurrentUid, Uid, Remark) of
        {error, {_, _, ErrorMsg}} ->
            imboy_response:error(Req0, ErrorMsg);
        ok ->
            imboy_response:success(Req0, Remark, "操作成功.")
    end.
