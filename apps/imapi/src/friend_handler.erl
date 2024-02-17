-module(friend_handler).
-behavior(cowboy_rest).

-export([init/2]).

-include_lib("imlib/include/log.hrl").

%% ===================================================================
%% API
%% ===================================================================


init(Req0, State0) ->
    % ?LOG(State),
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 =
        case Action of
            list ->
                list(Req0, State);
            add_friend ->
                add_friend(Req0, State);
            confirm ->
                confirm(Req0, State);
            delete_friend ->
                delete_friend(Req0, State);
            move ->
                move(Req0, State);
            information ->
                information(Req0, State);
            change_remark ->
                change_remark(Req0, State);
            false ->
                Req0
        end,
    {ok, Req1, State}.


%%% 申请添加好友
add_friend(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    PostVals = imboy_req:post_params(Req0),
    To = proplists:get_value(<<"to">>, PostVals),
    Payload = proplists:get_value(<<"payload">>, PostVals),
    CreatedAt = proplists:get_value(<<"created_at">>, PostVals),
    case friend_logic:add_friend(CurrentUid, To, Payload, CreatedAt) of
        ok ->
            imboy_response:success(Req0, #{}, "success.");
        {error, Msg, Param} ->
            imboy_response:error(Req0, Msg, 1, [{<<"field">>, Param}])
    end.


%%% 申请添加好友确认
confirm(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    PostVals = imboy_req:post_params(Req0),
    From = proplists:get_value(<<"from">>, PostVals),
    To = proplists:get_value(<<"to">>, PostVals),
    Payload = proplists:get_value(<<"payload">>, PostVals),
    case friend_logic:confirm_friend(CurrentUid, From, To, Payload) of
        {ok, FromID, Remark, Source} ->
            % From 的个人信息
            % Remark 为 to 对 from 定义的 remark
            Payload2 = friend_logic:confirm_friend_resp(FromID, Remark),
            Payload3 = Payload2#{
                <<"source">> => Source
            },
            imboy_response:success(Req0, Payload3);
        {error, Msg, Param} ->
            imboy_response:error(Req0, Msg, 1, [{<<"field">>, Param}])
    end.


%%% 删除好友关系
delete_friend(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    PostVals = imboy_req:post_params(Req0),
    Uid = proplists:get_value(<<"uid">>, PostVals),
    friend_logic:delete_friend(CurrentUid, Uid),
    imboy_response:success(Req0, #{}).


%%% 我的好友，无好友分组的
list(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    % ?LOG(["CurrentUid", CurrentUid, "; State ", State]),
    Mine = user_logic:find_by_id(CurrentUid),
    {K, V} = user_logic:mine_state(CurrentUid),
    Friend = friend_ds:page_by_uid(CurrentUid),
    Payload = list_transfer(Mine#{K => V}, Friend),
    % ?LOG(Payload),
    imboy_response:success(Req0, Payload).


list_transfer(User, Friends) ->
    #{
        <<"mine">> => imboy_hashids:replace_id(User),
        <<"friend">> => Friends
    }.


%%% 移动好友分组
move(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    PostVals = imboy_req:post_params(Req0),
    Uid = proplists:get_value(<<"uid">>, PostVals),
    CategoryId = proplists:get_value(<<"category_id">>, PostVals, 0),

    friend_logic:move_to_category(CurrentUid, Uid, CategoryId),
    imboy_response:success(Req0, #{}).


%%% 好友群资料
information(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    #{id := Uid} = cowboy_req:match_qs([{id, [], undefined}], Req0),
    case cowboy_req:match_qs([{type, [], undefined}], Req0) of
        #{type := <<"friend">>} ->
            Column = <<"id, nickname, account,gender, experience, avatar, sign">>,
            User = user_logic:find_by_id(Uid, Column),
            % ?LOG(User),
            UserSetting = user_setting_ds:find_by_uid(Uid),
            % ?LOG([UserSetting, Uid]),
            Payload = information_transfer(CurrentUid, <<"friend">>, User, UserSetting),
            imboy_response:success(Req0, Payload);
        #{type := <<"group">>} ->
            imboy_response:success(Req0, #{});
        _ ->
            imboy_response:success(Req0, #{})
    end.


information_transfer(CurrentUid, Type, User, UserSetting) ->
    User2 = imboy_hashids:replace_id(User),
    User2#{
        <<"mine_uid">> => imboy_hashids:encode(CurrentUid),
        <<"type">> => Type,
        <<"user_setting">> => UserSetting
    }.


%%% 修改好友备注
change_remark(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    PostVals = imboy_req:post_params(Req0),
    Uid = proplists:get_value(<<"uid">>, PostVals),
    Remark = proplists:get_value(<<"remark">>, PostVals, ""),
    Uid2 = imboy_hashids:decode(Uid),
    case friend_ds:change_remark(CurrentUid, Uid2, Remark) of
        {error, ErrorMsg} ->
            imboy_response:error(Req0, ErrorMsg);
        {ok, _Num} ->
            imboy_response:success(Req0, Remark, "success.")
    end.
