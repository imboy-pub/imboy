-module(handler_user).
-behavior(cowboy_rest).

-export([init/2]).

-include("common.hrl").


init(Req0, State) ->
    % ?LOG(State),
    Req1 =
        case lists:keyfind(action, 1, State) of
            {action, change_state} ->
                change_state(Req0, State);
            {action, change_sign} ->
                change_sign(Req0, State);
            {action, open_info} ->
                open_info(Req0, State);
            false ->
                Req0
        end,
    {ok, Req1, State}.


%% 切换在线状态
change_state(Req0, State) ->
    CurrentUid = proplists:get_value(current_uid, State),
    {ok, PostVals, _Req} = cowboy_req:read_urlencoded_body(Req0),
    ChatState = proplists:get_value(<<"state">>, PostVals, <<"hide">>),
    ds_user_setting:save_state(CurrentUid, ChatState),
    % 切换在线状态 异步通知好友
    server_user:cast_notice_friend(CurrentUid, ChatState),
    dto_resp_json:success(Req0, [], "success.").


%% 修改签名
change_sign(Req0, State) ->
    CurrentUid = proplists:get_value(current_uid, State),
    {ok, PostVals, _Req} = cowboy_req:read_urlencoded_body(Req0),
    Sign = proplists:get_value(<<"sign">>, PostVals, <<"">>),
    case ds_user:change_sign(CurrentUid, Sign) of
        {error, {_, _, ErrorMsg}} ->
            dto_resp_json:error(Req0, ErrorMsg);
        ok ->
            dto_resp_json:success(Req0, [], "success.")
    end.


% 用户网络公开信息
open_info(Req0, _State) ->
    % CurrentUid = proplists:get_value(current_uid, State),
    #{id := Uid} = cowboy_req:match_qs([{id, [], undefined}], Req0),
    % ?LOG(["open_info", Uid, Req0]),
    Column = <<"`id`, `nickname`, `avatar`, `account`,`sign`">>,
    User = ds_user:find_by_id(hashids_translator:uid_decode(Uid),
                              Column),
    % ?LOG(User),
    dto_resp_json:success(Req0,
                          hashids_translator:replace_id(User),
                          "success.").
