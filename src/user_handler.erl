-module(user_handler).
-behavior(cowboy_rest).

-export([init/2]).

-include("common.hrl").


init(Req0, State) ->
    % ?LOG(State),
    Req1 =
        case lists:keyfind(action, 1, State) of
            {action, change_state} ->
                change_state(Req0, State);
            {action, update} ->
                update(Req0, State);
            {action, open_info} ->
                open_info(Req0, State);
            {action, uqrcode} ->
                uqrcode(Req0, State);
            false ->
                Req0
        end,
    {ok, Req1, State}.

%% 扫描“我的二维码”
uqrcode(Req0, State) ->
    #{id := Uid} = cowboy_req:match_qs([{id, [], undefined}], Req0),
    CurrentUid = proplists:get_value(current_uid, State),
    case CurrentUid of
        undefined ->
            Req = cowboy_req:reply(
                302,
                #{<<"Location">> => <<"http://www.imboy.pub">>},
                Req0
            ),
            {ok, Req, State};
        _ ->
            Uid2 = imboy_hashids:uid_decode(Uid),
            Column = <<"`id`,`nickname`,`gender`,`avatar`,`sign`,`region`,`status`">>,
            User = user_logic:find_by_id(Uid2, Column),
            Status = proplists:get_value(<<"status">>, User),
            response:success(Req0,
                uqrcode_transfer(CurrentUid, Uid2, Status, User),
                "success.")
    end.

uqrcode_transfer(_, _, undefined, []) ->
    [
        {<<"result">>, <<"user_not_exist">>},
        {<<"msg">>, <<"用户不存在">>}
    ];
uqrcode_transfer(CurrentUid, Uid2, 1, User) ->
    User2 = proplists:delete(<<"status">>, User),
    [
        {<<"is_friend">>, friend_ds:is_friend(CurrentUid, Uid2)}
    ] ++ imboy_hashids:replace_id(User2);
uqrcode_transfer(_, _, _Status, _User) ->
    % 状态: -1 删除  0 禁用  1 启用
    [
        {<<"result">>, <<"user_is_disabled_or_deleted">>},
        {<<"msg">>, <<"用户被禁用或已删除">>}
    ].


%% 切换在线状态
change_state(Req0, State) ->
    CurrentUid = proplists:get_value(current_uid, State),
    {ok, PostVals, _Req} = cowboy_req:read_urlencoded_body(Req0),
    ChatState = proplists:get_value(<<"state">>, PostVals, <<"hide">>),
    user_setting_ds:save_state(CurrentUid, ChatState),
    % 切换在线状态 异步通知好友
    user_server:cast_notice_friend(CurrentUid, ChatState),
    response:success(Req0, [], "success.").


%% 修改用户信息
update(Req0, State) ->
    CurrentUid = proplists:get_value(current_uid, State),
    {ok, PostVals, _Req} = cowboy_req:read_urlencoded_body(Req0),
    Field = proplists:get_value(<<"field">>, PostVals, <<"">>),
    Value = proplists:get_value(<<"value">>, PostVals, <<"">>),
    case user_logic:update(CurrentUid, Field, Value) of
        {error, {_, _, ErrorMsg}} ->
            response:error(Req0, ErrorMsg);
        ok ->
            response:success(Req0, [], "success.")
    end.


% 用户网络公开信息
open_info(Req0, _State) ->
    % CurrentUid = proplists:get_value(current_uid, State),
    #{id := Uid} = cowboy_req:match_qs([{id, [], undefined}], Req0),
    % ?LOG(["open_info", Uid, Req0]),
    Column = <<"`id`, `nickname`, `avatar`, `account`,`sign`">>,
    User = user_logic:find_by_id(imboy_hashids:uid_decode(Uid),
                              Column),
    % ?LOG(User),
    response:success(Req0,
                          imboy_hashids:replace_id(User),
                          "success.").
