-module(user_handler).
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
        change_state ->
            change_state(Req0, State);
        setting ->
            setting(Req0, State);
        update ->
            update(Req0, State);
        open_info ->
            open_info(Req0, State);
        uqrcode ->
            uqrcode(Req0, State);
        credential ->
            credential(Req0, State);
        false ->
            Req0
    end,
    {ok, Req1, State}.

% credential的计算方式 base64(sha1_HMAC(timestamp:username,secret-key))
credential(Req0, State) ->
    Uris = imboy_func:env(eturnal_uris),
    Secret = imboy_func:env(eturnal_secret),
    CurrentUid = maps:get(current_uid, State),
    Uid = imboy_hashids:uid_encode(CurrentUid),
    TmBin = integer_to_binary(imboy_dt:second() + 86400),
    Username = <<TmBin/binary, ":", Uid/binary>>,
    imboy_response:success(Req0, [
         {<<"uris">>, Uris},
         {<<"username">>, Username},
         {<<"credential">>, base64:encode(crypto:mac(hmac, sha, Secret, Username))}
        ], "success.").

%% 扫描“我的二维码”
uqrcode(Req0, State) ->
    #{id := Uid} = cowboy_req:match_qs([{id, [], undefined}], Req0),
    CurrentUid = maps:get(current_uid, State),
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
            imboy_response:success(Req0,
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
    {Isfriend, Remark} = friend_ds:is_friend(CurrentUid, Uid2, <<"remark">>),
    [
        {<<"remark">>, Remark},
        {<<"isfriend">>, Isfriend}
    ] ++ imboy_hashids:replace_id(User2);
uqrcode_transfer(_, _, _Status, _User) ->
    % 状态: -1 删除  0 禁用  1 启用
    [
        {<<"result">>, <<"user_is_disabled_or_deleted">>},
        {<<"msg">>, <<"用户被禁用或已删除">>}
    ].

%% 切换在线状态
change_state(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    PostVals = imboy_req:post_params(Req0),
    ChatState = proplists:get_value(<<"state">>, PostVals, <<"hide">>),
    user_setting_ds:save(CurrentUid, <<"chat_state">>, ChatState),
    % 切换在线状态 异步通知好友
    user_server:cast_notice_friend(CurrentUid, ChatState),
    imboy_response:success(Req0, #{}, "success.").

%% 用户 批量修改设置功能
setting(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    PostVals = imboy_req:post_params(Req0),
    Li = proplists:get_value(<<"setting">>, PostVals, []),
    % ?LOG({CurrentUid, Li}),
    try
        [user_setting_ds:save(CurrentUid, Key, Val) || [{Key, Val} | _] <- Li]
    of
        _ ->
            imboy_response:success(Req0, #{}, "success.")
    catch
        error:function_clause ->
            imboy_response:error(Req0, <<"undefined setting key">>);
        error:Err1 ->
            ?LOG([err1, Err1]),
            imboy_response:error(Req0, <<"unknown">>, 1)
    end.

%% 修改用户信息
update(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    PostVals = imboy_req:post_params(Req0),
    Field = proplists:get_value(<<"field">>, PostVals, <<"">>),
    Value = proplists:get_value(<<"value">>, PostVals, <<"">>),
    case user_logic:update(CurrentUid, Field, Value) of
        {error, {_, _, ErrorMsg}} ->
            imboy_response:error(Req0, ErrorMsg);
        ok ->
            imboy_response:success(Req0, #{}, "success.")
    end.


% 用户网络公开信息
open_info(Req0, _State) ->
    #{id := Uid} = cowboy_req:match_qs([{id, [], undefined}], Req0),
    Column = <<"`id`, `nickname`, `avatar`, `account`,`sign`">>,
    User = user_logic:find_by_id(imboy_hashids:uid_decode(Uid), Column),
    imboy_response:success(Req0, imboy_hashids:replace_id(User), "success.").
