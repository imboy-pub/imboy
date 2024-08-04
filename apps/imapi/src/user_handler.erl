-module(user_handler).
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
            change_state ->
                change_state(Req0, State);
            setting ->
                setting(Req0, State);
            update ->
                update(Req0, State);
            show ->
                show(Req0, State);
            qrcode ->
                qrcode(Req0, State);
            credential ->
                credential(Req0, State);
            change_password ->
                change_password(Req0, State);
            apply_logout ->
                apply_logout(Req0, State);
            cancel_logout ->
                cancel_logout(Req0, State);
            false ->
                Req0
        end,
    {ok, Req1, State}.


%%修改密码
change_password(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    case user_logic:change_password(CurrentUid, Req0) of
        {ok, _} ->
            imboy_response:success(Req0);
        {error, Msg} ->
            imboy_response:error(Req0, Msg)
    end.


%%注销申请
apply_logout(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    case user_logic:apply_logout(CurrentUid, Req0) of
        {ok, _} ->
            imboy_response:success(Req0);
        {error, Msg} ->
            imboy_response:error(Req0, Msg)
    end.


%%撤销注销申请
cancel_logout(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    case user_logic:cancel_logout(CurrentUid, Req0) of
        {ok, _} ->
            imboy_response:success(Req0);
        {error, Msg} ->
            imboy_response:error(Req0, Msg)
    end.

% credential的计算方式 base64(sha1_HMAC(timestamp:username,secret-key))
credential(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    Payload = user_ds:webrtc_credential(CurrentUid),
    imboy_response:success(Req0, Payload).


%% 扫描“我的二维码”
qrcode(Req0, State) ->
    #{id := Uid} = cowboy_req:match_qs([{id, [], undefined}], Req0),
    CurrentUid = maps:get(current_uid, State),
    case CurrentUid of
        undefined ->
            Req = cowboy_req:reply(302, #{<<"Location">> => <<"http://www.imboy.pub">>}, Req0),
            {ok, Req, State};
        _ ->
            Uid2 = imboy_hashids:decode(Uid),
            Column = <<"id,nickname,gender,avatar,sign,region,status">>,
            User = user_logic:find_by_id(Uid2, Column),
            Status = maps:get(<<"status">>, User, -2),
            % ?LOG([User, Status]),
            Payload = qrcode_transfer(CurrentUid, Status, User),
            imboy_response:success(Req0, Payload)
    end.


qrcode_transfer(_, -2, #{}) ->
    #{
        <<"result">> => <<"user_not_exist">>,
        <<"msg">> => <<"用户不存在"/utf8>>
    };
qrcode_transfer(CurrentUid, 1, User) ->
    Uid2 = maps:get(<<"id">>, User),
    User2 = maps:remove(<<"status">>, User),
    {Isfriend, Remark} = friend_ds:is_friend(CurrentUid, Uid2, <<"remark">>),
    User2#{
        <<"type">> => <<"user">>,
        <<"id">> => imboy_hashids:encode(Uid2),
        <<"isfriend">> => Isfriend,
        <<"remark">> => Remark
    };
    % [{<<"remark">>, Remark}, {<<"isfriend">>, Isfriend}] ++ imboy_hashids:replace_id(User2);
qrcode_transfer(_, _, _) ->
    % 状态: -1 删除  0 禁用  1 启用
    #{
        <<"result">> => <<"user_is_disabled_or_deleted">>,
        <<"msg">> => <<"用户被禁用或已删除"/utf8>>
    }.


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
    try [ user_setting_ds:save(CurrentUid, Key, Val) || [{Key, Val} | _] <- Li ] of
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
    Field = proplists:get_value(<<"field">>, PostVals, <<>>),
    Value = proplists:get_value(<<"value">>, PostVals, <<>>),
    % ?LOG(["update ", Field, Value]),
    case user_logic:update(CurrentUid, Field, Value) of
        {error, {_, _, ErrorMsg}} ->
            imboy_response:error(Req0, ErrorMsg);
        {ok, Msg} ->
            imboy_response:success(Req0, #{}, Msg);
        ok ->
            imboy_response:success(Req0, #{}, "success.");
        _ ->
            imboy_response:error(Req0, <<"unknown error">>)
    end.


% 用户网络公开信息
show(Req0, _State) ->
    #{id := Uid} = cowboy_req:match_qs([{id, [], undefined}], Req0),
    Column = <<"id, nickname, avatar, account, sign">>,
    User = user_logic:find_by_id(imboy_hashids:decode(Uid), Column),
    imboy_response:success(Req0, imboy_hashids:replace_id(User)).
