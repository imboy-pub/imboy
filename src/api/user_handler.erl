-module(user_handler).

-behavior(cowboy_rest).

-dialyzer([{nowarn_function, search/2},
           {nowarn_function, change_password/2},
           {nowarn_function, set_password/2},
           {nowarn_function, apply_logout/2},
           {nowarn_function, cancel_logout/2},
           {nowarn_function, qrcode/2},
           {nowarn_function, change_state/2},
           {nowarn_function, show/2}]).

-export([init/2]).

-include("log.hrl").
-include("def_column.hrl").
-include("error_code.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    % ?DEBUG_LOG(State),
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
            set_password ->
                set_password(Req0, State);
            apply_logout ->
                apply_logout(Req0, State);
            cancel_logout ->
                cancel_logout(Req0, State);
            search ->
                search(Req0, State);
            false ->
                Req0
        end,
    {ok, Req1, State}.

-spec search(cowboy_req:req(), map()) -> cowboy_req:req().
search(Req0, State) ->
    CurrentUid = maps:get(current_uid, State, 0),
    {Page, Size} = imboy_param:page(Req0),
    #{keyword := Kwd} = cowboy_req:match_qs([{keyword, [], <<"">>}], Req0),
    KwdBin = imboy_cnv:safe_to_binary(Kwd),
    IsEmail = imboy_func:is_email(KwdBin),
    IsMobile = imboy_func:is_mobile(KwdBin),
    User =
        if IsEmail ->
               user_repo:find_by_email(KwdBin, ?DEF_USER_COLUMN);
           IsMobile ->
               user_repo:find_by_mobile(KwdBin, ?DEF_USER_COLUMN);
           true ->
               user_repo:find_by_account(KwdBin, ?DEF_USER_COLUMN)
        end,
    _ = User, % 消除未使用变量警告
    % ?DEBUG_LOG(['User ', User]),
    Uid2 = maps:get(<<"id">>, User, 0),
    AllowSearch = fts_user_repo:allow_search(Uid2),
    Payload =
        if Uid2 == 0 ->
               #{total => 0,
                 page => Page,
                 size => Size,
                 list => []};
           AllowSearch == false ->
               #{total => 0,
                 page => Page,
                 size => Size,
                 list => []};
           true ->
               {IsF, Remark} = friend_ds:is_friend(CurrentUid, Uid2, <<"remark">>),
               User2 = User#{<<"is_friend">> => IsF, <<"remark">> => Remark},
               #{total => 1,
                 page => Page,
                 size => Size,
                 list => [imboy_hashids:replace_id(User2)]}
        end,
    imboy_response:success(Req0, Payload).

%%修改密码
-spec change_password(cowboy_req:req(), map()) -> cowboy_req:req().
change_password(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    case user_logic:change_password(CurrentUid, Req0) of
        {ok, _Msg} ->
            imboy_response:success(Req0);
        {error, Msg} ->
            imboy_response:error(Req0, Msg)
    end.

-spec set_password(cowboy_req:req(), map()) -> cowboy_req:req().
set_password(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    case user_logic:set_password(CurrentUid, Req0) of
        {ok, _Msg} ->
            imboy_response:success(Req0);
        {error, Msg} ->
            imboy_response:error(Req0, Msg)
    end.

%%注销申请
-spec apply_logout(cowboy_req:req(), map()) -> cowboy_req:req().
apply_logout(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    _ = user_logic:apply_logout(CurrentUid, Req0),
    imboy_response:success(Req0).

%%撤销注销申请
-spec cancel_logout(cowboy_req:req(), map()) -> cowboy_req:req().
cancel_logout(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    case user_logic:cancel_logout(CurrentUid, Req0) of
        {ok, _Msg} ->
            imboy_response:success(Req0);
        {error, Msg} ->
            imboy_response:error(Req0, Msg)
    end.

% credential的计算方式 base64(sha1_HMAC(timestamp:username,secret-key))
-spec credential(cowboy_req:req(), map()) -> cowboy_req:req().
credential(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    Payload = user_ds:webrtc_credential(CurrentUid),
    imboy_response:success(Req0, Payload).

%% 扫描“我的二维码”
-spec qrcode(cowboy_req:req(), map()) -> cowboy_req:req().
qrcode(Req0, State) ->
    #{id := Uid} = cowboy_req:match_qs([{id, [], undefined}], Req0),
    CurrentUid = maps:get(current_uid, State, undefined),
    case CurrentUid of
        undefined ->
            Req = cowboy_req:reply(302, #{<<"Location">> => <<"http://www.imboy.pub">>}, Req0),
            {ok, Req, State};
        _ ->
            Uid2 = imboy_hashids:decode(Uid),
            Column = <<"id,nickname,gender,avatar,sign,region,status">>,
            User = user_logic:find_by_id(Uid2, Column),
            Status = maps:get(<<"status">>, User, -2),
            % ?DEBUG_LOG([User, Status]),
            Payload = qrcode_transfer(CurrentUid, Status, User),
            imboy_response:success(Req0, Payload)
    end.

qrcode_transfer(_, -2, #{}) ->
    #{<<"result">> => <<"user_not_exist">>, <<"msg">> => <<"用户不存在"/utf8>>};
qrcode_transfer(CurrentUid, 1, User) ->
    Uid2 = maps:get(<<"id">>, User),
    User2 = maps:remove(<<"status">>, User),
    {Isfriend, Remark} = friend_ds:is_friend(CurrentUid, Uid2, <<"remark">>),
    User2#{<<"type">> => <<"user">>,
           <<"id">> => imboy_hashids:encode(Uid2),
           <<"isfriend">> => Isfriend,
           <<"remark">> => Remark};
% [{<<"remark">>, Remark}, {<<"isfriend">>, Isfriend}] ++ imboy_hashids:replace_id(User2);
qrcode_transfer(_, _, _) ->
    % 状态: -1 删除  0 禁用  1 启用
    #{<<"result">> => <<"user_is_disabled_or_deleted">>, <<"msg">> => <<"用户被禁用或已删除"/utf8>>}.

%% 切换在线状态
-spec change_state(cowboy_req:req(), map()) -> cowboy_req:req().
change_state(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    PostVals = imboy_param:post(Req0),
    ChatState = maps:get(<<"state">>, PostVals, <<"hide">>),
    user_setting_ds:save(CurrentUid, <<"chat_state">>, ChatState),
    % 切换在线状态 异步通知好友
    user_server:cast_notice_friend(CurrentUid, ChatState),
    imboy_response:success(Req0, #{}, "success.").

%% 用户 批量修改设置功能
-spec setting(cowboy_req:req(), map()) -> cowboy_req:req().
setting(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    PostVals = imboy_param:post(Req0),
    Li = maps:get(<<"setting">>, PostVals, []),
    % ?DEBUG_LOG({CurrentUid, Li}),
    try [user_setting_ds:save(CurrentUid, Key, Val) || [{Key, Val} | _] <- Li] of
        _ ->
            imboy_response:success(Req0, #{}, "success.")
    catch
        error:function_clause ->
            imboy_response:error(Req0, <<"undefined setting key">>);
        error:Err1 ->
            ok = ?DEBUG_LOG([err1, Err1]),
            imboy_response:error(Req0, <<"unknown"/utf8>>, ?ERR_OPERATION_FAILED)
    end.

%% 修改用户信息
-spec update(cowboy_req:req(), map()) -> cowboy_req:req().
update(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    PostVals = imboy_param:post(Req0),
    Field = maps:get(<<"field">>, PostVals, <<>>),
    Value = maps:get(<<"value">>, PostVals, <<>>),

    ok = ?DEBUG_LOG(["update ", Field, Value]),
    case user_logic:update(CurrentUid, Field, ec_cnv:to_binary(Value)) of
        {error, {_, _, ErrorMsg}} ->
            imboy_response:error(Req0, ErrorMsg);
        ok ->
            imboy_response:success(Req0, #{}, "success.")
    end.

% 用户网络公开信息
-spec show(cowboy_req:req(), map()) -> cowboy_req:req().
show(Req0, _State) ->
    #{id := Uid} = cowboy_req:match_qs([{id, [], undefined}], Req0),
    % 验证 ID 参数不为空
    case Uid of
        undefined ->
            imboy_response:error(Req0, <<"缺少ID参数"/utf8>>);
        _ ->
            % 验证 ID 是否有效（解码后不为 0）
            DecodedUid = imboy_hashids:decode(Uid),
            case DecodedUid of
                0 ->
                    imboy_response:error(Req0, <<"无效的ID"/utf8>>);
                _ ->
                    Column = <<"id, nickname, avatar, account, sign">>,
                    User =
                        user_logic:find_by_id(
                            DecodedUid, Column),
                    imboy_response:success(Req0, imboy_hashids:replace_id(User))
            end
    end.
