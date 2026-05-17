-module(user_handler).
%% Thin HTTP adapter for the identity user boundary.
%% Keep transport concerns here and route user lifecycle rules through user_logic.

-behavior(cowboy_rest).

-dialyzer([
    {nowarn_function, search/2},
    {nowarn_function, change_password/2},
    {nowarn_function, set_password/2},
    {nowarn_function, apply_logout/2},
    {nowarn_function, cancel_logout/2},
    {nowarn_function, qrcode/2},
    {nowarn_function, change_state/2},
    {nowarn_function, show/2},
    {nowarn_function, handle_action/3}
]).

-export([init/2]).
-export([handle_action/3]).

-include("log.hrl").
-include("common.hrl").
-include("error_code.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 初始化用户处理器
%% 根据请求中的 action 参数调用相应的处理函数
%%
%% @param Req0 Cowboy请求对象
%% @param State0 状态映射，包含 action 和 current_uid 等信息
%% @return {ok, Req1, State} 处理后的请求对象和状态
%% @end
-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 = handle_action(Action, Req0, State),
    {ok, Req1, State}.

%% @doc Action 分发处理
%% 将不同的 action 分发到对应的处理函数
-spec handle_action(atom() | false, cowboy_req:req(), map()) -> cowboy_req:req().
handle_action(change_state, Req, State) -> change_state(Req, State);
handle_action(setting, Req, State) -> setting(Req, State);
handle_action(update, Req, State) -> update(Req, State);
handle_action(show, Req, State) -> show(Req, State);
handle_action(qrcode, Req, State) -> qrcode(Req, State);
handle_action(credential, Req, State) -> credential(Req, State);
handle_action(change_password, Req, State) -> change_password(Req, State);
handle_action(set_password, Req, State) -> set_password(Req, State);
handle_action(apply_logout, Req, State) -> apply_logout(Req, State);
handle_action(cancel_logout, Req, State) -> cancel_logout(Req, State);
handle_action(search, Req, State) -> search(Req, State);
handle_action(export_data, Req, State) -> export_data(Req, State);
handle_action(false, Req, _State) -> Req.

%% @doc 搜索用户
%% 根据关键词搜索用户（支持邮箱、手机号、账号）
%%
%% @param Req0 Cowboy请求对象，包含搜索关键词
%% @param State 状态映射，包含 current_uid
%% @return 返回包含搜索结果的响应
%% @end
-spec search(cowboy_req:req(), map()) -> cowboy_req:req().
search(Req0, State) ->
    CurrentUid = maps:get(current_uid, State, 0),
    {Page, Size} = elib_param:page(Req0),
    Qs0 = cowboy_req:parse_qs(Req0),
    Kwd = proplists:get_value(<<"keyword">>, Qs0, <<>>),
    KwdBin = elib_cnv:safe_to_binary(Kwd),
    IsEmail = elib_type:is_email(KwdBin),
    IsMobile = elib_type:is_mobile(KwdBin),
    User =
        if
            IsEmail ->
                user_ds:find_by_email(KwdBin, ?DEF_USER_COLUMN);
            IsMobile ->
                user_ds:find_by_mobile(KwdBin, ?DEF_USER_COLUMN);
            true ->
                user_ds:find_by_account(KwdBin, ?DEF_USER_COLUMN)
        end,
    % 消除未使用变量警告
    _ = User,
    % ?DEBUG_LOG(['User ', User]),
    Uid2 = maps:get(<<"id">>, User, 0),
    AllowSearch = fts_user_ds:allow_search(Uid2),
    Payload =
        if
            Uid2 == 0 ->
                #{
                    total => 0,
                    page => Page,
                    size => Size,
                    list => []
                };
            AllowSearch == false ->
                #{
                    total => 0,
                    page => Page,
                    size => Size,
                    list => []
                };
            true ->
                {IsF, Remark} = friend_ds:is_friend(CurrentUid, Uid2, <<"remark">>),
                User2 = User#{<<"is_friend">> => IsF, <<"remark">> => Remark},
                User3 = convert_user_id(User2),
                #{
                    total => 1,
                    page => Page,
                    size => Size,
                    list => [User3]
                }
        end,
    elib_response:success(Req0, Payload).

%% @doc 修改密码
%% 修改当前用户的登录密码
%%
%% @param Req0 Cowboy请求对象，包含旧密码和新密码
%% @param State 状态映射，包含 current_uid
%% @return 返回成功或错误响应
%% @end
-spec change_password(cowboy_req:req(), map()) -> cowboy_req:req().
change_password(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    case user_logic:change_password(CurrentUid, Req0) of
        {ok, _Msg} ->
            elib_response:success(Req0);
        {error, Msg} ->
            elib_response:error(Req0, Msg)
    end.

%% @doc 设置密码
%% 为未设置密码的账号设置密码
%%
%% @param Req0 Cowboy请求对象，包含新密码
%% @param State 状态映射，包含 current_uid
%% @return 返回成功或错误响应
%% @end
-spec set_password(cowboy_req:req(), map()) -> cowboy_req:req().
set_password(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    case user_logic:set_password(CurrentUid, Req0) of
        {ok, _Msg} ->
            elib_response:success(Req0);
        {error, Msg} ->
            elib_response:error(Req0, Msg)
    end.

%% @doc 申请注销账号
%% 提交账号注销申请
%%
%% @param Req0 Cowboy请求对象
%% @param State 状态映射，包含 current_uid
%% @return 返回成功响应
%% @end
-spec apply_logout(cowboy_req:req(), map()) -> cowboy_req:req().
apply_logout(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    _ = user_logic:apply_logout(CurrentUid, Req0),
    elib_response:success(Req0).

%% @doc 撤销注销申请
%% 取消账号注销申请
%%
%% @param Req0 Cowboy请求对象
%% @param State 状态映射，包含 current_uid
%% @return 返回成功或错误响应
%% @end
-spec cancel_logout(cowboy_req:req(), map()) -> cowboy_req:req().
cancel_logout(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    case user_logic:cancel_logout(CurrentUid, Req0) of
        {ok, _Msg} ->
            elib_response:success(Req0);
        {error, Msg} ->
            elib_response:error(Req0, Msg)
    end.

%% @doc 获取WebRTC凭证
%% 生成WebRTC连接所需的凭证
%%
%% @param Req0 Cowboy请求对象
%% @param State 状态映射，包含 current_uid
%% @return 返回包含WebRTC凭证的响应
%% @end
% credential的计算方式 base64(sha1_HMAC(timestamp:username,secret-key))
-spec credential(cowboy_req:req(), map()) -> cowboy_req:req().
credential(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    Payload = user_ds:webrtc_credential(CurrentUid),
    elib_response:success(Req0, Payload).

%% @doc 扫描用户二维码
%% 通过扫描用户二维码获取用户信息
%%
%% @param Req0 Cowboy请求对象，包含用户ID
%% @param State 状态映射，包含 current_uid
%% @return 返回包含用户信息的响应
%% @end
-spec qrcode(cowboy_req:req(), map()) -> cowboy_req:req().
qrcode(Req0, State) ->
    Qs1 = cowboy_req:parse_qs(Req0),
    Uid = proplists:get_value(<<"id">>, Qs1, undefined),
    CurrentUid = maps:get(current_uid, State, undefined),
    case CurrentUid of
        undefined ->
            RedirectUrl = config_ds:env(redirect_url, <<"http://www.imboy.pub">>),
            Req = cowboy_req:reply(302, #{<<"Location">> => RedirectUrl}, Req0),
            {ok, Req, State};
        _ ->
            Uid2 = ec_cnv:to_integer(Uid),
            Column = <<"id,nickname,gender,avatar,sign,region,status">>,
            User = user_logic:find_by_id(Uid2, Column),
            Status = maps:get(<<"status">>, User, -2),
            % ?DEBUG_LOG([User, Status]),
            Payload = qrcode_transfer(CurrentUid, Status, User),
            elib_response:success(Req0, Payload)
    end.

%% @doc 转换二维码信息
%% 将用户信息转换为二维码扫描结果
%%
%% @param CurrentUid 当前用户ID
%% @param Status 用户状态
%% @param User 用户信息
%% @return 转换后的用户信息
%% @end
-spec qrcode_transfer(integer(), integer(), map()) -> map().
qrcode_transfer(_, -2, #{}) ->
    #{<<"result">> => <<"user_not_exist">>, <<"msg">> => <<"用户不存在"/utf8>>};
qrcode_transfer(CurrentUid, 1, User) ->
    Uid2 = maps:get(<<"id">>, User),
    User2 = maps:remove(<<"status">>, User),
    {Isfriend, Remark} = friend_ds:is_friend(CurrentUid, Uid2, <<"remark">>),
    User2#{
        <<"type">> => <<"user">>,
        <<"id">> => Uid2,
        <<"isfriend">> => Isfriend,
        <<"remark">> => Remark
    };
qrcode_transfer(_, _, _) ->
    % 状态: -1 删除  0 禁用  1 启用
    #{<<"result">> => <<"user_is_disabled_or_deleted">>, <<"msg">> => <<"用户被禁用或已删除"/utf8>>}.

%% @doc 切换在线状态
%% 切换用户的在线状态（在线/隐身）
%%
%% @param Req0 Cowboy请求对象，包含新状态
%% @param State 状态映射，包含 current_uid
%% @return 返回成功响应
%% @end
-spec change_state(cowboy_req:req(), map()) -> cowboy_req:req().
change_state(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    ChatState = maps:get(<<"state">>, PostVals, <<"hide">>),
    user_setting_ds:save(CurrentUid, <<"chat_state">>, ChatState),
    % 切换在线状态 异步通知好友
    user_server:cast_notice_friend(CurrentUid, ChatState),
    elib_response:success(Req0, #{}, "success.").

%% @doc 用户设置
%% 批量修改用户设置
%%
%% @param Req0 Cowboy请求对象，包含设置列表
%% @param State 状态映射，包含 current_uid
%% @return 返回成功或错误响应
%% @end
-spec setting(cowboy_req:req(), map()) -> cowboy_req:req().
setting(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    Li = maps:get(<<"setting">>, PostVals, []),
    % ?DEBUG_LOG({CurrentUid, Li}),
    try [user_setting_ds:save(CurrentUid, Key, Val) || [{Key, Val} | _] <- Li] of
        _ ->
            elib_response:success(Req0, #{}, "success.")
    catch
        error:function_clause ->
            elib_response:error(Req0, <<"undefined setting key">>);
        error:Err1 ->
            ok = ?DEBUG_LOG([err1, Err1]),
            elib_response:error(Req0, <<"unknown"/utf8>>, ?ERR_OPERATION_FAILED)
    end.

%% @doc 修改用户信息
%% 修改用户的个人信息
%%
%% @param Req0 Cowboy请求对象，包含字段和值
%% @param State 状态映射，包含 current_uid
%% @return 返回成功或错误响应
%% @end
-spec update(cowboy_req:req(), map()) -> cowboy_req:req().
update(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    Field = maps:get(<<"field">>, PostVals, <<>>),
    Value = maps:get(<<"value">>, PostVals, <<>>),

    ok = ?DEBUG_LOG(["update ", Field, Value]),
    case user_logic:update(CurrentUid, Field, ec_cnv:to_binary(Value)) of
        {error, {_, _, ErrorMsg}} ->
            elib_response:error(Req0, ErrorMsg);
        {ok, _} ->
            elib_response:success(Req0, #{}, "success.")
    end.

%% @doc 获取用户公开信息
%% 获取用户的网络公开信息
%%
%% @param Req0 Cowboy请求对象，包含用户ID
%% @param _State 状态映射
%% @return 返回包含用户信息的响应
%% @end
-spec show(cowboy_req:req(), map()) -> cowboy_req:req().
show(Req0, _State) ->
    Qs2 = cowboy_req:parse_qs(Req0),
    Uid = proplists:get_value(<<"id">>, Qs2, undefined),
    % 【优化】使用统一的 ID 验证函数
    case Uid of
        undefined ->
            elib_response:error(Req0, <<"缺少ID参数"/utf8>>);
        _ ->
            case imboy_error:validate_id(Req0, Uid) of
                {error, Req} ->
                    Req;
                {ok, DecodedUid} ->
                    Column = <<"id, nickname, avatar, account, sign">>,
                    User = user_logic:find_by_id(DecodedUid, Column),
                    User2 = convert_user_id(User),
                    elib_response:success(Req0, User2)
            end
    end.

%% @doc 转换用户数据中的 ID 字段为 binary 字符串
%% TSID 大整数超过 JS 安全范围，必须转为 binary 字符串
-spec convert_user_id(map()) -> map().
convert_user_id(User) ->
    case maps:find(<<"id">>, User) of
        {ok, Id} when is_integer(Id) ->
            maps:put(<<"id">>, Id, User);
        _ ->
            User
    end.

%% @doc 个人数据全量导出（GDPR 第 20 条 - Right to data portability）
%% POST /v1/user/export_data
%%
%% 实现状态：占位（D-cleanup phase 27, 2026-05-17）。
%% router 早已注册到 export_data action，但 user_logic 尚未实现真正的
%% 异步打包逻辑。本占位返回 501 ERR_NOT_IMPLEMENTED 让客户端能感知"功能
%% 已契约化但服务侧未就绪"，而不是返回 false 兜底掩盖问题。
%%
%% 真正实现时需要：
%% - 异步任务：扫描 user / friend / group / msg_archive / moment / setting
%%   等表，打包为加密 zip → 写入对象存储
%% - 返回 task_id + 预期 ETA + 完成后的下载短链通知方式
%% - 配额：单用户冷却期、并发限制
%%
%% @param Req0 Cowboy请求对象
%% @param _State 状态映射，包含 current_uid
%% @return 501 ERR_NOT_IMPLEMENTED 响应
%% @end
-spec export_data(cowboy_req:req(), map()) -> cowboy_req:req().
export_data(Req0, _State) ->
    elib_response:error(
        Req0,
        <<"个人数据导出功能正在开发中，敬请期待"/utf8>>,
        ?ERR_NOT_IMPLEMENTED
    ).
