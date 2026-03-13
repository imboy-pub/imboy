-module(friend_handler).

-behavior(cowboy_rest).

-export([init/2]).
-export([handle_action/3]).

-include("log.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 初始化好友处理器
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
handle_action(list, Req, State) -> list(Req, State);
handle_action(add_friend, Req, State) -> add_friend(Req, State);
handle_action(confirm, Req, State) -> confirm(Req, State);
handle_action(delete_friend, Req, State) -> delete_friend(Req, State);
handle_action(move, Req, State) -> move(Req, State);
handle_action(information, Req, State) -> information(Req, State);
handle_action(change_remark, Req, State) -> change_remark(Req, State);
handle_action(false, Req, _State) -> Req.

%% @doc 申请添加好友
%% 向目标用户发送好友申请
%%
%% @param Req0 Cowboy请求对象，包含目标用户ID和申请信息
%% @param State 状态映射，包含 current_uid
%% @return 返回成功或错误响应
%% @end
-spec add_friend(cowboy_req:req(), map()) -> cowboy_req:req().
add_friend(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    To = maps:get(<<"to">>, PostVals, undefined),
    Payload = maps:get(<<"payload">>, PostVals, undefined),
    CreatedAt = maps:get(<<"created_at">>, PostVals, undefined),
    case friend_logic:add_friend(CurrentUid, To, Payload, CreatedAt) of
        ok ->
            elib_response:success(Req0, #{}, "success.");
        {error, Msg, Param} ->
            elib_response:error(Req0, Msg, 1, #{<<"field">> => Param})
    end.

%% @doc 确认好友申请
%% 接受或拒绝好友申请
%%
%% @param Req0 Cowboy请求对象，包含申请相关信息
%% @param State 状态映射，包含 current_uid
%% @return 返回成功或错误响应
%% @end
-spec confirm(cowboy_req:req(), map()) -> cowboy_req:req().
confirm(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    From = maps:get(<<"from">>, PostVals, undefined),
    To = maps:get(<<"to">>, PostVals, undefined),
    Payload = maps:get(<<"payload">>, PostVals, undefined),
    case friend_logic:confirm_friend(CurrentUid, From, To, Payload) of
        {ok, FromID, Remark, Source} ->
            % From 的个人信息
            % Remark 为 to 对 from 定义的 remark
            Payload2 = friend_logic:confirm_friend_resp(FromID, Remark),
            Payload3 = Payload2#{<<"source">> => Source},
            elib_response:success(Req0, Payload3);
        {error, Msg, Param} ->
            elib_response:error(Req0, Msg, 1, #{<<"field">> => Param})
    end.

%% @doc 删除好友关系
%% 删除与指定用户的好友关系
%%
%% @param Req0 Cowboy请求对象，包含目标用户ID
%% @param State 状态映射，包含 current_uid
%% @return 返回成功响应
%% @end
-spec delete_friend(cowboy_req:req(), map()) -> cowboy_req:req().
delete_friend(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    Uid = maps:get(<<"uid">>, PostVals, undefined),
    friend_logic:delete_friend(CurrentUid, Uid),
    elib_response:success(Req0, #{}).

%% @doc 获取好友列表
%% 获取当前用户的所有好友信息
%%
%% @param Req0 Cowboy请求对象
%% @param State 状态映射，包含 current_uid
%% @return 返回包含好友列表的响应
%% @end
-spec list(cowboy_req:req(), map()) -> cowboy_req:req().
list(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    % ?DEBUG_LOG(["CurrentUid", CurrentUid, "; State ", State]),
    Mine = user_logic:find_by_id(CurrentUid),
    {K, V} = user_logic:mine_state(CurrentUid),
    % ?DEBUG_LOG(["CurrentUid", CurrentUid, "; State ", K, V, Mine#{K => V}]),
    Friend = friend_ds:page_by_uid(CurrentUid),
    % ?DEBUG_LOG(["friend_handler/list", CurrentUid, "; Friend ", Friend]),
    Payload = list_transfer(Mine#{K => V}, Friend),
    % ?DEBUG_LOG(Payload),
    elib_response:success(Req0, Payload).

%% @doc 转换好友列表数据
%% 将用户信息和好友列表进行编码转换
%%
%% @param User 用户信息映射
%% @param Friends 好友列表
%% @return 转换后的映射
%% @end
-spec list_transfer(map(), list()) -> map().
list_transfer(User, Friends) ->
    #{<<"mine">> => elib_hashids:replace_id(User), <<"friend">> => Friends}.

%% @doc 移动好友到分组
%% 将好友移动到指定的分组
%%
%% @param Req0 Cowboy请求对象，包含目标用户ID和分组ID
%% @param State 状态映射，包含 current_uid
%% @return 返回成功响应
%% @end
-spec move(cowboy_req:req(), map()) -> cowboy_req:req().
move(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    Uid = maps:get(<<"uid">>, PostVals, undefined),
    CategoryId = ec_cnv:to_integer(maps:get(<<"category_id">>, PostVals, 0)),

    friend_logic:move_to_category(CurrentUid, Uid, CategoryId),
    elib_response:success(Req0, #{}).

%% @doc 获取好友或群组信息
%% 查询指定用户或群组的详细信息
%%
%% @param Req0 Cowboy请求对象，包含目标ID和类型
%% @param State 状态映射，包含 current_uid
%% @return 返回包含详细信息的响应
%% @end
-spec information(cowboy_req:req(), map()) -> cowboy_req:req().
information(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    Qs = cowboy_req:parse_qs(Req0),
    Uid = proplists:get_value(<<"id">>, Qs, undefined),
    Type = proplists:get_value(<<"type">>, Qs, undefined),
    case Type of
        <<"friend">> ->
            Column = <<"id, nickname, account,gender, experience, avatar, sign">>,
            User = user_logic:find_by_id(Uid, Column),
            % ?DEBUG_LOG(User),
            UserSetting = user_setting_ds:find_by_uid(Uid),
            % ?DEBUG_LOG([UserSetting, Uid]),
            Payload = information_transfer(CurrentUid, <<"friend">>, User, UserSetting),
            elib_response:success(Req0, Payload);
        <<"group">> ->
            elib_response:success(Req0, #{});
        _ ->
            elib_response:success(Req0, #{})
    end.

%% @doc 转换好友或群组信息
%% 将用户信息、类型和设置信息进行编码转换
%%
%% @param CurrentUid 当前用户ID
%% @param Type 类型（friend 或 group）
%% @param User 用户信息映射
%% @param UserSetting 用户设置信息
%% @return 转换后的信息映射
%% @end
-spec information_transfer(integer(), binary(), map(), map()) -> map().
information_transfer(CurrentUid, Type, User, UserSetting) ->
    User2 = elib_hashids:replace_id(User),
    User2#{<<"mine_uid">> => elib_hashids:encode(CurrentUid),
           <<"type">> => Type,
           <<"user_setting">> => UserSetting}.

%% @doc 修改好友备注
%% 修改好友的备注名称
%%
%% @param Req0 Cowboy请求对象，包含目标用户ID和新备注
%% @param State 状态映射，包含 current_uid
%% @return 返回成功或错误响应
%% @end
-spec change_remark(cowboy_req:req(), map()) -> cowboy_req:req().
change_remark(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    Uid = maps:get(<<"uid">>, PostVals, undefined),
    Remark = maps:get(<<"remark">>, PostVals, ""),
    % 【优化】使用统一的 ID 验证函数
    case imboy_error:validate_id(Req0, Uid) of
        {error, Req} -> Req;
        {ok, Uid2} ->
            case friend_ds:change_remark(CurrentUid, Uid2, Remark) of
                {error, ErrorMsg} ->
                    elib_response:error(Req0, ErrorMsg);
                {ok, _Num} ->
                    elib_response:success(Req0, #{<<"remark">> => Remark}, "success.")
            end
    end.
