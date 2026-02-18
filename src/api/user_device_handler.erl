-module(user_device_handler).
-dialyzer({nowarn_function, [format_error/1]}).

%%%
% user_device 控制器模块
% user_device controller module
%
% 提供以下 API：
% - GET  /v1/user_device/page - 获取设备档案列表（数据库）
% - POST /v1/user_device/change_name - 修改设备名称
% - POST /v1/user_device/delete - 删除设备
% - GET  /v1/user_device/sessions - 获取活跃会话列表（内存）
% - POST /v1/user_device/check_login - 检查登录冲突
% - POST /v1/user_device/kick - 踢出指定设备
% - POST /v1/user_device/kick-others - 踢出所有其他设备
%%%
-behavior(cowboy_handler).

-export([init/2]).
-export([handle_action/3]).

% 导入错误消息函数
-import(imboy_error, [error_msg/1]).

-include_lib("eunit/include/eunit.hrl").

-include("log.hrl").
-include("common.hrl").
-include("error_code.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 = handle_action(Action, Req0, State),
    {ok, Req1, State}.

%% @doc Action 分发处理
-spec handle_action(atom() | false, cowboy_req:req(), map()) -> cowboy_req:req().
handle_action(page, Req, State) -> page(Req, State);
handle_action(change_name, Req, State) -> change_name(Req, State);
handle_action(delete, Req, State) -> delete(Req, State);
handle_action(sessions, Req, State) -> sessions(Req, State);
handle_action(check_login, Req, State) -> check_login(Req, State);
handle_action(kick, Req, State) -> kick(Req, State);
handle_action(kick_others, Req, State) -> kick_others(Req, State);
handle_action(false, Req, _State) -> Req.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 设备分页列表
%% 获取用户的设备列表（分页）
%%
%% @param Req0 Cowboy请求对象，包含分页参数
%% @param State 状态映射，包含 current_uid
%% @return 返回包含设备列表的响应
%% @end
-spec page(cowboy_req:req(), map()) -> cowboy_req:req().
page(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    {Page, Size} = elib_param:page(Req0),
    Payload = user_device_logic:page(CurrentUid, Page, Size),
    elib_response:success(Req0, Payload).

%% @doc 修改设备名称
%% 修改设备的显示名称
%%
%% @param Req0 Cowboy请求对象，包含设备ID和新名称
%% @param State 状态映射，包含 current_uid
%% @return 返回成功响应
%% @end
-spec change_name(cowboy_req:req(), map()) -> cowboy_req:req().
change_name(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    % ?DEBUG_LOG(PostVals),
    DID = maps:get(<<"did">>, PostVals, <<"">>),
    Name = maps:get(<<"name">>, PostVals, <<"">>),
    user_device_logic:change_name(CurrentUid, DID, Name),
    elib_response:success(Req0).

%% @doc 删除设备
%% 删除指定的设备
%%
%% @param Req0 Cowboy请求对象，包含设备ID
%% @param State 状态映射，包含 current_uid
%% @return 返回成功响应
%% @end
-spec delete(cowboy_req:req(), map()) -> cowboy_req:req().
delete(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    % ?DEBUG_LOG(PostVals),
    DID = maps:get(<<"did">>, PostVals, <<"">>),
    user_device_logic:delete(CurrentUid, DID),
    elib_response:success(Req0).

%% ===================================================================
%% Device Session Actions
%% ===================================================================

%% @doc 获取活跃会话列表
%% GET /v1/user_device/sessions
-spec sessions(cowboy_req:req(), map()) -> cowboy_req:req().
sessions(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    case user_device_logic:get_active_devices(Uid) of
        {ok, Devices} ->
            elib_response:success(Req0, #{
                <<"devices">> => Devices,
                <<"count">> => length(Devices)
            });
        {error, Reason} ->
            elib_response:error(Req0, format_error(Reason), ?ERR_INTERNAL_SERVER_ERROR)
    end.

%% @doc 检查登录冲突
%% POST /v1/user_device/check_login
%% Body: {"device_type": "ios"}
-spec check_login(cowboy_req:req(), map()) -> cowboy_req:req().
check_login(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    {ok, Body, _} = cowboy_req:read_body(Req0),
    Data = jsx:decode(Body, [return_maps]),
    DType = maps:get(<<"device_type">>, Data, <<>>),

    case user_device_logic:validate_device_type(DType) of
        false ->
            elib_response:error(Req0, error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST);
        true ->
            case user_device_logic:check_login_conflict(Uid, DType) of
                {ok, no_conflict} ->
                    elib_response:success(Req0, #{
                        <<"conflict">> => false,
                        <<"message">> => <<"没有冲突，可以登录"/utf8>>
                    });
                {ok, conflict, ConflictInfo} ->
                    elib_response:success(Req0, #{
                        <<"conflict">> => true,
                        <<"conflict_device">> => ConflictInfo,
                        <<"message">> => error_msg(?ERR_DEVICE_TYPE_CONFLICT)
                    });
                {error, Reason} ->
                    elib_response:error(Req0, format_error(Reason), ?ERR_INTERNAL_SERVER_ERROR)
            end
    end.

%% @doc 踢出指定设备
%% POST /v1/user_device/kick
%% Body: {"device_type": "ios", "device_id": "device-123"}
-spec kick(cowboy_req:req(), map()) -> cowboy_req:req().
kick(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    {ok, Body, _} = cowboy_req:read_body(Req0),
    Data = jsx:decode(Body, [return_maps]),
    DType = maps:get(<<"device_type">>, Data, <<>>),
    DID = maps:get(<<"device_id">>, Data, <<>>),

    case {user_device_logic:validate_device_type(DType), byte_size(DID) > 0} of
        {false, _} ->
            elib_response:error(Req0, error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST);
        {_, false} ->
            elib_response:error(Req0, error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST);
        {true, true} ->
            case user_device_logic:kick_device(Uid, DType, DID) of
                ok ->
                    elib_response:success(Req0, #{<<"message">> => <<"设备已踢出"/utf8>>});
                {error, Reason} ->
                    elib_response:error(Req0, format_error(Reason), ?ERR_DEVICE_NOT_FOUND)
            end
    end.

%% @doc 踢出所有其他设备
%% POST /v1/user_device/kick-others
%% Body: {"device_type": "ios", "device_id": "current-device"}
-spec kick_others(cowboy_req:req(), map()) -> cowboy_req:req().
kick_others(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    {ok, Body, _} = cowboy_req:read_body(Req0),
    Data = jsx:decode(Body, [return_maps]),
    DType = maps:get(<<"device_type">>, Data, <<>>),
    DID = maps:get(<<"device_id">>, Data, <<>>),

    case {user_device_logic:validate_device_type(DType), byte_size(DID) > 0} of
        {false, _} ->
            elib_response:error(Req0, error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST);
        {_, false} ->
            elib_response:error(Req0, error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST);
        {true, true} ->
            case user_device_logic:kick_all_other_devices(Uid, {DType, DID}) of
                ok ->
                    elib_response:success(Req0, #{<<"message">> => <<"其他设备已踢出"/utf8>>});
                {error, Reason} ->
                    elib_response:error(Req0, format_error(Reason), ?ERR_INTERNAL_SERVER_ERROR)
            end
    end.

%% @doc 格式化错误消息
-spec format_error(term()) -> binary().
format_error(Reason) when is_binary(Reason) ->
    Reason;
format_error(Reason) when is_list(Reason) ->
    iolist_to_binary(Reason);
format_error(Reason) ->
    iolist_to_binary(io_lib:format("~p", [Reason])).

%% ===================================================================
%% EUnit tests.
%% ===================================================================

