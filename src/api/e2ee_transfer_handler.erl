-module(e2ee_transfer_handler).
-dialyzer({nowarn_function, [
    get_sender_private_key/1,
    get_receiver_public_key/1
]}).

-behavior(cowboy_rest).

-export([init/2]).
-export([handle_action/3]).
-export([create_transfer/2]).
-export([accept_transfer/2]).
-export([confirm_transfer/2]).
-export([cancel_transfer/2]).
-export([get_transfer_info/2]).
-export([get_pending_transfers/2]).

-include("log.hrl").
-include("common.hrl").
-include("error_code.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 初始化 E2EE 传输处理器
-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 = handle_action(Action, Req0, State),
    {ok, Req1, State}.

%% @doc Action 分发处理
-spec handle_action(atom() | false, cowboy_req:req(), map()) -> cowboy_req:req().
handle_action(create, Req, State) -> create_transfer(Req, State);
handle_action(accept, Req, State) -> accept_transfer(Req, State);
handle_action(confirm, Req, State) -> confirm_transfer(Req, State);
handle_action(cancel, Req, State) -> cancel_transfer(Req, State);
handle_action(info, Req, State) -> get_transfer_info(Req, State);
handle_action(pending, Req, State) -> get_pending_transfers(Req, State);
handle_action(false, Req, _State) -> Req.

%% ===================================================================
%% Action Handlers
%% ===================================================================

%% @doc 创建传输会话
%% POST /v1/e2ee/transfer/create
%% Body: {"to_uid": 123, "to_nickname": "新设备"}
-spec create_transfer(cowboy_req:req(), map()) -> cowboy_req:req().
create_transfer(Req0, State) ->
    CurrentUid = maps:get(current_uid, State, 0),
    {ok, Body, _} = cowboy_req:read_body(Req0),
    Data = jsx:decode(Body, [return_maps]),

    case normalize_to_uid(maps:get(<<"to_uid">>, Data, undefined)) of
        {error, missing} ->
            elib_response:error(Req0, <<"缺少 to_uid 参数"/utf8>>, ?ERR_BAD_REQUEST);
        {error, invalid} ->
            elib_response:error(Req0, <<"to_uid 参数无效"/utf8>>, ?ERR_BAD_REQUEST);
        {ok, ToUid} when ToUid =:= CurrentUid ->
            elib_response:error(Req0, <<"不能给自己创建传输会话"/utf8>>, ?ERR_BAD_REQUEST);
        {ok, ToUid} ->
            % 验证接收方用户是否存在
            case user_repo:may_exist(ToUid) of
                false ->
                    elib_response:error(Req0, <<"接收方用户不存在"/utf8>>, ?ERR_USER_NOT_FOUND);
                true ->
                    % 获取发送方设备 ID 和私钥
                    case get_sender_private_key(CurrentUid) of
                        {error, _Reason} ->
                            elib_response:error(Req0, <<"私钥不存在"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR);
                        {ok, {PrivateKeyPem, DeviceId}} ->
                            % 获取接收方公钥
                            case get_receiver_public_key(ToUid) of
                                {error, _Reason} ->
                                    elib_response:error(Req0, <<"接收方公钥不存在"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR);
                                {ok, PublicKeyPem} ->
                                    % 创建传输会话
                                    case e2ee_transfer_logic:create_transfer(
                                        CurrentUid, DeviceId, ToUid, PrivateKeyPem, PublicKeyPem
                                    ) of
                                        {ok, Session} ->
                                            elib_response:success(Req0, #{
                                                <<"session_id">> => maps:get(<<"session_id">>, Session),
                                                <<"expires_at">> => maps:get(<<"expires_at">>, Session)
                                            });
                                        {error, {Msg, Code}} ->
                                            elib_response:error(Req0, Msg, Code);
                                        {error, _Reason} ->
                                            elib_response:error(Req0, <<"创建传输会话失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR)
                                    end
                            end
                    end
            end
    end.

%% @doc 接受传输
%% POST /v1/e2ee/transfer/accept
%% Body: {"session_id": "uuid", "device_id": "new_device_id"}
-spec accept_transfer(cowboy_req:req(), map()) -> cowboy_req:req().
accept_transfer(Req0, State) ->
    CurrentUid = maps:get(current_uid, State, 0),
    {ok, Body, _} = cowboy_req:read_body(Req0),
    Data = jsx:decode(Body, [return_maps]),

    % 验证参数
    SessionId = case maps:get(<<"session_id">>, Data, <<>>) of
        <<>> -> elib_response:error(Req0, <<"缺少 session_id 参数"/utf8>>, ?ERR_BAD_REQUEST);
        Id -> Id
    end,

    ToDeviceId = maps:get(<<"device_id">>, Data, <<>>),

    % 接受传输
    case e2ee_transfer_logic:accept_transfer(SessionId, CurrentUid, ToDeviceId) of
        {ok, Session} ->
            elib_response:success(Req0, #{
                <<"session_id">> => maps:get(<<"session_id">>, Session),
                <<"from_uid">> => maps:get(<<"from_uid">>, Session),
                <<"from_device_id">> => maps:get(<<"from_device_id">>, Session),
                <<"encrypted_key_bundle">> => maps:get(<<"encrypted_key_bundle">>, Session),
                <<"status">> => maps:get(<<"status">>, Session),
                <<"expires_at">> => maps:get(<<"expires_at">>, Session)
            });
        {error, {Msg, Code}} ->
            elib_response:error(Req0, Msg, Code);
        {error, _Reason} ->
            elib_response:error(Req0, <<"接受传输失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR)
    end.

%% @doc 确认传输完成
%% POST /v1/e2ee/transfer/confirm
%% Body: {"session_id": "uuid"}
-spec confirm_transfer(cowboy_req:req(), map()) -> cowboy_req:req().
confirm_transfer(Req0, State) ->
    CurrentUid = maps:get(current_uid, State, 0),
    {ok, Body, _} = cowboy_req:read_body(Req0),
    Data = jsx:decode(Body, [return_maps]),

    % 验证参数
    SessionId = case maps:get(<<"session_id">>, Data, <<>>) of
        <<>> -> elib_response:error(Req0, <<"缺少 session_id 参数"/utf8>>, ?ERR_BAD_REQUEST);
        Id -> Id
    end,

    % 确认传输
    case e2ee_transfer_logic:confirm_transfer(SessionId, CurrentUid) of
        ok ->
            elib_response:success(Req0, #{<<"message">> => <<"传输成功"/utf8>>});
        {error, {Msg, Code}} ->
            elib_response:error(Req0, Msg, Code);
        {error, _Reason} ->
            elib_response:error(Req0, <<"确认传输失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR)
    end.

%% @doc 取消传输会话
%% POST /v1/e2ee/transfer/cancel
%% Body: {"session_id": "uuid"}
-spec cancel_transfer(cowboy_req:req(), map()) -> cowboy_req:req().
cancel_transfer(Req0, State) ->
    CurrentUid = maps:get(current_uid, State, 0),
    {ok, Body, _} = cowboy_req:read_body(Req0),
    Data = jsx:decode(Body, [return_maps]),

    % 验证参数
    SessionId = case maps:get(<<"session_id">>, Data, <<>>) of
        <<>> -> elib_response:error(Req0, <<"缺少 session_id 参数"/utf8>>, ?ERR_BAD_REQUEST);
        Id -> Id
    end,

    % 取消传输
    case e2ee_transfer_logic:cancel_transfer(SessionId, CurrentUid) of
        ok ->
            elib_response:success(Req0, #{<<"message">> => <<"已取消传输"/utf8>>});
        {error, {Msg, Code}} ->
            elib_response:error(Req0, Msg, Code);
        {error, _Reason} ->
            elib_response:error(Req0, <<"取消传输失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR)
    end.

%% @doc 查询传输会话信息
%% GET /v1/e2ee/transfer/info?session_id=xxx
-spec get_transfer_info(cowboy_req:req(), map()) -> cowboy_req:req().
get_transfer_info(Req0, _State) ->
    Qs = cowboy_req:parse_qs(Req0),
    SessionId = proplists:get_value(<<"session_id">>, Qs, <<>>),

    case SessionId of
        <<>> ->
            elib_response:error(Req0, <<"缺少 session_id 参数"/utf8>>, ?ERR_BAD_REQUEST);
        _ ->
            case e2ee_transfer_logic:get_transfer_info(SessionId) of
                {ok, Session} ->
                    elib_response:success(Req0, #{
                        <<"session_id">> => maps:get(<<"session_id">>, Session),
                        <<"from_uid">> => maps:get(<<"from_uid">>, Session),
                        <<"from_device_id">> => maps:get(<<"from_device_id">>, Session),
                        <<"status">> => maps:get(<<"status">>, Session),
                        <<"expires_at">> => maps:get(<<"expires_at">>, Session)
                    });
                {error, not_found} ->
                    elib_response:error(Req0, <<"会话不存在或已过期"/utf8>>, ?ERR_NOT_FOUND);
                {error, _Reason} ->
                    elib_response:error(Req0, <<"查询会话失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR)
            end
    end.

%% @doc 获取待处理的传输列表
%% GET /v1/e2ee/transfer/pending
-spec get_pending_transfers(cowboy_req:req(), map()) -> cowboy_req:req().
get_pending_transfers(Req0, State) ->
    CurrentUid = maps:get(current_uid, State, 0),

    case e2ee_transfer_logic:get_pending_transfers(CurrentUid) of
        {ok, Sessions} ->
            % 格式化返回数据
            FormattedSessions = lists:map(fun(Session) ->
                #{
                    <<"session_id">> => maps:get(<<"session_id">>, Session),
                    <<"from_uid">> => maps:get(<<"from_uid">>, Session),
                    <<"from_device_id">> => maps:get(<<"from_device_id">>, Session),
                    <<"expires_at">> => maps:get(<<"expires_at">>, Session),
                    <<"created_at">> => maps:get(<<"created_at">>, Session)
                }
            end, Sessions),
            elib_response:success(Req0, #{<<"transfers">> => FormattedSessions});
        {error, _Reason} ->
            elib_response:error(Req0, <<"获取传输列表失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR)
    end.

%% ===================================================================
%% Internal Functions
%% ===================================================================

%% @doc 获取发送方的私钥和设备 ID
-spec get_sender_private_key(integer()) -> {ok, {binary(), binary()}} | {error, term()}.
get_sender_private_key(Uid) ->
    case user_device_repo:get_public_by_uid(Uid) of
        {ok, [Device | _]} ->
            DeviceId = maps:get(<<"device_id">>, Device),
            case user_device_repo:get_private_key(Uid, DeviceId) of
                {ok, PrivateKeyPem} when PrivateKeyPem /= <<>> ->
                    {ok, {PrivateKeyPem, DeviceId}};
                _ ->
                    {error, private_key_not_found}
            end;
        _ ->
            {error, device_not_found}
    end.

%% @doc 获取接收方的公钥
-spec get_receiver_public_key(integer()) -> {ok, binary()} | {error, term()}.
get_receiver_public_key(ToUid) ->
    case user_device_repo:get_public_by_uid(ToUid) of
        {ok, [Device | _]} ->
            PublicKeyPem = maps:get(<<"public_key">>, Device, <<>>),
            case PublicKeyPem of
                <<>> -> {error, public_key_not_found};
                _ -> {ok, PublicKeyPem}
            end;
        _ ->
            {error, device_not_found}
    end.

-spec normalize_to_uid(term()) -> {ok, integer()} | {error, missing | invalid}.
normalize_to_uid(undefined) ->
    {error, missing};
normalize_to_uid(ToUid) when is_integer(ToUid), ToUid > 0 ->
    {ok, ToUid};
normalize_to_uid(ToUid) when is_binary(ToUid); is_list(ToUid) ->
    ToUidBin = ec_cnv:to_binary(ToUid),
    case ToUidBin of
        <<>> ->
            {error, invalid};
        _ ->
            case catch elib_hashids:decode(ToUidBin) of
                Uid when is_integer(Uid), Uid > 0 ->
                    {ok, Uid};
                _ ->
                    case safe_positive_integer(ToUidBin) of
                        {ok, Uid} ->
                            {ok, Uid};
                        error ->
                            {error, invalid}
                    end
            end
    end;
normalize_to_uid(_) ->
    {error, invalid}.

-spec safe_positive_integer(binary()) -> {ok, integer()} | error.
safe_positive_integer(Value) when is_binary(Value) ->
    try
        Parsed = binary_to_integer(Value),
        case Parsed > 0 of
            true -> {ok, Parsed};
            false -> error
        end
    catch
        _:_ -> error
    end.
