%%%-------------------------------------------------------------------
%%% @doc QR 码登录处理器
%%% 提供 WhatsApp Web 风格的扫码登录功能
%%%
%%% API 端点:
%%% - POST /v1/passport/qr_login/create  - 创建登录二维码
%%% - GET  /v1/passport/qr_login/status  - 查询扫码状态
%%% - POST /v1/passport/qr_login/scan    - 扫码（手机端调用）
%%% - POST /v1/passport/qr_login/confirm - 确认登录（手机端调用）
%%% - POST /v1/passport/qr_login/cancel  - 取消登录
%%% @end
%%%-------------------------------------------------------------------
-module(qr_login_handler).
-behavior(cowboy_rest).

-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([handle_request/2]).

-include("log.hrl").
-include("common.hrl").
-include("error_code.hrl").

%% ===================================================================
%% Cowboy Callbacks
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {cowboy_rest, cowboy_req:req(), map()}.
init(Req, State) ->
    {cowboy_rest, Req, State}.

-spec allowed_methods(cowboy_req:req(), map()) ->
    {[binary()], cowboy_req:req(), map()}.
allowed_methods(Req, State) ->
    {[<<"POST">>, <<"GET">>], Req, State}.

-spec content_types_provided(cowboy_req:req(), map()) ->
    {[{binary(), atom()}], cowboy_req:req(), map()}.
content_types_provided(Req, State) ->
    {[{<<"application/json">>, handle_request}], Req, State}.

-spec content_types_accepted(cowboy_req:req(), map()) ->
    {[{binary(), atom()}], cowboy_req:req(), map()}.
content_types_accepted(Req, State) ->
    {[{<<"application/json">>, handle_request}], Req, State}.

%% ===================================================================
%% Request Handler
%% ===================================================================

-spec handle_request(cowboy_req:req(), map()) ->
    {stop, cowboy_req:req(), map()}.
handle_request(Req0, State) ->
    Action = maps:get(action, State, undefined),
    Method = cowboy_req:method(Req0),

    Req1 =
        case {Method, Action} of
            {<<"POST">>, create} -> handle_create(Req0);
            {<<"GET">>, status} -> handle_status(Req0);
            {<<"POST">>, scan} -> handle_scan(Req0, State);
            {<<"POST">>, confirm} -> handle_confirm(Req0, State);
            {<<"POST">>, cancel} -> handle_cancel(Req0, State);
            _ -> elib_response:error(Req0, <<"Bad Request"/utf8>>, ?ERR_BAD_REQUEST)
        end,
    {stop, Req1, State}.

%% ===================================================================
%% API Handlers
%% ===================================================================

%% @doc 创建登录二维码
%% POST /v1/passport/qr_login/create
%% Body: {"device_id": "xxx", "device_name": "Web Browser", "platform": "web"}
handle_create(Req) ->
    {ok, Body, _} = cowboy_req:read_body(Req),
    Data = jsx:decode(Body, [return_maps]),
    DeviceId = maps:get(<<"device_id">>, Data, <<>>),
    DeviceName = maps:get(<<"device_name">>, Data, <<"Web Browser">>),
    Platform = maps:get(<<"platform">>, Data, <<"web">>),

    case DeviceId of
        <<>> ->
            elib_response:error(Req, <<"参数错误"/utf8>>, ?ERR_BAD_REQUEST);
        _ ->
            % 生成唯一的会话 Token
            SessionToken = generate_session_token(),
            % 生成 QR 码数据（包含 session_token）
            QRToken = generate_qr_token(SessionToken),

            % 存储会话信息（60 秒过期）
            SessionData = #{
                <<"session_token">> => SessionToken,
                <<"qr_token">> => QRToken,
                <<"device_id">> => DeviceId,
                <<"device_name">> => DeviceName,
                <<"platform">> => Platform,
                <<"status">> => <<"waiting">>,
                <<"created_at">> => erlang:system_time(millisecond),
                <<"expires_at">> => erlang:system_time(millisecond) + 60000
            },

            % 存储到缓存（60 秒过期）
            cache_session(SessionToken, SessionData),

            Payload = #{
                <<"qr_token">> => QRToken,
                <<"session_token">> => SessionToken,
                <<"expires_in">> => 60
            },
            elib_response:success(Req, Payload)
    end.

%% @doc 查询扫码状态
%% GET /v1/passport/qr_login/status?session_token=xxx
handle_status(Req) ->
    % 使用 parse_qs 安全地获取查询参数
    Qs = cowboy_req:parse_qs(Req),
    SessionToken = proplists:get_value(<<"session_token">>, Qs, <<>>),

    case SessionToken of
        <<>> ->
            elib_response:error(Req, <<"参数错误"/utf8>>, ?ERR_BAD_REQUEST);
        _ ->
            case get_session(SessionToken) of
                undefined ->
                    elib_response:error(Req, <<"会话不存在或已过期"/utf8>>, ?ERR_NOT_FOUND);
                Session ->
                    Status = maps:get(<<"status">>, Session, <<"waiting">>),
                    LoginToken = maps:get(<<"login_token">>, Session, null),

                    case Status of
                        <<"confirmed">> when LoginToken =/= null ->
                            Payload = #{
                                <<"status">> => <<"confirmed">>,
                                <<"token">> => LoginToken
                            },
                            elib_response:success(Req, Payload);
                        _ ->
                            Payload = #{
                                <<"status">> => Status
                            },
                            elib_response:success(Req, Payload)
                    end
            end
    end.

%% @doc 扫码（手机端调用）
%% POST /v1/passport/qr_login/scan
%% Body: {"qr_token": "xxx"}
handle_scan(Req, State) ->
    Uid = maps:get(current_uid, State, 0),
    {ok, Body, _} = cowboy_req:read_body(Req),
    Data = jsx:decode(Body, [return_maps]),

    QRToken = maps:get(<<"qr_token">>, Data, <<>>),

    case QRToken of
        <<>> ->
            elib_response:error(Req, <<"参数错误"/utf8>>, ?ERR_BAD_REQUEST);
        _ ->
            % 从 QR Token 解析 session_token
            case parse_qr_token(QRToken) of
                {error, _} ->
                    elib_response:error(Req, <<"无效的二维码"/utf8>>, ?ERR_INVALID_QR_TOKEN);
                {ok, SessionToken} ->
                    case get_session(SessionToken) of
                        undefined ->
                            elib_response:error(Req, <<"二维码已过期，请刷新"/utf8>>, ?ERR_QR_LOGIN_EXPIRED);
                        Session ->
                            Status = maps:get(<<"status">>, Session, <<"waiting">>),
                            case Status of
                                <<"confirmed">> ->
                                    elib_response:error(
                                        Req,
                                        <<"二维码已确认，请刷新"/utf8>>,
                                        ?ERR_QR_LOGIN_ALREADY_USED
                                    );
                                <<"cancelled">> ->
                                    elib_response:error(
                                        Req,
                                        <<"登录已取消，请刷新"/utf8>>,
                                        ?ERR_QR_LOGIN_CANCELLED
                                    );
                                _ ->
                                    ExpiresAt = maps:get(<<"expires_at">>, Session, 0),
                                    case erlang:system_time(millisecond) > ExpiresAt of
                                        true ->
                                            delete_session(SessionToken),
                                            elib_response:error(
                                                Req,
                                                <<"二维码已过期，请刷新"/utf8>>,
                                                ?ERR_QR_LOGIN_EXPIRED
                                            );
                                        false ->
                                            ScannedBy = maps:get(<<"scanned_by">>, Session, 0),
                                            case
                                                Status =:= <<"scanned">> andalso
                                                    ScannedBy =/= 0 andalso
                                                    ScannedBy =/= Uid
                                            of
                                                true ->
                                                    elib_response:error(
                                                        Req,
                                                        <<"无权限操作"/utf8>>,
                                                        ?ERR_FORBIDDEN
                                                    );
                                                false ->
                                                    UpdatedSession = Session#{
                                                        <<"status">> => <<"scanned">>,
                                                        <<"scanned_by">> => Uid,
                                                        <<"scanned_at">> => erlang:system_time(
                                                            millisecond
                                                        )
                                                    },
                                                    cache_session(SessionToken, UpdatedSession),
                                                    %% PR-2β: 广播 scanned 事件给该会话的 SSE 订阅者
                                                    %% （Web 端立即感知，无需等下一轮轮询）。
                                                    %% notify/2 内部已 try/catch 兜底，不影响主流程。
                                                    _ = qr_login_event_ds:notify(
                                                        SessionToken,
                                                        qr_login_event_ds:event(scanned, undefined)
                                                    ),
                                                    %% 回带设备信息便于手机端 UI 展示"哪台设备要登录"。
                                                    %% 字段对齐 create/2 时存储的 device_name / platform。
                                                    DeviceName = maps:get(
                                                        <<"device_name">>, Session, <<>>
                                                    ),
                                                    Platform = maps:get(
                                                        <<"platform">>, Session, <<>>
                                                    ),
                                                    elib_response:success(
                                                        Req,
                                                        #{
                                                            <<"status">> => <<"scanned">>,
                                                            <<"device_name">> => DeviceName,
                                                            <<"platform">> => Platform
                                                        }
                                                    )
                                            end
                                    end
                            end
                    end
            end
    end.

%% @doc 确认登录（手机端调用）
%% POST /v1/passport/qr_login/confirm
%% Body: {"qr_token": "xxx"}
handle_confirm(Req, State) ->
    Uid = maps:get(current_uid, State, 0),
    {ok, Body, _} = cowboy_req:read_body(Req),
    Data = jsx:decode(Body, [return_maps]),

    QRToken = maps:get(<<"qr_token">>, Data, <<>>),

    case QRToken of
        <<>> ->
            elib_response:error(Req, <<"参数错误"/utf8>>, ?ERR_BAD_REQUEST);
        _ ->
            case parse_qr_token(QRToken) of
                {error, _} ->
                    elib_response:error(Req, <<"无效的二维码"/utf8>>, ?ERR_INVALID_QR_TOKEN);
                {ok, SessionToken} ->
                    case get_session(SessionToken) of
                        undefined ->
                            elib_response:error(Req, <<"二维码已过期，请刷新"/utf8>>, ?ERR_QR_LOGIN_EXPIRED);
                        Session ->
                            ExpiresAt = maps:get(<<"expires_at">>, Session, 0),
                            case erlang:system_time(millisecond) > ExpiresAt of
                                true ->
                                    delete_session(SessionToken),
                                    elib_response:error(
                                        Req,
                                        <<"二维码已过期，请刷新"/utf8>>,
                                        ?ERR_QR_LOGIN_EXPIRED
                                    );
                                false ->
                                    Status = maps:get(<<"status">>, Session, <<"waiting">>),
                                    case Status of
                                        <<"waiting">> ->
                                            elib_response:error(
                                                Req,
                                                <<"二维码尚未扫码"/utf8>>,
                                                ?ERR_QR_LOGIN_NOT_SCANNED
                                            );
                                        <<"cancelled">> ->
                                            elib_response:error(
                                                Req,
                                                <<"登录已取消，请刷新"/utf8>>,
                                                ?ERR_QR_LOGIN_CANCELLED
                                            );
                                        <<"confirmed">> ->
                                            elib_response:error(
                                                Req,
                                                <<"二维码已确认，请刷新"/utf8>>,
                                                ?ERR_QR_LOGIN_ALREADY_USED
                                            );
                                        <<"scanned">> ->
                                            ScannedBy = maps:get(<<"scanned_by">>, Session, 0),
                                            case ScannedBy of
                                                0 ->
                                                    elib_response:error(
                                                        Req,
                                                        <<"二维码尚未扫码"/utf8>>,
                                                        ?ERR_QR_LOGIN_NOT_SCANNED
                                                    );
                                                Uid ->
                                                    LoginToken = generate_login_token(Uid),
                                                    UpdatedSession = Session#{
                                                        <<"status">> => <<"confirmed">>,
                                                        <<"login_token">> => LoginToken,
                                                        <<"confirmed_at">> => erlang:system_time(
                                                            millisecond
                                                        )
                                                    },
                                                    cache_session(SessionToken, UpdatedSession),
                                                    %% PR-2β: 广播 confirmed + login_token 给 SSE 订阅者
                                                    %% Web 端收到后调 _completeLogin 落地 token，无需轮询。
                                                    _ = qr_login_event_ds:notify(
                                                        SessionToken,
                                                        qr_login_event_ds:event(
                                                            confirmed, LoginToken
                                                        )
                                                    ),

                                                    DeviceId = maps:get(<<"device_id">>, Session),
                                                    DeviceName = maps:get(
                                                        <<"device_name">>, Session
                                                    ),
                                                    Platform = maps:get(<<"platform">>, Session),
                                                    log_device_login(
                                                        Uid, DeviceId, DeviceName, Platform
                                                    ),
                                                    elib_response:success(
                                                        Req,
                                                        #{<<"status">> => <<"confirmed">>}
                                                    );
                                                _ ->
                                                    elib_response:error(
                                                        Req,
                                                        <<"无权限操作"/utf8>>,
                                                        ?ERR_FORBIDDEN
                                                    )
                                            end;
                                        _ ->
                                            elib_response:error(
                                                Req,
                                                <<"参数错误"/utf8>>,
                                                ?ERR_BAD_REQUEST
                                            )
                                    end
                            end
                    end
            end
    end.

%% @doc 取消登录
%% POST /v1/passport/qr_login/cancel
%% Body: {"session_token": "xxx"}
handle_cancel(Req, _State) ->
    {ok, Body, _} = cowboy_req:read_body(Req),
    Data = jsx:decode(Body, [return_maps]),

    SessionToken = maps:get(<<"session_token">>, Data, <<>>),

    case SessionToken of
        <<>> ->
            elib_response:error(Req, <<"参数错误"/utf8>>, ?ERR_BAD_REQUEST);
        _ ->
            case get_session(SessionToken) of
                undefined ->
                    elib_response:success(Req, #{<<"status">> => <<"cancelled">>});
                Session ->
                    % 更新状态
                    UpdatedSession = Session#{
                        <<"status">> => <<"cancelled">>
                    },
                    cache_session(SessionToken, UpdatedSession),
                    elib_response:success(Req, #{<<"status">> => <<"cancelled">>})
            end
    end.

%% ===================================================================
%% Internal Functions
%% ===================================================================

%% @doc 生成会话 Token
-spec generate_session_token() -> binary().
generate_session_token() ->
    % 使用 crypto 生成安全的随机 Token
    RandomBytes = crypto:strong_rand_bytes(32),
    Timestamp = erlang:system_time(millisecond),
    Data = <<RandomBytes/binary, (integer_to_binary(Timestamp))/binary>>,
    base64:encode(Data).

%% @doc 生成 QR Token
-spec generate_qr_token(binary()) -> binary().
generate_qr_token(SessionToken) ->
    % 简单编码：base64(session_token + timestamp)
    Timestamp = integer_to_binary(erlang:system_time(millisecond)),
    Data = <<SessionToken/binary, ":", Timestamp/binary>>,
    base64:encode(Data).

%% @doc 解析 QR Token
-spec parse_qr_token(binary()) -> {ok, binary()} | {error, term()}.
parse_qr_token(QRToken) ->
    try
        Decoded = base64:decode(QRToken),
        [SessionToken, _Timestamp] = binary:split(Decoded, <<":">>),
        {ok, SessionToken}
    catch
        _:_ -> {error, invalid_format}
    end.

%% @doc 生成登录 Token
-spec generate_login_token(integer()) -> binary().
generate_login_token(Uid) ->
    % 调用 token_ds 生成 JWT Token
    token_ds:encrypt_token(Uid).

%% @doc 缓存会话
-spec cache_session(binary(), map()) -> ok.
cache_session(SessionToken, SessionData) ->
    Key = {qr_login, SessionToken},
    % 60 秒过期
    imboy_cache:set(Key, SessionData, 60),
    ok.

%% @doc 获取会话
-spec get_session(binary()) -> map() | undefined.
get_session(SessionToken) ->
    Key = {qr_login, SessionToken},
    case imboy_cache:get(Key) of
        {ok, Session} -> Session;
        undefined -> undefined
    end.

%% @doc 删除会话
-spec delete_session(binary()) -> ok.
delete_session(SessionToken) ->
    Key = {qr_login, SessionToken},
    imboy_cache:delete(Key),
    ok.

%% @doc 记录设备登录
-spec log_device_login(integer(), binary(), binary(), binary()) -> ok.
log_device_login(Uid, DeviceId, DeviceName, Platform) ->
    qr_login_logic:log_device_login(Uid, DeviceId, DeviceName, Platform).
