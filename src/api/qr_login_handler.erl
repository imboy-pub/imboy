%%%-------------------------------------------------------------------
%%% @doc QR 码登录处理器
%%% 提供 WhatsApp Web 风格的扫码登录功能
%%%
%%% G-QR（收口 C08）：`qr_login_authority = pg | cache`（默认 cache）。
%%% - cache：原 depcache 路径（生产行为不变，迁移未启用期间的兼容层）；
%%% - pg：qr_login_session_ds（薄转发 qr_login_session_repo）PostgreSQL 权威——scan/confirm/cancel 状态机
%%%   CAS、status 原子 claim/consume（UPDATE...RETURNING 单语句原子性）；
%%%   confirm 的 SSE 通知为 confirmed_ready 无 token 信号，token 一律经
%%%   原子领取（SSE 服务端代领或 status 轮询领取，同一消费语义）。
%%%
%%% API 端点:
%%% - POST /api/v1/passport/qr_login/create  - 创建登录二维码
%%% - GET  /api/v1/passport/qr_login/status  - 查询扫码状态
%%% - POST /api/v1/passport/qr_login/scan    - 扫码（手机端调用）
%%% - POST /api/v1/passport/qr_login/confirm - 确认登录（手机端调用）
%%% - POST /api/v1/passport/qr_login/cancel  - 取消登录
%%% @end
%%%-------------------------------------------------------------------
-module(qr_login_handler).
-behaviour(cowboy_rest).

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
%% G-QR 权威选择
%% ===================================================================

%% @doc QR 会话权威存储：cache（原 depcache）| pg（qr_login_session_ds → qr_login_session_repo）。
%% pg 模式要求 00000170_qr_login_session 迁移已在目标库 apply
%% （本轮仅在专属临时测试库验证；生产启用属部署决策）。
-spec authority() -> cache | pg.
authority() ->
    case application:get_env(imboy, qr_login_authority, cache) of
        pg -> pg;
        _ -> cache
    end.

%% @doc PG：QR token 反查 session_token。
-spec pg_parse_qr_token(binary()) -> {ok, binary()} | {error, term()}.
pg_parse_qr_token(QRToken) ->
    case qr_login_session_ds:find_by_qr_token(QRToken) of
        #{<<"session_token">> := ST} -> {ok, ST};
        undefined -> {error, invalid_format};
        {error, Reason} -> {error, Reason}
    end.

%% @doc QR token 解析（权威自适应）。
-spec parse_qr_token_by_authority(binary()) -> {ok, binary()} | {error, term()}.
parse_qr_token_by_authority(QRToken) ->
    case authority() of
        pg -> pg_parse_qr_token(QRToken);
        cache -> parse_qr_token(QRToken)
    end.

%% ===================================================================
%% API Handlers
%% ===================================================================

%% @doc 创建登录二维码
%% POST /api/v1/passport/qr_login/create
%% Body: {"device_id": "xxx", "device_name": "Web Browser", "platform": "web", "purpose": "human"|"seat"}
handle_create(Req) ->
    {ok, Body, _} = cowboy_req:read_body(Req),
    Data = jsone:decode(Body),
    DeviceId = maps:get(<<"device_id">>, Data, <<>>),
    DeviceName = maps:get(<<"device_name">>, Data, <<"Web Browser">>),
    Platform = maps:get(<<"platform">>, Data, <<"web">>),
    Purpose = maps:get(<<"purpose">>, Data, <<"human">>),

    case
        is_binary(DeviceId) andalso DeviceId =/= <<>> andalso
            (Purpose =:= <<"human">> orelse Purpose =:= <<"seat">>)
    of
        false ->
            elib_response:error(Req, <<"参数错误"/utf8>>, ?ERR_BAD_REQUEST);
        true ->
            % 生成唯一的会话 Token
            SessionToken = generate_session_token(),
            SessionData = #{
                <<"device_id">> => DeviceId,
                <<"device_name">> => DeviceName,
                <<"platform">> => Platform,
                <<"purpose">> => Purpose,
                <<"expires_at_ms">> => erlang:system_time(millisecond) + 60000
            },
            QRToken =
                case authority() of
                    pg ->
                        pg_create_session(SessionToken, SessionData);
                    cache ->
                        cache_create_session(SessionToken, SessionData)
                end,
            Payload = #{
                <<"qr_token">> => QRToken,
                <<"session_token">> => SessionToken,
                <<"expires_in">> => 60
            },
            elib_response:success(Req, Payload)
    end.

%% @doc PG：创建会话（status=waiting；qr_token 映射入表 UNIQUE 反查）。
-spec pg_create_session(binary(), map()) -> binary().
pg_create_session(SessionToken, SessionData) ->
    QRToken = generate_qr_token_value(),
    ok = qr_login_session_ds:create(#{
        session_token => SessionToken,
        qr_token => QRToken,
        device_id => maps:get(<<"device_id">>, SessionData),
        device_name => maps:get(<<"device_name">>, SessionData),
        platform => maps:get(<<"platform">>, SessionData),
        purpose => maps:get(<<"purpose">>, SessionData, <<"human">>),
        expires_at_ms => maps:get(<<"expires_at_ms">>, SessionData)
    }),
    QRToken.

%% @doc cache：创建会话（原路径：qr 映射 + 会话入 depcache 60s）。
-spec cache_create_session(binary(), map()) -> binary().
cache_create_session(SessionToken, SessionData) ->
    QRToken = generate_qr_token(SessionToken),
    cache_session(SessionToken, SessionData#{
        <<"session_token">> => SessionToken,
        <<"qr_token">> => QRToken,
        <<"status">> => <<"waiting">>,
        <<"created_at">> => erlang:system_time(millisecond),
        <<"expires_at">> => maps:get(<<"expires_at_ms">>, SessionData)
    }),
    QRToken.

%% @doc 查询扫码状态
%% GET /api/v1/passport/qr_login/status?session_token=xxx
handle_status(Req) ->
    Qs = cowboy_req:parse_qs(Req),
    SessionToken = proplists:get_value(<<"session_token">>, Qs, <<>>),

    case SessionToken of
        <<>> ->
            elib_response:error(Req, <<"参数错误"/utf8>>, ?ERR_BAD_REQUEST);
        _ ->
            case authority() of
                pg -> pg_handle_status(Req, SessionToken);
                cache -> cache_handle_status(Req, SessionToken)
            end
    end.

%% @doc PG 权威 status：confirmed 时原子 claim（UPDATE...RETURNING 行锁
%% 保证并发/SSE 双读竞争下恰好一个消费者拿到 token，G-QR oracle）。
-spec pg_handle_status(cowboy_req:req(), binary()) -> cowboy_req:req().
pg_handle_status(Req, SessionToken) ->
    case qr_login_session_ds:find_by_session_token(SessionToken) of
        undefined ->
            elib_response:error(Req, <<"会话不存在或已过期"/utf8>>, ?ERR_NOT_FOUND);
        {error, _} ->
            elib_response:error(Req, <<"会话不存在或已过期"/utf8>>, ?ERR_NOT_FOUND);
        Session ->
            case maps:get(<<"status">>, Session, <<"waiting">>) of
                <<"confirmed">> ->
                    case qr_login_session_ds:claim_login_token(SessionToken) of
                        {ok, #{login_token := LoginToken, scanned_by := Uid}} ->
                            elib_response:success(Req, #{
                                <<"status">> => <<"confirmed">>,
                                <<"token">> => LoginToken,
                                <<"uid">> => Uid
                            });
                        {error, not_claimable} ->
                            %% 已消费/过期：既有兼容非成功响应（C08 oracle）
                            elib_response:error(
                                Req, <<"二维码已确认，请刷新"/utf8>>, ?ERR_QR_LOGIN_ALREADY_USED
                            )
                    end;
                Status ->
                    elib_response:success(Req, #{<<"status">> => Status})
            end
    end.

%% @doc 原 cache 路径 status（行为保持；迁移未启用期间的生产路径）。
-spec cache_handle_status(cowboy_req:req(), binary()) -> cowboy_req:req().
cache_handle_status(Req, SessionToken) ->
    case get_session(SessionToken) of
        undefined ->
            elib_response:error(Req, <<"会话不存在或已过期"/utf8>>, ?ERR_NOT_FOUND);
        Session ->
            Status = maps:get(<<"status">>, Session, <<"waiting">>),
            LoginToken = maps:get(<<"login_token">>, Session, null),
            case Status of
                <<"confirmed">> when LoginToken =/= null ->
                    %% login_token 一次性消费：取走即销毁会话，
                    %% 否则会话剩余 TTL 内任何知道 session_token 的人
                    %% 都能重复取到同一个可登录 token。
                    %% ponytail: 只删会话本体；qr_token 映射 60s 自然过期，
                    %% 且会话已消失时它单独存在无意义（scan/confirm 会判"已过期"）。
                    delete_session(SessionToken),
                    %% 带上 uid：客户端 QR 登录落地需要写 currentUid
                    %% 建立登录态（路由守卫 isLoggedIn 依据），
                    %% token 是加密串客户端自己解不出 uid（批次126）。
                    Payload = #{
                        <<"status">> => <<"confirmed">>,
                        <<"token">> => LoginToken,
                        <<"uid">> => maps:get(<<"scanned_by">>, Session, 0)
                    },
                    elib_response:success(Req, Payload);
                _ ->
                    elib_response:success(Req, #{<<"status">> => Status})
            end
    end.

%% @doc 扫码（手机端调用）
%% POST /api/v1/passport/qr_login/scan
%% Body: {"qr_token": "xxx"}
handle_scan(Req, State) ->
    Uid = maps:get(current_uid, State, 0),
    {ok, Body, _} = cowboy_req:read_body(Req),
    Data = jsone:decode(Body),

    QRToken = maps:get(<<"qr_token">>, Data, <<>>),

    case {Uid, QRToken} of
        {0, _} ->
            %% 扫码必须是已登录的手机端用户；匿名请求（未带/带无效 token）会把
            %% scanned_by 写成 0，导致真实用户随后确认登录时被误判为无权限。
            elib_response:error(Req, <<"未登录"/utf8>>, ?ERR_UNAUTHORIZED);
        {_, <<>>} ->
            elib_response:error(Req, <<"参数错误"/utf8>>, ?ERR_BAD_REQUEST);
        _ ->
            case parse_qr_token_by_authority(QRToken) of
                {error, _} ->
                    elib_response:error(Req, <<"无效的二维码"/utf8>>, ?ERR_INVALID_QR_TOKEN);
                {ok, SessionToken} ->
                    case authority() of
                        pg -> pg_handle_scan(Req, SessionToken, Uid);
                        cache -> cache_handle_scan(Req, SessionToken, Uid)
                    end
            end
    end.

%% @doc PG scan：waiting→scanned CAS；非转移结果按权威状态映射既有错误码。
-spec pg_handle_scan(cowboy_req:req(), binary(), integer()) -> cowboy_req:req().
pg_handle_scan(Req, SessionToken, Uid) ->
    case qr_login_session_ds:transition_scan(SessionToken, Uid) of
        {ok, Row} ->
            %% PR-2β：广播 scanned 事件给 SSE 订阅者
            _ = qr_login_event_ds:notify(
                SessionToken, qr_login_event_ds:event(scanned, undefined)
            ),
            elib_response:success(Req, #{
                <<"status">> => <<"scanned">>,
                <<"device_name">> => maps:get(<<"device_name">>, Row, <<>>),
                <<"platform">> => maps:get(<<"platform">>, Row, <<>>)
            });
        {error, not_transitioned} ->
            pg_scan_error(Req, SessionToken, Uid)
    end.

-spec pg_scan_error(cowboy_req:req(), binary(), integer()) -> cowboy_req:req().
pg_scan_error(Req, SessionToken, Uid) ->
    case qr_login_session_ds:find_by_session_token(SessionToken) of
        undefined ->
            elib_response:error(Req, <<"二维码已过期，请刷新"/utf8>>, ?ERR_QR_LOGIN_EXPIRED);
        #{<<"status">> := <<"confirmed">>} ->
            elib_response:error(Req, <<"二维码已确认，请刷新"/utf8>>, ?ERR_QR_LOGIN_ALREADY_USED);
        #{<<"status">> := <<"cancelled">>} ->
            elib_response:error(Req, <<"登录已取消，请刷新"/utf8>>, ?ERR_QR_LOGIN_CANCELLED);
        #{<<"status">> := <<"scanned">>, <<"scanned_by">> := Other} when Other =/= Uid ->
            elib_response:error(Req, <<"无权限操作"/utf8>>, ?ERR_FORBIDDEN);
        #{<<"status">> := <<"scanned">>} ->
            %% 同一用户重复扫码：幂等成功（CAS 拒绝重复转移，状态不变）
            elib_response:success(Req, #{<<"status">> => <<"scanned">>});
        _ ->
            elib_response:error(Req, <<"二维码已过期，请刷新"/utf8>>, ?ERR_QR_LOGIN_EXPIRED)
    end.

%% @doc 原 cache 路径 scan（行为保持）。
-spec cache_handle_scan(cowboy_req:req(), binary(), integer()) -> cowboy_req:req().
cache_handle_scan(Req, SessionToken, Uid) ->
    case get_session(SessionToken) of
        undefined ->
            elib_response:error(Req, <<"二维码已过期，请刷新"/utf8>>, ?ERR_QR_LOGIN_EXPIRED);
        Session ->
            Status = maps:get(<<"status">>, Session, <<"waiting">>),
            case Status of
                <<"confirmed">> ->
                    elib_response:error(
                        Req, <<"二维码已确认，请刷新"/utf8>>, ?ERR_QR_LOGIN_ALREADY_USED
                    );
                <<"cancelled">> ->
                    elib_response:error(
                        Req, <<"登录已取消，请刷新"/utf8>>, ?ERR_QR_LOGIN_CANCELLED
                    );
                _ ->
                    ExpiresAt = maps:get(<<"expires_at">>, Session, 0),
                    case erlang:system_time(millisecond) > ExpiresAt of
                        true ->
                            delete_session(SessionToken),
                            elib_response:error(
                                Req, <<"二维码已过期，请刷新"/utf8>>, ?ERR_QR_LOGIN_EXPIRED
                            );
                        false ->
                            ScannedBy = maps:get(<<"scanned_by">>, Session, 0),
                            case
                                Status =:= <<"scanned">> andalso
                                    ScannedBy =/= 0 andalso
                                    ScannedBy =/= Uid
                            of
                                true ->
                                    elib_response:error(Req, <<"无权限操作"/utf8>>, ?ERR_FORBIDDEN);
                                false ->
                                    UpdatedSession = Session#{
                                        <<"status">> => <<"scanned">>,
                                        <<"scanned_by">> => Uid,
                                        <<"scanned_at">> => erlang:system_time(millisecond)
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
                                    DeviceName = maps:get(<<"device_name">>, Session, <<>>),
                                    Platform = maps:get(<<"platform">>, Session, <<>>),
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
    end.

%% @doc 确认登录（手机端调用）
%% POST /api/v1/passport/qr_login/confirm
%% Body: {"qr_token": "xxx"}
handle_confirm(Req, State) ->
    Uid = maps:get(current_uid, State, 0),
    {ok, Body, _} = cowboy_req:read_body(Req),
    Data = jsone:decode(Body),

    QRToken = maps:get(<<"qr_token">>, Data, <<>>),

    case QRToken of
        <<>> ->
            elib_response:error(Req, <<"参数错误"/utf8>>, ?ERR_BAD_REQUEST);
        _ ->
            case parse_qr_token_by_authority(QRToken) of
                {error, _} ->
                    elib_response:error(Req, <<"无效的二维码"/utf8>>, ?ERR_INVALID_QR_TOKEN);
                {ok, SessionToken} ->
                    case authority() of
                        pg -> pg_handle_confirm(Req, SessionToken, Uid);
                        cache -> cache_handle_confirm(Req, SessionToken, Uid)
                    end
            end
    end.

%% @doc PG confirm：scanned→confirmed CAS 单语句原子写入 login_token；
%% SSE 只发 confirmed_ready 无 token 信号（token 一律走原子领取，C08）。
-spec pg_handle_confirm(cowboy_req:req(), binary(), integer()) -> cowboy_req:req().
pg_handle_confirm(Req, SessionToken, Uid) ->
    case qr_login_session_ds:find_by_session_token(SessionToken) of
        undefined ->
            elib_response:error(Req, <<"二维码已过期，请刷新"/utf8>>, ?ERR_QR_LOGIN_EXPIRED);
        {error, _} ->
            elib_response:error(Req, <<"二维码已过期，请刷新"/utf8>>, ?ERR_QR_LOGIN_EXPIRED);
        #{<<"status">> := <<"waiting">>} ->
            elib_response:error(Req, <<"二维码尚未扫码"/utf8>>, ?ERR_QR_LOGIN_NOT_SCANNED);
        #{<<"status">> := <<"cancelled">>} ->
            elib_response:error(Req, <<"登录已取消，请刷新"/utf8>>, ?ERR_QR_LOGIN_CANCELLED);
        #{<<"status">> := <<"confirmed">>} ->
            elib_response:error(Req, <<"二维码已确认，请刷新"/utf8>>, ?ERR_QR_LOGIN_ALREADY_USED);
        #{<<"status">> := <<"scanned">>, <<"scanned_by">> := ScannedBy} ->
            case ScannedBy of
                0 ->
                    elib_response:error(Req, <<"二维码尚未扫码"/utf8>>, ?ERR_QR_LOGIN_NOT_SCANNED);
                Uid ->
                    pg_confirm_as_scanner(Req, SessionToken, Uid);
                _ ->
                    elib_response:error(Req, <<"无权限操作"/utf8>>, ?ERR_FORBIDDEN)
            end;
        _ ->
            elib_response:error(Req, <<"二维码已过期，请刷新"/utf8>>, ?ERR_QR_LOGIN_EXPIRED)
    end.

-spec pg_confirm_as_scanner(cowboy_req:req(), binary(), integer()) -> cowboy_req:req().
pg_confirm_as_scanner(Req, SessionToken, Uid) ->
    case qr_login_session_ds:find_by_session_token(SessionToken) of
        #{
            <<"device_id">> := DeviceId,
            <<"device_name">> := DeviceName,
            <<"platform">> := Platform,
            <<"purpose">> := Purpose
        } ->
            %% E2EE-013：先写设备行再签发 token（批次约束保持）
            log_device_login(Uid, DeviceId, DeviceName, Platform),
            LoginToken = generate_login_token(Uid, DeviceId, Purpose),
            case qr_login_session_ds:transition_confirm(SessionToken, Uid, LoginToken) of
                {ok, _} ->
                    %% C08：syn 只发事件通知，不广播 login_token；
                    %% Web 端落地 token 经 SSE 服务端代领或 status 原子领取
                    _ = qr_login_event_ds:notify(
                        SessionToken,
                        qr_login_event_ds:event(confirmed_ready, undefined)
                    ),
                    elib_response:success(Req, #{<<"status">> => <<"confirmed">>});
                {error, not_transitioned} ->
                    %% 并发 confirm/会话过期：既有兼容非成功响应
                    elib_response:error(
                        Req, <<"二维码已确认，请刷新"/utf8>>, ?ERR_QR_LOGIN_ALREADY_USED
                    )
            end;
        _ ->
            elib_response:error(Req, <<"二维码已过期，请刷新"/utf8>>, ?ERR_QR_LOGIN_EXPIRED)
    end.

%% @doc 原 cache 路径 confirm（行为保持）。
-spec cache_handle_confirm(cowboy_req:req(), binary(), integer()) -> cowboy_req:req().
cache_handle_confirm(Req, SessionToken, Uid) ->
    case get_session(SessionToken) of
        undefined ->
            elib_response:error(Req, <<"二维码已过期，请刷新"/utf8>>, ?ERR_QR_LOGIN_EXPIRED);
        Session ->
            ExpiresAt = maps:get(<<"expires_at">>, Session, 0),
            case erlang:system_time(millisecond) > ExpiresAt of
                true ->
                    delete_session(SessionToken),
                    elib_response:error(
                        Req, <<"二维码已过期，请刷新"/utf8>>, ?ERR_QR_LOGIN_EXPIRED
                    );
                false ->
                    Status = maps:get(<<"status">>, Session, <<"waiting">>),
                    case Status of
                        <<"waiting">> ->
                            elib_response:error(
                                Req, <<"二维码尚未扫码"/utf8>>, ?ERR_QR_LOGIN_NOT_SCANNED
                            );
                        <<"cancelled">> ->
                            elib_response:error(
                                Req, <<"登录已取消，请刷新"/utf8>>, ?ERR_QR_LOGIN_CANCELLED
                            );
                        <<"confirmed">> ->
                            elib_response:error(
                                Req, <<"二维码已确认，请刷新"/utf8>>, ?ERR_QR_LOGIN_ALREADY_USED
                            );
                        <<"scanned">> ->
                            ScannedBy = maps:get(<<"scanned_by">>, Session, 0),
                            case ScannedBy of
                                0 ->
                                    elib_response:error(
                                        Req, <<"二维码尚未扫码"/utf8>>, ?ERR_QR_LOGIN_NOT_SCANNED
                                    );
                                Uid ->
                                    %% E2EE-013：被登录的设备是 create 时 Web 端上报的
                                    %% device_id（不是扫码的手机）。必须先写 user_device
                                    %% 行、再签发/广播 token，否则 Web 端可能拿着绑定
                                    %% did 的 token 抢在设备行落库前发请求，被设备级
                                    %% 校验当成不存在的设备拒掉。
                                    DeviceId = maps:get(<<"device_id">>, Session),
                                    DeviceName = maps:get(<<"device_name">>, Session),
                                    Platform = maps:get(<<"platform">>, Session),
                                    log_device_login(Uid, DeviceId, DeviceName, Platform),
                                    LoginToken = generate_login_token(
                                        Uid,
                                        DeviceId,
                                        maps:get(<<"purpose">>, Session, <<"human">>)
                                    ),
                                    UpdatedSession = Session#{
                                        <<"status">> => <<"confirmed">>,
                                        <<"login_token">> => LoginToken,
                                        <<"confirmed_at">> => erlang:system_time(millisecond)
                                    },
                                    cache_session(SessionToken, UpdatedSession),
                                    %% PR-2β: 广播 confirmed + login_token 给 SSE 订阅者
                                    %% Web 端收到后调 _completeLogin 落地 token，无需轮询。
                                    _ = qr_login_event_ds:notify(
                                        SessionToken,
                                        qr_login_event_ds:event(confirmed, LoginToken)
                                    ),
                                    elib_response:success(
                                        Req, #{<<"status">> => <<"confirmed">>}
                                    );
                                _ ->
                                    elib_response:error(Req, <<"无权限操作"/utf8>>, ?ERR_FORBIDDEN)
                            end;
                        _ ->
                            elib_response:error(Req, <<"参数错误"/utf8>>, ?ERR_BAD_REQUEST)
                    end
            end
    end.

%% @doc 取消登录
%% POST /api/v1/passport/qr_login/cancel
%% Body: {"session_token": "xxx"}
handle_cancel(Req, _State) ->
    {ok, Body, _} = cowboy_req:read_body(Req),
    Data = jsone:decode(Body),

    SessionToken = maps:get(<<"session_token">>, Data, <<>>),

    case SessionToken of
        <<>> ->
            elib_response:error(Req, <<"参数错误"/utf8>>, ?ERR_BAD_REQUEST);
        _ ->
            case authority() of
                pg -> pg_handle_cancel(Req, SessionToken);
                cache -> cache_handle_cancel(Req, SessionToken)
            end
    end.

%% @doc PG cancel：waiting|scanned→cancelled CAS。
%% confirmed 后不可取消（G-QR：并发 confirm/cancel 结果唯一——已确认即生效），
%% 返回既有兼容 ALREADY_USED 错误码（cache 路径原实现会覆盖 confirmed 状态，
%% 属埋雷语义；PG 权威修正，边缘差异记录于收口文档）。
-spec pg_handle_cancel(cowboy_req:req(), binary()) -> cowboy_req:req().
pg_handle_cancel(Req, SessionToken) ->
    case qr_login_session_ds:transition_cancel(SessionToken) of
        ok ->
            elib_response:success(Req, #{<<"status">> => <<"cancelled">>});
        {error, not_transitioned} ->
            case qr_login_session_ds:find_by_session_token(SessionToken) of
                undefined ->
                    %% 不存在/过期：幂等成功（与 cache 路径对齐）
                    elib_response:success(Req, #{<<"status">> => <<"cancelled">>});
                _ ->
                    elib_response:error(
                        Req, <<"二维码已确认，请刷新"/utf8>>, ?ERR_QR_LOGIN_ALREADY_USED
                    )
            end
    end.

%% @doc 原 cache 路径 cancel（行为保持）。
-spec cache_handle_cancel(cowboy_req:req(), binary()) -> cowboy_req:req().
cache_handle_cancel(Req, SessionToken) ->
    case get_session(SessionToken) of
        undefined ->
            elib_response:success(Req, #{<<"status">> => <<"cancelled">>});
        Session ->
            UpdatedSession = Session#{
                <<"status">> => <<"cancelled">>
            },
            cache_session(SessionToken, UpdatedSession),
            elib_response:success(Req, #{<<"status">> => <<"cancelled">>})
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

%% @doc 生成 QR Token 值（不落任何存储；存储映射由权威决定）。
-spec generate_qr_token_value() -> binary().
generate_qr_token_value() ->
    base64:encode(crypto:strong_rand_bytes(32)).

%% @doc 生成 QR Token（cache 权威：随机值 + 缓存反查映射，TTL 60 秒）
%%
%% 二维码图像会展示在屏幕上（截屏、投屏、肩窥都能拿到），因此其内容
%% **不得可反解出 session_token**。旧实现是 base64(session_token ++ ":" ++ ts)，
%% 见到二维码 = 拿到 session_token = 可轮询 /status 或订阅 /subscribe，
%% 在真实用户手机确认的瞬间窃取 login_token 接管账号。
%% 现改为独立随机值 + 缓存反查映射，TTL 与会话同为 60 秒。
-spec generate_qr_token(binary()) -> binary().
generate_qr_token(SessionToken) ->
    QRToken = generate_qr_token_value(),
    imboy_cache:set({qr_login_qr, QRToken}, SessionToken, 60),
    QRToken.

%% @doc 解析 QR Token（缓存反查，不可由 QR 内容推导）
-spec parse_qr_token(binary()) -> {ok, binary()} | {error, term()}.
parse_qr_token(QRToken) ->
    case imboy_cache:get({qr_login_qr, QRToken}) of
        {ok, SessionToken} when is_binary(SessionToken) -> {ok, SessionToken};
        _ -> {error, invalid_format}
    end.

%% @doc 生成登录 Token（E2EE-013：绑定被登录设备的 DID）
-spec generate_login_token(integer(), binary(), binary()) -> binary().
generate_login_token(Uid, Did, <<"human">>) ->
    token_ds:encrypt_token(Uid, Did);
generate_login_token(Uid, Did, <<"seat">>) ->
    token_ds:encrypt_seat_token(Uid, Did).

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
