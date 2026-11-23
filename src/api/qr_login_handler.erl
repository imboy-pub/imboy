%%%-------------------------------------------------------------------
%%% @doc QR 码登录处理器
%%% 提供 WhatsApp Web 风格的扫码登录功能
%%%
%%% LEE-5 / G-QR（收口 C08）：**PostgreSQL 是 QR 登录唯一权威**。
%%% - `qr_login_authority` 仅接受 `pg | postgres`（默认 `pg`）；其他任何值
%%%   （含显式 `cache`）触发节点级一次告警并按 `pg` 处理——配置无法削弱
%%%   安全。原 cache 权威路径（非原子领取，W1-06 竞态 + SSE 零消费语义）
%%%   已整体删除，cache 不保存也不发放任何 LoginToken。
%%% - LoginToken 一律经 `qr_login_session_ds:claim_login_token` 原子领取
%%%   （UPDATE ... RETURNING OLD 单语句，恰好一个消费者；SSE 服务端代领
%%%   与 status 轮询同一消费语义）；confirm 的 SSE 通知为 confirmed_ready
%%%   无 token 信号。
%%% - PG 不可用 / 版本不足（claim 依赖 PG18 RETURNING OLD）/ 迁移缺失时
%%%   fail closed 明确拒绝（HTTP 200 信封业务码 503），**绝不降级 cache**。
%%% - 过期行由 qr_login_cleanup_worker 批量清扫（过期安全另由各语句内联
%%%   expires_at 判断保证，不依赖清扫）。
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
    %% LEE-5 配置门禁：每请求校验 qr_login_authority（仅 pg|postgres 合法，
    %% 非法值告警并按 pg——唯一权威，见 authority/0）。
    _ = authority(),
    Action = maps:get(action, State, undefined),
    Method = cowboy_req:method(Req0),

    Req1 =
        case {Method, Action} of
            {<<"POST">>, create} -> handle_create(Req0, State);
            {<<"GET">>, status} -> handle_status(Req0, State);
            {<<"POST">>, scan} -> handle_scan(Req0, State);
            {<<"POST">>, confirm} -> handle_confirm(Req0, State);
            {<<"POST">>, cancel} -> handle_cancel(Req0, State);
            _ -> elib_response:error(Req0, <<"Bad Request"/utf8>>, ?ERR_BAD_REQUEST)
        end,
    {stop, Req1, State}.

%% ===================================================================
%% LEE-5 权威（唯一：PostgreSQL）
%% ===================================================================

%% @doc QR 会话权威：**恒为 pg**（qr_login_session_ds → qr_login_session_repo）。
%% `qr_login_authority` 仅接受 pg | postgres（默认 pg）；其他任何值（含显式
%% cache——原非原子路径已删除）触发节点级一次告警后按 pg 处理：配置无法
%% 削弱安全，也不会因误配导致服务中断（能力门禁另由 repo 层
%% ensure_claim_compatible 保证：PG18 以下/PG 故障 fail closed 明确拒绝）。
-spec authority() -> pg.
authority() ->
    case application:get_env(imboy, qr_login_authority, pg) of
        pg ->
            pg;
        postgres ->
            pg;
        Other ->
            warn_authority_override(Other),
            pg
    end.

%% @doc 非法 qr_login_authority 值的节点级一次告警（persistent_term 去重）。
-spec warn_authority_override(term()) -> ok.
warn_authority_override(Other) ->
    Key = {imboy, qr_login_authority_warned},
    case persistent_term:get(Key, false) of
        true ->
            ok;
        false ->
            persistent_term:put(Key, true),
            ?WARN_LOG(
                "[lee-5] qr_login_authority=~p 被拒绝并按 pg 处理：PostgreSQL 是"
                "QR 登录唯一权威（仅接受 pg|postgres，默认 pg）；cache 领取路径"
                "已删除，绝不降级",
                [Other]
            )
    end.

%% @doc QR token 反查 session_token（PG 权威；qr_token 不可反解）。
-spec parse_qr_token(binary()) -> {ok, binary()} | {error, term()}.
parse_qr_token(QRToken) ->
    case qr_login_session_ds:find_by_qr_token(QRToken) of
        #{<<"session_token">> := ST} -> {ok, ST};
        undefined -> {error, invalid_format};
        {error, Reason} -> {error, {pg_error, Reason}}
    end.

%% ===================================================================
%% API Handlers
%% ===================================================================

%% @doc 创建登录二维码
%% POST /api/v1/passport/qr_login/create
%% Body: {"device_id": "xxx", "device_name": "Web Browser", "platform": "web", "purpose": "human"|"seat"}
handle_create(Req, State) ->
    {ok, Body, _} = cowboy_req:read_body(Req),
    Data =
        try jsone:decode(Body) of
            Parsed when is_map(Parsed) -> Parsed;
            _ -> #{}
        catch
            _:_ -> #{}
        end,
    DeviceId = maps:get(<<"device_id">>, Data, <<>>),
    DeviceName = maps:get(<<"device_name">>, Data, <<"Web Browser">>),
    IsWeb = maps:get(jwt_purpose, State, human) =:= web_human,
    Platform =
        case IsWeb of
            true -> <<"web">>;
            false -> maps:get(<<"platform">>, Data, <<"web">>)
        end,
    Purpose = maps:get(<<"purpose">>, Data, <<"human">>),
    WebValid =
        not IsWeb orelse
            (Purpose =:= <<"human">> andalso valid_utf8_length(DeviceId, 128) andalso
                valid_utf8_length(DeviceName, 80)),

    case
        is_binary(DeviceId) andalso DeviceId =/= <<>> andalso
            (Purpose =:= <<"human">> orelse Purpose =:= <<"seat">>) andalso WebValid
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
            case pg_create_session(SessionToken, SessionData) of
                {ok, QRToken} ->
                    Payload = #{
                        <<"qr_token">> => QRToken,
                        <<"session_token">> => SessionToken,
                        <<"expires_in">> => 60
                    },
                    elib_response:success(Req, Payload);
                {error, Reason} ->
                    %% LEE-5 fail closed：PG18 门禁不过/PG 不可用/迁移缺失/
                    %% 冲突——明确拒绝创建，绝不降级 cache。
                    ?ERROR_LOG(
                        "[lee-5] qr_login create rejected (fail closed): ~p", [Reason]
                    ),
                    elib_response:error(
                        Req,
                        <<"服务暂不可用，请稍后再试"/utf8>>,
                        ?ERR_SERVICE_UNAVAILABLE
                    )
            end
    end.

%% @doc 创建会话（status=waiting；qr_token 映射入表 UNIQUE 反查）。
%% LEE-5：入口过 PG18 能力门禁（repo 层 create 内亦有兜底闸，任何调用方
%% 无法绕过）；失败返回 {error, Reason} 由调用方 fail closed 映射 503。
-spec pg_create_session(binary(), map()) -> {ok, binary()} | {error, term()}.
pg_create_session(SessionToken, SessionData) ->
    case qr_login_session_ds:ensure_claim_compatible() of
        ok ->
            QRToken = generate_qr_token_value(),
            case
                qr_login_session_ds:create(#{
                    session_token => SessionToken,
                    qr_token => QRToken,
                    device_id => maps:get(<<"device_id">>, SessionData),
                    device_name => maps:get(<<"device_name">>, SessionData),
                    platform => maps:get(<<"platform">>, SessionData),
                    purpose => maps:get(<<"purpose">>, SessionData, <<"human">>),
                    expires_at_ms => maps:get(<<"expires_at_ms">>, SessionData)
                })
            of
                ok -> {ok, QRToken};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 查询扫码状态
%% GET /api/v1/passport/qr_login/status?session_token=xxx
handle_status(Req, State) ->
    Qs = cowboy_req:parse_qs(Req),
    SessionToken = proplists:get_value(<<"session_token">>, Qs, <<>>),

    case SessionToken of
        <<>> ->
            elib_response:error(Req, <<"参数错误"/utf8>>, ?ERR_BAD_REQUEST);
        _ ->
            %% DEF-F6-QR-STATUS-403-FREEZE（合并恢复，task 分支原版语义）：
            %% web_human 会话门先于权威查询——web 端轮询冻结缺陷的前置防线。
            with_web_session(Req, State, SessionToken, fun() ->
                pg_handle_status(Req, SessionToken)
            end)
    end.

%% @doc PG 权威 status：confirmed 时原子 claim（UPDATE...RETURNING 行锁
%% 保证并发/SSE 双读竞争下恰好一个消费者拿到 token，G-QR oracle）。
%% LEE-5：查询/claim 执行故障与门禁失败均映射 503；正常零行仍返回 5203。
%% Storage failures never masquerade as a missing or already-consumed session.
-spec pg_handle_status(cowboy_req:req(), binary()) -> cowboy_req:req().
pg_handle_status(Req, SessionToken) ->
    case qr_login_session_ds:find_by_session_token(SessionToken) of
        undefined ->
            elib_response:error(Req, <<"会话不存在或已过期"/utf8>>, ?ERR_NOT_FOUND);
        {error, _} ->
            service_unavailable(Req);
        Session ->
            case maps:get(<<"status">>, Session, <<"waiting">>) of
                <<"confirmed">> ->
                    case qr_login_session_ds:claim_login_token(SessionToken) of
                        {ok, Claim} ->
                            confirmed_response(
                                Req,
                                maps:get(login_token, Claim, undefined),
                                maps:get(scanned_by, Claim, undefined)
                            );
                        {error, not_claimable} ->
                            %% 已消费/过期：既有兼容非成功响应（C08 oracle）
                            elib_response:error(
                                Req, <<"二维码已确认，请刷新"/utf8>>, ?ERR_QR_LOGIN_ALREADY_USED
                            );
                        {error, {pg_error, _}} ->
                            service_unavailable(Req);
                        {error, {claim_gate, Reason}} ->
                            ?ERROR_LOG(
                                "[lee-5] qr_login status claim gate rejected: ~p", [Reason]
                            ),
                            elib_response:error(
                                Req,
                                <<"服务暂不可用，请稍后再试"/utf8>>,
                                ?ERR_SERVICE_UNAVAILABLE
                            )
                    end;
                Status ->
                    elib_response:success(Req, #{<<"status">> => Status})
            end
    end.

%% @doc 与 SSE 使用同一 UID/token 合同，不发送缺失或非法身份的凭据。
%% Share the confirmed payload with SSE; never publish an unowned token.
-spec confirmed_response(cowboy_req:req(), term(), term()) -> cowboy_req:req().
confirmed_response(Req, Token, Uid) ->
    case qr_login_event_ds:confirmed(Token, Uid) of
        {ok, Payload} ->
            elib_response:success(Req, Payload);
        {error, invalid_credentials} ->
            elib_response:error(
                Req, <<"二维码已确认，请刷新"/utf8>>, ?ERR_QR_LOGIN_ALREADY_USED
            )
    end.

%% @doc 数据库故障不冒充业务状态；不回传 SQL/凭据，也不重试未知结果的写入。
%% Report availability only; never expose storage details or replay an uncertain write.
-spec service_unavailable(cowboy_req:req()) -> cowboy_req:req().
service_unavailable(Req) ->
    elib_response:error(
        Req, <<"服务暂不可用，请稍后再试"/utf8>>, ?ERR_SERVICE_UNAVAILABLE
    ).

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
            case parse_qr_token(QRToken) of
                {error, invalid_format} ->
                    elib_response:error(Req, <<"无效的二维码"/utf8>>, ?ERR_INVALID_QR_TOKEN);
                {error, {pg_error, _}} ->
                    service_unavailable(Req);
                {ok, SessionToken} ->
                    pg_handle_scan(Req, SessionToken, Uid)
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
        {error, {pg_error, _}} ->
            service_unavailable(Req);
        {error, not_transitioned} ->
            pg_scan_error(Req, SessionToken, Uid)
    end.

-spec pg_scan_error(cowboy_req:req(), binary(), integer()) -> cowboy_req:req().
pg_scan_error(Req, SessionToken, Uid) ->
    case qr_login_session_ds:find_by_session_token(SessionToken) of
        {error, _} ->
            service_unavailable(Req);
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
            case parse_qr_token(QRToken) of
                {error, invalid_format} ->
                    elib_response:error(Req, <<"无效的二维码"/utf8>>, ?ERR_INVALID_QR_TOKEN);
                {error, {pg_error, _}} ->
                    service_unavailable(Req);
                {ok, SessionToken} ->
                    pg_handle_confirm(Req, SessionToken, Uid)
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
            service_unavailable(Req);
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
            %% E2EE-013：先写设备行再签发 token（批次约束保持）——写入失败
            %% fail-closed（与 cache/web confirm 路径 log_device_login case 同契约）；
            %% 原实现丢弃返回值继续签发，设备行未落库即发 token=孤儿 token。
            case log_device_login(Uid, DeviceId, DeviceName, Platform) of
                {error, _} ->
                    device_registration_error(Req);
                {ok, DeviceRowId} ->
                    confirm_and_reply_pg(
                        Req, SessionToken, Uid, DeviceId, Purpose, Platform, DeviceRowId
                    );
                ok ->
                    confirm_and_reply_pg(
                        Req, SessionToken, Uid, DeviceId, Purpose, Platform, undefined
                    )
            end;
        {error, _} ->
            service_unavailable(Req);
        _ ->
            elib_response:error(Req, <<"二维码已过期，请刷新"/utf8>>, ?ERR_QR_LOGIN_EXPIRED)
    end.

%% @doc 签发+确认流转+通知（设备行写成功后的收尾段，自 pg_confirm_as_scanner 拆出）。
%% LEE-5：签发必须走 generate_login_token/5（web 平台 epoch 门）——原 /3 调用
%% 对 web 平台完全绕过 auth_session epoch 校验，坏 epoch 也会签出 token。
confirm_and_reply_pg(Req, SessionToken, Uid, DeviceId, Purpose, Platform, DeviceRowId) ->
    case generate_login_token(Uid, DeviceId, Purpose, Platform, DeviceRowId) of
        {ok, LoginToken} ->
            confirm_transition_pg(Req, SessionToken, Uid, LoginToken);
        {error, _} ->
            %% epoch 门拒发等签发失败：fail-closed 带 503 状态（session_unavailable，
            %% 非 200+code 的 service_unavailable——坏 epoch 必须让客户端可见失败）。
            session_unavailable(Req)
    end.

confirm_transition_pg(Req, SessionToken, Uid, LoginToken) ->
    case qr_login_session_ds:transition_confirm(SessionToken, Uid, LoginToken) of
        {ok, _} ->
            %% C08：syn 只发事件通知，不广播 login_token；
            %% Web 端落地 token 经 SSE 服务端代领或 status 原子领取
            _ = qr_login_event_ds:notify(
                SessionToken,
                qr_login_event_ds:event(confirmed_ready, undefined)
            ),
            elib_response:success(Req, #{<<"status">> => <<"confirmed">>});
        {error, {pg_error, _}} ->
            service_unavailable(Req);
        {error, not_transitioned} ->
            %% 并发 confirm/会话过期：既有兼容非成功响应
            elib_response:error(Req, <<"二维码已确认，请刷新"/utf8>>, ?ERR_QR_LOGIN_ALREADY_USED)
    end.

%% @doc 取消登录
%% POST /api/v1/passport/qr_login/cancel
%% Body: {"session_token": "xxx"}
handle_cancel(Req, State) ->
    {ok, Body, _} = cowboy_req:read_body(Req),
    Data = jsone:decode(Body),

    SessionToken = maps:get(<<"session_token">>, Data, <<>>),

    case SessionToken of
        <<>> ->
            elib_response:error(Req, <<"参数错误"/utf8>>, ?ERR_BAD_REQUEST);
        _ ->
            %% DEF-F6 同族（task 分支原版语义）：web_human 会话门先于取消。
            with_web_session(Req, State, SessionToken, fun() ->
                pg_handle_cancel(Req, SessionToken)
            end)
    end.

%% @doc PG cancel：waiting|scanned→cancelled CAS。
%% confirmed 后不可取消（G-QR：并发 confirm/cancel 结果唯一——已确认即生效），
%% 返回既有兼容 ALREADY_USED 错误码（cache 路径原实现会覆盖 confirmed 状态，
%% 属埋雷语义；PG 权威修正，边缘差异记录于收口文档）。
-spec pg_handle_cancel(cowboy_req:req(), binary()) -> cowboy_req:req().
pg_handle_cancel(Req, SessionToken) ->
    case qr_login_session_ds:transition_cancel(SessionToken) of
        ok ->
            %% 广播 cancelled 事件给该会话的 SSE 订阅者（Web 端立即感知，
            %% 无需等下一轮轮询）；notify/2 内部已 try/catch 兜底，不影响主流程。
            _ = qr_login_event_ds:notify(
                SessionToken, qr_login_event_ds:event(cancelled, undefined)
            ),
            elib_response:success(Req, #{<<"status">> => <<"cancelled">>});
        {error, {pg_error, _}} ->
            service_unavailable(Req);
        {error, not_transitioned} ->
            case qr_login_session_ds:find_by_session_token(SessionToken) of
                undefined ->
                    %% 不存在/过期：幂等成功（与 cache 路径对齐）
                    elib_response:success(Req, #{<<"status">> => <<"cancelled">>});
                {error, _} ->
                    service_unavailable(Req);
                _ ->
                    elib_response:error(
                        Req, <<"二维码已确认，请刷新"/utf8>>, ?ERR_QR_LOGIN_ALREADY_USED
                    )
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

%% @doc 生成 QR Token 值（随机值入 PG 表 qr_token UNIQUE 反查映射）。
%%
%% 二维码图像会展示在屏幕上（截屏、投屏、肩窥都能拿到），因此其内容
%% **不得可反解出 session_token**（旧实现 base64(session_token++ts) 已废弃）。
%% 反查映射由 PG qr_login_session.qr_token UNIQUE 列承担（LEE-5：cache
%% 映射 {qr_login_qr,_} 已随 cache 权威一并删除）。
-spec generate_qr_token_value() -> binary().
generate_qr_token_value() ->
    base64:encode(crypto:strong_rand_bytes(32)).

%% @doc 生成登录 Token（E2EE-013：绑定被登录设备的 DID）
-spec generate_login_token(integer(), binary(), binary(), binary(), term()) ->
    {ok, binary()} | {error, term()}.
generate_login_token(Uid, Did, Purpose, <<"web">>, DeviceRowId) when
    (Purpose =:= <<"human">> orelse Purpose =:= <<"seat">>),
    is_integer(DeviceRowId),
    DeviceRowId > 0
->
    try
        case auth_session_ds:current_epoch(Uid) of
            {ok, Ep} when is_integer(Ep), Ep >= 1 ->
                Token =
                    case Purpose of
                        <<"human">> -> token_ds:encrypt_web_token(Uid, Did, Ep, DeviceRowId);
                        <<"seat">> -> token_ds:encrypt_web_seat_token(Uid, Did, Ep, DeviceRowId)
                    end,
                {ok, Token};
            _ ->
                {error, unavailable}
        end
    catch
        _:_ -> {error, unavailable}
    end;
generate_login_token(_, _, _, <<"web">>, _) ->
    {error, invalid_device};
generate_login_token(Uid, Did, <<"human">>, _, _) ->
    {ok, token_ds:encrypt_token(Uid, Did)};
generate_login_token(Uid, Did, <<"seat">>, _, _) ->
    {ok, token_ds:encrypt_seat_token(Uid, Did)}.

%% @doc 记录设备登录
-spec log_device_login(integer(), binary(), binary(), binary()) ->
    ok | {ok, pos_integer()} | {error, term()}.
log_device_login(Uid, DeviceId, DeviceName, Platform) ->
    qr_login_logic:log_device_login(Uid, DeviceId, DeviceName, Platform).

with_web_session(Req, State, SessionToken, Next) ->
    case maps:get(jwt_purpose, State, human) of
        web_human ->
            case qr_login_logic:web_session(SessionToken) of
                {ok, _} ->
                    Next();
                {error, unavailable} ->
                    elib_response:error_with_status(Req, 503, <<"QR session unavailable">>, 503);
                %% Web 轮询缺失/过期会话返回既有 5203，不泛化为 403。
                %% 已消费的 confirmed 行由 PG claim 返回 not_claimable，
                %% 同样映射 5203；不会再次交付登录凭据。
                {error, not_found} ->
                    elib_response:error(
                        Req, <<"二维码已确认，请刷新"/utf8>>, ?ERR_QR_LOGIN_ALREADY_USED
                    );
                %% 403 仅保留给 purpose/platform 不匹配的真 forbidden。
                {error, _} ->
                    elib_response:error(Req, <<"Forbidden QR session">>, ?ERR_FORBIDDEN)
            end;
        _ ->
            Next()
    end.

device_registration_error(Req) ->
    elib_response:error_with_status(Req, 503, <<"设备登记失败，请重试"/utf8>>, 503).

valid_utf8_length(Value, Max) when is_binary(Value), byte_size(Value) =< Max * 4 ->
    case unicode:characters_to_list(Value) of
        Chars when is_list(Chars) -> length(Chars) =< Max;
        _ -> false
    end;
valid_utf8_length(_, _) ->
    false.

session_unavailable(Req) ->
    elib_response:error_with_status(Req, 503, <<"会话服务暂不可用，请重试"/utf8>>, 503).
