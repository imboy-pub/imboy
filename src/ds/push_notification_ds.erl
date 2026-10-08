-module(push_notification_ds).
-compile([nowarn_deprecated_catch]).
%%%
% push_notification_ds 是 push notification domain service 缩写
% 推送通知数据服务层，封装 FCM / APNs 推送发送逻辑
%%%

-include("log.hrl").

-export([send_to_user/3]).
-export([send_to_user_with_data/4]).
-export([send_to_users/3, send_to_group_users/4]).
-export([send_fcm/3]).
-export([send_fcm/4]).
-export([send_apns/3]).
-export([send_apns/4]).
-export([cleanup_inactive_tokens/1]).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 向单个用户的所有设备发送推送通知
%% @param Uid 用户 ID
%% @param Title 推送标题
%% @param Body 推送内容
-spec send_to_user(integer(), binary(), binary()) -> ok.
send_to_user(Uid, Title, Body) ->
    send_to_user_with_data(Uid, Title, Body, #{}).

%% @doc 向单个用户的所有设备发送推送通知，携带固定常量路由数据。
%% Data 仅允许固定常量键值（如 notify_type），禁止动态内容（隐私红线
%% 与 title/body 常量同口径）；空 map = 与 send_to_user/3 完全同形状。
-spec send_to_user_with_data(integer(), binary(), binary(), #{binary() => binary()}) -> ok.
send_to_user_with_data(Uid, Title, Body, Data) when is_map(Data) ->
    case push_token_repo:list_by_uid(Uid) of
        {ok, Rows} when is_list(Rows), length(Rows) > 0 ->
            lists:foreach(
                fun(Row) ->
                    do_send_push(Row, Title, Body, Data)
                end,
                Rows
            ),
            ok;
        {ok, _} ->
            ?DEBUG_LOG(["push_notification_ds: no push token for uid", Uid]),
            ok;
        {error, Reason} ->
            ?ERROR_LOG(["push_notification_ds: list_by_uid failed", Uid, Reason]),
            ok
    end.

%% @doc 向多个用户发送推送通知
-spec send_to_users([integer()], binary(), binary()) -> ok.
send_to_users([], _Title, _Body) ->
    ok;
send_to_users(Uids, Title, Body) ->
    case push_token_repo:list_by_uids(Uids) of
        {ok, Rows} when is_list(Rows), length(Rows) > 0 ->
            lists:foreach(
                fun(Row) ->
                    do_send_push(Row, Title, Body)
                end,
                Rows
            ),
            ok;
        {ok, _} ->
            ok;
        {error, Reason} ->
            ?ERROR_LOG(["push_notification_ds: list_by_uids failed", Uids, Reason]),
            ok
    end.

%% Discussion identity stays in the server-side retry closure, never in provider
%% payload data. Each fired retry rechecks membership and wall-clock entitlement.
send_to_group_users(Gid, Uids, Title, Body) ->
    case push_token_repo:list_by_uids(Uids) of
        {ok, Rows} when is_list(Rows) ->
            lists:foreach(
                fun(Row) ->
                    Uid = maps:get(<<"user_id">>, Row, 0),
                    Guard = fun() ->
                        try
                            micro_community_access_repo:delivery_allowed(Gid, Uid)
                        catch
                            _:_ -> false
                        end
                    end,
                    do_send_push(Row, Title, Body, #{}, Guard)
                end,
                Rows
            ),
            ok;
        _ ->
            ok
    end.

%% @doc 发送 FCM 推送（Google Firebase Cloud Messaging HTTP v1）
%% 需要在 sys.config 配置 {push, [{fcm_project_id, "..."}, {fcm_service_account_key, "..."}]}
-spec send_fcm(binary(), binary(), binary()) -> ok | {error, term()}.
send_fcm(Token, Title, Body) ->
    send_fcm(Token, Title, Body, #{}).

%% @doc FCM 推送 + 固定常量路由数据（message.data；键值必须二进制）。
-spec send_fcm(binary(), binary(), binary(), #{binary() => binary()}) ->
    ok | {error, term()}.
%% 不加 is_map(Data) 守卫：对已声明 #{binary() => binary()} 的参数做冗余
%% 守卫细化触发 Gradualizer pick_value(none()) 崩溃（配方 b'，2026-10-04）。
send_fcm(Token, Title, Body, Data) ->
    case get_fcm_config() of
        {ok, ProjectId, AccessToken} ->
            Url = <<"https://fcm.googleapis.com/v1/projects/", ProjectId/binary, "/messages:send">>,
            NotificationMsg =
                #{
                    <<"token">> => Token,
                    <<"notification">> => #{
                        <<"title">> => Title,
                        <<"body">> => Body
                    },
                    <<"android">> => #{
                        <<"priority">> => <<"high">>
                    }
                },
            %% map_size 判空（case 的 `#{}` 模式匹配任意 map）：空 Data 时
            %% 请求体与历史版本逐字节一致
            Message =
                case map_size(Data) of
                    0 -> NotificationMsg;
                    _ -> NotificationMsg#{<<"data">> => Data}
                end,
            Payload = jsone:encode(#{<<"message">> => Message}, [native_utf8]),
            Headers = [
                {<<"authorization">>, <<"Bearer ", AccessToken/binary>>},
                {<<"content-type">>, <<"application/json">>}
            ],
            case http_post(Url, Headers, Payload) of
                {ok, 200, _RespBody} ->
                    ok;
                {ok, StatusCode, RespBody} ->
                    ?ERROR_LOG(["FCM push failed", StatusCode, RespBody]),
                    _ = maybe_deactivate_token(StatusCode, Token),
                    {error, {fcm_error, StatusCode}};
                {error, Reason} ->
                    ?ERROR_LOG(["FCM push request failed", Reason]),
                    {error, Reason}
            end;
        {error, not_configured} ->
            ?DEBUG_LOG(["FCM not configured, skip push"]),
            {error, not_configured};
        {error, Reason} ->
            %% G4：刷新链路失败（配置类/token endpoint 拒绝）——错误原样上抛，
            %% 调用方按失败分类决定是否重试
            ?ERROR_LOG(["FCM access token unavailable", Reason]),
            {error, Reason}
    end.

%% @doc 发送 APNs 推送（Apple Push Notification Service HTTP/2）
%% 需要在 sys.config 配置:
%%   {push, [{apns_key_id, "..."}, {apns_team_id, "..."}, {apns_bundle_id, "..."},
%%           {apns_auth_key_path, "/path/to/AuthKey_XXXX.p8"},
%%           {apns_env, production | development}]}
-spec send_apns(binary(), binary(), binary()) -> ok | {error, term()}.
send_apns(Token, Title, Body) ->
    send_apns(Token, Title, Body, #{}).

%% @doc APNs 推送 + 固定常量路由数据（aps 之外的根级自定义键）。
-spec send_apns(binary(), binary(), binary(), #{binary() => binary()}) ->
    ok | {error, term()}.
%% 不加 is_map(Data) 守卫：同 send_fcm，冗余守卫细化触发
%% pick_value(none()) 崩溃（配方 b'）。
send_apns(Token, Title, Body, Data) ->
    case get_apns_config() of
        {ok, KeyId, TeamId, BundleId} ->
            Aps =
                #{
                    <<"aps">> => #{
                        <<"alert">> => #{
                            <<"title">> => Title,
                            <<"body">> => Body
                        },
                        <<"sound">> => <<"default">>,
                        <<"badge">> => 1,
                        <<"mutable-content">> => 1
                    }
                },
            %% map_size 判空（case 的 `#{}` 模式匹配任意 map）：空 Data 时
            %% 载荷与历史版本一致
            Payload0 =
                case map_size(Data) of
                    0 -> Aps;
                    _ -> maps:merge(Aps, Data)
                end,
            Payload = jsone:encode(Payload0, [native_utf8]),
            case get_apns_jwt(KeyId, TeamId) of
                {ok, Jwt} ->
                    Host = get_apns_host(),
                    Path = <<"/3/device/", Token/binary>>,
                    Headers = [
                        {<<"authorization">>, <<"bearer ", Jwt/binary>>},
                        {<<"apns-topic">>, BundleId},
                        {<<"apns-push-type">>, <<"alert">>},
                        {<<"apns-priority">>, <<"10">>}
                    ],
                    case http_post(<<"https://", Host/binary, Path/binary>>, Headers, Payload) of
                        {ok, 200, _RespBody} ->
                            ok;
                        {ok, StatusCode, RespBody} ->
                            ?ERROR_LOG(["APNs push failed", StatusCode, RespBody]),
                            _ = maybe_deactivate_token(StatusCode, Token),
                            {error, {apns_error, StatusCode}};
                        {error, Reason} ->
                            ?ERROR_LOG(["APNs push request failed", Reason]),
                            {error, Reason}
                    end;
                {error, JwtErr} ->
                    ?ERROR_LOG(["APNs JWT generation failed", JwtErr]),
                    {error, {jwt_error, JwtErr}}
            end;
        {error, not_configured} ->
            ?DEBUG_LOG(["APNs not configured, skip push"]),
            {error, not_configured}
    end.

%% @doc 清理超过指定天数未更新的不活跃推送 Token
%% 建议通过定时任务（如每天一次）调用此函数，InactiveDays 建议设为 30
%% 示例: push_notification_ds:cleanup_inactive_tokens(30).
-spec cleanup_inactive_tokens(pos_integer()) -> {ok, integer()} | {error, term()}.
cleanup_inactive_tokens(InactiveDays) when InactiveDays > 0 ->
    case push_token_repo:deactivate_inactive(InactiveDays) of
        {ok, Count} ->
            ?DEBUG_LOG([
                "push_notification_ds: cleaned up inactive tokens",
                InactiveDays,
                <<"days">>,
                Count,
                <<"deactivated">>
            ]),
            {ok, Count};
        {error, Reason} = Err ->
            ?ERROR_LOG([
                "push_notification_ds: cleanup_inactive_tokens failed",
                Reason
            ]),
            Err
    end.

%% ===================================================================
%% Internal Functions
%% ===================================================================

%% @doc 根据平台选择推送方式（带重试）
do_send_push(Row, Title, Body) ->
    do_send_push(Row, Title, Body, #{}).

do_send_push(Row, Title, Body, Data) when is_map(Data) ->
    do_send_push(Row, Title, Body, Data, fun() -> true end).

do_send_push(Row, Title, Body, Data, Guard) ->
    {Platform, Token} = extract_push_info(Row),
    elib_async:async_retry(
        fun() ->
            case Guard() of
                true -> send_platform_push(Platform, Token, Title, Body, Data);
                false -> ok
            end
        end,
        2,
        3000
    ).

send_platform_push(Platform, Token, Title, Body, Data) ->
    case Platform of
        <<"fcm">> ->
            case send_fcm(Token, Title, Body, Data) of
                ok -> ok;
                % 不重试配置缺失
                {error, not_configured} -> ok;
                % token 不存在，不重试
                {error, {fcm_error, 404}} -> ok;
                % token 已注销，不重试
                {error, {fcm_error, 410}} -> ok;
                % G4：service account 配置类错误，不重试
                {error, {config_error, _}} -> ok;
                % G4：token endpoint 明确拒绝，不重试（下轮缓存过期再试）
                {error, {fcm_token_error, _}} -> ok;
                {error, Reason} -> error({push_failed, Reason})
            end;
        <<"apns">> ->
            case send_apns(Token, Title, Body, Data) of
                ok -> ok;
                {error, not_configured} -> ok;
                % token 无效，不重试
                {error, {apns_error, 410}} -> ok;
                {error, Reason} -> error({push_failed, Reason})
            end;
        <<"jpush">> ->
            send_jpush(Token, Title, Body, Data);
        _ ->
            ?DEBUG_LOG(["Unknown push platform", Platform]),
            ok
    end.

%% @doc JPush 分派（EPGZ-07）：错误分类见 push_provider_jpush:send/4
send_jpush(Token, Title, Body, Data) ->
    case push_provider_jpush:send(Token, Title, Body, Data) of
        ok ->
            ok;
        % 未配置凭证：fail-closed 跳过，不重试
        {error, not_configured} ->
            ok;
        % registration_id 失效：下线 token，不重试（对齐 FCM 404/410 语义）
        {error, {jpush_error, invalid_token}} ->
            _ = push_token_repo:deactivate_by_token(Token),
            ok;
        % 凭证/鉴权配置错误：不重试、不下线 token
        {error, {jpush_error, unauthorized}} ->
            ok;
        % rate_limited / 其余错误：经 async_retry 重试
        {error, Reason} ->
            error({push_failed, Reason})
    end.

%% @doc 从查询结果行提取推送信息
%% elib_pg:query 返回 [map()]，每个 map 包含 <<"platform">> 和 <<"token">> 键
extract_push_info(Row) when is_map(Row) ->
    {maps:get(<<"platform">>, Row, <<>>), maps:get(<<"token">>, Row, <<>>)}.

%% @doc 获取 FCM 配置（G4：access token 来源优先级）
%%   1. service account（fcm_service_account_json/_path）→ 动态刷新
%%      （push_provider_fcm_token，OAuth2 JWT-bearer，缓存 50min）；
%%   2. 静态 {fcm_access_token, ...}（向后兼容，短期/测试用途）；
%%   3. 均无 → not_configured。
%% 刷新失败（配置类/endpoint 拒绝）原样返回 {error, Reason}，由 send_fcm
%% 上抛、do_send_push 按分类决定不重试。
get_fcm_config() ->
    case application:get_env(imboy, push) of
        {ok, PushConfig} ->
            case proplists:get_value(fcm_project_id, PushConfig) of
                undefined ->
                    {error, not_configured};
                ProjectId ->
                    TokenResult =
                        case service_account_configured(PushConfig) of
                            true ->
                                push_provider_fcm_token:get_access_token();
                            false ->
                                case proplists:get_value(fcm_access_token, PushConfig) of
                                    undefined -> {error, not_configured};
                                    Static -> {ok, elib_cnv:safe_to_binary(Static)}
                                end
                        end,
                    case TokenResult of
                        {ok, AccessToken} ->
                            {ok, elib_cnv:safe_to_binary(ProjectId), AccessToken};
                        {error, Reason} ->
                            {error, Reason}
                    end
            end;
        undefined ->
            {error, not_configured}
    end.

service_account_configured(PushConfig) ->
    proplists:get_value(fcm_service_account_json, PushConfig) =/= undefined orelse
        proplists:get_value(fcm_service_account_path, PushConfig) =/= undefined.

%% @doc 获取 APNs 配置
get_apns_config() ->
    case application:get_env(imboy, push) of
        {ok, PushConfig} ->
            KeyId = proplists:get_value(apns_key_id, PushConfig),
            TeamId = proplists:get_value(apns_team_id, PushConfig),
            BundleId = proplists:get_value(apns_bundle_id, PushConfig),
            case KeyId =/= undefined andalso TeamId =/= undefined andalso BundleId =/= undefined of
                true ->
                    {ok, elib_cnv:safe_to_binary(KeyId), elib_cnv:safe_to_binary(TeamId),
                        elib_cnv:safe_to_binary(BundleId)};
                false ->
                    {error, not_configured}
            end;
        undefined ->
            {error, not_configured}
    end.

%% @doc HTTP POST 请求（使用 gun，连接按 Host 复用）
http_post(Url, Headers, Body) ->
    try
        %% 解析 URL；path 归一为 binary：uri_string:parse 的 path 是
        %% charlist() | unicode_binary() 联合，gun 要求 iodata()。
        %% characters_to_binary 的 OTP29 spec 带 error/incomplete 臂，
        %% 全臂 case 收敛为纯 binary（实际 URL 为 ASCII，首臂恒成立）。
        #{host := Host, path := Path0} = uri_string:parse(Url),
        Path =
            case unicode:characters_to_binary(Path0) of
                B when is_binary(B) -> B;
                {incomplete, B, _} -> B;
                _ -> <<>>
            end,
        Port = 443,
        ConnPid = get_or_open_conn(Host, Port),
        HttpTimeout = config_ds:env(push_http_timeout, 15000),
        BodyTimeout = config_ds:env(push_body_timeout, 8000),
        StreamRef = gun:post(ConnPid, Path, Headers, Body),
        case gun:await(ConnPid, StreamRef, HttpTimeout) of
            {response, fin, Status, _RespHeaders} ->
                {ok, Status, <<>>};
            {response, nofin, Status, _RespHeaders} ->
                {ok, RespBody} = gun:await_body(ConnPid, StreamRef, BodyTimeout),
                {ok, Status, RespBody};
            {error, Reason} ->
                %% 连接可能失效，清除缓存让下次重建
                close_cached_conn(Host),
                {error, Reason}
        end
    catch
        _:Error ->
            {error, Error}
    end.

%% @doc 获取或建立到指定 Host 的 HTTP/2 连接（进程字典缓存）
%% NOTE: 进程字典缓存仅在当前进程生命周期内有效。
%% 由于 do_send_push 通过 elib_async:async_retry 在短生命周期进程中执行，
%% 连接会随进程结束而释放。如果未来改为长生命周期进程池，需迁移至 ETS 全局缓存。
get_or_open_conn(Host, Port) ->
    Key = {gun_conn, Host},
    case get(Key) of
        Pid when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true ->
                    Pid;
                false ->
                    erase(Key),
                    open_and_cache_conn(Key, Host, Port)
            end;
        _ ->
            open_and_cache_conn(Key, Host, Port)
    end.

open_and_cache_conn(Key, Host, Port) ->
    case
        gun:open(binary_to_list(Host), Port, #{
            transport => tls,
            protocols => [http2],
            tls_opts => [
                {verify, verify_peer},
                {customize_hostname_check, [
                    {match_fun, public_key:pkix_verify_hostname_match_fun(https)}
                ]}
            ]
        })
    of
        {ok, ConnPid} ->
            case gun:await_up(ConnPid, 5000) of
                {ok, http2} ->
                    put(Key, ConnPid),
                    ConnPid;
                {error, AwaitReason} ->
                    catch gun:close(ConnPid),
                    erase(Key),
                    error({gun_await_up_failed, AwaitReason})
            end;
        {error, OpenReason} ->
            error({gun_open_failed, OpenReason})
    end.

close_cached_conn(Host) ->
    Key = {gun_conn, Host},
    case erase(Key) of
        Pid when is_pid(Pid) ->
            catch gun:close(Pid),
            ok;
        _ ->
            ok
    end.

%% @doc 获取 APNs 服务器地址
get_apns_host() ->
    case application:get_env(imboy, push) of
        {ok, PushConfig} ->
            case proplists:get_value(apns_env, PushConfig, production) of
                development -> <<"api.sandbox.push.apple.com">>;
                _ -> <<"api.push.apple.com">>
            end;
        _ ->
            <<"api.push.apple.com">>
    end.

%% @doc 生成 APNs JWT (ES256)
%% 缓存 50 分钟（APNs JWT 有效期 60 分钟）
%% 返回 {ok, JWT} | {error, Reason}
get_apns_jwt(KeyId, TeamId) ->
    CacheKey = {apns_jwt, KeyId},
    case imboy_cache:get(CacheKey) of
        {ok, Jwt} ->
            {ok, Jwt};
        _ ->
            case generate_apns_jwt(KeyId, TeamId) of
                {ok, Jwt} ->
                    % 缓存 50 分钟
                    imboy_cache:set(CacheKey, Jwt, 3000),
                    {ok, Jwt};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @doc 生成 APNs ES256 JWT
%% 返回 {ok, JWT} | {error, Reason}，调用方需匹配错误不发请求
-spec generate_apns_jwt(binary(), binary()) -> {ok, binary()} | {error, term()}.
generate_apns_jwt(KeyId, TeamId) ->
    Now = erlang:system_time(second),
    Header = base64url_encode(
        jsone:encode(#{
            <<"alg">> => <<"ES256">>,
            <<"kid">> => KeyId
        })
    ),
    Claims = base64url_encode(
        jsone:encode(#{
            <<"iss">> => TeamId,
            <<"iat">> => Now
        })
    ),
    SignInput = <<Header/binary, ".", Claims/binary>>,
    case get_apns_private_key() of
        {ok, PrivateKey} ->
            Signature = base64url_encode(
                public_key:sign(SignInput, sha256, PrivateKey)
            ),
            {ok, <<SignInput/binary, ".", Signature/binary>>};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 读取 APNs P8 私钥文件
get_apns_private_key() ->
    case application:get_env(imboy, push) of
        {ok, PushConfig} ->
            case proplists:get_value(apns_auth_key_path, PushConfig) of
                undefined ->
                    {error, no_key_path};
                KeyPath ->
                    case file:read_file(elib_cnv:safe_to_binary(KeyPath)) of
                        {ok, PemBin} ->
                            [Entry] = public_key:pem_decode(PemBin),
                            Key = public_key:pem_entry_decode(Entry),
                            {ok, Key};
                        {error, Reason} ->
                            ?ERROR_LOG(["Failed to read APNs key", KeyPath, Reason]),
                            {error, Reason}
                    end
            end;
        _ ->
            {error, not_configured}
    end.

%% @doc base64url 编码（无填充）
base64url_encode(Data) ->
    B64 = base64:encode(Data),
    <<
        <<
            (case C of
                $+ -> $-;
                $/ -> $_;
                _ -> C
            end)
        >>
     || <<C>> <= B64, C =/= $=
    >>.

%% @doc 根据 HTTP 状态码决定是否使 token 失效
%% FCM 404 = token 不存在，410 = token 已注销
maybe_deactivate_token(404, Token) ->
    push_token_repo:deactivate_by_token(Token);
maybe_deactivate_token(410, Token) ->
    push_token_repo:deactivate_by_token(Token);
maybe_deactivate_token(_, _Token) ->
    ok.
