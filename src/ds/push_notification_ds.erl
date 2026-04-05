-module(push_notification_ds).
%%%
% push_notification_ds 是 push notification domain service 缩写
% 推送通知数据服务层，封装 FCM / APNs 推送发送逻辑
%%%

-include("log.hrl").

-export([send_to_user/3]).
-export([send_to_users/3]).
-export([send_fcm/3]).
-export([send_apns/3]).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 向单个用户的所有设备发送推送通知
%% @param Uid 用户 ID
%% @param Title 推送标题
%% @param Body 推送内容
-spec send_to_user(integer(), binary(), binary()) -> ok.
send_to_user(Uid, Title, Body) ->
    case push_token_repo:list_by_uid(Uid) of
        {ok, _, Rows} when is_list(Rows), length(Rows) > 0 ->
            lists:foreach(fun(Row) ->
                do_send_push(Row, Title, Body)
            end, Rows),
            ok;
        _ ->
            ?DEBUG_LOG(["push_notification_ds: no push token for uid", Uid]),
            ok
    end.

%% @doc 向多个用户发送推送通知
-spec send_to_users([integer()], binary(), binary()) -> ok.
send_to_users([], _Title, _Body) ->
    ok;
send_to_users(Uids, Title, Body) ->
    case push_token_repo:list_by_uids(Uids) of
        {ok, _, Rows} when is_list(Rows), length(Rows) > 0 ->
            lists:foreach(fun(Row) ->
                do_send_push(Row, Title, Body)
            end, Rows),
            ok;
        _ ->
            ok
    end.

%% @doc 发送 FCM 推送（Google Firebase Cloud Messaging HTTP v1）
%% 需要在 sys.config 配置 {push, [{fcm_project_id, "..."}, {fcm_service_account_key, "..."}]}
-spec send_fcm(binary(), binary(), binary()) -> ok | {error, term()}.
send_fcm(Token, Title, Body) ->
    case get_fcm_config() of
        {ok, ProjectId, AccessToken} ->
            Url = <<"https://fcm.googleapis.com/v1/projects/",
                    ProjectId/binary, "/messages:send">>,
            Payload = jsone:encode(#{
                <<"message">> => #{
                    <<"token">> => Token,
                    <<"notification">> => #{
                        <<"title">> => Title,
                        <<"body">> => Body
                    },
                    <<"android">> => #{
                        <<"priority">> => <<"high">>
                    }
                }
            }, [native_utf8]),
            Headers = [
                {<<"authorization">>, <<"Bearer ", AccessToken/binary>>},
                {<<"content-type">>, <<"application/json">>}
            ],
            case http_post(Url, Headers, Payload) of
                {ok, 200, _RespBody} ->
                    ok;
                {ok, StatusCode, RespBody} ->
                    ?ERROR_LOG(["FCM push failed", StatusCode, RespBody]),
                    maybe_deactivate_token(StatusCode, Token),
                    {error, {fcm_error, StatusCode}};
                {error, Reason} ->
                    ?ERROR_LOG(["FCM push request failed", Reason]),
                    {error, Reason}
            end;
        {error, not_configured} ->
            ?DEBUG_LOG(["FCM not configured, skip push"]),
            {error, not_configured}
    end.

%% @doc 发送 APNs 推送（Apple Push Notification Service HTTP/2）
%% 需要在 sys.config 配置:
%%   {push, [{apns_key_id, "..."}, {apns_team_id, "..."}, {apns_bundle_id, "..."},
%%           {apns_auth_key_path, "/path/to/AuthKey_XXXX.p8"},
%%           {apns_env, production | development}]}
-spec send_apns(binary(), binary(), binary()) -> ok | {error, term()}.
send_apns(Token, Title, Body) ->
    case get_apns_config() of
        {ok, KeyId, TeamId, BundleId} ->
            Payload = jsone:encode(#{
                <<"aps">> => #{
                    <<"alert">> => #{
                        <<"title">> => Title,
                        <<"body">> => Body
                    },
                    <<"sound">> => <<"default">>,
                    <<"badge">> => 1,
                    <<"mutable-content">> => 1
                }
            }, [native_utf8]),
            Host = get_apns_host(),
            Path = <<"/3/device/", Token/binary>>,
            Headers = [
                {<<"authorization">>, <<"bearer ", (get_apns_jwt(KeyId, TeamId))/binary>>},
                {<<"apns-topic">>, BundleId},
                {<<"apns-push-type">>, <<"alert">>},
                {<<"apns-priority">>, <<"10">>}
            ],
            case http_post(<<"https://", Host/binary, Path/binary>>, Headers, Payload) of
                {ok, 200, _RespBody} ->
                    ok;
                {ok, StatusCode, RespBody} ->
                    ?ERROR_LOG(["APNs push failed", StatusCode, RespBody]),
                    maybe_deactivate_token(StatusCode, Token),
                    {error, {apns_error, StatusCode}};
                {error, Reason} ->
                    ?ERROR_LOG(["APNs push request failed", Reason]),
                    {error, Reason}
            end;
        {error, not_configured} ->
            ?DEBUG_LOG(["APNs not configured, skip push"]),
            {error, not_configured}
    end.

%% ===================================================================
%% Internal Functions
%% ===================================================================

%% @doc 根据平台选择推送方式（带重试）
do_send_push(Row, Title, Body) ->
    {Platform, Token} = extract_push_info(Row),
    elib_async:async_retry(fun() ->
        case Platform of
            <<"fcm">> ->
                case send_fcm(Token, Title, Body) of
                    ok -> ok;
                    {error, not_configured} -> ok; % 不重试配置缺失
                    {error, {fcm_error, 404}} -> ok; % token 不存在，不重试
                    {error, {fcm_error, 410}} -> ok; % token 已注销，不重试
                    {error, Reason} -> error({push_failed, Reason})
                end;
            <<"apns">> ->
                case send_apns(Token, Title, Body) of
                    ok -> ok;
                    {error, not_configured} -> ok;
                    {error, {apns_error, 410}} -> ok; % token 无效，不重试
                    {error, Reason} -> error({push_failed, Reason})
                end;
            _ ->
                ?DEBUG_LOG(["Unknown push platform", Platform]),
                ok
        end
    end, 2, 3000). % 最多重试 2 次，间隔 3 秒

%% @doc 从查询结果行提取推送信息
%% list_by_uid 返回: {device_id, device_type, platform, token}
%% list_by_uids 返回: {user_id, device_id, device_type, platform, token}
extract_push_info({_DeviceId, _DeviceType, Platform, Token}) ->
    {Platform, Token};
extract_push_info({_UserId, _DeviceId, _DeviceType, Platform, Token}) ->
    {Platform, Token};
extract_push_info(Row) when is_map(Row) ->
    {maps:get(<<"platform">>, Row, <<>>), maps:get(<<"token">>, Row, <<>>)}.

%% @doc 获取 FCM 配置
get_fcm_config() ->
    case application:get_env(imboy, push) of
        {ok, PushConfig} ->
            ProjectId = proplists:get_value(fcm_project_id, PushConfig),
            AccessToken = proplists:get_value(fcm_access_token, PushConfig),
            case ProjectId =/= undefined andalso AccessToken =/= undefined of
                true ->
                    {ok, elib_cnv:to_binary(ProjectId), elib_cnv:to_binary(AccessToken)};
                false ->
                    {error, not_configured}
            end;
        undefined ->
            {error, not_configured}
    end.

%% @doc 获取 APNs 配置
get_apns_config() ->
    case application:get_env(imboy, push) of
        {ok, PushConfig} ->
            KeyId = proplists:get_value(apns_key_id, PushConfig),
            TeamId = proplists:get_value(apns_team_id, PushConfig),
            BundleId = proplists:get_value(apns_bundle_id, PushConfig),
            case KeyId =/= undefined andalso TeamId =/= undefined andalso BundleId =/= undefined of
                true ->
                    {ok, elib_cnv:to_binary(KeyId),
                         elib_cnv:to_binary(TeamId),
                         elib_cnv:to_binary(BundleId)};
                false ->
                    {error, not_configured}
            end;
        undefined ->
            {error, not_configured}
    end.

%% @doc HTTP POST 请求（使用 gun）
http_post(Url, Headers, Body) ->
    try
        %% 解析 URL
        #{host := Host, path := Path} = uri_string:parse(Url),
        Port = 443,
        {ok, ConnPid} = gun:open(binary_to_list(Host), Port, #{
            transport => tls,
            protocols => [http2],
            tls_opts => [{verify, verify_peer},
                         {customize_hostname_check,
                          [{match_fun, public_key:pkix_verify_hostname_match_fun(https)}]}]
        }),
        {ok, http2} = gun:await_up(ConnPid, 5000),
        StreamRef = gun:post(ConnPid, Path, Headers, Body),
        case gun:await(ConnPid, StreamRef, 10000) of
            {response, fin, Status, _RespHeaders} ->
                gun:close(ConnPid),
                {ok, Status, <<>>};
            {response, nofin, Status, _RespHeaders} ->
                {ok, RespBody} = gun:await_body(ConnPid, StreamRef, 5000),
                gun:close(ConnPid),
                {ok, Status, RespBody};
            {error, Reason} ->
                gun:close(ConnPid),
                {error, Reason}
        end
    catch
        _:Error ->
            {error, Error}
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
get_apns_jwt(KeyId, TeamId) ->
    CacheKey = {apns_jwt, KeyId},
    case imboy_cache:get(CacheKey) of
        {ok, Jwt} ->
            Jwt;
        _ ->
            Jwt = generate_apns_jwt(KeyId, TeamId),
            imboy_cache:set(CacheKey, Jwt, 3000), % 缓存 50 分钟
            Jwt
    end.

%% @doc 生成 APNs ES256 JWT
generate_apns_jwt(KeyId, TeamId) ->
    Now = erlang:system_time(second),
    Header = base64url_encode(jsone:encode(#{
        <<"alg">> => <<"ES256">>,
        <<"kid">> => KeyId
    })),
    Claims = base64url_encode(jsone:encode(#{
        <<"iss">> => TeamId,
        <<"iat">> => Now
    })),
    SignInput = <<Header/binary, ".", Claims/binary>>,
    case get_apns_private_key() of
        {ok, PrivateKey} ->
            Signature = base64url_encode(
                public_key:sign(SignInput, sha256, PrivateKey)
            ),
            <<SignInput/binary, ".", Signature/binary>>;
        {error, _} ->
            <<>>
    end.

%% @doc 读取 APNs P8 私钥文件
get_apns_private_key() ->
    case application:get_env(imboy, push) of
        {ok, PushConfig} ->
            case proplists:get_value(apns_auth_key_path, PushConfig) of
                undefined ->
                    {error, no_key_path};
                KeyPath ->
                    case file:read_file(elib_cnv:to_list(KeyPath)) of
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
    << <<(case C of $+ -> $-; $/ -> $_; $= -> <<>>; _ -> C end)>>
       || <<C>> <= B64, C =/= $= >>.

%% @doc 根据 HTTP 状态码决定是否使 token 失效
%% FCM 404 = token 不存在，410 = token 已注销
maybe_deactivate_token(404, Token) ->
    push_token_repo:deactivate_by_token(Token);
maybe_deactivate_token(410, Token) ->
    push_token_repo:deactivate_by_token(Token);
maybe_deactivate_token(_, _Token) ->
    ok.
