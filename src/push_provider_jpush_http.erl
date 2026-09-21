-module(push_provider_jpush_http).
%%%
% push_provider_jpush_http 极光推送 HTTP seam（EPGZ-07）
%
% JPush REST API v3 的薄 HTTP POST 封装：gun + HTTP/2 + TLS verify_peer，
% 连接按 Host 进程字典缓存（与 push_notification_ds:http_post/3 同构）。
%
% 独立成模块的唯一目的：让 push_provider_jpush 的单元测试可以 meck 桩
% 掉真实网络（现有 FCM/APNs 的 http_post 是私有函数无法打桩）。
% 生产调用方仅 push_provider_jpush:send/3，勿在其他模块直接依赖。
%%%

-export([post/3]).

%% ===================================================================
%% API
%% ===================================================================

%% @doc POST JSON 到 JPush 端点
%%
%% 返回 {ok, StatusCode, RespBody} | {error, Reason}；
%% 状态码分类由 push_provider_jpush:classify/2 负责，本层不解释。
-spec post(Url :: binary(), Headers :: [{binary(), binary()}], Body :: binary()) ->
    {ok, integer(), binary()} | {error, term()}.
post(Url, Headers, Body) ->
    try
        #{host := Host, path := Path} = uri_string:parse(Url),
        Port = 443,
        ConnPid = get_or_open_conn(Host, Port),
        HttpTimeout = config_ds:env(push_http_timeout, 15000),
        BodyTimeout = config_ds:env(push_body_timeout, 8000),
        StreamRef = gun:post(ConnPid, Path, Headers, Body),
        case gun:await(ConnPid, StreamRef, HttpTimeout) of
            {response, fin, Status, _RespHeaders} ->
                {ok, Status, <<>>};
            {response, nofin, Status, _RespHeaders} ->
                case gun:await_body(ConnPid, StreamRef, BodyTimeout) of
                    {ok, RespBody} -> {ok, Status, RespBody};
                    {error, Reason} -> {error, Reason}
                end;
            {error, Reason} ->
                %% 连接可能失效，清除缓存让下次重建
                close_cached_conn(Host),
                {error, Reason}
        end
    catch
        _:Error ->
            {error, Error}
    end.

%% ===================================================================
%% Internal Functions
%% ===================================================================

%% @doc 获取或建立到指定 Host 的 HTTP/2 连接（进程字典缓存）
%% NOTE: 与 push_notification_ds 的实现一致，仅在当前进程生命周期内
%% 有效（async_retry 的短生命周期进程中每次执行重建）。
get_or_open_conn(Host, Port) ->
    Key = {gun_conn, ?MODULE, Host},
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
                    _ =
                        try
                            gun:close(ConnPid)
                        catch
                            _:_ -> ok
                        end,
                    erase(Key),
                    error({gun_await_up_failed, AwaitReason})
            end;
        {error, OpenReason} ->
            error({gun_open_failed, OpenReason})
    end.

close_cached_conn(Host) ->
    Key = {gun_conn, ?MODULE, Host},
    case erase(Key) of
        Pid when is_pid(Pid) ->
            _ =
                try
                    gun:close(Pid)
                catch
                    _:_ -> ok
                end,
            ok;
        _ ->
            ok
    end.
