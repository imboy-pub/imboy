%%%-------------------------------------------------------------------
%%% @doc QR 码登录业务逻辑层
%%% 封装 qr_login_handler 的业务逻辑，避免 Handler 直接调用 DS 层。
%%% @end
%%%-------------------------------------------------------------------
-module(qr_login_logic).

-moduledoc "QR 码登录业务逻辑层 / QR login logic。".
-include("log.hrl").
-include("common.hrl").

-export([log_device_login/4, web_session/1, claim_web_cache_token/1]).

%% @doc 记录设备 QR 扫码登录
%% @param Uid    用户ID
%% @param DeviceId 设备ID
%% @param DeviceName 设备名称
%% @param Platform 平台
%% @return ok
-spec log_device_login(integer(), binary(), binary(), binary()) ->
    ok | {ok, pos_integer()} | {error, term()}.
log_device_login(Uid, DeviceId, DeviceName, Platform) ->
    Now = elib_dt:now(),
    PostVals = #{
        <<"dname">> => DeviceName,
        <<"cos">> => Platform,
        <<"login_type">> => <<"qr_scan">>
    },
    try
        case Platform of
            <<"web">> ->
                case user_ds:find_by_id(Uid, <<"status,account_type">>) of
                    #{<<"status">> := Status, <<"account_type">> := 0} when
                        Status =:= 1; Status =:= 2
                    ->
                        user_device_ds:register_web(Now, Uid, DeviceId, PostVals);
                    _ ->
                        {error, invalid_account}
                end;
            _ ->
                case user_device_ds:save(Now, Uid, DeviceId, PostVals) of
                    {ok, _} -> ok;
                    {error, _} = Error -> Error;
                    _ -> {error, unavailable}
                end
        end
    catch
        _:_ -> {error, unavailable}
    end.

%% Web aliases may consume only the matching server-owned Human Web session.
-spec web_session(binary()) -> {ok, map()} | {error, term()}.
web_session(SessionToken) ->
    try
        Session =
            case application:get_env(imboy, qr_login_authority, cache) of
                pg ->
                    qr_login_session_ds:find_by_session_token(SessionToken);
                _ ->
                    case imboy_cache:get({qr_login, SessionToken}) of
                        {ok, Value} -> Value;
                        _ -> undefined
                    end
            end,
        case Session of
            #{<<"purpose">> := <<"human">>, <<"platform">> := <<"web">>} -> {ok, Session};
            {error, _} -> {error, unavailable};
            undefined -> {error, not_found};
            _ -> {error, forbidden}
        end
    catch
        _:_ -> {error, unavailable}
    end.

%% All Human Web cache consumers share one node-local critical section.
-spec claim_web_cache_token(binary()) -> {ok, map()} | {error, term()}.
claim_web_cache_token(SessionToken) ->
    try
        global:trans(
            {{qr_login_claim, SessionToken}, self()},
            fun() ->
                %% A caller may already hold a stale depcache process-dictionary copy.
                ok = imboy_cache:flush_process_dict(),
                case imboy_cache:get({qr_login, SessionToken}) of
                    {ok,
                        #{
                            <<"purpose">> := <<"human">>,
                            <<"platform">> := <<"web">>,
                            <<"status">> := <<"confirmed">>,
                            <<"login_token">> := Token,
                            <<"scanned_by">> := Uid
                        } = Session} when is_binary(Token) ->
                        %% is_binary(Token)：墓碑（DEF-F6 R2）的 login_token 是 null 原子，
                        %% map 模式不做类型过滤，缺此守卫二次领取会照常放行。
                        %% DEF-F6-QR-STATUS-403-FREEZE：领取后不删除会话，改写为无 token
                        %% 的 confirmed 墓碑（TTL 120s ≥ App 2s 轮询宽限窗）。一次性交付
                        %% 语义不变：login_token 字段已抹除，二次领取走下方 not_claimable；
                        %% status 轮询在宽限窗内拿到 200 {status: "confirmed"}（幂等终态），
                        %% 不再踩 "Forbidden QR session" 403 冻结已扫描态。
                        %% delete/1 -> depcache:flush/2 uses gen_server:call and replies after deletion.
                        Tombstone = Session#{
                            <<"status">> => <<"confirmed">>,
                            <<"login_token">> => null,
                            <<"claimed_at">> => erlang:system_time(millisecond)
                        },
                        ok = imboy_cache:set({qr_login, SessionToken}, Tombstone, 120),
                        {ok, #{login_token => Token, scanned_by => Uid}};
                    _ ->
                        {error, not_claimable}
                end
            end,
            [node()]
        )
    of
        {ok, _} = Claim -> Claim;
        {error, _} = Error -> Error;
        _ -> {error, unavailable}
    catch
        _:_ -> {error, unavailable}
    end.
