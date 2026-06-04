-module(login_attempt_ds).
%%%===================================================================
%%% @doc
%%% login_attempt_ds - 登录失败次数限制数据服务层
%%%
%%% 功能：
%%% - 记录登录失败次数（原子操作，防止竞态条件）
%%% - 检查账号是否被锁定
%%% - 重置失败计数
%%% - IP 限流保护
%%%
%%% 配置：
%%% - MAX_ATTEMPTS: 最大失败次数（默认 5）
%%% - LOCK_DURATION_MINUTES: 锁定时长（默认 30 分钟）
%%% - MAX_IP_ATTEMPTS_PER_HOUR: 单IP每小时最大尝试次数（默认 100）
%%%
%%% 使用示例：
%%% ```
%%% {ok, Count} = login_attempt_ds:record_failure(<<"user@test.com">>, <<"127.0.0.1">>),
%%% true = login_attempt_ds:is_locked(<<"user@test.com">>, <<"127.0.0.1">>)
%%% ```
%%%
%%% 竞态条件防护：
%%% - record_failure/2 使用 gen_server + ETS 保证原子操作
%%% - 即使在高并发场景下也能正确限制登录尝试次数
%%%===================================================================

-behaviour(gen_server).

%% Dialyzer 抑制误报警告：时间比较操作确实会返回布尔值，Dialyzer 类型推断问题
-dialyzer({nowarn_function, [handle_call/3]}).

-include("error_code.hrl").
-include("log.hrl").
-include("common.hrl").

%% API 函数
-export([start_link/0]).
-export([record_failure/2]).
-export([is_locked/2]).
-export([get_attempts/2]).
-export([reset/2]).
-export([get_remaining_attempts/2]).
-export([cache_key/2]).
-export([check_ip_rate_limit/1]).
-export([extract_real_ip/1]).
-export([validate_ip/1]).
-export([admin_unlock/2]).
-export([is_admin/1]).

%% gen_server 回调
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ETS 表名和缓存键宏定义
-define(LOGIN_ATTEMPT_ETS, login_attempt_ets).
-define(LOGIN_ATTEMPT_CACHE_KEY(Identifier, Ip), {login_attempt, Identifier, Ip}).
-define(IP_RATE_LIMIT_CACHE_KEY(Ip), {login_attempt_ip_limit, Ip}).

%% 配置常量宏定义（用于 guard 表达式）
-define(MAX_ATTEMPTS_DEFAULT, 5).
-define(LOCK_DURATION_MINUTES_DEFAULT, 30).
-define(MAX_IP_ATTEMPTS_PER_HOUR_DEFAULT, 100).

%% 为了兼容 guard，使用默认值常量
-define(MAX_ATTEMPTS, ?MAX_ATTEMPTS_DEFAULT).
-define(LOCK_DURATION_MINUTES, ?LOCK_DURATION_MINUTES_DEFAULT).
-define(MAX_IP_ATTEMPTS_PER_HOUR, ?MAX_IP_ATTEMPTS_PER_HOUR_DEFAULT).

%% 类型定义
-type login_result() :: {ok, pos_integer()} | {ok, pos_integer(), locked} | {error, term()}.
-type ip_rate_limit_result() :: {ok, pos_integer()} | {error, rate_limited}.

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc 启动 login_attempt_ds gen_server
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc 记录登录失败并返回当前失败次数（原子操作版本）
%% @param Identifier 登录标识符（邮箱/手机号/账号）
%% @param Ip 客户端IP地址
%% @returns {ok, Count} | {error, invalid_input}
-spec record_failure(binary(), binary()) -> login_result().
record_failure(Identifier, Ip) when
    is_binary(Identifier),
    is_binary(Ip),
    byte_size(Identifier) > 0,
    byte_size(Ip) > 0
->
    Key = cache_key(Identifier, Ip),
    gen_server:call(?MODULE, {record_failure, Key, Identifier, Ip});
record_failure(Identifier, Ip) ->
    % 记录无效输入
    ok = ?ERROR_LOG([login_attempt, invalid_input, Identifier, Ip]),
    {error, invalid_input}.

%% @doc 检查是否被锁定
%% @param Identifier 登录标识符
%% @param Ip 客户端IP地址
%% @returns true | false
-spec is_locked(binary(), binary()) -> boolean().
is_locked(<<>>, _Ip) ->
    false;
is_locked(Identifier, Ip) ->
    Key = cache_key(Identifier, Ip),
    MaxAttempts = get_max_attempts(),
    LockDurationMins = get_lock_duration_minutes(),
    case ets:lookup(?LOGIN_ATTEMPT_ETS, Key) of
        [{Key, #{<<"count">> := Count, <<"first_fail_at">> := FirstFailAt}}] when
            Count >= MaxAttempts
        ->
            % 检查是否在锁定时间内
            Now = elib_dt:now(),
            LockExpiryTime = elib_dt:add(FirstFailAt, {LockDurationMins, minute}),
            Now < LockExpiryTime;
        _ ->
            false
    end.

%% @doc 获取失败次数
%% @param Identifier 登录标识符
%% @param Ip 客户端IP地址
%% @returns {ok, Count} 失败次数
-spec get_attempts(binary(), binary()) -> {ok, non_neg_integer()}.
get_attempts(Identifier, Ip) ->
    Key = cache_key(Identifier, Ip),
    case ets:lookup(?LOGIN_ATTEMPT_ETS, Key) of
        [{Key, #{<<"count">> := Count}}] ->
            {ok, Count};
        [] ->
            {ok, 0}
    end.

%% @doc 重置失败次数（登录成功时调用）
%% @param Identifier 登录标识符
%% @param Ip 客户端IP地址
%% @returns ok
-spec reset(binary(), binary()) -> ok.
reset(Identifier, Ip) ->
    Key = cache_key(Identifier, Ip),
    ets:delete(?LOGIN_ATTEMPT_ETS, Key),
    ok.

%% @doc 获取剩余尝试次数
%% @param Identifier 登录标识符
%% @param Ip 客户端IP地址
%% @returns {ok, Remaining} 剩余尝试次数
-spec get_remaining_attempts(binary(), binary()) -> {ok, non_neg_integer()}.
get_remaining_attempts(Identifier, Ip) ->
    MaxAttempts = get_max_attempts(),
    case get_attempts(Identifier, Ip) of
        {ok, Count} when Count >= MaxAttempts ->
            {ok, 0};
        {ok, Count} ->
            {ok, MaxAttempts - Count}
    end.

%% @doc 生成缓存键
%% @param Identifier 登录标识符
%% @param Ip 客户端IP地址
%% @returns 缓存键（tuple）
-spec cache_key(binary(), binary()) -> term().
cache_key(Identifier, Ip) ->
    % 使用元组作为缓存键，避免冲突
    ?LOGIN_ATTEMPT_CACHE_KEY(Identifier, Ip).

%% @doc 检查IP限流
%% @param Ip 客户端IP地址
%% @returns {ok, Count} | {error, rate_limited}
-spec check_ip_rate_limit(binary()) -> ip_rate_limit_result().
check_ip_rate_limit(Ip) when
    is_binary(Ip), byte_size(Ip) > 0
->
    Key = ?IP_RATE_LIMIT_CACHE_KEY(Ip),
    MaxIpAttempts = get_max_ip_attempts_per_hour(),
    case imboy_cache:get(Key) of
        undefined ->
            imboy_cache:set(Key, 1, 3600),
            {ok, 1};
        {ok, Count} when Count >= MaxIpAttempts ->
            {error, rate_limited};
        {ok, Count} ->
            imboy_cache:set(Key, Count + 1, 3600),
            {ok, Count + 1}
    end;
check_ip_rate_limit(Ip) ->
    ok = ?ERROR_LOG([login_attempt, invalid_ip, Ip]),
    {error, invalid_ip}.

%% @doc 提取真实客户端IP地址（防止伪造）
%% @param Headers Cowboy 请求头 Map
%% @returns {ok, Ip} | {error, invalid_ip}
%%
%% 优先级：
%% 1. CF-Connecting-IP (Cloudflare)
%% 2. X-Real-IP (Nginx)
%% 3. X-Forwarded-For 的第一个 IP
%% 4. 默认 0.0.0.0
%%
%% 使用示例：
%% ```
%% % 从 Cowboy Req 提取头部
%% Headers = cowboy_req:headers(Req),
%% {ok, RealIp} = login_attempt_ds:extract_real_ip(Headers)
%% ```
-spec extract_real_ip(map()) -> {ok, binary()} | {error, term()}.
extract_real_ip(Headers) when is_map(Headers) ->
    % 优先级：CF-Connecting-IP > X-Real-IP > X-Forwarded-For
    CfConnectingIp = maps:get(<<"cf-connecting-ip">>, Headers, <<>>),
    XRealIp = maps:get(<<"x-real-ip">>, Headers, <<>>),
    XForwardedFor = maps:get(<<"x-forwarded-for">>, Headers, <<>>),

    % 选择最可信的 IP
    RawIp =
        case CfConnectingIp of
            <<>> ->
                case XRealIp of
                    <<>> -> XForwardedFor;
                    _ -> XRealIp
                end;
            _ ->
                CfConnectingIp
        end,

    % 提取第一个 IP（X-Forwarded-For 可能是 "client, proxy1, proxy2"）
    FirstIp = parse_first_ip(RawIp),

    % 验证 IP 格式
    case validate_ip(FirstIp) of
        {ok, ValidIp} -> {ok, ValidIp};
        % 返回默认值
        {error, _} -> {ok, <<"0.0.0.0">>}
    end;
extract_real_ip(_) ->
    {ok, <<"0.0.0.0">>}.

%% @doc 验证IP地址格式
%% @param Ip IP地址（binary）
%% @returns {ok, Ip} | {error, invalid_ip}
-spec validate_ip(binary()) -> {ok, binary()} | {error, invalid_ip}.
validate_ip(Ip) when is_binary(Ip), byte_size(Ip) > 0 ->
    TrimmedIp = string:trim(Ip),
    case inet:parse_ipv4strict_address(binary_to_list(TrimmedIp)) of
        {ok, _} ->
            {ok, TrimmedIp};
        {error, _} ->
            case inet:parse_ipv6strict_address(binary_to_list(TrimmedIp)) of
                {ok, _} -> {ok, TrimmedIp};
                _ -> {error, invalid_ip}
            end
    end;
validate_ip(_) ->
    {error, invalid_ip}.

%% @doc 管理员强制解锁账户
%% @param AdminUid 管理员用户ID（需要验证权限）
%% @param Identifier 要解锁的账户标识符
%% @returns {ok, unlocked} | {error, unauthorized} | {error, term()}
%%
%% 使用示例：
%% ```
%% {ok, unlocked} = login_attempt_ds:admin_unlock(AdminUid, <<"user@test.com">>)
%% ```
-spec admin_unlock(integer(), binary()) -> {ok, unlocked} | {error, term()}.
admin_unlock(AdminUid, Identifier) when is_integer(AdminUid), AdminUid > 0 ->
    % 1. 验证管理员权限
    case is_admin(AdminUid) of
        false ->
            ok = ?WARN_LOG([login_attempt, admin_unlock_unauthorized, AdminUid, Identifier]),
            {error, unauthorized};
        true ->
            % 2. 清除该标识符的所有IP的锁定记录
            clear_all_lockouts_for_identifier(Identifier),

            % 3. 记录审计日志
            ok = ?INFO_LOG([
                login_attempt, admin_unlock_success, AdminUid, sanitize_identifier(Identifier)
            ]),
            {ok, unlocked}
    end;
admin_unlock(AdminUid, Identifier) ->
    ok = ?ERROR_LOG([login_attempt, admin_unlock_invalid_input, AdminUid, Identifier]),
    {error, invalid_input}.

%% @doc 检查是否为管理员
%% @param Uid 用户ID
%% @returns true | false
-spec is_admin(integer()) -> boolean().
is_admin(Uid) when is_integer(Uid) ->
    % 从用户数据中获取角色信息
    % 这里简化实现，实际应该查询 user_ds 或 user_repo
    case application:get_env(imboy, admin_uids) of
        {ok, AdminUids} when is_list(AdminUids) ->
            lists:member(Uid, AdminUids);
        _ ->
            % 默认检查 UID 是否小于 10000（假设管理员是小UID）
            Uid < 10000
    end;
is_admin(_) ->
    false.

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @doc 敏感信息脱敏（用于日志）
%% @private
sanitize_identifier(Identifier) ->
    case byte_size(Identifier) of
        N when N > 8 ->
            % 保留前4位和后4位，中间用***代替
            Prefix = binary:part(Identifier, 0, 4),
            Suffix = binary:part(Identifier, N - 4, 4),
            <<Prefix/binary, "***", Suffix/binary>>;
        _ ->
            % 太短不脱敏
            Identifier
    end.

%% @doc 解析 X-Forwarded-For 中的第一个 IP
%% @private
parse_first_ip(<<>>) ->
    <<"0.0.0.0">>;
parse_first_ip(XForwardedFor) ->
    % X-Forwarded-For 格式: "client, proxy1, proxy2"
    case binary:split(XForwardedFor, <<",">>) of
        [FirstIp | _] -> string:trim(FirstIp);
        _ -> <<"0.0.0.0">>
    end.

%% @doc 清除指定标识符的所有锁定记录
%% @private
clear_all_lockouts_for_identifier(Identifier) ->
    % 遍历 ETS 表，删除匹配的记录
    % 注意：这里使用默认值，因为 guard 中不能调用函数
    MaxAttempts = get_max_attempts(),
    ets:foldl(
        fun
            ({Key, #{<<"count">> := Count}}, Acc) when Count >= MaxAttempts ->
                case Key of
                    {login_attempt, Id, _Ip} when Id =:= Identifier ->
                        ets:delete(?LOGIN_ATTEMPT_ETS, Key);
                    _ ->
                        Acc
                end;
            (_, Acc) ->
                Acc
        end,
        ok,
        ?LOGIN_ATTEMPT_ETS
    ).

%%%===================================================================
%%% gen_server 回调函数
%%%===================================================================

%% @private
init([]) ->
    % 创建 ETS 表用于原子计数
    _ = ets:new(?LOGIN_ATTEMPT_ETS, [
        named_table,
        set,
        public,
        {read_concurrency, true},
        {write_concurrency, true}
    ]),
    % 定期清理过期数据
    _ = timer:send_interval(60000, self(), cleanup_expired),
    {ok, #{}}.

%% @private
handle_call({record_failure, Key, Identifier, Ip}, _From, State) ->
    Now = elib_dt:now(),
    MaxAttempts = get_max_attempts(),
    LockDurationMins = get_lock_duration_minutes(),

    % 使用 ETS 操作来原子性地更新计数
    Result =
        case ets:lookup(?LOGIN_ATTEMPT_ETS, Key) of
            [] ->
                % 第一次失败，创建新记录
                Data = #{
                    <<"count">> => 1,
                    <<"first_fail_at">> => Now
                },
                ets:insert(?LOGIN_ATTEMPT_ETS, {Key, Data}),
                % 记录审计日志
                ok = ?INFO_LOG([
                    login_attempt, record_failure, sanitize_identifier(Identifier), Ip, 1
                ]),
                {ok, 1};
            [{Key, #{<<"count">> := Count, <<"first_fail_at">> := FirstFailAt}}] ->
                % 检查是否已过期
                LockExpiryTime = elib_dt:add(FirstFailAt, {LockDurationMins, minute}),

                case Now >= LockExpiryTime of
                    true ->
                        % 已过期，重新计数
                        NewData = #{
                            <<"count">> => 1,
                            <<"first_fail_at">> => Now
                        },
                        ets:insert(?LOGIN_ATTEMPT_ETS, {Key, NewData}),
                        ok = ?INFO_LOG([
                            login_attempt, reset_and_record, sanitize_identifier(Identifier), Ip, 1
                        ]),
                        {ok, 1};
                    false ->
                        % 未过期，增加计数（gen_server 保证原子性）
                        NewCount = Count + 1,
                        NewData = #{
                            <<"count">> => NewCount,
                            <<"first_fail_at">> => FirstFailAt
                        },
                        ets:insert(?LOGIN_ATTEMPT_ETS, {Key, NewData}),

                        % 记录审计日志
                        case NewCount >= MaxAttempts of
                            true ->
                                ok = ?WARN_LOG([
                                    login_attempt,
                                    account_locked,
                                    sanitize_identifier(Identifier),
                                    Ip,
                                    NewCount
                                ]);
                            false ->
                                ok = ?INFO_LOG([
                                    login_attempt,
                                    record_failure,
                                    sanitize_identifier(Identifier),
                                    Ip,
                                    NewCount
                                ])
                        end,
                        {ok, NewCount}
                end
        end,
    {reply, Result, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(cleanup_expired, State) ->
    % 定期清理过期的登录尝试记录
    Now = elib_dt:now(),
    LockDurationMins = get_lock_duration_minutes(),
    LockExpiryTime = elib_dt:add(Now, {-LockDurationMins, minute}),

    % 遍历 ETS 表，删除过期记录
    ets:foldl(
        fun({Key, #{<<"first_fail_at">> := FirstFailAt}}, Acc) ->
            case FirstFailAt < LockExpiryTime of
                true ->
                    ets:delete(?LOGIN_ATTEMPT_ETS, Key);
                false ->
                    Acc
            end
        end,
        ok,
        ?LOGIN_ATTEMPT_ETS
    ),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% 内部辅助函数 - 运行时配置获取
%%%===================================================================

%% @doc 获取最大失败次数配置（支持运行时覆盖）
%% @private
-spec get_max_attempts() -> pos_integer().
get_max_attempts() ->
    case application:get_env(imboy, login_max_attempts) of
        {ok, Val} when is_integer(Val), Val > 0 -> Val;
        _ -> ?MAX_ATTEMPTS_DEFAULT
    end.

%% @doc 获取锁定时长配置（支持运行时覆盖）
%% @private
-spec get_lock_duration_minutes() -> pos_integer().
get_lock_duration_minutes() ->
    case application:get_env(imboy, login_lock_duration_minutes) of
        {ok, Val} when is_integer(Val), Val > 0 -> Val;
        _ -> ?LOCK_DURATION_MINUTES_DEFAULT
    end.

%% @doc 获取IP限流配置（支持运行时覆盖）
%% @private
-spec get_max_ip_attempts_per_hour() -> pos_integer().
get_max_ip_attempts_per_hour() ->
    case application:get_env(imboy, login_max_ip_attempts_per_hour) of
        {ok, Val} when is_integer(Val), Val > 0 -> Val;
        _ -> ?MAX_IP_ATTEMPTS_PER_HOUR_DEFAULT
    end.
