-module(login_security_logic).
%%%===================================================================
%%% @doc
%%% login_security_logic - 登录安全逻辑层
%%%
%%% 功能：
%%% - 检查登录是否被允许
%%% - 记录登录失败/成功
%%% - 获取锁定信息
%%% - IP限流检查
%%%
%%% 使用示例：
%%% ```
%%% {ok, true} = login_security_logic:check_login_allowed(<<"user@test.com">>, <<"127.0.0.1">>),
%%% {ok, 1} = login_security_logic:record_login_failure(<<"user@test.com">>, <<"127.0.0.1">>)
%%% ```
%%%===================================================================

%% Dialyzer 抑制误报警告：布尔类型模式匹配的局限性
-dialyzer({nowarn_function, [check_login_allowed/2, record_login_failure/2, build_lock_info/2, get_lock_duration_minutes/0]}).

-include("error_code.hrl").
-include("log.hrl").
-include("common.hrl").

%% API 函数
-export([check_login_allowed/2]).
-export([record_login_failure/2]).
-export([record_login_success/2]).
-export([get_lock_info/2]).
-export([check_ip_rate_limit/1]).
-export([admin_unlock/2]).

%% 类型定义
-type login_check_result() :: {ok, true} | {error, locked, map()}.
-type failure_result() :: {ok, pos_integer()} | {ok, pos_integer(), locked}.
-type ip_limit_result() :: {ok, pos_integer()} | {error, rate_limited}.

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc 检查是否允许登录
%% @param Identifier 登录标识符（邮箱/手机号/账号）
%% @param Ip 客户端IP地址
%% @returns {ok, true} | {error, locked, map()}
-spec check_login_allowed(binary(), binary()) -> login_check_result().
check_login_allowed(Identifier, Ip) ->
    case login_attempt_ds:is_locked(Identifier, Ip) of
        true ->
            % 被锁定，返回锁定信息
            LockInfo = build_lock_info(Identifier, Ip),
            {error, locked, LockInfo};
        false ->
            {ok, true}
    end.

%% @doc 记录登录失败
%% @param Identifier 登录标识符
%% @param Ip 客户端IP地址
%% @returns {ok, Count} | {ok, Count, locked} | {error, term()}
-spec record_login_failure(binary(), binary()) -> failure_result().
record_login_failure(Identifier, Ip) ->
    % 首先检查IP限流
    case login_attempt_ds:check_ip_rate_limit(Ip) of
        {error, rate_limited} ->
            {error, rate_limited};
        {ok, _IpCount} ->
            {ok, Count} = login_attempt_ds:record_failure(Identifier, Ip),
            % 检查是否达到锁定阈值
            case login_attempt_ds:is_locked(Identifier, Ip) of
                true ->
                    {ok, Count, locked};
                false ->
                    {ok, Count}
            end
    end.

%% @doc 记录登录成功（重置失败计数）
%% @param Identifier 登录标识符
%% @param Ip 客户端IP地址
%% @returns ok
-spec record_login_success(binary(), binary()) -> ok.
record_login_success(Identifier, Ip) ->
    login_attempt_ds:reset(Identifier, Ip).

%% @doc 获取锁定信息
%% @param Identifier 登录标识符
%% @param Ip 客户端IP地址
%% @returns {ok, LockInfo}
-spec get_lock_info(binary(), binary()) -> {ok, map()}.
get_lock_info(Identifier, Ip) ->
    {ok, build_lock_info(Identifier, Ip)}.

%% @doc 检查IP限流
%% @param Ip 客户端IP地址
%% @returns {ok, Count} | {error, rate_limited}
-spec check_ip_rate_limit(binary()) -> ip_limit_result().
check_ip_rate_limit(Ip) ->
    login_attempt_ds:check_ip_rate_limit(Ip).

%% @doc 管理员强制解锁账户
%% @param AdminUid 管理员用户ID
%% @param Identifier 要解锁的账户标识符
%% @returns {ok, unlocked} | {error, term()}
-spec admin_unlock(integer(), binary()) -> {ok, unlocked} | {error, term()}.
admin_unlock(AdminUid, Identifier) ->
    login_attempt_ds:admin_unlock(AdminUid, Identifier).

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @doc 获取最大尝试次数（从配置读取）
%% @private
get_max_attempts() ->
    case application:get_env(imboy, login_max_attempts) of
        {ok, Val} when is_integer(Val), Val > 0 -> Val;
        _ -> 5
    end.

%% @doc 获取锁定时长（从配置读取，单位：分钟）
%% @private
get_lock_duration_minutes() ->
    case application:get_env(imboy, login_lock_duration_minutes) of
        {ok, Val} when is_integer(Val), Val > 0 -> Val;
        _ -> 30
    end.

%% @doc 构建锁定信息
%% @private
-spec build_lock_info(binary(), binary()) -> map().
build_lock_info(Identifier, Ip) ->
    {ok, Attempts} = login_attempt_ds:get_attempts(Identifier, Ip),
    IsLocked = login_attempt_ds:is_locked(Identifier, Ip),
    {ok, Remaining} = login_attempt_ds:get_remaining_attempts(Identifier, Ip),
    MaxAttempts = get_max_attempts(),

    BaseInfo = #{
        <<"is_locked">> => IsLocked,
        <<"attempts">> => Attempts,
        <<"remaining_attempts">> => Remaining,
        <<"max_attempts">> => MaxAttempts
    },

    % 如果被锁定，添加锁定过期时间
    case IsLocked of
        true ->
            case imboy_cache:get(login_attempt_ds:cache_key(Identifier, Ip)) of
                {ok, #{<<"first_fail_at">> := FirstFailAt}} ->
                    LockDurationMinutes = get_lock_duration_minutes(),
                    LockExpiryTime = elib_dt:add(FirstFailAt, {LockDurationMinutes, minute}),
                    BaseInfo#{<<"locked_until">> => LockExpiryTime};
                _ ->
                    BaseInfo
            end;
        false ->
            BaseInfo
    end.
