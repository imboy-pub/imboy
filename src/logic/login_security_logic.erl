-module(login_security_logic).
-moduledoc "登录安全业务逻辑层 / login security logic。".
%%%===================================================================
%%% @doc
%%% login_security_logic - 登录安全逻辑层（Limiter V1 L3 接线版）
%%%
%%% 本层是 login_attempt_ds 权威状态的无状态薄转发（计划附录 C.3/C.6）：
%%% - 账户域身份键：{user, Uid} | {unknown, Kind, Norm}，跨 IP（D1 冻结）；
%%% - check/failure/reset/snapshot 单一权威，同一逻辑快照（F3/F4 修复）；
%%% - 认证生命周期 begin_attempt/end_attempt 由 passport_logic 编排，
%%%   本层不持有任何状态。
%%%
%%% 使用示例：
%%% ```
%%% Identity = {user, 123},
%%% {ok, true} = login_security_logic:check_login_allowed(Identity),
%%% {ok, Snap} = login_security_logic:record_login_failure(Identity),
%%% ok = login_security_logic:record_login_success(Identity)
%%% ```
%%%===================================================================

%% Dialyzer 抑制误报警告：布尔类型模式匹配的局限性
-dialyzer(
    {nowarn_function, [
        check_login_allowed/2,
        record_login_failure/2,
        build_lock_info/1,
        get_max_attempts/0
    ]}
).

-include("error_code.hrl").
-include("log.hrl").
-include("common.hrl").

%% 新权威 API（C.1/C.3 身份键）
-export([check_login_allowed/1]).
-export([record_login_failure/1]).
-export([record_login_success/1]).
-export([get_lock_info/1]).

%% deprecated 兼容导出（旧 (Identifier, Ip) 键；Ip 参数忽略——账户域跨 IP。
%% 仅供既有调用者/测试过渡，勿新增调用）。
-export([check_login_allowed/2]).
-export([record_login_failure/2]).
-export([record_login_success/2]).
-export([get_lock_info/2]).
-export([check_ip_rate_limit/1]).
-export([admin_unlock/2]).

%% 类型定义
-type identity() :: login_attempt_state:identity().
-type lock_info() :: map().
-type login_check_result() ::
    {ok, true} | {error, locked, lock_info()} | {error, unavailable}.
-type failure_result() ::
    {ok, map()} | {error, capacity_lost | unavailable}.
-type compat_failure_result() ::
    {ok, non_neg_integer()}
    | {ok, non_neg_integer(), locked}
    | {error, invalid_input | capacity_lost | unavailable}.
-type ip_limit_result() :: {ok, pos_integer()} | {error, rate_limited | invalid_ip}.

%%%===================================================================
%%% 新权威 API
%%%===================================================================

%% @doc 检查是否允许登录（不计数、不预扣）。
%% unavailable（owner 缺失/degraded）原样上抛，调用方必须转服务不可用，
%% 不得默认放行（F2 修复：三元组与不可用返回都必须显式处理）。
-spec check_login_allowed(identity()) -> login_check_result().
check_login_allowed(Identity) ->
    case login_attempt_ds:check(Identity) of
        {ok, Snap} ->
            case maps:get(locked, Snap) of
                true ->
                    {error, locked, build_lock_info(Snap)};
                false ->
                    {ok, true}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 记录一次已确定的凭据失败（返回同一逻辑快照）。
%% 锁定判断用返回 Snap 的 locked 字段（同一次判断产出，不再分散读）。
-spec record_login_failure(identity()) -> failure_result().
record_login_failure(Identity) ->
    login_attempt_ds:failure(Identity).

%% @doc 记录登录成功（重置失败计数）。
%% 不再无条件 ok：degraded/owner 缺失返回 {error, unavailable}，
%% 调用方（passport_logic）必须确认成功后才签发凭据（AC-17 成功点）。
-spec record_login_success(identity()) -> ok | {error, unavailable}.
record_login_success(Identity) ->
    login_attempt_ds:reset(Identity).

%% @doc 获取锁定信息（单一快照构造，字段形状兼容旧消费方）。
-spec get_lock_info(identity()) -> {ok, lock_info()} | {error, unavailable}.
get_lock_info(Identity) ->
    case login_attempt_ds:snapshot(Identity) of
        {ok, Snap} ->
            {ok, build_lock_info(Snap)};
        {error, Reason} ->
            {error, Reason}
    end.

%%%===================================================================
%%% deprecated 兼容导出（旧 (Identifier, Ip) 键 → {unknown, account, Id}）
%%%===================================================================

%% deprecated: 转发 check_login_allowed/1；Ip 参数忽略（账户域跨 IP）。
%% 空标识符保持旧行为放行（格式非法属 C.2 格式错误，不进计数域）。
%% @private 兼容转发，勿新增调用。
-spec check_login_allowed(binary(), binary()) -> login_check_result().
check_login_allowed(<<>>, _Ip) ->
    {ok, true};
check_login_allowed(Identifier, _Ip) when is_binary(Identifier) ->
    check_login_allowed({unknown, account, Identifier});
check_login_allowed(_, _) ->
    {ok, true}.

%% deprecated: 转发 record_login_failure/1；返回旧形状
%% {ok, Count} | {ok, Count, locked}。
%% @private 兼容转发，勿新增调用。
-spec record_login_failure(binary(), binary()) -> compat_failure_result().
record_login_failure(Identifier, _Ip) when is_binary(Identifier), byte_size(Identifier) > 0 ->
    case record_login_failure({unknown, account, Identifier}) of
        {ok, Snap} ->
            case maps:get(locked, Snap) of
                true -> {ok, maps:get(count, Snap), locked};
                false -> {ok, maps:get(count, Snap)}
            end;
        {error, Reason} ->
            {error, Reason}
    end;
record_login_failure(Identifier, Ip) ->
    ok = ?ERROR_LOG([login_attempt, invalid_input, Identifier, Ip]),
    {error, invalid_input}.

%% deprecated: 转发 record_login_success/1；真实结果透传（不再无条件 ok）。
%% @private 兼容转发，勿新增调用。
-spec record_login_success(binary(), binary()) -> ok | {error, unavailable}.
record_login_success(Identifier, _Ip) when is_binary(Identifier), byte_size(Identifier) > 0 ->
    record_login_success({unknown, account, Identifier});
record_login_success(Identifier, Ip) ->
    ok = ?ERROR_LOG([login_attempt, invalid_input, Identifier, Ip]),
    {error, unavailable}.

%% deprecated: 转发 get_lock_info/1。
%% @private 兼容转发，勿新增调用。
-spec get_lock_info(binary(), binary()) -> {ok, lock_info()} | {error, unavailable}.
get_lock_info(Identifier, _Ip) when is_binary(Identifier), byte_size(Identifier) > 0 ->
    get_lock_info({unknown, account, Identifier});
get_lock_info(_, _) ->
    {error, unavailable}.

%% deprecated: IP 请求准入已迁 login_attempt_ds:admit/2（C.3/C.8），
%% 本转发仅供未迁移调用者（OIDC 归 A3）过渡。
%% @private 兼容转发，勿新增调用。
-spec check_ip_rate_limit(binary()) -> ip_limit_result().
check_ip_rate_limit(Ip) ->
    login_attempt_ds:check_ip_rate_limit(Ip).

%% deprecated: 管理员解锁；identity 归一由 DS 兼容层处理。
%% @private 兼容转发，勿新增调用。
-spec admin_unlock(integer(), binary() | identity()) ->
    {ok, unlocked} | {error, term()}.
admin_unlock(AdminUid, Identifier) ->
    login_attempt_ds:admin_unlock(AdminUid, Identifier).

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @doc 构建锁定信息：count/remaining/locked/expires 出自同一逻辑快照
%% （F4 修复：不再分散读 ETS + Cache）。max_attempts 取快照冻结的窗口
%% 规则（rule_version = {Max, WindowMs}），配置热变更不影响已开窗口。
%% locked_until 为单调毫秒（expires_at_ms，可 undefined）；仅供
%% passport_logic 内部字符串化，不再伪造 wall 时间。
-spec build_lock_info(map()) -> lock_info().
build_lock_info(Snap) ->
    MaxAttempts =
        case maps:get(rule_version, Snap) of
            {F, _WindowMs} when is_integer(F), F > 0 -> F;
            _ -> get_max_attempts()
        end,
    %% locked_until 仅锁定时有意义：取同一快照的窗口到期（单调毫秒）；
    %% 未锁定恒 undefined（旧行为：未锁定不给出到期时间）。
    LockedUntil =
        case maps:get(locked, Snap) of
            true -> maps:get(expires_at_ms, Snap);
            false -> undefined
        end,
    #{
        <<"is_locked">> => maps:get(locked, Snap),
        <<"attempts">> => maps:get(count, Snap),
        <<"remaining_attempts">> => maps:get(remaining, Snap),
        <<"max_attempts">> => MaxAttempts,
        <<"locked_until">> => LockedUntil
    }.

%% @doc 兜底最大尝试次数（快照规则异常时才走配置）。
%% @private
get_max_attempts() ->
    case application:get_env(imboy, login_max_attempts) of
        {ok, Val} when is_integer(Val), Val > 0 -> Val;
        _ -> 5
    end.
