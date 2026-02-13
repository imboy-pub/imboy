-module(login_security_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("log.hrl").
-include("common.hrl").

%%%===================================================================
%%% @doc
%%% login_security_logic 模块的 EUnit 测试
%%%
%%% 目标：验证登录安全逻辑功能
%%% 覆盖：登录前检查、登录后处理、失败记录、锁定判断
%%%===================================================================

%% ===================================================================
%% check_login_allowed/2 测试
%% ===================================================================

check_login_allowed_succeeds_when_no_prior_failures_test_() ->
    ?WITH_MECK(login_attempt_ds, [
        {'is_locked', 2, fun(_Identifier, _Ip) -> false end},
        {'get_remaining_attempts', 2, fun(_Identifier, _Ip) -> {ok, 5} end}
    ], fun() ->
        Identifier = <<"test@example.com">>,
        Ip = <<"127.0.0.1">>,

        {ok, allowed} = login_security_logic:check_login_allowed(Identifier, Ip)
     end).

check_login_allowed_succeeds_when_below_limit_test_() ->
    ?WITH_MECK(login_attempt_ds, [
        {'is_locked', 2, fun(_Identifier, _Ip) -> false end},
        {'get_remaining_attempts', 2, fun(_Identifier, _Ip) -> {ok, 2} end}
    ], fun() ->
        Identifier = <<"test@example.com">>,
        Ip = <<"127.0.0.1">>,

        {ok, allowed} = login_security_logic:check_login_allowed(Identifier, Ip)
     end).

check_login_allowed_fails_when_locked_test_() ->
    ?WITH_MECK(login_attempt_ds, [
        {'is_locked', 2, fun(_Identifier, _Ip) -> true end}
    ], fun() ->
        Identifier = <<"test@example.com">>,
        Ip = <<"127.0.0.1">>,

        Result = login_security_logic:check_login_allowed(Identifier, Ip),
        ?assertMatch({error, locked, _}, Result)
     end).

%% ===================================================================
%% record_login_failure/2 测试
%% ===================================================================

record_login_failure_increments_count_test_() ->
    ?WITH_MECK(login_attempt_ds, [
        {'record_failure', 2, fun(_Identifier, _Ip) -> {ok, 3} end}
    ], fun() ->
        Identifier = <<"test@example.com">>,
        Ip = <<"127.0.0.1">>,

        {ok, 3} = login_security_logic:record_login_failure(Identifier, Ip)
     end).

record_login_failure_returns_lock_warning_when_at_limit_test_() ->
    ?WITH_MECKS([
        {login_attempt_ds, [
            {'record_failure', 2, fun(_Identifier, _Ip) -> {ok, 5} end},
            {'is_locked', 2, fun(_Identifier, _Ip) -> true end}
        ]}
    ], fun() ->
        Identifier = <<"test@example.com">>,
        Ip = <<"127.0.0.1">>,

        Result = login_security_logic:record_login_failure(Identifier, Ip),
        ?assertMatch({ok, 5, locked}, Result)
     end).

%% ===================================================================
%% record_login_success/2 测试
%% ===================================================================

record_login_success_resets_count_test_() ->
    ?WITH_MECK(login_attempt_ds, [
        {'reset', 2, fun(_Identifier, _Ip) -> ok end}
    ], fun() ->
        Identifier = <<"test@example.com">>,
        Ip = <<"127.0.0.1">>,

        ok = login_security_logic:record_login_success(Identifier, Ip)
     end).

%% ===================================================================
%% get_lock_info/2 测试
%% ===================================================================

get_lock_info_returns_details_when_locked_test_() ->
    ?WITH_MECKS([
        {login_attempt_ds, [
            {'get_attempts', 2, fun(_Identifier, _Ip) -> {ok, 5} end},
            {'is_locked', 2, fun(_Identifier, _Ip) -> true end},
            {'get_remaining_attempts', 2, fun(_Identifier, _Ip) -> {ok, 0} end}
        ]},
        {imboy_cache, [
            {'get', 1, fun(_Key) -> {ok, #{<<"count">> => 5, <<"first_fail_at">> => 1640995200000}} end}
        ]},
        {elib_dt, [
            {'add', 2, fun(_Time, _Duration) -> 1640995200000 + 1800000 end}
        ]}
    ], fun() ->
        Identifier = <<"test@example.com">>,
        Ip = <<"127.0.0.1">>,

        {ok, Info} = login_security_logic:get_lock_info(Identifier, Ip),
        ?assertMatch(#{<<"is_locked">> := true,
                       <<"attempts">> := 5,
                       <<"remaining_attempts">> := 0}, Info)
     end).

get_lock_info_returns_details_when_not_locked_test_() ->
    ?WITH_MECKS([
        {login_attempt_ds, [
            {'get_attempts', 2, fun(_Identifier, _Ip) -> {ok, 2} end},
            {'is_locked', 2, fun(_Identifier, _Ip) -> false end},
            {'get_remaining_attempts', 2, fun(_Identifier, _Ip) -> {ok, 3} end}
        ]}
    ], fun() ->
        Identifier = <<"test@example.com">>,
        Ip = <<"127.0.0.1">>,

        {ok, Info} = login_security_logic:get_lock_info(Identifier, Ip),
        ?assertMatch(#{<<"is_locked">> := false,
                       <<"attempts">> := 2,
                       <<"remaining_attempts">> := 3}, Info)
     end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

check_login_allowed_with_empty_identifier_test_() ->
    ?WITH_MECK(login_attempt_ds, [
        {'is_locked', 2, fun(_Identifier, _Ip) -> false end},
        {'get_remaining_attempts', 2, fun(_Identifier, _Ip) -> {ok, 5} end}
    ], fun() ->
        Identifier = <<>>,
        Ip = <<"127.0.0.1">>,

        % 空标识符应该允许登录（可能不是邮箱/手机号登录）
        {ok, allowed} = login_security_logic:check_login_allowed(Identifier, Ip)
     end).

record_login_failure_with_empty_ip_test_() ->
    ?WITH_MECK(login_attempt_ds, [
        {'record_failure', 2, fun(_Identifier, _Ip) -> {ok, 1} end}
    ], fun() ->
        Identifier = <<"test@example.com">>,
        Ip = <<>>,

        % 空IP应该正常记录
        {ok, 1} = login_security_logic:record_login_failure(Identifier, Ip)
     end).

%% ===================================================================
%% 集成测试场景
%% ===================================================================

login_failure_flow_records_and_locks_test_() ->
    ?WITH_MECKS([
        {login_attempt_ds, [
            {'record_failure', 2, fun(_Identifier, _Ip) -> {ok, 1} end},
            {'is_locked', 2, fun(_Identifier, _Ip) -> false end}
        ]}
    ], fun() ->
        Identifier = <<"test@example.com">>,
        Ip = <<"127.0.0.1">>,

        % 第一次失败
        {ok, 1} = login_security_logic:record_login_failure(Identifier, Ip),

        % 检查是否被锁定
        {ok, not_locked} = login_security_logic:check_login_allowed(Identifier, Ip)
     end).

login_success_flow_resets_count_test_() ->
    ?WITH_MECK(login_attempt_ds, [
        {'reset', 2, fun(_Identifier, _Ip) -> ok end}
    ], fun() ->
        Identifier = <<"test@example.com">>,
        Ip = <<"127.0.0.1">>,

        % 登录成功，重置计数
        ok = login_security_logic:record_login_success(Identifier, Ip)
     end).
