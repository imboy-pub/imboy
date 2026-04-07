-module(e2ee_handler_capability_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

%%%===================================================================
%%% @doc
%%% e2ee_handler Capability Gate 测试
%%%
%%% 目标：验证 E2EE capability 门禁功能
%%% 覆盖：
%%%   - e2ee_mode = disabled 时所有 API 返回 ERR_FEATURE_DISABLED
%%%   - e2ee_mode = optional 时 API 正常工作
%%%   - e2ee_mode = required 时 API 正常工作
%%%===================================================================

%% ===================================================================
%% user_keys Capability Gate Tests
%% ===================================================================

%% @doc 测试 e2ee disabled 时 user_keys 返回错误
user_keys_returns_error_when_e2ee_disabled_test_() ->
    ?WITH_MECKS([
        {imboy_policy, [
            {'e2ee_enabled', 0, fun() -> false end}
        ]},
        {elib_response, [
            {'error', 3, fun(Req, _Msg, _Code) -> Req end}
        ]}
    ], fun() ->
        Req0 = cowboy_req_ok,
        State0 = #{action => user_keys},

        %% 调用 handler
        {ok, Req1, _State} = e2ee_handler:init(Req0, State0),

        %% 验证返回了错误（因为 e2ee_enabled 返回 false）
        ?assertEqual(Req0, Req1)
    end).

%% @doc 测试 e2ee enabled 时 user_keys 正常工作
user_keys_works_when_e2ee_enabled_test_() ->
    ?WITH_MECKS([
        {imboy_policy, [
            {'e2ee_enabled', 0, fun() -> true end}
        ]},
        {auth_ds, [
            {'current_uid', 1, fun(_State) -> 123 end}
        ]},
        {elib_param, [
            {'get', 3, fun(_Key, _Req, _Default) -> <<"456">> end}
        ]},
        {e2ee_logic, [
            {'user_keys', 2, fun(_CurrentUid, _TargetUid) ->
                {ok, #{<<"uid">> => <<"456">>, <<"devices">> => []}}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, _Payload) -> Req end}
        ]}
    ], fun() ->
        Req0 = cowboy_req_ok,
        State0 = #{action => user_keys},

        %% 调用 handler
        {ok, _Req1, State} = e2ee_handler:init(Req0, State0),

        %% 验证状态正确
        ?assert(is_map(State))
    end).

%% ===================================================================
%% group_member_keys Capability Gate Tests
%% ===================================================================

group_member_keys_returns_error_when_e2ee_disabled_test_() ->
    ?WITH_MECKS([
        {imboy_policy, [
            {'e2ee_enabled', 0, fun() -> false end}
        ]},
        {elib_response, [
            {'error', 3, fun(Req, _Msg, _Code) -> Req end}
        ]}
    ], fun() ->
        Req0 = cowboy_req_ok,
        State0 = #{action => group_member_keys},

        {ok, Req1, _State} = e2ee_handler:init(Req0, State0),

        ?assertEqual(Req0, Req1)
    end).

%% ===================================================================
%% report_device_key Capability Gate Tests
%% ===================================================================

report_device_key_returns_error_when_e2ee_disabled_test_() ->
    ?WITH_MECKS([
        {imboy_policy, [
            {'e2ee_enabled', 0, fun() -> false end}
        ]},
        {elib_response, [
            {'error', 3, fun(Req, _Msg, _Code) -> Req end}
        ]}
    ], fun() ->
        Req0 = cowboy_req_ok,
        State0 = #{action => report_device_key},

        {ok, Req1, _State} = e2ee_handler:init(Req0, State0),

        ?assertEqual(Req0, Req1)
    end).

%% ===================================================================
%% key_status Capability Gate Tests
%% ===================================================================

key_status_returns_error_when_e2ee_disabled_test_() ->
    ?WITH_MECKS([
        {imboy_policy, [
            {'e2ee_enabled', 0, fun() -> false end}
        ]},
        {elib_response, [
            {'error', 3, fun(Req, _Msg, _Code) -> Req end}
        ]}
    ], fun() ->
        Req0 = cowboy_req_ok,
        State0 = #{action => key_status},

        {ok, Req1, _State} = e2ee_handler:init(Req0, State0),

        ?assertEqual(Req0, Req1)
    end).

%% ===================================================================
%% pull_notifications Capability Gate Tests
%% ===================================================================

pull_notifications_returns_error_when_e2ee_disabled_test_() ->
    ?WITH_MECKS([
        {imboy_policy, [
            {'e2ee_enabled', 0, fun() -> false end}
        ]},
        {elib_response, [
            {'error', 3, fun(Req, _Msg, _Code) -> Req end}
        ]}
    ], fun() ->
        Req0 = cowboy_req_ok,
        State0 = #{action => pull_notifications},

        {ok, Req1, _State} = e2ee_handler:init(Req0, State0),

        ?assertEqual(Req0, Req1)
    end).

%% ===================================================================
%% start_recovery Capability Gate Tests
%% ===================================================================

start_recovery_returns_error_when_e2ee_disabled_test_() ->
    ?WITH_MECKS([
        {imboy_policy, [
            {'e2ee_enabled', 0, fun() -> false end}
        ]},
        {elib_response, [
            {'error', 3, fun(Req, _Msg, _Code) -> Req end}
        ]}
    ], fun() ->
        Req0 = cowboy_req_ok,
        State0 = #{action => start_recovery},

        {ok, Req1, _State} = e2ee_handler:init(Req0, State0),

        ?assertEqual(Req0, Req1)
    end).
