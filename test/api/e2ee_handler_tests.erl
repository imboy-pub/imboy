-module(e2ee_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% e2ee_handler 模块的 EUnit 测试
%%%
%%% 目标：验证端到端加密 API 功能
%%% 覆盖：用户公钥获取、群成员公钥获取、参数验证
%%%===================================================================

%% ===================================================================
%% init/2 测试
%% ===================================================================

init_with_user_keys_action_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'get', 3, fun(_Key, _Req, _Default) -> <<"value">> end}
        ]},
        {auth_ds, [
            {'current_uid', 1, fun(_State) -> 123 end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"value">>) -> 456 end}
        ]},
        {e2ee_logic, [
            {'user_keys', 2, fun(_CurrentUid, _TargetUid) ->
                {ok, #{<<"uid">> => <<"encoded_456">>, <<"devices">> => []}}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, _Payload) -> cowboy_req_ok end}
        ]}
    ], fun() ->
        Req0 = cowboy_req_ok,
        State0 = #{action => user_keys},

        {ok, _Req1, State} = e2ee_handler:init(Req0, State0),
        ?assert(is_map(State))
    end).

init_with_group_member_keys_action_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'get', 3, fun(_Key, _Req, _Default) -> <<"value">> end}
        ]},
        {auth_ds, [
            {'current_uid', 1, fun(_State) -> 123 end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"value">>) -> 1 end}
        ]},
        {e2ee_logic, [
            {'group_member_keys', 2, fun(_CurrentUid, _Gid) ->
                {ok, #{<<"gid">> => <<"encoded_1">>, <<"members">> => []}}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, _Payload) -> cowboy_req_ok end}
        ]}
    ], fun() ->
        Req0 = cowboy_req_ok,
        State0 = #{action => group_member_keys},

        {ok, _Req1, State} = e2ee_handler:init(Req0, State0),
        ?assert(is_map(State))
    end).

init_with_unknown_action_returns_404_test_() ->
    ?WITH_MECK(elib_response, [
        {'error', 3, fun(_Req, _Msg, 404) -> cowboy_req_404 end}
    ], fun() ->
        Req0 = cowboy_req_ok,
        State0 = #{action => unknown_action},

        {ok, Req1, _State} = e2ee_handler:init(Req0, State0),
        ?assertEqual(cowboy_req_404, Req1)
    end).

%% ===================================================================
%% user_keys/2 测试
%% ===================================================================

user_keys_with_valid_uid_returns_success_test_() ->
    ?WITH_MECKS([
        {auth_ds, [
            {'current_uid', 1, fun(_State) -> 123 end}
        ]},
        {elib_param, [
            {'get', 3, fun(<<"uid">>, _Req, _Default) -> <<"encoded_456">> end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"encoded_456">>) -> 456 end}
        ]},
        {e2ee_logic, [
            {'user_keys', 2, fun(_CurrentUid, _TargetUid) ->
                {ok, #{<<"uid">> => <<"encoded_456">>, <<"devices">> => []}}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, _Payload) -> cowboy_req_ok end}
        ]}
    ], fun() ->
        Req0 = cowboy_req_ok,
        State = #{},

        Result = e2ee_handler:user_keys(Req0, State),
        ?assertEqual(cowboy_req_ok, Result)
    end).

user_keys_with_invalid_uid_returns_400_test_() ->
    ?WITH_MECKS([
        {auth_ds, [
            {'current_uid', 1, fun(_State) -> 123 end}
        ]},
        {elib_param, [
            {'get', 3, fun(<<"uid">>, _Req, _Default) -> <<"invalid_uid">> end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"invalid_uid">>) -> undefined end}
        ]},
        {elib_response, [
            {'error', 3, fun(_Req, _Msg, 400) -> cowboy_req_400 end}
        ]}
    ], fun() ->
        Req0 = cowboy_req_ok,
        State = #{},

        Result = e2ee_handler:user_keys(Req0, State),
        ?assertEqual(cowboy_req_400, Result)
    end).

user_keys_with_forbidden_returns_403_test_() ->
    ?WITH_MECKS([
        {auth_ds, [
            {'current_uid', 1, fun(_State) -> 123 end}
        ]},
        {elib_param, [
            {'get', 3, fun(<<"uid">>, _Req, _Default) -> <<"encoded_789">> end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"encoded_789">>) -> 789 end}
        ]},
        {e2ee_logic, [
            {'user_keys', 2, fun(_CurrentUid, _TargetUid) ->
                {error, <<"forbidden">>, 403}
            end}
        ]},
        {elib_response, [
            {'error', 3, fun(_Req, _Msg, 403) -> cowboy_req_403 end}
        ]}
    ], fun() ->
        Req0 = cowboy_req_ok,
        State = #{},

        Result = e2ee_handler:user_keys(Req0, State),
        ?assertEqual(cowboy_req_403, Result)
    end).

user_keys_with_internal_error_returns_500_test_() ->
    ?WITH_MECKS([
        {auth_ds, [
            {'current_uid', 1, fun(_State) -> 123 end}
        ]},
        {elib_param, [
            {'get', 3, fun(<<"uid">>, _Req, _Default) -> <<"encoded_456">> end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"encoded_456">>) -> 456 end}
        ]},
        {e2ee_logic, [
            {'user_keys', 2, fun(_CurrentUid, _TargetUid) ->
                {error, <<"internal_error">>, 500}
            end}
        ]},
        {elib_response, [
            {'error', 3, fun(_Req, _Msg, 500) -> cowboy_req_500 end}
        ]}
    ], fun() ->
        Req0 = cowboy_req_ok,
        State = #{},

        Result = e2ee_handler:user_keys(Req0, State),
        ?assertEqual(cowboy_req_500, Result)
    end).

%% ===================================================================
%% group_member_keys/2 测试
%% ===================================================================

group_member_keys_with_valid_gid_returns_success_test_() ->
    ?WITH_MECKS([
        {auth_ds, [
            {'current_uid', 1, fun(_State) -> 123 end}
        ]},
        {elib_param, [
            {'get', 3, fun(<<"gid">>, _Req, _Default) -> <<"encoded_1">> end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"encoded_1">>) -> 1 end}
        ]},
        {e2ee_logic, [
            {'group_member_keys', 2, fun(_CurrentUid, _Gid) ->
                {ok, #{
                    <<"gid">> => <<"encoded_1">>,
                    <<"members">> => [
                        #{<<"uid">> => <<"encoded_123">>, <<"devices">> => []}
                    ]
                }}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, _Payload) -> cowboy_req_ok end}
        ]}
    ], fun() ->
        Req0 = cowboy_req_ok,
        State = #{},

        Result = e2ee_handler:group_member_keys(Req0, State),
        ?assertEqual(cowboy_req_ok, Result)
    end).

group_member_keys_with_invalid_gid_returns_400_test_() ->
    ?WITH_MECKS([
        {auth_ds, [
            {'current_uid', 1, fun(_State) -> 123 end}
        ]},
        {elib_param, [
            {'get', 3, fun(<<"gid">>, _Req, _Default) -> <<"invalid_gid">> end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"invalid_gid">>) -> undefined end}
        ]},
        {elib_response, [
            {'error', 3, fun(_Req, _Msg, 400) -> cowboy_req_400 end}
        ]}
    ], fun() ->
        Req0 = cowboy_req_ok,
        State = #{},

        Result = e2ee_handler:group_member_keys(Req0, State),
        ?assertEqual(cowboy_req_400, Result)
    end).

group_member_keys_with_non_member_returns_403_test_() ->
    ?WITH_MECKS([
        {auth_ds, [
            {'current_uid', 1, fun(_State) -> 123 end}
        ]},
        {elib_param, [
            {'get', 3, fun(<<"gid">>, _Req, _Default) -> <<"encoded_999">> end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"encoded_999">>) -> 999 end}
        ]},
        {e2ee_logic, [
            {'group_member_keys', 2, fun(_CurrentUid, _Gid) ->
                {error, <<"forbidden">>, 403}
            end}
        ]},
        {elib_response, [
            {'error', 3, fun(_Req, _Msg, 403) -> cowboy_req_403 end}
        ]}
    ], fun() ->
        Req0 = cowboy_req_ok,
        State = #{},

        Result = e2ee_handler:group_member_keys(Req0, State),
        ?assertEqual(cowboy_req_403, Result)
    end).

group_member_keys_with_database_error_returns_500_test_() ->
    ?WITH_MECKS([
        {auth_ds, [
            {'current_uid', 1, fun(_State) -> 123 end}
        ]},
        {elib_param, [
            {'get', 3, fun(<<"gid">>, _Req, _Default) -> <<"encoded_1">> end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"encoded_1">>) -> 1 end}
        ]},
        {e2ee_logic, [
            {'group_member_keys', 2, fun(_CurrentUid, _Gid) ->
                {error, <<"internal_error">>, 500}
            end}
        ]},
        {elib_response, [
            {'error', 3, fun(_Req, _Msg, 500) -> cowboy_req_500 end}
        ]}
    ], fun() ->
        Req0 = cowboy_req_ok,
        State = #{},

        Result = e2ee_handler:group_member_keys(Req0, State),
        ?assertEqual(cowboy_req_500, Result)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

user_keys_with_same_user_test_() ->
    ?WITH_MECKS([
        {auth_ds, [
            {'current_uid', 1, fun(_State) -> 123 end}
        ]},
        {elib_param, [
            {'get', 3, fun(<<"uid">>, _Req, _Default) -> <<"encoded_123">> end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"encoded_123">>) -> 123 end}
        ]},
        {e2ee_logic, [
            {'user_keys', 2, fun(_CurrentUid, _TargetUid) ->
                {ok, #{<<"uid">> => <<"encoded_123">>, <<"devices">> => []}}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, _Payload) -> cowboy_req_ok end}
        ]}
    ], fun() ->
        Req0 = cowboy_req_ok,
        State = #{},

        Result = e2ee_handler:user_keys(Req0, State),
        ?assertEqual(cowboy_req_ok, Result)
    end).

group_member_keys_with_empty_group_test_() ->
    ?WITH_MECKS([
        {auth_ds, [
            {'current_uid', 1, fun(_State) -> 123 end}
        ]},
        {elib_param, [
            {'get', 3, fun(<<"gid">>, _Req, _Default) -> <<"encoded_1">> end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"encoded_1">>) -> 1 end}
        ]},
        {e2ee_logic, [
            {'group_member_keys', 2, fun(_CurrentUid, _Gid) ->
                {ok, #{<<"gid">> => <<"encoded_1">>, <<"members">> => []}}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, _Payload) -> cowboy_req_ok end}
        ]}
    ], fun() ->
        Req0 = cowboy_req_ok,
        State = #{},

        Result = e2ee_handler:group_member_keys(Req0, State),
        ?assertEqual(cowboy_req_ok, Result)
    end).

user_keys_with_zero_uid_returns_400_test_() ->
    ?WITH_MECKS([
        {auth_ds, [
            {'current_uid', 1, fun(_State) -> 123 end}
        ]},
        {elib_param, [
            {'get', 3, fun(<<"uid">>, _Req, _Default) -> <<"encoded_0">> end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"encoded_0">>) -> 0 end}
        ]},
        {elib_response, [
            {'error', 3, fun(_Req, _Msg, 400) -> cowboy_req_400 end}
        ]}
    ], fun() ->
        Req0 = cowboy_req_ok,
        State = #{},

        Result = e2ee_handler:user_keys(Req0, State),
        ?assertEqual(cowboy_req_400, Result)
    end).

user_keys_with_negative_uid_returns_400_test_() ->
    ?WITH_MECKS([
        {auth_ds, [
            {'current_uid', 1, fun(_State) -> 123 end}
        ]},
        {elib_param, [
            {'get', 3, fun(<<"uid">>, _Req, _Default) -> <<"encoded_invalid">> end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"encoded_invalid">>) -> -1 end}
        ]},
        {elib_response, [
            {'error', 3, fun(_Req, _Msg, 400) -> cowboy_req_400 end}
        ]}
    ], fun() ->
        Req0 = cowboy_req_ok,
        State = #{},

        Result = e2ee_handler:user_keys(Req0, State),
        ?assertEqual(cowboy_req_400, Result)
    end).

group_member_keys_with_zero_gid_returns_400_test_() ->
    ?WITH_MECKS([
        {auth_ds, [
            {'current_uid', 1, fun(_State) -> 123 end}
        ]},
        {elib_param, [
            {'get', 3, fun(<<"gid">>, _Req, _Default) -> <<"encoded_0">> end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"encoded_0">>) -> 0 end}
        ]},
        {elib_response, [
            {'error', 3, fun(_Req, _Msg, 400) -> cowboy_req_400 end}
        ]}
    ], fun() ->
        Req0 = cowboy_req_ok,
        State = #{},

        Result = e2ee_handler:group_member_keys(Req0, State),
        ?assertEqual(cowboy_req_400, Result)
    end).
