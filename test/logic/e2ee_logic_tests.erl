-module(e2ee_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% e2ee_logic 模块的 EUnit 测试
%%%
%%% 目标：验证端到端加密密钥管理功能
%%% 覆盖：用户公钥获取、群成员公钥获取、权限验证
%%%===================================================================

%% ===================================================================
%% user_keys/2 测试
%% ===================================================================

user_keys_same_user_success_test_() ->
    ?WITH_MECK(user_device_ds, [
        {'list_public_keys', 1, fun(_Uid) ->
            {ok, [
                #{
                    <<"device_id">> => <<"device_1">>,
                    <<"public_key">> => <<"public_key_1">>
                },
                #{
                    <<"device_id">> => <<"device_2">>,
                    <<"public_key">> => <<"public_key_2">>
                }
            ]}
        end}
    ], fun() ->
        ?WITH_MECK(elib_hashids, [
            {'encode', 1, fun(123) -> <<"encoded_123">> end}
        ], fun() ->
            CurrentUid = 123,
            TargetUid = 123,  % 同一个用户

            Result = e2ee_logic:user_keys(CurrentUid, TargetUid),
            ?assertMatch({ok, #{
                <<"uid">> := <<"encoded_123">>,
                <<"devices">> := _
            }}, Result)
        end)
    end).

user_keys_friend_success_test_() ->
    ?WITH_MECK(user_device_ds, [
        {'list_public_keys', 1, fun(_Uid) ->
            {ok, [
                #{
                    <<"device_id">> => <<"device_1">>,
                    <<"public_key">> => <<"MIIBIjANBgkqhkiG9w...">>
                }
            ]}
        end}
    ], fun() ->
        ?WITH_MECK(elib_hashids, [
            {'encode', 1, fun(456) -> <<"encoded_456">> end}
        ], fun() ->
            CurrentUid = 123,
            TargetUid = 456,  % 好友

            Result = e2ee_logic:user_keys(CurrentUid, TargetUid),
            ?assertMatch({ok, #{
                <<"uid">> := <<"encoded_456">>,
                <<"devices">> := [_]
            }}, Result)
        end)
    end).

user_keys_non_friend_still_returns_public_keys_test_() ->
    ?WITH_MECK(user_device_ds, [
        {'list_public_keys', 1, fun(_Uid) ->
            {ok, [
                #{
                    <<"device_id">> => <<"device_public">>,
                    <<"public_key">> => <<"public_key_non_friend">>
                }
            ]}
        end}
    ], fun() ->
        ?WITH_MECK(elib_hashids, [
            {'encode', 1, fun(789) -> <<"encoded_789">> end}
        ], fun() ->
            CurrentUid = 123,
            TargetUid = 789,  % 非好友

            Result = e2ee_logic:user_keys(CurrentUid, TargetUid),
            ?assertMatch({ok, #{
                <<"uid">> := <<"encoded_789">>,
                <<"devices">> := [_]
            }}, Result)
        end)
    end).

user_keys_in_denylist_still_returns_public_keys_test_() ->
    ?WITH_MECK(user_device_ds, [
        {'list_public_keys', 1, fun(_Uid) ->
            {ok, [
                #{
                    <<"device_id">> => <<"device_denylist">>,
                    <<"public_key">> => <<"public_key_denylist">>
                }
            ]}
        end}
    ], fun() ->
        ?WITH_MECK(elib_hashids, [
            {'encode', 1, fun(999) -> <<"encoded_999">> end}
        ], fun() ->
            CurrentUid = 123,
            TargetUid = 999,  % 在黑名单

            Result = e2ee_logic:user_keys(CurrentUid, TargetUid),
            ?assertMatch({ok, #{
                <<"uid">> := <<"encoded_999">>,
                <<"devices">> := [_]
            }}, Result)
        end)
    end).

user_keys_database_error_returns_500_test_() ->
    ?WITH_MECK(user_device_ds, [
        {'list_public_keys', 1, fun(_Uid) ->
            {error, database_connection_lost}
        end}
    ], fun() ->
        CurrentUid = 123,
        TargetUid = 123,  % 同一个用户

        Result = e2ee_logic:user_keys(CurrentUid, TargetUid),
        ?assertEqual({error, <<"internal_error">>, 500}, Result)
    end).

user_keys_with_multiple_devices_test_() ->
    ?WITH_MECK(user_device_ds, [
        {'list_public_keys', 1, fun(_Uid) ->
            {ok, [
                #{
                    <<"device_id">> => <<"ios_device">>,
                    <<"public_key">> => <<"ios_public_key">>,
                    <<"device_type">> => <<"ios">>
                },
                #{
                    <<"device_id">> => <<"android_device">>,
                    <<"public_key">> => <<"android_public_key">>,
                    <<"device_type">> => <<"android">>
                },
                #{
                    <<"device_id">> => <<"web_device">>,
                    <<"public_key">> => <<"web_public_key">>,
                    <<"device_type">> => <<"web">>
                }
            ]}
        end}
    ], fun() ->
        ?WITH_MECK(elib_hashids, [
            {'encode', 1, fun(123) -> <<"encoded_123">> end}
        ], fun() ->
            CurrentUid = 123,
            TargetUid = 123,

            Result = e2ee_logic:user_keys(CurrentUid, TargetUid),
            ?assertMatch({ok, #{
                <<"uid">> := <<"encoded_123">>,
                <<"devices">> := _
            }}, Result),
            {ok, #{<<"devices">> := Devices}} = Result,
            ?assertEqual(3, length(Devices))
        end)
    end).

%% ===================================================================
%% group_member_keys/2 测试
%% ===================================================================

group_member_keys_member_success_test_() ->
    ?WITH_MECK(group_ds, [
        {'is_member', 2, fun(_CurrentUid, _Gid) -> true end},
        {'member_uids', 1, fun(_Gid) -> [123, 456, 789] end}
    ], fun() ->
        ?WITH_MECK(user_device_ds, [
            {'list_public_keys_by_uids', 1, fun(_Uids) ->
                {ok, [
                    #{
                        <<"user_id">> => 123,
                        <<"device_id">> => <<"device_1">>,
                        <<"public_key">> => <<"key_1">>
                    },
                    #{
                        <<"user_id">> => 456,
                        <<"device_id">> => <<"device_2">>,
                        <<"public_key">> => <<"key_2">>
                    },
                    #{
                        <<"user_id">> => 789,
                        <<"device_id">> => <<"device_3">>,
                        <<"public_key">> => <<"key_3">>
                    }
                ]}
            end}
        ], fun() ->
            ?WITH_MECK(elib_hashids, [
                {'encode', 1, fun
                    (1) -> <<"encoded_1">>;
                    (123) -> <<"encoded_123">>;
                    (456) -> <<"encoded_456">>;
                    (789) -> <<"encoded_789">>
                end}
            ], fun() ->
                CurrentUid = 123,
                Gid = 1,

                Result = e2ee_logic:group_member_keys(CurrentUid, Gid),
                ?assertMatch({ok, #{
                    <<"gid">> := <<"encoded_1">>,
                    <<"members">> := _
                }}, Result),
                {ok, #{<<"members">> := Members}} = Result,
                ?assertEqual(3, length(Members))
            end)
        end)
    end).

group_member_keys_non_member_forbidden_test_() ->
    ?WITH_MECK(group_ds, [
        {'is_member', 2, fun(_CurrentUid, _Gid) -> false end}
    ], fun() ->
        CurrentUid = 123,
        Gid = 999,  % 不是群成员

        Result = e2ee_logic:group_member_keys(CurrentUid, Gid),
        ?assertEqual({error, <<"forbidden">>, 403}, Result)
    end).

group_member_keys_database_error_returns_500_test_() ->
    ?WITH_MECK(group_ds, [
        {'is_member', 2, fun(_CurrentUid, _Gid) -> true end},
        {'member_uids', 1, fun(_Gid) -> [123, 456] end}
    ], fun() ->
        ?WITH_MECK(user_device_ds, [
            {'list_public_keys_by_uids', 1, fun(_Uids) ->
                {error, database_timeout}
            end}
        ], fun() ->
            CurrentUid = 123,
            Gid = 1,

            Result = e2ee_logic:group_member_keys(CurrentUid, Gid),
            ?assertEqual({error, <<"internal_error">>, 500}, Result)
        end)
    end).

group_member_keys_empty_group_test_() ->
    ?WITH_MECK(group_ds, [
        {'is_member', 2, fun(_CurrentUid, _Gid) -> true end},
        {'member_uids', 1, fun(_Gid) -> [] end}
    ], fun() ->
        ?WITH_MECK(user_device_ds, [
            {'list_public_keys_by_uids', 1, fun(_Uids) ->
                {ok, []}
            end}
        ], fun() ->
            ?WITH_MECK(elib_hashids, [
                {'encode', 1, fun(1) -> <<"encoded_1">> end}
            ], fun() ->
                CurrentUid = 123,
                Gid = 1,

                Result = e2ee_logic:group_member_keys(CurrentUid, Gid),
                ?assertMatch({ok, #{
                    <<"gid">> := <<"encoded_1">>,
                    <<"members">> := []
                }}, Result)
            end)
        end)
    end).

%% ===================================================================
%% group_by_uid/1 测试
%% ===================================================================

group_by_uid_single_device_test_() ->
    Input = [
        #{
            <<"user_id">> => 123,
            <<"device_id">> => <<"device_1">>,
            <<"public_key">> => <<"key_1">>
        }
    ],
    ?WITH_MECK(elib_hashids, [
        {'encode', 1, fun(123) -> <<"encoded_123">> end}
    ], fun() ->
        Result = e2ee_logic:group_by_uid(Input),
        ?assertMatch([#{<<"uid">> := <<"encoded_123">>, <<"devices">> := [_]}], Result),
        [#{<<"devices">> := [Device | _]}] = Result,
        ?assertEqual(<<"device_1">>, maps:get(<<"device_id">>, Device)),
        ?assertEqual(<<"key_1">>, maps:get(<<"public_key">>, Device))
    end).

group_by_uid_multiple_devices_same_user_test_() ->
    Input = [
        #{
            <<"user_id">> => 123,
            <<"device_id">> => <<"device_1">>,
            <<"public_key">> => <<"key_1">>
        },
        #{
            <<"user_id">> => 123,
            <<"device_id">> => <<"device_2">>,
            <<"public_key">> => <<"key_2">>
        }
    ],
    ?WITH_MECK(elib_hashids, [
        {'encode', 1, fun(123) -> <<"encoded_123">> end}
    ], fun() ->
        Result = e2ee_logic:group_by_uid(Input),
        ?assertMatch([
            #{
                <<"uid">> := <<"encoded_123">>,
                <<"devices">> := [_, _]
            }
        ], Result),
        [#{<<"devices">> := Devices}] = Result,
        ?assertEqual(2, length(Devices))
    end).

group_by_uid_multiple_users_test_() ->
    Input = [
        #{
            <<"user_id">> => 123,
            <<"device_id">> => <<"device_1">>,
            <<"public_key">> => <<"key_1">>
        },
        #{
            <<"user_id">> => 456,
            <<"device_id">> => <<"device_2">>,
            <<"public_key">> => <<"key_2">>
        }
    ],
    ?WITH_MECK(elib_hashids, [
        {'encode', 1, fun
            (123) -> <<"encoded_123">>;
            (456) -> <<"encoded_456">>
        end}
    ], fun() ->
        Result = e2ee_logic:group_by_uid(Input),
        ?assertMatch([_, _], Result),
        ?assertEqual(2, length(Result))
    end).

group_by_uid_removes_user_id_field_test_() ->
    Input = [
        #{
            <<"user_id">> => 123,
            <<"device_id">> => <<"device_1">>,
            <<"public_key">> => <<"key_1">>
        }
    ],
    ?WITH_MECK(elib_hashids, [
        {'encode', 1, fun(123) -> <<"encoded_123">> end}
    ], fun() ->
        Result = e2ee_logic:group_by_uid(Input),
        [#{<<"devices">> := [Device | _]}] = Result,
        % user_id 字段应被移除
        ?assertEqual(undefined, maps:get(<<"user_id">>, Device, undefined))
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

user_keys_with_no_devices_test_() ->
    ?WITH_MECK(user_device_ds, [
        {'list_public_keys', 1, fun(_Uid) ->
            {ok, []}
        end}
    ], fun() ->
        ?WITH_MECK(elib_hashids, [
            {'encode', 1, fun(123) -> <<"encoded_123">> end}
        ], fun() ->
            CurrentUid = 123,
            TargetUid = 123,

            Result = e2ee_logic:user_keys(CurrentUid, TargetUid),
            ?assertMatch({ok, #{
                <<"uid">> := <<"encoded_123">>,
                <<"devices">> := []
            }}, Result)
        end)
    end).

group_member_keys_sorts_by_uid_test_() ->
    ?WITH_MECK(group_ds, [
        {'is_member', 2, fun(_CurrentUid, _Gid) -> true end},
        {'member_uids', 1, fun(_Gid) -> [789, 123, 456] end}
    ], fun() ->
        ?WITH_MECK(user_device_ds, [
            {'list_public_keys_by_uids', 1, fun(_Uids) ->
                {ok, [
                    #{
                        <<"user_id">> => 789,
                        <<"device_id">> => <<"device_3">>,
                        <<"public_key">> => <<"key_3">>
                    },
                    #{
                        <<"user_id">> => 123,
                        <<"device_id">> => <<"device_1">>,
                        <<"public_key">> => <<"key_1">>
                    },
                    #{
                        <<"user_id">> => 456,
                        <<"device_id">> => <<"device_2">>,
                        <<"public_key">> => <<"key_2">>
                    }
                ]}
            end}
        ], fun() ->
            ?WITH_MECK(elib_hashids, [
                {'encode', 1, fun
                    (1) -> <<"encoded_1">>;
                    (123) -> <<"encoded_123">>;
                    (456) -> <<"encoded_456">>;
                    (789) -> <<"encoded_789">>
                end}
            ], fun() ->
                CurrentUid = 123,
                Gid = 1,

                Result = e2ee_logic:group_member_keys(CurrentUid, Gid),
                {ok, #{<<"members">> := Members}} = Result,
                % 验证成员按 UID 排序
                Uids = [maps:get(<<"uid">>, M) || M <- Members],
                ?assert(Uids =:= lists:sort(Uids))
            end)
        end)
    end).
