-module(group_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% group_logic 模块的 EUnit 测试
%%%
%%% 目标：验证群组业务逻辑功能
%%% 覆盖：面对面建群、群组创建、解散、附近群组、数据转换
%%%===================================================================

%% ===================================================================
%% group_transfer/1 测试
%% ===================================================================

group_transfer_returns_input_unchanged_test_() ->
    ?TEST_SIMPLE(fun() ->
        Input = #{
            <<"id">> => 1,
            <<"creator_uid">> => 100,
            <<"owner_uid">> => 100,
            <<"gid">> => 1,
            <<"name">> => <<"测试群"/utf8>>
        },
        Result = group_logic:group_transfer(Input),
        ?assertEqual(Input, Result)
    end).

group_transfer_with_empty_map_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = group_logic:group_transfer(#{}),
        ?assertEqual(#{}, Result)
    end).

%% chat_aes_key（群消息密钥列）不得随 DTO 出站：detail 等端点走
%% SELECT *，该列会随行带出；当前恒空串但未来一旦启用即为密钥泄漏。
group_transfer_strips_chat_aes_key_test_() ->
    ?TEST_SIMPLE(fun() ->
        G = #{
            <<"id">> => 1001,
            <<"name">> => <<"g">>,
            <<"chat_aes_key">> => <<"should_not_leak">>
        },
        Result = group_logic:group_transfer(G),
        ?assertNot(maps:is_key(<<"chat_aes_key">>, Result)),
        ?assertEqual(#{<<"id">> => 1001, <<"name">> => <<"g">>}, Result)
    end).

%% ===================================================================
%% face2face/4 测试
%% ===================================================================

face2face_with_empty_code_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 123,
        Code = <<>>,
        Lng = <<"116.4074">>,
        Lat = <<"39.9042">>,

        Result = group_logic:face2face(Uid, Code, Lng, Lat),
        ?assertEqual({error, <<"Code 必须"/utf8>>}, Result)
    end).

face2face_with_undefined_lng_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 123,
        Code = <<"ABC123">>,
        Lng = undefined,
        Lat = <<"39.9042">>,

        Result = group_logic:face2face(Uid, Code, Lng, Lat),
        ?assertEqual({error, <<"longitude 必须"/utf8>>}, Result)
    end).

face2face_with_undefined_lat_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 123,
        Code = <<"ABC123">>,
        Lng = <<"116.4074">>,
        Lat = undefined,

        Result = group_logic:face2face(Uid, Code, Lng, Lat),
        ?assertEqual({error, <<"latitude 必须"/utf8>>}, Result)
    end).

face2face_matches_atomically_test_() ->
    ?WITH_MECK(
        group_ds,
        [
            {face2face, 4, fun(123, <<"0123">>, 116.4074, 39.9042) -> {ok, 999} end}
        ],
        fun() ->
            ?assertEqual(
                {ok, 999}, group_logic:face2face(123, <<"0123">>, <<"116.4074">>, <<"39.9042">>)
            )
        end
    ).

face2face_storage_failure_is_error_test_() ->
    ?WITH_MECK(
        group_ds,
        [
            {face2face, 4, fun(_, _, _, _) -> {error, database_unavailable} end}
        ],
        fun() ->
            ?assertMatch(
                {error, _}, group_logic:face2face(123, <<"1234">>, <<"116.4">>, <<"39.9">>)
            )
        end
    ).

face2face_rejects_invalid_inputs_test_() ->
    [
        ?_assertMatch({error, _}, group_logic:face2face(123, Code, Lng, Lat))
     || {Code, Lng, Lat} <- [
            {<<"123">>, <<"116.4">>, <<"39.9">>},
            {<<"12345">>, <<"116.4">>, <<"39.9">>},
            {<<"12a4">>, <<"116.4">>, <<"39.9">>},
            {<<"１２３４"/utf8>>, <<"116.4">>, <<"39.9">>},
            {null, <<"116.4">>, <<"39.9">>},
            {<<"1234">>, <<"0.0">>, <<"0.0">>},
            {<<"1234">>, <<"181.0">>, <<"39.9">>},
            {<<"1234">>, <<"116.4">>, <<"91.0">>},
            {<<"1234">>, <<"NaN">>, <<"39.9">>},
            {<<"1234">>, <<"bad">>, <<"39.9">>}
        ]
    ].

face2face_save_rejects_invalid_inputs_test_() ->
    [
        ?_assertMatch({error, _}, group_logic:face2face_save(Code, Gid, 123))
     || {Code, Gid} <- [
            {<<"123">>, 999},
            {<<"1234">>, 0},
            {<<"1234">>, undefined},
            {<<"1234">>, 9223372036854775808}
        ]
    ].

%% ===================================================================
%% face2face_save/3 测试
%% ===================================================================

face2face_save_success_test_() ->
    ?WITH_MECK(
        group_ds,
        [
            {'face2face_save', 3, fun(_Code, _Gid, _Uid) -> {ok, <<"saved">>} end}
        ],
        fun() ->
            Code = <<"1234">>,
            Gid = 999,
            Uid = 123,

            Result = group_logic:face2face_save(Code, Gid, Uid),
            ?assertEqual({ok, <<"saved">>}, Result)
        end
    ).

%% ===================================================================
%% add/4 测试
%% ===================================================================

add_with_exceeding_count_limit_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        Count = 101,
        Uid = 123,
        Type = 1,
        MemberUids = [<<"1">>, <<"2">>],

        Result = group_logic:add(Count, Uid, Type, MemberUids),
        ?assertEqual({error, <<"每人最多创建100个群"/utf8>>}, Result)
    end).

add_creates_new_group_success_test_() ->
    ?WITH_MECKS(
        [
            {elib_dt, [
                {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
            ]},
            {group_ds, [
                {'create_group', 6, fun(_Conn, _Gid, _Uid, _Now, _Type, _Status) -> 999 end}
            ]},
            {user_ds, [
                {'title', 1, fun(_Uid) -> <<"user123">> end}
            ]},
            {group_member_logic, [
                {'join_group', 5, fun(_Conn, _Mode, _Uid, _Gid, _Data) -> {ok, 1} end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Fun) -> Fun(self()) end}
            ]}
        ],
        fun() ->
            Count = 5,
            Uid = 123,
            Type = 1,
            MemberUids = [<<"1">>, <<"2">>],

            Result = group_logic:add(Count, Uid, Type, MemberUids),
            ?assertMatch({ok, _Gid}, Result)
        end
    ).

%% P0 终局（决策文档 2026-08-29）：同成员集允许多群，建群无幂等去重，
%% 任何重试/重复建群都走全新创建路径（集成级双建断言见 group_user_id_sum_p0_tests B1）。
add_same_set_creates_new_group_test_() ->
    ?WITH_MECKS(
        [
            {elib_dt, [
                {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
            ]},
            {group_ds, [
                {'create_group', 6, fun(_Conn, _Gid, _Uid, _Now, _Type, _Status) -> 999 end}
            ]},
            {user_ds, [
                {'title', 1, fun(_Uid) -> <<"user123">> end}
            ]},
            {group_member_logic, [
                {'join_group', 5, fun(_Conn, _Mode, _Uid, _Gid, _Data) -> {ok, 1} end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Fun) -> Fun(self()) end}
            ]}
        ],
        fun() ->
            Count = 5,
            Uid = 123,
            Type = 1,
            MemberUids = [<<"1">>],

            Result = group_logic:add(Count, Uid, Type, MemberUids),
            ?assertEqual({ok, 999}, Result)
        end
    ).

add_member_failure_aborts_transaction_test_() ->
    ?WITH_MECKS(
        [
            {elib_dt, [{'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}]},
            {group_ds, [{'create_group', 6, fun(_, _, _, _, _, _) -> 999 end}]},
            {user_ds, [{'title', 1, fun(_) -> <<"user123">> end}]},
            %% join_group/5 的真实失败通道是事务内 throw({abort_tx,_})
            %% （资格/守卫/成员资格校验同型），与 PG 套例
            %% group_creation_recovery_pg_tests 的 workspace_membership_required
            %% 回滚路径一致；成功契约恒 {ok, _}。
            {group_member_logic, [
                {'join_group', 5, fun(_, _, _, _, _) ->
                    throw({abort_tx, simulated_member_failure})
                end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Fun) ->
                    try
                        Fun(self())
                    catch
                        throw:{abort_tx, Reason} -> {error, Reason}
                    end
                end}
            ]}
        ],
        fun() ->
            ?assertMatch({error, _}, group_logic:add(0, 123, 1, [<<"1">>]))
        end
    ).

add_with_empty_member_list_test_() ->
    ?WITH_MECKS(
        [
            {elib_dt, [
                {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
            ]},
            {group_ds, [
                {'create_group', 6, fun(_Conn, _Gid, _Uid, _Now, _Type, _Status) -> 999 end}
            ]},
            {user_ds, [
                {'title', 1, fun(_Uid) -> <<"user123">> end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Fun) -> Fun(self()) end}
            ]}
        ],
        fun() ->
            Count = 5,
            Uid = 123,
            Type = 1,
            MemberUids = [],

            Result = group_logic:add(Count, Uid, Type, MemberUids),
            ?assertMatch({ok, _Gid}, Result)
        end
    ).

add_with_invalid_member_list_test_() ->
    ?WITH_MECKS(
        [
            {elib_dt, [
                {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
            ]},
            {group_ds, [
                {'create_group', 6, fun(_Conn, _Gid, _Uid, _Now, _Type, _Status) -> 999 end}
            ]},
            {user_ds, [
                {'title', 1, fun(_Uid) -> <<"user123">> end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Fun) -> Fun(self()) end}
            ]}
        ],
        fun() ->
            Count = 5,
            Uid = 123,
            Type = 1,
            MemberUids = invalid,

            Result = group_logic:add(Count, Uid, Type, MemberUids),
            ?assertMatch({ok, _Gid}, Result)
        end
    ).

%% ===================================================================
%% dissolve/4 测试
%% ===================================================================

dissolve_success_test_() ->
    ?WITH_MECK(
        group_ds,
        [
            {'dissolve_group', 4, fun(_Uid, _Gid, _OwnerUid, _G) -> ok end}
        ],
        fun() ->
            Uid = 123,
            Gid = 999,
            OwnerUid = 123,
            G = #{<<"id">> => 999},

            Result = group_logic:dissolve(Uid, Gid, OwnerUid, G),
            ?assertEqual(ok, Result)
        end
    ).

%% ===================================================================
%% nearby_gid/6 测试
%% ===================================================================

nearby_gid_returns_groups_test_() ->
    ?WITH_MECK(
        group_ds,
        [
            {'nearby_gid', 6, fun(_Lng, _Lat, _Radius, _Unit, _Limit, _Code) ->
                {ok, [#{<<"group_id">> => 888, <<"name">> => <<"附近群"/utf8>>}]}
            end}
        ],
        fun() ->
            Lng = <<"116.4074">>,
            Lat = <<"39.9042">>,
            Radius = <<"50">>,
            Unit = <<"m">>,
            Limit = <<"1">>,
            Code = <<"ABC123">>,

            Result = group_logic:nearby_gid(Lng, Lat, Radius, Unit, Limit, Code),
            ?assertMatch({ok, [_ | _]}, Result)
        end
    ).

nearby_gid_with_no_results_test_() ->
    ?WITH_MECK(
        group_ds,
        [
            {'nearby_gid', 6, fun(_Lng, _Lat, _Radius, _Unit, _Limit, _Code) ->
                {ok, []}
            end}
        ],
        fun() ->
            Lng = <<"116.4074">>,
            Lat = <<"39.9042">>,
            Radius = <<"50">>,
            Unit = <<"m">>,
            Limit = <<"1">>,
            Code = <<"XYZ789">>,

            Result = group_logic:nearby_gid(Lng, Lat, Radius, Unit, Limit, Code),
            ?assertEqual({ok, []}, Result)
        end
    ).

%% ===================================================================
%% 群级 E2EE 开启入口（全局硬闸 guard）
%% ===================================================================

%% 全局硬闸（storage_mode=disabled）下必须拒绝开启群 E2EE。
%% 放行会造出"全局关着、这个群开着"的裂缝：该群成员的设备会按群标志继续加密，
%% 而密钥端点已随硬闸关闭，群消息必然发不出去；且群 E2EE 在 API 上只能 0→1，
%% 事后无法从客户端回退。
%% 断言"未触达 group_member_ds"是刻意的：拦截必须在查库/查角色之前完成，
%% 否则硬闸部署下这个入口会带上一次无意义的 PG 依赖。
set_e2ee_mode_refused_under_global_hard_off_test_() ->
    ?WITH_MECKS(
        [
            {imboy_policy, [{'e2ee_disabled', 0, fun() -> true end}]},
            {group_member_ds, [
                {'find_by_gid_and_uid', 3, fun(_, _, _) ->
                    erlang:error(unexpected_group_member_lookup)
                end}
            ]}
        ],
        fun() ->
            ?assertMatch({error, _}, group_logic:set_e2ee_mode(1001, 88, 1))
        end
    ).

%% 对照组：非硬闸部署维持既有语义（0→1 单向拒绝不变），证明上面的拒绝来自硬闸
set_e2ee_mode_one_way_error_kept_without_hard_off_test_() ->
    ?WITH_MECKS(
        [{imboy_policy, [{'e2ee_disabled', 0, fun() -> false end}]}],
        fun() ->
            ?assertEqual(
                {error, <<"e2ee_mode 仅支持单向开启（0→1）"/utf8>>},
                group_logic:set_e2ee_mode(1001, 88, 0)
            )
        end
    ).

%% ===================================================================
%% transfer/3 群转让通知链（Q05/F07/AC10）
%% ===================================================================

%% 转让事务成功前持久入队，提交后不能再经总线产生另一份随机消息身份。
%% 修复前该场景事务成功后无任何通知（设计文档 Q05 §1.1 唯一"零通知"缺口）。
transfer_success_stages_owner_transferred_test_() ->
    ?WITH_MECKS(
        [
            {group_ds, [
                {'find_by_id', 2, fun(1, _Cols) -> #{<<"owner_uid">> => 100} end},
                {'update_owner_tx', 4, fun(_Conn, 1, 200, 100) -> ok end}
            ]},
            {group_member_ds, [
                {'find_by_gid_and_uid', 3, fun(1, 200, _Col) -> #{<<"id">> => 9} end},
                {'update_active_role_tx', 4, fun(_Conn, 1, _Uid, _Role) -> ok end}
            ]},
            {workspace_guard, [
                {'ensure_writable_tx', 2, fun(_Conn, _Target) -> ok end}
            ]},
            {group_log_ds, [
                {'add', 2, fun(_Conn, _Data) -> {ok, 1} end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Fun) -> Fun(self()) end}
            ]},
            {group_event_handler, [
                {'stage_tx', 3, fun(_Conn, Event, EffectId) ->
                    ?assertEqual({owner_transferred, 1, 100, 200}, Event),
                    ?assertEqual(1, EffectId),
                    ok
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(ok, group_logic:transfer(100, 1, 200)),
            ?assertEqual(1, meck:num_calls(group_event_handler, stage_tx, 3))
        end
    ).

%% 转让事务失败 → 不 publish（通知只随成功提交；AC10 事务回滚变体）。
transfer_failure_does_not_publish_test_() ->
    ?WITH_MECKS(
        [
            {group_ds, [
                {'find_by_id', 2, fun(1, _Cols) -> #{<<"owner_uid">> => 100} end},
                {'update_owner_tx', 4, fun(_Conn, _Gid, _New, _Expected) -> {error, db_down} end}
            ]},
            {group_member_ds, [
                {'find_by_gid_and_uid', 3, fun(1, 200, _Col) -> #{<<"id">> => 9} end}
            ]},
            {workspace_guard, [
                {'ensure_writable_tx', 2, fun(_Conn, _Target) -> ok end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Fun) -> Fun(self()) end}
            ]},
            {imboy_domain_event, [
                {'publish', 1, fun(_Events) -> ok end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"db_down">>},
                group_logic:transfer(100, 1, 200)
            ),
            ?assertEqual(0, meck:num_calls(imboy_domain_event, publish, 1))
        end
    ).
