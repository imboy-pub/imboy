%% moya_invite_logic_tests
%% W3：老师邀请码 → 家长加入班级 —— logic 层契约测试。
%%
%% 直接模块调用（模式照 moya_learner_bind_handler_tests 的 logic 段）：
%%   create_or_get_code —— 守卫矩阵（班不存在/staff 放行/owner 兜底/
%%                          双拒）+ upsert 接线 + repo 错误折叠 db_error
%%   invite_info        —— 码校验统一折叠（不存在/revoked/过期 →
%%                          not_found，不区分细节）+ learners 透传
%%   join               —— 事务内：码校验 / 学员不在班 / already_joined
%%                          幂等不插 / joined 插入接线
%%
%% Mock 纪律：同一模块的全部期望合并为单条目（meck_helper:setup_mock
%% 对同模块二次 setup 会先 unload 再装，先装的期望会被清掉）。
%%
%% 日志纪律说明：code 仅指纹、display_name 不入日志——日志内容不做文本
%% 断言，靠 logic 源码纪律与评审锚定。
%%
%% 全部 meck，零真实网络、零真实库。

-module(moya_invite_logic_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(UID, 990101).
-define(ORG_ID, 111001).
-define(GROUP_ID, 771001).
-define(LEARNER_ID, 994101).
-define(CODE, <<"7Q3NB1M5KZ">>).
-define(TEST_CONN, invite_test_conn).

%%%===================================================================
%%% create_or_get_code：守卫矩阵
%%%===================================================================

%% 班不存在（含机构解析失败的群）→ not_found；upsert 不得被调
create_code_group_not_found_test_() ->
    ?WITH_MECKS(
        create_code_mocks(
            {ok, staff_row()},
            {error, not_owner},
            {ok, ?CODE},
            fun(?GROUP_ID) -> {ok, undefined} end
        ),
        fun() ->
            ?assertEqual(
                {error, not_found},
                moya_invite_logic:create_or_get_code(?UID, ?GROUP_ID)
            ),
            receive
                {upsert_called, _, _} -> ?assert(false, "upsert must not run for missing group")
            after 0 -> ok
            end
        end
    ).

%% 非 staff 非 owner → not_authorized；upsert 不得被调
create_code_not_authorized_test_() ->
    ?WITH_MECKS(
        create_code_mocks(
            {error, not_staff},
            {error, not_owner},
            {ok, ?CODE},
            fun(?GROUP_ID) -> {ok, brief_row()} end
        ),
        fun() ->
            ?assertEqual(
                {error, not_authorized},
                moya_invite_logic:create_or_get_code(?UID, ?GROUP_ID)
            ),
            receive
                {upsert_called, _, _} -> ?assert(false, "upsert must not run when denied")
            after 0 -> ok
            end
        end
    ).

%% 该班 active class_staff（任一教学角色）→ 放行，upsert 被调
create_code_staff_ok_test_() ->
    ?WITH_MECKS(
        create_code_mocks(
            {ok, staff_row()},
            {error, not_owner},
            {ok, ?CODE},
            fun(?GROUP_ID) -> {ok, brief_row()} end
        ),
        fun() ->
            ?assertEqual(
                {ok, ?CODE},
                moya_invite_logic:create_or_get_code(?UID, ?GROUP_ID)
            ),
            receive
                {upsert_called, ?GROUP_ID, ?UID} -> ok
            after 0 -> ?assert(false, "upsert not called with (group, uid)")
            end
        end
    ).

%% staff 行 inactive（已离职）但为 org owner → owner 兜底放行
create_code_owner_fallback_ok_test_() ->
    ?WITH_MECKS(
        create_code_mocks(
            {error, inactive},
            ok,
            {ok, ?CODE},
            fun(?GROUP_ID) -> {ok, brief_row()} end
        ),
        fun() ->
            ?assertEqual(
                {ok, ?CODE},
                moya_invite_logic:create_or_get_code(?UID, ?GROUP_ID)
            ),
            receive
                {upsert_called, ?GROUP_ID, ?UID} -> ok
            after 0 -> ?assert(false, "upsert not called on owner path")
            end
        end
    ).

%% 守卫通过但 repo 写入失败 → 折叠 db_error（epgsql 原始错误不外泄）
create_code_upsert_db_error_test_() ->
    ?WITH_MECKS(
        create_code_mocks(
            {ok, staff_row()},
            {error, not_owner},
            {error, epgsql_down},
            fun(?GROUP_ID) -> {ok, brief_row()} end
        ),
        fun() ->
            ?assertEqual(
                {error, db_error},
                moya_invite_logic:create_or_get_code(?UID, ?GROUP_ID)
            )
        end
    ).

%%%===================================================================
%%% invite_info：码校验统一折叠 + learners 透传
%%%===================================================================

invite_info_code_not_found_test_() ->
    ?WITH_MECKS(
        [
            {moya_invite_repo, [
                {'find_code', 1, fun(?CODE) -> {ok, undefined} end}
            ]}
        ],
        fun() ->
            ?assertEqual({error, not_found}, moya_invite_logic:invite_info(?CODE))
        end
    ).

%% revoked 与 expired 折叠为 not_found（防探测：响应不区分细节）
invite_info_revoked_folds_to_not_found_test_() ->
    ?WITH_MECKS(
        [
            {moya_invite_repo, [
                {'find_code', 1, fun(?CODE) ->
                    {ok, (active_code_row())#{<<"status">> => <<"revoked">>}}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual({error, not_found}, moya_invite_logic:invite_info(?CODE))
        end
    ).

invite_info_expired_folds_to_not_found_test_() ->
    ?WITH_MECKS(
        [
            {moya_invite_repo, [
                {'find_code', 1, fun(?CODE) ->
                    {ok, (active_code_row())#{<<"expired">> => true}}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual({error, not_found}, moya_invite_logic:invite_info(?CODE))
        end
    ).

%% 码 active 且未过期（expired=NULL 不算过期）→ brief + learners 透传
invite_info_ok_test_() ->
    Learners = [
        #{<<"id">> => 994102, <<"display_name">> => <<"陈小二"/utf8>>},
        #{<<"id">> => ?LEARNER_ID, <<"display_name">> => <<"张小一"/utf8>>}
    ],
    ?WITH_MECKS(
        [
            {moya_invite_repo, [
                {'find_code', 1, fun(?CODE) -> {ok, active_code_row()} end},
                {'class_brief', 1, fun(?GROUP_ID) -> {ok, brief_row()} end},
                {'class_learners', 1, fun(?GROUP_ID) -> {ok, Learners} end}
            ]}
        ],
        fun() ->
            ?assertMatch(
                {ok, #{
                    org_name := <<"逸云书法"/utf8>>,
                    group_name := <<"周五班"/utf8>>,
                    learners := [
                        #{id := 994102, display_name := <<"陈小二"/utf8>>},
                        #{id := ?LEARNER_ID, display_name := <<"张小一"/utf8>>}
                    ]
                }},
                moya_invite_logic:invite_info(?CODE)
            )
        end
    ).

%% 码 active 但班已不可解析（FK CASCADE 防御兜底）→ not_found
invite_info_group_gone_test_() ->
    ?WITH_MECKS(
        [
            {moya_invite_repo, [
                {'find_code', 1, fun(?CODE) -> {ok, active_code_row()} end},
                {'class_brief', 1, fun(?GROUP_ID) -> {ok, undefined} end}
            ]}
        ],
        fun() ->
            ?assertEqual({error, not_found}, moya_invite_logic:invite_info(?CODE))
        end
    ).

%%%===================================================================
%%% join：事务内校验 + 幂等 + 插入接线
%%%===================================================================

%% with_tx 直执行 Tx（模式照 learner_bind logic 测试）
join_mocks(FindCodeReturn, InClassReturn, GuardianActiveReturn, InsertReturn) ->
    [
        {elib_pg, [
            {'with_tx', 2, fun(Tx, _Opts) -> Tx(?TEST_CONN) end}
        ]},
        {moya_invite_repo, [
            {'find_code_tx', 2, fun(_Conn, ?CODE) -> FindCodeReturn end},
            {'learner_active_in_class_tx', 3, fun(_Conn, ?GROUP_ID, ?LEARNER_ID) ->
                InClassReturn
            end},
            {'guardian_active_tx', 3, fun(_Conn, ?UID, ?LEARNER_ID) -> GuardianActiveReturn end},
            {'insert_guardian_tx', 3, fun(_Conn, ?UID, ?LEARNER_ID) ->
                self() ! {insert_guardian, ?UID, ?LEARNER_ID},
                InsertReturn
            end}
        ]}
    ].

join_code_not_found_test_() ->
    ?WITH_MECKS(
        join_mocks({ok, undefined}, true, false, ok),
        fun() ->
            ?assertEqual(
                {error, not_found},
                moya_invite_logic:join(?UID, ?CODE, ?LEARNER_ID)
            )
        end
    ).

join_revoked_code_folds_to_not_found_test_() ->
    Row = (active_code_row())#{<<"status">> => <<"revoked">>},
    ?WITH_MECKS(
        join_mocks({ok, Row}, true, false, ok),
        fun() ->
            ?assertEqual(
                {error, not_found},
                moya_invite_logic:join(?UID, ?CODE, ?LEARNER_ID)
            )
        end
    ).

%% 学员不在该班 active → learner_not_in_class，不插
join_learner_not_in_class_test_() ->
    ?WITH_MECKS(
        join_mocks({ok, active_code_row()}, false, false, ok),
        fun() ->
            ?assertEqual(
                {error, learner_not_in_class},
                moya_invite_logic:join(?UID, ?CODE, ?LEARNER_ID)
            ),
            receive
                {insert_guardian, _, _} ->
                    ?assert(false, "insert must not run for out-of-class learner")
            after 0 -> ok
            end
        end
    ).

%% 已 active 监护 → already_joined 幂等，不重复插
join_already_joined_test_() ->
    ?WITH_MECKS(
        join_mocks({ok, active_code_row()}, true, true, ok),
        fun() ->
            ?assertEqual(
                {ok, already_joined},
                moya_invite_logic:join(?UID, ?CODE, ?LEARNER_ID)
            ),
            receive
                {insert_guardian, _, _} -> ?assert(false, "insert must not run when already active")
            after 0 -> ok
            end
        end
    ).

%% 正常路径：无 active 行（含 removed 复活场景）→ insert_guardian_tx 被调 → joined
join_joined_test_() ->
    ?WITH_MECKS(
        join_mocks({ok, active_code_row()}, true, false, ok),
        fun() ->
            ?assertEqual(
                {ok, joined},
                moya_invite_logic:join(?UID, ?CODE, ?LEARNER_ID)
            ),
            receive
                {insert_guardian, ?UID, ?LEARNER_ID} -> ok
            after 0 -> ?assert(false, "insert_guardian_tx not called")
            end
        end
    ).

%% repo 插入失败 → db_error
join_insert_db_error_test_() ->
    ?WITH_MECKS(
        join_mocks({ok, active_code_row()}, true, false, {error, fk_violation}),
        fun() ->
            ?assertEqual(
                {error, db_error},
                moya_invite_logic:join(?UID, ?CODE, ?LEARNER_ID)
            )
        end
    ).

%% with_tx rollback → db_error
join_tx_rollback_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'with_tx', 2, fun(_Tx, _Opts) -> {rollback, connection_down} end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, db_error},
                moya_invite_logic:join(?UID, ?CODE, ?LEARNER_ID)
            )
        end
    ).

%%%===================================================================
%%% code_fingerprint：确定性 + 长度（日志纪律锚点）
%%%===================================================================

code_fingerprint_test() ->
    Fp = moya_invite_logic:code_fingerprint(?CODE),
    ?assertEqual(12, byte_size(Fp)),
    ?assertEqual(Fp, moya_invite_logic:code_fingerprint(?CODE)),
    %% 不同码不同指纹；非 binary 入参不抛
    ?assertNotEqual(Fp, moya_invite_logic:code_fingerprint(<<"OTHERCODE">>)),
    ?assertEqual(<<"invalid">>, moya_invite_logic:code_fingerprint(atom_input)).

%%%===================================================================
%%% Mock 基建
%%%===================================================================

%% create_code 守卫 + upsert：同一模块期望合并单条目（见文件头纪律）
create_code_mocks(StaffReturn, OwnerReturn, UpsertReturn, BriefFun) ->
    [
        {moya_invite_repo, [
            {'class_brief', 1, BriefFun},
            {'upsert_active_code', 2, fun(GroupId, Uid) ->
                self() ! {upsert_called, GroupId, Uid},
                UpsertReturn
            end}
        ]},
        {moya_acl, [
            {'resolve_staff', 2, fun(?UID, ?GROUP_ID) -> StaffReturn end},
            {'resolve_org_owner', 2, fun(?UID, ?ORG_ID) -> OwnerReturn end}
        ]},
        {moya_context_repo, [
            {'group_org', 1, fun(?GROUP_ID) -> {ok, ?ORG_ID} end}
        ]}
    ].

brief_row() ->
    #{<<"org_name">> => <<"逸云书法"/utf8>>, <<"group_name">> => <<"周五班"/utf8>>}.

staff_row() ->
    #{
        <<"group_id">> => ?GROUP_ID,
        <<"user_id">> => ?UID,
        <<"role">> => <<"teacher">>,
        <<"status">> => <<"active">>
    }.

active_code_row() ->
    #{
        <<"code">> => ?CODE,
        <<"group_id">> => ?GROUP_ID,
        <<"status">> => <<"active">>,
        <<"expires_at">> => null,
        <<"expired">> => null
    }.
