-module(organization_invitation_app_tests).

%% Invitation command（application 层）编排测试：
%% 锁序（组织行先）、target-only、一次性消费、幂等分支、错误映射与 hook 挂点。
%% SQL 真实行为由一次性 PG 上的 organization_invitation_behavior_harness.escript 覆盖。

-include_lib("eunit/include/eunit.hrl").

-define(ORG_ID, 301).
-define(OWNER, 401).
-define(TARGET, 402).
-define(STRANGER, 403).
-define(INV_ID, 501).

%% elib_pg:with_tx 同语义直通：在测试进程内以 fake_conn 执行事务体。
run_tx(TxFun) ->
    try TxFun(fake_conn) of
        Result -> Result
    catch
        throw:{abort_tx, {Code, Msg}} when is_integer(Code), is_binary(Msg) ->
            {error, {Code, Msg}};
        throw:{abort_tx, Reason} ->
            {error, Reason};
        throw:{rollback, Reason} ->
            {rollback, Reason};
        Class:Reason ->
            {error, {db_exception, Class, Reason}}
    end.

%%--------------------------------------------------------------------
%% store mock 装配
%%--------------------------------------------------------------------

default_store_mocks() ->
    OrgRow = #{<<"id">> => ?ORG_ID, <<"owner_id">> => ?OWNER, <<"status">> => <<"active">>},
    [
        {lock_organization_tx, 2, fun(fake_conn, _OrgId) -> {ok, OrgRow} end},
        {lock_organization_for_share_tx, 2, fun(Conn, OrgId) ->
            organization_invitation_pg:lock_organization_tx(Conn, OrgId)
        end},
        {member_tx, 3, fun(fake_conn, _OrgId, Uid) ->
            case get({t_member, Uid}) of
                undefined -> {error, not_found};
                Member -> {ok, Member}
            end
        end},
        {target_user_tx, 2, fun(fake_conn, Uid) -> {ok, #{<<"id">> => Uid}} end},
        {insert_tx, 2, fun(fake_conn, _Row) -> ok end},
        {find_by_digest_tx, 4, fun(fake_conn, _OrgId, _Target, _Digest) ->
            case get(t_invitation_row) of
                undefined -> {error, not_found};
                Row -> {ok, Row}
            end
        end},
        {find_latest_active_for_target_tx, 3, fun(fake_conn, _OrgId, _Target) ->
            case get(t_invitation_row) of
                undefined -> {error, not_found};
                Row -> {ok, Row}
            end
        end},
        {find_tx, 4, fun(fake_conn, _OrgId, _InvId, _Target) ->
            case get(t_invitation_row) of
                undefined -> {error, not_found};
                Row -> {ok, Row}
            end
        end},
        {expire_due_tx, 2, fun(fake_conn, _OrgId) -> {ok, 0} end},
        {consume_pending_tx, 3, fun(fake_conn, _InvId, Kind) ->
            record_event({consume, Kind}),
            StatusBin =
                case Kind of
                    accept -> <<"accepted">>;
                    reject -> <<"rejected">>;
                    revoke -> <<"revoked">>
                end,
            {ok, (base_row())#{<<"status">> => StatusBin, <<"responded_at">> => 1700000000}}
        end},
        {list_for_target_tx, 4, fun(fake_conn, _Target, _Status, _Limit) -> {ok, []} end},
        {list_for_org_tx, 4, fun(fake_conn, _OrgId, _Status, _Limit) -> {ok, []} end}
    ].

base_row() ->
    #{
        <<"id">> => ?INV_ID,
        <<"organization_id">> => ?ORG_ID,
        <<"target_user_id">> => ?TARGET,
        <<"invited_by">> => ?OWNER,
        <<"token_digest">> => organization_invitation:token_digest(<<"tok">>),
        <<"status">> => <<"pending">>,
        <<"expires_at">> => os:system_time(second) + 3600,
        <<"responded_at">> => null,
        <<"created_at">> => os:system_time(second),
        <<"updated_at">> => null
    }.

with_mocks(ExtraStoreMocks, TestFun) ->
    erlang:put(t_events, []),
    meck:new(organization_invitation_pg, [non_strict, no_link]),
    lists:foreach(
        fun({Name, Arity, Fun}) ->
            meck:expect(organization_invitation_pg, Name, Arity, Fun)
        end,
        merge_mocks(default_store_mocks(), ExtraStoreMocks)
    ),
    meck:new(elib_pg, [non_strict, no_link]),
    meck:expect(elib_pg, with_tx, 1, fun run_tx/1),
    %% 触达（P1）替身：记录调用并拦截真实 spawn（推送链不入本套件裁决范围）
    meck:new(organization_invitation_notify, [non_strict, no_link]),
    meck:expect(organization_invitation_notify, notify_created, 1, fun(TargetUid) ->
        record_event({notify, TargetUid}),
        ok
    end),
    try
        TestFun()
    after
        meck:unload(organization_invitation_pg),
        meck:unload(elib_pg),
        meck:unload(organization_invitation_notify),
        erlang:erase(t_events),
        lists:foreach(
            fun(K) -> erlang:erase(K) end,
            [{t_member, ?OWNER}, {t_member, ?TARGET}, t_invitation_row]
        )
    end.

merge_mocks(Base, Overrides) ->
    lists:map(
        fun({Name, Arity, Fun}) ->
            case lists:keyfind(Name, 1, Overrides) of
                {Name, Arity, OverFun} -> {Name, Arity, OverFun};
                false -> {Name, Arity, Fun}
            end
        end,
        Base
    ).

record_event(E) -> erlang:put(t_events, [E | erlang:get(t_events)]).
events() -> lists:reverse(erlang:get(t_events)).

set_member(Uid, Role, Status) ->
    put({t_member, Uid}, #{<<"role">> => Role, <<"status">> => Status}).

%%--------------------------------------------------------------------
%% create
%%--------------------------------------------------------------------

create_success_test_() ->
    {"create 成功：治理裁决、先 sweep 后插入；明文 token 只在响应出现一次；触发离线触达", fun() ->
        with_mocks([], fun() ->
            set_member(?OWNER, <<"owner">>, <<"active">>),
            {ok, View} = organization_invitation_app:create(?OWNER, ?ORG_ID, ?TARGET, #{
                invitation_id => ?INV_ID
            }),
            #{token := Token, status := <<"pending">>} = View,
            ?assert(is_binary(Token)),
            ?assertEqual(64, byte_size(Token)),
            %% 投影白名单：无 token_digest
            ?assertEqual(false, is_map_key(token_digest, View)),
            %% 触达（P1）：create 成功后对 target 发一次离线推送
            ?assertEqual([{notify, ?TARGET}], events())
        end)
    end}.

create_lock_order_test_() ->
    {"create 锁序断言：lock_organization → member(inviter) → member(target) → target_user → expire_due → insert",
        fun() ->
            with_mocks(
                [
                    {lock_organization_tx, 2, fun(fake_conn, _O) ->
                        record_event(lock_org),
                        {ok, #{<<"status">> => <<"active">>}}
                    end},
                    {member_tx, 3, fun(fake_conn, _O, Uid) ->
                        record_event({member, Uid}),
                        case Uid of
                            ?OWNER ->
                                {ok, #{<<"role">> => <<"owner">>, <<"status">> => <<"active">>}};
                            _ ->
                                {error, not_found}
                        end
                    end},
                    {target_user_tx, 2, fun(fake_conn, _Uid) ->
                        record_event(user_exists),
                        {ok, #{<<"id">> => ?TARGET}}
                    end},
                    {expire_due_tx, 2, fun(fake_conn, _O) ->
                        record_event(sweep),
                        {ok, 0}
                    end},
                    {insert_tx, 2, fun(fake_conn, _R) ->
                        record_event(insert),
                        ok
                    end}
                ],
                fun() ->
                    {ok, _} = organization_invitation_app:create(?OWNER, ?ORG_ID, ?TARGET, #{
                        invitation_id => ?INV_ID
                    }),
                    %% 触达（P1）恒在事务提交之后：notify 是序列末位
                    ?assertEqual(
                        [
                            lock_org,
                            {member, ?OWNER},
                            {member, ?TARGET},
                            user_exists,
                            sweep,
                            insert,
                            {notify, ?TARGET}
                        ],
                        events()
                    )
                end
            )
        end}.

create_guards_test_() ->
    [
        {"archived 组织 409", fun() ->
            with_mocks(
                [
                    {lock_organization_tx, 2, fun(fake_conn, _O) ->
                        {ok, #{<<"status">> => <<"archived">>}}
                    end}
                ],
                fun() ->
                    ?assertMatch(
                        {error, {409, _}},
                        organization_invitation_app:create(?OWNER, ?ORG_ID, ?TARGET, #{
                            invitation_id => ?INV_ID
                        })
                    )
                end
            )
        end},
        {"组织不存在 404", fun() ->
            with_mocks(
                [
                    {lock_organization_tx, 2, fun(fake_conn, _O) -> {error, not_found} end}
                ],
                fun() ->
                    ?assertMatch(
                        {error, {404, _}},
                        organization_invitation_app:create(?OWNER, ?ORG_ID, ?TARGET, #{
                            invitation_id => ?INV_ID
                        })
                    )
                end
            )
        end},
        {"非治理成员邀请 403（member 角色不够）", fun() ->
            with_mocks([], fun() ->
                set_member(?OWNER, <<"member">>, <<"active">>),
                ?assertMatch(
                    {error, {403, _}},
                    organization_invitation_app:create(?OWNER, ?ORG_ID, ?TARGET, #{
                        invitation_id => ?INV_ID
                    })
                )
            end)
        end},
        {"inviter 非成员 403", fun() ->
            with_mocks([], fun() ->
                ?assertMatch(
                    {error, {403, _}},
                    organization_invitation_app:create(?STRANGER, ?ORG_ID, ?TARGET, #{
                        invitation_id => ?INV_ID
                    })
                )
            end)
        end},
        {"target 已是 active 成员 409", fun() ->
            with_mocks([], fun() ->
                set_member(?OWNER, <<"owner">>, <<"active">>),
                set_member(?TARGET, <<"member">>, <<"active">>),
                ?assertMatch(
                    {error, {409, _}},
                    organization_invitation_app:create(?OWNER, ?ORG_ID, ?TARGET, #{
                        invitation_id => ?INV_ID
                    })
                )
            end)
        end},
        {"target 用户不存在 404（不落 FK 违规兜底 500；REAL BUG 2026-09-16 FINDING-2）", fun() ->
            with_mocks(
                [
                    {target_user_tx, 2, fun(fake_conn, _Uid) -> {error, not_found} end}
                ],
                fun() ->
                    set_member(?OWNER, <<"owner">>, <<"active">>),
                    ?assertMatch(
                        {error, {404, <<"用户不存在（仅支持邀请已注册用户）"/utf8>>}},
                        organization_invitation_app:create(?OWNER, ?ORG_ID, ?TARGET, #{
                            invitation_id => ?INV_ID
                        })
                    )
                end
            )
        end},
        {"并发窗口：校验通过后插入前 target 被删（23503→target_user_missing）→ 同一业务 404", fun() ->
            with_mocks(
                [
                    {insert_tx, 2, fun(fake_conn, _R) ->
                        {error, target_user_missing}
                    end}
                ],
                fun() ->
                    set_member(?OWNER, <<"owner">>, <<"active">>),
                    ?assertMatch(
                        {error, {404, <<"用户不存在（仅支持邀请已注册用户）"/utf8>>}},
                        organization_invitation_app:create(?OWNER, ?ORG_ID, ?TARGET, #{
                            invitation_id => ?INV_ID
                        })
                    )
                end
            )
        end},
        {"已有 pending（唯一索引冲突）409", fun() ->
            with_mocks(
                [
                    {insert_tx, 2, fun(fake_conn, _R) -> {error, pending_conflict} end}
                ],
                fun() ->
                    set_member(?OWNER, <<"owner">>, <<"active">>),
                    ?assertMatch(
                        {error, {409, _}},
                        organization_invitation_app:create(?OWNER, ?ORG_ID, ?TARGET, #{
                            invitation_id => ?INV_ID
                        })
                    )
                end
            )
        end},
        {"removed 成员可再次被邀请（restore 属 member command，不在此阻断）", fun() ->
            with_mocks([], fun() ->
                set_member(?OWNER, <<"owner">>, <<"active">>),
                set_member(?TARGET, <<"member">>, <<"removed">>),
                ?assertMatch(
                    {ok, _},
                    organization_invitation_app:create(?OWNER, ?ORG_ID, ?TARGET, #{
                        invitation_id => ?INV_ID
                    })
                )
            end)
        end},
        {"非法参量 400", fun() ->
            with_mocks([], fun() ->
                ?assertMatch(
                    {error, {400, _}},
                    organization_invitation_app:create(?OWNER, 0, ?TARGET, #{
                        invitation_id => ?INV_ID
                    })
                )
            end)
        end},
        {"create 失败不触发触达（archived 409 路径零 notify）", fun() ->
            with_mocks(
                [
                    {lock_organization_tx, 2, fun(fake_conn, _O) ->
                        {ok, #{<<"status">> => <<"archived">>}}
                    end}
                ],
                fun() ->
                    ?assertMatch(
                        {error, {409, _}},
                        organization_invitation_app:create(?OWNER, ?ORG_ID, ?TARGET, #{
                            invitation_id => ?INV_ID
                        })
                    ),
                    ?assertEqual([], events())
                end
            )
        end}
    ].

%%--------------------------------------------------------------------
%% accept
%%--------------------------------------------------------------------

accept_success_test_() ->
    [
        {"首次 accept：一次性消费 + already_accepted=false + hook 同事务调用", fun() ->
            Row = base_row(),
            with_mocks([], fun() ->
                put(t_invitation_row, Row),
                Me = self(),
                Hook = fun(Conn, HookRow) ->
                    Me ! {hook_called, Conn, HookRow},
                    ok
                end,
                {ok, View} = organization_invitation_app:accept(
                    ?TARGET,
                    ?ORG_ID,
                    <<"tok">>,
                    #{membership_hook => Hook}
                ),
                ?assertEqual(<<"accepted">>, maps:get(status, View)),
                ?assertEqual(false, maps:get(already_accepted, View)),
                ?assertEqual([{consume, accept}], events()),
                receive
                    {hook_called, fake_conn, HookRow} ->
                        ?assertEqual(?INV_ID, maps:get(<<"id">>, HookRow))
                after 0 ->
                    ?assert(hook_not_called)
                end,
                %% 投影无 digest
                ?assertEqual(false, is_map_key(token_digest, View))
            end)
        end},
        {"重复 accept 幂等：读终态返回 already_accepted=true，不二次消费/hook", fun() ->
            Row = (base_row())#{
                <<"status">> => <<"accepted">>,
                <<"responded_at">> => 1700000000
            },
            with_mocks([], fun() ->
                put(t_invitation_row, Row),
                {ok, View} = organization_invitation_app:accept(?TARGET, ?ORG_ID, <<"tok">>, #{}),
                ?assertEqual(<<"accepted">>, maps:get(status, View)),
                ?assertEqual(true, maps:get(already_accepted, View)),
                ?assertEqual([], events())
            end)
        end},
        {"非目标用户 accept：digest 同语句 target 作用域命中不了行 → 404", fun() ->
            with_mocks(
                [
                    {find_by_digest_tx, 4, fun(fake_conn, _O, _T, _D) -> {error, not_found} end}
                ],
                fun() ->
                    ?assertMatch(
                        {error, {404, _}},
                        organization_invitation_app:accept(
                            ?STRANGER,
                            ?ORG_ID,
                            <<"tok">>,
                            #{}
                        )
                    )
                end
            )
        end},
        {"过期 accept 409", fun() ->
            Row = (base_row())#{<<"status">> => <<"expired">>},
            with_mocks([], fun() ->
                put(t_invitation_row, Row),
                ?assertMatch(
                    {error, {409, _}},
                    organization_invitation_app:accept(?TARGET, ?ORG_ID, <<"tok">>, #{})
                )
            end)
        end},
        {"已撤销 accept 409", fun() ->
            Row = (base_row())#{<<"status">> => <<"revoked">>},
            with_mocks([], fun() ->
                put(t_invitation_row, Row),
                ?assertMatch(
                    {error, {409, _}},
                    organization_invitation_app:accept(?TARGET, ?ORG_ID, <<"tok">>, #{})
                )
            end)
        end},
        {"已拒绝 accept 409", fun() ->
            Row = (base_row())#{<<"status">> => <<"rejected">>},
            with_mocks([], fun() ->
                put(t_invitation_row, Row),
                ?assertMatch(
                    {error, {409, _}},
                    organization_invitation_app:accept(?TARGET, ?ORG_ID, <<"tok">>, #{})
                )
            end)
        end},
        {"consume 竞争落败后重读终态：收敛为幂等成功（不 500）", fun() ->
            Row = base_row(),
            AcceptedRow = (base_row())#{
                <<"status">> => <<"accepted">>,
                <<"responded_at">> => 1700000000
            },
            with_mocks(
                [
                    {consume_pending_tx, 3, fun(fake_conn, _Id, _K) -> {error, not_pending} end},
                    {find_by_digest_tx, 4, fun(fake_conn, _O, _T, _D) ->
                        case get(t_second_read) of
                            undefined ->
                                put(t_second_read, true),
                                {ok, Row};
                            _ ->
                                {ok, AcceptedRow}
                        end
                    end}
                ],
                fun() ->
                    {ok, View} = organization_invitation_app:accept(
                        ?TARGET,
                        ?ORG_ID,
                        <<"tok">>,
                        #{}
                    ),
                    ?assertEqual(true, maps:get(already_accepted, View))
                end
            )
        end},
        {"hook 失败 → 事务整体回滚（错误透传）", fun() ->
            with_mocks([], fun() ->
                put(t_invitation_row, base_row()),
                Hook = fun(_Conn, _Row) -> {error, {409, <<"成员不可恢复"/utf8>>}} end,
                ?assertMatch(
                    {error, {409, <<"成员不可恢复"/utf8>>}},
                    organization_invitation_app:accept(
                        ?TARGET,
                        ?ORG_ID,
                        <<"tok">>,
                        #{membership_hook => Hook}
                    )
                )
            end)
        end}
    ].

%%--------------------------------------------------------------------
%% accept_targeted（P0 定向邀请免口令：JWT 身份即凭据）
%%--------------------------------------------------------------------

accept_targeted_success_test_() ->
    [
        {"首次免口令 accept：按 (org,target) 定位 → 消费 + hook + already_accepted=false", fun() ->
            Row = base_row(),
            with_mocks([], fun() ->
                put(t_invitation_row, Row),
                Me = self(),
                Hook = fun(Conn, HookRow) ->
                    Me ! {hook_called, Conn, HookRow},
                    ok
                end,
                {ok, View} = organization_invitation_app:accept_targeted(
                    ?TARGET,
                    ?ORG_ID,
                    #{membership_hook => Hook}
                ),
                ?assertEqual(<<"accepted">>, maps:get(status, View)),
                ?assertEqual(false, maps:get(already_accepted, View)),
                ?assertEqual([{consume, accept}], events()),
                receive
                    {hook_called, fake_conn, HookRow} ->
                        ?assertEqual(?INV_ID, maps:get(<<"id">>, HookRow))
                after 0 ->
                    ?assert(hook_not_called)
                end,
                ?assertEqual(false, is_map_key(token_digest, View))
            end)
        end},
        {"重复免口令 accept 幂等：读到 accepted 终态 → already_accepted=true，不二次消费", fun() ->
            Row = (base_row())#{
                <<"status">> => <<"accepted">>,
                <<"responded_at">> => 1700000000
            },
            with_mocks([], fun() ->
                put(t_invitation_row, Row),
                {ok, View} = organization_invitation_app:accept_targeted(?TARGET, ?ORG_ID, #{}),
                ?assertEqual(<<"accepted">>, maps:get(status, View)),
                ?assertEqual(true, maps:get(already_accepted, View)),
                ?assertEqual([], events())
            end)
        end},
        {"无 pending/accepted 行 → 404（非目标用户同语句命中不了行）", fun() ->
            with_mocks(
                [
                    {find_latest_active_for_target_tx, 3, fun(_C, _O, _T) ->
                        {error, not_found}
                    end}
                ],
                fun() ->
                    ?assertMatch(
                        {error, {404, _}},
                        organization_invitation_app:accept_targeted(?STRANGER, ?ORG_ID, #{})
                    )
                end
            )
        end},
        {"过期 accept_targeted 409", fun() ->
            Row = (base_row())#{<<"status">> => <<"expired">>},
            with_mocks([], fun() ->
                put(t_invitation_row, Row),
                ?assertMatch(
                    {error, {409, _}},
                    organization_invitation_app:accept_targeted(?TARGET, ?ORG_ID, #{})
                )
            end)
        end},
        {"已撤销 accept_targeted 409", fun() ->
            Row = (base_row())#{<<"status">> => <<"revoked">>},
            with_mocks([], fun() ->
                put(t_invitation_row, Row),
                ?assertMatch(
                    {error, {409, _}},
                    organization_invitation_app:accept_targeted(?TARGET, ?ORG_ID, #{})
                )
            end)
        end},
        {"已拒绝 accept_targeted 409", fun() ->
            Row = (base_row())#{<<"status">> => <<"rejected">>},
            with_mocks([], fun() ->
                put(t_invitation_row, Row),
                ?assertMatch(
                    {error, {409, _}},
                    organization_invitation_app:accept_targeted(?TARGET, ?ORG_ID, #{})
                )
            end)
        end},
        {"免口令路径 consume 竞争落败：重读终态收敛幂等成功（不 500）", fun() ->
            Row = base_row(),
            AcceptedRow = (base_row())#{
                <<"status">> => <<"accepted">>,
                <<"responded_at">> => 1700000000
            },
            with_mocks(
                [
                    {consume_pending_tx, 3, fun(fake_conn, _Id, _K) -> {error, not_pending} end},
                    {find_latest_active_for_target_tx, 3, fun(_C, _O, _T) ->
                        case get(t_second_read) of
                            undefined ->
                                put(t_second_read, true),
                                {ok, Row};
                            _ ->
                                {ok, AcceptedRow}
                        end
                    end}
                ],
                fun() ->
                    {ok, View} = organization_invitation_app:accept_targeted(
                        ?TARGET,
                        ?ORG_ID,
                        #{}
                    ),
                    ?assertEqual(true, maps:get(already_accepted, View))
                end
            )
        end},
        {"免口令路径 hook 失败 → 事务整体回滚（错误透传）", fun() ->
            with_mocks([], fun() ->
                put(t_invitation_row, base_row()),
                Hook = fun(_Conn, _Row) -> {error, {409, <<"成员不可恢复"/utf8>>}} end,
                ?assertMatch(
                    {error, {409, <<"成员不可恢复"/utf8>>}},
                    organization_invitation_app:accept_targeted(
                        ?TARGET,
                        ?ORG_ID,
                        #{membership_hook => Hook}
                    )
                )
            end)
        end},
        {"非法参量 400", fun() ->
            with_mocks([], fun() ->
                ?assertMatch(
                    {error, {400, _}},
                    organization_invitation_app:accept_targeted(0, ?ORG_ID, #{})
                ),
                ?assertMatch(
                    {error, {400, _}},
                    organization_invitation_app:accept_targeted(?TARGET, 0, #{})
                )
            end)
        end}
    ].

%%--------------------------------------------------------------------
%% reject / revoke
%%--------------------------------------------------------------------

reject_idempotent_test_() ->
    [
        {"target 拒绝 pending → consumed", fun() ->
            with_mocks([], fun() ->
                put(t_invitation_row, base_row()),
                {ok, View} = organization_invitation_app:reject(?TARGET, ?ORG_ID, ?INV_ID),
                ?assertEqual(<<"rejected">>, maps:get(status, View))
            end)
        end},
        {"重复拒绝幂等：already_terminal=true，不二次消费", fun() ->
            Row = (base_row())#{<<"status">> => <<"rejected">>},
            with_mocks([], fun() ->
                put(t_invitation_row, Row),
                {ok, View} = organization_invitation_app:reject(?TARGET, ?ORG_ID, ?INV_ID),
                ?assertEqual(true, maps:get(already_terminal, View)),
                ?assertEqual([], events())
            end)
        end},
        {"已接受后拒绝 409（终态不可逆）", fun() ->
            Row = (base_row())#{<<"status">> => <<"accepted">>},
            with_mocks([], fun() ->
                put(t_invitation_row, Row),
                ?assertMatch(
                    {error, {409, _}},
                    organization_invitation_app:reject(?TARGET, ?ORG_ID, ?INV_ID)
                )
            end)
        end}
    ].

revoke_governance_test_() ->
    [
        {"owner 撤销 pending → consumed", fun() ->
            with_mocks([], fun() ->
                put(t_invitation_row, base_row()),
                set_member(?OWNER, <<"owner">>, <<"active">>),
                {ok, View} = organization_invitation_app:revoke(?OWNER, ?ORG_ID, ?INV_ID),
                ?assertEqual(<<"revoked">>, maps:get(status, View))
            end)
        end},
        {"admin 撤销放行", fun() ->
            with_mocks([], fun() ->
                put(t_invitation_row, base_row()),
                set_member(?OWNER, <<"admin">>, <<"active">>),
                {ok, _} = organization_invitation_app:revoke(?OWNER, ?ORG_ID, ?INV_ID),
                ok
            end)
        end},
        {"普通 member 撤销 403", fun() ->
            with_mocks([], fun() ->
                set_member(?OWNER, <<"member">>, <<"active">>),
                ?assertMatch(
                    {error, {403, _}},
                    organization_invitation_app:revoke(?OWNER, ?ORG_ID, ?INV_ID)
                )
            end)
        end},
        {"非成员撤销 403", fun() ->
            with_mocks([], fun() ->
                ?assertMatch(
                    {error, {403, _}},
                    organization_invitation_app:revoke(?STRANGER, ?ORG_ID, ?INV_ID)
                )
            end)
        end},
        {"重复撤销幂等", fun() ->
            Row = (base_row())#{<<"status">> => <<"revoked">>},
            with_mocks([], fun() ->
                put(t_invitation_row, Row),
                set_member(?OWNER, <<"owner">>, <<"active">>),
                {ok, View} = organization_invitation_app:revoke(?OWNER, ?ORG_ID, ?INV_ID),
                ?assertEqual(true, maps:get(already_terminal, View))
            end)
        end},
        {"跨 Org 撤销：find_tx 同语句 Org 作用域 → 404", fun() ->
            with_mocks(
                [
                    {lock_organization_tx, 2, fun(fake_conn, O) ->
                        {ok, #{<<"id">> => O, <<"status">> => <<"active">>}}
                    end},
                    {find_tx, 4, fun(fake_conn, _O, _I, _T) -> {error, not_found} end}
                ],
                fun() ->
                    set_member(?OWNER, <<"owner">>, <<"active">>),
                    ?assertMatch(
                        {error, {404, _}},
                        organization_invitation_app:revoke(?OWNER, ?ORG_ID, ?INV_ID)
                    )
                end
            )
        end}
    ].

%%--------------------------------------------------------------------
%% list
%%--------------------------------------------------------------------

list_test_() ->
    [
        {"target 列表走作用域语句", fun() ->
            with_mocks(
                [
                    {list_for_target_tx, 4, fun(fake_conn, T, Status, Limit) ->
                        record_event({list_target, T, Status, Limit}),
                        {ok, [base_row()]}
                    end}
                ],
                fun() ->
                    {ok, [View]} = organization_invitation_app:list_for_target(?TARGET, #{}),
                    ?assertEqual(?TARGET, maps:get(target_user_id, View)),
                    ?assertEqual(false, is_map_key(token_digest, View)),
                    ?assertMatch([{list_target, ?TARGET, undefined, 20}], events())
                end
            )
        end},
        {"org 列表要求治理成员", fun() ->
            with_mocks([], fun() ->
                set_member(?OWNER, <<"member">>, <<"active">>),
                ?assertMatch(
                    {error, {403, _}},
                    organization_invitation_app:list_for_org(?OWNER, ?ORG_ID, #{})
                )
            end)
        end}
    ].

%%--------------------------------------------------------------------
%% store SQL 形状回归（REAL BUG 2026-09-16：list 参数表漏 $1 主参 + LIMIT 占位
%% 差一，真实 PG 上报 42804 datatype_mismatch）。
%% 白盒断言：占位符数 = 参数数，且首参为作用域主参、末参为 Limit。
%%--------------------------------------------------------------------

list_sql_shape_regression_test_() ->
    [
        {"list_for_org_tx(status=pending)：3 占位符 / 3 参数 [$1 org, $2 status, $3 limit]", fun() ->
            {Sql, Params} = capture_list(fun(C) ->
                organization_invitation_pg:list_for_org_tx(C, ?ORG_ID, pending, 20)
            end),
            ?assertEqual(3, count_placeholders(Sql)),
            ?assertEqual(3, length(Params)),
            ?assertMatch([?ORG_ID, <<"pending">>, 20], Params)
        end},
        {"list_for_org_tx(status=undefined)：2 占位符 / 2 参数 [$1 org, $2 limit]", fun() ->
            {Sql, Params} = capture_list(fun(C) ->
                organization_invitation_pg:list_for_org_tx(C, ?ORG_ID, undefined, 20)
            end),
            ?assertEqual(2, count_placeholders(Sql)),
            ?assertEqual(2, length(Params)),
            ?assertMatch([?ORG_ID, 20], Params)
        end},
        {"list_for_target_tx(status=pending)：3 占位符 / 3 参数 [$1 target, $2 status, $3 limit]",
            fun() ->
                {Sql, Params} = capture_list(fun(C) ->
                    organization_invitation_pg:list_for_target_tx(C, ?TARGET, pending, 20)
                end),
                ?assertEqual(3, count_placeholders(Sql)),
                ?assertEqual(3, length(Params)),
                ?assertMatch([?TARGET, <<"pending">>, 20], Params)
            end},
        {"find_latest_active_for_target_tx：2 占位符 / 2 参数 [$1 org, $2 target]，只取 pending/accepted",
            fun() ->
                {Sql, Params} = capture_one_tx(fun(C) ->
                    organization_invitation_pg:find_latest_active_for_target_tx(
                        C,
                        ?ORG_ID,
                        ?TARGET
                    )
                end),
                ?assertEqual(2, count_placeholders(Sql)),
                ?assertEqual(2, length(Params)),
                ?assertMatch([?ORG_ID, ?TARGET], Params),
                %% 只定位 pending/accepted：rejected/revoked/expired 终态不得被再消费
                ?assert(nomatch =/= binary:match(Sql, <<"status IN ('pending', 'accepted')">>))
            end}
    ].

capture_list(Fun) ->
    meck:new(elib_pg, [non_strict, no_link]),
    meck:expect(
        elib_pg,
        query,
        3,
        fun
            (_Conn, Sql, P) when is_list(P) -> {ok, {captured, Sql, P}};
            (_Conn, Sql, P) -> {ok, {captured, Sql, [P]}}
        end
    ),
    try
        {ok, {captured, Sql, Params}} = Fun(fake_conn),
        {Sql, Params}
    after
        meck:unload(elib_pg)
    end.

%% one_tx 形态语句的捕获：query 返回单行集（one_tx 要求 [Row|_] 形状），
%% SQL/参数经消息带回测试进程。
capture_one_tx(Fun) ->
    meck:new(elib_pg, [non_strict, no_link]),
    Me = self(),
    meck:expect(elib_pg, query, 3, fun(_Conn, Sql, P) ->
        Me ! {captured_sql, Sql, P},
        {ok, [#{<<"captured_row">> => true}]}
    end),
    try
        {ok, _Row} = Fun(fake_conn),
        receive
            {captured_sql, Sql, Params} -> {Sql, Params}
        after 0 -> error(no_sql_captured)
        end
    after
        meck:unload(elib_pg)
    end.

count_placeholders(Sql) when is_binary(Sql) ->
    count_placeholders(binary_to_list(Sql), 0);
count_placeholders(Sql) when is_list(Sql) ->
    count_placeholders(Sql, 0).

count_placeholders([], N) ->
    N;
count_placeholders([$\$ | Rest], N) ->
    case Rest of
        [D | _] when D >= $0, D =< $9 -> count_placeholders(tail_after_digits(Rest), N + 1);
        _ -> count_placeholders(Rest, N)
    end;
count_placeholders([_ | Rest], N) ->
    count_placeholders(Rest, N).

tail_after_digits([D | Rest]) when D >= $0, D =< $9 -> tail_after_digits(Rest);
tail_after_digits(Rest) -> Rest.
