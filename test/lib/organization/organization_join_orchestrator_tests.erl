-module(organization_join_orchestrator_tests).

%% GZAPP-01 Join Orchestrator（统一加入编排）测试：
%% 四级落地顺序、幂等重放、默认 WS 缺失不阻塞、ws archived 980 整体回滚、
%% org archived 409 / 不存在 404、role_conflict 409、membership_hook 适配。
%% SQL 真实行为由一次性 PG 上的 organization_invite_code_behavior_harness.escript 覆盖。

-include_lib("eunit/include/eunit.hrl").
-include("error_code.hrl").

-define(ORG_ID, 301).
-define(WS_ID, 311).
-define(GID, 321).
-define(CID, 331).
-define(OWNER, 401).
-define(TARGET, 402).

record_event(E) -> erlang:put(t_events, [E | erlang:get(t_events)]).
events() -> lists:reverse(erlang:get(t_events)).

default_mocks() ->
    [
        {organization_member_repo, [
            {'find_organization_for_share_tx', 3, fun(fake_conn, _O, _C) ->
                record_event(lock_org),
                {ok, #{
                    <<"id">> => ?ORG_ID,
                    <<"owner_id">> => ?OWNER,
                    <<"status">> => <<"active">>
                }}
            end},
            {'upsert_active_tx', 5, fun(fake_conn, _O, _U, _R, _B) ->
                record_event(upsert_org_member),
                {ok, changed, #{}}
            end}
        ]},
        {organization_default_workspace_pg, [
            {'find_tx', 2, fun(fake_conn, _O) ->
                record_event(find_default_ws),
                {ok, ?WS_ID}
            end}
        ]},
        {workspace_guard, [
            {'ensure_writable_tx', 2, fun(fake_conn, {workspace, ?WS_ID}) ->
                record_event(guard_ws),
                ok
            end},
            {'abort_on_error', 1, fun
                (ok) -> ok;
                ({error, Reason}) -> throw({abort_tx, Reason})
            end}
        ]},
        {workspace_member_repo, [
            {'upsert_active_tx', 5, fun(fake_conn, _W, _U, _R, _B) ->
                record_event(upsert_ws_member),
                {ok, changed, #{}}
            end}
        ]},
        {elib_pg, [
            %% 群/频道查找（one_id_tx）：按 SQL 表名区分返回
            {'query', 3, fun(fake_conn, Sql, _P) when is_binary(Sql) ->
                case binary:match(Sql, [<<"\"group\"">>]) of
                    nomatch ->
                        record_event(find_channel),
                        {ok, [#{<<"id">> => ?CID}]};
                    _ ->
                        record_event(find_group),
                        {ok, [#{<<"id">> => ?GID}]}
                end
            end}
        ]},
        {group_member_ds, [
            {'join_group', 5, fun(fake_conn, _Mode, _U, Gid, _Opt) ->
                record_event({join_group, Gid}),
                {ok, 1}
            end}
        ]},
        {channel_subscription_repo, [
            {'upsert_active', 3, fun(fake_conn, CId, _U) ->
                record_event({subscribe_channel, CId}),
                {ok, true}
            end}
        ]}
    ].

with_mocks(Extra, TestFun) ->
    erlang:put(t_events, []),
    Mocks = merge_mocks(default_mocks(), Extra),
    Mods = [M || {M, _} <- Mocks],
    lists:foreach(fun(M) -> meck:new(M, [non_strict, no_link]) end, Mods),
    lists:foreach(
        fun({M, Expects}) ->
            lists:foreach(
                fun({Name, Arity, Fun}) -> meck:expect(M, Name, Arity, Fun) end,
                Expects
            )
        end,
        Mocks
    ),
    try
        TestFun()
    after
        lists:foreach(fun(M) -> meck:unload(M) end, Mods),
        erlang:erase(t_events)
    end.

%% Base/Extra 均为 [{Module, [{Name, Arity, Fun}]}]：
%% 同 Module 同 Name 的替换，同 Module 新增的追加。
merge_mocks(Base, Extra) ->
    AllMods = lists:usort([M || {M, _} <- Base] ++ [M || {M, _} <- Extra]),
    [
        begin
            BaseExpects = proplists:get_value(M, Base, []),
            ExtraExpects = proplists:get_value(M, Extra, []),
            Overridden = [Name || {Name, _A, _F} <- ExtraExpects],
            Kept = [E || E = {Name, _A, _F} <- BaseExpects, not lists:member(Name, Overridden)],
            {M, Kept ++ ExtraExpects}
        end
     || M <- AllMods
    ].

%%--------------------------------------------------------------------
%% join_tx：四级落地
%%--------------------------------------------------------------------

join_tx_full_chain_test_() ->
    {"全链顺序：锁org → org member → 默认WS → ws守卫 → ws member → 群 → 频道", fun() ->
        with_mocks([], fun() ->
            {ok, joined, Summary} =
                organization_join_orchestrator:join_tx(
                    fake_conn, ?ORG_ID, ?TARGET, ?OWNER, <<"member">>
                ),
            ?assertEqual(
                [
                    lock_org,
                    upsert_org_member,
                    find_default_ws,
                    guard_ws,
                    upsert_ws_member,
                    find_group,
                    {join_group, ?GID},
                    find_channel,
                    {subscribe_channel, ?CID}
                ],
                events()
            ),
            ?assertEqual(?WS_ID, maps:get(workspace_id, Summary)),
            ?assertEqual(?GID, maps:get(group_id, Summary)),
            ?assertEqual(?CID, maps:get(channel_id, Summary))
        end)
    end}.

join_tx_idempotent_replay_test_() ->
    {"幂等重放：三级 upsert 均 unchanged → outcome unchanged，无第二次副作用差异", fun() ->
        with_mocks(
            [
                {organization_member_repo, [
                    {'upsert_active_tx', 5, fun(_C, _O, _U, _R, _B) ->
                        record_event(upsert_org_member),
                        {ok, unchanged, #{}}
                    end}
                ]},
                {workspace_member_repo, [
                    {'upsert_active_tx', 5, fun(_C, _W, _U, _R, _B) ->
                        record_event(upsert_ws_member),
                        {ok, unchanged, #{}}
                    end}
                ]},
                {group_member_ds, [
                    {'join_group', 5, fun(_C, _M, _U, _G, _O) ->
                        record_event(join_group),
                        {ok, 0}
                    end}
                ]},
                {channel_subscription_repo, [
                    {'upsert_active', 3, fun(_C, _Ch, _U) ->
                        record_event(subscribe_channel),
                        {ok, false}
                    end}
                ]}
            ],
            fun() ->
                {ok, unchanged, Summary} =
                    organization_join_orchestrator:join_tx(
                        fake_conn, ?ORG_ID, ?TARGET, ?OWNER, <<"member">>
                    ),
                ?assertEqual(?WS_ID, maps:get(workspace_id, Summary))
            end
        )
    end}.

join_tx_no_default_ws_test_() ->
    {"默认 WS 缺失：只写 org member（不阻塞），summary 三级 none", fun() ->
        with_mocks(
            [
                {organization_default_workspace_pg, [
                    {'find_tx', 2, fun(fake_conn, _O) ->
                        record_event(find_default_ws),
                        {error, not_found}
                    end}
                ]}
            ],
            fun() ->
                {ok, joined, Summary} =
                    organization_join_orchestrator:join_tx(
                        fake_conn, ?ORG_ID, ?TARGET, ?OWNER, <<"member">>
                    ),
                ?assertEqual(none, maps:get(workspace_id, Summary)),
                ?assertEqual(none, maps:get(group_id, Summary)),
                ?assertEqual(none, maps:get(channel_id, Summary)),
                ?assertEqual(
                    [lock_org, upsert_org_member, find_default_ws],
                    events()
                )
            end
        )
    end}.

join_tx_guards_test_() ->
    [
        {"org archived 409（整体回滚）", fun() ->
            with_mocks(
                [
                    {organization_member_repo, [
                        {'find_organization_for_share_tx', 3, fun(_C, _O, _C2) ->
                            {ok, #{<<"status">> => <<"archived">>}}
                        end}
                    ]}
                ],
                fun() ->
                    ?assertThrow(
                        {abort_tx, {409, _}},
                        organization_join_orchestrator:join_tx(
                            fake_conn, ?ORG_ID, ?TARGET, ?OWNER, <<"member">>
                        )
                    )
                end
            )
        end},
        {"org 不存在 404", fun() ->
            with_mocks(
                [
                    {organization_member_repo, [
                        {'find_organization_for_share_tx', 3, fun(_C, _O, _C2) ->
                            {error, not_found}
                        end}
                    ]}
                ],
                fun() ->
                    ?assertThrow(
                        {abort_tx, {404, _}},
                        organization_join_orchestrator:join_tx(
                            fake_conn, ?ORG_ID, ?TARGET, ?OWNER, <<"member">>
                        )
                    )
                end
            )
        end},
        {"org member role_conflict 409（不降级既有角色）", fun() ->
            with_mocks(
                [
                    {organization_member_repo, [
                        {'upsert_active_tx', 5, fun(_C, _O, _U, _R, _B) ->
                            {ok, role_conflict, #{}}
                        end}
                    ]}
                ],
                fun() ->
                    ?assertThrow(
                        {abort_tx, {409, _}},
                        organization_join_orchestrator:join_tx(
                            fake_conn, ?ORG_ID, ?TARGET, ?OWNER, <<"member">>
                        )
                    )
                end
            )
        end},
        {"默认 WS archived 980（980 语义整体回滚，org member 也不落）", fun() ->
            with_mocks(
                [
                    {workspace_guard, [
                        {'ensure_writable_tx', 2, fun(_C, {workspace, ?WS_ID}) ->
                            {error, {?ERR_WORKSPACE_ARCHIVED, <<"工作区已归档"/utf8>>}}
                        end},
                        {'abort_on_error', 1, fun
                            (ok) -> ok;
                            ({error, Reason}) -> throw({abort_tx, Reason})
                        end}
                    ]}
                ],
                fun() ->
                    ?assertThrow(
                        {abort_tx, {?ERR_WORKSPACE_ARCHIVED, _}},
                        organization_join_orchestrator:join_tx(
                            fake_conn, ?ORG_ID, ?TARGET, ?OWNER, <<"member">>
                        )
                    ),
                    %% ws member 未写（守卫在 upsert 之前）
                    ?assertEqual(
                        [lock_org, upsert_org_member, find_default_ws],
                        events()
                    )
                end
            )
        end},
        {"ws member role_conflict 409", fun() ->
            with_mocks(
                [
                    {workspace_member_repo, [
                        {'upsert_active_tx', 5, fun(_C, _W, _U, _R, _B) ->
                            {ok, role_conflict, #{}}
                        end}
                    ]}
                ],
                fun() ->
                    ?assertThrow(
                        {abort_tx, {409, _}},
                        organization_join_orchestrator:join_tx(
                            fake_conn, ?ORG_ID, ?TARGET, ?OWNER, <<"member">>
                        )
                    )
                end
            )
        end},
        {"群/频道行缺失（历史数据异常）跳过该级不阻塞", fun() ->
            with_mocks(
                [
                    {elib_pg, [
                        {'query', 3, fun(_C, _Sql, _P) -> {ok, []} end}
                    ]}
                ],
                fun() ->
                    {ok, joined, Summary} =
                        organization_join_orchestrator:join_tx(
                            fake_conn, ?ORG_ID, ?TARGET, ?OWNER, <<"member">>
                        ),
                    ?assertEqual(none, maps:get(group_id, Summary)),
                    ?assertEqual(none, maps:get(channel_id, Summary)),
                    ?assertEqual(?WS_ID, maps:get(workspace_id, Summary))
                end
            )
        end},
        {"群加入失败整体回滚", fun() ->
            with_mocks(
                [
                    {group_member_ds, [
                        {'join_group', 5, fun(_C, _M, _U, _G, _O) ->
                            {error, membership_required}
                        end}
                    ]}
                ],
                fun() ->
                    ?assertThrow(
                        {abort_tx, {internal, {general_group_join, ?GID, membership_required}}},
                        organization_join_orchestrator:join_tx(
                            fake_conn, ?ORG_ID, ?TARGET, ?OWNER, <<"member">>
                        )
                    )
                end
            )
        end}
    ].

%%--------------------------------------------------------------------
%% membership_hook（invitation accept 挂点）
%%--------------------------------------------------------------------

membership_hook_test_() ->
    [
        {"hook 从邀请行取作用域转发 join_tx，成功 → ok", fun() ->
            with_mocks([], fun() ->
                Row = #{
                    <<"organization_id">> => ?ORG_ID,
                    <<"target_user_id">> => ?TARGET,
                    <<"invited_by">> => ?OWNER
                },
                ?assertEqual(ok, organization_join_orchestrator:membership_hook(fake_conn, Row)),
                ?assert(lists:member(upsert_org_member, events()))
            end)
        end},
        {"hook：join_tx 业务拒绝（409）→ {error, {409, Msg}}（accept 整体回滚）", fun() ->
            with_mocks(
                [
                    {organization_member_repo, [
                        {'find_organization_for_share_tx', 3, fun(_C, _O, _C2) ->
                            {ok, #{<<"status">> => <<"archived">>}}
                        end}
                    ]}
                ],
                fun() ->
                    Row = #{<<"organization_id">> => ?ORG_ID, <<"target_user_id">> => ?TARGET},
                    ?assertMatch(
                        {error, {409, _}},
                        organization_join_orchestrator:membership_hook(fake_conn, Row)
                    )
                end
            )
        end},
        {"hook：invited_by NULL 归一为 null（不透传 undefined）", fun() ->
            with_mocks([], fun() ->
                Row = #{
                    <<"organization_id">> => ?ORG_ID,
                    <<"target_user_id">> => ?TARGET,
                    <<"invited_by">> => null
                },
                ?assertEqual(ok, organization_join_orchestrator:membership_hook(fake_conn, Row))
            end)
        end}
    ].
