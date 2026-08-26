-module(channel_scope_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% 双体验 v2.5.2 WP3/T5 — channel scope 感知单元测试
%%% 覆盖：scope 创建校验（Guest 拒建/非成员拒建/非法 workspace_id 400/
%%% 非法 scope 400）、personal 默认零变化、workspace 创建注入 scope 字段、
%%% scope 不可变（update_channel_checked 400）。

-define(UID, 900001).
-define(WS_ID, 800001).
-define(CID, 666001).

create_scope_validation_test_() ->
    Self = self(),
    ?WITH_MECKS(
        [
            {workspace_logic, [
                {'ensure_can_create_resource', 2, fun(WsId, Uid) ->
                    Self ! {ensure_create, WsId, Uid},
                    case {WsId, Uid} of
                        {?WS_ID, 900002} -> {error, {403, <<"Guest 角色不能创建工作区资源"/utf8>>}};
                        {?WS_ID, 900003} -> {error, {403, <<"非工作区成员，禁止访问该资源"/utf8>>}};
                        _ -> ok
                    end
                end}
            ]},
            {channel_logic_message, [
                {'create_channel', 4, fun(Uid, Name, Opts, Max) ->
                    Self ! {legacy_create, Uid, Name, Opts, Max},
                    {ok, #{<<"id">> => ?CID, <<"name">> => Name}}
                end}
            ]}
        ],
        fun() ->
            [
                {"guest cannot create workspace channel (403)", fun() ->
                    ?assertMatch(
                        {error, {403, _}},
                        channel_logic:create_channel(
                            900002, <<"ws-ch">>, #{}, 20, {<<"workspace">>, ?WS_ID}
                        )
                    )
                end},
                {"non workspace member cannot create workspace channel (403)", fun() ->
                    ?assertMatch(
                        {error, {403, _}},
                        channel_logic:create_channel(
                            900003, <<"ws-ch">>, #{}, 20, {<<"workspace">>, ?WS_ID}
                        )
                    )
                end},
                {"workspace scope without workspace_id is 400", fun() ->
                    ?assertMatch(
                        {error, {400, _}},
                        channel_logic:create_channel(
                            ?UID, <<"ws-ch">>, #{}, 20, {<<"workspace">>, 0}
                        )
                    )
                end},
                {"invalid scope value is 400", fun() ->
                    ?assertMatch(
                        {error, {400, _}},
                        channel_logic:create_channel(?UID, <<"ch">>, #{}, 20, {<<"org">>, ?WS_ID})
                    )
                end},
                {"owner/member creates workspace channel with scope fields", fun() ->
                    ?assertMatch(
                        {ok, _},
                        channel_logic:create_channel(
                            ?UID, <<"ws-ch">>, #{}, 20, {<<"workspace">>, ?WS_ID}
                        )
                    ),
                    receive
                        {legacy_create, ?UID, <<"ws-ch">>, Opts, 20} ->
                            ?assertEqual(<<"workspace">>, maps:get(scope, Opts)),
                            ?assertEqual(?WS_ID, maps:get(workspace_id, Opts))
                    after 500 -> ?assert(false, "create not delegated")
                    end
                end},
                {"personal default path is zero-change (no role check)", fun() ->
                    ?assertMatch(
                        {ok, _},
                        channel_logic:create_channel(
                            ?UID, <<"my-ch">>, #{}, 20, {<<"personal">>, 0}
                        )
                    ),
                    receive
                        {legacy_create, ?UID, <<"my-ch">>, _, 20} -> ok
                    after 500 -> ?assert(false)
                    end,
                    receive
                        {ensure_create, _, _} ->
                            ?assert(false, "personal must not check workspace role")
                    after 0 -> ok
                    end,
                    %% personal 路径不注入 scope 键（落 DB 默认值）
                    receive
                        {legacy_create, _, _, Opts2, _} ->
                            ?assertEqual(false, maps:is_key(scope, Opts2)),
                            ?assertEqual(false, maps:is_key(workspace_id, Opts2))
                    after 0 -> ok
                    end
                end}
            ]
        end
    ).

update_rejects_scope_mutation_test_() ->
    ?WITH_MECKS(
        [
            {channel_logic, [
                %% 不该走到真实 update_channel（meck 拦截防 DB 访问）
                {'update_channel', 3, fun(_, _, _) -> {error, must_not_reach} end}
            ]},
            {channel_logic_subscription, [
                {'update_channel', 3, fun(_, _, _) -> {error, must_not_reach} end}
            ]}
        ],
        fun() ->
            [
                {"scope immutable on update (400)", fun() ->
                    ?assertMatch(
                        {error, {400, _}},
                        channel_logic:update_channel_checked(?UID, <<"1">>, #{
                            <<"scope">> => <<"workspace">>
                        })
                    )
                end},
                {"workspace_id immutable on update (400)", fun() ->
                    ?assertMatch(
                        {error, {400, _}},
                        channel_logic:update_channel_checked(
                            ?UID, <<"1">>, #{<<"workspace_id">> => 123}
                        )
                    )
                end}
            ]
        end
    ).

list_workspace_channels_partitions_by_scope_test_() ->
    Self = self(),
    ?WITH_MECKS(
        [
            {channel_repo, [
                {'tablename', 0, fun() -> <<"public.channel">> end}
            ]},
            {elib_pg, [
                {'query', 2, fun(Sql, [?WS_ID, Limit]) ->
                    Self ! {list_sql, Sql, ?WS_ID, Limit},
                    {ok, [#{<<"id">> => ?CID, <<"name">> => <<"Announcements">>}]}
                end}
            ]},
            {channel_logic_common, [
                {'channel_transfer', 1, fun(C) -> C#{<<"transferred">> => true} end}
            ]}
        ],
        fun() ->
            {"workspace channel list filters scope strictly", fun() ->
                ?assertMatch(
                    {ok, [#{<<"transferred">> := true}]},
                    channel_logic:list_workspace_channels(?WS_ID, 50)
                ),
                receive
                    {list_sql, Sql, ?WS_ID, 50} ->
                        ?assert(binary:match(Sql, <<"scope = 'workspace'">>) =/= nomatch),
                        ?assert(binary:match(Sql, <<"workspace_id = $1">>) =/= nomatch)
                after 500 -> ?assert(false)
                end
            end}
        end
    ).
