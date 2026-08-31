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
    %% ⚠️ TestFun 须单表达式直接断言：{Desc, fun} 列表会被 ?_test 吞掉
    %% 静默空转；哨兵经进程字典传递（generator 与执行异进程，Self 收不到）
    ?WITH_MECKS(
        [
            {workspace_logic, [
                {'ensure_can_create_resource', 2, fun(WsId, Uid) ->
                    put(t_cs_ensure_create, {WsId, Uid}),
                    case {WsId, Uid} of
                        {?WS_ID, 900002} -> {error, {403, <<"Guest 角色不能创建工作区资源"/utf8>>}};
                        {?WS_ID, 900003} -> {error, {403, <<"非工作区成员，禁止访问该资源"/utf8>>}};
                        _ -> ok
                    end
                end}
            ]},
            {channel_logic_message, [
                {'create_channel', 4, fun(Uid, Name, Opts, Max) ->
                    put(t_cs_legacy_create, {Uid, Name, Opts, Max}),
                    {ok, #{<<"id">> => ?CID, <<"name">> => Name}}
                end}
            ]}
        ],
        fun() -> create_scope_validation_body() end
    ).

create_scope_validation_body() ->
    begin
        %% guest cannot create workspace channel (403)
        ?assertMatch(
            {error, {403, _}},
            channel_logic:create_channel(
                900002, <<"ws-ch">>, #{}, 20, {<<"workspace">>, ?WS_ID}
            )
        ),
        %% non workspace member cannot create workspace channel (403)
        ?assertMatch(
            {error, {403, _}},
            channel_logic:create_channel(
                900003, <<"ws-ch">>, #{}, 20, {<<"workspace">>, ?WS_ID}
            )
        ),
        %% workspace scope without workspace_id is 400
        ?assertMatch(
            {error, {400, _}},
            channel_logic:create_channel(
                ?UID, <<"ws-ch">>, #{}, 20, {<<"workspace">>, 0}
            )
        ),
        %% invalid scope value is 400
        ?assertMatch(
            {error, {400, _}},
            channel_logic:create_channel(?UID, <<"ch">>, #{}, 20, {<<"org">>, ?WS_ID})
        ),
        %% owner/member creates workspace channel with scope fields
        ?assertMatch(
            {ok, _},
            channel_logic:create_channel(
                ?UID, <<"ws-ch">>, #{}, 20, {<<"workspace">>, ?WS_ID}
            )
        ),
        {?UID, <<"ws-ch">>, Opts, 20} = erase(t_cs_legacy_create),
        ?assertEqual(<<"workspace">>, maps:get(scope, Opts)),
        ?assertEqual(?WS_ID, maps:get(workspace_id, Opts)),

        %% personal default path is zero-change (no role check)
        erase(t_cs_ensure_create),
        ?assertMatch(
            {ok, _},
            channel_logic:create_channel(
                ?UID, <<"my-ch">>, #{}, 20, {<<"personal">>, 0}
            )
        ),
        ?assert(
            undefined =:= get(t_cs_ensure_create),
            "personal must not check workspace role"
        ),
        %% personal 路径不注入 scope 键（落 DB 默认值）
        {?UID, <<"my-ch">>, Opts2, 20} = erase(t_cs_legacy_create),
        ?assertEqual(false, maps:is_key(scope, Opts2)),
        ?assertEqual(false, maps:is_key(workspace_id, Opts2)),
        ok
    end.

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
        fun() -> update_rejects_scope_mutation_body() end
    ).

update_rejects_scope_mutation_body() ->
    begin
        %% scope immutable on update (400)
        ?assertMatch(
            {error, {400, _}},
            channel_logic:update_channel_checked(?UID, <<"1">>, #{
                <<"scope">> => <<"workspace">>
            })
        ),
        %% workspace_id immutable on update (400)
        ?assertMatch(
            {error, {400, _}},
            channel_logic:update_channel_checked(
                ?UID, <<"1">>, #{<<"workspace_id">> => 123}
            )
        ),
        ok
    end.

list_workspace_channels_partitions_by_scope_test_() ->
    ?WITH_MECKS(
        [
            {channel_repo, [
                {'tablename', 0, fun() -> <<"public.channel">> end}
            ]},
            {elib_pg, [
                {'query', 2, fun(Sql, [?WS_ID, Limit]) ->
                    put(t_cs_list_sql, {Sql, Limit}),
                    {ok, [#{<<"id">> => ?CID, <<"name">> => <<"Announcements">>}]}
                end}
            ]},
            {channel_logic_common, [
                {'channel_transfer', 1, fun(C) -> C#{<<"transferred">> => true} end}
            ]}
        ],
        fun() -> list_workspace_channels_partitions_by_scope_body() end
    ).

list_workspace_channels_partitions_by_scope_body() ->
    begin
        %% workspace channel list filters scope strictly
        ?assertMatch(
            {ok, [#{<<"transferred">> := true}]},
            channel_logic:list_workspace_channels(?WS_ID, 50)
        ),
        {Sql, 50} = erase(t_cs_list_sql),
        ?assert(binary:match(Sql, <<"scope = 'workspace'">>) =/= nomatch),
        ?assert(binary:match(Sql, <<"workspace_id = $1">>) =/= nomatch),
        ok
    end.
