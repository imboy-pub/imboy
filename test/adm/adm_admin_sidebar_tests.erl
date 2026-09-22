-module(adm_admin_sidebar_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc
%%% EADM-02 企业管理菜单契约测试
%%%
%%% 权威源: W0 冻结合同 C1 表（MENU_CONTRACT 比较 =
%%% 归一化后的 path + permission + roles 集合双向匹配）。
%%% 纯函数测试，仅调用 default_sidebar_config/0，不依赖 DB。
%%%===================================================================

%% C1 冻结表: {Label, Path, Permission, Roles}
-define(C1_LEAVES, [
    {<<"企业组织"/utf8>>, <<"/organizations">>, <<"organizations:read">>, [1, 2, 3]},
    {<<"客服开通"/utf8>>, <<"/customer-service/provisioning">>, <<"customer_service:write">>, [1, 2]},
    {<<"客服坐席"/utf8>>, <<"/customer-service">>, <<"customer_service:read">>, [1, 2]},
    {<<"客服会话"/utf8>>, <<"/customer-service/sessions">>, <<"customer_service:read">>, [1, 2]},
    {<<"Widget 接入"/utf8>>, <<"/customer-service/widgets">>, <<"customer_service:read">>, [1, 2]},
    {<<"企业业务数据"/utf8>>, <<"/enterprise-business">>, <<"enterprise_business:read">>, [1, 2]},
    {<<"离岗交接"/utf8>>, <<"/enterprise-business/offboarding">>, <<"enterprise_business:read">>, [
        1, 2
    ]},
    {<<"坐席工作台"/utf8>>, <<"/customer-service/workspace">>, <<"customer_service:read">>, [1, 2]}
]).

sidebar_config() ->
    adm_admin_handler:default_sidebar_config().

sidebar_items() ->
    maps:get(<<"items">>, sidebar_config()).

%% 取唯一「企业管理」组；不存在或多于一个即显式报错
enterprise_group() ->
    Groups = [I || I = #{<<"label">> := <<"企业管理"/utf8>>} <- sidebar_items()],
    case Groups of
        [Group] -> Group;
        Count -> erlang:error({enterprise_group_count_unexpected, Count})
    end.

enterprise_children() ->
    maps:get(<<"children">>, enterprise_group()).

%% 摘取契约三元组 {path, permission, roles}（roles 归一化排序，与
%% MENU_CONTRACT「roles 数字归一为 int 后集合比较」口径一致）
contract_triples(Children) ->
    lists:usort([
        {
            maps:get(<<"path">>, C),
            maps:get(<<"permission">>, C),
            lists:usort(maps:get(<<"roles">>, C))
        }
     || C <- Children
    ]).

%%--------------------------------------------------------------------
%% 1. 「企业管理」顶级组存在，恰 8 个叶子
%%--------------------------------------------------------------------

sidebar_enterprise_group_exists_test_() ->
    [
        ?_assertMatch(#{<<"children">> := [_ | _]}, enterprise_group()),
        ?_assertEqual(8, length(enterprise_children()))
    ].

%%--------------------------------------------------------------------
%% 2. 8 叶子逐条精确匹配（label + path + permission + roles，含顺序）
%%--------------------------------------------------------------------

sidebar_enterprise_leaves_exact_test_() ->
    Children = enterprise_children(),
    Actual = [
        {
            maps:get(<<"label">>, C),
            maps:get(<<"path">>, C),
            maps:get(<<"permission">>, C),
            maps:get(<<"roles">>, C)
        }
     || C <- Children
    ],
    [
        ?_assertEqual(?C1_LEAVES, Actual)
    ].

%%--------------------------------------------------------------------
%% 3. MENU_CONTRACT 双向集合匹配：归一化 (path, permission, roles) 集合
%%    实际 ⊆ 期望 且 期望 ⊆ 实际（等价即双向成立）
%%--------------------------------------------------------------------

sidebar_enterprise_contract_bidirectional_test_() ->
    ActualSet = contract_triples(enterprise_children()),
    ExpectedSet = lists:usort([
        {Path, Permission, lists:usort(Roles)}
     || {_, Path, Permission, Roles} <- ?C1_LEAVES
    ]),
    [
        ?_assertEqual(ExpectedSet, ActualSet),
        ?_assertEqual([], ActualSet -- ExpectedSet),
        ?_assertEqual([], ExpectedSet -- ActualSet)
    ].

%%--------------------------------------------------------------------
%% 4. 结构一致性：每个叶子含全部 binary key（path/icon/label/roles/permission），
%%    roles 为整数数组、permission 为 binary
%%--------------------------------------------------------------------

sidebar_enterprise_leaf_structure_test_() ->
    Children = enterprise_children(),
    [
        ?_assertEqual(
            [true],
            lists:usort([
                begin
                    Keys = [<<"path">>, <<"icon">>, <<"label">>, <<"roles">>, <<"permission">>],
                    ValuesOk =
                        is_binary(maps:get(<<"path">>, C)) andalso
                            is_binary(maps:get(<<"icon">>, C)) andalso
                            is_binary(maps:get(<<"label">>, C)) andalso
                            is_binary(maps:get(<<"permission">>, C)) andalso
                            lists:all(fun erlang:is_integer/1, maps:get(<<"roles">>, C)),
                    lists:all(fun(K) -> maps:is_key(K, C) end, Keys) andalso ValuesOk
                end
             || C <- Children
            ])
        )
    ].

%%--------------------------------------------------------------------
%% 5. 旧菜单组保留：顶级组与既有叶子完全不动
%%--------------------------------------------------------------------

sidebar_legacy_top_level_preserved_test_() ->
    Labels = [maps:get(<<"label">>, I) || I <- sidebar_items()],
    [
        ?_assertEqual(
            [
                <<"仪表盘"/utf8>>,
                <<"运营中心"/utf8>>,
                <<"治理中心"/utf8>>,
                <<"审计中心"/utf8>>,
                <<"企业管理"/utf8>>,
                <<"系统配置"/utf8>>
            ],
            Labels
        )
    ].

sidebar_legacy_children_preserved_test_() ->
    Config = sidebar_config(),
    ChildrenByGroup = fun(Label) ->
        ChildrenOf = fun(I) ->
            case maps:find(<<"children">>, I) of
                {ok, C} -> C;
                error -> []
            end
        end,
        [
            Child
         || Group <- [G || G = #{<<"label">> := L} <- maps:get(<<"items">>, Config), L =:= Label],
            Child <- ChildrenOf(Group)
        ]
    end,
    LegacyLeafPaths = fun(Label) ->
        [maps:get(<<"path">>, C) || C <- ChildrenByGroup(Label), maps:is_key(<<"path">>, C)]
    end,
    [
        ?_assertEqual(
            [<<"/users">>, <<"/groups">>, <<"/channels">>, <<"/moments">>],
            LegacyLeafPaths(<<"运营中心"/utf8>>)
        ),
        ?_assertEqual([<<"/reports">>, <<"/feedback">>], LegacyLeafPaths(<<"治理中心"/utf8>>)),
        ?_assertEqual(
            [<<"/groups/context">>, <<"/messages">>, <<"/logout-applications">>, <<"/logs">>],
            LegacyLeafPaths(<<"审计中心"/utf8>>)
        ),
        ?_assertEqual(
            [
                <<"/settings">>,
                <<"/settings/product-experience">>,
                <<"/admins">>,
                <<"/roles">>,
                <<"/plugins">>,
                <<"/storage">>,
                <<"/system-health">>
            ],
            LegacyLeafPaths(<<"系统配置"/utf8>>)
        ),
        %% 仪表盘为顶级叶子（无 children）
        ?_assertEqual(
            #{<<"path">> => <<"/dashboard">>, <<"permission">> => <<"dashboard:view">>},
            maps:with([<<"path">>, <<"permission">>], hd(sidebar_items()))
        ),
        %% 「工作区管理」保留在运营中心语义不变：运营中心仍含 4 个既有叶子
        ?_assertEqual(4, length(ChildrenByGroup(<<"运营中心"/utf8>>)))
    ].

%%--------------------------------------------------------------------
%% 6. 禁止项守护：不得新增"企业总览"大屏 / Application/Credential/Grant 菜单
%%--------------------------------------------------------------------

sidebar_enterprise_forbidden_entries_absent_test_() ->
    AllPaths = lists:usort([
        maps:get(<<"path">>, C)
     || I <- sidebar_items(),
        C <-
            case maps:find(<<"children">>, I) of
                {ok, Cs} -> Cs;
                error -> []
            end
    ]),
    Forbidden = [
        <<"/enterprise-overview">>,
        <<"/enterprise/overview">>,
        <<"/applications">>,
        <<"/credentials">>,
        <<"/grants">>
    ],
    [
        ?_assertEqual([], [P || P <- Forbidden, lists:member(P, AllPaths)])
    ].
