-module(ai_agent_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc ai_agent_ds EUnit 测试（Phase 1 T1.2）
%%% 覆盖：建号编排（建 user + 标 account_type=1 + 绑 ai_agent）、
%%%       边界校验、is_agent/1 路由判定、trigger_policy jsonb 编解码。
%%%===================================================================

%% ===================================================================
%% create/1 — 建号 + 绑定编排
%% TX-01：三步写收进 elib_pg:with_tx 单事务；meck 用例以 fake_conn 模拟
%% 事务连接（abort_tx 归一 {error, Reason}，与 elib_pg:with_tx 语义一致），
%% 真库故障注入矩阵见文件末尾 TX-01 段（?TEST_WITH_DB 直连 SQL 断言）。
%% ===================================================================

create_ok_promotes_user_and_binds_test_() ->
    ?WITH_MECKS(
        [
            {elib_tsid, [{'generate', 1, fun(user) -> 999 end}]},
            {elib_pg, [
                {'with_tx', 1, fun(Fun) ->
                    try
                        Fun(fake_conn)
                    catch
                        throw:{abort_tx, Reason} -> {error, Reason}
                    end
                end}
            ]},
            {user_repo, [
                {'create_tx', 2, fun(_Conn, #{id := 999}) -> {ok, 999} end},
                {'update_tx', 3, fun(_Conn, 999, #{account_type := 1}) -> {ok, 1} end}
            ]},
            {ai_agent_repo, [
                {'upsert_tx', 2, fun(_Conn, #{user_id := 999}) ->
                    {ok, [#{<<"user_id">> => 999}]}
                end}
            ]}
        ],
        fun() ->
            Cfg = #{
                <<"nickname">> => <<"客服助手"/utf8>>,
                <<"provider">> => <<"qianfan">>,
                <<"trigger_policy">> => #{<<"mention">> => true}
            },
            {ok, #{<<"user_id">> := Uid}} = ai_agent_ds:create(Cfg),
            ?assertEqual(999, Uid),
            %% account_type 被标记为 1（agent），同一事务连接贯穿三步
            ?assert(meck:called(user_repo, update_tx, [fake_conn, 999, #{account_type => 1}])),
            %% ai_agent 绑定被调用（provider 透传，trigger_policy 编码为 JSON binary）
            ?assert(meck:called(ai_agent_repo, upsert_tx, '_'))
        end
    ).

create_rejects_empty_nickname_test_() ->
    ?WITH_MECKS(
        [{user_repo, [{'create_tx', 2, fun(_Conn, _Data) -> {ok, 1} end}]}],
        fun() ->
            ?assertEqual(
                {error, <<"nickname 不能为空"/utf8>>},
                ai_agent_ds:create(#{<<"provider">> => <<"qianfan">>})
            ),
            %% 校验失败不应触碰建号
            ?assertNot(meck:called(user_repo, create_tx, '_'))
        end
    ).

create_rejects_empty_provider_test_() ->
    ?WITH_MECKS(
        [{user_repo, [{'create_tx', 2, fun(_Conn, _Data) -> {ok, 1} end}]}],
        fun() ->
            ?assertEqual(
                {error, <<"provider 不能为空"/utf8>>},
                ai_agent_ds:create(#{<<"nickname">> => <<"bot">>})
            )
        end
    ).

update_rejects_empty_provider_test_() ->
    ?WITH_MECKS(
        [{ai_agent_repo, [{'upsert', 1, fun(_) -> {ok, []} end}]}],
        fun() ->
            ?assertEqual(
                {error, <<"provider 不能为空"/utf8>>},
                ai_agent_ds:update(123, #{<<"model">> => <<"m1">>})
            ),
            ?assertNot(meck:called(ai_agent_repo, upsert, '_'))
        end
    ).

update_preserves_omitted_behavior_fields_test_() ->
    ?WITH_MECKS(
        [
            {ai_agent_repo, [
                {'patch', 2, fun(7, Data) ->
                    ?assertEqual(
                        #{
                            provider => <<"qianfan">>,
                            greeting => <<"新的欢迎语"/utf8>>
                        },
                        Data
                    ),
                    {ok, [#{<<"user_id">> => 7}]}
                end}
            ]}
        ],
        fun() ->
            ?assertMatch(
                {ok, _},
                ai_agent_ds:update(7, #{
                    <<"provider">> => <<"qianfan">>,
                    <<"greeting">> => <<"新的欢迎语"/utf8>>
                })
            )
        end
    ).

%% ===================================================================
%% is_agent/1 — 消息路由判定
%% ===================================================================

is_agent_returns_config_for_active_test_() ->
    ?WITH_MECKS(
        [
            {ai_agent_repo, [
                {'find', 1, fun(42) ->
                    {ok, #{
                        <<"user_id">> => 42,
                        <<"provider">> => <<"qianfan">>,
                        <<"status">> => 1,
                        <<"trigger_policy">> => <<"{\"mention\":true}">>
                    }}
                end}
            ]}
        ],
        fun() ->
            {true, Agent} = ai_agent_ds:is_agent(42),
            ?assertEqual(<<"qianfan">>, maps:get(<<"provider">>, Agent)),
            %% jsonb 解码为 map
            ?assertEqual(#{<<"mention">> => true}, maps:get(<<"trigger_policy">>, Agent))
        end
    ).

is_agent_false_for_disabled_test_() ->
    ?WITH_MECKS(
        [{ai_agent_repo, [{'find', 1, fun(_) -> {ok, #{<<"status">> => 0}} end}]}],
        fun() ->
            ?assertEqual(false, ai_agent_ds:is_agent(42))
        end
    ).

is_agent_false_for_notfound_test_() ->
    ?WITH_MECKS(
        [{ai_agent_repo, [{'find', 1, fun(_) -> {error, notfound} end}]}],
        fun() ->
            ?assertEqual(false, ai_agent_ds:is_agent(99))
        end
    ).

is_agent_inherits_published_role_behavior_test_() ->
    ?WITH_MECKS(
        [
            {ai_agent_repo, [
                {'find', 1, fun(42) ->
                    {ok, #{
                        <<"user_id">> => 42,
                        <<"provider">> => <<"qianfan">>,
                        <<"model">> => <<"qwen-flash">>,
                        <<"role_id">> => <<"doctor">>,
                        <<"system_prompt">> => <<"legacy prompt">>,
                        <<"capabilities">> => <<"{\"knowledge\":false}">>,
                        <<"status">> => 1
                    }}
                end}
            ]},
            {ai_agent_role_repo, [
                {'find_published', 1, fun(<<"doctor">>) ->
                    {ok, #{
                        <<"code">> => <<"doctor">>,
                        <<"active_version">> => 2,
                        <<"version">> => 2,
                        <<"system_prompt">> => <<"role prompt">>,
                        <<"capabilities">> => <<"{\"knowledge\":true}">>,
                        <<"knowledge_policy">> => <<"{}">>
                    }}
                end}
            ]}
        ],
        fun() ->
            {true, Agent} = ai_agent_ds:is_agent(42),
            ?assertEqual(<<"role prompt">>, maps:get(<<"system_prompt">>, Agent)),
            ?assertEqual(#{<<"knowledge">> => true}, maps:get(<<"capabilities">>, Agent)),
            ?assertEqual(<<"doctor">>, maps:get(<<"role_code">>, Agent)),
            ?assertEqual(2, maps:get(<<"role_version">>, Agent)),
            ?assertEqual(<<"role">>, maps:get(<<"policy_source">>, Agent))
        end
    ).

is_agent_ignores_unpublished_role_draft_test_() ->
    ?WITH_MECKS(
        [
            {ai_agent_repo, [
                {'find', 1, fun(42) ->
                    {ok, #{
                        <<"user_id">> => 42,
                        <<"role_id">> => <<"doctor">>,
                        <<"system_prompt">> => <<"legacy prompt">>,
                        <<"capabilities">> => <<"{\"knowledge\":false}">>,
                        <<"status">> => 1
                    }}
                end}
            ]},
            {ai_agent_role_repo, [
                {'find_published', 1, fun(<<"doctor">>) -> {error, notfound} end}
            ]}
        ],
        fun() ->
            {true, Agent} = ai_agent_ds:is_agent(42),
            ?assertEqual(<<"legacy prompt">>, maps:get(<<"system_prompt">>, Agent)),
            ?assertEqual(#{<<"knowledge">> => false}, maps:get(<<"capabilities">>, Agent))
        end
    ).

is_agent_false_when_bound_role_is_disabled_test_() ->
    ?WITH_MECKS(
        [
            {ai_agent_repo, [
                {'find', 1, fun(42) ->
                    {ok, #{
                        <<"user_id">> => 42,
                        <<"role_id">> => <<"disabled">>,
                        <<"status">> => 1
                    }}
                end}
            ]},
            {ai_agent_role_repo, [
                {'find_published', 1, fun(<<"disabled">>) ->
                    {ok, #{
                        <<"code">> => <<"disabled">>,
                        <<"version">> => 1,
                        <<"status">> => 0,
                        <<"system_prompt">> => <<"prompt">>,
                        <<"capabilities">> => <<"{}">>,
                        <<"knowledge_policy">> => <<"{}">>
                    }}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(false, ai_agent_ds:is_agent(42))
        end
    ).

%% ===================================================================
%% get/1 — trigger_policy jsonb 解码
%% ===================================================================

get_decodes_trigger_policy_test_() ->
    ?WITH_MECKS(
        [
            {ai_agent_repo, [
                {'find', 1, fun(7) ->
                    {ok, #{
                        <<"user_id">> => 7,
                        <<"trigger_policy">> => <<"{\"keywords\":[\"help\"]}">>
                    }}
                end}
            ]}
        ],
        fun() ->
            {ok, Agent} = ai_agent_ds:get(7),
            ?assertEqual(
                #{<<"keywords">> => [<<"help">>]},
                maps:get(<<"trigger_policy">>, Agent)
            )
        end
    ).

%% ===================================================================
%% update/2 — nickname 同步 user 表（agent 资料管理后台可配）
%% ===================================================================

update_syncs_nickname_when_present_test_() ->
    ?WITH_MECKS(
        [
            {user_repo, [{'update', 2, fun(_Uid, _Data) -> {ok, 1} end}]},
            {ai_agent_repo, [
                {'patch', 2, fun(_Uid, _Data) ->
                    {ok, [#{<<"user_id">> => 7}]}
                end}
            ]}
        ],
        fun() ->
            {ok, _} = ai_agent_ds:update(7, #{
                <<"provider">> => <<"qianfan">>,
                <<"nickname">> => <<"新昵称"/utf8>>
            }),
            ?assert(meck:called(user_repo, update, [7, #{nickname => <<"新昵称"/utf8>>}]))
        end
    ).

update_skips_nickname_when_absent_or_blank_test_() ->
    ?WITH_MECKS(
        [
            {user_repo, [{'update', 2, fun(_, _) -> {ok, 1} end}]},
            {ai_agent_repo, [
                {'patch', 2, fun(_Uid, _Data) ->
                    {ok, [#{<<"user_id">> => 7}]}
                end}
            ]}
        ],
        fun() ->
            %% 不带 nickname
            {ok, _} = ai_agent_ds:update(7, #{<<"provider">> => <<"qianfan">>}),
            %% 带空白 nickname
            {ok, _} = ai_agent_ds:update(7, #{
                <<"provider">> => <<"qianfan">>, <<"nickname">> => <<"  ">>
            }),
            ?assertNot(meck:called(user_repo, update, '_'))
        end
    ).

%% ===================================================================
%% update/2 — 扩展属性（category/voice_id/greeting/capabilities/temperature）
%% + avatar 同步 user 表（迁移 000057 新增 5 字段）
%% ===================================================================

update_persists_extended_fields_test_() ->
    ?WITH_MECKS(
        [
            {user_repo, [{'update', 2, fun(_, _) -> {ok, 1} end}]},
            {ai_agent_repo, [
                {'patch', 2, fun(_Uid, _Data) ->
                    {ok, [#{<<"user_id">> => 7}]}
                end}
            ]}
        ],
        fun() ->
            {ok, _} = ai_agent_ds:update(7, #{
                <<"provider">> => <<"bailian">>,
                <<"model">> => <<"qwen-flash">>,
                <<"role_id">> => <<"doctor">>,
                <<"system_prompt">> => <<"你是医生"/utf8>>,
                <<"description">> => <<"客服助手"/utf8>>,
                <<"visibility">> => 1,
                <<"category">> => <<"客服"/utf8>>,
                <<"voice_id">> => <<"xiaoyan">>,
                <<"greeting">> => <<"您好，我是客服助手"/utf8>>,
                <<"capabilities">> => #{<<"knowledge">> => true, <<"proactive">> => false},
                <<"temperature">> => 0.3
            }),
            %% 5 个新字段全部透传到 repo patch（capabilities 编码为 JSON binary，
            %% temperature 透传数值）
            ?assert(
                meck:called(ai_agent_repo, patch, [
                    7,
                    #{
                        provider => <<"bailian">>,
                        model => <<"qwen-flash">>,
                        role_id => <<"doctor">>,
                        system_prompt => <<"你是医生"/utf8>>,
                        description => <<"客服助手"/utf8>>,
                        visibility => 1,
                        category => <<"客服"/utf8>>,
                        voice_id => <<"xiaoyan">>,
                        greeting => <<"您好，我是客服助手"/utf8>>,
                        capabilities => <<"{\"knowledge\":true,\"proactive\":false}">>,
                        temperature => 0.3
                    }
                ])
            )
        end
    ).

update_syncs_avatar_when_present_test_() ->
    ?WITH_MECKS(
        [
            {user_repo, [{'update', 2, fun(_, _) -> {ok, 1} end}]},
            {ai_agent_repo, [
                {'patch', 2, fun(_Uid, _Data) ->
                    {ok, [#{<<"user_id">> => 7}]}
                end}
            ]}
        ],
        fun() ->
            {ok, _} = ai_agent_ds:update(7, #{
                <<"provider">> => <<"bailian">>,
                <<"avatar">> => <<"https://s3.example.com/u7/avatar.png">>
            }),
            ?assert(
                meck:called(user_repo, update, [
                    7,
                    #{avatar => <<"https://s3.example.com/u7/avatar.png">>}
                ])
            )
        end
    ).

update_skips_avatar_when_absent_or_blank_test_() ->
    ?WITH_MECKS(
        [
            {user_repo, [{'update', 2, fun(_, _) -> {ok, 1} end}]},
            {ai_agent_repo, [
                {'patch', 2, fun(_Uid, _Data) ->
                    {ok, [#{<<"user_id">> => 7}]}
                end}
            ]}
        ],
        fun() ->
            {ok, _} = ai_agent_ds:update(7, #{<<"provider">> => <<"bailian">>}),
            {ok, _} = ai_agent_ds:update(7, #{
                <<"provider">> => <<"bailian">>, <<"avatar">> => <<"  ">>
            }),
            ?assertNot(meck:called(user_repo, update, '_'))
        end
    ).

%% ===================================================================
%% roles/0, save_role/2, delete_role/1 — ai_roles 人格 KV 管理
%% （持久层走 config_ds get/set，与 msg_c2s_logic 的 ai_roles 消费点对齐）
%% ===================================================================

roles_returns_empty_map_when_unset_test_() ->
    ?WITH_MECKS(
        [{config_ds, [{'get', 2, fun(<<"ai_roles">>, Default) -> Default end}]}],
        fun() ->
            ?assertEqual(#{}, ai_agent_ds:roles())
        end
    ).

roles_reads_back_saved_roles_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'get', 2, fun(<<"ai_roles">>, Default) ->
                    case get(saved_roles) of
                        undefined -> Default;
                        Saved -> Saved
                    end
                end},
                {'set', 2, fun(<<"ai_roles">>, Map) ->
                    put(saved_roles, Map),
                    ok
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(#{}, ai_agent_ds:roles()),
            %% 保存两个角色后读回
            ok = ai_agent_ds:save_role(<<"doctor">>, <<"你是医生"/utf8>>),
            ok = ai_agent_ds:save_role(<<"lawyer">>, <<"你是律师"/utf8>>),
            ?assertEqual(
                #{
                    <<"doctor">> => <<"你是医生"/utf8>>,
                    <<"lawyer">> => <<"你是律师"/utf8>>
                },
                ai_agent_ds:roles()
            )
        end
    ).

save_role_overwrites_existing_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'get', 2, fun(<<"ai_roles">>, Default) ->
                    case get(saved_roles) of
                        undefined -> Default;
                        Saved -> Saved
                    end
                end},
                {'set', 2, fun(<<"ai_roles">>, Map) ->
                    put(saved_roles, Map),
                    ok
                end}
            ]}
        ],
        fun() ->
            ok = ai_agent_ds:save_role(<<"doctor">>, <<"旧版"/utf8>>),
            ok = ai_agent_ds:save_role(<<"doctor">>, <<"新版"/utf8>>),
            ?assertEqual(
                #{<<"doctor">> => <<"新版"/utf8>>},
                ai_agent_ds:roles()
            )
        end
    ).

delete_role_removes_existing_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'get', 2, fun(<<"ai_roles">>, Default) ->
                    case get(saved_roles) of
                        undefined -> Default;
                        Saved -> Saved
                    end
                end},
                {'set', 2, fun(<<"ai_roles">>, Map) ->
                    put(saved_roles, Map),
                    ok
                end}
            ]}
        ],
        fun() ->
            ok = ai_agent_ds:save_role(<<"doctor">>, <<"你是医生"/utf8>>),
            ok = ai_agent_ds:save_role(<<"lawyer">>, <<"你是律师"/utf8>>),
            ok = ai_agent_ds:delete_role(<<"doctor">>),
            ?assertEqual(
                #{<<"lawyer">> => <<"你是律师"/utf8>>},
                ai_agent_ds:roles()
            )
        end
    ).

%% ===================================================================
%% list/3 — 分类筛选透传
%% ===================================================================

list_with_category_calls_repo_test_() ->
    ?WITH_MECKS(
        [
            {ai_agent_repo, [
                {'page', 3, fun(Page, Size, <<"medical">>) ->
                    {ok, #{total => 1, page => Page, size => Size, list => [x]}}
                end}
            ]}
        ],
        fun() ->
            {ok, #{list := [x]}} = ai_agent_ds:list(1, 10, <<"medical">>),
            ?assert(meck:called(ai_agent_repo, page, [1, 10, <<"medical">>]))
        end
    ).

list_without_category_falls_back_to_page2_test_() ->
    ?WITH_MECKS(
        [
            {ai_agent_repo, [
                {'page', 2, fun(Page, Size) ->
                    {ok, #{total => 0, page => Page, size => Size, list => []}}
                end}
            ]}
        ],
        fun() ->
            {ok, #{list := []}} = ai_agent_ds:list(2, 10),
            ?assert(meck:called(ai_agent_repo, page, [2, 10]))
        end
    ).

%% ===================================================================
%% TX-01 事务收敛 — 真库直连（scratch 库）
%% 故障注入矩阵 + 正常路径 + 并发幂等；断言全部直连 SQL。
%% ===================================================================

tx01_create_success_persists_user_and_agent_rows_test_() ->
    ?TEST_WITH_DB(fun() ->
        Account = tx01_unique_account(<<"ok">>),
        tx01_cleanup_account(Account),
        {ok, #{<<"user_id">> := Uid}} =
            ai_agent_ds:create(#{
                <<"nickname">> => <<"真库 Agent"/utf8>>,
                <<"provider">> => <<"qianfan">>,
                <<"account">> => Account,
                <<"trigger_policy">> => #{<<"mention">> => true}
            }),
        %% user 行存在且 account_type=1、account 透传
        {ok, [User]} =
            elib_pg:query(
                <<"SELECT id, account, account_type, nickname FROM ",
                    (user_repo:tablename())/binary, " WHERE id = $1">>,
                [Uid]
            ),
        ?assertEqual(1, maps:get(<<"account_type">>, User)),
        ?assertEqual(Account, maps:get(<<"account">>, User)),
        %% ai_agent 绑定行存在且 provider/status 正确
        {ok, [Agent]} =
            elib_pg:query(
                <<
                    "SELECT user_id, provider, status, trigger_policy FROM ai_agent"
                    " WHERE user_id = $1"
                >>,
                [Uid]
            ),
        ?assertEqual(<<"qianfan">>, maps:get(<<"provider">>, Agent)),
        ?assertEqual(1, maps:get(<<"status">>, Agent)),
        tx01_cleanup_account(Account)
    end).

tx01_step1_user_insert_failure_leaves_zero_orphans_test_() ->
    ?TEST_WITH_DB(fun() ->
        Account = tx01_unique_account(<<"s1">>),
        tx01_cleanup_account(Account),
        {Result, Uid} = tx01_run_create_with_injection(step1, Account),
        ?assertEqual({error, <<"创建 Agent 账号失败"/utf8>>}, Result),
        tx01_assert_zero_orphans(Uid, Account),
        tx01_cleanup_account(Account)
    end).

tx01_step2_account_type_failure_leaves_zero_orphans_test_() ->
    ?TEST_WITH_DB(fun() ->
        Account = tx01_unique_account(<<"s2">>),
        tx01_cleanup_account(Account),
        {Result, Uid} = tx01_run_create_with_injection(step2, Account),
        ?assertEqual({error, <<"创建 Agent 账号失败"/utf8>>}, Result),
        tx01_assert_zero_orphans(Uid, Account),
        tx01_cleanup_account(Account)
    end).

tx01_step3_bind_failure_leaves_zero_orphans_test_() ->
    ?TEST_WITH_DB(fun() ->
        Account = tx01_unique_account(<<"s3">>),
        tx01_cleanup_account(Account),
        {Result, Uid} = tx01_run_create_with_injection(step3, Account),
        ?assertEqual({error, <<"绑定 Agent 元数据失败"/utf8>>}, Result),
        tx01_assert_zero_orphans(Uid, Account),
        tx01_cleanup_account(Account)
    end).

%% 重试幂等：第 3 步失败整体回滚后，重跑（同参数）成功且无残留
tx01_retry_after_step3_failure_succeeds_test_() ->
    ?TEST_WITH_DB(fun() ->
        Account = tx01_unique_account(<<"retry">>),
        tx01_cleanup_account(Account),
        {Result, Uid} = tx01_run_create_with_injection(step3, Account),
        ?assertEqual({error, <<"绑定 Agent 元数据失败"/utf8>>}, Result),
        tx01_assert_zero_orphans(Uid, Account),
        {ok, #{<<"user_id">> := _Uid2}} =
            ai_agent_ds:create(#{
                <<"nickname">> => <<"重试 Agent"/utf8>>,
                <<"provider">> => <<"qianfan">>,
                <<"account">> => Account
            }),
        tx01_cleanup_account(Account)
    end).

%% 并发重复：同 account（user.account 唯一约束为幂等 identifier）并发两次
%% create → 恰一个实体（一个 {ok,_}，一个整体回滚 {error,_}）
%% 并发用例真库 + spawn 往返可能超过 eunit 默认 5s：timeout 须经
%% TEST_WITH_DB_TIMEOUT 放在 setup 体内（外包 {timeout, T, ?TEST_WITH_DB(...)}
%% 不生效——带 fixture 的 group 不下推 timeout，见 eunit_setup.hrl 注释）
tx01_duplicate_account_concurrent_exactly_one_entity_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        Account = tx01_unique_account(<<"conc">>),
        tx01_cleanup_account(Account),
        Parent = self(),
        Worker = fun() ->
            Parent !
                {tx01_done,
                    ai_agent_ds:create(#{
                        <<"nickname">> => <<"并发 Agent"/utf8>>,
                        <<"provider">> => <<"qianfan">>,
                        <<"account">> => Account
                    })}
        end,
        spawn(Worker),
        spawn(Worker),
        Results = tx01_collect_results(2, []),
        OkCount = length([X || {ok, X} <- Results]),
        ?assertEqual(1, OkCount, {results, Results}),
        %% 恰一实体：user 表恰 1 行（同 account），ai_agent 表恰 1 行
        {ok, [#{<<"n">> := 1, <<"uid">> := Uid}]} =
            elib_pg:query(
                <<"SELECT id AS uid, count(*) AS n FROM ", (user_repo:tablename())/binary,
                    " WHERE account = $1 GROUP BY id">>,
                [Account]
            ),
        {ok, [#{<<"n">> := 1}]} =
            elib_pg:query(
                <<"SELECT count(*) AS n FROM ai_agent WHERE user_id = $1">>,
                [Uid]
            ),
        tx01_cleanup_account(Account)
    end).

%% ===================================================================
%% TX-01 Internal — 数据准备、断言与故障注入
%% ===================================================================

tx01_unique_account(Prefix) ->
    <<
        "tx01_agent_",
        Prefix/binary,
        "_",
        (integer_to_binary(erlang:unique_integer([positive])))/binary
    >>.

tx01_cleanup_account(Account) ->
    {ok, Uids} =
        elib_pg:query(
            <<"SELECT id FROM ", (user_repo:tablename())/binary, " WHERE account = $1">>,
            [Account]
        ),
    lists:foreach(
        fun(#{<<"id">> := Uid}) ->
            _ = elib_pg:query(<<"DELETE FROM ai_agent WHERE user_id = $1">>, [Uid]),
            _ = elib_pg:query(
                <<"DELETE FROM ", (user_repo:tablename())/binary, " WHERE id = $1">>,
                [Uid]
            )
        end,
        Uids
    ),
    ok.

%% 零孤儿断言：user 行（含 account_type 标记）与 ai_agent 行全查空（直连 SQL）
tx01_assert_zero_orphans(Uid, Account) ->
    {ok, UserRows} =
        elib_pg:query(
            <<"SELECT id, account_type FROM ", (user_repo:tablename())/binary,
                " WHERE id = $1 OR account = $2">>,
            [Uid, Account]
        ),
    ?assertEqual([], UserRows, {orphan_user_rows, UserRows}),
    {ok, AgentRows} =
        elib_pg:query(
            <<"SELECT user_id FROM ai_agent WHERE user_id = $1">>,
            [Uid]
        ),
    ?assertEqual([], AgentRows, {orphan_agent_rows, AgentRows}),
    ok.

%% 收齐 N 条结果立即返回；兜底 40s（外层 TEST_WITH_DB_TIMEOUT 60s 内）
tx01_collect_results(N, Acc) when N > 0 ->
    receive
        {tx01_done, Result} -> tx01_collect_results(N - 1, [Result | Acc])
    after 40000 ->
        lists:reverse([{tx01_timeout, N} | Acc])
    end;
tx01_collect_results(_, Acc) ->
    lists:reverse(Acc).

%% 故障注入：meck 只覆盖被 expect 的函数（passthrough 其余走真库），
%% 注入 fun 先捕获本次要写入的 user id（供零孤儿断言精确定位），再返回 error。
%% 返回 {CreateResult, CapturedUid}。
tx01_run_create_with_injection(Step, Account) ->
    Ets = ets:new(tx01_agent_capture, [public, set]),
    MeckMod =
        case Step of
            step3 -> ai_agent_repo;
            _ -> user_repo
        end,
    Expectations =
        case Step of
            step1 ->
                [
                    {'create_tx', 2, fun(_Conn, Data) ->
                        ets:insert(Ets, {uid, maps:get(id, Data)}),
                        {error, {injected, step1}}
                    end}
                ];
            step2 ->
                [
                    {'create_tx', 2, fun(Conn, Data) ->
                        Ret = meck:passthrough([Conn, Data]),
                        ets:insert(Ets, {uid, element(2, Ret)}),
                        Ret
                    end},
                    {'update_tx', 3, fun(_Conn, Id, _Data) ->
                        ets:insert(Ets, {uid, Id}),
                        {error, {injected, step2}}
                    end}
                ];
            step3 ->
                [
                    {'upsert_tx', 2, fun(_Conn, Data) ->
                        ets:insert(Ets, {uid, maps:get(user_id, Data)}),
                        {error, {injected, step3}}
                    end}
                ]
        end,
    _ = meck_helper:setup_mock(MeckMod, Expectations),
    Result =
        try
            ai_agent_ds:create(#{
                <<"nickname">> => <<"注入 Agent"/utf8>>,
                <<"provider">> => <<"qianfan">>,
                <<"account">> => Account
            })
        after
            meck_helper:cleanup_mock(MeckMod)
        end,
    [{uid, Uid}] = ets:lookup(Ets, uid),
    ets:delete(Ets),
    {Result, Uid}.
