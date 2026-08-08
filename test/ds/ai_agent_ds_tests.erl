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
%% ===================================================================

create_ok_promotes_user_and_binds_test_() ->
    ?WITH_MECKS(
        [
            {elib_tsid, [{'generate', 0, fun() -> 999 end}]},
            {user_repo, [
                {'create', 1, fun(_) -> ok end},
                {'update', 2, fun(_Uid, _Data) -> {ok, 1} end}
            ]},
            {ai_agent_repo, [
                {'upsert', 1, fun(#{user_id := Uid}) -> {ok, [#{<<"user_id">> => Uid}]} end}
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
            %% account_type 被标记为 1（agent）
            ?assert(meck:called(user_repo, update, [Uid, #{account_type => 1}])),
            %% ai_agent 绑定被调用（provider 透传，trigger_policy 编码为 JSON binary）
            ?assert(meck:called(ai_agent_repo, upsert, '_'))
        end
    ).

create_rejects_empty_nickname_test_() ->
    ?WITH_MECKS(
        [{user_repo, [{'create', 1, fun(_) -> ok end}]}],
        fun() ->
            ?assertEqual(
                {error, <<"nickname 不能为空"/utf8>>},
                ai_agent_ds:create(#{<<"provider">> => <<"qianfan">>})
            ),
            %% 校验失败不应触碰建号
            ?assertNot(meck:called(user_repo, create, '_'))
        end
    ).

create_rejects_empty_provider_test_() ->
    ?WITH_MECKS(
        [{user_repo, [{'create', 1, fun(_) -> ok end}]}],
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
                {'upsert', 1, fun(#{user_id := Uid}) -> {ok, [#{<<"user_id">> => Uid}]} end}
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
                {'upsert', 1, fun(#{user_id := Uid}) -> {ok, [#{<<"user_id">> => Uid}]} end}
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
                {'upsert', 1, fun(#{user_id := Uid}) -> {ok, [#{<<"user_id">> => Uid}]} end}
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
            %% 5 个新字段全部透传到 repo upsert（capabilities 编码为 JSON binary，
            %% temperature 透传数值）
            ?assert(
                meck:called(ai_agent_repo, upsert, [
                    #{
                        user_id => 7,
                        provider => <<"bailian">>,
                        model => <<"qwen-flash">>,
                        role_id => <<"doctor">>,
                        system_prompt => <<"你是医生"/utf8>>,
                        owner_uid => 0,
                        trigger_policy => <<"{}">>,
                        status => 1,
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
                {'upsert', 1, fun(#{user_id := Uid}) -> {ok, [#{<<"user_id">> => Uid}]} end}
            ]}
        ],
        fun() ->
            {ok, _} = ai_agent_ds:update(7, #{
                <<"provider">> => <<"bailian">>,
                <<"avatar">> => <<"https://s3.example.com/u7/avatar.png">>
            }),
            ?assert(meck:called(user_repo, update, [
                7,
                #{avatar => <<"https://s3.example.com/u7/avatar.png">>}
            ]))
        end
    ).

update_skips_avatar_when_absent_or_blank_test_() ->
    ?WITH_MECKS(
        [
            {user_repo, [{'update', 2, fun(_, _) -> {ok, 1} end}]},
            {ai_agent_repo, [
                {'upsert', 1, fun(#{user_id := Uid}) -> {ok, [#{<<"user_id">> => Uid}]} end}
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
                {'set', 2, fun(<<"ai_roles">>, Map) -> put(saved_roles, Map), ok end}
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
                {'set', 2, fun(<<"ai_roles">>, Map) -> put(saved_roles, Map), ok end}
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
                {'set', 2, fun(<<"ai_roles">>, Map) -> put(saved_roles, Map), ok end}
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
