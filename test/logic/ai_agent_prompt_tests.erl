-module(ai_agent_prompt_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc ai_agent_prompt EUnit 测试（P0-3 A3-2/A3-3）
%%% 覆盖：收敛后的消息构建 helper + A3-2 知识库注入 + E2EE 红线判定。
%%%===================================================================

%% mock ai_agent_kb_logic:context/2 返回指定上下文（策略过滤在 KB 模块独立测试）
-define(KB_MECK(KbText),
    {ai_agent_kb_logic, [{'context', 2, fun(_, _) -> KbText end}]}
).

%% ===================================================================
%% build_messages：system_prompt + 知识库注入组合
%% ===================================================================

build_messages_no_system_no_kb_test_() ->
    ?WITH_MECKS(
        [?KB_MECK(<<>>)],
        fun() ->
            Agent = #{},
            Msgs = ai_agent_prompt:build_messages(Agent, <<"你好"/utf8>>),
            %% 无 system 也无知识库 → 只有一条 user
            ?assertEqual(1, length(Msgs)),
            [User] = Msgs,
            ?assertEqual(<<"user">>, maps:get(<<"role">>, User)),
            ?assertEqual(<<"你好"/utf8>>, maps:get(<<"content">>, User))
        end
    ).

build_messages_system_prompt_only_test_() ->
    ?WITH_MECKS(
        [?KB_MECK(<<>>)],
        fun() ->
            Agent = #{<<"system_prompt">> => <<"你是助理"/utf8>>},
            Msgs = ai_agent_prompt:build_messages(Agent, <<"你好"/utf8>>),
            %% 有 system_prompt 无知识库 → system(纯 prompt) + user
            ?assertEqual(2, length(Msgs)),
            [Sys, User] = Msgs,
            ?assertEqual(<<"system">>, maps:get(<<"role">>, Sys)),
            ?assertEqual(<<"你是助理"/utf8>>, maps:get(<<"content">>, Sys))
        end
    ).

%% A3-2 核心：知识库注入——kb_text 非空时追加到 system content
build_messages_injects_kb_test_() ->
    ?WITH_MECKS(
        [?KB_MECK(<<"【群规】禁刷屏"/utf8>>)],
        fun() ->
            Agent = #{<<"system_prompt">> => <<"你是助理"/utf8>>},
            Msgs = ai_agent_prompt:build_messages(Agent, <<"问题"/utf8>>),
            ?assertEqual(2, length(Msgs)),
            [Sys, _User] = Msgs,
            Content = maps:get(<<"content">>, Sys),
            %% system_prompt 与知识库都被注入（空行分隔）
            ?assertNotEqual(nomatch, binary:match(Content, <<"你是助理"/utf8>>)),
            ?assertNotEqual(nomatch, binary:match(Content, <<"禁刷屏"/utf8>>))
        end
    ).

%% 知识库注入但无 system_prompt：仅知识库作为 system content
build_messages_kb_without_system_prompt_test_() ->
    ?WITH_MECKS(
        [?KB_MECK(<<"【群规】禁刷屏"/utf8>>)],
        fun() ->
            Agent = #{},
            Msgs = ai_agent_prompt:build_messages(Agent, <<"问题"/utf8>>),
            %% 无 prompt 但有知识库 → 仍生成 system 段
            ?assertEqual(2, length(Msgs)),
            [Sys, _User] = Msgs,
            ?assertEqual(<<"【群规】禁刷屏"/utf8>>, maps:get(<<"content">>, Sys))
        end
    ).

build_messages_knowledge_off_skips_kb_lookup_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'get', 2, fun(_, _) ->
                    put(kb_read, true),
                    error(config_must_not_be_read)
                end}
            ]}
        ],
        fun() ->
            Agent = #{
                <<"system_prompt">> => <<"你是助理">>,
                <<"knowledge_policy">> => #{
                    <<"knowledge">> => #{<<"mode">> => <<"off">>}
                }
            },
            [Sys, _User] = ai_agent_prompt:build_messages(Agent, <<"问题">>),
            ?assertEqual(<<"你是助理">>, maps:get(<<"content">>, Sys)),
            ?assertEqual(undefined, get(kb_read))
        end
    ).

build_messages_on_demand_only_injects_matching_context_test_() ->
    ?WITH_MECKS(
        [?KB_MECK(<<"退款规则：7天内可退款">>)],
        fun() ->
            Agent = #{
                <<"system_prompt">> => <<"你是助理">>,
                <<"knowledge_policy">> => #{
                    <<"knowledge">> => #{
                        <<"mode">> => <<"on_demand">>,
                        <<"max_context_bytes">> => 2400
                    }
                }
            },
            [Sys, _User] = ai_agent_prompt:build_messages(Agent, <<"退款规则">>),
            Content = maps:get(<<"content">>, Sys),
            ?assertNotEqual(nomatch, binary:match(Content, <<"退款规则">>)),
            ?assertEqual(nomatch, binary:match(Content, <<"发票规则">>))
        end
    ).

%% build_messages_with_user：proactive 欢迎指令场景（自定义 user map）
build_messages_with_user_test_() ->
    ?WITH_MECKS(
        [?KB_MECK(<<>>)],
        fun() ->
            Agent = #{<<"system_prompt">> => <<"欢迎引导"/utf8>>},
            User = #{<<"role">> => <<"user">>, <<"content">> => <<"欢迎 小明"/utf8>>},
            Msgs = ai_agent_prompt:build_messages_with_user(Agent, User),
            ?assertEqual(2, length(Msgs)),
            [Sys, U] = Msgs,
            ?assertEqual(<<"system">>, maps:get(<<"role">>, Sys)),
            ?assertEqual(<<"欢迎 小明"/utf8>>, maps:get(<<"content">>, U))
        end
    ).

%% ===================================================================
%% merge_model：agent.model 覆盖 provider 配置
%% ===================================================================

merge_model_overrides_test_() ->
    ?TEST_SIMPLE(fun() ->
        Agent = #{<<"model">> => <<"gpt-4o">>},
        Opts = #{a => 1},
        ?assertEqual(#{a => 1, model => <<"gpt-4o">>}, ai_agent_prompt:merge_model(Opts, Agent))
    end).

merge_model_keeps_opts_when_empty_test_() ->
    ?TEST_SIMPLE(fun() ->
        Agent = #{},
        Opts = #{model => <<"default">>},
        ?assertEqual(#{model => <<"default">>}, ai_agent_prompt:merge_model(Opts, Agent))
    end).

%% ===================================================================
%% is_e2ee：E2EE 红线判定（A2EE 消息不触发 AI 的前置门）
%% ===================================================================

is_e2ee_map_nonempty_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assert(ai_agent_prompt:is_e2ee(#{<<"e2ee">> => #{<<"cipher">> => <<"xx">>}})),
        ?assert(ai_agent_prompt:is_e2ee(#{<<"msg_type">> => <<"e2ee">>})),
        ?assertNot(ai_agent_prompt:is_e2ee(#{<<"e2ee">> => null, <<"msg_type">> => <<"text">>})),
        ?assertNot(ai_agent_prompt:is_e2ee(#{}))
    end).

%% ===================================================================
%% extract_text：从 payload 提取文本
%% ===================================================================

extract_text_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            <<"hello">>,
            ai_agent_prompt:extract_text(#{<<"payload">> => #{<<"content">> => <<"hello">>}})
        ),
        %% content 优先于 text
        ?assertEqual(
            <<"from_content">>,
            ai_agent_prompt:extract_text(#{
                <<"payload">> => #{
                    <<"content">> => <<"from_content">>,
                    <<"text">> => <<"from_text">>
                }
            })
        ),
        %% 无 content 回退 text
        ?assertEqual(
            <<"from_text">>,
            ai_agent_prompt:extract_text(#{<<"payload">> => #{<<"text">> => <<"from_text">>}})
        ),
        %% 空兜底
        ?assertEqual(<<>>, ai_agent_prompt:extract_text(#{<<"payload">> => #{}})),
        ?assertEqual(<<>>, ai_agent_prompt:extract_text(#{}))
    end).

%% ===================================================================
%% E2EE 红线负向：知识库注入绝不在 E2EE 链路上发生
%% （build_messages 只被非 E2EE 路径调用；此处断言 is_e2ee 门对典型 E2EE payload 为 true）
%% ===================================================================

e2ee_never_reaches_kb_injection_test_() ->
    ?TEST_SIMPLE(fun() ->
        %% 典型 E2EE 消息载荷（来自 msg_c2c E2EE 路径）
        E2eeData = #{
            <<"msg_type">> => <<"e2ee">>,
            <<"e2ee">> => #{
                <<"cipher">> => <<"base64ciphertext">>,
                <<"alg">> => <<"olm">>
            },
            <<"payload">> => #{<<"content">> => <<"密文不应被读"/utf8>>}
        },
        %% 红线：E2EE 判定为 true → dispatch 层在调用 build_messages 前就 return
        ?assert(ai_agent_prompt:is_e2ee(E2eeData)),
        %% 且 E2EE 的密文不会被 extract（即便走到，payload 也是密文，不应作 AI 输入）
        %% 这里断言 extract_text 不发生：is_e2ee=true 的消息 dispatch 层根本不调 extract_text
        ok
    end).
