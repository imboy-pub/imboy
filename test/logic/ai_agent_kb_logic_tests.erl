-module(ai_agent_kb_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc ai_agent_kb_logic EUnit 测试（P0-3 A3-1）
%%% 覆盖：知识库配置 get/put（白名单 + 类型校验 + 半量更新 + 读写回环）；
%%%       kb_text 注入文本拼装 + enabled 门控 + 故障安全。
%%%===================================================================

%% ===================================================================
%% get_config/0：读全量键
%% ===================================================================

get_config_returns_all_keys_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'get', 2, fun
                    (<<"ai_agent.kb.enabled">>, _) -> true;
                    (<<"ai_agent.kb.group_rule">>, _) -> <<"禁止刷屏"/utf8>>;
                    (<<"ai_agent.kb.faq">>, _) -> <<"Q: 怎么建群？"/utf8>>
                end}
            ]}
        ],
        fun() ->
            Cfg = ai_agent_kb_logic:get_config(),
            ?assertEqual(true, maps:get(<<"enabled">>, Cfg)),
            ?assertEqual(<<"禁止刷屏"/utf8>>, maps:get(<<"group_rule">>, Cfg)),
            ?assertEqual(<<"Q: 怎么建群？"/utf8>>, maps:get(<<"faq">>, Cfg))
        end
    ).

%% ===================================================================
%% put_config/1：半量写入白名单键；未知键忽略
%% ===================================================================

put_config_writes_whitelisted_keys_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'get', 2, fun(_, D) -> D end},
                {'set', 2, fun(_, _) -> ok end}
            ]}
        ],
        fun() ->
            %% 半量更新：只写传入键；未知键忽略不报错
            {ok, _} = ai_agent_kb_logic:put_config(#{
                <<"enabled">> => true,
                <<"group_rule">> => <<"禁止刷屏"/utf8>>,
                <<"unknown_key">> => <<"x">>
            }),
            ?assert(meck:called(config_ds, set, [<<"ai_agent.kb.enabled">>, true])),
            ?assert(meck:called(config_ds, set, [<<"ai_agent.kb.group_rule">>, <<"禁止刷屏"/utf8>>])),
            %% set 调用数 = 白名单键数（2），未知键不触发 set
            ?assertEqual(2, length(meck:history(config_ds)))
        end
    ).

%% ===================================================================
%% 读写回环：put 后 get 能读回（验证键映射一致性）
%% ===================================================================

put_then_get_roundtrip_test_() ->
    %% 用进程字典模拟 config 表存储，验证 put 写的键正是 get 读的键
    MeckConfig = [
        {config_ds, [
            {'get', 2, fun(Key, Default) ->
                case get({cfg, Key}) of
                    undefined -> Default;
                    Val -> Val
                end
            end},
            {'set', 2, fun(Key, Val) ->
                put({cfg, Key}, Val),
                ok
            end}
        ]}
    ],
    ?WITH_MECKS(MeckConfig, fun() ->
        %% 初始 get = 全默认
        C0 = ai_agent_kb_logic:get_config(),
        ?assertEqual(false, maps:get(<<"enabled">>, C0)),
        ?assertEqual(<<>>, maps:get(<<"group_rule">>, C0)),
        %% put 写入
        {ok, _} = ai_agent_kb_logic:put_config(#{
            <<"enabled">> => true,
            <<"group_rule">> => <<"禁止刷屏"/utf8>>,
            <<"faq">> => <<"Q:建群"/utf8>>
        }),
        %% get 读回写入值
        C1 = ai_agent_kb_logic:get_config(),
        ?assertEqual(true, maps:get(<<"enabled">>, C1)),
        ?assertEqual(<<"禁止刷屏"/utf8>>, maps:get(<<"group_rule">>, C1)),
        ?assertEqual(<<"Q:建群"/utf8>>, maps:get(<<"faq">>, C1))
    end).

%% ===================================================================
%% put_config/1：类型校验（任一非法零写入）
%% ===================================================================

put_config_validates_types_test_() ->
    ?WITH_MECKS(
        [{config_ds, [{'set', 2, fun(_, _) -> ok end}]}],
        fun() ->
            %% enabled 非 boolean
            ?assertMatch(
                {error, _},
                ai_agent_kb_logic:put_config(#{<<"enabled">> => <<"yes">>})
            ),
            %% group_rule 非 binary
            ?assertMatch(
                {error, _},
                ai_agent_kb_logic:put_config(#{<<"group_rule">> => 123})
            ),
            %% faq 超 8000 字节
            ?assertMatch(
                {error, _},
                ai_agent_kb_logic:put_config(#{
                    <<"faq">> => binary:copy(<<"a">>, 8001)
                })
            ),
            %% 全部非法：零写入
            ?assertNot(meck:called(config_ds, set, '_'))
        end
    ).

%% ===================================================================
%% kb_text/0：enabled=false → 空；enabled=true 拼装知识库文本
%% ===================================================================

kb_text_disabled_returns_empty_test_() ->
    ?WITH_MECKS(
        [{config_ds, [{'get', 2, fun(_, D) -> D end}]}],
        fun() ->
            %% 默认 enabled=false → <<>>
            ?assertEqual(<<>>, ai_agent_kb_logic:kb_text())
        end
    ).

kb_text_assembles_both_sections_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'get', 2, fun
                    (<<"ai_agent.kb.enabled">>, _) -> true;
                    (<<"ai_agent.kb.group_rule">>, _) -> <<"禁止刷屏"/utf8>>;
                    (<<"ai_agent.kb.faq">>, _) -> <<"Q:怎么建群"/utf8>>
                end}
            ]}
        ],
        fun() ->
            Text = ai_agent_kb_logic:kb_text(),
            ?assert(is_binary(Text)),
            %% 两段都含分隔标记
            ?assertNotEqual(nomatch, binary:match(Text, <<"群规"/utf8>>)),
            ?assertNotEqual(nomatch, binary:match(Text, <<"常见问答"/utf8>>)),
            ?assertNotEqual(nomatch, binary:match(Text, <<"禁止刷屏"/utf8>>))
        end
    ).

kb_text_only_group_rule_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'get', 2, fun
                    (<<"ai_agent.kb.enabled">>, _) -> true;
                    (<<"ai_agent.kb.group_rule">>, _) -> <<"禁止刷屏"/utf8>>;
                    (<<"ai_agent.kb.faq">>, _) -> <<>>
                end}
            ]}
        ],
        fun() ->
            Text = ai_agent_kb_logic:kb_text(),
            %% 只有群规，不含 FAQ 段
            ?assertNotEqual(nomatch, binary:match(Text, <<"禁止刷屏"/utf8>>)),
            ?assertEqual(nomatch, binary:match(Text, <<"常见问答"/utf8>>))
        end
    ).

kb_text_both_empty_returns_empty_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'get', 2, fun
                    (<<"ai_agent.kb.enabled">>, _) -> true;
                    (<<"ai_agent.kb.group_rule">>, _) -> <<>>;
                    (<<"ai_agent.kb.faq">>, _) -> <<>>
                end}
            ]}
        ],
        fun() ->
            %% enabled 但两段全空 → <<>>（注入点据此跳过）
            ?assertEqual(<<>>, ai_agent_kb_logic:kb_text())
        end
    ).

%% ===================================================================
%% kb_text/0：config 读取异常恒返回 <<>>（不拖垮回复主链路）
%% ===================================================================

kb_text_config_crash_safe_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [{'get', 2, fun(_, _) -> error(config_table_down) end}]},
            {elib_log, [{'internal_log', 5, fun(_, _, _, _, _) -> ok end}]}
        ],
        fun() ->
            ?assertEqual(<<>>, ai_agent_kb_logic:kb_text())
        end
    ).
