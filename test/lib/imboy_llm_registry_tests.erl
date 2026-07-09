-module(imboy_llm_registry_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% imboy_llm_registry 模块的 EUnit 测试
%%%
%%% 目标：验证配置驱动的 LLM provider 注册表
%%% 覆盖：配置命中（opts 剥离 name/module）、内置 qianfan 兜底、
%%%        配置覆盖内置、未知 provider、脏配置容错
%%%===================================================================

%% ===================================================================
%% 辅助函数
%% ===================================================================

setup_config(Providers) ->
    meck:new(config_ds, [no_link, passthrough]),
    meck:expect(config_ds, env, 2, fun(llm_providers, _Def) -> Providers end).

cleanup(_State) ->
    meck:unload(config_ds).

with_config(Providers, TestFun) ->
    {setup, fun() -> setup_config(Providers) end, fun cleanup/1, fun(_) -> ?_test(TestFun()) end}.

%% ===================================================================
%% lookup/1
%% ===================================================================

lookup_configured_provider_strips_name_module_test_() ->
    with_config(
        [
            #{
                name => <<"openai">>,
                module => imboy_llm_openai,
                base_url => <<"https://api.deepseek.com/v1">>,
                api_key => <<"sk-test">>,
                model => <<"deepseek-chat">>
            }
        ],
        fun() ->
            ?assertEqual(
                {ok, #{
                    module => imboy_llm_openai,
                    opts => #{
                        base_url => <<"https://api.deepseek.com/v1">>,
                        api_key => <<"sk-test">>,
                        model => <<"deepseek-chat">>
                    }
                }},
                imboy_llm_registry:lookup(<<"openai">>)
            )
        end
    ).

lookup_builtin_qianfan_without_config_test_() ->
    with_config([], fun() ->
        ?assertEqual(
            {ok, #{module => imboy_llm_qianfan, opts => #{}}},
            imboy_llm_registry:lookup(<<"qianfan">>)
        )
    end).

lookup_config_overrides_builtin_test_() ->
    with_config(
        [#{name => <<"qianfan">>, module => my_custom_qianfan}],
        fun() ->
            ?assertMatch(
                {ok, #{module := my_custom_qianfan}},
                imboy_llm_registry:lookup(<<"qianfan">>)
            )
        end
    ).

lookup_unknown_returns_undefined_test_() ->
    with_config([], fun() ->
        ?assertEqual(undefined, imboy_llm_registry:lookup(<<"nope">>))
    end).

lookup_non_binary_returns_undefined_test() ->
    ?assertEqual(undefined, imboy_llm_registry:lookup(nope)).

lookup_dirty_config_falls_back_to_builtin_test_() ->
    % 配置不是 list 时容错为内置表
    with_config(#{bad => config}, fun() ->
        ?assertMatch({ok, #{module := imboy_llm_qianfan}}, imboy_llm_registry:lookup(<<"qianfan">>))
    end).
