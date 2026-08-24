-module(llm_provider_config_contract_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc LLM provider 配置文件契约测试（Bailian 接入）
%%% 保护对象：发布配置源（config/sys.config 或 sys.config.example）与
%%% config/sys.local.config 的 llm_providers
%%% 必须包含可用的 bailian（阿里云百炼 OpenAI 兼容）provider——
%%% 这是「AI agent 收不到回复」修复的可测保证：agent 的 provider 字段
%%% 一旦切到 bailian，注册表 lookup 必须命中这条配置，否则又是静默无回复。
%%%
%%% 契约（与 imboy_llm_registry 的解析规则对齐）：
%%%   name     = <<"bailian">>
%%%   module   = imboy_llm_openai          （OpenAI 兼容端点复用）
%%%   base_url = 专属模型网关 compatible-mode 端点（CSV 交付，2026-08-08 实测可用）
%%%   api_key  = {env, <<"BAILIAN_API_KEY">>}   密钥绝不落盘
%%%   model    = {env, <<"BAILIAN_MODEL">>, <<"qwen3.7-flash">>} 模型可经 env 覆盖
%%%===================================================================

-define(EXPECTED_BASE_URL,
    <<"https://llm-zyz63lgdmo1wvx8k.cn-beijing.maas.aliyuncs.com/compatible-mode/v1">>
).
-define(EXPECTED_MODEL, <<"qwen3.7-flash">>).

%% ===================================================================
%% 配置解析辅助：读 sys.config / sys.local.config 的 llm_providers
%% ===================================================================

providers_from(File) ->
    case file:consult(File) of
        {ok, [Top]} when is_list(Top) ->
            case lists:keyfind(imboy, 1, Top) of
                {imboy, Env} ->
                    case lists:keyfind(llm_providers, 1, Env) of
                        {llm_providers, Providers} when is_list(Providers) ->
                            Providers;
                        _ ->
                            erlang:error({missing_llm_providers, File})
                    end;
                false ->
                    erlang:error({missing_imboy_section, File})
            end;
        {error, Reason} ->
            erlang:error({consult_failed, File, Reason})
    end.

%% ===================================================================
%% 生产默认配置源契约
%% ===================================================================

sys_config_has_bailian_provider_test_() ->
    {timeout, 10, fun() ->
        Providers = providers_from(shipped_config_path()),
        Bailian = find_provider(<<"bailian">>, Providers),
        ?assertMatch(#{name := <<"bailian">>, module := imboy_llm_openai}, Bailian),
        ?assertEqual(?EXPECTED_BASE_URL, maps:get(base_url, Bailian)),
        %% 密钥走 env 占位：配置里出现真实 key 即视为泄露失败
        ?assertEqual({env, <<"BAILIAN_API_KEY">>}, maps:get(api_key, Bailian)),
        %% model 必须有 env 覆盖占位，默认值=网关实测确认的 qwen3.7-flash
        ?assertEqual({env, <<"BAILIAN_MODEL">>, ?EXPECTED_MODEL}, maps:get(model, Bailian))
    end}.

local_config_has_bailian_provider_test_() ->
    {timeout, 10, fun() ->
        Providers = providers_from("config/sys.local.config"),
        Bailian = find_provider(<<"bailian">>, Providers),
        ?assertMatch(#{name := <<"bailian">>, module := imboy_llm_openai}, Bailian),
        ?assertEqual(?EXPECTED_BASE_URL, maps:get(base_url, Bailian)),
        ?assertEqual({env, <<"BAILIAN_API_KEY">>}, maps:get(api_key, Bailian))
    end}.

%% 独立小测：注册表能按契约命中 bailian（mock 配置，直接测 lookup 解析链路）
registry_resolves_bailian_like_config_test_() ->
    ?_test(
        begin
            os:putenv("BAILIAN_KEY_CONTRACT_TEST", "sk-bailian-test"),
            Providers = [
                #{
                    name => <<"bailian">>,
                    module => imboy_llm_openai,
                    base_url => ?EXPECTED_BASE_URL,
                    api_key => {env, <<"BAILIAN_KEY_CONTRACT_TEST">>},
                    model => {env, <<"BAILIAN_MODEL_X">>, ?EXPECTED_MODEL}
                }
            ],
            meck:new(config_ds, [no_link, passthrough]),
            meck:expect(config_ds, env, 2, fun(llm_providers, _Def) -> Providers end),
            try
                {ok, #{module := imboy_llm_openai, opts := Opts}} =
                    imboy_llm_registry:lookup(<<"bailian">>),
                ?assertEqual(<<"sk-bailian-test">>, maps:get(api_key, Opts)),
                ?assertEqual(?EXPECTED_BASE_URL, maps:get(base_url, Opts)),
                ?assertEqual(?EXPECTED_MODEL, maps:get(model, Opts))
            after
                meck:unload(config_ds),
                os:unsetenv("BAILIAN_KEY_CONTRACT_TEST")
            end
        end
    ).

%% ===================================================================
%% Internal
%% ===================================================================

find_provider(Name, Providers) ->
    case [P || P <- Providers, is_map(P), maps:get(name, P, undefined) =:= Name] of
        [Found | _] -> Found;
        [] -> erlang:error({provider_not_configured, Name})
    end.

shipped_config_path() ->
    case filelib:is_file("config/sys.config") of
        true -> "config/sys.config";
        false -> "config/sys.config.example"
    end.
