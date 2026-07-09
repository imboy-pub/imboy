-module(imboy_llm_registry).

%%% @doc 配置驱动的 LLM provider 注册表（参照 payment_gateway 模式）
%%% sys.config 配置示例：
%%%   {llm_providers, [
%%%       #{name => <<"qianfan">>, module => imboy_llm_qianfan},
%%%       #{name => <<"openai">>, module => imboy_llm_openai,
%%%         base_url => <<"https://api.deepseek.com/v1">>,
%%%         api_key => <<"sk-...">>, model => <<"deepseek-chat">>}
%%%   ]}
%%% 注册表 = 配置项 ⊕ 内置默认（qianfan），配置优先。
%%% 新增 provider 只需实现 imboy_llm behaviour 并加配置，无需改分派代码。
%%% @since 2026-07-09

-export([lookup/1]).

%% @doc 按名称查 provider，返回实现模块与其余配置项（作 chat/3 的 Opts）
-spec lookup(binary()) -> {ok, #{module := module(), opts := map()}} | undefined.
lookup(Name) when is_binary(Name) ->
    Configured =
        case config_ds:env(llm_providers, []) of
            L when is_list(L) -> L;
            _ -> []
        end,
    find(Name, Configured ++ builtin());
lookup(_Name) ->
    undefined.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

builtin() ->
    [#{name => <<"qianfan">>, module => imboy_llm_qianfan}].

find(_Name, []) ->
    undefined;
find(Name, [#{name := Name, module := Mod} = Provider | _]) ->
    {ok, #{module => Mod, opts => maps:without([name, module], Provider)}};
find(Name, [_ | Rest]) ->
    find(Name, Rest).
