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
    Opts = resolve_env(maps:without([name, module], Provider)),
    {ok, #{module => Mod, opts => Opts}};
find(Name, [_ | Rest]) ->
    find(Name, Rest).

%% @doc 解析 opts 中的环境变量占位：api_key 等敏感值配 {env, <<"VAR">>} /
%% {env, <<"VAR">>, Default}，运行时从 os:getenv 取，避免密钥写死进配置文件。
-spec resolve_env(map()) -> map().
resolve_env(Opts) ->
    maps:map(fun(_K, V) -> resolve_val(V) end, Opts).

resolve_val({env, Var}) when is_binary(Var) ->
    env_get(Var, <<>>);
resolve_val({env, Var, Default}) when is_binary(Var) ->
    env_get(Var, Default);
resolve_val(V) ->
    V.

env_get(Var, Default) ->
    case os:getenv(binary_to_list(Var)) of
        false -> Default;
        [] -> Default;
        S -> unicode:characters_to_binary(S)
    end.
