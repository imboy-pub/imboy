%%% @doc PoC（IMBOY_HIRD_AUTH_REPLAY_POC_V1）专用能力目录——**非生产模块**。
%%%
%%% 真实静态实现（非 meck）：单条目目录，仅声明 P1 只读业务工具
%%% `workspace.detail / read / workspace`。经 app env
%%% `agent_capability_catalog_module` 显式启用；生产默认仍是
%%% `agent_capability_catalog`（V3.1 冻结空集，staged off）。
%%%
%%% 条目演进遵守目录契约 §5（只许加法）；PoC 条目未进生产目录，
%%% 不构成能力面放开。
-module(imboy_hird_poc_catalog).

-export([entries/0, lookup/3]).

entries() ->
    [
        #{
            capability => <<"workspace.detail">>,
            action => <<"read">>,
            resource_type => <<"workspace">>,
            legal_constraint_keys => []
        }
    ].

lookup(Capability, Action, ResourceType) when
    is_binary(Capability), is_binary(Action), is_binary(ResourceType)
->
    case
        lists:search(
            fun(Entry) ->
                maps:get(capability, Entry) =:= Capability andalso
                    maps:get(action, Entry) =:= Action andalso
                    maps:get(resource_type, Entry) =:= ResourceType
            end,
            entries()
        )
    of
        {value, Entry} -> {ok, Entry};
        false -> {error, not_found}
    end;
lookup(_Capability, _Action, _ResourceType) ->
    {error, not_found}.
