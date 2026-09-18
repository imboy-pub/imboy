%%% @doc Agent Capability Catalog（AG31-03；DECISIONS.md D7 裁决附件
%%% `capability-catalog-contract-v31.md` v1，SHA256=6c3d1b163013f0cb… 冻结实现）。
%%%
%%% 目录契约要点（全部冻结，逐字落地）：
%%%
%%%   * 条目格式（§1）：
%%%
%%%       #{capability => binary(),       %% 非空
%%%         action => binary(),           %% 非空
%%%         resource_type => binary(),    %% 非空
%%%         legal_constraint_keys => [binary()]}  %% constraint_json 允许键白名单
%%%
%%%     三元组 (capability, action, resource_type) 全域唯一。
%%%   * V3.1 首切片枚举（§2）：**空集**（零条目）。本 run 不发明任何产品能力
%%%     语义。空集语义 = 任何 capability 在发行与授权阶段均为「未知 capability」
%%%     → 一律拒绝（架构 §7.2 冻结规则），即 Grant 授权机器完整建成但能力面
%%%     **staged off**。
%%%   * 消费语义（§3，对 AG31-03/05 有约束力）：发行与授权侧都必须
%%%     (capability, action, resource_type) 命中本目录，constraint_json 顶层键
%%%     ⊆ 条目 legal_constraint_keys；目录查询异常按 fail-closed deny。
%%%   * 不从 Profile/Role/任何非目录来源推导能力（架构 §7.2 冻结规则重申）。
%%%   * 演进规则（§5）：只许加法（每条独立 DECISIONS 裁决 + 契约文件版本行
%%%     更新 + SHA 重算），禁止删除/放宽既有条目，禁止运行时写目录。
%%%
%%% 消费方（agent_grant_command / 未来的 authorizer）经 `imboy` app env
%%% `agent_capability_catalog_module` 覆盖本模块（默认 `agent_capability_catalog`），
%%% 与 `agent_org_facts_module`（AG31-01）/`agent_membership_module`（AG31-03）
%%% 同模式；生产枚举为本模块内静态冻结数据，测试用 meck/env 注入假目录。
%%%
%%% 纯数据模块：零 I/O、零进程状态、零时钟依赖。
-module(agent_capability_catalog).

-export([entries/0, lookup/3]).

-export_type([entry/0, capability/0, action/0, resource_type/0]).

-type capability() :: binary().
-type action() :: binary().
-type resource_type() :: binary().
-type entry() :: #{
    capability := capability(),
    action := action(),
    resource_type := resource_type(),
    legal_constraint_keys := [binary()]
}.

%% @doc 目录全量条目（V3.1 首切片冻结 = 空集，契约 §2）。
%%
%% 加法演进时在此追加静态条目：三元组全域唯一、四个键齐全且 binary 非空。
entries() ->
    [].

%% @doc 按三元组查条目；命中返回 {ok, Entry}，未命中 {error, not_found}。
%%
%% 空集目录下本函数恒返回 {error, not_found}——消费方把 not_found 一律
%% 映射为 unknown_capability 拒绝（fail closed，契约 §3）。
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
