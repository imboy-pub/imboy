%%% @doc Widget 接入的 env 级事实装配（CSB-02R）：把 CSB-02 留给 Ctx 注入的
%%% 四类服务端事实（`subject_key` / `default_workspace` /
%%% `intake_business_identity_id` / `assertion_verifier`）从 config/store
%%% 解析出来，经 facade 参数收敛合并进 Params——装配齐全时 HTTP 真链路可通。
%%%
%%% 语义（fail-closed 保留）：
%%%   * Params 同键注入**恒优先**（测试/内部合同不变）；
%%%   * env 解析不出的键**不合并**（透传缺参）——application 既有缺参语义
%%%     原样生效（`{invalid_argument,*}` 422 / `{missing_injection,*}` 500），
%%%     绝不用占位值/默认值兜底；
%%%   * store 级真实失败（DB 不可用）显式传播，绝不吞成缺参。
%%%
%%% 配置键（config/sys.config.example）：
%%%   * `cs_widget_subject_key`                —— 匿名 subject HMAC 材料（binary）
%%%   * `cs_widget_identity_keys`              —— 签名断言验签材料 [{Version, Key}]
%%%   * `cs_widget_intake_business_identity_id` —— 接待 identity 覆写（0 = store 派生）
%%%
%%% **本模块不做**：不做业务判定、不触 HTTP；验签逻辑在
%%% `cs_identity_assertion`，digest/密钥只在本模块与验签器内流转，不进日志。
-module(cs_widget_env).

-moduledoc "Widget 接入的 env 级事实装配（CSB-02R）—— subject_key / default_workspace 等四类服务端事实。".
-export([
    merge_bootstrap/2,
    %% BE-W01 A06：identity/exchange 能力开关（第一阶段 capability_disabled）
    identity_exchange_enabled/0,
    merge_identity_exchange/2,
    merge_session/2,
    merge_visitor/2,
    subject_key/0,
    default_workspace_fun/0,
    intake_identity/1,
    assertion_verifier_fun/0
]).

-define(INTAKE_CONFIG_KEY, cs_widget_intake_business_identity_id).
-define(CS_FUNCTION, <<"customer_service">>).

%% ===================================================================
%% facade 合并入口（每个 widget 用例一个；只补缺失键）
%% ===================================================================

merge_bootstrap(OrgId, Params) ->
    merge_facts(OrgId, Params, [fun merge_subject_key/2, fun merge_default_workspace/2]).

%% @doc 签名身份换绑能力开关（BE-W01 A06，默认 false = capability_disabled）。
%% 只显式接受布尔 true（与 imboy_env 的布尔解析口径一致）；其余任何值
%% （含未配置）一律 false——fail-closed，拼错不等于开启。
-spec identity_exchange_enabled() -> boolean().
identity_exchange_enabled() ->
    config_ds:env(cs_widget_identity_exchange_enabled, false) =:= true.

merge_identity_exchange(OrgId, Params) ->
    case shape_assertion(Params) of
        {error, _} = Err ->
            Err;
        {ok, Shaped} ->
            merge_facts(
                OrgId,
                Shaped,
                [
                    fun merge_subject_key/2,
                    fun merge_default_workspace/2,
                    fun merge_assertion_verifier/2
                ]
            )
    end.

%% HTTP 面 assertion 对象的参数收敛：JSON 嵌套对象键是 binary，收敛为
%% application 合同的原子键（key_version / claims / sig）；claims 原样透传
%% （其键归一与签名复核在 cs_identity_assertion，取值全查在 domain）。
shape_assertion(#{assertion := Assertion} = Params) when is_map(Assertion) ->
    Shaped = #{
        key_version => nested(Assertion, key_version),
        claims => nested(Assertion, claims),
        sig => nested(Assertion, sig)
    },
    case
        is_integer(maps:get(key_version, Shaped)) andalso
            is_map(maps:get(claims, Shaped)) andalso
            is_binary(maps:get(sig, Shaped))
    of
        true -> {ok, Params#{assertion => Shaped}};
        false -> {error, {invalid_argument, identity_exchange}}
    end;
shape_assertion(_Params) ->
    {error, {invalid_argument, identity_exchange}}.

nested(Map, Key) when is_atom(Key) ->
    case maps:is_key(Key, Map) of
        true -> maps:get(Key, Map);
        false -> maps:get(atom_to_binary(Key, utf8), Map, undefined)
    end.

merge_session(OrgId, Params) ->
    merge_facts(
        OrgId,
        Params,
        [fun merge_default_workspace/2, fun merge_intake_identity/2]
    ).

merge_visitor(OrgId, Params) ->
    merge_facts(OrgId, Params, [fun merge_default_workspace/2]).

merge_facts(_OrgId, Params, []) ->
    {ok, Params};
merge_facts(OrgId, Params, [Fun | Rest]) ->
    case Fun(OrgId, Params) of
        {ok, Merged} -> merge_facts(OrgId, Merged, Rest);
        {skip, Params} -> merge_facts(OrgId, Params, Rest);
        {error, _} = Err -> Err
    end.

%% ===================================================================
%% 各事实的解析与合并
%% ===================================================================

merge_subject_key(_OrgId, Params) ->
    case is_map_key(subject_key, Params) of
        true ->
            {ok, Params};
        false ->
            case subject_key() of
                {ok, Key} -> {ok, Params#{subject_key => Key}};
                {error, _} -> {skip, Params}
            end
    end.

merge_default_workspace(_OrgId, Params) ->
    case is_map_key(default_workspace, Params) of
        true ->
            {ok, Params};
        false ->
            case default_workspace_fun() of
                {ok, Fun} -> {ok, Params#{default_workspace => Fun}};
                {error, _} -> {skip, Params}
            end
    end.

merge_assertion_verifier(_OrgId, Params) ->
    case is_map_key(assertion_verifier, Params) of
        true ->
            {ok, Params};
        false ->
            case assertion_verifier_fun() of
                {ok, Fun} -> {ok, Params#{assertion_verifier => Fun}};
                {error, _} -> {skip, Params}
            end
    end.

merge_intake_identity(OrgId, Params) ->
    case is_map_key(intake_business_identity_id, Params) of
        true ->
            {ok, Params};
        false ->
            case intake_identity(OrgId) of
                {ok, IdentityId} -> {ok, Params#{intake_business_identity_id => IdentityId}};
                %% 无可派生的接待 identity（本 Org 未配坐席）= 配置缺口，
                %% 透传缺参 → application {missing_injection,*} 500。
                {error, not_found} -> {skip, Params};
                {error, _} = Err -> Err
            end
    end.

%% ===================================================================
%% env 解析（facade 之外的直接读取点；测试/诊断也可直调）
%% ===================================================================

%% 匿名 subject HMAC 材料：env 非空 binary 才可用；缺配置 = 不可用（fail-closed）。
-spec subject_key() -> {ok, binary()} | {error, missing_config}.
subject_key() ->
    case config_ds:env(cs_widget_subject_key, <<>>) of
        Key when is_binary(Key), Key =/= <<>> -> {ok, Key};
        _ -> {error, {missing_config, cs_widget_subject_key}}
    end.

%% 缺省 Workspace 解析器：fun(Org) → 经 store 端口取本 Org active 最小
%% workspace（org 作用域确定性规则冻结在 SQL，见 cs_pg_widget:default_workspace/1）。
-spec default_workspace_fun() -> {ok, fun((integer()) -> term())} | {error, term()}.
default_workspace_fun() ->
    {ok, fun(OrgId) ->
        cs_app_support:with_store(#{}, fun(Store) -> Store:default_workspace(OrgId) end)
    end}.

%% 接待 identity：env 覆写（>0）优先；否则 store 派生——本 Org enabled 且
%% customer_service 职能的派单快照中 business_identity_id 最小者（确定性：
%% 同输入同输出；派单语义与 claim 的 least-active 不同——接待是入口锚点）。
-spec intake_identity(integer()) -> {ok, integer()} | {error, not_found | term()}.
intake_identity(OrgId) when is_integer(OrgId) ->
    case config_ds:env(?INTAKE_CONFIG_KEY, 0) of
        N when is_integer(N), N > 0 ->
            {ok, N};
        _ ->
            case
                cs_app_support:with_store(#{}, fun(Store) ->
                    Store:list_dispatchable_seats(OrgId)
                end)
            of
                {error, _} = Err ->
                    Err;
                {ok, Rows} ->
                    Candidates = [
                        maps:get(business_identity_id, Row)
                     || Row <- Rows,
                        maps:get(enabled, Row, false) =:= true,
                        maps:get(function_key, Row, undefined) =:= ?CS_FUNCTION,
                        is_integer(maps:get(business_identity_id, Row, undefined))
                    ],
                    case Candidates of
                        [] -> {error, not_found};
                        _ -> {ok, lists:min(Candidates)}
                    end
            end
    end;
intake_identity(_OrgId) ->
    {error, not_found}.

%% 签名身份断言验签器：fun(Assertion, KeyDigest) → {ok, Claims} | {error, _}。
%% 验签实现见 cs_identity_assertion（digest 复核 + HMAC + claims 归一）。
-spec assertion_verifier_fun() -> {ok, fun((map(), binary()) -> term())}.
assertion_verifier_fun() ->
    {ok, fun cs_identity_assertion:verify/2}.
