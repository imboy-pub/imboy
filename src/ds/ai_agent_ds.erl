-module(ai_agent_ds).

%%%
% AI Agent 数据服务 / AI Agent data service
%
% 职责：agent 账号编排（建 user 行 + 标 account_type=1 + 绑 ai_agent 元数据）、
%       jsonb(trigger_policy) 编解码、供消息路由判定 is_agent/1。
% 边界：屏蔽 repo 存储细节；账号类型常量集中在此。
%%%

-export([create/1]).
-export([update/2]).
-export([get/1]).
-export([list/2]).
-export([list/3]).
-export([set_status/2]).
-export([is_agent/1]).
-export([roles/0]).
-export([save_role/2]).
-export([delete_role/1]).
-export([categories/0]).

-include("log.hrl").

%% account_type 枚举（对齐迁移 00000027 注释）
-define(ACCOUNT_TYPE_AGENT, 1).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 创建 agent：新建 user 账号 → 标 account_type=1 → 绑定 ai_agent 元数据
%% ConfigMap 键（binary key，来自管理后台 POST）：
%%   nickname(必填) account? provider(必填) model? role_id? system_prompt?
%%   owner_uid? trigger_policy?(map)
%% ponytail: 三步非单事务（user_repo 走 auto-commit，非 with_tx Conn）；
%%   agent 绑定失败会留下 account_type=1 但无 ai_agent 行的孤儿 user，
%%   管理端可 upsert 重试修复。真需强一致再下沉到单个 with_tx。
-spec create(map()) -> {ok, map()} | {error, binary()}.
create(ConfigMap) when is_map(ConfigMap) ->
    Nickname = trim(maps:get(<<"nickname">>, ConfigMap, <<>>)),
    Provider = trim(maps:get(<<"provider">>, ConfigMap, <<>>)),
    case validate(Nickname, Provider) of
        ok ->
            Uid = elib_tsid:generate(),
            Account = default_account(maps:get(<<"account">>, ConfigMap, <<>>), Nickname, Uid),
            case create_agent_user(Uid, Nickname, Account) of
                ok ->
                    AgentData = agent_data(Uid, Provider, ConfigMap),
                    case ai_agent_repo:upsert(AgentData) of
                        {ok, _} ->
                            {ok, #{<<"user_id">> => Uid}};
                        {error, Reason} ->
                            ?ERROR_LOG("ai_agent_ds:create bind error ~p~n", [Reason]),
                            {error, <<"绑定 Agent 元数据失败"/utf8>>}
                    end;
                {error, Reason} ->
                    ?ERROR_LOG("ai_agent_ds:create user error ~p~n", [Reason]),
                    {error, <<"创建 Agent 账号失败"/utf8>>}
            end;
        {error, _} = Err ->
            Err
    end.

%% @doc 更新既有 agent 绑定（不新建 user，按 user_id upsert 元数据）；
%% ConfigMap 含非空 nickname/avatar 时同步 user 表（agent 资料管理后台可配）。
-spec update(integer(), map()) -> {ok, map()} | {error, binary()}.
update(UserId, ConfigMap) when is_map(ConfigMap) ->
    case trim(maps:get(<<"provider">>, ConfigMap, <<>>)) of
        <<>> ->
            {error, <<"provider 不能为空"/utf8>>};
        Provider ->
            case ai_agent_repo:patch(UserId, patch_data(UserId, Provider, ConfigMap)) of
                {ok, _} ->
                    ok = maybe_update_user_profile(UserId, ConfigMap),
                    {ok, #{<<"user_id">> => UserId}};
                {error, Reason} ->
                    ?ERROR_LOG("ai_agent_ds:update user_id=~p error ~p~n", [UserId, Reason]),
                    {error, <<"更新 Agent 失败"/utf8>>}
            end
    end.

%% nickname/avatar 任一非空则同步 user 表（一次 update 合并）；
%% 失败仅记日志（元数据更新已成功，资料可重试）
-spec maybe_update_user_profile(integer(), map()) -> ok.
maybe_update_user_profile(UserId, ConfigMap) ->
    Profile = maps:from_list([
        {K, V}
     || {K, V} <- [
            {nickname, trim(maps:get(<<"nickname">>, ConfigMap, <<>>))},
            {avatar, trim(maps:get(<<"avatar">>, ConfigMap, <<>>))}
        ],
        V =/= <<>>
    ]),
    case map_size(Profile) of
        0 ->
            ok;
        _ ->
            case user_repo:update(UserId, Profile) of
                {ok, _} ->
                    ok;
                {error, Reason} ->
                    ?ERROR_LOG("ai_agent_ds:update profile user_id=~p error ~p~n", [
                        UserId, Reason
                    ]),
                    ok
            end
    end.

%% ===================================================================
%% ai_roles 人格 KV 管理（admin 角色管理页后端）
%% 持久层走 config_ds get/set（DB config 表 + 缓存）；
%% 消费点 msg_c2s_logic:c2s_to_role_chat 读 env(ai_roles) 优先、
%% config_ds:get 兜底——admin 保存的角色运行时即生效。
%% ===================================================================

%% @doc 读取全部角色：#{RoleId => SystemPrompt}
-spec roles() -> map().
roles() ->
    config_ds:get(<<"ai_roles">>, #{}).

%% @doc 保存/覆盖单个角色 system_prompt
-spec save_role(binary(), binary()) -> ok.
save_role(RoleId, Prompt) when is_binary(RoleId), is_binary(Prompt) ->
    config_ds:set(<<"ai_roles">>, (roles())#{RoleId => Prompt}).

%% @doc 删除单个角色（不存在也返回 ok，幂等）
-spec delete_role(binary()) -> ok.
delete_role(RoleId) ->
    config_ds:set(<<"ai_roles">>, maps:remove(RoleId, roles())).

%% @doc 读取单个 agent（解码 trigger_policy jsonb 为 map）
-spec get(integer()) -> {ok, map()} | {error, notfound | term()}.
get(UserId) ->
    case ai_agent_repo:find(UserId) of
        {ok, Row} ->
            {ok, decode_agent(Row)};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 分页列出 agent（管理后台）
-spec list(pos_integer(), pos_integer()) -> {ok, map()} | {error, term()}.
list(Page, Size) ->
    ai_agent_repo:page(Page, Size).

%% @doc 分页列出 agent（管理后台，按分类筛选；空分类回退全量）
-spec list(pos_integer(), pos_integer(), binary()) -> {ok, map()} | {error, term()}.
list(Page, Size, Category) when Page > 0, Size > 0 ->
    ai_agent_repo:page(Page, Size, Category).

%% @doc 启用/停用 agent
-spec set_status(integer(), 0 | 1) -> {ok, non_neg_integer()} | {error, term()}.
set_status(UserId, Status) when Status =:= 0; Status =:= 1 ->
    ai_agent_repo:set_status(UserId, Status).

%% @doc 消息路由判定：UserId 是否为「启用中」的 agent
%% 返回 {true, AgentMap} 给下游直接复用配置；否则 false。
-spec is_agent(integer()) -> {true, map()} | false.
is_agent(UserId) ->
    case ai_agent_repo:find(UserId) of
        {ok, #{<<"status">> := 1} = Row} ->
            Effective = decode_agent(Row),
            case maps:get(<<"role_status">>, Effective, 1) of
                1 -> {true, Effective};
                _ -> false
            end;
        _ ->
            false
    end.

%% ===================================================================
%% Internal
%% ===================================================================

-spec validate(binary(), binary()) -> ok | {error, binary()}.
validate(<<>>, _Provider) -> {error, <<"nickname 不能为空"/utf8>>};
validate(_Nickname, <<>>) -> {error, <<"provider 不能为空"/utf8>>};
validate(_Nickname, _Provider) -> ok.

%% 建 agent 用户行并标记 account_type=1
-spec create_agent_user(integer(), binary(), binary()) -> ok | {error, term()}.
create_agent_user(Uid, Nickname, Account) ->
    case user_repo:create(#{id => Uid, nickname => Nickname, account => Account}) of
        ok ->
            case user_repo:update(Uid, #{account_type => ?ACCOUNT_TYPE_AGENT}) of
                {ok, _} -> ok;
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% 组装 ai_agent 行数据（trigger_policy/capabilities map → JSON binary）
-spec agent_data(integer(), binary(), map()) -> map().
agent_data(Uid, Provider, ConfigMap) ->
    TriggerMap = maps:get(<<"trigger_policy">>, ConfigMap, #{}),
    Capabilities = maps:get(<<"capabilities">>, ConfigMap, #{}),
    #{
        user_id => Uid,
        provider => Provider,
        model => maps:get(<<"model">>, ConfigMap, <<>>),
        role_id => maps:get(<<"role_id">>, ConfigMap, <<>>),
        system_prompt => maps:get(<<"system_prompt">>, ConfigMap, <<>>),
        owner_uid => ec_cnv:to_integer(maps:get(<<"owner_uid">>, ConfigMap, 0)),
        trigger_policy => jsone:encode(TriggerMap, [native_utf8]),
        status => 1,
        %% 可选：管理后台传则透传，缺省保持列 DEFAULT（description='' visibility=0）
        description => maps:get(<<"description">>, ConfigMap, <<>>),
        visibility => ec_cnv:to_integer(maps:get(<<"visibility">>, ConfigMap, 0)),
        %% 迁移 000057 新增的可定制属性
        category => trim(maps:get(<<"category">>, ConfigMap, <<>>)),
        voice_id => trim(maps:get(<<"voice_id">>, ConfigMap, <<>>)),
        greeting => trim(maps:get(<<"greeting">>, ConfigMap, <<>>)),
        capabilities => jsone:encode(Capabilities, [native_utf8]),
        temperature => maps:get(<<"temperature">>, ConfigMap, 0.7)
    }.

-spec patch_data(integer(), binary(), map()) -> map().
patch_data(Uid, Provider, ConfigMap) ->
    Full = agent_data(Uid, Provider, ConfigMap),
    maps:filter(
        fun
            (provider, _) -> true;
            (Key, _) -> maps:is_key(atom_to_binary(Key, utf8), ConfigMap)
        end,
        Full
    ).

-spec decode_trigger(map()) -> map().
decode_trigger(#{<<"trigger_policy">> := Tp} = Row) ->
    Row#{<<"trigger_policy">> => decode_json(Tp)};
decode_trigger(Row) ->
    Row.

-spec decode_agent(map()) -> map().
decode_agent(Row) ->
    Effective = inherit_role(decode_capabilities(decode_trigger(Row))),
    Policy = ai_agent_policy:effective(Effective),
    Effective#{<<"policy_source">> => maps:get(<<"policy_source">>, Policy)}.

-spec decode_capabilities(map()) -> map().
decode_capabilities(#{<<"capabilities">> := Capabilities} = Row) ->
    Row#{<<"capabilities">> => decode_json(Capabilities)};
decode_capabilities(Row) ->
    Row.

%% 角色是行为配置的唯一来源；角色不存在、未发布或配置异常时保留 agent
%% 旧配置，保证历史助手可以继续工作并允许后台逐步迁移。
-spec inherit_role(map()) -> map().
inherit_role(#{<<"role_id">> := RoleCode} = Agent) when
    is_binary(RoleCode), RoleCode =/= <<>>
->
    try ai_agent_role_repo:find_published(RoleCode) of
        {ok, RoleRow} ->
            case role_config(RoleCode, RoleRow) of
                {ok, Role} ->
                    case ai_agent_role_ds:effective_config(Agent, Role) of
                        {ok, Effective} ->
                            Effective;
                        {error, Reason} ->
                            log_role_fallback(RoleCode, {invalid_role_policy, Reason}),
                            Agent
                    end;
                error ->
                    log_role_fallback(RoleCode, unpublished_or_invalid),
                    Agent
            end;
        {error, Reason} ->
            log_role_fallback(RoleCode, Reason),
            Agent;
        Other ->
            log_role_fallback(RoleCode, Other),
            Agent
    catch
        Class:Reason ->
            log_role_fallback(RoleCode, {Class, Reason}),
            Agent
    end;
inherit_role(Agent) ->
    Agent.

log_role_fallback(RoleCode, Reason) ->
    try
        ?WARN_LOG([
            ai_agent_role_legacy_fallback,
            #{role_code => RoleCode, reason => Reason}
        ])
    catch
        _:_ -> ok
    end,
    ok.

-spec role_config(binary(), map()) -> {ok, map()} | error.
role_config(RoleCode, Row) ->
    Version = maps:get(<<"version">>, Row, maps:get(<<"active_version">>, Row, 0)),
    Prompt = maps:get(<<"system_prompt">>, Row, <<>>),
    case
        is_integer(Version) andalso Version > 0 andalso is_binary(Prompt) andalso Prompt =/= <<>>
    of
        true ->
            {ok, #{
                <<"code">> => maps:get(<<"code">>, Row, RoleCode),
                <<"version">> => Version,
                <<"status">> => maps:get(<<"status">>, Row, 1),
                <<"system_prompt">> => Prompt,
                <<"capabilities">> => decode_json(maps:get(<<"capabilities">>, Row, #{})),
                <<"knowledge_policy">> => decode_json(
                    maps:get(<<"knowledge_policy">>, Row, #{})
                )
            }};
        false ->
            error
    end.

-spec decode_json(binary() | map()) -> map().
decode_json(V) when is_map(V) ->
    V;
decode_json(V) when is_binary(V) ->
    try jsone:decode(V, [{object_format, map}]) of
        M when is_map(M) -> M;
        _ -> #{}
    catch
        _:_ -> #{}
    end;
decode_json(_) ->
    #{}.

-spec default_account(binary(), binary(), integer()) -> binary().
default_account(<<>>, _Nickname, Uid) ->
    <<"agent_", (ec_cnv:to_binary(Uid))/binary>>;
default_account(Account, _Nickname, _Uid) ->
    Account.

-spec trim(term()) -> binary().
trim(B) when is_binary(B) -> string:trim(B);
trim(V) -> ec_cnv:to_binary(V).

%% @doc 列出启用且公开的 agent 分类（去重，非空）
-spec categories() -> {ok, [binary()]} | {error, term()}.
categories() ->
    ai_agent_repo:categories().
