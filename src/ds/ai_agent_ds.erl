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
-export([set_status/2]).
-export([is_agent/1]).

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
%% ConfigMap 含非空 nickname 时同步 user 表昵称（agent 资料管理后台可配）。
-spec update(integer(), map()) -> {ok, map()} | {error, binary()}.
update(UserId, ConfigMap) when is_map(ConfigMap) ->
    case trim(maps:get(<<"provider">>, ConfigMap, <<>>)) of
        <<>> ->
            {error, <<"provider 不能为空"/utf8>>};
        Provider ->
            case ai_agent_repo:upsert(agent_data(UserId, Provider, ConfigMap)) of
                {ok, _} ->
                    ok = maybe_update_nickname(UserId, ConfigMap),
                    {ok, #{<<"user_id">> => UserId}};
                {error, Reason} ->
                    ?ERROR_LOG("ai_agent_ds:update user_id=~p error ~p~n", [UserId, Reason]),
                    {error, <<"更新 Agent 失败"/utf8>>}
            end
    end.

%% nickname 非空则同步 user 表；失败仅记日志（元数据更新已成功，昵称可重试）
-spec maybe_update_nickname(integer(), map()) -> ok.
maybe_update_nickname(UserId, ConfigMap) ->
    case trim(maps:get(<<"nickname">>, ConfigMap, <<>>)) of
        <<>> ->
            ok;
        Nickname ->
            case user_repo:update(UserId, #{nickname => Nickname}) of
                {ok, _} ->
                    ok;
                {error, Reason} ->
                    ?ERROR_LOG("ai_agent_ds:update nickname user_id=~p error ~p~n", [
                        UserId, Reason
                    ]),
                    ok
            end
    end.

%% @doc 读取单个 agent（解码 trigger_policy jsonb 为 map）
-spec get(integer()) -> {ok, map()} | {error, notfound | term()}.
get(UserId) ->
    case ai_agent_repo:find(UserId) of
        {ok, Row} ->
            {ok, decode_trigger(Row)};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 分页列出 agent（管理后台）
-spec list(pos_integer(), pos_integer()) -> {ok, map()} | {error, term()}.
list(Page, Size) ->
    ai_agent_repo:page(Page, Size).

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
            {true, decode_trigger(Row)};
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

%% 组装 ai_agent 行数据（trigger_policy map → JSON binary）
-spec agent_data(integer(), binary(), map()) -> map().
agent_data(Uid, Provider, ConfigMap) ->
    TriggerMap = maps:get(<<"trigger_policy">>, ConfigMap, #{}),
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
        visibility => ec_cnv:to_integer(maps:get(<<"visibility">>, ConfigMap, 0))
    }.

-spec decode_trigger(map()) -> map().
decode_trigger(#{<<"trigger_policy">> := Tp} = Row) ->
    Row#{<<"trigger_policy">> => decode_json(Tp)};
decode_trigger(Row) ->
    Row.

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
