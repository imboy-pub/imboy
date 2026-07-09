-module(ai_agent_repo).

%%%
% AI Agent 元数据仓库 / AI Agent metadata repository
% 表 ai_agent（见 priv/migrations/00000027_ai_agent）：user_id 主键 = agent 的 user.id
% 所有 SQL 经 elib_pg 参数化；trigger_policy 为 jsonb（写入用 $N::jsonb cast）。
%%%

-export([tablename/0]).
-export([upsert/1]).
-export([find/1]).
-export([set_status/2]).
-export([page/2]).

-include("log.hrl").

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"ai_agent">>).

%% @doc 创建或更新 agent 绑定（按 user_id upsert）
%% Data 键：user_id(必填), provider(必填), model, role_id, system_prompt,
%%          owner_uid, trigger_policy(已编码 JSON binary), status
-spec upsert(map()) -> {ok, [map()]} | {error, term()}.
upsert(#{user_id := UserId, provider := Provider} = Data) ->
    Tb = tablename(),
    Model = maps:get(model, Data, <<>>),
    RoleId = maps:get(role_id, Data, <<>>),
    SystemPrompt = maps:get(system_prompt, Data, <<>>),
    OwnerUid = maps:get(owner_uid, Data, 0),
    TriggerJson = maps:get(trigger_policy, Data, <<"{}">>),
    Status = maps:get(status, Data, 1),
    Sql =
        <<"INSERT INTO ", Tb/binary,
            " (user_id, provider, model, role_id, system_prompt, owner_uid,"
            "  trigger_policy, status, created_at, updated_at)"
            " VALUES ($1,$2,$3,$4,$5,$6,$7::jsonb,$8,NOW(),NOW())"
            " ON CONFLICT (user_id) DO UPDATE SET"
            " provider = EXCLUDED.provider, model = EXCLUDED.model,"
            " role_id = EXCLUDED.role_id, system_prompt = EXCLUDED.system_prompt,"
            " owner_uid = EXCLUDED.owner_uid, trigger_policy = EXCLUDED.trigger_policy,"
            " status = EXCLUDED.status, updated_at = NOW()"
            " RETURNING user_id">>,
    case
        elib_pg:query(Sql, [
            UserId, Provider, Model, RoleId, SystemPrompt, OwnerUid, TriggerJson, Status
        ])
    of
        {ok, Rows} ->
            {ok, Rows};
        {error, Reason} ->
            ?ERROR_LOG("ai_agent_repo:upsert user_id=~p error ~p~n", [UserId, Reason]),
            {error, Reason}
    end.

%% @doc 按 user_id 查单个 agent 元数据行
-spec find(integer()) -> {ok, map()} | {error, notfound | term()}.
find(UserId) ->
    Tb = tablename(),
    Sql =
        <<
            "SELECT user_id, provider, model, role_id, system_prompt, owner_uid,"
            " trigger_policy, status FROM ",
            Tb/binary,
            " WHERE user_id = $1"
        >>,
    case elib_pg:query(Sql, [UserId]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, notfound};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 启用/停用 agent（status 1=启用 0=停用）
-spec set_status(integer(), 0 | 1) -> {ok, non_neg_integer()} | {error, term()}.
set_status(UserId, Status) ->
    Tb = tablename(),
    elib_pg:update(Tb, #{status => Status, updated_at => elib_dt:now()}, <<"user_id = $1">>, [
        UserId
    ]).

%% @doc 分页列出 agent（管理后台）
-spec page(pos_integer(), pos_integer()) -> {ok, map()} | {error, term()}.
page(Page, Size) ->
    Tb = tablename(),
    Column = <<"user_id, provider, model, role_id, owner_uid, status, created_at">>,
    elib_pg:page_with_total(Tb, Column, #{}, <<"created_at DESC">>, Page, Size).
