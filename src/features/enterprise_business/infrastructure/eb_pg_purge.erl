%%% @doc 唯一的 bounded retention purge worker（物理清理企业消息）。
%%%
%%% 依据：plan v4.1 EB-D12、§2.1 #17、§4.3、EB-03 §3.1/§3.3（A06）。
%%%
%%% 这是全系统**唯一**允许物理删除企业消息/附件的通道，其约束逐条落在代码里：
%%%
%%%   1. **显式 OrgId/WorkspaceId**：两个业务参数都是必填整数，且每一条 SQL 都同时
%%%      带 `organization_id` 与 `workspace_id`（`sql_statements/0` 可机械核对）；
%%%      跨租户（Org A + Workspace B）不报错也不删行，删除数为 0。
%%%   2. **注入时钟**：候选由调用方传入的 `now`（Unix 秒，来自注入时钟端口）筛选，
%%%      本模块不读系统时间。DB 守卫另按数据库时钟复核（纵深防御）。
%%%   3. **batch limit + `SKIP LOCKED`**：候选 `ORDER BY retain_until, id LIMIT $4
%%%      FOR UPDATE SKIP LOCKED`，因此并发 worker 不会互相阻塞、也不会重复清理。
%%%   4. **失败多保留**：整批一个事务；任一守卫拒绝（未到期 / active hold / 附件
%%%      保留期更长 / RESTRICT 引用）都会回滚，本轮**一行不删**且不写审计。
%%%   5. **append-only 逐批审计**：仅在真的删了行时追加一条 `message.purge` 审计。
%%%   6. **子表顺序**：先删 `enterprise_asset` 与 `enterprise_message_delivery`，
%%%      再删消息——与 EB-01 的 RESTRICT FK 一致，保证「附件不得早于其消息被删」，
%%%      也不留孤儿投递行。
%%%
%%% 资格裁决复用 domain：`eb_retention:purge_eligible/3`（retain_until 未到 /
%%% active hold 覆盖 → 不清理）；被裁决为不合格的行只记录 skipped（多保留）。
-module(eb_pg_purge).

-include_lib("epgsql/include/epgsql.hrl").

-export([purge_batch/3, sql_statements/0]).

-define(DEFAULT_BATCH_LIMIT, 100).
-define(MAX_BATCH_LIMIT, 1000).
-define(PURGE_ACTION, <<"message.purge">>).
-define(PURGE_GUC, <<"SET LOCAL imboy.enterprise_purge = 'on'">>).

-define(SQL_CANDIDATES, <<
    "SELECT id, organization_id, workspace_id, conversation_id, retain_until"
    "  FROM enterprise_message"
    " WHERE organization_id = $1 AND workspace_id = $2"
    "   AND retain_until <= to_timestamp($3::bigint/1000)"
    " ORDER BY retain_until, id"
    " LIMIT $4"
    " FOR UPDATE SKIP LOCKED"
>>).

-define(SQL_DELETE_ASSETS, <<
    "DELETE FROM enterprise_asset"
    " WHERE organization_id = $1 AND workspace_id = $2 AND message_id = ANY($3::bigint[])"
>>).

-define(SQL_DELETE_DELIVERIES, <<
    "DELETE FROM enterprise_message_delivery"
    " WHERE organization_id = $1 AND workspace_id = $2 AND message_id = ANY($3::bigint[])"
>>).

-define(SQL_DELETE_MESSAGES, <<
    "DELETE FROM enterprise_message"
    " WHERE organization_id = $1 AND workspace_id = $2 AND id = ANY($3::bigint[])"
    "   AND retain_until <= to_timestamp($4::bigint/1000)"
>>).

%% FND-4（RULING-2026-09-15 §五）：purge 批次的对象字节回收前置查询 ——
%% 在删元数据**之前**取到每个 asset 的 id（对象 key 由 asset 端按行解析）。
-define(SQL_ASSETS_OF_MESSAGES, <<
    "SELECT id, message_id, retain_until"
    "  FROM enterprise_asset"
    " WHERE organization_id = $1 AND workspace_id = $2 AND message_id = ANY($3::bigint[])"
>>).

%% active hold 必须按 domain 的 hold_covers/2 口径给出 conversation_id / message_id：
%% message scope 的会话由所属消息解析（同一条语句内完成，租户两键都在）。
-define(SQL_ACTIVE_HOLDS, <<
    "SELECT h.id, h.organization_id, h.workspace_id, h.scope_type, h.scope_conversation_id,"
    "       h.scope_message_id, h.reason_code,"
    "       coalesce(h.scope_conversation_id, m.conversation_id) AS conversation_id,"
    "       h.scope_message_id AS message_id"
    "  FROM enterprise_retention_hold h"
    "  LEFT JOIN enterprise_message m"
    "         ON m.organization_id = h.organization_id AND m.workspace_id = h.workspace_id"
    "        AND m.id = h.scope_message_id"
    " WHERE h.organization_id = $1 AND h.workspace_id = $2 AND h.released_at IS NULL"
>>).

%% @doc 执行一批 bounded purge。
%%
%% Opts：`now`（必填，注入时钟 Unix 秒）、`batch_limit`（可选，1..1000，默认 100）。
%%
%% 返回 `{ok, #{purged, deleted, eligible, skipped, audit_id}}` 或 `{error, Reason}`。
%% `skipped` 是 `[{MessageId, {ineligible, Reason}}]`（domain 裁决），
%% `deleted` 恒等于 `length(purged)`。
-spec purge_batch(integer(), integer(), map()) -> {ok, map()} | {error, term()}.
purge_batch(OrgId, WorkspaceId, Opts) when is_map(Opts) ->
    case validate(OrgId, WorkspaceId, Opts) of
        {ok, Now, Limit} -> run(OrgId, WorkspaceId, Now, Limit);
        {error, _} = Err -> Err
    end;
purge_batch(_OrgId, _WorkspaceId, _Opts) ->
    {error, invalid_opts}.

%% 参数校验顺序固定：租户 → 注入时钟 → 批量上限（任一失败都不触库）。
validate(OrgId, WorkspaceId, Opts) ->
    case tenant_error(OrgId, WorkspaceId) of
        {error, _} = Err ->
            Err;
        ok ->
            case clock(Opts) of
                {error, _} = Err ->
                    Err;
                {ok, Now} ->
                    case batch_limit(Opts) of
                        {error, _} = Err -> Err;
                        {ok, Limit} -> {ok, Now, Limit}
                    end
            end
    end.

%% @doc 冻结的语句列表（全部带 $1/$2 与 organization_id + workspace_id）。
-spec sql_statements() -> [binary()].
sql_statements() ->
    [
        ?SQL_CANDIDATES,
        ?SQL_ACTIVE_HOLDS,
        ?SQL_ASSETS_OF_MESSAGES,
        ?SQL_DELETE_ASSETS,
        ?SQL_DELETE_DELIVERIES,
        ?SQL_DELETE_MESSAGES
    ].

%% ===================================================================
%% 事务主体
%% ===================================================================

run(OrgId, WorkspaceId, Now, Limit) ->
    %% FND-4：对象字节回收发生在**元数据事务之前**（对象存储无事务性）。
    %% 顺序不变量：对象先删、元数据后删 ⇒ 事务若回滚，下批重试时对象侧
    %% not_found 被幂等化放行（目标状态已达成），元数据再删 —— 永不出现
    %% 「元数据已删而对象残留」的孤儿；反向顺序则会因 not_found 死循环。
    case reclaim_objects(OrgId, WorkspaceId, Now, Limit) of
        {error, _} = Err ->
            Err;
        {ok, Keep, ObjectFailures} ->
            case
                elib_pg:with_tx(
                    fun(Conn) ->
                        do_purge(Conn, OrgId, WorkspaceId, Now, Limit, Keep, ObjectFailures)
                    end,
                    [{reraise, false}]
                )
            of
                {rollback, Reason} -> {error, Reason};
                Result -> Result
            end
    end.

%% 事务外对象回收：重选候选（与事务内同判据）→ 域裁决 → 逐 asset 回收。
%% 对象删除失败的消息**不进本批**（元数据与对象都不动 ⇒ 行仍 eligible，
%% 下批自动重试 —— 可重试状态由数据本身承载，不另设队列）。
reclaim_objects(OrgId, WorkspaceId, Now, Limit) ->
    AssetPort = asset_port(),
    case elib_pg:query(?SQL_CANDIDATES, [OrgId, WorkspaceId, Now * 1000, Limit]) of
        {ok, Rows} ->
            Candidates = [
                #{
                    message_id => maps:get(<<"id">>, Row),
                    organization_id => maps:get(<<"organization_id">>, Row),
                    workspace_id => maps:get(<<"workspace_id">>, Row),
                    conversation_id => maps:get(<<"conversation_id">>, Row),
                    retain_until => to_unix(maps:get(<<"retain_until">>, Row))
                }
             || Row <- Rows
            ],
            case active_holds_standalone(OrgId, WorkspaceId) of
                {ok, Holds} ->
                    {Eligible, _Skipped} = eligible(Candidates, Holds, Now),
                    Ids = [maps:get(message_id, M) || M <- Eligible],
                    case Ids of
                        [] -> {ok, [], []};
                        _ -> reclaim_each(AssetPort, OrgId, WorkspaceId, Eligible, Ids, Now)
                    end;
                {error, _} = Err ->
                    Err
            end;
        {error, Reason} ->
            {error, {candidate_query_failed, Reason}}
    end.

reclaim_each(AssetPort, OrgId, WorkspaceId, _Eligible, Ids, Now) ->
    case asset_rows(OrgId, WorkspaceId, Ids) of
        {ok, Assets} ->
            {Keep, Failures} = lists:foldl(
                fun(Asset, {KeepAcc, FailAcc}) ->
                    Mid = maps:get(message_id, Asset),
                    case lists:member(Mid, KeepAcc) of
                        false ->
                            %% 该消息已因前面某个 asset 失败被剔除
                            {KeepAcc, FailAcc};
                        true ->
                            AssetId = maps:get(id, Asset),
                            case maps:get(retain_until, Asset, undefined) of
                                RU when is_integer(RU), RU > Now ->
                                    %% FND-4：asset 自身未到期 ⇒ 对象**不得预删**。
                                    %% 消息保留在批内（不剔除对象也不剔除元数据），
                                    %% 由 DB 的 trg_enterprise_asset_purge_guard
                                    %% 在事务内拒绝整批（附件不得早于消息被删）。
                                    {KeepAcc, FailAcc};
                                _ExpiredOrUnknown ->
                                    case AssetPort:delete_private(OrgId, WorkspaceId, AssetId) of
                                        ok ->
                                            {KeepAcc, FailAcc};
                                        {error, {object_store, not_found}} ->
                                            %% 幂等化：对象已不在（上次事务回滚后的重试）
                                            {KeepAcc, FailAcc};
                                        {error, Reason} ->
                                            {lists:delete(Mid, KeepAcc), [
                                                {Mid, AssetId, {object_delete_failed, Reason}}
                                                | FailAcc
                                            ]}
                                    end
                            end
                    end
                end,
                {Ids, []},
                Assets
            ),
            {ok, Keep, Failures};
        {error, _} = Err ->
            Err
    end.

asset_rows(OrgId, WorkspaceId, Ids) ->
    case elib_pg:query(?SQL_ASSETS_OF_MESSAGES, [OrgId, WorkspaceId, Ids]) of
        {ok, Rows} ->
            {ok, [
                #{
                    id => maps:get(<<"id">>, Row),
                    message_id => maps:get(<<"message_id">>, Row),
                    retain_until => to_unix(maps:get(<<"retain_until">>, Row))
                }
             || Row <- Rows
            ]};
        {error, Reason} ->
            {error, {asset_query_failed, Reason}}
    end.

active_holds_standalone(OrgId, WorkspaceId) ->
    case elib_pg:query(?SQL_ACTIVE_HOLDS, [OrgId, WorkspaceId]) of
        {ok, Rows} -> {ok, [hold_map(Row) || Row <- Rows]};
        {error, Reason} -> {error, {hold_query_failed, Reason}}
    end.

asset_port() ->
    eb_infra_ports:asset().

do_purge(Conn, OrgId, WorkspaceId, Now, Limit, KeepIds, ObjectFailures) ->
    %% 1) 进入 bounded purge 上下文（DB 守卫第一步；失败宁可多保留）
    case epgsql:squery(Conn, ?PURGE_GUC) of
        {ok, _, _} ->
            case candidates(Conn, OrgId, WorkspaceId, Now, Limit) of
                {ok, Candidates} ->
                    case active_holds(Conn, OrgId, WorkspaceId) of
                        {ok, Holds} ->
                            case eligible(Candidates, Holds, Now) of
                                {[], Skipped} ->
                                    %% 无可清理行：不删、不写审计（跳过原因如实返回）
                                    {ok, summary([], Skipped, undefined, ObjectFailures)};
                                {Eligible0, Skipped} ->
                                    %% FND-4：对象回收失败的消息已被剔除（元数据不动可重试）
                                    Eligible = [
                                        M
                                     || M <- Eligible0,
                                        lists:member(maps:get(message_id, M), KeepIds)
                                    ],
                                    case Eligible of
                                        [] ->
                                            {ok, summary([], Skipped, undefined, ObjectFailures)};
                                        _ ->
                                            delete_and_audit(
                                                Conn,
                                                OrgId,
                                                WorkspaceId,
                                                Now,
                                                Eligible,
                                                Skipped,
                                                ObjectFailures
                                            )
                                    end
                            end;
                        {error, _} = Err ->
                            Err
                    end;
                {error, _} = Err ->
                    Err
            end;
        {error, Reason} ->
            {error, {purge_context_failed, Reason}}
    end.

candidates(Conn, OrgId, WorkspaceId, Now, Limit) ->
    case elib_pg:query(Conn, ?SQL_CANDIDATES, [OrgId, WorkspaceId, Now * 1000, Limit]) of
        {ok, Rows} ->
            {ok, [
                #{
                    message_id => maps:get(<<"id">>, Row),
                    organization_id => maps:get(<<"organization_id">>, Row),
                    workspace_id => maps:get(<<"workspace_id">>, Row),
                    conversation_id => maps:get(<<"conversation_id">>, Row),
                    retain_until => to_unix(maps:get(<<"retain_until">>, Row))
                }
             || Row <- Rows
            ]};
        {error, Reason} ->
            {error, {candidate_query_failed, Reason}}
    end.

%% 只加载 active hold（released_at IS NULL）；release 后立即失效（domain 亦按此判）。
active_holds(Conn, OrgId, WorkspaceId) ->
    case elib_pg:query(Conn, ?SQL_ACTIVE_HOLDS, [OrgId, WorkspaceId]) of
        {ok, Rows} ->
            {ok, [hold_map(Row) || Row <- Rows]};
        {error, Reason} ->
            {error, {hold_query_failed, Reason}}
    end.

hold_map(Row) ->
    #{
        id => maps:get(<<"id">>, Row),
        organization_id => maps:get(<<"organization_id">>, Row),
        workspace_id => maps:get(<<"workspace_id">>, Row),
        scope => atom_scope(maps:get(<<"scope_type">>, Row)),
        scope_conversation_id => value_or_undefined(maps:get(<<"scope_conversation_id">>, Row)),
        scope_message_id => value_or_undefined(maps:get(<<"scope_message_id">>, Row)),
        conversation_id => value_or_undefined(maps:get(<<"conversation_id">>, Row)),
        message_id => value_or_undefined(maps:get(<<"message_id">>, Row)),
        reason_code => maps:get(<<"reason_code">>, Row),
        released_at => undefined
    }.

%% 唯一裁决入口：domain 的 purge_eligible/3（EB-02 冻结的不变量语义）。
eligible(Candidates, Holds, Now) ->
    lists:foldr(
        fun(Message, {Ok, Skipped}) ->
            case eb_retention:purge_eligible(Message, Holds, Now) of
                eligible ->
                    {[Message | Ok], Skipped};
                {ineligible, Reason} ->
                    {Ok, [{maps:get(message_id, Message), {ineligible, Reason}} | Skipped]}
            end
        end,
        {[], []},
        Candidates
    ).

delete_and_audit(Conn, OrgId, WorkspaceId, Now, Eligible, Skipped, ObjectFailures) ->
    Ids = [maps:get(message_id, Message) || Message <- Eligible],
    %% 先删子表（asset / delivery），再删消息：与 RESTRICT FK 顺序一致。
    case delete_children(Conn, OrgId, WorkspaceId, Ids) of
        ok ->
            case
                elib_pg:execute(Conn, ?SQL_DELETE_MESSAGES, [
                    OrgId, WorkspaceId, Ids, Now * 1000
                ])
            of
                {ok, Deleted} when is_integer(Deleted) ->
                    case Deleted =:= length(Ids) of
                        true ->
                            write_audit(
                                Conn, OrgId, WorkspaceId, Now, Ids, Skipped, ObjectFailures
                            );
                        false ->
                            %% 影响行数与候选不符（并发/守卫变化）→ 整批回滚，多保留
                            throw({rollback, {purge_count_mismatch, length(Ids), Deleted}})
                    end;
                {ok, Deleted, _Rows} when is_integer(Deleted) ->
                    throw({rollback, {purge_count_mismatch, length(Ids), Deleted}});
                {error, Reason} ->
                    {error, {sql, sql_state(Reason), constraint_name(Reason)}}
            end;
        {error, Reason} ->
            {error, {sql, sql_state(Reason), constraint_name(Reason)}}
    end.

delete_children(Conn, OrgId, WorkspaceId, Ids) ->
    case elib_pg:execute(Conn, ?SQL_DELETE_ASSETS, [OrgId, WorkspaceId, Ids]) of
        {ok, _} ->
            case elib_pg:execute(Conn, ?SQL_DELETE_DELIVERIES, [OrgId, WorkspaceId, Ids]) of
                {ok, _} -> ok;
                {error, _} = Err -> Err
            end;
        {error, _} = Err ->
            Err
    end.

write_audit(Conn, OrgId, WorkspaceId, Now, Ids, Skipped, ObjectFailures) ->
    Event = #{
        id => eb_tsid:new_id(enterprise_audit),
        resource_type => <<"enterprise_message">>,
        resource_id => undefined,
        action => ?PURGE_ACTION,
        detail => #{
            <<"workspace_id">> => WorkspaceId,
            <<"worker">> => <<"eb_pg_purge">>,
            <<"injected_now">> => Now,
            <<"deleted">> => length(Ids),
            <<"message_ids">> => Ids,
            <<"skipped">> => length(Skipped),
            <<"object_delete_failures">> => length(ObjectFailures)
        }
    },
    case eb_pg_audit:append_in(Conn, OrgId, Event) of
        {ok, AuditId} ->
            {ok, summary(Ids, Skipped, AuditId, ObjectFailures)};
        {error, Reason} ->
            {error, {audit_failed, Reason}}
    end.

summary(Ids, Skipped, AuditId, ObjectFailures) ->
    #{
        purged => Ids,
        deleted => length(Ids),
        skipped => Skipped,
        object_delete_failures => ObjectFailures,
        audit_id => AuditId
    }.

%% ===================================================================
%% 参数校验 / 工具
%% ===================================================================

tenant_error(OrgId, WorkspaceId) when is_integer(OrgId), is_integer(WorkspaceId) ->
    ok;
tenant_error(OrgId, WorkspaceId) ->
    {error, {invalid_tenant, {OrgId, WorkspaceId}}}.

%% 注入时钟：缺 now 或非整数 → fail-closed（绝不回落到系统时间）。
clock(Opts) ->
    case maps:get(now, Opts, undefined) of
        Now when is_integer(Now) -> {ok, Now};
        undefined -> {error, {missing_clock, Opts}};
        Other -> {error, {invalid_clock, Other}}
    end.

batch_limit(Opts) ->
    case maps:get(batch_limit, Opts, ?DEFAULT_BATCH_LIMIT) of
        Limit when is_integer(Limit), Limit >= 1, Limit =< ?MAX_BATCH_LIMIT -> {ok, Limit};
        Other -> {error, {invalid_batch_limit, Other}}
    end.

to_unix(Value) when is_integer(Value) ->
    Value;
to_unix(Value) when is_binary(Value) ->
    case elib_dt:rfc3339_to(Value, second) of
        Seconds when is_integer(Seconds) -> Seconds;
        _ -> undefined
    end.

atom_scope(<<"workspace">>) -> workspace;
atom_scope(<<"conversation">>) -> conversation;
atom_scope(<<"message">>) -> message;
atom_scope(Other) -> Other.

value_or_undefined(null) -> undefined;
value_or_undefined(Value) -> Value.

%% epgsql 的查询错误可能直接是 #error{}，也可能被 elib_pg 包成
%% {error, #error{}, Stacktrace} / {Class, Reason, Stack}，故按结构搜索记录。
find_error(#error{} = Err) ->
    Err;
find_error(Tuple) when is_tuple(Tuple) ->
    first_error(tuple_to_list(Tuple));
find_error(List) when is_list(List) ->
    first_error(List);
find_error(_Other) ->
    undefined.

first_error([]) ->
    undefined;
first_error([Head | Rest]) ->
    case find_error(Head) of
        undefined -> first_error(Rest);
        #error{} = Err -> Err
    end.

sql_state(Reason) ->
    case find_error(Reason) of
        #error{code = Code} -> Code;
        undefined -> undefined
    end.

constraint_name(Reason) ->
    case find_error(Reason) of
        #error{extra = Extra} when is_list(Extra) ->
            case lists:keyfind(constraint_name, 1, Extra) of
                {constraint_name, Name} -> Name;
                false -> undefined
            end;
        _ ->
            undefined
    end.
