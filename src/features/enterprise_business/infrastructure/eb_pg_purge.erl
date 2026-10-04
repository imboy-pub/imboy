%%% Bounded retention purge: metadata, durable object intents and audit commit together.
%%% Workspace lock serializes new holds; candidate row locks serialize confirmation.
%%% Object deletion runs only after commit and retries from durable keys.
-module(eb_pg_purge).

-moduledoc "bounded retention purge 仓储 —— 元数据、持久化对象 intent 与审计同事务提交；Workspace 锁串行化新 hold。".
-include_lib("epgsql/include/epgsql.hrl").

-export([purge_batch/3, sql_statements/0]).

-define(DEFAULT_BATCH_LIMIT, 100).
-define(MAX_BATCH_LIMIT, 1000).
-define(PURGE_ACTION, <<"message.purge">>).
-define(ORPHAN_PURGE_ACTION, <<"asset.purge">>).
-define(PURGE_GUC, <<"SET LOCAL imboy.enterprise_purge = 'on'">>).

%% F-1：孤儿 pending 资产的年龄阈值（秒），默认 24h。理由：
%%   * 上传凭证 TTL 上界 1h（`?MAX_UPLOAD_TTL_SEC`）——超龄即**永久**不可能再被
%%     confirm，清理不误伤任何在途上传；
%%   * 与既有配置惯例同量级（`enterprise_internal_idempotency_ttl_seconds = 86400`），
%%     适配按天调度的 bounded purge 运维节奏；
%%   * 对比 `eb_asset_app:cleanup_pending/2` 的 1h 默认更保守：purge 是不可逆物理
%%     通道，多留一天换时钟偏差 / 排查窗口的余量。
-define(DEFAULT_ORPHAN_ASSET_AGE_SEC, 86400).

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

%% F-1：孤儿 pending 资产候选。retain_until 门与 DB 守卫
%% （trg_enterprise_asset_purge_guard 的 retain_until <= now()）同判据——
%% 未固化的（presign 未带保留期，孤儿常态）视为可清；已固化未到期的一律不选，
%% 避免对象已被预删而事务被守卫整批回滚。$3 = 过期线（(Now-age) 毫秒），
%% $4 = 注入时钟毫秒（供 retain_until 门使用），$5 = LIMIT。
-define(SQL_ORPHAN_ASSET_CANDIDATES, <<
    "SELECT id, organization_id, workspace_id, conversation_id, retain_until"
    "  FROM enterprise_asset"
    " WHERE organization_id = $1 AND workspace_id = $2"
    "   AND status = 'pending_confirm'"
    "   AND message_id IS NULL"
    "   AND created_at <= to_timestamp($3::bigint/1000)"
    "   AND (retain_until IS NULL OR retain_until <= to_timestamp($4::bigint/1000))"
    " ORDER BY created_at, id"
    " LIMIT $5"
    " FOR UPDATE SKIP LOCKED"
>>).

%% F-1：孤儿资产元数据删除。语句内复述候选谓词（纵深防御：即便候选与删除之间
%% 状态被并发改写，也不会误删非孤儿行；计数不符即整批回滚）。
-define(SQL_DELETE_ORPHAN_ASSETS, <<
    "DELETE FROM enterprise_asset"
    " WHERE organization_id = $1 AND workspace_id = $2 AND id = ANY($3::bigint[])"
    "   AND status = 'pending_confirm' AND message_id IS NULL"
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
%% Opts：`now`（必填，注入时钟 Unix 秒）、`batch_limit`（可选，1..1000，默认 100）、
%% `orphan_asset_age_seconds`（可选，F-1 孤儿 pending 资产年龄阈值；缺省读
%% `imboy` app env `eb_purge_orphan_asset_age_seconds`，再缺省 86400）。
%%
%% 返回 `{ok, #{purged, deleted, orphan_assets_purged, orphan_assets_deleted,
%% skipped, object_delete_failures, audit_id}}` 或 `{error, Reason}`。
%% `skipped` 是 `[{MessageId|AssetId, {ineligible|orphan_asset_ineligible, Reason}}]`
%% （domain 裁决），`deleted` 恒等于 `length(purged)`（消息数；孤儿资产数单列）。
-spec purge_batch(integer(), integer(), map()) -> {ok, map()} | {error, term()}.
purge_batch(OrgId, WorkspaceId, Opts) when is_map(Opts) ->
    case validate(OrgId, WorkspaceId, Opts) of
        {ok, Now, Limit, Age} ->
            run(OrgId, WorkspaceId, Now, Limit, Age);
        {error, _} = Err ->
            Err
    end;
purge_batch(_OrgId, _WorkspaceId, _Opts) ->
    {error, invalid_opts}.

%% 参数校验顺序固定：租户 → 注入时钟 → 批量上限 → 孤儿年龄（任一失败都不触库）。
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
                        {error, _} = Err ->
                            Err;
                        {ok, Limit} ->
                            case orphan_asset_age(Opts) of
                                {error, _} = Err -> Err;
                                {ok, Age} -> {ok, Now, Limit, Age}
                            end
                    end
            end
    end.

%% @doc 冻结的语句列表（全部带 $1/$2 与 organization_id + workspace_id）。
-spec sql_statements() -> [binary()].
sql_statements() ->
    [
        ?SQL_CANDIDATES,
        ?SQL_ACTIVE_HOLDS,
        ?SQL_DELETE_ASSETS,
        ?SQL_DELETE_DELIVERIES,
        ?SQL_DELETE_MESSAGES,
        ?SQL_ORPHAN_ASSET_CANDIDATES,
        ?SQL_DELETE_ORPHAN_ASSETS
    ].

%% ===================================================================
%% 事务主体
%% ===================================================================

%% ponytail: serialize each workspace claim; use narrower hold locks if measured contention warrants it.
run(OrgId, WorkspaceId, Now, Limit, AgeSec) ->
    Result = elib_pg:with_tx(
        fun(Conn) ->
            case
                elib_pg:query(
                    Conn,
                    <<"SELECT id FROM workspace WHERE organization_id=$1 AND id=$2 FOR UPDATE">>,
                    [OrgId, WorkspaceId]
                )
            of
                {ok, _} -> ok;
                {error, Reason} -> throw({rollback, {workspace_lock_failed, Reason}})
            end,
            case do_purge(Conn, OrgId, WorkspaceId, Now, Limit, AgeSec) of
                {error, Reason1} -> throw({rollback, Reason1});
                Success -> Success
            end
        end,
        [{reraise, true}]
    ),
    case Result of
        {rollback, Reason} ->
            {error, Reason};
        {ok, Summary} ->
            case eb_pg_asset_delete_queue:drain(OrgId, WorkspaceId, Limit) of
                {ok, Failures} -> {ok, Summary#{object_delete_failures => Failures}};
                {error, Reason} -> {error, Reason}
            end;
        Other ->
            Other
    end.

do_purge(
    Conn,
    OrgId,
    WorkspaceId,
    Now,
    Limit,
    AgeSec
) ->
    %% 1) 进入 bounded purge 上下文（DB 守卫第一步；失败宁可多保留）
    case epgsql:squery(Conn, ?PURGE_GUC) of
        {ok, _, _} ->
            case candidates(Conn, OrgId, WorkspaceId, Now, Limit) of
                {ok, Candidates} ->
                    case active_holds(Conn, OrgId, WorkspaceId) of
                        {ok, Holds} ->
                            {Eligible, Skipped} = eligible(Candidates, Holds, Now),
                            orphan_phase(
                                Conn,
                                OrgId,
                                WorkspaceId,
                                Now,
                                Limit,
                                AgeSec,
                                Holds,
                                Eligible,
                                Skipped
                            );
                        {error, _} = Err ->
                            Err
                    end;
                {error, _} = Err ->
                    Err
            end;
        {error, Reason} ->
            {error, {purge_context_failed, Reason}}
    end.

%% F-1: lock orphan candidates inside the metadata transaction.
%% Eligibility and deletion are in the same transaction; queued objects are drained after commit.
orphan_phase(
    Conn,
    OrgId,
    WorkspaceId,
    Now,
    Limit,
    AgeSec,
    Holds,
    MsgEligible,
    Skipped0
) ->
    case orphan_candidates(Conn, OrgId, WorkspaceId, Now, Limit, AgeSec) of
        {error, _} = Err ->
            Err;
        {ok, Orphans} ->
            {OrphanEligible, Skipped1} = orphan_eligible(Orphans, Holds, Now),
            Skipped = Skipped0 ++ Skipped1,
            Failures = [],
            case {MsgEligible, OrphanEligible} of
                {[], []} ->
                    %% 无可清理行：不删、不写审计（跳过原因如实返回）
                    {ok, summary([], [], Skipped, undefined, Failures)};
                {_, _} ->
                    delete_and_audit(
                        Conn,
                        OrgId,
                        WorkspaceId,
                        Now,
                        MsgEligible,
                        OrphanEligible,
                        Skipped,
                        Failures
                    )
            end
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

%% F-1: transaction-local orphan candidates.
orphan_candidates(Conn, OrgId, WorkspaceId, Now, Limit, AgeSec) ->
    case
        elib_pg:query(Conn, ?SQL_ORPHAN_ASSET_CANDIDATES, [
            OrgId, WorkspaceId, (Now - AgeSec) * 1000, Now * 1000, Limit
        ])
    of
        {ok, Rows} ->
            {ok, [orphan_asset_map(Row) || Row <- Rows]};
        {error, Reason} ->
            {error, {orphan_candidate_query_failed, Reason}}
    end.

orphan_asset_map(Row) ->
    #{
        id => maps:get(<<"id">>, Row),
        organization_id => maps:get(<<"organization_id">>, Row),
        workspace_id => maps:get(<<"workspace_id">>, Row),
        conversation_id => value_or_undefined(maps:get(<<"conversation_id">>, Row)),
        retain_until => or_unix(maps:get(<<"retain_until">>, Row))
    }.

%% F-1：孤儿资产的资格裁决（域侧纵深；DB 守卫仍是最终兜底）：
%%   1. retain_until 已固化且未到期 → 跳过（与 trg_enterprise_asset_purge_guard 同判据；
%%      未固化 = presign 未带保留期，孤儿常态，视为可清）；
%%   2. 任一 active hold 覆盖（workspace / conversation；message scope 对
%%      message_id NULL 恒不覆盖）→ 跳过，避免整批被 DB 守卫回滚（失败宁可多保留）；
%%   3. 否则 eligible（age 与 message_id IS NULL 已由候选 SQL 保证）。
orphan_eligible(Orphans, Holds, Now) ->
    lists:foldr(
        fun(Asset, {Ok, Skipped}) ->
            case orphan_asset_gate(Asset, Holds, Now) of
                eligible ->
                    {[Asset | Ok], Skipped};
                {ineligible, Reason} ->
                    {Ok, [{maps:get(id, Asset), {orphan_asset_ineligible, Reason}} | Skipped]}
            end
        end,
        {[], []},
        Orphans
    ).

orphan_asset_gate(Asset, Holds, Now) ->
    case maps:get(retain_until, Asset) of
        RetainUntil when is_integer(RetainUntil), RetainUntil > Now ->
            {ineligible, retain_not_reached};
        _ExpiredOrUnbound ->
            Target = #{
                organization_id => maps:get(organization_id, Asset),
                workspace_id => maps:get(workspace_id, Asset),
                conversation_id => maps:get(conversation_id, Asset),
                message_id => undefined
            },
            case lists:any(fun(Hold) -> eb_retention:hold_covers(Hold, Target) end, Holds) of
                true -> {ineligible, active_hold};
                false -> eligible
            end
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

delete_and_audit(Conn, OrgId, WorkspaceId, Now, MsgEligible, OrphanEligible, Skipped, Failures) ->
    Ids = [maps:get(message_id, Message) || Message <- MsgEligible],
    OrphanIds = [maps:get(id, Asset) || Asset <- OrphanEligible],
    ok = eb_pg_asset_delete_queue:enqueue_in(Conn, OrgId, WorkspaceId, Ids, OrphanIds),
    %% 先删子表（asset / delivery），再删消息：与 RESTRICT FK 顺序一致；
    %% 孤儿资产（F-1）无消息外键，随后按 id 精确删除。
    case delete_message_rows(Conn, OrgId, WorkspaceId, Now, Ids) of
        ok ->
            case delete_orphan_rows(Conn, OrgId, WorkspaceId, OrphanIds) of
                ok ->
                    write_audit(Conn, OrgId, WorkspaceId, Now, Ids, OrphanIds, Skipped, Failures);
                {error, Reason} ->
                    {error, {sql, sql_state(Reason), constraint_name(Reason)}}
            end;
        {error, Reason} ->
            {error, {sql, sql_state(Reason), constraint_name(Reason)}}
    end.

delete_message_rows(_Conn, _OrgId, _WorkspaceId, _Now, []) ->
    ok;
delete_message_rows(Conn, OrgId, WorkspaceId, Now, Ids) ->
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
                            ok;
                        false ->
                            %% 影响行数与候选不符（并发/守卫变化）→ 整批回滚，多保留
                            throw({rollback, {purge_count_mismatch, length(Ids), Deleted}})
                    end;
                {ok, Deleted, _Rows} when is_integer(Deleted) ->
                    throw({rollback, {purge_count_mismatch, length(Ids), Deleted}});
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% F-1：孤儿资产删除。语句内复述候选谓词（status/message_id 租户两键），
%% 计数不符（并发改写）→ 整批回滚，多保留。
delete_orphan_rows(_Conn, _OrgId, _WorkspaceId, []) ->
    ok;
delete_orphan_rows(Conn, OrgId, WorkspaceId, OrphanIds) ->
    case elib_pg:execute(Conn, ?SQL_DELETE_ORPHAN_ASSETS, [OrgId, WorkspaceId, OrphanIds]) of
        {ok, Deleted} when is_integer(Deleted) ->
            case Deleted =:= length(OrphanIds) of
                true ->
                    ok;
                false ->
                    throw({rollback, {orphan_purge_count_mismatch, length(OrphanIds), Deleted}})
            end;
        {ok, Deleted, _Rows} when is_integer(Deleted) ->
            throw({rollback, {orphan_purge_count_mismatch, length(OrphanIds), Deleted}});
        {error, Reason} ->
            {error, Reason}
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

write_audit(Conn, OrgId, WorkspaceId, Now, Ids, OrphanIds, Skipped, Failures) ->
    %% 逐批一条审计：批内删了消息 → message.purge（既有口径不变）；
    %% 只删了孤儿资产 → asset.purge（F-1）。一行未删不会走到这里（无空审计）。
    {Action, ResourceType} =
        case Ids of
            [] -> {?ORPHAN_PURGE_ACTION, <<"enterprise_asset">>};
            _ -> {?PURGE_ACTION, <<"enterprise_message">>}
        end,
    Event = #{
        id => eb_tsid:new_id(enterprise_audit),
        resource_type => ResourceType,
        resource_id => undefined,
        action => Action,
        detail => #{
            <<"workspace_id">> => WorkspaceId,
            <<"worker">> => <<"eb_pg_purge">>,
            <<"injected_now">> => Now,
            <<"deleted">> => length(Ids),
            <<"message_ids">> => Ids,
            <<"orphan_assets_deleted">> => length(OrphanIds),
            <<"orphan_asset_ids">> => OrphanIds,
            <<"skipped">> => length(Skipped),
            <<"object_delete_failures">> => length(Failures)
        }
    },
    case eb_pg_audit:append_in(Conn, OrgId, Event) of
        {ok, AuditId} ->
            {ok, summary(Ids, OrphanIds, Skipped, AuditId, Failures)};
        {error, Reason} ->
            {error, {audit_failed, Reason}}
    end.

summary(Ids, OrphanIds, Skipped, AuditId, Failures) ->
    #{
        purged => Ids,
        deleted => length(Ids),
        orphan_assets_purged => OrphanIds,
        orphan_assets_deleted => length(OrphanIds),
        skipped => Skipped,
        object_delete_failures => Failures,
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

%% F-1：孤儿资产年龄阈值。解析顺序：显式 Opts → `imboy` app env
%% `eb_purge_orphan_asset_age_seconds` → ?DEFAULT_ORPHAN_ASSET_AGE_SEC。
%% 非法值 fail-closed（purge 整体拒绝 ⇒ 宁可多保留），绝不静默回落默认值。
orphan_asset_age(Opts) ->
    case maps:get(orphan_asset_age_seconds, Opts, env_orphan_asset_age()) of
        Age when is_integer(Age), Age >= 0 -> {ok, Age};
        Other -> {error, {invalid_orphan_asset_age, Other}}
    end.

env_orphan_asset_age() ->
    application:get_env(
        imboy, eb_purge_orphan_asset_age_seconds, ?DEFAULT_ORPHAN_ASSET_AGE_SEC
    ).

to_unix(Value) when is_integer(Value) ->
    Value;
to_unix(Value) when is_binary(Value) ->
    case elib_dt:rfc3339_to(Value, second) of
        Seconds when is_integer(Seconds) -> Seconds;
        _ -> undefined
    end.

%% F-1：孤儿资产的 retain_until 允许 NULL（presign 未带保留期）——
%% null 原样映射为 undefined（未固化），不进 to_unix（binary/integer 专用）。
or_unix(null) -> undefined;
or_unix(Value) -> to_unix(Value).

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
