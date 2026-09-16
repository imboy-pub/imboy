%%% @doc 企业保留策略、Hold 与 bounded purge 的应用层用例。
%%%
%%% 依据：plan v4.1 EB-D12、§2.1 #17、§8 EB-06 A07、§9；作业书 §4 A07。
%%%
%%% ## 保留策略（`open_retention_policy/2`）
%%%
%%%   * 版本由服务端计算（`latest.version + 1`），调用方不能自选版本；
%%%   * **不可缩短**：新版本 `retention_days` 不得小于同 (Org, Workspace, data_class)
%%%     的既有最大值（domain `eb_retention:policy_extension_only/2` 判定，DB 触发器
%%%     纵深）；已接受消息固化的 `retain_until` 因此永不前移；
%%%   * **没有默认保留期**：缺 `retention_days` 一律拒绝。1095d 只由调用方（合成
%%%     fixture）显式给出，本模块不替用户决定生产保留期（plan §9）。
%%%
%%% ## Hold（`create_hold/2` / `release_hold/2`）
%%%
%%%   * hold 是 append-only 事实；release 一次性写入 `released_at` + `released_by`；
%%%   * 真实 hold 的创建/释放属**需担责操作**（plan §9），本模块只接受显式声明
%%%     `synthetic => true` 的调用，否则 `{error, {synthetic_hold_required, synthetic}}`；
%%%   * scope 形状与迁移 117 的 `ck_erh_scope_shape` 同口径（workspace / conversation /
%%%     message 三选一且与 scope_* 列自洽），目标必须在本租户内。
%%%
%%% ## bounded purge（`purge_batch/2`）
%%%
%%% 物理清理**只**经唯一具名用例 `eb_purge_port:purge_batch/4`（`(OrgId, WorkspaceId,
%%% NowMs, Limit)` → `{ok, #{deleted := N}}`），实现经 `eb_infra_ports:resolve(purge)`
%%% 装配。本模块因此**不出现任何持久化实现模块名**（无 `eb_pg_` 前缀）：拿到的能力
%%% 恰好是「删一批到期行」，而不是「一个事务」。worker 内部才用 `SKIP LOCKED`+limit，
%%% 并在同一事务里保证：`retain_until <= now()`、无 active hold、batch limit、
%%% 逐批 append-only 审计；失败宁可多保留。
%%% 本模块只负责：注入时钟、租户参数、批量上限，并把 bypass 类入参
%%%（force / offboarding / ignore_hold / skip_guard）**忽略**——普通角色与离职路径
%%% 都不得提前物理删除。
%%%
%%% ## 边界
%%%
%%% 本模块零 SQL、零 `elib_pg`、不触 `*_repo` / `*_ds`；读写经 `eb_store_port`，
%%% 清理经唯一 purge 用例端口。**不调用**个人 CLIENT_ACK / msg_archive 清理链。
-module(eb_retention_app).

-export([
    open_retention_policy/2,
    latest_retention_policy/2,
    create_hold/2,
    release_hold/2,
    fetch_hold/2,
    hold_lookup_verdict/3,
    purge_batch/2
]).

-define(DATA_CLASSES, [<<"enterprise_message">>, <<"enterprise_asset">>]).
-define(HOLD_SCOPES, [<<"workspace">>, <<"conversation">>, <<"message">>]).
-define(DEFAULT_BATCH_LIMIT, 100).
-define(MAX_BATCH_LIMIT, 1000).

%% ===================================================================
%% 保留策略
%% ===================================================================

%% @doc 新建一个保留策略版本（只允许延长；版本由服务端计算）。
%%
%% Params：
%%   workspace_id     必填整数
%%   data_class       必填，∈ enterprise_message | enterprise_asset
%%   retention_days   必填正整数（**无默认值**）
%%   trigger_event    可选（缺省 `message.accept`）
%%   actor_user_id / created_by_user_id 可选审计快照
%%   store / id / audit 可选端口覆盖
-spec open_retention_policy(integer(), map()) -> {ok, map()} | {error, term()}.
open_retention_policy(OrgId, Params) when is_map(Params) ->
    case tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            policy_args(OrgId, WorkspaceId, Params)
    end;
open_retention_policy(_OrgId, _Params) ->
    {error, {invalid_argument, open_retention_policy}}.

policy_args(OrgId, WorkspaceId, Params) ->
    DataClass = maps:get(data_class, Params, undefined),
    Days = maps:get(retention_days, Params, undefined),
    case lists:member(DataClass, ?DATA_CLASSES) of
        false ->
            {error, {unknown_data_class, DataClass}};
        true ->
            case is_pos_int(Days) of
                false ->
                    {error, {invalid_retention_days, Days}};
                true ->
                    policy_version(OrgId, WorkspaceId, DataClass, Days, Params)
            end
    end.

policy_version(OrgId, WorkspaceId, DataClass, Days, Params) ->
    case
        with_store(Params, fun(Store) ->
            Store:latest_policy(OrgId, WorkspaceId, DataClass)
        end)
    of
        {error, not_found} ->
            policy_extension_gate(OrgId, WorkspaceId, DataClass, Days, 1, [], Params);
        {error, _} = Err ->
            Err;
        {ok, Latest} ->
            Existing = maps:get(version, Latest, 0),
            policy_extension_gate(
                OrgId, WorkspaceId, DataClass, Days, Existing + 1, [Latest], Params
            )
    end.

%% 只允许延长（domain 判定是唯一真源；DB 触发器为纵深防御）。
policy_extension_gate(OrgId, WorkspaceId, DataClass, Days, Version, Existing, Params) ->
    case eb_retention:policy_extension_only(Days, Existing) of
        {error, _} = Err ->
            Err;
        ok ->
            policy_insert(OrgId, WorkspaceId, DataClass, Days, Version, Params)
    end.

policy_insert(OrgId, WorkspaceId, DataClass, Days, Version, Params) ->
    case new_id(enterprise_retention_policy, Params) of
        {error, _} = Err ->
            Err;
        {ok, PolicyId} ->
            Policy = #{
                id => PolicyId,
                data_class => DataClass,
                version => Version,
                retention_days => Days,
                trigger_event => maps:get(trigger_event, Params, <<"message.accept">>),
                created_by_user_id => actor_user_id(Params)
            },
            case
                with_store(Params, fun(Store) ->
                    Store:insert_policy(OrgId, WorkspaceId, Policy)
                end)
            of
                {error, conflict} ->
                    {error, {policy_version_conflict, {DataClass, Version}}};
                {error, _} = Err ->
                    Err;
                {ok, Stored} ->
                    policy_audit(OrgId, WorkspaceId, DataClass, Version, Days, Stored, Params)
            end
    end.

policy_audit(OrgId, WorkspaceId, DataClass, Version, Days, Stored, Params) ->
    Event = #{
        resource_type => <<"enterprise_retention_policy">>,
        resource_id => maps:get(id, Stored),
        action => <<"retention_policy.open">>,
        actor_user_id => actor_user_id(Params),
        detail => #{
            <<"workspace_id">> => WorkspaceId,
            <<"data_class">> => DataClass,
            <<"version">> => Version,
            <<"retention_days">> => Days
        }
    },
    case audit_append(OrgId, Event, Params) of
        {error, _} = Err ->
            Err;
        {ok, AuditId} ->
            {ok, #{
                policy => Stored,
                policy_id => maps:get(id, Stored),
                version => Version,
                retention_days => Days,
                audit_id => AuditId
            }}
    end.

%% @doc 当前生效的保留策略版本（`POLICY-LATEST`，EB-06-A16）。
%%
%% 这条能力此前只是**私有**函数（`policy_version/5` 内部的读），调用方无法直接问
%% 「现在的版本/保留期是多少」。现在它是经 `eb_store_port:latest_policy/3` 的**公开**
%% 入口：调用方不再需要绕过契约去读策略表。
%%
%% Params：`workspace_id` / `data_class` 必填；`store` 可选覆盖。
%% 无策略 ⇒ `{error, not_found}`（**没有**默认保留期，不替用户决定，见 plan §9）。
-spec latest_retention_policy(integer(), map()) -> {ok, map()} | {error, term()}.
latest_retention_policy(OrgId, Params) when is_map(Params) ->
    case tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            case maps:get(data_class, Params, undefined) of
                DataClass when is_binary(DataClass) ->
                    case lists:member(DataClass, ?DATA_CLASSES) of
                        false ->
                            {error, {unknown_data_class, DataClass}};
                        true ->
                            with_store(Params, fun(Store) ->
                                Store:latest_policy(OrgId, WorkspaceId, DataClass)
                            end)
                    end;
                Other ->
                    {error, {unknown_data_class, Other}}
            end
    end;
latest_retention_policy(_OrgId, _Params) ->
    {error, {invalid_argument, latest_retention_policy}}.

%% ===================================================================
%% Hold（合成件）
%% ===================================================================

%% @doc 创建一条**合成**保留 hold（append-only 事实；真实 hold 属人工 Gate）。
%%
%% Params：
%%   workspace_id           必填整数
%%   scope                  必填，∈ workspace | conversation | message
%%   reason_code            必填非空（合成/受控 reason code）
%%   synthetic              必须为 `true`（否则拒绝）
%%   scope_conversation_id  conversation scope 必填；其余必须缺省
%%   scope_message_id       message scope 必填；其余必须缺省
%%   actor_user_id          可选审计快照
-spec create_hold(integer(), map()) -> {ok, map()} | {error, term()}.
create_hold(OrgId, Params) when is_map(Params) ->
    case tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            hold_args(OrgId, WorkspaceId, Params)
    end;
create_hold(_OrgId, _Params) ->
    {error, {invalid_argument, create_hold}}.

hold_args(OrgId, WorkspaceId, Params) ->
    case is_synthetic(Params) of
        false ->
            {error, {synthetic_hold_required, synthetic}};
        true ->
            ReasonCode = maps:get(reason_code, Params, undefined),
            case is_non_empty_binary(ReasonCode) of
                false ->
                    {error, {invalid_reason_code, ReasonCode}};
                true ->
                    hold_scope(OrgId, WorkspaceId, ReasonCode, Params)
            end
    end.

hold_scope(OrgId, WorkspaceId, ReasonCode, Params) ->
    Scope = maps:get(scope, Params, undefined),
    ConversationId = maps:get(scope_conversation_id, Params, undefined),
    MessageId = maps:get(scope_message_id, Params, undefined),
    case lists:member(Scope, ?HOLD_SCOPES) of
        false ->
            {error, {unknown_hold_scope, Scope}};
        true ->
            case hold_scope_shape(Scope, ConversationId, MessageId) of
                {error, _} = Err ->
                    Err;
                ok ->
                    hold_target(
                        OrgId, WorkspaceId, Scope, ConversationId, MessageId, ReasonCode, Params
                    )
            end
    end.

%% 与迁移 117 的 ck_erh_scope_shape 逐字同口径。
hold_scope_shape(<<"workspace">>, undefined, undefined) ->
    ok;
hold_scope_shape(<<"conversation">>, ConversationId, undefined) when is_integer(ConversationId) ->
    ok;
hold_scope_shape(<<"message">>, undefined, MessageId) when is_integer(MessageId) ->
    ok;
hold_scope_shape(Scope, _ConversationId, _MessageId) ->
    {error, {hold_scope_mismatch, Scope}}.

%% 目标必须在本租户内（不靠 DB FK 报错；跨 Org 目标在此被拒）。
hold_target(OrgId, WorkspaceId, <<"workspace">>, undefined, undefined, ReasonCode, Params) ->
    hold_insert(OrgId, WorkspaceId, <<"workspace">>, undefined, undefined, ReasonCode, Params);
hold_target(OrgId, WorkspaceId, <<"conversation">>, ConversationId, undefined, ReasonCode, Params) ->
    case
        with_store(Params, fun(Store) ->
            Store:fetch_conversation(OrgId, WorkspaceId, ConversationId)
        end)
    of
        {error, not_found} ->
            {error, {hold_target_not_in_scope, ConversationId}};
        {error, _} = Err ->
            Err;
        {ok, _Conversation} ->
            hold_insert(
                OrgId,
                WorkspaceId,
                <<"conversation">>,
                ConversationId,
                undefined,
                ReasonCode,
                Params
            )
    end;
hold_target(OrgId, WorkspaceId, <<"message">>, undefined, MessageId, ReasonCode, Params) ->
    case with_store(Params, fun(Store) -> Store:fetch_message(OrgId, WorkspaceId, MessageId) end) of
        {error, not_found} ->
            {error, {hold_target_not_in_scope, MessageId}};
        {error, _} = Err ->
            Err;
        {ok, _Message} ->
            hold_insert(OrgId, WorkspaceId, <<"message">>, undefined, MessageId, ReasonCode, Params)
    end.

hold_insert(OrgId, WorkspaceId, Scope, ConversationId, MessageId, ReasonCode, Params) ->
    case new_id(enterprise_retention_hold, Params) of
        {error, _} = Err ->
            Err;
        {ok, HoldId} ->
            Hold = #{
                id => HoldId,
                scope_type => Scope,
                scope_conversation_id => ConversationId,
                scope_message_id => MessageId,
                reason_code => ReasonCode,
                actor_user_id => actor_user_id(Params)
            },
            case
                with_store(Params, fun(Store) ->
                    Store:insert_hold(OrgId, WorkspaceId, Hold)
                end)
            of
                {error, _} = Err ->
                    Err;
                {ok, Stored} ->
                    hold_audit(OrgId, WorkspaceId, Scope, ReasonCode, Stored, Params)
            end
    end.

hold_audit(OrgId, WorkspaceId, Scope, ReasonCode, Stored, Params) ->
    Event = #{
        resource_type => <<"enterprise_retention_hold">>,
        resource_id => maps:get(id, Stored),
        action => <<"retention_hold.create">>,
        actor_user_id => actor_user_id(Params),
        detail => #{
            <<"workspace_id">> => WorkspaceId,
            <<"scope">> => Scope,
            <<"reason_code">> => ReasonCode,
            <<"synthetic_hold">> => true
        }
    },
    case audit_append(OrgId, Event, Params) of
        {error, _} = Err ->
            Err;
        {ok, AuditId} ->
            {ok, #{
                hold => Stored,
                hold_id => maps:get(id, Stored),
                scope => Scope,
                audit_id => AuditId
            }}
    end.

%% @doc 释放一条**合成** hold（一次性写入 `released_at` + `released_by`，append-only）。
-spec release_hold(integer(), map()) -> {ok, map()} | {error, term()}.
release_hold(OrgId, Params) when is_map(Params) ->
    case tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            release_args(OrgId, WorkspaceId, Params)
    end;
release_hold(_OrgId, _Params) ->
    {error, {invalid_argument, release_hold}}.

release_args(OrgId, WorkspaceId, Params) ->
    HoldId = maps:get(hold_id, Params, undefined),
    case is_synthetic(Params) of
        false ->
            {error, {synthetic_hold_required, synthetic}};
        true ->
            case is_pos_int(HoldId) of
                false ->
                    {error, {invalid_hold_id, HoldId}};
                true ->
                    release_gate(OrgId, WorkspaceId, HoldId, Params)
            end
    end.

%% 精确三态裁决（EB-06-A12 的机械判据，**纯函数**）。
%%
%% 三种结果必须**可区分**，不得都归成 `not_found`：
%%   * 跨 Org / 租户错配 → `{error, {workspace_not_in_org, Ws}}`（连查询都不会发出去）
%%   * 本租户内不存在     → `{error, {hold_not_found, HoldId}}`
%%   * 存在但已释放       → `{ok, #{status := released, released_at := T}}`
%%   * 存在且 active      → `{ok, #{status := active}}`
%%
%% 「已释放」是**正常读数**而不是错误：append-only 的 hold 行 forever 存在，调用方
%% 需要区分「没有这条 hold」与「这条 hold 已经释放了」。把它压成 `not_found` 会让
%% 审计与运维无法回答「为什么这条消息现在能删了」。
-spec hold_lookup_verdict(term(), term(), integer()) -> {ok, map()} | {error, term()}.
hold_lookup_verdict({error, {workspace_not_in_org, _} = Reason}, _Fetch, _HoldId) ->
    {error, Reason};
hold_lookup_verdict(ok, {ok, Hold}, _HoldId) ->
    {ok, hold_status(Hold)};
hold_lookup_verdict(ok, {error, not_found}, HoldId) ->
    {error, {hold_not_found, HoldId}};
hold_lookup_verdict(ok, {error, Reason}, _HoldId) ->
    {error, Reason}.

%% @doc 读取**单条** hold 事实（含 released_at；EB-06-A12 / E6-4）。
%%
%% Params：`workspace_id` / `hold_id` 必填；`store` 可选覆盖。
%%
%% 与 `release_hold/2` 共用同一套裁决（`hold_lookup_verdict/3`），因此「不存在 /
%% 已释放 / 跨 Org」在两条入口上的结论逐字一致。
-spec fetch_hold(integer(), map()) -> {ok, map()} | {error, term()}.
fetch_hold(OrgId, Params) when is_map(Params) ->
    case tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            case maps:get(hold_id, Params, undefined) of
                HoldId when is_integer(HoldId), HoldId > 0 ->
                    hold_lookup(OrgId, WorkspaceId, HoldId, Params);
                Other ->
                    {error, {invalid_hold_id, Other}}
            end
    end;
fetch_hold(_OrgId, _Params) ->
    {error, {invalid_argument, fetch_hold}}.

%% 查询路径：租户作用域自检 → 单条读取 → 三态裁决。
hold_lookup(OrgId, WorkspaceId, HoldId, Params) ->
    Scope =
        case hold_scope_ok(OrgId, WorkspaceId, Params) of
            ok -> ok;
            {error, _} = Err -> Err
        end,
    Fetch =
        case Scope of
            ok ->
                with_store(Params, fun(Store) ->
                    Store:fetch_hold(OrgId, WorkspaceId, HoldId)
                end);
            {error, _} ->
                {error, scope_short_circuit}
        end,
    hold_lookup_verdict(Scope, Fetch, HoldId).

%% 与 `open_conversation`/`handover` 同口径：同语句带 Org + Workspace 证明归属。
%% Workspace 不属于该 Org ⇒ `{error, {workspace_not_in_org, Ws}}`（跨租户在此被拒，
%% 不会退化成「查不到」）。
hold_scope_ok(OrgId, WorkspaceId, Params) ->
    case with_store(Params, fun(Store) -> Store:list_assignments(OrgId, WorkspaceId) end) of
        {error, {workspace_not_in_org, _} = Reason} -> {error, Reason};
        {error, _} = Err -> Err;
        {ok, _Assignments} -> ok
    end.

%% hold 行的状态视图：`released_at` 是唯一的释放事实（append-only，不可回改）。
hold_status(Hold) ->
    case maps:get(released_at, Hold, undefined) of
        undefined ->
            #{
                hold_id => maps:get(id, Hold, undefined),
                status => active,
                hold => Hold,
                released_at => undefined
            };
        ReleasedAt ->
            #{
                hold_id => maps:get(id, Hold, undefined),
                status => released,
                hold => Hold,
                released_at => ReleasedAt
            }
    end.

%% 迁移 117 的 append-only 守卫要求 release 必须同时写入 released_at 与
%% released_by_user_id ⇒ 释放人缺失时在此 fail-closed（与 DB 同口径，零副作用）。
%%
%% 释放前的三态判定（EB-06-A12）：
%%   * 跨 Org / 租户错配 ⇒ `{error, {workspace_not_in_org, Ws}}`
%%   * 不存在           ⇒ `{error, {hold_not_found, HoldId}}`
%%   * 已释放           ⇒ `{error, {hold_already_released, HoldId, ReleasedAt}}`
%%     （append-only：不二次 release，也**不谎称**从未存在）
%% 这三条此前被压成同一个 `hold_not_releasable`（notes[EB06-C2]），现已拆开。
release_gate(OrgId, WorkspaceId, HoldId, Params) ->
    ReleasedBy = actor_user_id(Params),
    case is_pos_int(ReleasedBy) of
        false ->
            {error, {invalid_released_by, ReleasedBy}};
        true ->
            case hold_lookup(OrgId, WorkspaceId, HoldId, Params) of
                {error, _} = Err ->
                    Err;
                {ok, #{status := released, released_at := ReleasedAt}} ->
                    {error, {hold_already_released, HoldId, ReleasedAt}};
                {ok, #{hold := Hold}} ->
                    release_cas(OrgId, WorkspaceId, HoldId, Hold, ReleasedBy, Params)
            end
    end.

release_cas(OrgId, WorkspaceId, HoldId, Hold, ReleasedBy, Params) ->
    case
        with_store(Params, fun(Store) ->
            Store:release_hold(OrgId, WorkspaceId, HoldId, ReleasedBy)
        end)
    of
        {error, conflict} ->
            %% CAS 未命中（并发释放/状态变化）：交给调用方重试，绝不当作成功。
            {error, {hold_release_conflict, HoldId}};
        {error, _} = Err ->
            Err;
        ok ->
            release_audit(OrgId, WorkspaceId, HoldId, ReleasedBy, Hold, Params)
    end.

release_audit(OrgId, WorkspaceId, HoldId, ReleasedBy, Hold, Params) ->
    Event = #{
        resource_type => <<"enterprise_retention_hold">>,
        resource_id => HoldId,
        action => <<"retention_hold.release">>,
        actor_user_id => ReleasedBy,
        detail => #{
            <<"workspace_id">> => WorkspaceId,
            <<"scope">> => maps:get(scope, Hold, undefined),
            <<"synthetic_hold">> => true
        }
    },
    case audit_append(OrgId, Event, Params) of
        {error, _} = Err ->
            Err;
        {ok, AuditId} ->
            {ok, #{hold_id => HoldId, released_by => ReleasedBy, audit_id => AuditId}}
    end.

%% ===================================================================
%% bounded purge（唯一物理清理通道的应用层入口）
%% ===================================================================

%% @doc 执行一批 bounded purge（显式 Org/Workspace + 注入时钟毫秒 + 批量上限）。
%%
%% Params：
%%   workspace_id  必填整数
%%   now           可选（Unix 秒；缺省经注入时钟端口）——**绝不**回落到 domain 侧隐式时间
%%   batch_limit   可选（1..1000，缺省 100）
%%   purge         可选端口覆盖（缺省经 `eb_infra_ports:resolve(purge)` 装配）
%%
%% 语义（全部由 purge 用例端口 + DB 守卫裁决，本模块不做例外）：
%%   `retain_until` 未到 / active hold / 附件保留期更长 / 非 purge 角色 ⇒ 一行不删。
%%   调用方传入的 `force` / `offboarding` / `ignore_hold` / `skip_guard` 等键**一律忽略**。
%%
%% 信封是 `/4` 的窄形状 `(OrgId, WorkspaceId, NowMs, Limit)`：本模块把注入的 Unix 秒
%% 换算成毫秒后交给端口（端口再换算回秒），调用方**拿不到**「事务」或「任意 SQL」。
-spec purge_batch(integer(), map()) -> {ok, map()} | {error, term()}.
purge_batch(OrgId, Params) when is_map(Params) ->
    case tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            purge_args(OrgId, WorkspaceId, Params)
    end;
purge_batch(_OrgId, _Params) ->
    {error, {invalid_argument, purge_batch}}.

purge_args(OrgId, WorkspaceId, Params) ->
    case purge_now(Params) of
        {error, _} = Err ->
            Err;
        {ok, Now} ->
            case batch_limit(Params) of
                {error, _} = Err ->
                    Err;
                {ok, Limit} ->
                    run_purge(OrgId, WorkspaceId, Now, Limit, Params)
            end
    end.

purge_now(Params) ->
    case maps:get(now, Params, undefined) of
        Now when is_integer(Now) ->
            {ok, Now};
        undefined ->
            case clock_port(Params) of
                {ok, Clock} ->
                    try
                        {ok, Clock:now()}
                    catch
                        Class:Reason -> {error, {clock_unavailable, {Class, Reason}}}
                    end;
                {error, _} = Err ->
                    Err
            end;
        Other ->
            {error, {invalid_clock, Other}}
    end.

batch_limit(Params) ->
    case maps:get(batch_limit, Params, ?DEFAULT_BATCH_LIMIT) of
        Limit when is_integer(Limit), Limit >= 1, Limit =< ?MAX_BATCH_LIMIT ->
            {ok, Limit};
        Other ->
            {error, {invalid_batch_limit, Other}}
    end.

%% 具名用例端口调用：`NowMs`（毫秒）与 `Limit` 是端口 `/4` 信封的全部参数。
%% 本模块**不转发**任何 Opts map —— 「过渡信封」/3 已随 EB-06 删除（A18）。
run_purge(OrgId, WorkspaceId, Now, Limit, Params) ->
    case purge_worker(Params) of
        {error, _} = Err ->
            Err;
        {ok, Purge} ->
            NowMs = Now * 1000,
            try Purge:purge_batch(OrgId, WorkspaceId, NowMs, Limit) of
                {ok, Summary} -> {ok, Summary};
                {error, _} = Err -> Err
            catch
                Class:Reason -> {error, {purge_failed, {Class, Reason}}}
            end
    end.

%% 唯一物理清理用例端口的装配默认：经**用例级 Port** `eb_purge_port` 解析
%%（实现按装配选择，application 层不出现实现模块名）。
purge_worker(Params) ->
    case maps:get(purge, Params, undefined) of
        Mod when is_atom(Mod), Mod =/= undefined -> {ok, Mod};
        _ -> eb_infra_ports:resolve(purge)
    end.

%% ===================================================================
%% 内部辅助
%% ===================================================================

port(Key, Params) ->
    case maps:get(Key, Params, undefined) of
        Mod when is_atom(Mod), Mod =/= undefined -> {ok, Mod};
        _ -> eb_infra_ports:resolve(Key)
    end.

with_store(Params, Fun) ->
    case port(store, Params) of
        {ok, Store} -> Fun(Store);
        {error, _} = Err -> Err
    end.

audit_append(OrgId, Event, Params) ->
    case port(audit, Params) of
        {error, _} = Err ->
            Err;
        {ok, Audit} ->
            case Audit:append(OrgId, Event) of
                {ok, AuditId} -> {ok, AuditId};
                {error, Reason} -> {error, {audit_append_failed, Reason}}
            end
    end.

new_id(Kind, Params) ->
    case port(id, Params) of
        {error, _} = Err ->
            Err;
        {ok, IdPort} ->
            try
                {ok, IdPort:new_id(Kind)}
            catch
                Class:Reason -> {error, {id_generation_failed, Kind, {Class, Reason}}}
            end
    end.

%% 真实 hold 的创建/释放属需担责操作：只接受显式 synthetic 声明。
is_synthetic(Params) ->
    maps:get(synthetic, Params, false) =:= true.

clock_port(Params) ->
    case maps:get(clock, Params, undefined) of
        Mod when is_atom(Mod), Mod =/= undefined -> {ok, Mod};
        _ -> eb_infra_ports:resolve(clock)
    end.

tenant(OrgId, Params) ->
    case is_integer(OrgId) andalso OrgId > 0 of
        false ->
            {error, {invalid_organization_id, OrgId}};
        true ->
            case maps:get(workspace_id, Params, undefined) of
                WorkspaceId when is_integer(WorkspaceId), WorkspaceId > 0 ->
                    {ok, WorkspaceId};
                Other ->
                    {error, {invalid_workspace_id, Other}}
            end
    end.

actor_user_id(Params) ->
    first_defined([
        maps:get(actor_user_id, Params, undefined),
        maps:get(created_by_user_id, Params, undefined)
    ]).

first_defined([]) ->
    undefined;
first_defined([undefined | Rest]) ->
    first_defined(Rest);
first_defined([Value | _Rest]) ->
    Value.

is_pos_int(Value) ->
    is_integer(Value) andalso Value > 0.

is_non_empty_binary(Value) ->
    is_binary(Value) andalso byte_size(Value) > 0.
