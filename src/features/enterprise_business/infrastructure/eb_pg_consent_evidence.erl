%%% @doc EB-03R M2：同意证据类别的**唯一写入路径**（`consent_evidence_kind`）。
%%%
%%% 依据：Q4 裁决 + EB-03R §2.4 M2——
%%%   * 列**可空**、**无 DEFAULT**；无 consent 时必须 `NULL`；
%%%   * 有 consent 且 V1 可接受证据时，取值**恰为** `'synthetic'`；
%%%   * `'real'` / `'verified_real'` / 其他取值由 **DB 拒绝**（`ck_ec_consent_evidence_kind`）；
%%%   * 迁移**不得**回填历史行（本模块也无任何回填语句）。
%%%
%%% ## 为什么写入值是**字面量**而不是参数
%%%
%%% A07 的机械判定要读「该列的 INSERT/UPDATE 实参」：本模块把 `'synthetic'` 写成
%%% **SQL 字面量**，且 SQL 里**没有**任何 `DEFAULT`、没有任何其他取值分支。
%%% 调用方因此不可能通过参数注入 `'real'`——不是「应用层拒绝」，而是**能力上不存在**。
%%%
%%% ## 报告口径（M2-d）
%%%
%%% 本模块与全卡报告只允许声明 **`synthetic_state_machine_only`**：它表示
%%% 「V1 本地状态机可接受的合成证据」，**不构成**真实同意 / 真实告知 / 任何生产合规结论。
-module(eb_pg_consent_evidence).

-moduledoc "同意证据类别的唯一写入路径（EB-03R M2，consent_evidence_kind）。".
-export([record_synthetic_consent/3, fetch_consent_evidence_kind/3, sql_statements/0]).

%% 精确一条语句：只写字面量 'synthetic'，且要求该会话确有 consent（consent_at 非空）。
%% 无 consent 的会话写不进去（0 行）——「无 consent 必须 NULL」由 DB CHECK 与本语句双保险。
-define(SQL_RECORD_SYNTHETIC, <<
    "UPDATE enterprise_conversation c"
    "   SET consent_evidence_kind = 'synthetic',"
    "       version = c.version + 1,"
    "       updated_at = now()"
    " WHERE c.organization_id = $1 AND c.workspace_id = $2 AND c.id = $3"
    "   AND c.consent_at IS NOT NULL"
    "   AND c.consent_evidence_kind IS DISTINCT FROM 'synthetic'"
    " RETURNING c.id"
>>).

-define(SQL_FETCH_KIND, <<
    "SELECT c.consent_evidence_kind, c.consent_at"
    "  FROM enterprise_conversation c"
    "  JOIN workspace w ON w.id = $2 AND w.organization_id = c.organization_id"
    " WHERE c.organization_id = $1 AND c.workspace_id = $2 AND c.id = $3"
>>).

%% @doc 冻结语句（供机械判据：只允许 'synthetic' 字面量，无 DEFAULT）。
-spec sql_statements() -> [binary()].
sql_statements() ->
    [?SQL_RECORD_SYNTHETIC, ?SQL_FETCH_KIND].

%% @doc 记录 V1 可接受的**合成**同意证据（幂等：已在 `'synthetic'` 时 0 行 → `ok`）。
%%
%% 无 consent（`consent_at IS NULL`）的会话 → `{error, no_consent}`（不得伪装成有证据）。
-spec record_synthetic_consent(integer(), integer(), integer()) -> ok | {error, term()}.
record_synthetic_consent(OrgId, WorkspaceId, ConversationId) ->
    case eb_pg_exec:tenant_error(OrgId, WorkspaceId) of
        ok ->
            case
                eb_pg_exec:execute_returning(?SQL_RECORD_SYNTHETIC, [
                    OrgId, WorkspaceId, ConversationId
                ])
            of
                {ok, [_ | _]} ->
                    ok;
                {ok, []} ->
                    diagnose(OrgId, WorkspaceId, ConversationId);
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    end.

diagnose(OrgId, WorkspaceId, ConversationId) ->
    case fetch_consent_evidence_kind(OrgId, WorkspaceId, ConversationId) of
        {ok, #{consent_evidence_kind := Kind}} ->
            %% 归一化后的取值：'synthetic' → 原子 synthetic；NULL → undefined。
            case Kind of
                synthetic -> ok;
                null -> {error, no_consent};
                undefined -> {error, no_consent};
                _Other -> {error, conflict}
            end;
        {error, _} = Err ->
            Err
    end.

%% @doc 读取该会话的同意证据类别（`NULL` → `undefined`；无 consent 时必须是 NULL）。
-spec fetch_consent_evidence_kind(integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
fetch_consent_evidence_kind(OrgId, WorkspaceId, ConversationId) ->
    eb_pg_exec:with_tenant(OrgId, WorkspaceId, fun() ->
        eb_pg_exec:fetch_one(
            ?SQL_FETCH_KIND,
            [OrgId, WorkspaceId, ConversationId],
            [
                {consent_evidence_kind, <<"consent_evidence_kind">>, atom},
                {consent_at, <<"consent_at">>, ts}
            ]
        )
    end).
