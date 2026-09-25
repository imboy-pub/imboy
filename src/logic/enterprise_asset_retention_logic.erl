-module(enterprise_asset_retention_logic).

%%%
% enterprise_asset_retention_logic 是企业附件的**内容策略 + 留存/hold/purge**
% use case（FULL-02 / plan-full §3.1「企业附件 presign/confirm/content policy、
% message 原子绑定、retention/hold/purge 不变量」）。
%
%% 两条边界：
%   1. **内容策略**（content_policy_tx/3）：Application 上可配 allowed_mime_types
%      （空表 = 沿用全局 elib_oss 白名单）与 max_file_size_bytes（NULL = 沿用
%      全局上限）。presign 用声明值预检，confirm 用 HEAD 真实值复核——两处都不
%      放宽全局：有效上限恒为 min(全局, 应用值)，有效类型集恒为
%      应用值 ∩ 全局白名单。
%   2. **留存/hold/purge**（governance_tx/3）：附件的治理行（retention_until /
%      hold_state / purge_state）由 confirm 同事务建立；OA 可延长留存、设置/释放
%      法务 hold、在 hold 释放且留存到期后 purge。**不变量在 DB 层声明式强制**
%      （migration 00000140 触发器）：hold 生效中禁止 purge、留存未到期禁止 purge、
%      purged 终态不可回退、留存只可延长、治理行禁删。本模块只做调用与错误归一。
%
%% 拒绝码（stable 13 码内）：
%   * invalid_request  —— 参数非法 / 对象未 confirm / 留存缩短 / hold 未生效时
%                          释放 / purge 被 hold 或留存窗口挡住 / 已 purged；
%                          detail 明确区分（purge_blocked_by_hold /
%                          retention_not_elapsed / already_purged / …），
%                          不新增码（manifest stable_error_codes 冻结）。
%   * resource_not_found —— 对象不属于本 (org, app)（跨租户不泄露存在性）。
%   * internal_error    —— 存储侧删除失败等（事务回滚，不落半成品状态）。
%%%

-export([
    content_policy_tx/3,
    effective_max_bytes/2,
    mime_allowed/2,
    register_retention_tx/2,
    governance_tx/3
]).

-include("log.hrl").

%% 留存窗口上限（CA 10 年）：超过即拒，避免「配成永久」绕过治理复核。
-define(MAX_RETENTION_DAYS, 3650).
-define(MAX_HOLD_REASON_LEN, 200).

%% ===================================================================
%% 内容策略
%% ===================================================================

%% @doc 读本 (org, app) 的内容策略；应用行缺失 → fail-closed（internal_error）。
%% 返回 {ok, #{allowed_mime_types, max_file_size_bytes}}。
-spec content_policy_tx(any(), map(), map()) ->
    {ok, map()} | {error, {binary(), term()}}.
content_policy_tx(Conn, #{organization_id := OrgId, application_id := AppId}, _Input) ->
    case enterprise_application_repo:policy_tx(Conn, OrgId, AppId) of
        {ok, Policy} ->
            {ok, Policy};
        {error, not_found} ->
            {error, {<<"internal_error">>, application_missing}};
        {error, Reason} ->
            {error, {<<"internal_error">>, Reason}}
    end.

%% @doc 有效单文件上限 = min(全局 elib_oss:max_file_size(), 应用配置)。
%% 应用策略只可**收紧**全局上限，不可放大（plan-full §7 安全硬门：不放宽）。
-spec effective_max_bytes(map(), undefined | integer()) -> pos_integer().
effective_max_bytes(Policy, _SizeHint) ->
    Global = elib_oss:max_file_size(),
    AppMax =
        case maps:get(max_file_size_bytes, Policy, undefined) of
            N when is_integer(N), N > 0 -> N;
            _ -> Global
        end,
    min(Global, AppMax).

%% @doc 声明类型是否被策略允许：应用 allowlist 非空时必须命中（逐字精确匹配，
%% 无通配），且必须仍在全局白名单内（策略只可收紧，不可放大）。
-spec mime_allowed(map(), binary()) -> boolean().
mime_allowed(Policy, Mime) when is_binary(Mime) ->
    case elib_oss:validate_file_type(Mime) of
        false ->
            false;
        true ->
            case maps:get(allowed_mime_types, Policy, []) of
                [] -> true;
                Allowed -> lists:member(Mime, Allowed)
            end
    end;
mime_allowed(_Policy, _Mime) ->
    false.

%% ===================================================================
%% 留存登记（confirm 同事务）
%% ===================================================================

%% @doc confirm 转正时登记留存（同事务原子：附件转正失败则治理行一并回滚）。
%% 已存在治理行时只延长、不缩短（触发器兜底）。
-spec register_retention_tx(any(), {integer(), integer(), integer()}) ->
    ok | {error, {binary(), term()}}.
register_retention_tx(Conn, {AttachmentId, OrgId, AppId}) ->
    case enterprise_attachment_retention_repo:upsert_tx(Conn, AttachmentId, OrgId, AppId) of
        {ok, _Row} ->
            ok;
        {error, Reason} ->
            {error, {<<"internal_error">>, {retention_register, Reason}}}
    end.

%% ===================================================================
%% 治理操作（hold / release / extend / purge）
%% ===================================================================

%% @doc 附件治理操作入口。
%% Input（atom 键）：
%%   op            必填 set_retention | hold | release_hold | purge
%%   object_key    必填 binary（必须是本 (org, app) 已 confirm 的附件）
%%   retention_days  op=set_retention 必填 1..3650（**延长**留存）
%%   reason         op=hold 必填 1..200（法务 hold 事由）
%% 返回 {ok, #{op, file_id, object_key, retention_until, hold_state, purge_state,
%% purged}}；失败 {error, {Code, Detail}}（码表见 moduledoc）。
-spec governance_tx(any(), map(), map()) -> {ok, map()} | {error, {binary(), term()}}.
governance_tx(Conn, Ctx, Input) when is_map(Input) ->
    Op = maps:get(op, Input, undefined),
    case resolve_attachment(Conn, Ctx, maps:get(object_key, Input, undefined)) of
        {ok, Att} ->
            %% INT-BE-03 冻结政策 INT-22=REQUIRED_AUDIT：治理审计（audit_action
            %% 冻结为 file.governance.op，具体 op 落 detail.op）与治理写同事务
            %% ——审计失败 → error → 调用方整体回滚（purge 不可逆动作尤其要求
            %% 留痕原子）。
            case apply_op(Conn, Ctx, Op, Att, Input) of
                {ok, Ok} ->
                    case audit_mutation(Conn, Ctx, Op, maps:get(<<"id">>, Att, null)) of
                        ok -> {ok, Ok};
                        {error, Reason} -> {error, {<<"internal_error">>, {audit, Reason}}}
                    end;
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    end;
governance_tx(_Conn, _Ctx, _Input) ->
    {error, {<<"invalid_request">>, input_not_map}}.

%% ===================================================================
%% Internal
%% ===================================================================

%% @doc 对象定位：必须是本 (org, app) 已 confirm 的附件（跨 org/app 的 key
%% 一律 resource_not_found，不泄露存在性）。
-spec resolve_attachment(any(), map(), term()) ->
    {ok, map()} | {error, {binary(), term()}}.
resolve_attachment(Conn, Ctx, ObjectKey) when is_binary(ObjectKey), ObjectKey =/= <<>> ->
    OrgId = maps:get(organization_id, Ctx),
    AppId = maps:get(application_id, Ctx),
    case enterprise_asset_repo:find_confirmed_tx(Conn, OrgId, AppId, ObjectKey) of
        {ok, Att} ->
            {ok, Att};
        {error, not_found} ->
            {error, {<<"resource_not_found">>, file_not_confirmed}}
    end;
resolve_attachment(_Conn, _Ctx, _ObjectKey) ->
    {error, {<<"invalid_request">>, object_key_required}}.

%% @doc 治理 op 统一成功出口：INT-BE-03 冻结政策 INT-22=REQUIRED_AUDIT——
%% 治理审计（audit_action 冻结为 file.governance.op，具体 op 落 detail.op）
%% 与治理写在同一事务（审计失败 → error → 调用方整体回滚；purge 不可逆动作
%% 尤其要求留痕原子）。
-spec apply_op(any(), map(), term(), map(), map()) -> {ok, map()} | {error, {binary(), term()}}.
apply_op(Conn, Ctx, <<"set_retention">>, Att, Input) ->
    with_row(Conn, Ctx, Att, fun(AttId) -> do_set_retention(Conn, AttId, Att, Input) end);
apply_op(Conn, Ctx, <<"hold">>, Att, Input) ->
    with_row(Conn, Ctx, Att, fun(AttId) -> do_hold(Conn, AttId, Att, Input) end);
apply_op(Conn, Ctx, <<"release_hold">>, Att, _Input) ->
    with_row(Conn, Ctx, Att, fun(AttId) -> do_release_hold(Conn, AttId, Att) end);
apply_op(Conn, Ctx, <<"purge">>, Att, _Input) ->
    with_row(Conn, Ctx, Att, fun(AttId) -> do_purge(Conn, Ctx, AttId, Att) end);
apply_op(_Conn, _Ctx, _Op, _Att, _Input) ->
    {error, {<<"invalid_request">>, invalid_op}}.

%% @doc 保证治理行存在（confirm 之后理应存在；历史遗留/并发补写走本 upsert，
%% 幂等且只延长留存）。
-spec with_row(any(), map(), map(), fun((pos_integer()) -> {ok, map()} | {error, term()})) ->
    {ok, map()} | {error, {binary(), term()}}.
with_row(Conn, Ctx, Att, Fun) ->
    AttId = maps:get(<<"id">>, Att),
    OrgId = maps:get(organization_id, Ctx),
    AppId = maps:get(application_id, Ctx),
    case enterprise_attachment_retention_repo:find_tx(Conn, AttId) of
        {ok, _} ->
            Fun(AttId);
        {error, not_found} ->
            case enterprise_attachment_retention_repo:upsert_tx(Conn, AttId, OrgId, AppId) of
                {ok, _} -> Fun(AttId);
                {error, Reason} -> {error, {<<"internal_error">>, {retention_register, Reason}}}
            end;
        {error, Reason} ->
            {error, {<<"internal_error">>, Reason}}
    end.

-spec do_set_retention(any(), pos_integer(), map(), map()) ->
    {ok, map()} | {error, {binary(), term()}}.
do_set_retention(Conn, AttId, Att, Input) ->
    case maps:get(retention_days, Input, undefined) of
        Days when is_integer(Days), Days >= 1, Days =< ?MAX_RETENTION_DAYS ->
            Until = elib_dt:add(elib_dt:now(), {Days * 86400, second}),
            case enterprise_attachment_retention_repo:extend_tx(Conn, AttId, Until) of
                {ok, NewUntil} ->
                    row_reply(Conn, AttId, Att, #{<<"retention_days">> => Days}, NewUntil);
                {error, retention_shrink_rejected} ->
                    {error, {<<"invalid_request">>, retention_not_extended}};
                {error, not_found} ->
                    {error, {<<"resource_not_found">>, file_not_confirmed}};
                {error, Reason} ->
                    {error, {<<"internal_error">>, Reason}}
            end;
        _ ->
            {error, {<<"invalid_request">>, invalid_retention_days}}
    end.

-spec do_hold(any(), pos_integer(), map(), map()) -> {ok, map()} | {error, {binary(), term()}}.
do_hold(Conn, AttId, Att, Input) ->
    case maps:get(reason, Input, undefined) of
        Reason when
            is_binary(Reason), byte_size(Reason) > 0, byte_size(Reason) =< ?MAX_HOLD_REASON_LEN
        ->
            case enterprise_attachment_retention_repo:hold_tx(Conn, AttId, Reason) of
                ok ->
                    row_reply(Conn, AttId, Att, #{<<"held">> => true}, undefined);
                {error, not_found} ->
                    %% 0 行：已 purged（终态）或不存在 → 状态错误，非参数错误
                    {error, {<<"invalid_request">>, purge_terminal_or_missing}};
                {error, Reason2} ->
                    {error, {<<"internal_error">>, Reason2}}
            end;
        _ ->
            {error, {<<"invalid_request">>, hold_reason_required}}
    end.

-spec do_release_hold(any(), pos_integer(), map()) -> {ok, map()} | {error, {binary(), term()}}.
do_release_hold(Conn, AttId, Att) ->
    case enterprise_attachment_retention_repo:release_tx(Conn, AttId) of
        ok ->
            row_reply(Conn, AttId, Att, #{<<"held">> => false}, undefined);
        {error, not_found} ->
            {error, {<<"invalid_request">>, hold_not_active}};
        {error, Reason} ->
            {error, {<<"internal_error">>, Reason}}
    end.

%% @doc purge：DB 守卫先行（hold/留存/终态在 DB 触发器判定），随后删存储对象。
%% 顺序刻意如此：对象删除失败会让整个事务回滚，绝不出现「对象已删但治理行未
%% 记录」或「治理行 purged 但对象仍在」的半成品状态。
-spec do_purge(any(), map(), pos_integer(), map()) -> {ok, map()} | {error, {binary(), term()}}.
do_purge(Conn, Ctx, AttId, Att) ->
    case enterprise_attachment_retention_repo:is_purgeable_tx(Conn, AttId) of
        {ok, false} ->
            {error, {<<"invalid_request">>, purge_precondition_failed}};
        {ok, true} ->
            case enterprise_attachment_retention_repo:purge_tx(Conn, AttId) of
                {ok, PurgedAt} ->
                    case delete_object(maps:get(<<"path">>, Att)) of
                        ok ->
                            _ = enterprise_application_usage_repo:bump_tx(
                                Conn,
                                maps:get(organization_id, Ctx),
                                maps:get(application_id, Ctx),
                                <<"file.confirmed">>
                            ),
                            Base = base_reply(AttId, Att, PurgedAt),
                            {ok, Base#{<<"purged">> => true, <<"held">> => false}};
                        {error, Reason} ->
                            {error, {<<"internal_error">>, {object_delete_failed, Reason}}}
                    end;
                {error, already_purged} ->
                    {error, {<<"invalid_request">>, already_purged}};
                {error, purge_blocked} ->
                    {error, {<<"invalid_request">>, purge_precondition_failed}};
                {error, not_found} ->
                    {error, {<<"resource_not_found">>, file_not_confirmed}};
                {error, Reason} ->
                    {error, {<<"internal_error">>, Reason}}
            end;
        {error, Reason} ->
            {error, {<<"internal_error">>, Reason}}
    end.

-spec delete_object(binary()) -> ok | {error, term()}.
delete_object(ObjectKey) ->
    Bucket = elib_oss:get_bucket(<<"enterprise">>),
    try elib_oss:delete_object(Bucket, ObjectKey) of
        ok -> ok;
        {error, not_found} -> ok;
        {error, Reason} -> {error, Reason};
        _Other -> ok
    catch
        Class:Reason ->
            ?ERROR_LOG([
                enterprise_asset_retention_object_delete_failed,
                #{class => Class, reason => Reason}
            ]),
            {error, {Class, Reason}}
    end.

%% @doc 治理操作回执：把治理行当前状态一并回读（单一形状，便于调用方/审计）。
-spec row_reply(any(), pos_integer(), map(), map(), undefined | binary()) ->
    {ok, map()} | {error, {binary(), term()}}.
row_reply(Conn, AttId, Att, Extra, NewUntil) ->
    case enterprise_attachment_retention_repo:find_tx(Conn, AttId) of
        {ok, Row} ->
            Held = maps:get(<<"hold_state">>, Row) =:= <<"held">>,
            {ok,
                maps:merge(
                    Extra,
                    (base_reply(AttId, Att, undefined))#{
                        <<"retention_until">> => maps:get(<<"retention_until">>, Row),
                        <<"hold_state">> => maps:get(<<"hold_state">>, Row),
                        <<"purge_state">> => maps:get(<<"purge_state">>, Row),
                        <<"extended_until">> => NewUntil,
                        <<"held">> => Held
                    }
                )};
        {error, Reason} ->
            {error, {<<"internal_error">>, Reason}}
    end.

-spec base_reply(pos_integer(), map(), undefined | binary()) -> map().
base_reply(AttId, Att, PurgedAt) ->
    #{
        <<"file_id">> => AttId,
        <<"object_key">> => maps:get(<<"path">>, Att),
        <<"purged">> => false,
        <<"purged_at">> => PurgedAt
    }.

%% ===================================================================
%% INT-BE-03 冻结政策审计接线（REQUIRED_AUDIT 条目统一入口）
%% ===================================================================

%% @doc 附件治理审计（file.governance.op；detail.op = set_retention | hold |
%% release_hold | purge）：调用 enterprise_audit_event_repo:append_tx/3
%% （append-only 真源的唯一 repo 入口）在调用方事务内落审计行；actor_role
%% 恒为 enterprise_application；detail 只放结构化摘要
%% （application/correlation/object_key），无 secret / 无正文。
-spec audit_mutation(any(), map(), binary(), term()) -> ok | {error, term()}.
audit_mutation(Conn, Ctx, Op, ResourceId) ->
    Detail = #{
        <<"op">> => op_name(Op),
        <<"origin_application_id">> => maps:get(application_id, Ctx, null),
        <<"correlation_id">> => maps:get(correlation_id, Ctx, null)
    },
    case
        enterprise_audit_event_repo:append_tx(Conn, maps:get(organization_id, Ctx), #{
            resource_type => <<"attachment">>,
            resource_id => ResourceId,
            action => <<"file.governance.op">>,
            actor_user_id => maps:get(principal_user_id, Ctx, undefined),
            actor_role => <<"enterprise_application">>,
            detail => Detail
        })
    of
        {ok, _AuditId} -> ok;
        {error, Reason} -> {error, Reason}
    end.

-spec op_name(term()) -> binary().
op_name(Op) when is_binary(Op) ->
    Op;
op_name(_Other) ->
    <<"unknown">>.
