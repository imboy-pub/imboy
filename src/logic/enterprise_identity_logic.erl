-module(enterprise_identity_logic).

-moduledoc "企业身份映射 use case（EPGZ-03）—— INT-02 绑定 / INT-03 批量 resolve。".
%%%
% enterprise_identity_logic 是 EPGZ-03 身份映射 use case（INT-02 绑定 /
% INT-03 批量 resolve）的业务逻辑层。
%
% 契约（handler 壳由 A0 W4 接线，见 checkpoint handoff）：
%   * 输入 Ctx 为 A2 认证产物（至少 organization_id / application_id）；
%     scope 校验在中间件（decide）完成，本层不重复。
%   * 错误统一 {error, {ErrorCode, Detail}}，ErrorCode ∈ stable 13 码：
%       - invalid_request                    400 参数非法 / user 已被本
%                                           app 其他 external_user_id 映射
%       - organization_boundary_violation    403 目标不是本 Org 成员
%       - identity_not_mapped                422 目标是本 Org 成员但不可映射
%                                           （非 active / 非 Human / 账号停用）
%   * INT-03 只按入参 external_user_id 集合返回命中项（active 行），
%     本模块不提供任何 list/全量导出形态函数（无全量导出能力）。
%   * FULL-02 扩展：revoke_mapping_tx/3（INT-02 撤销面，软删 + 聚合计量）；
%     **cursor directory 不在本模块**——受限分页目录独占
%     enterprise_directory_logic（page_mappings_tx/3 / page_users_tx/3，每页
%     有硬上限、无 OFFSET、无无 LIMIT 读）；本模块导出面因此仍无任何
%     list/all/export 形态（真库套件钉死）。
%%%

-export([
    bind_mapping_tx/4,
    resolve_mappings_tx/3,
    revoke_mapping_tx/3
]).

-define(MAX_EXTERNAL_ID_LEN, 256).
-define(MAX_RESOLVE_BATCH, 100).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc INT-02：绑定 external_user_id <-> active Human member。
%% 幂等 upsert 语义（A1 bind_tx）：同 (org, app, external) 重绑覆盖
%% user_id 并复活为 active；该 user 已绑其他 external → invalid_request。
%% 前置甄别把 DB 触发器 23514 细分为 boundary / not-mapped 两类 stable 码。
-spec bind_mapping_tx(any(), map(), binary(), integer()) ->
    {ok, map()} | {error, {binary(), term()}}.
bind_mapping_tx(Conn, Ctx, ExternalUserId, UserId) ->
    %% 参数校验先行：畸形入参不触碰 Ctx/DB（缺 org/app 的 ctx 也能安全拒绝）
    case validate_external_id(ExternalUserId) of
        ok ->
            case validate_user_id(UserId) of
                ok ->
                    OrgId = org_id(Ctx),
                    case classify_target(Conn, OrgId, UserId) of
                        ok ->
                            do_bind(Conn, Ctx, ExternalUserId, UserId);
                        {error, {Code, Detail}} ->
                            {error, {Code, Detail}}
                    end;
                {error, Detail} ->
                    {error, {<<"invalid_request">>, Detail}}
            end;
        {error, Detail} ->
            {error, {<<"invalid_request">>, Detail}}
    end.

%% @doc INT-03：按 external_user_id 列表批量解析 active 映射。
%% 只返回命中项（[{external_user_id, user_id}]）；未映射项不报错、不返回
%% （调用方以缺席判定）。空列表 / 超限量 / 元素非法 → invalid_request。
-spec resolve_mappings_tx(any(), map(), [binary()]) ->
    {ok, [map()]} | {error, {binary(), term()}}.
resolve_mappings_tx(Conn, Ctx, ExternalUserIds) when is_list(ExternalUserIds) ->
    OrgId = org_id(Ctx),
    AppId = app_id(Ctx),
    case validate_resolve_batch(ExternalUserIds) of
        ok ->
            case
                enterprise_external_identity_repo:resolve_tx(Conn, OrgId, AppId, ExternalUserIds)
            of
                {ok, Rows} ->
                    {ok, rows_to_pairs(Rows)};
                {error, Reason} ->
                    {error, {<<"internal_error">>, Reason}}
            end;
        {error, Detail} ->
            {error, {<<"invalid_request">>, Detail}}
    end;
resolve_mappings_tx(_Conn, _Ctx, _Other) ->
    {error, {<<"invalid_request">>, external_ids_not_list}}.

%% @doc FULL-02 INT-02 撤销面：撤销（软删）本 (org, app) 下某 external_user_id 的
%% 映射（repo unbind_tx：status -> 'removed'，行保留供审计/重绑）。撤销后该
%% external_user_id 立即不再是 sender/成员解析来源（resolve 只认 active 行），
%% 重绑走 bind_mapping_tx/4 的 upsert。
%% 语义与拒绝码：
%%   * 撤销成功 → {ok, #{external_user_id, status => <<"removed">>, revoked => true}}，
%%     记 identity.revoked 聚合计量（同事务）；
%%   * 目标无 active 映射（不存在 / 已被撤销）→ resource_not_found
%%     （不区分「不存在」与「已撤销」，不给存在性 oracle）；
%%   * external_user_id 形态非法 → invalid_request。
%% 幂等：INV-7 由调用方（handler）的 Idempotency-Key 保证——同 key 同 body 重放
%% 返回首次结果；换 key 重复撤销才会落 resource_not_found（事实口径）。
-spec revoke_mapping_tx(any(), map(), binary()) ->
    {ok, map()} | {error, {binary(), term()}}.
revoke_mapping_tx(Conn, Ctx, ExternalUserId) ->
    case validate_external_id(ExternalUserId) of
        ok ->
            OrgId = org_id(Ctx),
            AppId = app_id(Ctx),
            case
                enterprise_external_identity_repo:unbind_tx(
                    Conn, OrgId, AppId, ExternalUserId
                )
            of
                ok ->
                    case
                        enterprise_application_usage_repo:bump_tx(
                            Conn, OrgId, AppId, <<"identity.revoked">>
                        )
                    of
                        ok ->
                            %% INT-BE-03 冻结政策 INT-15=REQUIRED_AUDIT：撤销
                            %% 审计与业务写同事务（resource_id 未知 → null，
                            %% external_user_id 落 detail）。
                            case
                                audit_mutation(
                                    Conn,
                                    Ctx,
                                    <<"identity.mapping.revoked">>,
                                    null,
                                    #{<<"external_user_id">> => ExternalUserId}
                                )
                            of
                                ok ->
                                    {ok, #{
                                        <<"external_user_id">> => ExternalUserId,
                                        <<"status">> => <<"removed">>,
                                        <<"revoked">> => true
                                    }};
                                {error, Reason} ->
                                    {error, {<<"internal_error">>, {audit, Reason}}}
                            end;
                        {error, Reason} ->
                            {error, {<<"internal_error">>, Reason}}
                    end;
                {error, not_found} ->
                    {error, {<<"resource_not_found">>, mapping_not_found}};
                {error, not_active} ->
                    {error, {<<"resource_not_found">>, mapping_not_active}};
                {error, Reason} ->
                    {error, {<<"internal_error">>, Reason}}
            end;
        {error, Detail} ->
            {error, {<<"invalid_request">>, Detail}}
    end.

%% ===================================================================
%% Internal Functions
%% ===================================================================

-spec org_id(map()) -> integer().
org_id(Ctx) ->
    maps:get(organization_id, Ctx).

-spec app_id(map()) -> integer().
app_id(Ctx) ->
    maps:get(application_id, Ctx).

-spec validate_external_id(binary()) -> ok | {error, term()}.
validate_external_id(E) when
    is_binary(E), byte_size(E) > 0, byte_size(E) =< ?MAX_EXTERNAL_ID_LEN
->
    ok;
validate_external_id(_) ->
    {error, invalid_external_id}.

-spec validate_user_id(integer()) -> ok | {error, term()}.
validate_user_id(U) when is_integer(U), U > 0, U =< 9223372036854775807 ->
    ok;
validate_user_id(_) ->
    {error, invalid_user_id}.

-spec validate_resolve_batch([binary()]) -> ok | {error, term()}.
validate_resolve_batch(Ids) ->
    case length(Ids) of
        0 ->
            {error, empty_external_ids};
        N when N > ?MAX_RESOLVE_BATCH ->
            {error, batch_too_large};
        _ ->
            validate_each_external(Ids)
    end.

-spec validate_each_external([binary()]) -> ok | {error, term()}.
validate_each_external([]) ->
    ok;
validate_each_external([E | Rest]) ->
    case validate_external_id(E) of
        ok ->
            validate_each_external(Rest);
        {error, _} = Err ->
            Err
    end.

%% @doc 目标甄别：非本 Org 成员 → boundary；成员但非 active Human/账号停用 →
%% not_mapped（与 trg_..._member_guard 判定字段同源：om.status='active'、
%% account_type=0、user.status=1）。
-spec classify_target(any(), integer(), integer()) -> ok | {error, {binary(), term()}}.
classify_target(Conn, OrgId, UserId) ->
    case enterprise_org_member_repo:find_membership_with_user_tx(Conn, OrgId, UserId) of
        {ok, #{
            <<"account_type">> := 0, <<"member_status">> := <<"active">>, <<"user_status">> := 1
        }} ->
            ok;
        {ok, Row} ->
            {error, {<<"identity_not_mapped">>, Row}};
        {error, not_found} ->
            {error, {<<"organization_boundary_violation">>, not_org_member}};
        {error, Reason} ->
            {error, {<<"internal_error">>, Reason}}
    end.

-spec do_bind(any(), map(), binary(), integer()) ->
    {ok, map()} | {error, {binary(), term()}}.
do_bind(Conn, Ctx, ExternalUserId, UserId) ->
    OrgId = org_id(Ctx),
    AppId = app_id(Ctx),
    case enterprise_external_identity_repo:bind_tx(Conn, OrgId, AppId, ExternalUserId, UserId) of
        {ok, Row} ->
            %% 聚合计量（只计数，不含 external_user_id/正文/PII，plan-full §5）。
            case
                enterprise_application_usage_repo:bump_tx(Conn, OrgId, AppId, <<"identity.bound">>)
            of
                ok ->
                    %% INT-BE-03 冻结政策 INT-02=REQUIRED_AUDIT：绑定审计与业务
                    %% 写同事务（审计失败 → 本返回 error → 调用方整体回滚）。
                    case
                        audit_mutation(
                            Conn,
                            Ctx,
                            <<"identity.mapping.bound">>,
                            maps:get(<<"id">>, Row, null),
                            #{<<"external_user_id">> => ExternalUserId}
                        )
                    of
                        ok ->
                            {ok, mapping_view(Row)};
                        {error, Reason} ->
                            {error, {<<"internal_error">>, {audit, Reason}}}
                    end;
                {error, Reason} ->
                    {error, {<<"internal_error">>, Reason}}
            end;
        {error, invalid_member} ->
            %% 前置甄别与触发器判定间的竞态（成员状态并发变化）：终局守卫口径
            {error, {<<"identity_not_mapped">>, member_guard_rejected}};
        {error, user_already_mapped} ->
            {error, {<<"invalid_request">>, user_already_mapped}};
        {error, Reason} ->
            {error, {<<"internal_error">>, Reason}}
    end.

%% @doc INT-BE-03 冻结政策 REQUIRED_AUDIT 的统一审计接线（本模块私有）：
%% 调用 enterprise_audit_event_repo:append_tx/3（append-only 真源的唯一 repo
%% 入口）在调用方事务内落审计行；actor_role 恒为 enterprise_application
%% （OA internal 面），detail 只放结构化摘要（application/correlation/业务键），
%% 无 secret / 无正文 / 无 Authorization。
-spec audit_mutation(any(), map(), binary(), term(), map()) -> ok | {error, term()}.
audit_mutation(Conn, Ctx, Action, ResourceId, ExtraDetail) ->
    Detail = maps:merge(ExtraDetail, #{
        <<"origin_application_id">> => maps:get(application_id, Ctx, null),
        <<"correlation_id">> => maps:get(correlation_id, Ctx, null)
    }),
    case
        enterprise_audit_event_repo:append_tx(Conn, org_id(Ctx), #{
            resource_type => <<"enterprise_external_identity">>,
            resource_id => ResourceId,
            action => Action,
            actor_user_id => maps:get(principal_user_id, Ctx, undefined),
            actor_role => <<"enterprise_application">>,
            detail => Detail
        })
    of
        {ok, _AuditId} -> ok;
        {error, Reason} -> {error, Reason}
    end.

-spec mapping_view(map()) -> map().
mapping_view(Row) ->
    #{
        <<"external_user_id">> => maps:get(<<"external_user_id">>, Row),
        <<"user_id">> => maps:get(<<"user_id">>, Row),
        <<"status">> => maps:get(<<"status">>, Row)
    }.

-spec rows_to_pairs([map()]) -> [map()].
rows_to_pairs(Rows) ->
    [
        #{
            <<"external_user_id">> => maps:get(<<"external_user_id">>, Row),
            <<"user_id">> => maps:get(<<"user_id">>, Row)
        }
     || Row <- Rows
    ].
