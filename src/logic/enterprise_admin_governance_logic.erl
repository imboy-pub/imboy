-module(enterprise_admin_governance_logic).

%%%
% enterprise_admin_governance_logic 是 **Admin（Human/Admin 会话）** 企业应用
% 治理面的业务层（FULL-08 / plan-full §3.1、§3.2、§7）。
%
% 它服务的端点族是 `/api/adm/enterprise/organizations/:org_id/applications/**`
% （A-01..A-14），契约冻结在 imboyadmin 分支 run/full-candidate-admin-20260921T101806Z
% 的 src/modules/enterprise_apps/api/contracts.ts（ENDPOINTS 逐字字符串被单测钉死）。
%
% 硬边界（不可协商）：
%   1. 本模块**只**服务 Admin Cookie 会话。它不接收、不识别、不签发 Application
%      Credential；OA/Application 凭据面是 /api/internal/v1/* 的独立鉴权链
%      （enterprise_internal_auth）。两者鉴权不可互换（plan 产品硬边界 §2）。
%   2. 读面**永不**投影 secret / digest / payload。credential 只给元数据；
%      投递只给元数据（payload 列即使存在于 bot_delivery 也绝不下发——那是
%      业务正文，Admin 无权看）。前端对这两族键有熔断守卫，后端是第二道闸。
%   3. 所有 ID 走 TSID。64-bit ID 在 JSON 里以字符串下发（防 JS 精度丢失）；
%      bot_delivery.delivery_id 是 text，原样下发。
%   4. 写面一律 CAS（expected_version）＋ 审计留痕。审计落在 append-only 的
%      enterprise_audit_event（迁移 00000119），执行者记 **平台管理员**
%      （actor_role = platform_admin，admin 账号记在 detail.actor_account）——
%      平台管理员不是租户 user，不能伪造成 user 归因。
%
% 依赖方向：handler(adm) → logic(本模块) → ops/repo。本模块不碰 HTTP、
% 不碰鉴权判定（那是 handler + adm_acl 的事）。
%%%

-export([
    list_applications/4,
    application_detail/2,
    list_credentials/2,
    list_grants/2,
    delivery_stats/2,
    list_deliveries/5,
    list_audit/4,
    set_status/5,
    set_scopes/5,
    issue_credential/4,
    rotate_credential/4,
    revoke_credential/4,
    issue_grant/5,
    patch_grant/6
]).

%% 审计资源类型：Admin 治理面统一按 application 维度留痕。
-define(AUDIT_RESOURCE_TYPE, <<"enterprise_application">>).
-define(ACTOR_ROLE, <<"platform_admin">>).

%% Admin 未指定授权有效期时的默认窗口（天）。
%% ⚠ 这是**显式决策**，不是静默缺省：enterprise_application_grant.expires_at 是
%% NOT NULL（授权天然有界，不允许签发无限期授权），而冻结的 Admin 契约里
%% valid_from/valid_to 是可选字段、且 FULL-04 的 UI 根本没有有效期输入。
%% ⇒ 后端必须给一个默认值。取 365 天并在签发响应与审计里回显，
%% 让「实际有效期」对运维可见（不是悄悄挂在暗处）。
-define(DEFAULT_GRANT_VALID_DAYS, 365).

%% ===================================================================
%% 读面
%% ===================================================================

%% @doc A-01 按组织分页浏览 Application。
%% Opts：#{status => binary(), q => binary()}（均可缺省）。
%% 返回 {ok, #{items, total, page, size}}——items 每行是冻结键集的投影。
-spec list_applications(integer(), pos_integer(), pos_integer(), map()) ->
    {ok, map()} | {error, invalid_status | term()}.
list_applications(OrgId, Page, Size, Opts) ->
    case
        tx(fun(Conn) ->
            case enterprise_application_repo:list_page_tx(Conn, OrgId, Page, Size, Opts) of
                {ok, #{items := Items, total := Total}} ->
                    {ok, #{
                        <<"items">> => [application_summary(I) || I <- Items],
                        <<"total">> => Total,
                        <<"page">> => Page,
                        <<"size">> => Size
                    }};
                {error, Reason} ->
                    throw({rollback, Reason})
            end
        end)
    of
        {ok, _} = Ok -> Ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc A-02 Application 详情（顶层即 application；与前端
%% toApplicationDetailFromPayload 的两种形状都兼容）。
%% description / owner_application_key 是详情扩展键：
%%   * description —— 本阶段**没有**该列，恒为空串（不编造描述）；
%%   * owner_application_key —— 取 application_key（该应用的对外标识）。
-spec application_detail(integer(), integer()) -> {ok, map()} | {error, not_found | term()}.
application_detail(OrgId, AppId) ->
    case
        tx(fun(Conn) ->
            case enterprise_application_repo:find_tx(Conn, OrgId, AppId) of
                {ok, Row} ->
                    {ok, (application_summary(Row))#{
                        <<"description">> => <<>>,
                        <<"owner_application_key">> => maps:get(<<"application_key">>, Row, <<>>)
                    }};
                {error, Reason} ->
                    throw({rollback, Reason})
            end
        end)
    of
        {ok, _} = Ok -> Ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc A-05 credential **元数据**列表（无 secret / 无 digest）。
-spec list_credentials(integer(), integer()) -> {ok, [map()]}.
list_credentials(OrgId, AppId) ->
    {ok, [credential_meta(R) || R <- enterprise_internal_ops:list_credentials(OrgId, AppId)]}.

%% @doc A-09 Grant 列表（含 scopes / workspace_ids / version）。
-spec list_grants(integer(), integer()) -> {ok, [map()]} | {error, term()}.
list_grants(OrgId, AppId) ->
    tx(fun(Conn) ->
        case enterprise_application_grant_repo:list_tx(Conn, OrgId, AppId) of
            {ok, Rows} -> {ok, [grant_view(R) || R <- Rows]};
            {error, Reason} -> throw({rollback, Reason})
        end
    end).

%% @doc A-12 投递健康度（聚合数字；**无 payload**）。
-spec delivery_stats(integer(), integer()) -> {ok, map()} | {error, term()}.
delivery_stats(OrgId, AppId) ->
    Ctx = #{organization_id => OrgId, application_id => AppId},
    case
        tx(fun(Conn) ->
            case enterprise_webhook_logic:delivery_stats_tx(Conn, Ctx) of
                {ok, Stats} -> {ok, delivery_stats_view(Stats)};
                {error, {_Code, Reason}} -> throw({rollback, Reason});
                {error, Reason} -> throw({rollback, Reason})
            end
        end)
    of
        {ok, _} = Ok -> Ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc A-13 投递**元数据**列表（无 payload / 无 secret）。
-spec list_deliveries(integer(), integer(), pos_integer(), pos_integer(), undefined | binary()) ->
    {ok, [map()]} | {error, term()}.
list_deliveries(OrgId, AppId, Page, Size, Status) ->
    tx(fun(Conn) ->
        case
            enterprise_webhook_repo:list_deliveries_admin_tx(Conn, OrgId, AppId, Status, Page, Size)
        of
            {ok, Rows} -> {ok, [delivery_row(R) || R <- Rows]};
            {error, Reason} -> throw({rollback, Reason})
        end
    end).

%% @doc A-14 审计（before/after diff 只含业务字段）。
-spec list_audit(integer(), integer(), pos_integer(), pos_integer()) ->
    {ok, [map()]} | {error, term()}.
list_audit(OrgId, AppId, Page, Size) ->
    Opts = #{page => Page, size => Size},
    tx(fun(Conn) ->
        case
            enterprise_audit_event_repo:list_tx(
                Conn, OrgId, ?AUDIT_RESOURCE_TYPE, AppId, Opts
            )
        of
            {ok, Rows} -> {ok, [audit_entry(R) || R <- Rows]};
            {error, Reason} -> throw({rollback, Reason})
        end
    end).

%% ===================================================================
%% 写面（CAS + 审计）
%% ===================================================================

%% 所有治理写入与审计共用连接；失败不返回凭证明文、不提交局部变更。
set_status(OrgId, AppId, Version, Status, Actor) ->
    audited(OrgId, AppId, Actor, fun(C, App) ->
        must(
            enterprise_internal_ops:set_application_status_cas_tx(
                C, OrgId, AppId, Version, Status
            )
        ),
        After = must(enterprise_application_repo:find_tx(C, OrgId, AppId)),
        {<<"application_status_changed">>, app_diff(App), app_diff(After), ok}
    end).

set_scopes(OrgId, AppId, Version, Scopes, Actor) ->
    audited(OrgId, AppId, Actor, fun(C, App) ->
        must(enterprise_internal_ops:update_scopes_cas_tx(C, OrgId, AppId, Version, Scopes)),
        After = must(enterprise_application_repo:find_tx(C, OrgId, AppId)),
        {<<"application_scopes_changed">>, app_diff(App), app_diff(After), {ok, Scopes}}
    end).

issue_credential(OrgId, AppId, ExpiresAt, Actor) ->
    audited(OrgId, AppId, Actor, fun(C, _App) ->
        Issued = must(enterprise_internal_ops:issue_credential_tx(C, OrgId, AppId, ExpiresAt)),
        credential_result(C, OrgId, AppId, Issued, <<"credential_issued">>, #{})
    end).

rotate_credential(OrgId, AppId, CredId, Actor) ->
    audited(OrgId, AppId, Actor, fun(C, _App) ->
        Old = credential_in(C, OrgId, AppId, CredId),
        Issued = must(enterprise_internal_ops:rotate_credential_tx(C, OrgId, CredId)),
        credential_result(
            C,
            OrgId,
            AppId,
            Issued,
            <<"credential_rotated">>,
            #{credential_prefix => prefix_of(Old)}
        )
    end).

revoke_credential(OrgId, AppId, CredId, Actor) ->
    audited(OrgId, AppId, Actor, fun(C, _App) ->
        Old = credential_in(C, OrgId, AppId, CredId),
        must(enterprise_internal_ops:revoke_credential_tx(C, OrgId, CredId)),
        {<<"credential_revoked">>, #{credential_prefix => prefix_of(Old)},
            #{credential_status => <<"revoked">>}, ok}
    end).

issue_grant(OrgId, AppId, Input, Version, Actor) ->
    audited(OrgId, AppId, Actor, fun(C, App) ->
        case maps:get(<<"version">>, App) of
            Version -> ok;
            _ -> throw({rollback, version_conflict})
        end,
        {ValidFrom, ValidTo} = validity_window(Input),
        Spec = #{
            scopes => maps:get(scopes, Input, []),
            workspace_scope_kind => maps:get(workspace_scope_kind, Input, none),
            workspace_ids => maps:get(workspace_ids, Input, []),
            valid_from => ValidFrom,
            expires_at => ValidTo,
            idempotency_key => maps:get(idempotency_key, Input, grant_idem(Input, ValidTo))
        },
        Grant = must(enterprise_internal_ops:issue_grant_tx(C, OrgId, AppId, Spec)),
        {<<"grant_issued">>, #{}, grant_diff(Grant), {ok, grant_view(Grant)}}
    end).

patch_grant(OrgId, AppId, GrantId, Version, Patch, Actor) ->
    audited(OrgId, AppId, Actor, fun(C, _App) ->
        Before = must(enterprise_application_grant_repo:find_tx(C, OrgId, AppId, GrantId)),
        Action = patch_grant_in(C, OrgId, AppId, GrantId, Version, Patch, Actor),
        After = must(enterprise_application_grant_repo:find_tx(C, OrgId, AppId, GrantId)),
        {Action, grant_diff(Before), grant_diff(After), ok}
    end).

patch_grant_in(C, OrgId, AppId, GrantId, Version, Patch, Actor) ->
    Revoke = maps:get(revoke, Patch, false) =:= true,
    Scopes = maps:is_key(scopes, Patch),
    Workspaces = maps:is_key(workspace_scope_kind, Patch) orelse maps:is_key(workspace_ids, Patch),
    case {Revoke, Scopes, Workspaces} of
        {true, _, _} ->
            must(
                enterprise_application_grant_repo:revoke_admin_tx(
                    C, OrgId, AppId, GrantId, Version, maps:get(adm_user_id, Actor, 0)
                )
            ),
            <<"grant_revoked">>;
        {false, false, false} ->
            throw({rollback, invalid_request});
        {false, _, _} ->
            patch_grant_fields(C, OrgId, AppId, GrantId, Version, Patch, Scopes, Workspaces)
    end.

patch_grant_fields(C, OrgId, AppId, GrantId, Version, Patch, Scopes, Workspaces) ->
    case Scopes of
        true ->
            must(
                enterprise_internal_ops:set_grant_scopes_tx(
                    C, OrgId, AppId, GrantId, Version, maps:get(scopes, Patch)
                )
            );
        false ->
            ok
    end,
    case Workspaces of
        true ->
            NextVersion =
                case Scopes of
                    true -> Version + 1;
                    false -> Version
                end,
            must(
                enterprise_internal_ops:set_grant_workspaces_tx(
                    C,
                    OrgId,
                    AppId,
                    GrantId,
                    NextVersion,
                    maps:get(workspace_scope_kind, Patch, undefined),
                    maps:get(workspace_ids, Patch, undefined)
                )
            ),
            <<"grant_workspaces_changed">>;
        false ->
            <<"grant_scopes_changed">>
    end.

audited(OrgId, AppId, Actor, Fun) ->
    tx(fun(C) ->
        App = must(enterprise_application_repo:lock_tx(C, OrgId, AppId)),
        {Action, Before, After, Reply} = Fun(C, App),
        append_audit(C, OrgId, AppId, Action, Before, After, Actor),
        Reply
    end).

must(ok) -> ok;
must({ok, Value}) -> Value;
must({error, Reason}) -> throw({rollback, Reason}).

credential_in(C, OrgId, AppId, CredId) ->
    Row = must(enterprise_application_credential_repo:find_tx(C, OrgId, CredId)),
    case maps:get(<<"application_id">>, Row) of
        AppId -> credential_meta(Row);
        _ -> throw({rollback, not_found})
    end.

credential_result(C, OrgId, AppId, Issued, Action, Before) ->
    Meta = credential_in(C, OrgId, AppId, maps:get(credential_id, Issued)),
    After = #{credential_status => <<"active">>, credential_prefix => prefix_of(Meta)},
    Reply = {ok, #{<<"credential">> => Meta, <<"secret">> => maps:get(credential, Issued)}},
    {Action, Before, After, Reply}.

%% ===================================================================
%% 审计
%% ===================================================================

append_audit(Conn, OrgId, AppId, Action, Before, After, Actor) ->
    Event = #{
        resource_type => ?AUDIT_RESOURCE_TYPE,
        resource_id => AppId,
        action => Action,
        %% 平台管理员**不是**租户 user：actor_user_id 恒为 null，归因靠
        %% actor_role + detail.actor_account（见本模块头硬边界 §4）。
        actor_user_id => undefined,
        actor_role => ?ACTOR_ROLE,
        detail => #{
            <<"actor_account">> => account_of(Actor),
            <<"actor_kind">> => ?ACTOR_ROLE,
            <<"before">> => stringify_diff(Before),
            <<"after">> => stringify_diff(After)
        }
    },
    case enterprise_audit_event_repo:append_tx(Conn, OrgId, Event) of
        {ok, _AuditId} -> ok;
        {error, Reason} -> throw({rollback, {audit_failed, Reason}})
    end.

-spec account_of(map()) -> binary().
account_of(Actor) ->
    case maps:get(account, Actor, undefined) of
        A when is_binary(A), A =/= <<>> -> A;
        _ -> <<"unknown">>
    end.

%% diff 里的值统一成 JSON 可编码形态（整数 ID → 字符串；atom → binary）。
-spec stringify_diff(map()) -> map().
stringify_diff(Diff) when is_map(Diff) ->
    maps:fold(
        fun(K, V, Acc) ->
            Acc#{atom_to_binary(K, utf8) => stringify_value(V)}
        end,
        #{},
        Diff
    );
stringify_diff(_) ->
    #{}.

stringify_value(V) when is_integer(V) -> integer_to_binary(V);
stringify_value(V) when is_binary(V) -> V;
stringify_value(V) when is_atom(V) -> atom_to_binary(V, utf8);
stringify_value(V) when is_list(V) -> [stringify_value(X) || X <- V];
stringify_value(_) -> null.

%% ===================================================================
%% 投影（后端是第二道闸：只输出冻结键集）
%% ===================================================================

-spec application_summary(map()) -> map().
application_summary(Row) ->
    #{
        <<"id">> => id_bin(maps:get(<<"id">>, Row, 0)),
        <<"organization_id">> => id_bin(maps:get(<<"organization_id">>, Row, 0)),
        <<"name">> => maps:get(<<"name">>, Row, <<>>),
        <<"status">> => maps:get(<<"status">>, Row, <<>>),
        <<"scopes">> => decode_scopes(maps:get(<<"allowed_scopes">>, Row, <<"[]">>)),
        <<"version">> => maps:get(<<"version">>, Row, 1),
        <<"created_at">> => maps:get(<<"created_at">>, Row, null),
        <<"updated_at">> => maps:get(<<"updated_at">>, Row, null)
    }.

-spec credential_meta(map()) -> map().
credential_meta(Row) ->
    #{
        <<"id">> => id_bin(maps:get(<<"id">>, Row, 0)),
        <<"credential_prefix">> => maps:get(<<"credential_prefix">>, Row, <<>>),
        <<"status">> => maps:get(<<"status">>, Row, <<>>),
        <<"created_at">> => maps:get(<<"created_at">>, Row, null),
        <<"expires_at">> => maps:get(<<"expires_at">>, Row, null),
        <<"last_used_at">> => maps:get(<<"last_used_at">>, Row, null),
        <<"revoked_at">> => maps:get(<<"revoked_at">>, Row, null)
    }.

-spec grant_view(map()) -> map().
grant_view(Row) ->
    #{
        <<"id">> => id_bin(maps:get(<<"id">>, Row, 0)),
        <<"workspace_scope_kind">> => maps:get(<<"workspace_scope_kind">>, Row, <<"none">>),
        <<"workspace_ids">> => [
            id_bin(W)
         || W <- normalize_ids(maps:get(<<"workspace_ids">>, Row, []))
        ],
        <<"scopes">> => normalize_binaries(maps:get(<<"scopes">>, Row, [])),
        <<"status">> => maps:get(<<"status">>, Row, <<>>),
        <<"version">> => maps:get(<<"version">>, Row, 1),
        <<"valid_from">> => maps:get(<<"valid_from">>, Row, null),
        <<"valid_to">> => maps:get(<<"expires_at">>, Row, null)
    }.

-spec delivery_row(map()) -> map().
delivery_row(Row) ->
    #{
        <<"id">> => maps:get(<<"delivery_id">>, Row, <<>>),
        <<"event_id">> => maps:get(<<"event_id">>, Row, <<>>),
        <<"event_type">> => maps:get(<<"event_type">>, Row, <<>>),
        <<"status">> => maps:get(<<"status">>, Row, <<>>),
        <<"attempt_count">> => maps:get(<<"attempt_count">>, Row, 0),
        <<"endpoint_generation">> => maps:get(<<"ewh_endpoint_generation">>, Row, 0),
        <<"ledger_version">> => maps:get(<<"ewh_ledger_version">>, Row, 1),
        <<"replay_of">> => replay_of(maps:get(<<"ewh_replay_of">>, Row, null)),
        <<"correlation_id">> => maps:get(<<"correlation_id">>, Row, <<>>),
        <<"next_retry_at">> => maps:get(<<"next_retry_at">>, Row, null),
        <<"created_at">> => maps:get(<<"created_at">>, Row, null),
        <<"updated_at">> => maps:get(<<"updated_at">>, Row, null)
    }.

-spec delivery_stats_view(map()) -> map().
delivery_stats_view(Stats) ->
    Counts = maps:get(<<"status_counts">>, Stats, #{}),
    Success = maps:get(<<"success">>, Counts, 0),
    Dead = maps:get(<<"dead_letter_count">>, Stats, 0),
    Retry = maps:get(<<"retry">>, Counts, 0),
    Pending = maps:get(<<"pending">>, Counts, 0),
    Total = Success + Dead + Retry + Pending,
    Settled = Success + Dead,
    Rate =
        case Settled of
            0 -> 1.0;
            _ -> Success / Settled
        end,
    DeadRate =
        case Settled of
            0 -> 0.0;
            _ -> Dead / Settled
        end,
    #{
        <<"total">> => Total,
        <<"success">> => Success,
        <<"retry">> => Retry,
        <<"dead">> => Dead,
        <<"pending">> => Pending,
        <<"success_rate">> => Rate,
        <<"dead_letter_rate">> => DeadRate
    }.

-spec audit_entry(map()) -> map().
audit_entry(Row) ->
    Detail = decode_map(maps:get(<<"detail">>, Row, #{})),
    #{
        <<"id">> => id_bin(maps:get(<<"id">>, Row, 0)),
        <<"action">> => maps:get(<<"action">>, Row, <<>>),
        <<"actor_account">> => maps:get(<<"actor_account">>, Detail, <<>>),
        <<"target_kind">> => maps:get(<<"resource_type">>, Row, <<>>),
        <<"target_id">> => nullable_id(maps:get(<<"resource_id">>, Row, null)),
        <<"created_at">> => maps:get(<<"created_at">>, Row, null),
        <<"before">> => maps:get(<<"before">>, Detail, #{}),
        <<"after">> => maps:get(<<"after">>, Detail, #{})
    }.

%% ===================================================================
%% 内部：读辅助
%% ===================================================================

app_diff(Row) ->
    #{
        status => maps:get(<<"status">>, Row),
        scopes => decode_scopes(maps:get(<<"allowed_scopes">>, Row)),
        version => maps:get(<<"version">>, Row)
    }.

-spec prefix_of(map()) -> binary().
prefix_of(Meta) ->
    maps:get(<<"credential_prefix">>, Meta, <<>>).

-spec grant_diff(map()) -> map().
grant_diff(Row) ->
    #{
        status => maps:get(<<"status">>, Row, <<>>),
        scopes => normalize_binaries(maps:get(<<"scopes">>, Row, [])),
        workspace_scope_kind => maps:get(<<"workspace_scope_kind">>, Row, <<"none">>),
        workspace_ids => [id_bin(W) || W <- normalize_ids(maps:get(<<"workspace_ids">>, Row, []))],
        valid_from => maps:get(<<"valid_from">>, Row, null),
        valid_to => maps:get(<<"expires_at">>, Row, null)
    }.

%% ===================================================================
%% 内部：值归一化 / 事务
%% ===================================================================

-spec tx(fun()) -> term().
tx(Fun) ->
    case elib_pg:with_tx(Fun) of
        {rollback, Reason} -> {error, Reason};
        Result -> Result
    end.

-spec id_bin(integer() | binary()) -> binary().
id_bin(V) when is_integer(V) -> integer_to_binary(V);
id_bin(V) when is_binary(V) -> V;
id_bin(_) -> <<"0">>.

-spec nullable_id(null | integer()) -> null | binary().
nullable_id(null) -> null;
nullable_id(V) -> id_bin(V).

-spec replay_of(null | binary()) -> null | binary().
replay_of(null) -> null;
replay_of(V) when is_binary(V), V =/= <<>> -> V;
replay_of(_) -> null.

-spec decode_scopes(term()) -> [binary()].
decode_scopes(L) when is_list(L) -> [S || S <- L, is_binary(S)];
decode_scopes(B) when is_binary(B) ->
    try jsone:decode(B) of
        L when is_list(L) -> [S || S <- L, is_binary(S)];
        _ -> []
    catch
        _:_ -> []
    end;
decode_scopes(_) ->
    [].

-spec normalize_binaries(term()) -> [binary()].
normalize_binaries(L) when is_list(L) -> [stringify_value(X) || X <- L];
normalize_binaries(_) -> [].

-spec normalize_ids(term()) -> [integer()].
normalize_ids(L) when is_list(L) -> [I || I <- L, is_integer(I)];
normalize_ids(_) -> [].

-spec decode_map(term()) -> map().
decode_map(M) when is_map(M) -> M;
decode_map(B) when is_binary(B) ->
    try jsone:decode(B) of
        M2 when is_map(M2) -> M2;
        _ -> #{}
    catch
        _:_ -> #{}
    end;
decode_map(_) ->
    #{}.

%% @doc 授权有效期窗口。未指定时给 ?DEFAULT_GRANT_VALID_DAYS 天（见常量注释：
%% 这是显式决策，且会在审计的 after 里回显真实 valid_to）。
-spec validity_window(map()) -> {binary(), binary()}.
validity_window(Input) ->
    From =
        case maps:get(valid_from, Input, undefined) of
            F when is_binary(F), F =/= <<>> -> F;
            _ -> elib_dt:now()
        end,
    To =
        case maps:get(valid_to, Input, undefined) of
            T when is_binary(T), T =/= <<>> -> T;
            _ -> shift_days(From, ?DEFAULT_GRANT_VALID_DAYS)
        end,
    {From, To}.

%% 起始时间 + N 天。elib_dt:now/0 与 elib_dt:add/2 都走 RFC3339 binary，
%% 无效输入时 elib_dt 返回 {error, invalid_datetime} —— 此时回落到「当前时间 + N 天」
%% 而不是接受一个非法窗口（授权窗口非法等于静默放宽/收紧授权面）。
-spec shift_days(binary() | term(), pos_integer()) -> binary().
shift_days(From, Days) when is_binary(From) ->
    case elib_dt:add(From, {Days * 86400, second}) of
        T when is_binary(T) -> T;
        _ -> elib_dt:add(elib_dt:now(), {Days * 86400, second})
    end;
shift_days(_From, Days) ->
    shift_days(elib_dt:now(), Days).

%% @doc Grant 幂等键：调用方未给时按请求内容生成确定性指纹。
%% 同内容重复提交会命中 (org, app, idempotency_key) 唯一约束 → key_conflict，
%% 而不是静默再发一份（FULL-01 纪律：静默再发会以并集形式扩大授权面）。
-spec grant_idem(map(), binary() | term()) -> binary().
grant_idem(Input, ValidTo) ->
    Scopes = lists:sort(decode_scopes(maps:get(scopes, Input, []))),
    Ws = lists:sort(normalize_ids(maps:get(workspace_ids, Input, []))),
    %% INT-BE-05 缺陷修复：kind 经 handler 归一后是 atom（none/explicit），
    %% 原实现把 atom 直接放进 iolist —— iolist_to_binary badarg，admin 面
    %% 签发 Grant 必 500。统一转 binary（兼容 binary 直传形态）。
    KindBin =
        case maps:get(workspace_scope_kind, Input, none) of
            K when is_atom(K) -> atom_to_binary(K, utf8);
            K when is_binary(K) -> K;
            _ -> <<"none">>
        end,
    Payload = iolist_to_binary([
        KindBin,
        "|",
        lists:join(<<",">>, Scopes),
        "|",
        lists:join(<<",">>, [integer_to_binary(W) || W <- Ws]),
        "|",
        stringify_value(ValidTo)
    ]),
    <<"admin-grant-", (bin_b64url(crypto:hash(sha256, Payload)))/binary>>.

bin_b64url(Bin) ->
    B64 = base64:encode(Bin),
    NoPad = binary:part(B64, 0, byte_size(B64) - 2),
    <<<<(urlsafe(C))/binary>> || <<C>> <= NoPad>>.

urlsafe($+) -> <<"-">>;
urlsafe($/) -> <<"_">>;
urlsafe(C) -> <<C>>.
