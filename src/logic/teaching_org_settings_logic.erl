-module(teaching_org_settings_logic).
%%%
% 机构设置读写逻辑（墨芽教学域）—— 首版只放开 AI 辅助批改开关。
% Org settings logic
%
% 守卫（deny-by-default）：active organization_member 的 owner/admin 可读写。
% Organization Member 与 Workspace Member 相互独立；跨 Organization 的
% Workspace 协作者不会因此获得机构级策略权限。
%
% 这不是个人偏好，而是机构级策略：普通组织成员、Workspace 成员及班 staff
% 均不因此获得修改权限。
%
% 客户端契约（moya 老师端「机构」页）：
%   GET  → #{organization_id, organization_name, ai_assist_enabled}
%   POST → 同上（写后回读，返回落库后的真实值，不让客户端猜）
%   organization_id 一律 TSID 字符串（64-bit ID 走 JSON string 铁规则）。
%%%

-export([get_settings/2, set_ai_assist/3]).

-include("log.hrl").
-include_lib("kernel/include/logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc 读机构设置。错误：not_authorized / not_found / db_error。
-spec get_settings(integer(), integer()) -> {ok, map()} | {error, atom()}.
get_settings(Uid, OrgId) ->
    guarded(Uid, OrgId, read, fun(Conn) ->
        case teaching_org_settings_repo:org_row_tx(Conn, OrgId) of
            {ok, Row} ->
                {ok, payload(OrgId, Row)};
            {error, not_found} ->
                {error, not_found};
            {error, Reason} ->
                ?LOG_ERROR("teaching_org_settings read db error ~p", [Reason]),
                {error, db_error}
        end
    end).

%% @doc 写 AI 辅助开关（真门控的服务端落点：关闭后 AI 草稿不再生成/不再调用）。
%% 写后回读同一行，返回真实终值（避免「客户端以为开了其实没开」）。
-spec set_ai_assist(integer(), integer(), boolean()) -> {ok, map()} | {error, atom()}.
set_ai_assist(Uid, OrgId, Enabled) when is_boolean(Enabled) ->
    guarded(Uid, OrgId, write, fun(Conn) ->
        case teaching_org_settings_repo:set_ai_assist_enabled_tx(Conn, OrgId, Enabled) of
            ok ->
                case teaching_org_settings_repo:org_row_tx(Conn, OrgId) of
                    {ok, Row} ->
                        audit(OrgId, Uid, Enabled),
                        {ok, payload(OrgId, Row)};
                    {error, Reason} ->
                        ?LOG_ERROR("teaching_org_settings readback db error ~p", [Reason]),
                        {error, db_error}
                end;
            {error, not_found} ->
                {error, not_found};
            {error, Reason} ->
                ?LOG_ERROR("teaching_org_settings write db error ~p", [Reason]),
                {error, db_error}
        end
    end).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% 守卫与业务读写共用一个事务连接，避免成员关系校验后的竞态窗口。
-spec guarded(integer(), integer(), read | write, fun((any()) -> {ok, map()} | {error, atom()})) ->
    {ok, map()} | {error, atom()}.
guarded(Uid, OrgId, Mode, Inner) when is_integer(Uid), is_integer(OrgId) ->
    Tx = fun(Conn) ->
        Membership =
            case Mode of
                read ->
                    organization_member_repo:find_active_tx(Conn, OrgId, Uid, <<"role">>);
                write ->
                    organization_member_repo:find_active_for_share_tx(Conn, OrgId, Uid, <<"role">>)
            end,
        case Membership of
            {ok, #{<<"role">> := Role}} when Role =:= <<"owner">>; Role =:= <<"admin">> ->
                Inner(Conn);
            {ok, _} ->
                ?LOG_INFO(
                    "[teaching_org_settings] denied uid=~p org_id=~p",
                    [Uid, OrgId]
                ),
                {error, not_authorized};
            {error, not_found} ->
                {error, not_authorized};
            {error, Reason} ->
                ?LOG_ERROR("teaching_org_settings acl db error ~p", [Reason]),
                {error, db_error}
        end
    end,
    tx_run(Tx).

%% with_tx 透传 Tx 的原始返回（{ok,_} / 业务 {error, Atom}），异常与 rollback 折叠 db_error。
tx_run(Tx) ->
    case elib_pg:with_tx(Tx, [{reraise, false}]) of
        {rollback, Reason} ->
            ?LOG_ERROR("teaching_org_settings tx rollback ~p", [Reason]),
            {error, db_error};
        Result ->
            Result
    end.

-spec payload(integer(), map()) -> map().
payload(OrgId, Row) ->
    #{
        <<"organization_id">> => integer_to_binary(OrgId),
        <<"organization_name">> => elib_cnv:safe_to_binary(maps:get(<<"name">>, Row, <<>>)),
        <<"ai_assist_enabled">> => teaching_org_settings_repo:ai_assist_enabled_of(Row)
    }.

%% 审计：只记 ID 与事件，不含 PII（与 teaching_learner_bind_logic 同纪律）。
-spec audit(integer(), integer(), boolean()) -> ok.
audit(OrgId, Uid, Enabled) ->
    ?LOG_INFO(
        "[teaching_org_settings] event=ai_assist_set org_id=~p operator_uid=~p enabled=~p",
        [OrgId, Uid, Enabled]
    ).
