-module(enterprise_workspace_logic).

%%%
% enterprise_workspace_logic 是 V2.1 Internal 资源只读面 INT-24/25 的
% workspace adapter logic（plan §6.1 冻结名 planned -> 落地；§6.2 行为矩阵：
% credential O 内、仅 Grant 覆盖的 W、active only、A-R read、CURSOR-V2）。
%
% 只做三件事（不复制 Human workspace_logic 的任何治理面）：
%   * INT-24 列表：cursor family=workspaces（filter 冻结为空 map）；
%     行集收窄谓词（仅 Grant 覆盖 W）在 workspace_repo:internal_covered_page_tx/5
%     的 SQL 内实现（kind=list 的行级收窄是 SQL 义务，禁止整表回读后内存过滤）；
%   * INT-25 详情：Org 边界 active 定位（跨 Org/不存在/已归档同体 404，
%     IDOR 不泄露存在性）——Grant 覆盖判定由 handler 经
%     enterprise_internal_boundary:enforce/4（INT-25，workspace kind）承担；
%   * A-R read usage：workspace.read 聚合计数（§12：usage counter，无逐项
%     PII、无响应体）。
%
% 投影冻结（V2.1 §15.1 封闭投影，实现前登记 RESULT.json）：
%   列表项/详情：workspace_id(int64), name(string), owner_id(int64),
%   created_at(RFC3339 string)。
%%%

-export([
    list_workspaces_tx/3,
    locate_active_tx/3,
    detail_tx/3
]).

-define(FAMILY, <<"workspaces">>).

%%%===================================================================
%%% INT-24 列表
%%%===================================================================

%% @doc INT-24：Grant 覆盖 W 集合的 active workspace keyset 列表。
%% Opts（atom 键）：limit（1..100，已由 handler 校验）、cursor（binary|undefined）。
%% 返回信封 {ok, #{items, limit, has_more, next_cursor}}（§10.1 冻结形态）。
-spec list_workspaces_tx(any(), map(), map()) ->
    {ok, map()} | {error, {binary(), term()}}.
list_workspaces_tx(Conn, Ctx, Opts) when is_map(Opts) ->
    OrgId = maps:get(organization_id, Ctx),
    AppId = maps:get(application_id, Ctx),
    Limit = maps:get(limit, Opts, 50),
    Cursor = maps:get(cursor, Opts, undefined),
    Filter = #{},
    case enterprise_internal_read_page:resolve(Cursor, Ctx, ?FAMILY, Filter) of
        {ok, Pivot} ->
            case
                workspace_repo:internal_covered_page_tx(
                    Conn, OrgId, AppId, Pivot, Limit + 1
                )
            of
                {ok, Rows0} when length(Rows0) > Limit ->
                    Rows = lists:sublist(Rows0, Limit),
                    page_reply(Conn, Ctx, Rows, Limit, true, ?FAMILY, Filter);
                {ok, Rows} ->
                    page_reply(Conn, Ctx, Rows, Limit, false, ?FAMILY, Filter);
                {error, Reason} ->
                    {error, {<<"internal_error">>, Reason}}
            end;
        {error, invalid_request} ->
            {error, {<<"invalid_request">>, cursor_invalid}};
        {error, security_gate_closed} ->
            {error, {<<"security_gate_closed">>, cursor_signing_key_unavailable}}
    end;
list_workspaces_tx(_Conn, _Ctx, _Opts) ->
    {error, {<<"invalid_request">>, opts_not_map}}.

%%%===================================================================
%%% INT-25 详情
%%%===================================================================

%% @doc Org 边界 active 定位（handler 先调本函数拿行 → 404 同体拒绝 →
%% 同一事务内 enforce INT-25（Grant 覆盖判定）→ 再调 detail_tx/3 投影）。
-spec locate_active_tx(any(), map(), integer()) ->
    {ok, map()} | {error, {binary(), term()}}.
locate_active_tx(Conn, Ctx, WsId) when is_integer(WsId), WsId > 0 ->
    OrgId = maps:get(organization_id, Ctx),
    case workspace_repo:internal_find_tx(Conn, OrgId, WsId) of
        {ok, Row} ->
            {ok, Row};
        {error, not_found} ->
            {error, {<<"resource_not_found">>, workspace_not_found}};
        {error, Reason} ->
            {error, {<<"internal_error">>, Reason}}
    end;
locate_active_tx(_Conn, _Ctx, _WsId) ->
    {error, {<<"invalid_request">>, invalid_workspace_id}}.

%% @doc INT-25 投影 + A-R usage（Row 来自 locate_active_tx，同一事务）。
-spec detail_tx(any(), map(), map()) -> {ok, map()} | {error, {binary(), term()}}.
detail_tx(_Conn, _Ctx, Row) ->
    %% A-R：usage counter 扩展 metric 需 A0 分配新迁移号（ck_eau_metric DB
    %% CHECK 封闭枚举，proposal 见 A2 RESULT）——本期以 handler 层结构化访问
    %% 日志承担 A-R 事件面，计数面待迁移。
    {ok, workspace_view(Row)}.

%%%===================================================================
%%% Internal
%%%===================================================================

-spec workspace_view(map()) -> map().
workspace_view(Row) ->
    #{
        <<"workspace_id">> => maps:get(<<"id">>, Row),
        <<"name">> => maps:get(<<"name">>, Row),
        <<"owner_id">> => maps:get(<<"owner_id">>, Row),
        <<"created_at">> => maps:get(<<"created_at">>, Row)
    }.

%% 列表信封 + 末行 sort_tuple 游标签发（§10.1/§10.2：created_at DESC, id DESC）。
-spec page_reply(any(), map(), [map()], pos_integer(), boolean(), binary(), map()) ->
    {ok, map()} | {error, {binary(), term()}}.
page_reply(_Conn, Ctx, Rows, Limit, HasMore, Family, Filter) ->
    Next =
        case HasMore andalso Rows =/= [] of
            true ->
                Last = lists:last(Rows),
                Tuple = {maps:get(<<"created_at">>, Last), maps:get(<<"id">>, Last)},
                case enterprise_internal_read_page:encode(Ctx, Family, Filter, Tuple) of
                    {ok, Cursor} -> Cursor;
                    {error, EncodeReason} -> erlang:error({cursor_encode_failed, EncodeReason})
                end;
            false ->
                null
        end,
    {ok, #{
        <<"items">> => [workspace_view(R) || R <- Rows],
        <<"limit">> => Limit,
        <<"has_more">> => HasMore,
        <<"next_cursor">> => Next
    }}.
