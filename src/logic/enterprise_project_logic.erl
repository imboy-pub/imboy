-module(enterprise_project_logic).

%%%
% enterprise_project_logic 是 V2.1 Internal 资源只读面 INT-28/29 的
% project adapter logic（plan §6.1 冻结名 planned -> 落地；§6.2：
% workspace_id 必填（缺失 400 invalid_request）、status active|done|all、
% 无 archive（active|done 均可读）、A-R read、CURSOR-V2 family=projects）。
%
% 只读 adapter：只调 project_repo 的 internal_* 读面；不复制 Human
% project_logic 的创建/更新/成员治理面。
%
% 投影冻结（V2.1 §15.1 封闭投影）：
%   列表项：project_id(int64), name(string), owner_id(int64),
%           status("active"|"done"), created_at(RFC3339 string)
%   详情：  project_id, workspace_id(int64), name, description(string),
%           owner_id, status, created_at
%%%

-export([
    list_projects_tx/3,
    locate_tx/3,
    detail_tx/3
]).

-define(FAMILY, <<"projects">>).

%%%===================================================================
%%% INT-28 列表
%%%===================================================================

%% @doc INT-28：必填 workspace_id 过滤的 project keyset 列表。
%% Opts：workspace_id（integer，必填，handler 已校验）、status
%% （active|done|all，缺省 all）、limit（1..100）、cursor。
%% cursor filter 冻结为 #{workspace_id => W, status => StatusBin}。
-spec list_projects_tx(any(), map(), map()) ->
    {ok, map()} | {error, {binary(), term()}}.
list_projects_tx(Conn, Ctx, Opts) when is_map(Opts) ->
    WsId = maps:get(workspace_id, Opts, undefined),
    Status = maps:get(status, Opts, all),
    StatusBin =
        case Status of
            all -> <<"all">>;
            S when is_binary(S) -> S
        end,
    Limit = maps:get(limit, Opts, 50),
    Cursor = maps:get(cursor, Opts, undefined),
    Filter = #{<<"workspace_id">> => WsId, <<"status">> => StatusBin},
    case
        is_integer(WsId) andalso WsId > 0 andalso
            valid_status(Status)
    of
        true ->
            list_page(Conn, Ctx, WsId, Status, Limit, Cursor, Filter);
        false ->
            {error, {<<"invalid_request">>, invalid_list_input}}
    end;
list_projects_tx(_Conn, _Ctx, _Opts) ->
    {error, {<<"invalid_request">>, opts_not_map}}.

-spec list_page(
    any(), map(), integer(), all | binary(), pos_integer(), undefined | binary(), map()
) -> {ok, map()} | {error, {binary(), term()}}.
list_page(Conn, Ctx, WsId, Status, Limit, Cursor, Filter) ->
    case enterprise_internal_read_page:resolve(Cursor, Ctx, ?FAMILY, Filter) of
        {ok, Pivot} ->
            case project_repo:internal_page_tx(Conn, WsId, Status, Pivot, Limit + 1) of
                {ok, Rows0} when length(Rows0) > Limit ->
                    page_reply(
                        Conn, Ctx, lists:sublist(Rows0, Limit), Limit, true, Filter
                    );
                {ok, Rows} ->
                    page_reply(Conn, Ctx, Rows, Limit, false, Filter);
                {error, Reason} ->
                    {error, {<<"internal_error">>, Reason}}
            end;
        {error, invalid_request} ->
            {error, {<<"invalid_request">>, cursor_invalid}};
        {error, security_gate_closed} ->
            {error, {<<"security_gate_closed">>, cursor_signing_key_unavailable}}
    end.

%%%===================================================================
%%% INT-29 详情
%%%===================================================================

%% @doc 详情定位（project W 须归属 credential O 且 workspace active）。
%% handler 先调本函数 → 404 同体拒绝 → 同一事务 enforce INT-29（Grant 覆盖
%% project 所属 W）→ detail_tx/3 投影。
-spec locate_tx(any(), map(), integer()) ->
    {ok, map()} | {error, {binary(), term()}}.
locate_tx(Conn, Ctx, ProjectId) when is_integer(ProjectId), ProjectId > 0 ->
    OrgId = maps:get(organization_id, Ctx),
    case project_repo:internal_find_tx(Conn, OrgId, ProjectId) of
        {ok, Row} ->
            {ok, Row};
        {error, not_found} ->
            {error, {<<"resource_not_found">>, project_not_found}};
        {error, Reason} ->
            {error, {<<"internal_error">>, Reason}}
    end;
locate_tx(_Conn, _Ctx, _ProjectId) ->
    {error, {<<"invalid_request">>, invalid_project_id}}.

%% @doc INT-29 投影 + A-R usage（Row 来自 locate_tx，同一事务）。
-spec detail_tx(any(), map(), map()) -> {ok, map()} | {error, {binary(), term()}}.
detail_tx(_Conn, _Ctx, Row) ->
    %% A-R：usage counter 扩展 metric 需 A0 分配新迁移号（ck_eau_metric DB
    %% CHECK 封闭枚举，proposal 见 A2 RESULT）——本期以 handler 层结构化访问
    %% 日志承担 A-R 事件面，计数面待迁移。
    {ok, detail_view(Row)}.

%%%===================================================================
%%% Internal
%%%===================================================================

-spec valid_status(term()) -> boolean().
valid_status(all) -> true;
valid_status(<<"active">>) -> true;
valid_status(<<"done">>) -> true;
valid_status(_) -> false.

-spec item_view(map()) -> map().
item_view(Row) ->
    #{
        <<"project_id">> => maps:get(<<"id">>, Row),
        <<"name">> => maps:get(<<"name">>, Row),
        <<"owner_id">> => maps:get(<<"owner_id">>, Row),
        <<"status">> => maps:get(<<"status">>, Row),
        <<"created_at">> => maps:get(<<"created_at">>, Row)
    }.

-spec detail_view(map()) -> map().
detail_view(Row) ->
    Base = item_view(Row),
    Base#{
        <<"workspace_id">> => maps:get(<<"workspace_id">>, Row),
        <<"description">> => maps:get(<<"description">>, Row)
    }.

-spec page_reply(any(), map(), [map()], pos_integer(), boolean(), map()) ->
    {ok, map()} | {error, {binary(), term()}}.
page_reply(_Conn, Ctx, Rows, Limit, HasMore, Filter) ->
    Next =
        case HasMore andalso Rows =/= [] of
            true ->
                Last = lists:last(Rows),
                Tuple = {maps:get(<<"created_at">>, Last), maps:get(<<"id">>, Last)},
                case enterprise_internal_read_page:encode(Ctx, ?FAMILY, Filter, Tuple) of
                    {ok, Cursor} -> Cursor;
                    {error, EncodeReason} -> erlang:error({cursor_encode_failed, EncodeReason})
                end;
            false ->
                null
        end,
    {ok, #{
        <<"items">> => [item_view(R) || R <- Rows],
        <<"limit">> => Limit,
        <<"has_more">> => HasMore,
        <<"next_cursor">> => Next
    }}.
