-module(enterprise_channel_logic).

%%%
% enterprise_channel_logic 是 V2.1 Internal 资源只读面 INT-30/31 的
% channel adapter logic（plan §6.1 冻结名 planned -> 落地；§6.2：
% workspace_id 必填（缺失 400 invalid_request）、scope=workspace AND
% status=1 强制过滤、A-R read、CURSOR-V2 family=channels）。
%
% 只读 adapter：只调 channel_repo 的 internal_* 读面；不复制 Human
% channel_logic 的创建/订阅/发现治理面。
%
% 投影冻结（V2.1 §15.1 封闭投影）：
%   列表项：channel_id(int64), workspace_id(int64), name(string),
%           subscriber_count(int64), created_at(RFC3339 string)
%   详情：  channel_id, workspace_id, name, description(string),
%           subscriber_count, created_at
%%%

-export([
    list_channels_tx/3,
    locate_tx/3,
    detail_tx/3
]).

-define(FAMILY, <<"channels">>).

%%%===================================================================
%%% INT-30 列表
%%%===================================================================

%% @doc INT-30：必填 workspace_id 过滤的 workspace 频道 keyset 列表
%% （scope=workspace AND status=1 在 repo SQL 内强制）。
%% Opts：workspace_id（integer，必填，handler 已校验）、limit、cursor。
%% cursor filter 冻结为 #{workspace_id => W}。
-spec list_channels_tx(any(), map(), map()) ->
    {ok, map()} | {error, {binary(), term()}}.
list_channels_tx(Conn, Ctx, Opts) when is_map(Opts) ->
    WsId = maps:get(workspace_id, Opts, undefined),
    Limit = maps:get(limit, Opts, 50),
    Cursor = maps:get(cursor, Opts, undefined),
    Filter = #{<<"workspace_id">> => WsId},
    case is_integer(WsId) andalso WsId > 0 of
        true ->
            list_page(Conn, Ctx, WsId, Limit, Cursor, Filter);
        false ->
            {error, {<<"invalid_request">>, invalid_workspace_id}}
    end;
list_channels_tx(_Conn, _Ctx, _Opts) ->
    {error, {<<"invalid_request">>, opts_not_map}}.

-spec list_page(any(), map(), integer(), pos_integer(), undefined | binary(), map()) ->
    {ok, map()} | {error, {binary(), term()}}.
list_page(Conn, Ctx, WsId, Limit, Cursor, Filter) ->
    case enterprise_internal_read_page:resolve(Cursor, Ctx, ?FAMILY, Filter) of
        {ok, Pivot} ->
            case channel_repo:internal_page_tx(Conn, WsId, Pivot, Limit + 1) of
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
%%% INT-31 详情
%%%===================================================================

%% @doc 详情定位（channel W 须归属 credential O；scope=workspace AND status=1）。
%% handler 先调本函数 → 404 同体拒绝 → 同一事务 enforce INT-31（Grant 覆盖
%% channel 所属 W）→ detail_tx/3 投影。
-spec locate_tx(any(), map(), integer()) ->
    {ok, map()} | {error, {binary(), term()}}.
locate_tx(Conn, Ctx, ChannelId) when is_integer(ChannelId), ChannelId > 0 ->
    OrgId = maps:get(organization_id, Ctx),
    case channel_repo:internal_find_tx(Conn, OrgId, ChannelId) of
        {ok, Row} ->
            {ok, Row};
        {error, not_found} ->
            {error, {<<"resource_not_found">>, channel_not_found}};
        {error, Reason} ->
            {error, {<<"internal_error">>, Reason}}
    end;
locate_tx(_Conn, _Ctx, _ChannelId) ->
    {error, {<<"invalid_request">>, invalid_channel_id}}.

%% @doc INT-31 投影 + A-R usage（Row 来自 locate_tx，同一事务）。
-spec detail_tx(any(), map(), map()) -> {ok, map()} | {error, {binary(), term()}}.
detail_tx(_Conn, _Ctx, Row) ->
    %% A-R：usage counter 扩展 metric 需 A0 分配新迁移号（ck_eau_metric DB
    %% CHECK 封闭枚举，proposal 见 A2 RESULT）——本期以 handler 层结构化访问
    %% 日志承担 A-R 事件面，计数面待迁移。
    {ok, detail_view(Row)}.

%%%===================================================================
%%% Internal
%%%===================================================================

-spec item_view(map()) -> map().
item_view(Row) ->
    #{
        <<"channel_id">> => maps:get(<<"id">>, Row),
        <<"workspace_id">> => maps:get(<<"workspace_id">>, Row),
        <<"name">> => maps:get(<<"name">>, Row),
        <<"subscriber_count">> => maps:get(<<"subscriber_count">>, Row),
        <<"created_at">> => maps:get(<<"created_at">>, Row)
    }.

-spec detail_view(map()) -> map().
detail_view(Row) ->
    Base = item_view(Row),
    Base#{<<"description">> => maps:get(<<"description">>, Row)}.

-spec page_reply(any(), map(), [map()], pos_integer(), boolean(), map()) ->
    {ok, map()} | {error, {binary(), term()}}.
page_reply(Conn, Ctx, Rows, Limit, HasMore, Filter) ->
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
