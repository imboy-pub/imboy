-module(talk_record_grant_candidates).
-moduledoc "面聊记录授权候选目录（T17）：通用 + record 级 share-candidates。".
%%%
% share-candidates 域业务逻辑（§8：准备页通用候选入口 + existing record
% 场景 record 级入口；§11.3 仅返回主体有权可见目录的最小信息）。
%
% 通用 GET /api/v1/talk-records/share-candidates：
%   * scope=organization：org 必填且为当前有效归属（org active + 调用者
%     active 成员，否则 403——单凭传入 org_id 不能授权）；ws 可选，须
%     属于该 org（组合非法 422）；候选 = 同 org active 成员（+ ws 时
%     叠加候选侧 ws 资源授权谓词），排除调用者自己；
%   * scope=personal：候选 = 有效本人或双向有效好友；不会枚举全站账号；
%   * 跨企业成员、无权 ws 用户不在候选内（SQL 谓词排除，非事后过滤）。
%
% record 级 GET /:id/share-candidates：
%   * owner 门控（visible + owner；非 owner 可见 403、不可见 404）；
%   * 归属从记录行服务端取（§8：不信任客户端传入归属）；org 记录按
%     记录 org/ws 过滤，候选须满足记录 ws 资源授权；
%   * 排除 owner（主表）与已有活跃授权的被授权人（重复授权无意义，
%     T18 fixture 同语义）。
%
% 分页（§8/§4）：opaque cursor + limit 默认 20 最大 100；cursor 绑定
% 调用者与筛选（签名游标复用 talk_record_ds；record 级再绑定 record_id），
% 不能切 org 沿用。q 限长 100（nickname/account ILIKE）。
% 候选查询结果不是永久授权凭证——每次授权提交（PUT grants）仍重新
% 验证目标资格（talk_record_grant_logic 目录校验）。
%%%

-export([generic/5, for_record/5, parse_scope_filters/1]).

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

-define(MAX_QUERY_CHARS, 100).
-define(CURSOR_FAMILY, <<"talk_record_share_candidates">>).

%% ===================================================================
%% 通用候选 GET /api/v1/talk-records/share-candidates
%% ===================================================================

%% @doc 通用候选入口。Raw：#{scope, organization_id, workspace_id}（Handler
%% 已解析原始值）；Q 已限长；返回 {ok, #{list, next_cursor}} | {error, Reason}。
-spec generic(integer(), map(), binary(), binary() | undefined, pos_integer()) ->
    {ok, map()} | {error, term()}.
generic(Uid, Raw, Q, Cursor, Limit) ->
    case parse_scope_filters(Raw) of
        {ok, #{scope := _Scope} = Filters} ->
            case generic_access(Uid, Filters) of
                ok ->
                    page(Uid, Filters, Q, Cursor, Limit, undefined);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @private 归属权校验（§1：单凭传入 org_id/workspace_id 不能授权）。
%% org：org active + 调用者 active 成员；ws：必须属于该 org（否则 422）。
%% ws 下的候选侧资源授权由候选 SQL 谓词过滤（无权 ws 用户不出现在
% 候选中，同时无权调用者本身进不了 org 目录 → forbidden）。
-spec generic_access(integer(), map()) -> ok | {error, term()}.
generic_access(_Uid, #{scope := <<"personal">>}) ->
    ok;
generic_access(Uid, #{scope := <<"organization">>, organization_id := OrgId} = Filters) ->
    case maps:get(workspace_id, Filters, 0) of
        0 ->
            org_access(Uid, OrgId);
        WsId ->
            case ws_in_org(WsId, OrgId) of
                {ok, true} ->
                    org_access(Uid, OrgId);
                {ok, false} ->
                    {error, invalid_workspace_id};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

-spec org_access(integer(), integer()) -> ok | {error, term()}.
org_access(Uid, OrgId) ->
    Sql =
        <<"SELECT EXISTS (SELECT 1 FROM organization o",
            " JOIN organization_member om ON om.organization_id = o.id",
            " WHERE o.id = $2 AND o.status = 'active'",
            " AND om.user_id = $1 AND om.status = 'active') AS allowed">>,
    case elib_pg:query(Sql, [Uid, OrgId]) of
        {ok, [#{<<"allowed">> := true}]} -> ok;
        {ok, _} -> {error, forbidden};
        {error, Reason} -> {error, Reason}
    end.

-spec ws_in_org(integer(), integer()) -> {ok, boolean()} | {error, term()}.
ws_in_org(WsId, OrgId) ->
    Sql = <<"SELECT organization_id FROM workspace WHERE id = $1">>,
    case elib_pg:query(Sql, [WsId]) of
        {ok, [#{<<"organization_id">> := OrgId}]} -> {ok, true};
        {ok, [_]} -> {ok, false};
        {ok, []} -> {ok, false};
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% record 级候选 GET /:id/share-candidates
%% ===================================================================

%% @doc record 级候选入口（归属服务端取；owner 门控；排除 owner + 已
%% 授权）。返回 {ok, #{list, next_cursor}} | {error, Reason}。
-spec for_record(integer(), integer(), binary(), binary() | undefined, pos_integer()) ->
    {ok, map()} | {error, term()}.
for_record(Uid, RecordId, Q, Cursor, Limit) ->
    case talk_record_content_common:visible_or_404(Uid, RecordId) of
        {ok, #{<<"owner_user_id">> := Uid} = Row} ->
            Filters = #{
                scope => maps:get(<<"scope">>, Row),
                organization_id => maps:get(<<"organization_id">>, Row, 0),
                workspace_id => maps:get(<<"workspace_id">>, Row, 0)
            },
            page(Uid, Filters, Q, Cursor, Limit, #{
                record_id => RecordId,
                owner_uid => maps:get(<<"owner_user_id">>, Row)
            });
        {ok, _} ->
            {error, forbidden};
        {error, Reason} ->
            {error, Reason}
    end.

%% ===================================================================
%% 分页主体
%% ===================================================================

-spec page(
    integer(),
    map(),
    binary(),
    binary() | undefined,
    pos_integer(),
    map() | undefined
) ->
    {ok, map()} | {error, term()}.
page(Uid, Filters, Q, Cursor, Limit, RecordCtx) ->
    Binding = cursor_binding(Uid, Filters, Q, RecordCtx),
    case talk_record_ds:decode_page_cursor(Cursor, ?CURSOR_FAMILY, Binding) of
        {ok, undefined} ->
            page_candidates(Uid, Filters, Q, undefined, Limit, RecordCtx, Binding, normal);
        {ok, LastId} when is_integer(LastId), LastId > 0 ->
            page_candidates(Uid, Filters, Q, LastId, Limit, RecordCtx, Binding, normal);
        {ok, _} ->
            {error, cursor_invalid};
        {error, key_unavailable} ->
            %% 签名 key 缺失：仅禁用翻页（首页可用），与 T05 列表同策略
            page_candidates(Uid, Filters, Q, undefined, Limit, RecordCtx, Binding, no_cursor);
        {error, _} ->
            {error, cursor_invalid}
    end.

-spec page_candidates(
    integer(),
    map(),
    binary(),
    integer() | undefined,
    pos_integer(),
    map() | undefined,
    map(),
    atom()
) ->
    {ok, map()} | {error, term()}.
page_candidates(Uid, Filters, Q, CursorId, Limit, RecordCtx, Binding, CursorMode) ->
    Spec0 = #{
        scope => maps:get(scope, Filters),
        actor_uid => Uid,
        organization_id => maps:get(organization_id, Filters, 0),
        workspace_id => maps:get(workspace_id, Filters, 0),
        exclude_uids => [],
        q => like_pattern(Q),
        cursor_id => CursorId,
        limit => Limit + 1
    },
    Spec = merge_record_ctx(Spec0, RecordCtx),
    case
        elib_pg:with_conn(fun(Conn) -> talk_record_grant_repo:page_candidates_tx(Conn, Spec) end)
    of
        {ok, Rows} ->
            {HasMore, PageRows} = split_page(Rows, Limit),
            Next = next_cursor(CursorMode, HasMore, Binding, PageRows),
            {ok, #{
                <<"list">> => [candidate_dto(R) || R <- PageRows],
                <<"next_cursor">> => Next
            }};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private record 上下文并入（record_id/owner_uid 仅 record 级存在；
%% 触发 repo 侧 owner+活跃被授权人排除谓词）。
-spec merge_record_ctx(map(), map() | undefined) -> map().
merge_record_ctx(Spec, undefined) ->
    Spec;
merge_record_ctx(Spec, Ctx) when is_map(Ctx) ->
    maps:merge(Spec, Ctx).

-spec like_pattern(binary()) -> binary().
like_pattern(<<>>) ->
    <<>>;
like_pattern(Q) ->
    <<"%", (elib_pg:escape_like(Q))/binary, "%">>.

-spec split_page([map()], pos_integer()) -> {boolean(), [map()]}.
split_page(Rows, Limit) ->
    case length(Rows) > Limit of
        true -> {true, lists:sublist(Rows, Limit)};
        false -> {false, Rows}
    end.

-spec next_cursor(atom(), boolean(), map(), [map()]) -> binary() | null.
next_cursor(_Mode, false, _Binding, _Page) ->
    null;
next_cursor(no_cursor, true, _Binding, _Page) ->
    null;
next_cursor(normal, true, Binding, Page) when Page =/= [] ->
    Last = lists:last(Page),
    case talk_record_ds:encode_cursor(?CURSOR_FAMILY, Binding, maps:get(<<"user_id">>, Last)) of
        {ok, Cursor} -> Cursor;
        {error, _} -> null
    end.

%% @private 游标绑定（cursor 绑定调用者与筛选，不能切 org 沿用）。
-spec cursor_binding(integer(), map(), binary(), map() | undefined) -> map().
cursor_binding(Uid, Filters, Q, RecordCtx) ->
    Base = #{
        <<"uid">> => Uid,
        <<"scope">> => maps:get(scope, Filters, <<>>),
        <<"org">> => maps:get(organization_id, Filters, 0),
        <<"ws">> => maps:get(workspace_id, Filters, 0),
        <<"q">> => Q
    },
    case RecordCtx of
        #{record_id := RecordId} -> Base#{<<"record">> => RecordId};
        _ -> Base
    end.

%% @private 候选 DTO（§11.3 最小信息：user_id/display_name/account/avatar）。
-spec candidate_dto(map()) -> map().
candidate_dto(R) ->
    #{
        <<"user_id">> => maps:get(<<"user_id">>, R),
        <<"display_name">> => maps:get(<<"display_name">>, R, <<>>),
        <<"account">> => maps:get(<<"account">>, R, <<>>),
        <<"avatar">> => maps:get(<<"avatar">>, R, <<>>)
    }.

%% ===================================================================
%% 入参解析
%% ===================================================================

%% @doc scope/org/ws 组合校验（§8）：personal 拒绝 org/ws；organization
%% 必填 org；ws 仅在 org 下允许。
-spec parse_scope_filters(map()) -> {ok, map()} | {error, atom()}.
parse_scope_filters(#{scope := <<"personal">>} = Raw) ->
    case {maps:get(organization_id, Raw, 0), maps:get(workspace_id, Raw, 0)} of
        {0, 0} -> {ok, Raw};
        _ -> {error, personal_with_org}
    end;
parse_scope_filters(#{scope := <<"organization">>} = Raw) ->
    case positive(maps:get(organization_id, Raw, 0)) of
        {ok, OrgId} ->
            {ok, Raw#{
                organization_id => OrgId,
                workspace_id => ws_or_zero(maps:get(workspace_id, Raw, 0))
            }};
        error ->
            {error, invalid_organization_id}
    end;
parse_scope_filters(_) ->
    {error, invalid_scope}.

-spec ws_or_zero(term()) -> integer().
ws_or_zero(V) ->
    case positive(V) of
        {ok, WsId} -> WsId;
        _ -> 0
    end.

-spec positive(term()) -> {ok, pos_integer()} | error.
positive(N) when is_integer(N), N > 0 -> {ok, N};
positive(B) when is_binary(B) ->
    try
        positive(binary_to_integer(B))
    catch
        _:_ -> error
    end;
positive(_) ->
    error.
