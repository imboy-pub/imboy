-module(enterprise_directory_logic).

%%%
% enterprise_directory_logic 是**受限 cursor directory** use case
% （FULL-02 / plan-full §3.1「外部身份 … 受限 cursor directory，不允许无界导出」、
% §4 `/identity-mappings` cursor list 与 `/directory/users`「最小字段、cursor、
% Workspace Grant 过滤，无全量导出」）。
%
%% 硬边界（本模块的对外契约）：
%   * **分页有上限**：page_size ∈ [1, 100]，缺省 50。超出上限**拒绝**
%     （invalid_request），不静默截断——静默截断会把「想要全量」变成一个永远
%     成功的循环，掩盖无界导出意图。page_size 非整数/0/负数同样拒绝。
%   * **无全量导出形态**：本模块只导出 page_mappings_tx/3 与 page_users_tx/3
%     两个**分页**入口；仓储每次查询恒带 ORDER BY + LIMIT（keyset pagination），
%     没有 OFFSET、没有无 LIMIT 读、没有 COUNT 导出。真库套件以导出面 + 超大
%     page_size 负例 + 逐页遍历终止性三重证据钉死（用例名见 checkpoint）。
%   * **游标不透明且不可跨页族复用**：cursor = base64url("<族标签>:<keyset>")；
%     畸形/换族/越族 → invalid_request。游标只含**键**，不含任何 PII。
%   * **只返回最小字段**（见模块内 items 构造）；不返回姓名/手机号/邮箱等。
%   * 跨 Org / 跨 App 的键在 SQL 内不可见（repo 复合过滤），游标重放不构成越权。
%   * 计量：每次成功分页记 `directory.page`（聚合计数，不含任何内容）。
%%%

-export([
    page_mappings_tx/3,
    page_users_tx/3,
    max_page/0,
    default_page/0
]).

-define(MAX_PAGE, 100).
-define(DEFAULT_PAGE, 50).
-define(TAG_MAPPINGS, <<"mappings">>).
-define(TAG_USERS, <<"users">>).

%% ===================================================================
%% API functions
%% ===================================================================

-spec max_page() -> pos_integer().
max_page() ->
    ?MAX_PAGE.

-spec default_page() -> pos_integer().
default_page() ->
    ?DEFAULT_PAGE.

%% @doc 映射目录一页（active external identity，最小字段）。
%% Opts（atom 键）：
%%   cursor    :: binary() | undefined（上一页 next_cursor）
%%   page_size :: integer() | undefined（缺省 ?DEFAULT_PAGE）
%% 返回 {ok, #{items, page_size, has_more, next_cursor}}；
%% 入参非法 → {error, {<<"invalid_request">>, Detail}}。
-spec page_mappings_tx(any(), map(), map()) -> {ok, map()} | {error, {binary(), term()}}.
page_mappings_tx(Conn, Ctx, Opts) when is_map(Opts) ->
    with_page(Conn, Ctx, Opts, ?TAG_MAPPINGS, fun(After, Limit) ->
        case
            enterprise_directory_repo:page_mappings_tx(
                Conn, org_id(Ctx), app_id(Ctx), After, Limit
            )
        of
            {ok, Rows} ->
                {ok, [
                    #{
                        <<"external_user_id">> => maps:get(<<"external_user_id">>, R),
                        <<"user_id">> => maps:get(<<"user_id">>, R)
                    }
                 || R <- Rows
                ]};
            {error, Reason} ->
                {error, Reason}
        end
    end);
page_mappings_tx(_Conn, _Ctx, _Opts) ->
    {error, {<<"invalid_request">>, opts_not_map}}.

%% @doc 成员目录一页（本 Org active Human，可选 Workspace 过滤）。
%% Opts（atom 键）：
%%   cursor       :: binary() | undefined
%%   page_size    :: integer() | undefined
%%   workspace_id :: integer() | undefined（给出时要求 identities:read 的
%%                   Workspace Grant 覆盖该 workspace——受管应用若只有别的
%%                   workspace 的授权，本请求即 organization_boundary_violation）
%% 返回同 page_mappings_tx/3；items 为最小字段
%% （user_id / external_user_id（本 app 未映射则 null）/ member_role /
%%  member_status）。
-spec page_users_tx(any(), map(), map()) -> {ok, map()} | {error, {binary(), term()}}.
page_users_tx(Conn, Ctx, Opts) when is_map(Opts) ->
    case parse_workspace_id(maps:get(workspace_id, Opts, undefined)) of
        {ok, WsId} ->
            case workspace_boundary(Conn, Ctx, WsId) of
                ok ->
                    with_page(Conn, Ctx, Opts, ?TAG_USERS, fun(After, Limit) ->
                        user_page(Conn, Ctx, WsId, After, Limit)
                    end);
                {error, _} = Err ->
                    Err
            end;
        {error, Detail} ->
            {error, {<<"invalid_request">>, Detail}}
    end;
page_users_tx(_Conn, _Ctx, _Opts) ->
    {error, {<<"invalid_request">>, opts_not_map}}.

%% ===================================================================
%% Internal
%% ===================================================================

-spec user_page(any(), map(), undefined | integer(), undefined | integer(), pos_integer()) ->
    {ok, [map()]} | {error, term()}.
user_page(Conn, Ctx, undefined, After, Limit) ->
    fetch_users(
        enterprise_directory_repo:page_members_tx(
            Conn, org_id(Ctx), app_id(Ctx), After, Limit
        )
    );
user_page(Conn, Ctx, WsId, After, Limit) ->
    fetch_users(
        enterprise_directory_repo:page_members_in_workspace_tx(
            Conn, org_id(Ctx), app_id(Ctx), WsId, After, Limit
        )
    ).

-spec fetch_users({ok, [map()]} | {error, term()}) ->
    {ok, [map()]} | {error, term()}.
fetch_users({ok, Rows}) ->
    {ok, [user_view(R) || R <- Rows]};
fetch_users({error, Reason}) ->
    {error, Reason}.

-spec user_view(map()) -> map().
user_view(R) ->
    #{
        <<"user_id">> => maps:get(<<"user_id">>, R),
        <<"external_user_id">> => maps:get(<<"external_user_id">>, R, null),
        <<"member_role">> => maps:get(<<"member_role">>, R, null),
        <<"member_status">> => maps:get(<<"member_status">>, R, null)
    }.

%% @doc 分页骨架：解析 page_size / cursor → 多取一行判 has_more → 组装响应 →
%% 记聚合计量。Limit+1 的额外行**只**用于 has_more 判定，不进入 items。
-spec with_page(any(), map(), map(), binary(), fun(
    (undefined | term(), pos_integer()) -> {ok, [map()]} | {error, term()}
)) ->
    {ok, map()} | {error, {binary(), term()}}.
with_page(Conn, Ctx, Opts, Tag, Fetch) ->
    case page_size(Opts) of
        {ok, PageSize} ->
            case decode_cursor(maps:get(cursor, Opts, undefined), Tag) of
                {ok, After} ->
                    case Fetch(After, PageSize + 1) of
                        {ok, Rows} when length(Rows) > PageSize ->
                            {Items, _Extra} = lists:split(PageSize, Rows),
                            reply_page(Conn, Ctx, Items, PageSize, true, Tag);
                        {ok, Rows} ->
                            reply_page(Conn, Ctx, Rows, PageSize, false, Tag);
                        {error, Reason} ->
                            {error, {<<"internal_error">>, Reason}}
                    end;
                {error, Detail} ->
                    {error, {<<"invalid_request">>, Detail}}
            end;
        {error, Detail} ->
            {error, {<<"invalid_request">>, Detail}}
    end.

-spec reply_page(any(), map(), [map()], pos_integer(), boolean(), binary()) ->
    {ok, map()} | {error, {binary(), term()}}.
reply_page(Conn, Ctx, Items, PageSize, HasMore, Tag) ->
    Next =
        case HasMore of
            false -> null;
            true -> encode_cursor(Tag, next_key(Tag, lists:last(Items)))
        end,
    case
        enterprise_application_usage_repo:bump_tx(
            Conn, org_id(Ctx), app_id(Ctx), <<"directory.page">>
        )
    of
        ok ->
            {ok, #{
                <<"items">> => Items,
                <<"page_size">> => PageSize,
                <<"has_more">> => HasMore,
                <<"next_cursor">> => Next
            }};
        {error, Reason} ->
            {error, {<<"internal_error">>, Reason}}
    end.

%% @doc 下一页 keyset（族标签不同 → 键字段不同）。
-spec next_key(binary(), map()) -> binary().
next_key(?TAG_MAPPINGS, Item) ->
    maps:get(<<"external_user_id">>, Item);
next_key(?TAG_USERS, Item) ->
    integer_to_binary(maps:get(<<"user_id">>, Item)).

%% @doc page_size 上限判定：缺省 50；非 [1,100] 一律拒绝（不静默截断）。
-spec page_size(map()) -> {ok, pos_integer()} | {error, term()}.
page_size(Opts) ->
    case maps:get(page_size, Opts, undefined) of
        undefined ->
            {ok, ?DEFAULT_PAGE};
        N when is_integer(N), N >= 1, N =< ?MAX_PAGE ->
            {ok, N};
        _ ->
            {error, {page_size_out_of_range, ?MAX_PAGE}}
    end.

%% @doc cursor 解码：base64url(<族标签>:<keyset>)；畸形/换族一律拒绝。
-spec decode_cursor(undefined | binary(), binary()) ->
    {ok, undefined | binary() | integer()} | {error, term()}.
decode_cursor(undefined, _Tag) ->
    {ok, undefined};
decode_cursor(Cursor, Tag) when is_binary(Cursor) ->
    try base64:decode(Cursor) of
        Decoded ->
            case binary:split(Decoded, <<":">>) of
                [Tag, Key] when Key =/= <<>> -> decode_key(Tag, Key);
                [_Other, _Key] -> {error, cursor_wrong_family};
                _ -> {error, invalid_cursor}
            end
    catch
        _:_ ->
            {error, invalid_cursor}
    end;
decode_cursor(_Cursor, _Tag) ->
    {error, invalid_cursor}.

-spec decode_key(binary(), binary()) -> {ok, binary() | integer()} | {error, term()}.
decode_key(?TAG_MAPPINGS, Key) when byte_size(Key) =< 256 ->
    {ok, Key};
decode_key(?TAG_USERS, Key) ->
    try binary_to_integer(Key) of
        N when N > 0 -> {ok, N};
        _ -> {error, invalid_cursor}
    catch
        _:_ -> {error, invalid_cursor}
    end;
decode_key(_Tag, _Key) ->
    {error, invalid_cursor}.

-spec encode_cursor(binary(), binary()) -> binary().
encode_cursor(Tag, Key) ->
    base64:encode(<<Tag/binary, ":", Key/binary>>).

-spec parse_workspace_id(undefined | integer()) ->
    {ok, undefined | integer()} | {error, term()}.
parse_workspace_id(undefined) ->
    {ok, undefined};
parse_workspace_id(WsId) when is_integer(WsId), WsId > 0 ->
    {ok, WsId};
parse_workspace_id(_) ->
    {error, invalid_workspace_id}.

%% @doc Workspace Grant 过滤（仅给出 workspace_id 时）：显式 Workspace Grant
%% 必须由**同一个**生效 Grant 覆盖 identities:read 与该 workspace；未受管应用
%% 是 no-op（沿用广州期口径）。
-spec workspace_boundary(any(), map(), undefined | integer()) ->
    ok | {error, {binary(), term()}}.
workspace_boundary(_Conn, _Ctx, undefined) ->
    ok;
workspace_boundary(Conn, Ctx, WsId) ->
    case
        enterprise_application_grant_logic:require_workspace_tx(
            Conn, Ctx, WsId, <<"identities:read">>
        )
    of
        ok -> ok;
        {error, Code} -> {error, {atom_to_binary(Code, utf8), grant_boundary}}
    end.

-spec org_id(map()) -> integer().
org_id(Ctx) ->
    maps:get(organization_id, Ctx).

-spec app_id(map()) -> integer().
app_id(Ctx) ->
    maps:get(application_id, Ctx).
