-module(enterprise_directory_logic).

%%%
% enterprise_directory_logic 是**受限 cursor directory** use case
% （FULL-02 / plan-full §3.1「外部身份 … 受限 cursor directory，不允许无界导出」、
% §4 `/identity-mappings` cursor list 与 `/directory/users`「最小字段、cursor、
% Workspace Grant 过滤，无全量导出」；CP-CON-01 游标迁移 CURSOR-V2）。
%
% 硬边界（本模块的对外契约）：
%   * **分页有上限**：page_size ∈ [1, 100]，缺省 50。超出上限**拒绝**
%     （invalid_request），不静默截断——静默截断会把「想要全量」变成一个永远
%     成功的循环，掩盖无界导出意图。page_size 非整数/0/负数同样拒绝。
%   * **无全量导出形态**：本模块只导出 page_mappings_tx/3 与 page_users_tx/3
%     两个**分页**入口；仓储每次查询恒带 ORDER BY + LIMIT（keyset pagination），
%     没有 OFFSET、没有无 LIMIT 读、没有 COUNT 导出。真库套件以导出面 + 超大
%     page_size 负例 + 逐页遍历终止性三重证据钉死（用例名见 checkpoint）。
%   * **游标是 CURSOR-V2 签名形态**（§10.1 冻结合同；签名/验签本体在
%     src/lib/enterprise_cursor_v2.erl，本模块不重复实现编码与 HMAC）：
%     - 族标签冻结：identity_mappings（INT-16）/ directory_users（INT-17）
%       （§10.2 白名单内）；sort_tuple：mappings=[external_user_id binary]、
%       users=[user_id integer]；
%     - 验签 + **逐字段绑定比对**（family / organization_id / application_id /
%       filter 整 map）：malformed / tampered / foreign-family / foreign 绑定 /
%       expired（>24h）一律 invalid_request（400，不回显原因）；换 filter 的
%       旧游标不得翻新 filter 行集；
%     - **旧 unsigned 形态**（base64("<tag>:<keyset>")，无 HMAC 段）没有
%       合法签名——verify 必拒 → 400；
%     - 签名密钥缺失/非法（{imboy, enterprise_internal_cursor_signing_key}
%       < 32 bytes）→ security_gate_closed（503，fail-closed；has_more 页
%       签不出下一页同样 503，绝不伪装成末页）；
%     - 游标只含**键**，不含任何 PII。
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

%% §10.2 冻结页族（enterprise_cursor_v2 白名单内）。
-define(FAMILY_MAPPINGS, <<"identity_mappings">>).
-define(FAMILY_USERS, <<"directory_users">>).

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
%%   cursor      :: binary() | undefined（上一页 next_cursor）
%%   page_size   :: integer() | undefined（缺省 ?DEFAULT_PAGE）
%%   workspace_id :: integer() | undefined（参与 Grant 边界与游标 filter 绑定）
%% 返回 {ok, #{items, page_size, has_more, next_cursor}}；
%% 入参非法 → {error, {<<"invalid_request">>, Detail}}。
-spec page_mappings_tx(any(), map(), map()) -> {ok, map()} | {error, {binary(), term()}}.
page_mappings_tx(Conn, Ctx, Opts) when is_map(Opts) ->
    case parse_workspace_id(maps:get(workspace_id, Opts, undefined)) of
        {ok, WsId} ->
            with_page(Conn, Ctx, Opts, ws_filter(WsId), ?FAMILY_MAPPINGS, fun(After, Limit) ->
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
        {error, Detail} ->
            {error, {<<"invalid_request">>, Detail}}
    end;
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
                    with_page(Conn, Ctx, Opts, ws_filter(WsId), ?FAMILY_USERS, fun(After, Limit) ->
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
-spec with_page(any(), map(), map(), map(), binary(), fun(
    (undefined | term(), pos_integer()) -> {ok, [map()]} | {error, term()}
)) ->
    {ok, map()} | {error, {binary(), term()}}.
with_page(Conn, Ctx, Opts, Filter, Family, Fetch) ->
    case page_size_opts(Opts) of
        {ok, PageSize} ->
            case resolve_cursor(maps:get(cursor, Opts, undefined), Family, Ctx, Filter) of
                {ok, After} ->
                    case Fetch(After, PageSize + 1) of
                        {ok, Rows} when length(Rows) > PageSize ->
                            {Items, _Extra} = lists:split(PageSize, Rows),
                            reply_page(Conn, Ctx, Filter, Family, Items, PageSize, true);
                        {ok, Rows} ->
                            reply_page(Conn, Ctx, Filter, Family, Rows, PageSize, false);
                        {error, Reason} ->
                            {error, {<<"internal_error">>, Reason}}
                    end;
                {error, {<<"security_gate_closed">>, _}} = Gate ->
                    Gate;
                {error, Detail} ->
                    {error, {<<"invalid_request">>, Detail}}
            end;
        {error, Detail} ->
            {error, {<<"invalid_request">>, Detail}}
    end.

-spec reply_page(any(), map(), map(), binary(), [map()], pos_integer(), boolean()) ->
    {ok, map()} | {error, {binary(), term()}}.
reply_page(Conn, Ctx, Filter, Family, Items, PageSize, HasMore) ->
    Next =
        case HasMore of
            false -> null;
            true -> sign_page_cursor(Ctx, Filter, Family, sort_tuple(Family, Items))
        end,
    case Next of
        {error, _} = Err ->
            Err;
        _ ->
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
            end
    end.

%% @doc 下一页 keyset sort_tuple（§10.2 族标签不同 → 键列不同；只含键不含 PII）。
-spec sort_tuple(binary(), [map()]) -> [term()].
sort_tuple(?FAMILY_MAPPINGS, Items) ->
    [maps:get(<<"external_user_id">>, lists:last(Items))];
sort_tuple(?FAMILY_USERS, Items) ->
    [maps:get(<<"user_id">>, lists:last(Items))].

%% @doc page_size 上限判定：缺省 50；非 [1,100] 一律拒绝（不静默截断）。
-spec page_size_opts(map()) -> {ok, pos_integer()} | {error, term()}.
page_size_opts(Opts) ->
    case maps:get(page_size, Opts, undefined) of
        undefined ->
            {ok, ?DEFAULT_PAGE};
        N when is_integer(N), N >= 1, N =< ?MAX_PAGE ->
            {ok, N};
        _ ->
            {error, {page_size_out_of_range, ?MAX_PAGE}}
    end.

%% ===================================================================
%% CURSOR-V2（§10.1）：验签 + 绑定 → keyset pivot；签发下一页游标
%% ===================================================================

%% @doc 游标求值：verify → family/O/App/filter 逐字段绑定 → sort_tuple 形状。
%% 无游标 → {ok, undefined}（首页）。
%% malformed / tampered / foreign-family / foreign 绑定 / expired → invalid；
%% 签名密钥缺失 → security_gate_closed（调用方 503）。
-spec resolve_cursor(undefined | binary(), binary(), map(), map()) ->
    {ok, undefined | binary() | integer()} | {error, term()}.
resolve_cursor(undefined, _Family, _Ctx, _Filter) ->
    {ok, undefined};
resolve_cursor(Cursor, Family, Ctx, Filter) when is_binary(Cursor) ->
    case enterprise_cursor_v2:signing_key() of
        {ok, Key} ->
            case enterprise_cursor_v2:verify(Cursor, Key) of
                {ok, Payload} ->
                    binds_current(Payload, Family, Ctx, Filter);
                {error, _InvalidOrExpired} ->
                    %% 不回显原因（§10.1）；expired 与 invalid 同为 400。
                    %% 旧 unsigned 形态（无 HMAC 段）在此必拒。
                    {error, invalid_cursor}
            end;
        {error, key_unavailable} ->
            {error, {<<"security_gate_closed">>, cursor_signing_key_unavailable}}
    end;
resolve_cursor(_Cursor, _Family, _Ctx, _Filter) ->
    {error, invalid_cursor}.

%% 绑定比对（§10.1 handler 义务）：family / organization_id / application_id /
%% filter（整 map 相等——任何漂移键/漂移值都拒绝，杜绝「旧 filter 游标翻新
%% filter 行集」）。
-spec binds_current(map(), binary(), map(), map()) ->
    {ok, binary() | integer()} | {error, term()}.
binds_current(Payload, Family, Ctx, Filter) ->
    Binds =
        maps:get(<<"family">>, Payload, undefined) =:= Family andalso
            maps:get(<<"organization_id">>, Payload, undefined) =:=
                maps:get(organization_id, Ctx, undefined) andalso
            maps:get(<<"application_id">>, Payload, undefined) =:=
                maps:get(application_id, Ctx, undefined) andalso
            maps:get(<<"filter">>, Payload, undefined) =:= Filter,
    case Binds of
        true ->
            pivot_of(Family, maps:get(<<"sort_tuple">>, Payload, undefined));
        false ->
            {error, invalid_cursor}
    end.

%% sort_tuple 形状按族冻结：mappings=[binary external_user_id]、users=[正整数
%% user_id]；形状不符（含跨族形状）一律拒。
-spec pivot_of(binary(), term()) -> {ok, binary() | integer()} | {error, term()}.
pivot_of(?FAMILY_MAPPINGS, [Key]) when is_binary(Key), byte_size(Key) > 0, byte_size(Key) =< 256 ->
    {ok, Key};
pivot_of(?FAMILY_USERS, [Key]) when is_integer(Key), Key > 0 ->
    {ok, Key};
pivot_of(_Family, _SortTuple) ->
    {error, invalid_cursor}.

%% @doc 下一页游标签发（build_payload 规范形态：v/family/O/App/filter/
%% sort_tuple/issued_at）。密钥缺失 → 503 security_gate_closed（has_more 页
%% 不得伪装成末页）；payload 不可规范化 → internal_error。
-spec sign_page_cursor(map(), map(), binary(), [term()]) ->
    binary() | {error, {binary(), term()}}.
sign_page_cursor(Ctx, Filter, Family, SortTuple) ->
    case enterprise_cursor_v2:signing_key() of
        {ok, Key} ->
            Payload = enterprise_cursor_v2:build_payload(
                Family,
                maps:get(organization_id, Ctx, undefined),
                maps:get(application_id, Ctx, undefined),
                Filter,
                SortTuple,
                os:system_time(second)
            ),
            case enterprise_cursor_v2:sign(Payload, Key) of
                {ok, Cursor} when is_binary(Cursor) ->
                    Cursor;
                {error, Reason} ->
                    {error, {<<"internal_error">>, Reason}}
            end;
        {error, key_unavailable} ->
            {error, {<<"security_gate_closed">>, cursor_signing_key_unavailable}}
    end.

%% ===================================================================
%% 参数归一化
%% ===================================================================

%% 游标 filter 的冻结表示：无 workspace 过滤 → #{}；有 → 整数值绑定
%% （换 workspace 的旧游标一律拒）。
-spec ws_filter(undefined | integer()) -> map().
ws_filter(undefined) ->
    #{};
ws_filter(WsId) when is_integer(WsId) ->
    #{<<"workspace_id">> => WsId}.

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
