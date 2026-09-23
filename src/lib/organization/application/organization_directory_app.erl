%%% @doc Human Organization Directory 用例层（计划 §14.2 P1 CORE）。
%%%
%%% 4 个只读端点的业务编排：
%%%   list_departments/3 —— 直接 active 子部门（parent_id 缺省=根级）
%%%   list_members/3     —— 本级 active Human（department_id 缺省=根成员）
%%%   my_departments/2   —— /me：当前用户 active 部门（无部门=空数组）
%%%   search/3           —— 部门名/昵称/账号检索（U-02：不搜手机号/邮箱/职位）
%%%
%%% 授权门（单一裁决，先于参数校验与任何目录读）：
%%%   org 不存在                     → {error, <<"resource_not_found">>}      404
%%%   org archived                   → {error, <<"organization_disabled">>}   403
%%%   非成员 / removed / suspended   → {error, <<"insufficient_scope">>}      403
%%%
%%% 游标（CURSOR-V2 算法，Human 域绑定）：
%%%   * payload domain=<<"human_directory">>，绑定 Human uid / Org / endpoint /
%%%     filter；Human 游标与 Internal 游标互不通用（域不符一律 400）。
%%%   * 游标模块经 cursor_mod 注入（默认 enterprise_cursor_v2，A1 交付）；
%%%     单元测试注入 test 目录下的同语义 mock，不依赖 A1 分支。
%%%   * 本层负责 verify 后的 domain/org/user/endpoint/filter/expired 六项
%%%     绑定校验；verify 本身的 malformed/tampered/expired 统一 400
%%%     invalid_request（不回显原因）。
%%%   * 签名密钥缺失/不可用 → 503 security_gate_closed（§10.1）。
%%%
%%% 分页：limit 缺省 50、上界 100；越界 400 invalid_request 不截断；
%%% keyset 排序列（id/user_id 均为 PK 列 NOT NULL）出现异常 NULL 行时丢弃该行
%%% 并记 data-integrity 失败日志，不静默改序（§10.1 null handling）。
%%%
%%% N+1 红线：member_count 内联单 SQL；department_ids 每页一次批量查询；
%%% 全部端点的 SQL 次数与页大小无关（focused 测试以真实计数断言）。
%%%
%%% 错误返回统一为 stable error code 二进制（信封/状态映射见 interfaces 层）。
-module(organization_directory_app).

-export([
    list_departments/3,
    list_members/3,
    my_departments/2,
    search/3
]).

-include("log.hrl").

-define(PG, organization_directory_pg).
-define(DEFAULT_CURSOR_MOD, enterprise_cursor_v2).
-define(DOMAIN, <<"human_directory">>).
-define(CURSOR_TTL_SECONDS, 24 * 3600).
-define(DEFAULT_LIMIT, 50).
-define(MAX_LIMIT, 100).

%% ===================================================================
%% GET /directory/departments
%% ===================================================================

%% @doc 直接 active 子部门。Params（atom 键）：
%%   parent_id  —— undefined（缺省=根级）| 正整数 | 其他形状 → 400
%%   cursor     —— undefined | binary
%%   limit      —— undefined | integer 1..100
%%   cursor_mod —— 游标模块（测试注入）
-spec list_departments(integer(), integer(), map()) ->
    {ok, #{list := [map()], cursor := binary() | null, has_more := boolean()}}
    | {error, binary()}.
list_departments(OrgId, Uid, Params) when is_integer(OrgId), is_integer(Uid), is_map(Params) ->
    Ctx = #{endpoint => <<"departments">>, filter_key => parent_id},
    list_with_gate(OrgId, Uid, Params, Ctx, fun read_departments/4, fun project_department/1);
list_departments(_OrgId, _Uid, _Params) ->
    {error, <<"invalid_request">>}.

%% ===================================================================
%% GET /directory/members
%% ===================================================================

%% @doc 本级 active Human。Params：department_id（undefined=根成员）、cursor、limit。
%% 投影 uid/display_name/avatar/department_ids；user_id ASC keyset。
-spec list_members(integer(), integer(), map()) ->
    {ok, #{list := [map()], cursor := binary() | null, has_more := boolean()}}
    | {error, binary()}.
list_members(OrgId, Uid, Params) when is_integer(OrgId), is_integer(Uid), is_map(Params) ->
    Ctx = #{endpoint => <<"members">>, filter_key => department_id},
    list_with_gate(OrgId, Uid, Params, Ctx, fun read_humans/4, fun project_human/1);
list_members(_OrgId, _Uid, _Params) ->
    {error, <<"invalid_request">>}.

%% ===================================================================
%% GET /directory/me
%% ===================================================================

%% @doc 当前用户在该 Org 的 active 部门；无部门返回空数组（不报错）。
-spec my_departments(integer(), integer()) ->
    {ok, #{organization_id := integer(), list := [map()]}} | {error, binary()}.
my_departments(OrgId, Uid) when is_integer(OrgId), is_integer(Uid) ->
    case authorize(OrgId, Uid) of
        ok ->
            case ?PG:list_my_departments(OrgId, Uid) of
                {ok, Rows} ->
                    Clean = integrity_filter(Rows, <<"departments">>),
                    {ok, #{
                        organization_id => OrgId,
                        list => [project_department(R) || R <- Clean]
                    }};
                {error, Reason} ->
                    directory_db_error(list_my_departments, Reason)
            end;
        {error, Code} ->
            {error, Code}
    end;
my_departments(_OrgId, _Uid) ->
    {error, <<"invalid_request">>}.

%% ===================================================================
%% GET /directory/search
%% ===================================================================

%% @doc 联合检索（kind 0=部门 / 1=成员），(kind, id) ASC keyset。
%% q trim 后 2..64 字符；limit 缺省 50 上界 100，越界 400 不截断。
-spec search(integer(), integer(), map()) ->
    {ok, #{list := [map()], cursor := binary() | null, has_more := boolean()}}
    | {error, binary()}.
search(OrgId, Uid, Params) when is_integer(OrgId), is_integer(Uid), is_map(Params) ->
    case authorize(OrgId, Uid) of
        ok ->
            case normalize_query(Params) of
                {error, Code} ->
                    {error, Code};
                {ok, Q} ->
                    case normalize_limit(Params) of
                        {error, CodeL} ->
                            {error, CodeL};
                        {ok, Limit} ->
                            Ctx = #{endpoint => <<"search">>, filter_key => q},
                            search_page(OrgId, Uid, Params, Ctx, Q, Limit)
                    end
            end;
        {error, Code} ->
            {error, Code}
    end;
search(_OrgId, _Uid, _Params) ->
    {error, <<"invalid_request">>}.

%% ===================================================================
%% 授权门（先于任何目录读与参数校验）
%% ===================================================================

-spec authorize(integer(), integer()) -> ok | {error, binary()}.
authorize(OrgId, Uid) ->
    case ?PG:gate(OrgId, Uid) of
        {ok, #{<<"org_status">> := <<"archived">>}} ->
            {error, <<"organization_disabled">>};
        {ok, #{<<"member_status">> := <<"active">>}} ->
            ok;
        {ok, _Other} ->
            %% 非成员（member_status NULL）或 removed/suspended
            {error, <<"insufficient_scope">>};
        {error, not_found} ->
            {error, <<"resource_not_found">>};
        {error, Reason} ->
            directory_db_error(gate, Reason)
    end.

directory_db_error(Where, Reason) ->
    ?ERROR_LOG([
        organization_directory_db_failure,
        #{where => Where, reason => io_lib:format("~0p", [Reason])}
    ]),
    {error, <<"internal_error">>}.

%% ===================================================================
%% 列表公共编排：gate → 参数/游标 → keyset 查询(limit+1) → 完整性过滤
%%                → 投影（+批量补齐）→ 签发下一页游标
%% ===================================================================

list_with_gate(OrgId, Uid, Params, Ctx, ReadFun, ProjectFun) ->
    case authorize(OrgId, Uid) of
        ok ->
            FilterKey = maps:get(filter_key, Ctx),
            case normalize_filter(Params, FilterKey) of
                {error, Code} ->
                    {error, Code};
                {ok, Filter} ->
                    Ctx2 = Ctx#{filter => Filter},
                    case normalize_limit(Params) of
                        {error, CodeL} ->
                            {error, CodeL};
                        {ok, Limit} ->
                            CursorMod = cursor_mod(Params),
                            CursorBin = maps:get(cursor, Params, undefined),
                            case resolve_after(CursorMod, CursorBin, OrgId, Uid, Ctx2) of
                                {error, CodeC} ->
                                    {error, CodeC};
                                {ok, After} ->
                                    read_page(
                                        OrgId,
                                        Uid,
                                        ReadFun(OrgId, Filter, After, Limit + 1),
                                        Limit,
                                        Ctx2,
                                        CursorMod,
                                        ProjectFun
                                    )
                            end
                    end
            end;
        {error, Code} ->
            {error, Code}
    end.

read_departments(OrgId, ParentId, After, Fetch) ->
    ?PG:list_children_departments(OrgId, ParentId, After, Fetch).

read_humans(OrgId, DeptId, After, Fetch) ->
    case DeptId of
        null -> ?PG:list_root_humans(OrgId, After, Fetch);
        _ -> ?PG:list_department_humans(OrgId, DeptId, After, Fetch)
    end.

read_page(OrgId, Uid, PgResult, Limit, Ctx, CursorMod, ProjectFun) ->
    case PgResult of
        {ok, Rows} ->
            {HasMore, Page0} = split_page(Rows, Limit),
            Page = integrity_filter(Page0, maps:get(endpoint, Ctx)),
            Projected = [ProjectFun(R) || R <- Page],
            case
                {
                    enrich(OrgId, Ctx, Page, Projected),
                    next_cursor(CursorMod, OrgId, Uid, Ctx, Page, HasMore)
                }
            of
                {{error, Code}, _} ->
                    {error, Code};
                {_, {error, CodeS}} ->
                    {error, CodeS};
                {{ok, Final}, NextCursor} ->
                    {ok, #{list => Final, cursor => NextCursor, has_more => HasMore}}
            end;
        {error, Reason} ->
            directory_db_error(list_query, Reason)
    end.

%% 投影后批量补齐（每页至多一次批量 SQL，与页大小无关）：
%%   members → department_ids；departments 无需补齐（member_count 内联）。
enrich(OrgId, #{endpoint := <<"members">>}, Page, Projected) ->
    Uids = [maps:get(<<"user_id">>, R) || R <- Page],
    fill_department_ids(OrgId, Uids, Projected);
enrich(_OrgId, _Ctx, _Page, Projected) ->
    {ok, Projected}.

%% 每页一次批量查询回填 department_ids（禁 N+1：SQL 次数与页大小无关）。
fill_department_ids(_OrgId, [], Projected) ->
    {ok, Projected};
fill_department_ids(OrgId, Uids, Projected) ->
    case ?PG:batch_user_departments(OrgId, Uids) of
        {ok, DeptMap} ->
            Merged =
                [
                    begin
                        Uid = maps:get(user_id, P),
                        P#{department_ids => maps:get(Uid, DeptMap, [])}
                    end
                 || P <- Projected
                ],
            {ok, Merged};
        {error, Reason} ->
            directory_db_error(batch_user_departments, Reason)
    end.

%% 抓 limit+1 判定 has_more；溢出行丢弃。
split_page(Rows, Limit) ->
    case length(Rows) > Limit of
        true ->
            {true, lists:sublist(Rows, Limit)};
        false ->
            {false, Rows}
    end.

%% ===================================================================
%% search 页编排（联合 keyset + 成员 department_ids 批量补齐）
%% ===================================================================

search_page(OrgId, Uid, Params, Ctx, Q, Limit) ->
    CursorMod = cursor_mod(Params),
    CursorBin = maps:get(cursor, Params, undefined),
    case resolve_search_after(CursorMod, CursorBin, OrgId, Uid, Ctx#{filter => Q}) of
        {error, Code} ->
            {error, Code};
        {ok, {AfterKind, AfterId}} ->
            Pattern = like_pattern(Q),
            case ?PG:search_directory(OrgId, Pattern, AfterKind, AfterId, Limit + 1) of
                {ok, Rows} ->
                    {HasMore, Page0} = split_page(Rows, Limit),
                    Page = integrity_filter(Page0, <<"search">>),
                    MemberIds = member_ids(Page),
                    case batch_member_departments(OrgId, MemberIds) of
                        {error, CodeB} ->
                            {error, CodeB};
                        {ok, DeptMap} ->
                            Projected = [project_search_row(R, DeptMap) || R <- Page],
                            case
                                next_cursor(
                                    CursorMod, OrgId, Uid, Ctx#{filter => Q}, Page, HasMore
                                )
                            of
                                {error, CodeS} ->
                                    {error, CodeS};
                                NextCursor ->
                                    {ok, #{
                                        list => Projected,
                                        cursor => NextCursor,
                                        has_more => HasMore
                                    }}
                            end
                    end;
                {error, Reason} ->
                    directory_db_error(search_query, Reason)
            end
    end.

batch_member_departments(_OrgId, []) ->
    {ok, #{}};
batch_member_departments(OrgId, MemberIds) ->
    case ?PG:batch_user_departments(OrgId, MemberIds) of
        {ok, DeptMap} ->
            {ok, DeptMap};
        {error, Reason} ->
            directory_db_error(batch_user_departments, Reason)
    end.

member_ids(Rows) ->
    [
        maps:get(<<"user_id">>, R)
     || R <- Rows,
        maps:get(<<"kind">>, R, null) =:= 1,
        is_integer(maps:get(<<"user_id">>, R, null))
    ].

%% ===================================================================
%% 完整性守卫：keyset 排序列（§10.1 null handling）
%% ===================================================================

%% 排序列出现 NULL/非整数：丢弃该行并记 data-integrity 失败日志（不静默改序）。
integrity_filter(Rows, Endpoint) ->
    lists:filter(
        fun(R) ->
            case row_sort_ok(R, Endpoint) of
                ok ->
                    true;
                bad_row ->
                    ?ERROR_LOG([
                        organization_directory_data_integrity,
                        #{
                            endpoint => Endpoint,
                            row => io_lib:format("~0p", [R])
                        }
                    ]),
                    false
            end
        end,
        Rows
    ).

row_sort_ok(Row, <<"search">>) ->
    case
        {
            maps:get(<<"kind">>, Row, null),
            maps:get(<<"id">>, Row, null)
        }
    of
        {K, Id} when is_integer(K), is_integer(Id) -> ok;
        _ -> bad_row
    end;
row_sort_ok(Row, <<"members">>) ->
    case maps:get(<<"user_id">>, Row, null) of
        Id when is_integer(Id) -> ok;
        _ -> bad_row
    end;
row_sort_ok(Row, _) ->
    case maps:get(<<"id">>, Row, null) of
        Id when is_integer(Id) -> ok;
        _ -> bad_row
    end.

%% ===================================================================
%% 投影（最小化白名单；职位 UNKNOWN_DATA_SOURCE，本期不出字段——U-02）
%% ===================================================================

%% department：id / name / parent_id / member_count
project_department(Row) ->
    #{
        id => maps:get(<<"id">>, Row),
        name => maps:get(<<"name">>, Row),
        parent_id => maps:get(<<"parent_id">>, Row),
        member_count => maps:get(<<"member_count">>, Row, 0)
    }.

%% member：uid / display_name / avatar / department_ids（随后批量补齐）
project_human(Row) ->
    #{
        user_id => maps:get(<<"user_id">>, Row),
        display_name => display_name(Row),
        avatar => maps:get(<<"avatar">>, Row, <<>>),
        department_ids => []
    }.

%% search 行：按 kind 分型投影，成员补 department_ids。
project_search_row(Row, DeptMap) ->
    case maps:get(<<"kind">>, Row) of
        0 ->
            (project_department(Row))#{type => department};
        1 ->
            #{
                type => member,
                user_id => maps:get(<<"user_id">>, Row),
                display_name => display_name(Row),
                avatar => maps:get(<<"avatar">>, Row, <<>>),
                department_ids =>
                    maps:get(maps:get(<<"user_id">>, Row), DeptMap, [])
            }
    end.

%% display_name = nickname，空串回落 account（user.nickname NOT NULL DEFAULT ''）。
display_name(Row) ->
    Nickname = maps:get(<<"nickname">>, Row, <<>>),
    case Nickname of
        <<>> -> maps:get(<<"account">>, Row, <<>>);
        _ -> Nickname
    end.

%% ===================================================================
%% 参数归一化
%% ===================================================================

%% parent_id / department_id 过滤值：缺省/空串→null（根级/根成员）；正整数
%% 原样；其他形状（负数、非整数、垃圾值——含会被 ec_cnv 静默取整的
%% "1.5" 类输入）→ 400 invalid_request，不静默纠正。
normalize_filter(Params, FilterKey) ->
    case maps:get(FilterKey, Params, undefined) of
        undefined ->
            {ok, null};
        <<>> ->
            {ok, null};
        null ->
            {ok, null};
        V ->
            case strict_positive_integer(V) of
                {ok, N} -> {ok, N};
                error -> {error, <<"invalid_request">>}
            end
    end.

%% limit：缺省 50；1..100 之外（含非整数）400 invalid_request，不截断。
normalize_limit(Params) ->
    case maps:get(limit, Params, undefined) of
        undefined ->
            {ok, ?DEFAULT_LIMIT};
        <<>> ->
            {ok, ?DEFAULT_LIMIT};
        V ->
            case strict_integer(V) of
                {ok, N} when N >= 1, N =< ?MAX_LIMIT -> {ok, N};
                _ -> {error, <<"invalid_request">>}
            end
    end.

%% 严格整数解析：二进制必须是纯十进制数字串（拒绝 "1.5"/"+1"/" 1"/"1e3"），
%% 整数原样，其他形状一律 error——ec_cnv 的容错取整不得渗透进查询参数。
strict_integer(N) when is_integer(N) ->
    {ok, N};
strict_integer(Bin) when is_binary(Bin), byte_size(Bin) > 0 ->
    case is_all_digits(Bin) of
        true -> catch_binary_to_integer(Bin);
        false -> error
    end;
strict_integer(_Other) ->
    error.

strict_positive_integer(V) ->
    case strict_integer(V) of
        {ok, N} when N > 0 -> {ok, N};
        _ -> error
    end.

is_all_digits(<<>>) ->
    true;
is_all_digits(<<C, Rest/binary>>) when C >= $0, C =< $9 ->
    is_all_digits(Rest);
is_all_digits(_) ->
    false.

catch_binary_to_integer(Bin) ->
    try
        {ok, binary_to_integer(Bin)}
    catch
        _:_ -> error
    end.

%% q：trim 后 2..64 字符（按 Unicode codepoint 计）；越界 400。
normalize_query(Params) ->
    case maps:get(q, Params, undefined) of
        Raw when is_binary(Raw) ->
            Q = string:trim(Raw),
            Len = string:length(Q),
            case Len >= 2 andalso Len =< 64 of
                true -> {ok, Q};
                false -> {error, <<"invalid_request">>}
            end;
        _ ->
            {error, <<"invalid_request">>}
    end.

%% LIKE 模式串：转义 \ % _ 后前后包 %（仅字面量语义，无通配注入）。
like_pattern(Q) ->
    Escaped = escape_like(Q, <<>>),
    <<"%", Escaped/binary, "%">>.

escape_like(<<>>, Acc) ->
    Acc;
escape_like(<<C/utf8, Rest/binary>>, Acc) ->
    Ch = <<C/utf8>>,
    Acc2 =
        case C of
            $\\ -> <<Acc/binary, "\\\\", Ch/binary>>;
            $% -> <<Acc/binary, "\\\\", Ch/binary>>;
            $_ -> <<Acc/binary, "\\\\", Ch/binary>>;
            _ -> <<Acc/binary, Ch/binary>>
        end,
    escape_like(Rest, Acc2).

%% ===================================================================
%% 游标：注入模块 + Human 域绑定
%% ===================================================================

cursor_mod(Params) ->
    case maps:get(cursor_mod, Params, ?DEFAULT_CURSOR_MOD) of
        Mod when is_atom(Mod) -> Mod;
        _ -> ?DEFAULT_CURSOR_MOD
    end.

%% 解码请求游标 → keyset 起点；无游标 → 0（departments/members 单列 keyset）。
resolve_after(CursorMod, CursorBin, OrgId, Uid, Ctx) ->
    case normalize_cursor_bin(CursorBin) of
        error ->
            {error, <<"invalid_request">>};
        none ->
            {ok, 0};
        {ok, Cursor} ->
            case decode_cursor(CursorMod, Cursor, OrgId, Uid, Ctx) of
                {error, Code} ->
                    {error, Code};
                {ok, Payload} ->
                    case maps:get(<<"sort_tuple">>, Payload, []) of
                        [After] when is_integer(After), After >= 0 ->
                            {ok, After};
                        _ ->
                            {error, <<"invalid_request">>}
                    end
            end
    end.

%% search 游标：sort_tuple = [Kind, Id] 双列 keyset。
resolve_search_after(CursorMod, CursorBin, OrgId, Uid, Ctx) ->
    case normalize_cursor_bin(CursorBin) of
        error ->
            {error, <<"invalid_request">>};
        none ->
            {ok, {0, 0}};
        {ok, Cursor} ->
            case decode_cursor(CursorMod, Cursor, OrgId, Uid, Ctx) of
                {error, Code} ->
                    {error, Code};
                {ok, Payload} ->
                    case maps:get(<<"sort_tuple">>, Payload, []) of
                        [Kind, Id] when
                            (Kind =:= 0 orelse Kind =:= 1), is_integer(Id), Id >= 0
                        ->
                            {ok, {Kind, Id}};
                        _ ->
                            {error, <<"invalid_request">>}
                    end
            end
    end.

normalize_cursor_bin(undefined) ->
    none;
normalize_cursor_bin(<<>>) ->
    none;
normalize_cursor_bin(Bin) when is_binary(Bin) ->
    {ok, Bin};
normalize_cursor_bin(_) ->
    error.

%% verify + Human 域六项绑定校验。任何失败统一 invalid_request（不回显原因）。
decode_cursor(CursorMod, CursorBin, OrgId, Uid, #{endpoint := Endpoint, filter := Filter}) ->
    case CursorMod:signing_key() of
        {ok, Key} when is_binary(Key) ->
            case CursorMod:verify(CursorBin, Key) of
                {ok, Payload} when is_map(Payload) ->
                    check_binding(Payload, OrgId, Uid, Endpoint, Filter);
                _ ->
                    %% malformed / tampered / expired（游标模块内判）一律 400。
                    {error, <<"invalid_request">>}
            end;
        _ ->
            {error, <<"security_gate_closed">>}
    end.

check_binding(Payload, OrgId, Uid, Endpoint, Filter) ->
    IssuedAt = maps:get(<<"issued_at">>, Payload, 0),
    Fresh =
        is_integer(IssuedAt) andalso
            IssuedAt > 0 andalso
            os:system_time(second) - IssuedAt < ?CURSOR_TTL_SECONDS,
    case
        {
            maps:get(<<"v">>, Payload, undefined),
            maps:get(<<"domain">>, Payload, undefined),
            maps:get(<<"organization_id">>, Payload, undefined),
            maps:get(<<"user_id">>, Payload, undefined),
            maps:get(<<"endpoint">>, Payload, undefined),
            maps:get(<<"filter">>, Payload, undefined),
            Fresh
        }
    of
        {2, ?DOMAIN, OrgId, Uid, Endpoint, Filter, true} ->
            {ok, Payload};
        _ ->
            %% 跨域（Internal↔Human）/跨 Org/跨用户/跨端点/跨过滤/过期：全部拒。
            {error, <<"invalid_request">>}
    end.

%% 末页之后签发下一页游标；末页 cursor=null（客户端 endReached 依据）。
%% 整页被完整性守卫清空时不签发（无 keyset 位置可锚定）。
next_cursor(_CursorMod, _OrgId, _Uid, _Ctx, [], _HasMore) ->
    null;
next_cursor(_CursorMod, _OrgId, _Uid, _Ctx, _Page, false) ->
    null;
next_cursor(CursorMod, OrgId, Uid, Ctx, Page, true) ->
    Last = lists:last(Page),
    SortTuple =
        case maps:get(endpoint, Ctx) of
            <<"search">> ->
                [maps:get(<<"kind">>, Last), maps:get(<<"id">>, Last)];
            <<"members">> ->
                [maps:get(<<"user_id">>, Last)];
            _ ->
                [maps:get(<<"id">>, Last)]
        end,
    sign_page_cursor(CursorMod, OrgId, Uid, Ctx, SortTuple).

sign_page_cursor(CursorMod, OrgId, Uid, #{endpoint := Endpoint, filter := Filter}, SortTuple) ->
    Payload =
        #{
            <<"v">> => 2,
            <<"domain">> => ?DOMAIN,
            <<"organization_id">> => OrgId,
            <<"user_id">> => Uid,
            <<"endpoint">> => Endpoint,
            <<"filter">> => Filter,
            <<"sort_tuple">> => SortTuple,
            <<"issued_at">> => os:system_time(second)
        },
    case CursorMod:signing_key() of
        {ok, Key} when is_binary(Key) ->
            case CursorMod:sign(Payload, Key) of
                {ok, Cursor} when is_binary(Cursor) ->
                    Cursor;
                _Other ->
                    %% 语义上 has_more 却无法签发下一页：不得伪装成末页
                    %%（客户端会误判 endReached 丢数据）——fail-closed 503。
                    {error, <<"security_gate_closed">>}
            end;
        _ ->
            %% 密钥缺失/不可读（§10.1）：503 security_gate_closed，不降级。
            {error, <<"security_gate_closed">>}
    end.
