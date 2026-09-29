%%% @doc 客服 application 共用的编排辅助（租户门 / 端口注入 / 事件追加）。
%%%
%%% 只做机械动作，不含业务规则：
%%%   * `tenant/2`：OrgId / workspace_id 形状门（不触库）；
%%%   * `with_store/2` / `new_id/2`：端口解析——`Params` 里同键（`store` / `id`）
%%%     可注入覆盖（测试用），未给则用 `cs_infra_ports` 装配默认；
%%%   * `append_event/3`：客服域 append-only 审计；失败显式返回
%%%     `{error, {audit_append_failed, _}}`（审计丢失不得静默）；
%%%   * `page_cursor/1` / `page_view/4`（C1~C4 contracts-w2）：列表键集分页的
%%%     机械口径——limit 1..200 缺省 50（越界 `{invalid_limit,_}`）、after_id
%%%     TSID（非法 `{invalid_after_id,_}`）、投影白名单 + `next_after_id`
%%%     （满页 = 本页尾行游标键，否则结束）。
-module(cs_app_support).

-export([
    tenant/2,
    with_store/2,
    with_id/2,
    new_id/2,
    append_event/3,
    store_port/1,
    pos_int/1,
    non_empty_binary/1,
    page_cursor/1,
    page_view/5,
    session_status/1,
    default_page_limit/0,
    max_page_limit/0,
    %% CS-BE-03：坐席面读模型的公共事实推导（掩码名 / 会话来源）——
    %% 坐席队列行与会话上下文共用同一语义，唯一实现点在本模块。
    masked_name/1,
    source_of/1
]).

-define(DEFAULT_PAGE_LIMIT, 50).
-define(MAX_PAGE_LIMIT, 200).

%% @doc 列表分页缺省 limit（C1~C4 冻结口径）。
-spec default_page_limit() -> pos_integer().
default_page_limit() -> ?DEFAULT_PAGE_LIMIT.

%% @doc 列表分页 limit 上界（越界即 `{error, {invalid_limit, _}}`）。
-spec max_page_limit() -> pos_integer().
max_page_limit() -> ?MAX_PAGE_LIMIT.

%% @doc 解析 C1~C4 冻结口径的 after_id / limit 查询参数。
%%
%% `after_id` 缺省 0（首页）；binary 十进制 TSID 或正整数；其余
%% `{error, {invalid_after_id, V}}`。`limit` 缺省 50；1..200；越界/非整数
%% `{error, {invalid_limit, V}}`。
-spec page_cursor(map()) ->
    {ok, AfterId :: non_neg_integer(), Limit :: pos_integer()}
    | {error, term()}.
page_cursor(Params) ->
    case parse_limit(maps:get(limit, Params, undefined)) of
        {error, _} = Err ->
            Err;
        {ok, Limit} ->
            case parse_after_id(maps:get(after_id, Params, undefined)) of
                {error, _} = Err2 -> Err2;
                {ok, AfterId} -> {ok, AfterId, Limit}
            end
    end.

parse_limit(undefined) ->
    {ok, ?DEFAULT_PAGE_LIMIT};
parse_limit(V) when is_integer(V), V >= 1, V =< ?MAX_PAGE_LIMIT ->
    {ok, V};
parse_limit(V) when is_integer(V) ->
    {error, {invalid_limit, V}};
parse_limit(V) when is_binary(V) ->
    try binary_to_integer(V) of
        %% 越界时错误值保留**原始入参**（binary），不换成形整数——对外只看
        %% {invalid_limit, _} 原子，但测试/日志里的取值应可对应回请求原文。
        N when N >= 1, N =< ?MAX_PAGE_LIMIT -> {ok, N};
        _Other -> {error, {invalid_limit, V}}
    catch
        _:_ -> {error, {invalid_limit, V}}
    end;
parse_limit(V) ->
    {error, {invalid_limit, V}}.

parse_after_id(undefined) ->
    {ok, 0};
parse_after_id(V) when is_integer(V), V >= 0 ->
    {ok, V};
parse_after_id(V) when is_binary(V) ->
    case elib_tsid:from_binary(V) of
        {ok, N} -> {ok, N};
        error -> {error, {invalid_after_id, V}}
    end;
parse_after_id(V) ->
    {error, {invalid_after_id, V}}.

%% @doc C1 平台 session 列表的 status 白名单（queued|active|closed；缺省不过滤）。
%% 归一为 binary（SQL text 参数形态）；非法值 `{error, {invalid_status, V}}`。
-spec session_status(term()) -> {ok, binary() | undefined} | {error, term()}.
session_status(undefined) ->
    {ok, undefined};
session_status(<<"queued">>) ->
    {ok, <<"queued">>};
session_status(<<"active">>) ->
    {ok, <<"active">>};
session_status(<<"closed">>) ->
    {ok, <<"closed">>};
session_status(Atom) when Atom =:= queued orelse Atom =:= active orelse Atom =:= closed ->
    {ok, atom_to_binary(Atom, utf8)};
session_status(Other) ->
    {error, {invalid_status, Other}}.

%% @doc 列表视图组装（防泄漏的唯一出口）：按白名单投影逐行裁剪，满页时
%% `next_after_id` = 本页最后一行的游标键（DESC 页即最小 id，ASC 页即最大
%% 游标键），不足一页为 `undefined`（出站经 cs_http:encode_entity 编为 null）。
%% 返回 `{ok, #{ListKey => [投影行], next_after_id => Cursor | undefined}}`。
-spec page_view(atom(), [atom()], [map()], pos_integer(), atom()) ->
    {ok, #{atom() => term(), next_after_id => term()}}.
page_view(ListKey, Projection, Rows, Limit, CursorKey) ->
    Projected = [project(Projection, Row) || Row <- Rows],
    Next =
        case length(Rows) =:= Limit andalso Rows =/= [] of
            true -> maps:get(CursorKey, lists:last(Rows), undefined);
            false -> undefined
        end,
    {ok, #{ListKey => Projected, next_after_id => Next}}.

project(Projection, Row) ->
    maps:with(Projection, Row).

%% @doc 租户门：OrgId / workspace_id 必须都是正整数。
-spec tenant(term(), map()) -> {ok, integer()} | {error, term()}.
tenant(OrgId, Params) when is_map(Params) ->
    case pos_int(OrgId) of
        false ->
            {error, {invalid_organization_id, OrgId}};
        true ->
            case maps:get(workspace_id, Params, undefined) of
                Ws when is_integer(Ws), Ws > 0 -> {ok, Ws};
                Other -> {error, {invalid_workspace_id, Other}}
            end
    end;
tenant(OrgId, _Params) ->
    {error, {invalid_organization_id, OrgId}}.

%% @doc 解析 store 端口并执行；`Params` 里 `store` 键可注入覆盖。
-spec with_store(map(), fun((module()) -> T)) -> T | {error, term()}.
with_store(Params, Fun) ->
    case port(store, Params) of
        {ok, Store} -> Fun(Store);
        {error, _} = Err -> Err
    end.

%% @doc 只解析 store 端口模块（REVIEW-3 F-2：append_message 的 canonical
%% 事务钩子要在闭包内持有同一 store——注入面与 `with_store/2` 逐字同口径）。
-spec store_port(map()) -> {ok, module()} | {error, term()}.
store_port(Params) ->
    port(store, Params).

%% @doc 解析 id 端口并执行；`Params` 里 `id` 键可注入覆盖。
-spec with_id(map(), fun((module()) -> T)) -> T | {error, term()}.
with_id(Params, Fun) ->
    case port(id, Params) of
        {ok, IdPort} -> Fun(IdPort);
        {error, _} = Err -> Err
    end.

%% @doc 生成一个 TSID（Kind 分域）；失败显式报错，不静默兜底。
-spec new_id(atom(), map()) -> {ok, integer()} | {error, term()}.
new_id(Kind, Params) ->
    with_id(Params, fun(IdPort) ->
        try
            {ok, IdPort:new_id(Kind)}
        catch
            Class:Reason -> {error, {id_generation_failed, Kind, {Class, Reason}}}
        end
    end).

%% @doc 追加客服状态审计事件（append-only）。
-spec append_event(map(), integer(), map()) -> ok | {error, term()}.
append_event(Params, OrgId, Event) when is_map(Event) ->
    case with_store(Params, fun(Store) -> Store:append_event(OrgId, Event) end) of
        {ok, _EventId} -> ok;
        {error, Reason} -> {error, {audit_append_failed, Reason}}
    end;
append_event(_Params, _OrgId, _Event) ->
    {error, {invalid_argument, append_event}}.

port(Key, Params) ->
    %% undefined 也是 atom：maps:get 的默认值不得被 is_atom 守卫当作注入模块放行。
    case maps:get(Key, Params, undefined) of
        Mod when is_atom(Mod), Mod =/= undefined -> {ok, Mod};
        _ -> cs_infra_ports:resolve(Key)
    end.

-spec pos_int(term()) -> boolean().
pos_int(V) when is_integer(V), V > 0 -> true;
pos_int(_) -> false.

-spec non_empty_binary(term()) -> boolean().
non_empty_binary(B) when is_binary(B), B =/= <<>> -> true;
non_empty_binary(_) -> false.

%% ===================================================================
%% CS-BE-03：坐席面读模型的公共事实推导（唯一实现点）
%% ===================================================================

%% @doc contact 掩码名（CS-DEC-01 白名单字段；原始 display_name 不出站）：
%% 优先 enterprise 侧既有 subject_mask（本就是掩码），其次 display_name 打码
%% （保留首尾各一字符，中间 ***），二者皆缺 → 稳定匿名柄 `guest#NNNNN`
%% （contact_id 低五位，零 PII）。
-spec masked_name(map()) -> binary().
masked_name(Row) ->
    Mask = maps:get(contact_subject_mask, Row, undefined),
    Name = maps:get(contact_display_name, Row, undefined),
    case {is_binary(Mask), Mask =/= <<>>, Mask =/= undefined} of
        {true, true, true} ->
            Mask;
        _ ->
            case is_binary(Name) andalso Name =/= <<>> of
                true -> mask_display_name(Name);
                false -> default_masked_name(maps:get(contact_id, Row, 0))
            end
    end.

mask_display_name(Name) ->
    Chars = unicode:characters_to_list(Name, utf8),
    Masked =
        case length(Chars) of
            0 -> [];
            1 -> "*";
            2 -> [hd(Chars), $*];
            _ -> [hd(Chars), $*, $*, $*, lists:last(Chars)]
        end,
    unicode:characters_to_binary(Masked, utf8).

default_masked_name(ContactId) when is_integer(ContactId), ContactId > 0 ->
    <<"guest#", (pad5(integer_to_binary(ContactId rem 100000)))/binary>>;
default_masked_name(_ContactId) ->
    <<"guest#00000">>.

pad5(Bin) when byte_size(Bin) >= 5 ->
    Bin;
pad5(Bin) ->
    pad5(<<"0", Bin/binary>>).

%% @doc 会话来源推导（存储派生事实，浏览器不可申报）：
%%   * visit_token_id 非空   → widget（访客经 widget/visit 令牌开会话）；
%%   * created_by_user_id 非空 → seat（坐席在建会话时创建）；
%%   * 其余                   → shop_key（门店接入 POST /sessions/queue）。
-spec source_of(map()) -> binary().
source_of(#{visit_token_id := V}) when V =/= undefined -> <<"widget">>;
source_of(#{created_by_user_id := U}) when U =/= undefined -> <<"seat">>;
source_of(_Row) -> <<"shop_key">>.
