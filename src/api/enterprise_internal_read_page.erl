-module(enterprise_internal_read_page).

%%%
% enterprise_internal_read_page 是 V2.1 新增资源只读面（INT-24/26/27/28/30
% 五个 list family）共用的 **CURSOR-V2 handler/logic 侧骨架**（plan §10.1
% 冻结合同；签名/验签本体在 src/lib/enterprise_cursor_v2.erl，本模块不重复
% 实现编码与 HMAC）。
%
% 职责（§10.1 的 handler 义务收敛为三个原语）：
%   * parse_limit/1 —— query `limit`：缺省 50；非整数 / <1 / >100 一律
%     {error, invalid_request}（越界**拒绝不截断**，冻结值）；
%   * resolve/4 —— cursor 入参的验签 + family/O(App ctx)/App/filter 的
%     **逐字段绑定比对**（foreign family/O/App/filter → invalid_request，
%     不得自动改写为当前请求；malformed/tampered/expired 同样
%     invalid_request，不回显原因）；签名密钥缺失 → security_gate_closed。
%     返回 keyset pivot {CreatedAtBin, Id}（RFC3339 + TSID tie-breaker）；
%   * encode/4 —— 下一页游标签发（用当前页末行的 sort_tuple）。
%
% sort_tuple 冻结形态：[CreatedAt（RFC3339 binary）, Id（integer）]——
% timestamptz 微秒精度无损往返（epgsql rfc3339 codec 双向支持），且
% canonical_json 天然可编码（binary+integer，无浮点）。
%
% 分层：本模块是 api 层的内部 helper（handler 参数解析 + logic 游标求值
% 共用）；不触 SQL、不触投影。
%%%

-export([
    parse_limit/1,
    parse_cursor/1,
    resolve/4,
    encode/4,
    default_limit/0,
    max_limit/0
]).

-define(DEFAULT_LIMIT, 50).
-define(MAX_LIMIT, 100).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc query 参数 limit（§10.1）：缺省 50；值必须是无符号整数字符串且
%% 1..100，越界/非整数一律拒绝（不截断、不 clamp）。
-spec parse_limit(map()) -> {ok, pos_integer()} | {error, invalid_request}.
parse_limit(Qs) when is_map(Qs) ->
    case maps:get(<<"limit">>, Qs, undefined) of
        undefined ->
            {ok, ?DEFAULT_LIMIT};
        Bin when is_binary(Bin) ->
            case to_pos_integer(Bin) of
                N when N >= 1, N =< ?MAX_LIMIT -> {ok, N};
                _ -> {error, invalid_request}
            end;
        _ ->
            {error, invalid_request}
    end.

%% @doc query 参数 cursor 原样提取（无值形态 true 也视为提供——由 verify
%% 统一拒为 invalid）。
-spec parse_cursor(map()) -> undefined | binary().
parse_cursor(Qs) when is_map(Qs) ->
    case maps:get(<<"cursor">>, Qs, undefined) of
        Bin when is_binary(Bin), Bin =/= <<>> -> Bin;
        _ -> undefined
    end.

%% @doc cursor 求值：验签 + 结构 + 绑定比对 → keyset pivot。
%% Filter 是该请求的冻结过滤 map（如 #{workspace_id => W, status => <<"all">>}
%% / #{group_id => G} / #{}）；cursor payload 的 filter 必须与当前请求
%% **整 map 相等**（逐字段绑定的最严形态——任何漂移键/漂移值都拒绝，
%% 不做局部匹配，杜绝「旧 filter 游标翻新 filter 行集」）。
%% 返回：
%%   {ok, undefined}                     —— 首页（无 cursor）
%%   {ok, {CreatedAtBin, Id}}            —— 续页 pivot（sort_tuple）
%%   {error, invalid_request}            —— malformed/tampered/expired/foreign
%%   {error, security_gate_closed}       —— 签名密钥缺失/非法（503）
-spec resolve(undefined | binary(), map(), binary(), map()) ->
    {ok, undefined | {binary(), integer()}}
    | {error, invalid_request | security_gate_closed}.
resolve(undefined, _Ctx, _Family, _Filter) ->
    {ok, undefined};
resolve(Cursor, Ctx, Family, Filter) when
    is_binary(Cursor), is_binary(Family), is_map(Filter)
->
    case enterprise_cursor_v2:signing_key() of
        {ok, Secret} ->
            case enterprise_cursor_v2:verify(Cursor, Secret) of
                {ok, Payload} ->
                    binds_current(Payload, Ctx, Family, Filter);
                {error, _InvalidOrExpired} ->
                    %% 不回显原因（§10.1）；expired 与 invalid 同为 400。
                    {error, invalid_request}
            end;
        {error, key_unavailable} ->
            {error, security_gate_closed}
    end;
resolve(_Cursor, _Ctx, _Family, _Filter) ->
    {error, invalid_request}.

%% @doc 下一页游标签发：Tuple 是当前页末行的 {CreatedAt, Id}。
%% Filter 与 resolve/4 同源（当前请求的冻结过滤 map）。
-spec encode(map(), binary(), map(), {binary(), integer()}) ->
    {ok, binary()} | {error, security_gate_closed | term()}.
encode(Ctx, Family, Filter, {CreatedAt, Id}) when
    is_binary(Family), is_map(Filter), is_binary(CreatedAt), is_integer(Id)
->
    case enterprise_cursor_v2:signing_key() of
        {ok, Secret} ->
            Payload = enterprise_cursor_v2:build_payload(
                Family,
                maps:get(organization_id, Ctx),
                maps:get(application_id, Ctx),
                Filter,
                [CreatedAt, Id],
                os:system_time(second)
            ),
            enterprise_cursor_v2:sign(Payload, Secret);
        {error, key_unavailable} ->
            {error, security_gate_closed}
    end;
encode(_Ctx, _Family, _Filter, _Tuple) ->
    {error, invalid_sort_tuple}.

-spec default_limit() -> pos_integer().
default_limit() ->
    ?DEFAULT_LIMIT.

-spec max_limit() -> pos_integer().
max_limit() ->
    ?MAX_LIMIT.

%%%===================================================================
%%% Internal
%%%===================================================================

%% 绑定比对（§10.1 handler 义务）：family / organization_id /
%% application_id / filter 逐字段与当前请求上下文相等；sort_tuple 形态
%% 必须是 [RFC3339 binary, integer]（供 repo keyset 谓词回传）。
-spec binds_current(map(), map(), binary(), map()) ->
    {ok, {binary(), integer()}} | {error, invalid_request}.
binds_current(Payload, Ctx, Family, Filter) ->
    SortTuple = maps:get(<<"sort_tuple">>, Payload, undefined),
    Binds =
        maps:get(<<"family">>, Payload, undefined) =:= Family andalso
            maps:get(<<"organization_id">>, Payload, undefined) =:=
                maps:get(organization_id, Ctx, undefined) andalso
            maps:get(<<"application_id">>, Payload, undefined) =:=
                maps:get(application_id, Ctx, undefined) andalso
            maps:get(<<"filter">>, Payload, undefined) =:= Filter,
    case Binds of
        true ->
            pivot_of(SortTuple);
        false ->
            {error, invalid_request}
    end.

-spec pivot_of(term()) -> {ok, {binary(), integer()}} | {error, invalid_request}.
pivot_of([CreatedAt, Id]) when is_binary(CreatedAt), is_integer(Id) ->
    {ok, {CreatedAt, Id}};
pivot_of(_) ->
    {error, invalid_request}.

-spec to_pos_integer(binary()) -> pos_integer() | error.
to_pos_integer(Bin) ->
    case is_all_digits(Bin) of
        true ->
            try
                binary_to_integer(Bin)
            catch
                _:_ -> error
            end;
        false ->
            error
    end.

-spec is_all_digits(binary()) -> boolean().
is_all_digits(<<>>) ->
    false;
is_all_digits(Bin) ->
    lists:all(fun(C) -> C >= $0 andalso C =< $9 end, binary_to_list(Bin)).
