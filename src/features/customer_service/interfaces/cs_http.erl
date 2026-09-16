%%% @doc 客服 HTTP 协议适配（CS-02）：解析 → 验证 → 认证 → 调用 → 映射，**零业务规则**。
%%%
%%% 依据：plan v4.1 §5.2/§5.3、CS-02-A01..A05、`docs/architecture/feature-slice-rules.md`
%%% 铁律 2/5。与 `eb_enterprise_http` 同职责但不跨单元复用其模块（铁律 5：跨 Feature
%%% 只准引用 facade）——本模块只依赖 core（elib_response/elib_cnv/elib_tsid）、
%%% 本 feature 的 cs_actions/cs_auth/cs_facade_call，以及两个 facade。
%%%
%%% 固定纪律：
%%%
%%%   * TSID 以 JSON string 传输，投影成 integer 交给 application，出站编回 string；
%%%   * 服务端派生键（动作表 `client_forbidden`：操作人/时钟/坐席与访客身份）客户端
%%%     **提供即 400**；
%%%   * **必填键前置结构化校验**（CS-01 审查观察项：application 对 `client_msg_id`/
%%%     `key_ref` 用无默认 maps:get）——缺失/类型错在 handler 返回 4xx，绝不把
%%%     badarg 泄漏成 500；
%%%   * 错误映射默认 **500 fail-closed**；offboarding 降级 = **HTTP 409 + envelope
%%%     `offboarding_required`**（A0 客户端契约基准，双通道语义）；
%%%   * credential 面（visit/shop key 凭证路径）由 `is_credential_surface_path/1`
%%%     声明，`auth_middleware_api_v1` 据此免 JWT/签名直通——handler 侧照常
%%%     fail-closed 校验凭证。
%%%
%%% **本模块不做**：不读库、不写 SQL、不引 `elib_pg`/`cs_pg_*`/`eb_pg_*`、
%%% 不做业务判定、不签发任何 URL。
-module(cs_http).

-export([
    read_body/1,
    org_id/3,
    workspace_id/2,
    path_params/2,
    build_params/5,
    respond/3,
    reply_error/2,
    status/1,
    tag/1,
    encode_entity/1,
    is_tsid_key/1,
    is_credential_surface_path/1,
    now_ms/0
]).

-include("error_code.hrl").

%% TSID 在 JSON 里的载体是 string；版本/评分/时间戳不是 TSID，保持 number。
-define(TSID_KEY_SUFFIX, <<"_id">>).
%% 客户端**不得**提供的 Workspace 归属之外的派生归属键。
-define(FORBIDDEN_SCOPE_KEYS, [workspace_organization_id]).
%% 五类身份的凭证传输头（与 cs_auth 的头常量同值；中间件免签/免 JWT 面判定用）。
-define(VISIT_HEADER, <<"x-cs-visit-token">>).
-define(SHOP_KEY_HEADER, <<"x-cs-shop-key">>).

%% ===================================================================
%% credential 面（auth_middleware_api_v1 精确行经本函数判定；F-EB10-1 同款
%% -ifdef 保护在中间件侧。判据是**冻结路径形状**，不是 URL 猜测：与动作表的
%% principal 声明由 cs_route_contract_tests 双向核对）。
%% ===================================================================

%% @doc 访客/门店凭证面路径：这些路径不走 JWT/设备签名（访客没有 IMBoy 设备），
%% 凭证在专用头里由 handler fail-closed 校验。其余 /api/v1/cs/* 一律照常走
%% 中间件签名 + JWT 门。
-spec is_credential_surface_path(binary()) -> boolean().
is_credential_surface_path(Path) when is_binary(Path) ->
    case segments(Path) of
        %% GET /api/v1/cs/sessions（访客列自己的会话）
        [<<"api">>, <<"v1">>, <<"cs">>, <<"sessions">>] ->
            true;
        %% POST /api/v1/cs/sessions/queue（门店开会话）
        [<<"api">>, <<"v1">>, <<"cs">>, <<"sessions">>, <<"queue">>] ->
            true;
        %% POST /api/v1/cs/sessions/:id/messages | /rating（访客消息/评分）
        [<<"api">>, <<"v1">>, <<"cs">>, <<"sessions">>, _Id, Last] when
            Last =:= <<"messages">>; Last =:= <<"rating">>
        ->
            true;
        _ ->
            false
    end;
is_credential_surface_path(_Path) ->
    false.

segments(Path) ->
    [S || S <- binary:split(Path, <<"/">>, [global]), S =/= <<>>].

%% ===================================================================
%% 路径 / 查询 / 正文
%% ===================================================================

%% @doc 读取并规范化请求正文（JSX binary 键 → 动作表白名单 atom；空正文 = #{}）。
-spec read_body(cowboy_req:req()) -> {ok, map()} | {error, term()}.
read_body(Req) ->
    case cowboy_req:has_body(Req) of
        false ->
            {ok, #{}};
        true ->
            {ok, Raw, _} = cowboy_req:read_body(Req, #{length => 1024 * 1024, period => 5000}),
            decode(Raw)
    end.

decode(<<>>) ->
    {ok, #{}};
decode(Raw) ->
    try jsx:decode(Raw, [return_maps]) of
        Map when is_map(Map) -> {ok, normalize_body(Map)};
        _NotObject -> {error, body_not_object}
    catch
        _:_ -> {error, malformed_json}
    end.

normalize_body(Map) ->
    Known = known_keys(),
    maps:from_list([
        {maps:get(K, Known, K), V}
     || {K, V} <- maps:to_list(Map)
    ]).

%% 动作表全部参数键 + 面级键的 binary→atom 白名单（惰性构建；不做 binary_to_atom）。
known_keys() ->
    case persistent_term:get({?MODULE, known_keys}, undefined) of
        undefined ->
            %% 服务端派生键必须进白名单：它们不是任何动作的参数，但不进
            %% binary→atom 白名单的话，客户端提供时 forbidden 检查看不见。
            Keys =
                param_keys() ++
                    [organization_id, workspace_id, workspace_organization_id] ++
                    server_derived_keys(),
            Known = maps:from_list([{atom_to_binary(K, utf8), K} || K <- lists:usort(Keys)]),
            persistent_term:put({?MODULE, known_keys}, Known),
            Known;
        Known ->
            Known
    end.

param_keys() ->
    Tables =
        [cs_actions:tenant(A) || A <- cs_actions:tenant_actions()] ++
            [cs_actions:platform(A) || A <- cs_actions:platform_actions()],
    lists:usort([
        K
     || {ok, Entry} <- Tables,
        Case <- maps:get(cases, Entry),
        {K, _Type, _Req} <- maps:get(params, Case)
    ]).

%% @doc OrgId 解析（cs_actions:org_source/1 决定来源）：
%%   * `path` —— cowboy 绑定 `org_id`（治理/平台面）；
%%   * `param` —— A0 冻结路径（path 无 org 段）取查询/正文的必填 `organization_id`，
%%     作为**申报值**交由 cs_auth 用凭证/事实证明。
%% @doc OrgId 解析（cs_actions:org_source/1 决定来源）：
%%   * `path` —— cowboy 绑定 `org_id`（治理/平台面）；
%%   * `param` —— A0 冻结路径（path 无 org 段）取查询/正文的必填 `organization_id`，
%%     作为**申报值**交由 cs_auth 用凭证/事实证明。
-spec org_id(map(), cowboy_req:req(), map()) -> {ok, integer()} | {error, term()}.
org_id(Entry, Req, Body) ->
    case cs_actions:org_source(Entry) of
        path ->
            path_tsid(Req, org_id, missing_org_id);
        param ->
            case value(organization_id, Req, Body) of
                undefined ->
                    {error, missing_org_id};
                Raw ->
                    case tsid(Raw) of
                        {ok, Id} -> {ok, Id};
                        error -> {error, invalid_org_id}
                    end
            end
    end.

%% @doc 显式 Workspace 归属：GET 走查询串，写走正文；缺一即 422，不取默认值。
-spec workspace_id(cowboy_req:req(), map()) -> {ok, integer()} | {error, term()}.
workspace_id(Req, Body) ->
    case value(workspace_id, Req, Body) of
        undefined ->
            {error, missing_workspace_id};
        Raw ->
            case tsid(Raw) of
                {ok, Id} -> {ok, Id};
                error -> {error, invalid_workspace_id}
            end
    end.

%% @doc 路径参数（id / conversation_id 等）→ 动作表登记的 facade 键。
-spec path_params(map(), cowboy_req:req()) -> {ok, map()} | {error, term()}.
path_params(Case, Req) ->
    path_params(maps:get(path_params, Case), Req, #{}).

path_params([], _Req, Acc) ->
    {ok, Acc};
path_params([{Binding, Key} | Rest], Req, Acc) ->
    case path_tsid(Req, Binding, {missing_path_param, Key}) of
        {ok, Id} -> path_params(Rest, Req, Acc#{Key => Id});
        {error, _} = Err -> Err
    end.

%% cowboy 的 binding 名必须是 atom（cowboy_req:binding/3 守卫）。
path_tsid(Req, Name, MissingTag) when is_atom(Name) ->
    case cowboy_req:binding(Name, Req) of
        undefined ->
            {error, MissingTag};
        Raw ->
            case tsid(Raw) of
                {ok, Id} -> {ok, Id};
                error -> {error, invalid_tsid}
            end
    end.

%% ===================================================================
%% 参数投影（白名单 + 类型收敛 + 服务端派生键守卫）
%% ===================================================================

%% @doc 按动作表把请求投影成 facade 的 `Params`。
%%
%% `Derived` 是服务端派生键（organization_id / workspace_id / actor_user_id / at /
%% business_identity_id / contact_id / created_by_* 等，来自认证上下文与服务端时钟，
%% 一个都不来自客户端）；动作表 `client_forbidden` 里的键客户端提供即 400。
-spec build_params(
    map(), map(), cowboy_req:req(), map(), map()
) ->
    {ok, map()} | {error, term()}.
build_params(Entry, Case, Req, Body, Derived) ->
    Forbidden = maps:get(client_forbidden, Entry, []),
    case check_forbidden(Body, Forbidden ++ ?FORBIDDEN_SCOPE_KEYS) of
        {error, _} = Err ->
            Err;
        ok ->
            case path_params(Case, Req) of
                {error, _} = Err ->
                    Err;
                {ok, PathParams} ->
                    Base = maps:merge(server_derived(), PathParams),
                    collect(maps:get(params, Case), Req, Body, maps:merge(Base, Derived))
            end
    end.

%% 服务端派生键全集（动作表 client_forbidden 的并集 + 面级归属键）。
server_derived_keys() ->
    Tables =
        [cs_actions:tenant(A) || A <- cs_actions:tenant_actions()] ++
            [cs_actions:platform(A) || A <- cs_actions:platform_actions()],
    lists:usort(lists:append([maps:get(client_forbidden, E, []) || {ok, E} <- Tables])).

server_derived() ->
    #{}.

check_forbidden(Body, Keys) ->
    case [K || K <- Keys, is_map_key(K, Body)] of
        [] -> ok;
        [Key | _] -> {error, {forbidden_client_key, Key}}
    end.

collect([], _Req, _Body, Acc) ->
    {ok, Acc};
collect([{Key, Type, Requiredness} | Rest], Req, Body, Acc) ->
    case coerce(Type, value(Key, Req, Body)) of
        {ok, undefined} when Requiredness =:= optional ->
            collect(Rest, Req, Body, Acc);
        {ok, undefined} ->
            {error, {missing_param, Key}};
        {ok, Value} ->
            collect(Rest, Req, Body, Acc#{Key => Value});
        {error, _} ->
            {error, {invalid_param, Key}}
    end.

%% 正文优先（键已按白名单规范成 atom），其次查询串。
value(Key, Req, Body) ->
    AsBin = key_bin(Key),
    BodyKey = maps:get(AsBin, known_keys(), AsBin),
    case is_map_key(BodyKey, Body) of
        true ->
            maps:get(BodyKey, Body);
        false ->
            proplists:get_value(AsBin, cowboy_req:parse_qs(Req))
    end.

key_bin(K) when is_binary(K) -> K;
key_bin(K) when is_atom(K) -> atom_to_binary(K, utf8).

coerce(_Type, undefined) ->
    {ok, undefined};
%% 空串与缺省同义（游标习惯 `?after_id=`）：optional 静默跳过，required 走
%% missing_param——这不是取值错误。
coerce(_Type, <<>>) ->
    {ok, undefined};
coerce(tsid, Raw) ->
    case tsid(Raw) of
        {ok, V} -> {ok, V};
        error -> {error, invalid_value}
    end;
coerce(int, Raw) when is_integer(Raw) ->
    {ok, Raw};
coerce(int, Raw) when is_binary(Raw) ->
    try binary_to_integer(Raw) of
        Int -> {ok, Int}
    catch
        _:_ -> {error, invalid_value}
    end;
coerce(binary, Raw) when is_binary(Raw), Raw =/= <<>> ->
    {ok, Raw};
coerce(_Type, _Raw) ->
    {error, invalid_value}.

%% @doc 入站 TSID：十进制字符串（A02 传输形态）或 JSON number（兼容），必须 > 0。
-spec tsid(term()) -> {ok, integer()} | error.
tsid(Raw) when is_integer(Raw), Raw > 0 ->
    {ok, Raw};
tsid(Raw) when is_binary(Raw) ->
    elib_tsid:from_binary(Raw);
tsid(_Raw) ->
    error.

%% @doc 服务端时钟（动作表把 `at` 列为服务端派生键；客户端不可报时）。
-spec now_ms() -> integer().
now_ms() ->
    os:system_time(millisecond).

%% ===================================================================
%% 响应映射
%% ===================================================================

-spec respond(map(), cowboy_req:req(), term()) -> cowboy_req:req().
respond(_Entry, Req, {ok, View}) ->
    elib_response:success_rfc3339(Req, encode_entity(View));
%% 无载荷成功（revoke_shop_key/revoke_visit_token 返回裸 ok）——空载荷 200，
%% 缺此子句会 function_clause 500（DEFECT-4 收尾，CS-04 E2E 发现）。
respond(_Entry, Req, ok) ->
    elib_response:success_rfc3339(Req, #{});
respond(_Entry, Req, {error, Reason}) ->
    reply_error(Req, Reason).

-spec reply_error(cowboy_req:req(), term()) -> cowboy_req:req().
reply_error(Req, Reason) ->
    Status = status(Reason),
    elib_response:error_with_status(Req, Status, tag(Reason), Status).

%% ===================================================================
%% 错误 → HTTP 状态（默认 500 fail-closed）
%% ===================================================================

-spec status(term()) -> pos_integer().
status(Reason) ->
    case classify(Reason) of
        Status when is_integer(Status) -> Status;
        unknown -> ?ERR_INTERNAL_SERVER_ERROR
    end.

%% --- 400：请求形状错误 ---
classify({forbidden_client_key, _}) ->
    ?ERR_BAD_REQUEST;
classify({invalid_param, _}) ->
    ?ERR_BAD_REQUEST;
classify(invalid_tsid) ->
    ?ERR_BAD_REQUEST;
classify(invalid_org_id) ->
    ?ERR_BAD_REQUEST;
classify(missing_org_id) ->
    ?ERR_BAD_REQUEST;
classify({missing_path_param, _}) ->
    ?ERR_BAD_REQUEST;
classify(malformed_json) ->
    ?ERR_BAD_REQUEST;
classify(body_not_object) ->
    ?ERR_BAD_REQUEST;
classify(method_not_allowed) ->
    ?ERR_METHOD_NOT_ALLOWED;
%% --- 401：凭证缺失/无效（访客与门店凭证也是凭证）---
classify(credential_missing) ->
    ?ERR_UNAUTHORIZED;
classify({principal_mismatch, _, _}) ->
    ?ERR_UNAUTHORIZED;
classify(route_metadata_missing_auth_context) ->
    ?ERR_UNAUTHORIZED;
classify({unknown_auth_context, _}) ->
    ?ERR_UNAUTHORIZED;
classify({invalid_secret, _}) ->
    ?ERR_UNAUTHORIZED;
classify(credential_invalid) ->
    ?ERR_UNAUTHORIZED;
classify(visit_token_revoked) ->
    ?ERR_UNAUTHORIZED;
classify(visit_token_expired) ->
    ?ERR_UNAUTHORIZED;
classify(token_expired) ->
    ?ERR_UNAUTHORIZED;
classify(revoked) ->
    ?ERR_UNAUTHORIZED;
classify(token_revoked) ->
    ?ERR_UNAUTHORIZED;
classify(shop_key_revoked) ->
    ?ERR_UNAUTHORIZED;
classify({unknown_action, _}) ->
    ?ERR_UNAUTHORIZED;
%% --- 403：授权不足 / 状态性拒绝（含 A04：suspended seat actor 即时拒绝）---
classify(seat_disabled) ->
    ?ERR_FORBIDDEN;
classify({seat_not_found, _}) ->
    ?ERR_FORBIDDEN;
classify({member_not_active, _}) ->
    ?ERR_FORBIDDEN;
classify(member_not_found) ->
    ?ERR_FORBIDDEN;
classify(no_member) ->
    ?ERR_FORBIDDEN;
classify(identity_assignment_missing) ->
    ?ERR_FORBIDDEN;
classify({multiple_active_assignment, _}) ->
    ?ERR_FORBIDDEN;
classify({permission_missing, _}) ->
    ?ERR_FORBIDDEN;
classify({governance_insufficient, _}) ->
    ?ERR_FORBIDDEN;
classify(platform_identity_mismatch) ->
    ?ERR_FORBIDDEN;
classify(cross_org) ->
    ?ERR_FORBIDDEN;
classify(cross_contact) ->
    ?ERR_FORBIDDEN;
classify({function_mismatch, _, _}) ->
    ?ERR_FORBIDDEN;
%% --- 404：资源不在本租户作用域（不区分不存在与跨 Org，避免枚举）---
classify(not_found) ->
    ?ERR_NOT_FOUND;
classify({not_found, _}) ->
    ?ERR_NOT_FOUND;
classify({session_not_found, _}) ->
    ?ERR_NOT_FOUND;
%% --- 409：并发/状态竞争 + offboarding 降级（双通道：状态码 + envelope 标签）---
classify(conflict) ->
    ?ERR_CONFLICT;
classify(duplicate_occupation) ->
    ?ERR_CONFLICT;
classify({stale_version, _}) ->
    ?ERR_CONFLICT;
classify({cas_mismatch, _}) ->
    ?ERR_CONFLICT;
classify({invalid_transition, _, _}) ->
    ?ERR_CONFLICT;
classify({invalid_transition, _}) ->
    ?ERR_CONFLICT;
classify({not_claimable, _}) ->
    ?ERR_CONFLICT;
classify(session_already_closed) ->
    ?ERR_CONFLICT;
classify(already_rated) ->
    ?ERR_CONFLICT;
classify({rating_requires_closed, _}) ->
    ?ERR_CONFLICT;
classify(no_seat_available) ->
    ?ERR_CONFLICT;
classify(seat_at_capacity) ->
    ?ERR_CONFLICT;
classify({assignee_change_requires_offboarding, _}) ->
    ?ERR_CONFLICT;
classify({conversation_exists, _}) ->
    ?ERR_CONFLICT;
%% --- 422：形状合法但取值不成立（缺必填/域值错）---
classify(missing_workspace_id) ->
    ?ERR_UNPROCESSABLE_ENTITY;
classify(invalid_workspace_id) ->
    ?ERR_UNPROCESSABLE_ENTITY;
classify({missing_param, _}) ->
    ?ERR_UNPROCESSABLE_ENTITY;
classify({invalid_param_value, _}) ->
    ?ERR_UNPROCESSABLE_ENTITY;
classify({invalid_argument, _}) ->
    ?ERR_UNPROCESSABLE_ENTITY;
classify({unexpected_argument, _}) ->
    ?ERR_UNPROCESSABLE_ENTITY;
classify({invalid_session_id, _}) ->
    ?ERR_UNPROCESSABLE_ENTITY;
classify({invalid_identity_id, _}) ->
    ?ERR_UNPROCESSABLE_ENTITY;
classify({invalid_contact_id, _}) ->
    ?ERR_UNPROCESSABLE_ENTITY;
classify({invalid_rating, _}) ->
    ?ERR_UNPROCESSABLE_ENTITY;
classify({invalid_organization_id, _}) ->
    ?ERR_UNPROCESSABLE_ENTITY;
classify({not_session_contact, _, _}) ->
    ?ERR_UNPROCESSABLE_ENTITY;
classify({not_session_seat, _, _}) ->
    ?ERR_UNPROCESSABLE_ENTITY;
%% --- 500：服务端自身不可用/配置缺失（绝不伪装成 4xx）---
classify(Reason) ->
    case server_side(Reason) of
        true -> ?ERR_INTERNAL_SERVER_ERROR;
        false -> unknown
    end.

%% 服务端侧失败面（审计失败、ID 生成失败、事实源不可用、端口未装配等）。
server_side({forbidden, Inner}) -> server_side(Inner);
server_side({audit_append_failed, _}) -> true;
server_side({id_generation_failed, _}) -> true;
server_side({id_generation_failed, _, _}) -> true;
server_side({facts_unavailable, _}) -> true;
server_side({member_fact_query_failed, _}) -> true;
server_side(auth_assembly_missing) -> true;
server_side({unimplemented_port, _}) -> true;
server_side({unknown_port, _}) -> true;
server_side({missing_config, _}) -> true;
server_side(clock_unavailable) -> true;
server_side({enterprise_business_feature_not_selected, _}) -> true;
server_side(_Other) -> false.

%% ===================================================================
%% 原因标签（对外可见的稳定文本；丢弃整数与二进制取值）
%% ===================================================================

%% @doc 把错误项编成点分原子路径。**唯一特例**：offboarding 降级
%%% `{assignee_change_requires_offboarding, _}` 对外是客户端契约冻结标签
%%% `offboarding_required`（HTTP 409 + envelope code，双通道语义）。
-spec tag(term()) -> binary().
tag({assignee_change_requires_offboarding, _}) ->
    <<"offboarding_required">>;
tag(Reason) ->
    Parts = path(Reason, []),
    case Parts of
        [] -> <<"internal_error">>;
        _ -> iolist_to_binary(lists:join(<<".">>, lists:reverse(Parts)))
    end.

path(Atom, Acc) when is_atom(Atom) -> [atom_to_binary(Atom, utf8) | Acc];
path({Tag, Rest}, Acc) when is_atom(Tag) -> path(Rest, [atom_to_binary(Tag, utf8) | Acc]);
path({Tag, _, _}, Acc) when is_atom(Tag) -> [atom_to_binary(Tag, utf8) | Acc];
path({Tag, _, _, _}, Acc) when is_atom(Tag) -> [atom_to_binary(Tag, utf8) | Acc];
path(_Other, Acc) -> Acc.

%% ===================================================================
%% 出站编码（TSID → string）
%% ===================================================================

-spec encode_entity(term()) -> term().
encode_entity(Map) when is_map(Map) ->
    maps:from_list([{K, encode_value(K, V)} || {K, V} <- maps:to_list(Map)]);
encode_entity(List) when is_list(List) ->
    [encode_entity(V) || V <- List];
encode_entity(Other) ->
    Other.

encode_value(Key, Value) when is_integer(Value) ->
    case is_tsid_key(Key) of
        true -> integer_to_binary(Value);
        false -> Value
    end;
encode_value(_Key, Value) when is_map(Value) ->
    encode_entity(Value);
encode_value(_Key, Value) when is_list(Value) ->
    encode_entity(Value);
encode_value(_Key, Value) ->
    Value.

%% @doc 该键承载的是 TSID 吗：`id` 或 `*_id`。排除长得像但不是 TSID 的键
%% （device_id/client_msg_id/key_ref 是字符串语义）。
-spec is_tsid_key(term()) -> boolean().
is_tsid_key(Key) when is_atom(Key) ->
    is_tsid_key(atom_to_binary(Key, utf8));
is_tsid_key(Key) when is_binary(Key) ->
    (Key =:= <<"id">> orelse has_tsid_suffix(Key)) andalso
        not lists:member(Key, non_tsid_id_keys());
is_tsid_key(_Key) ->
    false.

has_tsid_suffix(Key) ->
    Size = byte_size(Key),
    case Size > 3 of
        true -> binary:part(Key, Size - 3, 3) =:= ?TSID_KEY_SUFFIX;
        false -> false
    end.

non_tsid_id_keys() ->
    [<<"trace_id">>, <<"device_id">>, <<"client_msg_id">>, <<"key_ref">>].
