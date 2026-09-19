%%% @doc 企业业务 HTTP 协议适配（EB-09）：解析 → 验证 → 映射，**零业务规则**。
%%%
%%% 依据：plan v4.1 §5、EB-09-A02/A03/A05/A06、`docs/architecture/feature-slice-rules.md`
%%% 铁律 2（层名与依赖方向）。
%%%
%%% 本模块与两个 handler 一起构成 `interfaces/` 层：它只做**协议**工作——
%%%
%%%   * 路径/查询/正文的取值与类型收敛（TSID 以 **JSON string** 传输，投影成
%%%     integer 交给 application；出站再编回 string，见 `encode_entity/1`，A02）；
%%%   * 授权入口收敛（**唯一**调用 `eb_auth_app:authorize/2`，principal 由 route
%%%     metadata 决定，绝不按 URL 猜，A01）；
%%%   * 结果映射（`{ok, _}` → 200 / 流式 200；`{error, Reason}` → 400/401/403/404/
%%%     409/422/500，响应只回**稳定原因标签**、不回内部值，A03/A05）；
%%%   * 存储能力泄露判据 `storage_leak_scan/1`（生产侧与测试侧共用，A05）。
%%%
%%% **本模块不做**：不读库、不写 SQL、不引 `elib_pg`/`eb_pg_*`（A05 的静态判据）、
%%% 不签发任何 URL、不接触对象存储（asset content 只把 facade 返回的字节原样流式
%%% 写出，响应里不含 object key / storage endpoint / presigned）。
-module(eb_enterprise_http).

-export([
    authorize/4,
    org_id/1,
    tsid/1,
    workspace_id/2,
    read_body/1,
    path_params/2,
    build_params/5,
    respond/3,
    reply_error/2,
    reply_content/2,
    status/1,
    tag/1,
    encode_entity/1,
    is_tsid_key/1,
    storage_leak_scan/1
]).

%% TSID 在 JSON 里的载体是 **string**（A02）；版本/天数/字节数/时间戳不是 TSID，
%% 保持 number。
-define(TSID_KEY_SUFFIX, <<"_id">>).
%% 客户端**不得**提供的服务端派生键：租户归属 / Workspace 归属。
-define(FORBIDDEN_TENANT_KEYS, [organization_id, workspace_organization_id]).
%% 租户面额外禁止客户端自报**操作人**（actor 取自认证主体，见 `eb_tenant_handler`）。
-define(FORBIDDEN_TENANT_ACTOR_KEY, actor_user_id).

-include("error_code.hrl").

%% ===================================================================
%% 授权（唯一入口）
%% ===================================================================

%% @doc 按 route metadata 判定一次企业请求。
%%
%% `State` 来自 cowboy route Opts：含路由登记的 `auth_context` / `surface` /
%% `required_*`、中间件注入的会话键（`current_uid` / `adm_user_id`）、装配键
%% `auth_facts`，以及 handler 解析出的 `organization_id`。
%%
%% 缺装配（无 `auth_facts`）一律 fail-closed（500，不降级为「不要求企业授权」）。
-spec authorize(
    eb_enterprise_actions:entry(), eb_enterprise_actions:kase(), cowboy_req:req(), map()
) ->
    {ok, map()} | {error, term()}.
%% F-SEC-02：Case（已匹配 method 的用例）可声明逐用例权限覆盖——路径级
%% required_permission 只描述最宽方法（如 GET 列表的 read），写动作在 case
%% 上收紧为 write。覆盖发生在进入 eb_auth_app 之前，判定口径不变。
authorize(Entry, Case, Req, State) ->
    case {metadata(State, Req), facts_module(State), credential(Entry, State)} of
        {{error, _} = Err, _, _} ->
            Err;
        {_, {error, _} = Err, _} ->
            Err;
        {_, _, {error, _} = Err} ->
            Err;
        {{ok, RouteMetadata}, {ok, FactsModule}, {ok, Credential}} ->
            FactsRequest = facts_request(Case, Req, State),
            Request = #{
                organization_id => maps:get(organization_id, State, undefined),
                user_id => credential_user_id(Credential),
                credential => Credential,
                facts => {load, fun() -> FactsModule:load_request_facts(FactsRequest) end}
            },
            eb_auth_app:authorize(case_requirement(RouteMetadata, Case), Request)
    end.

%% ===================================================================
%% A05 分层门修正（EB-01 / A01.36 方案 a 实施记录）：接口层禁止直连
%% facade，资源归属 hint（会话经办 business_identity_id）改道授权事实
%% 通道——http 层只把请求路径里的会话 id 投进 FactsRequest，由 facts
%% 实现按需加载（见 eb_pg_auth_facts:load_request_facts/1）。
%% ===================================================================

%% 只读事实源收到的请求形状（租户类 / 平台类两类事实源共同的最小键集）。
%% 请求路径携带 `conversation_id` 绑定时一并投影：它承担「身份归属确定性
%% hint」的取数锚点（授权歧义消歧用；取不到 → facts 无该键 → 维持歧义拒绝）。
facts_request(Case, Req, State) ->
    #{
        organization_id => maps:get(organization_id, State, undefined),
        user_id => maps:get(current_uid, State, 0),
        adm_user_id => maps:get(adm_user_id, State, undefined),
        path => maps:get(path, State, undefined),
        conversation_id => conversation_binding(maps:get(path_params, Case, []), Req)
    }.

%% 从 Case 的路径参数声明里找 `conversation_id` 类绑定并解析当前请求的绑定值；
%% 无该类绑定 / 绑定缺失或非法 → `undefined`（合法性仍由后续 build_params 报错）。
conversation_binding(PathParams, Req) when is_list(PathParams) ->
    case [Binding || {Binding, Key} <- PathParams, Key =:= conversation_id] of
        [Binding | _] ->
            case path_tsid(Req, Binding, {missing_path_param, conversation_id}) of
                {ok, Id} -> Id;
                _ -> undefined
            end;
        [] ->
            undefined
    end;
conversation_binding(_PathParams, _Req) ->
    undefined.

case_requirement(RouteMetadata, Case) ->
    case maps:get(required_permission, Case, undefined) of
        undefined -> RouteMetadata;
        Permission -> RouteMetadata#{required_permission => Permission}
    end.

%% route metadata：**只**取白名单键，避免把客户端可控值混进判定输入。
metadata(State, Req) ->
    Keys = [
        auth_context,
        surface,
        required_function,
        required_permission,
        required_governance
    ],
    Present = [{K, maps:get(K, State, undefined)} || K <- Keys, maps:is_key(K, State)],
    case proplists:get_value(auth_context, Present) of
        undefined ->
            {error, route_metadata_missing_auth_context};
        _ ->
            {ok, maps:put(path, cowboy_req:path(Req), maps:from_list(Present))}
    end.

facts_module(State) ->
    case maps:get(auth_facts, State, undefined) of
        Mod when is_atom(Mod), Mod =/= undefined -> {ok, Mod};
        _ -> {error, auth_assembly_missing}
    end.

%% 凭证：租户面 = 普通 IMBoy JWT（中间件注入的 current_uid）；平台面 = Admin
%% session（adm_auth_middleware 注入的 adm_user_id）。**不从 body/query 取**，
%% 客户端无法自报身份。
credential(#{owner := tenant}, State) ->
    case maps:get(current_uid, State, 0) of
        Uid when is_integer(Uid), Uid > 0 -> {ok, #{class => imboy_jwt, user_id => Uid}};
        _ -> {error, credential_missing}
    end;
credential(#{owner := platform}, State) ->
    case maps:get(adm_user_id, State, undefined) of
        Adm when is_integer(Adm), Adm > 0 -> {ok, #{class => adm_session, adm_user_id => Adm}};
        _ -> {error, credential_missing}
    end.

credential_user_id(#{user_id := Uid}) -> Uid;
credential_user_id(_Credential) -> undefined.

%% ===================================================================
%% 路径 / 查询 / 正文
%% ===================================================================

%% @doc 读取并**规范化**请求正文。
%%
%% JSX 把 JSON 对象键解成 binary；本函数只把**动作表已知**的键名转成 atom
%% （白名单映射，不对客户端输入做 `binary_to_atom`——那会带来 atom 表耗尽的攻击面），
%% 其余键保持 binary 并在投影时被忽略。空正文与空 JSON 对象同义（`#{}`）。
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

%% 动作表里出现的全部参数键 + 服务端派生键的 binary→atom 白名单（惰性构建）。
known_keys() ->
    case persistent_term:get({?MODULE, known_keys}, undefined) of
        undefined ->
            Keys = param_keys() ++ [workspace_id, organization_id, workspace_organization_id],
            Known = maps:from_list([{atom_to_binary(K, utf8), K} || K <- lists:usort(Keys)]),
            persistent_term:put({?MODULE, known_keys}, Known),
            Known;
        Known ->
            Known
    end.

param_keys() ->
    Tables =
        [eb_enterprise_actions:tenant(A) || A <- eb_enterprise_actions:tenant_actions()] ++
            [eb_enterprise_actions:platform(A) || A <- eb_enterprise_actions:platform_actions()],
    lists:usort([
        K
     || {ok, Entry} <- Tables,
        Case <- maps:get(cases, Entry),
        {K, _Type, _Req} <- maps:get(params, Case)
    ]).

%% @doc 从 path 取 `org_id`：**显式**租户归属（平台面同样强制，A04）。
-spec org_id(cowboy_req:req()) -> {ok, integer()} | {error, term()}.
org_id(Req) ->
    path_tsid(Req, org_id, missing_org_id).

%% @doc 显式 Workspace 归属：每个用例都必须带（EB-03-A01 口径——store 的前两个业务
%% 参数是 OrgId/WorkspaceId）。GET 走查询串，写走正文；缺一即 422，绝不取默认值。
-spec workspace_id(cowboy_req:req(), map()) -> {ok, integer()} | {error, term()}.
workspace_id(Req, Body) ->
    case value(<<"workspace_id">>, Req, Body) of
        undefined ->
            {error, missing_workspace_id};
        Raw ->
            case tsid(Raw) of
                {ok, Id} -> {ok, Id};
                error -> {error, invalid_workspace_id}
            end
    end.

%% @doc 路径参数（`id` / `message_id` / `uid`）→ 动作表登记的 facade 键。
%% 每个动作**显式**登记（见 `eb_enterprise_actions`），不存在按名字猜测的路径。
-spec path_params(eb_enterprise_actions:kase(), cowboy_req:req()) ->
    {ok, map()} | {error, term()}.
path_params(Case, Req) ->
    path_params(maps:get(path_params, Case), Req, #{}).

path_params([], _Req, Acc) ->
    {ok, Acc};
path_params([{Binding, Key} | Rest], Req, Acc) ->
    case path_tsid(Req, Binding, {missing_path_param, Key}) of
        {ok, Id} -> path_params(Rest, Req, Acc#{Key => Id});
        {error, _} = Err -> Err
    end.

%% 注意：cowboy 的 binding 名字必须是 **atom**（`cowboy_req:binding/3` 的两个子句
%% 都带 `is_atom(Name)` 守卫）；用 binary 会直接 no function clause 崩在 handler 里。
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
%% 参数投影（白名单 + 类型收敛）
%% ===================================================================

%% @doc 按动作表把请求投影成 facade 的 `Params`。
%%
%% 固定纪律：
%%   * `organization_id` / `workspace_organization_id` / `actor_user_id` 由服务端派生，
%%     客户端**提供即 400**（租户归属与操作人不可由调用方决定）；
%%   * 表外的键一律**忽略**（不透传）；表内 required 缺一即 422，类型错 400/422；
%%   * TSID 只接受十进制字符串（A02 传输形态）或 JSON number，且必须 > 0。
-spec build_params(
    eb_enterprise_actions:entry(), eb_enterprise_actions:kase(), cowboy_req:req(), map(), map()
) ->
    {ok, map()} | {error, term()}.
build_params(Entry, Case, Req, Body, Ctx) ->
    case forbidden_client_keys(Body, maps:get(owner, Entry)) of
        {error, _} = Err ->
            Err;
        ok ->
            %% F6（RULING-2026-09-15 §七）：密钥材料键统一守卫（见下）。
            case check_forbidden_crypto_keys(Req, Body) of
                {error, _} = Err ->
                    Err;
                ok ->
                    case path_params(Case, Req) of
                        {error, _} = Err ->
                            Err;
                        {ok, PathParams} ->
                            Base = maps:merge(server_derived(Ctx), PathParams),
                            collect(maps:get(params, Case), Req, Body, Base)
                    end
            end
    end.

%% F6（RULING-2026-09-15 §七）：主密钥材料键在任何动作的 HTTP/JSON 面（正文与
%% 查询串）都不被接受——动作表不声明它们，这里再统一守卫：客户端显式提交即
%% 结构化 422（`unexpected_argument.key_ref`）。密钥只由服务端经
%% `imboy.eb_enterprise_keyring` 装配（eb_env_keyring → application 层）。
forbidden_crypto_keys() ->
    [
        key_ref,
        key,
        key_version,
        keyring,
        key_material,
        master_key,
        %% F-SEC-03（FND-5 贯彻到 contact 域）：profile 密文只由服务端封装。
        profile_cipher,
        profile_key_version
    ].

check_forbidden_crypto_keys(Req, Body) ->
    Qs = cowboy_req:parse_qs(Req),
    case
        [
            K
         || K <- forbidden_crypto_keys(),
            Bin <- [key_bin(K)],
            is_map_key(Bin, Body) orelse proplists:is_defined(Bin, Qs)
        ]
    of
        [] -> ok;
        [Key | _] -> {error, {unexpected_argument, Key}}
    end.

%% 租户面：客户端不得提供 OrgId / Workspace 归属，也不得自报操作人。
%% 平台面：OrgId / Workspace 归属同样禁止客户端提供；但「被代理的组织责任人」
%% `actor_user_id` 是平台面**登记为 required 的显式参数**（平台管理员不是 Org 成员，
%% 而 Core/asset 的 actor 契约要求 actor 属于本 Org），故平台面允许它来自请求。
forbidden_client_keys(Body, tenant) ->
    check_forbidden(Body, ?FORBIDDEN_TENANT_KEYS ++ [?FORBIDDEN_TENANT_ACTOR_KEY]);
forbidden_client_keys(Body, platform) ->
    check_forbidden(Body, ?FORBIDDEN_TENANT_KEYS).

check_forbidden(Body, Keys) ->
    case [K || K <- Keys, is_map_key(K, Body)] of
        [] -> ok;
        [Key | _] -> {error, {forbidden_client_key, Key}}
    end.

%% 服务端派生：租户归属（path org_id）+ Workspace + 操作人。全部来自已认证上下文，
%% 一个都不来自客户端。
server_derived(Ctx) ->
    #{
        organization_id => maps:get(organization_id, Ctx, undefined),
        workspace_id => maps:get(workspace_id, Ctx, undefined),
        actor_user_id => maps:get(actor_user_id, Ctx, undefined)
    }.

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

%% 取参数：正文优先（键已按白名单规范成 atom），其次查询串（键是 binary）。
%% `Key` 允许是 atom（动作表）或 binary（`workspace_id` 这类面级参数）。
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

%% @doc 入站 TSID：十进制字符串（A02 的传输形态）或 JSON number（兼容）。
-spec tsid(term()) -> {ok, integer()} | error.
tsid(Raw) when is_integer(Raw), Raw > 0 ->
    {ok, Raw};
tsid(Raw) when is_binary(Raw) ->
    elib_tsid:from_binary(Raw);
tsid(_Raw) ->
    error.

%% ===================================================================
%% 响应映射
%% ===================================================================

%% @doc 结果 → HTTP。
%%
%% `proxy_content` 动作（asset content）把 facade 视图里的**字节**流式写出；
%% 其余动作把视图编成 JSON（TSID → string）。
-spec respond(eb_enterprise_actions:entry(), cowboy_req:req(), term()) -> cowboy_req:req().
respond(Entry, Req, {ok, View}) ->
    case maps:get(proxy_content, Entry, false) of
        true -> reply_content(Req, View);
        false -> reply_ok(Req, View)
    end;
respond(_Entry, Req, {error, Reason}) ->
    reply_error(Req, Reason).

reply_ok(Req, Payload) ->
    elib_response:success_rfc3339(Req, encode_entity(Payload)).

-spec reply_error(cowboy_req:req(), term()) -> cowboy_req:req().
reply_error(Req, Reason) ->
    Status = status(Reason),
    elib_response:error_with_status(Req, Status, tag(Reason), Status).

%% @doc asset content：把 facade 取得的**字节**原样流式写出。
%%
%% 契约（A05）：响应里**不得**出现 object key / storage endpoint / presigned URL /
%% bucket / storage_ref —— 本函数只可能写出 `body` 的字节与下面四个头（全部是
%% 白名单非存储字段：asset id、对象哈希、mime、长度）。
-spec reply_content(cowboy_req:req(), term()) -> cowboy_req:req().
reply_content(Req, #{body := Bytes} = View) when is_binary(Bytes) ->
    Headers = #{
        <<"content-type">> => maps:get(mime, View, <<"application/octet-stream">>),
        <<"cache-control">> => <<"private, no-store">>,
        <<"x-asset-id">> => to_bin(maps:get(asset_id, View, undefined)),
        <<"x-asset-sha256">> => to_bin(maps:get(object_hash, View, undefined))
    },
    stream(Headers, Bytes, Req);
reply_content(Req, {content_stream, Bytes}) when is_binary(Bytes) ->
    stream(#{<<"content-type">> => <<"application/octet-stream">>}, Bytes, Req).

%% 固定带 `content-length` 的流式响应：不使用 chunked（响应体就是对象字节，
%% 调用方与证据脚本都能按定长读取）。
stream(Headers, Bytes, Req) ->
    Headers1 = Headers#{<<"content-length">> => integer_to_binary(byte_size(Bytes))},
    Req1 = cowboy_req:stream_reply(200, Headers1, Req),
    _ = cowboy_req:stream_body(Bytes, fin, Req1),
    Req1.

to_bin(undefined) -> <<>>;
to_bin(V) when is_binary(V) -> V;
to_bin(V) when is_integer(V) -> integer_to_binary(V);
to_bin(V) -> elib_cnv:safe_to_binary(V).

%% ===================================================================
%% 错误 → HTTP 状态（A03）
%% ===================================================================

%% @doc 错误 → HTTP 状态码。默认 **500**（fail-closed：未登记的内部原因不得被
%% 降级成 4xx 而让调用方以为「只是参数问题」）。
-spec status(term()) -> pos_integer().
status(Reason) ->
    case classify(Reason) of
        Status when is_integer(Status) -> Status;
        unknown -> ?ERR_INTERNAL_SERVER_ERROR
    end.

%% --- 400：请求形状错误（请求侧的错，不是业务状态） ---
classify({forbidden_client_key, _}) ->
    ?ERR_BAD_REQUEST;
classify({invalid_param, _}) ->
    ?ERR_BAD_REQUEST;
classify(invalid_tsid) ->
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
%% --- 422：形状合法但取值不成立（缺必填/域值错） ---
classify(missing_workspace_id) ->
    ?ERR_UNPROCESSABLE_ENTITY;
classify(invalid_workspace_id) ->
    ?ERR_UNPROCESSABLE_ENTITY;
classify({missing_param, _}) ->
    ?ERR_UNPROCESSABLE_ENTITY;
%% C5 分页参数门（显式登记，无兜底）：键集游标/页大小非法是「形状合法但取值
%% 不成立」——与 CS 面 `{invalid_after_id,_}` / `{invalid_limit,_}` → 422 同口径。
classify({invalid_after_id, _}) ->
    ?ERR_UNPROCESSABLE_ENTITY;
classify({invalid_limit, _}) ->
    ?ERR_UNPROCESSABLE_ENTITY;
classify(empty_patch) ->
    ?ERR_UNPROCESSABLE_ENTITY;
classify({invalid_argument, _}) ->
    ?ERR_UNPROCESSABLE_ENTITY;
classify({unexpected_argument, _}) ->
    ?ERR_UNPROCESSABLE_ENTITY;
%% --- 401 / 403：凭证与授权（EB-04 的拒绝面） ---
classify(credential_missing) ->
    ?ERR_UNAUTHORIZED;
classify({principal_mismatch, _, _}) ->
    ?ERR_UNAUTHORIZED;
classify(route_metadata_missing_auth_context) ->
    ?ERR_UNAUTHORIZED;
classify({unknown_auth_context, _}) ->
    ?ERR_UNAUTHORIZED;
%% --- 404：资源不在本租户作用域内（**不区分**不存在与跨 Org，避免枚举） ---
classify(not_found) ->
    ?ERR_NOT_FOUND;
classify({not_found, _}) ->
    ?ERR_NOT_FOUND;
classify({identity_not_found, _}) ->
    ?ERR_NOT_FOUND;
classify({contact_not_found, _}) ->
    ?ERR_NOT_FOUND;
classify({hold_not_found, _}) ->
    ?ERR_NOT_FOUND;
classify({message_not_in_scope, _}) ->
    ?ERR_NOT_FOUND;
classify({case_not_found, _}) ->
    ?ERR_NOT_FOUND;
%% F-LAY-02：workspace 不属于本 Org（infrastructure 有意返回的 9 处原子）——
%% 作用域错配是 404 语义，此前落 500 兜底。
classify({workspace_not_in_org, _}) ->
    ?ERR_NOT_FOUND;
%% --- 409：并发/状态竞争（CAS、重复占用、幂等冲突） ---
classify(conflict) ->
    ?ERR_CONFLICT;
classify(duplicate_occupation) ->
    ?ERR_CONFLICT;
classify({duplicate_active_user_function, _}) ->
    ?ERR_CONFLICT;
classify({identity_bound_to_other_member, _}) ->
    ?ERR_CONFLICT;
classify({conversation_exists, _}) ->
    ?ERR_CONFLICT;
classify({already_verified, _}) ->
    ?ERR_CONFLICT;
classify({case_conflict, _}) ->
    ?ERR_CONFLICT;
classify({stale_version, _}) ->
    ?ERR_CONFLICT;
classify({invalid_transition, _}) ->
    ?ERR_CONFLICT;
classify({hold_already_released, _}) ->
    ?ERR_CONFLICT;
classify({leaver_not_active, _}) ->
    ?ERR_CONFLICT;
classify({successor_not_active, _}) ->
    ?ERR_CONFLICT;
classify({multiple_active_assignment, _}) ->
    ?ERR_CONFLICT;
classify({multiple_active_identity, _}) ->
    ?ERR_CONFLICT;
classify({multiple_active_user_function, _}) ->
    ?ERR_CONFLICT;
%% F-LAY-02：上传引用过期是客户端可重试的冲突语义（此前 500）。
classify(expired_upload_ref) ->
    ?ERR_CONFLICT;
%% F-SEC-01：发送者归属权威校验失败——认证事实与请求声明不一致。
classify({sender_identity_unauthorized, _, _}) ->
    ?ERR_FORBIDDEN;
classify({sender_contact_mismatch, _, _}) ->
    ?ERR_FORBIDDEN;
%% --- 500：**服务端**自身不可用 / 配置缺失 / 完整性失败（绝不伪装成 4xx，
%%      否则调用方会以为「只是参数问题」而重试或改参数）---
classify(Reason) ->
    case server_side(Reason) of
        true -> ?ERR_INTERNAL_SERVER_ERROR;
        false -> enterprise_classify(Reason)
    end.

%% 服务端侧失败面：密钥缺失/版本错、密文封装与打开失败、对象不可读、完整性校验失败、
%% 事实源不可用、端口未装配、ID 生成失败、审计写入失败、时钟不可用。
%% 判据只看**原因本身**（递归一层 `{forbidden, Inner}`，使「事实源不可用」不被
%% 折叠成 403）。
server_side({forbidden, Inner}) -> server_side(Inner);
server_side(missing_key) -> true;
server_side(missing_key_version) -> true;
server_side(invalid_key_length) -> true;
server_side(clock_unavailable) -> true;
server_side({missing_key, _}) -> true;
server_side({missing_key_version, _}) -> true;
server_side({invalid_key_length, _}) -> true;
server_side({unknown_key_version, _}) -> true;
server_side({seal_failed, _}) -> true;
server_side({open_failed, _}) -> true;
server_side({object_unreadable, _}) -> true;
server_side({crypto_unavailable, _}) -> true;
server_side({facts_unavailable, _}) -> true;
server_side({member_fact_query_failed, _}) -> true;
server_side({unimplemented_port, _}) -> true;
server_side({unknown_port, _}) -> true;
server_side({id_generation_failed, _}) -> true;
server_side({audit_append_failed, _}) -> true;
server_side({integrity_check_failed, _}) -> true;
server_side({integrity_check_failed, _, _}) -> true;
server_side({unexpected_content_stream, _}) -> true;
server_side({missing_config, _}) -> true;
server_side({clock_unavailable, _}) -> true;
server_side(_Other) -> false.

%% --- 其余企业拒绝面：先按显式登记判定，未登记时按**原因标签的形状**兜底 ---
enterprise_classify(Reason) ->
    case enterprise_status(Reason) of
        unknown -> textual_status(tag(Reason));
        Status -> Status
    end.

%% 形状兜底（Erlang 模式无法做原子前缀匹配，故对标签文本判定）：`invalid_*` /
%% `missing_*` / `unknown_*` 是域值/前提不成立 ⇒ 422；`forbidden*` ⇒ 403。
textual_status(Tag) ->
    case Tag of
        <<"invalid", _/binary>> -> ?ERR_UNPROCESSABLE_ENTITY;
        <<"missing", _/binary>> -> ?ERR_UNPROCESSABLE_ENTITY;
        <<"unknown", _/binary>> -> ?ERR_UNPROCESSABLE_ENTITY;
        <<"forbidden", _/binary>> -> ?ERR_FORBIDDEN;
        _ -> unknown
    end.

enterprise_status(no_member) -> ?ERR_FORBIDDEN;
enterprise_status({no_member, _}) -> ?ERR_FORBIDDEN;
enterprise_status({forbidden, _}) -> ?ERR_FORBIDDEN;
enterprise_status(forbidden) -> ?ERR_FORBIDDEN;
enterprise_status(not_assignee) -> ?ERR_FORBIDDEN;
enterprise_status(consent_required) -> ?ERR_FORBIDDEN;
enterprise_status({consent_required, _}) -> ?ERR_FORBIDDEN;
enterprise_status({invalid_transition, _}) -> ?ERR_CONFLICT;
enterprise_status({assignee_change_requires_offboarding, _}) -> ?ERR_CONFLICT;
enterprise_status({not_verified, _}) -> ?ERR_CONFLICT;
enterprise_status({self_handover, _}) -> ?ERR_UNPROCESSABLE_ENTITY;
enterprise_status({synthetic_hold_required, _}) -> ?ERR_UNPROCESSABLE_ENTITY;
enterprise_status({policy_shrink_forbidden, _}) -> ?ERR_UNPROCESSABLE_ENTITY;
enterprise_status({retention_shorten_forbidden, _}) -> ?ERR_UNPROCESSABLE_ENTITY;
enterprise_status({notice_version_mismatch, _}) -> ?ERR_UNPROCESSABLE_ENTITY;
enterprise_status({snapshot_mismatch, _}) -> ?ERR_UNPROCESSABLE_ENTITY;
enterprise_status(cross_org) -> ?ERR_FORBIDDEN;
enterprise_status(cross_identity) -> ?ERR_FORBIDDEN;
enterprise_status(identity_assignment_missing) -> ?ERR_FORBIDDEN;
enterprise_status({member_not_active, _}) -> ?ERR_FORBIDDEN;
enterprise_status({function_mismatch, _, _}) -> ?ERR_FORBIDDEN;
enterprise_status({permission_missing, _}) -> ?ERR_FORBIDDEN;
enterprise_status({function_cannot_substitute_permission, _}) -> ?ERR_FORBIDDEN;
enterprise_status({invalid_required_permission, _}) -> ?ERR_FORBIDDEN;
enterprise_status({surface_mismatch, _, _}) -> ?ERR_FORBIDDEN;
enterprise_status({surface_principal_mismatch, _, _}) -> ?ERR_FORBIDDEN;
enterprise_status({missing_required_function, _}) -> ?ERR_FORBIDDEN;
enterprise_status({missing_required_permission, _}) -> ?ERR_FORBIDDEN;
enterprise_status({unknown_governance_role, _}) -> ?ERR_FORBIDDEN;
%% F2（RULING-2026-09-15 §六）：治理不足是可区分的 403 —— 此前该错误从未到达
%% HTTP 层（投影缺失使治理门恒不可通过），修复投影后必须显式映射，不得落 500。
enterprise_status({governance_insufficient, _}) -> ?ERR_FORBIDDEN;
enterprise_status({governance_denied, _}) -> ?ERR_FORBIDDEN;
enterprise_status({ambiguous_surface, _, _}) -> ?ERR_FORBIDDEN;
enterprise_status(_Other) -> unknown.

%% ===================================================================
%% 原因标签（对外可见的**稳定**文本：只含原子路径，绝不含取值）
%% ===================================================================

%% @doc 把错误项编成点分原子路径（如 `identity_assignment_missing`、
%% `forbidden.member_status.suspended`）。**丢弃**所有整数与二进制取值——
%% 响应里不得回显 id、路径、object key 或任何内部值（A03/A05）。
-spec tag(term()) -> binary().
tag(Reason) ->
    Parts = path(Reason, []),
    case Parts of
        [] -> <<"internal_error">>;
        _ -> iolist_to_binary(lists:join(<<".">>, lists:reverse(Parts)))
    end.

path(Atom, Acc) when is_atom(Atom) -> [atom_to_binary(Atom, utf8) | Acc];
path({Tag, Rest}, Acc) when is_atom(Tag) -> path(Rest, [atom_to_binary(Tag, utf8) | Acc]);
path({Tag, _, _}, Acc) when is_atom(Tag) -> [atom_to_binary(Tag, utf8) | Acc];
path(_Other, Acc) -> Acc.

%% ===================================================================
%% 出站编码（A02）
%% ===================================================================

%% @doc 把 application 返回的实体编成 JSON 可传输形状：TSID（64-bit）一律十进制
%% **字符串**，其余值原样（时间戳/计数仍是 number）。递归处理 map/list。
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

%% @doc 该键承载的是 TSID 吗：`id` 或 `*_id`。显式排除**长得像但不是 TSID** 的键
%% （`device_id` 是设备 DID、`client_msg_id` 是客户端幂等键，均为字符串语义），
%% 避免把版本/摘要/设备标识错编成数字字符串或被反向编错。
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
    [<<"trace_id">>, <<"device_id">>, <<"client_msg_id">>].

%% ===================================================================
%% 存储能力泄露扫描（A05；生产侧与测试侧共用同一判据）
%% ===================================================================

%% @doc 扫描一段可对外可见的值（响应头 / 响应体 / 错误信息），命中任何存储侧能力
%% 即返回 `{leak, Kind}`：object key 字面量、scheme URL、presigned、bucket/endpoint、
%% Garage 专有串。
-spec storage_leak_scan(term()) -> ok | {leak, term()}.
storage_leak_scan(Value) ->
    scans(elib_cnv:safe_to_binary(Value)).

scans(Bin) ->
    Patterns = [
        {object_key, <<"object_key">>},
        {storage_ref, <<"storage_ref">>},
        {bucket, <<"bucket">>},
        {endpoint, <<"endpoint">>},
        {presign, <<"presign">>},
        {url_scheme, <<"://">>},
        {garage, <<"garage">>},
        {amz_signature, <<"X-Amz-">>}
    ],
    case [Kind || {Kind, Needle} <- Patterns, binary:match(Bin, Needle) =/= nomatch] of
        [] -> ok;
        [Kind | _] -> {leak, Kind}
    end.
