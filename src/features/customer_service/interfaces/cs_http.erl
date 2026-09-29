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
%%%   * **必填键前置结构化校验**（CS-01 审查观察项：application 对 `client_msg_id`
%%%     用无默认 maps:get）——缺失/类型错在 handler 返回 4xx，绝不把 badarg 泄漏
%%%     成 500；
%%%   * F6（RULING-2026-09-15 §七）：主密钥材料不经 HTTP/JSON 面——`key_ref` 等
%%%     密钥键客户端提交即 422（unexpected_argument.*）；缺 key_ref 正常放行，
%%%     密钥由服务端 env 装配；
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
    tsid/1,
    is_credential_surface_path/1,
    is_web_seat_surface_path/1,
    credential_in_query_string/1,
    check_forbidden/3,
    now_ms/0,
    now_sec/0
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
    imboy_route_shape:is_cs_widget_frame_path(Path) orelse
        %% seat-console-embed SC-BE：/seat/:id 零凭证导航面同款免签直通。
        imboy_route_shape:is_cs_seat_console_frame_path(Path) orelse
        case segments(Path) of
            %% GET /api/v1/cs/sessions（访客列自己的会话）
            [<<"api">>, <<"v1">>, <<"cs">>, <<"sessions">>] ->
                true;
            %% POST /api/v1/cs/organizations/:org_id/sessions/queue（门店开会话——
            %% T-2 后 org 显式在路径；GET 坐席队列视图同路径，凭证面以门店 POST
            %% 为准，坐席 GET 照常由 handler 的 cs_seat 分支校验 JWT）。
            [
                <<"api">>,
                <<"v1">>,
                <<"cs">>,
                <<"organizations">>,
                _OrgId,
                <<"sessions">>,
                <<"queue">>
            ] ->
                true;
            %% POST /api/v1/cs/sessions/:id/messages | /rating（访客消息/评分）
            [<<"api">>, <<"v1">>, <<"cs">>, <<"sessions">>, _Id, Last] when
                Last =:= <<"messages">>; Last =:= <<"rating">>
            ->
                true;
            %% CSB-03：POST /api/v1/cs/widget/bootstrap（widget 引导，签发点本身）
            [<<"api">>, <<"v1">>, <<"cs">>, <<"widget">>, <<"bootstrap">>] ->
                true;
            %% CSB-03：POST /api/v1/cs/widget/identity/exchange（签名身份换绑）
            [<<"api">>, <<"v1">>, <<"cs">>, <<"widget">>, <<"identity">>, <<"exchange">>] ->
                true;
            %% BE-W01 A05：GET /api/v1/cs/widget/frame/:id（动态 frame HTML，
            %% iframe 导航落点——零凭证面）已由 imboy_route_shape 前置判定。
            %% CSB-03：GET+POST /api/v1/cs/widget/sessions（访客会话建立/列表）
            [<<"api">>, <<"v1">>, <<"cs">>, <<"widget">>, <<"sessions">>] ->
                true;
            %% CSB-03：GET+POST .../sessions/:id/messages、GET .../:id/events（SSE）、
            %% POST .../:id/rating（访客消息/事件流/评分——令牌在专用头）
            [<<"api">>, <<"v1">>, <<"cs">>, <<"widget">>, <<"sessions">>, _Id, Last] when
                Last =:= <<"messages">>; Last =:= <<"events">>; Last =:= <<"rating">>
            ->
                true;
            %% CSB-03：POST .../sessions/:id/assets/presign | /confirm（访客附件面）
            [<<"api">>, <<"v1">>, <<"cs">>, <<"widget">>, <<"sessions">>, _Id, <<"assets">>, _Last] ->
                true;
            %% BE-S01b：GET .../sessions/:id/assets/:asset_id/content（访客附件
            %% 内容代理——令牌在专用头，与 presign/confirm 同一面）
            [
                <<"api">>,
                <<"v1">>,
                <<"cs">>,
                <<"widget">>,
                <<"sessions">>,
                _Id,
                <<"assets">>,
                _AssetId,
                <<"content">>
            ] ->
                true;
            _ ->
                false
        end;
is_credential_surface_path(_Path) ->
    false.

%% @doc Web 坐席面路径（2026-09-23 生产 902 修复）：浏览器坐席工作台
%% （imboyadmin `seat/`）经 QR 登录拿到 JWT 后消费的 /api/v1 坐席合同路径族。
%%
%% 为什么免**设备签名**（不是免 JWT）：`auth_middleware_api_v1` 的签名门
%% （`auth_ds:verify_sign`）校验的是移动端设备 HMAC（did|vsn|cos|pkg），
%% 密钥是 APP 内置 solidified key——**不能下发到浏览器**（等于公开）。
%% SEAT-02 已为登录前四条 QR 会话路由做过同款豁免（IsWebQrLoginPath）；
%% 本面把豁免延到登录后的坐席消费路径。这些路径不在 open/option 名单，
%% 中间件 condition 照常走 do_authorization：缺 Bearer 即 401，JWT 门
%% **没有**放宽。
%%
%% 判据是**冻结路径形状**（is_credential_surface_path 同款纪律）：与
%% `cs_route_contract_tests` 的 web_seat_surface 双向核对——tenant 面
%% cs_seat 主体（route metadata 或 case_auth）的路径必须命中本面；
%% enterprise 面被工作台复用的两条消息路径也在列（浏览器与 APP 共用
%% 同一合同，形状层面统一免签）。开发期 api_auth_switch=off 掩盖了这
%% 一缺口；生产该开关是启动强制 on（imboy_app:ensure_api_auth_switch_on）。
-spec is_web_seat_surface_path(binary()) -> boolean().
is_web_seat_surface_path(Path) when is_binary(Path) ->
    case segments(Path) of
        %% BE-S01a：坐席上下文清单（工作台登录后第一跳）。
        [<<"api">>, <<"v1">>, <<"cs">>, <<"me">>, <<"seat-contexts">>] ->
            true;
        %% 坐席会话面（T-2 后 org 显式在路径）：queue（GET=坐席 case_auth /
        %% POST=门店凭证面，同形状双方免签语义一致）；会话详情；
        %% claim/transfer/close 生命周期。
        [<<"api">>, <<"v1">>, <<"cs">>, <<"organizations">>, _OrgId, <<"sessions">>, <<"queue">>] ->
            true;
        [<<"api">>, <<"v1">>, <<"cs">>, <<"organizations">>, _OrgId, <<"sessions">>, _SessionId] ->
            true;
        [
            <<"api">>,
            <<"v1">>,
            <<"cs">>,
            <<"organizations">>,
            _OrgId,
            <<"sessions">>,
            _SessionId,
            Action
        ] when
            Action =:= <<"claim">>;
            Action =:= <<"transfer">>;
            Action =:= <<"close">>;
            %% CS-BE-03：客户上下文只读投影（工作台右栏；cs_seat 主体）。
            Action =:= <<"context">>;
            %% CS-BE-04：会话已读游标（GET 读状态 / POST ACK；cs_seat 主体）。
            Action =:= <<"read-cursor">>
        ->
            true;
        %% 工作台 active/closed 两视图 / 转接目标 / 坐席 SSE 事件流。
        [<<"api">>, <<"v1">>, <<"cs">>, <<"organizations">>, _OrgId, <<"seats">>, <<"sessions">>] ->
            true;
        [<<"api">>, <<"v1">>, <<"cs">>, <<"organizations">>, _OrgId, <<"transfer-targets">>] ->
            true;
        [
            <<"api">>,
            <<"v1">>,
            <<"cs">>,
            <<"organizations">>,
            _OrgId,
            <<"seats">>,
            <<"me">>,
            <<"events">>
        ] ->
            true;
        %% CS-BE-06：席位 entitlement 治理（PUT 配置/清除 + GET 视图）。
        [
            <<"api">>,
            <<"v1">>,
            <<"cs">>,
            <<"organizations">>,
            _OrgId,
            <<"seat-limit">>
        ] ->
            true;
        %% CS-BE-05：presence 心跳（POST）与手动状态/运行态视图（PUT/GET）。
        [
            <<"api">>,
            <<"v1">>,
            <<"cs">>,
            <<"organizations">>,
            _OrgId,
            <<"seats">>,
            <<"me">>,
            <<"heartbeat">>
        ] ->
            true;
        [
            <<"api">>,
            <<"v1">>,
            <<"cs">>,
            <<"organizations">>,
            _OrgId,
            <<"seats">>,
            <<"me">>,
            <<"presence">>
        ] ->
            true;
        [
            <<"api">>,
            <<"v1">>,
            <<"cs">>,
            <<"organizations">>,
            _OrgId,
            <<"seats">>,
            <<"presence">>
        ] ->
            true;
        %% A0 契约：坐席侧企业消息历史 + 发送（enterprise 真源复用路径）。
        [<<"api">>, <<"v1">>, <<"enterprise">>, <<"conversations">>, _ConvId, <<"messages">>] ->
            true;
        [
            <<"api">>,
            <<"v1">>,
            <<"enterprise">>,
            <<"organizations">>,
            _OrgId,
            <<"conversations">>,
            _ConvId,
            <<"messages">>
        ] ->
            true;
        _ ->
            false
    end;
is_web_seat_surface_path(_Path) ->
    false.

%% @doc 查询串里出现凭证样式的键即 true（widget 面凭证只准走专用头，
%% 查询串会进访问日志/代理日志——发现即 400，绝不解析其值）。
-spec credential_in_query_string(cowboy_req:req()) -> boolean().
credential_in_query_string(Req) ->
    Qs = cowboy_req:parse_qs(Req),
    lists:any(fun(K) -> proplists:is_defined(K, Qs) end, credential_qs_keys()).

credential_qs_keys() ->
    [
        <<"token">>,
        <<"secret">>,
        <<"access_token">>,
        <<"visit_token">>,
        <<"shop_key">>,
        <<"x-cs-visit-token">>,
        <<"x-cs-shop-key">>
    ].

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
    try jsone:decode(Raw) of
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
    Tables = action_tables(),
    lists:usort([
        K
     || {ok, Entry} <- Tables,
        Case <- maps:get(cases, Entry),
        {K, _Type, _Req} <- maps:get(params, Case)
    ]).

%% 三张动作表（租户/平台/widget）的统一读取点——新增面漏改这里会在
%% cs_route_contract_tests 的白名单核对处显式红。
action_tables() ->
    [cs_actions:tenant(A) || A <- cs_actions:tenant_actions()] ++
        [cs_actions:platform(A) || A <- cs_actions:platform_actions()] ++
        [cs_actions:widget(A) || A <- cs_actions:widget_actions()].

%% @doc OrgId 解析（cs_actions:org_source/1 决定来源）：
%%   * `path` —— cowboy 绑定 `org_id`（T-2 后坐席/治理/门店面）；
%%   * `param` —— A0 冻结访客路径（path 无 org 段）取查询/正文的必填
%%     `organization_id`，作为**申报值**交由 cs_auth 用凭证/事实证明；
%%   * `self` —— 主体自身作用域（BE-S01a 坐席上下文清单）：无 Org 键，
%%     返回 0 占位（facade 的 self 用例不读 OrgId，作用域是 actor 本人）；
%%   * `derived` —— CSD-BE-01R/01S（hosted-widget-contract S3 v1.1）：widget
%%     面的浏览器零申报面——无 Org 输入，返回 0 占位（facade 调用点同构），
%%     租户由 application 权威派生（bootstrap：public_widget_id 全局反查；
%%     持 token 动作面：(installation_id, secret) 的 digest 全局命中行）；
%%     客户端申报 `organization_id` 被动作表 client_forbidden 拦为
%%     400 `server_derived_key_rejected`；
%%   * `param_optional` —— 平台全局面（p_platform_seats）：organization_id 是
%%     **可选**过滤参数——缺失 = 跨企业全局列举（OrgId=0 占位，同 self/derived
%%     的占位语义），给出则收窄到该企业（非法值仍 422，不静默全局）。
-spec org_id(map(), cowboy_req:req(), map()) -> {ok, integer()} | {error, term()}.
org_id(Entry, Req, Body) ->
    case cs_actions:org_source(Entry) of
        path ->
            path_tsid(Req, org_id, missing_org_id);
        self ->
            {ok, 0};
        derived ->
            {ok, 0};
        param ->
            case value(organization_id, Req, Body) of
                undefined ->
                    {error, missing_org_id};
                Raw ->
                    case tsid(Raw) of
                        {ok, Id} -> {ok, Id};
                        error -> {error, invalid_org_id}
                    end
            end;
        param_optional ->
            case value(organization_id, Req, Body) of
                undefined ->
                    {ok, 0};
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
    Qs = cowboy_req:parse_qs(Req),
    case check_forbidden(Body, Qs, Forbidden ++ ?FORBIDDEN_SCOPE_KEYS) of
        {error, _} = Err ->
            Err;
        ok ->
            %% F6：密钥材料键统一守卫（见 check_forbidden_crypto_keys/2）。
            case check_forbidden_crypto_keys(Req, Body) of
                {error, _} = Err ->
                    Err;
                ok ->
                    case path_params(Case, Req) of
                        {error, _} = Err ->
                            Err;
                        {ok, PathParams} ->
                            Base = maps:merge(server_derived(), PathParams),
                            collect(
                                maps:get(params, Case), Req, Body, maps:merge(Base, Derived)
                            )
                    end
            end
    end.

%% 服务端派生键全集（动作表 client_forbidden 的并集 + 面级归属键）。
server_derived_keys() ->
    lists:usort(lists:append([maps:get(client_forbidden, E, []) || {ok, E} <- action_tables()])).

server_derived() ->
    #{}.

%% R2-F2（hosted-widget-contract S3 查询串面）：派生键申报面 = 正文**与**
%% 查询串双查——与 check_forbidden_crypto_keys/2 同口径。派生事实永不来自
%% 客户端，query 里的申报显式 400 而非静默忽略。
check_forbidden(Body, Qs, Keys) when is_map(Body), is_list(Qs) ->
    case
        [
            K
         || K <- Keys,
            is_map_key(K, Body) orelse proplists:is_defined(key_bin(K), Qs)
        ]
    of
        [] ->
            ok;
        [Key | _] ->
            %% CSD-BE-01R（hosted-widget-contract S3）：400 `server_derived_key_rejected`。
            {error, {server_derived_key_rejected, Key}}
    end.

%% F6（RULING-2026-09-15 §七）：主密钥材料键在任何动作的 HTTP/JSON 面（正文与
%% 查询串）都不被接受——动作表不声明它们，这里再统一守卫：客户端显式提交即
%% 结构化 422（`unexpected_argument.key_ref`；FND-5 body_cipher 同款先例），
%% 请求不抵达 application。密钥只由服务端经 `imboy.eb_enterprise_keyring` 装配。
forbidden_crypto_keys() ->
    [key_ref, key, key_version, keyring, key_material, master_key].

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
coerce(list, Raw) when is_list(Raw) ->
    {ok, Raw};
%% 嵌套 JSON 对象（widget 断言 assertion 等）原样透传；形状/取值由
%% application 的 claims 全查承担（缺键/类型错在 claims 判定处结构化失败）。
coerce(map, Raw) when is_map(Raw) ->
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

%% @doc Unix **秒**（CSB-02S D2/D3）：widget 面的统一时间基准——bootstrap
%% 令牌 TTL（秒）、identity key expires_at（epoch 秒）、JWT claims exp/iat
%% （秒）与 nonce to_timestamp（秒）全部以此为口径。毫秒基准（now_ms/0）仍
%% 是租户面既有约定，不在本卡范围。
-spec now_sec() -> integer().
now_sec() ->
    os:system_time(second).

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
%% F-6（REVIEW-3）：CAS 失败对外契约——409 + `cas_mismatch` 标签，envelope
%% payload 携带期望/当前 version（纯整数，无其他内部细节；调用方据此提示
%% 「已被他人更新」并刷新重试）。classify({cas_mismatch, _}) → 409 既有映射
%% 不变，本子句只是把 Detail 展开进响应体。
reply_error(Req, {cas_mismatch, Detail}) when is_map(Detail) ->
    Data = maps:with([expected_version, actual_version], Detail),
    elib_response:error_with_status(Req, ?ERR_CONFLICT, <<"cas_mismatch">>, Data, ?ERR_CONFLICT);
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
%% CSD-BE-01R（hosted-widget-contract S3）：客户端申报服务端派生键 = 400
%% `server_derived_key_rejected`（原名 forbidden_client_key，按合同对齐）。
classify({server_derived_key_rejected, _}) ->
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
%% CSB-03：widget 面凭证只准走专用头——查询串携带凭证样式键即 400（值不读）。
classify(credential_in_query_string) ->
    ?ERR_BAD_REQUEST;
%% CSD-BE-01：/w/:public_widget_id 路径绑定形状非法（空/越界字符集）——
%% 与旧 frame 的 invalid_tsid 同为 400 形状面（无枚举，不区分形状错与不存在
%% ——不存在的 installation 走 404 installation_unavailable）。
classify(invalid_public_widget_id) ->
    ?ERR_BAD_REQUEST;
%% seat-console-embed SC-BE：/seat/:public_seat_console_id 路径绑定形状非法
%% （空/越界字符集）——与 invalid_public_widget_id 同为 400 形状面（无枚举，
%% 不区分形状错与不存在——不存在的控制台走 404 seat_console_unavailable）。
classify(invalid_public_seat_console_id) ->
    ?ERR_BAD_REQUEST;
%% CSB-03：Origin 头形状非法（含 path/userinfo/非法端口等）——fail-closed 400。
classify({invalid_origin, _}) ->
    ?ERR_BAD_REQUEST;
%% DF-2：坏 upload_ref（附件 confirm 的唯一凭证）是客户端凭证错误——篡改/
%% 跨租户重放/垃圾串都进不了解密门。未显式登记时落 server_side 兜底恒 500，
%% 客户端会把自身凭证问题当服务端故障重试；显式登记为 400（EB 面同因
%% `{invalid_recipient_ref, _}` ⇒ 400 的显式登记先例：凭证形状是请求错误，
%% 不是域值不成立）。
classify(invalid_upload_ref) ->
    ?ERR_BAD_REQUEST;
classify(method_not_allowed) ->
    ?ERR_METHOD_NOT_ALLOWED;
%% BE-S01a：坐席 SSE 占位（seat_events 路由族已注册、流式实现在 BE-S01b）。
%% 501 语义：路由存在但能力未交付——客户端探测能力，不误判路由缺失（404）。
classify(not_implemented) ->
    ?ERR_NOT_IMPLEMENTED;
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
%% CP-SEC-05（DEC-VISIT-TOKEN=FIX_401_VISIT_TOKEN_INVALID）：伪造/跨租户
%% 重放的 visit token（digest 无命中行）——凭证无效 401。application 层
%% （cs_widget_support fetch/derive + cs_widget_app replay）统一翻译此原子。
classify(visit_token_invalid) ->
    ?ERR_UNAUTHORIZED;
%% F-LAY-03：CS 域真原子（cs_session:assert_visitor_scope 产出）；下列 EB 侧
%% 词汇（visit_token_revoked/visit_token_expired/shop_key_revoked/cross_contact）
%% 是从 eb_auth_app 抄来的死条目，CS 链路永不产出，已删除。
classify(contact_mismatch) ->
    ?ERR_UNAUTHORIZED;
classify(token_expired) ->
    ?ERR_UNAUTHORIZED;
classify(revoked) ->
    ?ERR_UNAUTHORIZED;
classify(token_revoked) ->
    ?ERR_UNAUTHORIZED;
%% CSB-03：widget 签名身份断言 / 令牌重放面的 401 词汇（claims 值不符、
%% subject 不符、identity key 过期——凭证语义，不是资源错误）。
classify({invalid_claim, _}) ->
    ?ERR_UNAUTHORIZED;
classify(assertion_aud_mismatch) ->
    ?ERR_UNAUTHORIZED;
classify(assertion_widget_mismatch) ->
    ?ERR_UNAUTHORIZED;
classify(assertion_expired) ->
    ?ERR_UNAUTHORIZED;
classify(assertion_iat_in_future) ->
    ?ERR_UNAUTHORIZED;
classify(subject_mismatch) ->
    ?ERR_UNAUTHORIZED;
classify(identity_key_expired) ->
    ?ERR_UNAUTHORIZED;
%% CSB-02R：签名断言 HMAC 复核失败（凭证语义）。
classify(assertion_signature_mismatch) ->
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
%% F-SEC-05：路由元数据缺权限声明 = 配置错误，fail-closed（403 而非静默放行）。
classify({missing_required_permission, _}) ->
    ?ERR_FORBIDDEN;
classify({governance_insufficient, _}) ->
    ?ERR_FORBIDDEN;
classify(platform_identity_mismatch) ->
    ?ERR_FORBIDDEN;
classify(cross_org) ->
    ?ERR_FORBIDDEN;
%% CS-BE-03：客户上下文的 session ownership 门——请求坐席不是该会话当前
%% 经办（含 queued 无经办 undefined）：授权面拒绝（403），不是资源不存在
%% （会话存在性已由坐席工作台可见，404 反而制造枚举歧义）。
classify({not_session_owner, _, _}) ->
    ?ERR_FORBIDDEN;
classify({function_mismatch, _, _}) ->
    ?ERR_FORBIDDEN;
%% CSB-03：widget 接入面的 403 词汇——Origin 不在 installation allowlist、
%% installation 已吊销（kill switch）、identity key 已吊销。
classify(origin_not_allowed) ->
    ?ERR_FORBIDDEN;
classify(installation_revoked) ->
    ?ERR_FORBIDDEN;
classify(identity_key_revoked) ->
    ?ERR_FORBIDDEN;
%% seat-console-embed SC-BE：控制台管理面的 403 词汇——已吊销行拒绝编辑。
classify(seat_console_revoked) ->
    ?ERR_FORBIDDEN;
%% CSB-02S D6：访客附件作用域——令牌 contact 与会话 contact 不符（403 面）。
classify({forbidden, contact_scope_mismatch}) ->
    ?ERR_FORBIDDEN;
%% BE-W01 A06：identity/exchange 第一阶段能力未开放（默认关闭；明确状态，
%% 非客户端过错集合，但按冻结合同归入拒绝面）。
classify({capability_disabled, _}) ->
    ?ERR_FORBIDDEN;
%% --- 404：资源不在本租户作用域（不区分不存在与跨 Org，避免枚举）---
classify(not_found) ->
    ?ERR_NOT_FOUND;
classify({not_found, _}) ->
    ?ERR_NOT_FOUND;
classify({session_not_found, _}) ->
    ?ERR_NOT_FOUND;
%% CSD-BE-01（hosted-widget-contract S3）：public_widget_id 反查面（/w/ 与
%% bootstrap）的统一 404——不存在 / disabled / revoked(kill switch) 三态归一
%% `installation_unavailable`，不泄漏 installation 存在性差异。旧 frame 面的
%% `installation_revoked`=403 分类保留（兼容窗口，见下方 403 段）。
classify(installation_unavailable) ->
    ?ERR_NOT_FOUND;
%% seat-console-embed SC-BE：public_seat_console_id 反查面（/seat/:id）的统一
%% 404——不存在 / revoked(kill switch) 三态归一 `seat_console_unavailable`，
%% 不泄漏控制台存在性差异。
classify(seat_console_unavailable) ->
    ?ERR_NOT_FOUND;
%% F-LAY-01：seat 绑定不存在的业务身份 → 与 EB 面 404 同口径（此前 500）。
classify({identity_not_found, _}) ->
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
%% CSB-03：widget 会话侧幂等（同一 contact 已有未关闭会话）与 nonce 重放。
classify({session_already_open, _}) ->
    ?ERR_CONFLICT;
classify(replay) ->
    ?ERR_CONFLICT;
classify(already_rated) ->
    ?ERR_CONFLICT;
%% DF-2：过期 upload_ref 与 EB 面同口径（eb_enterprise_http F-LAY-02：上传
%% 引用过期是客户端可重试的冲突语义，此前 widget 面未登记恒 500）。
classify(expired_upload_ref) ->
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
%% F-LAY-01：身份存在但职能不是 customer_service → 形状合法取值不成立（此前 500）。
classify({identity_not_customer_service, _, _}) ->
    ?ERR_UNPROCESSABLE_ENTITY;
classify({invalid_contact_id, _}) ->
    ?ERR_UNPROCESSABLE_ENTITY;
classify({invalid_rating, _}) ->
    ?ERR_UNPROCESSABLE_ENTITY;
classify({invalid_organization_id, _}) ->
    ?ERR_UNPROCESSABLE_ENTITY;
classify({not_session_contact, _, _}) ->
    ?ERR_UNPROCESSABLE_ENTITY;
%% CSB-02S D6 补全：访客附件桥接（eb_asset_content 域校验）的取值不成立
%% ——此前未登记被当 500（eb 面有 invalid_* 形状兜底，widget 面无兜底故
%% 显式登记）。缺参在动作表层已是 422 missing_param，此处覆盖值不合法。
classify({invalid_object_hash, _}) ->
    ?ERR_UNPROCESSABLE_ENTITY;
classify({invalid_mime, _}) ->
    ?ERR_UNPROCESSABLE_ENTITY;
classify({invalid_size_bytes, _}) ->
    ?ERR_UNPROCESSABLE_ENTITY;
%% C1~C4（contracts-w2）：列表查询参数的取值不成立——显式登记，无兜底。
classify({invalid_after_id, _}) ->
    ?ERR_UNPROCESSABLE_ENTITY;
%% CS-BE-07：统计窗口参数的取值不成立（date 非日历日 / tz_offset 越界）。
classify({invalid_date, _}) ->
    ?ERR_UNPROCESSABLE_ENTITY;
classify({invalid_tz_offset, _}) ->
    ?ERR_UNPROCESSABLE_ENTITY;
classify({invalid_limit, _}) ->
    ?ERR_UNPROCESSABLE_ENTITY;
classify({invalid_status, _}) ->
    ?ERR_UNPROCESSABLE_ENTITY;
classify({not_session_seat, _, _}) ->
    ?ERR_UNPROCESSABLE_ENTITY;
%% CS-BE-04：ACK 候选游标非负整数取值不成立（形状由动作表 tsid 裁决，
%% 这里覆盖负值等域值错误——显式登记，无兜底）。
classify({invalid_message_id, _}) ->
    ?ERR_UNPROCESSABLE_ENTITY;
%% CSB-03：installation 未配置 identity key（形状合法但取值不成立——
%% 租户未启用签名身份换绑）。
classify(identity_key_not_configured) ->
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
%% CSB-03：widget 用例的服务端注入事实缺失（HMAC 材料 / 默认 Workspace /
%% 接待 identity / 断言验证器）与默认 Workspace 解析失败——配置问题，
%% 绝不伪装成 4xx。
server_side({missing_injection, _}) -> true;
server_side(default_workspace_unresolved) -> true;
%% CSB-02R：provisioned 验签材料与 DB 行 digest 不符 = 服务端配置漂移。
server_side(identity_key_digest_mismatch) -> true;
server_side({unimplemented_port, _}) -> true;
server_side({unknown_port, _}) -> true;
server_side({missing_config, _}) -> true;
server_side(clock_unavailable) -> true;
server_side({enterprise_business_feature_not_selected, _}) -> true;
server_side(_Other) -> false.

%% ===================================================================
%% 原因标签（对外可见的稳定文本；丢弃整数与二进制取值）
%% ===================================================================

%% @doc 把错误项编成点分原子路径。**两个特例**（对外契约冻结标签，不回显
%%% 取值/键名）：offboarding 降级对外是 `offboarding_required`；
%%% 客户端申报服务端派生键对外是合同 S3 的 `server_derived_key_rejected`
%%% ——不回显命中键名，避免把服务端派生键集合变成客户端的枚举面。
-spec tag(term()) -> binary().
tag({assignee_change_requires_offboarding, _}) ->
    <<"offboarding_required">>;
%% CSD-BE-01R（hosted-widget-contract S3 冻结码）：键名不出线（枚举面收口）。
tag({server_derived_key_rejected, _Key}) ->
    <<"server_derived_key_rejected">>;
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

encode_value(_Key, undefined) ->
    %% contracts-w2 C1~C4：`next_after_id: string|null` 等可空出站键——undefined
    %% 统一编为 JSON null（jsone 默认把 undefined atom 写成字符串 "undefined"，
    %% 语义错误；JSON 惯例空值是 null）。
    null;
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
%% （device_id/client_msg_id 是字符串语义；key_ref 已从 HTTP 面整体删除，F6）。
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
    %% F6：key_ref 已从 HTTP 面整体删除（提交即 422），不再是出站键候选。
    [<<"trace_id">>, <<"device_id">>, <<"client_msg_id">>].
