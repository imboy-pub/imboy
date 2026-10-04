-module(auth_middleware_api_v1).

-moduledoc "API v1 认证中间件的纯路径谓词导出层 —— 供离线合同测试直接调用，无副作用。".
-behaviour(cowboy_middleware).

-export([execute/2]).
%% C12 / AC-25：导出纯路径谓词供离线合同测试
%% （scripts/test/e2ee_version_gate_test.sh）直接调用；无副作用。
-export([is_e2ee_api_path/1]).

-include("log.hrl").
-include("error_code.hrl").
-include("generated/imboy_product_features.hrl").

%% @doc Cowboy中间件执行函数
%% 处理 /v1 路由的认证和授权验证
%%
%% @param Req Cowboy请求对象
%% @param Env 环境变量映射
%% @return 中间件执行结果
-spec execute(cowboy_req:req(), map()) ->
    {ok, cowboy_req:req(), map()} | {stop, cowboy_req:req()}.
execute(Req, Env) ->
    RouteOpts = maps:get(handler_opts, Env, #{}),
    case maps:get(jwt_methods, RouteOpts, undefined) of
        Methods when is_list(Methods) ->
            case lists:member(cowboy_req:method(Req), Methods) of
                true ->
                    execute_authorized(Req, Env);
                false ->
                    Req405 = cowboy_req:set_resp_header(
                        <<"allow">>,
                        iolist_to_binary(lists:join(<<", ">>, Methods)),
                        Req
                    ),
                    {stop,
                        elib_response:error_with_status(
                            Req405, 405, <<"Method Not Allowed">>, ?ERR_BAD_REQUEST
                        )}
            end;
        _ ->
            execute_authorized(Req, Env)
    end.

execute_authorized(Req, Env) ->
    IsSeatConsoleRoute = maps:get(jwt_purpose, maps:get(handler_opts, Env, #{}), human) =:= seat,
    Path = auth_ds:remove_last_forward_slash(cowboy_req:path(Req)),

    OpenLi = imboy_router:open(),
    OptionLi = imboy_router:option(),
    %% 支付回调来自第三方服务器无 JWT，仅 /api/v1/payment/callback/:gateway 免认证。
    %% :gateway 为变量段无法在 open/0 精确枚举，
    %% 并把前缀命中折叠进 InOpenLi —— 这样既跳过 verify_sign，又能让下游
    %% auth_ds:condition/5 以「开放路由」放行（否则 condition 仍会因无 token 而 stop）。
    IsPaymentCallback = is_single_segment_route(Path, <<"/api/v1/payment/callback/">>),
    %% 频道 incoming webhook 同款范式：token 即凭证（:token 变量段无法在 open/0
    %% 精确枚举），限流/token 校验在 channel_webhook_logic:incoming/2 完成。
    IsChannelWebhook = is_single_segment_route(Path, <<"/api/v1/webhook/channel/">>),
    %% MCP Server（MCP-01）：该路由只接受 MCP client credential（Bearer mck 场景
    %% 的 secret 无 JWT 语义），认证收敛于 mcp_handler（digest 查找+fail-closed），
    %% 中间件直通。
    IsMcpPath = Path =:= <<"/api/v1/mcp">>,
    %% EB-04（EB-D10）：企业租户面（/api/v1/enterprise/*、/api/v1/cs/*）的 principal
    %% 类别由 route metadata 决定（eb_auth_principal:principal_for_route/1 在
    %% handler 侧消费），中间件不按 URL 字符串猜 principal，只判定「是否租户面」。
    %% 租户面一律**不得进入开放直通**：即使企业路径被误登记进 open()，也必须照常
    %% 走签名门 + condition，避免企业资源被当公开路由放行（fail-closed）。
    %% 平台运营面 /api/adm/* 在 auth_middleware 已先分流给 adm_auth_middleware，
    %% 因此企业身份无路径从租户面越权到 Admin 面。
    %% F-EB10-1：租户面判定收进 is_enterprise_tenant_path/1（函数级 -ifdef 保护，
    %% 见定义处）。中间件是全站 /api/v1 必经路径，不能在表达式序列中间直接
    %% 调用被裁模块 —— 未选中档的编译产物必须零引用（beam 级验证）。
    IsEnterpriseTenantPath = is_enterprise_tenant_path(Path),
    %% CS-02（EB-D10 五类身份的凭证面）：/api/v1/cs/* 里**访客/门店**动作
    %% （queue、列自己的会话、入站消息、评分）的凭证是专用传输头
    %% （x-cs-visit-token / x-cs-shop-key），不是 IMBoy JWT、也没有设备签名——
    %% 商城访客不是 IMBoy 设备。这些路径免 verify_sign + 免 JWT 直通，由
    %% cs_tenant_handler 侧 cs_auth:authorize/3 fail-closed 校验（digest/过期/
    %% 吊销/租户作用域全部在 handler 裁决，直通 ≠ 放行）。
    %% 路径形状判定收进 is_cs_credential_path/1（函数级 -ifdef 保护，见定义处，
    %% 与 F-EB10-1 同款）；其余 /api/v1/cs/*（seat/治理动作）照常走签名 + JWT 门。
    IsCsCredentialPath = is_cs_credential_path(Path),
    %% 2026-09-23 生产 902 修复：Web 坐席工作台（imboyadmin seat/）浏览器端
    %% 无法持有 APP 设备签名密钥（HMAC 材料不能下发浏览器），QR 登录后消费
    %% 的坐席合同路径族免 verify_sign；JWT 门不变（不在 open/option 名单，
    %% condition 照常 do_authorization）。路径形状由
    %% cs_http:is_web_seat_surface_path/1 冻结声明（cs_route_contract_tests
    %% 双向核对）。与 IsCsCredentialPath 同款函数级 -ifdef 保护（见定义处）。
    IsWebSeatPath = IsSeatConsoleRoute orelse is_web_seat_path(Path),
    InOpenLi =
        (not IsSeatConsoleRoute) andalso (not IsEnterpriseTenantPath) andalso
            (IsPaymentCallback orelse IsChannelWebhook orelse IsMcpPath orelse
                lists:member(Path, OpenLi)),
    InOptionLi = (not IsSeatConsoleRoute) andalso lists:member(Path, OptionLi),
    Switch = ec_cnv:to_binary(config_ds:env(api_auth_switch, <<"on">>)),
    %% ws/init/refreshtoken/passport 是 JWT-open 但仍需设备签名校验的端点
    %% （open() 命中会让 InOpenLi=true，若不在此显式拦截会被直接放行，
    %% 丢失签名防篡改校验）。2026-07-08 v0 裸 /api/* 路由已下架，
    %% 只保留 /api/v1/* 形态。
    IsPassportPath =
        string:sub_string(binary_to_list(Path), 1, 17) == "/api/v1/passport/",
    %% Web 坐席在登录前无法生成移动端设备签名；这四条 QR 会话路由本身
    %% 已在 open/0 精确登记。手机端 scan/confirm 不在此列，仍走签名 + JWT。
    IsWebQrLoginPath =
        lists:member(Path, [
            <<"/api/v1/passport/qr_login/create">>,
            <<"/api/v1/passport/qr_login/status">>,
            <<"/api/v1/passport/qr_login/cancel">>,
            <<"/api/v1/passport/qr_login/subscribe">>
        ]),
    Res1 =
        if
            Path == <<"/api/v1/ws">>, Switch == <<"on">> ->
                auth_ds:verify_sign(Req, Env);
            Path == <<"/api/v1/init">>, Switch == <<"on">> ->
                auth_ds:verify_sign(Req, Env);
            Path == <<"/api/v1/refreshtoken">>, Switch == <<"on">> ->
                auth_ds:verify_sign(Req, Env);
            IsPassportPath, not IsWebQrLoginPath, Switch == <<"on">> ->
                auth_ds:verify_sign(Req, Env);
            InOpenLi == false, not IsCsCredentialPath, not IsWebSeatPath, Switch == <<"on">> ->
                auth_ds:verify_sign(Req, Env);
            true ->
                {ok, Req, Env}
        end,
    case Res1 of
        {ok, Req, Env} ->
            %% C12 / AC-25：E2EE REST 面硬版本门（verify_sign 之后、JWT 之前——
            %% 签名门保证 vsn/cos 头真实，版本门再按头裁决）。低于生效最低版本
            %% 的旧端在此被拒绝，不进入任何 E2EE 业务逻辑（handler 不再可达）。
            case e2ee_version_gate(Path, Req) of
                ok ->
                    Authorization = cowboy_req:header(<<"authorization">>, Req),
                    %% CS credential 面：无 Authorization 头也放行（凭证在专用头），
                    %% handler 侧 fail-closed；带 JWT 的误用请求会在 cs_auth 处
                    %% credential_missing（principal 只认专用头，不混淆）。
                    %% CSB-02R：CS credential 面改走 option 语义——无 Authorization 的
                    %% 访客/门店请求照旧直通（凭证在专用头）；**带** Authorization 的
                    %% 请求照常过 JWT 门并注入会话键（current_uid），支撑同路径
                    %% method+auth_context 分流（GET /sessions/queue 的坐席主体）。
                    %% 主体混淆仍由 cs_auth 按 route metadata/case_auth 裁决：JWT 在场
                    %% 不采信为 visit/shop_key，反之亦然。
                    auth_ds:condition(
                        InOptionLi orelse IsCsCredentialPath,
                        InOpenLi andalso not IsCsCredentialPath,
                        Authorization,
                        Req,
                        Env
                    );
                {stop, ReqGated} ->
                    {stop, ReqGated}
            end;
        Res2 ->
            Res2
    end.

is_single_segment_route(Path, Prefix) ->
    PrefixSize = byte_size(Prefix),
    case Path of
        <<Prefix:PrefixSize/binary, Segment/binary>> when Segment =/= <<>> ->
            binary:match(Segment, <<"/">>) =:= nomatch;
        _ ->
            false
    end.

%% ===================================================================
%% C12 / AC-25：E2EE REST 面硬版本门
%% -------------------------------------------------------------------
%% 覆盖 /api/v1/e2ee/* 全部端点（user_keys / group_member_keys /
%% group_history_grant / report_device_key / key/status /
%% notifications/pull / recovery/start / compliance_key / backup/* /
%% olm/* / devices* / trust/record）与 /api/v1/group/set_e2ee_mode。
%% 门未配置（app_version 无该平台记录或 min_vsn=0.0.0）时恒 allow，
%% 现网行为零变化；rollout 顺序（客户端先发布→后端配置 min_vsn→开
%% required E2EE）见 docs/design/2026-10-03-e2ee-production-excellence/
%% runbook。判定真源复用 app_version_logic:e2ee_gate/2（与升级提示
%% check/3 同一套 min_vsn），阻断响应错误码 ?ERR_E2EE_APP_VERSION_TOO_LOW。
%% ===================================================================
-spec e2ee_version_gate(binary(), cowboy_req:req()) ->
    ok | {stop, cowboy_req:req()}.
e2ee_version_gate(Path, Req) ->
    case is_e2ee_api_path(Path) of
        false ->
            ok;
        true ->
            try
                Vsn = cowboy_req:header(<<"vsn">>, Req, undefined),
                Cos = cowboy_req:header(<<"cos">>, Req, <<>>),
                case app_version_logic:e2ee_gate(Vsn, Cos) of
                    allow ->
                        ok;
                    {block, MinVsn} ->
                        Msg = iolist_to_binary([
                            <<"客户端版本过低，E2EE 功能要求版本 >= "/utf8>>,
                            MinVsn,
                            <<"，请升级客户端"/utf8>>
                        ]),
                        {stop, elib_response:error(Req, Msg, ?ERR_E2EE_APP_VERSION_TOO_LOW)}
                end
            catch
                _:_ ->
                    %% 判定链故障（如 DB 不可用）不阻塞 E2EE 业务：本门是协议
                    %% rollout 门而非安全边界，fail-open 与 WS 升级提醒
                    %% maybe_send_upgrade_notice 的容错语义一致。
                    ok
            end
    end.

-spec is_e2ee_api_path(binary()) -> boolean().
is_e2ee_api_path(<<"/api/v1/e2ee/", _/binary>>) ->
    true;
is_e2ee_api_path(<<"/api/v1/group/set_e2ee_mode">>) ->
    true;
is_e2ee_api_path(_Path) ->
    false.

%% ===================================================================
%% F-EB10-1：企业租户面判定的特性裁剪保护
%% -------------------------------------------------------------------
%% eb_auth_principal 属 enterprise_business 特性专属模块：未选中时被
%% ERLC_EXCLUDE 物理排除（编译期不报错、运行期 undef）。中间件是全站
%% /api/v1 的必经路径，因此该调用以**函数级** -ifdef 保护（先例：
%% imboy_router 的 moment_api_routes/0）—— 未选中档编译产物零引用被裁
%% 模块，判定恒 false：企业路由在生成期 fail-closed 从未注册，普通
%% /api/v1 请求不受影响；选中档语义与原先完全一致。
%% ===================================================================
-ifdef(IMBOY_FEATURE_ENTERPRISE_BUSINESS).
-spec is_enterprise_tenant_path(binary()) -> boolean().
is_enterprise_tenant_path(Path) ->
    eb_auth_principal:is_tenant_surface_path(Path).
-else.
-spec is_enterprise_tenant_path(binary()) -> boolean().
is_enterprise_tenant_path(_Path) ->
    false.
-endif.

%% ===================================================================
%% CS-02：客服凭证面判定的特性裁剪保护
%% -------------------------------------------------------------------
%% cs_http 属 customer_service 特性专属模块：未选中时被 ERLC_EXCLUDE 物理排除。
%% 中间件是全站 /api/v1 必经路径，因此该调用同样以**函数级** -ifdef 保护
%% （F-EB10-1 同款）：未选中档编译产物零引用被裁模块，判定恒 false——
%% 访客/门店路径照常走签名 + JWT 门（即 fail-closed，不会误放行）；
%% 选中档语义见 cs_http:is_credential_surface_path/1。
%% ===================================================================
-ifdef(IMBOY_FEATURE_CUSTOMER_SERVICE).
-spec is_cs_credential_path(binary()) -> boolean().
is_cs_credential_path(Path) ->
    cs_http:is_credential_surface_path(Path).
-else.
-spec is_cs_credential_path(binary()) -> boolean().
is_cs_credential_path(_Path) ->
    false.
-endif.

%% ===================================================================
%% Web 坐席面判定的特性裁剪保护（IsCsCredentialPath 同款）：未选中
%% customer_service 档时恒 false——相关路由在生成期从未注册，普通 /api/v1
%% 请求不受影响；选中档语义见 cs_http:is_web_seat_surface_path/1。
%% ===================================================================
-ifdef(IMBOY_FEATURE_CUSTOMER_SERVICE).
-spec is_web_seat_path(binary()) -> boolean().
is_web_seat_path(Path) ->
    cs_http:is_web_seat_surface_path(Path).
-else.
-spec is_web_seat_path(binary()) -> boolean().
is_web_seat_path(_Path) ->
    false.
-endif.
