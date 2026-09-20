%%% @doc Widget 接入面的薄 Handler（CSB-03，plan v4.1 §12.4/§12.7）。
%%%
%%% 与 `cs_tenant_handler` 同职责同顺序（解析 → 验证 → 认证 → 调用 → 映射），
%%% 差异只在认证语义与两条接入面纪律：
%%%
%%%   1. **凭证 = bootstrap 令牌专用头**（`x-cs-visit-token`，与访客 visit token
%%%      同一 principal 类别）。凭证**只准走头**：查询串出现凭证样式键即 400
%%%      （`cs_http:credential_in_query_string/1`，值不读、不解析）。令牌校验
%%%      （digest 命中 (Org, installation) + 未吊销 + 未过期）在 application
%%%      用例内逐请求裁决（`cs_widget_support:verify_bootstrap_token/2`）——
%%%      handler 只做「缺头即 401」的 fail-closed 门；`widget_bootstrap` 是
%%%      签发点本身，令牌可选（携带即重放心跳）。
%%%   2. **Origin 头校验**（仅 bootstrap）：handler 用 domain
%%%      `cs_widget:normalize_origin/1` 做 scheme+host+port 归一（非法形状
%%%      400），归一值注入 `origin` 参数交给 application 与 installation
%%%      allowlist **精确**匹配（`cs_widget:origin_allowed/2`）——Origin 是
%%%      bootstrap 的必要条件，不是唯一认证：令牌/限流/租户 scope 照常生效。
%%%      动态 CORS：仅当请求成功才回 `Access-Control-Allow-Origin`
%%%      （echo 归一化后的**具体值**，绝不 `*`、绝不由此开 credentials）；
%%%      全局 CORS 由 `cors_middleware` 按既有口径先行。
%%%
%%%   * **动作 → facade → application**：只经 `cs_facade_call:call/3`（禁直连
%%%     DB / 禁 apply / 禁 crypto——cs_route_contract_tests 静态判据直扫源码）；
%%%   * 服务端派生键（动作表 `widget_server_derived/0`：时钟 / contact /
%%%     workspace / origin / secret / HMAC 材料与 Ctx 注入键）客户端**提供即
%%%     400**——浏览器永远不能自报服务端事实；
%%%   * TSID 全 string 出站（`cs_http:encode_entity/1`）。
%%%
%%% SSE（GET .../sessions/:id/events，`text/event-stream`）：
%%%   * 先发 `retry:` + 当前状态（`state`）resource-id 事件，再进轮询；
%%%   * 事件 id = 消息表**单调** after_id 序号（消息事件 id = 消息 id；
%%%     状态事件 id = 当前游标）——断线重连凭 `Last-Event-ID` 头（或
%%%     `after_id` 参数）从消息表 after 游标补偿，不重不漏；
%%%   * 无新事件的轮询周期性发注释行保活；不推任何明文 secret；
%%%   * 零新依赖：cowboy 2 原生 `stream_reply`/`stream_body`。
%%%
%%% **本模块不做**：不读库、不写 SQL、不做业务判定、不签发任何 URL。
-module(cs_widget_handler).

-export([init/2, handle/3]).

%% SSE 帧构造纯函数（导出仅供套件零 socket 断言；生产路径只在流循环内使用）。
-export([comment_frame/0, event_frame/3, message_data/1, retry_frame/1, state_data/2]).

%% SSE 缺省节奏（route Opts 可注入覆盖：sse_retry_ms / sse_poll_ms / sse_max_ms）。
-define(SSE_RETRY_MS, 5000).
-define(SSE_POLL_MS, 15000).
-define(SSE_MAX_MS, 300000).
-define(SSE_KEEPALIVE_EVERY, 3).
%% SSE 增量单次拉取上限（页大小上限 = cs_app_support:max_page_limit/0）。
-define(SSE_POLL_LIMIT, 200).

%% cowboy 普通 handler：State = route Opts（含 route metadata + 中间件会话键）。
-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0, undefined),
    Req = handle(Action, Req0, State0),
    {ok, Req, State0}.

%% @doc 单动作处理（导出以便契约测试直接驱动；生产路径由 init/2 调用）。
-spec handle(atom() | undefined, cowboy_req:req(), map()) -> cowboy_req:req().
handle(Action, Req0, State0) ->
    case cs_actions:widget(Action) of
        {error, {unknown_action, _}} ->
            cs_http:reply_error(Req0, {unknown_action, Action});
        {ok, Entry} when Action =:= widget_session_events ->
            events(Entry, Req0, State0);
        {ok, Entry} when Action =:= widget_asset_content ->
            %% BE-S01b：内容代理走同一解析→验证→认证→投影链，命中后响应是
            %% 对象字节本体（mime 定 content-type），不落 cs_http:respond 的
            %% JSON 面。
            asset_content(Entry, Req0, State0);
        {ok, Entry} when Action =:= widget_asset_put ->
            %% BE-PATCH-01：字节上传代理——payload=请求体字节（非 JSON 线格式，
            %% asset_content 同款线格式分支先例）；upload_ref 是唯一凭证（FE 裸
            %% PUT 合同：无凭证头），因此不走 token_credential 门。
            asset_put(Entry, Req0, State0);
        {ok, Entry} ->
            dispatch(Entry, Req0, State0)
    end.

%% ===================================================================
%% 普通动作：方法门 → 传输守卫 → Org → 凭证/Origin → 参数投影 → facade
%% ===================================================================

dispatch(Entry, Req0, State0) ->
    case cs_actions:case_for(Entry, cowboy_req:method(Req0)) of
        {error, method_not_allowed} ->
            cs_http:reply_error(Req0, method_not_allowed);
        {ok, Case} ->
            case cs_http:read_body(Req0) of
                {error, Reason} ->
                    cs_http:reply_error(Req0, Reason);
                {ok, Body} ->
                    guarded(Entry, Case, Req0, Body, State0)
            end
    end.

guarded(Entry, Case, Req0, Body, _State0) ->
    case transport_guard(Req0) of
        {error, Reason} ->
            cs_http:reply_error(Req0, Reason);
        ok ->
            case cs_http:org_id(Entry, Req0, Body) of
                {error, Reason} ->
                    cs_http:reply_error(Req0, Reason);
                {ok, OrgId} ->
                    authorize_and_invoke(Entry, Case, Req0, Body, OrgId)
            end
    end.

authorize_and_invoke(Entry, Case, Req0, Body, OrgId) ->
    OptionalToken = maps:get(action, Entry) =:= widget_bootstrap,
    case token_credential(Req0, OptionalToken) of
        {error, Reason} ->
            cs_http:reply_error(Req0, Reason);
        {ok, TokenDerived} ->
            case origin_guard(Entry, Req0) of
                {error, Reason} ->
                    cs_http:reply_error(Req0, Reason);
                {ok, OriginDerived} ->
                    Derived = maps:merge(
                        maps:merge(#{at => cs_http:now_sec()}, TokenDerived), OriginDerived
                    ),
                    invoke(Entry, Case, Req0, Body, OrgId, Derived)
            end
    end.

invoke(Entry, Case, Req0, Body, OrgId, Derived) ->
    case cs_http:build_params(Entry, Case, Req0, Body, Derived) of
        {error, Reason} ->
            cs_http:reply_error(Req0, Reason);
        {ok, Params} ->
            Result = cs_facade_call:call(maps:get(facade, Case), OrgId, Params),
            Req1 = dynamic_cors(Req0, origin_or_undefined(Req0), Result),
            cs_http:respond(Entry, Req1, Result)
    end.

%% 凭证传输守卫：查询串出现凭证样式键即 400（值不读——凡进入访问日志的
%% 凭证一律视为已泄漏）。凭证面只准走专用头，这是禁令不是偏好。
transport_guard(Req) ->
    case cs_http:credential_in_query_string(Req) of
        true ->
            {error, credential_in_query_string};
        false ->
            ok
    end.

%% 令牌凭证：只从专用头取；缺头 = 401（bootstrap 例外：令牌可选重放）。
token_credential(Req, Optional) ->
    case cowboy_req:header(cs_auth:visit_header(), Req) of
        Raw when is_binary(Raw), Raw =/= <<>> ->
            {ok, #{secret => Raw}};
        _ when Optional ->
            {ok, #{}};
        _ ->
            {error, credential_missing}
    end.

%% Origin 守卫（仅 bootstrap）：头缺失即 400；形状非法（含 path/userinfo/
%% 非法端口）即 400；合法值归一化后注入 `origin` 参数——allowlist 精确
%% 匹配在 application（`cs_widget:origin_allowed/2`）。
origin_guard(Entry, Req) ->
    case maps:get(action, Entry) =:= widget_bootstrap of
        false ->
            {ok, #{}};
        true ->
            case normalized_origin(Req) of
                {ok, Norm} -> {ok, #{origin => Norm}};
                {error, missing_origin} -> {error, {missing_param, origin}};
                {error, Reason} -> {error, Reason}
            end
    end.

normalized_origin(Req) ->
    case cowboy_req:header(<<"origin">>, Req) of
        undefined ->
            {error, missing_origin};
        Raw when is_binary(Raw) ->
            cs_widget:normalize_origin(Raw)
    end.

%% 请求成功时才可用于动态 CORS 的归一化 Origin（失败/无头 = undefined）。
origin_or_undefined(Req) ->
    case normalized_origin(Req) of
        {ok, Norm} -> Norm;
        _ -> undefined
    end.

%% 动态 CORS：只对**成功**且带合法 Origin 头的请求 echo 具体值（归一化后），
%% 附 `vary: Origin`。绝不回 `*`，绝不由此设置 credentials；拒绝响应不由本
%% handler 新增 ACAO（全局 allowlist 口径由 cors_middleware 按既有行为先行）。
dynamic_cors(Req, undefined, _Result) ->
    Req;
dynamic_cors(Req, Origin, {ok, _}) ->
    Req1 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, Origin, Req),
    cowboy_req:set_resp_header(<<"vary">>, <<"Origin">>, Req1);
dynamic_cors(Req, _Origin, _Error) ->
    Req.

%% ===================================================================
%% 内容代理（GET .../sessions/:id/assets/:asset_id/content，BE-S01b）
%% ===================================================================

%% 与普通动作同链（传输守卫 → Org → 令牌 → 参数投影），差只在响应映射：
%% 成功 = 对象字节本体（content-type = asset mime，private no-store，
%% 定长流式，零 URL / object key）；失败照常 JSON 结构化错误。
asset_content(Entry, Req0, State0) ->
    case cs_actions:case_for(Entry, cowboy_req:method(Req0)) of
        {error, method_not_allowed} ->
            cs_http:reply_error(Req0, method_not_allowed);
        {ok, Case} ->
            case cs_http:read_body(Req0) of
                {error, Reason} ->
                    cs_http:reply_error(Req0, Reason);
                {ok, Body} ->
                    content_guarded(Entry, Case, Req0, Body, State0)
            end
    end.

content_guarded(Entry, Case, Req0, Body, _State0) ->
    case transport_guard(Req0) of
        {error, Reason} ->
            cs_http:reply_error(Req0, Reason);
        ok ->
            case cs_http:org_id(Entry, Req0, Body) of
                {error, Reason} ->
                    cs_http:reply_error(Req0, Reason);
                {ok, OrgId} ->
                    content_authorized(Entry, Case, Req0, Body, OrgId)
            end
    end.

content_authorized(Entry, Case, Req0, Body, OrgId) ->
    %% 内容代理无 bootstrap 例外：令牌必填（缺头即 401，在响应前拒绝）。
    case token_credential(Req0, false) of
        {error, Reason} ->
            cs_http:reply_error(Req0, Reason);
        {ok, TokenDerived} ->
            Derived = maps:merge(#{at => cs_http:now_sec()}, TokenDerived),
            case cs_http:build_params(Entry, Case, Req0, Body, Derived) of
                {error, Reason} ->
                    cs_http:reply_error(Req0, Reason);
                {ok, Params} ->
                    Result = cs_facade_call:call(maps:get(facade, Case), OrgId, Params),
                    content_respond(Req0, origin_or_undefined(Req0), Result)
            end
    end.

content_respond(Req, _Origin, {error, Reason}) ->
    cs_http:reply_error(Req, Reason);
content_respond(Req, _Origin, {ok, #{body := Bytes} = View}) when is_binary(Bytes) ->
    Headers = #{
        <<"content-type">> => maps:get(mime, View, <<"application/octet-stream">>),
        <<"cache-control">> => <<"private, no-store">>,
        <<"x-asset-id">> => content_bin(maps:get(asset_id, View, undefined)),
        <<"x-asset-sha256">> => maps:get(object_hash, View, undefined)
    },
    Req1 = cowboy_req:stream_reply(200, Headers, Req),
    _ = cowboy_req:stream_body(Bytes, fin, Req1),
    Req1;
content_respond(Req, Origin, {ok, _Other}) ->
    %% 用例返回形状异常：fail-closed（不把未知形状当成功），但成功鉴权事实
    %% 已确立，动态 CORS echo 照常（与普通动作口径一致）。
    Req1 = dynamic_cors(Req, Origin, {ok, ok}),
    cs_http:reply_error(Req1, {invalid_argument, widget_asset_content}).

content_bin(undefined) ->
    <<>>;
content_bin(AssetId) when is_integer(AssetId) ->
    integer_to_binary(AssetId);
content_bin(Bin) when is_binary(Bin) ->
    Bin.

%% ===================================================================
%% 字节上传代理（POST .../sessions/:id/assets/upload，BE-PATCH-01）
%% ===================================================================

%% 与普通动作同链的前半段（传输守卫 → Org → 参数投影），差在两处线格式：
%%   1. **不要求**凭证头——本端点以 upload_ref（不透明、HMAC、TTL 900s、绑定
%%      Org/Workspace/conversation/actor/hash/size/mime）为唯一凭证，FE 裸 PUT
%%      合同（无凭证头/Cookie）；安装级 active 门与会话归属门在 application 用例。
%%   2. 请求体是**原始字节**（附件内容），不经 JSON 解码——先完成零成本参数
%%      校验（缺参 422 优先），再读原始体注入 `payload` 键进 facade。
asset_put(Entry, Req0, _State0) ->
    case cs_actions:case_for(Entry, cowboy_req:method(Req0)) of
        {error, method_not_allowed} ->
            cs_http:reply_error(Req0, method_not_allowed);
        {ok, Case} ->
            case transport_guard(Req0) of
                {error, Reason} ->
                    cs_http:reply_error(Req0, Reason);
                ok ->
                    case cs_http:org_id(Entry, Req0, #{}) of
                        {error, Reason2} ->
                            cs_http:reply_error(Req0, Reason2);
                        {ok, OrgId} ->
                            asset_put_params(Entry, Case, Req0, OrgId)
                    end
            end
    end.

asset_put_params(Entry, Case, Req0, OrgId) ->
    %% Derived 只注入服务端时钟（无令牌可派生 secret）；空正文投影让缺参在
    %% 读字节前 fail-fast（422），不先消费上传体。
    case cs_http:build_params(Entry, Case, Req0, #{}, #{at => cs_http:now_sec()}) of
        {error, Reason} ->
            cs_http:reply_error(Req0, Reason);
        {ok, Params0} ->
            case read_upload_body(Req0, []) of
                {error, Reason2} ->
                    cs_http:reply_error(Req0, Reason2);
                {ok, Payload, Req1} ->
                    Params = Params0#{payload => Payload},
                    Result = cs_facade_call:call(maps:get(facade, Case), OrgId, Params),
                    Req2 = dynamic_cors(Req1, origin_or_undefined(Req1), Result),
                    cs_http:respond(Entry, Req2, Result)
            end
    end.

%% 原始体累积读上限：与 presign 申报 size 的企业面上界一致（25 MiB）。
%% 本地常量而非跨单元引用——接口层只依赖 core/本 feature/facade（铁律 5）；
%% 真正的 size 语义校验（申报值=实际值、mime sniff、hash 复核）全部在企业
%% eb_asset_app:put_object，这里是纯传输 DoS 门（超限 400 fail-closed）。
-define(UPLOAD_MAX_BYTES, 25 * 1024 * 1024).

read_upload_body(Req, Acc) ->
    case cowboy_req:read_body(Req, #{length => 4 * 1024 * 1024, period => 10000}) of
        {ok, Data, Req1} ->
            {ok, iolist_to_binary(lists:reverse([Data | Acc])), Req1};
        {more, Data, Req1} ->
            case iolist_size(Acc) + byte_size(Data) > ?UPLOAD_MAX_BYTES of
                true -> {error, {invalid_param, payload}};
                false -> read_upload_body(Req1, [Data | Acc])
            end
    end.

%% ===================================================================
%% SSE（GET .../sessions/:id/events）
%% ===================================================================

events(Entry, Req0, State0) ->
    case cs_actions:case_for(Entry, cowboy_req:method(Req0)) of
        {error, method_not_allowed} ->
            cs_http:reply_error(Req0, method_not_allowed);
        {ok, Case} ->
            case cs_http:read_body(Req0) of
                {error, Reason} ->
                    cs_http:reply_error(Req0, Reason);
                {ok, Body} ->
                    events_guarded(Entry, Case, Req0, Body, State0)
            end
    end.

events_guarded(Entry, Case, Req0, Body, State0) ->
    case transport_guard(Req0) of
        {error, Reason} ->
            cs_http:reply_error(Req0, Reason);
        ok ->
            case cs_http:org_id(Entry, Req0, Body) of
                {error, Reason} ->
                    cs_http:reply_error(Req0, Reason);
                {ok, OrgId} ->
                    events_authorized(Entry, Case, Req0, Body, OrgId, State0)
            end
    end.

events_authorized(Entry, Case, Req0, Body, OrgId, State0) ->
    %% SSE 无 bootstrap 例外：令牌必填（缺头即 401，在开流之前拒绝）。
    case token_credential(Req0, false) of
        {error, Reason} ->
            cs_http:reply_error(Req0, Reason);
        {ok, TokenDerived} ->
            Derived = maps:merge(#{at => cs_http:now_sec()}, TokenDerived),
            case cs_http:build_params(Entry, Case, Req0, Body, Derived) of
                {error, Reason} ->
                    cs_http:reply_error(Req0, Reason);
                {ok, Params} ->
                    stream(Req0, OrgId, Params, State0)
            end
    end.

%% 开流前先做**会话归属裁决**（令牌 contact 的会话列表内必须有 :id）——
%% 不属于即 404，绝不把流开给越权会话（list_sessions 的 store 同语句过滤
%% 保证别的 contact 的会话根本不在列）。
stream(Req0, OrgId, Params, State0) ->
    case cs_facade_call:call(widget_list_sessions, OrgId, scoped(Params)) of
        {error, Reason} ->
            cs_http:reply_error(Req0, Reason);
        {ok, Sessions} when is_list(Sessions) ->
            SessionId = maps:get(session_id, Params),
            case [S || S <- Sessions, maps:get(id, S) =:= SessionId] of
                [Session] ->
                    start_stream(
                        Req0, OrgId, Params, maps:get(status, Session), SessionId, State0
                    );
                [] ->
                    cs_http:reply_error(Req0, {session_not_found, SessionId})
            end;
        {ok, _Other} ->
            cs_http:reply_error(Req0, {unknown_action, widget_session_events})
    end.

start_stream(Req0, OrgId, Params, Status, SessionId, State0) ->
    Cursor = initial_cursor(Req0, Params),
    Req1 = cowboy_req:stream_reply(200, stream_headers(origin_for_stream(Req0)), Req0),
    First = [
        retry_frame(retry_ms(State0)),
        event_frame(Cursor, <<"state">>, state_data(SessionId, Status))
    ],
    case send(Req1, First) of
        ok ->
            Deadline = erlang:monotonic_time(millisecond) + max_ms(State0),
            stream_loop(
                Req1,
                OrgId,
                Params,
                #{
                    cursor => Cursor,
                    status => Status,
                    session_id => SessionId,
                    deadline => Deadline,
                    empty => 0,
                    poll_ms => poll_ms(State0)
                }
            );
        {error, _} ->
            %% 客户端在首帧前即断开：静默收流（socket 归属已交还 ranch）。
            Req1
    end.

%% 轮询主循环：到 deadline 即 fin（客户端凭 Last-Event-ID 无损重连续传）。
stream_loop(Req, OrgId, Params, #{deadline := Deadline} = Ctx) ->
    case erlang:monotonic_time(millisecond) >= Deadline of
        true ->
            finish(Req);
        false ->
            timer:sleep(maps:get(poll_ms, Ctx)),
            stream_step(Req, OrgId, Params, Ctx)
    end.

stream_step(Req, OrgId, Params, Ctx) ->
    #{cursor := Cursor} = Ctx,
    case poll_messages(OrgId, Params, Cursor) of
        {ok, Messages} when is_list(Messages) ->
            deliver(Req, OrgId, Params, Ctx, sort_messages(Messages));
        {error, Reason} ->
            case is_stream_fatal(Reason) of
                true ->
                    %% DF-10 修复：凭证/安装终态失效（吊销族）不是瞬态错误——
                    %% 立即 fin 关流，客户端重连走 4xx fail-closed，访客侧
                    %% banner 呈现断线/重连态（撤权可感知降级）。
                    finish(Req);
                false ->
                    %% 单次轮询失败（瞬态，如 DB 抖动）不终止流；也不推进保活计数。
                    stream_loop(Req, OrgId, Params, Ctx)
            end
    end.

%% 凭证/接入的终态失效词汇：流保持已无意义（重试只会持续 4xx），且访客侧
%% 必须可感知降级。瞬态错误（DB 抖动等）不在此列，维持静默续流。
is_stream_fatal(token_revoked) -> true;
is_stream_fatal(token_expired) -> true;
is_stream_fatal(revoked) -> true;
is_stream_fatal(installation_revoked) -> true;
is_stream_fatal(identity_key_revoked) -> true;
is_stream_fatal(_) -> false.

%% 逐条发新消息帧（id 严格递增）→ 状态变更检查 → 空轮询保活注释。
deliver(Req, OrgId, Params, Ctx, Messages) ->
    #{cursor := Cursor0, empty := Empty0} = Ctx,
    {Frames, Cursor} = message_frames(Messages, Cursor0, []),
    case Frames of
        [] ->
            state_and_keepalive(Req, OrgId, Params, Ctx#{cursor => Cursor, empty => Empty0 + 1});
        _ ->
            case send(Req, Frames) of
                ok ->
                    state_and_keepalive(
                        Req, OrgId, Params, Ctx#{cursor => Cursor, empty => 0}
                    );
                {error, _} ->
                    Req
            end
    end.

%% 状态变更即发 `state` 事件（id = 当前游标，单调不回退）；无变更时按空轮询
%% 计数周期性发保活注释行。
state_and_keepalive(Req, OrgId, Params, Ctx = #{session_id := SessionId, cursor := Cursor}) ->
    case poll_status(OrgId, Params, SessionId) of
        {ok, Status} when is_atom(Status); is_binary(Status) ->
            case Status =/= maps:get(status, Ctx) of
                true ->
                    Frame = event_frame(Cursor, <<"state">>, state_data(SessionId, Status)),
                    case send(Req, Frame) of
                        ok ->
                            stream_loop(Req, OrgId, Params, Ctx#{status => Status, empty => 0});
                        {error, _} ->
                            Req
                    end;
                false ->
                    keepalive(Req, OrgId, Params, Ctx)
            end;
        _ ->
            keepalive(Req, OrgId, Params, Ctx)
    end.

keepalive(Req, OrgId, Params, Ctx = #{empty := Empty}) ->
    case Empty > 0 andalso Empty rem ?SSE_KEEPALIVE_EVERY =:= 0 of
        true ->
            case send(Req, comment_frame()) of
                ok ->
                    stream_loop(Req, OrgId, Params, Ctx);
                {error, _} ->
                    Req
            end;
        false ->
            stream_loop(Req, OrgId, Params, Ctx)
    end.

poll_status(OrgId, Params, SessionId) ->
    case cs_facade_call:call(widget_list_sessions, OrgId, scoped(Params)) of
        {ok, Sessions} when is_list(Sessions) ->
            case [S || S <- Sessions, maps:get(id, S) =:= SessionId] of
                [Session] -> {ok, maps:get(status, Session)};
                [] -> {ok, missing}
            end;
        _ ->
            error
    end.

%% 收敛消息帧：只取 id > Cursor 的（严格递增——不重），一次 write 合并。
message_frames([], Cursor, Acc) ->
    {lists:reverse(Acc), Cursor};
message_frames([Message | Rest], Cursor, Acc) ->
    Id = message_id(Message),
    case Id > Cursor of
        true ->
            Frame = event_frame(Id, <<"message">>, message_data(Message)),
            message_frames(Rest, Id, [Frame | Acc]);
        false ->
            message_frames(Rest, Cursor, Acc)
    end.

message_id(Message) when is_map(Message) ->
    case maps:get(id, Message, 0) of
        Id when is_integer(Id) -> Id;
        _ -> 0
    end;
message_id(_Other) ->
    0.

poll_messages(OrgId, Params, Cursor) ->
    Base = scoped(Params#{limit => ?SSE_POLL_LIMIT}),
    WithCursor =
        case Cursor > 0 of
            true -> Base#{after_id => Cursor};
            false -> Base
        end,
    cs_facade_call:call(widget_history_after, OrgId, WithCursor).

sort_messages(Messages) ->
    lists:sort(fun(A, B) -> message_id(A) =< message_id(B) end, Messages).

%% facade 参数收敛：只留注入键 + 游标/页大小键 + session_id（DF-5：流内
%% `widget_history_after` 补偿读与 REST 历史同源，真 facade 的
%% `visitor_session_scope` 需要它裁决会话归属——丢键即补偿读恒
%% invalid_argument，被 stream_step 静默吞掉，message/state 帧全死）。
scoped(Params) ->
    maps:with(
        [installation_id, secret, at, store, id, default_workspace, digest, limit, session_id],
        Params
    ).

%% 断线重连游标：`Last-Event-ID` 头优先，其次 `after_id` 参数，缺省 0
%% （0 = 全量补偿——「不漏」优先，客户端可自行丢弃）。
initial_cursor(Req, Params) ->
    case cowboy_req:header(<<"last-event-id">>, Req) of
        Raw when is_binary(Raw) ->
            case cursor_of(Raw) of
                {ok, Id} -> Id;
                error -> maps:get(after_id, Params, 0)
            end;
        _ ->
            maps:get(after_id, Params, 0)
    end.

cursor_of(Raw) ->
    try
        case binary_to_integer(Raw) of
            Id when Id >= 0 -> {ok, Id};
            _ -> error
        end
    catch
        _:_ -> error
    end.

origin_for_stream(Req) ->
    origin_or_undefined(Req).

stream_headers(undefined) ->
    base_stream_headers();
stream_headers(Origin) ->
    (base_stream_headers())#{
        <<"access-control-allow-origin">> => Origin,
        <<"vary">> => <<"Origin">>
    }.

base_stream_headers() ->
    #{
        <<"content-type">> => <<"text/event-stream; charset=utf-8">>,
        <<"cache-control">> => <<"no-cache">>
    }.

retry_ms(State) ->
    positive(maps:get(sse_retry_ms, State, ?SSE_RETRY_MS)).

poll_ms(State) ->
    positive(maps:get(sse_poll_ms, State, ?SSE_POLL_MS)).

max_ms(State) ->
    positive(maps:get(sse_max_ms, State, ?SSE_MAX_MS)).

positive(N) when is_integer(N), N > 0 ->
    N;
positive(_) ->
    1.

%% 单次写出；cowboy 断连/错误统一折叠为 {error, _}（socket 关闭由框架收尾）。
%% stream_body 的非 ok 返回不走 of 分支（case_clause 被 catch 收拢）。
send(Req, IoData) ->
    try
        _ = cowboy_req:stream_body(IoData, nofin, Req),
        ok
    catch
        _:_ -> {error, stream_closed}
    end.

finish(Req) ->
    try cowboy_req:stream_body(<<>>, fin, Req) of
        _ -> Req
    catch
        _:_ -> Req
    end.

%% ===================================================================
%% SSE 帧构造（纯函数；text/event-stream 语法）
%% ===================================================================

%% @doc 单事件帧：`id`（单调序号 = 重连游标）+ `event`（类型）+ `data`（JSON）。
-spec event_frame(integer(), binary(), binary()) -> binary().
event_frame(Id, Event, Data) when is_integer(Id), is_binary(Event), is_binary(Data) ->
    <<"id: ", (integer_to_binary(Id))/binary, "\nevent: ", Event/binary, "\ndata: ", Data/binary,
        "\n\n">>;
event_frame(_Id, _Event, _Data) ->
    <<>>.

%% @doc 重连建议帧（`retry:` 字段；与首个事件同块派发即生效）。
-spec retry_frame(pos_integer()) -> binary().
retry_frame(Ms) when is_integer(Ms), Ms > 0 ->
    <<"retry: ", (integer_to_binary(Ms))/binary, "\n">>;
retry_frame(_) ->
    <<>>.

%% @doc 保活注释行（SSE 注释 = 冒号开头行；不产生事件）。
-spec comment_frame() -> binary().
comment_frame() ->
    <<": keep-alive\n\n">>.

%% @doc 状态事件的 data：resource-id + 状态（TSID string；零 secret）。
-spec state_data(integer(), atom() | binary()) -> binary().
state_data(SessionId, Status) when is_integer(SessionId) ->
    jsx:encode(
        cs_http:encode_entity(#{
            resource => <<"cs.session">>,
            session_id => SessionId,
            status => status_bin(Status)
        })
    );
state_data(_SessionId, _Status) ->
    <<"{}">>.

%% @doc 消息事件的 data：投影白名单后出站（TSID string；零 secret）。
-spec message_data(map()) -> binary().
message_data(Message) when is_map(Message) ->
    Projected = maps:with(
        [id, conversation_id, body, mime, size_bytes, created_at, sender_kind, kind], Message
    ),
    jsx:encode(cs_http:encode_entity(Projected));
message_data(_Other) ->
    <<"{}">>.

status_bin(Status) when is_atom(Status) ->
    atom_to_binary(Status, utf8);
status_bin(Status) when is_binary(Status) ->
    Status;
status_bin(_Other) ->
    <<"unknown">>.
