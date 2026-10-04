%%% @doc 企业**租户面**薄 Handler（EB-09，plan §5.1）。
%%%
%%% 依据：plan v4.1 §5.1、§5.4、EB-09-A01..A06、`docs/architecture/feature-slice-rules.md`
%%% 铁律 2（层名与依赖方向）。
%%%
%%% Handler 只做四件事，顺序固定：
%%%
%%%   1. **解析**：路径绑定（`org_id` / 资源 id）+ `workspace_id` + JSON 正文；
%%%   2. **验证**：方法门（405）、动作表白名单投影（表外键忽略、客户端派生键 400、
%%%      缺必填 422）、租户归属（`org_id` 必须来自 path）；
%%%   3. **授权**：`eb_auth_app:authorize/2`（principal 由 route metadata 决定；
%%%      逐请求加载事实；suspended/removed 立即失效）；
%%%   4. **调用 + 映射**：`eb_enterprise_facade_call:call/3`（**只**进 facade），
%%%      结果 → HTTP（200 / 流式 200 / 400 / 401 / 403 / 404 / 409 / 422 / 500）。
%%%
%%% 三条红线在本模块的落点：
%%%   * **asset content** 走 `proxy_content` 动作 ⇒ 经 facade 取流后**流式**写出，
%%%     不调个人 `view_url`、不签发 GET URL、响应里不含 object key / storage
%%%     endpoint / presigned（见 `eb_enterprise_http:reply_content/2`）；
%%%   * **message ACK** 是 `delivery_only` 动作 ⇒ 只调 delivery 用例，响应**不含**
%%%     删除/归档/回收语义（A06；契约断言见 `eb_route_contract_tests`）；
%%%   * 平台面不在本模块（见 `eb_platform_handler`）。
%%%
%%% CS-BE-01B（坐席发送面协议补全）的两个线格式落点：
%%%   * presign 的 **PUT 用例**是字节上传（原始体注入 `payload`，非 JSON），
%%%     与 presign POST 同路径、同路由级授权门，凭证复核在
%%%     `eb_asset_app:put_object`（widget 面 widget_asset_put 同款协议）；
%%%   * presign POST 成功视图补 `upload.url`（本 API 的 PUT 目标；短 TTL
%%%     凭证在查询串，object key / 存储引用零出站）。
%%%
%%% **本模块不做**：不读库、不写 SQL、不做业务判定、不拼 SQL、不缓存事实。
-module(eb_tenant_handler).

-moduledoc "企业租户面薄 Handler（EB-09，plan §5.1）。".
-export([init/2, handle/3]).

%% cowboy 普通 handler：State = route Opts（含 route metadata + 中间件会话键）。
-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0, undefined),
    Req = handle(Action, Req0, State0),
    {ok, Req, State0}.

%% @doc 单动作处理（导出以便契约测试直接驱动；生产路径由 `init/2` 调用）。
-spec handle(atom() | undefined, cowboy_req:req(), map()) -> cowboy_req:req().
handle(Action, Req0, State0) ->
    case eb_enterprise_actions:tenant(Action) of
        {error, {unknown_action, _}} ->
            eb_enterprise_http:reply_error(Req0, {unknown_action, Action});
        {ok, Entry} ->
            case eb_enterprise_actions:case_for(Entry, cowboy_req:method(Req0)) of
                {error, method_not_allowed} ->
                    eb_enterprise_http:reply_error(Req0, method_not_allowed);
                {ok, Case} ->
                    dispatch(Entry, Case, Req0, State0)
            end
    end.

dispatch(Entry, Case, Req0, State0) ->
    case eb_enterprise_http:org_id(Req0) of
        {error, Reason} ->
            eb_enterprise_http:reply_error(Req0, Reason);
        {ok, OrgId} ->
            State = State0#{organization_id => OrgId},
            case eb_enterprise_http:authorize(Entry, Case, Req0, State) of
                {error, Reason} ->
                    eb_enterprise_http:reply_error(Req0, Reason);
                {ok, AuthContext} ->
                    invoke(Entry, Case, Req0, OrgId, AuthContext)
            end
    end.

invoke(Entry, Case, Req0, OrgId, AuthContext) ->
    case maps:get(facade, Case) of
        %% CS-BE-01B：字节上传线格式（presign 的 PUT 用例）——请求体是原始
        %% 字节而非 JSON，线格式分支在此（widget 面 cs_widget_handler 的
        %% asset_put 同款先例）：先按查询串投影参数（缺参在读字节前
        %% fail-fast），再读原始体注入 `payload` 键。
        put_object ->
            invoke_upload(Entry, Case, Req0, OrgId, AuthContext);
        _JsonCase ->
            invoke_json(Entry, Case, Req0, OrgId, AuthContext)
    end.

invoke_json(Entry, Case, Req0, OrgId, AuthContext) ->
    case eb_enterprise_http:read_body(Req0) of
        {error, Reason} ->
            eb_enterprise_http:reply_error(Req0, Reason);
        {ok, Body} ->
            case eb_enterprise_http:workspace_id(Req0, Body) of
                {error, Reason} ->
                    eb_enterprise_http:reply_error(Req0, Reason);
                {ok, WorkspaceId} ->
                    Ctx = #{
                        organization_id => OrgId,
                        workspace_id => WorkspaceId,
                        actor_user_id => actor_user_id(Entry, AuthContext),
                        %% F-SEC-01：调用者自己的业务身份（认证事实派生，客户端不可报）。
                        caller_identity_id => caller_identity_id(AuthContext),
                        %% BE-S01a：调用者的职能类型（同源认证事实派生）——
                        %% eb_message_app 的 ACK 坐席经办门据此分流（customer_service
                        %% 走 ACL；sales 保持原行为），客户端不可申报。
                        caller_function_key => caller_function_key(AuthContext)
                    },
                    case eb_enterprise_http:build_params(Entry, Case, Req0, Body, Ctx) of
                        {error, Reason} ->
                            eb_enterprise_http:reply_error(Req0, Reason);
                        {ok, Params} ->
                            Result = eb_enterprise_facade_call:call(
                                maps:get(facade, Case), OrgId, Params
                            ),
                            %% CS-BE-01B：presign（POST）成功视图补 upload.url
                            %% （指向本 API 的 PUT 用例；非 presign 原样透传）。
                            Decorated = seat_send_view(Entry, Req0, WorkspaceId, Result),
                            eb_enterprise_http:respond(Entry, Req0, Decorated)
                    end
            end
    end.

invoke_upload(Entry, Case, Req0, OrgId, AuthContext) ->
    %% Body 置空：参数只来自查询串（presign 下发的 upload.url 已把
    %% workspace_id / upload_ref 拼进查询串），缺参 422 在消费上传体之前。
    case eb_enterprise_http:workspace_id(Req0, #{}) of
        {error, Reason} ->
            eb_enterprise_http:reply_error(Req0, Reason);
        {ok, WorkspaceId} ->
            Ctx = #{
                organization_id => OrgId,
                workspace_id => WorkspaceId,
                actor_user_id => actor_user_id(Entry, AuthContext),
                caller_identity_id => caller_identity_id(AuthContext),
                caller_function_key => caller_function_key(AuthContext)
            },
            case eb_enterprise_http:build_params(Entry, Case, Req0, #{}, Ctx) of
                {error, Reason} ->
                    eb_enterprise_http:reply_error(Req0, Reason);
                {ok, Params} ->
                    case eb_enterprise_http:read_upload_body(Req0) of
                        {error, Reason} ->
                            eb_enterprise_http:reply_error(Req0, Reason);
                        {ok, Payload, _Req1} ->
                            Result = eb_enterprise_facade_call:call(
                                put_object, OrgId, Params#{payload => Payload}
                            ),
                            eb_enterprise_http:respond(Entry, Req0, Result)
                    end
            end
    end.

%% presign 动作的 POST 成功视图补 `upload.url`；其余动作/方法原样透传。
seat_send_view(#{action := presign}, Req0, WorkspaceId, Result) ->
    case cowboy_req:method(Req0) of
        <<"POST">> ->
            eb_enterprise_http:with_seat_upload_url(Req0, WorkspaceId, Result);
        _OtherMethod ->
            Result
    end;
seat_send_view(_Entry, _Req0, _WorkspaceId, Result) ->
    Result.

%% 操作人（审计快照）：租户面取自**认证主体**（`eb_auth_app` 判定出的 `user_id`），
%% 客户端无法自报（动作表把 `actor_user_id` 列为禁止的客户端键）。
actor_user_id(_Entry, #{user_id := Uid}) when is_integer(Uid) -> Uid;
actor_user_id(_Entry, _AuthContext) -> undefined.

%% F-SEC-01：调用者本人的业务身份（active assignment 派生）。发送者归属的
%% 权威来源：application 层用它覆盖/校验客户端自报的 identity_id。
caller_identity_id(#{business_identity_id := Bid}) when is_integer(Bid) -> Bid;
caller_identity_id(_AuthContext) -> undefined.

%% BE-S01a：调用者的职能类型（eb_auth_app 选中的 assignment 派生；同样不可
%% 自报）。ACK 等动作的坐席经办 ACL 用它区分 sales / customer_service 行为。
caller_function_key(#{function_key := Fk}) when is_binary(Fk) -> Fk;
caller_function_key(_AuthContext) -> undefined.
