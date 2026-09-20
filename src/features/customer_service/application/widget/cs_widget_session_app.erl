%%% @doc Widget 访客会话生命周期的应用层用例（CSB-02）：
%%% create/list/message/history/rating/asset。
%%%
%%% 依据：plan v4.1 §12.4（Widget API 最小合同）、§12.7 CSB-02、EB-D08。
%%% 接入面（bootstrap / identity exchange）在 `cs_widget_app`；跨裁剪出口与
%%% 机械辅助在 `cs_widget_support`。
%%%
%%% 安全合同：
%%%   * 令牌作用域：digest 命中 (Org, installation) + 未吊销 + 未过期；
%%%     contact 恒取自令牌行（服务端派生），浏览器申报值一律不读——每个用例
%%%     先用 `maps:with` 白名单收敛入参，再注入派生值；
%%%   * 默认 Workspace / 接待 identity 是服务端注入事实（缺注入 fail-closed）；
%%%   * conversation 服务端经 `enterprise_business_facade:open_conversation`
%%%     打开（合成 consent 由 enterprise 侧裁决）；session 复用既有
%%%     `cs_session_app:open_session` 路径（业务规则零复制）；
%%%   * 消息/历史/附件**只经 enterprise 真源**：出站统一是
%%%     `cs_session_app:append_session_message` / `enterprise_business_facade`
%%%     （经 `cs_widget_support` 的跨裁剪出口），本 feature 零副本表；
%%%   * rating 状态机（仅 closed、1..5、不可重复）由 domain `cs_session`
%%%     与 store CAS 承担，本模块只做令牌作用域与会话归属裁决。
-module(cs_widget_session_app).

-export([
    create_session/2,
    list_sessions/2,
    history_after/2,
    visitor_message/2,
    rate/2,
    asset_presign/2,
    asset_confirm/2,
    %% BE-PATCH-01：访客附件字节上传代理
    asset_put/2,
    %% BE-S01b：访客附件内容代理
    asset_content/2
]).

%% ===================================================================
%% 访客开会话（服务端派生 Org/Workspace/contact/conversation）
%% ===================================================================

%% @doc 访客开会话：服务端派生默认 Workspace / contact（取自令牌行）/
%% conversation（服务端经 enterprise 打开），再复用既有
%% `cs_session_app:open_session` 路径建 queued session。
%%
%% 必填注入：default_workspace（fun/1）、intake_business_identity_id（本 Org
%% 的接待 identity——服务端事实，浏览器不可申报）。同一 contact 已有未关闭
%% 会话时 `{error, {session_already_open, SessionId}}`（会话侧幂等）。
-spec create_session(integer(), map()) -> {ok, map()} | {error, term()}.
create_session(OrgId, Params) when is_map(Params) ->
    case cs_widget_support:verify_bootstrap_token(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, Token} ->
            create_session_in(OrgId, Token, Params)
    end;
create_session(_OrgId, _Params) ->
    {error, {invalid_argument, create_session}}.

create_session_in(OrgId, Token, Params) ->
    InstallationId = maps:get(installation_id, Params),
    case cs_widget_support:fetch_installation(Params, OrgId, InstallationId) of
        {error, _} = Err ->
            Err;
        {ok, Installation} ->
            case cs_widget_support:installation_active(Installation) of
                {error, _} = Err2 ->
                    Err2;
                {ok, Active} ->
                    open_visitor_session(OrgId, Active, Token, Params)
            end
    end.

open_visitor_session(OrgId, Installation, Token, Params) ->
    ContactId = maps:get(contact_id, Token),
    case cs_widget_support:resolve_default_workspace(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            case cs_widget_support:intake_identity(Params) of
                {error, _} = Err2 ->
                    Err2;
                {ok, IntakeId} ->
                    ensure_single_open(
                        OrgId, Installation, Token, ContactId, WorkspaceId, IntakeId, Params
                    )
            end
    end.

ensure_single_open(OrgId, Installation, Token, ContactId, WorkspaceId, IntakeId, Params) ->
    case existing_open_session(OrgId, ContactId, WorkspaceId, Params) of
        {error, _} = Err ->
            Err;
        {ok, undefined} ->
            create_session_rows(
                OrgId, Installation, Token, ContactId, WorkspaceId, IntakeId, Params
            );
        {ok, SessionId} ->
            {error, {session_already_open, SessionId}}
    end.

existing_open_session(OrgId, ContactId, WorkspaceId, Params) ->
    Clean = maps:with([store, id], Params),
    case
        cs_widget_support:session_list_contact(OrgId, Clean#{
            workspace_id => WorkspaceId, contact_id => ContactId
        })
    of
        {error, _} = Err ->
            Err;
        {ok, Sessions} ->
            {ok, open_session_id(Sessions)}
    end.

open_session_id(Sessions) ->
    Open = [S || S <- Sessions, maps:get(status, S) =/= closed],
    case Open of
        [S | _] -> maps:get(id, S);
        [] -> undefined
    end.

create_session_rows(OrgId, Installation, Token, ContactId, WorkspaceId, IntakeId, Params) ->
    case open_visitor_conversation(OrgId, ContactId, WorkspaceId, IntakeId, Params) of
        {error, _} = Err ->
            Err;
        {ok, ConversationId} ->
            insert_visitor_session(
                OrgId, Installation, Token, ContactId, WorkspaceId, ConversationId, Params
            )
    end.

%% conversation 只经 enterprise facade 打开（合成 consent 由 enterprise 侧裁决）。
open_visitor_conversation(OrgId, ContactId, WorkspaceId, IntakeId, Params) ->
    EbParams = cs_widget_support:eb_params(Params, #{
        workspace_id => WorkspaceId,
        contact_id => ContactId,
        business_identity_id => IntakeId,
        default_workspace => maps:get(default_workspace, Params)
    }),
    case cs_widget_support:eb_open_conversation(OrgId, EbParams) of
        {ok, Result} ->
            {ok, maps:get(conversation_id, Result)};
        {error, {conversation_exists, ConversationId}} ->
            {ok, ConversationId};
        {error, _} = Err ->
            Err
    end.

insert_visitor_session(OrgId, Installation, Token, ContactId, WorkspaceId, ConversationId, Params) ->
    Clean = maps:with([store, id], Params),
    case
        cs_widget_support:session_open(OrgId, Clean#{
            workspace_id => WorkspaceId,
            contact_id => ContactId,
            conversation_id => ConversationId,
            visit_token_id => maps:get(id, Token),
            at => maps:get(at, Params)
        })
    of
        {error, _} = Err ->
            Err;
        {ok, Session} ->
            _ = cs_widget_support:append_event(Params, OrgId, WorkspaceId, #{
                actor_kind => <<"visitor">>,
                action => <<"widget.session_created">>,
                detail => #{
                    <<"installation_id">> => maps:get(id, Installation),
                    <<"session_id">> => maps:get(id, Session)
                }
            }),
            {ok, #{
                session_id => maps:get(id, Session),
                conversation_id => ConversationId,
                contact_id => ContactId,
                workspace_id => WorkspaceId,
                installation_id => maps:get(id, Installation),
                status => maps:get(status, Session)
            }}
    end.

%% ===================================================================
%% 访客视角读取 / 消息 / 评分 / 附件
%% ===================================================================

%% @doc 访客视角会话列表：只列令牌绑定 contact 的会话（读取边界在
%% `cs_session_app:list_contact_sessions` 的 store 同语句过滤）。
-spec list_sessions(integer(), map()) -> {ok, [map()]} | {error, term()}.
list_sessions(OrgId, Params) when is_map(Params) ->
    case visitor_scope(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, #{workspace_id := WorkspaceId, contact_id := ContactId}} ->
            Clean = maps:with([store, id], Params),
            case
                cs_widget_support:session_list_contact(OrgId, Clean#{
                    workspace_id => WorkspaceId, contact_id => ContactId
                })
            of
                {error, _} = Err2 ->
                    Err2;
                {ok, Sessions} ->
                    {ok, [visitor_session_view(S) || S <- Sessions]}
            end
    end;
list_sessions(_OrgId, _Params) ->
    {error, {invalid_argument, list_sessions}}.

%% 列表投影白名单：内部审计列（visit_token_id / close_reason）不出访客面。
visitor_session_view(Session) ->
    maps:with(
        [
            id,
            organization_id,
            workspace_id,
            conversation_id,
            business_identity_id,
            status,
            rating,
            queued_at,
            claimed_at,
            closed_at,
            version
        ],
        Session
    ).

%% @doc 访客历史（只读，after_id 键集）：先证明会话属于令牌 contact，
%% 再经 `enterprise_business_facade:list_messages` 读企业真源。
-spec history_after(integer(), map()) -> {ok, map()} | {error, term()}.
history_after(OrgId, Params) when is_map(Params) ->
    case visitor_session_scope(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, #{workspace_id := WorkspaceId, session := Session}} ->
            Base = #{
                workspace_id => WorkspaceId,
                conversation_id => maps:get(conversation_id, Session)
            },
            WithCursor = cs_widget_support:optional_keys(Base, Params, [after_id, limit]),
            cs_widget_support:eb_list_messages(
                OrgId, cs_widget_support:eb_params(Params, WithCursor)
            )
    end;
history_after(_OrgId, _Params) ->
    {error, {invalid_argument, history_after}}.

%% @doc 访客入站消息：sender 恒为令牌 contact（服务端派生），唯一写入路径是
%% `cs_session_app:append_session_message` → `enterprise_business_facade`。
%% client_msg_id 幂等口径由 enterprise 侧冻结语义承担。
%%
%% BE-PATCH-01（attachment-state-machine append_message）：`asset_ids` 为 TSID
%% string 数组（传输形态），此处投影为 pos int 交企业面 canonical 事务做绑定
%% （单事务锁定 active+unbound asset + 写 message_id，widget 不复制绑定逻辑）。
%% 投影失败 = 形状取值不成立（{invalid_param, asset_ids}，400 面）。
-spec visitor_message(integer(), map()) -> {ok, map()} | {error, term()}.
visitor_message(OrgId, Params) when is_map(Params) ->
    case project_asset_ids(maps:get(asset_ids, Params, undefined)) of
        {error, _} = Err ->
            Err;
        {ok, AssetIds} ->
            visitor_message_scoped(OrgId, Params, AssetIds)
    end;
visitor_message(_OrgId, _Params) ->
    {error, {invalid_argument, visitor_message}}.

visitor_message_scoped(OrgId, Params, AssetIds) ->
    case visitor_session_scope(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, #{workspace_id := WorkspaceId, contact_id := ContactId}} ->
            Clean0 = maps:with(
                [
                    store,
                    id,
                    key_ref,
                    canonical_tx,
                    accepted_at,
                    session_id,
                    client_msg_id,
                    body,
                    asset_ids
                ],
                Params
            ),
            Clean =
                case AssetIds of
                    [] -> Clean0;
                    _ -> Clean0#{asset_ids => AssetIds}
                end,
            cs_widget_support:session_append_message(OrgId, Clean#{
                workspace_id => WorkspaceId, contact_id => ContactId
            })
    end.

%% TSID string 数组 → pos int 数组（传输投影；elib_tsid 是 core，application
%% 可依赖）。元素十进制字符串或 number 兼容（与 cs_http:tsid 同口径）。
project_asset_ids(undefined) ->
    {ok, []};
project_asset_ids(Ids) when is_list(Ids) ->
    project_asset_ids(Ids, []);
project_asset_ids(_Other) ->
    {error, {invalid_param, asset_ids}}.

project_asset_ids([], Acc) ->
    {ok, lists:reverse(Acc)};
project_asset_ids([Raw | Rest], Acc) ->
    case tsid_value(Raw) of
        {ok, Id} when Id > 0 -> project_asset_ids(Rest, [Id | Acc]);
        _ -> {error, {invalid_param, asset_ids}}
    end.

tsid_value(Raw) when is_integer(Raw), Raw > 0 ->
    {ok, Raw};
tsid_value(Raw) when is_binary(Raw) ->
    elib_tsid:from_binary(Raw);
tsid_value(_Raw) ->
    error.

%% @doc 访客评分：仅 closed、1..5、不可重复（状态机由 `cs_session` 域真源
%% 与 store CAS 承担，本用例只做令牌作用域与会话归属裁决）。
-spec rate(integer(), map()) -> {ok, map()} | {error, term()}.
rate(OrgId, Params) when is_map(Params) ->
    case visitor_session_scope(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, #{workspace_id := WorkspaceId}} ->
            Clean = maps:with(
                [store, id, session_id, rating, expected_version, at], Params
            ),
            cs_widget_support:session_rate(OrgId, Clean#{workspace_id => WorkspaceId})
    end;
rate(_OrgId, _Params) ->
    {error, {invalid_argument, rate}}.

%% @doc 访客附件预签名：会话归属裁决后经 `enterprise_business_facade` 走
%% 企业私有 PUT（confirm 在 enterprise 侧重新鉴权）。
-spec asset_presign(integer(), map()) -> {ok, map()} | {error, term()}.
asset_presign(OrgId, Params) when is_map(Params) ->
    case visitor_session_scope(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, #{workspace_id := WorkspaceId, contact_id := ContactId, session := Session}} ->
            EbParams = cs_widget_support:eb_params(
                Params,
                #{
                    workspace_id => WorkspaceId,
                    conversation_id => maps:get(conversation_id, Session),
                    mime => maps:get(mime, Params, undefined),
                    size_bytes => maps:get(size_bytes, Params, undefined),
                    %% CSB-02S D6 补全：object_hash 由 widget 面动作表收齐后
                    %% 透传（企业面 request_presign 必填，PUT 后服务端复核）。
                    object_hash => maps:get(object_hash, Params, undefined),
                    %% CSB-02S D6：访客主体（令牌 contact，服务端派生）进企业
                    %% 面的访客作用域分支——企业面 member 校验照旧不放宽。
                    actor_contact_id => ContactId
                }
            ),
            case cs_widget_support:eb_request_presign(OrgId, EbParams) of
                {ok, View} ->
                    {ok, with_upload_url(OrgId, Params, Session, View)};
                {error, _} = Err2 ->
                    Err2
            end
    end;
asset_presign(_OrgId, _Params) ->
    {error, {invalid_argument, asset_presign}}.

%% BE-PATCH-01：presign 响应补本 widget 代理端点的裸 PUT 目标（绝对 https
%% URL）。代理端点不是对象存储——不含 object key / storage 引用，不违反
%% 「永不出对象 URL」；url 查询串携带 organization_id / installation_id /
%% upload_ref（裸 PUT 的唯一凭证），FE 检测到 upload.url 即零改动闭环。
%% base_url 未配置（或非 https）→ 不加 url（fail-closed，与现状同形）。
with_upload_url(OrgId, Params, Session, View) ->
    case upload_proxy_url(OrgId, Params, Session, View) of
        undefined ->
            View;
        Url ->
            Upload = maps:get(upload, View, #{}),
            View#{upload := Upload#{url => Url, method => <<"PUT">>}}
    end.

upload_proxy_url(OrgId, Params, Session, View) ->
    Base = cs_widget_support:api_base(),
    case is_https_base(Base) of
        false ->
            undefined;
        true ->
            SessionId = maps:get(id, Session, undefined),
            Ref = maps:get(upload_ref, View, undefined),
            InstallationId = maps:get(installation_id, Params, undefined),
            case
                is_integer(SessionId) andalso is_integer(InstallationId) andalso
                    is_binary(Ref) andalso Ref =/= <<>>
            of
                true ->
                    Path = upload_path(SessionId),
                    Query = uri_string:compose_query([
                        {<<"organization_id">>, integer_to_binary(OrgId)},
                        {<<"installation_id">>, integer_to_binary(InstallationId)},
                        {<<"upload_ref">>, Ref}
                    ]),
                    <<Base/binary, Path/binary, $?, Query/binary>>;
                false ->
                    undefined
            end
    end.

upload_path(SessionId) ->
    iolist_to_binary([
        <<"/api/v1/cs/widget/sessions/">>,
        integer_to_binary(SessionId),
        <<"/assets/upload">>
    ]).

is_https_base(Base) when is_binary(Base) ->
    case Base of
        <<"https://", _Rest/binary>> -> true;
        _ -> false
    end;
is_https_base(_) ->
    false.

%% @doc 访客附件确认（confirm 重新鉴权：本用例先裁决令牌作用域，再进
%% enterprise confirm 面）。
-spec asset_confirm(integer(), map()) -> {ok, map()} | {error, term()}.
asset_confirm(OrgId, Params) when is_map(Params) ->
    case visitor_scope(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, #{workspace_id := WorkspaceId, contact_id := ContactId}} ->
            EbParams = cs_widget_support:eb_params(
                Params,
                #{
                    workspace_id => WorkspaceId,
                    upload_ref => maps:get(upload_ref, Params, undefined),
                    %% CSB-02S D6：confirm 重新鉴权同样走访客分支。
                    actor_contact_id => ContactId
                }
            ),
            cs_widget_support:eb_confirm_asset(OrgId, EbParams)
    end;
asset_confirm(_OrgId, _Params) ->
    {error, {invalid_argument, asset_confirm}}.

%% @doc 访客附件字节上传代理（BE-PATCH-01）：
%% POST .../sessions/:session_id/assets/upload，payload=请求体字节。
%%
%% FE 裸 PUT 合同（无凭证头/Cookie）⇒ 本用例**不验 bootstrap 令牌**，鉴权链
%% 改由以下服务端事实+企业面既有实现构成（逐项）：
%%   1. installation 事实：installation_id 必须命中本 Org 且 active（kill switch）；
%%   2. 默认 Workspace：env 注入事实（fail-closed）；
%%   3. 会话事实：session 必须命中 (Org, Workspace)，其 contact 行值是**服务端
%%      派生**的访客主体（浏览器不可申报）；
%%   4. `eb_asset_app:put_object`（既有实现复用）：upload_ref open（HMAC 防篡改/
%%      过期/跨 Org/Workspace/conversation 作用域）+ ref claim 的 contact 与本
%%      会话 contact 逐字比对（同上传人门）+ contact 会话归属门 + hash/size/mime
%%      服务端复核——走私字节/偷换内容在此 fail-closed。
%% 响应只含 asset 元数据投影，**永不**暴露 storage URL / object key。
-spec asset_put(integer(), map()) -> {ok, map()} | {error, term()}.
asset_put(OrgId, Params) when is_map(Params) ->
    InstallationId = maps:get(installation_id, Params, undefined),
    case cs_widget_support:fetch_installation(Params, OrgId, InstallationId) of
        {error, _} = Err ->
            Err;
        {ok, Installation} ->
            case cs_widget_support:installation_active(Installation) of
                {error, _} = Err2 ->
                    Err2;
                {ok, _Active} ->
                    asset_put_in(OrgId, Params)
            end
    end;
asset_put(_OrgId, _Params) ->
    {error, {invalid_argument, asset_put}}.

asset_put_in(OrgId, Params) ->
    case cs_widget_support:resolve_default_workspace(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            Clean = maps:with([store, id], Params),
            SessionId = maps:get(session_id, Params, undefined),
            case
                cs_widget_support:session_fetch(OrgId, Clean#{
                    workspace_id => WorkspaceId, session_id => SessionId
                })
            of
                {error, _} = Err2 ->
                    Err2;
                {ok, Session} ->
                    EbParams = cs_widget_support:eb_params(
                        Params,
                        #{
                            workspace_id => WorkspaceId,
                            upload_ref => maps:get(upload_ref, Params, undefined),
                            payload => maps:get(payload, Params, undefined),
                            %% 服务端派生主体：会话行的 contact（不是任何客户端申报值）。
                            actor_contact_id => maps:get(contact_id, Session)
                        }
                    ),
                    cs_widget_support:eb_put_object(OrgId, EbParams)
            end
    end.

%% @doc 访客附件内容代理（BE-S01b，api-surface-freeze widget_apis）：
%% GET .../sessions/:session_id/assets/:asset_id/content。
%%
%% 逐项校验链：令牌 → installation → 默认 Workspace → 会话归属
%% （visitor_session_scope）→ **绑定门**（asset 的 conversation 必须 == 本
%% session 的 conversation，且访客只读 linked 资产——见 eb_asset_app
%% content_stream 的 conversation_id 绑定作用域键）。asset_id 是路径绑定
%% （服务端解析），浏览器不可能申报会话外的作用域。返回体是 asset 内容
%% 投影（mime/size/hash/字节），**永不**暴露 storage URL / object key。
-spec asset_content(integer(), map()) -> {ok, map()} | {error, term()}.
asset_content(OrgId, Params) when is_map(Params) ->
    case visitor_session_scope(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, #{workspace_id := WorkspaceId, contact_id := ContactId, session := Session}} ->
            EbParams = cs_widget_support:eb_params(
                Params,
                #{
                    workspace_id => WorkspaceId,
                    asset_id => maps:get(asset_id, Params, undefined),
                    %% 绑定作用域（服务端派生自本 session）：enterprise 侧逐字
                    %% 比对 asset 行的 conversation——跨会话 not_found，不枚举。
                    conversation_id => maps:get(conversation_id, Session),
                    %% CSB-02S D6：访客主体走企业面 contact 分支（会话归属门）。
                    actor_contact_id => ContactId
                }
            ),
            cs_widget_support:eb_content_stream(OrgId, EbParams)
    end;
asset_content(_OrgId, _Params) ->
    {error, {invalid_argument, asset_content}}.

%% ===================================================================
%% 作用域辅助（服务端派生事实；申报值只作逐字比对不成为授权事实）
%% ===================================================================

%% 会话级作用域：令牌有效 + 默认 Workspace 事实 + 会话属于令牌 contact。
visitor_session_scope(OrgId, Params) ->
    case visitor_scope(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, Scope} ->
            session_in_scope(OrgId, Params, Scope)
    end.

session_in_scope(OrgId, Params, Scope) ->
    WorkspaceId = maps:get(workspace_id, Scope),
    ContactId = maps:get(contact_id, Scope),
    Clean = maps:with([store, id], Params),
    case
        cs_widget_support:session_fetch(OrgId, Clean#{
            workspace_id => WorkspaceId, session_id => maps:get(session_id, Params, undefined)
        })
    of
        {error, _} = Err ->
            Err;
        {ok, Session} ->
            case maps:get(contact_id, Session) =:= ContactId of
                true ->
                    {ok, Scope#{session => Session}};
                false ->
                    {error, {not_session_contact, ContactId, maps:get(contact_id, Session)}}
            end
    end.

%% 令牌级作用域：digest 命中 + 未吊销/未过期 + 默认 Workspace 事实解析。
visitor_scope(OrgId, Params) ->
    case cs_widget_support:verify_bootstrap_token(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, Token} ->
            case cs_widget_support:resolve_default_workspace(OrgId, Params) of
                {error, _} = Err2 ->
                    Err2;
                {ok, WorkspaceId} ->
                    {ok, #{
                        token => Token,
                        contact_id => maps:get(contact_id, Token),
                        workspace_id => WorkspaceId
                    }}
            end
    end.
