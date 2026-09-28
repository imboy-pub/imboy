%%% @doc 客服**租户面**薄 Handler（CS-02，plan §5.2）。
%%%
%%% 依据：plan v4.1 §5.2、§5.4、CS-02-A01/A02/A04、`docs/architecture/
%%% feature-slice-rules.md` 铁律 2/3/5。与 `eb_tenant_handler` 同职责同顺序：
%%%
%%%   1. **解析**：路径绑定 + JSON 正文 + OrgId（path 或申报参数，见
%%%      `cs_actions:org_source/1`）+ `workspace_id`；
%%%   2. **验证**：方法门（405）、动作表白名单投影（表外键忽略、服务端派生键
%%%      400、缺必填 422——`client_msg_id`/`key_ref` 等 application 的无默认
%%%      maps:get 键在到达 application 之前必被结构化校验，绝不 badarg 500）；
%%%   3. **认证**：`cs_auth:authorize/3`（五类身份由 route metadata 决定；
%%%      suspended seat actor 即时拒绝，CS-02-A04）；
%%%   4. **调用 + 映射**：`cs_facade_call:call/3`（只进 facade），结果 → HTTP
%%%      （200 / 400 / 401 / 403 / 404 / 405 / 409 / 422 / 500；offboarding 降级
%%%      = 409 + envelope `offboarding_required`）。
%%%
%%% 平台运营面不在本模块（见 `cs_platform_handler`）；两面共用同一 application，
%%% 接口层不复制业务逻辑（CS-02-A02）。
%%%
%%% **本模块不做**：不读库、不写 SQL、不做业务判定、不拼 SQL、不缓存事实。
-module(cs_tenant_handler).

-export([init/2, handle/3]).

%% SSE 帧构造纯函数（导出仅供套件零 socket 断言；生产路径只在流循环内使用）。
-export([event_frame/2, resync_frame/4, revocation_frame/3]).

%% BE-S01b（sse-event-contract）：SSE 缺省节奏（route Opts 可注入覆盖：
%% sse_poll_ms / sse_max_ms）。retry: 2000 是合同首帧字面值，不注入。
-define(SSE_RETRY_MS, 2000).
-define(SSE_POLL_MS, 2000).
-define(SSE_MAX_MS, 300000).
-define(SSE_KEEPALIVE_EVERY, 3).
%% 撤权复查节奏：每 N 次轮询做一次逐请求认证复查（cs_auth 全链——facts +
%% seat enabled 门），撤权后发最小 revoked 帧并关流。
-define(SSE_RECHECK_EVERY, 5).

%% cowboy 普通 handler：State = route Opts（含 route metadata + 中间件会话键）。
-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0, undefined),
    Req = handle(Action, Req0, State0),
    {ok, Req, State0}.

%% @doc 单动作处理（导出以便契约测试直接驱动；生产路径由 init/2 调用）。
-spec handle(atom() | undefined, cowboy_req:req(), map()) -> cowboy_req:req().
handle(Action, Req0, State0) ->
    case cs_actions:tenant(Action) of
        {error, {unknown_action, _}} ->
            cs_http:reply_error(Req0, {unknown_action, Action});
        {ok, Entry} ->
            case cs_actions:case_for(Entry, cowboy_req:method(Req0)) of
                {error, method_not_allowed} ->
                    cs_http:reply_error(Req0, method_not_allowed);
                {ok, Case} when Action =:= seat_events ->
                    %% BE-S01b：seat_events 走同一解析→验证→认证链，命中后进
                    %% 流式分支（不落 cs_http:respond 的 JSON 面）。
                    events(Entry, Case, Req0, State0);
                {ok, Case} ->
                    dispatch(Entry, Case, Req0, State0)
            end
    end.

dispatch(Entry, Case, Req0, State0) ->
    case cs_http:read_body(Req0) of
        {error, Reason} ->
            cs_http:reply_error(Req0, Reason);
        {ok, Body} ->
            case cs_http:org_id(Entry, Req0, Body) of
                {error, Reason} ->
                    cs_http:reply_error(Req0, Reason);
                {ok, OrgId} ->
                    State = State0#{organization_id => OrgId},
                    authorize(Entry, Case, Req0, Body, State, OrgId)
            end
    end.

authorize(Entry, Case, Req0, Body, State, OrgId) ->
    Metadata = authorize_metadata(Entry, Case, State),
    case authorizer(Entry, Metadata, Req0, State) of
        {error, Reason} ->
            cs_http:reply_error(Req0, Reason);
        {ok, AuthContext} ->
            invoke(Entry, Case, Req0, Body, OrgId, AuthContext)
    end.

%% BE-S01a：`org_source = self` 的主体自身作用域用例（坐席上下文清单）。
%% 五类 principal 的凭证类别语义不变（cs_seat = IMBoy JWT），但**不**做
%% org 级 member/assignment/seat 判定——清单的意义正是枚举这些事实，各 Org
%% 的复核由 application 聚合时逐 Org 下推（store SQL 同语句过滤 active
%% member / active assignment / seat enabled）。缺 JWT 会话键即 401。
authorizer(Entry, Metadata, Req, State) ->
    case cs_actions:org_source(Entry) of
        self ->
            case maps:get(current_uid, State, 0) of
                Uid when is_integer(Uid), Uid > 0 ->
                    {ok, #{auth_context => cs_seat, user_id => Uid}};
                _ ->
                    {error, credential_missing}
            end;
        _ ->
            cs_auth:authorize(Metadata, Req, State)
    end.

%% route metadata（auth_context 等）+ 动作表 case_auth 覆盖：同一 cowboy 路径
%% 的 method+auth_context 分流（CSB-02R：GET /sessions/queue 的坐席语义）。
%% case_auth 只能**收窄**到五类 principal 内的声明（cs_route_contract_tests
%% 审计其方法/主体合法性）；未覆盖的方法沿用 route metadata 主体。
authorize_metadata(Entry, Case, State) ->
    Base = metadata(State),
    case maps:get(case_auth, Entry, undefined) of
        CaseAuth when is_map(CaseAuth) ->
            Override = maps:get(maps:get(method, Case), CaseAuth, #{}),
            maps:merge(Base, Override);
        _ ->
            Base
    end.

invoke(Entry, Case, Req0, Body, OrgId, AuthContext) ->
    case workspace_gate(Case, Req0, Body) of
        {error, Reason} ->
            cs_http:reply_error(Req0, Reason);
        {ok, WorkspaceId} ->
            Derived = derived_params(Case, AuthContext, WorkspaceId),
            case cs_http:build_params(Entry, Case, Req0, Body, Derived) of
                {error, Reason} ->
                    cs_http:reply_error(Req0, Reason);
                {ok, Params} ->
                    Result = cs_facade_call:call(maps:get(facade, Case), OrgId, Params),
                    cs_http:respond(Entry, Req0, Result)
            end
    end.

%% workspace 门（CSB-02R）：缺省 required（既有口径不变）；动作表声明
%% `workspace => optional` 的用例缺失不 422（坐席 org-wide 列表的作用域由
%% Org/assignment 决定，显式给出才收窄）。
workspace_gate(Case, Req, Body) ->
    case maps:get(workspace, Case, required) of
        optional ->
            case cs_http:workspace_id(Req, Body) of
                {error, missing_workspace_id} -> {ok, undefined};
                Other -> Other
            end;
        required ->
            cs_http:workspace_id(Req, Body)
    end.

%% route metadata：只取白名单键 + 认证需要的装配/会话键走 State 本体。
metadata(State) ->
    Keys = [auth_context, surface, required_function, required_permission, required_governance],
    maps:from_list([{K, maps:get(K, State, undefined)} || K <- Keys, maps:is_key(K, State)]).

%% 服务端派生参数：操作人/时钟/主体身份全部来自认证上下文与服务端时钟，
%% 客户端无法自报（动作表已把这些键列为 client_forbidden，400 兜底）。
derived_params(Case, AuthContext, WorkspaceId) ->
    %% CSB-02R：optional workspace **缺省时键不存在**（不是值为 undefined 的
    %% 键）——「未提供」与「提供了 undefined」是两种形状，facade/application
    %% 与消费方只见前者；此处是 optional 派生键的唯一归一点。
    Base0 = #{
        at => now(Case),
        actor_user_id => actor_user_id(AuthContext)
    },
    Base =
        case WorkspaceId of
            Ws when is_integer(Ws), Ws > 0 -> Base0#{workspace_id => Ws};
            _ -> Base0
        end,
    maps:merge(Base, identity_derived(AuthContext)).

actor_user_id(#{user_id := Uid}) when is_integer(Uid) -> Uid;
actor_user_id(_Other) -> undefined.

%% 时钟量纲选择（与 cs_platform_handler:now/1 同机制）：store 的时间写路径
%% 统一 `to_timestamp`（epoch 秒）。默认沿用租户面毫秒既有口径；动作表声明
%% `clock_unit => second` 的用例改用秒——DF-4 修复：吊销写路径（shop_key /
%% visit_token revoke）以毫秒喂 `to_timestamp` 会把 revoked_at 写成约 5.8 万
%% 年后，`cs_session:assert_visitor_scope` 的吊销判定永不命中（fail-open）。
now(Case) ->
    case maps:get(clock_unit, Case, millisecond) of
        second -> cs_http:now_sec();
        millisecond -> cs_http:now_ms()
    end.

identity_derived(#{auth_context := cs_seat, business_identity_id := Bid}) ->
    #{business_identity_id => Bid};
identity_derived(#{auth_context := cs_visit, contact_id := Cid}) ->
    #{contact_id => Cid};
identity_derived(#{auth_context := enterprise_owner_admin, user_id := Uid}) when
    is_integer(Uid)
->
    #{created_by_user_id => Uid};
identity_derived(_Other) ->
    #{}.

%% ===================================================================
%% BE-S01b：坐席 SSE 事件流（GET .../seats/me/events）
%%
%% 与普通动作共用解析→验证→认证→参数投影链（开流前的 401/403/400/422 与
%% JSON 面逐字一致）；认证通过后进流式分支：
%%   * 首帧 `retry: 2000`（合同字面值）；
%%   * 游标缺失/超窗 ⇒ 先发一次 `resync.required` 合成帧再从水位续传；
%%   * 事件帧 `id:`/`data:` 全 TSID-string，event type 恰六种（信封投影在
%%     application `cs_seat_event_app`，本层只做线格式）；
%%   * 轮询空转周期性发注释行保活（不占 event_id）；
%%   * 周期性逐请求复查认证（cs_auth 全链），seat disabled / assignment
%%     ended ⇒ 发最小 revoked 帧后关流；下一次连接 403（cs_auth 门）；
%%   * 到 deadline 正常 fin——客户端凭 Last-Event-ID 无损重连续传。
%% ===================================================================

events(Entry, Case, Req0, State0) ->
    case cs_http:read_body(Req0) of
        {error, Reason} ->
            cs_http:reply_error(Req0, Reason);
        {ok, Body} ->
            case cs_http:org_id(Entry, Req0, Body) of
                {error, Reason} ->
                    cs_http:reply_error(Req0, Reason);
                {ok, OrgId} ->
                    State = State0#{organization_id => OrgId},
                    Metadata = authorize_metadata(Entry, Case, State),
                    case cs_auth:authorize(Metadata, Req0, State) of
                        {error, Reason} ->
                            cs_http:reply_error(Req0, Reason);
                        {ok, AuthContext} ->
                            events_invoke(
                                Entry, Case, Req0, Body, OrgId, State, Metadata, AuthContext
                            )
                    end
            end
    end.

events_invoke(Entry, Case, Req0, Body, OrgId, State, Metadata, AuthContext) ->
    case workspace_gate(Case, Req0, Body) of
        {error, Reason} ->
            cs_http:reply_error(Req0, Reason);
        {ok, WorkspaceId} ->
            Derived = derived_params(Case, AuthContext, WorkspaceId),
            case cs_http:build_params(Entry, Case, Req0, Body, Derived) of
                {error, Reason} ->
                    cs_http:reply_error(Req0, Reason);
                {ok, Params} ->
                    case event_cursor(Req0, Params) of
                        {error, Reason} ->
                            cs_http:reply_error(Req0, Reason);
                        {ok, AfterId} ->
                            stream(
                                Req0,
                                OrgId,
                                with_cursor(Params, AfterId),
                                State#{cursor_state => #{metadata => Metadata}}
                            )
                    end
            end
    end.

%% 游标归一：`Last-Event-ID` 头**优先**于 `after_id` 查询参数（合同）。
%% 非法头值 400（不静默回退——与 widget 面补偿读不同，这里合同明文要求）。
event_cursor(Req, Params) ->
    case cowboy_req:header(<<"last-event-id">>, Req) of
        Raw when is_binary(Raw), Raw =/= <<>> ->
            case cs_http:tsid(Raw) of
                {ok, Id} -> {ok, {cursor, Id}};
                error -> {error, {invalid_param, last_event_id}}
            end;
        _ ->
            case maps:get(after_id, Params, undefined) of
                undefined -> {ok, no_cursor};
                Id when is_integer(Id), Id > 0 -> {ok, {cursor, Id}};
                Bad -> {error, {invalid_param, after_id, Bad}}
            end
    end.

with_cursor(Params, {cursor, Id}) -> Params#{after_id => Id};
with_cursor(Params, no_cursor) -> maps:remove(after_id, Params).

%% 开流：先用例调用裁决游标（跨作用域 403 等在开流前结构化拒绝），成功即
%% stream_reply + retry 帧（+ 可选 resync 帧 + 首页事件帧）。
stream(Req0, OrgId, Params, State) ->
    case cs_facade_call:call(seat_events, OrgId, Params) of
        {error, Reason} ->
            cs_http:reply_error(Req0, Reason);
        {ok, #{events := Events, cursor := Cursor, resync_required := Resync} = Page} ->
            Req1 = cowboy_req:stream_reply(200, stream_headers(), Req0),
            First = [
                retry_frame(?SSE_RETRY_MS),
                case Resync of
                    true ->
                        resync_frame(
                            OrgId,
                            maps:get(workspace_id, Params),
                            Cursor,
                            reason_bin(Page)
                        );
                    false ->
                        <<>>
                end,
                event_frames(Events)
            ],
            case send(Req1, First) of
                ok ->
                    stream_loop(
                        Req1,
                        OrgId,
                        Params,
                        State,
                        #{
                            cursor => Cursor,
                            deadline => erlang:monotonic_time(millisecond) + max_ms(State),
                            empty => 0,
                            polls => 0,
                            poll_ms => poll_ms(State)
                        }
                    );
                {error, _} ->
                    %% 客户端在首帧前即断开：静默收流。
                    Req1
            end;
        {ok, _Other} ->
            cs_http:reply_error(Req0, {unknown_action, seat_events})
    end.

stream_loop(Req, OrgId, Params, State, #{deadline := Deadline} = Ctx) ->
    case erlang:monotonic_time(millisecond) >= Deadline of
        true ->
            finish(Req);
        false ->
            timer:sleep(maps:get(poll_ms, Ctx)),
            recheck_or_poll(Req, OrgId, Params, State, Ctx)
    end.

%% 周期性逐请求复查（cs_auth 全链：active member + assignment + seat enabled）。
%% 每 ?SSE_RECHECK_EVERY 次轮询复查一次；其余轮询只读事件页。
recheck_or_poll(Req, OrgId, Params, State, Ctx = #{polls := Polls}) ->
    case (Polls + 1) rem ?SSE_RECHECK_EVERY of
        0 ->
            case cs_auth:authorize(recheck_metadata(State), Req, State) of
                {ok, _} ->
                    poll(Req, OrgId, Params, State, Ctx#{polls := Polls + 1});
                {error, Reason} ->
                    revocation_close(Req, OrgId, Params, Reason)
            end;
        _ ->
            poll(Req, OrgId, Params, State, Ctx#{polls := Polls + 1})
    end.

recheck_metadata(State) ->
    metadata(State).

poll(Req, OrgId, Params, State, Ctx) ->
    case
        cs_facade_call:call(
            seat_events, OrgId, with_cursor(Params, {cursor, maps:get(cursor, Ctx)})
        )
    of
        {ok, #{events := Events, cursor := Cursor}} ->
            deliver(Req, OrgId, Params, State, Ctx, event_frames(Events), Cursor);
        _ ->
            %% 单次轮询失败（瞬态）不终止流；也不推进保活计数。
            stream_loop(Req, OrgId, Params, State, Ctx)
    end.

deliver(Req, OrgId, Params, State, Ctx = #{empty := Empty0}, <<>>, Cursor) ->
    %% 空轮询：按计数周期性发保活注释（不占 event_id）。
    Empty = Empty0 + 1,
    case Empty > 0 andalso Empty rem ?SSE_KEEPALIVE_EVERY =:= 0 of
        true ->
            case send(Req, comment_frame()) of
                ok ->
                    stream_loop(Req, OrgId, Params, State, Ctx#{empty := Empty, cursor := Cursor});
                {error, _} ->
                    Req
            end;
        false ->
            stream_loop(Req, OrgId, Params, State, Ctx#{empty := Empty, cursor := Cursor})
    end;
deliver(Req, OrgId, Params, State, Ctx, Frames, Cursor) ->
    case send(Req, Frames) of
        ok ->
            stream_loop(Req, OrgId, Params, State, Ctx#{empty := 0, cursor := Cursor});
        {error, _} ->
            Req
    end.

%% 撤权关流：seat disabled → seat.changed/revoked；assignment/member 消失 →
%% assignment.changed/revoked（最小帧，resource_id = 本人 identity）。其余
%% 复查失败（事实源不可用等）不显形，直接关流——客户端重连时 401/403 显式化。
revocation_close(Req, OrgId, Params, seat_disabled) ->
    close_with_revocation(Req, OrgId, Params, <<"seat.changed">>, <<"seat">>);
revocation_close(Req, OrgId, Params, {seat_not_found, _}) ->
    close_with_revocation(Req, OrgId, Params, <<"seat.changed">>, <<"seat">>);
revocation_close(Req, OrgId, Params, identity_assignment_missing) ->
    close_with_revocation(Req, OrgId, Params, <<"assignment.changed">>, <<"assignment">>);
revocation_close(Req, OrgId, Params, {member_not_active, _}) ->
    close_with_revocation(Req, OrgId, Params, <<"assignment.changed">>, <<"assignment">>);
revocation_close(Req, OrgId, Params, member_not_found) ->
    close_with_revocation(Req, OrgId, Params, <<"assignment.changed">>, <<"assignment">>);
revocation_close(Req, _OrgId, _Params, _Other) ->
    finish(Req).

close_with_revocation(Req, OrgId, Params, Type, ResourceType) ->
    Frame = revocation_frame(OrgId, maps:get(workspace_id, Params), #{
        type => Type,
        resource_type => ResourceType,
        resource_id => maps:get(business_identity_id, Params, undefined)
    }),
    _ = send(Req, Frame),
    finish(Req).

%% ===================================================================
%% SSE 线格式（text/event-stream 语法；帧构造纯函数可零 socket 断言）
%% ===================================================================

stream_headers() ->
    #{
        <<"content-type">> => <<"text/event-stream; charset=utf-8">>,
        <<"cache-control">> => <<"no-store">>,
        <<"x-accel-buffering">> => <<"no">>,
        <<"x-cs-event-retention-seconds">> => <<"86400">>
    }.

%% @doc 单事件帧：id/event/data 全合同形状（data 是九字段信封 JSON，
%% TSID integer 先经 cs_http:encode_entity 编为 string）。
-spec event_frame(integer(), map()) -> binary().
event_frame(Id, Envelope) when is_integer(Id), is_map(Envelope) ->
    Data = jsone:encode(cs_http:encode_entity(Envelope), [native_utf8]),
    <<"id: ", (integer_to_binary(Id))/binary, "\nevent: ", (type_bin(Envelope))/binary, "\ndata: ",
        Data/binary, "\n\n">>;
event_frame(_Id, _Envelope) ->
    <<>>.

%% @doc 事件列表 → 帧序列（调用方保证 id 严格递增——键集升序读页天然序）。
-spec event_frames([map()]) -> binary().
event_frames(Events) when is_list(Events) ->
    iolist_to_binary([event_frame(maps:get(event_id, E, 0), E) || E <- Events]);
event_frames(_) ->
    <<>>.

%% resync 成因取值收敛（合同 reason 枚举内；非二进制一律归 unknown）。
reason_bin(Page) ->
    case maps:get(resync_reason, Page, <<"unknown">>) of
        Bin when is_binary(Bin), Bin =/= <<>> -> Bin;
        _ -> <<"unknown">>
    end.

%% @doc 合成 resync.required 帧（event_id = 续传水位，单调不回退；reason 按
%% resync 成因给值——超窗 = expired，首连无游标 = unknown）。
-spec resync_frame(integer(), integer(), integer(), binary()) -> binary().
resync_frame(OrgId, WorkspaceId, Watermark, Reason) when
    is_integer(OrgId), is_integer(WorkspaceId), is_integer(Watermark), is_binary(Reason)
->
    Envelope = cs_seat_event_app:resync_envelope(Watermark, OrgId, WorkspaceId, Reason),
    event_frame(Watermark, Envelope);
resync_frame(_OrgId, _WorkspaceId, _Watermark, _Reason) ->
    <<>>.

%% @doc 合成撤权帧（reason = revoked；handler 关流前的最后一条线格式）。
-spec revocation_frame(integer(), integer(), map()) -> binary().
revocation_frame(OrgId, WorkspaceId, Spec) when
    is_integer(OrgId), is_integer(WorkspaceId), is_map(Spec)
->
    Envelope = #{
        event_id => 0,
        type => maps:get(type, Spec),
        organization_id => OrgId,
        workspace_id => WorkspaceId,
        resource_type => maps:get(resource_type, Spec),
        resource_id => maps:get(resource_id, Spec, undefined),
        resource_version => 1,
        occurred_at => occurred_now(),
        reason => <<"revoked">>
    },
    event_frame(0, Envelope);
revocation_frame(_OrgId, _WorkspaceId, _Spec) ->
    <<>>.

occurred_now() ->
    %% 与 application 面同口径：Unix 秒 → RFC3339（elib_dt 是 core lib）。
    elib_dt:to_rfc3339(os:system_time(millisecond)).

type_bin(#{type := Type}) when is_binary(Type) -> Type;
type_bin(_Other) -> <<"session.changed">>.

%% 重连建议帧（`retry:` 字段；合同首帧 2000）。
-spec retry_frame(pos_integer()) -> binary().
retry_frame(Ms) when is_integer(Ms), Ms > 0 ->
    <<"retry: ", (integer_to_binary(Ms))/binary, "\n">>;
retry_frame(_) ->
    <<>>.

%% 保活注释行（SSE 注释 = 冒号开头行；不产生事件、不占 event_id）。
-spec comment_frame() -> binary().
comment_frame() ->
    <<": keep-alive\n\n">>.

%% 单次写出；cowboy 断连/错误统一折叠为 {error, _}。stream_body 的
%% 非 ok 返回不走 of 分支（case_clause 被 catch 收拢），socket 错误不崩。
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

poll_ms(State) ->
    positive(maps:get(sse_poll_ms, State, ?SSE_POLL_MS)).

max_ms(State) ->
    positive(maps:get(sse_max_ms, State, ?SSE_MAX_MS)).

positive(N) when is_integer(N), N > 0 ->
    N;
positive(_) ->
    1.
