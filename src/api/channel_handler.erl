-module(channel_handler).
-compile([nowarn_deprecated_catch]).
%% Thin HTTP adapter for the channel_content domain.
%% Message/order/admin functions split into sub-handlers (H4):
%%   channel_handler_message  (消息/置顶/反应/订阅者)
%%   channel_handler_admin    (管理员/邀请/同步)
%%   channel_handler_order    (订单/支付)
%%
%%% =====================================================================
%%% T5 路由片段清单（双体验 v2.5.2；全部挂既有路由，无新增路由——T7 无需注册）
%%% =====================================================================
%%% 既有路由的行为增强（本 WP 落地）：
%%%   POST /api/v1/channel/create           → create 新增 scope+workspace_id 参数
%%%                                           （scope=workspace 须 Owner/Member，Guest 403；
%%%                                            personal 默认，行为零变化）
%%%   POST /api/v1/channel/:channel_id/update → update 拒绝改 scope/workspace_id（400，不可变）
%%%   POST /api/v1/channel/:channel_id/archive|restore → GZAPP-02/G4 频道归档/恢复
%%%                                           （仅创建者；status 0↔1 与删除 -1 区分；980 同口径）
%%%   GET  /api/v1/channel/:channel_id 等 :channel_id 入口（show/publish/messages/
%%%        subscribe/unsubscribe/mark_read/add_admin/stats 及 message/admin/comment/
%%%        webhook 子 handler 全部 :channel_id 入口）→ init 前置 workspace 边界：
%%%        workspace 频道要求请求者为 active 工作区成员，否则 403；
%%%        personal 频道零行为变化。
%%% 工作区频道列表（新能力，供 workspace IA 消费）：
%%%   GET /api/v1/workspaces/:workspace_id/channels → 已落在 workspace_handler
%%%       的 channel_list 动作（T7 按 workspace_handler 顶部清单注册；
%%%       本 handler 不重复提供，避免双入口）
%%% =====================================================================

-behaviour(cowboy_rest).

-include("error_code.hrl").

-export([init/2]).
-export([handle_action/3]).
-export([stats/2]).

-include("log.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 =
        case imboy_plugin_registry:required_feature(api, channel_handler, Action) of
            undefined ->
                guarded_handle(Action, Req0, State);
            Feature ->
                case imboy_feature:ensure_enabled(Req0, Feature) of
                    ok ->
                        guarded_handle(Action, Req0, State);
                    {error, RespReq} ->
                        RespReq
                end
        end,
    {ok, Req1, State}.

%% @doc T5 Workspace 边界前置校验（镜像 JWT 前置校验模式）：
%% 路由带 :channel_id 的入口，若目标频道 scope=workspace，要求请求者为
%% active 工作区成员（§1.4.2 授权规则 2），非成员稳定 403；
%% personal 频道 / 无频道上下文 / 频道不存在 → 放行走既有流程（零行为变化）。
-spec guarded_handle(atom() | false, cowboy_req:req(), map()) -> cowboy_req:req().
guarded_handle(Action, Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    case workspace_resolver:guard_channel_binding(Req0, Uid) of
        ok ->
            handle_action(Action, Req0, State);
        {error, {403, Msg}} ->
            elib_response:error(Req0, Msg, 403);
        %% 边界守卫 DB 异常 fail-closed（503，不吞异常放行）
        {error, {503, Msg}} ->
            elib_response:error(Req0, Msg, 503)
    end.

-spec handle_action(atom() | false, cowboy_req:req(), map()) -> cowboy_req:req().
handle_action(create, Req, State) -> create(Req, State);
handle_action(show, Req, State) -> show(Req, State);
handle_action(qrcode, Req, State) -> qrcode(Req, State);
handle_action(by_custom_id, Req, State) -> by_custom_id(Req, State);
handle_action(update, Req, State) -> update(Req, State);
handle_action(delete, Req, State) -> delete(Req, State);
handle_action(archive, Req, State) -> archive(Req, State);
handle_action(restore, Req, State) -> restore(Req, State);
handle_action(subscribe, Req, State) -> subscribe(Req, State);
handle_action(unsubscribe, Req, State) -> unsubscribe(Req, State);
handle_action(subscribed, Req, State) -> subscribed(Req, State);
handle_action(managed, Req, State) -> managed(Req, State);
handle_action(unread_summary, Req, State) -> unread_summary(Req, State);
handle_action(publish_message, Req, State) -> publish_message(Req, State);
handle_action(messages, Req, State) -> messages(Req, State);
handle_action(mark_read, Req, State) -> mark_read(Req, State);
handle_action(search, Req, State) -> search(Req, State);
handle_action(discover, Req, State) -> discover(Req, State);
handle_action(add_admin, Req, State) -> add_admin(Req, State);
handle_action(stats, Req, State) -> stats(Req, State);
handle_action(stats_daily, Req, State) -> stats_daily(Req, State);
handle_action(false, Req, _State) -> Req.

%% @doc 创建频道
%% T5（双体验 v2.5.2）：接受可选 scope+workspace_id；
%% 缺省/ personal 与既有行为完全一致（回归红线）。
-spec create(cowboy_req:req(), map()) -> cowboy_req:req().
create(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    Name = maps:get(<<"name">>, PostVals, <<>>),
    Visibility = elib_cnv:safe_to_integer(maps:get(<<"visibility">>, PostVals, 0)),
    AccessType = elib_cnv:safe_to_integer(maps:get(<<"access_type">>, PostVals, 0)),
    JoinPolicy = elib_cnv:safe_to_integer(maps:get(<<"join_policy">>, PostVals, 0)),
    Description = maps:get(<<"description">>, PostVals, <<>>),
    Avatar = maps:get(<<"avatar">>, PostVals, <<>>),
    CustomId = maps:get(<<"custom_id">>, PostVals, undefined),
    Tags = maps:get(<<"tags">>, PostVals, []),
    Scope = maps:get(<<"scope">>, PostVals, <<"personal">>),
    WorkspaceId = elib_cnv:safe_to_integer(maps:get(<<"workspace_id">>, PostVals, 0)),

    case Name of
        <<>> ->
            elib_response:error(Req0, <<"频道名称不能为空"/utf8>>);
        _ ->
            %% GZAPP-02/G5 企业频道内部化：workspace scope 频道强制非公开——
            %% 公开组合 C1(0,0,0)→C2(1,0,1)、C3(0,1,3)→C4(1,1,3)，
            %% 私有组合原样；确保企业频道不出现在公开发现面
            %% （discover/trending/featured/search 均只取 visibility=0）。
            {Visibility2, JoinPolicy2} = internalize_visibility(Scope, Visibility, JoinPolicy),
            case validate_access_policy(Visibility2, AccessType, JoinPolicy2) of
                true ->
                    Opts = #{
                        description => Description,
                        avatar => Avatar,
                        custom_id => CustomId,
                        tags => Tags,
                        visibility => Visibility2,
                        access_type => AccessType,
                        join_policy => JoinPolicy2
                    },
                    MaxChannels = 20,
                    %% W04.c（channel-group-ux v2.2 §6.4 C1）：携带非空
                    %% request_id 的请求走受理合同（占位/重放/recover_only）；
                    %% 缺失或空串走旧路径，行为零变化（回归红线）。
                    %% 绝不剥离客户端已携带的键。
                    case receipt_request_id(PostVals) of
                        <<>> ->
                            case
                                channel_logic:create_channel(
                                    Uid, Name, Opts, MaxChannels, {Scope, WorkspaceId}
                                )
                            of
                                {ok, Channel} ->
                                    elib_response:success(Req0, Channel);
                                {error, {Code, Msg}} ->
                                    elib_response:error(Req0, Msg, Code);
                                {error, Msg} ->
                                    elib_response:error(Req0, Msg)
                            end;
                        RequestId ->
                            RecoverOnly = recover_only_flag(PostVals),
                            respond_channel_create_receipt(
                                Req0,
                                Uid,
                                RequestId,
                                channel_logic:create_channel_with_receipt(
                                    Uid,
                                    Name,
                                    Opts,
                                    MaxChannels,
                                    {Scope, WorkspaceId},
                                    RequestId,
                                    RecoverOnly
                                )
                            )
                    end;
                false ->
                    elib_response:error(Req0, <<"频道访问策略组合无效"/utf8>>)
            end
    end.

%% ===================================================================
%% W04.c（channel-group-ux v2.2 §6.4 C1）：创建受理合同响应
%% ===================================================================

%% request_id 解析：非空二进制视为新合同意图；缺失/空串走旧路径。
-spec receipt_request_id(map()) -> binary().
receipt_request_id(PostVals) when is_map(PostVals) ->
    Raw =
        case maps:get(<<"request_id">>, PostVals, <<>>) of
            Bin when is_binary(Bin) -> Bin;
            Other -> elib_cnv:safe_to_binary(Other)
        end,
    elib_str:trunc(string:trim(Raw), 128);
receipt_request_id(_) ->
    <<>>.

%% recover_only 只查标志（§6.4：>=24h 的未知意图绝不创建，只查）。
-spec recover_only_flag(map()) -> boolean().
recover_only_flag(PostVals) when is_map(PostVals) ->
    case maps:get(<<"recover_only">>, PostVals, false) of
        true -> true;
        <<"true">> -> true;
        1 -> true;
        <<"1">> -> true;
        _ -> false
    end;
recover_only_flag(_) ->
    false.

%% 受理结果 → HTTP 响应：成功 Channel map 附 request_id/creation_state
%% 增量字段；四个稳定错误符号分开（§6.4），错误 envelope payload 携带
%% 核对元信息（request_id，可达时附频道 ID），不泄露名称/成员。
-spec respond_channel_create_receipt(cowboy_req:req(), integer(), binary(), term()) ->
    cowboy_req:req().
respond_channel_create_receipt(Req0, _Uid, RequestId, {ok, Channel, CreationState}) ->
    elib_response:success(
        Req0,
        maps:merge(Channel, #{
            <<"request_id">> => RequestId,
            <<"creation_state">> => CreationState
        })
    );
respond_channel_create_receipt(Req0, _Uid, RequestId, {error, request_id_conflict}) ->
    elib_response:error_with_status(
        Req0,
        409,
        <<"request_id_conflict：同一 request_id 已绑定不同的创建内容"/utf8>>,
        #{<<"request_id">> => RequestId},
        ?ERR_CREATE_REQUEST_ID_CONFLICT
    );
respond_channel_create_receipt(Req0, _Uid, RequestId, {error, create_not_accepted}) ->
    %% 仅表示查询时未见已提交受理，不证明在途请求不会迟到（§6.4）。
    elib_response:error_with_status(
        Req0,
        404,
        <<"create_not_accepted：查询时未见该 request_id 的受理记录"/utf8>>,
        #{<<"request_id">> => RequestId},
        ?ERR_CREATE_NOT_ACCEPTED
    );
respond_channel_create_receipt(Req0, _Uid, RequestId, {error, {create_recovery_expired, Cid}}) ->
    elib_response:error_with_status(
        Req0,
        410,
        <<"create_recovery_expired：受理结果已超过7天保留窗口，仅可用于核对"/utf8>>,
        #{<<"request_id">> => RequestId, <<"channel_id">> => Cid},
        ?ERR_CREATE_RECOVERY_EXPIRED
    );
respond_channel_create_receipt(Req0, _Uid, RequestId, {error, {create_resource_unavailable, Cid}}) ->
    Payload =
        case is_integer(Cid) andalso Cid > 0 of
            true -> #{<<"request_id">> => RequestId, <<"channel_id">> => Cid};
            false -> #{<<"request_id">> => RequestId}
        end,
    elib_response:error_with_status(
        Req0,
        409,
        <<"create_resource_unavailable：该创建结果当前不可用"/utf8>>,
        Payload,
        ?ERR_CREATE_RESOURCE_UNAVAILABLE
    );
respond_channel_create_receipt(Req0, _Uid, _RequestId, {error, {Code, Msg}}) ->
    elib_response:error(Req0, Msg, Code);
respond_channel_create_receipt(Req0, _Uid, _RequestId, {error, Msg}) when is_binary(Msg) ->
    elib_response:error(Req0, Msg);
respond_channel_create_receipt(Req0, Uid, RequestId, Unexpected) ->
    ?ERROR_LOG([channel_create_receipt_unexpected, Uid, RequestId, Unexpected]),
    elib_response:error(Req0, <<"频道创建失败，请稍后重试"/utf8>>).

%% @doc 通过自定义ID获取频道
-spec by_custom_id(cowboy_req:req(), map()) -> cowboy_req:req().
by_custom_id(Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    case cowboy_req:binding(custom_id, Req0) of
        undefined ->
            elib_response:error(Req0, <<"自定义ID不能为空"/utf8>>);
        CustomId ->
            %% T5：custom_id 直访不能绕过 Workspace 边界（§1.4.2 规则 2）
            case workspace_resolver:guard_channel_custom_id(Uid, CustomId) of
                {error, {403, Msg}} ->
                    elib_response:error(Req0, Msg, 403);
                %% 边界守卫 DB 异常 fail-closed（503，不吞异常放行）
                {error, {503, Msg}} ->
                    elib_response:error(Req0, Msg, 503);
                ok ->
                    case channel_logic:get_channel_by_custom_id(CustomId, Uid) of
                        {ok, Channel} ->
                            elib_response:success(Req0, Channel);
                        {error, {Code, Msg}} when is_integer(Code) ->
                            elib_response:error(Req0, Msg, Code);
                        {error, Msg} ->
                            elib_response:error(Req0, Msg)
                    end
            end
    end.

%% @doc 更新频道信息
%% T5（双体验 v2.5.2）：scope/workspace_id 创建后不可变（§1.4.2 规则 9），
%% 提交这两个字段之一即 400（不静默忽略）；其余字段走既有更新路径。
-spec update(cowboy_req:req(), map()) -> cowboy_req:req().
update(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    ChannelId = resolve_channel_id(Req0, PostVals),
    Data = maps:without([<<"channel_id">>], PostVals),

    case ChannelId of
        <<>> ->
            elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
        _ ->
            case maps:is_key(<<"scope">>, Data) orelse maps:is_key(<<"workspace_id">>, Data) of
                true ->
                    elib_response:error(Req0, <<"scope 与 workspace_id 创建后不可修改"/utf8>>, 400);
                false ->
                    case channel_logic:update_channel(Uid, ChannelId, Data) of
                        {ok, Channel} ->
                            elib_response:success(Req0, Channel);
                        {error, {Code, Msg}} when is_integer(Code) ->
                            elib_response:error(Req0, Msg, Code);
                        {error, Msg} ->
                            elib_response:error(Req0, Msg)
                    end
            end
    end.

%% @doc 获取频道信息
-spec show(cowboy_req:req(), map()) -> cowboy_req:req().
show(Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    case cowboy_req:binding(channel_id, Req0) of
        undefined ->
            elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
        ChannelId ->
            case channel_logic:get_channel(ChannelId, Uid) of
                {ok, Channel} ->
                    elib_response:success(Req0, Channel);
                {error, {Code, Msg}} when is_integer(Code) ->
                    %% T7 归档写守卫稳定错误码（980）透传 envelope code
                    elib_response:error(Req0, Msg, Code);
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% @doc 频道二维码解析（扫码名片）。
%% 与 user/group 二维码同契约：exp 为毫秒时间戳、tk=md5(exp_solidifiedKey)；
%% 未登录或 tk 校验失败 302 跳转，过期回业务错误。
%% 只回读频道名片、不自动订阅：订阅必须走 /subscribe 的
%% access_type/join_policy 与付费门禁，GET 扫码不得绕过订单。
-spec qrcode(cowboy_req:req(), map()) -> cowboy_req:req().
qrcode(Req0, State) ->
    Qs = cowboy_req:parse_qs(Req0),
    ChannelId = proplists:get_value(<<"id">>, Qs, undefined),
    ExpiredAt = proplists:get_value(<<"exp">>, Qs, undefined),
    Tk = proplists:get_value(<<"tk">>, Qs, undefined),
    Key = config_ds:env(solidified_key),
    ExpiredAt2 = ec_cnv:to_binary(ExpiredAt),
    %% 非法/缺失 exp 参数时安全返回 0，避免 binary_to_integer badarg 崩溃
    ExpiredAtInt = elib_cnv:safe_to_integer(ExpiredAt2),
    Verified =
        elib_hasher:md5(
            <<ExpiredAt2/binary, "_", (ec_cnv:to_binary(Key))/binary>>
        ) ==
            Tk,
    NowInt = elib_dt:rfc3339_to(elib_dt:now()),
    CurrentUid = auth_ds:current_uid(State),
    case {CurrentUid, Verified} of
        {0, _} ->
            qrcode_redirect(Req0);
        {_, false} ->
            qrcode_redirect(Req0);
        {_, true} when NowInt > ExpiredAtInt ->
            elib_response:error(Req0, "验证码已过期");
        _ ->
            case channel_logic:get_channel(ChannelId, CurrentUid) of
                {ok, Channel} ->
                    elib_response:success(
                        Req0,
                        Channel#{<<"type">> => <<"channel">>}
                    );
                {error, {Code, Msg}} when is_integer(Code) ->
                    %% T7 归档写守卫稳定错误码透传 envelope code
                    elib_response:error(Req0, Msg, Code);
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

-spec qrcode_redirect(cowboy_req:req()) -> cowboy_req:req().
qrcode_redirect(Req0) ->
    cowboy_req:reply(
        302,
        #{<<"Location">> => config_ds:env(redirect_url, <<"http://www.imboy.pub">>)},
        Req0
    ).

%% @doc 删除频道
-spec delete(cowboy_req:req(), map()) -> cowboy_req:req().
delete(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    ChannelId = resolve_channel_id(Req0, PostVals),

    case ChannelId of
        <<>> ->
            elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
        _ ->
            case channel_logic:delete_channel(Uid, ChannelId) of
                ok ->
                    elib_response:success(Req0, #{});
                {error, {Code, Msg}} when is_integer(Code) ->
                    %% T7 归档写守卫稳定错误码（980）透传 envelope code
                    elib_response:error(Req0, Msg, Code);
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% @doc 归档频道（GZAPP-02/G4）：仅创建者；status 1→0（可恢复，与删除区分）。
%% 归档后频道退出全部列表/发现/搜索面（查询恒过滤 status=1）；
%% ws 频道所属工作区已归档时稳定 980。
-spec archive(cowboy_req:req(), map()) -> cowboy_req:req().
archive(Req0, State) ->
    channel_status_action(Req0, State, fun channel_logic:archive_channel/2).

%% @doc 恢复频道（GZAPP-02/G4）：仅创建者；status 0→1；已删除（-1）不可恢复。
-spec restore(cowboy_req:req(), map()) -> cowboy_req:req().
restore(Req0, State) ->
    channel_status_action(Req0, State, fun channel_logic:restore_channel/2).

%% archive/restore 共用入口：binding/post 解析 + 两形态错误映射（同 delete）。
-spec channel_status_action(
    cowboy_req:req(), map(), fun((integer(), binary()) -> {ok, map()} | {error, any()})
) ->
    cowboy_req:req().
channel_status_action(Req0, State, LogicFun) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    ChannelId = resolve_channel_id(Req0, PostVals),
    case ChannelId of
        <<>> ->
            elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
        _ ->
            case LogicFun(Uid, ChannelId) of
                {ok, Result} ->
                    elib_response:success(Req0, Result);
                {error, {Code, Msg}} when is_integer(Code) ->
                    elib_response:error(Req0, Msg, Code);
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% @doc 订阅频道
-spec subscribe(cowboy_req:req(), map()) -> cowboy_req:req().
subscribe(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    ChannelId = resolve_channel_id(Req0, PostVals),

    case ChannelId of
        <<>> ->
            elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
        _ ->
            case channel_logic:subscribe(Uid, ChannelId) of
                ok ->
                    elib_response:success(Req0, #{});
                {error, {Code, Msg}} when is_integer(Code) ->
                    %% T7 归档写守卫稳定错误码（980）透传 envelope code
                    elib_response:error(Req0, Msg, Code);
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% @doc 取消订阅频道
-spec unsubscribe(cowboy_req:req(), map()) -> cowboy_req:req().
unsubscribe(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    ChannelId = resolve_channel_id(Req0, PostVals),

    case ChannelId of
        <<>> ->
            elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
        _ ->
            case channel_logic:unsubscribe(Uid, ChannelId) of
                ok ->
                    elib_response:success(Req0, #{});
                {error, {Code, Msg}} when is_integer(Code) ->
                    %% T7 归档写守卫稳定错误码（980）透传 envelope code
                    elib_response:error(Req0, Msg, Code);
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% @doc 获取用户订阅的频道列表
-spec subscribed(cowboy_req:req(), map()) -> cowboy_req:req().
subscribed(Req0, State) ->
    Uid = maps:get(current_uid, State),
    Qs = cowboy_req:parse_qs(Req0),
    CursorBin = proplists:get_value(<<"cursor">>, Qs, <<>>),
    Cursor =
        case CursorBin of
            <<>> -> undefined;
            _ -> parse_qs_int(CursorBin, 0, 0, 16#7fffffff)
        end,
    Limit = parse_qs_int(proplists:get_value(<<"limit">>, Qs), 50, 1, 200),

    %% W07.a（C2 已订阅频道）：可选服务端 q（订阅交集内 name ILIKE；
    %% 缺席时行为零变化，cursor/limit 回显保持原样）。
    Q = proplists:get_value(<<"q">>, Qs, <<>>),
    case channel_logic:get_subscribed_channels(Uid, q_qs_binary(Q)) of
        {ok, Channels} ->
            elib_response:success(Req0, #{list => Channels, cursor => Cursor, limit => Limit});
        {error, Msg} ->
            elib_response:error(Req0, normalize_error_binary(Msg, <<"查询失败"/utf8>>))
    end.

%% @doc 获取用户管理的频道列表
-spec managed(cowboy_req:req(), map()) -> cowboy_req:req().
managed(Req0, State) ->
    Uid = maps:get(current_uid, State),
    %% W07.a（C2 我管理频道）：可选服务端 q（管理交集内 name ILIKE）。
    Q = q_qs_binary(proplists:get_value(<<"q">>, cowboy_req:parse_qs(Req0), <<>>)),
    case channel_logic:get_managed_channels(Uid, Q) of
        {ok, Channels} ->
            elib_response:success(Req0, #{list => Channels});
        {error, Msg} ->
            elib_response:error(Req0, normalize_error_binary(Msg, <<"查询失败"/utf8>>))
    end.

%% @doc 获取频道未读聚合
-spec unread_summary(cowboy_req:req(), map()) -> cowboy_req:req().
unread_summary(Req0, State) ->
    Uid = maps:get(current_uid, State),
    case channel_logic:get_unread_summary(Uid) of
        {ok, Summary} ->
            elib_response:success(Req0, Summary);
        {error, Msg} ->
            elib_response:error(Req0, normalize_error_binary(Msg, <<"查询失败"/utf8>>))
    end.

%% @doc 发布频道消息
-spec publish_message(cowboy_req:req(), map()) -> cowboy_req:req().
publish_message(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    ChannelId = resolve_channel_id(Req0, PostVals),
    Content = maps:get(<<"content">>, PostVals, <<>>),
    MsgType = maps:get(<<"msg_type">>, PostVals, <<"text">>),
    Payload = maps:get(<<"payload">>, PostVals, #{}),
    RequestId = maps:get(<<"request_id">>, PostVals, <<>>),

    case ChannelId of
        <<>> ->
            elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
        _ when Content == <<>> ->
            elib_response:error(Req0, <<"消息内容不能为空"/utf8>>);
        _ when not is_binary(RequestId); byte_size(RequestId) > 64 ->
            elib_response:error(Req0, <<"request_id 无效"/utf8>>);
        _ ->
            Result = publish_channel_message(
                Uid, ChannelId, Content, MsgType, Payload, RequestId
            ),
            case Result of
                {ok, Message} ->
                    elib_response:success(Req0, Message);
                %% T7 归档写守卫稳定错误码（980）透传 envelope code
                {error, {Code, Msg}} when is_integer(Code) ->
                    elib_response:error(Req0, Msg, Code);
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

publish_channel_message(Uid, ChannelId, Content, MsgType, Payload, <<>>) ->
    channel_logic:publish_message(Uid, ChannelId, Content, MsgType, Payload);
publish_channel_message(Uid, ChannelId, Content, MsgType, Payload, RequestId) ->
    channel_logic:publish_message(
        Uid, ChannelId, Content, MsgType, Payload, RequestId
    ).

%% @doc 获取频道消息列表
-spec messages(cowboy_req:req(), map()) -> cowboy_req:req().
messages(Req0, State) ->
    Uid = maps:get(current_uid, State),
    case cowboy_req:binding(channel_id, Req0) of
        undefined ->
            elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
        ChannelId ->
            Qs = cowboy_req:parse_qs(Req0),
            CursorBin = proplists:get_value(<<"cursor">>, Qs, <<>>),
            Limit = parse_qs_int(proplists:get_value(<<"limit">>, Qs), 20, 1, 200),
            case channel_logic:get_messages_page(Uid, ChannelId, CursorBin, Limit) of
                {ok, Page} ->
                    elib_response:success(Req0, Page);
                {error, {Code, Msg}} when is_integer(Code) ->
                    %% T7 归档写守卫稳定错误码（980）透传 envelope code
                    elib_response:error(Req0, Msg, Code);
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% @doc 标记消息已读
-spec mark_read(cowboy_req:req(), map()) -> cowboy_req:req().
mark_read(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    ChannelId = resolve_channel_id(Req0, PostVals),
    MessageId = maps:get(<<"message_id">>, PostVals, <<>>),

    case ChannelId of
        <<>> ->
            elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
        _ ->
            case channel_logic:mark_as_read(Uid, ChannelId, MessageId) of
                ok ->
                    elib_response:success(Req0, #{});
                {error, {Code, Msg}} when is_integer(Code) ->
                    %% T7 归档写守卫稳定错误码（980）透传 envelope code
                    elib_response:error(Req0, Msg, Code);
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% @doc 搜索频道
-spec search(cowboy_req:req(), map()) -> cowboy_req:req().
search(Req0, _State) ->
    Qs = cowboy_req:parse_qs(Req0),
    Keyword = proplists:get_value(<<"keyword">>, Qs, <<>>),
    Limit = parse_qs_int(proplists:get_value(<<"limit">>, Qs), 20, 1, 200),

    case Keyword of
        <<>> ->
            elib_response:success(Req0, #{list => []});
        _ ->
            case channel_logic:search_channels(Keyword, Limit) of
                {ok, Channels} ->
                    elib_response:success(Req0, #{list => Channels});
                {error, Msg} ->
                    elib_response:error(Req0, normalize_error_binary(Msg, <<"查询失败"/utf8>>))
            end
    end.

%% @doc 发现频道（推荐）
-spec discover(cowboy_req:req(), map()) -> cowboy_req:req().
discover(Req0, _State) ->
    Qs = cowboy_req:parse_qs(Req0),
    Limit = parse_qs_int(proplists:get_value(<<"limit">>, Qs), 20, 1, 200),
    _Category = proplists:get_value(<<"category">>, Qs, undefined),

    case channel_logic:get_discover_channels(Limit) of
        {ok, Channels} ->
            elib_response:success(Req0, #{list => Channels});
        {error, Msg} ->
            elib_response:error(Req0, normalize_error_binary(Msg, <<"查询失败"/utf8>>))
    end.

%% @doc 添加频道管理员
-spec add_admin(cowboy_req:req(), map()) -> cowboy_req:req().
add_admin(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    ChannelId = resolve_channel_id(Req0, PostVals),
    NewAdminUid = decode_positive_id(maps:get(<<"user_id">>, PostVals, <<>>)),
    Role = elib_cnv:safe_to_integer(maps:get(<<"role">>, PostVals, 1)),

    case ChannelId of
        <<>> ->
            elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
        _ when Role < 1; Role > 3 ->
            elib_response:error(Req0, <<"角色值必须在1-3之间"/utf8>>);
        _ when NewAdminUid =:= 0 ->
            elib_response:error(Req0, <<"用户ID不能为空"/utf8>>);
        _ ->
            case channel_logic:add_admin(Uid, ChannelId, NewAdminUid, Role) of
                ok ->
                    elib_response:success(Req0, #{});
                {error, {Code, Msg}} when is_integer(Code) ->
                    %% T7 归档写守卫稳定错误码（980）透传 envelope code
                    elib_response:error(Req0, Msg, Code);
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% ===================================================================
%% 统计相关 API
%% ===================================================================

-spec stats(cowboy_req:req(), map()) -> cowboy_req:req().
stats(Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    case cowboy_req:binding(channel_id, Req0) of
        undefined ->
            elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
        ChannelId ->
            ChannelIdInt = elib_cnv:safe_to_integer(ChannelId),
            case channel_logic_subscription:is_subscribed(ChannelIdInt, Uid) of
                false ->
                    elib_response:error(Req0, <<"无权限查看该频道统计"/utf8>>, 403);
                true ->
                    case channel_logic:get_channel_stats(Uid, ChannelId) of
                        {ok, Stats} ->
                            elib_response:success(Req0, Stats);
                        {error, Msg} ->
                            elib_response:error(Req0, Msg)
                    end
            end
    end.

-spec stats_daily(cowboy_req:req(), map()) -> cowboy_req:req().
stats_daily(Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    case cowboy_req:binding(channel_id, Req0) of
        undefined ->
            elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
        ChannelId ->
            Qs = cowboy_req:parse_qs(Req0),
            Days = parse_qs_int(proplists:get_value(<<"days">>, Qs), 7, 1, 365),
            case channel_logic:get_daily_stats(Uid, ChannelId, Days) of
                {ok, Stats} ->
                    elib_response:success(Req0, #{list => Stats});
                {error, {Code, Msg}} when is_integer(Code) ->
                    %% T7 归档写守卫稳定错误码（980）透传 envelope code
                    elib_response:error(Req0, Msg, Code);
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% ===================================================================
%% Internal Functions
%% ===================================================================

-spec resolve_channel_id(cowboy_req:req(), map()) -> binary().
resolve_channel_id(Req0, PostVals) ->
    case binding_or_empty(channel_id, Req0) of
        <<>> -> maps:get(<<"channel_id">>, PostVals, <<>>);
        ChannelId -> ChannelId
    end.

-spec binding_or_empty(atom(), cowboy_req:req()) -> binary().
binding_or_empty(Key, Req0) ->
    case cowboy_req:binding(Key, Req0) of
        undefined -> <<>>;
        Val -> Val
    end.

-spec parse_qs_int(term(), integer(), integer(), integer()) -> integer().
parse_qs_int(undefined, Default, _Min, _Max) ->
    Default;
parse_qs_int(Value, Default, Min, Max) ->
    case safe_to_integer(Value) of
        {ok, Int} when Int < Min ->
            Min;
        {ok, Int} when Int > Max ->
            Max;
        {ok, Int} ->
            Int;
        error ->
            Default
    end.

-spec safe_to_integer(term()) -> {ok, integer()} | error.
safe_to_integer(Value) when is_integer(Value) ->
    {ok, Value};
safe_to_integer(Value) when is_binary(Value) ->
    try
        {ok, binary_to_integer(Value)}
    catch
        _:_ -> error
    end;
safe_to_integer(Value) when is_list(Value) ->
    try
        {ok, list_to_integer(Value)}
    catch
        _:_ -> error
    end;
safe_to_integer(_) ->
    error.

-spec decode_positive_id(term()) -> integer().
decode_positive_id(Value) when is_integer(Value), Value > 0 ->
    Value;
decode_positive_id(Value) ->
    try elib_cnv:safe_to_integer(Value) of
        Id when is_integer(Id), Id > 0 ->
            Id;
        _ ->
            0
    catch
        _:_ -> 0
    end.

-spec normalize_non_empty_binary(binary()) -> binary().
normalize_non_empty_binary(Value) ->
    list_to_binary(string:trim(binary_to_list(Value))).

-spec normalize_error_binary(term(), binary()) -> binary().
normalize_error_binary(Msg, Default) ->
    case Msg of
        Value when is_binary(Value); is_list(Value); is_integer(Value) ->
            case normalize_non_empty_binary(Value) of
                <<>> ->
                    Default;
                Bin ->
                    Bin
            end;
        _ ->
            %% atom / epgsql 错误元组等：不 dump term 给用户，记日志后用中文兜底
            ?ERROR_LOG([<<"channel_handler op failed">>, Msg]),
            Default
    end.

%% @doc 验证频道访问策略组合（ADR §8.3.1）
-spec validate_access_policy(non_neg_integer(), non_neg_integer(), non_neg_integer()) -> boolean().
% C1: public/free/open
validate_access_policy(0, 0, 0) -> true;
% C2: private/free/invite
validate_access_policy(1, 0, 1) -> true;
% C3: public/paid/purchase
validate_access_policy(0, 1, 3) -> true;
% C4: private/paid/purchase
validate_access_policy(1, 1, 3) -> true;
validate_access_policy(_, _, _) -> false.

%% GZAPP-02/G5 企业频道内部化：workspace scope 强制非公开。
%% 公开组合收敛到对应私有组合（C1→C2、C3→C4）；personal scope 零行为变化。
%% 收敛后仍是合法组合（validate_access_policy 复核），客户端显式传公开值
%% 不报错——服务端强制私有（内部化语义），create 响应回传实际 visibility。
-spec internalize_visibility(binary(), non_neg_integer(), non_neg_integer()) ->
    {non_neg_integer(), non_neg_integer()}.
internalize_visibility(<<"workspace">>, 0, 0) -> {1, 1};
internalize_visibility(<<"workspace">>, 0, 3) -> {1, 3};
internalize_visibility(_Scope, Visibility, JoinPolicy) -> {Visibility, JoinPolicy}.

%% W07.a：query string 里的 q 统一转 binary（非二进制容错）。
-spec q_qs_binary(term()) -> binary().
q_qs_binary(V) when is_binary(V) -> V;
q_qs_binary(V) -> elib_cnv:safe_to_binary(V).
