-module(adm_channel_handler).
-compile([nowarn_deprecated_catch]).

%%%
% adm_channel 控制器模块
% 频道管理后台 API
%%%
-behavior(cowboy_rest).

-export([init/2]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include("common.hrl").
-include("error_code.hrl").

-ifdef(TEST).
-export([validate_price_inputs/4]).
-export([validate_price_channel/1]).
-endif.

-define(ADM_CHANNEL_AUDIT_TYPE, 901).
-define(CHANNEL_CACHE_KEY(ChannelId), {channel, ChannelId}).
-define(CHANNEL_SUBS_KEY(ChannelId), {channel_subs, ChannelId}).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 初始化 REST 处理器
-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 =
        case imboy_plugin_registry:required_feature(admin, adm_channel_handler, Action) of
            undefined ->
                dispatch(Action, Method, Req0, State);
            Feature ->
                case imboy_feature:ensure_enabled(Req0, Feature) of
                    ok ->
                        dispatch(Action, Method, Req0, State);
                    {error, RespReq} ->
                        RespReq
                end
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

-spec dispatch(atom() | false, binary(), cowboy_req:req(), map()) -> cowboy_req:req().
dispatch(list, Method, Req0, _State) -> list(Method, Req0);
dispatch(detail, Method, Req0, State) -> detail(Method, Req0, State);
dispatch(messages, Method, Req0, _State) -> messages(Method, Req0);
dispatch(subscribers, Method, Req0, _State) -> subscribers(Method, Req0);
dispatch(remove_subscriber, Method, Req0, State) -> remove_subscriber_action(Method, Req0, State);
dispatch(admins, Method, Req0, _State) -> admins(Method, Req0);
dispatch(update_admin_role, Method, Req0, State) -> update_admin_role_action(Method, Req0, State);
dispatch(remove_admin, Method, Req0, State) -> remove_admin_action(Method, Req0, State);
dispatch(invitations, Method, Req0, _State) -> invitations(Method, Req0);
dispatch(orders, Method, Req0, _State) -> orders(Method, Req0);
dispatch(stats, Method, Req0, _State) -> stats(Method, Req0);
dispatch(pin_message, Method, Req0, State) -> pin_message_action(Method, Req0, State);
dispatch(delete_message, Method, Req0, State) -> delete_message_action(Method, Req0, State);
dispatch(search, Method, Req0, _State) -> search(Method, Req0);
dispatch(delete, Method, Req0, State) -> delete_action(Method, Req0, State);
dispatch(set_price, Method, Req0, State) -> set_price_action(Method, Req0, State);
dispatch(refund_order, Method, Req0, State) -> refund_order_action(Method, Req0, State);
dispatch(false, _Method, Req0, _State) -> Req0.

%% @doc 管理端代发退款：对任意频道订单发起退款（需 channel_order_refund 权限）。
%%      区别于买家本人退款 /v1/channel/order/refund：此处不校验订单归属。
-spec refund_order_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
refund_order_action(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"channel_order_refund">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            PostVals = elib_param:post(Req0),
            OrderNo = maps:get(<<"order_no">>, PostVals, <<>>),
            Reason = maps:get(<<"refund_reason">>, PostVals, <<>>),
            case {OrderNo, Reason} of
                {<<>>, _} ->
                    elib_response:error(Req0, <<"缺少订单号"/utf8>>, ?ERR_BAD_REQUEST);
                {_, <<>>} ->
                    elib_response:error(Req0, <<"退款原因必填"/utf8>>, ?ERR_BAD_REQUEST);
                _ ->
                    case channel_logic_order:admin_refund_order(OrderNo, Reason) of
                        ok ->
                            _ = adm_operation_log_ds:insert(
                                maps:get(adm_user_id, State, 0),
                                <<"channel_order_refund">>,
                                0,
                                <<"channel_order">>,
                                #{<<"order_no">> => OrderNo, <<"reason">> => Reason},
                                elib_req:peer_ip(Req0)
                            ),
                            elib_response:success(Req0, #{<<"order_no">> => OrderNo});
                        {error, Msg} ->
                            elib_response:error(Req0, Msg, ?ERR_BUSINESS_FAILED)
                    end
            end
    end;
refund_order_action(_, Req0, _State) ->
    elib_response:error(Req0, <<"方法不允许"/utf8>>, ?ERR_BAD_REQUEST).

%% @doc 获取频道列表
-spec list(binary(), cowboy_req:req()) -> cowboy_req:req().
list(<<"GET">>, Req0) ->
    {Page, Size} = elib_param:page(Req0),
    Qs = cowboy_req:parse_qs(Req0),
    StatusFilter = proplists:get_value(<<"status">>, Qs, <<"-1">>),

    Column = <<
        "id, name, type, creator_uid as owner_id, custom_id, description, avatar, "
        "subscriber_count, status, created_at, updated_at"
    >>,

    StatusWhere =
        case StatusFilter of
            <<"-1">> -> #{};
            <<"1">> -> #{status => 1};
            <<"0">> -> #{status => 0};
            _ -> #{}
        end,

    %% 可选 type 过滤（付费频道 type=2 等）下沉到服务端分页，
    %% 避免前端按 type 过滤导致 total/page 与可见行数不一致
    Where =
        case proplists:get_value(<<"type">>, Qs) of
            undefined -> StatusWhere;
            <<>> -> StatusWhere;
            TypeBin -> StatusWhere#{type => ec_cnv:to_integer(TypeBin)}
        end,

    case channel_ds:page(Column, Where, <<"id desc">>, Page, Size) of
        {ok, Payload} ->
            Payload2 = normalize_channel_payload(Payload),
            elib_response:success(Req0, Payload2);
        {error, Reason} ->
            ?DEBUG_LOG("频道列表查询失败: ~p", [Reason]),
            elib_response:error(Req0, <<"查询失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR)
    end;
list(_, Req0) ->
    Req0.

%% @doc 获取频道详情
-spec detail(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
detail(<<"GET">>, Req0, _State) ->
    case parse_channel_id(Req0) of
        {error, Msg} ->
            elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
        {ok, ChannelId} ->
            case channel_ds:find_by_id_with_price(ChannelId) of
                {error, _} ->
                    elib_response:error(Req0, <<"频道不存在"/utf8>>, ?ERR_NOT_FOUND);
                Channel ->
                    Channel2 = normalize_channel(Channel),
                    elib_response:success(Req0, Channel2)
            end
    end;
detail(<<"PUT">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"channels:update">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            case parse_channel_id(Req0) of
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                {ok, ChannelId} ->
                    PostVals = elib_param:post(Req0),
                    case build_update_data(PostVals) of
                        {error, Msg} ->
                            elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                        {ok, UpdateData} ->
                            case
                                channel_ds:update(
                                    ChannelId, UpdateData#{updated_at => elib_dt:now()}
                                )
                            of
                                {ok, _} ->
                                    elib_response:success(Req0, #{}, <<"频道已更新"/utf8>>);
                                {error, Reason} ->
                                    ?DEBUG_LOG("更新频道失败: ~p", [Reason]),
                                    elib_response:error(
                                        Req0, <<"更新失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR
                                    )
                            end
                    end
            end
    end;
detail(_, Req0, _State) ->
    Req0.

%% @doc 获取频道消息列表（分页）
-spec messages(binary(), cowboy_req:req()) -> cowboy_req:req().
messages(<<"GET">>, Req0) ->
    case parse_channel_id(Req0) of
        {error, Msg} ->
            elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
        {ok, ChannelId} ->
            {Page, Size} = elib_param:page(Req0),
            Column = <<
                "id, channel_id, author_id, author_name, content, msg_type, "
                "is_pinned, view_count, created_at, updated_at"
            >>,
            %% 只返回未删除消息（status=1），否则删除后行仍留在列表
            Where = #{channel_id => ChannelId, status => 1},
            case channel_message_ds:page(Column, Where, <<"id desc">>, Page, Size) of
                {ok, Payload} ->
                    Payload2 = normalize_message_payload(Payload),
                    elib_response:success(Req0, Payload2);
                {error, Reason} ->
                    ?DEBUG_LOG("频道消息列表查询失败: ~p", [Reason]),
                    elib_response:error(Req0, <<"查询失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR)
            end
    end;
messages(_, Req0) ->
    Req0.

%% @doc 获取频道订阅者列表（分页）
-spec subscribers(binary(), cowboy_req:req()) -> cowboy_req:req().
subscribers(<<"GET">>, Req0) ->
    case parse_channel_id(Req0) of
        {error, Msg} ->
            elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
        {ok, ChannelId} ->
            {Page, Size} = elib_param:page(Req0),
            Column = <<
                "id, channel_id, user_id, is_pinned, unread_count, "
                "last_read_at, subscribed_at"
            >>,
            Where = #{channel_id => ChannelId, status => 1},
            case channel_subscription_ds:page(Column, Where, <<"id desc">>, Page, Size) of
                {ok, Payload} ->
                    elib_response:success(Req0, enrich_payload_users(Payload, [<<"user_id">>]));
                {error, Reason} ->
                    ?DEBUG_LOG("频道订阅者列表查询失败: ~p", [Reason]),
                    elib_response:error(Req0, <<"查询失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR)
            end
    end;
subscribers(_, Req0) ->
    Req0.

%% @doc 管理后台移除订阅者
-spec remove_subscriber_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
remove_subscriber_action(<<"DELETE">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"channels:update">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            case parse_channel_user_ids(Req0) of
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                {ok, ChannelId, UserId} ->
                    Result = elib_pg:with_tx(fun(Conn) ->
                        case channel_subscription_ds:delete(Conn, ChannelId, UserId) of
                            {ok, Affected} when Affected > 0 ->
                                case channel_ds:increment_subscribers(Conn, ChannelId, -1) of
                                    {ok, _} ->
                                        changed;
                                    {error, Reason} ->
                                        throw({abort_tx, Reason})
                                end;
                            {ok, 0} ->
                                noop;
                            {error, Reason} ->
                                throw({abort_tx, Reason})
                        end
                    end),
                    case Result of
                        changed ->
                            flush_channel_cache(ChannelId),
                            _ = audit_channel_governance(
                                maps:get(adm_user_id, State, 0),
                                ChannelId,
                                <<"remove_subscriber">>,
                                UserId,
                                #{<<"scope">> => <<"subscription">>}
                            ),
                            elib_response:success(Req0, #{}, <<"订阅者已移除"/utf8>>);
                        noop ->
                            flush_channel_cache(ChannelId),
                            elib_response:success(Req0, #{}, <<"订阅者已移除"/utf8>>);
                        {error, Reason} ->
                            ?DEBUG_LOG("移除频道订阅者失败: ~p", [Reason]),
                            elib_response:error(
                                Req0, <<"移除失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR
                            )
                    end
            end
    end;
remove_subscriber_action(_, Req0, _State) ->
    Req0.

%% @doc 获取频道管理员列表（分页）
-spec admins(binary(), cowboy_req:req()) -> cowboy_req:req().
admins(<<"GET">>, Req0) ->
    case parse_channel_id(Req0) of
        {error, Msg} ->
            elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
        {ok, ChannelId} ->
            {Page, Size} = elib_param:page(Req0),
            Column = <<"id, channel_id, user_id, role, created_at">>,
            Where = #{channel_id => ChannelId},
            case channel_admin_ds:page(Column, Where, <<"role desc, id desc">>, Page, Size) of
                {ok, Payload} ->
                    elib_response:success(Req0, enrich_payload_users(Payload, [<<"user_id">>]));
                {error, Reason} ->
                    ?DEBUG_LOG("频道管理员列表查询失败: ~p", [Reason]),
                    elib_response:error(Req0, <<"查询失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR)
            end
    end;
admins(_, Req0) ->
    Req0.

%% @doc 管理后台更新频道管理员角色
-spec update_admin_role_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
update_admin_role_action(<<"PUT">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"channels:update">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            case parse_channel_user_ids(Req0) of
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                {ok, ChannelId, UserId} ->
                    PostVals = elib_param:post(Req0),
                    Role = ec_cnv:to_integer(maps:get(<<"role">>, PostVals, 0)),
                    case Role < 1 orelse Role > 3 of
                        true ->
                            elib_response:error(
                                Req0, <<"角色值必须在1-3之间"/utf8>>, ?ERR_BAD_REQUEST
                            );
                        false ->
                            case channel_admin_ds:find(ChannelId, UserId) of
                                AdminRow when map_size(AdminRow) =:= 0 ->
                                    elib_response:error(
                                        Req0, <<"用户不是该频道管理员"/utf8>>, ?ERR_BAD_REQUEST
                                    );
                                AdminRow ->
                                    CurrentRole = maps:get(<<"role">>, AdminRow, 0),
                                    case CurrentRole =:= 3 andalso Role =/= 3 of
                                        true ->
                                            elib_response:error(
                                                Req0, <<"创建者角色不可修改"/utf8>>, ?ERR_BAD_REQUEST
                                            );
                                        false ->
                                            case
                                                channel_admin_ds:update_role(
                                                    ChannelId, UserId, Role
                                                )
                                            of
                                                {ok, _} ->
                                                    _ = audit_channel_governance(
                                                        maps:get(adm_user_id, State, 0),
                                                        ChannelId,
                                                        <<"update_admin_role">>,
                                                        UserId,
                                                        #{<<"role">> => Role}
                                                    ),
                                                    elib_response:success(
                                                        Req0, #{}, <<"管理员角色已更新"/utf8>>
                                                    );
                                                {error, Reason} ->
                                                    ?DEBUG_LOG(
                                                        "更新频道管理员角色失败: ~p", [Reason]
                                                    ),
                                                    elib_response:error(
                                                        Req0,
                                                        <<"更新失败"/utf8>>,
                                                        ?ERR_INTERNAL_SERVER_ERROR
                                                    )
                                            end
                                    end
                            end
                    end
            end
    end;
update_admin_role_action(_, Req0, _State) ->
    Req0.

%% @doc 管理后台移除频道管理员
-spec remove_admin_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
remove_admin_action(<<"DELETE">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"channels:update">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            case parse_channel_user_ids(Req0) of
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                {ok, ChannelId, UserId} ->
                    case channel_admin_ds:find(ChannelId, UserId) of
                        AdminRow when map_size(AdminRow) =:= 0 ->
                            elib_response:error(
                                Req0, <<"用户不是该频道管理员"/utf8>>, ?ERR_BAD_REQUEST
                            );
                        AdminRow ->
                            CurrentRole = maps:get(<<"role">>, AdminRow, 0),
                            case CurrentRole =:= 3 of
                                true ->
                                    elib_response:error(
                                        Req0, <<"创建者不可移除"/utf8>>, ?ERR_BAD_REQUEST
                                    );
                                false ->
                                    case channel_admin_ds:delete(ChannelId, UserId) of
                                        {ok, _} ->
                                            _ = audit_channel_governance(
                                                maps:get(adm_user_id, State, 0),
                                                ChannelId,
                                                <<"remove_admin">>,
                                                UserId,
                                                #{<<"scope">> => <<"admin">>}
                                            ),
                                            elib_response:success(Req0, #{}, <<"管理员已移除"/utf8>>);
                                        {error, Reason} ->
                                            ?DEBUG_LOG("移除频道管理员失败: ~p", [Reason]),
                                            elib_response:error(
                                                Req0, <<"移除失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR
                                            )
                                    end
                            end
                    end
            end
    end;
remove_admin_action(_, Req0, _State) ->
    Req0.

%% @doc 获取频道邀请列表（分页）
-spec invitations(binary(), cowboy_req:req()) -> cowboy_req:req().
invitations(<<"GET">>, Req0) ->
    case parse_channel_id(Req0) of
        {error, Msg} ->
            elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
        {ok, ChannelId} ->
            {Page, Size} = elib_param:page(Req0),
            Column = <<
                "id, channel_id, inviter_uid, invitee_uid, invitation_code, status, "
                "message, expires_at, accepted_at, created_at, updated_at"
            >>,
            Where = #{channel_id => ChannelId},
            case channel_invitation_ds:page(Column, Where, <<"id desc">>, Page, Size) of
                {ok, Payload0} ->
                    Payload1 = enrich_payload_users(Payload0, [<<"inviter_uid">>]),
                    Payload2 = enrich_payload_users(Payload1, [<<"invitee_uid">>]),
                    elib_response:success(Req0, Payload2);
                {error, Reason} ->
                    ?DEBUG_LOG("频道邀请列表查询失败: ~p", [Reason]),
                    elib_response:error(Req0, <<"查询失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR)
            end
    end;
invitations(_, Req0) ->
    Req0.

%% @doc 获取频道订单列表（分页）
-spec orders(binary(), cowboy_req:req()) -> cowboy_req:req().
orders(<<"GET">>, Req0) ->
    case parse_channel_id(Req0) of
        {error, Msg} ->
            elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
        {ok, ChannelId} ->
            {Page, Size} = elib_param:page(Req0),
            Column = <<
                "id, channel_id, user_id, order_no, amount, currency, status, payment_method, "
                "payment_no, payment_at, subscription_start_at, subscription_end_at, expires_at, "
                "refund_reason, refund_at, created_at, updated_at"
            >>,
            Where = #{channel_id => ChannelId},
            case channel_order_ds:page(Column, Where, <<"id desc">>, Page, Size) of
                {ok, Payload} ->
                    elib_response:success(Req0, enrich_payload_users(Payload, [<<"user_id">>]));
                {error, Reason} ->
                    ?DEBUG_LOG("频道订单列表查询失败: ~p", [Reason]),
                    elib_response:error(Req0, <<"查询失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR)
            end
    end;
orders(_, Req0) ->
    Req0.

%% @doc 获取频道统计看板数据（订阅、消息、阅读、互动）
-spec stats(binary(), cowboy_req:req()) -> cowboy_req:req().
stats(<<"GET">>, Req0) ->
    case parse_channel_id(Req0) of
        {error, Msg} ->
            elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
        {ok, ChannelId} ->
            ChannelIdBin = integer_to_binary(ChannelId),
            case channel_logic:get_channel_stats_admin(ChannelIdBin) of
                {ok, Stats} ->
                    Payload = #{
                        <<"channel_id">> => ChannelId,
                        <<"subscriber_count">> => maps:get(<<"subscriber_count">>, Stats, 0),
                        <<"total_messages">> => maps:get(<<"total_messages">>, Stats, 0),
                        <<"total_views">> => maps:get(<<"total_views">>, Stats, 0),
                        <<"total_reactions">> => maps:get(<<"total_reactions">>, Stats, 0)
                    },
                    elib_response:success(Req0, Payload);
                {error, <<"频道不存在"/utf8>>} ->
                    elib_response:error(Req0, <<"频道不存在"/utf8>>, ?ERR_NOT_FOUND);
                {error, Reason} ->
                    ?DEBUG_LOG("频道统计查询失败: ~p", [Reason]),
                    elib_response:error(Req0, <<"查询失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR)
            end
    end;
stats(_, Req0) ->
    Req0.

%% @doc 管理后台置顶/取消置顶频道消息
-spec pin_message_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
pin_message_action(<<"PUT">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"channels:pin">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            case parse_message_scope_ids(Req0) of
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                {ok, ChannelId, MessageId} ->
                    PostVals = elib_param:post(Req0),
                    IsPinned = parse_pinned(maps:get(<<"pinned">>, PostVals, true)),
                    case ensure_message_belongs_to_channel(ChannelId, MessageId) of
                        ok ->
                            case
                                channel_message_ds:update(
                                    MessageId,
                                    #{is_pinned => IsPinned, updated_at => elib_dt:now()}
                                )
                            of
                                {ok, _} ->
                                    _ = audit_channel_governance(
                                        maps:get(adm_user_id, State, 0),
                                        ChannelId,
                                        <<"pin_message">>,
                                        MessageId,
                                        #{<<"pinned">> => IsPinned}
                                    ),
                                    elib_response:success(Req0, #{}, <<"消息状态已更新"/utf8>>);
                                {error, Reason} ->
                                    ?DEBUG_LOG("更新消息置顶状态失败: ~p", [Reason]),
                                    elib_response:error(
                                        Req0, <<"更新失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR
                                    )
                            end;
                        {error, Msg2} ->
                            elib_response:error(Req0, Msg2, ?ERR_BAD_REQUEST)
                    end
            end
    end;
pin_message_action(_, Req0, _State) ->
    Req0.

%% @doc 管理后台删除频道消息
-spec delete_message_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
delete_message_action(<<"DELETE">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"channels:delete">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            case parse_message_scope_ids(Req0) of
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                {ok, ChannelId, MessageId} ->
                    case ensure_message_belongs_to_channel(ChannelId, MessageId) of
                        ok ->
                            case channel_message_ds:delete(MessageId) of
                                {ok, _} ->
                                    _ = audit_channel_governance(
                                        maps:get(adm_user_id, State, 0),
                                        ChannelId,
                                        <<"delete_message">>,
                                        MessageId,
                                        #{<<"scope">> => <<"message">>}
                                    ),
                                    elib_response:success(Req0, #{}, <<"消息已删除"/utf8>>);
                                {error, Reason} ->
                                    ?DEBUG_LOG("删除频道消息失败: ~p", [Reason]),
                                    elib_response:error(
                                        Req0, <<"删除失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR
                                    )
                            end;
                        {error, Msg2} ->
                            elib_response:error(Req0, Msg2, ?ERR_BAD_REQUEST)
                    end
            end
    end;
delete_message_action(_, Req0, _State) ->
    Req0.

%% @doc 搜索频道
-spec search(binary(), cowboy_req:req()) -> cowboy_req:req().
search(<<"GET">>, Req0) ->
    Qs = cowboy_req:parse_qs(Req0),
    Keyword = proplists:get_value(<<"keyword">>, Qs, <<>>),
    Limit =
        case proplists:get_value(<<"limit">>, Qs) of
            undefined ->
                20;
            LimitBin ->
                try binary_to_integer(LimitBin) of
                    L -> L
                catch
                    error:badarg -> 20
                end
        end,

    case Keyword of
        <<>> ->
            elib_response:success(Req0, #{list => [], page => 1, size => Limit, total => 0});
        _ ->
            Column = <<
                "id, name, type, creator_uid as owner_id, custom_id, description, "
                "subscriber_count, status, created_at"
            >>,
            case channel_ds:search(Keyword, Limit, Column) of
                {ok, Channels} ->
                    Channels2 = [normalize_channel(Channel) || Channel <- Channels],
                    elib_response:success(Req0, #{
                        list => Channels2, page => 1, size => Limit, total => length(Channels2)
                    });
                {error, _} ->
                    elib_response:success(Req0, #{list => [], page => 1, size => Limit, total => 0})
            end
    end;
search(_, Req0) ->
    Req0.

%% @doc 删除频道（软删除）
-spec delete_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
%% @doc 设置/更新付费频道价格（UPSERT）
%% PUT /adm/channel/:channel_id/price
%% Body: {price_fen, currency, subscription_type, original_price_fen, description}
%% price_fen/original_price_fen 单位为分，存入 DB 时转换为元
-spec set_price_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
set_price_action(<<"PUT">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"channels:update">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            ChannelIdBin = cowboy_req:binding(channel_id, Req0, undefined),
            PostVals = elib_param:post(Req0),
            PriceFen = maps:get(<<"price_fen">>, PostVals, undefined),
            case {ChannelIdBin, PriceFen} of
                {undefined, _} ->
                    elib_response:error(Req0, <<"channel_id 不能为空"/utf8>>, ?ERR_BAD_REQUEST);
                {_, undefined} ->
                    elib_response:error(Req0, <<"price_fen 不能为空"/utf8>>, ?ERR_BAD_REQUEST);
                _ ->
                    OrigFen = strict_integer(maps:get(<<"original_price_fen">>, PostVals, 0)),
                    Currency = maps:get(<<"currency">>, PostVals, <<"CNY">>),
                    SubType = strict_integer(maps:get(<<"subscription_type">>, PostVals, 1)),
                    Desc = maps:get(<<"description">>, PostVals, <<>>),
                    ChannelId = ec_cnv:to_integer(ChannelIdBin),
                    PriceFenInt = strict_integer(PriceFen),
                    case validate_price_inputs(ChannelId, PriceFenInt, OrigFen, SubType) of
                        {error, Msg} ->
                            elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                        ok ->
                            case validate_price_channel(ChannelId) of
                                {error, Msg2} ->
                                    elib_response:error(Req0, Msg2, ?ERR_BAD_REQUEST);
                                ok ->
                                    PriceYuan = PriceFenInt / 100.0,
                                    OrigYuan = ec_cnv:to_float(OrigFen) / 100.0,
                                    Now = elib_dt:now(),
                                    PriceTb = elib_pg_sql:public_tablename(<<"channel_price">>),
                                    NewId = elib_tsid:generate(channel_price),
                                    Sql =
                                        <<"INSERT INTO ", PriceTb/binary,
                                            " (id, channel_id, price, currency, subscription_type, original_price,"
                                            " description, status, created_at, updated_at)"
                                            " VALUES ($1,$2,$3,$4,$5,$6,$7,1,$8,$8)"
                                            " ON CONFLICT (channel_id) DO UPDATE SET"
                                            " price=$3, currency=$4, subscription_type=$5, original_price=$6,"
                                            " description=$7, status=1, updated_at=$8">>,
                                    case
                                        elib_pg:query(Sql, [
                                            NewId,
                                            ChannelId,
                                            PriceYuan,
                                            Currency,
                                            SubType,
                                            OrigYuan,
                                            Desc,
                                            Now
                                        ])
                                    of
                                        {ok, _} ->
                                            elib_response:success(Req0, #{}, <<"频道价格已更新"/utf8>>);
                                        {error, Reason} ->
                                            ?DEBUG_LOG("设置频道价格失败: ~p", [Reason]),
                                            elib_response:error(
                                                Req0, <<"设置价格失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR
                                            )
                                    end
                            end
                    end
            end
    end;
set_price_action(_, Req0, _State) ->
    Req0.

%% @doc 价格接口的服务端边界校验；不能只依赖 admin 前端表单。
%% price_fen 必须是正整数，原价只能为非负整数，订阅类型限定为一次性/月/年。
-spec validate_price_inputs(term(), term(), term(), term()) -> ok | {error, binary()}.
validate_price_inputs(ChannelId0, PriceFen0, OriginalFen0, SubType0) ->
    ChannelId = strict_integer(ChannelId0),
    PriceFen = strict_integer(PriceFen0),
    OriginalFen = strict_integer(OriginalFen0),
    SubType = strict_integer(SubType0),
    case
        {
            ChannelId > 0,
            PriceFen > 0,
            OriginalFen >= 0,
            lists:member(SubType, [1, 2, 3])
        }
    of
        {true, true, true, true} ->
            ok;
        _ ->
            {error, <<"价格配置无效：价格必须大于 0，订阅类型必须为 1/2/3"/utf8>>}
    end.

%% 价格只能挂在存活的付费频道上；否则运营端可能误给公开/私有频道
%% 写入价格配置，导致后台显示与内容访问规则不一致。
-spec validate_price_channel(term()) -> ok | {error, binary()}.
validate_price_channel(ChannelId0) ->
    ChannelId = strict_integer(ChannelId0),
    case channel_ds:find_by_id(ChannelId, <<"id,type,status">>) of
        #{<<"type">> := Type0, <<"status">> := Status0} ->
            Type = strict_integer(Type0),
            Status = strict_integer(Status0),
            case {Type, Status} of
                {2, 1} -> ok;
                {_, 1} -> {error, <<"只有付费频道可以配置价格"/utf8>>};
                _ -> {error, <<"频道不存在或已禁用"/utf8>>}
            end;
        _ ->
            {error, <<"频道不存在或已禁用"/utf8>>}
    end.

-spec strict_integer(term()) -> integer().
strict_integer(Value) when is_integer(Value) ->
    Value;
strict_integer(Value) when is_binary(Value) ->
    case catch list_to_integer(string:trim(binary_to_list(Value))) of
        Int when is_integer(Int) -> Int;
        _ -> -1
    end;
strict_integer(Value) when is_list(Value) ->
    case catch list_to_integer(string:trim(Value)) of
        Int when is_integer(Int) -> Int;
        _ -> -1
    end;
strict_integer(_) ->
    -1.

delete_action(<<"DELETE">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"channels:delete">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            PostVals = elib_param:post(Req0),
            ChannelId = maps:get(<<"id">>, PostVals, undefined),

            case ChannelId of
                undefined ->
                    elib_response:error(Req0, <<"频道ID不能为空"/utf8>>, ?ERR_BAD_REQUEST);
                _ ->
                    ChannelIdInt = ec_cnv:to_integer(ChannelId),
                    case channel_ds:delete(ChannelIdInt) of
                        {ok, _} ->
                            elib_response:success(Req0, #{}, <<"频道已删除"/utf8>>);
                        {error, Reason} ->
                            ?DEBUG_LOG("删除频道失败: ~p", [Reason]),
                            elib_response:error(
                                Req0, <<"删除失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR
                            )
                    end
            end
    end;
delete_action(_, Req0, _State) ->
    Req0.

-spec build_update_data(map()) -> {ok, map()} | {error, binary()}.
build_update_data(PostVals) ->
    NameResult =
        case maps:find(<<"name">>, PostVals) of
            error ->
                {ok, undefined};
            {ok, Val} ->
                NameBin0 = ec_cnv:to_binary(Val),
                case byte_size(NameBin0) > 0 of
                    true -> {ok, NameBin0};
                    false -> {error, <<"频道名称不能为空"/utf8>>}
                end
        end,
    case NameResult of
        {error, _} = Err ->
            Err;
        {ok, NameBin} ->
            TypeResult = parse_enum_field(PostVals, <<"type">>, [0, 1, 2], <<"频道类型无效"/utf8>>),
            case TypeResult of
                {error, _} = Err2 ->
                    Err2;
                {ok, TypeVal} ->
                    StatusResult = parse_enum_field(
                        PostVals, <<"status">>, [0, 1], <<"频道状态无效"/utf8>>
                    ),
                    case StatusResult of
                        {error, _} = Err3 ->
                            Err3;
                        {ok, StatusVal} ->
                            Data0 = #{},
                            Data1 = maybe_put(Data0, name, NameBin),
                            Data2 = maybe_put(Data1, type, TypeVal),
                            Data3 = maybe_put(Data2, status, StatusVal),
                            Data4 = maybe_put_binary_non_empty(
                                PostVals, Data3, <<"custom_id">>, custom_id
                            ),
                            Data5 = maybe_put_binary(
                                PostVals, Data4, <<"description">>, description
                            ),
                            Data6 = maybe_put_binary(PostVals, Data5, <<"avatar">>, avatar),
                            case map_size(Data6) > 0 of
                                true -> {ok, Data6};
                                false -> {error, <<"至少提供一个可更新字段"/utf8>>}
                            end
                    end
            end
    end.

-spec parse_enum_field(map(), binary(), [integer()], binary()) ->
    {ok, integer() | undefined} | {error, binary()}.
parse_enum_field(PostVals, Field, Allowed, ErrMsg) ->
    case maps:find(Field, PostVals) of
        error ->
            {ok, undefined};
        {ok, Val} ->
            IntVal = ec_cnv:to_integer(Val),
            case lists:member(IntVal, Allowed) of
                true -> {ok, IntVal};
                false -> {error, ErrMsg}
            end
    end.

-spec maybe_put(map(), atom(), any()) -> map().
maybe_put(Data, _Key, undefined) ->
    Data;
maybe_put(Data, Key, Value) ->
    Data#{Key => Value}.

-spec maybe_put_binary(map(), map(), binary(), atom()) -> map().
maybe_put_binary(PostVals, Data, InKey, OutKey) ->
    case maps:find(InKey, PostVals) of
        error ->
            Data;
        {ok, Val} ->
            Data#{OutKey => ec_cnv:to_binary(Val)}
    end.

-spec maybe_put_binary_non_empty(map(), map(), binary(), atom()) -> map().
maybe_put_binary_non_empty(PostVals, Data, InKey, OutKey) ->
    case maps:find(InKey, PostVals) of
        error ->
            Data;
        {ok, Val} ->
            Bin = ec_cnv:to_binary(Val),
            case byte_size(Bin) > 0 of
                true -> Data#{OutKey => Bin};
                false -> Data
            end
    end.

-spec parse_channel_id(cowboy_req:req()) -> {ok, integer()} | {error, binary()}.
parse_channel_id(Req0) ->
    case cowboy_req:binding(channel_id, Req0) of
        undefined ->
            {error, <<"频道ID不能为空"/utf8>>};
        ChannelIdBin ->
            ChannelId = parse_id(ChannelIdBin),
            case ChannelId > 0 of
                true -> {ok, ChannelId};
                false -> {error, <<"频道ID格式错误"/utf8>>}
            end
    end.

-spec parse_channel_user_ids(cowboy_req:req()) -> {ok, integer(), integer()} | {error, binary()}.
parse_channel_user_ids(Req0) ->
    case parse_channel_id(Req0) of
        {error, _} = Err ->
            Err;
        {ok, ChannelId} ->
            case cowboy_req:binding(user_id, Req0) of
                undefined ->
                    {error, <<"用户ID不能为空"/utf8>>};
                UserIdBin ->
                    UserId = parse_id(UserIdBin),
                    case UserId > 0 of
                        true -> {ok, ChannelId, UserId};
                        false -> {error, <<"用户ID格式错误"/utf8>>}
                    end
            end
    end.

-spec enrich_payload_users(map(), [binary()]) -> map().
enrich_payload_users(Payload, UserIdFields) ->
    List = maps:get(list, Payload, []),
    UserIds = collect_user_ids(List, UserIdFields, []),
    UserMap = fetch_users_map(UserIds),
    List2 = lists:map(
        fun(Item) ->
            attach_users(Item, UserIdFields, UserMap)
        end,
        List
    ),
    maps:remove(items, Payload#{list => List2}).

-spec collect_user_ids([map()], [binary()], [integer()]) -> [integer()].
collect_user_ids([], _Fields, Acc) ->
    lists:usort(Acc);
collect_user_ids([Item | Rest], Fields, Acc0) ->
    Acc1 = lists:foldl(
        fun(Field, Acc) ->
            case maps:get(Field, Item, 0) of
                Uid when is_integer(Uid), Uid > 0 -> [Uid | Acc];
                _ -> Acc
            end
        end,
        Acc0,
        Fields
    ),
    collect_user_ids(Rest, Fields, Acc1).

-spec fetch_users_map([integer()]) -> map().
fetch_users_map([]) ->
    #{};
fetch_users_map(UserIds) ->
    Column = <<"id,account,nickname,avatar,status">>,
    case user_ds:list_by_ids(UserIds, Column) of
        {ok, Rows} ->
            lists:foldl(
                fun(Row, Acc) ->
                    Uid = maps:get(<<"id">>, Row, 0),
                    case Uid of
                        Id when is_integer(Id), Id > 0 -> Acc#{Id => Row};
                        _ -> Acc
                    end
                end,
                #{},
                Rows
            );
        _ ->
            #{}
    end.

-spec attach_users(map(), [binary()], map()) -> map().
attach_users(Item, Fields, UserMap) ->
    lists:foldl(
        fun(Field, Acc) ->
            case maps:get(Field, Acc, 0) of
                Uid when is_integer(Uid), Uid > 0 ->
                    UserKey = user_field_to_key(Field),
                    User = maps:get(Uid, UserMap, #{}),
                    % 编码 ID 字段并附加用户对象
                    Acc#{Field => Uid, UserKey => User};
                _ ->
                    Acc
            end
        end,
        Item,
        Fields
    ).

-spec user_field_to_key(binary()) -> binary().
user_field_to_key(<<"inviter_uid">>) ->
    <<"inviter_user">>;
user_field_to_key(<<"invitee_uid">>) ->
    <<"invitee_user">>;
user_field_to_key(_) ->
    <<"user">>.

-spec parse_message_scope_ids(cowboy_req:req()) -> {ok, integer(), integer()} | {error, binary()}.
parse_message_scope_ids(Req0) ->
    case cowboy_req:binding(channel_id, Req0) of
        undefined ->
            {error, <<"频道ID不能为空"/utf8>>};
        ChannelIdBin ->
            case cowboy_req:binding(message_id, Req0) of
                undefined ->
                    {error, <<"消息ID不能为空"/utf8>>};
                MessageIdBin ->
                    ChannelId = parse_id(ChannelIdBin),
                    MessageId = parse_id(MessageIdBin),
                    case ChannelId > 0 andalso MessageId > 0 of
                        true -> {ok, ChannelId, MessageId};
                        false -> {error, <<"ID格式错误"/utf8>>}
                    end
            end
    end.

-spec parse_id(term()) -> integer().
parse_id(Value) when is_integer(Value), Value > 0 ->
    Value;
parse_id(Value) when is_list(Value) ->
    parse_id(ec_cnv:to_binary(Value));
parse_id(Value) when is_binary(Value), Value =/= <<>> ->
    case elib_type:is_numeric(Value) of
        true ->
            ec_cnv:to_integer(Value);
        false ->
            case catch ec_cnv:to_integer(Value) of
                Id when is_integer(Id), Id > 0 -> Id;
                _ -> 0
            end
    end;
parse_id(_) ->
    0.

-spec ensure_message_belongs_to_channel(integer(), integer()) -> ok | {error, binary()}.
ensure_message_belongs_to_channel(ChannelId, MessageId) ->
    case channel_message_ds:find_by_id(MessageId) of
        {error, _} ->
            {error, <<"消息不存在"/utf8>>};
        Message ->
            case maps:get(<<"channel_id">>, Message, 0) =:= ChannelId of
                true -> ok;
                false -> {error, <<"消息不属于当前频道"/utf8>>}
            end
    end.

-spec parse_pinned(term()) -> boolean().
parse_pinned(Pinned) when is_boolean(Pinned) ->
    Pinned;
parse_pinned(Pinned) when is_integer(Pinned) ->
    Pinned =/= 0;
parse_pinned(Pinned) when is_binary(Pinned) ->
    case to_lower_binary(Pinned) of
        <<"1">> -> true;
        <<"true">> -> true;
        <<"yes">> -> true;
        <<"on">> -> true;
        _ -> false
    end;
parse_pinned(_) ->
    false.

-spec to_lower_binary(binary()) -> binary().
to_lower_binary(Bin) when is_binary(Bin) ->
    <<
        <<
            (if
                C >= $A, C =< $Z -> C + 32;
                true -> C
            end)
        >>
     || <<C>> <= Bin
    >>.

-spec flush_channel_cache(integer()) -> ok.
flush_channel_cache(ChannelId) ->
    imboy_cache:flush(?CHANNEL_SUBS_KEY(ChannelId)),
    imboy_cache:flush(?CHANNEL_CACHE_KEY(ChannelId)),
    ok.

-spec audit_channel_governance(integer(), integer(), binary(), integer(), map()) -> ok.
audit_channel_governance(AdmUserId, ChannelId, Action, TargetId, Extra) ->
    Now = elib_dt:now(),
    AuditBody = #{
        <<"action">> => Action,
        <<"operator_uid">> => AdmUserId,
        <<"channel_id">> => ChannelId,
        <<"target_id">> => TargetId,
        <<"occurred_at">> => Now,
        <<"extra">> => Extra
    },
    try
        _ = user_log_ds:add(#{
            type => ?ADM_CHANNEL_AUDIT_TYPE,
            uid => AdmUserId,
            body => jsone:encode(AuditBody, [native_utf8]),
            remark => <<"adm_channel_governance">>,
            created_at => Now
        }),
        ok
    catch
        Class:Reason:Stacktrace ->
            ?DEBUG_LOG("频道治理审计日志写入失败: ~p", [{Class, Reason, Stacktrace}]),
            ok
    end.

%% @doc 规范化频道分页数据（编码ID字段）
-spec normalize_channel_payload(map()) -> map().
normalize_channel_payload(Payload) ->
    List = maps:get(list, Payload, []),
    List2 = [normalize_channel(Item) || Item <- List],
    maps:remove(items, Payload#{list => List2}).

%% @doc 规范化单条频道数据（编码ID字段）
%% Admin API 将 TSID 整数转为字符串，避免 JS 精度丢失。
-spec normalize_channel(map()) -> map().
normalize_channel(Channel) ->
    elib_id:tsid_keys_to_bin(Channel, [<<"id">>, <<"owner_id">>]).

%% @doc 规范化消息分页数据（编码ID字段）
-spec normalize_message_payload(map()) -> map().
normalize_message_payload(Payload) ->
    List = maps:get(list, Payload, []),
    List2 = [normalize_message(Item) || Item <- List],
    maps:remove(items, Payload#{list => List2}).

%% @doc 规范化单条消息数据（编码ID字段）
%% Admin API 将 TSID 整数转为字符串，避免 JS 精度丢失。
-spec normalize_message(map()) -> map().
normalize_message(Message) ->
    elib_id:tsid_keys_to_bin(Message, [<<"id">>, <<"author_id">>]).

%% ===================================================================
%% EUnit tests.
%% ===================================================================
