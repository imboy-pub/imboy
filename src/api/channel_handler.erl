-module(channel_handler).
%% Thin HTTP adapter for the channel_content domain.
%% Keep transport parsing here and route domain work through channel_logic.

-behavior(cowboy_rest).

-export([init/2]).
-export([handle_action/3]).

-include("log.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 初始化频道处理器
-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 =
        case imboy_plugin_registry:required_feature(api, channel_handler, Action) of
            undefined ->
                handle_action(Action, Req0, State);
            Feature ->
                case imboy_feature:ensure_enabled(Req0, Feature) of
                    ok ->
                        handle_action(Action, Req0, State);
                    {error, RespReq} ->
                        RespReq
                end
        end,
    {ok, Req1, State}.

%% @doc Action 分发处理
-spec handle_action(atom() | false, cowboy_req:req(), map()) -> cowboy_req:req().
handle_action(create, Req, State) -> create(Req, State);
handle_action(show, Req, State) -> show(Req, State);
handle_action(by_custom_id, Req, State) -> by_custom_id(Req, State);
handle_action(update, Req, State) -> update(Req, State);
handle_action(delete, Req, State) -> delete(Req, State);
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
handle_action(remove_admin, Req, State) -> remove_admin(Req, State);
% 统计相关 API
handle_action(stats, Req, State) -> stats(Req, State);
handle_action(record_view, Req, State) -> record_view(Req, State);
handle_action(add_reaction, Req, State) -> add_reaction(Req, State);
handle_action(remove_reaction, Req, State) -> remove_reaction(Req, State);
handle_action(stats_daily, Req, State) -> stats_daily(Req, State);
% 消息管理
handle_action(pin_message, Req, State) -> pin_message(Req, State);
handle_action(delete_message, Req, State) -> delete_message(Req, State);
handle_action(revoke_message, Req, State) -> revoke_message(Req, State);
% 订阅者管理
handle_action(subscribers, Req, State) -> subscribers(Req, State);
handle_action(remove_subscriber, Req, State) -> remove_subscriber(Req, State);
% 管理员列表与角色更新
handle_action(admins, Req, State) -> admins(Req, State);
handle_action(update_admin_role, Req, State) -> update_admin_role(Req, State);
% 频道增量同步
handle_action(sync, Req, State) -> sync(Req, State);
% 邀请相关（私有频道）
handle_action(create_invitation, Req, State) -> create_invitation(Req, State);
handle_action(accept_invitation, Req, State) -> accept_invitation(Req, State);
handle_action(reject_invitation, Req, State) -> reject_invitation(Req, State);
handle_action(my_invitations, Req, State) -> my_invitations(Req, State);
handle_action(sent_invitations, Req, State) -> sent_invitations(Req, State);
% 订单相关（付费频道）
handle_action(create_order, Req, State) -> create_order(Req, State);
handle_action(pay_order, Req, State) -> pay_order(Req, State);
handle_action(my_orders, Req, State) -> my_orders(Req, State);
handle_action(get_order, Req, State) -> get_order(Req, State);
handle_action(false, Req, _State) -> Req.

%% @doc 创建频道
-spec create(cowboy_req:req(), map()) -> cowboy_req:req().
create(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    Name = maps:get(<<"name">>, PostVals, <<>>),
    Type = ec_cnv:to_integer(maps:get(<<"type">>, PostVals, 0)),
    Description = maps:get(<<"description">>, PostVals, <<>>),
    Avatar = maps:get(<<"avatar">>, PostVals, <<>>),
    CustomId = maps:get(<<"custom_id">>, PostVals, undefined),
    Tags = maps:get(<<"tags">>, PostVals, []),

    case Name of
        <<>> ->
            elib_response:error(Req0, <<"频道名称不能为空"/utf8>>);
        _ when Type < 0; Type > 2 ->
            elib_response:error(Req0, <<"频道类型无效"/utf8>>);
        _ ->
            Opts = #{
                description => Description,
                avatar => Avatar,
                custom_id => CustomId,
                tags => Tags
            },
            MaxChannels = 20,  % 每个用户最多创建20个频道
            case channel_logic:create_channel(Uid, Name, Type, Opts, MaxChannels) of
                {ok, Channel} ->
                    elib_response:success(Req0, Channel);
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% @doc 获取频道信息
-spec show(cowboy_req:req(), map()) -> cowboy_req:req().
show(Req0, State) ->
    % 从路径参数获取 channel_id
    Uid = maps:get(current_uid, State, 0),
    case cowboy_req:binding(channel_id, Req0) of
        undefined ->
            elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
        ChannelId ->
            case channel_logic:get_channel(ChannelId, Uid) of
                {ok, Channel} ->
                    elib_response:success(Req0, Channel);
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% @doc 通过自定义ID获取频道
-spec by_custom_id(cowboy_req:req(), map()) -> cowboy_req:req().
by_custom_id(Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    % 从路径参数获取 custom_id
    case cowboy_req:binding(custom_id, Req0) of
        undefined ->
            elib_response:error(Req0, <<"自定义ID不能为空"/utf8>>);
        CustomId ->
            case channel_logic:get_channel_by_custom_id(CustomId, Uid) of
                {ok, Channel} ->
                    elib_response:success(Req0, Channel);
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% @doc 更新频道信息
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
            case channel_logic:update_channel(Uid, ChannelId, Data) of
                {ok, Channel} ->
                    elib_response:success(Req0, Channel);
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

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
    Cursor = case CursorBin of
        <<>> -> undefined;
        _ -> parse_qs_int(CursorBin, 0, 0, 16#7fffffff)
    end,
    Limit = parse_qs_int(proplists:get_value(<<"limit">>, Qs), 50, 1, 200),

    case channel_logic:get_subscribed_channels(Uid) of
        {ok, Channels} ->
            elib_response:success(Req0, #{list => Channels, cursor => Cursor, limit => Limit});
        {error, Msg} ->
            elib_response:error(Req0, normalize_error_binary(Msg, <<"查询失败"/utf8>>))
    end.

%% @doc 获取用户管理的频道列表
-spec managed(cowboy_req:req(), map()) -> cowboy_req:req().
managed(Req0, State) ->
    Uid = maps:get(current_uid, State),
    case channel_logic:get_managed_channels(Uid) of
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

    case ChannelId of
        <<>> ->
            elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
        _ when Content == <<>> ->
            elib_response:error(Req0, <<"消息内容不能为空"/utf8>>);
        _ ->
            case channel_logic:publish_message(Uid, ChannelId, Content, MsgType, Payload) of
                {ok, Message} ->
                    elib_response:success(Req0, Message);
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% @doc 获取频道消息列表
-spec messages(cowboy_req:req(), map()) -> cowboy_req:req().
messages(Req0, State) ->
    Uid = maps:get(current_uid, State),
    % 从路径参数获取 channel_id
    case cowboy_req:binding(channel_id, Req0) of
        undefined ->
            elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
        ChannelId ->
            Qs = cowboy_req:parse_qs(Req0),
            CursorBin = proplists:get_value(<<"cursor">>, Qs, <<>>),
            Limit = parse_qs_int(proplists:get_value(<<"limit">>, Qs), 20, 1, 200),
            Cursor = case CursorBin of
                <<>> -> 0;
                _ -> parse_qs_int(CursorBin, 0, 0, 16#7fffffff)
            end,
            case channel_logic:get_messages(Uid, ChannelId, Cursor, Limit) of
                {ok, Messages} ->
                    elib_response:success(Req0, #{list => Messages});
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

    % 返回公开的、订阅数最多的频道
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
    Role = ec_cnv:to_integer(maps:get(<<"role">>, PostVals, 1)),

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
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% @doc 移除频道管理员
-spec remove_admin(cowboy_req:req(), map()) -> cowboy_req:req().
remove_admin(Req0, State) ->
    Method = cowboy_req:method(Req0),
    case Method of
        <<"PUT">> ->
            % 兼容旧客户端: /admin/:user_id 使用 PUT 更新角色
            update_admin_role(Req0, State);
        <<"DELETE">> ->
            Uid = maps:get(current_uid, State),
            PostVals = elib_param:post(Req0),
            ChannelId = resolve_channel_id(Req0, PostVals),
            AdminUidBin = resolve_user_id_bin(Req0, PostVals),
            AdminUid = decode_positive_id(AdminUidBin),

            case ChannelId of
                <<>> ->
                    elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
                _ when AdminUid =:= 0 ->
                    elib_response:error(Req0, <<"用户ID不能为空"/utf8>>);
                _ ->
                    case channel_logic:remove_admin(Uid, ChannelId, AdminUid) of
                        ok ->
                            elib_response:success(Req0, #{});
                        {error, Msg} ->
                            elib_response:error(Req0, Msg)
                    end
            end;
        _ ->
            elib_response:error(Req0, <<"请求方法不支持"/utf8>>)
    end.

%% ===================================================================
%% 统计相关 API
%% ===================================================================

%% @doc 获取频道统计数据
-spec stats(cowboy_req:req(), map()) -> cowboy_req:req().
stats(Req0, _State) ->
    % 从路径参数获取 channel_id
    case cowboy_req:binding(channel_id, Req0) of
        undefined ->
            elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
        ChannelId ->
            case channel_logic:get_channel_stats(ChannelId) of
                {ok, Stats} ->
                    elib_response:success(Req0, Stats);
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% @doc 记录消息阅读
-spec record_view(cowboy_req:req(), map()) -> cowboy_req:req().
record_view(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    ChannelId = resolve_channel_id(Req0, PostVals),
    MessageId = resolve_message_id(Req0, PostVals),

    case ChannelId of
        <<>> ->
            elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
        _ when MessageId == <<>> ->
            elib_response:error(Req0, <<"消息ID不能为空"/utf8>>);
        _ ->
            case channel_logic:record_message_view(Uid, ChannelId, MessageId) of
                ok ->
                    elib_response:success(Req0, #{});
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% @doc 添加消息反应
-spec add_reaction(cowboy_req:req(), map()) -> cowboy_req:req().
add_reaction(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    ChannelId = resolve_channel_id(Req0, PostVals),
    MessageId = resolve_message_id(Req0, PostVals),
    ReactionType = maps:get(<<"reaction_type">>, PostVals, <<"like">>),

    case ChannelId of
        <<>> ->
            elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
        _ when MessageId == <<>> ->
            elib_response:error(Req0, <<"消息ID不能为空"/utf8>>);
        _ ->
            case channel_logic:add_reaction(Uid, ChannelId, MessageId, ReactionType) of
                ok ->
                    elib_response:success(Req0, #{});
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% @doc 移除消息反应
-spec remove_reaction(cowboy_req:req(), map()) -> cowboy_req:req().
remove_reaction(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    ChannelId = resolve_channel_id(Req0, PostVals),
    MessageId = resolve_message_id(Req0, PostVals),
    ReactionType = resolve_reaction_type(Req0, PostVals),

    case ChannelId of
        <<>> ->
            elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
        _ when MessageId == <<>> ->
            elib_response:error(Req0, <<"消息ID不能为空"/utf8>>);
        _ ->
            case channel_logic:remove_reaction(Uid, ChannelId, MessageId, ReactionType) of
                ok ->
                    elib_response:success(Req0, #{});
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% @doc 获取频道每日统计数据
-spec stats_daily(cowboy_req:req(), map()) -> cowboy_req:req().
stats_daily(Req0, _State) ->
    % 从路径参数获取 channel_id
    case cowboy_req:binding(channel_id, Req0) of
        undefined ->
            elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
        ChannelId ->
            Qs = cowboy_req:parse_qs(Req0),
            Days = parse_qs_int(proplists:get_value(<<"days">>, Qs), 7, 1, 365),

            case channel_logic:get_daily_stats(ChannelId, Days) of
                {ok, Stats} ->
                    elib_response:success(Req0, #{list => Stats});
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% ===================================================================
%% 消息管理 API
%% ===================================================================

%% @doc 置顶/取消置顶消息
-spec pin_message(cowboy_req:req(), map()) -> cowboy_req:req().
pin_message(Req0, State) ->
    Uid = maps:get(current_uid, State),
    % 从路径参数获取 message_id
    case cowboy_req:binding(message_id, Req0) of
        undefined ->
            elib_response:error(Req0, <<"消息ID不能为空"/utf8>>);
        MessageId ->
            PostVals = elib_param:post(Req0),
            % 前端传 'pinned' 字段
            Pinned = maps:get(<<"pinned">>, PostVals, true),
            case channel_logic:pin_message(Uid, MessageId, Pinned) of
                {ok, Message} ->
                    elib_response:success(Req0, Message);
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% @doc 删除消息 (通过 DELETE 方法)
-spec delete_message(cowboy_req:req(), map()) -> cowboy_req:req().
delete_message(Req0, State) ->
    Uid = maps:get(current_uid, State),
    % 从路径参数获取 message_id
    case cowboy_req:binding(message_id, Req0) of
        undefined ->
            elib_response:error(Req0, <<"消息ID不能为空"/utf8>>);
        MessageId ->
            case channel_logic:delete_message(Uid, MessageId) of
                ok ->
                    elib_response:success(Req0, #{});
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% @doc 撤回消息
-spec revoke_message(cowboy_req:req(), map()) -> cowboy_req:req().
revoke_message(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    ChannelId = resolve_channel_id(Req0, PostVals),
    MessageId = resolve_message_id(Req0, PostVals),
    case ChannelId of
        <<>> ->
            elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
        _ when MessageId =:= <<>> ->
            elib_response:error(Req0, <<"消息ID不能为空"/utf8>>);
        _ ->
            case channel_logic:revoke_message(Uid, ChannelId, MessageId) of
                ok ->
                    elib_response:success(Req0, #{});
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% ===================================================================
%% 订阅者管理 API
%% ===================================================================

%% @doc 获取频道订阅者列表
-spec subscribers(cowboy_req:req(), map()) -> cowboy_req:req().
subscribers(Req0, _State) ->
    % 从路径参数获取 channel_id
    case cowboy_req:binding(channel_id, Req0) of
        undefined ->
            elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
        ChannelId ->
            Qs = cowboy_req:parse_qs(Req0),
            CursorBin = proplists:get_value(<<"cursor">>, Qs, <<>>),
            Limit = parse_qs_int(proplists:get_value(<<"limit">>, Qs), 50, 1, 200),

            Cursor = case CursorBin of
                <<>> -> 0;
                _ -> parse_qs_int(CursorBin, 0, 0, 16#7fffffff)
            end,

            case channel_logic:get_subscribers(ChannelId, Cursor, Limit) of
                {ok, Subscribers} ->
                    elib_response:success(Req0, #{list => Subscribers, cursor => Cursor, limit => Limit});
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% ===================================================================
%% 邀请相关 API（私有频道）
%% ===================================================================

%% @doc 创建邀请
-spec create_invitation(cowboy_req:req(), map()) -> cowboy_req:req().
create_invitation(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    ChannelId = resolve_channel_id(Req0, PostVals),
    InviteeUid = decode_positive_id(maps:get(<<"invitee_uid">>, PostVals, <<>>)),

    case ChannelId of
        <<>> ->
            elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
        _ when InviteeUid =:= 0 ->
            elib_response:error(Req0, <<"被邀请人ID不能为空"/utf8>>);
        _ ->
            case channel_logic:create_invitation(Uid, ChannelId, InviteeUid) of
                {ok, Invitation} ->
                    elib_response:success(Req0, Invitation);
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% @doc 接受邀请
-spec accept_invitation(cowboy_req:req(), map()) -> cowboy_req:req().
accept_invitation(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    InvitationId = decode_positive_id(maps:get(<<"invitation_id">>, PostVals, <<>>)),

    case InvitationId of
        0 ->
            elib_response:error(Req0, <<"邀请ID不能为空"/utf8>>);
        _ ->
            case channel_logic:accept_invitation(Uid, InvitationId) of
                ok ->
                    elib_response:success(Req0, #{});
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% @doc 拒绝邀请
-spec reject_invitation(cowboy_req:req(), map()) -> cowboy_req:req().
reject_invitation(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    InvitationId = decode_positive_id(maps:get(<<"invitation_id">>, PostVals, <<>>)),

    case InvitationId of
        0 ->
            elib_response:error(Req0, <<"邀请ID不能为空"/utf8>>);
        _ ->
            case channel_logic:reject_invitation(Uid, InvitationId) of
                ok ->
                    elib_response:success(Req0, #{});
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% @doc 获取我的邀请列表
-spec my_invitations(cowboy_req:req(), map()) -> cowboy_req:req().
my_invitations(Req0, State) ->
    Uid = maps:get(current_uid, State),
    case channel_logic:get_my_invitations(Uid) of
        {ok, Invitations} ->
            elib_response:success(Req0, #{list => Invitations});
        {error, Msg} ->
            elib_response:error(Req0, normalize_error_binary(Msg, <<"查询失败"/utf8>>))
    end.

%% @doc 获取我发出的邀请列表
-spec sent_invitations(cowboy_req:req(), map()) -> cowboy_req:req().
sent_invitations(Req0, State) ->
    Uid = maps:get(current_uid, State),
    case channel_logic:get_sent_invitations(Uid) of
        {ok, Invitations} ->
            elib_response:success(Req0, #{list => Invitations});
        {error, Msg} ->
            elib_response:error(Req0, normalize_error_binary(Msg, <<"查询失败"/utf8>>))
    end.

%% ===================================================================
%% 订单相关 API（付费频道）
%% ===================================================================

%% @doc 创建订单
-spec create_order(cowboy_req:req(), map()) -> cowboy_req:req().
create_order(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    ChannelId = resolve_channel_id(Req0, PostVals),

    case ChannelId of
        <<>> ->
            elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
        _ ->
            case channel_logic:create_order(Uid, ChannelId) of
                {ok, Order} ->
                    elib_response:success(Req0, Order);
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% @doc 支付订单（模拟支付）
-spec pay_order(cowboy_req:req(), map()) -> cowboy_req:req().
pay_order(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    OrderNo = normalize_non_empty_binary(maps:get(<<"order_no">>, PostVals, <<>>)),

    case OrderNo of
        <<>> ->
            elib_response:error(Req0, <<"订单号不能为空"/utf8>>);
        _ ->
            case channel_logic:pay_order(Uid, OrderNo) of
                ok ->
                    elib_response:success(Req0, #{});
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% @doc 获取我的订单列表
-spec my_orders(cowboy_req:req(), map()) -> cowboy_req:req().
my_orders(Req0, State) ->
    Uid = maps:get(current_uid, State),
    case channel_logic:get_my_orders(Uid) of
        {ok, Orders} ->
            elib_response:success(Req0, #{list => Orders});
        {error, Msg} ->
            elib_response:error(Req0, Msg)
    end.

%% @doc 获取订单详情
-spec get_order(cowboy_req:req(), map()) -> cowboy_req:req().
get_order(Req0, State) ->
    Uid = maps:get(current_uid, State),
    % 从路径参数获取 order_no
    OrderNo = normalize_non_empty_binary(cowboy_req:binding(order_no, Req0)),
    case OrderNo of
        <<>> ->
            elib_response:error(Req0, <<"订单号不能为空"/utf8>>);
        _ ->
            case channel_logic:get_order(Uid, OrderNo) of
                {ok, Order} ->
                    elib_response:success(Req0, Order);
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% ===================================================================
%% 管理员列表与角色更新 API
%% ===================================================================

%% @doc 获取频道管理员列表
-spec admins(cowboy_req:req(), map()) -> cowboy_req:req().
admins(Req0, _State) ->
    case cowboy_req:binding(channel_id, Req0) of
        undefined ->
            elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
        ChannelId ->
            case channel_logic:get_admins(ChannelId) of
                {ok, Admins} ->
                    elib_response:success(Req0, #{list => Admins});
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% @doc 更新管理员角色
-spec update_admin_role(cowboy_req:req(), map()) -> cowboy_req:req().
update_admin_role(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    Role = ec_cnv:to_integer(maps:get(<<"role">>, PostVals, 1)),
    case Role < 1 orelse Role > 3 of
        true ->
            elib_response:error(Req0, <<"角色值必须在1-3之间"/utf8>>);
        false ->
            ChannelId = resolve_channel_id(Req0, PostVals),
            UserIdBin = resolve_user_id_bin(Req0, PostVals),
            case ChannelId of
                <<>> ->
                    elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
                _ ->
                    case UserIdBin of
                        <<>> ->
                            elib_response:error(Req0, <<"用户ID不能为空"/utf8>>);
                        _ ->
                            TargetUid = decode_positive_id(UserIdBin),
                            case TargetUid of
                                0 ->
                                    elib_response:error(Req0, <<"用户ID不能为空"/utf8>>);
                                _ ->
                                    case channel_logic:update_admin_role(Uid, ChannelId, TargetUid, Role) of
                                        ok ->
                                            elib_response:success(Req0, #{});
                                        {error, Msg} ->
                                            elib_response:error(Req0, Msg)
                                    end
                            end
                    end
            end
    end.

%% @doc 统一解析频道 ID：路径参数优先，body 兼容回退
-spec resolve_channel_id(cowboy_req:req(), map()) -> binary().
resolve_channel_id(Req0, PostVals) ->
    case binding_or_empty(channel_id, Req0) of
        <<>> -> maps:get(<<"channel_id">>, PostVals, <<>>);
        ChannelId -> ChannelId
    end.

%% @doc 统一解析消息 ID：路径参数优先，body 兼容回退
-spec resolve_message_id(cowboy_req:req(), map()) -> binary().
resolve_message_id(Req0, PostVals) ->
    case binding_or_empty(message_id, Req0) of
        <<>> -> maps:get(<<"message_id">>, PostVals, <<>>);
        MessageId -> MessageId
    end.

%% @doc 统一解析用户 ID（HashID 文本）：路径参数优先，body 兼容回退
-spec resolve_user_id_bin(cowboy_req:req(), map()) -> binary().
resolve_user_id_bin(Req0, PostVals) ->
    case binding_or_empty(user_id, Req0) of
        <<>> -> maps:get(<<"user_id">>, PostVals, <<>>);
        UserIdBin -> UserIdBin
    end.

%% @doc 统一解析反应类型：路径参数优先，body 兼容回退
-spec resolve_reaction_type(cowboy_req:req(), map()) -> binary().
resolve_reaction_type(Req0, PostVals) ->
    case binding_or_empty(reaction_type, Req0) of
        <<>> -> maps:get(<<"reaction_type">>, PostVals, <<"like">>);
        ReactionType -> ReactionType
    end.

-spec binding_or_empty(atom(), cowboy_req:req()) -> binary().
binding_or_empty(Key, Req0) ->
    case cowboy_req:binding(Key, Req0) of
        undefined -> <<>>;
        Val -> Val
    end.

%% ===================================================================
%% 移除订阅者 API
%% ===================================================================

%% @doc 移除频道订阅者（管理员及以上权限）
-spec remove_subscriber(cowboy_req:req(), map()) -> cowboy_req:req().
remove_subscriber(Req0, State) ->
    Uid = maps:get(current_uid, State),
    case cowboy_req:binding(channel_id, Req0) of
        undefined ->
            elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
        ChannelId ->
            case cowboy_req:binding(user_id, Req0) of
                undefined ->
                    elib_response:error(Req0, <<"用户ID不能为空"/utf8>>);
                UserIdBin ->
                    TargetUid = decode_positive_id(UserIdBin),
                    case TargetUid of
                        0 ->
                            elib_response:error(Req0, <<"用户ID不能为空"/utf8>>);
                        _ ->
                            case channel_logic:remove_subscriber(Uid, ChannelId, TargetUid) of
                                ok ->
                                    elib_response:success(Req0, #{});
                                {error, Msg} ->
                                    elib_response:error(Req0, Msg)
                            end
                    end
            end
    end.

%% ===================================================================
%% 频道增量同步 API
%% ===================================================================

%% @doc 频道增量同步
-spec sync(cowboy_req:req(), map()) -> cowboy_req:req().
sync(Req0, State) ->
    Uid = maps:get(current_uid, State),
    Qs = cowboy_req:parse_qs(Req0),
    Since = parse_qs_int(proplists:get_value(<<"since">>, Qs), 0, 0, 16#7fffffff),
    case channel_logic:sync_channels(Uid, Since) of
        {ok, Data} ->
            elib_response:success(Req0, Data);
        {error, Msg} ->
            elib_response:error(Req0, Msg)
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
decode_positive_id(Value) ->
    case catch elib_hashids:decode(Value) of
        Id when is_integer(Id), Id > 0 ->
            Id;
        _ ->
            0
    end.

-spec normalize_non_empty_binary(term()) -> binary().
normalize_non_empty_binary(undefined) ->
    <<>>;
normalize_non_empty_binary(Value) when is_binary(Value) ->
    list_to_binary(string:trim(binary_to_list(Value)));
normalize_non_empty_binary(Value) when is_list(Value) ->
    list_to_binary(string:trim(Value));
normalize_non_empty_binary(Value) when is_integer(Value) ->
    integer_to_binary(Value);
normalize_non_empty_binary(_) ->
    <<>>.

-spec normalize_error_binary(term(), binary()) -> binary().
normalize_error_binary(Msg, Default) ->
    Bin0 = case Msg of
        Value when is_binary(Value); is_list(Value); is_integer(Value) ->
            normalize_non_empty_binary(Value);
        _ ->
            elib_cnv:safe_to_binary(Msg)
    end,
    case normalize_non_empty_binary(Bin0) of
        <<>> ->
            Default;
        Bin ->
            Bin
    end.
