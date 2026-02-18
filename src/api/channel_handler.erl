-module(channel_handler).

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
    Req1 = handle_action(Action, Req0, State),
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
% 订阅者管理
handle_action(subscribers, Req, State) -> subscribers(Req, State);
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
    Type = maps:get(<<"type">>, PostVals, 0),
    Description = maps:get(<<"description">>, PostVals, <<>>),
    Avatar = maps:get(<<"avatar">>, PostVals, <<>>),
    CustomId = maps:get(<<"custom_id">>, PostVals, undefined),
    Tags = maps:get(<<"tags">>, PostVals, []),

    case Name of
        <<>> ->
            elib_response:error(Req0, <<"频道名称不能为空"/utf8>>);
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
by_custom_id(Req0, _State) ->
    % 从路径参数获取 custom_id
    case cowboy_req:binding(custom_id, Req0) of
        undefined ->
            elib_response:error(Req0, <<"自定义ID不能为空"/utf8>>);
        CustomId ->
            case channel_logic:get_channel_by_custom_id(CustomId) of
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
    ChannelId = maps:get(<<"channel_id">>, PostVals, <<>>),
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
    ChannelId = maps:get(<<"channel_id">>, PostVals, <<>>),

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
    ChannelId = maps:get(<<"channel_id">>, PostVals, <<>>),

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
    ChannelId = maps:get(<<"channel_id">>, PostVals, <<>>),

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
    Cursor = case CursorBin of <<>> -> undefined; _ -> binary_to_integer(CursorBin) end,
    Limit = case proplists:get_value(<<"limit">>, Qs) of
        undefined -> 50;
        LimitBin -> binary_to_integer(LimitBin)
    end,

    {ok, Channels} = channel_logic:get_subscribed_channels(Uid),
    elib_response:success(Req0, #{list => Channels, cursor => Cursor, limit => Limit}).

%% @doc 获取用户管理的频道列表
-spec managed(cowboy_req:req(), map()) -> cowboy_req:req().
managed(Req0, State) ->
    Uid = maps:get(current_uid, State),
    {ok, Channels} = channel_logic:get_managed_channels(Uid),
    elib_response:success(Req0, #{list => Channels}).

%% @doc 发布频道消息
-spec publish_message(cowboy_req:req(), map()) -> cowboy_req:req().
publish_message(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    ChannelId = maps:get(<<"channel_id">>, PostVals, <<>>),
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
messages(Req0, _State) ->
    % 从路径参数获取 channel_id
    case cowboy_req:binding(channel_id, Req0) of
        undefined ->
            elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
        ChannelId ->
            Qs = cowboy_req:parse_qs(Req0),
            CursorBin = proplists:get_value(<<"cursor">>, Qs, <<>>),
            Limit = case proplists:get_value(<<"limit">>, Qs) of
                undefined -> 20;
                LimitBin -> binary_to_integer(LimitBin)
            end,

            Cursor = case CursorBin of <<>> -> 0; _ -> binary_to_integer(CursorBin) end,

            {ok, Messages} = channel_logic:get_messages(ChannelId, Cursor, Limit),
            elib_response:success(Req0, #{list => Messages})
    end.

%% @doc 标记消息已读
-spec mark_read(cowboy_req:req(), map()) -> cowboy_req:req().
mark_read(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    ChannelId = maps:get(<<"channel_id">>, PostVals, <<>>),
    MessageId = maps:get(<<"message_id">>, PostVals, <<>>),

    case ChannelId of
        <<>> ->
            elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
        _ ->
            ok = channel_logic:mark_as_read(Uid, ChannelId, MessageId),
            elib_response:success(Req0, #{})
    end.

%% @doc 搜索频道
-spec search(cowboy_req:req(), map()) -> cowboy_req:req().
search(Req0, _State) ->
    Qs = cowboy_req:parse_qs(Req0),
    Keyword = proplists:get_value(<<"keyword">>, Qs, <<>>),
    Limit = case proplists:get_value(<<"limit">>, Qs) of
        undefined -> 20;
        LimitBin -> binary_to_integer(LimitBin)
    end,

    case Keyword of
        <<>> ->
            elib_response:success(Req0, #{list => []});
        _ ->
            {ok, Channels} = channel_logic:search_channels(Keyword, Limit),
            elib_response:success(Req0, #{list => Channels})
    end.

%% @doc 发现频道（推荐）
-spec discover(cowboy_req:req(), map()) -> cowboy_req:req().
discover(Req0, _State) ->
    Qs = cowboy_req:parse_qs(Req0),
    Limit = case proplists:get_value(<<"limit">>, Qs) of
        undefined -> 20;
        LimitBin -> binary_to_integer(LimitBin)
    end,
    _Category = proplists:get_value(<<"category">>, Qs, undefined),

    % 返回公开的、订阅数最多的频道
    {ok, Channels} = channel_logic:get_discover_channels(Limit),
    elib_response:success(Req0, #{list => Channels}).

%% @doc 添加频道管理员
-spec add_admin(cowboy_req:req(), map()) -> cowboy_req:req().
add_admin(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    ChannelId = maps:get(<<"channel_id">>, PostVals, <<>>),
    NewAdminUid = elib_hashids:decode(maps:get(<<"user_id">>, PostVals, <<>>)),
    Role = maps:get(<<"role">>, PostVals, 1),

    case ChannelId of
        <<>> ->
            elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
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
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    ChannelId = maps:get(<<"channel_id">>, PostVals, <<>>),
    AdminUid = elib_hashids:decode(maps:get(<<"user_id">>, PostVals, <<>>)),

    case ChannelId of
        <<>> ->
            elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
        _ ->
            case channel_logic:remove_admin(Uid, ChannelId, AdminUid) of
                ok ->
                    elib_response:success(Req0, #{});
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
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
    ChannelId = maps:get(<<"channel_id">>, PostVals, <<>>),
    MessageId = maps:get(<<"message_id">>, PostVals, <<>>),

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
    ChannelId = maps:get(<<"channel_id">>, PostVals, <<>>),
    MessageId = maps:get(<<"message_id">>, PostVals, <<>>),
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
    ChannelId = maps:get(<<"channel_id">>, PostVals, <<>>),
    MessageId = maps:get(<<"message_id">>, PostVals, <<>>),
    ReactionType = maps:get(<<"reaction_type">>, PostVals, <<"like">>),

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
            Days = case proplists:get_value(<<"days">>, Qs) of
                undefined -> 7;
                DaysBin -> binary_to_integer(DaysBin)
            end,

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
            Limit = case proplists:get_value(<<"limit">>, Qs) of
                undefined -> 50;
                LimitBin -> binary_to_integer(LimitBin)
            end,

            Cursor = case CursorBin of <<>> -> 0; _ -> binary_to_integer(CursorBin) end,

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
    ChannelId = maps:get(<<"channel_id">>, PostVals, <<>>),
    InviteeUid = elib_hashids:decode(maps:get(<<"invitee_uid">>, PostVals, <<>>)),

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
    InvitationId = elib_hashids:decode(maps:get(<<"invitation_id">>, PostVals, <<>>)),

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
    InvitationId = elib_hashids:decode(maps:get(<<"invitation_id">>, PostVals, <<>>)),

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
    {ok, Invitations} = channel_logic:get_my_invitations(Uid),
    elib_response:success(Req0, #{list => Invitations}).

%% @doc 获取我发出的邀请列表
-spec sent_invitations(cowboy_req:req(), map()) -> cowboy_req:req().
sent_invitations(Req0, State) ->
    Uid = maps:get(current_uid, State),
    {ok, Invitations} = channel_logic:get_sent_invitations(Uid),
    elib_response:success(Req0, #{list => Invitations}).

%% ===================================================================
%% 订单相关 API（付费频道）
%% ===================================================================

%% @doc 创建订单
-spec create_order(cowboy_req:req(), map()) -> cowboy_req:req().
create_order(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    ChannelId = maps:get(<<"channel_id">>, PostVals, <<>>),

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
    OrderNo = maps:get(<<"order_no">>, PostVals, <<>>),

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
    {ok, Orders} = channel_logic:get_my_orders(Uid),
    elib_response:success(Req0, #{list => Orders}).

%% @doc 获取订单详情
-spec get_order(cowboy_req:req(), map()) -> cowboy_req:req().
get_order(Req0, State) ->
    Uid = maps:get(current_uid, State),
    % 从路径参数获取 order_no
    case cowboy_req:binding(order_no, Req0) of
        undefined ->
            elib_response:error(Req0, <<"订单号不能为空"/utf8>>);
        OrderNo ->
            case channel_logic:get_order(Uid, OrderNo) of
                {ok, Order} ->
                    elib_response:success(Req0, Order);
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.
