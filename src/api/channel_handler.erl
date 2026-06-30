-module(channel_handler).
-compile([nowarn_deprecated_catch]).
%% Thin HTTP adapter for the channel_content domain.
%% Message/order/admin functions split into sub-handlers (H4):
%%   channel_handler_message  (消息/置顶/反应/订阅者)
%%   channel_handler_admin    (管理员/邀请/同步)
%%   channel_handler_order    (订单/支付)

-behavior(cowboy_rest).

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
handle_action(stats, Req, State) -> stats(Req, State);
handle_action(stats_daily, Req, State) -> stats_daily(Req, State);
handle_action(false, Req, _State) -> Req.

%% @doc 创建频道
-spec create(cowboy_req:req(), map()) -> cowboy_req:req().
create(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    Name = maps:get(<<"name">>, PostVals, <<>>),
    Type = elib_cnv:safe_to_integer(maps:get(<<"type">>, PostVals, 0)),
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
            MaxChannels = 20,
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
    Cursor =
        case CursorBin of
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
    case cowboy_req:binding(channel_id, Req0) of
        undefined ->
            elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
        ChannelId ->
            Qs = cowboy_req:parse_qs(Req0),
            CursorBin = proplists:get_value(<<"cursor">>, Qs, <<>>),
            Limit = parse_qs_int(proplists:get_value(<<"limit">>, Qs), 20, 1, 200),
            Cursor =
                case CursorBin of
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
                    case channel_logic:get_channel_stats(ChannelId) of
                        {ok, Stats} ->
                            elib_response:success(Req0, Stats);
                        {error, Msg} ->
                            elib_response:error(Req0, Msg)
                    end
            end
    end.

-spec stats_daily(cowboy_req:req(), map()) -> cowboy_req:req().
stats_daily(Req0, _State) ->
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
    case catch elib_cnv:safe_to_integer(Value) of
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
    Bin0 =
        case Msg of
            Value when is_binary(Value); is_list(Value); is_integer(Value) ->
                normalize_non_empty_binary(Value);
            _ ->
                elib_cnv:safe_to_binary(Msg)
        end,
    % ponytail: Bin0 is always binary here; skip normalize_non_empty_binary to avoid dead-pattern warnings
    case Bin0 of
        <<>> -> Default;
        Bin -> Bin
    end.
